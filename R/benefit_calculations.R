
# =============================================================================
# BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains the core benefit calculation functions for the ssmbar package.
# Functions are organized in the order they are called in the benefit calculation
# pipeline (see calculate_benefits() in CL_benefit_calculator.R). Spousal benefit
# functions are included in a separate file:
#
#   earnings -> aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#            -> spouse_benefit() -> ret() -> final_benefit()
#
# =============================================================================


# =============================================================================
# SECTION 0: Assumptions Join Helper
# =============================================================================
# This helper joins all assumption columns needed by the benefit calculation
# pipeline in a single operation, avoiding redundant joins in each function.

#' Join All Required Assumptions Columns
#'
#' Joins all assumption columns needed by the benefit calculation pipeline
#' in a single operation. This avoids redundant joins in individual functions.
#'
#' @param worker Data frame with worker data (must have 'year' column)
#' @param assumptions Data frame with assumptions (tr2025 or similar)
#'
#' @return Worker data frame with all assumption columns joined
#'
#' @keywords internal
join_all_assumptions <- function(worker, assumptions) {
  # All columns needed by the benefit calculation pipeline
  # - aime: awi, taxmax, qc_rec, qc_required, max_qc_per_year, max_dropout_years, min_comp_period, index_age_offset
  # - pia: bp1, bp2, fact1, fact2, fact3, elig_age_retired
  # - cola: cpi_w
  # - worker_benefit: nra, rf1, rf2, drc, drc_max_months
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - ret: ret1, ret_phaseout_rate

  cols_needed <- c("year", "awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "cpi_w", "nra", "rf1", "rf2", "drc", "drc_max_months",
                   "s_pia_share", "s_rf1", "s_rf2", "ret1", "ret_phaseout_rate")

  # Only join columns that aren't already present
  cols_present <- names(worker)
  cols_to_join <- cols_needed[!cols_needed %in% cols_present | cols_needed == "year"]

  if (length(cols_to_join) > 1) {  # More than just 'year'
    worker <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  }

  return(worker)
}


# =============================================================================
# SECTION 1: Actuarial Adjustment Helper
# =============================================================================
# This helper function is used by worker_benefit(), spouse_benefit(), and ret()
# to calculate early retirement reduction factors and delayed retirement credits.

#' Early Retirement Reduction Factor and Delayed Retirement Credit Calculation
#'
#' Function that computes a worker's actuarial adjustment to their benefits based on their claiming age.
#'
#' @param claim_age Numeric value that represents the age at which a worker first claims benefits
#' @param nra Numeric value that represents a worker's Normal Retirement Age
#' @param rf1 Numeric value that represents the incremental reduction in benefits for the first 36 months prior to the NRA based on the worker's birth cohort.
#' @param rf2 Numeric value that represents the incremental reduction in benefits for the additional months past 36 that in which benefits are claimed early.
#' @param drc Numeric value that represents the incremental increase in benefits for the months claimed past the NRA, based on the worker's birth cohort.
#' @param drc_max_months Numeric value for maximum months of DRC (currently 36 months, capping at age 70). Default 36.
#'
#' @return act_factor numeric value used for adjusting a worker's PIA to compute their monthly benefit
#'
#' @export
rf_and_drc <- function(claim_age, nra, rf1, rf2, drc, drc_max_months = 36) {
  # Benefit reduction factors are described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  #
  # rf1: Reduction for first 36 months early (5/9 of 1% per month)
  # rf2: Reduction for months beyond 36 early (5/12 of 1% per month)
  # drc: Delayed retirement credit per month (varies by birth year, max 8%/yr)
  # drc_max_months: Maximum months of DRC (36 = 3 years past NRA, capping at age 70)

  dist_from_nra <- (claim_age - nra) * 12 # Distance from Normal Retirement Age in months

  # Calculate reduction factors
  rf_amt <- if_else(dist_from_nra >= 0, 0, # If claiming at or above NRA, no RFs
                   if_else(dist_from_nra <= -36, (-36*rf1) + (pmax(-24,(dist_from_nra + 36))*rf2), # If claiming more than three years before NRA
                          dist_from_nra * rf1)) # If claiming less than three years before NRA

  # Calculate DRCs (capped at drc_max_months)
  drc_amt <- if_else(dist_from_nra <= 0, 0, # If claiming at or below NRA
                    pmin(drc_max_months * drc, dist_from_nra * drc)) # If claiming above NRA. DRCs capped at drc_max_months past NRA

  act_factor <- 1 + rf_amt + drc_amt # Final actuarial factor for adjusting benefits

  return(act_factor)

}


# =============================================================================
# SECTION 2: Main Benefit Calculation Pipeline
# =============================================================================
# These functions are called in sequence to calculate benefits.
# Order: aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#        -> spouse_benefit() -> ret() -> final_benefit()


# -----------------------------------------------------------------------------
# 2.1 AIME Calculation
# -----------------------------------------------------------------------------

#' AIME Calculation
#'
#' Function that computes a worker's Average Indexed Monthly Earnings by age
#'
#' @param worker Dataframe with a worker's earnings by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's AIME by year and age
#'
#' @export
aime <- function(worker, assumptions, debugg = FALSE){ #Function for calculating the AIME of a specific worker
  #How earnings are indexed is described in Section 700.3 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  #AIME Computation is described in Section 701 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html

  # Determine which columns to join from assumptions (avoid duplicates)
  # Skip join if all columns already present (from join_all_assumptions)
  # Program rule parameters needed for AIME calculation:
  # - qc_required: QCs needed for eligibility (Section 203)
  # - max_qc_per_year: Max QCs per year (Section 212)
  # - max_dropout_years, min_comp_period: For computation period (Section 703)
  # - index_age_offset: Indexing year offset from eligibility age (Section 700.4)
  cols_needed <- c("taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset", "awi")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>% qc_comp(debugg) # Function for determining annual and cumulative QCs earned at each age
  dataset <- dataset %>% comp_period(debugg) # Function for determining a worker's computation period based on their eligibility age

  # Calculate indexed earnings
  # Earnings are indexed to AWI at (elig_age - index_age_offset) years before eligibility
  # SSA Handbook Section 700.4: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      index_age = elig_age - first(index_age_offset), # Age for wage indexing (e.g., 62 - 2 = 60)
      awi_index_age = awi[which(age == index_age)], # Retrieve AWI at indexing age
      index_factor = pmax(awi_index_age / awi, 1), # Calculate indexing factor. Earnings past indexing age are taken at face value.
      capped_earn = pmin(earnings, taxmax), # Cap earnings amounts at the taxable maximum at each age
      indexed_earn = capped_earn * index_factor # Indexed capped earnings amounts
    ) %>%
    ungroup()

  # AIME Calculation
  # SSA Handbook Section 701: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      n <- nrow(.x)
      aime_vals <- numeric(n)
      indexed_earnings <- .x$indexed_earn
      qc_required_val <- .x$qc_required[1] # QCs required for eligibility (from assumptions)
      qc_eligible <- .x$qc_tot >= qc_required_val # Workers need qc_required QCs for retirement benefits (Section 203)
      comp_period <- .x$comp_period # Worker's computation (or, averaging) period
      age_eligible <- .x$age >= .x$elig_age # Worker's eligibility age (age 62 for retirement, age of disability, or age of death of deceased spouse)

      for (i in seq_len(n)) {
        if (!is.na(qc_eligible[i]) && qc_eligible[i] && !is.na(age_eligible[i]) && age_eligible[i]) { #Only calculates AIME if worker has enough QCs and is at or past their eligiblity age.
          years_to_use <- min(i, comp_period[i]) #Restriction so AIME calculation doesn't break if not enough years have passed to equal full comp period.
          # Optimized: use partial sort when years_to_use < i (O(n) vs O(n log n))
          # partial = k ensures elements 1:k are the k smallest, so we negate to get largest
          earnings_subset <- indexed_earnings[1:i]
          if (i > years_to_use) {
            # Partial sort: get the years_to_use largest values efficiently
            top_earnings_sum <- sum(-sort(-earnings_subset, partial = 1:years_to_use)[1:years_to_use])
          } else {
            # Use all available earnings
            top_earnings_sum <- sum(earnings_subset)
          }
          aime_vals[i] <- floor(top_earnings_sum / (comp_period[i] * 12)) #AIME calculation, rounded to the next lowest dollar (see Handbook)
        }
      }

      .x$aime <- aime_vals #Stores AIME vals
      .x
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime, qc_i, qc_tot, qc_rec, comp_period, elapsed_years, dropout_years, index_age, awi_index_age, index_factor, capped_earn, indexed_earn),
                                   by = c("id", "age")) #Selects additional output if debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime),
                                   by = c("id", "age")) #left joins only those variables needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.2 PIA Calculation
# -----------------------------------------------------------------------------

#' PIA Calculation
#'
#' Function that computes a worker's Primary Insurance Amount by age
#'
#' @param worker Dataframe with a worker's earnings and AIME by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker dataframe with a worker's retired worker PIA by age
#'
#' @export
pia <- function(worker, assumptions, debugg = FALSE) {
  # PIA calculation is described in Section 706 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0706.html
  #
  # Bend points and replacement factors are determined at the worker's eligibility age
  # (elig_age_retired from assumptions, currently 62 for retirement benefits)


  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
      bp1_elig = bp1[which(age == elig_age_ret)], # First PIA bend point at eligibility age
      bp2_elig = bp2[which(age == elig_age_ret)], # Second PIA bend point at eligibility age
      fact1_elig = fact1[which(age == elig_age_ret)], # First replacement factor (90%)
      fact2_elig = fact2[which(age == elig_age_ret)], # Second replacement factor (32%)
      fact3_elig = fact3[which(age == elig_age_ret)], # Third replacement factor (15%)
      basic_pia = case_when(
      age >= elig_age ~ floor(case_when( # PIA Calculation -- only occurs in and after a worker's eligibility age
                        aime > bp2_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (bp2_elig - bp1_elig)) + (fact3_elig * (aime - bp2_elig)),
                        aime > bp1_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (aime - bp1_elig)),
                        TRUE ~ fact1_elig * aime
                      )),
      TRUE ~ 0)
    ) %>% select(-bp1, -bp2, -fact1, -fact2, -fact3, -elig_age_retired) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia, bp1_elig, bp2_elig, fact1_elig, fact2_elig, fact3_elig),
                                   by = c("id","age")) # Left joins vars for debugging
  }

  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia),
                                   by = c("id","age")) #Left joins ony vars needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.3 COLA Calculation
# -----------------------------------------------------------------------------

#' COLA Calculation
#'
#' Function that computes a worker's COLA-adjusted PIA
#'
#' @param worker Dataframe with a worker's unadjusted PIA -- both retired worker and spousal -- by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's COLA-adjusted retired worker and spousal PIA by age
#'
#' @export
cola <- function (worker, assumptions, debugg = FALSE) {
  # COLA adjustments begin at eligibility age (elig_age_retired from assumptions)
  # Negative COLAs are not payable under current law
  # SSA Handbook Section 719: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0719.html

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("cpi_w", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>% mutate(
    elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
    cpi_elig = cpi_w[which(age == elig_age_ret)], # CPI-W at eligibility age, used for indexing COLAs
    cpi_index_factor = pmax(cummax(cpi_w) / cpi_elig, 1), # Indexing factor for COLAs. Negative COLAs are not payable under current law.
    cola_basic_pia = floor(basic_pia * cpi_index_factor) # COLA'd PIA at each age, rounded down to the nearest dollar
  ) %>% select(-elig_age_retired) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cpi_elig, cpi_index_factor),
                                   by = c("id","age")) # Left joins full vars for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia),
                                   by = c("id","age")) #Left joins only those vars needed to continue benefit calc
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.4 Worker Benefit Calculation
# -----------------------------------------------------------------------------

#' Retired Worker Benefit Calculation
#'
#' Function that calculates a worker's retirement benefit based on their own earnings record
#'
#' @param worker Dataframe with a worker's COLA-adjusted retired worker PIA by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a workjer's retired worker benefit by age
#'
#' @export
worker_benefit <- function(worker, assumptions, debugg = FALSE) {
  #Benefit reduction factors are descrinbed in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  #Function currently can only handle retired beneficiaries.

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("rf1", "rf2", "drc", "nra", "s_rf1", "s_rf2")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62, #RF/DRC amounts and NRA are based on year turning age 62 in assumptions.
      rf1_ind = rf1[which(year == yr_62)], #First reduction factor
      rf2_ind = rf2[which(year == yr_62)], #Second reduction factor
      drc_ind = drc[which(year == yr_62)], #Third reduction factor
      nra_ind = nra[which(year == yr_62)], #NRA for age 62 cohort
      act_factor = rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind), #Function that computes actuarial adjustment based on NRA, claiming age, and RFs and DRC levels
      wrk_ben = case_when(
        age >= claim_age ~ floor(cola_basic_pia * act_factor), #Computees retired worker benefit with retired worker COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, nra_ind, rf1_ind, rf2_ind, drc_ind, act_factor, wrk_ben),
                                   by = c("id","age") ) #Left joins variable for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, wrk_ben),
                                   by = c("id","age") ) #Left joins variables needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.5 Final Benefit Calculation
# -----------------------------------------------------------------------------
# Note: ret() function has been moved to R/ret.R

#' Final Benefit Calculation
#'
#' Function that calculates a worker's total retirement benefit combining
#' retired worker and spousal benefits.
#'
#' @param worker Dataframe with a worker's retired worker and spousal benefit by age
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's final monthly benefit by age
#'
#' @export
final_benefit <- function(worker, debugg = FALSE) {

  dataset <- worker %>%
    mutate(
      ben = pmax(wrk_ben, 0, na.rm = TRUE) + pmax(spouse_ben, 0, na.rm = TRUE)
    )

  if(debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, ben),
                                   by=c("id","age"))
  } else {
    # Join ben first, then select output columns
    worker <- worker %>%
      left_join(dataset %>% select(id, age, ben), by=c("id","age"))

    # Select core output columns, including sex and spouse_spec if they exist
    output_cols <- c("id", "sex", "year", "age", "spouse_spec", "earnings", "ben")
    available_cols <- intersect(output_cols, names(worker))

    worker <- worker %>% select(all_of(available_cols))
  }

  return(worker)

}
