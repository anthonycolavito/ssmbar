
# =============================================================================
# BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains the core benefit calculation functions for the ssmbar package.
# Functions are organized in the order they are called in the benefit calculation
# pipeline (see calculate_benefits() in CL_benefit_calculator.R). Spousal and survivor
# benefit functions are in separate files (spousal.R, survivor.R):
#
#   earnings -> aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#            -> spouse_benefit() -> widow_pia() -> widow_benefit() -> ret() -> final_benefit()
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
  # - cola: cola (year-by-year COLA percentage)
  # - worker_benefit: nra, rf1, rf2, drc, drc_max_months
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - ret: ret1, ret_phaseout_rate

  cols_needed <- c("year", "awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "cola", "nra", "rf1", "rf2", "drc", "drc_max_months",
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
# SECTION 0.5: Rounding Helper
# =============================================================================
# Per 42 USC 415(a)(2)(C) and 415(i)(2)(A)(ii), PIA and COLA-adjusted amounts
# are rounded to the next lower multiple of $0.10 (dime). This helper function
# applies consistent dime rounding across all benefit calculations.

#' Floor to Next Lower Dime
#'
#' Rounds a dollar amount down to the next lower multiple of $0.10, per
#' 42 USC 415(a)(2)(C) (PIA rounding) and 42 USC 415(i)(2)(A)(ii) (COLA rounding).
#'
#' @param x Numeric value(s) representing dollar amounts
#'
#' @return Numeric value(s) rounded down to the nearest $0.10
#'
#' @examples
#' floor_dime(1234.56)  # Returns 1234.50
#' floor_dime(1234.99)  # Returns 1234.90
#'
#' @keywords internal
floor_dime <- function(x) {
  floor(x * 10) / 10
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
  # rf1: Reduction for first 36 months early (5/9 of 1% per month for retired worker beneficiaries)
  # rf2: Reduction for months beyond 36 early (5/12 of 1% per month for retired worker beneficiaries)
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
  # Earnings are indexed to AWI at (elig_age - index_age_offset) 2 years before eligibility
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

      #AIME is equal to the average monthly earnings of the hightest earnings years in the computation period (typically 35, as for retired worker beneficiaries)
      # For January 1 claims: AIME at age X uses earnings through age X-1 (last complete year before claim)
      for (i in seq_len(n)) {
        if (!is.na(qc_eligible[i]) && qc_eligible[i] && !is.na(age_eligible[i]) && age_eligible[i]) { #Only calculates AIME if worker has enough QCs and is at or past their eligiblity age.
          # Use earnings through age-1 (i-1 rows) since claim is on January 1 before current year's earnings
          available_years <- i - 1
          years_to_use <- min(available_years, comp_period[i]) #Restriction so AIME calculation doesn't break if not enough years have passed to equal full comp period.
          # Optimized: use partial sort when years_to_use < available_years (O(n) vs O(n log n))
          # partial = k ensures elements 1:k are the k smallest, so we negate to get largest
          earnings_subset <- indexed_earnings[1:available_years]
          if (available_years > years_to_use) {
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
    cols_to_add <- c("aime", "qc_i", "qc_tot", "qc_rec", "comp_period", "elapsed_years", "dropout_years", "index_age", "awi_index_age", "index_factor", "capped_earn", "indexed_earn")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, age, all_of(cols_new)),
                                     by = c("id", "age"))
    }
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
      # Use worker's elig_age for bend points (disability age for disabled workers, 62 for retired workers)
      bp1_elig = bp1[which(age == first(elig_age))], # First PIA bend point at worker's eligibility age
      bp2_elig = bp2[which(age == first(elig_age))], # Second PIA bend point at worker's eligibility age
      fact1_elig = fact1[which(age == first(elig_age))], # First replacement factor (90%)
      fact2_elig = fact2[which(age == first(elig_age))], # Second replacement factor (32%)
      fact3_elig = fact3[which(age == first(elig_age))], # Third replacement factor (15%)
      # PIA per 42 USC 415(a)(2)(C): round to next lower $0.10
      basic_pia = case_when(
      age >= elig_age ~ floor_dime(case_when( # PIA Calculation -- only occurs in and after a worker's eligibility age
                        aime > bp2_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (bp2_elig - bp1_elig)) + (fact3_elig * (aime - bp2_elig)),
                        aime > bp1_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (aime - bp1_elig)),
                        TRUE ~ fact1_elig * aime
                      )),
      TRUE ~ 0)
    ) %>% select(-bp1, -bp2, -fact1, -fact2, -fact3, -elig_age_retired) %>% ungroup()

  if (debugg) {
    cols_to_add <- c("basic_pia", "bp1_elig", "bp2_elig", "fact1_elig", "fact2_elig", "fact3_elig")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, age, all_of(cols_new)),
                                     by = c("id","age"))
    }
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
#' @importFrom dplyr lag
#' @export
cola <- function (worker, assumptions, debugg = FALSE) {
  # COLA adjustments apply year-by-year starting at eligibility age
  # SSA Handbook Section 719: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0719.html
  #
  # How COLA works:
  # - At eligibility age (62): cola_basic_pia = basic_pia (no COLA applied yet)
  # - At age 63: cola_basic_pia = basic_pia × (1 + COLA from eligibility year)
  # - At age 64: cola_basic_pia = cola_pia_63 × (1 + COLA from age 63 year)
  # - etc.
  #
  # The COLA announced in year Y (based on Q3 CPI-W change) is applied to
  # benefits starting in January of year Y+1. So a worker reaching age 62
  # in 2025 first receives a COLA'd benefit in 2026 (using the 2025 COLA).

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("cola", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Calculate the COLA factor for each year
      # At eligibility age: factor = 1 (no COLA yet)
      # After eligibility age: factor = 1 + (previous year's COLA / 100)
      # The lag() gives us the COLA from the previous year, which applies to current year's benefit
      cola_factor = if_else(
        age == elig_age,
        1,
        1 + pmax(lag(cola, default = 0), 0) / 100  # Negative COLAs not payable
      ),
      # Before eligibility age, factor is 1 (no effect)
      cola_factor = if_else(age >= elig_age, cola_factor, 1)
    ) %>%
    # Apply COLA with year-by-year rounding (SSA method)
    # Each year's COLA-adjusted PIA is the PREVIOUS year's rounded PIA times current COLA factor
    # This matches how SSA actually calculates benefits
    group_modify(~ {
      n <- nrow(.x)
      cola_basic_pia_vals <- numeric(n)
      basic_pia <- .x$basic_pia
      cola_factor <- .x$cola_factor
      elig_age_val <- .x$elig_age[1]
      ages <- .x$age

      for (i in seq_len(n)) {
        if (ages[i] < elig_age_val) {
          cola_basic_pia_vals[i] <- 0
        } else if (ages[i] == elig_age_val) {
          # At eligibility age: no COLA yet, use basic_pia
          cola_basic_pia_vals[i] <- basic_pia[i]
        } else {
          # After eligibility: multiply previous year's rounded PIA by current COLA factor
          # Per 42 USC 415(i)(2)(A)(ii): round to next lower $0.10
          cola_basic_pia_vals[i] <- floor_dime(cola_basic_pia_vals[i-1] * cola_factor[i])
        }
      }

      .x$cola_basic_pia <- cola_basic_pia_vals
      # Calculate cumulative factor for reference (informational only)
      .x$cola_cum_factor <- cumprod(.x$cola_factor)
      .x
    }) %>%
    select(-elig_age_retired) %>%
    ungroup()

  if (debugg) {
    cols_to_add <- c("cola_basic_pia", "cola_factor", "cola_cum_factor", "cola")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, age, all_of(cols_new)),
                                     by = c("id","age"))
    }
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
#' @return worker Dataframe with a worker's retired worker benefit by age
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
      elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
      # Disabled workers (elig_age < elig_age_ret) get no actuarial adjustment - their benefit = 100% of PIA
      # Retired workers get actuarial adjustment based on claiming age relative to NRA
      act_factor = if_else(
        elig_age < elig_age_ret,
        1.0,  # Disabled workers: no actuarial reduction or credits
        rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind)  # Retired workers: apply actuarial adjustment
      ),
      wrk_ben = case_when(
        age >= claim_age ~ floor(cola_basic_pia * act_factor), #Computes worker benefit with COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    cols_to_add <- c("nra_ind", "rf1_ind", "rf2_ind", "drc_ind", "act_factor", "wrk_ben")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, age, all_of(cols_new)),
                                     by = c("id","age"))
    }
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
#' retired worker, spousal, and survivor benefits. Also assigns the Composite
#' Benefit Class (bc) indicator following the SSA BEPUF classification.
#'
#' @param worker Dataframe with a worker's retired worker, spousal, and survivor benefit by age
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's final monthly benefit by age and benefit class (bc)
#'
#' @details
#' The Composite Benefit Class (bc) follows the SSA Benefits and Earnings Public-Use File
#' (BEPUF) classification system. Currently supported benefit classes are:
#' \itemize{
#'   \item \strong{AR}: Retired Worker (not dually entitled)
#'   \item \strong{ARB}: Retired Worker dually entitled to a Spouse benefit
#'   \item \strong{ARD}: Retired Worker dually entitled to a Widow(er) benefit
#'   \item \strong{ARF}: Retired Worker dually entitled to a Disabled Widow(er) benefit
#'   \item \strong{AD}: Disabled Worker (not dually entitled)
#'   \item \strong{ADB}: Disabled Worker dually entitled to a Spouse benefit
#'   \item \strong{ADD}: Disabled Worker dually entitled to a Widow(er) benefit
#'   \item \strong{ADF}: Disabled Worker dually entitled to a Disabled Widow(er) benefit
#'   \item \strong{BR}: Spouse of Retired Worker (no own worker benefit, only spousal benefit)
#'   \item \strong{BD}: Spouse of Disabled Worker (no own worker benefit, spouse is disabled and before NRA)
#'   \item \strong{D}: Widow(er) only (not dually entitled to own worker benefit)
#'   \item \strong{F}: Disabled Widow(er) only (not dually entitled to own worker benefit)
#' }
#'
#' Note: Disabled workers (AD*) transition to retired workers (AR*) at Normal Retirement Age.
#' This affects only the benefit class code, not the benefit amount. Similarly, workers receiving
#' BD benefits transition to BR when their disabled spouse reaches NRA.
#'
#' Benefit classes not currently implemented in ssmbar include: E (other Survivor-only),
#' and CR, CD, CS (Child benefits).
#'
#' @seealso SSA BEPUF User Guide for complete benefit class definitions
#'
#' @export
final_benefit <- function(worker, debugg = FALSE) {
  #Section 733: Entitlement to More Than One Social Security Benefit at the Same Time
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0733.html
  #Section 734: Entitlement to Retirement or Disability Insurance Benefits and Another Benefit
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0734.html
  #
  # Per Section 733.2: If entitled to multiple benefits, only the higher is payable,
  # UNLESS one is a retirement/disability benefit (Section 734).
  # Per Section 734: Worker receives their full retired worker benefit PLUS the
  # difference between that and any higher auxiliary benefit.
  #
  # In this calculator:
  # - wrk_ben: Worker's own retired worker benefit (always paid)
  # - spouse_ben: EXCESS spousal benefit (spousal PIA - own PIA, with actuarial adjustment)
  # - survivor_ben: EXCESS survivor benefit (survivor PIA - own PIA, with actuarial adjustment)
  #
  # Spousal benefits apply while spouse is alive; survivor benefits apply after spouse dies.
  # Worker receives: wrk_ben + max(spouse_ben, survivor_ben)

  # Handle case where survivor_ben column doesn't exist (backwards compatibility)
  if (!"survivor_ben" %in% names(worker)) {
    worker$survivor_ben <- 0
  }

  # Handle case where worker_age_at_spouse_death column doesn't exist (backwards compatibility)
  if (!"worker_age_at_spouse_death" %in% names(worker)) {
    worker$worker_age_at_spouse_death <- NA_real_
  }

  # Handle case where elig_age_retired column doesn't exist (backwards compatibility)
  # Default to 62 if not present
  if (!"elig_age_retired" %in% names(worker)) {
    worker$elig_age_retired <- 62
  }

  # Handle case where is_disabled_widow column doesn't exist (backwards compatibility)
  if (!"is_disabled_widow" %in% names(worker)) {
    worker$is_disabled_widow <- FALSE
  }

  # Handle case where nra column doesn't exist (backwards compatibility)
  # Default to 67 if not present
  if (!"nra" %in% names(worker)) {
    worker$nra <- 67
  }

  # Handle case where s_elig_age column doesn't exist (backwards compatibility)
  # Default to elig_age_retired (62) meaning spouse is retired, not disabled
  if (!"s_elig_age" %in% names(worker)) {
    worker$s_elig_age <- worker$elig_age_retired
  }

  # Handle case where s_age column doesn't exist (backwards compatibility)
  # Default to NA (no spouse)
  if (!"s_age" %in% names(worker)) {
    worker$s_age <- NA_real_
  }

  dataset <- worker %>%
    group_by(id) %>%
    mutate(
      # Get worker's individual NRA (based on birth cohort)
      yr_62 = year - age + 62,
      nra_ind = nra[which(year == yr_62)][1],
      # Get spouse's NRA based on spouse's birth cohort (derived from s_age)
      # s_birth_yr = year - s_age, so spouse's yr_62 = (year - s_age) + 62
      s_yr_62 = if_else(!is.na(s_age), year - s_age + 62, NA_real_),
      s_nra_ind = if_else(!is.na(s_yr_62), nra[which(year == s_yr_62)][1], NA_real_)
    ) %>%
    ungroup() %>%
    mutate(
      # Zero out spousal benefit after spouse dies - survivor benefit takes over
      # Spousal benefits require the spouse to be alive
      spouse_ben_adj = if_else(
        !is.na(worker_age_at_spouse_death) & age >= worker_age_at_spouse_death,
        0,  # Spouse is dead, spousal benefit stops
        spouse_ben
      ),
      ben = pmax(wrk_ben, 0, na.rm = TRUE) + pmax(spouse_ben_adj, survivor_ben, 0, na.rm = TRUE),

      # Composite Benefit Class (bc) indicator
      # Following SSA BEPUF classification system
      # See: SSA Benefits and Earnings Public-Use File User Guide
      #
      # Logic:
      # - Originally disabled: elig_age < elig_age_retired
      # - Currently disabled (before NRA): elig_age < elig_age_retired AND age < nra_ind
      # - Now retired (at/after NRA): age >= nra_ind (regardless of original elig_age)
      # - Disabled widow(er): is_disabled_widow = TRUE (survivor benefit claimed at 50-59)
      # - Standard widow(er): survivor_ben > 0 AND is_disabled_widow = FALSE
      # - Dually entitled to Spouse: spouse_ben_adj > 0 (and >= survivor_ben)
      # - Dually entitled to Widow(er): survivor_ben > 0 (and > spouse_ben_adj)
      #
      # Note: Disabled workers transition to retired workers at NRA per SSA rules.
      # This affects only the BC code, not the benefit amount.

      # Determine if worker is currently classified as disabled or retired
      # Originally disabled (elig_age < 62) but transitions to retired at NRA
      is_originally_disabled = elig_age < elig_age_retired,
      is_currently_disabled = is_originally_disabled & age < nra_ind,

      # Determine if SPOUSE is currently classified as disabled or retired
      # Used for BD (Spouse of Disabled Worker) vs BR (Spouse of Retired Worker) codes
      # Spouse is originally disabled if s_elig_age < elig_age_retired
      # Spouse is currently disabled if originally disabled AND s_age < s_nra_ind
      s_is_originally_disabled = !is.na(s_elig_age) & s_elig_age < elig_age_retired,
      s_is_currently_disabled = s_is_originally_disabled & !is.na(s_age) & !is.na(s_nra_ind) & s_age < s_nra_ind,

      # Benefit Class indicator is defined so as to match the BC codes in SSA's 2020 BEPUF files
      # https://www.ssa.gov/policy/docs/microdata/bepuf-2020/index.html
      bc = case_when(
        # Not yet receiving any benefits (no own, spousal, or survivor benefit)
        wrk_ben <= 0 & spouse_ben_adj <= 0 & survivor_ben <= 0 ~ NA_character_,

        # Spouse-only benefit classes (no own worker benefit, only spousal benefit)
        # BD = Spouse of Disabled Worker (spouse is currently disabled, before their NRA)
        # BR = Spouse of Retired Worker (spouse is retired or has reached their NRA)
        # Note: This occurs when worker has no earnings but receives spousal benefits from spouse's record
        wrk_ben <= 0 & survivor_ben <= 0 & spouse_ben_adj > 0 & s_is_currently_disabled ~ "BD",
        wrk_ben <= 0 & survivor_ben <= 0 & spouse_ben_adj > 0 ~ "BR",

        # Survivor-only benefit classes (no own worker benefit)
        # F = Disabled Widow(er) only
        # D = Standard Widow(er) only
        wrk_ben <= 0 & survivor_ben > 0 & is_disabled_widow ~ "F",
        wrk_ben <= 0 & survivor_ben > 0 ~ "D",

        # Disabled worker benefit classes (AD*) - before NRA
        # ADF = Disabled Worker + Disabled Widow(er) benefit
        # ADD = Disabled Worker + Standard Widow(er) benefit
        # ADB = Disabled Worker + Spouse benefit
        # AD  = Disabled Worker only
        is_currently_disabled & survivor_ben > 0 & survivor_ben > spouse_ben_adj & is_disabled_widow ~ "ADF",
        is_currently_disabled & survivor_ben > 0 & survivor_ben > spouse_ben_adj ~ "ADD",
        is_currently_disabled & spouse_ben_adj > 0 ~ "ADB",
        is_currently_disabled ~ "AD",

        # Retired worker benefit classes (AR*) - at/after NRA or originally retired
        # ARF = Retired Worker + Disabled Widow(er) benefit
        # ARD = Retired Worker + Standard Widow(er) benefit
        # ARB = Retired Worker + Spouse benefit
        # AR  = Retired Worker only
        survivor_ben > 0 & survivor_ben > spouse_ben_adj & is_disabled_widow ~ "ARF",
        survivor_ben > 0 & survivor_ben > spouse_ben_adj ~ "ARD",
        spouse_ben_adj > 0 ~ "ARB",
        TRUE ~ "AR"
      )
    )

  if(debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, ben, bc),
                                   by=c("id","age"))
  } else {
    # Join ben and bc, then select output columns
    worker <- worker %>%
      left_join(dataset %>% select(id, age, ben, bc), by=c("id","age"))

    # Select core output columns, including sex, spouse_spec, and bc if they exist
    output_cols <- c("id", "sex", "year", "age", "spouse_spec", "earnings", "ben", "bc")
    available_cols <- intersect(output_cols, names(worker))

    worker <- worker %>% select(all_of(available_cols))
  }

  return(worker)

}
