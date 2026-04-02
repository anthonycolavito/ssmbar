# =============================================================================
# BENEFIT CALCULATIONS - Baseline Functions and Shared Helpers
# =============================================================================
#
# This file contains:
#   1. Shared helper functions (used by both baseline and reform pipelines)
#   2. Baseline (current law) benefit calculation functions
#
# Baseline functions in this file:
#   - aime(): Average Indexed Monthly Earnings calculation
#   - pia(): Primary Insurance Amount calculation
#   - cola(): Cost-of-Living Adjustment calculation
#
# Shared functions in this file:
#   - join_all_assumptions(): Joins assumption columns to worker data
#   - floor_dime(): Rounds down to nearest $0.10
#   - rf_and_drc(): Actuarial adjustment calculation
#   - worker_benefit(): Applies actuarial adjustments to PIA
#   - family_maximum(): Calculates and applies family maximum
#   - final_benefit(): Combines worker + auxiliary benefits
#
# For REFORM-CAPABLE versions that support policy analysis, see:
#   - aime_reform(), pia_reform(), cola_reform() in reform_functions.R
#   - ret_reform() in reform_functions.R
#   - widow_benefit_reform() in reform_functions.R
#   - basic_minimum_benefit() in reform_functions.R
#
# Calculation pipeline:
#   Baseline: earnings -> aime() -> pia() -> cola() -> worker_benefit()
#             -> spousal_pia() -> spouse_benefit() -> child_pia() -> child_benefit()
#             -> family_maximum() -> widow_pia() -> widow_benefit() -> ret()
#             -> final_benefit()
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
  # - pia: bp1, bp2, fact1, fact2, fact3, elig_age_retired, yoc_threshold, special_min_rate, min_yoc_for_special_min
  # - cola: cola (year-by-year COLA percentage)
  # - worker_benefit: nra, rf1, rf2, drc, max_drc_age
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - child_pia: child_pia_share
  # - family_maximum: fm_bp1, fm_bp2, fm_bp3
  # - ret: ret1, ret_phaseout_rate

  cols_needed <- c("year", "awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min",
                   "cola", "nra", "rf1", "rf2", "drc", "max_drc_age",
                   "s_pia_share", "s_rf1", "s_rf2", "child_pia_share",
                   "fm_bp1", "fm_bp2", "fm_bp3", "ret1", "ret_phaseout_rate")

  # Include reform columns when present in assumptions
  if ("ret_enabled" %in% names(assumptions)) {
    cols_needed <- c(cols_needed, "ret_enabled")
  }

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
# Per 42 USC 415(a)(1)(A) and 415(i)(2)(A)(ii), PIA and COLA-adjusted amounts
# are rounded to the next lower multiple of $0.10 (dime). This helper function
# applies consistent dime rounding across all benefit calculations.

#' Floor to Next Lower Dime
#'
#' Rounds a dollar amount down to the next lower multiple of $0.10, per
#' 42 USC 415(a)(1)(A) (PIA rounding) and 42 USC 415(i)(2)(A)(ii) (COLA rounding).
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
#' @param max_drc_age Numeric value for maximum age at which DRCs accrue (default 70).
#'
#' @return act_factor numeric value used for adjusting a worker's PIA to compute their monthly benefit
#'
#' @export
rf_and_drc <- function(claim_age, nra, rf1, rf2, drc, max_drc_age = 70) {
  # Benefit reduction factors are described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  #
  # rf1: Reduction for first 36 months early (5/9 of 1% per month for retired worker beneficiaries)
  # rf2: Reduction for months beyond 36 early (5/12 of 1% per month for retired worker beneficiaries)
  # drc: Delayed retirement credit per month (varies by birth year, max 8%/yr)
  # max_drc_age: Age at which DRCs stop accruing (70 under current law)

  dist_from_nra <- (claim_age - nra) * 12 # Distance from Normal Retirement Age in months
  drc_max_months <- (max_drc_age - nra) * 12 # Maximum months of DRC per 42 USC 402(w)

  # Calculate reduction factors
  rf_amt <- pmin(0, # If claiming at or above NRA, no RFs
                 pmax(-36, dist_from_nra)*rf1 + # If claiming less than three years before NRA
                 pmin(0, pmin(-36, dist_from_nra) + 36)*rf2 # If claiming more than three years before NRA
                 )

  # Calculate DRCs (capped at drc_max_months past NRA, i.e., max_drc_age)
  drc_amt <- pmax(0, # If claiming at or below NRA, no DRCs
                  pmin(drc_max_months, dist_from_nra) * drc # If claiming above NRA. DRCs capped at max_drc_age
                  )

  act_factor <- 1 + rf_amt + drc_amt # Final actuarial factor for adjusting benefits

  return(act_factor)

}


# =============================================================================
# SECTION 2: Baseline Benefit Calculation Functions
# =============================================================================
# These functions implement current statutory rules WITHOUT any reform parameters.

# -----------------------------------------------------------------------------
# 2.1 AIME Calculation (Baseline)
# -----------------------------------------------------------------------------

#' AIME Calculation (Baseline - Current Law)
#'
#' Function that computes a worker's Average Indexed Monthly Earnings by age
#' using current statutory rules without any reform parameters.
#'
#' This is the baseline version. For reform-capable AIME calculation, see
#' \code{\link{aime_reform}}.
#'
#' @param worker Dataframe with a worker's earnings by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's AIME by year and age
#'
#' @seealso \code{\link{aime_reform}} for the reform-capable version
#'
#' @export
aime <- function(worker, assumptions, debugg = FALSE) {
  # How earnings are indexed is described in Section 700.3 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  # AIME Computation is described in Section 701 of the Social Security Handbook
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
  #
  # AIME equals the average monthly earnings of the highest earning years
  # in the computation period (typically 35 years for retired workers).
  # For January 2 claims, AIME at age X uses earnings through age X-1.
  #
  # Optimization: Uses partial sort which is O(n) vs full sort O(n log n).
  # Pre-computes eligibility flags to reduce redundant checks.
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      n <- nrow(.x)
      aime_vals <- numeric(n)
      indexed_earnings <- .x$indexed_earn
      qc_required_val <- .x$qc_required[1]
      comp_period <- .x$comp_period

      # Pre-compute eligibility: needs QCs and at/past eligibility age
      is_eligible <- (!is.na(.x$qc_tot) & .x$qc_tot >= qc_required_val &
                      !is.na(.x$age) & !is.na(.x$elig_age) & .x$age >= .x$elig_age)

      # Find first eligible index to skip early years
      first_eligible <- which(is_eligible)[1]

      if (!is.na(first_eligible)) {
        # Only iterate from first eligible year onward
        for (i in first_eligible:n) {
          if (is_eligible[i]) {
            # Earnings through age-1 (i-1 rows) -- When a worker claims benefits, only the earnings through the last year are available to SSA, so current year earnings are not included in the calculation
            available_years <- i - 1

            if (available_years > 0) {
              years_to_use <- min(available_years, comp_period[i])
              earnings_subset <- indexed_earnings[1:available_years]

              # Compute sum of top years_to_use earnings
              # Partial sort is O(n) for finding k largest elements
              if (available_years > years_to_use) {
                top_earnings_sum <- sum(-sort(-earnings_subset, partial = 1:years_to_use)[1:years_to_use])
              } else {
                top_earnings_sum <- sum(earnings_subset)
              }

              # AIME rounded down to the next lowest dollar
              aime_vals[i] <- floor(top_earnings_sum / (comp_period[i] * 12))
            }
          }
        }
      }

      .x$aime <- aime_vals
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
# 2.2 PIA Calculation (Baseline)
# -----------------------------------------------------------------------------

#' PIA Calculation (Baseline - Current Law)
#'
#' Function that computes a worker's Primary Insurance Amount by age using
#' the standard 90/32/15 bend point formula per 42 USC 415(a)
#' Compares the regular PIA with the special minimum PIA per 42 USC 415(a)(1)(C)
#' and returns the higher of the two.
#'
#' This is the baseline version. For reform-capable PIA calculation (with
#' options for PIA multipliers, flat benefits, 4-bracket formulas, etc.),
#' see \code{\link{pia_reform}}.
#'
#' @param worker Dataframe with a worker's earnings and AIME by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker dataframe with a worker's retired worker PIA by age
#'
#' @seealso \code{\link{pia_reform}} for the reform-capable version
#'
#' @export
pia <- function(worker, assumptions, debugg = FALSE) {
  # PIA calculation is described in Section 706 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0706.html
  #
  # Per 42 USC 415(a)(1), PIA is the HIGHER of:
  # - Regular PIA: 90/32/15 bend point formula per 42 USC 415(a)(1)(A)
  # - Special minimum PIA: $11.50 x (years_of_coverage - 10), COLA-adjusted, per 42 USC 415(a)(1)(C)
  #
  # Bend points and replacement factors are determined at the worker's eligibility age
  # (elig_age_retired from assumptions, currently 62 for retirement benefits)


  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  # Calculate years of coverage for special minimum PIA
  # This counts years where earnings >= yoc_threshold
  dataset <- dataset %>% years_of_coverage(debugg)

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      # Use worker's elig_age for bend points (disability age for disabled workers, 62 for retired workers)
      bp1_elig = bp1[which(age == first(elig_age))], # First PIA bend point at worker's eligibility age
      bp2_elig = bp2[which(age == first(elig_age))], # Second PIA bend point at worker's eligibility age
      fact1_elig = fact1[which(age == first(elig_age))], # First replacement factor (90%)
      fact2_elig = fact2[which(age == first(elig_age))], # Second replacement factor (32%)
      fact3_elig = fact3[which(age == first(elig_age))], # Third replacement factor (15%)

      # Get special minimum rate at eligibility age (COLA-adjusted $11.50 base)
      special_min_rate_elig = special_min_rate[which(age == first(elig_age))],
      min_yoc_elig = min_yoc_for_special_min[which(age == first(elig_age))],

      # Regular PIA per 42 USC 415(a)(1)(A): 90/32/15 bend point formula
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
      regular_pia = case_when(
        age >= elig_age ~ floor_dime(
                          pmin(aime, bp1_elig)*fact1_elig + #90% Replacement of AIME below first bend point
                          pmax(0, pmin(aime, bp2_elig) - bp1_elig)*fact2_elig + #32% Replacement of AIME between second and third bend point
                          pmax(0, aime - bp2_elig)*fact3_elig #15% Replacement of AIME above third bend point
                          ),
        TRUE ~ 0),

      # Special minimum PIA per 42 USC 415(a)(1)(C)(i):
      # PIA = special_min_rate x (years_of_coverage - 10)
      # Only applies if years_of_coverage >= min_yoc_for_special_min (11)
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
      special_min_pia = case_when(
        age >= elig_age & years_of_coverage >= min_yoc_elig ~
          floor_dime(special_min_rate_elig * (years_of_coverage - 10)),
        TRUE ~ 0),

      # Final PIA is the higher of regular or special minimum per 42 USC 415(a)(1)
      basic_pia = pmax(regular_pia, special_min_pia)
    ) %>% select(-bp1, -bp2, -fact1, -fact2, -fact3, -elig_age_retired,
                 -yoc_threshold, -special_min_rate, -min_yoc_for_special_min) %>% ungroup()

  if (debugg) {
    cols_to_add <- c("basic_pia", "regular_pia", "special_min_pia", "years_of_coverage",
                     "bp1_elig", "bp2_elig", "fact1_elig", "fact2_elig", "fact3_elig",
                     "special_min_rate_elig")
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
# 2.3 COLA Calculation (Baseline)
# -----------------------------------------------------------------------------

#' COLA Calculation (Baseline - Current Law)
#'
#' Function that computes a worker's COLA-adjusted PIA using current statutory
#' rules without any reform parameters (such as COLA caps).
#'
#' This is the baseline version. For reform-capable COLA calculation (with
#' options for COLA caps), see \code{\link{cola_reform}}.
#'
#' @param worker Dataframe with a worker's unadjusted PIA -- both retired worker and spousal -- by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's COLA-adjusted retired worker and spousal PIA by age
#'
#' @seealso \code{\link{cola_reform}} for the reform-capable version
#'
#' @importFrom dplyr lag
#' @export
cola <- function (worker, assumptions, debugg = FALSE) {
  # COLA adjustments apply year-by-year starting at eligibility age
  # SSA Handbook Section 719: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0719.html
  #
  # How COLA works:
  # - At eligibility age (62): cola_basic_pia = basic_pia (no COLA applied yet)
  # - At age 63: cola_basic_pia = basic_pia x (1 + COLA from eligibility year)
  # - At age 64: cola_basic_pia = cola_pia_63 x (1 + COLA from age 63 year)
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

      # Find index of eligibility age for COLA replay
      elig_idx <- which(ages == elig_age_val)[1]

      for (i in seq_len(n)) {
        if (ages[i] < elig_age_val) {
          cola_basic_pia_vals[i] <- 0
        } else if (ages[i] == elig_age_val) {
          # At eligibility age: no COLA yet, use basic_pia
          cola_basic_pia_vals[i] <- basic_pia[i]
        } else {
          # COLA the previous year's COLA'd PIA forward
          # Per 42 USC 415(i)(2)(A)(ii): round to next lower $0.10
          cola_forward <- floor_dime(cola_basic_pia_vals[i-1] * cola_factor[i])

          # Automatic recomputation (SSA Handbook Section 715):
          # If AIME increased from continued earnings, basic_pia[i] may be
          # higher than the original. Replay all COLAs from eligibility year
          # on the new basic_pia and take the max.
          if (basic_pia[i] > basic_pia[elig_idx]) {
            recomp_pia <- basic_pia[i]
            for (j in (elig_idx + 1):i) {
              recomp_pia <- floor_dime(recomp_pia * cola_factor[j])
            }
            cola_basic_pia_vals[i] <- max(cola_forward, recomp_pia)
          } else {
            cola_basic_pia_vals[i] <- cola_forward
          }
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


# =============================================================================
# SECTION 3: Shared Benefit Calculation Functions
# =============================================================================
# These functions are used by both baseline and reform pipelines.
# They do NOT contain any reform logic.

# -----------------------------------------------------------------------------
# 3.1 Worker Benefit Calculation
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
  #Benefit reduction factors are described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  #Function currently can only handle retired beneficiaries.

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("rf1", "rf2", "drc", "nra", "s_rf1", "s_rf2", "elig_age_retired")
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
# 3.2 Family Maximum Calculation
# -----------------------------------------------------------------------------

#' Calculate Family Maximum
#'
#' Calculates the family maximum benefit per 42 USC 403(a)(1) and applies
#' proportional reduction to auxiliary benefits (spouse, child) when the total
#' exceeds the family maximum.
#'
#' Per 42 USC 403(a)(1), the family maximum is calculated using a bend point formula:
#'   150% of PIA up to fm_bp1 +
#'   272% of PIA between fm_bp1 and fm_bp2 +
#'   134% of PIA between fm_bp2 and fm_bp3 +
#'   175% of PIA above fm_bp3
#'
#' For disabled workers, 42 USC 403(a)(6) provides an alternative:
#'   family_max = min(85% of AIME × 12, 150% of PIA × 12) / 12
#'
#' The family maximum limits total auxiliary benefits. If total auxiliary benefits
#' exceed (family_max - worker's own benefit), auxiliary benefits are proportionally
#' reduced. The worker's own benefit is NEVER reduced.
#'
#' @param worker Dataframe with worker's benefits by year and age. Must include:
#'   cola_basic_pia, wrk_ben, spouse_ben, child1_ben, child2_ben, child3_ben, aime, elig_age
#' @param assumptions Dataframe with the Social Security Trustees assumptions.
#'   Must include fm_bp1, fm_bp2, fm_bp3 columns.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with spouse_ben_fm, child1_ben_fm, child2_ben_fm, child3_ben_fm columns
#'   containing family-maximum-adjusted auxiliary benefits
#'
#' @export
family_maximum <- function(worker, assumptions, spouse_data = NULL, factors = NULL, debugg = FALSE) {
  # Family maximum is described in 42 USC 403(a)
  # https://www.ssa.gov/OP_Home/ssact/title02/0203.htm
  #
  # The family maximum limits total benefits payable on one worker's record.
  # Worker's own benefit is never reduced; only auxiliary benefits are reduced.
  #
  # Regular formula (42 USC 403(a)(1)):
  # FM = 150% × min(PIA, fm_bp1) +
  #      272% × max(0, min(PIA, fm_bp2) - fm_bp1) +
  #      134% × max(0, min(PIA, fm_bp3) - fm_bp2) +
  #      175% × max(0, PIA - fm_bp3)
  #
  # Disability alternative (42 USC 403(a)(6)):
  # FM = min(85% × AIME, 150% × PIA)

  # Ensure child benefit columns exist
  if (!"child1_ben" %in% names(worker)) {
    worker$child1_ben <- 0
    worker$child2_ben <- 0
    worker$child3_ben <- 0
  }

  # Ensure spouse_ben column exists
  if (!"spouse_ben" %in% names(worker)) {
    worker$spouse_ben <- 0
  }

  # Get family max bend points and COLA from assumptions
  cols_needed <- c("fm_bp1", "fm_bp2", "fm_bp3", "elig_age_retired", "cola")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate family maximum for each worker
  dataset <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get bend points at eligibility age
      fm_bp1_elig = fm_bp1[which(age == elig_age)][1],
      fm_bp2_elig = fm_bp2[which(age == elig_age)][1],
      fm_bp3_elig = fm_bp3[which(age == elig_age)][1],
      elig_age_ret = elig_age_retired[1],

      # Determine if worker is disabled (eligibility age < 62)
      is_disabled_worker = elig_age < elig_age_ret,

      # Calculate regular family maximum (42 USC 403(a)(1))
      # Uses worker's PIA at eligibility age (basic_pia or cola_basic_pia at elig_age)
      # Family max is calculated once at eligibility and then COLA'd
      pia_at_elig = basic_pia[which(age == elig_age)][1],
      regular_fm = floor_dime(
        1.50 * pmin(pia_at_elig, fm_bp1_elig) +
        2.72 * pmax(0, pmin(pia_at_elig, fm_bp2_elig) - fm_bp1_elig) +
        1.34 * pmax(0, pmin(pia_at_elig, fm_bp3_elig) - fm_bp2_elig) +
        1.75 * pmax(0, pia_at_elig - fm_bp3_elig)
      ),

      # Calculate disability family maximum alternative (42 USC 403(a)(6)(A))
      # For disabled workers: min(max(85% of AIME, 100% of PIA), 150% of PIA)
      # The inner max() ensures disability FM is never less than the PIA itself.
      aime_at_elig = aime[which(age == elig_age)][1],
      disability_fm = floor_dime(pmin(pmax(0.85 * aime_at_elig, pia_at_elig), 1.50 * pia_at_elig)),

      # Use disability formula if disabled worker and it's lower than regular formula
      # (disability formula is always used for disabled workers, even if lower)
      fm_at_elig = if_else(is_disabled_worker, disability_fm, regular_fm)
    ) %>%
    # Apply COLA to family maximum year by year (same as PIA COLA)
    group_modify(~ {
      n <- nrow(.x)
      fm_cola_vals <- numeric(n)
      fm_at_elig <- .x$fm_at_elig[1]
      elig_age_val <- .x$elig_age[1]
      ages <- .x$age
      cola_pct <- .x$cola  # COLA percentages from assumptions

      for (i in seq_len(n)) {
        if (ages[i] < elig_age_val) {
          fm_cola_vals[i] <- 0
        } else if (ages[i] == elig_age_val) {
          fm_cola_vals[i] <- fm_at_elig
        } else {
          # Calculate COLA factor from previous year's COLA percentage
          # The COLA announced in year Y-1 is applied to benefits in year Y
          prev_cola <- if (i > 1 && !is.na(cola_pct[i-1])) cola_pct[i-1] else 0
          cola_factor_i <- 1 + pmax(prev_cola, 0) / 100
          # Apply COLA and floor to nearest dime
          fm_cola_vals[i] <- floor_dime(fm_cola_vals[i-1] * cola_factor_i)
        }
      }

      .x$family_max <- fm_cola_vals
      .x
    }) %>%
    ungroup()

  # Compute dependent spouse benefit (benefit the SPOUSE receives from THIS worker's record)
  # Per 42 USC 403(a), this is an auxiliary on this worker's record and subject to family max
  if (!"spouse_spec" %in% names(dataset)) {
    dataset$spouse_dep_ben <- 0
  } else {
    dataset <- dataset %>%
      group_by(id) %>%
      group_modify(~ {
        spec <- .x$spouse_spec[1]
        if (!is.na(spec) && !is.null(spouse_data) && !is.null(spouse_data[[spec]])) {
          .x$spouse_dep_ben <- calculate_spouse_dep_benefit(.x, spouse_data[[spec]], assumptions)
        } else {
          .x$spouse_dep_ben <- 0
        }
        .x
      }) %>%
      ungroup()
  }

  # Apply family maximum reduction to auxiliary benefits
  dataset <- dataset %>%
    mutate(
      # Total auxiliary benefits on THIS worker's record (before family max reduction)
      # Per 42 USC 403(a), the family max applies to all auxiliaries on ONE worker's record:
      #   - spouse_dep_ben: benefit the SPOUSE receives from this worker's record (section 402(b)/(c))
      #   - child benefits: benefits children receive from this worker's record (section 402(d))
      # Does NOT include spouse_ben — that is the benefit THIS worker receives from the
      # SPOUSE's record, which is subject to the SPOUSE's family max, not this worker's.
      total_aux_ben = pmax(spouse_dep_ben, 0, na.rm = TRUE) +
                      pmax(child1_ben, 0, na.rm = TRUE) +
                      pmax(child2_ben, 0, na.rm = TRUE) +
                      pmax(child3_ben, 0, na.rm = TRUE),

      # Available amount for auxiliary benefits = family_max - worker's benefit
      # Worker's benefit is based on cola_basic_pia * act_factor, floored
      # We use wrk_ben which is already calculated
      aux_available = pmax(family_max - pmax(wrk_ben, 0, na.rm = TRUE), 0, na.rm = TRUE),

      # Reduction factor: if total_aux > aux_available, reduce proportionally
      fm_reduction_factor = if_else(
        total_aux_ben > aux_available & total_aux_ben > 0,
        aux_available / total_aux_ben,
        1.0  # No reduction needed
      ),

      # Apply proportional reduction to ALL auxiliaries on this worker's record.
      # spouse_ben is passed through unchanged (it's from the spouse's record).
      spouse_ben_fm = pmax(spouse_ben, 0, na.rm = TRUE),
      spouse_dep_ben_fm = floor(pmax(spouse_dep_ben, 0, na.rm = TRUE) * fm_reduction_factor),
      child1_ben_fm = floor(pmax(child1_ben, 0, na.rm = TRUE) * fm_reduction_factor),
      child2_ben_fm = floor(pmax(child2_ben, 0, na.rm = TRUE) * fm_reduction_factor),
      child3_ben_fm = floor(pmax(child3_ben, 0, na.rm = TRUE) * fm_reduction_factor)
    )

  # Add columns to worker
  if (debugg) {
    cols_to_add <- c("family_max", "regular_fm", "disability_fm", "fm_at_elig",
                     "total_aux_ben", "aux_available", "fm_reduction_factor",
                     "spouse_ben_fm", "spouse_dep_ben", "spouse_dep_ben_fm",
                     "child1_ben_fm", "child2_ben_fm", "child3_ben_fm")
  } else {
    cols_to_add <- c("family_max", "spouse_ben_fm", "spouse_dep_ben_fm",
                     "child1_ben_fm", "child2_ben_fm", "child3_ben_fm")
  }

  cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
  if (length(cols_new) > 0) {
    worker <- worker %>%
      left_join(dataset %>% select(id, year, all_of(cols_new)), by = c("id", "year"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 3.3 Final Benefit Calculation
# -----------------------------------------------------------------------------

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
  # - spouse_ben_fm: Family-max-adjusted spousal benefit
  # - child1_ben_fm, child2_ben_fm, child3_ben_fm: Family-max-adjusted child benefits
  # - survivor_ben: EXCESS survivor benefit (survivor PIA - own PIA, with actuarial adjustment)
  #
  # Spousal and child benefits apply while worker is alive (auxiliary benefits).
  # Survivor benefits apply after worker's spouse dies.
  # Worker receives: wrk_ben + spouse_ben_fm + child_bens_fm + max(0, survivor_ben)

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

  # Handle case where s_death_age column doesn't exist (backwards compatibility)
  # Default to NA (no spouse death age for proration)
  if (!"s_death_age" %in% names(worker)) {
    worker$s_death_age <- NA_real_
  }

  # Handle case where family-max-adjusted benefit columns don't exist (backwards compatibility)
  # If spouse_ben_fm doesn't exist but spouse_ben does, use spouse_ben
  if (!"spouse_ben_fm" %in% names(worker)) {
    if ("spouse_ben" %in% names(worker)) {
      worker$spouse_ben_fm <- worker$spouse_ben
    } else {
      worker$spouse_ben_fm <- 0
    }
  }

  # Handle missing child benefit columns
  if (!"child1_ben_fm" %in% names(worker)) {
    worker$child1_ben_fm <- 0
  }
  if (!"child2_ben_fm" %in% names(worker)) {
    worker$child2_ben_fm <- 0
  }
  if (!"child3_ben_fm" %in% names(worker)) {
    worker$child3_ben_fm <- 0
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
      # Fraction of the death year the spouse is alive (for mid-year death proration)
      # s_death_age is fractional (e.g. 86.45), so the fraction alive = s_death_age - floor(s_death_age)
      # This is used only in the death year to prorate between spousal and survivor benefits
      spouse_frac_alive = if_else(
        !is.na(s_death_age),
        s_death_age - floor(s_death_age),
        NA_real_
      ),
      # Is this the year the spouse dies? (worker's age equals worker_age_at_spouse_death)
      is_spouse_death_year = !is.na(worker_age_at_spouse_death) & age == worker_age_at_spouse_death,

      # Spousal benefit adjustment:
      # - Before death year: full spousal benefit
      # - Death year: prorated spousal benefit (fraction of year spouse was alive)
      # - After death year: zero (survivor benefit takes over)
      spouse_ben_adj = case_when(
        is_spouse_death_year ~ spouse_ben_fm * spouse_frac_alive,
        !is.na(worker_age_at_spouse_death) & age > worker_age_at_spouse_death ~ 0,
        TRUE ~ spouse_ben_fm
      ),
      # Survivor benefit adjustment for death year proration:
      # - Death year: prorated survivor benefit (fraction of year after spouse died)
      # - After death year: full survivor benefit
      # - Before death year: zero (spouse is alive)
      survivor_ben_adj = case_when(
        is_spouse_death_year ~ survivor_ben * (1 - spouse_frac_alive),
        !is.na(worker_age_at_spouse_death) & age > worker_age_at_spouse_death ~ survivor_ben,
        TRUE ~ 0
      ),
      # Total benefit = worker's own + max(spousal, survivor) + child benefits
      # In the death year, spousal and survivor are already prorated, so we sum them
      # rather than taking max — they cover different parts of the year.
      # In non-death years, one of the two is zero, so the sum equals the max behavior.
      # Child benefits only paid while worker is alive (age < death_age)
      total_child_ben = if_else(
        age < death_age,
        pmax(child1_ben_fm, 0, na.rm = TRUE) +
          pmax(child2_ben_fm, 0, na.rm = TRUE) +
          pmax(child3_ben_fm, 0, na.rm = TRUE),
        0
      ),
      ben = if_else(
        is_spouse_death_year,
        # Death year: worker benefit + prorated spousal + prorated survivor
        pmax(wrk_ben, 0, na.rm = TRUE) +
          pmax(spouse_ben_adj, 0, na.rm = TRUE) +
          pmax(survivor_ben_adj, 0, na.rm = TRUE) +
          total_child_ben,
        # Non-death years: original logic (max of spousal vs survivor)
        pmax(wrk_ben, 0, na.rm = TRUE) +
          pmax(spouse_ben_adj, survivor_ben_adj, 0, na.rm = TRUE) +
          total_child_ben
      ),

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
      # Uses survivor_ben_adj (which accounts for death-year proration) rather than raw survivor_ben
      # In the death year, both spouse_ben_adj and survivor_ben_adj are positive (prorated),
      # so BC reflects the post-death classification (survivor codes take priority)
      bc = case_when(
        # Not yet receiving any benefits (no own, spousal, or survivor benefit)
        wrk_ben <= 0 & spouse_ben_adj <= 0 & survivor_ben_adj <= 0 ~ NA_character_,

        # Spouse-only benefit classes (no own worker benefit, only spousal benefit)
        # BD = Spouse of Disabled Worker (spouse is currently disabled, before their NRA)
        # BR = Spouse of Retired Worker (spouse is retired or has reached their NRA)
        # Note: This occurs when worker has no earnings but receives spousal benefits from spouse's record
        wrk_ben <= 0 & survivor_ben_adj <= 0 & spouse_ben_adj > 0 & s_is_currently_disabled ~ "BD",
        wrk_ben <= 0 & survivor_ben_adj <= 0 & spouse_ben_adj > 0 ~ "BR",

        # Survivor-only benefit classes (no own worker benefit)
        # F = Disabled Widow(er) only
        # D = Standard Widow(er) only
        wrk_ben <= 0 & survivor_ben_adj > 0 & is_disabled_widow ~ "F",
        wrk_ben <= 0 & survivor_ben_adj > 0 ~ "D",

        # Disabled worker benefit classes (AD*) - before NRA
        # ADF = Disabled Worker + Disabled Widow(er) benefit
        # ADD = Disabled Worker + Standard Widow(er) benefit
        # ADB = Disabled Worker + Spouse benefit
        # AD  = Disabled Worker only
        is_currently_disabled & survivor_ben_adj > 0 & survivor_ben_adj > spouse_ben_adj & is_disabled_widow ~ "ADF",
        is_currently_disabled & survivor_ben_adj > 0 & survivor_ben_adj > spouse_ben_adj ~ "ADD",
        is_currently_disabled & spouse_ben_adj > 0 ~ "ADB",
        is_currently_disabled ~ "AD",

        # Retired worker benefit classes (AR*) - at/after NRA or originally retired
        # ARF = Retired Worker + Disabled Widow(er) benefit
        # ARD = Retired Worker + Standard Widow(er) benefit
        # ARB = Retired Worker + Spouse benefit
        # AR  = Retired Worker only
        survivor_ben_adj > 0 & survivor_ben_adj > spouse_ben_adj & is_disabled_widow ~ "ARF",
        survivor_ben_adj > 0 & survivor_ben_adj > spouse_ben_adj ~ "ARD",
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

    # Select core output columns, including sex, spouse_spec, child_spec, and bc if they exist
    output_cols <- c("id", "sex", "year", "age", "spouse_spec",
                     "child1_spec", "child2_spec", "child3_spec",
                     "earnings", "ben", "bc")
    available_cols <- intersect(output_cols, names(worker))

    worker <- worker %>% select(all_of(available_cols))
  }

  return(worker)

}
