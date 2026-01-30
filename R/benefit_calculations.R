
# =============================================================================
# BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains the core benefit calculation functions for the ssmbar package.
# Functions are organized in the order they are called in the benefit calculation
# pipeline (see calculate_benefits() in calculate_benefits.R). Spousal and survivor
# benefit functions are in separate files (spousal.R, survivor.R):
#
#   earnings -> aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#            -> spouse_benefit() -> child_pia() -> child_benefit() -> family_maximum()
#            -> widow_pia() -> widow_benefit() -> ret() -> final_benefit()
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
  # - worker_benefit: nra, rf1, rf2, drc, drc_max_months
  # - spousal_pia: s_pia_share
  # - spouse_benefit: s_rf1, s_rf2
  # - child_pia: child_pia_share
  # - family_maximum: fm_bp1, fm_bp2, fm_bp3
  # - ret: ret1, ret_phaseout_rate

  cols_needed <- c("year", "awi", "taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset",
                   "bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min",
                   "cola", "nra", "rf1", "rf2", "drc", "drc_max_months",
                   "s_pia_share", "s_rf1", "s_rf2", "child_pia_share",
                   "fm_bp1", "fm_bp2", "fm_bp3", "ret1", "ret_phaseout_rate")

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
  # - taxmax_benefit: For Reform #14, separate benefit calculation cap
  # - child_care_credit_active: For Reform #29, child care credit
  cols_needed <- c("taxmax", "qc_rec", "qc_required", "max_qc_per_year",
                   "max_dropout_years", "min_comp_period", "index_age_offset", "awi",
                   "taxmax_benefit", "child_care_credit_active", "max_child_care_years",
                   "child_care_earnings_rate")
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
  # Reform #29: Child Care Credit - Apply earnings floor for years with child under 6
  # Helper function to parse child birth year from child_spec (format: "birthyr-disabled")
  parse_child_birth_year <- function(spec) {
    if (is.na(spec) || spec == "") return(NA_integer_)
    parts <- strsplit(as.character(spec), "-")[[1]]
    if (length(parts) >= 1) {
      return(as.integer(parts[1]))
    }
    NA_integer_
  }

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      index_age = elig_age - first(index_age_offset), # Age for wage indexing (e.g., 62 - 2 = 60)
      awi_index_age = awi[which(age == index_age)], # Retrieve AWI at indexing age
      index_factor = pmax(awi_index_age / awi, 1), # Calculate indexing factor. Earnings past indexing age are taken at face value.
      # Reform #14: Use taxmax_benefit for AIME calculation if available
      # This allows taxmax_tax to be unlimited while keeping benefits capped at old taxmax
      benefit_cap = if_else(is.na(taxmax_benefit), taxmax, taxmax_benefit),

      # Reform #29: Child Care Credit
      # If enabled, credit earnings up to child_care_earnings_rate * AWI for years with child under 6
      child_care_floor = if_else(
        !is.na(child_care_credit_active) & child_care_credit_active == TRUE,
        child_care_earnings_rate * awi,
        0
      )
    ) %>%
    # Apply child care credit if active and worker has children
    group_modify(~ {
      n <- nrow(.x)
      cc_active <- .x$child_care_credit_active[1]
      cc_active <- !is.na(cc_active) && cc_active == TRUE

      # Parse child birth years from child_spec columns
      child1_by <- if ("child1_spec" %in% names(.x)) parse_child_birth_year(.x$child1_spec[1]) else NA_integer_
      child2_by <- if ("child2_spec" %in% names(.x)) parse_child_birth_year(.x$child2_spec[1]) else NA_integer_
      child3_by <- if ("child3_spec" %in% names(.x)) parse_child_birth_year(.x$child3_spec[1]) else NA_integer_

      has_children <- !is.na(child1_by) | !is.na(child2_by) | !is.na(child3_by)

      if (cc_active && has_children) {
        max_cc_years <- .x$max_child_care_years[1]
        max_cc_years <- if (is.na(max_cc_years)) 5 else max_cc_years

        # Determine which years had a child under 6
        years_with_child_under_6 <- logical(n)
        for (i in seq_len(n)) {
          yr <- .x$year[i]
          has_young_child <- FALSE
          if (!is.na(child1_by) && (yr - child1_by) >= 0 && (yr - child1_by) < 6) has_young_child <- TRUE
          if (!is.na(child2_by) && (yr - child2_by) >= 0 && (yr - child2_by) < 6) has_young_child <- TRUE
          if (!is.na(child3_by) && (yr - child3_by) >= 0 && (yr - child3_by) < 6) has_young_child <- TRUE
          years_with_child_under_6[i] <- has_young_child
        }

        # Calculate potential credit gain for each year
        potential_gain <- pmax(.x$child_care_floor - .x$earnings, 0) * years_with_child_under_6

        # Select best max_cc_years years (highest gain)
        if (sum(potential_gain > 0) > 0) {
          gain_order <- order(potential_gain, decreasing = TRUE)
          credit_years <- gain_order[1:min(max_cc_years, sum(potential_gain > 0))]
          credit_mask <- seq_len(n) %in% credit_years

          # Apply credit: use max(earnings, floor) for selected years
          .x$earnings_with_cc <- if_else(
            credit_mask,
            pmax(.x$earnings, .x$child_care_floor),
            .x$earnings
          )
        } else {
          .x$earnings_with_cc <- .x$earnings
        }
      } else {
        .x$earnings_with_cc <- .x$earnings
      }
      .x
    }) %>%
    mutate(
      # Cap earnings and index
      capped_earn = pmin(earnings_with_cc, benefit_cap), # Cap earnings amounts at the benefit cap
      indexed_earn = capped_earn * index_factor # Indexed capped earnings amounts
    ) %>%
    ungroup()

  # AIME Calculation
  # SSA Handbook Section 701: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html
  #
  # AIME equals the average monthly earnings of the highest earning years
  # in the computation period (typically 35 years for retired workers).
  # For January 1 claims, AIME at age X uses earnings through age X-1.
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
        # Only iterate from first eligible year onwards
        for (i in first_eligible:n) {
          if (is_eligible[i]) {
            # Earnings through age-1 (i-1 rows)
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
# 2.2 PIA Calculation
# -----------------------------------------------------------------------------

#' Calculate Mini-PIA Value (Reform #22)
#'
#' Calculates the "mini-PIA" which applies the PIA formula to each year's
#' indexed earnings, then averages the results. This is an alternative to
#' the standard method of averaging earnings first, then applying the formula.
#'
#' Mini-PIA formula: PIA = average(formula(each year's indexed earnings / 12))
#' vs Standard: PIA = formula(average(top 35 indexed earnings) / 12)
#'
#' @param indexed_earnings Vector of indexed earnings by year
#' @param comp_period Number of years in computation period
#' @param bp1 First PIA bend point (monthly)
#' @param bp2 Second PIA bend point (monthly)
#' @param fact1 First replacement factor (typically 0.90)
#' @param fact2 Second replacement factor (typically 0.32)
#' @param fact3 Third replacement factor (typically 0.15)
#'
#' @return The mini-PIA value
#' @keywords internal
calculate_mini_pia <- function(indexed_earnings, comp_period, bp1, bp2, fact1, fact2, fact3) {
  if (length(indexed_earnings) == 0 || comp_period <= 0) return(0)

  # Apply PIA formula to each year's monthly indexed earnings
  mini_pias <- sapply(indexed_earnings, function(earn) {
    monthly_earn <- earn / 12
    pia_val <- if (monthly_earn > bp2) {
      (fact1 * bp1) + (fact2 * (bp2 - bp1)) + (fact3 * (monthly_earn - bp2))
    } else if (monthly_earn > bp1) {
      (fact1 * bp1) + (fact2 * (monthly_earn - bp1))
    } else {
      fact1 * monthly_earn
    }
    floor(pia_val * 10) / 10  # Floor to dime
  })

  # Select top comp_period years by mini-PIA value and average
  if (length(mini_pias) > comp_period) {
    top_mini_pias <- sort(mini_pias, decreasing = TRUE)[1:comp_period]
  } else {
    top_mini_pias <- mini_pias
  }

  floor(mean(top_mini_pias) * 10) / 10
}


#' PIA Calculation
#'
#' Function that computes a worker's Primary Insurance Amount by age.
#' Compares the regular PIA (bend point formula) with the special minimum PIA
#' per 42 USC 415(a)(1)(C) and returns the higher of the two.
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
  # Per 42 USC 415(a)(1), PIA is the HIGHER of:
  # - Regular PIA: 90/32/15 bend point formula per 42 USC 415(a)(1)(A)
  # - Special minimum PIA: $11.50 × (years_of_coverage - 10), COLA-adjusted, per 42 USC 415(a)(1)(C)
  #
  # Bend points and replacement factors are determined at the worker's eligibility age
  # (elig_age_retired from assumptions, currently 62 for retirement benefits)


  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("bp1", "bp2", "fact1", "fact2", "fact3", "elig_age_retired",
                   "yoc_threshold", "special_min_rate", "min_yoc_for_special_min",
                   "pia_multiplier", "bp3", "fact4", "flat_benefit", "mini_pia_blend",
                   "taxmax_benefit")
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

      # Reform parameters: 4th bracket and PIA multiplier (Reform #1, #3, #12-14)
      # For Reforms #12-14, bp3 = old taxmax (in monthly terms for PIA formula)
      # When bp3 is NA but fact4 is set, fall back to taxmax_benefit / 12
      bp3_elig = if (all(is.na(bp3))) {
        if (!all(is.na(fact4))) taxmax_benefit[which(age == first(elig_age))] / 12 else NA_real_
      } else bp3[which(age == first(elig_age))],
      fact4_elig = if (all(is.na(fact4))) NA_real_ else fact4[which(age == first(elig_age))],
      pia_mult_elig = if (all(is.na(pia_multiplier))) 1.0 else pia_multiplier[which(age == first(elig_age))],
      flat_benefit_elig = if (all(is.na(flat_benefit))) NA_real_ else flat_benefit[which(age == first(elig_age))],
      # Reform #22: Mini-PIA blend factor (0 = current law, 1 = full mini-PIA)
      mini_pia_blend_elig = if (all(is.na(mini_pia_blend))) 0 else mini_pia_blend[which(age == first(elig_age))],

      # Get special minimum rate at eligibility age (COLA-adjusted $11.50 base)
      special_min_rate_elig = special_min_rate[which(age == first(elig_age))],
      min_yoc_elig = min_yoc_for_special_min[which(age == first(elig_age))],

      # Regular PIA per 42 USC 415(a)(1)(A): bend point formula
      # Standard: 90/32/15 (3 brackets)
      # Reform: 90/32/15/X (4 brackets) when bp3 and fact4 are specified
      # Per 42 USC 415(a)(2)(C): round to next lower $0.10
      regular_pia = case_when(
        age >= elig_age ~ floor_dime(case_when(
          # 4-bracket formula when bp3 and fact4 are specified
          !is.na(bp3_elig) & !is.na(fact4_elig) & aime > bp3_elig ~
            (fact1_elig * bp1_elig) + (fact2_elig * (bp2_elig - bp1_elig)) +
            (fact3_elig * (bp3_elig - bp2_elig)) + (fact4_elig * (aime - bp3_elig)),
          # Standard 3-bracket formula
          aime > bp2_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (bp2_elig - bp1_elig)) + (fact3_elig * (aime - bp2_elig)),
          aime > bp1_elig ~ (fact1_elig * bp1_elig) + (fact2_elig * (aime - bp1_elig)),
          TRUE ~ fact1_elig * aime
        )),
        TRUE ~ 0),

      # Special minimum PIA per 42 USC 415(a)(1)(C)(i):
      # PIA = special_min_rate × (years_of_coverage - 10)
      # Only applies if years_of_coverage >= min_yoc_for_special_min (11)
      # Per 42 USC 415(a)(2)(C): round to next lower $0.10
      special_min_pia = case_when(
        age >= elig_age & years_of_coverage >= min_yoc_elig ~
          floor_dime(special_min_rate_elig * (years_of_coverage - 10)),
        TRUE ~ 0),

      # Apply PIA multiplier (Reform #1)
      # This allows across-the-board benefit changes
      regular_pia_mult = floor_dime(regular_pia * pia_mult_elig),

      # Apply flat benefit floor (Reform #2) if specified
      # Worker's PIA = max(formula_pia, flat_benefit) if they have 40 QCs
      regular_pia_final = case_when(
        !is.na(flat_benefit_elig) & age >= elig_age ~ pmax(regular_pia_mult, flat_benefit_elig),
        TRUE ~ regular_pia_mult
      ),

      # Final PIA is the higher of regular (with multiplier and flat floor) or special minimum per 42 USC 415(a)(1)
      basic_pia = pmax(regular_pia_final, special_min_pia)
    ) %>% select(-bp1, -bp2, -fact1, -fact2, -fact3, -elig_age_retired,
                 -yoc_threshold, -special_min_rate, -min_yoc_for_special_min,
                 -pia_multiplier, -bp3, -fact4, -flat_benefit, -taxmax_benefit) %>% ungroup()

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
  cols_needed <- c("cola", "elig_age_retired", "cola_cap", "cola_cap_active")
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
    #
    # Reform #9 (COLA Cap): If cola_cap is set and beneficiary's PIA exceeds the cap,
    # they receive the same dollar COLA increase as the median beneficiary instead of
    # a percentage increase.
    group_modify(~ {
      n <- nrow(.x)
      cola_basic_pia_vals <- numeric(n)
      basic_pia <- .x$basic_pia
      cola_factor <- .x$cola_factor
      cola_cap_vals <- if ("cola_cap" %in% names(.x)) .x$cola_cap else rep(NA_real_, n)
      cola_cap_active_vals <- if ("cola_cap_active" %in% names(.x)) .x$cola_cap_active else rep(FALSE, n)
      elig_age_val <- .x$elig_age[1]
      ages <- .x$age

      for (i in seq_len(n)) {
        if (ages[i] < elig_age_val) {
          cola_basic_pia_vals[i] <- 0
        } else if (ages[i] == elig_age_val) {
          # At eligibility age: no COLA yet, use basic_pia
          cola_basic_pia_vals[i] <- basic_pia[i]
        } else {
          # After eligibility: apply COLA with potential capping
          prev_pia <- cola_basic_pia_vals[i-1]
          cap_threshold <- cola_cap_vals[i]

          cap_is_active <- if (length(cola_cap_active_vals) >= i) cola_cap_active_vals[i] else FALSE
          if (cap_is_active && !is.na(cap_threshold) && prev_pia > cap_threshold) {
            # Reform #9: COLA cap - give same dollar increase as median recipient
            # max_cola_increase = cap_threshold * (cola_factor - 1)
            max_cola_increase <- cap_threshold * (cola_factor[i] - 1)
            cola_basic_pia_vals[i] <- floor_dime(prev_pia + max_cola_increase)
          } else {
            # Standard COLA: multiply previous year's rounded PIA by current COLA factor
            # Per 42 USC 415(i)(2)(A)(ii): round to next lower $0.10
            cola_basic_pia_vals[i] <- floor_dime(prev_pia * cola_factor[i])
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
# 2.4b Basic Minimum Benefit Calculation (Reform #27)
# -----------------------------------------------------------------------------

#' Basic Minimum Benefit Calculation
#'
#' Applies the Basic Minimum Benefit (BMB) reform that supplements low benefits
#' at or after Normal Retirement Age. The BMB is calculated as:
#' BMB_supplement = max(BMB_rate - 0.70 * actuarially_adjusted_benefit, 0)
#'
#' This function should be called AFTER worker_benefit() in the calculation pipeline.
#'
#' @param worker Dataframe with a worker's benefits by age (must include wrk_ben, nra_ind)
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#'   (must include bmb_individual, bmb_couple if reform is active)
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with BMB supplement added to wrk_ben
#'
#' @export
basic_minimum_benefit <- function(worker, assumptions, debugg = FALSE) {
  # Reform #27: Basic Minimum Benefit
  # BMB kicks in at NRA, computed AFTER actuarial adjustments
  # BMB_supplement = max(BMB_rate - 0.70 * wrk_ben, 0)

  # Check if BMB reform is active (bmb_individual is not all NA)
  if (all(is.na(assumptions$bmb_individual))) {
    # BMB reform not active, return unchanged
    return(worker)
  }

  # Join BMB rates from assumptions
  cols_needed <- c("bmb_individual", "bmb_couple", "nra")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate BMB supplement
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Get worker's NRA
      yr_62 = year - age + 62,
      nra_bmb = nra[which(year == yr_62)][1],
      at_or_past_nra = age >= nra_bmb,

      # Determine BMB rate based on household type
      # Use couple rate if spouse_spec is not NA, otherwise individual rate
      has_spouse = !is.na(spouse_spec),
      bmb_rate = if_else(has_spouse, bmb_couple, bmb_individual),

      # Get BMB rate at eligibility year (AWI-indexed)
      bmb_rate_elig = bmb_rate[which(age == first(elig_age))][1],

      # Calculate BMB supplement: max(BMB - 0.70 * wrk_ben, 0)
      # Only applies at/after NRA
      bmb_supplement = if_else(
        at_or_past_nra & !is.na(bmb_rate_elig),
        pmax(bmb_rate_elig - 0.70 * wrk_ben, 0),
        0
      ),

      # Add BMB supplement to worker benefit
      wrk_ben = wrk_ben + floor(bmb_supplement)
    ) %>%
    ungroup()

  # Return result
  if (debugg) {
    cols_to_add <- c("bmb_rate_elig", "bmb_supplement")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]

    # Update wrk_ben in worker
    worker$wrk_ben <- NULL
    worker <- worker %>%
      left_join(dataset %>% select(id, age, wrk_ben), by = c("id", "age"))

    if (length(cols_new) > 0) {
      worker <- worker %>%
        left_join(dataset %>% select(id, age, all_of(cols_new)), by = c("id", "age"))
    }
  } else {
    # Update wrk_ben only
    worker$wrk_ben <- NULL
    worker <- worker %>%
      left_join(dataset %>% select(id, age, wrk_ben), by = c("id", "age"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 2.5 Family Maximum Calculation
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
family_maximum <- function(worker, assumptions, debugg = FALSE) {
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

      # Calculate disability family maximum alternative (42 USC 403(a)(6))
      # For disabled workers: min(85% of AIME, 150% of PIA)
      aime_at_elig = aime[which(age == elig_age)][1],
      disability_fm = floor_dime(pmin(0.85 * aime_at_elig, 1.50 * pia_at_elig)),

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

  # Apply family maximum reduction to auxiliary benefits
  dataset <- dataset %>%
    mutate(
      # Total auxiliary benefits (before family max reduction)
      # Only child benefits are included here, not spousal benefits, because spousal
      # benefits come from the spouse's record and are subject to the spouse's family
      # max, not this worker's family max.
      total_aux_ben = pmax(child1_ben, 0, na.rm = TRUE) +
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

      # Apply proportional reduction to each auxiliary benefit
      # Note: Spousal benefits are NOT reduced here because they are paid from the
      # spouse's record, not the worker's record. The spouse's family max (which we
      # don't calculate here) is what limits spousal benefits. Only child benefits
      # (which are paid from this worker's record) are subject to this worker's family max.
      spouse_ben_fm = pmax(spouse_ben, 0, na.rm = TRUE),  # Pass through unchanged
      child1_ben_fm = floor(pmax(child1_ben, 0, na.rm = TRUE) * fm_reduction_factor),
      child2_ben_fm = floor(pmax(child2_ben, 0, na.rm = TRUE) * fm_reduction_factor),
      child3_ben_fm = floor(pmax(child3_ben, 0, na.rm = TRUE) * fm_reduction_factor)
    )

  # Add columns to worker
  if (debugg) {
    cols_to_add <- c("family_max", "regular_fm", "disability_fm", "fm_at_elig",
                     "total_aux_ben", "aux_available", "fm_reduction_factor",
                     "spouse_ben_fm", "child1_ben_fm", "child2_ben_fm", "child3_ben_fm")
  } else {
    cols_to_add <- c("family_max", "spouse_ben_fm", "child1_ben_fm", "child2_ben_fm", "child3_ben_fm")
  }

  cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
  if (length(cols_new) > 0) {
    worker <- worker %>%
      left_join(dataset %>% select(id, year, all_of(cols_new)), by = c("id", "year"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 2.6 Final Benefit Calculation
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
      # Zero out spousal benefit after spouse dies - survivor benefit takes over
      # Spousal and child benefits require the worker to be alive
      spouse_ben_adj = if_else(
        !is.na(worker_age_at_spouse_death) & age >= worker_age_at_spouse_death,
        0,  # Spouse is dead, spousal benefit stops
        spouse_ben_fm  # Use family-max-adjusted spousal benefit
      ),
      # Total benefit = worker's own + spousal (FM adjusted) + child benefits (FM adjusted) + survivor
      # Child benefits only paid while worker is alive (age < death_age)
      total_child_ben = if_else(
        age < death_age,
        pmax(child1_ben_fm, 0, na.rm = TRUE) +
          pmax(child2_ben_fm, 0, na.rm = TRUE) +
          pmax(child3_ben_fm, 0, na.rm = TRUE),
        0
      ),
      ben = pmax(wrk_ben, 0, na.rm = TRUE) +
            pmax(spouse_ben_adj, survivor_ben, 0, na.rm = TRUE) +
            total_child_ben,

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

    # Select core output columns, including sex, spouse_spec, child_spec, and bc if they exist
    output_cols <- c("id", "sex", "year", "age", "spouse_spec",
                     "child1_spec", "child2_spec", "child3_spec",
                     "earnings", "ben", "bc")
    available_cols <- intersect(output_cols, names(worker))

    worker <- worker %>% select(all_of(available_cols))
  }

  return(worker)

}
