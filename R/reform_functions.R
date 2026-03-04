# =============================================================================
# REFORM FUNCTIONS - Social Security Policy Reform Calculations
# =============================================================================
#
# This file contains ALL reform-capable versions of benefit calculation functions
# that support policy reform modeling. These functions include additional
# parameters and code paths for simulating Social Security policy changes.
#
# Reform functions in this file:
#   - aime_reform(): AIME with Reform #14 (taxmax split) and #29 (child care credit)
#   - calculate_mini_pia(): Helper for Reform #22 (mini-PIA)
#   - pia_reform(): PIA with Reform #1 (multiplier), #2 (flat), #3/#12-14 (4th bracket), #22 (mini-PIA)
#   - cola_reform(): COLA with Reform #9 (COLA cap)
#   - basic_minimum_benefit(): Reform #27 (Basic Minimum Benefit)
#   - ret_reform(): RET with Reform #23 (RET repeal)
#   - widow_benefit_reform(): Widow benefit with Reform #28 (75% combined benefit)
#
# For BASELINE (current law) versions without reform parameters, see:
#   - aime(), pia(), cola() in benefit_calculations.R
#   - ret() in ret.R
#   - widow_benefit() in survivor.R
#
# =============================================================================


# =============================================================================
# SECTION 1: AIME Calculation (Reform-Capable)
# =============================================================================

#' AIME Calculation (Reform-Capable)
#'
#' Function that computes a worker's Average Indexed Monthly Earnings by age,
#' with support for reform parameters including separate tax/benefit caps
#' (Reform #14) and child care credits (Reform #29).
#'
#' This is the reform-capable version. For baseline (current law) AIME calculation,
#' see \code{\link{aime}}.
#'
#' @param worker Dataframe with a worker's earnings by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's AIME by year and age
#'
#' @seealso \code{\link{aime}} for the baseline version
#'
#' @export
aime_reform <- function(worker, assumptions, debugg = FALSE){ #Function for calculating the AIME of a specific worker
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


# =============================================================================
# SECTION 2: PIA Calculation (Reform-Capable)
# =============================================================================

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


#' PIA Calculation (Reform-Capable)
#'
#' Function that computes a worker's Primary Insurance Amount by age with
#' support for reform parameters including PIA multipliers (Reform #1),
#' flat benefits (Reform #2), 4th bracket (Reform #3, #12-14), and
#' mini-PIA blending (Reform #22).
#'
#' Compares the regular PIA (bend point formula) with the special minimum PIA
#' per 42 USC 415(a)(1)(C) and returns the higher of the two.
#'
#' This is the reform-capable version. For baseline (current law) PIA calculation,
#' see \code{\link{pia}}.
#'
#' @param worker Dataframe with a worker's earnings and AIME by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker dataframe with a worker's retired worker PIA by age
#'
#' @seealso \code{\link{pia}} for the baseline version
#'
#' @export
pia_reform <- function(worker, assumptions, debugg = FALSE) {
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
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
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
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
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


# =============================================================================
# SECTION 3: COLA Calculation (Reform-Capable)
# =============================================================================

#' COLA Calculation (Reform-Capable)
#'
#' Function that computes a worker's COLA-adjusted PIA with support for
#' the COLA cap reform (Reform #9) that limits COLA increases for
#' beneficiaries above the median PIA.
#'
#' This is the reform-capable version. For baseline (current law) COLA calculation,
#' see \code{\link{cola}}.
#'
#' @param worker Dataframe with a worker's unadjusted PIA -- both retired worker and spousal -- by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's COLA-adjusted retired worker and spousal PIA by age
#'
#' @seealso \code{\link{cola}} for the baseline version
#'
#' @importFrom dplyr lag
#' @export
cola_reform <- function (worker, assumptions, debugg = FALSE) {
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

      # Find index of eligibility age for COLA replay
      elig_idx <- which(ages == elig_age_val)[1]

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
            cola_forward <- floor_dime(prev_pia + max_cola_increase)
          } else {
            # Standard COLA: multiply previous year's rounded PIA by current COLA factor
            # Per 42 USC 415(i)(2)(A)(ii): round to next lower $0.10
            cola_forward <- floor_dime(prev_pia * cola_factor[i])
          }

          # Automatic recomputation (SSA Handbook Section 715):
          # If AIME increased from continued earnings, basic_pia[i] may be
          # higher than the original. Replay all COLAs from eligibility year
          # on the new basic_pia and take the max.
          if (basic_pia[i] > basic_pia[elig_idx]) {
            recomp_pia <- basic_pia[i]
            for (j in (elig_idx + 1):i) {
              # Replay with the same COLA/cap logic
              cap_j <- cola_cap_vals[j]
              cap_active_j <- if (length(cola_cap_active_vals) >= j) cola_cap_active_vals[j] else FALSE
              if (cap_active_j && !is.na(cap_j) && recomp_pia > cap_j) {
                max_increase <- cap_j * (cola_factor[j] - 1)
                recomp_pia <- floor_dime(recomp_pia + max_increase)
              } else {
                recomp_pia <- floor_dime(recomp_pia * cola_factor[j])
              }
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
# SECTION 4: Basic Minimum Benefit (Reform #27)
# =============================================================================

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

  # Check if BMB reform is active (bmb_start_year is present and not all NA)
  if (!"bmb_start_year" %in% names(assumptions) || all(is.na(assumptions$bmb_start_year))) {
    return(worker)
  }

  # Join BMB rates from assumptions
  cols_needed <- c("bmb_individual", "bmb_couple", "bmb_start_year", "nra")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate BMB supplement
  # The BMB base rate is AWI-indexed at eligibility (different cohorts get
  # different base amounts), then COLA'd forward after eligibility — same
  # pattern as regular PIA/COLA. All beneficiaries at/past NRA receive the
  # supplement starting in bmb_start_year.
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Get worker's NRA
      yr_62 = year - age + 62,
      nra_bmb = nra[which(year == yr_62)][1],
      at_or_past_nra = age >= nra_bmb,

      # Determine BMB rate based on household type
      has_spouse = !is.na(spouse_spec),
      bmb_rate_year = if_else(has_spouse, bmb_couple, bmb_individual),

      # Lock in the AWI-indexed rate at the worker's eligibility age
      bmb_rate_elig = bmb_rate_year[which(age == first(elig_age))][1],

      # COLA the base rate forward from eligibility, using the same cumulative
      # COLA factor as regular benefits (cola_basic_pia / basic_pia)
      bmb_cola_factor = if_else(
        basic_pia > 0,
        cola_basic_pia / basic_pia,
        1
      ),
      bmb_rate = bmb_rate_elig * bmb_cola_factor,

      # Calculate BMB supplement: max(BMB - 0.70 * wrk_ben, 0)
      # Only applies at/after NRA AND on/after the start year (calendar-year onset)
      bmb_supplement = if_else(
        at_or_past_nra & year >= first(bmb_start_year) & !is.na(bmb_rate),
        pmax(bmb_rate - 0.70 * wrk_ben, 0),
        0
      ),

      # Add BMB supplement to worker benefit
      wrk_ben = wrk_ben + floor(bmb_supplement)
    ) %>%
    ungroup()

  # Return result
  if (debugg) {
    cols_to_add <- c("bmb_rate", "bmb_supplement")
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


# =============================================================================
# SECTION 5: Retirement Earnings Test (Reform-Capable)
# =============================================================================

#' Retirement Earnings Test Calculation (Reform-Capable)
#'
#' Function that reduces an individual's benefits if their earnings exceed the
#' exempt amounts in the Retirement Earnings Test. Supports Reform #23 (RET repeal).
#'
#' This is the reform-capable version. For baseline (current law) RET calculation,
#' see \code{\link{ret}}.
#'
#' The RET reduces benefits for workers who have earnings above the exempt amount
#' (ret1) in years between their claiming age and Normal Retirement Age.
#' For every $2 of excess earnings, $1 of benefits is withheld.
#'
#' If the worker has a spouse claiming benefits based on their record, the total
#' benefit pot (worker's benefits + spouse's dependent benefit) is reduced, with
#' the reduction allocated proportionally.
#'
#' At NRA, the actuarial factor is recalculated to account for months of benefits
#' withheld, effectively treating the worker as if they claimed later.
#'
#' @param worker Dataframe with a worker's benefits by year and age
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia. NULL if no spouses or to generate on-the-fly.
#' @param factors Data frame for the Trustees' scaled earnings factors. Required
#'   if spouse_data is NULL and workers have spouse_spec for on-the-fly generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with RET-adjusted benefits
#'
#' @seealso \code{\link{ret}} for the baseline version
#'
#' @export
ret_reform <- function(worker, assumptions, spouse_data = NULL, factors = NULL, debugg = FALSE) {
  # RET is described in Chapter 18 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-toc18.html

  # Reform #23: Check if RET is enabled
  # If ret_enabled is FALSE for all years in the worker data, skip RET entirely.
  # ret_enabled is joined per-year by join_all_assumptions(), so we check the
  # worker's column (not assumptions[1]) to respect the effective_year.
  if ("ret_enabled" %in% names(worker) && all(worker$ret_enabled == FALSE, na.rm = TRUE)) {
    return(worker)
  }

  # Generate spouse_data on-the-fly if needed
  has_spouse_spec <- "spouse_spec" %in% names(worker) && any(!is.na(worker$spouse_spec))
  if (is.null(spouse_data) && has_spouse_spec) {
    if (is.null(factors)) {
      stop("factors parameter is required when spouse_data is NULL and workers have spouse_spec")
    }
    unique_specs <- unique(worker$spouse_spec[!is.na(worker$spouse_spec)])
    spouse_data <- lapply(unique_specs, function(spec) generate_spouse(spec, factors, assumptions))
    names(spouse_data) <- unique_specs
  }

  # Join required assumption columns
  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("ret1", "nra", "rf1", "rf2", "drc", "s_rf1", "s_rf2",
                   "ret_phaseout_rate", "elig_age_retired", "max_drc_age")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }


  # Ensure spousal columns exist (may not exist if spousal_pia/spouse_benefit were skipped)
  if (!"spouse_ben" %in% names(dataset)) {
    dataset$spouse_ben <- 0
  }
  if (!"spouse_pia" %in% names(dataset)) {
    dataset$spouse_pia <- 0
  }

  # Process each worker

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      wd <- .x  # worker_data
      spec <- wd$spouse_spec[1]
      elig_age_ret <- wd$elig_age_retired[1]
      ret_rate <- wd$ret_phaseout_rate[1]
      max_drc_age_val <- wd$max_drc_age[1]
      claim_age_val <- wd$claim_age[1]
      s_pia_share_val <- wd$s_pia_share[1]

      # Get birth-cohort parameters at eligibility age
      yr_elig <- wd$year[1] - wd$age[1] + elig_age_ret
      nra_ind <- wd$nra[wd$year == yr_elig][1]
      rf1_ind <- wd$rf1[wd$year == yr_elig][1]
      rf2_ind <- wd$rf2[wd$year == yr_elig][1]
      drc_ind <- wd$drc[wd$year == yr_elig][1]
      s_rf1_ind <- wd$s_rf1[wd$year == yr_elig][1]
      s_rf2_ind <- wd$s_rf2[wd$year == yr_elig][1]

      # =====================================================
      # Step 0: Apply SPOUSE's RET to worker's spouse_ben
      # =====================================================
      # If worker receives spouse_ben from spouse's record, and spouse has
      # excess earnings, reduce spouse_ben first (before worker's own RET).
      # The spouse is the "primary worker" for benefits based on their record.

      # Initialize spouse RET debug variables
      wd$s_excess_earnings <- 0
      wd$s_ret_reduction <- 0
      wd$s_own_ben <- 0
      wd$spouse_ret_to_spouse_ben <- 0
      wd$s_months_withheld <- 0
      wd$s_cum_months_withheld <- 0
      s_nra_ind <- NA_real_
      s_cum_months_at_s_nra <- 0

      if (!is.na(spec) && !is.null(spouse_data[[spec]]) && any(wd$spouse_ben > 0)) {
        spouse_ret <- calculate_spouse_ret_effect(wd, spouse_data[[spec]], assumptions, s_pia_share_val)

        # Store debug variables
        wd$s_excess_earnings <- spouse_ret$s_excess_earnings
        wd$s_ret_reduction <- spouse_ret$s_ret_reduction
        wd$s_own_ben <- spouse_ret$s_own_ben
        wd$spouse_ret_to_spouse_ben <- spouse_ret$spouse_ben_reduction * 12  # Annual for consistency
        wd$s_months_withheld <- spouse_ret$s_months_withheld
        s_nra_ind <- spouse_ret$s_nra

        # Apply spouse's RET reduction to worker's spouse_ben
        wd$spouse_ben <- pmax(wd$spouse_ben - spouse_ret$spouse_ben_reduction, 0)

        # Calculate cumulative months of spouse_ben withheld (for DRC payback)
        s_birth_yr <- spouse_data[[spec]]$s_birth_yr[1]
        wd$s_cum_months_withheld <- cumsum(if_else(
          (wd$year - s_birth_yr) < s_nra_ind,
          wd$s_months_withheld, 0
        ))
        s_cum_months_at_s_nra <- max(wd$s_cum_months_withheld, 0, na.rm = TRUE)
      }

      # =====================================================
      # Steps 1-5: Apply WORKER's RET (existing logic)
      # =====================================================
      # Now uses the already-reduced spouse_ben from Step 0

      # Calculate spouse's dependent benefit (based on worker's record)
      wd$spouse_dep_ben <- if (!is.na(spec) && !is.null(spouse_data[[spec]])) {
        calculate_spouse_dep_benefit(wd, spouse_data[[spec]], assumptions)
      } else { 0 }

      # Step 1: Calculate worker's excess earnings
      wd$excess_earnings <- calculate_excess_earnings(wd$earnings, wd$ret1, wd$age, claim_age_val, nra_ind)

      # Reform #23: Zero out excess earnings for years where RET is repealed
      if ("ret_enabled" %in% names(wd)) {
        wd$excess_earnings <- if_else(wd$ret_enabled == FALSE, 0, wd$excess_earnings)
      }

      # Step 2: Calculate reduction (capped at annual benefits)
      wrk_total_ben <- wd$wrk_ben + wd$spouse_ben
      total_ben_pot <- wrk_total_ben + wd$spouse_dep_ben
      wd$ret_reduction <- calculate_ret_reduction(wd$excess_earnings, ret_rate, total_ben_pot)

      # Step 3: Allocate reduction between benefits
      alloc <- allocate_ret_reduction(wd$ret_reduction, wd$wrk_ben, wd$spouse_ben, wd$spouse_dep_ben, s_pia_share_val)
      wd$wrk_share <- alloc$wrk_share
      wd$wrk_reduction <- alloc$wrk_reduction
      wrk_ben_reduced <- alloc$wrk_ben_reduced
      spouse_ben_reduced <- alloc$spouse_ben_reduced

      # Step 4: Calculate months withheld (from worker's own RET)
      wd$months_withheld <- calculate_months_withheld(wd$wrk_reduction, wrk_total_ben, wd$age, claim_age_val, nra_ind)
      wd$cum_months_withheld <- cumsum(if_else(wd$age < nra_ind, wd$months_withheld, 0))
      cum_months_at_nra <- max(wd$cum_months_withheld[wd$age < nra_ind], 0, na.rm = TRUE)

      # Step 5: Calculate DRC payback factors for worker's own benefits
      # Note: Disabled workers (elig_age < elig_age_ret) don't get actuarial adjustments
      # or DRC payback - their benefit equals 100% of their PIA at all ages
      is_disabled <- wd$elig_age[1] < elig_age_ret

      drc_factors <- calculate_drc_payback(claim_age_val, cum_months_at_nra, nra_ind,
                                            rf1_ind, rf2_ind, drc_ind, s_rf1_ind, s_rf2_ind, max_drc_age_val)

      # TODO: Verify DRC payback for spouse_ben withheld due to spouse's RET.
      # Currently assuming the worker gets DRC payback on their spousal actuarial
      # factor based on months of spouse_ben withheld. This needs SSA verification.
      # The payback would occur at the SPOUSE's NRA, not the worker's NRA.
      s_drc_payback_factor <- if (s_cum_months_at_s_nra > 0 && !is.na(s_nra_ind)) {
        # Recalculate spousal actuarial factor based on effective claim age
        effective_s_claim_age <- min(claim_age_val + (s_cum_months_at_s_nra / 12), s_nra_ind)
        rf_and_drc(effective_s_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0, max_drc_age_val)
      } else {
        drc_factors$new_s_act_factor
      }

      # Apply final benefits
      # For disabled workers: act_factor = 1.0 always (no DRC payback)
      # For retired workers: recalculate at NRA with DRC payback
      wd <- wd %>% mutate(
        wrk_ben = case_when(
          age < claim_age ~ 0,
          is_disabled ~ floor(cola_basic_pia * 1.0),  # Disabled: no actuarial adjustment
          age < nra_ind ~ wrk_ben_reduced,
          TRUE ~ floor(cola_basic_pia * drc_factors$new_act_factor)  # Retired: DRC payback at NRA
        ),
        spouse_ben = case_when(
          age < claim_age ~ 0,
          age < nra_ind ~ spouse_ben_reduced,
          # At/after NRA, use the DRC payback factor (accounting for spouse's RET withheld months)
          TRUE ~ floor(spouse_pia * s_drc_payback_factor)
        ),
        ret_adj_factor = if_else(age >= nra_ind, drc_factors$new_act_factor, drc_factors$orig_act_factor),
        ret_s_adj_factor = if_else(age >= nra_ind, s_drc_payback_factor, drc_factors$orig_s_act_factor),
        cum_months_withheld_final = cum_months_at_nra,
        s_cum_months_withheld_final = s_cum_months_at_s_nra
      )
      wd
    }) %>%
    ungroup()

  # Select output columns
  # Determine which columns to remove (only if they existed in original worker)
  cols_to_remove <- c()
  if ("wrk_ben" %in% names(worker)) cols_to_remove <- c(cols_to_remove, "wrk_ben_orig")
  if ("spouse_ben" %in% names(worker)) cols_to_remove <- c(cols_to_remove, "spouse_ben_orig")

  if (debugg) {
    # Debug columns need existence check
    debug_cols <- c("spouse_dep_ben", "s_excess_earnings", "s_ret_reduction", "s_own_ben",
                    "spouse_ret_to_spouse_ben", "s_months_withheld", "s_cum_months_withheld",
                    "excess_earnings", "ret_reduction", "wrk_share", "wrk_reduction",
                    "months_withheld", "cum_months_withheld", "ret_adj_factor", "ret_s_adj_factor",
                    "cum_months_withheld_final", "s_cum_months_withheld_final")
    cols_new <- debug_cols[!debug_cols %in% names(worker)]

    # First join wrk_ben and spouse_ben with suffix handling
    result <- worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-any_of(cols_to_remove))

    # Then join additional debug columns that don't already exist
    if (length(cols_new) > 0) {
      result <- result %>% left_join(
        dataset %>% select(id, age, all_of(cols_new)),
        by = c("id", "age")
      )
    }
    result
  } else {
    worker %>% left_join(
      dataset %>% select(id, age, wrk_ben, spouse_ben),
      by = c("id", "age"), suffix = c("_orig", "")
    ) %>% select(-any_of(cols_to_remove))
  }
}


# =============================================================================
# SECTION 6: Widow Benefit (Reform-Capable)
# =============================================================================

#' Widow Benefit Calculation (Reform-Capable)
#'
#' Function that calculates a widow's benefit based on the deceased spouse's earnings record,
#' with support for Reform #28 (75% combined widow benefit alternative).
#'
#' This is the reform-capable version. For baseline (current law) widow benefit calculation,
#' see \code{\link{widow_benefit}}.
#'
#' For disabled widow(er)s (age 50-59), the actuarial reduction is calculated as if they
#' claimed at age 60, per SSA rules.
#'
#' @param worker Dataframe with a worker's COLA-adjusted retired worker PIA by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's survivor benefit by age
#'
#' @seealso \code{\link{widow_benefit}} for the baseline version
#'
#' @export
widow_benefit_reform <- function(worker, assumptions, debugg = FALSE) {
  # TODO: Documentation needs verification - the citations below are for retired worker
  # benefits (Sections 720, 723, 724), not widow benefits. Need to add proper citations
  # for widow reduction factors. See POMS RS 00615.301 for widow benefit reductions.
  #
  # For DISABLED widow(er)s (age 50-59), the actuarial reduction is calculated as if
  # they claimed at age 60, regardless of their actual age. This means all disabled
  # widow(er)s receive the same reduction factor as a non-disabled widow claiming at 60.
  # See SSA Handbook Section 401.1 and POMS RS 00615.301.
  #
  # Current citations (for reference, but apply to retired workers not widows):
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("nra", "elig_age_retired", "widow_75_pct_active")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  # Handle case where is_disabled_widow column doesn't exist (backwards compatibility)
  if (!"is_disabled_widow" %in% names(dataset)) {
    dataset$is_disabled_widow <- FALSE
  }

  dataset <- dataset %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62, #RF/DRC amounts and NRA are based on year turning age 62 in assumptions.
      nra_ind = nra[which(year == yr_62)], #NRA for age 62 cohort
      w_elig_age_ind = elig_age_retired[which(year==yr_62)] - 2,  # Standard widow eligibility age (60)
      # TODO: Verify w_rf formula against POMS RS 00615.301 (widow reduction factor)
      # TODO: Document - explain derivation of 0.285 constant (28.5% maximum reduction)
      w_rf = .285/((nra_ind - w_elig_age_ind)*12),

      # Widow claim age is the later of: (1) worker's claim_age and (2) worker's age when spouse dies
      # If no spouse (worker_age_at_spouse_death is NA), use claim_age
      actual_widow_claim_age = if_else(
        is.na(worker_age_at_spouse_death),
        as.numeric(claim_age),
        as.numeric(pmax(claim_age, worker_age_at_spouse_death))
      ),

      # For DISABLED widow(er)s, actuarial factor is calculated as if claiming at age 60
      # regardless of their actual claim age (which may be 50-59).
      # The effective_widow_claim_age is used ONLY for actuarial factor calculation.
      effective_widow_claim_age = if_else(
        is_disabled_widow,
        w_elig_age_ind,  # Use age 60 for actuarial calculation
        actual_widow_claim_age  # Use actual claim age for non-disabled
      ),

      # Actuarial adjustment based on effective claim age (age 60 for disabled widow(er)s)
      w_act_factor = rf_and_drc(effective_widow_claim_age, nra_ind, w_rf, w_rf, 0),

      # For disabled widow(er)s, the benefit eligibility age is 50 (handled in widow_pia)
      # For standard widows, it's 60. The actual_widow_claim_age determines when benefits start.
      # For disabled widow(er)s, benefits start at the later of: age 50, disability onset, spouse death
      disabled_widow_claim_age = pmax(50, elig_age, worker_age_at_spouse_death, na.rm = TRUE),

      # Determine when survivor benefits actually start
      benefit_start_age = if_else(
        is_disabled_widow,
        disabled_widow_claim_age,
        actual_widow_claim_age
      ),

      # Current law survivor benefit
      survivor_ben_current_law = case_when(
        age >= benefit_start_age & survivor_pia > 0 ~ floor(survivor_pia * w_act_factor),
        TRUE ~ 0
      ),

      # Reform #28: 75% Combined Widow Benefit
      # Alternative = min(75% * (survivor_wrk_ben + deceased_wrk_ben), medium_worker_pia_cap)
      # For simplicity, use the survivor's cola_basic_pia * 1.5 as the medium worker cap proxy
      # (A full implementation would pre-calculate medium worker PIAs)
      widow_75_active = !is.na(widow_75_pct_active[1]) & widow_75_pct_active[1] == TRUE,

      # Need wrk_ben from the calculation. If not available, use cola_basic_pia * w_act_factor
      own_wrk_ben = if ("wrk_ben" %in% names(.)) wrk_ben else floor(cola_basic_pia * w_act_factor),

      # deceased_wrk_ben comes from survivor_pia + own_pia offset
      # survivor_pia is already the excess (deceased - own), so deceased_ben = survivor_pia + own_pia
      # We need the deceased's benefit which is approximated by survivor_pia / w_act_factor for the deceased

      # Alternative widow benefit: 75% of combined benefits, capped
      # Note: This is a simplified implementation. The deceased's benefit should come from spouse_data
      survivor_ben_alternative = case_when(
        age >= benefit_start_age & widow_75_active ~ floor(pmin(
          0.75 * (own_wrk_ben + survivor_pia * w_act_factor / 0.825),  # 0.825 is the minimum widow RIB-LIM
          cola_basic_pia * 1.5  # Medium worker PIA cap (proxy)
        )),
        TRUE ~ 0
      ),

      # Final survivor benefit: max of current law or 75% alternative
      survivor_ben = case_when(
        widow_75_active ~ pmax(survivor_ben_current_law, survivor_ben_alternative),
        TRUE ~ survivor_ben_current_law
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    cols_to_add <- c("actual_widow_claim_age", "effective_widow_claim_age", "benefit_start_age", "w_rf", "w_act_factor", "survivor_ben")
    cols_new <- cols_to_add[!cols_to_add %in% names(worker)]
    if (length(cols_new) > 0) {
      worker <- worker %>% left_join(dataset %>% select(id, age, all_of(cols_new)),
                                     by = c("id","age"))
    }
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, survivor_ben),
                                   by = c("id","age") ) #Left joins variables needed to continue benefit calculation
  }

  return(worker)

}
