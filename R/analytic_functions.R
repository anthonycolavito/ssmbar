# =============================================================================
# ANALYTIC FUNCTIONS
# =============================================================================
#
# This file contains the core analytical functions for the ssmbar package.
#
# =============================================================================


# =============================================================================
# Tax Calculation Functions
# =============================================================================

#' Calculate Social Security Taxes Paid
#'
#' Calculates the nominal Social Security payroll taxes paid on a worker's earnings
#' for each year. Uses the OASI trust fund rate, DI trust fund rate, and taxable
#' maximum from the assumptions data.
#'
#' @param worker Data frame with worker earnings by year. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{oasi_tr}, \code{di_tr}, \code{taxmax}.
#'
#' @return Data frame with the worker data plus additional tax columns:
#'   \itemize{
#'     \item \code{ss_taxable_earn}: Earnings subject to SS tax (capped at taxmax)
#'     \item \code{oasi_tax}: OASI tax paid (employee share)
#'     \item \code{di_tax}: DI tax paid (employee share)
#'     \item \code{ss_tax}: Total SS tax paid (employee share = oasi_tax + di_tax)
#'   }
#'
#' @details
#' The tax rates in the assumptions represent the EMPLOYEE portion of the payroll tax.
#' Employers pay an equal matching amount. The rates are expressed as percentages
#' (e.g., 5.3 means 5.3%), not decimals.
#'
#' The taxable maximum (taxmax) caps the amount of earnings subject to Social Security
#' taxes in each year. For example, in 2025 only the first $176,100 of earnings is taxable.
#'
#' @examples
#' \dontrun{
#' # Generate worker earnings
#' worker <- earnings_generator(birth_yr = 1960, sex = "male", type = "medium",
#'   age_claim = 67, factors = sef2025, assumptions = tr2025)
#'
#' # Calculate taxes paid
#' worker_with_taxes <- calculate_taxes(worker, tr2025)
#'
#' # View tax columns
#' worker_with_taxes[worker_with_taxes$age %in% c(25, 35, 45, 55),
#'   c("year", "age", "earnings", "ss_taxable_earn", "ss_tax")]
#' }
#'
#' @importFrom dplyr %>% mutate select left_join
#' @export
calculate_taxes <- function(worker, assumptions) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "oasi_tr", "di_tr", "taxmax")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Join tax parameters from assumptions if not already present
  cols_needed <- c("oasi_tr", "di_tr", "taxmax")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate taxes
  # Rates are percentages (e.g., 5.3 means 5.3%), so divide by 100
  dataset <- dataset %>%
    mutate(
      ss_taxable_earn = pmin(earnings, taxmax, na.rm = TRUE),  # Cap earnings at taxmax
      oasi_tax = ss_taxable_earn * oasi_tr / 100,              # OASI tax (employee share)
      di_tax = ss_taxable_earn * di_tr / 100,                  # DI tax (employee share)
      ss_tax = oasi_tax + di_tax                               # Total SS tax (employee share)
    )

  # Remove joined columns that were only needed for calculation
  if (length(cols_missing) > 0) {
    dataset <- dataset %>%
      select(-any_of(cols_missing))
  }

  return(dataset)
}


# =============================================================================
# Replacement Rate Functions
# =============================================================================

#' Calculate Multiple Replacement Rate Definitions
#'
#' Computes a comprehensive set of replacement rate measures for Social Security
#' benefits. The replacement rate is the ratio of initial annual benefits to
#' some measure of pre-retirement earnings, indicating how much of a worker's
#' earnings Social Security replaces.
#'
#' @param worker Data frame with calculated benefits. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{earnings}, \code{annual_ind},
#'   and \code{claim_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi} (GDP price index),
#'   \code{awi} (Average Wage Index), and \code{real_df} (real discount factor).
#'
#' @return Data frame in long format with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{type}: Replacement rate type (see Details)
#'     \item \code{rep_rate}: Replacement rate value
#'   }
#'
#' @details
#' This function calculates 16 different replacement rate definitions, allowing
#' comparison across methodologies commonly used in policy analysis.
#'
#' \strong{Indexing Methods:}
#' \itemize{
#'   \item \strong{Price-indexed (real_*)}: Earnings adjusted using GDP price index
#'     to constant dollars of the last working year. Measures purchasing power replacement.
#'   \item \strong{Wage-indexed (wage_*)}: Earnings adjusted using Average Wage Index (AWI)
#'     to wage-equivalent dollars of the last working year. Measures relative position
#'     in the earnings distribution.
#' }
#'
#' \strong{Replacement Rate Types:}
#' \itemize{
#'   \item \strong{pv_rr}: Present-value replacement rate. Compares initial benefit to the
#'     constant annual payment that has the same present value as career earnings.
#'     Most economically rigorous measure.
#'   \item \strong{*_all}: Simple average of all working years (ages 21 to claim_age - 1)
#'   \item \strong{*_h35, *_h10, *_h5}: Average of highest N earning years
#'   \item \strong{*_l35, *_l10, *_l5}: Average of last N working years (most recent)
#' }
#'
#' \strong{Indexing Methodology:}
#' Per SSA Handbook Section 700.3, earnings are indexed to the worker's indexing year
#' (typically 2 years before eligibility age). This function indexes to the last
#' working year (year before claiming) for replacement rate comparability.
#'
#' @references
#' SSA Handbook Section 700.3: How Are Earnings Indexed?
#' \url{https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html}
#'
#' @examples
#' \dontrun{
#' # Calculate benefits with debugg = TRUE to get annual_ind
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#'
#' # Calculate all replacement rates
#' rr <- rep_rates(worker, tr2025)
#'
#' # View results
#' rr
#'
#' # Filter to specific type
#' rr[rr$type == "pv_rr", ]
#' }
#'
#' @importFrom dplyr %>% group_by arrange mutate filter summarise select first
#' @importFrom tidyr pivot_longer
#' @export
rep_rates <- function(worker, assumptions) {

  # Error Prevention
  worker_cols_needed <- c("id", "year", "age", "earnings", "annual_ind", "claim_age")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker file must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  assumption_cols_needed <- c("year", "gdp_pi", "awi", "real_df")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions file must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Join assumption columns if not already present
  cols_to_join <- c("gdp_pi", "awi", "real_df")
  cols_missing <- cols_to_join[!cols_to_join %in% names(worker)]
  if (length(cols_missing) > 0) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Get claim age for this worker (use first non-NA value)
      worker_claim_age = first(claim_age[!is.na(claim_age)]),
      # Last working age is year before claiming
      last_working_age = worker_claim_age - 1,

      # Initial benefit at claim age (numerator in the replacement rate)
      init_ben = annual_ind[which(age == worker_claim_age)][1],

      # Scalars indexed to last working year (year before claim)
      wage_scalar = awi[which(age == last_working_age)][1] / awi,
      price_scalar = gdp_pi[which(age == last_working_age)][1] / gdp_pi,

      # Indexed Earnings
      wage_earnings = earnings * wage_scalar,
      real_earnings = earnings * price_scalar,

      # Discount factors normalized to age 21
      real_df_norm = real_df / real_df[which(age == 21)][1],

      # Present value of real earnings at age 21
      pv_real_earn = real_earnings / real_df_norm
    ) %>%
    # Filter to working years (21 through year before claiming)
    filter(age >= 21 & age <= last_working_age) %>%
    summarise(
      # Initial benefit
      init_ben = first(init_ben),
      claim_age = first(worker_claim_age),

      # PV of earnings at age 21 (in real dollars)
      pv_real_earnings = sum(pv_real_earn, na.rm = TRUE),

      # Constant real payment with same PV as career earnings
      real_annuity = pv_real_earnings / sum(1 / real_df_norm, na.rm = TRUE),

      # Replacement Rates -- All years
      pv_rr = init_ben / real_annuity,
      real_all = init_ben / mean(real_earnings, na.rm = TRUE),
      wage_all = init_ben / mean(wage_earnings, na.rm = TRUE),

      # High-N Year Replacement Rates
      real_sorted = list(sort(real_earnings, decreasing = TRUE)),
      wage_sorted = list(sort(wage_earnings, decreasing = TRUE)),

      # Use pmin to handle cases with fewer than N years
      n_years = n(),
      real_h35 = init_ben / mean(real_sorted[[1]][1:pmin(35, n_years)]),
      wage_h35 = init_ben / mean(wage_sorted[[1]][1:pmin(35, n_years)]),
      real_h10 = init_ben / mean(real_sorted[[1]][1:pmin(10, n_years)]),
      wage_h10 = init_ben / mean(wage_sorted[[1]][1:pmin(10, n_years)]),
      real_h5 = init_ben / mean(real_sorted[[1]][1:pmin(5, n_years)]),
      wage_h5 = init_ben / mean(wage_sorted[[1]][1:pmin(5, n_years)]),

      # Last-N Years Replacement Rates (with bounds checking)
      real_l35 = init_ben / mean(real_earnings[max(1, n_years - 34):n_years]),
      wage_l35 = init_ben / mean(wage_earnings[max(1, n_years - 34):n_years]),
      real_l10 = init_ben / mean(real_earnings[max(1, n_years - 9):n_years]),
      wage_l10 = init_ben / mean(wage_earnings[max(1, n_years - 9):n_years]),
      real_l5 = init_ben / mean(real_earnings[max(1, n_years - 4):n_years]),
      wage_l5 = init_ben / mean(wage_earnings[max(1, n_years - 4):n_years]),

      .groups = "drop"
    ) %>%
    select(id, pv_rr, real_all, wage_all,
           real_h35, wage_h35, real_h10, wage_h10, real_h5, wage_h5,
           real_l35, wage_l35, real_l10, wage_l10, real_l5, wage_l5) %>%
    pivot_longer(cols = !"id",
                 names_to = "type",
                 values_to = "rep_rate")

  return(dataset)
}


# =============================================================================
# Marginal Analysis Functions
# =============================================================================

#' Compute PV of lifetime benefits discounted to a specific working year
#'
#' Helper function for marginal_benefit_analysis that computes the present value
#' of lifetime benefits discounted to a specific working year using nominal
#' discount factors.
#'
#' @param worker Data frame with calculated benefits (needs cola_basic_pia, act_factor)
#' @param assumptions Data frame with Trustees assumptions
#' @param discount_to_year The calendar year to discount benefits to
#' @param claim_age Age when benefits start
#' @param death_age Age when benefits end
#' @param birth_yr Worker's birth year
#' @param s_pia Spouse's PIA (fixed/exogenous), or NA if no spouse
#' @param s_act_factor Spousal actuarial factor
#' @param s_pia_share Spousal PIA share (typically 0.5)
#'
#' @return Numeric PV of lifetime benefits in discount_to_year dollars
#' @keywords internal
compute_pv_to_year <- function(worker, assumptions, discount_to_year,
                                claim_age, death_age, birth_yr,
                                s_pia = NA, s_act_factor = 1, s_pia_share = 0.5) {

  # Get discount factor at the working year
  df_working <- assumptions$df[assumptions$year == discount_to_year]
  if (length(df_working) == 0 || is.na(df_working)) return(0)

  # Benefit years: from claim to death
  benefit_ages <- seq(ceiling(claim_age), floor(death_age) - 1)
  if (length(benefit_ages) == 0) return(0)

  benefit_years <- birth_yr + benefit_ages

  # Get worker's COLA-adjusted PIA at claim year
  claim_year <- birth_yr + ceiling(claim_age)
  claim_idx <- which(worker$year == claim_year)
  if (length(claim_idx) == 0) return(0)

  # Get worker's own PIA and actuarial factor at claim
  worker_pia <- worker$cola_basic_pia[claim_idx[1]]
  if (is.na(worker_pia) || worker_pia <= 0) {
    # Worker not eligible for own benefits
    worker_benefit_monthly <- 0
    effective_own_pia <- 0
  } else {
    act_factor <- worker$act_factor[claim_idx[1]]
    if (is.na(act_factor)) act_factor <- 1
    worker_benefit_monthly <- worker_pia * act_factor
    effective_own_pia <- worker_pia
  }

  # Spousal benefit (if spouse exists)
  # No work requirement for spousal benefits - based on spouse's record
  spousal_benefit_monthly <- 0
  if (!is.na(s_pia) && s_pia > 0) {
    spousal_pia <- max(0, s_pia_share * s_pia - effective_own_pia)
    spousal_benefit_monthly <- spousal_pia * s_act_factor
  }

  total_monthly <- worker_benefit_monthly + spousal_benefit_monthly
  if (total_monthly <= 0) return(0)

  # Compute PV of benefit stream
  # Benefits grow with COLA from eligibility year (age 62)
  elig_year <- birth_yr + 62
  pv_sum <- 0

  for (i in seq_along(benefit_years)) {
    ben_year <- benefit_years[i]

    # Get COLA factor for this year
    cola_idx <- which(worker$year == ben_year)
    if (length(cola_idx) > 0 && !is.na(worker$cola_cum_factor[cola_idx[1]])) {
      cola_factor <- worker$cola_cum_factor[cola_idx[1]]
    } else {
      # Estimate COLA factor from assumptions
      cola_factor <- 1
      if (ben_year > elig_year) {
        for (y in (elig_year + 1):ben_year) {
          cola_rate <- assumptions$cola[assumptions$year == y]
          if (length(cola_rate) > 0 && !is.na(cola_rate)) {
            cola_factor <- cola_factor * (1 + cola_rate / 100)
          }
        }
      }
    }

    annual_nominal <- total_monthly * 12 * cola_factor

    # Discount to working year
    df_benefit <- assumptions$df[assumptions$year == ben_year]
    if (length(df_benefit) == 0 || is.na(df_benefit)) {
      # Extrapolate if beyond assumptions
      last_df_year <- max(assumptions$year[!is.na(assumptions$df)])
      last_df <- assumptions$df[assumptions$year == last_df_year]
      years_beyond <- ben_year - last_df_year
      df_benefit <- last_df * (1.05 ^ years_beyond)
    }

    discount_factor <- df_working / df_benefit
    pv_sum <- pv_sum + annual_nominal * discount_factor
  }

  pv_sum
}


#' Marginal Benefit Analysis
#'
#' Computes per-year marginal benefit analysis for a worker. For each working
#' year t, calculates the change in present value of lifetime benefits from
#' working t years vs t-1 years.
#'
#' This function uses the actual package benefit functions (aime, pia, cola,
#' worker_benefit) to calculate benefits for truncated earnings histories,
#' ensuring consistency with the rest of the package.
#'
#' The PV calculation discounts nominal benefits back to each working year using
#' nominal discount factors. This ensures that delta_pv_benefits is expressed in
#' the same nominal dollars as that year's earnings and taxes, enabling meaningful
#' cross-cohort comparisons of net marginal tax rates.
#'
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}: \code{id}, \code{year},
#'   \code{age}, \code{earnings}, \code{claim_age}, \code{death_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{df} (nominal discount factor), \code{cola}.
#'
#' @return Data frame with columns for each working year:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{year}: Calendar year
#'     \item \code{age}: Worker's age
#'     \item \code{earnings}: Nominal earnings that year
#'     \item \code{years_worked}: Cumulative years of work (1 to 44)
#'     \item \code{qcs}: Cumulative quarters of coverage (4 per year)
#'     \item \code{eligible}: Whether worker is eligible for benefits (QCs >= 40)
#'     \item \code{cumulative_aime}: AIME if worker stopped at this year
#'     \item \code{cumulative_pia}: PIA if worker stopped at this year
#'     \item \code{cumulative_pv}: PV of lifetime benefits if stopped at this year,
#'       expressed in nominal dollars of that working year
#'     \item \code{delta_pv_benefits}: Change in PV from working this year,
#'       expressed in nominal dollars of that working year
#'   }
#'
#' @details
#' This function answers: "What is the value of working year t, given that I've
#' already worked years 1 through t-1?"
#'
#' For each working year, this function:
#' \enumerate{
#'   \item Creates a worker with earnings truncated at year t
#'   \item Runs through the actual benefit calculation pipeline (aime, pia, cola, worker_benefit)
#'   \item Computes PV of lifetime benefits discounted to year t
#'   \item Compares to year t-1 to get delta_pv
#' }
#'
#' \strong{Spousal Benefits:}
#' If the worker has spouse data (from \code{calculate_benefits()} with spouse
#' parameters), the function includes the worker's dependent spousal benefit in the
#' calculation. The spouse's PIA is treated as exogenous (fixed), while the worker's
#' spousal benefit is recalculated at each year as their own PIA changes:
#' \itemize{
#'   \item Spousal PIA = max(0, 50\% × spouse's PIA - worker's own PIA)
#'   \item As the worker's own PIA increases, their spousal benefit decreases
#'   \item Dependent spousal benefits have NO work requirement
#'   \item This can result in higher NMTRs for lower-earning spouses
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' marginal <- marginal_benefit_analysis(worker, tr2025)
#'
#' # Check pattern:
#' # Years 1-9: delta_pv should be 0 (not eligible, no spouse)
#' # Year 10: delta_pv should be large (eligibility transition)
#' marginal[marginal$years_worked <= 12, c("age", "years_worked", "eligible", "delta_pv_benefits")]
#' }
#'
#' @importFrom dplyr %>% filter group_by group_modify mutate arrange first ungroup left_join select summarise
#' @export
marginal_benefit_analysis <- function(worker, assumptions, use_reform_pipeline = FALSE) {

  # Validate required columns
  required_cols <- c("id", "year", "age", "earnings", "claim_age", "death_age")
  if (!all(required_cols %in% names(worker))) {
    missing <- required_cols[!required_cols %in% names(worker)]
    stop(paste("worker data must contain:", paste(missing, collapse = ", "),
               "\nDid you run calculate_benefits() with debugg = TRUE?"))
  }

  # Extract base columns needed for benefit recalculation
  # These are the columns needed by aime() and downstream functions
  base_cols <- c("id", "year", "age", "sex", "earnings",
                 "claim_age", "elig_age", "death_age",
                 "spouse_spec", "child1_spec", "child2_spec", "child3_spec")
  base_cols <- intersect(base_cols, names(worker))

  # Preserve spouse data (exogenous)
  has_spouse <- "s_pia" %in% names(worker) && any(!is.na(worker$s_pia))
  if (has_spouse) {
    # Get spouse's PIA at claim year (fixed)
    spouse_data <- worker %>%
      group_by(id) %>%
      summarise(
        s_pia_fixed = {
          claim_yr <- first(year) - first(age) + ceiling(first(claim_age))
          idx <- which(year == claim_yr & !is.na(s_pia))
          if (length(idx) > 0) s_pia[idx[1]] else first(s_pia[!is.na(s_pia)])
        },
        s_act_factor_fixed = {
          idx <- which(!is.na(s_act_factor))
          if (length(idx) > 0) s_act_factor[idx[1]] else 1
        },
        s_pia_share = {
          idx <- which(!is.na(s_pia_share))
          if (length(idx) > 0) first(s_pia_share[idx]) else 0.5
        },
        .groups = "drop"
      )
  }

  # Process each worker
  result <- worker %>%
    group_by(id) %>%
    group_modify(~ {
      w <- .x
      n <- nrow(w)

      # Get worker parameters
      birth_yr <- first(w$year) - first(w$age)
      claim_age <- first(w$claim_age)
      death_age <- first(w$death_age)
      worker_id <- .y$id

      # Identify working years (ages 21-64)
      working_idx <- which(w$age >= 21 & w$age <= 64)
      n_working <- length(working_idx)

      # Initialize output columns
      w$years_worked <- NA_integer_
      w$qcs <- NA_integer_
      w$eligible <- NA
      w$cumulative_aime <- NA_real_
      w$cumulative_pia <- NA_real_
      w$cumulative_pv <- NA_real_
      w$delta_pv_benefits <- NA_real_

      if (n_working == 0) return(w)

      # Get spouse data for this worker
      if (has_spouse) {
        sp <- spouse_data[spouse_data$id == worker_id, ]
        s_pia_fixed <- if (nrow(sp) > 0) sp$s_pia_fixed else NA
        s_act_factor_fixed <- if (nrow(sp) > 0) sp$s_act_factor_fixed else 1
        s_pia_share_val <- if (nrow(sp) > 0) sp$s_pia_share else 0.5
      } else {
        s_pia_fixed <- NA
        s_act_factor_fixed <- 1
        s_pia_share_val <- 0.5
      }

      # Create base worker data for recalculation
      # Note: In group_modify, id is in .y, not in .x
      base_worker <- w[, intersect(base_cols, names(w))]
      base_worker$id <- worker_id

      # Cache PV calculations to avoid redundant work
      # pv_cache[t] = PV of benefits with t years of work
      pv_cache <- rep(NA_real_, n_working + 1)  # index 0 to n_working

      # Function to compute benefits and PV for a given number of working years
      compute_benefits_for_years <- function(t_years, discount_to_year) {
        if (t_years < 0) return(list(aime = 0, pia = 0, pv = 0))

        # Create worker with earnings zeroed after t_years of work
        worker_t <- base_worker
        if (t_years == 0) {
          worker_t$earnings <- 0
        } else {
          cutoff_age <- 20 + t_years  # Age after t years (started at 21)
          worker_t$earnings[worker_t$age > cutoff_age] <- 0
        }

        # Run through benefit pipeline
        if (use_reform_pipeline) {
          worker_t <- aime_reform(worker_t, assumptions, debugg = TRUE)
          worker_t <- pia_reform(worker_t, assumptions, debugg = TRUE)
          worker_t <- cola_reform(worker_t, assumptions, debugg = TRUE)
        } else {
          worker_t <- aime(worker_t, assumptions, debugg = TRUE)
          worker_t <- pia(worker_t, assumptions, debugg = TRUE)
          worker_t <- cola(worker_t, assumptions, debugg = TRUE)
        }
        worker_t <- worker_benefit(worker_t, assumptions, debugg = TRUE)

        # Get AIME and PIA at claim year
        claim_year <- birth_yr + ceiling(claim_age)
        claim_idx <- which(worker_t$year == claim_year)

        if (length(claim_idx) == 0) {
          return(list(aime = 0, pia = 0, pv = 0))
        }

        aime_val <- worker_t$aime[claim_idx[1]]
        pia_val <- worker_t$cola_basic_pia[claim_idx[1]]

        if (is.na(aime_val)) aime_val <- 0
        if (is.na(pia_val)) pia_val <- 0

        # Check eligibility (need 40 QCs = 10 years)
        qc_val <- worker_t$qc_tot[claim_idx[1]]
        if (is.na(qc_val) || qc_val < 40) {
          # Not eligible for own benefits, but may get spousal
          aime_val <- 0
          pia_val <- 0
        }

        # Compute PV discounted to the working year
        pv_val <- compute_pv_to_year(
          worker = worker_t,
          assumptions = assumptions,
          discount_to_year = discount_to_year,
          claim_age = claim_age,
          death_age = death_age,
          birth_yr = birth_yr,
          s_pia = s_pia_fixed,
          s_act_factor = s_act_factor_fixed,
          s_pia_share = s_pia_share_val
        )

        list(aime = aime_val, pia = pia_val, pv = pv_val, worker = worker_t)
      }

      # Compute for each working year
      for (t in seq_len(n_working)) {
        idx <- working_idx[t]
        working_year <- w$year[idx]

        w$years_worked[idx] <- t
        w$qcs[idx] <- 4 * t
        w$eligible[idx] <- (4 * t) >= 40

        # Compute benefits with t years of work
        result_t <- compute_benefits_for_years(t, working_year)
        pv_t <- result_t$pv

        # Compute benefits with t-1 years of work (discounted to same year)
        result_t_minus_1 <- compute_benefits_for_years(t - 1, working_year)
        pv_t_minus_1 <- result_t_minus_1$pv

        w$cumulative_aime[idx] <- result_t$aime
        w$cumulative_pia[idx] <- result_t$pia
        w$cumulative_pv[idx] <- pv_t
        w$delta_pv_benefits[idx] <- pv_t - pv_t_minus_1
      }

      w
    }) %>%
    ungroup()

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings",
                   "years_worked", "qcs", "eligible",
                   "cumulative_aime", "cumulative_pia", "cumulative_pv",
                   "delta_pv_benefits")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}


#' Calculate Net Marginal Tax Rate
#'
#' Computes the net marginal tax rate on Social Security for each working year.
#' The net marginal tax rate accounts for the marginal benefit accrued from
#' additional earnings using the cumulative stopping-point method.
#'
#' The calculation uses nominal discounting: future benefits are discounted
#' to each working year using nominal discount factors, ensuring that
#' delta_pv_benefits is in the same dollars as that year's earnings and taxes.
#' This enables meaningful cross-cohort comparisons.
#'
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#' @param include_employer Logical. If TRUE, includes employer share of taxes
#'   (12.4% total). Default is FALSE (6.2% employee share only).
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{year}: Calendar year
#'     \item \code{age}: Worker's age
#'     \item \code{earnings}: Nominal earnings
#'     \item \code{years_worked}: Cumulative years of work
#'     \item \code{qcs}: Cumulative quarters of coverage
#'     \item \code{eligible}: Whether eligible for benefits (QCs >= 40)
#'     \item \code{ss_tax}: Social Security tax paid (employee share)
#'     \item \code{delta_pv_benefits}: Change in PV of lifetime benefits from this year,
#'       in nominal dollars of that working year
#'     \item \code{net_marginal_tax_rate}: (ss_tax - delta_pv_benefits) / earnings
#'   }
#'
#' @details
#' The net marginal tax rate is calculated as:
#' \deqn{NMTR = \frac{SS\_tax - \Delta PV\_benefits}{earnings}}
#'
#' Where \code{delta_pv_benefits} is the change in present value of lifetime
#' benefits from working year t vs t-1 (using the cumulative stopping-point method).
#'
#' Expected pattern for a max earner:
#' \itemize{
#'   \item Years 1-9: NMTR ≈ 12.4% (not yet eligible, pure tax)
#'   \item Year 10: NMTR strongly negative (eligibility transition, big benefit gain)
#'   \item Years 11-35: NMTR varies based on PIA bracket and benefit accrual
#'   \item Years 36+: NMTR = 12.4% if year doesn't enter top 35
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' nmtr <- net_marginal_tax_rate(worker, tr2025, include_employer = TRUE)
#'
#' # Check the expected pattern
#' nmtr[nmtr$age %in% 21:35, c("age", "years_worked", "eligible", "net_marginal_tax_rate")]
#' }
#'
#' @export
net_marginal_tax_rate <- function(worker, assumptions, include_employer = FALSE,
                                   use_reform_pipeline = FALSE) {

  # Get marginal benefit analysis (using cumulative stopping-point method)
  # Benefits are discounted to each working year in nominal dollars
  marginal <- marginal_benefit_analysis(worker, assumptions,
                                         use_reform_pipeline = use_reform_pipeline)

  # Calculate taxes
  worker_with_taxes <- calculate_taxes(worker, assumptions)

  # Combine
  result <- marginal %>%
    left_join(
      worker_with_taxes %>% select(id, year, ss_tax),
      by = c("id", "year")
    ) %>%
    mutate(
      # Apply employer share if requested
      ss_tax_total = if (include_employer) ss_tax * 2 else ss_tax,

      # Net marginal tax rate
      # NMTR = (tax - delta_pv_benefits) / earnings
      # Note: delta_pv_benefits is already the total change in PV (not per-dollar)
      net_marginal_tax_rate = if_else(
        !is.na(earnings) & earnings > 0 & age >= 21 & age <= 64,
        (ss_tax_total - delta_pv_benefits) / earnings,
        NA_real_
      )
    )

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings", "years_worked", "qcs", "eligible",
                   "ss_tax", "delta_pv_benefits", "net_marginal_tax_rate")
  if (include_employer) {
    output_cols <- c(output_cols[1:8], "ss_tax_total", output_cols[9:11])
  }
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}


#' Calculate Marginal Internal Rate of Return
#'
#' Computes the marginal IRR for each working year. This is the IRR on that
#' year's tax contribution, considering only the marginal benefit accrual
#' from that year's work using the cumulative stopping-point method.
#'
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#' @param include_employer Logical. If TRUE, includes employer share of taxes.
#'   Default is FALSE (employee share only).
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{year}: Calendar year
#'     \item \code{age}: Worker's age
#'     \item \code{earnings}: Nominal earnings
#'     \item \code{ss_tax}: Social Security tax paid
#'     \item \code{delta_pv_benefits}: Change in PV of lifetime benefits from this year
#'     \item \code{marginal_irr}: IRR on this year's tax contribution
#'   }
#'
#' @details
#' The marginal IRR is computed using delta_pv_benefits from the cumulative
#' stopping-point method. Since delta_pv_benefits is the present value of
#' additional lifetime benefits discounted to the working year using nominal
#' discount factors, the IRR is the rate that makes the tax contribution grow
#' to equal this benefit gain by the time benefits begin (claim age):
#'
#' \deqn{tax_t \times (1 + r)^{(claim\_age - t)} = \Delta PV\_benefits}
#'
#' Solving for r:
#' \deqn{r = (\Delta PV\_benefits / tax_t)^{1/(claim\_age - t)} - 1}
#'
#' Special cases:
#' \itemize{
#'   \item Years 1-9 (not yet eligible): delta_pv = 0, so IRR = -1 (-100%)
#'   \item Year 10 (eligibility transition): Very high IRR (large delta_pv)
#'   \item Years 36+ outside top 35: IRR = -1 (-100%)
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' mirr <- marginal_irr(worker, tr2025)
#'
#' # Check pattern - year 10 should have very high IRR
#' mirr[mirr$age %in% 25:35, c("age", "delta_pv_benefits", "marginal_irr")]
#' }
#'
#' @export
marginal_irr <- function(worker, assumptions, include_employer = FALSE) {

  # Get marginal benefit analysis (using cumulative stopping-point method)
  # Benefits are discounted to each working year in nominal dollars
  marginal <- marginal_benefit_analysis(worker, assumptions)

  # Calculate taxes
  worker_with_taxes <- calculate_taxes(worker, assumptions)

  # Get claim_age from worker data
  claim_age <- worker$claim_age[!is.na(worker$claim_age)][1]
  if (is.na(claim_age)) claim_age <- 67  # Default if not found

  # Combine data
  combined <- marginal %>%
    left_join(
      worker_with_taxes %>% select(id, year, ss_tax),
      by = c("id", "year")
    ) %>%
    mutate(
      ss_tax_total = if (include_employer) ss_tax * 2 else ss_tax
    )

  # Calculate marginal IRR for each working year
  # IRR = (delta_pv_benefits / tax)^(1/(claim_age-age)) - 1
  # This is the rate at which the tax contribution grows to equal
  # the PV of marginal benefits by the time claiming begins
  result <- combined %>%
    mutate(
      marginal_irr = case_when(
        # Not a working year
        age < 21 | age > 64 ~ NA_real_,

        # No tax paid
        is.na(ss_tax_total) | ss_tax_total <= 0 ~ NA_real_,

        # No benefit gain (not eligible, or not in top 35)
        is.na(delta_pv_benefits) | delta_pv_benefits <= 0 ~ -1.0,

        # Age at or after claim age (shouldn't happen for working years, but guard)
        age >= claim_age ~ NA_real_,

        # Normal case: compute IRR
        # tax * (1+r)^(claim_age-age) = delta_pv_benefits
        # r = (delta_pv_benefits / tax)^(1/(claim_age-age)) - 1
        TRUE ~ (delta_pv_benefits / ss_tax_total)^(1 / (claim_age - age)) - 1
      )
    )

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings", "ss_tax",
                   "delta_pv_benefits", "marginal_irr")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}

