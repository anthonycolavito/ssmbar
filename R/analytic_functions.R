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
#' @note This function is not exported. Use \code{ssmbar:::rep_rates()} to access it.
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
#' rr <- ssmbar:::rep_rates(worker, tr2025)
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
#' @keywords internal
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

#' Marginal Benefit Analysis
#'
#' Computes per-year marginal benefit analysis for a worker using the cumulative
#' stopping-point method. For each working year t, calculates the change in
#' present value of lifetime benefits from working t years vs t-1 years.
#'
#' The PV calculation discounts nominal benefits back to each working year using
#' nominal discount factors. This ensures that delta_pv_benefits is expressed in
#' the same nominal dollars as that year's earnings and taxes, enabling meaningful
#' cross-cohort comparisons of net marginal tax rates.
#'
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}: \code{id}, \code{year},
#'   \code{age}, \code{earnings}, \code{indexed_earn}, \code{index_factor}, \code{aime},
#'   \code{claim_age}, \code{death_age}, \code{bp1_elig}, \code{bp2_elig}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{df} (nominal discount factor), \code{cola}.
#'
#' @return Data frame with columns for each working year:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{year}: Calendar year
#'     \item \code{age}: Worker's age
#'     \item \code{earnings}: Nominal earnings that year
#'     \item \code{indexed_earn}: Indexed earnings for AIME calculation
#'     \item \code{years_worked}: Cumulative years of work (1 to 44)
#'     \item \code{qcs}: Cumulative quarters of coverage (4 per year)
#'     \item \code{eligible}: Whether worker is eligible for benefits (QCs >= 40)
#'     \item \code{cumulative_aime}: AIME if worker stopped at this year
#'     \item \code{cumulative_pia}: PIA if worker stopped at this year
#'     \item \code{cumulative_pv}: PV of lifetime benefits if stopped at this year,
#'       expressed in nominal dollars of that working year
#'     \item \code{delta_pv_benefits}: Change in PV from working this year,
#'       expressed in nominal dollars of that working year
#'     \item \code{in_top_35}: Whether this year is in top 35 indexed earnings
#'     \item \code{indexed_rank}: Rank of indexed earnings (1 = highest)
#'   }
#'
#' @details
#' This function answers: "What is the value of working year t, given that I've
#' already worked years 1 through t-1?"
#'
#' The calculation compares two hypothetical workers identical in every way except
#' one worked t years and the other worked t-1 years. The marginal value is the
#' difference in their lifetime PV of benefits.
#'
#' \strong{Spousal Benefits:}
#' If the worker has spouse data (from \code{calculate_benefits()} with spouse
#' parameters), the function includes the worker's dependent spousal benefit in the
#' calculation. The spouse's PIA is treated as exogenous (fixed), while the worker's
#' spousal benefit is recalculated at each year as their own PIA changes:
#' \itemize{
#'   \item Spousal PIA = max(0, 50\% × spouse's PIA - worker's own PIA)
#'   \item As the worker's own PIA increases, their spousal benefit decreases
#'   \item This can result in higher NMTRs for lower-earning spouses
#' }
#'
#' \strong{Statutory Calculations:}
#' This function uses the actual statutory formulas for AIME and PIA:
#' \itemize{
#'   \item \strong{AIME}: Per 42 USC 415(b), computed as floor(sum(top comp_period
#'     indexed earnings) / (comp_period * 12)), rounded DOWN to nearest dollar
#'   \item \strong{Regular PIA}: Per 42 USC 415(a)(1)(A), uses 90/32/15 bend point
#'     formula with factors from worker's eligibility year, floored to dime
#'   \item \strong{Special Minimum PIA}: Per 42 USC 415(a)(1)(C), computed if
#'     years of coverage >= 11, equals special_min_rate * (YOC - 10)
#'   \item \strong{Final PIA}: Maximum of regular and special minimum PIA
#' }
#'
#' \strong{Key insights:}
#' \itemize{
#'   \item Years 1-9: Worker not yet eligible (< 40 QCs), so delta_pv = 0
#'   \item Year 10: Worker becomes eligible, large positive delta_pv
#'   \item Years 11-35: Each year adds to AIME numerator
#'   \item Years 36+: Only adds value if displacing a lower-earning year from top 35
#' }
#'
#' The computation period (typically 35 years for retirement) is fixed based on
#' eligibility age per 42 USC 415(b)(2)(A), NOT on years worked.
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' marginal <- marginal_benefit_analysis(worker, tr2025)
#'
#' # Check pattern for max earner:
#' # Years 1-9: delta_pv should be 0 (not eligible)
#' # Year 10: delta_pv should be large (eligibility transition)
#' marginal[marginal$age %in% 21:35, c("age", "years_worked", "qcs", "eligible", "delta_pv_benefits")]
#' }
#'
#' @importFrom dplyr %>% filter group_by mutate arrange first ungroup left_join select
#' @export
marginal_benefit_analysis <- function(worker, assumptions) {

 # Validate required columns - need debugg = TRUE output
 required_cols <- c("id", "year", "age", "earnings", "indexed_earn",
                    "claim_age", "death_age")
 if (!all(required_cols %in% names(worker))) {
   missing <- required_cols[!required_cols %in% names(worker)]
   stop(paste("worker data must contain:", paste(missing, collapse = ", "),
              "\nDid you run calculate_benefits() with debugg = TRUE?"))
 }

 # Check for PIA bend points
 if (!"bp1_elig" %in% names(worker) || !"bp2_elig" %in% names(worker)) {
   stop("worker data must contain 'bp1_elig' and 'bp2_elig' columns (run with debugg = TRUE)")
 }

 # Get assumption columns - need nominal discount factor (df), cola, and special minimum params
 assumption_cols <- c("df", "cola", "yoc_threshold", "special_min_rate", "min_yoc_for_special_min")
 cols_missing <- assumption_cols[!assumption_cols %in% names(worker)]
 if (length(cols_missing) > 0) {
   worker <- worker %>%
     left_join(assumptions %>% select(year, any_of(cols_missing)), by = "year")
 }

 # Process each worker
 result <- worker %>%
   group_by(id) %>%
   group_modify(~ {
     w <- .x
     n <- nrow(w)

     # Extract worker parameters
     claim_age <- first(w$claim_age[!is.na(w$claim_age)])
     death_age <- first(w$death_age[!is.na(w$death_age)])
     birth_yr <- first(w$year) - first(w$age)
     bp1 <- first(w$bp1_elig[!is.na(w$bp1_elig)])
     bp2 <- first(w$bp2_elig[!is.na(w$bp2_elig)])
     eligibility_year <- birth_yr + 62
     elig_age <- 62  # Standard eligibility age for retirement

     # Get PIA factors from worker data (use statutory defaults if not available)
     fact1 <- if ("fact1_elig" %in% names(w)) first(w$fact1_elig[!is.na(w$fact1_elig)]) else 0.90
     fact2 <- if ("fact2_elig" %in% names(w)) first(w$fact2_elig[!is.na(w$fact2_elig)]) else 0.32
     fact3 <- if ("fact3_elig" %in% names(w)) first(w$fact3_elig[!is.na(w$fact3_elig)]) else 0.15

     # Get reform parameters from assumptions at eligibility year
     # These support policy reforms:
     # - bp3/fact4: 4th PIA bracket (Reform #3, #12-14)
     # - pia_multiplier: Across-the-board benefit changes (Reform #1)
     # - flat_benefit: Flat benefit floor (Reform #2)
     elig_yr_idx <- which(assumptions$year == eligibility_year)
     if (length(elig_yr_idx) > 0) {
       bp3 <- if ("bp3" %in% names(assumptions)) assumptions$bp3[elig_yr_idx[1]] else NA_real_
       fact4 <- if ("fact4" %in% names(assumptions)) assumptions$fact4[elig_yr_idx[1]] else NA_real_
       pia_multiplier <- if ("pia_multiplier" %in% names(assumptions)) {
         val <- assumptions$pia_multiplier[elig_yr_idx[1]]
         if (is.na(val)) 1.0 else val
       } else 1.0
       flat_benefit <- if ("flat_benefit" %in% names(assumptions)) assumptions$flat_benefit[elig_yr_idx[1]] else NA_real_
     } else {
       bp3 <- NA_real_
       fact4 <- NA_real_
       pia_multiplier <- 1.0
       flat_benefit <- NA_real_
     }

     # Get computation period from worker data (use statutory formula if not available)
     # Per 42 USC 415(b)(2)(A): comp_period = elapsed_years - dropout_years, min 2
     # For retirement: elapsed = elig_age - 1 - 21, dropout = 5
     if ("comp_period" %in% names(w)) {
       comp_period <- first(w$comp_period[!is.na(w$comp_period)])
     } else {
       elapsed_years <- elig_age - 1 - 21  # 40 for age 62
       dropout_years <- 5
       comp_period <- max(2, elapsed_years - dropout_years)  # 35 for age 62
     }

     # Get special minimum parameters at eligibility year
     elig_idx <- which(w$year == eligibility_year)
     if (length(elig_idx) > 0 && "special_min_rate" %in% names(w)) {
       special_min_rate_elig <- w$special_min_rate[elig_idx[1]]
       min_yoc_elig <- if ("min_yoc_for_special_min" %in% names(w)) {
         w$min_yoc_for_special_min[elig_idx[1]]
       } else 11
     } else {
       special_min_rate_elig <- NA
       min_yoc_elig <- 11
     }

     # Get actuarial factor if available
     if ("act_factor" %in% names(w)) {
       act_factor <- w$act_factor[which(w$age == claim_age)[1]]
       if (is.na(act_factor) || length(act_factor) == 0) act_factor <- 1.0
     } else {
       act_factor <- 1.0
     }

     # Get spousal actuarial factor if available (for dependent spousal benefit)
     if ("s_act_factor" %in% names(w)) {
       s_act_factor <- w$s_act_factor[which(w$age == claim_age)[1]]
       if (is.na(s_act_factor) || length(s_act_factor) == 0) s_act_factor <- act_factor
     } else {
       s_act_factor <- act_factor  # Use worker's factor as fallback
     }

     # Get spouse's PIA if available (fixed/exogenous - doesn't change with worker's years worked)
     # s_pia is the spouse's COLA-adjusted PIA from their own earnings record
     has_spouse <- "s_pia" %in% names(w) && any(!is.na(w$s_pia))
     if (has_spouse) {
       # Get spouse's PIA at claim year (when benefits start)
       claim_year <- birth_yr + ceiling(claim_age)
       s_pia_idx <- which(w$year == claim_year & !is.na(w$s_pia))
       if (length(s_pia_idx) > 0) {
         spouse_pia_fixed <- w$s_pia[s_pia_idx[1]]
       } else {
         # Fallback: use first non-NA s_pia value
         spouse_pia_fixed <- first(w$s_pia[!is.na(w$s_pia)])
       }
       if (is.na(spouse_pia_fixed)) has_spouse <- FALSE
     }
     if (!has_spouse) spouse_pia_fixed <- 0

     # Get s_pia_share (typically 0.5) from assumptions
     s_pia_share <- if ("s_pia_share" %in% names(w)) {
       first(w$s_pia_share[!is.na(w$s_pia_share)])
     } else if ("s_pia_share" %in% names(assumptions)) {
       first(assumptions$s_pia_share[!is.na(assumptions$s_pia_share)])
     } else {
       0.5  # Default spousal share
     }

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
     w$in_top_35 <- NA
     w$indexed_rank <- NA_integer_

     if (n_working == 0) return(w)

     # Get indexed earnings and nominal earnings for working years
     indexed_earnings <- w$indexed_earn[working_idx]
     nominal_earnings <- w$earnings[working_idx]

     # Get yoc_threshold for each working year (for special minimum calculation)
     if ("yoc_threshold" %in% names(w)) {
       yoc_thresholds <- w$yoc_threshold[working_idx]
     } else {
       yoc_thresholds <- rep(NA, n_working)
     }

     # Pre-compute COLA factors for each benefit year
     # COLA factors don't depend on the working year, so we compute once
     benefit_ages <- seq(ceiling(claim_age), floor(death_age) - 1)
     n_benefit_years <- length(benefit_ages)
     benefit_years <- birth_yr + benefit_ages

     # Build array of cumulative COLA factors
     cola_factors <- numeric(n_benefit_years)
     cumulative_cola <- 1.0
     for (i in seq_along(benefit_ages)) {
       ben_year <- benefit_years[i]

       # COLA is applied at the start of each year after eligibility
       # First benefit year gets no COLA adjustment, then each subsequent year
       if (ben_year > eligibility_year) {
         cola_idx <- which(assumptions$year == ben_year)
         if (length(cola_idx) > 0) {
           cola_rate <- assumptions$cola[cola_idx[1]]
           if (!is.na(cola_rate)) {
             cumulative_cola <- cumulative_cola * (1 + cola_rate / 100)
           }
         }
       }
       cola_factors[i] <- cumulative_cola
     }

     # Pre-fetch nominal discount factors (df) for benefit years
     # These will be used to discount back to each working year
     df_benefit_years <- numeric(n_benefit_years)
     for (i in seq_along(benefit_years)) {
       df_idx <- which(assumptions$year == benefit_years[i])
       if (length(df_idx) > 0) {
         df_benefit_years[i] <- assumptions$df[df_idx[1]]
       } else {
         # If benefit year is beyond assumptions, extrapolate using last available
         last_df_idx <- which(!is.na(assumptions$df))
         if (length(last_df_idx) > 0) {
           last_year <- assumptions$year[max(last_df_idx)]
           last_df <- assumptions$df[max(last_df_idx)]
           # Extrapolate assuming ~5% nominal growth (approximate)
           years_beyond <- benefit_years[i] - last_year
           df_benefit_years[i] <- last_df * (1.05 ^ years_beyond)
         } else {
           df_benefit_years[i] <- 1
         }
       }
     }

     # Helper function to compute PV of lifetime benefits given own PIA
     # Includes worker's dependent spousal benefit if spouse data is available
     # Discounts nominal benefits to the specified working year
     #
     # Eligibility rules:
     # - Worker's own retired worker benefit: Requires 40 QCs (10 years of work)
     # - Dependent spousal benefit: NO work requirement - based on spouse's record
     compute_pv <- function(own_pia, df_working_year, t_years) {
       if (is.na(df_working_year) || df_working_year == 0) return(0)

       # Worker's own benefit at claim = own_PIA × actuarial factor
       # Only payable if worker is insured (40 QCs = 10 years of work)
       if (t_years >= 10 && own_pia > 0) {
         worker_benefit_monthly <- own_pia * act_factor
       } else {
         worker_benefit_monthly <- 0
       }

       # Worker's dependent spousal benefit (based on spouse's record)
       # Spousal PIA = max(0, s_pia_share × spouse's PIA - worker's own PIA)
       # This decreases as the worker's own PIA increases
       # NO work requirement for spousal benefits - worker can receive them
       # even with zero quarters of coverage
       if (has_spouse && spouse_pia_fixed > 0) {
         # Use worker's own PIA in offset calculation (0 if not insured)
         effective_own_pia <- if (t_years >= 10) own_pia else 0
         spousal_pia <- max(0, s_pia_share * spouse_pia_fixed - effective_own_pia)
         spousal_benefit_monthly <- spousal_pia * s_act_factor
       } else {
         spousal_benefit_monthly <- 0
       }

       # Total monthly benefit
       total_monthly <- worker_benefit_monthly + spousal_benefit_monthly

       if (total_monthly <= 0) return(0)

       # Annual benefit at each age = monthly × 12 × COLA adjustment
       # Discount nominal benefits back to working year using nominal df
       pv_sum <- 0
       for (i in seq_along(benefit_ages)) {
         annual_nominal <- total_monthly * 12 * cola_factors[i]
         # Discount factor: df_working_year / df_benefit_year
         # This gives the value of $1 received in benefit year, in working-year dollars
         discount_factor <- df_working_year / df_benefit_years[i]
         pv_sum <- pv_sum + annual_nominal * discount_factor
       }
       pv_sum
     }

     # Rank all working years by indexed earnings (for reporting)
     all_ranks <- rank(-indexed_earnings, ties.method = "first")

     # Helper function to compute AIME given t years of indexed earnings
     # Uses statutory formula: floor(sum(top comp_period earnings) / (comp_period * 12))
     compute_aime <- function(t_years) {
       if (t_years < 1) return(0)

       earnings_so_far <- indexed_earnings[1:t_years]
       years_to_use <- min(t_years, comp_period)

       if (t_years <= comp_period) {
         # Use all earnings, zero-fill for missing years
         top_earnings_sum <- sum(earnings_so_far, na.rm = TRUE)
       } else {
         # Use top comp_period years
         top_earnings <- sort(earnings_so_far, decreasing = TRUE)[1:comp_period]
         top_earnings_sum <- sum(top_earnings, na.rm = TRUE)
       }

       # Per 42 USC 415(b): AIME rounded DOWN to next lowest dollar
       floor(top_earnings_sum / (comp_period * 12))
     }

     # Helper function to compute years of coverage for special minimum
     # Counts years where earnings >= yoc_threshold
     compute_yoc <- function(t_years) {
       if (t_years < 1 || all(is.na(yoc_thresholds[1:t_years]))) return(0)
       sum(nominal_earnings[1:t_years] >= yoc_thresholds[1:t_years], na.rm = TRUE)
     }

     # Helper function to compute PIA given AIME and years of coverage
     # Returns max of regular PIA and special minimum PIA per 42 USC 415(a)(1)
     # Supports reform parameters: 4th bracket (bp3/fact4), PIA multiplier, flat benefit
     compute_pia <- function(aime, yoc) {
       # Regular PIA: bend point formula per 42 USC 415(a)(1)(A)
       # Standard: 90/32/15 (3 brackets)
       # Reform: 90/32/15/X (4 brackets) when bp3 and fact4 are specified
       regular_pia <- if (!is.na(bp3) && !is.na(fact4) && aime > bp3) {
         # 4-bracket formula (Reform #3, #12-14)
         fact1 * bp1 + fact2 * (bp2 - bp1) + fact3 * (bp3 - bp2) + fact4 * (aime - bp3)
       } else if (aime <= bp1) {
         fact1 * aime
       } else if (aime <= bp2) {
         fact1 * bp1 + fact2 * (aime - bp1)
       } else {
         fact1 * bp1 + fact2 * (bp2 - bp1) + fact3 * (aime - bp2)
       }

       # Floor to dime per 42 USC 415(a)(2)(C)
       regular_pia <- floor(regular_pia * 10) / 10

       # Apply PIA multiplier (Reform #1) - across-the-board benefit changes
       regular_pia <- floor(regular_pia * pia_multiplier * 10) / 10

       # Apply flat benefit floor (Reform #2) if specified
       if (!is.na(flat_benefit) && flat_benefit > 0) {
         regular_pia <- max(regular_pia, flat_benefit)
       }

       # Special minimum PIA per 42 USC 415(a)(1)(C)(i)
       # Only if years_of_coverage >= min_yoc_elig (typically 11)
       special_min_pia <- 0
       if (!is.na(special_min_rate_elig) && yoc >= min_yoc_elig) {
         special_min_pia <- floor(special_min_rate_elig * (yoc - 10) * 10) / 10
       }

       # Per 42 USC 415(a)(1): PIA is the HIGHER of regular or special minimum
       max(regular_pia, special_min_pia)
     }

     # Compute marginal benefits for each working year
     for (t in seq_len(n_working)) {
       idx <- working_idx[t]
       working_year <- w$year[idx]

       w$years_worked[idx] <- t
       w$qcs[idx] <- 4 * t
       w$eligible[idx] <- (4 * t) >= 40
       w$indexed_rank[idx] <- all_ranks[t]
       w$in_top_35[idx] <- all_ranks[t] <= comp_period

       # Get nominal discount factor for this working year
       df_idx <- which(assumptions$year == working_year)
       if (length(df_idx) > 0) {
         df_working <- assumptions$df[df_idx[1]]
       } else {
         df_working <- NA_real_
       }

       if (w$qcs[idx] < 40) {
         # Not yet eligible for own benefits, but may receive spousal benefits
         w$cumulative_aime[idx] <- 0
         w$cumulative_pia[idx] <- 0

         # Spousal benefits don't require QCs - compute PV even for pre-eligibility
         pv_t <- compute_pv(0, df_working, t)
         pv_t_minus_1 <- compute_pv(0, df_working, t - 1)
         w$cumulative_pv[idx] <- pv_t
         w$delta_pv_benefits[idx] <- pv_t - pv_t_minus_1
       } else {
         # Compute AIME and years of coverage with t years
         aime_t <- compute_aime(t)
         yoc_t <- compute_yoc(t)
         pia_t <- compute_pia(aime_t, yoc_t)

         # Compute PIA with t-1 years (for delta calculation)
         aime_t_minus_1 <- compute_aime(t - 1)
         yoc_t_minus_1 <- compute_yoc(t - 1)
         pia_t_minus_1 <- compute_pia(aime_t_minus_1, yoc_t_minus_1)

         # Compute PV of benefits, both discounted to THIS working year
         # This ensures delta_pv is in the same dollars as earnings/taxes
         pv_t <- compute_pv(pia_t, df_working, t)
         pv_t_minus_1 <- compute_pv(pia_t_minus_1, df_working, t - 1)

         w$cumulative_aime[idx] <- aime_t
         w$cumulative_pia[idx] <- pia_t
         w$cumulative_pv[idx] <- pv_t
         w$delta_pv_benefits[idx] <- pv_t - pv_t_minus_1
       }
     }

     w
   }) %>%
   ungroup()

 # Select relevant columns to return
 output_cols <- c("id", "year", "age", "earnings", "indexed_earn",
                  "years_worked", "qcs", "eligible",
                  "cumulative_aime", "cumulative_pia", "cumulative_pv",
                  "delta_pv_benefits", "in_top_35", "indexed_rank")
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
#'     \item \code{in_top_35}: Whether this year is in top 35 indexed earnings
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
net_marginal_tax_rate <- function(worker, assumptions, include_employer = FALSE) {

  # Get marginal benefit analysis (using cumulative stopping-point method)
  # Benefits are discounted to each working year in nominal dollars
  marginal <- marginal_benefit_analysis(worker, assumptions)

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
                   "ss_tax", "delta_pv_benefits", "net_marginal_tax_rate", "in_top_35")
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
#'     \item \code{in_top_35}: Whether year is in top 35 indexed earnings
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
  output_cols <- c("id", "year", "age", "earnings", "ss_tax", "in_top_35",
                   "delta_pv_benefits", "marginal_irr")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}

