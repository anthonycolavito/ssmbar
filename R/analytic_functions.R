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
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}: \code{id}, \code{year},
#'   \code{age}, \code{earnings}, \code{indexed_earn}, \code{index_factor}, \code{aime},
#'   \code{claim_age}, \code{death_age}, \code{bp1_elig}, \code{bp2_elig}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi}, \code{real_df}, \code{cola}.
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
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
#'     \item \code{cumulative_pv}: PV of lifetime benefits if stopped at this year
#'     \item \code{delta_pv_benefits}: Change in PV from working this year
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
#' Key insights:
#' \itemize{
#'   \item Years 1-9: Worker not yet eligible (< 40 QCs), so delta_pv = 0
#'   \item Year 10: Worker becomes eligible, large positive delta_pv
#'   \item Years 11-35: Each year adds to AIME numerator (denominator fixed at 420)
#'   \item Years 36+: Only adds value if displacing a lower-earning year from top 35
#' }
#'
#' The computation period (35 years) is fixed based on eligibility age, NOT on
#' years worked. AIME = sum(top 35 indexed earnings, with zeros if < 35 years) / 420.
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
marginal_benefit_analysis <- function(worker, assumptions, base_year = 2025) {

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

 # Get assumption columns
 assumption_cols <- c("gdp_pi", "real_df", "cola")
 cols_missing <- assumption_cols[!assumption_cols %in% names(worker)]
 if (length(cols_missing) > 0) {
   worker <- worker %>%
     left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
 }

 # Get base year price index for real conversion
 gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
 if (length(gdp_pi_base) == 0) {
   stop(paste("base_year", base_year, "not found in assumptions"))
 }

 # Get PIA factors (typically 0.90, 0.32, 0.15)
 fact1 <- 0.90
 fact2 <- 0.32
 fact3 <- 0.15

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

     # Get actuarial factor if available
     if ("act_factor" %in% names(w)) {
       act_factor <- w$act_factor[which(w$age == claim_age)[1]]
       if (is.na(act_factor) || length(act_factor) == 0) act_factor <- 1.0
     } else {
       act_factor <- 1.0
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

     # Get indexed earnings for working years
     indexed_earnings <- w$indexed_earn[working_idx]

     # Get discount factor at age 65 for PV normalization
     real_df_65 <- w$real_df[which(w$year == birth_yr + 65)]
     if (length(real_df_65) == 0) real_df_65 <- 1

     # Pre-compute COLA factors and discount factors for each benefit year
     # This is the same regardless of AIME/PIA, so we compute once
     benefit_ages <- seq(ceiling(claim_age), floor(death_age) - 1)
     n_benefit_years <- length(benefit_ages)

     # Build arrays of cumulative COLA and discount factors
     cola_factors <- numeric(n_benefit_years)
     discount_factors <- numeric(n_benefit_years)
     price_deflators <- numeric(n_benefit_years)

     cumulative_cola <- 1.0
     for (i in seq_along(benefit_ages)) {
       ben_age <- benefit_ages[i]
       ben_year <- birth_yr + ben_age

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

       # Get discount factor and price deflator
       df_idx <- which(w$year == ben_year)
       if (length(df_idx) > 0) {
         discount_factors[i] <- real_df_65 / w$real_df[df_idx[1]]
         price_deflators[i] <- gdp_pi_base / w$gdp_pi[df_idx[1]]
       } else {
         discount_factors[i] <- 1
         price_deflators[i] <- 1
       }
     }

     # Helper function to compute PV of lifetime benefits given a PIA
     compute_pv <- function(pia) {
       if (pia <= 0) return(0)

       # Worker benefit at claim = PIA × actuarial factor
       # (COLA from 62 to claim is already captured in cola_factors)
       worker_benefit_monthly <- pia * act_factor

       # Annual benefit at each age = monthly × 12 × COLA adjustment
       # Convert to real dollars and discount
       pv_sum <- 0
       for (i in seq_along(benefit_ages)) {
         annual_nominal <- worker_benefit_monthly * 12 * cola_factors[i]
         annual_real <- annual_nominal * price_deflators[i]
         pv_sum <- pv_sum + annual_real * discount_factors[i]
       }
       pv_sum
     }

     # Rank all working years by indexed earnings (for reporting)
     all_ranks <- rank(-indexed_earnings, ties.method = "first")

     # Compute cumulative PV at each stopping point
     cumulative_pv_prev <- 0

     for (t in seq_len(n_working)) {
       idx <- working_idx[t]
       w$years_worked[idx] <- t
       w$qcs[idx] <- 4 * t
       w$eligible[idx] <- (4 * t) >= 40
       w$indexed_rank[idx] <- all_ranks[t]
       w$in_top_35[idx] <- all_ranks[t] <= 35

       if (w$qcs[idx] < 40) {
         # Not yet eligible
         w$cumulative_aime[idx] <- 0
         w$cumulative_pia[idx] <- 0
         w$cumulative_pv[idx] <- 0
         w$delta_pv_benefits[idx] <- 0
       } else {
         # Eligible - compute AIME with first t years of indexed earnings
         # AIME = sum(top 35 indexed earnings, padding with zeros) / 420
         earnings_so_far <- indexed_earnings[1:t]

         if (t <= 35) {
           # All earnings count (plus implicit zeros)
           aime <- sum(earnings_so_far, na.rm = TRUE) / 420
         } else {
           # Take top 35 only
           top_35 <- sort(earnings_so_far, decreasing = TRUE)[1:35]
           aime <- sum(top_35, na.rm = TRUE) / 420
         }

         # Compute PIA using bend point formula
         pia <- if (aime <= bp1) {
           fact1 * aime
         } else if (aime <= bp2) {
           fact1 * bp1 + fact2 * (aime - bp1)
         } else {
           fact1 * bp1 + fact2 * (bp2 - bp1) + fact3 * (aime - bp2)
         }

         # Floor to dime (SSA rule)
         pia <- floor(pia * 10) / 10

         # Compute PV using the full benefit calculation
         cumulative_pv <- compute_pv(pia)

         w$cumulative_aime[idx] <- aime
         w$cumulative_pia[idx] <- pia
         w$cumulative_pv[idx] <- cumulative_pv
         w$delta_pv_benefits[idx] <- cumulative_pv - cumulative_pv_prev

         cumulative_pv_prev <- cumulative_pv
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
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#' @param include_employer Logical. If TRUE, includes employer share of taxes
#'   (12.4% total). Default is FALSE (6.2% employee share only).
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
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
#'     \item \code{delta_pv_benefits}: Change in PV of lifetime benefits from this year
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
net_marginal_tax_rate <- function(worker, assumptions, include_employer = FALSE,
                                   base_year = 2025) {

  # Get marginal benefit analysis (using cumulative stopping-point method)
  marginal <- marginal_benefit_analysis(worker, assumptions, base_year)

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
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
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
#' stopping-point method. Since delta_pv_benefits is already the present value
#' of additional lifetime benefits (discounted to age 65), the IRR is the rate
#' that makes the tax contribution grow to equal this benefit gain:
#'
#' \deqn{tax_t \times (1 + r)^{(65 - t)} = \Delta PV\_benefits}
#'
#' Solving for r:
#' \deqn{r = (\Delta PV\_benefits / tax_t)^{1/(65-t)} - 1}
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
marginal_irr <- function(worker, assumptions, include_employer = FALSE,
                          base_year = 2025) {

  # Get marginal benefit analysis (using cumulative stopping-point method)
  marginal <- marginal_benefit_analysis(worker, assumptions, base_year)

  # Calculate taxes
  worker_with_taxes <- calculate_taxes(worker, assumptions)

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
  # IRR = (delta_pv_benefits / tax)^(1/(65-age)) - 1
  result <- combined %>%
    mutate(
      marginal_irr = case_when(
        # Not a working year
        age < 21 | age > 64 ~ NA_real_,

        # No tax paid
        is.na(ss_tax_total) | ss_tax_total <= 0 ~ NA_real_,

        # No benefit gain (not eligible, or not in top 35)
        is.na(delta_pv_benefits) | delta_pv_benefits <= 0 ~ -1.0,

        # Normal case: compute IRR
        # tax * (1+r)^(65-age) = delta_pv_benefits
        # r = (delta_pv_benefits / tax)^(1/(65-age)) - 1
        TRUE ~ (delta_pv_benefits / ss_tax_total)^(1 / (65 - age)) - 1
      )
    )

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings", "ss_tax", "in_top_35",
                   "delta_pv_benefits", "marginal_irr")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}

