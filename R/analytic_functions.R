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
#' Computes per-year marginal benefit analysis for a worker. For each working year,
#' calculates whether the year is in the top 35 indexed earnings years, the marginal
#' AIME rate, the marginal PIA rate, and the present value of additional lifetime
#' benefits from an additional dollar of earnings.
#'
#' @param worker Data frame with calculated benefits. Must contain columns from
#'   \code{calculate_benefits()} run with \code{debugg = TRUE}: \code{id}, \code{year},
#'   \code{age}, \code{earnings}, \code{indexed_earn}, \code{index_factor}, \code{aime},
#'   \code{claim_age}, \code{death_age}, \code{bp1_elig}, \code{bp2_elig}, \code{comp_period}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi}, \code{real_df}.
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
#'
#' @return Data frame with the worker data plus additional columns:
#'   \itemize{
#'     \item \code{in_top_35}: Logical, is this year in the worker's top 35 indexed earning years?
#'     \item \code{indexed_rank}: Rank of this year's indexed earnings (1 = highest)
#'     \item \code{marginal_pia_rate}: PIA bracket rate for marginal AIME (0.90, 0.32, or 0.15)
#'     \item \code{delta_aime_per_dollar}: AIME increase per additional dollar (index_factor / 420, or 0)
#'     \item \code{delta_pia_per_dollar}: PIA increase per additional dollar of earnings
#'     \item \code{delta_pv_benefits}: PV of lifetime benefit increase from $1 more earnings
#'   }
#'
#' @details
#' The marginal analysis answers: "If I earn $1 more this year, how much more
#' will I receive in lifetime Social Security benefits?"
#'
#' Key insights:
#' \itemize{
#'   \item Years not in the top 35 indexed earnings have zero marginal benefit
#'   \item Marginal AIME = index_factor / (35 × 12) = index_factor / 420
#'   \item Marginal PIA depends on which PIA bracket the marginal AIME falls into
#'   \item Delta PV benefits = delta_monthly_pia × actuarial_factor × months_of_benefits × discount_factor
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' marginal <- marginal_benefit_analysis(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by mutate arrange row_number first ungroup left_join select
#' @export
marginal_benefit_analysis <- function(worker, assumptions, base_year = 2025) {

  # Validate required columns - need debugg = TRUE output
  required_cols <- c("id", "year", "age", "earnings", "indexed_earn", "index_factor",
                     "aime", "claim_age", "death_age")
  if (!all(required_cols %in% names(worker))) {
    missing <- required_cols[!required_cols %in% names(worker)]
    stop(paste("worker data must contain:", paste(missing, collapse = ", "),
               "\nDid you run calculate_benefits() with debugg = TRUE?"))
  }

  # Check for PIA bend points
  if (!"bp1_elig" %in% names(worker) || !"bp2_elig" %in% names(worker)) {
    stop("worker data must contain 'bp1_elig' and 'bp2_elig' columns (run with debugg = TRUE)")
  }

  # Check for comp_period (computation period, typically 35 years)
  if (!"comp_period" %in% names(worker)) {
    # Default to 35 if not present
    worker$comp_period <- 35
  }

  # Get assumption columns for discounting
  assumption_cols <- c("gdp_pi", "real_df")
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

  # Calculate marginal analysis for each worker
  result <- worker %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Get computation period for this worker
      comp_period_val = first(comp_period[!is.na(comp_period)]),

      # Rank indexed earnings (1 = highest) - only for working years (ages 21-64)
      # Need to handle years before claim age (when indexed_earn is calculated)
      working_year = age >= 21 & age <= 64,

      # Create rank based on indexed earnings for working years
      indexed_rank = if_else(
        working_year,
        as.numeric(rank(-indexed_earn[working_year], ties.method = "first")[cumsum(working_year)]),
        NA_real_
      )
    ) %>%
    # Recalculate rank correctly using group_modify
    ungroup() %>%
    group_by(id) %>%
    group_modify(~ {
      # Get working years
      working_idx <- which(.x$age >= 21 & .x$age <= 64)
      n_working <- length(working_idx)

      # Initialize rank column
      .x$indexed_rank <- NA_real_

      if (n_working > 0) {
        # Rank the indexed earnings (descending - 1 = highest)
        indexed_vals <- .x$indexed_earn[working_idx]
        ranks <- rank(-indexed_vals, ties.method = "first")
        .x$indexed_rank[working_idx] <- ranks
      }

      .x
    }) %>%
    ungroup() %>%
    group_by(id) %>%
    mutate(
      # Get computation period for this worker (typically 35)
      comp_period_val = first(comp_period[!is.na(comp_period)]),

      # Is this year in the top N (computation period) indexed earning years?
      in_top_35 = !is.na(indexed_rank) & indexed_rank <= comp_period_val,

      # AIME at claim age (for determining marginal PIA rate)
      aime_at_claim = first(aime[age == first(claim_age)]),
      bp1_val = first(bp1_elig[!is.na(bp1_elig)]),
      bp2_val = first(bp2_elig[!is.na(bp2_elig)]),

      # Marginal PIA rate based on where AIME falls in the bend point formula
      # If AIME < bp1: marginal rate = 0.90
      # If bp1 <= AIME < bp2: marginal rate = 0.32
      # If AIME >= bp2: marginal rate = 0.15
      marginal_pia_rate = case_when(
        aime_at_claim < bp1_val ~ 0.90,
        aime_at_claim < bp2_val ~ 0.32,
        TRUE ~ 0.15
      ),

      # Delta AIME per dollar of additional earnings
      # AIME = sum(top 35 indexed earnings) / (35 * 12)
      # Marginal AIME = index_factor / (35 * 12) = index_factor / 420
      # Only applies if this year would be in the top 35
      delta_aime_per_dollar = if_else(
        in_top_35,
        index_factor / (comp_period_val * 12),
        0
      ),

      # Delta PIA per dollar (monthly PIA increase)
      delta_pia_per_dollar = delta_aime_per_dollar * marginal_pia_rate,

      # Calculate PV of additional lifetime benefits from $1 more earnings
      # Need: number of benefit years, actuarial adjustment, discount factor

      # Get actuarial factor (from worker_benefit calculation if available)
      claim_age_val = first(claim_age[!is.na(claim_age)]),
      death_age_val = first(death_age[!is.na(death_age)]),

      # Number of months of benefits
      benefit_months = (death_age_val - claim_age_val) * 12,

      # Discount factor from current age to age 65 (for normalization)
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + 65,
      real_df_norm = real_df[which(year == discount_year)][1],
      real_df_at_claim = real_df[which(year == birth_yr + claim_age_val)][1],

      # Price index for converting to real dollars
      gdp_pi_at_claim = gdp_pi[which(year == birth_yr + claim_age_val)][1]
    ) %>%
    # Calculate delta_pv_benefits using group_modify for complex calculation
    group_modify(~ {
      # Get actuarial factor if available
      if ("act_factor" %in% names(.x)) {
        act_factor_val <- .x$act_factor[which(.x$age == .x$claim_age_val[1])][1]
        if (is.na(act_factor_val)) act_factor_val <- 1.0
      } else {
        act_factor_val <- 1.0
      }

      # Calculate PV of lifetime benefit increase
      # Delta annual benefit = delta_pia_per_dollar * 12 * actuarial_factor
      # PV = sum over all benefit years, discounted

      claim_age <- .x$claim_age_val[1]
      death_age <- .x$death_age_val[1]
      birth_yr <- .x$birth_yr[1]
      gdp_pi_base_val <- gdp_pi_base

      # For each working year, calculate PV of benefit increase
      .x$delta_pv_benefits <- NA_real_

      for (i in seq_len(nrow(.x))) {
        if (!is.na(.x$in_top_35[i]) && .x$in_top_35[i]) {
          # Annual benefit increase in real dollars
          delta_annual_benefit <- .x$delta_pia_per_dollar[i] * 12 * act_factor_val

          # Sum PV over all benefit years
          pv_sum <- 0
          for (ben_age in seq(ceiling(claim_age), floor(death_age) - 1)) {
            ben_year <- birth_yr + ben_age
            # Get discount factor for this year
            df_idx <- which(.x$year == ben_year)
            if (length(df_idx) > 0) {
              real_df_ben <- .x$real_df[df_idx[1]]
              gdp_pi_ben <- .x$gdp_pi[df_idx[1]]

              if (!is.na(real_df_ben) && !is.na(gdp_pi_ben) && !is.na(.x$real_df_norm[1])) {
                # Convert to real dollars and discount
                real_benefit <- delta_annual_benefit * (gdp_pi_base_val / gdp_pi_ben)
                pv_factor <- .x$real_df_norm[1] / real_df_ben
                pv_sum <- pv_sum + real_benefit * pv_factor
              }
            }
          }
          .x$delta_pv_benefits[i] <- pv_sum
        } else if (.x$age[i] >= 21 && .x$age[i] <= 64) {
          .x$delta_pv_benefits[i] <- 0
        }
      }

      .x
    }) %>%
    ungroup()

  # Select relevant columns to return
  output_cols <- c("id", "year", "age", "earnings", "indexed_earn", "index_factor",
                   "in_top_35", "indexed_rank", "marginal_pia_rate",
                   "delta_aime_per_dollar", "delta_pia_per_dollar", "delta_pv_benefits")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}


#' Calculate Net Marginal Tax Rate
#'
#' Computes the net marginal tax rate on Social Security for each working year.
#' The net marginal tax rate accounts for the marginal benefit accrued from
#' additional earnings.
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
#'     \item \code{ss_tax}: Social Security tax paid
#'     \item \code{delta_pv_benefits}: PV of marginal benefit accrual
#'     \item \code{net_marginal_tax_rate}: (ss_tax - delta_pv_benefits_total) / earnings
#'   }
#'
#' @details
#' The net marginal tax rate is calculated as:
#' \deqn{NMTR = \frac{SS\_tax - \Delta PV\_benefits\_total}{earnings}}
#'
#' Where \code{delta_pv_benefits_total} is the present value of additional lifetime
#' benefits from this year's earnings (computed as \code{delta_pv_benefits × earnings},
#' where \code{delta_pv_benefits} from \code{marginal_benefit_analysis()} is per-dollar).
#'
#' Interpretation:
#' \itemize{
#'   \item Value near 0.062 (6.2%) or 0.124 (12.4% with employer) means no benefit accrual — pure tax
#'   \item Value near 0 means benefits offset most of the tax
#'   \item Negative value means benefits exceed taxes (common for workers in the
#'     90% or 32% PIA brackets where marginal benefits are substantial)
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' nmtr <- net_marginal_tax_rate(worker, tr2025)
#' }
#'
#' @export
net_marginal_tax_rate <- function(worker, assumptions, include_employer = FALSE,
                                   base_year = 2025) {

  # Get marginal benefit analysis
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

      # Convert delta_pv_benefits from per-dollar to total dollars
      delta_pv_benefits_total = delta_pv_benefits * earnings,

      # Net marginal tax rate
      # NMTR = (tax - marginal benefit) / earnings
      net_marginal_tax_rate = if_else(
        earnings > 0 & age >= 21 & age <= 64,
        (ss_tax_total - delta_pv_benefits_total) / earnings,
        NA_real_
      )
    )

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings", "ss_tax", "delta_pv_benefits",
                   "net_marginal_tax_rate")
  if (include_employer) {
    output_cols <- c(output_cols[1:5], "ss_tax_total", output_cols[6:7])
  }
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}


#' Calculate Marginal Internal Rate of Return
#'
#' Computes the marginal IRR for each working year. This is the IRR on that
#' year's tax contribution, considering only the marginal benefit accrual
#' from that year's earnings.
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
#'     \item \code{marginal_irr}: IRR on this year's tax contribution
#'   }
#'
#' @details
#' For each working year, the marginal IRR is the rate r that solves:
#' \deqn{tax_t = \sum_{y} \frac{\Delta benefit_y}{(1+r)^{(y - t)}}}
#'
#' When the year is NOT in the top 35 indexed earnings (marginal benefit = 0),
#' the marginal IRR is -1 (representing -100% return — complete loss of the
#' tax contribution).
#'
#' This calculation can be slow for many workers as it requires solving for r
#' numerically for each working year.
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' mirr <- marginal_irr(worker, tr2025)
#' }
#'
#' @importFrom stats uniroot
#' @export
marginal_irr <- function(worker, assumptions, include_employer = FALSE,
                          base_year = 2025) {

  # Get marginal benefit analysis and taxes
  marginal <- marginal_benefit_analysis(worker, assumptions, base_year)

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

  # Get worker metadata
  worker_meta <- worker %>%
    group_by(id) %>%
    summarise(
      claim_age = first(claim_age[!is.na(claim_age)]),
      death_age = first(death_age[!is.na(death_age)]),
      birth_yr = first(year) - first(age),
      .groups = "drop"
    )

  combined <- combined %>%
    left_join(worker_meta, by = "id")

  # Get actuarial factor if available
  if ("act_factor" %in% names(worker)) {
    act_factors <- worker %>%
      group_by(id) %>%
      summarise(
        act_factor_val = first(act_factor[!is.na(act_factor) & act_factor > 0]),
        .groups = "drop"
      )
    combined <- combined %>%
      left_join(act_factors, by = "id") %>%
      mutate(act_factor_val = if_else(is.na(act_factor_val), 1.0, act_factor_val))
  } else {
    combined$act_factor_val <- 1.0
  }

  # Calculate marginal IRR for each working year
  result <- combined %>%
    group_by(id) %>%
    group_modify(~ {
      n <- nrow(.x)
      mirr_vals <- rep(NA_real_, n)

      for (i in seq_len(n)) {
        age_i <- .x$age[i]
        year_i <- .x$year[i]

        # Only calculate for working years
        if (age_i >= 21 && age_i <= 64 && !is.na(.x$ss_tax_total[i]) && .x$ss_tax_total[i] > 0) {

          # If not in top 35, return is -100% (complete loss)
          if (!.x$in_top_35[i]) {
            mirr_vals[i] <- -1.0
          } else {
            # Calculate marginal IRR
            tax_i <- .x$ss_tax_total[i]
            delta_pia_i <- .x$delta_pia_per_dollar[i]
            act_factor <- .x$act_factor_val[i]
            claim_age <- .x$claim_age[i]
            death_age <- .x$death_age[i]
            earnings_i <- .x$earnings[i]

            # Skip if no earnings or delta_pia
            if (is.na(earnings_i) || earnings_i == 0 || is.na(delta_pia_i) || delta_pia_i == 0) {
              mirr_vals[i] <- NA_real_
              next
            }

            # Marginal annual benefit from this year's earnings
            # = delta_pia_per_dollar * earnings * 12 * actuarial_factor
            delta_annual_benefit <- delta_pia_i * earnings_i * 12 * act_factor

            # Benefit years
            ben_ages <- seq(ceiling(claim_age), floor(death_age) - 1)

            if (length(ben_ages) == 0) {
              mirr_vals[i] <- NA_real_
              next
            }

            # Define NPV function for this year's contribution
            npv_func <- function(r) {
              # PV of marginal benefits
              pv_benefits <- sum(
                delta_annual_benefit / (1 + r)^(ben_ages - age_i),
                na.rm = TRUE
              )
              return(pv_benefits - tax_i)
            }

            # Find IRR using uniroot
            mirr_vals[i] <- tryCatch({
              result <- uniroot(npv_func, interval = c(-0.99, 2.0), tol = 1e-8)
              result$root
            }, error = function(e) {
              NA_real_
            })
          }
        }
      }

      .x$marginal_irr <- mirr_vals
      .x
    }) %>%
    ungroup()

  # Select output columns
  output_cols <- c("id", "year", "age", "earnings", "ss_tax", "in_top_35", "marginal_irr")
  available_cols <- intersect(output_cols, names(result))

  return(result %>% select(all_of(available_cols)))
}

