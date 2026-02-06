# =============================================================================
# PRESENT VALUE ANALYTICS FUNCTIONS
# =============================================================================
#
# This file contains functions for calculating present values of lifetime
# Social Security benefits and taxes, as well as benefit-tax ratios.
#
# These functions are separate from benefit calculation code and are designed
# for use with the Benefit Explorer Shiny app and analytical work.
#
# =============================================================================


#' Calculate Present Value of Lifetime Benefits (Real 2025 Dollars)
#'
#' Calculates the present value of lifetime Social Security benefits from
#' claim age to expected death age. Benefits are first converted to real
#' 2025 dollars using the GDP price index, then discounted using the real
#' discount factor from the Trustees Report assumptions.
#'
#' @param worker Data frame with calculated benefits. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{annual_ind} (annual benefit),
#'   \code{claim_age}, and \code{death_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi} (GDP price index),
#'   and \code{real_df} (real discount factor).
#' @param discount_to_age Numeric value specifying the age to which benefits
#'   are discounted. Default is 65.
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_benefits}: Present value of lifetime benefits in real 2025 dollars
#'   }
#'
#' @details
#' The calculation proceeds in two steps:
#' 1. Convert nominal benefits to real 2025 dollars using GDP price index
#' 2. Discount real benefits using the real discount factor (real_df)
#'
#' This ensures the result is in real 2025 dollars, comparable to the
#' undiscounted real_lifetime_benefits() output.
#'
#' Benefits are summed from the worker's claim age through their expected death age
#' (based on cohort life expectancy). Benefits after death are excluded.
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#' pv_ben <- pv_lifetime_benefits(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join first
#' @export
pv_lifetime_benefits <- function(worker, assumptions, discount_to_age = 65, base_year = 2025) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "annual_ind")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Get claim_age and death_age - may be in worker data or need to be derived
  if (!"claim_age" %in% names(worker)) {
    worker <- worker %>%
      group_by(id) %>%
      mutate(claim_age = min(age[annual_ind > 0], na.rm = TRUE)) %>%
      ungroup()
  }

  if (!"death_age" %in% names(worker)) {
    stop("worker data must contain 'death_age' column")
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "gdp_pi", "real_df")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Get base year price index for real conversion

  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
  if (length(gdp_pi_base) == 0) {
    stop(paste("base_year", base_year, "not found in assumptions"))
  }

  # Join gdp_pi and real_df from assumptions if not already present
  cols_needed <- c("gdp_pi", "real_df")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]
  if (length(cols_missing) > 0) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate PV of lifetime benefits in real 2025 dollars
  # Includes partial year at death: fractional_year * annual_benefit
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      real_df_norm = real_df[which(year == discount_year)][1],
      floor_death_age = floor(first(death_age)),
      frac_death = first(death_age) - floor(first(death_age))
    ) %>%
    # Full years: claim_age to floor(death_age) - 1
    # Partial year: age == floor(death_age) gets fractional weight
    filter(age >= claim_age & age <= floor_death_age & annual_ind > 0) %>%
    mutate(
      # Apply fractional weight for the partial year at death
      year_weight = if_else(age == floor_death_age, frac_death, 1.0),
      # Step 1: Convert nominal to real 2025 dollars
      real_benefit = annual_ind * (gdp_pi_base / gdp_pi) * year_weight,
      # Step 2: Discount real benefits using real discount factor
      pv_factor = real_df_norm / real_df,
      pv_annual = real_benefit * pv_factor
    ) %>%
    summarise(
      pv_benefits = sum(pv_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Present Value of Lifetime Social Security Taxes (Real 2025 Dollars)
#'
#' Calculates the present value of lifetime Social Security taxes paid from
#' first working age (21) through age 64. Taxes are first converted to real
#' 2025 dollars using the GDP price index, then discounted using the real
#' discount factor from the Trustees Report assumptions.
#'
#' @param worker Data frame with worker earnings. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, and \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi}, \code{real_df},
#'   \code{oasi_tr}, \code{di_tr}, and \code{taxmax}.
#' @param discount_to_age Numeric value specifying the age to which taxes
#'   are discounted. Default is 65.
#' @param include_employer Logical. If TRUE, includes both employee and
#'   employer shares of payroll taxes (doubling the tax amount).
#'   Default is FALSE (employee share only).
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_taxes}: Present value of lifetime taxes in real 2025 dollars
#'   }
#'
#' @details
#' The calculation proceeds in two steps:
#' 1. Convert nominal taxes to real 2025 dollars using GDP price index
#' 2. Discount real taxes using the real discount factor (real_df)
#'
#' This ensures the result is in real 2025 dollars, comparable to other
#' real measures.
#'
#' @examples
#' \dontrun{
#' worker <- earnings_generator(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#' pv_tax <- pv_lifetime_taxes(worker, tr2025)
#' pv_tax_total <- pv_lifetime_taxes(worker, tr2025, include_employer = TRUE)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join mutate first
#' @export
pv_lifetime_taxes <- function(worker, assumptions, discount_to_age = 65,
                               include_employer = FALSE, base_year = 2025) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "gdp_pi", "real_df", "oasi_tr", "di_tr", "taxmax")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Get base year price index
  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
  if (length(gdp_pi_base) == 0) {
    stop(paste("base_year", base_year, "not found in assumptions"))
  }

  # Calculate taxes using existing function
  worker_with_taxes <- calculate_taxes(worker, assumptions)

  # Join gdp_pi and real_df from assumptions if not already present
  cols_needed <- c("gdp_pi", "real_df")
  cols_missing <- cols_needed[!cols_needed %in% names(worker_with_taxes)]
  if (length(cols_missing) > 0) {
    dataset <- worker_with_taxes %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker_with_taxes
  }

  # Calculate PV of lifetime taxes in real 2025 dollars (ages 21-64)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      real_df_norm = real_df[which(year == discount_year)][1]
    ) %>%
    filter(age >= 21 & age <= 64) %>%
    mutate(
      # Apply employer multiplier if requested
      tax_amount = if (include_employer) ss_tax * 2 else ss_tax,
      # Step 1: Convert nominal to real 2025 dollars
      real_tax = tax_amount * (gdp_pi_base / gdp_pi),
      # Step 2: Discount real taxes using real discount factor
      pv_factor = real_df_norm / real_df,
      pv_annual = real_tax * pv_factor
    ) %>%
    summarise(
      pv_taxes = sum(pv_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Benefit-Tax Ratio
#'
#' Calculates the ratio of present value of lifetime benefits to present
#' value of lifetime taxes.
#'
#' @param pv_benefits Numeric vector of present value of benefits, or a
#'   data frame with a \code{pv_benefits} column.
#' @param pv_taxes Numeric vector of present value of taxes, or a data
#'   frame with a \code{pv_taxes} column.
#'
#' @return Numeric vector of benefit-tax ratios.
#'
#' @details
#' The benefit-tax ratio indicates how many dollars of benefits are received
#' for each dollar of taxes paid, in present value terms. A ratio greater
#' than 1 indicates the worker receives more in benefits than they paid in
#' taxes; a ratio less than 1 indicates the opposite.
#'
#' Note: This ratio uses the real discount rate from Trustees assumptions,
#' which represents the government's cost of borrowing. Different discount
#' rates would yield different ratios.
#'
#' @examples
#' \dontrun{
#' # Calculate benefit-tax ratio
#' pv_ben <- pv_lifetime_benefits(worker, tr2025)
#' pv_tax <- pv_lifetime_taxes(worker, tr2025)
#' ratio <- benefit_tax_ratio(pv_ben, pv_tax)
#' }
#'
#' @export
benefit_tax_ratio <- function(pv_benefits, pv_taxes) {

  # Extract numeric values if data frames are provided
  if (is.data.frame(pv_benefits)) {
    if (!"pv_benefits" %in% names(pv_benefits)) {
      stop("pv_benefits data frame must contain 'pv_benefits' column")
    }
    pv_benefits <- pv_benefits$pv_benefits
  }

  if (is.data.frame(pv_taxes)) {
    if (!"pv_taxes" %in% names(pv_taxes)) {
      stop("pv_taxes data frame must contain 'pv_taxes' column")
    }
    pv_taxes <- pv_taxes$pv_taxes
  }

  # Calculate ratio, handling division by zero
  ratio <- ifelse(pv_taxes == 0, NA_real_, pv_benefits / pv_taxes)

  return(ratio)
}


#' Calculate Couple Measures
#'
#' Combines worker and spouse present value calculations and optionally
#' splits totals 50/50 for shared analysis.
#'
#' @param worker Data frame with worker data (must have been processed through
#'   \code{calculate_benefits()} with spouse information).
#' @param spouse Data frame with spouse data (must have been processed through
#'   \code{calculate_benefits()} independently, or NULL if using worker's spouse_spec).
#' @param assumptions Data frame with the prepared Trustees assumptions.
#' @param discount_to_age Numeric value specifying the age to which values
#'   are discounted. Default is 65.
#' @param include_employer Logical. If TRUE, includes employer share of taxes.
#'   Default is FALSE.
#' @param shared Logical. If TRUE, splits couple totals 50/50 between worker
#'   and spouse. Default is TRUE.
#'
#' @return A list with the following elements:
#'   \itemize{
#'     \item \code{worker_pv_benefits}: Worker's PV of benefits (individual)
#'     \item \code{worker_pv_taxes}: Worker's PV of taxes (individual)
#'     \item \code{spouse_pv_benefits}: Spouse's PV of benefits (individual)
#'     \item \code{spouse_pv_taxes}: Spouse's PV of taxes (individual)
#'     \item \code{couple_pv_benefits}: Total PV of benefits for couple
#'     \item \code{couple_pv_taxes}: Total PV of taxes for couple
#'     \item \code{worker_ratio}: Worker's benefit-tax ratio (individual or shared)
#'     \item \code{spouse_ratio}: Spouse's benefit-tax ratio (individual or shared)
#'     \item \code{couple_ratio}: Couple's benefit-tax ratio
#'     \item \code{shared}: Whether 50/50 split was applied
#'   }
#'
#' @details
#' When \code{shared = TRUE}, the couple's total benefits and taxes are
#' calculated and then split 50/50 between worker and spouse. This reflects
#' a household perspective where Social Security is viewed as a joint benefit.
#'
#' When \code{shared = FALSE}, individual measures are returned without
#' modification.
#'
#' @examples
#' \dontrun{
#' # Calculate benefits for worker with spouse
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
#'   spouse_type = "low", spouse_sex = "female",
#'   spouse_birth_yr = 1962, spouse_age_claim = 65,
#'   factors = sef2025, assumptions = tr2025
#' )
#'
#' # Calculate spouse benefits independently
#' spouse <- calculate_benefits(
#'   birth_yr = 1962, sex = "female", type = "low", age_claim = 65,
#'   factors = sef2025, assumptions = tr2025
#' )
#'
#' # Get couple measures with 50/50 split
#' couple <- couple_measures(worker, spouse, tr2025, shared = TRUE)
#' }
#'
#' @export
couple_measures <- function(worker, spouse = NULL, assumptions,
                             discount_to_age = 65, include_employer = FALSE,
                             shared = TRUE) {

  # Calculate worker measures
  worker_pv_ben <- pv_lifetime_benefits(worker, assumptions, discount_to_age)
  worker_pv_tax <- pv_lifetime_taxes(worker, assumptions, discount_to_age, include_employer)

  worker_benefits <- worker_pv_ben$pv_benefits[1]
  worker_taxes <- worker_pv_tax$pv_taxes[1]

  # Handle spouse calculations
  if (is.null(spouse)) {
    # No spouse - return individual measures only
    return(list(
      worker_pv_benefits = worker_benefits,
      worker_pv_taxes = worker_taxes,
      spouse_pv_benefits = NA_real_,
      spouse_pv_taxes = NA_real_,
      couple_pv_benefits = worker_benefits,
      couple_pv_taxes = worker_taxes,
      worker_ratio = benefit_tax_ratio(worker_benefits, worker_taxes),
      spouse_ratio = NA_real_,
      couple_ratio = benefit_tax_ratio(worker_benefits, worker_taxes),
      shared = FALSE
    ))
  }

  # Calculate spouse measures
  spouse_pv_ben <- pv_lifetime_benefits(spouse, assumptions, discount_to_age)
  spouse_pv_tax <- pv_lifetime_taxes(spouse, assumptions, discount_to_age, include_employer)

  spouse_benefits <- spouse_pv_ben$pv_benefits[1]
  spouse_taxes <- spouse_pv_tax$pv_taxes[1]

  # Calculate couple totals
  couple_benefits <- worker_benefits + spouse_benefits
  couple_taxes <- worker_taxes + spouse_taxes

  if (shared) {
    # 50/50 split
    shared_benefits <- couple_benefits / 2
    shared_taxes <- couple_taxes / 2

    return(list(
      worker_pv_benefits = worker_benefits,
      worker_pv_taxes = worker_taxes,
      spouse_pv_benefits = spouse_benefits,
      spouse_pv_taxes = spouse_taxes,
      couple_pv_benefits = couple_benefits,
      couple_pv_taxes = couple_taxes,
      worker_ratio = benefit_tax_ratio(shared_benefits, shared_taxes),
      spouse_ratio = benefit_tax_ratio(shared_benefits, shared_taxes),
      couple_ratio = benefit_tax_ratio(couple_benefits, couple_taxes),
      shared = TRUE
    ))
  } else {
    # Individual measures without splitting
    return(list(
      worker_pv_benefits = worker_benefits,
      worker_pv_taxes = worker_taxes,
      spouse_pv_benefits = spouse_benefits,
      spouse_pv_taxes = spouse_taxes,
      couple_pv_benefits = couple_benefits,
      couple_pv_taxes = couple_taxes,
      worker_ratio = benefit_tax_ratio(worker_benefits, worker_taxes),
      spouse_ratio = benefit_tax_ratio(spouse_benefits, spouse_taxes),
      couple_ratio = benefit_tax_ratio(couple_benefits, couple_taxes),
      shared = FALSE
    ))
  }
}


#' Calculate Real Lifetime Benefits
#'
#' Calculates the sum of lifetime Social Security benefits in constant 2025
#' dollars, deflated using the GDP price index from the Trustees Report assumptions.
#'
#' @param worker Data frame with calculated benefits. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{annual_ind} (annual benefit),
#'   \code{claim_age}, and \code{death_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year} and \code{gdp_pi} (GDP price index).
#' @param base_year Numeric value specifying the year to which benefits
#'   are deflated. Default is 2025.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{real_benefits}: Sum of real lifetime benefits in base_year dollars
#'   }
#'
#' @details
#' Real benefits are calculated by deflating nominal benefits to constant
#' dollars using the GDP price index. Benefits are normalized to the specified
#' \code{base_year} (default 2025) for comparability across workers with
#' different birth years.
#'
#' The formula for each year's real benefit is:
#' \code{annual_benefit * (gdp_pi_base_year / gdp_pi_benefit_year)}
#'
#' Benefits are summed from the worker's claim age through their expected death age
#' (based on cohort life expectancy). Benefits after death are excluded.
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#' real_ben <- real_lifetime_benefits(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join first mutate
#' @export
real_lifetime_benefits <- function(worker, assumptions, base_year = 2025) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "annual_ind")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Get claim_age and death_age - may be in worker data or need to be derived
  if (!"claim_age" %in% names(worker)) {
    worker <- worker %>%
      group_by(id) %>%
      mutate(claim_age = min(age[annual_ind > 0], na.rm = TRUE)) %>%
      ungroup()
  }

  if (!"death_age" %in% names(worker)) {
    stop("worker data must contain 'death_age' column")
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "gdp_pi")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Get base year price index
  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
  if (length(gdp_pi_base) == 0) {
    stop(paste("base_year", base_year, "not found in assumptions"))
  }

  # Join gdp_pi from assumptions if not already present
  if (!"gdp_pi" %in% names(worker)) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, gdp_pi), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate real lifetime benefits (with partial year at death)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      floor_death_age = floor(first(death_age)),
      frac_death = first(death_age) - floor(first(death_age))
    ) %>%
    filter(age >= claim_age & age <= floor_death_age & annual_ind > 0) %>%
    mutate(
      # Apply fractional weight for the partial year at death
      year_weight = if_else(age == floor_death_age, frac_death, 1.0),
      # Deflate benefits to base year constant dollars
      price_scalar = gdp_pi_base / gdp_pi,
      real_annual = annual_ind * price_scalar * year_weight
    ) %>%
    summarise(
      real_benefits = sum(real_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Real Lifetime Earnings
#'
#' Calculates the sum of lifetime earnings in constant 2025 dollars,
#' deflated using the GDP price index from the Trustees Report assumptions.
#'
#' @param worker Data frame with worker earnings. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, and \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year} and \code{gdp_pi} (GDP price index).
#' @param base_year Numeric value specifying the year to which earnings
#'   are deflated. Default is 2025.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{real_earnings}: Sum of real lifetime earnings in base_year dollars
#'   }
#'
#' @details
#' Real earnings are calculated by deflating nominal earnings to constant
#' dollars using the GDP price index. Earnings are normalized to the specified
#' \code{base_year} (default 2025) for comparability across workers with
#' different birth years.
#'
#' The formula for each year's real earnings is:
#' \code{earnings * (gdp_pi_base_year / gdp_pi_earnings_year)}
#'
#' Earnings are summed from age 21 through age 64 (working years).
#'
#' @examples
#' \dontrun{
#' worker <- earnings_generator(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#' real_earn <- real_lifetime_earnings(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join first mutate
#' @export
real_lifetime_earnings <- function(worker, assumptions, base_year = 2025) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "gdp_pi")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Get base year price index
  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
  if (length(gdp_pi_base) == 0) {
    stop(paste("base_year", base_year, "not found in assumptions"))
  }

  # Join gdp_pi from assumptions if not already present
  if (!"gdp_pi" %in% names(worker)) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, gdp_pi), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate real lifetime earnings
  result <- dataset %>%
    group_by(id) %>%
    filter(age >= 21 & age <= 64) %>%
    mutate(
      # Deflate earnings to base year constant dollars
      price_scalar = gdp_pi_base / gdp_pi,
      real_annual = earnings * price_scalar
    ) %>%
    summarise(
      real_earnings = sum(real_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Present Value of Lifetime Earnings (Real 2025 Dollars)
#'
#' Calculates the present value of lifetime earnings from age 21 through
#' age 64. Earnings are first converted to real 2025 dollars using the
#' GDP price index, then discounted using the real discount factor from
#' the Trustees Report assumptions.
#'
#' @param worker Data frame with worker earnings. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, and \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{gdp_pi}, and \code{real_df}.
#' @param discount_to_age Numeric value specifying the age to which earnings
#'   are discounted. Default is 65.
#' @param base_year Numeric value specifying the year for real dollar conversion.
#'   Default is 2025.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_earnings}: Present value of lifetime earnings in real 2025 dollars
#'   }
#'
#' @details
#' The calculation proceeds in two steps:
#' 1. Convert nominal earnings to real 2025 dollars using GDP price index
#' 2. Discount real earnings using the real discount factor (real_df)
#'
#' This ensures the result is in real 2025 dollars, comparable to the
#' undiscounted real_lifetime_earnings() output.
#'
#' Earnings are summed from age 21 through age 64 (working years).
#'
#' @examples
#' \dontrun{
#' worker <- earnings_generator(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#' pv_earn <- pv_lifetime_earnings(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join first mutate
#' @export
pv_lifetime_earnings <- function(worker, assumptions, discount_to_age = 65, base_year = 2025) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "gdp_pi", "real_df")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Get base year price index
  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]
  if (length(gdp_pi_base) == 0) {
    stop(paste("base_year", base_year, "not found in assumptions"))
  }

  # Join gdp_pi and real_df from assumptions if not already present
  cols_needed <- c("gdp_pi", "real_df")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]
  if (length(cols_missing) > 0) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker
  }

  # Calculate PV of lifetime earnings in real 2025 dollars (ages 21-64)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      real_df_norm = real_df[which(year == discount_year)][1]
    ) %>%
    filter(age >= 21 & age <= 64) %>%
    mutate(
      # Step 1: Convert nominal to real 2025 dollars
      real_earn = earnings * (gdp_pi_base / gdp_pi),
      # Step 2: Discount real earnings using real discount factor
      pv_factor = real_df_norm / real_df,
      pv_annual = real_earn * pv_factor
    ) %>%
    summarise(
      pv_earnings = sum(pv_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Real Benefit-Earnings Ratio
#'
#' Calculates the ratio of real (price-deflated) lifetime benefits to
#' real lifetime earnings.
#'
#' @param real_benefits Numeric vector of real lifetime benefits, or a
#'   data frame with a \code{real_benefits} column.
#' @param real_earnings Numeric vector of real lifetime earnings, or a
#'   data frame with a \code{real_earnings} column.
#'
#' @return Numeric vector of real benefit-earnings ratios.
#'
#' @details
#' The real benefit-earnings ratio indicates how many constant dollars of
#' benefits are received for each constant dollar of earnings, both deflated
#' to the same base year. A ratio greater than 1 indicates lifetime benefits
#' exceed lifetime earnings in real terms.
#'
#' @examples
#' \dontrun{
#' real_ben <- real_lifetime_benefits(worker, tr2025)
#' real_earn <- real_lifetime_earnings(worker, tr2025)
#' ratio <- real_benefit_earnings_ratio(real_ben, real_earn)
#' }
#'
#' @export
real_benefit_earnings_ratio <- function(real_benefits, real_earnings) {

  # Extract numeric values if data frames are provided
  if (is.data.frame(real_benefits)) {
    if (!"real_benefits" %in% names(real_benefits)) {
      stop("real_benefits data frame must contain 'real_benefits' column")
    }
    real_benefits <- real_benefits$real_benefits
  }

  if (is.data.frame(real_earnings)) {
    if (!"real_earnings" %in% names(real_earnings)) {
      stop("real_earnings data frame must contain 'real_earnings' column")
    }
    real_earnings <- real_earnings$real_earnings
  }

  # Calculate ratio, handling division by zero
  ratio <- ifelse(real_earnings == 0, NA_real_, real_benefits / real_earnings)

  return(ratio)
}


#' Calculate PV Benefit-Earnings Ratio
#'
#' Calculates the ratio of present value of lifetime benefits to
#' present value of lifetime earnings.
#'
#' @param pv_benefits Numeric vector of PV lifetime benefits, or a
#'   data frame with a \code{pv_benefits} column.
#' @param pv_earnings Numeric vector of PV lifetime earnings, or a
#'   data frame with a \code{pv_earnings} column.
#'
#' @return Numeric vector of PV benefit-earnings ratios.
#'
#' @details
#' The PV benefit-earnings ratio indicates how many present-value dollars of
#' benefits are received for each present-value dollar of earnings, both
#' discounted to the same base year. A ratio greater than 1 indicates PV of
#' lifetime benefits exceeds PV of lifetime earnings.
#'
#' @examples
#' \dontrun{
#' pv_ben <- pv_lifetime_benefits(worker, tr2025)
#' pv_earn <- pv_lifetime_earnings(worker, tr2025)
#' ratio <- pv_benefit_earnings_ratio(pv_ben, pv_earn)
#' }
#'
#' @export
pv_benefit_earnings_ratio <- function(pv_benefits, pv_earnings) {

  # Extract numeric values if data frames are provided
  if (is.data.frame(pv_benefits)) {
    if (!"pv_benefits" %in% names(pv_benefits)) {
      stop("pv_benefits data frame must contain 'pv_benefits' column")
    }
    pv_benefits <- pv_benefits$pv_benefits
  }

  if (is.data.frame(pv_earnings)) {
    if (!"pv_earnings" %in% names(pv_earnings)) {
      stop("pv_earnings data frame must contain 'pv_earnings' column")
    }
    pv_earnings <- pv_earnings$pv_earnings
  }

  # Calculate ratio, handling division by zero
  ratio <- ifelse(pv_earnings == 0, NA_real_, pv_benefits / pv_earnings)

  return(ratio)
}


#' Calculate Internal Rate of Return
#'
#' Calculates the lifetime internal rate of return (IRR) for Social Security
#' contributions. The IRR is the discount rate at which the present value of
#' lifetime benefits equals the present value of lifetime taxes.
#'
#' @param worker Data frame with calculated benefits. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{earnings}, \code{annual_ind},
#'   \code{claim_age}, and \code{death_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{oasi_tr}, \code{di_tr}, \code{taxmax}.
#' @param include_employer Logical. If TRUE, includes both employee and employer
#'   shares of payroll taxes. Default is FALSE (employee share only).
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{irr}: Internal rate of return as a decimal (e.g., 0.03 = 3%)
#'   }
#'
#' @details
#' The IRR is found by solving for r in the equation:
#' \deqn{\sum_{t} \frac{tax_t}{(1+r)^{(t - base\_year)}} = \sum_{t} \frac{benefit_t}{(1+r)^{(t - base\_year)}}}
#'
#' This function uses \code{uniroot()} to find the discount rate where the
#' net present value (PV benefits - PV taxes) equals zero.
#'
#' The IRR represents the "return" on Social Security contributions. Higher
#' values indicate better returns. Due to the progressive benefit formula,
#' lower earners typically have higher IRRs than higher earners.
#'
#' Returns NA when:
#' \itemize{
#'   \item Total lifetime benefits = 0
#'   \item Total lifetime taxes = 0
#'   \item No solution found in the range [-0.99, 1.0]
#' }
#'
#' @examples
#' \dontrun{
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, debugg = TRUE
#' )
#' irr <- internal_rate_of_return(worker, tr2025)
#' irr_total <- internal_rate_of_return(worker, tr2025, include_employer = TRUE)
#' }
#'
#' @importFrom stats uniroot
#' @importFrom dplyr %>% filter group_by summarise left_join first mutate
#' @export
internal_rate_of_return <- function(worker, assumptions, include_employer = FALSE) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings", "annual_ind")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Check for claim_age and death_age
  if (!"claim_age" %in% names(worker)) {
    stop("worker data must contain 'claim_age' column")
  }
  if (!"death_age" %in% names(worker)) {
    stop("worker data must contain 'death_age' column")
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "oasi_tr", "di_tr", "taxmax")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Calculate taxes using existing function
  worker_with_taxes <- calculate_taxes(worker, assumptions)

  # Get unique worker IDs
  worker_ids <- unique(worker$id)

  # Calculate IRR for each worker
  results <- lapply(worker_ids, function(wid) {
    # Filter to this worker
    w_data <- worker_with_taxes %>% filter(id == wid)

    # Get worker metadata
    claim_age_val <- w_data$claim_age[1]
    death_age_val <- w_data$death_age[1]
    birth_yr <- w_data$year[1] - w_data$age[1]

    # Extract tax stream (ages 21-64)
    tax_data <- w_data %>%
      filter(age >= 21 & age <= 64) %>%
      mutate(
        tax_amount = if (include_employer) ss_tax * 2 else ss_tax
      ) %>%
      select(year, age, tax_amount)

    # Extract benefit stream (claim_age to death_age, with partial year)
    floor_death <- floor(death_age_val)
    frac_death <- death_age_val - floor_death

    benefit_data <- w_data %>%
      filter(age >= claim_age_val & age <= floor_death & annual_ind > 0) %>%
      mutate(
        annual_ind = if_else(age == floor_death, annual_ind * frac_death, annual_ind)
      ) %>%
      select(year, age, annual_ind)

    # Check for edge cases
    total_taxes <- sum(tax_data$tax_amount, na.rm = TRUE)
    total_benefits <- sum(benefit_data$annual_ind, na.rm = TRUE)

    if (total_taxes == 0 || total_benefits == 0) {
      return(data.frame(id = wid, irr = NA_real_))
    }

    # Define the NPV function (PV benefits - PV taxes) as function of r
    # We want to find r where NPV = 0
    npv_func <- function(r) {
      # Use age 21 as base year for discounting
      base_age <- 21

      # PV of taxes
      pv_taxes <- sum(
        tax_data$tax_amount / (1 + r)^(tax_data$age - base_age),
        na.rm = TRUE
      )

      # PV of benefits
      pv_benefits <- sum(
        benefit_data$annual_ind / (1 + r)^(benefit_data$age - base_age),
        na.rm = TRUE
      )

      return(pv_benefits - pv_taxes)
    }

    # Use uniroot to find the IRR
    # Search in range [-0.99, 1.0] (can't have r <= -1)
    irr_result <- tryCatch({
      result <- uniroot(npv_func, interval = c(-0.99, 1.0), tol = 1e-8)
      result$root
    }, error = function(e) {
      NA_real_
    })

    data.frame(id = wid, irr = irr_result)
  })

  # Combine results
  result <- do.call(rbind, results)

  return(result)
}
