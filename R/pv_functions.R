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


#' Calculate Present Value of Lifetime Benefits
#'
#' Calculates the present value of lifetime Social Security benefits from
#' claim age to expected death age, discounted using the nominal effective
#' discount factor from the Trustees Report assumptions.
#'
#' @param worker Data frame with calculated benefits. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, \code{annual_ind} (annual benefit),
#'   \code{claim_age}, and \code{death_age}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year} and \code{df} (nominal discount factor).
#' @param discount_to_age Numeric value specifying the age to which benefits
#'   are discounted. Default is 65.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_benefits}: Present value of lifetime benefits
#'   }
#'
#' @details
#' The present value calculation uses the nominal effective discount factor
#' (\code{df}) from the Trustees Report assumptions. Since benefit amounts
#' are in nominal dollars, the nominal discount factor is appropriate.
#' Benefits are normalized to the specified \code{discount_to_age} (default 65)
#' for comparability across workers with different birth years.
#'
#' The formula for each year's discounted benefit is:
#' \code{annual_benefit * (df_at_discount_age / df_at_benefit_year)}
#'
#' Benefits are summed from the worker's claim age through their expected death age
#' (based on cohort life expectancy). Benefits after death are excluded.
#'
#' @examples
#' \dontrun{
#' # Calculate benefits for a worker
#' worker <- calculate_benefits(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#'
#' # Calculate PV of lifetime benefits
#' pv_ben <- pv_lifetime_benefits(worker, tr2025)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join first
#' @export
pv_lifetime_benefits <- function(worker, assumptions, discount_to_age = 65) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "annual_ind")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Get claim_age and death_age - may be in worker data or need to be derived
  if (!"claim_age" %in% names(worker)) {
    # Derive claim_age as first age with positive benefits
    worker <- worker %>%
      group_by(id) %>%
      mutate(claim_age = min(age[annual_ind > 0], na.rm = TRUE)) %>%
      ungroup()
  }

  if (!"death_age" %in% names(worker)) {
    stop("worker data must contain 'death_age' column")
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "df")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Join df from assumptions
  dataset <- worker %>%
    left_join(assumptions %>% select(year, df), by = "year")

  # Calculate PV of lifetime benefits
  # Only include benefits from claim_age to death_age (no benefits after death)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      # (so we can access df at age 65 even when filtering to claim_age+)
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      df_norm = df[which(year == discount_year)][1]
    ) %>%
    filter(age >= claim_age & age < death_age & annual_ind > 0) %>%
    mutate(
      # Discount benefits to the normalization age
      # PV = benefit * (df_norm / df_year) since higher df = further future
      pv_factor = df_norm / df,
      pv_annual = annual_ind * pv_factor
    ) %>%
    summarise(
      pv_benefits = sum(pv_annual, na.rm = TRUE),
      .groups = "drop"
    )

  return(result)
}


#' Calculate Present Value of Lifetime Social Security Taxes
#'
#' Calculates the present value of lifetime Social Security taxes paid from
#' first working age (21) through age 64, discounted using the nominal effective
#' discount factor from the Trustees Report assumptions.
#'
#' @param worker Data frame with worker earnings. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, and \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year}, \code{df}, \code{oasi_tr},
#'   \code{di_tr}, and \code{taxmax}.
#' @param discount_to_age Numeric value specifying the age to which taxes
#'   are discounted. Default is 65.
#' @param include_employer Logical. If TRUE, includes both employee and
#'   employer shares of payroll taxes (doubling the tax amount).
#'   Default is FALSE (employee share only).
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_taxes}: Present value of lifetime taxes
#'   }
#'
#' @details
#' Uses \code{calculate_taxes()} internally to compute annual tax amounts,
#' then discounts using the nominal effective discount factor. Since tax
#' amounts are in nominal dollars, the nominal discount factor is appropriate.
#' The discount factor is normalized to the specified \code{discount_to_age}
#' (default 65).
#'
#' The \code{include_employer} parameter allows for analysis that includes
#' the employer's matching contribution, which is relevant for evaluating
#' total contributions to the Social Security system.
#'
#' @examples
#' \dontrun{
#' # Generate worker with earnings
#' worker <- earnings_generator(
#'   birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#'
#' # Calculate PV of lifetime taxes (employee share only)
#' pv_tax <- pv_lifetime_taxes(worker, tr2025)
#'
#' # Include employer share
#' pv_tax_total <- pv_lifetime_taxes(worker, tr2025, include_employer = TRUE)
#' }
#'
#' @importFrom dplyr %>% filter group_by summarise left_join mutate first
#' @export
pv_lifetime_taxes <- function(worker, assumptions, discount_to_age = 65,
                               include_employer = FALSE) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "df", "oasi_tr", "di_tr", "taxmax")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Calculate taxes using existing function
  worker_with_taxes <- calculate_taxes(worker, assumptions)

  # Join df from assumptions
  dataset <- worker_with_taxes %>%
    left_join(assumptions %>% select(year, df), by = "year")

  # Calculate PV of lifetime taxes (ages 21-64)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      # (so we can access df at age 65 even when filtering to ages 21-64)
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      df_norm = df[which(year == discount_year)][1]
    ) %>%
    filter(age >= 21 & age <= 64) %>%
    mutate(
      # Discount taxes to the normalization age
      pv_factor = df_norm / df,
      # Apply employer multiplier if requested
      tax_amount = if (include_employer) ss_tax * 2 else ss_tax,
      pv_annual = tax_amount * pv_factor
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

  # Join gdp_pi from assumptions
  dataset <- worker %>%
    left_join(assumptions %>% select(year, gdp_pi), by = "year")

  # Calculate real lifetime benefits
  result <- dataset %>%
    group_by(id) %>%
    filter(age >= claim_age & age < death_age & annual_ind > 0) %>%
    mutate(
      # Deflate benefits to base year constant dollars
      price_scalar = gdp_pi_base / gdp_pi,
      real_annual = annual_ind * price_scalar
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

  # Join gdp_pi from assumptions
  dataset <- worker %>%
    left_join(assumptions %>% select(year, gdp_pi), by = "year")

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


#' Calculate Present Value of Lifetime Earnings
#'
#' Calculates the present value of lifetime earnings from age 21 through
#' age 64, discounted using the nominal effective discount factor from
#' the Trustees Report assumptions.
#'
#' @param worker Data frame with worker earnings. Must contain columns:
#'   \code{id}, \code{year}, \code{age}, and \code{earnings}.
#' @param assumptions Data frame with the prepared Trustees assumptions.
#'   Must contain columns: \code{year} and \code{df} (nominal discount factor).
#' @param discount_to_age Numeric value specifying the age to which earnings
#'   are discounted. Default is 65.
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item \code{id}: Worker identifier
#'     \item \code{pv_earnings}: Present value of lifetime earnings
#'   }
#'
#' @details
#' The present value calculation uses the nominal effective discount factor
#' (\code{df}) from the Trustees Report assumptions. Since earnings amounts
#' are in nominal dollars, the nominal discount factor is appropriate.
#' Earnings are normalized to the specified \code{discount_to_age} (default 65)
#' for comparability across workers.
#'
#' The formula for each year's discounted earnings is:
#' \code{earnings * (df_at_discount_age / df_at_earnings_year)}
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
pv_lifetime_earnings <- function(worker, assumptions, discount_to_age = 65) {

  # Validate required columns in worker data
  worker_cols_needed <- c("id", "year", "age", "earnings")
  if (!all(worker_cols_needed %in% names(worker))) {
    stop(paste("worker data must contain:", paste(worker_cols_needed, collapse = ", ")))
  }

  # Validate required columns in assumptions
  assumption_cols_needed <- c("year", "df")
  if (!all(assumption_cols_needed %in% names(assumptions))) {
    stop(paste("assumptions data must contain:", paste(assumption_cols_needed, collapse = ", ")))
  }

  # Join df from assumptions
  dataset <- worker %>%
    left_join(assumptions %>% select(year, df), by = "year")

  # Calculate PV of lifetime earnings (ages 21-64)
  result <- dataset %>%
    group_by(id) %>%
    mutate(
      # Get discount factor at the normalization age BEFORE filtering
      birth_yr = first(year) - first(age),
      discount_year = birth_yr + discount_to_age,
      df_norm = df[which(year == discount_year)][1]
    ) %>%
    filter(age >= 21 & age <= 64) %>%
    mutate(
      # Discount earnings to the normalization age
      pv_factor = df_norm / df,
      pv_annual = earnings * pv_factor
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
