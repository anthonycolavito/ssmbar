# =============================================================================
# REFORM ANALYSIS FUNCTIONS
# =============================================================================
#
# This file contains functions for comparing Social Security benefits and
# analytical measures under baseline (current law) vs reform scenarios.
#
# =============================================================================


#' Calculate All Analytical Measures for a Worker
#'
#' Computes a comprehensive set of analytical measures for a worker including
#' benefits, replacement rates, present values, and marginal analysis.
#'
#' @param worker Data frame from calculate_benefits() with debugg = TRUE
#' @param assumptions Data frame with prepared Trustees assumptions
#' @param base_year Year for real dollar conversions (default 2025)
#' @param include_employer Include employer share of taxes (default FALSE)
#'
#' @return A list with the following components:
#'   \itemize{
#'     \item \code{annual_data}: Data frame with year-by-year measures
#'     \item \code{summary}: List of summary statistics
#'     \item \code{replacement_rates}: Data frame of all replacement rate measures
#'     \item \code{marginal}: Data frame of marginal analysis results
#'   }
#'
#' @details
#' This function consolidates all analytical calculations into a single call,
#' making it easy to compare baseline vs reform scenarios.
#'
#' @export
calculate_all_measures <- function(worker, assumptions, base_year = 2025,
                                   include_employer = FALSE) {

  # Validate input
  required_cols <- c("id", "year", "age", "earnings", "ben", "claim_age", "death_age")
  if (!all(required_cols %in% names(worker))) {
    missing <- required_cols[!required_cols %in% names(worker)]
    stop(paste("worker data must contain:", paste(missing, collapse = ", ")))
  }

  # Get worker's key parameters
  worker_summary <- worker %>%
    group_by(id) %>%
    summarise(
      birth_yr = first(year) - first(age),
      claim_age = first(claim_age[!is.na(claim_age)]),
      death_age = first(death_age[!is.na(death_age)]),
      .groups = "drop"
    )

  birth_yr <- worker_summary$birth_yr[1]
  claim_age_val <- worker_summary$claim_age[1]
  death_age_val <- worker_summary$death_age[1]

  # Get gdp_pi for price conversions
  gdp_pi_base <- assumptions$gdp_pi[assumptions$year == base_year]

  # Build annual data with nominal and real benefits
  # Join gdp_pi if not already present
  if (!"gdp_pi" %in% names(worker)) {
    worker <- worker %>%
      left_join(assumptions %>% select(year, gdp_pi), by = "year")
  }

  annual_data <- worker %>%
    mutate(
      # Annual nominal benefits
      annual_nominal = ben * 12,

      # Annual real benefits (constant base_year dollars)
      annual_real = annual_nominal * (gdp_pi_base / gdp_pi)
    ) %>%
    select(id, year, age, earnings, ben, annual_nominal, annual_real, gdp_pi)

  # Calculate present values
  pv_benefits <- pv_lifetime_benefits(worker, assumptions, base_year = base_year)
  pv_taxes <- pv_lifetime_taxes(worker, assumptions, include_employer = include_employer,
                                 base_year = base_year)
  real_benefits <- real_lifetime_benefits(worker, assumptions, base_year = base_year)
  real_earnings <- real_lifetime_earnings(worker, assumptions, base_year = base_year)

  # Calculate benefit-tax ratio
  bt_ratio <- benefit_tax_ratio(pv_benefits, pv_taxes)

  # Summary statistics
  summary_stats <- list(
    birth_year = birth_yr,
    claim_age = claim_age_val,
    death_age = death_age_val,

    # First year benefit (monthly)
    initial_monthly_benefit = worker$ben[worker$age == claim_age_val][1],

    # Annual benefit at claim
    initial_annual_benefit = worker$ben[worker$age == claim_age_val][1] * 12,

    # Present values (real base_year dollars)
    pv_benefits = pv_benefits$pv_benefits[1],
    pv_taxes = pv_taxes$pv_taxes[1],
    benefit_tax_ratio = bt_ratio[1],

    # Undiscounted real totals
    real_lifetime_benefits = real_benefits$real_benefits[1],
    real_lifetime_earnings = real_earnings$real_earnings[1]
  )

  # Calculate replacement rates if annual_ind is available
  replacement_rates <- NULL
  if ("annual_ind" %in% names(worker)) {
    tryCatch({
      replacement_rates <- rep_rates(worker, assumptions)
    }, error = function(e) {
      warning("Could not calculate replacement rates: ", e$message)
    })
  }

  # Calculate marginal analysis if debugg columns available
  marginal_data <- NULL
  if (all(c("indexed_earn", "index_factor", "aime", "bp1_elig", "bp2_elig") %in% names(worker))) {
    tryCatch({
      marginal_data <- marginal_benefit_analysis(worker, assumptions, base_year)
      # Add net marginal tax rate
      nmtr <- net_marginal_tax_rate(worker, assumptions, include_employer, base_year)
      if (!is.null(nmtr) && "net_marginal_tax_rate" %in% names(nmtr)) {
        marginal_data <- marginal_data %>%
          left_join(nmtr %>% select(id, year, net_marginal_tax_rate), by = c("id", "year"))
      }
    }, error = function(e) {
      warning("Could not calculate marginal analysis: ", e$message)
    })
  }

  return(list(
    annual_data = annual_data,
    summary = summary_stats,
    replacement_rates = replacement_rates,
    marginal = marginal_data
  ))
}


#' Compare Baseline and Reform Scenarios
#'
#' Calculates all analytical measures under baseline (current law) and one or
#' more reform scenarios, returning a comprehensive comparison.
#'
#' @param birth_yr Worker's birth year
#' @param sex Worker's sex ("male" or "female")
#' @param type Worker type ("very_low", "low", "medium", "high", "max")
#' @param age_claim Age at which worker claims benefits
#' @param factors Scaled earnings factors (e.g., sef2025)
#' @param assumptions Baseline assumptions (e.g., tr2025)
#' @param reforms A list of Reform objects to compare, or a single Reform object.
#'   Each reform will be compared to baseline independently.
#' @param base_year Year for real dollar conversions (default 2025)
#' @param include_employer Include employer share of taxes (default FALSE)
#' @param spouse_type Optional spouse type
#' @param spouse_sex Optional spouse sex
#' @param spouse_birth_yr Optional spouse birth year
#' @param spouse_age_claim Optional spouse claim age
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{baseline}: All measures under current law
#'     \item \code{reforms}: Named list of measures under each reform
#'     \item \code{comparison}: Data frame comparing key metrics across scenarios
#'     \item \code{annual_comparison}: Data frame comparing annual benefits across scenarios
#'   }
#'
#' @examples
#' \dontrun{
#' # Compare baseline to two reforms
#' reforms <- list(
#'   "5% Benefit Cut" = reform_reduce_benefits(0.95, 2030),
#'   "NRA to 68" = reform_nra_to_68(2030)
#' )
#'
#' comparison <- compare_reform_scenarios(
#'   birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025, reforms = reforms
#' )
#'
#' # View summary comparison
#' comparison$comparison
#' }
#'
#' @export
compare_reform_scenarios <- function(birth_yr, sex, type, age_claim,
                                     factors, assumptions, reforms,
                                     base_year = 2025, include_employer = FALSE,
                                     spouse_type = NULL, spouse_sex = NULL,
                                     spouse_birth_yr = NULL, spouse_age_claim = NULL) {

  # Convert single reform to list
  if (inherits(reforms, "Reform")) {
    reforms <- list(reforms)
    names(reforms) <- reforms[[1]]$name
  }

  # Ensure reforms is a named list
  if (is.null(names(reforms))) {
    names(reforms) <- paste0("Reform_", seq_along(reforms))
  }

  # Calculate baseline
  baseline_worker <- calculate_benefits(
    birth_yr = birth_yr, sex = sex, type = type, age_claim = age_claim,
    factors = factors, assumptions = assumptions,
    spouse_type = spouse_type, spouse_sex = spouse_sex,
    spouse_birth_yr = spouse_birth_yr, spouse_age_claim = spouse_age_claim,
    debugg = TRUE
  )

  baseline_measures <- calculate_all_measures(baseline_worker, assumptions,
                                               base_year, include_employer)

  # Calculate reform scenarios
  reform_measures <- list()
  for (name in names(reforms)) {
    reform <- reforms[[name]]

    # Apply reform to assumptions
    reformed_assumptions <- apply_reform(assumptions, reform)

    # Calculate benefits under reform using reform-capable pipeline
    reform_worker <- calculate_benefits_reform(
      birth_yr = birth_yr, sex = sex, type = type, age_claim = age_claim,
      factors = factors, assumptions = reformed_assumptions,
      spouse_type = spouse_type, spouse_sex = spouse_sex,
      spouse_birth_yr = spouse_birth_yr, spouse_age_claim = spouse_age_claim,
      debugg = TRUE
    )

    reform_measures[[name]] <- calculate_all_measures(reform_worker, reformed_assumptions,
                                                       base_year, include_employer)
  }

  # Build summary comparison table
  comparison <- build_comparison_table(baseline_measures, reform_measures)

  # Build annual benefit comparison
  annual_comparison <- build_annual_comparison(baseline_measures, reform_measures)

  return(list(
    baseline = baseline_measures,
    reforms = reform_measures,
    comparison = comparison,
    annual_comparison = annual_comparison
  ))
}


#' Build Summary Comparison Table
#'
#' Internal function to build a comparison table from baseline and reform measures.
#'
#' @param baseline Baseline measures from calculate_all_measures()
#' @param reform_measures Named list of reform measures
#'
#' @return Data frame with one row per scenario and columns for key metrics
#' @keywords internal
build_comparison_table <- function(baseline, reform_measures) {

  # Extract summary metrics from baseline
  baseline_row <- data.frame(
    scenario = "Baseline",
    birth_year = baseline$summary$birth_year,
    claim_age = baseline$summary$claim_age,
    death_age = baseline$summary$death_age,
    initial_monthly_benefit = baseline$summary$initial_monthly_benefit,
    initial_annual_benefit = baseline$summary$initial_annual_benefit,
    pv_benefits = baseline$summary$pv_benefits,
    pv_taxes = baseline$summary$pv_taxes,
    benefit_tax_ratio = baseline$summary$benefit_tax_ratio,
    real_lifetime_benefits = baseline$summary$real_lifetime_benefits,
    stringsAsFactors = FALSE
  )

  # Add reform rows
  reform_rows <- lapply(names(reform_measures), function(name) {
    rm <- reform_measures[[name]]
    data.frame(
      scenario = name,
      birth_year = rm$summary$birth_year,
      claim_age = rm$summary$claim_age,
      death_age = rm$summary$death_age,
      initial_monthly_benefit = rm$summary$initial_monthly_benefit,
      initial_annual_benefit = rm$summary$initial_annual_benefit,
      pv_benefits = rm$summary$pv_benefits,
      pv_taxes = rm$summary$pv_taxes,
      benefit_tax_ratio = rm$summary$benefit_tax_ratio,
      real_lifetime_benefits = rm$summary$real_lifetime_benefits,
      stringsAsFactors = FALSE
    )
  })

  comparison <- do.call(rbind, c(list(baseline_row), reform_rows))

  # Add difference columns
  comparison$pv_benefits_diff <- comparison$pv_benefits - baseline_row$pv_benefits
  comparison$pv_benefits_pct <- (comparison$pv_benefits / baseline_row$pv_benefits - 1) * 100
  comparison$initial_benefit_diff <- comparison$initial_monthly_benefit - baseline_row$initial_monthly_benefit
  comparison$initial_benefit_pct <- (comparison$initial_monthly_benefit / baseline_row$initial_monthly_benefit - 1) * 100

  return(comparison)
}


#' Build Annual Benefit Comparison
#'
#' Internal function to build a year-by-year comparison of benefits.
#'
#' @param baseline Baseline measures from calculate_all_measures()
#' @param reform_measures Named list of reform measures
#'
#' @return Data frame with annual benefits under each scenario
#' @keywords internal
build_annual_comparison <- function(baseline, reform_measures) {

  # Start with baseline annual data
  annual <- baseline$annual_data %>%
    select(year, age, annual_nominal, annual_real) %>%
    rename(
      baseline_nominal = annual_nominal,
      baseline_real = annual_real
    )

  # Add reform columns
  for (name in names(reform_measures)) {
    rm <- reform_measures[[name]]

    # Clean name for column
    col_name <- gsub("[^a-zA-Z0-9]", "_", name)

    reform_annual <- rm$annual_data %>%
      select(year, annual_nominal, annual_real)

    names(reform_annual)[2:3] <- paste0(col_name, c("_nominal", "_real"))

    annual <- annual %>%
      left_join(reform_annual, by = "year")
  }

  return(annual)
}


#' Compare Replacement Rates Across Scenarios
#'
#' Extracts and compares replacement rates from baseline and reform scenarios.
#'
#' @param comparison_result Output from compare_reform_scenarios()
#' @param rate_type Which replacement rate type to compare. Options include:
#'   "pv_rr", "real_all", "wage_all", "real_h35", etc. Default is "pv_rr".
#'
#' @return Data frame with replacement rates by scenario
#'
#' @export
compare_replacement_rates <- function(comparison_result, rate_type = "pv_rr") {

  # Extract baseline rate
  baseline_rr <- comparison_result$baseline$replacement_rates
  if (is.null(baseline_rr)) {
    stop("No replacement rates available (did you run with debugg = TRUE?)")
  }

  baseline_val <- baseline_rr$rep_rate[baseline_rr$type == rate_type]
  if (length(baseline_val) == 0) {
    stop(paste("Rate type", rate_type, "not found"))
  }

  result <- data.frame(
    scenario = "Baseline",
    rate_type = rate_type,
    replacement_rate = baseline_val,
    diff = 0,
    pct_change = 0,
    stringsAsFactors = FALSE
  )

  # Add reform rates
  for (name in names(comparison_result$reforms)) {
    reform_rr <- comparison_result$reforms[[name]]$replacement_rates
    if (!is.null(reform_rr)) {
      reform_val <- reform_rr$rep_rate[reform_rr$type == rate_type]
      if (length(reform_val) > 0) {
        result <- rbind(result, data.frame(
          scenario = name,
          rate_type = rate_type,
          replacement_rate = reform_val,
          diff = reform_val - baseline_val,
          pct_change = (reform_val / baseline_val - 1) * 100,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(result)
}


#' Generate Reform Impact Report
#'
#' Creates a formatted text report comparing baseline to a single reform.
#'
#' @param comparison_result Output from compare_reform_scenarios()
#' @param reform_name Name of the reform to report on (if multiple reforms)
#'
#' @return Character string with formatted report
#'
#' @export
reform_impact_report <- function(comparison_result, reform_name = NULL) {

  # Get reform name if not specified
  if (is.null(reform_name)) {
    reform_name <- names(comparison_result$reforms)[1]
  }

  baseline <- comparison_result$baseline$summary
  reform <- comparison_result$reforms[[reform_name]]$summary
  comp <- comparison_result$comparison

  # Helper function for formatting currency
  fmt_currency <- function(x) paste0("$", format(round(x), big.mark = ","))

  # Build report
  lines <- c(
    "===================================================",
    sprintf("Reform Impact Report: %s", reform_name),
    "===================================================",
    "",
    "Worker Profile:",
    sprintf("  Birth Year: %d", baseline$birth_year),
    sprintf("  Claim Age: %d", baseline$claim_age),
    sprintf("  Life Expectancy: Age %.1f", baseline$death_age),
    "",
    "Monthly Benefits:",
    paste0("  Baseline: ", fmt_currency(baseline$initial_monthly_benefit)),
    paste0("  Reform:   ", fmt_currency(reform$initial_monthly_benefit)),
    paste0("  Change:   ", fmt_currency(reform$initial_monthly_benefit - baseline$initial_monthly_benefit),
           sprintf(" (%.1f%%)", (reform$initial_monthly_benefit / baseline$initial_monthly_benefit - 1) * 100)),
    "",
    "Present Value of Lifetime Benefits (Real Dollars):",
    paste0("  Baseline: ", fmt_currency(baseline$pv_benefits)),
    paste0("  Reform:   ", fmt_currency(reform$pv_benefits)),
    paste0("  Change:   ", fmt_currency(reform$pv_benefits - baseline$pv_benefits),
           sprintf(" (%.1f%%)", (reform$pv_benefits / baseline$pv_benefits - 1) * 100)),
    "",
    "Benefit-Tax Ratio:",
    sprintf("  Baseline: %.2f", baseline$benefit_tax_ratio),
    sprintf("  Reform:   %.2f", reform$benefit_tax_ratio),
    sprintf("  Change:   %.2f", reform$benefit_tax_ratio - baseline$benefit_tax_ratio),
    "",
    "==================================================="
  )

  report <- paste(lines, collapse = "\n")
  cat(report)
  invisible(report)
}


#' Quick Reform Comparison
#'
#' A convenience function for quickly comparing a single reform to baseline.
#'
#' @param reform A Reform object
#' @param birth_yr Worker's birth year
#' @param type Worker type (default "medium")
#' @param age_claim Claim age (default 67)
#' @param factors Scaled earnings factors (default uses sef2025)
#' @param assumptions Baseline assumptions (default uses tr2025)
#'
#' @return Data frame with key comparison metrics
#'
#' @examples
#' \dontrun{
#' quick_compare(reform_nra_to_68(2030), birth_yr = 1970)
#' }
#'
#' @export
quick_compare <- function(reform, birth_yr, type = "medium", age_claim = 67,
                          factors = NULL, assumptions = NULL) {

  # Load default data if not provided
  if (is.null(factors)) {
    data("sef2025", package = "ssmbar", envir = environment())
    factors <- get("sef2025", envir = environment())
  }
  if (is.null(assumptions)) {
    data("tr2025", package = "ssmbar", envir = environment())
    assumptions <- get("tr2025", envir = environment())
  }

  # Run comparison
  result <- compare_reform_scenarios(
    birth_yr = birth_yr,
    sex = "male",
    type = type,
    age_claim = age_claim,
    factors = factors,
    assumptions = assumptions,
    reforms = reform
  )

  # Return simplified comparison
  comp <- result$comparison
  data.frame(
    Metric = c("Monthly Benefit", "PV Benefits", "Benefit-Tax Ratio"),
    Baseline = c(
      paste0("$", format(round(comp$initial_monthly_benefit[1]), big.mark = ",")),
      paste0("$", format(round(comp$pv_benefits[1]), big.mark = ",")),
      sprintf("%.2f", comp$benefit_tax_ratio[1])
    ),
    Reform = c(
      paste0("$", format(round(comp$initial_monthly_benefit[2]), big.mark = ",")),
      paste0("$", format(round(comp$pv_benefits[2]), big.mark = ",")),
      sprintf("%.2f", comp$benefit_tax_ratio[2])
    ),
    Change = c(
      paste0("$", format(round(comp$initial_benefit_diff[2]), big.mark = ","),
             " (", sprintf("%.1f", comp$initial_benefit_pct[2]), "%)"),
      paste0("$", format(round(comp$pv_benefits_diff[2]), big.mark = ","),
             " (", sprintf("%.1f", comp$pv_benefits_pct[2]), "%)"),
      sprintf("%.2f", comp$benefit_tax_ratio[2] - comp$benefit_tax_ratio[1])
    ),
    stringsAsFactors = FALSE
  )
}
