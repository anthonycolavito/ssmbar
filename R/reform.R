# =============================================================================
# REFORM INFRASTRUCTURE
# =============================================================================
#
# This file contains functions for creating and applying Social Security
# policy reforms. Reforms modify assumptions parameters to model alternative
# policy scenarios.
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Reform Class Constructor
# -----------------------------------------------------------------------------

#' Create a Social Security Reform
#'
#' Creates a Reform object that specifies changes to Social Security program
#' parameters. The reform can be applied to assumptions using `apply_reform()`.
#'
#' @param name Character string naming the reform (e.g., "Raise NRA to 69")
#' @param description Character string describing the reform's purpose and effects
#' @param parameters Named list of parameter modifications. Each element should be
#'   a list with:
#'   \itemize{
#'     \item \code{param}: Name of the parameter to modify (must exist in assumptions)
#'     \item \code{value}: New value for the parameter (scalar or function)
#'     \item \code{type}: How to apply: "replace" (default), "add", or "multiply"
#'   }
#' @param effective_year Numeric year when the reform takes effect
#' @param phase_in_years Numeric number of years to phase in the reform (0 = immediate).
#'   Default is 0.
#' @param affected_cohorts Numeric vector of birth years affected, or NULL for all.
#'   Default is NULL.
#'
#' @return A Reform object (S3 class)
#'
#' @examples
#' \dontrun{
#' # Raise NRA to 69 starting in 2030
#' reform_nra <- create_reform(
#'   name = "Raise NRA to 69",
#'   description = "Gradually raise Normal Retirement Age from 67 to 69",
#'   parameters = list(
#'     list(param = "nra", value = 69, type = "replace")
#'   ),
#'   effective_year = 2030,
#'   phase_in_years = 10
#' )
#'
#' # Reduce benefit formula (means-test high earners)
#' reform_bend <- create_reform(
#'   name = "Reduce Top Replacement Rate",
#'   description = "Reduce factor3 from 15% to 10%",
#'   parameters = list(
#'     list(param = "fact3", value = 0.10, type = "replace")
#'   ),
#'   effective_year = 2027,
#'   phase_in_years = 0
#' )
#' }
#'
#' @export
create_reform <- function(name,
                          description,
                          parameters,
                          effective_year,
                          phase_in_years = 0,
                          affected_cohorts = NULL) {


  # Validate inputs
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    stop("'name' must be a non-empty character string")
  }

  if (!is.character(description) || length(description) != 1) {
    stop("'description' must be a character string")
  }

  if (!is.list(parameters) || length(parameters) == 0) {
    stop("'parameters' must be a non-empty list of parameter modifications")
  }

  # Validate each parameter specification
  for (i in seq_along(parameters)) {
    p <- parameters[[i]]
    if (!is.list(p)) {
      stop(sprintf("parameters[[%d]] must be a list", i))
    }
    if (!"param" %in% names(p) || !is.character(p$param)) {
      stop(sprintf("parameters[[%d]] must have a 'param' element (character)", i))
    }
    if (!"value" %in% names(p)) {
      stop(sprintf("parameters[[%d]] must have a 'value' element", i))
    }
    # Set default type if not specified
    if (!"type" %in% names(p)) {
      parameters[[i]]$type <- "replace"
    } else if (!p$type %in% c("replace", "add", "multiply")) {
      stop(sprintf("parameters[[%d]]$type must be 'replace', 'add', or 'multiply'", i))
    }
  }

  if (!is.numeric(effective_year) || length(effective_year) != 1) {
    stop("'effective_year' must be a single numeric value")
  }

  if (!is.numeric(phase_in_years) || length(phase_in_years) != 1 || phase_in_years < 0) {
    stop("'phase_in_years' must be a non-negative numeric value")
  }

  if (!is.null(affected_cohorts) && !is.numeric(affected_cohorts)) {
    stop("'affected_cohorts' must be NULL or a numeric vector of birth years")
  }

  # Create Reform object

  reform <- list(
    name = name,
    description = description,
    parameters = parameters,
    effective_year = effective_year,
    phase_in_years = phase_in_years,
    affected_cohorts = affected_cohorts
  )

  class(reform) <- "Reform"
  return(reform)
}


# -----------------------------------------------------------------------------
# 2. Print Method for Reform
# -----------------------------------------------------------------------------

#' Print Method for Reform Objects
#'
#' @param x A Reform object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the Reform object
#'
#' @export
print.Reform <- function(x, ...) {
  cat("Social Security Reform:\n")
  cat("=======================\n")
  cat("Name:", x$name, "\n")
  cat("Description:", x$description, "\n")
  cat("Effective Year:", x$effective_year, "\n")

  if (x$phase_in_years > 0) {
    cat("Phase-in Period:", x$phase_in_years, "years\n")
    cat("Fully Phased In:", x$effective_year + x$phase_in_years, "\n")
  } else {
    cat("Phase-in: Immediate\n")
  }

  if (!is.null(x$affected_cohorts)) {
    cat("Affected Cohorts:", min(x$affected_cohorts), "-", max(x$affected_cohorts), "\n")
  } else {
    cat("Affected Cohorts: All\n")
  }

  cat("\nParameter Changes:\n")
  for (p in x$parameters) {
    if (is.function(p$value)) {
      val_str <- "<function>"
    } else {
      val_str <- as.character(p$value)
    }
    cat(sprintf("  - %s: %s (%s)\n", p$param, val_str, p$type))
  }

  invisible(x)
}


# -----------------------------------------------------------------------------
# 3. Apply Reform to Assumptions
# -----------------------------------------------------------------------------

#' Apply a Reform to Assumptions
#'
#' Modifies an assumptions data frame according to the reform specification.
#' Handles immediate changes, phased-in changes, and cohort-specific changes.
#'
#' @param assumptions Data frame of Social Security assumptions (e.g., tr2025)
#' @param reform A Reform object created by `create_reform()`
#'
#' @return Modified assumptions data frame with reform applied
#'
#' @details
#' The function handles three types of parameter modifications:
#' \itemize{
#'   \item \code{replace}: Sets the parameter to the new value
#'   \item \code{add}: Adds the value to the existing parameter
#'   \item \code{multiply}: Multiplies the existing parameter by the value
#' }
#'
#' For phased-in reforms, the modification is linearly interpolated between
#' the original value and the target value over the phase-in period.
#'
#' @examples
#' \dontrun{
#' # Create a reform
#' reform <- create_reform(
#'   name = "Raise NRA",
#'   description = "Raise NRA from 67 to 69",
#'   parameters = list(list(param = "nra", value = 69, type = "replace")),
#'   effective_year = 2030,
#'   phase_in_years = 10
#' )
#'
#' # Apply to assumptions
#' reformed_assumptions <- apply_reform(tr2025, reform)
#' }
#'
#' @export
apply_reform <- function(assumptions, reform) {
  if (!inherits(reform, "Reform")) {
    stop("'reform' must be a Reform object created by create_reform()")
  }

  # Validate that all parameters exist in assumptions
  param_names <- sapply(reform$parameters, function(p) p$param)
  missing_params <- param_names[!param_names %in% names(assumptions)]
  if (length(missing_params) > 0) {
    stop(sprintf("Parameters not found in assumptions: %s",
                 paste(missing_params, collapse = ", ")))
  }

  # Make a copy to avoid modifying the original
  reformed <- assumptions

  # Apply each parameter modification
  for (p in reform$parameters) {
    reformed <- apply_single_parameter(
      assumptions = reformed,
      param = p$param,
      value = p$value,
      type = p$type,
      effective_year = reform$effective_year,
      phase_in_years = reform$phase_in_years
    )
  }

  # Store reform metadata as attribute
  attr(reformed, "reform") <- reform

  return(reformed)
}


#' Apply a Single Parameter Modification (Internal)
#'
#' @param assumptions Data frame of assumptions
#' @param param Parameter name to modify
#' @param value New value (or function that takes year and returns value)
#' @param type Modification type: "replace", "add", or "multiply"
#' @param effective_year Year when reform takes effect
#' @param phase_in_years Number of years to phase in (0 = immediate)
#'
#' @return Modified assumptions data frame
#' @keywords internal
apply_single_parameter <- function(assumptions, param, value, type,
                                   effective_year, phase_in_years) {

  # Get the original values
  original_values <- assumptions[[param]]

  # Calculate phase-in factor for each year
  # 0 = before reform, 1 = fully phased in
  if (phase_in_years > 0) {
    phase_factor <- pmax(0, pmin(1,
      (assumptions$year - effective_year + 1) / phase_in_years
    ))
  } else {
    # Immediate change
    phase_factor <- ifelse(assumptions$year >= effective_year, 1, 0)
  }

  # Calculate target values
  if (is.function(value)) {
    # Value is a function of year
    target_values <- sapply(assumptions$year, value)
  } else {
    # Value is a constant
    target_values <- rep(value, nrow(assumptions))
  }

  # Apply modification based on type
  # Handle NA values:
  # - Where target is NA: keep original
  # - Where original is NA and phase_factor > 0: use target directly
  if (type == "replace") {
    # For replace: when phase_factor = 1, new = target; when phase_factor = 0, new = original
    # Handle NA in original: treat as 0 for interpolation purposes, then use target when active
    new_values <- ifelse(
      is.na(original_values) & phase_factor > 0,
      target_values,  # Original is NA but reform is active - use target
      ifelse(
        is.na(target_values),
        original_values,  # Target is NA - keep original
        original_values + phase_factor * (target_values - original_values)  # Normal interpolation
      )
    )
    # Keep original NA when phase_factor = 0 and original was NA
    new_values <- ifelse(phase_factor == 0 & is.na(original_values), NA, new_values)
  } else if (type == "add") {
    # For add: new = original + phase_factor * target
    new_values <- ifelse(
      is.na(original_values),
      ifelse(phase_factor > 0, target_values, NA),  # Start from 0 if original NA
      ifelse(is.na(target_values), original_values, original_values + phase_factor * target_values)
    )
  } else if (type == "multiply") {
    # For multiply: new = original * (1 + phase_factor * (target - 1))
    multiplier <- 1 + phase_factor * (target_values - 1)
    new_values <- ifelse(
      is.na(original_values),
      ifelse(phase_factor > 0, target_values, NA),
      ifelse(is.na(target_values), original_values, original_values * multiplier)
    )
  }

  assumptions[[param]] <- new_values
  return(assumptions)
}


# -----------------------------------------------------------------------------
# 4. Compare Benefits
# -----------------------------------------------------------------------------

#' Compare Baseline and Reformed Benefits
#'
#' Creates a comparison data frame showing the difference between baseline
#' (current law) benefits and reformed benefits.
#'
#' @param baseline Data frame of benefits under current law (from calculate_benefits)
#' @param reformed Data frame of benefits under reform (from calculate_benefits with reform)
#' @param compare_cols Character vector of columns to compare. Default includes
#'   key benefit columns: ben, wrk_ben, spouse_ben, annual_ind.
#'
#' @return Data frame with columns from baseline suffixed with "_baseline",
#'   columns from reformed suffixed with "_reform", and difference columns
#'   suffixed with "_diff".
#'
#' @examples
#' \dontrun{
#' # Calculate baseline benefits
#' baseline <- calculate_benefits(
#'   birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = tr2025
#' )
#'
#' # Apply reform
#' reformed_assumptions <- apply_reform(tr2025, my_reform)
#'
#' # Calculate reformed benefits
#' reformed <- calculate_benefits(
#'   birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
#'   factors = sef2025, assumptions = reformed_assumptions
#' )
#'
#' # Compare
#' comparison <- compare_benefits(baseline, reformed)
#' }
#'
#' @export
compare_benefits <- function(baseline, reformed,
                             compare_cols = c("ben", "wrk_ben", "spouse_ben", "annual_ind")) {

  # Validate inputs
  if (!is.data.frame(baseline) || !is.data.frame(reformed)) {
    stop("'baseline' and 'reformed' must be data frames")
  }

  # Check that id and age/year columns exist for joining
  join_cols <- intersect(c("id", "age", "year"), names(baseline))
  if (length(join_cols) < 2) {
    stop("Data frames must have 'id' and either 'age' or 'year' columns for joining")
  }


  # Filter to columns that exist in both data frames
  compare_cols <- compare_cols[compare_cols %in% names(baseline) &
                                compare_cols %in% names(reformed)]

  if (length(compare_cols) == 0) {
    stop("No common comparison columns found in baseline and reformed data frames")
  }

  # Create comparison data frame
  # Start with join columns
  comparison <- baseline[, join_cols, drop = FALSE]

  # Add baseline columns
  for (col in compare_cols) {
    comparison[[paste0(col, "_baseline")]] <- baseline[[col]]
  }

  # Add reformed columns and calculate differences
  for (col in compare_cols) {
    comparison[[paste0(col, "_reform")]] <- reformed[[col]]
    comparison[[paste0(col, "_diff")]] <- reformed[[col]] - baseline[[col]]
  }

  return(comparison)
}


# -----------------------------------------------------------------------------
# 5. Reform Impact Summary
# -----------------------------------------------------------------------------

#' Summarize Reform Impact
#'
#' Creates a summary of the reform's impact including winners, losers,
#' and average changes.
#'
#' @param comparison Data frame from compare_benefits()
#' @param metric Column name (without suffix) to summarize. Default is "ben".
#' @param by_age Logical. If TRUE, summarize by age. Default is FALSE.
#'
#' @return A ReformImpact object containing summary statistics
#'
#' @examples
#' \dontrun{
#' comparison <- compare_benefits(baseline, reformed)
#' impact <- reform_impact_summary(comparison, metric = "annual_ind")
#' print(impact)
#' }
#'
#' @export
reform_impact_summary <- function(comparison, metric = "ben", by_age = FALSE) {

  diff_col <- paste0(metric, "_diff")
  baseline_col <- paste0(metric, "_baseline")
  reform_col <- paste0(metric, "_reform")

  # Validate columns exist
  required_cols <- c(diff_col, baseline_col, reform_col)
  missing <- required_cols[!required_cols %in% names(comparison)]
  if (length(missing) > 0) {
    stop(sprintf("Required columns not found: %s", paste(missing, collapse = ", ")))
  }

  # Filter to rows where there's an actual benefit (baseline > 0)
  has_benefit <- comparison[[baseline_col]] > 0 | comparison[[reform_col]] > 0
  benefit_rows <- comparison[has_benefit, ]

  if (nrow(benefit_rows) == 0) {
    warning("No rows with positive benefits found")
    impact <- list(
      metric = metric,
      n_observations = 0,
      n_winners = 0,
      n_losers = 0,
      n_unchanged = 0,
      mean_diff = NA,
      median_diff = NA,
      mean_pct_change = NA,
      total_baseline = NA,
      total_reform = NA,
      total_diff = NA,
      by_age = NULL
    )
    class(impact) <- "ReformImpact"
    return(impact)
  }

  # Calculate summary statistics
  diffs <- benefit_rows[[diff_col]]
  baselines <- benefit_rows[[baseline_col]]

  # Percent change (handle zeros carefully)
  pct_change <- ifelse(baselines > 0, (diffs / baselines) * 100, NA)

  # By-age summary if requested
  age_summary <- NULL
  if (by_age && "age" %in% names(benefit_rows)) {
    age_summary <- aggregate(
      benefit_rows[, c(baseline_col, reform_col, diff_col)],
      by = list(age = benefit_rows$age),
      FUN = function(x) c(mean = mean(x, na.rm = TRUE), sum = sum(x, na.rm = TRUE))
    )
  }

  impact <- list(
    metric = metric,
    n_observations = nrow(benefit_rows),
    n_winners = sum(diffs > 0, na.rm = TRUE),
    n_losers = sum(diffs < 0, na.rm = TRUE),
    n_unchanged = sum(diffs == 0, na.rm = TRUE),
    mean_diff = mean(diffs, na.rm = TRUE),
    median_diff = median(diffs, na.rm = TRUE),
    mean_pct_change = mean(pct_change, na.rm = TRUE),
    total_baseline = sum(baselines, na.rm = TRUE),
    total_reform = sum(benefit_rows[[reform_col]], na.rm = TRUE),
    total_diff = sum(diffs, na.rm = TRUE),
    by_age = age_summary
  )

  class(impact) <- "ReformImpact"
  return(impact)
}


#' Print Method for ReformImpact Objects
#'
#' @param x A ReformImpact object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the ReformImpact object
#'
#' @export
print.ReformImpact <- function(x, ...) {
  cat("Reform Impact Summary\n")
  cat("=====================\n")
  cat("Metric:", x$metric, "\n\n")

  cat("Observations with benefits:", x$n_observations, "\n")
  cat("  Winners (increase):", x$n_winners,
      sprintf("(%.1f%%)", 100 * x$n_winners / max(x$n_observations, 1)), "\n")
  cat("  Losers (decrease):", x$n_losers,
      sprintf("(%.1f%%)", 100 * x$n_losers / max(x$n_observations, 1)), "\n")
  cat("  Unchanged:", x$n_unchanged,
      sprintf("(%.1f%%)", 100 * x$n_unchanged / max(x$n_observations, 1)), "\n\n")

  cat("Change Statistics:\n")
  cat("  Mean change:", sprintf("$%.2f", x$mean_diff), "\n")
  cat("  Median change:", sprintf("$%.2f", x$median_diff), "\n")
  cat("  Mean % change:", sprintf("%.2f%%", x$mean_pct_change), "\n\n")

  cat("Totals:\n")
  cat("  Baseline:", sprintf("$%.0f", x$total_baseline), "\n")
  cat("  Reform:", sprintf("$%.0f", x$total_reform), "\n")
  cat("  Difference:", sprintf("$%.0f", x$total_diff), "\n")

  if (!is.null(x$by_age)) {
    cat("\n(By-age breakdown available in $by_age)\n")
  }

  invisible(x)
}


# -----------------------------------------------------------------------------
# 6. Pre-built Reform Templates
# -----------------------------------------------------------------------------

#' Create Common Reform: Raise Normal Retirement Age
#'
#' Creates a reform that raises the Normal Retirement Age.
#'
#' @param target_nra Target NRA (e.g., 69)
#' @param effective_year Year reform begins
#' @param phase_in_years Years to phase in (default 10)
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_raise_nra(target_nra = 69, effective_year = 2030)
#' }
#'
#' @export
reform_raise_nra <- function(target_nra, effective_year, phase_in_years = 10) {
  create_reform(
    name = sprintf("Raise NRA to %.0f", target_nra),
    description = sprintf("Gradually raise Normal Retirement Age to %.0f over %d years",
                          target_nra, phase_in_years),
    parameters = list(
      list(param = "nra", value = target_nra, type = "replace")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


#' Create Common Reform: Modify Benefit Formula
#'
#' Creates a reform that modifies the PIA benefit formula factors.
#'
#' @param fact1 First bend point factor (default 0.90)
#' @param fact2 Second bend point factor (default 0.32)
#' @param fact3 Third bend point factor (default 0.15)
#' @param effective_year Year reform begins
#' @param phase_in_years Years to phase in (default 0)
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' # Reduce top replacement rate
#' reform <- reform_benefit_formula(fact3 = 0.10, effective_year = 2027)
#' }
#'
#' @export
reform_benefit_formula <- function(fact1 = NULL, fact2 = NULL, fact3 = NULL,
                                   effective_year, phase_in_years = 0) {
  params <- list()

  if (!is.null(fact1)) {
    params <- c(params, list(list(param = "fact1", value = fact1, type = "replace")))
  }
  if (!is.null(fact2)) {
    params <- c(params, list(list(param = "fact2", value = fact2, type = "replace")))
  }
  if (!is.null(fact3)) {
    params <- c(params, list(list(param = "fact3", value = fact3, type = "replace")))
  }

  if (length(params) == 0) {
    stop("At least one of fact1, fact2, or fact3 must be specified")
  }

  create_reform(
    name = "Modify Benefit Formula",
    description = "Change PIA benefit formula replacement factors",
    parameters = params,
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}


#' Create Common Reform: Across-the-Board Benefit Cut
#'
#' Creates a reform that reduces all benefits by a percentage.
#'
#' @param cut_pct Percentage to cut benefits (e.g., 0.20 for 20% cut)
#' @param effective_year Year reform begins
#' @param phase_in_years Years to phase in (default 0)
#'
#' @return A Reform object
#'
#' @examples
#' \dontrun{
#' reform <- reform_benefit_cut(cut_pct = 0.20, effective_year = 2033)
#' }
#'
#' @export
reform_benefit_cut <- function(cut_pct, effective_year, phase_in_years = 0) {
  if (cut_pct <= 0 || cut_pct >= 1) {
    stop("'cut_pct' must be between 0 and 1 (e.g., 0.20 for 20% cut)")
  }

  # Reduce all three factors by the same percentage
  multiplier <- 1 - cut_pct

  create_reform(
    name = sprintf("%.0f%% Benefit Cut", cut_pct * 100),
    description = sprintf("Reduce all benefits by %.0f%%", cut_pct * 100),
    parameters = list(
      list(param = "fact1", value = multiplier, type = "multiply"),
      list(param = "fact2", value = multiplier, type = "multiply"),
      list(param = "fact3", value = multiplier, type = "multiply")
    ),
    effective_year = effective_year,
    phase_in_years = phase_in_years
  )
}
