
# =============================================================================
# CHILD BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains functions for calculating child Social Security benefits.
# These functions handle:
#   - Parsing child specifications (child_spec strings)
#   - Computing child PIA (Primary Insurance Amount)
#   - Computing child benefits (no actuarial adjustment for children)
#
# Child benefits are governed by 42 USC 402(d).
# Functions are called by calculate_benefits() in calculate_benefits.R
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Parse Child Specification
# -----------------------------------------------------------------------------

#' Parse Child Specification (Internal)
#'
#' Parses a child_spec string into its component parts.
#' child_spec format: "birthyr-disabled" where disabled is TRUE or FALSE
#'
#' Examples:
#'   - "2010-FALSE" - Child born 2010, not disabled
#'   - "2005-TRUE" - Disabled child born 2005 (disabled before age 22)
#'
#' @param child_spec Character string with child specification
#' @return Named list with birth_yr and is_disabled, or NULL if NA/NULL
#' @keywords internal

parse_child_spec <- function(child_spec) {
  if (is.na(child_spec) || is.null(child_spec) || child_spec == "") {
    return(NULL)
  }

  parts <- strsplit(as.character(child_spec), "-")[[1]]
  if (length(parts) != 2) {
    stop(paste("Invalid child_spec format:", child_spec,
               "- Expected format: 'birthyr-disabled' (e.g., '2010-FALSE')"))
  }

  birth_yr <- as.numeric(parts[1])
  is_disabled <- as.logical(toupper(parts[2]))

  if (is.na(birth_yr)) {
    stop(paste("Invalid birth year in child_spec:", child_spec))
  }
  if (is.na(is_disabled)) {
    stop(paste("Invalid disabled flag in child_spec:", child_spec,
               "- Must be TRUE or FALSE"))
  }

  return(list(
    birth_yr = birth_yr,
    is_disabled = is_disabled
  ))
}


# -----------------------------------------------------------------------------
# 2. Child Eligibility Check
# -----------------------------------------------------------------------------

#' Check Child Eligibility for Benefits (Internal)
#'
#' Determines if a child is eligible for benefits in a given year based on:
#' - Non-disabled child: eligible ages 0-17 (benefit stops at age 18)
#' - Disabled child (disabled before 22): eligible indefinitely while worker alive
#' - Worker must be alive and receiving benefits
#'
#' Per 42 USC 402(d)(1), a child is entitled to benefits if:
#' - Under age 18, OR
#' - Age 18-19 and full-time student (not implemented - simplified), OR
#' - Age 18+ and disabled before age 22
#'
#' @param child_birth_yr Numeric birth year of child
#' @param is_disabled Logical indicating if child is disabled (before age 22)
#' @param year Numeric calendar year
#' @param worker_claim_age Worker's claiming age
#' @param worker_age Worker's current age
#' @param worker_death_age Worker's expected death age
#' @return Logical indicating if child is eligible
#' @keywords internal

is_child_eligible <- function(child_birth_yr, is_disabled, year, worker_claim_age, worker_age, worker_death_age) {
  # Child's age in the given year
  child_age <- year - child_birth_yr

  # Worker must have claimed and be alive
  worker_receiving <- worker_age >= worker_claim_age & worker_age < worker_death_age


  # Child eligibility rules (simplified - no student benefits)
  # Non-disabled: ages 0-17 (stops at 18)
  # Disabled before 22: indefinitely
  # Note: is_disabled is scalar per child, so use if/else (not if_else)
  if (is_disabled) {
    child_eligible <- child_age >= 0
  } else {
    child_eligible <- child_age >= 0 & child_age < 18
  }

  return(worker_receiving & child_eligible)
}


# -----------------------------------------------------------------------------
# 3. Child PIA Calculation
# -----------------------------------------------------------------------------

#' Child PIA Calculation
#'
#' Function for computing child benefits based on worker's earnings record.
#' Per 42 USC 402(d)(2), child benefit is 50% of worker's PIA.
#'
#' Child benefits are calculated for up to 3 children (child1_spec, child2_spec, child3_spec).
#' Benefits begin when:
#' - Worker reaches claim_age (is receiving benefits)
#' - Child is eligible (under 18, or disabled before 22)
#' - Worker is alive
#'
#' @param worker Dataframe with a worker's earnings and COLA-adjusted PIA by year and age
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with child1_pia, child2_pia, child3_pia columns
#'
#' @export
child_pia <- function(worker, assumptions, debugg = FALSE) {
  # Child benefits are described in 42 USC 402(d)
  # https://www.ssa.gov/OP_Home/ssact/title02/0202.htm
  #
  # Per 42 USC 402(d)(2): "such child's insurance benefit for each month shall...
  # be equal to one-half of the primary insurance amount of such individual"

  # Check if we have any child_spec columns
  child_cols <- c("child1_spec", "child2_spec", "child3_spec")
  has_children <- any(child_cols %in% names(worker))

  if (!has_children) {
    # No children - add zero columns
    worker$child1_pia <- 0
    worker$child2_pia <- 0
    worker$child3_pia <- 0
    return(worker)
  }

  # Get child_pia_share from assumptions (should be 0.5)
  cols_needed <- c("child_pia_share")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    dataset <- worker %>%
      left_join(assumptions %>% select(year, all_of(cols_missing)), by = "year")
  } else {
    dataset <- worker
  }

  # Process each worker group
  dataset <- dataset %>%
    group_by(id) %>%
    group_modify(~ {
      # Get worker's parameters
      claim_age_val <- .x$claim_age[1]
      death_age_val <- .x$death_age[1]
      elig_age_val <- .x$elig_age[1]
      child_pia_share <- .x$child_pia_share[which(.x$age == elig_age_val)][1]
      if (is.na(child_pia_share)) child_pia_share <- 0.5  # Default

      # Initialize child PIA columns
      .x$child1_pia <- 0
      .x$child2_pia <- 0
      .x$child3_pia <- 0

      # Process each child
      for (i in 1:3) {
        col_name <- paste0("child", i, "_spec")
        pia_col <- paste0("child", i, "_pia")

        if (col_name %in% names(.x)) {
          spec <- .x[[col_name]][1]
          parsed <- parse_child_spec(spec)

          if (!is.null(parsed)) {
            # Calculate child eligibility for all years at once (vectorized)
            eligible <- is_child_eligible(
              child_birth_yr = parsed$birth_yr,
              is_disabled = parsed$is_disabled,
              year = .x$year,
              worker_claim_age = claim_age_val,
              worker_age = .x$age,
              worker_death_age = death_age_val
            )
            # Child PIA = 50% of worker's COLA-adjusted PIA when eligible
            .x[[pia_col]] <- if_else(eligible, child_pia_share * .x$cola_basic_pia, 0)
          }
        }
      }
      .x
    }) %>%
    ungroup()

  # Add child PIA columns to worker
  cols_to_add <- c("child1_pia", "child2_pia", "child3_pia")
  if (debugg) {
    cols_to_add <- c(cols_to_add, "child_pia_share")
  }
  cols_new <- cols_to_add[!cols_to_add %in% names(worker)]

  if (length(cols_new) > 0) {
    worker <- worker %>%
      left_join(dataset %>% select(id, year, all_of(cols_new)), by = c("id", "year"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 4. Child Benefit Calculation
# -----------------------------------------------------------------------------

#' Child Benefit Calculation
#'
#' Function that calculates child benefits. Unlike worker and spousal benefits,
#' child benefits are NOT subject to actuarial reduction for early claiming.
#' Per 42 USC 402(d), child benefits are simply 50% of worker's PIA (COLA-adjusted),
#' floored to the nearest dime.
#'
#' @param worker Dataframe with a worker's child PIAs by year and age
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with child1_ben, child2_ben, child3_ben columns
#'
#' @export
child_benefit <- function(worker, assumptions, debugg = FALSE) {
  # Child benefits are NOT subject to actuarial reduction for early claiming
  # Per 42 USC 402(d)(2), child benefit = 50% of worker's PIA
  # Benefits are floored to nearest $0.10 per 42 USC 415(a)(2)(C)

  # Ensure child_pia columns exist
  if (!"child1_pia" %in% names(worker)) {
    worker <- child_pia(worker, assumptions, debugg = FALSE)
  }

  # Calculate child benefits (no actuarial adjustment - just floor the PIA)
  worker <- worker %>%
    mutate(
      child1_ben = floor_dime(child1_pia),
      child2_ben = floor_dime(child2_pia),
      child3_ben = floor_dime(child3_pia)
    )

  return(worker)
}


# -----------------------------------------------------------------------------
# 5. Calculate Total Child Dependent Benefits (for RET)
# -----------------------------------------------------------------------------

#' Calculate Total Child Dependent Benefits (Internal)
#'
#' Calculates the sum of all child benefits for a worker. Used in RET calculations
#' to determine the total benefit pot subject to reduction.
#'
#' @param worker_data Dataframe for a single worker containing year, age, and child benefit columns
#' @return Numeric vector of total child benefits for each year
#' @keywords internal

calculate_child_dep_benefits <- function(worker_data) {
  child_ben_cols <- c("child1_ben", "child2_ben", "child3_ben")
  existing_cols <- child_ben_cols[child_ben_cols %in% names(worker_data)]

  if (length(existing_cols) == 0) {
    return(rep(0, nrow(worker_data)))
  }

  # Sum all child benefits
  rowSums(worker_data[, existing_cols, drop = FALSE], na.rm = TRUE)
}
