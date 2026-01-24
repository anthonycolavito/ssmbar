
# =============================================================================
# SPOUSAL BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains functions for calculating spousal Social Security benefits.
# These functions handle:
#   - Parsing spouse specifications (spouse_spec strings)
#   - Computing spousal PIA (Primary Insurance Amount)
#   - Computing spousal benefits with actuarial adjustments
#
# Functions are called by calculate_benefits() in CL_benefit_calculator.R
#
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Parse Spouse Specification
# -----------------------------------------------------------------------------

#' Parse Spouse Specification (Internal)
#'
#' Parses a spouse_spec string into its component parts.
#' spouse_spec format: "type-sex-birthyr-claimage" (e.g., "low-female-1962-65")
#' For custom type: "customXXXXX-sex-birthyr-claimage" (e.g., "custom50000-female-1962-65")
#'
#' @param spouse_spec Character string with spouse specification
#' @return Named list with type, sex, birth_yr, age_claim, and custom_avg_earnings (if applicable)
#' @keywords internal

parse_spouse_spec <- function(spouse_spec) {
  if (is.na(spouse_spec) || is.null(spouse_spec)) {
    return(NULL)
  }

  parts <- strsplit(spouse_spec, "-")[[1]]
  if (length(parts) != 4) {
    stop(paste("Invalid spouse_spec format:", spouse_spec))
  }

  type_part <- parts[1]
  sex <- parts[2]
  birth_yr <- as.numeric(parts[3])
  age_claim <- as.numeric(parts[4])

  # Check if type is custom (starts with "custom" followed by earnings amount)
  if (grepl("^custom", type_part)) {
    type <- "custom"
    custom_avg_earnings <- as.numeric(gsub("^custom", "", type_part))
  } else {
    type <- type_part
    custom_avg_earnings <- NULL
  }

  return(list(
    type = type,
    sex = sex,
    birth_yr = birth_yr,
    age_claim = age_claim,
    custom_avg_earnings = custom_avg_earnings
  ))
}


# -----------------------------------------------------------------------------
# 2. Generate Spouse Data
# -----------------------------------------------------------------------------

#' Generate Spouse Data (Internal)
#'
#' Consolidated function that generates all spouse data needed for spousal benefit
#' calculations. Parses spouse_spec, generates spouse's earnings, and calculates
#' their AIME, PIA, and COLA-adjusted PIA.
#'
#' This function is called ONCE at the start of the benefit calculation pipeline,
#' and the resulting spouse data is passed to spousal_pia(), spouse_benefit(), and ret().
#'
#' @param spouse_spec Character string with spouse specification in format
#'   "type-sex-birthyr-claimage" (e.g., "low-female-1962-65")
#' @param factors Data frame for the Trustees' scaled earnings factors
#' @param assumptions Data frame of the pre-prepared Trustees assumptions
#' @return Data frame with spouse's year, s_age, s_birth_yr, s_claim_age, s_pia (COLA'd PIA),
#'   or NULL if spouse_spec is NA/NULL
#' @keywords internal

generate_spouse <- function(spouse_spec, factors, assumptions) {
  # Parse the spouse specification
  spec <- parse_spouse_spec(spouse_spec)
  if (is.null(spec)) {
    return(NULL)
  }

  # Get eligibility age from assumptions
  elig_age_ret <- assumptions$elig_age_retired[1]

  # Generate spouse earnings using internal function from earnings.R
  spouse <- generate_single_worker(
    birth_yr = spec$birth_yr,
    sex = spec$sex,
    type = spec$type,
    age_claim = spec$age_claim,
    age_elig = elig_age_ret,  # Retirement eligibility age from assumptions
    factors = factors,
    assumptions = assumptions,
    custom_avg_earnings = spec$custom_avg_earnings,
    debugg = FALSE
  )

  # Calculate spouse's AIME and PIA
  spouse <- spouse %>%
    aime(assumptions, debugg = FALSE) %>%
    pia(assumptions, debugg = FALSE) %>%
    cola(assumptions, debugg = FALSE)

  # Return data frame with columns needed for spousal benefit calculations
  # Rename columns with s_ prefix for clarity when joining with worker data
  spouse %>%
    mutate(
      s_age = age,
      s_birth_yr = spec$birth_yr,
      s_claim_age = claim_age,
      s_pia = cola_basic_pia  # Spouse's COLA'd PIA (for worker's spousal_pia calculation)
    ) %>%
    select(year, s_age, s_birth_yr, s_claim_age, s_pia)
}


# -----------------------------------------------------------------------------
# 3. Spousal PIA Calculation
# -----------------------------------------------------------------------------

#' Spousal PIA Calculation
#'
#' Function for computing a worker's spousal PIA based on their spouse's earnings record.
#' Expects pre-generated spouse data from generate_spouse(), or generates it on-the-fly
#' from spouse_spec if spouse_data is NULL.
#'
#' @param worker Dataframe with a worker's earnings and PIA by year and age
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia. NULL if no spouses or to generate on-the-fly.
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param factors Data frame for the Trustees' scaled earnings factors. Required if spouse_data
#'   is NULL and workers have spouse_spec for on-the-fly generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's spousal PIA by age
#'
#' @export
spousal_pia <- function(worker, spouse_data = NULL, assumptions, factors = NULL, debugg = FALSE) {
  # The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  #
  # Spousal benefits can begin at elig_age_retired (currently 62).
  # They cannot be claimed before the spouse whose record they are based on claims retired worker benefits.
  # Spousal PIA = (s_pia_share * spouse's PIA) - own PIA (if positive)

  # Check if we have spouse_spec in the worker data
  has_spouse_spec <- "spouse_spec" %in% names(worker) && any(!is.na(worker$spouse_spec))

  # If no spouse_data provided but workers have spouse_spec, generate it on-the-fly
  if (is.null(spouse_data) && has_spouse_spec) {
    if (is.null(factors)) {
      stop("factors parameter is required when spouse_data is NULL and workers have spouse_spec")
    }
    # Get unique spouse_specs (excluding NA)
    unique_specs <- unique(worker$spouse_spec[!is.na(worker$spouse_spec)])
    # Generate spouse data for each unique spec and cache it
    spouse_data <- lapply(unique_specs, function(spec) {
      generate_spouse(spec, factors, assumptions)
    })
    names(spouse_data) <- unique_specs
  }

  # Process each worker group based on their spouse_spec
  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("s_pia_share", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>%
    group_by(id) %>%
    group_modify(~ {
      spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker
      elig_age_ret <- .x$elig_age_retired[1]

      if (is.na(spec) || is.null(spouse_data) || is.null(spouse_data[[spec]])) {
        # No spouse - set spouse_pia to 0
        .x$s_pia <- NA_real_
        .x$spouse_pia <- 0
      } else {
        # Get spouse data from cache
        spouse_df <- spouse_data[[spec]]

        # Join spouse PIA by year
        .x <- .x %>%
          left_join(spouse_df %>% select(year, s_age, s_claim_age, s_pia), by = "year")

        # Calculate spousal PIA
        s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])]
        yr_s_claim <- .x$year[which(.x$s_age == .x$s_claim_age)]

        .x$spouse_pia <- if_else(
          .x$age >= elig_age_ret & .x$year >= yr_s_claim,
          pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
          0
        )
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, spouse_pia),
                                   by = c("id", "year"))
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                   by = c("id", "year"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 4. Spousal Benefit Calculation
# -----------------------------------------------------------------------------

#' Spousal Benefit Calculation
#'
#' Function that calculates a worker's spousal benefit based on their spouse's earnings record.
#' Expects pre-generated spouse data from generate_spouse(), or derives spouse timing
#' from spouse_spec if spouse_data is NULL.
#'
#' @param worker Dataframe with a worker's COLA-adjusted spousal PIA by age
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia. NULL if no spouses or to derive from spouse_spec.
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's spousal benefit by age
#'
#' @export
spouse_benefit <- function(worker, spouse_data = NULL, assumptions, debugg = FALSE) {
  # How benefits are reduced is described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #
  # Birth cohort-specific parameters (NRA, reduction factors) are determined at eligibility age

  # Process each worker group
  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("nra", "s_rf1", "s_rf2", "elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>%
      left_join(assumptions %>% select(all_of(cols_to_join)), by = "year")
  } else {
    dataset <- worker
  }

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker
      elig_age_ret <- .x$elig_age_retired[1]

      # Get worker's birth cohort parameters
      yr_elig <- .x$year[1] - .x$age[1] + elig_age_ret
      nra_ind <- .x$nra[.x$year == yr_elig][1]
      s_rf1_ind <- .x$s_rf1[.x$year == yr_elig][1]
      s_rf2_ind <- .x$s_rf2[.x$year == yr_elig][1]
      claim_age_val <- .x$claim_age[1]

      # Actuarial adjustment for spousal benefits (no DRCs for spouses)
      # Spouses are not eligible to receive delayed retirement credits
      # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html to see how spousal benefits are determined.
      s_act_factor <- rf_and_drc(claim_age_val, nra_ind, s_rf1_ind, s_rf2_ind, 0)

      if (is.na(spec)) {
        # No spouse - set spouse_ben to 0
        .x <- .x %>%
          mutate(
            nra_ind = nra_ind,
            s_rf1_ind = s_rf1_ind,
            s_rf2_ind = s_rf2_ind,
            s_act_factor = s_act_factor,
            spouse_ben = 0
          )
      } else {
        # Get spouse timing info
        if (!is.null(spouse_data) && !is.null(spouse_data[[spec]])) {
          # Use pre-generated spouse data
          spouse_df <- spouse_data[[spec]]
          s_birth_yr <- spouse_df$s_birth_yr[1]
          s_claim_age <- spouse_df$s_claim_age[1]
        } else {
          # Parse spouse_spec to get timing info
          parsed <- parse_spouse_spec(spec)
          s_birth_yr <- parsed$birth_yr
          s_claim_age <- parsed$age_claim
        }

        # Calculate spouse's age and when they claim
        .x <- .x %>%
          mutate(
            s_age = year - s_birth_yr,
            s_claim_age_val = s_claim_age,
            nra_ind = nra_ind,
            s_rf1_ind = s_rf1_ind,
            s_rf2_ind = s_rf2_ind,
            s_act_factor = s_act_factor,
            yr_s_claim = year[s_age == s_claim_age_val],
            spouse_ben = case_when(
              age >= claim_age & year >= yr_s_claim & age >= elig_age ~ floor(spouse_pia * s_act_factor),
              TRUE ~ 0
            )
          ) %>%
          select(-s_age, -s_claim_age_val, -yr_s_claim)
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, nra_ind, s_rf1_ind, s_rf2_ind, s_act_factor, spouse_ben),
                                   by = c("id", "age"))
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, age, spouse_ben),
                                   by = c("id", "age"))
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 5. Spouse Dependent Benefit Calculation (for RET)
# -----------------------------------------------------------------------------

#' Calculate Spouse's Dependent Benefit Based on Worker's Record (Internal)
#'
#' Calculates the spouse's spousal benefit that is based on the WORKER's earnings record.
#' This is used for RET calculations to determine the total benefit pot subject to reduction.
#' This is the reverse of spousal_pia() which calculates the worker's benefit based on spouse's record.
#'
#' @param worker_data Dataframe for a single worker containing year, age, cola_basic_pia, claim_age
#' @param spouse_df Data frame from generate_spouse() containing year, s_age, s_birth_yr, s_claim_age, s_pia
#' @param assumptions Data frame of the pre-prepared Trustees assumptions
#' @return Numeric vector of spouse's dependent benefit for each year in worker_data
#' @keywords internal

calculate_spouse_dep_benefit <- function(worker_data, spouse_df, assumptions) {
  # The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  #
  # Spousal benefits can begin at elig_age_retired (currently 62).
  # They cannot be claimed before the spouse whose record they are based on claims retired worker benefits.
  # Spousal PIA = (s_pia_share * spouse's PIA) - own PIA (if positive)

  # Get spouse's info from spouse_df
  s_birth_yr <- spouse_df$s_birth_yr[1]
  s_claim_age <- spouse_df$s_claim_age[1]
  worker_claim_age <- worker_data$claim_age[1]

  # Get eligibility age from assumptions
  elig_age_ret <- assumptions$elig_age_retired[1]

  # Prepare spouse data for join - rename s_pia to s_own_pia before joining
  spouse_pia_data <- spouse_df %>%
    select(year, s_pia) %>%
    rename(s_own_pia = s_pia)

  # Join spouse's own PIA by year
  result <- worker_data %>%
    mutate(
      s_age = year - s_birth_yr,
      s_claim_age_val = s_claim_age
    ) %>%
    left_join(spouse_pia_data, by = "year")

  # Get spousal parameters from assumptions only if not already in worker_data
  # (worker_data from ret() already has these columns joined)
  cols_needed <- c("s_pia_share", "s_rf1", "s_rf2", "nra")
  cols_to_join <- cols_needed[!cols_needed %in% names(result)]

  if (length(cols_to_join) > 0) {
    result <- result %>%
      left_join(assumptions %>% select(year, all_of(cols_to_join)), by = "year")
  }

  # Get spouse's actuarial adjustment (based on spouse's claim age, not worker's)
  # Use worker's eligibility year to determine NRA and reduction factors
  yr_elig <- result$year[1] - result$age[1] + elig_age_ret
  nra_ind <- result$nra[result$year == yr_elig][1]
  s_rf1_ind <- result$s_rf1[result$year == yr_elig][1]
  s_rf2_ind <- result$s_rf2[result$year == yr_elig][1]
  s_dep_act_factor <- rf_and_drc(s_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0)

  # Calculate spouse's dependent PIA and benefit
  s_pia_share_ind <- result$s_pia_share[result$age == elig_age_ret][1]

  result <- result %>%
    mutate(
      # Spouse's spousal PIA = 50% of worker's PIA - spouse's own PIA
      spouse_dep_pia = pmax((s_pia_share_ind * cola_basic_pia) - pmax(s_own_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),

      # Spouse's dependent benefit only starts when both worker has claimed AND spouse has reached their claim age
      yr_s_claim = s_birth_yr + s_claim_age_val,
      spouse_dep_ben = case_when(
        age >= worker_claim_age & year >= yr_s_claim & s_age >= s_claim_age_val ~ floor(spouse_dep_pia * s_dep_act_factor),
        TRUE ~ 0
      )
    )

  return(result$spouse_dep_ben)
}
