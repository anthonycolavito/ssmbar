# =============================================================================
# SURVIVOR (WIDOW/WIDOWER) BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains functions for calculating widow(er) Social Security benefits.
# These functions handle:
#   - Computing survivor PIA from deceased spouse's earnings record
#   - Applying widow(er) actuarial adjustments
#
# Functions are called by calculate_benefits() in CL_benefit_calculator.R
# after spousal benefit calculations and before RET.
#
# Reference: SSA Handbook Chapter 4 (Survivors Insurance)
# https://www.ssa.gov/OP_Home/handbook/handbook.04/handbook-toc04.html
#
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1 Widow(er) PIA Calculation
# -----------------------------------------------------------------------------

#' Widow(er) PIA Calculation
#'
#' Function for computing a worker's survivor PIA based on their deceased spouse's
#' earnings record. The survivor benefit is payable after the spouse dies, starting
#' at the later of age 60 or the spouse's death (or age 50 for disabled widow(er)s).
#'
#' Disabled widow(er) benefits are available to disabled workers who meet all criteria:
#' - Worker is disabled (elig_age < elig_age_retired)
#' - Worker is between ages 50-59 (60+ qualifies for standard widow benefits)
#' - Spouse has died and had a PIA
#' - Disability occurred no more than 7 years after spouse's death
#'
#' @param worker Dataframe with a worker's earnings and PIA by year and age
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia, s_death_age. NULL if no spouses or to generate on-the-fly.
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param factors Data frame for the Trustees' scaled earnings factors. Required if spouse_data
#'   is NULL and workers have spouse_spec for on-the-fly generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with survivor_pia, worker_age_at_spouse_death, and
#'   is_disabled_widow (flag for disabled widow(er) status) by age
#'
#' @export
widow_pia <- function(worker, spouse_data = NULL, assumptions, factors = NULL, debugg = FALSE) {
  # The survivor insurance benefit is described in Section 400 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.04/handbook-0400.html
  #
  # Eligibility for Widow(er) Benefits is described in Section 401 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.04/handbook-0401.html
  #
  # Widow(er) benefits can begin at the later of age 60 or the death of the spouse
  # For DISABLED widow(er)s, benefits can begin at the later of age 50, disability onset,

  # or spouse's death, provided disability occurred within 7 years of spouse's death.
  # See SSA Handbook Section 401.1 for disabled widow(er) eligibility.
  #
  # Widow(er) PIA amount is described in https://www.ssa.gov/OP_Home/ssact/title02/0202.htm

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
  cols_needed <- c("elig_age_retired")
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
      worker_elig_age <- .x$elig_age[1]  # Worker's eligibility age (disability age for disabled workers)

      if (is.na(spec) || is.null(spouse_data) || is.null(spouse_data[[spec]])) {
        # No spouse - set all survivor-related columns to NA/0 for consistency
        .x$s_pia <- NA_real_
        .x$s_wrk_ben <- NA_real_
        .x$s_claim_age <- NA_real_
        .x$s_death_age <- NA_real_
        .x$worker_age_at_spouse_death <- NA_real_
        .x$survivor_pia <- 0
        .x$is_disabled_widow <- FALSE
      } else {
        # Get spouse data from cache
        spouse_df <- spouse_data[[spec]]

        # Rename spouse columns for survivor calculations to avoid conflicts
        # with s_pia and s_age from spousal_pia()
        spouse_survivor_df <- spouse_df %>%
          select(year, s_age, s_pia, s_wrk_ben, s_death_age, s_claim_age) %>%
          rename(
            surv_s_age = s_age,
            surv_s_pia = s_pia,
            surv_s_wrk_ben = s_wrk_ben,
            surv_s_death_age = s_death_age,
            surv_s_claim_age = s_claim_age
          )

        # Join spouse PIA by year
        .x <- .x %>%
          left_join(spouse_survivor_df, by = "year")

        # Calculate year of spouse's death and worker's age at that time
        yr_s_death <- .x$year[which(.x$surv_s_age == .x$surv_s_death_age)]
        .x$worker_age_at_spouse_death <- .x$age[which(.x$surv_s_age == .x$surv_s_death_age)]

        # Copy back to standard column names for output (needed for debug mode)
        .x$s_death_age <- .x$surv_s_death_age
        .x$s_wrk_ben <- .x$surv_s_wrk_ben
        .x$s_claim_age <- .x$surv_s_claim_age

        # Determine if worker qualifies for DISABLED widow(er) benefits
        # SSA Handbook Section 401.1: Disabled widow(er) benefits available to those who:
        # 1. Are disabled (elig_age < elig_age_ret)
        # 2. Are age 50-59 at the time of first claiming (60+ uses standard widow rules)
        # 3. Became disabled no more than 7 years after spouse's death
        #
        # Once someone claims disabled widow(er) benefits, they remain classified as such
        # for life - the BC code does NOT change at age 60.
        #
        # Calculate worker's birth year and year of disability
        birth_yr <- .x$year[1] - .x$age[1]
        yr_disability <- birth_yr + worker_elig_age
        is_disabled <- worker_elig_age < elig_age_ret

        # Check 7-year rule: disability must occur within 7 years of spouse's death
        meets_7yr_rule <- is_disabled && (yr_disability <= yr_s_death + 7)

        # Determine the effective widow eligibility age
        # - Standard: age 60 (elig_age_ret - 2)
        # - Disabled widow(er): age 50, but only if 7-year rule is met
        standard_widow_elig_age <- elig_age_ret - 2  # = 60
        disabled_widow_elig_age <- 50

        # Calculate the age when disabled widow(er) benefits would first start
        # This is the later of: age 50, disability onset age, worker's age when spouse dies
        worker_age_at_spouse_death_val <- .x$age[which(.x$s_age == .x$surv_s_death_age)]
        disabled_widow_start_age <- max(disabled_widow_elig_age, worker_elig_age, worker_age_at_spouse_death_val, na.rm = TRUE)

        # Worker is classified as a disabled widow(er) if:
        # 1. They are disabled AND meet 7-year rule
        # 2. Their benefit would start at age 50-59 (before standard eligibility age 60)
        # This is a CONSTANT flag for all ages - once a disabled widow(er), always one
        is_disabled_widow_beneficiary <- is_disabled & meets_7yr_rule & disabled_widow_start_age < standard_widow_elig_age
        .x$is_disabled_widow <- is_disabled_widow_beneficiary

        # For disabled widow(er)s, use age 50 as eligibility threshold
        # For all others, use standard age 60
        widow_elig_age <- if_else(
          is_disabled & meets_7yr_rule,
          disabled_widow_elig_age,
          standard_widow_elig_age
        )

        #https://www.ssa.gov/OP_Home/ssact/title02/0202.htm
        .x$prelim_survivor_pia <- case_when(
          .x$surv_s_death_age < .x$surv_s_claim_age ~ .x$surv_s_pia, #If deceased spouse died before claiming age
          .x$surv_s_wrk_ben > .x$surv_s_pia ~ .x$surv_s_wrk_ben, #If deceased spouse received DRCs
          TRUE ~ pmax(.x$surv_s_wrk_ben, .825 * .x$surv_s_pia) #If deceased spouse claimed at FRA or earlier
        )

        .x$survivor_pia <- if_else(
          .x$age >= widow_elig_age & .x$year >= yr_s_death,
          pmax(.x$prelim_survivor_pia - pmax(.x$cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
          0
        )

        # Clean up temporary columns
        .x <- .x %>% select(-starts_with("surv_"))
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()

  # Output worker_age_at_spouse_death and is_disabled_widow (needed by widow_benefit() and final_benefit())
  # s_death_age is already in spouse_data - no need to duplicate it here
  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, year, s_wrk_ben, s_claim_age, s_death_age, worker_age_at_spouse_death, survivor_pia, is_disabled_widow),
                                   by = c("id", "year"))
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, year, worker_age_at_spouse_death, survivor_pia, is_disabled_widow),
                                   by = c("id", "year"))
  }

  return(worker)
}

# -----------------------------------------------------------------------------
# 4.2 Widow Benefit Calculation
# -----------------------------------------------------------------------------

#' Widow Benefit Calculation
#'
#' Function that calculates a widow's benefit based on the deceased spouse's earnings record.
#' For disabled widow(er)s (age 50-59), the actuarial reduction is calculated as if they
#' claimed at age 60, per SSA rules.
#'
#' @param worker Dataframe with a worker's COLA-adjusted retired worker PIA by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's survivor benefit by age
#'
#' @export
widow_benefit <- function(worker, assumptions, debugg = FALSE) {
  # TODO: Documentation needs verification - the citations below are for retired worker
  # benefits (Sections 720, 723, 724), not widow benefits. Need to add proper citations
  # for widow reduction factors. See POMS RS 00615.301 for widow benefit reductions.
  #
  # For DISABLED widow(er)s (age 50-59), the actuarial reduction is calculated as if
  # they claimed at age 60, regardless of their actual age. This means all disabled
  # widow(er)s receive the same reduction factor as a non-disabled widow claiming at 60.
  # See SSA Handbook Section 401.1 and POMS RS 00615.301.
  #
  # Current citations (for reference, but apply to retired workers not widows):
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  # Skip join if columns already present (from join_all_assumptions)
  cols_needed <- c("nra","elig_age_retired")
  cols_missing <- cols_needed[!cols_needed %in% names(worker)]

  if (length(cols_missing) > 0) {
    cols_to_join <- c("year", cols_missing)
    dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                    by = "year")
  } else {
    dataset <- worker
  }

  # Handle case where is_disabled_widow column doesn't exist (backwards compatibility)
  if (!"is_disabled_widow" %in% names(dataset)) {
    dataset$is_disabled_widow <- FALSE
  }

  dataset <- dataset %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62, #RF/DRC amounts and NRA are based on year turning age 62 in assumptions.
      nra_ind = nra[which(year == yr_62)], #NRA for age 62 cohort
      w_elig_age_ind = elig_age_retired[which(year==yr_62)] - 2,  # Standard widow eligibility age (60)
      # TODO: Verify w_rf formula against POMS RS 00615.301 (widow reduction factor)
      w_rf = .285/((nra_ind - w_elig_age_ind)*12),

      # Widow claim age is the later of: (1) worker's claim_age and (2) worker's age when spouse dies
      # If no spouse (worker_age_at_spouse_death is NA), use claim_age
      actual_widow_claim_age = if_else(
        is.na(worker_age_at_spouse_death),
        claim_age,
        pmax(claim_age, worker_age_at_spouse_death)
      ),

      # For DISABLED widow(er)s, actuarial factor is calculated as if claiming at age 60
      # regardless of their actual claim age (which may be 50-59).
      # The effective_widow_claim_age is used ONLY for actuarial factor calculation.
      effective_widow_claim_age = if_else(
        is_disabled_widow,
        w_elig_age_ind,  # Use age 60 for actuarial calculation
        actual_widow_claim_age  # Use actual claim age for non-disabled
      ),

      # Actuarial adjustment based on effective claim age (age 60 for disabled widow(er)s)
      w_act_factor = rf_and_drc(effective_widow_claim_age, nra_ind, w_rf, w_rf, 0),

      # For disabled widow(er)s, the benefit eligibility age is 50 (handled in widow_pia)
      # For standard widows, it's 60. The actual_widow_claim_age determines when benefits start.
      # For disabled widow(er)s, benefits start at the later of: age 50, disability onset, spouse death
      disabled_widow_claim_age = pmax(50, elig_age, worker_age_at_spouse_death, na.rm = TRUE),

      # Determine when survivor benefits actually start
      benefit_start_age = if_else(
        is_disabled_widow,
        disabled_widow_claim_age,
        actual_widow_claim_age
      ),

      survivor_ben = case_when(
        age >= benefit_start_age & survivor_pia > 0 ~ floor(survivor_pia * w_act_factor),
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, actual_widow_claim_age, effective_widow_claim_age, benefit_start_age, w_rf, w_act_factor, survivor_ben),
                                   by = c("id","age") ) #Left joins variable for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, survivor_ben),
                                   by = c("id","age") ) #Left joins variables needed to continue benefit calculation
  }

  return(worker)

}

# =============================================================================
# TODO - Documentation Review Needed
# =============================================================================
#
# The following items need documentation updates or verification:
#
# 1. widow_pia():
#    - Verify the 82.5% floor rule calculation (line ~127) against SSA POMS
#    - Add more detailed explanation of prelim_survivor_pia calculation
#    - Document edge cases: spouse dies before claiming, spouse receives DRCs
#
# 2. widow_benefit():
#    - Add SSA Handbook citation for widow reduction factor formula (w_rf)
#    - Verify widow eligibility age calculation (elig_age_retired - 2 = 60)
#    - Document the effective_widow_claim_age logic more clearly
#
# 3. General:
#    - Add examples to roxygen documentation
#    - Consider adding a vignette for survivor benefits
#
# =============================================================================
