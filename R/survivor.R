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
#' at the later of age 60 or the spouse's death.
#'
#' @param worker Dataframe with a worker's earnings and PIA by year and age
#' @param spouse_data List of spouse data frames (keyed by spouse_spec), where each contains
#'   year, s_age, s_birth_yr, s_claim_age, s_pia, s_death_age. NULL if no spouses or to generate on-the-fly.
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param factors Data frame for the Trustees' scaled earnings factors. Required if spouse_data
#'   is NULL and workers have spouse_spec for on-the-fly generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with survivor_pia and worker_age_at_spouse_death by age
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

      if (is.na(spec) || is.null(spouse_data) || is.null(spouse_data[[spec]])) {
        # No spouse - set all survivor-related columns to NA/0 for consistency
        .x$s_pia <- NA_real_
        .x$s_wrk_ben <- NA_real_
        .x$s_claim_age <- NA_real_
        .x$s_death_age <- NA_real_
        .x$worker_age_at_spouse_death <- NA_real_
        .x$survivor_pia <- 0
      } else {
        # Get spouse data from cache
        spouse_df <- spouse_data[[spec]]

        # Rename spouse columns for survivor calculations to avoid conflicts
        # with s_pia from spousal_pia() (when debugg=TRUE)
        spouse_survivor_df <- spouse_df %>%
          select(year, s_age, s_pia, s_wrk_ben, s_death_age, s_claim_age) %>%
          rename(
            surv_s_pia = s_pia,
            surv_s_wrk_ben = s_wrk_ben,
            surv_s_death_age = s_death_age,
            surv_s_claim_age = s_claim_age
          )

        # Join spouse PIA by year
        .x <- .x %>%
          left_join(spouse_survivor_df, by = "year")

        # Calculate year of spouse's death and worker's age at that time
        yr_s_death <- .x$year[which(.x$s_age == .x$surv_s_death_age)]
        .x$worker_age_at_spouse_death <- .x$age[which(.x$s_age == .x$surv_s_death_age)]

        # Copy back to standard column names for output (needed for debug mode)
        .x$s_death_age <- .x$surv_s_death_age
        .x$s_wrk_ben <- .x$surv_s_wrk_ben
        .x$s_claim_age <- .x$surv_s_claim_age

        #https://www.ssa.gov/OP_Home/ssact/title02/0202.htm
        .x$prelim_survivor_pia <- case_when(
          .x$surv_s_death_age < .x$surv_s_claim_age ~ .x$surv_s_pia, #If deceased spouse died before claiming age
          .x$surv_s_wrk_ben > .x$surv_s_pia ~ .x$surv_s_wrk_ben, #If deceased spouse received DRCs
          TRUE ~ pmax(.x$surv_s_wrk_ben, .825 * .x$surv_s_pia) #If deceased spouse claimed at FRA or earlier
        )

        .x$survivor_pia <- if_else(
          .x$age >= elig_age_ret - 2 & .x$year >= yr_s_death,
          pmax(.x$prelim_survivor_pia - pmax(.x$cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
          0
        )

        # Clean up temporary columns
        .x <- .x %>% select(-starts_with("surv_"))
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()

  # Output worker_age_at_spouse_death (needed by widow_benefit() and final_benefit())
  # s_death_age is already in spouse_data - no need to duplicate it here
  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, year, s_wrk_ben, s_claim_age, s_death_age, worker_age_at_spouse_death, survivor_pia),
                                   by = c("id", "year"))
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, year, worker_age_at_spouse_death, survivor_pia),
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
#'
#' @param worker Dataframe with a worker's COLA-adjusted retired worker PIA by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's survivor benefit by age
#'
#' @export
widow_benefit <- function(worker, assumptions, debugg = FALSE) {
  #Benefit reduction factors are descrinbed in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
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

  dataset <- dataset %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62, #RF/DRC amounts and NRA are based on year turning age 62 in assumptions.
      nra_ind = nra[which(year == yr_62)], #NRA for age 62 cohort
      w_elig_age_ind = elig_age_retired[which(year==yr_62)] - 2,
      w_rf = .285/((nra_ind - w_elig_age_ind)*12),
      # Widow claim age is the later of: (1) worker's claim_age and (2) worker's age when spouse dies
      # If no spouse (worker_age_at_spouse_death is NA), use claim_age
      effective_widow_claim_age = if_else(
        is.na(worker_age_at_spouse_death),
        claim_age,
        pmax(claim_age, worker_age_at_spouse_death)
      ),
      # Actuarial adjustment based on when widow benefits actually start
      w_act_factor = rf_and_drc(effective_widow_claim_age, nra_ind, w_rf, w_rf, 0),
      survivor_ben = case_when(
        age >= effective_widow_claim_age & survivor_pia > 0 ~ floor(survivor_pia * w_act_factor),
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, effective_widow_claim_age, w_rf, w_act_factor, survivor_ben),
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
