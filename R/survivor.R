# -----------------------------------------------------------------------------
# 4.1 Widow(er) PIA Calculation
# -----------------------------------------------------------------------------

#' Widow(er) PIA Calculation
#'
#' Function for computing a worker's Widow(er) PIA based on their spouse's earnings record.
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
        # No spouse - set spouse_pia to 0
        .x$s_pia <- NA_real_
        .x$survivor_pia <- 0
      } else {
        # Get spouse data from cache
        spouse_df <- spouse_data[[spec]]

        # Join spouse PIA by year
        .x <- .x %>%
          left_join(spouse_df %>% select(year, s_age, s_pia, s_wrk_ben, s_death_age, s_claim_age), by = "year")

        # Calculate spousal PIA
        yr_s_death <- .x$year[which(.x$s_age == .x$s_death_age)]

        #https://www.ssa.gov/OP_Home/ssact/title02/0202.htm
        .x$prelim_survivor_pia <- case_when(
          .x$s_death_age < .x$s_claim_age ~ .x$s_pia, #If deceased spouse died before claiming age
          .x$s_wrk_ben > .x$s_pia ~ .x$s_wrk_ben, #If deceased spouse received DRCs
          TRUE ~ pmax(.x$s_wrk_ben, .825 * .x$s_pia) #If deceased spouse claimed at FRA or earlier
        )

        .x$survivor_pia <- if_else(
          .x$age >= elig_age_ret - 2 & .x$year >= yr_s_death,
          pmax(.x$prelim_survivor_pia - pmax(.x$cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
          0
        )
      }
      .x %>% select(-elig_age_retired)
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, s_wrk_ben, s_claim_age, s_death_age, survivor_pia),
                                   by = c("id", "year"))
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, year, survivor_pia),
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
#' @return worker Dataframe with a workjer's retired worker benefit by age
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
      w_act_factor = rf_and_drc(claim_age, nra_ind, w_rf, w_rf, 0), #Function that computes actuarial adjustment based on NRA, claiming age, and RFs and DRC levels
      survivor_ben = case_when(
        age >= claim_age ~ floor(survivor_pia * w_act_factor), #Computees retired worker benefit with retired worker COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, w_rf, w_act_factor, survivor_ben),
                                   by = c("id","age") ) #Left joins variable for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, survivor_ben),
                                   by = c("id","age") ) #Left joins variables needed to continue benefit calculation
  }

  return(worker)

}
