
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
# 2. Spousal PIA Calculation
# -----------------------------------------------------------------------------

#' Spousal PIA Calculation
#'
#' Function for computing a worker's PIA based on their spouse's earnings record.
#' Can use either a pre-computed spouse data frame or generate spouse data on-the-fly
#' from the spouse_spec column in the worker data.
#'
#' @param worker Dataframe with a worker's earnings and PIA by year and age
#' @param spouse Dataframe or null value with a spouse's PIA by the worker's age and year.
#'   If NULL and worker has spouse_spec column, spouse data is generated on-the-fly.
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param factors Data frame for the Trustees' scaled earnings factors. Required if spouse is NULL
#'   and spouse_spec needs to be used for on-the-fly spouse data generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's spousal PIA by age
#'
#' @export
spousal_pia <- function(worker, spouse=NULL, assumptions, factors=NULL, debugg=FALSE) {
  # The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  #
  # Spousal benefits can begin at elig_age_retired (currently 62)
  # Spousal PIA = (s_pia_share * spouse's PIA) - own PIA (if positive)

  # Check if we need to use spouse_spec for on-the-fly generation
  use_spouse_spec <- is.null(spouse) &&
                     "spouse_spec" %in% names(worker) &&
                     any(!is.na(worker$spouse_spec))

  if (!is.null(spouse)) {
    # Original behavior: use provided spouse data frame
    dataset <- worker %>% left_join(spouse %>% select(year, age, cola_basic_pia, claim_age) %>% rename(s_age = age, s_pia = cola_basic_pia, s_claim_age = claim_age),
                                    by="year") %>% # Selects and renames spouse's retired worker benefit information needed for computing dependent spousal PIA
      left_join(assumptions %>% select(year, s_pia_share, elig_age_retired), by="year") %>% # Selects the dependent spousal PIA share and eligibility age from assumptions
      group_by(id) %>%
      mutate(
        elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
        yr_s_claim = year[which(s_age == s_claim_age)], # Retrieves year the spouse claimed retired worker benefit, this is the first year an individual is eligible for dependent spousal benefits
        s_pia_share_ind = s_pia_share[which(age == elig_age)], # Retrieves Spousal PIA share based on birth cohort (constant across time as of now)
        spouse_pia =  case_when(
          year >= yr_s_claim & age >= elig_age_ret ~ pmax((s_pia_share_ind * s_pia) - pmax(cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE), # Spousal PIA = 50% of spouse's PIA - own PIA
          TRUE ~ 0)
      ) %>%
      select(-elig_age_retired) %>%
      ungroup()

    if (debugg) {
      worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, s_pia_share_ind, spouse_pia),
                                     by=c("id","year")) #Left joins variables needed for debugging
    } else {
      worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                     by=c("id","year")) #Left joins only those variables needed to continue benefit calculation
    }

  } else if (use_spouse_spec) {
    # New behavior: generate spouse data on-the-fly from spouse_spec
    # TODO: This should be the default behavior. The benefit functions should only care about a single worker.
    if (is.null(factors)) {
      stop("factors parameter is required when using spouse_spec for on-the-fly spouse data generation")
    }

    # Get unique spouse_specs (excluding NA)
    unique_specs <- unique(worker$spouse_spec[!is.na(worker$spouse_spec)])

    # Generate spouse data for each unique spec and cache it
    spouse_data_cache <- lapply(unique_specs, function(spec) {
      generate_spouse_data(spec, factors, assumptions)
    })
    names(spouse_data_cache) <- unique_specs

    # Process each worker group based on their spouse_spec
    dataset <- worker %>%
      left_join(assumptions %>% select(year, s_pia_share, elig_age_retired), by="year") %>% # Grabs spousal PIA share and eligibility age from assumptions
      group_by(id) %>%
      group_modify(~ {
        spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker
        elig_age_ret <- .x$elig_age_retired[1] # Retirement eligibility age from assumptions

        if (is.na(spec)) {
          # No spouse
          .x$s_pia <- NA_real_
          .x$spouse_pia <- 0
        } else {
          # Get cached spouse data
          spouse_data <- spouse_data_cache[[spec]]

          # Join spouse PIA by year
          .x <- .x %>%
            left_join(spouse_data %>% select(year, age, claim_age, cola_basic_pia) %>% rename(s_age = age, s_claim_age = claim_age, s_pia = cola_basic_pia),
                      by = "year") # Grabs spouse's retired worker PIA info needed to calculate spousal PIA

          # Calculate spousal PIA
          s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])] # Spousal PIA share based on birth cohort
          yr_s_claim <- .x$year[which(.x$s_age == .x$s_claim_age)] # The first year in which the spouse whose record the Spousal PIA is based on claims benefits
          .x$spouse_pia <- if_else(.x$age >= elig_age_ret & .x$year >= yr_s_claim, pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE), 0)
          # Spousal PIA = 50% of spouse's PIA - own PIA
        }
        .x %>% select(-elig_age_retired)
      }) %>%
      ungroup()

    if (debugg) {
      worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, spouse_pia),
                                     by=c("id","year")) #Left joins full var set for debugging
    } else {
      worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                     by=c("id","year")) #Left joins only those vars needed to continue the benefit calculation
    }

  } else {
    # No spouse specified
    dataset <- worker %>% mutate(
      spouse_pia = 0
    )

    worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                   by=c("id","year"))
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 3. Spousal Benefit Calculation
# -----------------------------------------------------------------------------

#' Spousal Benefit Calculation
#'
#' Function that calculates a worker's spousal benefit based on their spouse's earnings record.
#' Can use either a pre-computed spouse data frame or use spouse_spec from worker data
#' to determine spouse's claiming information.
#'
#' @param worker Dataframe with a worker's COLA-adjusted spousal PIA by age
#' @param spouse Dataframe or null value with a worker's spouse's ages and claiming age.
#'   If NULL and worker has spouse_spec column, spouse timing info is derived from spouse_spec.
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's spousal benefit by age
#'
#' @export
spouse_benefit <- function(worker, spouse = NULL, assumptions, debugg = FALSE) {
  # How benefits are reduced is described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #
  # Birth cohort-specific parameters (NRA, reduction factors) are determined at eligibility age

  # Check if we need to use spouse_spec
  use_spouse_spec <- is.null(spouse) &&
                     "spouse_spec" %in% names(worker) &&
                     any(!is.na(worker$spouse_spec))

  if (!is.null(spouse)) {
    # Original behavior: use provided spouse data frame
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2, elig_age_retired), by="year") %>% # Left joins parameters needed to adjust spousal benefits
      left_join(spouse %>% select(year, age, claim_age) %>% rename(s_age = age, s_claim_age = claim_age),
                by = "year") %>% # Renames needed vars to prevent forced renaming
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
        yr_elig = year - age + elig_age_ret, # Year worker reaches eligibility age -- for grabbing birth cohort parameters
        nra_ind = nra[which(year == yr_elig)], # NRA by birth cohort
        s_rf1_ind = s_rf1[which(year == yr_elig)], # First spousal reduction factor, based on birth cohort
        s_rf2_ind = s_rf2[which(year == yr_elig)], # Second spousal reduction factor
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), # Computes actuarial adjustment for spousal benefits -- dependent spouses do not receive DRCs
        yr_s_claim = year[s_age == s_claim_age], # Year their spouse first claims benefits, spousal benefits cannot be claimed before then
        spouse_ben = case_when(
          age >= claim_age & year >= yr_s_claim & age >= elig_age ~ floor(spouse_pia * s_act_factor), # Spousal benefit equals the spousal PIA adjusted for claiming
          TRUE ~ 0
        )) %>% select(-claim_age, -elig_age_retired) %>% ungroup()

  } else if (use_spouse_spec) {
    # New behavior: derive spouse timing from spouse_spec
    dataset <- worker %>%
      left_join(assumptions %>% select(year, nra, s_rf1, s_rf2, elig_age_retired), by="year") %>%
      group_by(id) %>%
      arrange(id, age) %>%
      group_modify(~ {
        spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker
        elig_age_ret <- .x$elig_age_retired[1] # Retirement eligibility age from assumptions

        if (is.na(spec)) {
          # No spouse - set spouse_ben to 0
          .x <- .x %>%
            mutate(
              yr_elig = year - age + elig_age_ret, # Year worker reaches eligibility age -- for grabbing parameters
              nra_ind = nra[which(year == yr_elig)], # NRA by birth cohort
              s_rf1_ind = s_rf1[which(year == yr_elig)], # First spousal reduction factor, based on birth cohort
              s_rf2_ind = s_rf2[which(year == yr_elig)], # Second spousal reduction factor
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), # Actuarial reduction for spouses -- no DRCs available under current law
              spouse_ben = 0
            )
        } else {
          # Parse spouse_spec to get spouse's birth_yr and claim_age
          parsed <- parse_spouse_spec(spec)
          s_birth_yr <- parsed$birth_yr
          s_claim_age <- parsed$age_claim

          # Calculate spouse's age at each year and when they claim
          .x <- .x %>%
            mutate(
              s_age = year - s_birth_yr, # Spouse age
              s_claim_age_val = s_claim_age, # Spouse claim age
              yr_elig = year - age + elig_age_ret, # Year worker reaches eligibility age -- for grabbing parameters
              nra_ind = nra[which(year == yr_elig)], # Individual's NRA
              s_rf1_ind = s_rf1[which(year == yr_elig)], # Spousal reduction factor 1
              s_rf2_ind = s_rf2[which(year == yr_elig)], # Spousal reduction factor 2
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), # Spousal actuarial adjustment -- no DRCs for spouses
              yr_s_claim = year[s_age == s_claim_age_val], # Year spouse claims
              spouse_ben = case_when(
                age >= claim_age & year >= yr_s_claim & age >= elig_age ~ floor(spouse_pia * s_act_factor),
                TRUE ~ 0
              )
            ) %>%
            select(-s_age, -s_claim_age_val)
        }
        .x %>% select(-elig_age_retired)
      }) %>%
      ungroup()

  } else {
    # No spouse specified
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2, elig_age_retired), by="year") %>%
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        elig_age_ret = first(elig_age_retired), # Retirement eligibility age from assumptions
        yr_elig = year - age + elig_age_ret, # Year worker reaches eligibility age for gathering parameters
        nra_ind = nra[which(year == yr_elig)], # NRA
        s_rf1_ind = s_rf1[which(year == yr_elig)], # First spousal reduction factor
        s_rf2_ind = s_rf2[which(year == yr_elig)], # Second spousal reduction factor
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), # Spousal reduction factor
        spouse_ben = 0
      ) %>% select(-elig_age_retired) %>%
      ungroup()
  }

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, s_rf1_ind, s_rf2_ind, s_act_factor, spouse_ben),
                                   by = c("id","age") ) #Left join all vars for debugging
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, age, spouse_ben),
                                   by = c("id","age") ) #Left join only vars needed for benefit calc
  }

  return(worker)

}
