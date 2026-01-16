
# =============================================================================
# BENEFIT CALCULATIONS
# =============================================================================
#
# This file contains the core benefit calculation functions for the ssmbar package.
# Functions are organized in the order they are called in the benefit calculation
# pipeline (see calculate_benefits() in CL_benefit_calculator.R):
#
#   earnings -> aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#            -> spouse_benefit() -> ret() -> final_benefit()
#
# =============================================================================


# =============================================================================
# SECTION 1: Actuarial Adjustment Helper
# =============================================================================
# This helper function is used by worker_benefit(), spouse_benefit(), and ret()
# to calculate early retirement reduction factors and delayed retirement credits.

#' Early Retirement Reduction Factor and Delayed Retirement Credit Calculation
#'
#' Function that computes a worker's actuarial adjustment to their benefits based on their claiming age.
#'
#' @param claim_age Numeric value that represents the age at which a worker first claims benefits
#' @param nra Numeric value that represents a worker's Normal Retirement Age
#' @param rf1 Numeric value that represents the incremental reduction in benefits for the first 36 months prior to the NRA based on the worker's birth cohort.
#' @param rf2 Numeric value that represents the incremental reduction in benefits for the additional months past 36 that in which benefits are claimed early.
#' @param drc Numeric value that represents the incremental increase in benefits for the months claimed past the NRA, based on the worker's birth cohort.
#'
#' @return act_factor numeric value used for adjusting a worker's PIA to compute their monthly benefit
#'
#' @export
rf_and_drc <- function(claim_age, nra, rf1, rf2, drc) {
  #Benefit reduction factors are descrinbed in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  dist_from_nra <- (claim_age - nra) * 12 #Distance from Normal Retirement Age in months

  #Calculate reduction factors
  rf_amt <- if_else(dist_from_nra >= 0, 0, #If claiming at or above NRA, no RFs
                   if_else(dist_from_nra <= -36, (-36*rf1) + (pmax(-24,(dist_from_nra + 36))*rf2), #If claiming more than three years before NRA. Can't claim before 62
                          dist_from_nra * rf1)) #If claiming less than three years before NRA

  #Calculate DRCs
  drc_amt <- if_else(dist_from_nra <= 0, 0, #If claiming at or below NRA
                    pmin(36*drc, dist_from_nra * drc)) #If claiming above NRA. DRCs are capped at 70

  act_factor <- 1 + rf_amt + drc_amt #Final actuarial factor for adjusting benefits

  return(act_factor)

}


# =============================================================================
# SECTION 2: Main Benefit Calculation Pipeline
# =============================================================================
# These functions are called in sequence to calculate benefits.
# Order: aime() -> pia() -> cola() -> worker_benefit() -> spousal_pia()
#        -> spouse_benefit() -> ret() -> final_benefit()


# -----------------------------------------------------------------------------
# 2.1 AIME Calculation
# -----------------------------------------------------------------------------

#' AIME Calculation
#'
#' Function that computes a worker's Average Indexed Monthly Earnings by age
#'
#' @param worker Dataframe with a worker's earnings by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's AIME by year and age
#'
#' @export
aime <- function(worker, assumptions, debugg = FALSE){ #Function for calculating the AIME of a specific worker
  #How earnings are indexed is described in Section 700.3 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  #AIME Computation is described in Section 701 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html

  # Determine which columns to join from assumptions (avoid duplicates when debugg=TRUE)
  cols_to_join <- c("year", "taxmax", "qc_rec")
  if (!"awi" %in% names(worker)) {
    cols_to_join <- c(cols_to_join, "awi")
  }

  dataset <- worker %>% left_join(assumptions %>% select(all_of(cols_to_join)),
                                  by = "year")

  dataset <- dataset %>% qc_comp(debugg) #Function for determing annual and cumulative QCs earned at each age
  dataset <- dataset %>% comp_period(debugg) #Function for determing a worker's computation period based on their eligiblity age

  # Calculate indexed earnings
  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      awi_age60 = awi[which(age == 60)], #Retrieve AWI at age 60 for indexing past earnings
      index_factor = pmax(awi_age60 / awi, 1), #Calculate indexing factor at each age using the AWI at age 60. Earnings past age 60 are not indexed.
      capped_earn = pmin(earnings, taxmax), #Cap earnings amounts at the taxable maximum at each age
      indexed_earn = capped_earn * index_factor) %>% #Indexed capped earnings amounts
    ungroup()

  #AIME Calculation
  # TODO: Add documentation about eligibility age conditional
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      n <- nrow(.x)
      aime_vals <- numeric(n)
      indexed_earnings <- .x$indexed_earn
      qc_eligible <- .x$qc_tot >= 40 #Workers need 40 QCs to be eligible for retirement benefits. This should be moved to the assumptions at some point
      comp_period <- .x$comp_period #Worker's computation (or, averaging) period
      age_eligible <- .x$age >= .x$elig_age #Worker's eligibility age (age 62 for retirement, age of disability, or age of death of deceased spouse)

      for (i in seq_len(n)) {
        if (!is.na(qc_eligible[i]) && qc_eligible[i] && !is.na(age_eligible[i]) && age_eligible[i]) { #Only calculates AIME if worker has enough QCs and is at or past their eligiblity age.
          years_to_use <- min(i, comp_period[i]) #Restriction so AIME calculation doesn't break if not enough years have passed to equal full comp period.
          top_earnings_sum <- sum(sort(indexed_earnings[1:i], decreasing = TRUE)[1:years_to_use]) #Sum of highest indexed earnings in comp period (35 under current law)
          aime_vals[i] <- floor(top_earnings_sum / (comp_period[i] * 12)) #AIME calculation, rounded to the next lowest dollar (see Handbook)
        }
      }

      .x$aime <- aime_vals #Stores AIME vals
      .x
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime, qc_i, qc_tot, qc_rec, comp_period, elapsed_years, dropout_years, awi_age60, index_factor, capped_earn, indexed_earn),
                                   by = c("id", "age")) #Selects additional output if debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime),
                                   by = c("id", "age")) #left joins only those variables needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.2 PIA Calculation
# -----------------------------------------------------------------------------

#' PIA Calculation
#'
#' Function that computes a worker's Primary Insurance Amount by age
#'
#' @param worker Dataframe with a worker's earnings and AIME by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker dataframe with a worker's retired worker PIA by age
#'
#' @export
pia <- function(worker, assumptions, debugg = FALSE) {
  #PIA calculation is described in Section 706 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0706.html

  dataset <- worker %>% left_join(assumptions %>% select(year, bp1, bp2, fact1, fact2, fact3),
                                  by="year")

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      bp1_age62 = bp1[which(age == 62)], #First PIA bendpoint, determined by age of eligiblity to avoid hardcoding
      bp2_age62 = bp2[which(age == 62)], #Second PIA bendpoint
      fact1_age62 = fact1[which(age == 62)], #First replacement factor (90%)
      fact2_age62 = fact2[which(age == 62)], #Second replacement factor (32%)
      fact3_age62 = fact3[which(age == 62)], #Third replacement factor (15%)
      basic_pia = case_when(
      age >= elig_age ~ floor(case_when( #PIA Calculation -- only occurs in and after a worker's eligibility age
                        aime > bp2_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (bp2_age62 - bp1_age62)) + (fact3_age62 * (aime - bp2_age62)),
                        aime > bp1_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (aime - bp1_age62)),
                        TRUE ~ fact1_age62 * aime
                      )),
      TRUE ~ 0)
    ) %>% select(-bp1, -bp2, -fact1, -fact2, -fact3) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia, bp1_age62, bp2_age62, fact1_age62, fact2_age62, fact3_age62),
                                   by = c("id","age")) #Left joins vars for debugging
  }

  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia),
                                   by = c("id","age")) #Left joins ony vars needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.3 COLA Calculation
# -----------------------------------------------------------------------------

#' COLA Calculation
#'
#' Function that computes a worker's COLA-adjusted PIA
#'
#' @param worker Dataframe with a worker's unadjusted PIA -- both retired worker and spousal -- by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's COLA-adjusted retired worker and spousal PIA by age
#'
#' @export
cola <- function (worker, assumptions, debugg = FALSE) {

  dataset <- worker %>% left_join(assumptions %>% select(year, cpi_w),
                                  by = "year")

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>% mutate(
    cpi_age62 = cpi_w[which(age == 62)], #CPI-W at age 62, used for indexing COLAs
    cpi_index_factor = pmax(cpi_w / cpi_age62, 1), #Indexing factor to index COLAs. Negative COLAs are not payable under CL.
    cola_basic_pia = floor(basic_pia * cpi_index_factor) #COLA'd PIA at each age, rounded down to the nearest dollar
  ) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cpi_age62, cpi_index_factor),
                                   by = c("id","age")) #Left joins full vars for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia),
                                   by = c("id","age")) #Left joins only those vars needed to continue benefit calc
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.4 Worker Benefit Calculation
# -----------------------------------------------------------------------------

#' Retired Worker Benefit Calculation
#'
#' Function that calculates a worker's retirement benefit based on their own earnings record
#'
#' @param worker Dataframe with a worker's COLA-adjusted retired worker PIA by age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a workjer's retired worker benefit by age
#'
#' @export
worker_benefit <- function(worker, assumptions, debugg = FALSE) {
  #Benefit reduction factors are descrinbed in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html

  #Function currently can only handle retired beneficiaries.

  dataset <- worker %>% left_join(assumptions %>% select(year, rf1, rf2, drc, nra, s_rf1, s_rf2), by="year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62, #RF/DRC amounts and NRA are based on year turning age 62 in assumptions.
      rf1_ind = rf1[which(year == yr_62)], #First reduction factor
      rf2_ind = rf2[which(year == yr_62)], #Second reduction factor
      drc_ind = drc[which(year == yr_62)], #Third reduction factor
      nra_ind = nra[which(year == yr_62)], #NRA for age 62 cohort
      act_factor = rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind), #Function that computes actuarial adjustment based on NRA, claiming age, and RFs and DRC levels
      wrk_ben = case_when(
        age >= claim_age ~ floor(cola_basic_pia * act_factor), #Computees retired worker benefit with retired worker COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, nra_ind, rf1_ind, rf2_ind, drc_ind, act_factor, wrk_ben),
                                   by = c("id","age") ) #Left joins variable for debugging
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, wrk_ben),
                                   by = c("id","age") ) #Left joins variables needed to continue benefit calculation
  }

  return(worker)

}


# -----------------------------------------------------------------------------
# 2.5 Spousal PIA Calculation
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
  #The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html

  # Check if we need to use spouse_spec for on-the-fly generation
  use_spouse_spec <- is.null(spouse) &&
                     "spouse_spec" %in% names(worker) &&
                     any(!is.na(worker$spouse_spec))

  if (!is.null(spouse)) {
    # Original behavior: use provided spouse data frame
    dataset <- worker %>% left_join(spouse %>% select(year, age, cola_basic_pia, claim_age) %>% rename(s_age = age, s_pia = cola_basic_pia, s_claim_age = claim_age),
                                    by="year") %>% #Selects and renames spouse's retired worker benefit information needed for computing dependent spousal PIA
      left_join(assumptions %>% select(year, s_pia_share), by="year") %>% #Selects the dependent spousal PIA share of worker's PIA from assumptions
      group_by(id) %>%
      mutate(
        yr_s_claim = year[which(s_age == s_claim_age)], #Retrieves year the spouse claimed retired worker benefit, this is the first year an individual is eligible for dependent spousal benefits
        s_pia_share_ind = s_pia_share[which(age == elig_age)], #Retrieves Spousal PIA share based on birth cohort (constant across time as of now)
        spouse_pia =  case_when(
          year >= yr_s_claim & age >= 62 ~ pmax((s_pia_share_ind * s_pia) - pmax(cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE), #Spousal PIA is equal to 50% of spouse's retired worker PIA, less individual's own retired worker PIA
          TRUE ~ 0) #TODO: Eligiblity age is currently hardcoded and should be moved to the assumptions at some point
      ) %>%
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
    #TODO: This should be the default behavior. The benefit functions should only care about a single worker.
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
      left_join(assumptions %>% select(year, s_pia_share), by="year") %>% #Grabs spousal PIA share from assumptions
      group_by(id) %>%
      group_modify(~ {
        spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker

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
                      by = "year") #Grabs spouse's retired worker PIA info needed to calculate spousal PIA

          # Calculate spousal PIA
          s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])] #Spousal PIA share based on birth cohort
          yr_s_claim <- .x$year[which(.x$s_age == .x$s_claim_age)] #The first year in which the spouse whose record the Spousal PIA is based on claims benefits
          .x$spouse_pia <- if_else(.x$age >= 62 & .x$year >= yr_s_claim, pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE), 0)
          #Spousal PIA is equal to 50% of spouse's retired worker PIA, less individual's own retired worker PIA
          #TODO: Eligibility age is hard-coded and should be moved to the assumptions file
        }
        .x
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
# 2.6 Spousal Benefit Calculation
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
  #How benefits are reduced is described in Sections 723 and 724 of the Social Security Handbook
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html

  # Check if we need to use spouse_spec
  use_spouse_spec <- is.null(spouse) &&
                     "spouse_spec" %in% names(worker) &&
                     any(!is.na(worker$spouse_spec))

  if (!is.null(spouse)) {
    # Original behavior: use provided spouse data frame
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2), by="year") %>% #Left joins parameters needed to adjust spousal benefits (NRA and reduction factors)
      left_join(spouse %>% select(year, age, claim_age) %>% rename(s_age = age, s_claim_age = claim_age),
                by = "year") %>% #Renames needed vars to prevent forced renaming
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        yr_62 = year - age + 62, #Year age 62 -- for grabbing parameters
        nra_ind = nra[which(year == yr_62)], #NRA by birth cohort
        s_rf1_ind = s_rf1[which(year == yr_62)], #First spousal reduction factor, based on birth cohort
        s_rf2_ind = s_rf2[which(year == yr_62)], #Second spousal reduction factor
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), #Computes actuarial adjustment for spousal benefits -- dependent spouse's do not receive DRCs
        yr_s_claim = year[s_age == s_claim_age], #Year their spouse first claims benefits, spousal benefits cannot be claimed before then
        spouse_ben = case_when(
          age >= claim_age & year >= yr_s_claim & age >= elig_age ~ floor(spouse_pia * s_act_factor), #Spousal benefit equals the spousal PIA adjusted for claiming
          TRUE ~ 0
        )) %>% select(-claim_age) %>% ungroup()

  } else if (use_spouse_spec) {
    # New behavior: derive spouse timing from spouse_spec
    dataset <- worker %>%
      left_join(assumptions %>% select(year, nra, s_rf1, s_rf2), by="year") %>%
      group_by(id) %>%
      arrange(id, age) %>%
      group_modify(~ {
        spec <- .x$spouse_spec[1]  # spouse_spec is constant within a worker

        if (is.na(spec)) {
          # No spouse - set spouse_ben to 0
          .x <- .x %>%
            mutate(
              yr_62 = year - age + 62, #Year age 62 -- for grabbing parameters
              nra_ind = nra[which(year == yr_62)], #NRA by birth cohort
              s_rf1_ind = s_rf1[which(year == yr_62)], #First spousal reduction factor, based on birth cohort
              s_rf2_ind = s_rf2[which(year == yr_62)], #Second spousal reduction factor
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), #Actuarial reduction for spouses -- no DRCs are available to them under current law
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
              s_age = year - s_birth_yr, #Spouse age
              s_claim_age_val = s_claim_age, #Spouse claim age
              yr_62 = year - age + 62, #Year age 62 -- for grabbing parameters
              nra_ind = nra[which(year == yr_62)], #Individual's NRA
              s_rf1_ind = s_rf1[which(year == yr_62)],#Spousal reduction factor 1
              s_rf2_ind = s_rf2[which(year == yr_62)], #Spousal reduction factor 2
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), #Spousal actuarial adjustment -- no DRCs for spouses
              yr_s_claim = year[s_age == s_claim_age_val], #Year spouse claims
              spouse_ben = case_when(
                age >= claim_age & year >= yr_s_claim & age >= elig_age ~ floor(spouse_pia * s_act_factor),
                TRUE ~ 0
              )
            ) %>%
            select(-s_age, -s_claim_age_val)
        }
        .x
      }) %>%
      ungroup()

  } else {
    # No spouse specified
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2), by="year") %>%
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        yr_62 = year - age + 62, #Year age 62 for gathering parameters
        nra_ind = nra[which(year == yr_62)], #NRA
        s_rf1_ind = s_rf1[which(year == yr_62)], #First spousal reduction factor
        s_rf2_ind = s_rf2[which(year == yr_62)], #Second spousal reduction factor
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0), #Spousal reduction factor
        spouse_ben = 0
      ) %>%
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


# -----------------------------------------------------------------------------
# 2.7 Retirement Earnings Test (RET)
# -----------------------------------------------------------------------------

#' Retirement Earnings Test Calculation
#'
#' Function that reduces an individual's benefits if their earnings exceed the
#' exempt amounts in the Retirement Earnings Test. Also calculates DRC payback
#' at NRA to account for months of benefits withheld.
#'
#' The RET reduces benefits for workers who have earnings above the exempt amount
#' (ret1) in years between their claiming age and Normal Retirement Age.
#' For every $2 of excess earnings, $1 of benefits is withheld.
#'
#' If the worker has a spouse claiming benefits based on their record, the total
#' benefit pot (worker's benefits + spouse's dependent benefit) is reduced, with
#' the reduction allocated proportionally.
#'
#' At NRA, the actuarial factor is recalculated to account for months of benefits
#' withheld, effectively treating the worker as if they claimed later.
#'
#' @param worker Dataframe with a worker's benefits by year and age
#' @param assumptions Dataframe with the Social Security Trustees assumptions
#' @param factors Data frame for the Trustees' scaled earnings factors. Required
#'   if worker has spouse_spec for on-the-fly spouse benefit generation.
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with RET-adjusted benefits
#'
#' @export
ret <- function(worker, assumptions, factors = NULL, debugg = FALSE) {
  # RET is described in Chapter 18 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.18/handbook-toc18.html
  # Relevant sections are 1801, 1803, 1804, and 1806.

  # Check if worker has spouse_spec for dependent benefit calculation
  has_spouse_spec <- "spouse_spec" %in% names(worker) && any(!is.na(worker$spouse_spec))

  if (has_spouse_spec && is.null(factors)) {
    stop("factors parameter is required when worker has spouse_spec")
  }

  # Join assumptions
  dataset <- worker %>%
    left_join(assumptions %>% select(year, ret1, nra, rf1, rf2, drc, s_rf1, s_rf2, cpi_w), by = "year")

  # Process each worker

  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      worker_data <- .x
      spec <- worker_data$spouse_spec[1]

      # Calculate spouse's dependent benefit if spouse_spec exists
      if (!is.na(spec) && !is.null(factors)) {
        spouse_dep_ben <- generate_spouse_dependent_benefit(worker_data, spec, factors, assumptions)
        worker_data$spouse_dep_ben <- spouse_dep_ben
      } else {
        worker_data$spouse_dep_ben <- 0
      }

      # Get worker's parameters
      claim_age_val <- worker_data$claim_age[1]
      yr_62 <- worker_data$year[1] - worker_data$age[1] + 62
      nra_ind <- worker_data$nra[worker_data$year == yr_62][1]
      rf1_ind <- worker_data$rf1[worker_data$year == yr_62][1]
      rf2_ind <- worker_data$rf2[worker_data$year == yr_62][1]
      drc_ind <- worker_data$drc[worker_data$year == yr_62][1]
      s_rf1_ind <- worker_data$s_rf1[worker_data$year == yr_62][1]
      s_rf2_ind <- worker_data$s_rf2[worker_data$year == yr_62][1]

      # Calculate RET for each year
      worker_data <- worker_data %>%
        mutate(
          # Worker's total monthly benefit (before RET)
          wrk_total_ben = wrk_ben + spouse_ben,

          # Total benefit pot includes spouse's dependent benefit
          total_ben_pot = wrk_total_ben + spouse_dep_ben,

          # Excess earnings and reduction amount (only between claim_age and NRA)
          excess_earnings = case_when(
            age < claim_age | age >= nra_ind ~ 0,
            TRUE ~ pmax(earnings - ret1, 0)
          ),
          ret_reduction = excess_earnings / 2, #Reflects 50% phase-out rate.

          # Cap reduction at total annual benefits
          annual_ben_pot = total_ben_pot * 12,
          ret_reduction_capped = pmin(ret_reduction, annual_ben_pot),

          # Worker's share of the reduction (proportional to their benefits)
          wrk_share = if_else(total_ben_pot > 0, wrk_total_ben / total_ben_pot, 1),
          wrk_reduction = ret_reduction_capped * wrk_share,

          # Allocate worker's reduction between wrk_ben and spouse_ben proportionally
          wrk_ben_share = if_else(wrk_total_ben > 0, wrk_ben / wrk_total_ben, 1),
          spouse_ben_share = if_else(wrk_total_ben > 0, spouse_ben / wrk_total_ben, 0),

          # Reduce benefits
          wrk_ben_reduced = pmax(wrk_ben - (wrk_reduction * wrk_ben_share / 12), 0),
          spouse_ben_reduced = pmax(spouse_ben - (wrk_reduction * spouse_ben_share / 12), 0),

          # Calculate months of worker's benefits withheld
          # months_withheld = annual_reduction / monthly_benefit
          months_withheld = if_else(
            wrk_total_ben > 0 & age >= claim_age & age < nra_ind,
            ceiling(pmin(wrk_reduction / wrk_total_ben, 12)*10)/10,
            0
          )
        )

      # Calculate cumulative months withheld (only up to NRA)
      worker_data <- worker_data %>%
        mutate(
          cum_months_withheld = cumsum(if_else(age < nra_ind, months_withheld, 0))
        )

      # Get cumulative months withheld at NRA for DRC payback
      cum_months_at_nra <- max(worker_data$cum_months_withheld[worker_data$age < nra_ind], 0, na.rm = TRUE)

      # Calculate adjusted actuarial factor at NRA
      # The worker is treated as if they claimed later by the number of months withheld
      effective_claim_age <- min(claim_age_val + (cum_months_at_nra / 12), nra_ind)
      new_act_factor <- rf_and_drc(effective_claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind)
      new_s_act_factor <- rf_and_drc(effective_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0)

      # Get original actuarial factor for comparison
      orig_act_factor <- rf_and_drc(claim_age_val, nra_ind, rf1_ind, rf2_ind, drc_ind)
      orig_s_act_factor <- rf_and_drc(claim_age_val, nra_ind, s_rf1_ind, s_rf2_ind, 0)

      # Apply DRC payback at NRA and beyond
      # Recalculate benefits with the new actuarial factor
      worker_data <- worker_data %>%
        mutate(

          # At NRA and beyond, use the new actuarial factor
          wrk_ben_final = case_when(
            age < claim_age ~ 0,
            age < nra_ind ~ wrk_ben_reduced,
            TRUE ~ floor(cola_basic_pia * new_act_factor)
          ),

          # Spouse benefit does not use new actuarial factor past NRA
          # (spousal early retirement reduction is also adjusted)
          spouse_ben_final = case_when(
            age < claim_age ~ 0,
            age < nra_ind ~ spouse_ben_reduced,
            TRUE ~ floor(spouse_pia * new_s_act_factor)  # Spouse benefit continues as calculated (no additional adjustment)
          ),

          # Store adjustment info
          ret_adj_factor = if_else(age >= nra_ind, new_act_factor, orig_act_factor),
          ret_s_adj_factor = if_else(age >= nra_ind, new_s_act_factor, orig_s_act_factor),
          cum_months_withheld_final = cum_months_at_nra
        )

      # Update the benefit columns
      worker_data <- worker_data %>%
        mutate(
          wrk_ben = wrk_ben_final,
          spouse_ben = spouse_ben_final
        )

      worker_data
    }) %>%
    ungroup()

  # Select output columns
  if (debugg) {
    worker <- worker %>%
      left_join(
        dataset %>% select(
          id, age, wrk_ben, spouse_ben, spouse_dep_ben,
          excess_earnings, ret_reduction, ret_reduction_capped,
          wrk_share, wrk_reduction, months_withheld, cum_months_withheld,
          ret_adj_factor, ret_s_adj_factor, cum_months_withheld_final
        ),
        by = c("id", "age"),
        suffix = c("_orig", "")
      ) %>%
      select(-wrk_ben_orig, -spouse_ben_orig)
  } else {
    worker <- worker %>%
      left_join(
        dataset %>% select(id, age, wrk_ben, spouse_ben),
        by = c("id", "age"),
        suffix = c("_orig", "")
      ) %>%
      select(-wrk_ben_orig, -spouse_ben_orig)
  }

  return(worker)
}


# -----------------------------------------------------------------------------
# 2.8 Final Benefit Calculation
# -----------------------------------------------------------------------------

#' Final Benefit Calculation
#'
#' Function that calculates a worker's total retirement benefit combining
#' retired worker and spousal benefits.
#'
#' @param worker Dataframe with a worker's retired worker and spousal benefit by age
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @return worker Dataframe with a worker's final monthly benefit by age
#'
#' @export
final_benefit <- function(worker, debugg = FALSE) {

  dataset <- worker %>%
    mutate(
      ben = pmax(wrk_ben, 0, na.rm = TRUE) + pmax(spouse_ben, 0, na.rm = TRUE)
    )

  if(debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, ben),
                                   by=c("id","age"))
  } else {
    # Join ben first, then select output columns
    worker <- worker %>%
      left_join(dataset %>% select(id, age, ben), by=c("id","age"))

    # Select core output columns, including sex and spouse_spec if they exist
    output_cols <- c("id", "sex", "year", "age", "spouse_spec", "earnings", "ben")
    available_cols <- intersect(output_cols, names(worker))

    worker <- worker %>% select(all_of(available_cols))
  }

  return(worker)

}


# =============================================================================
# SECTION 3: Internal Helper Functions for Spouse Calculations
# =============================================================================
# These functions support spousal benefit calculations. They are internal
# (@keywords internal) and not exported.


# -----------------------------------------------------------------------------
# 3.1 Parse Spouse Specification
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
# 3.2 Generate Spouse Data
# -----------------------------------------------------------------------------

#' Generate Spouse Data for Benefit Calculations (Internal)
#'
#' Generates spouse's earnings, AIME, and PIA for use in spousal benefit calculations.
#' This is called on-the-fly when spouse_spec is provided but no spouse data frame is passed.
#'
#' @param spouse_spec Character string with spouse specification
#' @param factors Data frame for the Trustees' scaled earnings factors
#' @param assumptions Data frame of the pre-prepared Trustees assumptions
#' @return Data frame with spouse's year, age, claim_age, and basic_pia
#' @keywords internal

generate_spouse_data <- function(spouse_spec, factors, assumptions) {
  # Parse the spouse specification

  spec <- parse_spouse_spec(spouse_spec)
  if (is.null(spec)) {
    return(NULL)
  }

  # Generate spouse earnings using internal function from earnings.R
  spouse <- generate_single_worker(
    birth_yr = spec$birth_yr,
    sex = spec$sex,
    type = spec$type,
    age_claim = spec$age_claim,
    age_elig = 62,  # Standard eligibility age
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

  # Return only the columns needed for spousal benefit calculations
  spouse %>% select(year, age, claim_age, cola_basic_pia)
}


# -----------------------------------------------------------------------------
# 3.3 Generate Spouse's Dependent Benefit
# -----------------------------------------------------------------------------

#' Generate Spouse's Dependent Benefit Based on Worker's Record (Internal)
#'
#' Calculates the spouse's spousal benefit that is based on the worker's earnings record.
#' This is used for RET calculations to determine the total benefit pot subject to reduction.
#'
#' @param worker_data Dataframe for a single worker containing year, age, basic_pia, cola_basic_pia
#' @param spouse_spec Character string with spouse specification
#' @param factors Data frame for the Trustees' scaled earnings factors
#' @param assumptions Data frame of the pre-prepared Trustees assumptions
#' @return Numeric value of spouse's spousal benefit based on worker's record for this year
#' @keywords internal

generate_spouse_dependent_benefit <- function(worker_data, spouse_spec, factors, assumptions) {

  # Parse the spouse specification
  spec <- parse_spouse_spec(spouse_spec)
  if (is.null(spec)) {
    return(rep(0, nrow(worker_data)))
  }

  # Generate spouse's earnings and calculate their own PIA
  spouse <- generate_single_worker(
    birth_yr = spec$birth_yr,
    sex = spec$sex,
    type = spec$type,
    age_claim = spec$age_claim,
    age_elig = 62,
    factors = factors,
    assumptions = assumptions,
    custom_avg_earnings = spec$custom_avg_earnings,
    debugg = FALSE
  )

  spouse <- spouse %>%
    aime(assumptions, debugg = FALSE) %>%
    pia(assumptions, debugg = FALSE) %>%
    cola(assumptions, debugg = FALSE)

  # Get spouse's claim age and birth year
  s_claim_age <- spec$age_claim
  s_birth_yr <- spec$birth_yr

  # For each year in worker_data, calculate spouse's spousal benefit based on worker's record
  # Note: worker_data is already for a single worker (from group_modify), so no need to group_by

  # Get the columns we need from assumptions (only if not already present in worker_data)
  cols_needed <- c("s_pia_share", "s_rf1", "s_rf2")
  cols_to_join <- cols_needed[!cols_needed %in% names(worker_data)]

  result <- worker_data %>%
    mutate(
      s_age = year - s_birth_yr,
      s_claim_age_val = s_claim_age
    ) %>%
    left_join(spouse %>% select(year, cola_basic_pia) %>% rename(s_own_pia = cola_basic_pia), by = "year")

  # Join additional assumption columns if needed
  if (length(cols_to_join) > 0) {
    result <- result %>%
      left_join(assumptions %>% select(year, all_of(cols_to_join)), by = "year")
  }

  # Calculate spouse's spousal PIA based on worker's record
  # Use cpi_w from worker_data (already joined in ret())
  s_pia_share_ind <- result$s_pia_share[which(result$age == 62)[1]]
  yr_62 <- result$year[1] - result$age[1] + 62
  nra_ind <- worker_data$nra[worker_data$year == yr_62][1]
  s_rf1_ind <- result$s_rf1[result$year == yr_62][1]
  s_rf2_ind <- result$s_rf2[result$year == yr_62][1]
  s_dep_act_factor <- rf_and_drc(s_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0)

  result <- result %>%
    mutate(
      # Spouse's spousal PIA = 50% of worker's PIA - spouse's own PIA
      spouse_dep_pia = pmax((s_pia_share_ind * cola_basic_pia) - pmax(s_own_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),

      # Spouse's dependent benefit only starts when both worker has claimed AND spouse has reached their claim age
      yr_s_claim = s_birth_yr + s_claim_age_val,
      spouse_dep_ben = case_when(
        age >= claim_age & year >= yr_s_claim & s_age >= s_claim_age_val ~ floor(spouse_dep_pia * s_dep_act_factor),
        TRUE ~ 0
      )
    )

  return(result$spouse_dep_ben)
}
