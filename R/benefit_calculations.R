
# =============================================================================
# Helper Functions for Spouse Data Generation
# =============================================================================

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


# =============================================================================
# Benefit Calculation Functions
# =============================================================================

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

  dataset <- dataset %>% qc_comp(debugg)
  dataset <- dataset %>% comp_period(debugg) #QC and Comp Period Calculation

  # Calculate indexed earnings
  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      awi_age60 = awi[which(age == 60)],
      index_factor = pmax(awi_age60 / awi, 1),
      capped_earn = pmin(earnings, taxmax),
      indexed_earn = capped_earn * index_factor) %>%
    ungroup()

  #AIME Calculation
  dataset <- dataset %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      n <- nrow(.x)
      aime_vals <- numeric(n)
      indexed_earnings <- .x$indexed_earn
      qc_eligible <- .x$qc_tot >= 40
      comp_period <- .x$comp_period
      age_eligible <- .x$age >= .x$elig_age

      for (i in seq_len(n)) {
        if (!is.na(qc_eligible[i]) && qc_eligible[i] && !is.na(age_eligible[i]) && age_eligible[i]) {
          years_to_use <- min(i, comp_period[i])
          top_earnings_sum <- sum(sort(indexed_earnings[1:i], decreasing = TRUE)[1:years_to_use])
          aime_vals[i] <- floor(top_earnings_sum / (comp_period[i] * 12))
        }
      }

      .x$aime <- aime_vals
      .x
    }) %>%
    ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime, qc_i, qc_tot, qc_rec, comp_period, elapsed_years, dropout_years, awi_age60, index_factor, capped_earn, indexed_earn),
                                   by = c("id", "age"))
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, aime),
                                   by = c("id", "age"))
  }

  return(worker)

}

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
      bp1_age62 = bp1[which(age == 62)],
      bp2_age62 = bp2[which(age == 62)],
      fact1_age62 = fact1[which(age == 62)],
      fact2_age62 = fact2[which(age == 62)],
      fact3_age62 = fact3[which(age == 62)],
      basic_pia = case_when(
      age >= elig_age ~ floor(case_when(
                        aime > bp2_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (bp2_age62 - bp1_age62)) + (fact3_age62 * (aime - bp2_age62)),
                        aime > bp1_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (aime - bp1_age62)),
                        TRUE ~ fact1_age62 * aime
                      )),
      TRUE ~ 0)
    ) %>% select(-bp1, -bp2) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia, bp1_age62, bp2_age62, fact1_age62, fact2_age62, fact3_age62),
                                   by = c("id","age"))
  }

  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, basic_pia),
                                   by = c("id","age"))
  }

  return(worker)

}

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
                                    by="year") %>%
      left_join(assumptions %>% select(year, s_pia_share), by="year") %>%
      group_by(id) %>%
      mutate(
        yr_s_claim = year[which(s_age == s_claim_age)],
        s_pia_share_ind = s_pia_share[which(age == elig_age)],
        spouse_pia =  case_when(
          year >= yr_s_claim & age >= 60 ~ pmax((s_pia_share_ind * s_pia) - pmax(cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE),
          TRUE ~ 0)
      ) %>%
      ungroup()

    if (debugg) {
      worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, s_pia_share_ind, spouse_pia),
                                     by=c("id","year"))
    } else {
      worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                     by=c("id","year"))
    }

  } else if (use_spouse_spec) {
    # New behavior: generate spouse data on-the-fly from spouse_spec
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
      left_join(assumptions %>% select(year, s_pia_share), by="year") %>%
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
                      by = "year")

          # Calculate spousal PIA
          s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])]
          yr_s_claim <- .x$year[which(.x$s_age == .x$s_claim_age)]
          .x$spouse_pia <- if_else(.x$age >= 60 & .x$year >= yr_s_claim, pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$cola_basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE), 0)
        }
        .x
      }) %>%
      ungroup()

    if (debugg) {
      worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, spouse_pia),
                                     by=c("id","year"))
    } else {
      worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                     by=c("id","year"))
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
    cpi_age62 = cpi_w[which(age == 62)],
    cpi_index_factor = pmax(cpi_w / cpi_age62, 1),
    cola_basic_pia = floor(basic_pia * cpi_index_factor)
  ) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cpi_age62, cpi_index_factor),
                                   by = c("id","age"))
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia),
                                   by = c("id","age"))
  }

  return(worker)

}


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

  # Program Parameters -- these will all need to be made flexible to permit policy changes at some point

  dist_from_nra <- (claim_age - nra) * 12

  #Calculate reduction factors
  rf_amt <- if_else(dist_from_nra >= 0, 0, #If claiming at or above NRA, no RFs
                   if_else(dist_from_nra <= -36, (-36*rf1) + (pmax(-24,(dist_from_nra + 36))*rf2), #If claiming more than three years before NRA. Can't claim before 62
                          dist_from_nra * rf1)) #If claiming less than three years before NRA

  #Calculate DRCs
  drc_amt <- if_else(dist_from_nra <= 0, 0, #If claiming at or below NRA
                    pmin(36*drc, dist_from_nra * drc)) #If claiming above NRA. DRCs are capped at 70

  act_factor <- 1 + rf_amt + drc_amt

  return(act_factor)

}

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


  dataset <- worker %>% left_join(assumptions %>% select(year, rf1, rf2, drc, nra, s_rf1, s_rf2), by="year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62,
      rf1_ind = rf1[which(year == yr_62)],
      rf2_ind = rf2[which(year == yr_62)],
      drc_ind = drc[which(year == yr_62)],
      nra_ind = nra[which(year == yr_62)],
      act_factor = rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind),
      wrk_ben = case_when(
        age >= claim_age ~ floor(cola_basic_pia * act_factor),
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, nra_ind, rf1_ind, rf2_ind, drc_ind, act_factor, wrk_ben),
                                   by = c("id","age") )
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, wrk_ben),
                                   by = c("id","age") )
  }

  return(worker)

}

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
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2), by="year") %>%
      left_join(spouse %>% select(year, age, claim_age) %>% rename(s_age = age, s_claim_age = claim_age),
                by = "year") %>%
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        yr_62 = year - age + 62,
        nra_ind = nra[which(year == yr_62)],
        s_rf1_ind = s_rf1[which(year == yr_62)],
        s_rf2_ind = s_rf2[which(year == yr_62)],
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),
        yr_s_claim = year[s_age == s_claim_age],
        spouse_ben = case_when(
          age >= claim_age & year >= yr_s_claim ~ floor(spouse_pia * s_act_factor),
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
              yr_62 = year - age + 62,
              nra_ind = nra[which(year == yr_62)],
              s_rf1_ind = s_rf1[which(year == yr_62)],
              s_rf2_ind = s_rf2[which(year == yr_62)],
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),
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
              s_age = year - s_birth_yr,
              s_claim_age_val = s_claim_age,
              yr_62 = year - age + 62,
              nra_ind = nra[which(year == yr_62)],
              s_rf1_ind = s_rf1[which(year == yr_62)],
              s_rf2_ind = s_rf2[which(year == yr_62)],
              s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),
              yr_s_claim = year[s_age == s_claim_age_val],
              spouse_ben = case_when(
                age >= claim_age & year >= yr_s_claim ~ floor(spouse_pia * s_act_factor),
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
        yr_62 = year - age + 62,
        nra_ind = nra[which(year == yr_62)],
        s_rf1_ind = s_rf1[which(year == yr_62)],
        s_rf2_ind = s_rf2[which(year == yr_62)],
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),
        spouse_ben = 0
      ) %>%
      ungroup()
  }

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, s_rf1_ind, s_rf2_ind, s_act_factor, spouse_ben),
                                   by = c("id","age") )
  } else {
    worker <- worker %>% left_join(dataset %>% select(id, age, spouse_ben),
                                   by = c("id","age") )
  }

  return(worker)

}

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


#' Calculate Spouse's Full Benefit (Internal)
#'
#' Calculates the spouse's total benefit for each year, which includes:
#' 1. Spouse's own worker benefit (from their earnings record)
#' 2. Spouse's spousal benefit (based on the primary worker's record)
#'
#' This is used for calculating the combined couple's annual benefit.
#'
#' @param worker_data Dataframe for a single worker containing year, age, basic_pia, cola_basic_pia
#' @param spouse_spec Character string with spouse specification
#' @param factors Data frame for the Trustees' scaled earnings factors
#' @param assumptions Data frame of the pre-prepared Trustees assumptions
#' @return Numeric vector of spouse's total benefit for each year
#' @keywords internal

calculate_spouse_full_benefit <- function(worker_data, spouse_spec, factors, assumptions) {
  # Parse the spouse specification
  spec <- parse_spouse_spec(spouse_spec)
  if (is.null(spec)) {
    return(rep(0, nrow(worker_data)))
  }

  # Generate spouse's earnings and run through the full benefit pipeline
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

  # Run spouse through benefit calculation pipeline
  # Note: spousal_pia is called with no spouse, which sets spouse_pia = 0
  # This is needed because cola() expects spouse_pia to exist -- AC NOTE: This may be able to change since cola() does not expect spouse_pia() to exist anymore
  spouse <- spouse %>%
    aime(assumptions, debugg = FALSE) %>%
    pia(assumptions, debugg = FALSE) %>%
    cola(assumptions, debugg = FALSE) %>%
    worker_benefit(assumptions, debugg = FALSE) %>%
    spousal_pia(spouse = NULL, assumptions, factors = factors, debugg = FALSE)


  # Get spouse's claim age and birth year
  s_claim_age <- spec$age_claim
  s_birth_yr <- spec$birth_yr

  # Get spouse's NRA for their birth year
  s_yr_62 <- s_birth_yr + 62
  s_nra_ind <- assumptions$nra[assumptions$year == s_yr_62][1]
  s_rf1_ind <- assumptions$s_rf1[assumptions$year == s_yr_62][1]
  s_rf2_ind <- assumptions$s_rf2[assumptions$year == s_yr_62][1]

  # Calculate spouse's actuarial factor for their spousal benefit
  # (based on spouse's claim age relative to spouse's NRA)
  s_spousal_act_factor <- rf_and_drc(s_claim_age, s_nra_ind, s_rf1_ind, s_rf2_ind, 0)

  # Calculate spouse's spousal PIA based on worker's record
  # Spousal PIA = 50% of worker's PIA - spouse's own PIA (floored at 0)
  s_pia_share_ind <- assumptions$s_pia_share[assumptions$year == s_yr_62][1]

  # Join worker's basic_pia and cpi_w (from assumptions) to spouse data by year
  spouse <- spouse %>%
    left_join(
      worker_data %>% select(year, cola_basic_pia) %>% rename(w_cola_basic_pia = cola_basic_pia),
      by = "year"
    ) %>%
    left_join(
      assumptions %>% select(year, cpi_w),
      by = "year"
    ) %>%
    mutate(
      # Spouse's spousal PIA from worker's record
      s_spousal_pia = pmax((s_pia_share_ind * w_cola_basic_pia) - pmax(cola_basic_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),

      # Spouse's spousal benefit (only after spouse claims)
      s_spousal_ben = case_when(
        age >= s_claim_age ~ floor(s_spousal_pia * s_spousal_act_factor),
        TRUE ~ 0
      ),

      # Spouse's total benefit = own worker benefit + spousal benefit
      spouse_total_ben = pmax(wrk_ben, 0, na.rm = TRUE) + pmax(s_spousal_ben, 0, na.rm = TRUE)
    )

  # Map spouse's benefits back to worker's years
  result <- worker_data %>%
    left_join(
      spouse %>% select(year, spouse_total_ben),
      by = "year"
    )

  # Replace NA with 0 (for years where spouse data doesn't exist)
  result$spouse_total_ben[is.na(result$spouse_total_ben)] <- 0

  return(result$spouse_total_ben)
}


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


