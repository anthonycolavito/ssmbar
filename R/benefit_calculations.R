
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
    pia(assumptions, debugg = FALSE)

  # Return only the columns needed for spousal benefit calculations
  spouse %>% select(year, age, claim_age, basic_pia)
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

      for (i in seq_len(n)) {
        if (!is.na(qc_eligible[i]) && qc_eligible[i]) {
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
      basic_pia = floor(case_when(
        aime > bp2_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (bp2_age62 - bp1_age62)) + (fact3_age62 * (aime - bp2_age62)),
        aime > bp1_age62 ~ (fact1_age62 * bp1_age62) + (fact2_age62 * (aime - bp1_age62)),
        TRUE ~ fact1_age62 * aime
      ))
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
    dataset <- worker %>% left_join(spouse %>% select(year, basic_pia) %>% rename(s_pia = basic_pia),
                                    by="year") %>%
      left_join(assumptions %>% select(year, s_pia_share), by="year") %>%
      group_by(id) %>%
      mutate(
        s_pia_share_ind = s_pia_share[which(age == elig_age)],
        spouse_pia =  pmax((s_pia_share_ind * s_pia) - pmax(basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE)
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
            left_join(spouse_data %>% select(year, basic_pia) %>% rename(s_pia = basic_pia),
                      by = "year")

          # Calculate spousal PIA
          s_pia_share_ind <- .x$s_pia_share[which(.x$age == .x$elig_age[1])]
          .x$spouse_pia <- pmax((s_pia_share_ind * .x$s_pia) - pmax(.x$basic_pia, 0, na.rm=TRUE), 0, na.rm = TRUE)
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
    cola_basic_pia = floor(basic_pia * cpi_index_factor),
    cola_spouse_pia = floor(spouse_pia * cpi_index_factor)
  ) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cola_spouse_pia, cpi_age62, cpi_index_factor),
                                   by = c("id","age"))
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cola_spouse_pia),
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
          age >= claim_age & year >= yr_s_claim ~ floor(cola_spouse_pia * s_act_factor),
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
                age >= claim_age & year >= yr_s_claim ~ floor(cola_spouse_pia * s_act_factor),
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

#' Retirement Earnings Test Calculation
#'
#' Function that reduces an individual's benefits if their earnings exceed the
#' exempt amounts in the Retirement Earnings Test
#'
#'
#' @return worker
#'
#' @export
ret <- function(worker, spouse = NULL, assumptions, debugg = FALSE) {

  # Check if we need to use spouse_spec
  use_spouse_spec <- is.null(spouse) &&
    "spouse_spec" %in% names(worker) &&
    any(!is.na(worker$spouse_spec))

  if (!is.null(spouse)) {
    # Original behavior: use provided spouse data frame
    dataset <- worker %>% left_join(assumptions %>% select(year, ret1, nra), by="year") %>%
      left_join(spouse %>% select(year, spouse_ben) %>% rename(s_ben = spouse_ben),
                by = "year") %>%
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        ben_type = case_when( #Temporary benefit type to track which benefits need to be reduced.
          wrk_ben > 0 & spouse_ben <= 0 ~ "R",
          wrk_ben > 0 & spouse_ben > 0 ~ "D",
          wrk_ben <= 0 & spouse_ben > 0 ~ "S",
          TRUE ~ "N"
        ),
        ret_earn = case_when(
          age < claim_age | age >= nra ~ 0,
          TRUE ~ pmax(earnings - ret1, 0) / 2),
        ret_ben = case_when(
          ben_type == "R" ~ wrk_ben + spouse_ben + s_ben,
          TRUE ~ wrk_ben + spouse_ben
        ),
        ret_share = (wrk_ben + spouse_ben) / ret_ben,
        reduction = ret_ben - ret_earn,
        wrk_ben = wrk_ben - ((reduction * ret_share) * (wrk_ben / (wrk_ben + spouse_ben))),
        spouse_ben = spouse_ben - ((reduction * ret_share) * (spouse_ben / (wrk_ben + spouse_ben))),
        months_red = pmax(pmin(ret_earn / ret_ben, 12), 0),
        cum_months_red = cumsum(months_red),

        )  %>% ungroup()
  }

}


