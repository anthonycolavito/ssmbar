
#' AIME Calculation
#'
#' Function that computes a worker's Average Indexed Monthly Earnings by age
#'
#' @param worker Dataframe with a worker's earnings by year and age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @export
aime <- function(worker, assumptions, debugg = FALSE){ #Function for calculating the AIME of a specific worker
  #How earnings are indexed is described in Section 700.3 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  #AIME Computation is described in Section 701 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html

  dataset <- worker %>% left_join(assumptions %>% select(year, awi, taxmax, qc_rec),
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
#' Function for computing a worker's PIA based on their spouse's earnings record
#'
#' @param worker Dataframe with a worker's earnings and PIA by year and age
#' @param spouse Dataframe or null value with a spouse's PIA by the worker's age and year
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @export
spousal_pia <- function(worker, spouse=NULL, assumptions, debugg=FALSE) {
  #The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html

  if(!is.null(spouse)) {
  dataset <- worker %>% left_join(spouse %>% select(year, basic_pia) %>% rename(s_pia = basic_pia),
                                  by="year") %>%
    left_join(assumptions %>% select(year, s_pia_share), by="year") %>%
    mutate(
      s_pia_share_ind = s_pia_share[which(age == elig_age)],
      spouse_pia =  pmax((s_pia_share_ind * s_pia) - pmax(basic_pia,0,na.rm=TRUE), 0, na.rm = TRUE)
    )

        if (debugg) {
          worker <- worker %>% left_join(dataset %>% select(id, year, s_pia, s_pia_share_ind, spouse_pia),
                                         by=c("id","year"))
        }
        else {
          worker <- worker %>% left_join(dataset %>% select(id, year, spouse_pia),
                                         by=c("id","year"))
        }
  }
  else {
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
#' Function that calculates a worker's spousal benefit based on their spouse's earnings record
#'
#' @param worker Dataframe with a worker's COLA-adjusted spousal PIA by age
#' @param spouse Dataframe or null value with a worker's spouse's ages and claiming age
#' @param assumptions Dataframe with the Social Security Trustees historical and projected economic variables and program parameters
#' @param debugg Boolean value that directs function to output additional variables if set to true
#'
#' @export
spouse_benefit <- function(worker, spouse = NULL, assumptions, debugg = FALSE) {
  #How benefits are reduced is described in Sections 723 and 724 of the Social Security Handbook
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html

  if(!is.null(spouse)) {

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

  }
  else {
    dataset <- worker %>% left_join(assumptions %>% select(year, nra, s_rf1, s_rf2), by="year") %>%
      group_by(id) %>% arrange(id, age) %>%
      mutate(
        yr_62 = year - age + 62,
        nra_ind = nra[which(year == yr_62)],
        s_rf1_ind = s_rf1[which(year == yr_62)],
        s_rf2_ind = s_rf2[which(year == yr_62)],
        s_act_factor = rf_and_drc(claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),
        spouse_ben = 0,
      )
  }

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, s_rf1_ind, s_rf2_ind, s_act_factor, spouse_ben),
                                   by = c("id","age") )
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, spouse_ben),
                                   by = c("id","age") )
  }

  return(worker)

}

#' Final Benefit Calculation
#'
#' Function that calculates a worker's retirement benefit based on their own earnings record
#'
#' @param worker Dataframe with a worker's retired worker and spousal benefit by age
#' @param debugg Boolean value that directs function to output additional variables if set to true
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
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, ben),
                                   by=c("id","age")) %>% select(id, year, age, earnings, ben)
  }

  return(worker)

}

