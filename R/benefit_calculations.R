
#' AIME Calculation

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
      awi_age60 = awi[age == 60],
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
        if (qc_eligible[i]) {
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

#PIA Calculation
pia <- function(worker, assumptions, debugg = FALSE) {

  dataset <- worker %>% left_join(assumptions %>% select(year, bp1, bp2, fact1, fact2, fact3),
                                  by="year")

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      bp1_age62 = bp1[age == 62],
      bp2_age62 = bp2[age == 62],
      fact1_age62 = fact1[age == 62],
      fact2_age62 = fact2[age == 62],
      fact3_age62 = fact3[age == 62],
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

# Spousal Benefit
spousal_pia <- function(worker, spouse=NULL, assumptions, debugg=FALSE) {

  if(!is.null(spouse)) {
  dataset <- worker %>% left_join(spouse %>% select(year, age, basic_pia, claim_age) %>% rename(s_pia = basic_pia, s_claim_age = claim_age, s_age = age),
                                  by="year") %>%
    mutate(
      s_yr_claim = year[s_age == s_claim_age],
      spouse_pia = case_when(
        year >= s_yr_claim ~ pmax((0.5 * s_pia) - pmax(basic_pia,0,na.rm=TRUE), 0, na.rm = TRUE),
        TRUE ~ 0)
    )

        if (debugg) {
          worker <- worker %>% left_join(dataset %>% select(year, s_pia, spouse_pia),
                                         by="year")
        }
        else {
          worker <- worker %>% left_join(dataset %>% select(year, spouse_pia),
                                         by="year")
        }
  }
  else {
    dataset <- worker %>% mutate(
      spouse_pia = 0
    )

    worker <- worker %>% left_join(dataset %>% select(year, spouse_pia),
                                   by="year")
  }

  return(worker)

}

#COLA Calculation
cola <- function (worker, assumptions, debugg = FALSE) {

  dataset <- worker %>% left_join(assumptions %>% select(year, cpi_w),
                                  by = "year")

  dataset <- dataset %>% group_by(id) %>% arrange(id, age) %>% mutate(
    cpi_age62 = cpi_w[age == 62],
    cpi_index_factor = pmax(cpi_w / cpi_age62, 1),
    cola_basic_pia = floor(basic_pia * cpi_index_factor),
    cola_spouse_pia = floor(basic_pia * cpi_index_factor),
    cola_pia = floor(full_pia * cpi_index_factor)
  ) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_basic_pia, cola_spouse_pia, cola_pia, cpi_age62, cpi_index_factor),
                                   by = c("id","age"))
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, cola_pia),
                                   by = c("id","age"))
  }

  return(worker)

}


#RFs and DRCs
rf_and_drc <- function(claim_age, nra, rf1, rf2, drc) {

  # Program Parameters -- these will all need to be made flexible to permit policy changes at some point

  dist_from_nra <- (claim_age - nra) * 12

  #Calculate reduction factors
  rf_amt <- ifelse(dist_from_nra >= 0, 0, #If claiming at or above NRA, no RFs
                   ifelse(dist_from_nra <= -36, (-36*rf1) + (max(-24,(dist_from_nra + 36))*rf2), #If claiming more than three years before NRA. Can't claim before 62
                          dist_from_nra * rf1)) #If claiming less than three years before NRA

  #Calculate DRCs
  drc_amt <- ifelse(dist_from_nra <= 0, 0, #If claiming at or below NRA
                    min(36*drc, dist_from_nra * drc)) #If claiming above NRA. DRCs are capped at 70

  act_factor <- 1 + rf_amt + drc_amt

  return(act_factor)

}

# Monthly Benefit

worker_benefit <- function(worker, assumptions, debugg = FALSE) {

  dataset <- worker %>% left_join(assumptions %>% select(year, rf1, rf2, drc, nra), by="year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      yr_62 = year - age + 62,
      rf1_ind = rf1[year == yr_62],
      rf2_ind = rf2[year == yr_62],
      drc_ind = drc[year == yr_62],
      nra_ind = nra[year == yr_62],
      act_factor = rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind),
      wrk_ben = case_when(
        age >= claim_age ~ floor(cola_pia * act_factor),
        TRUE ~ 0
      )) %>% select(-claim_age) %>% ungroup()

  if (debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, nra_ind, rf1_ind, rf2_ind, drc_ind,  act_factor, wrk_ben),
                                   by = c("id","age") )
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, age, wrk_ben),
                                   by = c("id","age") )
  }

  return(worker)

}


