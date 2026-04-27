rf_and_drc <- function(claim_age, nra, rf1, rf2, drc, max_drc_age = 70) {
  # Helper function for worker_benefit() 
  
  # Benefit reduction factors are described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  # Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  #
  # rf1: Reduction for first 36 months early (5/9 of 1% per month for retired worker beneficiaries)
  # rf2: Reduction for months beyond 36 early (5/12 of 1% per month for retired worker beneficiaries)
  # drc: Delayed retirement credit per month (varies by birth year, max 8%/yr)
  # max_drc_age: Age at which DRCs stop accruing (70 under current law)
  
  dist_from_nra <- (claim_age - nra) * 12 # Distance from Normal Retirement Age in months
  drc_max_months <- (max_drc_age - nra) * 12 # Maximum months of DRC per 42 USC 402(w)
  
  # Calculate reduction factors
  rf_amt <- pmin(0, # If claiming at or above NRA, no RFs
            pmax(-36, dist_from_nra)*rf1 + # If claiming less than three years before NRA
            pmin(0, pmin(-36, dist_from_nra) + 36)*rf2 # If claiming more than three years before NRA
  )
  
  # Calculate DRCs (capped at drc_max_months past NRA, i.e., max_drc_age)
  drc_amt <- pmax(0, # If claiming at or below NRA, no DRCs
                  pmin(drc_max_months, dist_from_nra) * drc # If claiming above NRA. DRCs capped at max_drc_age
  )
  
  act_factor <- 1 + rf_amt + drc_amt # Final actuarial factor for adjusting benefits
  
  return(act_factor)
  
}

worker_benefit <- function(worker, debugg = FALSE) {
  #Benefit reduction factors are described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  #Delayed retirement credits are described in Section 720
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0720.html
  
  #Function currently can only handle retired beneficiaries.
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      rf1_ind = rf1[which(age == first(elig_age))], #First reduction factor
      rf2_ind = rf2[which(age == first(elig_age))], #Second reduction factor
      drc_ind = drc[which(age == first(elig_age))], #Third reduction factor
      nra_ind = nra[which(age == first(elig_age))], #NRA for age 62 cohort

      # Disabled workers get no actuarial adjustment - their benefit = 100% of PIA
      # Retired workers get actuarial adjustment based on claiming age relative to NRA
      act_factor = if_else(
        !is.na(dis_age),
        1.0,  # Disabled workers: no actuarial reduction or credits
        rf_and_drc(claim_age, nra_ind, rf1_ind, rf2_ind, drc_ind)  # Retired workers: apply actuarial adjustment
      ),
      wrk_ben = case_when(
        age >= claim_age ~ floor(rw_pia * act_factor), #Computes worker benefit with COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% ungroup()
  
  if (!debugg) worker <- worker %>% select(-nra_ind, -rf1_ind, -rf2_ind, -drc_ind, -act_factor) 
  
  return(worker)
  
}
