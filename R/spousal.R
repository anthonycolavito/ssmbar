
spousal_pia <- function(worker, debugg = FALSE) {
  # The spousal insurance benefit is described in Section 320 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.03/handbook-0320.html
  #
  # Spousal benefits can begin at eea (currently 62).
  # They cannot be claimed before the spouse whose record they are based on claims retired worker benefits.
  # Spousal PIA = (s_pia_share * spouse's PIA) - own PIA (if positive)

  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      eea_ind = eea[match(birth_yr + 62, year)],
      s_claim_yr = year[match(s_claim_age, age)],
      s_pia_share_ind = s_pia_share[match(birth_yr + 62, year)],
      
      spousal_pia = case_when(
        year >= s_claim_yr & age >= eea_ind & s_pia > 0 ~ pmax((s_pia_share_ind * s_pia) - pmax(rw_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
        TRUE ~ 0
      )
    ) %>% ungroup()
    
  
  if (!debugg) worker <- worker %>% select(-eea_ind, -s_claim_yr, -s_pia_share_ind)
  
  return(worker)
}

spousal_benefit <- function(worker, debugg = FALSE) {
  # How benefits are reduced is described in Sections 723 and 724 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0723.html
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0724.html
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      s_rf1_ind = s_rf1[match(birth_yr + 62, year)], #First spousal reduction factor
      s_rf2_ind = s_rf2[match(birth_yr + 62, year)], #Second spousal reduction factor
      nra_ind = nra[match(birth_yr + 62, year)], #NRA for age 62 cohort
      age_at_spouse_claim = age[match(s_claim_age, s_age)], #Age when spouse first claims benefits
      spousal_ben_claim_age = pmax(claim_age, age_at_spouse_claim), #Age when worker claims spousal benefits -- cannot be before spouse claims benefits
      
      #Spousal benefit actuarial adjustment
      spousal_act_factor = rf_and_drc(spousal_ben_claim_age, nra_ind, s_rf1_ind, s_rf2_ind, 0),  #No DRCs are paid to spousal beneficiaries
      spousal_ben = case_when(
        age >= claim_age ~ floor(spousal_pia * spousal_act_factor), #Computes spousal benefit with COLA'd PIA and the actuarial adjustment
        TRUE ~ 0
      )) %>% ungroup()
  
  if (!debugg) worker <- worker %>% select(-nra_ind, -age_at_spouse_claim, -spousal_ben_claim_age, -s_rf1_ind, -s_rf2_ind, -spousal_act_factor) 
  
  return(worker)
  
}

generate_spousal_info <- function(par, spouse) {
  
  spouse <- join_all_assumptions(spouse, par) #Joins needed assumption
  
  spouse <- spouse %>% 
    eligibility() %>%
    aime() %>%
    basic_pia() %>%
    cola() %>%
    special_min_pia() %>%
    select(s_id = id, year, s_age = age, s_claim_age = claim_age, s_pia = rw_pia)
  
  return(spouse)
}