
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
      
      spouse_pia = case_when(
        year >= s_claim_yr & age >= eea_ind & s_pia > 0 ~ pmax((s_pia_share_ind * s_pia) - pmax(rw_pia, 0, na.rm = TRUE), 0, na.rm = TRUE),
        TRUE ~ 0
      )
    ) %>% ungroup()
    
  
  if (!debugg) worker <- worker %>% select(-eea_ind, -s_claim_yr, -s_pia_share_ind)
  
  return(worker)
}