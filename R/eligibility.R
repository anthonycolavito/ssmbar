qc_comp <- function(worker, debugg = FALSE) {
  
  # Helper function used inside eligibility
  #
  # Rules for acquiring Quarters of Coverage are detailed in Section 212 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0212.html
  #
  # Program rules (from assumptions):
  # - max_qc_per_year: Maximum QCs that can be earned per year (currently 4)
  # - qc_rec: Earnings required for one QC (indexed to AWI, see assumptions_prep.R)
  # - Fully insured status requires qc_required QCs (currently 40) - see Section 203
  
 worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      qc = pmin(floor(earnings / qc_rec), max_qc_per_year), # Annual QCs earned (capped at max_qc_per_year)
      qc_tot = cumsum(qc), # Cumulative QCs earned through each age. Used for determining eligibility.
      dis_qc_tot = qc_tot[match(dis_age, age)], # Cumulate QCs through the age of disability, if it exists. Used for special insured status
      qc_past_10 = slide_dbl(qc, sum, .before = 9, .complete = FALSE) #Cumulative sum of QCs in the last 10 years (for disability insurance status)
    ) %>% ungroup()
  
  
  if(!debugg) worker <- worker %>% select(-qc) 

  return(worker)
  
}

insured_status <- function(worker, debugg = FALSE) {
  
  #Helper function used inside eligibility
  
  worker <- worker %>% 
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      full_ins_req = pmin(40, pmax(6, age - 21)), #QC requirement for fully insured status -- a credit for every year between 21 and 62, with a min of 6 and a max of 40
      
      #To be "Permanently Insured", a worker needs to have earned a minimum of 40 QCs during their career.
      #Description from OCACT: https://www.ssa.gov/OACT/ProgData/insured.html 
      perm_ins = case_when(
        qc_tot >= 40 ~ TRUE,
        TRUE ~ FALSE
      ),
      #To be "Fully Insured", a worker needs at least one QC for every elapsed year between 21 and 62 or be permanently insured. 
      #Section 203 of the Social Security Handbook https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0203.html 
      full_ins = case_when(
        qc_tot >= full_ins_req | perm_ins ~ TRUE,
        TRUE ~ FALSE
      ),
      #To be "Disability Insured", a worker needs to be Fully Insured and have earned at least 20 QCs during the last 10 years.
      #Description from OCACT: https://www.ssa.gov/OACT/ProgData/insured.html 
      dis_ins = case_when(
        full_ins & qc_past_10 >= 20 ~ TRUE,
        TRUE ~ FALSE
      ),
      #To be "Special Insured", a worker must be disabled before 31, fully insured, and have QCs in at least one half of the quarters between 21 and the quarter they become disabled
      #Section 208 of the Social Security Handbook https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0208.html 
      spec_ins = case_when(
        dis_age < 31 & full_ins & dis_qc_tot >= pmax(6,2*(dis_age - 21)) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% ungroup()
  
  if(!debugg) worker <- worker %>% select(-full_ins_req) 

  return(worker)
  
}

eligibility <- function(worker, debugg=FALSE) {
  
  #Determines whether an individual is eligible for benefits at any given age.
  
  #Uses two helper functions: qc_comp() and insured_status()
  
  #Required parameters:
  # eea - earliest eligibility age from assumptions
  
  worker <- qc_comp(worker, debugg=debugg)
  worker <- insured_status(worker, debugg=debugg)
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      #Find the worker's earliest eligibility age -- used for seeing when the worker first becomes eligible for benefits
      eea_ind = eea[match(birth_yr + 62, year)],
      
      #Determine worker's eligiblity status for benefits at each age
      #If they are fully insured and at or above their eea, they are eligible for retirement benefits
      #If they are disability insured and disabled, they are eligible for disability benefits 
      elig = case_when(
        (full_ins & age >= eea_ind) | (dis_ins & age >= dis_age) | (spec_ins & age >= dis_age) ~ TRUE,
        TRUE ~ FALSE
      ),
      
      #Ensures that once a worker first becomes eligible for benefits, that they remain benefits. 
      #Implicitly assumes that disabled workers remain disabled. 
      elig = cumany(elig),
      
      elig_age = first(age[elig])
    ) %>% ungroup()
  
  if(!debugg) worker <- worker %>% select(-eea_ind) 

  return(worker)
  
}

