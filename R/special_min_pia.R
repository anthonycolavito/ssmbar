
years_of_coverage <- function(worker, debugg=FALSE) {
  
  # Helper function for special_min_pia()
  
  # Count years where earnings >= yoc_threshold
  # Only count years from age 21 through eligibility age - 1 (working years)
  worker <- worker %>%
    group_by(id) %>%
    arrange(id, age) %>%
    mutate(
      # Flag each year as a coverage year (1) or not (0)
      is_coverage_year = if_else(
        age >= 21 & age < elig_age & !is.na(yoc_threshold) & earnings >= yoc_threshold,
        1L,
        0L
      ),
      # Cumulative years of coverage through each age
      # Capped at 30 per 42 USC 415(a)(1)(C)(ii): special minimum PIA
      # maxes out at 30 years of coverage (30 - 10 = 20 increments)
      years_of_coverage = pmin(cumsum(is_coverage_year), 30L)
    ) %>%
    ungroup()
  
  if(!debugg) worker <- worker %>% select(-is_coverage_year) 
  
  return(worker)
  
}

special_min_pia <- function(worker, debugg=FALSE) {
  #Special Minimum PIA calculation in Section 717 of the Social Security Handbook
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0717.html 
  
  #Required parameters:
  # min_yoc_for_special_min: Minimum number of YOCs needed to be eligible for the Special Min PIA
  # yoc_threshold: earnings threshold for receiving a YOC in a given year
  # special_min_rate: Amount of Special Min PIA received for each YOC
  
  #Compute each worker's YOC amounts
  worker <- years_of_coverage(worker, debugg = debugg)
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      #Get minimum number of YOC needed for SMP at eligibility age
      min_yoc_elig = min_yoc_for_special_min[match(elig_age, age)],
      
      # Special minimum PIA per 42 USC 415(a)(1)(C)(i):
      # PIA = special_min_rate x (years_of_coverage - 10)
      # Only applies if years_of_coverage >= min_yoc_for_special_min (11)
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
      special_min_pia = case_when(
        age >= elig_age & years_of_coverage >= min_yoc_elig ~
          floor_dime(special_min_rate * (years_of_coverage - 10)),
        TRUE ~ 0),
      
      # Final Retired Worker PIA is the higher of regular or special minimum per 42 USC 415(a)(1)
      rw_pia = pmax(cola_pia, special_min_pia),
      
      #Marker for whether a worke received a special min PIA in a given year (informational only)
      received_smp = case_when(
        special_min_pia > cola_pia ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% ungroup()
  
  if(!debugg) worker <- worker %>% select(-received_smp, -min_yoc_elig, -special_min_pia) 
  
  return(worker)
  
}