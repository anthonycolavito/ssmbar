basic_pia <- function(worker, debugg = FALSE) {
  # Basic PIA calculation is described in Section 706 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0706.html
  #
  # Per 42 USC 415(a)(1), PIA is:
  # - Regular PIA: 90/32/15 bend point formula per 42 USC 415(a)(1)(A)
  #
  # Bend points and replacement factors are determined at the worker's eligibility age
  
  # Required Parameters:
  # bp1 and bp2 - PIA bendpoints from assumptions
  # fact1, fact2, fact3 - PIA replacement factors from assumptions
  # eea - Earliest Eligibility Age from assumptions


  worker <- worker %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      # Use worker's elig_age for bend points (disability age for disabled workers, 62 for retired workers)
      bp1_elig = bp1[match(elig_age, age)], # First PIA bend point at worker's eligibility age
      bp2_elig = bp2[match(elig_age, age)], # Second PIA bend point at worker's eligibility age
      fact1_elig = fact1[match(elig_age, age)], # First replacement factor (90%)
      fact2_elig = fact2[match(elig_age, age)], # Second replacement factor (32%)
      fact3_elig = fact3[match(elig_age, age)], # Third replacement factor (15%)
      
      # Basic PIA per 42 USC 415(a)(1)(A): 90/32/15 bend point formula
      # Per 42 USC 415(a)(1)(A): round to next lower $0.10
      basic_pia = case_when(
        age >= elig_age ~ floor_dime(
            pmin(aime, bp1_elig)*fact1_elig + #90% Replacement of AIME below first bend point
            pmax(0, pmin(aime, bp2_elig) - bp1_elig)*fact2_elig + #32% Replacement of AIME between second and third bend point
            pmax(0, aime - bp2_elig)*fact3_elig #15% Replacement of AIME above third bend point
        )),
      
    )  %>% ungroup()
  
  if (!debugg) worker <- worker %>% select(-bp1_elig, -bp2_elig, -fact1_elig, -fact2_elig, -fact3_elig) 
  
  return(worker)
  
}

cola <- function (worker, debugg = FALSE) {
  # COLA adjustments apply year-by-year starting at eligibility age
  # SSA Handbook Section 719: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0719.html
  #
  # How COLA works:
  # - At eligibility age (62): cola_pia = basic_pia (no COLA applied yet)
  # - At age 63: cola_pia = basic_pia x (1 + COLA from eligibility year)
  # - At age 64: cola_pia = cola_pia_63 x (1 + COLA from age 63 year)
  # - etc.
  #
  # The COLA announced in year Y (based on Q3 CPI-W change) is applied to
  # benefits starting in January of year Y+1. So a worker reaching age 62
  # in 2025 first receives a COLA'd benefit in 2026 (using the 2025 COLA).
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      # Calculate the COLA factor for each year
      # At eligibility age: factor = 1 (no COLA yet)
      # After eligibility age: factor = 1 + (previous year's COLA)
      # The lag() gives us the COLA from the previous year, which applies to current year's benefit
      cola_factor = if_else(
        age == elig_age,
        1,
        1 + pmax(lag(cola, default = 0), 0)  # Negative COLAs not payable
      ),
      # Before eligibility age, factor is 1 (no effect)
      cola_factor = if_else(age >= elig_age, cola_factor, 1)
    ) %>%
    # Apply COLA with year-by-year rounding (SSA method)
    # Each year's COLA-adjusted PIA is the PREVIOUS year's rounded PIA times current COLA factor
    # This matches how SSA actually calculates benefits
    group_modify(~ {
      n <- nrow(.x)
      cola_pia_vals <- numeric(n)
      basic_pia <- .x$basic_pia
      cola_factor <- .x$cola_factor
      elig_age_val <- .x$elig_age[1]
      ages <- .x$age
      
      #Guard for ineligible workers -- COLA'd PIA stays at 0. 
      if (is.na(elig_age_val)) {
        .x$cola_pia <- 0
        .x$cola_cum_factor <- cumprod(.x$cola_factor)
        
        return(.x)
      }
      
      # Find index of eligibility age for COLA replay
      elig_idx <- which(ages == elig_age_val)[1]
      
      for (i in seq_len(n)) {
        if (ages[i] < elig_age_val) {
          cola_pia_vals[i] <- 0
        } else if (ages[i] == elig_age_val) {
          # At eligibility age: no COLA yet, use basic_pia
          cola_pia_vals[i] <- basic_pia[i]
        } else {
          # COLA the previous year's COLA'd PIA forward
          # Per 42 USC 415(i)(2)(A)(ii): round to next lower $0.10
          cola_forward <- floor_dime(cola_pia_vals[i-1] * cola_factor[i])
          
          # Automatic recomputation (SSA Handbook Section 715):
          # If AIME increased from continued earnings, basic_pia[i] may be
          # higher than the original. Replay all COLAs from eligibility year
          # on the new basic_pia and take the max.
          if (basic_pia[i] > basic_pia[elig_idx]) {
            recomp_pia <- basic_pia[i]
            for (j in (elig_idx + 1):i) {
              recomp_pia <- floor_dime(recomp_pia * cola_factor[j])
            }
            cola_pia_vals[i] <- max(cola_forward, recomp_pia)
          } else {
            cola_pia_vals[i] <- cola_forward
          }
        }
      }
      
      .x$cola_pia <- cola_pia_vals
      # Calculate cumulative factor for reference (informational only)
      .x$cola_cum_factor <- cumprod(.x$cola_factor)
      .x
    }) %>%
    ungroup()
  
  if (!debugg) worker <- worker %>% select(-cola_factor, -cola_cum_factor) 
  
  return(worker)
  
}