
comp_period <- function(worker, debugg=FALSE) {
  
  # Helper function used inside AIME
  #
  # Computation period rules per 42 USC 415(b)(2)(A):
  # https://www.law.cornell.edu/uscode/text/42/415#b_2_A
  # Also see SSA Handbook Section 703: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0703.html
  #
  # Computation period = elapsed years - dropout years, minimum 2 years
  #
  # Elapsed years per 42 USC 415(b)(2)(B)(ii):
  # Calendar years after 1950 (or age 21 if later) and before year of eligibility
  # For age 62 eligibility: years from age 22 through 61 = elig_age - 1 - 21
  #
  
  # Dropout years per 42 USC 415(b)(2)(A):
  # (i) Old-age/death: 5 years (fixed)
  # (ii) Disability: "one-fifth of such individual's elapsed years (disregarding any
  #      resulting fractional part of a year), but not by more than 5 years"
  #     DI beneficiaries can earn dropout years from childcare, but this function does
  #     not incorporate this aspect of the law. 
  #
  # Program rules (from assumptions):
  # - max_dropout_years: Maximum years that can be dropped (currently 5)
  # - min_comp_period: Minimum computation period (currently 2)
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      elapsed_years = pmax(elig_age - 1 - 21, 0),
      # Dropout years differ by benefit type per 42 USC 415(b)(2)(A)
      # Retirement (elig_age >= 62): fixed 5 years per (b)(2)(A)(i)
      # Disability (elig_age < 62): floor(elapsed/5), max 5 per (b)(2)(A)(ii)
      dropout_years = if_else(
        elig_age >= eea,
        max_dropout_years,
        pmin(max_dropout_years, floor(elapsed_years / 5))
      ),
      comp_period = pmax(min_comp_period, elapsed_years - dropout_years)
    ) %>% ungroup()
  
  if(!debugg) worker <- worker %>% select(-elapsed_years, -dropout_years)
  
  return(worker)
  
}


index_earnings <- function(worker, debugg = FALSE) {
  
  # Helper function inside aime()
  
  # How earnings are indexed is described in Section 700.3 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  
  #Parameters needed for indexing earnings:
  # elig_age: Worker's age at which they become eligble for benefits. Computed in eligibility() 
  # awi: Average wage index for a given year
  # index_age_offset: The number of years prior to eligibility that earnings are indexed to
  # taxmax: The Social Security taxable maximum in a given year. 
  
  # Calculate indexed earnings
  # Earnings are indexed to AWI at (elig_age - index_age_offset) 2 years before eligibility
  # SSA Handbook Section 700.4: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0700.html
  worker <- worker %>% group_by(id) %>% arrange(id, age) %>%
    mutate(
      index_age = elig_age - first(index_age_offset), # Age for wage indexing (e.g., 62 - 2 = 60)
      awi_index_age = awi[which(age == index_age)], # Retrieve AWI at indexing age
      index_factor = pmax(awi_index_age / awi, 1), # Calculate indexing factor. Earnings past indexing age are taken at face value.
      capped_earn = pmin(earnings, taxmax), # Cap earnings amounts at the taxable maximum at each age
      indexed_earn = capped_earn * index_factor # Indexed capped earnings amounts
    ) %>%
    ungroup()
  
  if(!debugg) worker <- worker %>% select(-index_age, -awi_index_age, -index_factor, -capped_earn)
  
  return(worker)
  
}

aime <- function(worker, debugg = FALSE) {
  # AIME Computation is described in Section 701 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html
  
  # Parameters needed for AIME calculation:
  # - qc_required: QCs needed for eligibility (Section 203)
  # - max_qc_per_year: Max QCs per year (Section 212)
  # - max_dropout_years, min_comp_period: For computation period (Section 703)
  # - index_age_offset: Indexing year offset from eligibility age (Section 700.4)
  
  worker <- comp_period(worker, debugg = debugg)
  worker <- index_earnings(worker, debugg = debugg)
  
  # AIME Calculation
  # SSA Handbook Section 701: https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0701.html
  #
  # AIME equals the average monthly earnings of the highest earning years
  # in the computation period (typically 35 years for retired workers).
  # For January 2 claims, AIME at age X uses earnings through age X-1.
  #
  # Optimization: Uses partial sort which is O(n) vs full sort O(n log n).
  # Pre-computes eligibility flags to reduce redundant checks.
  worker <- worker %>%
    group_by(id) %>%
    arrange(id, age) %>%
    group_modify(~ {
      n <- nrow(.x)
      aime_vals <- numeric(n)
      indexed_earnings <- .x$indexed_earn
      qc_required_val <- .x$qc_required[1]
      comp_period <- .x$comp_period
      elig <- .x$elig
      
      # Find first eligible index to skip early years
      first_eligible <- which(elig)[1]
      
      if (!is.na(first_eligible)) {
        # Only iterate from first eligible year onwards
        for (i in first_eligible:n) {
          if (elig[i]) {
            # Earnings through age-1 (i-1 rows)
            available_years <- i - 1
            
            if (available_years > 0) {
              years_to_use <- min(available_years, comp_period[i])
              earnings_subset <- indexed_earnings[1:available_years]
              
              # Compute sum of top years_to_use earnings
              # Partial sort is O(n) for finding k largest elements
              if (available_years > years_to_use) {
                top_earnings_sum <- sum(-sort(-earnings_subset, partial = 1:years_to_use)[1:years_to_use])
              } else {
                top_earnings_sum <- sum(earnings_subset)
              }
              
              # AIME rounded down to the next lowest dollar
              aime_vals[i] <- floor(top_earnings_sum / (comp_period[i] * 12))
            }
          }
        }
      }
      
      .x$aime <- aime_vals
      .x
    }) %>%
    ungroup()
  
  if (!debugg) worker <- worker 
  
  return(worker)
  
}