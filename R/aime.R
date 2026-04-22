
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
  #
  # Program rules (from assumptions):
  # - max_dropout_years: Maximum years that can be dropped (currently 5)
  # - min_comp_period: Minimum computation period (currently 2)
  
  dataset <- worker %>% filter(age == elig_age) %>%
    group_by(id) %>%
    mutate(
      elapsed_years = pmax(elig_age - 1 - 21, 0),
      # Dropout years differ by benefit type per 42 USC 415(b)(2)(A)
      # Retirement (elig_age >= 62): fixed 5 years per (b)(2)(A)(i)
      # Disability (elig_age < 62): floor(elapsed/5), max 5 per (b)(2)(A)(ii)
      dropout_years = if_else(
        elig_age >= 62,
        pmin(max_dropout_years, 5),
        pmin(max_dropout_years, floor(elapsed_years / 5))
      ),
      comp_period = pmax(min_comp_period, elapsed_years - dropout_years)
    ) %>% ungroup()
  
  if(debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, comp_period, elapsed_years, dropout_years),
                                   by="id")
  }
  else {
    worker <- worker %>% left_join(dataset %>% select(id, comp_period),
                                   by="id")
  }
  
  return(worker)
  
}