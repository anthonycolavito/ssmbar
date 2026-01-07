retired_worker <- function(birth_yr=1960, type="medium", age_claim, age_elig=62, factors, assumptions,
                           custom_avg_earnings=NULL,
                           spouse = "none", spouse_type = "medium",
                           debugg = FALSE) {

  worker <- earnings_generator(birth_yr = birth_yr, type = type, age_claim = age_claim,
                               factors = factors, assumptions = assumptions, debugg = debugg)


  worker <- dataset %>% group_by(id) %>% #Creates new age rows if they are missing in the data
    mutate(birth_yr = first(year - age)) %>%
    complete(age = 15:119, fill = list(earnings = 0)) %>%
    mutate(
      birth_yr = first(na.omit(birth_yr)),
      claim_age = first(na.omit(claim_age)),
      elig_age = first(na.omit(elig_age)),
      year = birth_yr + age) %>%
    select(-birth_yr) %>%
    ungroup()

  worker <- worker %>% aime(assumptions, debugg = debugg) %>% pia(assumptions, debugg = debugg) %>%
    cola(assumptions, debugg = debugg)

  if (spouse != "none") {
    spouse <- earnings_generator(birth_yr = birth_yr, type = spouse, age_claim = age_claim,
                                 factors = factors, assumptions = assumptions, debugg = debugg)
  }

  return(worker)

}
