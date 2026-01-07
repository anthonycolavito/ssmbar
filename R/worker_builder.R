retired_worker <- function(dataset, assumptions, debugg=FALSE) {

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
    cola(assumptions, debugg = debugg) %>% worker_benefit(assumptions, debugg = debugg)

  return(worker)

}
