#' Quarters of Coverage Computation
#'
#' Function for computation a worker's QC's at each age. QC's are used for
#' determining eligibility.
#'
#' @param worker Data frame with workers' earnings and QC requirements by age.
#' @param debugg Optional boolean variable that allows for the output of additional variables for testing.
#'
#' @return worker Data frame with a workers' earnings and QC's by age.
#'
#' @export

qc_comp <- function(worker, debugg=FALSE) {

  #Rules for acquiring Quarters of Coverage are detailed in Section 212 of the Social Security Handbook
  #https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0212.html#S0212

  dataset <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      qc_i = pmin(floor(earnings / qc_rec), 4), #Annual QCs earned.
      qc_tot = cumsum(qc_i) #Cumulative QCs earned through each age. Used for determining eligibility.
    ) %>% ungroup()

  if(debugg) {
    worker <- worker %>% left_join(dataset %>% select(id, age, qc_tot, qc_i),
                                   by = c("id","age"))
  }
  else {
  worker <- worker %>% left_join(dataset %>% select(id, age, qc_tot),
                                 by=c("id","age"))
  }

  return(worker)

}

#' Computation Period
#'
#' Function for determining a worker's computation period.
#'
#' @param worker Data frame with workers by age.
#' @param debugg Optional boolean variable that allows for the output of additional variables for testing.
#'
#' @return worker Data frame with workers by age, with computation period appended.
#'
#' @export

comp_period <- function(worker, debugg=FALSE) {

  #Rules for determining a worker's computation period are outlined in Section 703 of the Social Security Handbook
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0703.html
  #A worker's computation period is equal to their elapsed years less their dropout years and cannot fall below 2.
  #See Section 704 for the definition of elapsed years.
  #https://www.ssa.gov/OP_Home/handbook/handbook.07/handbook-0704.html

  dataset <- worker %>% filter(age == elig_age) %>%
    group_by(id) %>%
    mutate(
      elapsed_years = pmax(elig_age - 1 - 21,0),
      dropout_years = pmin(5, elapsed_years - floor(elig_age/5)),
      comp_period = pmax(2, elapsed_years - dropout_years)
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
