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

  # Rules for acquiring Quarters of Coverage are detailed in Section 212 of the Social Security Handbook
  # https://www.ssa.gov/OP_Home/handbook/handbook.02/handbook-0212.html
  #
  # Program rules (from assumptions):
  # - max_qc_per_year: Maximum QCs that can be earned per year (currently 4)
  # - qc_rec: Earnings required for one QC (indexed to AWI, see assumptions_prep.R)
  # - Fully insured status requires qc_required QCs (currently 40) - see Section 203

  dataset <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      qc_i = pmin(floor(earnings / qc_rec), max_qc_per_year), # Annual QCs earned (capped at max_qc_per_year)
      qc_tot = cumsum(qc_i) # Cumulative QCs earned through each age. Used for determining eligibility.
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
