
#' Earnings Generator
#'
#' Function that generates the lifetime earnings for a specified worker
#'
#' @param birth_yr Numeric value representing the birth year of the worker.
#' @param type Character value specifying the earnings level of the worker.
#' @param age_claim Numeric value for the age in which the worker claims benefits.
#' @param age_elig Numeric value for the age in which the worker becomes eligible for benefits.
#' @param factors Data frame for the Trustees' scaled earnings factors.
#' @param assumptions Data frame of the pre-prepared Trustees assumptions.
#' @param zero_yrs Numeric value for the number of zero-earning years held by the worker.
#' @param custom_avg_earnings Numeric value for the real average earnings for the worker, if type="custom" is selected.
#' @param spouse Boolean value for whether the worker has a spouse -- not currently in use.
#'
#' @return worker Data frame with the earnings of the worker.
#' @examples
#' \dontrun{
#' med_worker <- earnings_generator(birth_yr=1960, type="medium", age_claim = 67, factors=sef, assumptions=tr2025)
#' }
#'
#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange case_when if_else first row_number group_modify
#' @export

earnings_generator <- function(birth_yr=1960, type="medium", age_claim, age_elig=62, factors, assumptions, zero_yrs=0,
                               custom_avg_earnings=NULL, age_stop=65,
                               spouse=FALSE) {

  first_yr <- birth_yr + 21 #First earnings year
  last_yr <- birth_yr + 119 #Last possible year alive (used for benefit amounts)

  years <- seq(first_yr, last_yr, 1)
  ages <- seq(21,119,1)

  worker_type <- if_else(type == "custom", paste0("custom",custom_avg_earnings), type) #Used for constructing a worker's ID

  id <- paste0(worker_type, "-", birth_yr, "-", age_claim, "-", zero_yrs)

  claim_age <- age_claim #Age a worker claims benefits.
  elig_age <- age_elig #Age a worker is eligible for benefits.

  worker <- data.frame(year = years, age = ages, id = id, claim_age = claim_age, elig_age = elig_age) %>% left_join(assumptions %>% select(year, awi, gdp_pi),
                                                                                                                    by = "year")
  #Initial dataframe that merges in necessary assumptions with the worker's trait variables.


  if (worker_type != "custom") {
  #Base condition -- when a worker is one of the Trustees' scaled workers.

    worker <- worker %>% left_join(sef %>% filter(worker == type) %>% select(age, factor),
                                   by = "age") %>% #Left joins scaled earnings factors for the type of worker selected.
      mutate(
        earnings = case_when(
          age < age_stop ~ awi * factor * if_else(row_number() <= zero_yrs, 0, 1), #Creates earnings at each age
          TRUE ~ 0)
      )

  } # End of type conditional
  else {
  #Condition for when a worker is specified as custom.
  #The average earnings levels for these workers are equal the specified amounts in inflation-adjusted terms.
  #This section needs to be modified to make these workers more realistic based on the Trustees' scaled earnings factors.

    worker <- worker %>% mutate(
      earnings = case_when(
        age < age_stop ~ custom_avg_earnings * gdp_pi / pi_age65 * if_else(row_number() <= zero_yrs, 0, 1),
        TRUE ~ 0)
    )
  }

  worker <- worker %>% select(id, year, age, claim_age, elig_age, earnings) #Selects only the needed variables.

  return(worker)

}
