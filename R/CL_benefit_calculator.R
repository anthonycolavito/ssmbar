#' Calculate Social Security Benefits
#'
#' Convenience function that creates one or more stylized workers (and optionally their spouses)
#' and calculates their Social Security benefits through their lifetime in a single call.
#' Supports vectorized inputs to calculate benefits for multiple workers at once.
#'
#' @param birth_yr Numeric value(s) representing the birth year of the worker(s).
#' @param sex Character value(s) specifying the sex of the worker(s): "male", "female", or "all" (gender-neutral).
#' @param type Character value(s) specifying the earnings level of the worker(s).
#'   Options: "very_low", "low", "medium", "high", "max", or "custom".
#' @param age_claim Numeric value(s) for the age(s) at which the worker(s) claim benefits.
#' @param factors Data frame containing the Trustees' scaled earnings factors.
#' @param assumptions Data frame containing the prepared Trustees assumptions.
#' @param age_elig Numeric value(s) for the age(s) at which the worker(s) become eligible for benefits. Default is 62.
#' @param custom_avg_earnings Numeric value(s) for real average earnings if type = "custom". Default is NULL.
#' @param spouse_type Character value(s) specifying the spouse's earnings level. NULL if no spouse. Default is NULL.
#' @param spouse_sex Character value(s) specifying the spouse's sex: "male", "female", or "all". Default is "all".
#' @param spouse_birth_yr Numeric value(s) for the spouse's birth year. Default is NULL.
#' @param spouse_age_claim Numeric value(s) for the age(s) at which the spouse claims benefits. Default is NULL.
#' @param spouse_custom_avg_earnings Numeric value(s) for spouse's real average earnings if spouse_type = "custom". Default is NULL.
#' @param debugg Boolean value that outputs additional variables if set to TRUE. Default is FALSE.
#'
#' @return Returns a data frame with the worker's earnings and benefits by age.
#'   When multiple workers are specified, their data is combined into a single data frame.
#'   Output includes:
#'   \itemize{
#'     \item \code{ben}: Monthly benefit amount for the individual worker
#'     \item \code{annual_ind}: Annual benefit amount for the individual worker (ben * 12)
#'     \item \code{annual_couple}: Combined annual benefit for the couple (worker + spouse).
#'       NA if no spouse is specified. Includes both the spouse's own worker benefit
#'       (from their earnings record) and the spouse's spousal benefit (from the worker's record).
#'   }
#'
#' @examples
#' \dontrun{
#' # Single worker example
#' med_worker <- calculate_benefits(
#'   birth_yr = 1960,
#'   sex = "male",
#'   type = "medium",
#'   age_claim = 67,
#'   factors = sef2025,
#'   assumptions = tr2025
#' )
#'
#' # Worker with spouse example
#' worker_with_spouse <- calculate_benefits(
#'   birth_yr = 1960,
#'   sex = "male",
#'   type = "high",
#'   age_claim = 67,
#'   factors = sef2025,
#'   assumptions = tr2025,
#'   spouse_type = "low",
#'   spouse_sex = "female",
#'   spouse_birth_yr = 1962,
#'   spouse_age_claim = 65
#' )
#'
#' # Multiple workers (vectorized)
#' multiple_workers <- calculate_benefits(
#'   birth_yr = c(1960, 1970, 1980),
#'   sex = c("male", "female", "all"),
#'   type = c("low", "medium", "high"),
#'   age_claim = c(62, 67, 70),
#'   factors = sef2025,
#'   assumptions = tr2025
#' )
#'
#' # Custom earnings example
#' custom_worker <- calculate_benefits(
#'   birth_yr = 1970,
#'   sex = "male",
#'   type = "custom",
#'   age_claim = 65,
#'   factors = sef2025,
#'   assumptions = tr2025,
#'   custom_avg_earnings = 50000
#' )
#' }
#'
#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange case_when if_else
#' @export

calculate_benefits <- function(birth_yr,
                               sex = "all",
                               type,
                               age_claim,
                               factors,
                               assumptions,
                               age_elig = 62,
                               custom_avg_earnings = NULL,
                               spouse_type = NULL,
                               spouse_sex = "all",
                               spouse_birth_yr = NULL,
                               spouse_age_claim = NULL,
                               spouse_custom_avg_earnings = NULL,
                               debugg = FALSE) {

  # Generate worker(s) with earnings using earnings_generator
  # This handles vectorized inputs and creates spouse_spec automatically
  worker <- earnings_generator(
    birth_yr = birth_yr,
    sex = sex,
    type = type,
    age_claim = age_claim,
    age_elig = age_elig,
    factors = factors,
    assumptions = assumptions,
    custom_avg_earnings = custom_avg_earnings,
    spouse_type = spouse_type,
    spouse_sex = spouse_sex,
    spouse_birth_yr = spouse_birth_yr,
    spouse_age_claim = spouse_age_claim,
    spouse_custom_avg_earnings = spouse_custom_avg_earnings,
    debugg = debugg
  )

  # Calculate benefits through the pipeline
  # The benefit functions handle multiple workers via group_by(id)
  # and use spouse_spec for on-the-fly spousal benefit calculations
  worker <- worker %>%
    aime(assumptions, debugg) %>%
    pia(assumptions, debugg) %>%
    cola(assumptions, debugg) %>%
    worker_benefit(assumptions, debugg) %>%
    spousal_pia(spouse = NULL, assumptions, factors = factors, debugg = debugg) %>%
    spouse_benefit(spouse = NULL, assumptions, debugg) %>%
    ret(assumptions, factors = factors, debugg = debugg) %>%
    final_benefit(debugg)

  # Calculate annual individual benefit
  worker <- worker %>%
    mutate(annual_ind = ben * 12)

  return(worker)

}
