#' Calculate Social Security Benefits
#'
#' Convenience function that creates a stylized worker (and optionally their spouse)
#' and calculates their Social Security benefits through their lifetime in a single call.
#'
#' @param birth_yr Numeric value representing the birth year of the worker.
#' @param type Character value specifying the earnings level of the worker.
#'   Options: "very_low", "low", "medium", "high", "max", or "custom".
#' @param age_claim Numeric value for the age at which the worker claims benefits.
#' @param factors Data frame containing the Trustees' scaled earnings factors.
#' @param assumptions Data frame containing the prepared Trustees assumptions.
#' @param age_elig Numeric value for the age at which the worker becomes eligible for benefits. Default is 62.
#' @param custom_avg_earnings Numeric value for real average earnings if type = "custom". Default is NULL.
#' @param spouse_birth_yr Numeric value for the spouse's birth year. Default is NULL (no spouse).
#' @param spouse_type Character value specifying the spouse's earnings level. Default is NULL.
#' @param spouse_age_claim Numeric value for the age at which the spouse claims benefits. Default is NULL.
#' @param spouse_age_elig Numeric value for the spouse's eligibility age. Default is 62.
#' @param spouse_custom_avg_earnings Numeric value for spouse's real average earnings if spouse_type = "custom". Default is NULL.
#' @param debugg Boolean value that outputs additional variables if set to TRUE. Default is FALSE.
#'
#' @return If no spouse is specified, returns a data frame with the worker's earnings and benefits by age.
#'   If a spouse is specified, returns a list containing two data frames: one for the worker and one for the spouse.
#'
#' @examples
#' \dontrun{
#' # Single worker example
#' med_worker <- calculate_benefits(
#'   birth_yr = 1960,
#'   type = "medium",
#'   age_claim = 67,
#'   factors = sef,
#'   assumptions = tr2025
#' )
#'
#' # Worker with spouse example
#' couple <- calculate_benefits(
#'   birth_yr = 1960,
#'   type = "high",
#'   age_claim = 67,
#'   factors = sef,
#'   assumptions = tr2025,
#'   spouse_birth_yr = 1962,
#'   spouse_type = "low",
#'   spouse_age_claim = 65
#' )
#' # Access worker: couple$worker
#' # Access spouse: couple$spouse
#'
#' # Custom earnings example
#' custom_worker <- calculate_benefits(
#'   birth_yr = 1970,
#'   type = "custom",
#'   age_claim = 65,
#'   factors = sef,
#'   assumptions = tr2025,
#'   custom_avg_earnings = 50000
#' )
#' }
#'
#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange case_when if_else
#' @export

calculate_benefits <- function(birth_yr,
                               type,
                               age_claim,
                               factors,
                               assumptions,
                               age_elig = 62,
                               custom_avg_earnings = NULL,
                               spouse_birth_yr = NULL,
                               spouse_type = NULL,
                               spouse_age_claim = NULL,
                               spouse_age_elig = 62,
                               spouse_custom_avg_earnings = NULL,
                               debugg = FALSE) {

  # Input validation
  valid_types <- c("very_low", "low", "medium", "high", "max", "custom")
  if (!type %in% valid_types) {
    stop(paste("type must be one of:", paste(valid_types, collapse = ", ")))
  }

  if (type == "custom" && is.null(custom_avg_earnings)) {
    stop("custom_avg_earnings is required when type = 'custom'")
  }

  # Check if spouse is specified

  has_spouse <- !is.null(spouse_birth_yr) && !is.null(spouse_type) && !is.null(spouse_age_claim)

  # Validate spouse parameters if partially specified

  if (!is.null(spouse_birth_yr) || !is.null(spouse_type) || !is.null(spouse_age_claim)) {
    if (!has_spouse) {
      stop("If specifying a spouse, all of spouse_birth_yr, spouse_type, and spouse_age_claim are required")
    }
    if (!spouse_type %in% valid_types) {
      stop(paste("spouse_type must be one of:", paste(valid_types, collapse = ", ")))
    }
    if (spouse_type == "custom" && is.null(spouse_custom_avg_earnings)) {
      stop("spouse_custom_avg_earnings is required when spouse_type = 'custom'")
    }
  }

  # Calculate benefits for single worker (no spouse)
  if (!has_spouse) {

    worker <- earnings_generator(
      birth_yr = birth_yr,
      type = type,
      age_claim = age_claim,
      age_elig = age_elig,
      factors = factors,
      assumptions = assumptions,
      custom_avg_earnings = custom_avg_earnings,
      debugg = debugg
    ) %>%
      aime(assumptions, debugg) %>%
      pia(assumptions, debugg) %>%
      spousal_pia(spouse = NULL, assumptions, debugg) %>%
      cola(assumptions, debugg) %>%
      worker_benefit(assumptions, debugg) %>%
      spouse_benefit(spouse = NULL, assumptions, debugg) %>%
      final_benefit(debugg)

    return(worker)

  }

  # Calculate benefits for worker with spouse
  else {

    # Step 1: Generate earnings for both worker and spouse
    worker <- earnings_generator(
      birth_yr = birth_yr,
      type = type,
      age_claim = age_claim,
      age_elig = age_elig,
      factors = factors,
      assumptions = assumptions,
      custom_avg_earnings = custom_avg_earnings,
      debugg = debugg
    )

    spouse <- earnings_generator(
      birth_yr = spouse_birth_yr,
      type = spouse_type,
      age_claim = spouse_age_claim,
      age_elig = spouse_age_elig,
      factors = factors,
      assumptions = assumptions,
      custom_avg_earnings = spouse_custom_avg_earnings,
      debugg = debugg
    )

    # Step 2: Calculate AIME and PIA for both
    worker <- worker %>%
      aime(assumptions, debugg) %>%
      pia(assumptions, debugg)

    spouse <- spouse %>%
      aime(assumptions, debugg) %>%
      pia(assumptions, debugg)

    # Step 3: Calculate spousal PIA (each person's spousal benefit based on the other's record)
    worker <- worker %>%
      spousal_pia(spouse = spouse, assumptions, debugg)

    spouse <- spouse %>%
      spousal_pia(spouse = worker, assumptions, debugg)

    # Step 4: Apply COLA adjustments
    worker <- worker %>%
      cola(assumptions, debugg)

    spouse <- spouse %>%
      cola(assumptions, debugg)

    # Step 5: Calculate retired worker benefits
    worker <- worker %>%
      worker_benefit(assumptions, debugg)

    spouse <- spouse %>%
      worker_benefit(assumptions, debugg)

    # Step 6: Calculate spousal benefits
    worker <- worker %>%
      spouse_benefit(spouse = spouse, assumptions, debugg)

    spouse <- spouse %>%
      spouse_benefit(spouse = worker, assumptions, debugg)

    # Step 7: Calculate final benefits
    worker <- worker %>%
      final_benefit(debugg)

    spouse <- spouse %>%
      final_benefit(debugg)

    # Return as a list
    return(list(worker = worker, spouse = spouse))

  }

}
