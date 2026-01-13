
#' Earnings Generator
#'
#' Function that generates the lifetime earnings for a specified worker
#'
#' @param birth_yr Numeric value representing the birth year of the worker.
#' @param sex Character value specifying the sex of the worker: "male", "female", or "all" (gender-neutral).
#' @param type Character value specifying the earnings level of the worker.
#' @param age_claim Numeric value for the age in which the worker claims benefits.
#' @param age_elig Numeric value for the age in which the worker becomes eligible for benefits.
#' @param factors Data frame for the Trustees' scaled earnings factors.
#' @param assumptions Data frame of the pre-prepared Trustees assumptions.
#' @param custom_avg_earnings Numeric value for the real average earnings for the worker, if type="custom" is selected.
#' @param spouse_type Character value specifying the spouse's earnings level. NULL if no spouse. Default is NULL.
#' @param spouse_sex Character value specifying the spouse's sex: "male", "female", or "all". Default is "all".
#' @param spouse_birth_yr Numeric value for the spouse's birth year. Default is NULL.
#' @param spouse_age_claim Numeric value for the age at which the spouse claims benefits. Default is NULL.
#' @param spouse_custom_avg_earnings Numeric value for spouse's real average earnings if spouse_type = "custom". Default is NULL.
#' @param debugg Boolean variable used to output additional variables for debugging.
#'
#' @return worker Data frame with the earnings of the worker, including spouse_spec if spouse is specified.
#' @examples
#' \dontrun{
#' # Single worker
#' med_worker <- earnings_generator(birth_yr=1960, sex="male", type="medium", age_claim = 67, factors=sef, assumptions=tr2025)
#'
#' # Worker with spouse
#' worker_with_spouse <- earnings_generator(birth_yr=1960, sex="male", type="high", age_claim = 67,
#'   spouse_type="low", spouse_sex="female", spouse_birth_yr=1962, spouse_age_claim=65,
#'   factors=sef, assumptions=tr2025)
#' }
#'
#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange case_when if_else first row_number group_modify
#' @export

earnings_generator <- function(birth_yr=1960, sex="all", type="medium", age_claim, age_elig=62, factors, assumptions,
                               custom_avg_earnings=NULL,
                               spouse_type=NULL, spouse_sex="all", spouse_birth_yr=NULL, spouse_age_claim=NULL,
                               spouse_custom_avg_earnings=NULL,
                               debugg = FALSE) {

  # Validate sex parameter
  valid_sex <- c("male", "female", "all")
  if (!sex %in% valid_sex) {
    stop(paste("sex must be one of:", paste(valid_sex, collapse = ", ")))
  }

  # Validate and construct spouse_spec
  # spouse_spec encodes spouse info in format: "type-sex-birthyr-claimage" (e.g., "low-female-1962-65")
  # This single variable travels with worker data and is used for on-the-fly spousal PIA calculation
  valid_types <- c("very_low", "low", "medium", "high", "max", "custom")

  if (!is.null(spouse_type)) {
    # Validate spouse parameters
    if (!spouse_type %in% valid_types) {
      stop(paste("spouse_type must be one of:", paste(valid_types, collapse = ", ")))
    }
    if (!spouse_sex %in% valid_sex) {
      stop(paste("spouse_sex must be one of:", paste(valid_sex, collapse = ", ")))
    }
    if (is.null(spouse_birth_yr)) {
      stop("spouse_birth_yr is required when spouse_type is specified")
    }
    if (is.null(spouse_age_claim)) {
      stop("spouse_age_claim is required when spouse_type is specified")
    }
    if (spouse_type == "custom" && is.null(spouse_custom_avg_earnings)) {
      stop("spouse_custom_avg_earnings is required when spouse_type = 'custom'")
    }

    # Construct spouse_spec
    spouse_type_label <- if_else(spouse_type == "custom",
                                  paste0("custom", spouse_custom_avg_earnings),
                                  spouse_type)
    spouse_spec <- paste0(spouse_type_label, "-", spouse_sex, "-", spouse_birth_yr, "-", spouse_age_claim)
  } else {
    spouse_spec <- NA_character_
  }

  first_yr <- birth_yr + 21 #First earnings year
  last_yr <- birth_yr + 119 #Last possible year alive (used for benefit amounts)

  years <- seq(first_yr, last_yr, 1)
  ages <- seq(21,119,1)

  worker_type <- if_else(type == "custom", paste0("custom",custom_avg_earnings), type) #Used for constructing a worker's ID

  # ID format: type-sex-birthyr-claimage (e.g., "medium-male-1960-67")
  id <- paste0(worker_type, "-", sex, "-", birth_yr, "-", age_claim)

  claim_age <- age_claim #Age a worker claims benefits.
  elig_age <- age_elig #Age a worker is eligible for benefits.
  worker_sex <- sex #Sex of the worker for lifetime benefit calculations.

  worker <- data.frame(year = years, age = ages, id = id, sex = worker_sex, claim_age = claim_age, elig_age = elig_age,
                       spouse_spec = spouse_spec, stringsAsFactors = FALSE) %>%
    left_join(assumptions %>% select(year, awi, gdp_pi), by = "year")
  #Initial dataframe that merges in necessary assumptions with the worker's trait variables.
  #spouse_spec encodes spouse info for later spousal benefit calculation (NA if no spouse).


  if (type != "custom") {
  #Base condition -- when a worker is one of the Trustees' scaled workers.

    worker <- worker %>% left_join(factors %>% filter(.data$worker == type) %>% select(age, factor),
                                   by = "age") %>% #Left joins scaled earnings factors for the type of worker selected.
      mutate(
        earnings =  pmax(awi * factor * if_else(age < elig_age, 1, if_else(elig_age < 62, 0, 1)),0, na.rm = TRUE), #Creates earnings at each age
      )

  } # End of type conditional
  else {
  #Condition for when a worker is specified as custom.
  #Earnings are generated using the raw scaled earnings factors provided by the trustee in five (5) steps
  #1. The raw scaled earnings factors are multiplied by the AWI at each age to produce earnings by age in nominal dollars
  #2. These earnings are inflated/deflated to be in terms of today's dollars.
  #3. The average of the highest 35 years of real earnings is taken (following the Trustees' method for determining other scaled worker factors)
  #4. The ratio of the user-specified average earnings to the average resulting from the raw factors is found.
  #5. The real earnings at each age are multiplied by this ratio and then converted back into nominal dollars.

    worker <- worker %>% left_join(factors %>% filter(.data$worker == "raw") %>% select(age, factor),
                                   by = "age") %>%
      mutate(
      pi_curr = gdp_pi[which(year==as.numeric(format(Sys.Date(), "%Y")))], #Price index for the current year
      index = pi_curr / gdp_pi, #Indexing factors to convert nominal earnings into real earnings
      nom_earn = factor * awi, #Nominal earnings used the raw scaled earnings factor and the yearly AWI
      real_earn = nom_earn * index) #Real earnings

    real_earn <- worker$real_earn #Vector of the worke's real earnings
    avg_real_earn <- sum(sort(real_earn, decreasing = TRUE)[1:35]) / 35 #Average of the highest 35 real earnings
    scalar <- custom_avg_earnings / avg_real_earn #Ratio of the specified average to the average found

    worker <- worker %>% mutate(
      adj_real_earn = real_earn * scalar, #Adjusted real earnings using the scalar previously calculated
      earnings = pmax(adj_real_earn * gdp_pi / pi_curr * if_else(age < elig_age, 1, if_else(elig_age < 62, 0, 1)),0, na.rm = TRUE) #Final nominal earnings
    )
  }

  if (!debugg) {
    worker <- worker %>% select(id, sex, year, age, claim_age, elig_age, spouse_spec, earnings) #Selects only the needed variables.
  }

  return(worker)

}
