
# =============================================================================
# Constants for Earnings Generation
# =============================================================================
# These constants define the age range for earnings generation.
# They are not expected to change under policy reforms.

FIRST_WORKING_AGE <- 21  # First age at which earnings can be recorded
MAX_AGE <- 119           # Maximum age for benefit calculations (end of lifetime)

#' Earnings Generator
#'
#' Function that generates the lifetime earnings for one or more specified workers.
#' Supports vectorized inputs to generate multiple workers in a single call.
#'
#' @param birth_yr Numeric value(s) representing the birth year of the worker(s).
#' @param sex Character value(s) specifying the sex of the worker(s): "male", "female", or "all" (gender-neutral).
#' @param type Character value(s) specifying the earnings level of the worker(s).
#' @param age_claim Numeric value(s) for the age(s) at which the worker(s) claim benefits.
#' @param disabled_age Numeric value(s) for the age(s) at which the worker(s) becomes disabled. Default is NULL
#' @param factors Data frame for the Trustees' scaled earnings factors.
#' @param assumptions Data frame of the pre-prepared Trustees assumptions.
#' @param custom_avg_earnings Numeric value(s) for the real average earnings for worker(s), if type="custom" is selected.
#' @param spouse_type Character value(s) specifying the spouse's earnings level. NULL or NA if no spouse. Default is NULL.
#' @param spouse_sex Character value(s) specifying the spouse's sex: "male", "female", or "all". Default is "all".
#' @param spouse_birth_yr Numeric value(s) for the spouse's birth year. Default is NULL.
#' @param spouse_age_claim Numeric value(s) for the age(s) at which the spouse claims benefits. Default is NULL.
#' @param spouse_custom_avg_earnings Numeric value(s) for spouse's real average earnings if spouse_type = "custom". Default is NULL.
#' @param spouse_disabled_age Numeric value(s) for the age(s) at which the spouse becomes disabled. Default is NULL.
#'   Use this for spouses who become disabled at age 62 or later (up to NRA).
#' @param child1_spec Character value(s) specifying the first child. Format: "birthyr-disabled"
#'   (e.g., "2010-FALSE" for non-disabled child, "2005-TRUE" for disabled child). Default is NULL.
#' @param child2_spec Character value(s) specifying the second child. Same format as child1_spec. Default is NULL.
#' @param child3_spec Character value(s) specifying the third child. Same format as child1_spec. Default is NULL.
#' @param debugg Boolean variable used to output additional variables for debugging.
#'
#' @return worker Data frame with the earnings of the worker(s), including spouse_spec if spouse is specified.
#'   When multiple workers are specified, their data is combined into a single data frame with unique IDs.
#' @examples
#' \dontrun{
#' # Single worker
#' med_worker <- earnings_generator(birth_yr=1960, sex="male", type="medium", age_claim=67,
#'   factors=sef2025, assumptions=tr2025)
#'
#' # Worker with spouse
#' worker_with_spouse <- earnings_generator(birth_yr=1960, sex="male", type="high", age_claim=67,
#'   spouse_type="low", spouse_sex="female", spouse_birth_yr=1962, spouse_age_claim=65,
#'   factors=sef2025, assumptions=tr2025)
#'
#' # Multiple workers (vectorized)
#' multiple_workers <- earnings_generator(
#'   birth_yr = c(1960, 1970, 1980),
#'   sex = c("male", "female", "all"),
#'   type = c("low", "medium", "high"),
#'   age_claim = c(62, 67, 70),
#'   factors = sef2025,
#'   assumptions = tr2025
#' )
#' }
#'
#' @importFrom dplyr %>% mutate select filter left_join group_by ungroup arrange case_when if_else first row_number group_modify bind_rows
#' @export

earnings_generator <- function(birth_yr=1960, sex="all", type="medium", age_claim=65, disabled_age=NULL, factors, assumptions,
                               custom_avg_earnings=NULL,
                               spouse_type=NULL, spouse_sex="all", spouse_birth_yr=NULL, spouse_age_claim=NULL,
                               spouse_custom_avg_earnings=NULL, spouse_disabled_age=NULL,
                               child1_spec=NULL, child2_spec=NULL, child3_spec=NULL,
                               debugg = FALSE) {

  # Determine the number of workers from the longest input vector
  n_workers <- max(
    length(birth_yr),
    length(sex),
    length(type),
    length(age_claim),
    length(disabled_age),
    length(custom_avg_earnings),
    length(spouse_type),
    length(spouse_sex),
    length(spouse_birth_yr),
    length(spouse_age_claim),
    length(spouse_custom_avg_earnings),
    length(spouse_disabled_age),
    length(child1_spec),
    length(child2_spec),
    length(child3_spec)
  )

  # Helper function to recycle parameters to n_workers length
  # Handles NULL by converting to NA vector
  recycle_param <- function(x, n, allow_null = FALSE) {
    if (is.null(x)) {
      if (allow_null) return(rep(NA, n))
      else return(NULL)
    }
    if (length(x) == 1) return(rep(x, n))
    if (length(x) == n) return(x)
    stop("Parameter lengths must be 1 or match the number of workers")
  }

  # Recycle all parameters
  birth_yr_vec <- recycle_param(birth_yr, n_workers)
  sex_vec <- recycle_param(sex, n_workers)
  type_vec <- recycle_param(type, n_workers)
  age_claim_vec <- recycle_param(age_claim, n_workers)
  disabled_age_vec <- recycle_param(disabled_age, n_workers)
  custom_avg_earnings_vec <- recycle_param(custom_avg_earnings, n_workers, allow_null = TRUE)
  spouse_type_vec <- recycle_param(spouse_type, n_workers, allow_null = TRUE)
  spouse_sex_vec <- recycle_param(spouse_sex, n_workers)
  spouse_birth_yr_vec <- recycle_param(spouse_birth_yr, n_workers, allow_null = TRUE)
  spouse_age_claim_vec <- recycle_param(spouse_age_claim, n_workers, allow_null = TRUE)
  spouse_custom_avg_earnings_vec <- recycle_param(spouse_custom_avg_earnings, n_workers, allow_null = TRUE)
  spouse_disabled_age_vec <- recycle_param(spouse_disabled_age, n_workers, allow_null = TRUE)
  child1_spec_vec <- recycle_param(child1_spec, n_workers, allow_null = TRUE)
  child2_spec_vec <- recycle_param(child2_spec, n_workers, allow_null = TRUE)
  child3_spec_vec <- recycle_param(child3_spec, n_workers, allow_null = TRUE)

  # Generate each worker and combine
  workers_list <- lapply(seq_len(n_workers), function(i) {
    generate_single_worker(
      birth_yr = birth_yr_vec[i],
      sex = sex_vec[i],
      type = type_vec[i],
      age_claim = age_claim_vec[i],
      disabled_age = disabled_age_vec[i],
      factors = factors,
      assumptions = assumptions,
      custom_avg_earnings = if (is.na(custom_avg_earnings_vec[i])) NULL else custom_avg_earnings_vec[i],
      spouse_type = if (is.na(spouse_type_vec[i])) NULL else spouse_type_vec[i],
      spouse_sex = spouse_sex_vec[i],
      spouse_birth_yr = if (is.na(spouse_birth_yr_vec[i])) NULL else spouse_birth_yr_vec[i],
      spouse_age_claim = if (is.na(spouse_age_claim_vec[i])) NULL else spouse_age_claim_vec[i],
      spouse_custom_avg_earnings = if (is.na(spouse_custom_avg_earnings_vec[i])) NULL else spouse_custom_avg_earnings_vec[i],
      spouse_disabled_age = if (is.na(spouse_disabled_age_vec[i])) NULL else spouse_disabled_age_vec[i],
      child1_spec = if (is.na(child1_spec_vec[i])) NULL else child1_spec_vec[i],
      child2_spec = if (is.na(child2_spec_vec[i])) NULL else child2_spec_vec[i],
      child3_spec = if (is.na(child3_spec_vec[i])) NULL else child3_spec_vec[i],
      debugg = debugg
    )
  })

  # Combine all workers into single data frame
  workers <- bind_rows(workers_list)

  # Fix duplicate IDs by adding numeric suffix
  # This handles cases where multiple workers have the same type-sex-birthyr-claimage
  unique_ids <- unique(workers$id)
  for (base_id in unique_ids) {
    id_rows <- which(workers$id == base_id)
    # Count unique workers with this base ID (each worker has multiple rows for different ages)
    n_ages <- sum(workers$id == base_id) / length(unique(workers$age[id_rows]))
    if (n_ages > 1) {
      # There are duplicate workers - add numeric suffix
      ages <- unique(workers$age)
      rows_per_worker <- length(ages)
      worker_indices <- split(id_rows, rep(1:n_ages, each = rows_per_worker))
      for (j in seq_along(worker_indices)) {
        workers$id[worker_indices[[j]]] <- paste0(base_id, "-", j)
      }
    }
  }

  return(workers)
}


#' Generate Single Worker (Internal)
#'
#' Internal function that generates the lifetime earnings for a single worker.
#' Called by earnings_generator() for each worker specification.
#'
#' @inheritParams earnings_generator
#' @return worker Data frame with the earnings of a single worker.
#' @keywords internal

generate_single_worker <- function(birth_yr, sex, type, age_claim, disabled_age, factors, assumptions,
                                   custom_avg_earnings = NULL,
                                   spouse_type = NULL, spouse_sex = "all", spouse_birth_yr = NULL,
                                   spouse_age_claim = NULL, spouse_custom_avg_earnings = NULL,
                                   spouse_disabled_age = NULL,
                                   child1_spec = NULL, child2_spec = NULL, child3_spec = NULL,
                                   debugg = FALSE) {

  # Validate sex parameter
  valid_sex <- c("male", "female", "all")
  if (!sex %in% valid_sex) {
    stop(paste("sex must be one of:", paste(valid_sex, collapse = ", ")))
  }

  # Validate and construct spouse_spec
  # spouse_spec encodes spouse info in format: "type-sex-birthyr-claimage[-disabledage]"
  # Examples: "low-female-1962-65" or "low-female-1962-63-63" (disabled at 63)
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
    # Add disabled_age if specified (for disabled spouses claiming at 62-NRA)
    if (!is.null(spouse_disabled_age) && !is.na(spouse_disabled_age)) {
      spouse_spec <- paste0(spouse_spec, "-", spouse_disabled_age)
    }
  } else {
    spouse_spec <- NA_character_
  }

  first_yr <- birth_yr + FIRST_WORKING_AGE  # First earnings year
  last_yr <- birth_yr + MAX_AGE  # Last possible year alive (used for benefit amounts)

  years <- seq(first_yr, last_yr, 1)
  ages <- seq(FIRST_WORKING_AGE, MAX_AGE, 1)

  worker_type <- if_else(type == "custom", paste0("custom", custom_avg_earnings), type) #Used for constructing a worker's ID

  # ID format: type-sex-birthyr-claimage (e.g., "medium-male-1960-67")
  id <- paste0(worker_type, "-", sex, "-", birth_yr, "-", age_claim)

  # Get the default retirement eligibility age from assumptions
  # Use birth_yr + 62 to find the year the worker turns 62
  elig_age_ret <- assumptions$elig_age_retired[assumptions$year == birth_yr + 62]

  # For disabled workers, claim_age and elig_age are both the disability age
  # For retired workers, claim_age is when they choose to claim, elig_age is 62 (EEA)
  if (is.null(disabled_age) || is.na(disabled_age)) {
    claim_age <- age_claim
    elig_age <- elig_age_ret
  } else {
    claim_age <- disabled_age  # Disabled workers claim when they become disabled
    elig_age <- disabled_age   # Eligibility age is disability age
  }
  # TODO: Document - explain how sex affects benefit calculations (life expectancy lookup)
  worker_sex <- sex #Sex of the worker for lifetime benefit calculations.
  # Calculate expected death age based on cohort life expectancy at age 65
  # Keep fractional value — PV functions use floor(death_age) for full years
  # and the fractional remainder to weight the partial final year
  death_age <- if(sex == "male") {
    assumptions$le_m[which(assumptions$year == birth_yr + 65)]
  }
  else if (sex == "female") {
    assumptions$le_f[which(assumptions$year == birth_yr + 65)]
  }
  else {
    # Average of male and female life expectancy for sex = "all"
    mean(c(assumptions$le_m[which(assumptions$year == birth_yr + 65)],
           assumptions$le_f[which(assumptions$year == birth_yr + 65)]))
  }

  # Convert child_spec NULL to NA_character_ for data frame
  child1_spec_val <- if (is.null(child1_spec) || is.na(child1_spec)) NA_character_ else child1_spec
  child2_spec_val <- if (is.null(child2_spec) || is.na(child2_spec)) NA_character_ else child2_spec
  child3_spec_val <- if (is.null(child3_spec) || is.na(child3_spec)) NA_character_ else child3_spec

  worker <- data.frame(year = years, age = ages, id = id, sex = worker_sex, claim_age = claim_age, elig_age = elig_age,
                       death_age = death_age,
                       spouse_spec = spouse_spec,
                       child1_spec = child1_spec_val,
                       child2_spec = child2_spec_val,
                       child3_spec = child3_spec_val,
                       stringsAsFactors = FALSE) %>%
    left_join(assumptions %>% select(year, awi, gdp_pi), by = "year")
  #Initial dataframe that merges in necessary assumptions with the worker's trait variables.
  #spouse_spec encodes spouse info for later spousal benefit calculation (NA if no spouse).
  #child_spec columns encode child info for child benefit calculation (NA if no children).


  if (type %in% c("very_low","low","medium","high")) {
  #Base condition -- when a worker is one of the Trustees' scaled workers.

    worker <- worker %>% left_join(factors %>% filter(.data$worker == type) %>% select(age, factor),
                                   by = "age") %>% #Left joins scaled earnings factors for the type of worker selected.
      mutate(
        earnings =  pmax(awi * factor * if_else(age < elig_age, 1, if_else(elig_age < elig_age_ret, 0, 1)), 0, na.rm = TRUE), #Creates earnings at each age -- earnings past eligibility age set to 0 if disabled worker (elig_age < elig_age_ret)
      )

  } # End of type conditional
  else if (type == "max") {
    worker <- worker %>% left_join(assumptions %>% select(year, taxmax),
                                   by = "year") %>% #Left joins scaled earnings factors for the type of worker selected.
      mutate(
        earnings =  case_when(
          elig_age < elig_age_ret & age >= 21 & age < elig_age ~ taxmax, # Disabled max earner: earnings until disability
          elig_age >= elig_age_ret & age >= 21 & age <= 64 ~ taxmax,     # Retired max earner: earnings until age 64
          TRUE ~ 0
        ) #Creates earnings at each age
      ) %>% select(!taxmax) #Removing taxmax var to prevent errors with debugger
  }
  else {
  #Condition for when a worker is specified as custom.
  # TODO: Document - add citation to Trustees Report methodology for scaled earnings factors
  #Earnings are generated using the raw scaled earnings factors provided by the trustee in five (5) steps
  #1. The raw scaled earnings factors are multiplied by the AWI at each age to produce earnings by age in nominal dollars
  #2. These earnings are inflated/deflated to be in terms of today's dollars.
  #3. The average of the highest 35 years of real earnings is taken (following the Trustees' method for determining other scaled worker factors)
  #4. The ratio of the user-specified average earnings to the average resulting from the raw factors is found.
  #5. The real earnings at each age are multiplied by this ratio and then converted back into nominal dollars.

    pi_curr <- assumptions$gdp_pi[assumptions$year == as.numeric(format(Sys.Date(), "%Y"))]

    worker <- worker %>% left_join(factors %>% filter(.data$worker == "raw") %>% select(age, factor),
                                   by = "age") %>%
      mutate(
      pi_curr = pi_curr, #Price index for the current year (looked up from assumptions)
      index = pi_curr / gdp_pi, #Indexing factors to convert nominal earnings into real earnings
      nom_earn = factor * awi, #Nominal earnings used the raw scaled earnings factor and the yearly AWI
      real_earn = nom_earn * index) #Real earnings

    real_earn <- worker$real_earn #Vector of the worker's real earnings
    avg_real_earn <- sum(sort(real_earn, decreasing = TRUE)[1:35]) / 35 #Average of the highest 35 real earnings
    scalar <- custom_avg_earnings / avg_real_earn #Ratio of the specified average to the average found

    worker <- worker %>% mutate(
      adj_real_earn = real_earn * scalar, #Adjusted real earnings using the scalar previously calculated
      earnings = pmax(adj_real_earn * gdp_pi / pi_curr * if_else(age < elig_age, 1, if_else(elig_age < elig_age_ret, 0, 1)), 0, na.rm = TRUE) #Final nominal earnings -- earnings past eligibility age set to 0 if disabled worker
    )
  }

  if (!debugg) {
    worker <- worker %>% select(id, sex, year, age, claim_age, elig_age, death_age, spouse_spec,
                                child1_spec, child2_spec, child3_spec, earnings) #Selects only the needed variables.
  }

  return(worker)

}
