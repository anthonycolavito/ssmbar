generate_single_worker <- function(sef, par,
                                   birth_yr, type, career_length,
                                   custom_avg_earnings = NULL,
                                   debugg = FALSE) {
  
  valid_types <- c("very_low", "low", "medium", "high", "max", "custom")
  
  if (!is.null(type)) {
    if (!type %in% valid_types){
      stop("Type is not valid")
    }
    if (type == "custom" && is.null(custom_avg_earnings)) {
      stop("custom_avg_earnings is required when spouse_type = 'custom'")
    }
  }
  else {
    stop("Type is required")
  }
  
  worker_type <- if_else(type == "custom", paste0("custom", custom_avg_earnings), type) #Used for constructing a worker's ID
  
  # ID format: type-birthyr-career_length (e.g., "medium-1960-40")
  id <- paste0(worker_type,"-", birth_yr, "-", career_length)
  
  
  earn <- left_join(
    sef %>% mutate(year = age + birth_yr), #Selects scaled scaled factors dataframe and creates year column for merging
    par %>% select(year, awi, taxmax), #Selects only the needed variables from the assumptios dataframe
    by = "year" #by variable used for merging
  ) %>%
    mutate(
      earnings = factor * awi, #Scaled earnings equals factor * awi
      work_years = career_length, #Number of work years this worker has, used for AIME calc later
      type = type, #Earnings level indicator, used for ID'ing
      career_length = career_length #Length of workers career 
    )

  if (type == "max"){ #If a max earner, the values of the taxmax are taken as the earnings amounts at each age. 
    earn <- earn %>% filter(worker == "raw") %>% select(year, age, type, birth_yr, career_length, earnings = taxmax)
  }
  else if (type != "custom") { #Otherwise, the earnings column calculated above is kept. 
    earn <- earn %>% filter(worker == type) %>% select(year, age, type, birth_yr, career_length, earnings)
  }
  else {
    #Earnings are generated using the raw scaled earnings factors provided by the trustee in five (5) steps
    #1. The raw scaled earnings factors are multiplied by the AWI at each age to produce earnings by age in nominal dollars
    #2. These earnings are inflated/deflated to be in terms of today's dollars.
    #3. The average of the highest 35 years of real earnings is taken (following the Trustees' method for determining other scaled worker factors)
    #4. The ratio of the user-specified average earnings to the average resulting from the raw factors is found.
    #5. The real earnings at each age are multiplied by this ratio and then converted back into nominal dollars.
    pi_curr <- par$gdp_pi[par$year == as.numeric(format(Sys.Date(), "%Y"))]
    
    earn <- earn %>% filter(worker == "raw") %>% select(year, age, type, birth_yr, career_length, earnings) %>%
      left_join(par %>% select(year, gdp_pi), by="year") %>%
      mutate(
        pi_curr = pi_curr, #Price index for the current year (looked up from assumptions)
        index = pi_curr / gdp_pi, #Indexing factors to convert nominal earnings into real earnings
        nom_earn = factor * awi, #Nominal earnings used the raw scaled earnings factor and the yearly AWI
        real_earn = nom_earn * index) #Real earnings
    
    real_earn <- earn$real_earn #Vector of the worker's real earnings
    avg_real_earn <- sum(sort(real_earn, decreasing = TRUE)[1:35]) / 35 #Average of the highest 35 real earnings
    scalar <- custom_avg_earnings / avg_real_earn #Ratio of the specified average to the average found
    
    worker <- worker %>% mutate(
      adj_real_earn = real_earn * scalar, #Adjusted real earnings using the scalar previously calculated
      earnings = pmax(adj_real_earn * gdp_pi / pi_curr * if_else(age < elig_age, 1, if_else(elig_age < elig_age_ret, 0, 1)), 0, na.rm = TRUE) #Final nominal earnings -- earnings past eligibility age set to 0 if disabled worker
    )
  }
  
  if (!debugg) {
    worker <- worker %>% select(id, sex, year, age, earnings) #Selects only the needed variables.
  }
  
  return(worker)
  
}