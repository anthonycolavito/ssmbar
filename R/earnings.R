
generate_single_worker <- function(sef, par,
                                   birth_yr, type=NULL, career_length=44,
                                   custom_avg_earnings = NULL,
                                   debugg = FALSE) {
  
  valid_types <- c("very_low", "low", "medium", "high", "max", "custom")
  
  if (!type %in% valid_types){
    stop("Type is not valid")
  }
  if (type == "custom" && is.null(custom_avg_earnings)) {
    stop("custom_avg_earnings is required when type = 'custom'")
  }

  worker_type <- if_else(type == "custom", paste0("custom", custom_avg_earnings), type) #Used for constructing a worker's ID
  
  # ID format: type-birthyr-career_length (e.g., "medium-1960-40")
  earn_id <- paste0(worker_type,"-", birth_yr, "-", career_length)
  
  
  earn <- left_join(
    sef %>% mutate(year = age + birth_yr), #Selects scaled scaled factors dataframe and creates year column for merging
    par %>% select(year, awi, taxmax), #Selects only the needed variables from the assumptios dataframe
    by = "year" #by variable used for merging
  ) %>%
    mutate(
      prelim_earnings = factor * awi, #Scaled earnings equals factor * awi
      career_length = career_length, #Length of workers career
      earn_id = earn_id
    )

  if (type == "max"){ #If a max earner, the values of the taxmax are taken as the earnings amounts at each age. 
    earn <- earn %>% filter(worker == "raw") %>% mutate(earnings = taxmax)
  }
  else if (type != "custom") { #Otherwise, the earnings column calculated above is kept. 
    earn <- earn %>% filter(worker == type) %>% mutate(earnings = prelim_earnings)
  }
  else {
    #Earnings are generated using the raw scaled earnings factors provided by the trustee in five (5) steps
    #1. The raw scaled earnings factors are multiplied by the AWI at each age to produce earnings by age in nominal dollars
    #2. These earnings are inflated/deflated to be in terms of today's dollars.
    #3. The average of the highest 35 years of real earnings is taken (following the Trustees' method for determining other scaled worker factors)
    #4. The ratio of the user-specified average earnings to the average resulting from the raw factors is found.
    #5. The real earnings at each age are multiplied by this ratio and then converted back into nominal dollars.
    pi_curr <- par$gdp_pi[par$year == as.numeric(format(Sys.Date(), "%Y"))]
    
    earn <- earn %>% filter(worker == "raw") %>%
      left_join(par %>% select(year, gdp_pi), by="year") %>%
      mutate(
        pi_curr = pi_curr, #Price index for the current year (looked up from assumptions)
        index = pi_curr / gdp_pi, #Indexing factors to convert nominal earnings into real earnings
        real_earn = prelim_earnings * index) #Real earnings
    
    real_earn <- earn$real_earn #Vector of the worker's real earnings
    avg_real_earn <- sum(sort(real_earn, decreasing = TRUE)[1:35]) / 35 #Average of the highest 35 real earnings
    scalar <- custom_avg_earnings / avg_real_earn #Ratio of the specified average to the average found
    
    earn <- earn %>% mutate(
      adj_real_earn = real_earn * scalar, #Adjusted real earnings using the scalar previously calculated
      earnings = adj_real_earn / index  #Final nominal earnings 
    )
  }
  
  #Zeroing out earnings in years after career length
  earn <- earn %>%
    mutate(
      work_year = age - 20,
      earnings = case_when(
        work_year <= career_length ~ earnings, 
        TRUE ~ 0
      )
    )
  
  if (!debugg) {
    earn <- earn %>% select(earn_id, year, age, earnings) #Selects only the needed variables.
  }
  
  return(earn)
  
}