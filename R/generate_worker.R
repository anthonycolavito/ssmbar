
generate_worker <- function(sef, par,
                            birth_yr, career_length=44, claim_age,
                            type=NULL, custom_avg_earnings = NULL,
                            spouse = NULL, s_type=NULL, s_custom_avg_earnings = NULL, #Will always assume that spouses, if they exist are born and claim in the same year
                            debugg = FALSE) {
  
  checkmate::assert_data_frame(sef, null.ok = FALSE)
  checkmate::assert_data_frame(par, null.ok = FALSE)
  checkmate::assert_int(birth_yr, lower = 1941, upper = 2036)
  checkmate::assert_choice(type, c("very_low","low", "medium", "high", "custom"))
  if (type == "custom") checkmate::assert_numeric(custom_avg_earnings, lower = 0)
  if (!is.null(spouse)) {
    checkmate::assert_choice(s_type, c("very_low","low", "medium", "high", "custom"))
    if (s_type == "custom") checkmate::assert_numeric(custom_avg_earnings, lower = 0)
  }
  checkmate::assert_int(claim_age, upper = 70, lower = 22)
  
  #Retired or disabled beneficiary type, used for ID'ing 
  ben_type <- if(is.null(dis_age)) "R" else "D"
  
  worker_type <- if_else(type == "custom", paste0("custom", custom_avg_earnings), type) #Used for constructing a worker's 
  

  career_length <- min(career_length, claim_age - 21) #Ensures career length does not exceed # of years until worker becomes disabled
  if(career_length > claim_age - 21) warning("Career length has been truncated to exclude earnings past disability age")
  
  # ID format: type-birthyr-career_length (e.g., "medium-1960-40")
  id <- paste0(ben_type, "-",worker_type,"-", birth_yr, "-", career_length,"-",claim_age)
  
  first_yr <- birth_yr + 21
  last_yr <- birth_yr + 119
  years <- seq(first_yr, last_yr, 1)
  ages <- seq(21,119,1)
  
  
  
  
}