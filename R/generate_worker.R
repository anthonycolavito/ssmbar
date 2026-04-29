
generate_retired_worker <- function(sef, par,
                            birth_yr, career_length=44, claim_age,
                            type=NULL, custom_avg_earnings = NULL,
                            spouse_id = NULL,
                            debugg = FALSE) {
  
  checkmate::assert_data_frame(sef, null.ok = FALSE)
  checkmate::assert_data_frame(par, null.ok = FALSE)
  checkmate::assert_int(birth_yr, lower = 1930, upper = 2036)
  checkmate::assert_choice(type, c("very_low","low", "medium", "high", "max", "custom"))
  if (type == "custom") checkmate::assert_numeric(custom_avg_earnings, lower = 0)
  checkmate::assert_int(claim_age, upper = 70, lower = 62)
  
  worker_type <- if_else(type == "custom", paste0("custom", custom_avg_earnings), type) #Used for constructing a worker's 
  
  # ID format: type-birthyr-career_length (e.g., "medium-1960-40")
  id <- paste0("R","-",worker_type,"-", birth_yr, "-", career_length,"-",claim_age)
  
  first_yr <- birth_yr + 21
  last_yr <- birth_yr + 119
  years <- seq(first_yr, last_yr, 1)
  ages <- seq(21,119,1)
  
  dis_age <- NA
  
  worker <- data.frame(id = id, age=ages, year=years, birth_yr=birth_yr, claim_age=claim_age, dis_age=dis_age)
  
  earnings <- generate_earnings(sef=sef, par=par,
                                birth_yr=birth_yr, type=type, custom_avg_earnings=custom_avg_earnings,
                                debugg=debugg)
  
  worker <- worker %>% left_join(earnings %>% select(age, earnings), by="age") %>%
    mutate(earnings = case_when(
      is.na(earnings) ~ 0,
      TRUE ~ earnings
    ))
  
  if(!is.null(spouse_id)) worker <- worker %>% mutate(spouse_id = spouse_id)
  
  return(worker)
  
}