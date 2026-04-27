
calc_ben <- function(par, worker, spouse=NULL, debugg=FALSE, output="skinny") {
  
  checkmate::assert_data_frame(par, null.ok = FALSE)
  checkmate::assert_data_frame(worker, null.ok = FALSE)
  checkmate::assert_data_frame(spouse, null.ok = TRUE)
  checkmate::assert_choice(output, c("skinny","detailed", "full"))
  
  if(debugg & output != "full") warning("Output must be set to mode full to view all parameters in debugg mode")
  
  worker <- join_all_assumptions(worker, par) #Joins needed assumptions
  
  worker <- worker %>%
    eligibility(debugg = debugg) %>%
    aime(debugg = debugg) %>%
    basic_pia(debugg = debugg) %>%
    cola(debugg = debugg) %>%
    special_min_pia(debugg = debugg) %>%
    worker_benefit(debugg = debugg)
  
  if(!is.null(spouse)) {
    
    spouse <- join_all_assumptions(spouse, par) #Joins needed assumption
    
    spouse <- spouse %>% 
      eligibility(debugg = debugg) %>%
      aime(debugg = debugg) %>%
      basic_pia(debugg = debugg) %>%
      cola(debugg = debugg) %>%
      special_min_pia(debugg = debugg) %>%
      select(s_id = id, spouse_id, year, s_age = age, s_claim_age = claim_age, s_pia = rw_pia)
      
    worker <- worker %>% left_join(
      spouse,
      by=c("id"="spouse_id","year")
    ) %>%
      spousal_pia(debugg = debugg) %>%
      spousal_benefit(debugg = debugg)
  }
  
  worker <- worker %>% mutate(final_ben = coalesce(wrk_ben, 0) + coalesce(spousal_ben, 0))
  
  if(output == "skinny") worker <- worker %>% select(id, year, age, earnings, wrk_ben)
  else if (output == "detailed") worker <- worker %>% remove_all_assumptions()
  
  return(worker)
  
}