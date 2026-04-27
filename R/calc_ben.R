
calc_ben <- function(par, worker, debugg=FALSE, output="skinny") {
  
  checkmate::assert_data_frame(par, null.ok = FALSE)
  checkmate::assert_data_frame(worker, null.ok = FALSE)
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
  
  if(output == "skinny") worker <- worker %>% select(id, year, age, earnings, wrk_ben)
  else if (output == "detailed") worker <- worker %>% remove_all_assumptions()
  
  return(worker)
  
}