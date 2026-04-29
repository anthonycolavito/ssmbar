apply_payable <- function(worker, par, debugg=TRUE) {
  
  worker <- worker %>% left_join(par %>% select(year, payable), by="year") %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      payable_ben = annual_ben * payable
    ) %>%
    ungroup()
  
  if(!debugg) worker <- worker %>% select(-payable)
  
  return(worker)
  
}