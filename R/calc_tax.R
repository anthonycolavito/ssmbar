calc_tax <- function(par, worker, debugg = FALSE) {
  
  checkmate::assert_data_frame(par, null.ok = FALSE)
  checkmate::assert_data_frame(worker, null.ok = FALSE)
  
  worker <- join_all_assumptions(worker, par)
  
  worker <- worker %>%
    group_by(id) %>% arrange(id, age) %>%
    mutate(
      total_tr = (coalesce(oasi_tr, 0) + coalesce(di_tr, 0)) * 2 / 100,
      capped_earn = pmin(earnings, taxmax),
      tax = total_tr * capped_earn
    ) %>% ungroup()
  
  if(!debugg) {
    worker <- worker %>% select(-total_tr, -capped_earn)
    
    worker <- worker %>% remove_all_assumptions()
  } 
  
  return(worker)
}