# create_marginal_irr.R
# -----------------------------------------------------------------------------                                                   
# Marginal Internal Rate of Return on each year of OASDI earnings, by worker                                                      
# config, under both scheduled and payable benefits scenarios.                                                                    
#                                                                                                                                 
# For each working age t in [21, 64], the marginal IRR is the real annualized                                                     
# rate r at which the year-t tax compounds (over 65 - t years) to equal the                                                       
# additional household PV of benefits accrued from including that year's                                                          
# earnings:                                                                                                                       
#                                                                                                                                 
#   T_t_real * (1 + r)^(65 - t) = delta_pv_ben(t)                                                                                 
#   =>  r = ( delta_pv_ben(t) / T_t_real )^(1 / (65 - t)) - 1                                                                     
#                                                                                                                                 
# T_t_real is the year-t nominal tax converted to real $2026.                                                                     
# delta_pv_ben(t) is already in real $2026 anchored at age 65 in                                                                  
# pb_net_tax_on_earnings.csv (under both scenarios).                                                                              
#                                                                                                                                 
# NA is returned when delta_pv_ben(t) <= 0 (the closed-form solution is real                                                      
# only for positive delta). In practice this is age 21 for everyone (the                                                          
# post-hoc fix in create_net_tax_on_earnings.R zeros it) plus any year the                                                        
# worker was still uninsured (cumulative QCs < 40), so the chart will show                                                        
# gaps at those ages.                                                                                                             
#                                                                                                                                 
# Inputs:                                                                                                                         
#   output/pb_net_tax_on_earnings.csv  (tax + delta_pv_ben_sched + delta_pv_ben_pb)                                               
#   data/tr2025.rda                     (gdp_pi for nominal->real conversion)                                                     
#                                                                                                                                 
# Output:                                                                                                                         
#   output/marginal_irr_by_age.csv  columns:                                                                                      
#     worker_type, spouse_type, birth_yr, claim_age, age, year,                                                                   
#     marginal_irr_scheduled, marginal_irr_payable                                                                                
# -----------------------------------------------------------------------------                                                   

library(tidyverse)                                                                                                                

load("./data/tr2025.rda")                                                                                                         

# ---- Configuration ----------------------------------------------------------
ref_year  <- 2026L
claim_age <- 65L                                                                                                                  
work_ages <- 21:64

# ---- Real-conversion factor: nominal $year -> real $2026 --------------------                                                   
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == ref_year]
stopifnot(length(gdp_pi_2026) == 1, !is.na(gdp_pi_2026))                                                                          
real_factor <- tibble(                                                                                                            
  year        = tr2025$year,                                                                                                      
  real_factor = gdp_pi_2026 / tr2025$gdp_pi                                                                                       
)                                                                                                                                 

# ---- Load source data -------------------------------------------------------                                                   
nte <- read_csv("./output/pb_net_tax_on_earnings.csv", show_col_types = FALSE)

# ---- Closed-form marginal IRR ----------------------------------------------
# Vectorized: returns NA where delta or tax_real is missing/non-positive, or                                                      
# the row is outside working ages.                                                                                                
marginal_irr <- function(delta, tax_real, age) {
  ok <- !is.na(delta) & !is.na(tax_real) &                                                                                        
    delta > 0 & tax_real > 0 &                        
    age >= min(work_ages) & age <= max(work_ages)                                                                             
  r <- rep(NA_real_, length(delta))                       
  r[ok] <- (delta[ok] / tax_real[ok])^(1 / (claim_age - age[ok])) - 1                                                             
  r                                                                                                                               
}                                                                                                                                 

# ---- Compute --------------------------------------------------------------
mirr <- nte %>%
  filter(age %in% work_ages) %>%
  inner_join(real_factor, by = "year") %>%                                                                                        
  mutate(
    tax_real               = tax * real_factor,                                                                                   
    marginal_irr_scheduled = marginal_irr(delta_pv_ben_sched, tax_real, age),                                                     
    marginal_irr_payable   = marginal_irr(delta_pv_ben_pb,    tax_real, age)
  ) %>%                                                                                                                           
  select(worker_type, spouse_type, birth_yr, claim_age, age, year,
         marginal_irr_scheduled, marginal_irr_payable)                                                                            

# ---- Save -----------------------------------------------------------------                                                     
saveRDS(mirr,  "./output/marginal_irr_by_age.rds")        
write_csv(mirr, "./output/marginal_irr_by_age.csv")                                                                               

cat(sprintf("Wrote %d marginal IRR rows to output/marginal_irr_by_age.csv\n",                                                     
            nrow(mirr)))                                                                                                          
print(head(mirr, 15))                       
