#create_irr.R
# -----------------------------------------------------------------------------                                                   
# Internal Rate of Return on lifetime OASDI participation, by worker config,
# under scheduled and payable benefits scenarios.                                                                                 
#
# IRR is the real annualized rate r solving NPV = 0:                                                                              
#   sum_t (B_t - T_t) / (1 + r)^(t - 21)                  
# where T_t is OASDI tax paid in year t (ages 21-64) and B_t is the annual                                                        
# Social Security benefit in year t (age 65 to expected death age). Cash
# flows are in real $2026; the resulting IRR is a real rate.                                                                      
#                                                         
# For couples, primary and spouse-as-primary cash flows are summed; IRR is                                                        
# invariant to scaling, so this matches per-member or household-level                                                             
# treatment.
#                                                                                                                                 
# Inputs:                                                 
#   output/net_tax_on_earnings.csv         (nominal tax stream)
#   output/benefits_by_worker_age.csv      (scheduled real benefits)                                                              
#   output/pb_benefits_by_worker_age.csv   (payable real benefits)                                                                
#   data/tr2025.rda                         (gdp_pi for nominal->real)                                                            
#                                                                                                                                 
# Output:                                                 
#   output/irr_by_config.csv  columns:                                                                                            
#     worker_type, spouse_type, birth_yr, claim_age, irr_scheduled, irr_payable                                                   
# -----------------------------------------------------------------------------

library(tidyverse)                                                                                                                

load("./data/tr2025.rda")                                                                                                         

# ---- Configuration ----------------------------------------------------------
worker_types <- c("very_low", "low", "medium", "high", "max")
spouse_types <- c("none", "very_low", "low", "medium", "high", "max")                                                             
birth_years  <- c(1930L, 1935L, 1940L, 1945L, 1950L, 1955L, 1960L, 1965L,
                  1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)                                                  
claim_age    <- 65L                                                                                                               
ref_year     <- 2026L                                                                                                             
work_ages    <- 21:64                                                                                                             

# ---- Real-conversion factor: nominal $year -> real $2026 --------------------
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == ref_year]
stopifnot(length(gdp_pi_2026) == 1, !is.na(gdp_pi_2026))
real_factor <- tibble(                                                                                                            
  year        = tr2025$year,
  real_factor = gdp_pi_2026 / tr2025$gdp_pi                                                                                       
)                                                                                                                                 

# ---- Load streams -----------------------------------------------------------                                                   
nte   <- read_csv("./output/net_tax_on_earnings.csv",       show_col_types = FALSE)
ba    <- read_csv("./output/benefits_by_worker_age.csv",    show_col_types = FALSE)                                               
ba_pb <- read_csv("./output/pb_benefits_by_worker_age.csv", show_col_types = FALSE)

# Primary worker's nominal tax -> real $2026.                                                                                     
tax_real <- nte %>%                                                                                                               
  filter(age %in% work_ages) %>%                                                                                                  
  inner_join(real_factor, by = "year") %>%                
  transmute(worker_type, spouse_type, birth_yr, age,
            tax_real = tax * real_factor)

# Benefits files already carry real_ben / real_ben_pb in $2026.                                                                   
ben_sched <- ba    %>% transmute(worker_type, spouse_type, birth_yr, age, ben_real = real_ben)
ben_pb    <- ba_pb %>% transmute(worker_type, spouse_type, birth_yr, age, ben_real = real_ben_pb)                                 

# ---- Helpers ----------------------------------------------------------------                                                   
# Build a one-row-per-age cash-flow tibble for a given (worker, spouse, birth)
# config under one benefits scenario. Cash flow at age t = household benefit                                                      
# minus household tax (tax in working years only, benefit from claim age on).                                                     
build_cash_flow <- function(w, s, b, ben_data) {                                                                                  
  pri_tax <- tax_real %>% filter(worker_type == w, spouse_type == s, birth_yr == b)                                               
  pri_ben <- ben_data %>% filter(worker_type == w, spouse_type == s, birth_yr == b)                                               
  
  if (s == "none") {                                      
    spo_tax <- pri_tax %>% mutate(tax_real = 0)                                                                                   
    spo_ben <- pri_ben %>% mutate(ben_real = 0)                                                                                   
  } else {
    spo_tax <- tax_real %>% filter(worker_type == s, spouse_type == w, birth_yr == b)                                             
    spo_ben <- ben_data %>% filter(worker_type == s, spouse_type == w, birth_yr == b)                                             
  }
  
  ages_all <- sort(unique(c(pri_tax$age, pri_ben$age)))                                                                           
  tibble(age = ages_all) %>%
    left_join(pri_tax %>% select(age, pri_tax = tax_real), by = "age") %>%                                                        
    left_join(spo_tax %>% select(age, spo_tax = tax_real), by = "age") %>%                                                        
    left_join(pri_ben %>% select(age, pri_ben = ben_real), by = "age") %>%
    left_join(spo_ben %>% select(age, spo_ben = ben_real), by = "age") %>%                                                        
    mutate(across(c(pri_tax, spo_tax, pri_ben, spo_ben), ~replace_na(., 0)),                                                      
           cash_flow = (pri_ben + spo_ben) - (pri_tax + spo_tax)) %>%                                                             
    arrange(age)                                                                                                                  
}                                                         

# Solve NPV = 0 for r. Cash flows are one year apart; t = age - 21.                                                               
# Returns NA when (lower, upper) doesn't bracket a root (extreme configs).
solve_irr <- function(cf_df, lower = -0.20, upper = 0.30) {                                                                       
  if (nrow(cf_df) == 0) return(NA_real_)                                                                                          
  t  <- cf_df$age - 21L                                                                                                           
  cf <- cf_df$cash_flow                                                                                                           
  npv <- function(r) sum(cf / (1 + r)^t)                  
  if (sign(npv(lower)) == sign(npv(upper))) return(NA_real_)                                                                      
  uniroot(npv, lower = lower, upper = upper, tol = 1e-6)$root                                                                     
}

# ---- Run --------------------------------------------------------------------                                                   
grid <- expand_grid(
  worker_type = worker_types,                                                                                                     
  spouse_type = spouse_types,                             
  birth_yr    = birth_years
)

irr_results <- grid %>%
  rowwise() %>%                                                                                                                   
  mutate(                                                 
    irr_scheduled = solve_irr(build_cash_flow(worker_type, spouse_type, birth_yr, ben_sched)),
    irr_payable   = solve_irr(build_cash_flow(worker_type, spouse_type, birth_yr, ben_pb))                                        
  ) %>%
  ungroup() %>%                                                                                                                   
  mutate(claim_age = claim_age) %>%                       
  select(worker_type, spouse_type, birth_yr, claim_age, irr_scheduled, irr_payable)                                               

# ---- Save -------------------------------------------------------------------
saveRDS(irr_results,  "./output/irr_by_config.rds")                                                                               
write_csv(irr_results, "./output/irr_by_config.csv")      

cat(sprintf("Wrote %d IRR rows to output/irr_by_config.csv\n", nrow(irr_results)))                                                
print(irr_results, n = 15)