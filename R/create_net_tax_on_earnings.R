# create_net_tax_on_earnings.R                                                                                                    
# -----------------------------------------------------------------------------                                                   
# Marginal net tax on earnings at each working age, for the primary worker                                                        
# (worker_type) given a fixed spouse (spouse_type).                                                                               
#                                                                                                                                 
#   net_tax(t) = ( tax(t) - [PV_ben(through t) - PV_ben(through t-1)] )                                                           
#                / earnings(t)                                                                                                    
#                                                                                                                                 
# PV_ben(through t) is the household-level real PV of benefits at age 65                                                          
# (in $2026) when primary's earnings beyond age t are zeroed out. Household                                                       
# benefits include:                                                                                                               
#   - primary's own retired-worker benefit                                                                                        
#   - primary's spousal benefit on partner's record (depends on primary PIA)                                                      
#   - partner's spousal benefit on primary's record (depends on primary PIA)                                                      
# Partner's career earnings are taken as given (not perturbed).                                                                   
#                                                                                                                                 
# All terms in the ratio are placed in the same units (real $2026 at age 65),                                                     
# but the ratio is invariant to that choice — equivalent to computing in                                                          
# year-t nominal, since the scaling factor cancels in numerator and denominator.                                                  
#                                                                                                                                 
# SIGN: net_tax > 0 means the worker pays more in tax than they receive in                                                        
# marginal PV of benefits (net tax burden). net_tax < 0 means marginal PV                                                         
# of benefits exceeds tax (net subsidy). This matches the standard                                                                
# Feldstein-Samwick / Goda-Shoven-Slavov convention.                                                                              
#                                                                                                                                 
# Output columns:                                                                                                                 
#   worker_type, spouse_type, birth_yr, claim_age, age, year,                                                                     
#   earnings, tax, pv_ben_through_t, delta_pv_ben, net_tax
# -----------------------------------------------------------------------------                                                   

library(tidyverse)                                                                                                                
library(slider)                                           
library(checkmate)                                                                                                                

load("./data/tr2025.rda")                                                                                                         
load("./data/sef2025.rda")

source("./R/general_helpers.R")                                                                                                   
source("./R/generate_worker.R")
source("./R/earnings.R")                                                                                                          
source("./R/eligibility.R")                                                                                                       
source("./R/aime.R")
source("./R/pia.R")                                                                                                               
source("./R/special_min_pia.R")                                                                                                   
source("./R/worker_ben.R")
source("./R/spousal.R")                                                                                                           
source("./R/calc_ben.R")                                  
source("./R/calc_tax.R")                                                                                                          

# ---- Configuration -----------------------------------------------------------                                                  
worker_types  <- c("very_low", "low", "medium", "high", "max")
spouse_types  <- c("none", "very_low", "low", "medium", "high", "max")                                                            
birth_years   <- c(1930L, 1935L, 1940L, 1945L, 1950L, 1955L, 1960L, 1965L,                                                        
                   1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)                                                 
claim_age     <- 65L                                                                                                              
career_length <- 44L                                                                                                              
ref_year      <- 2026L                                                                                                            
work_ages     <- 21:64                                    

par_max_yr  <- max(tr2025$year)                                                                                                   
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == ref_year]                                                                             

# ---- Pre-compute (type, birth_yr) baselines once ----------------------------                                                   
# Each unique (type, birth_yr) shows up many times across the grid (as primary                                                    
# and as partner). Cache the worker frame, the tax stream, and the spouse-PIA                                                     
# pipeline output so we never recompute them.                                                                                     
type_combos <- expand_grid(type = worker_types, birth_yr = birth_years)                                                           

cache <- type_combos %>% pmap(\(type, birth_yr) {                                                                                 
  w <- generate_retired_worker(                                                                                                   
    sef = sef2025, par = tr2025,                                                                                                  
    birth_yr      = as.integer(birth_yr),                 
    claim_age     = as.integer(claim_age),                                                                                        
    type          = type,                                 
    career_length = career_length                                                                                                 
  ) %>% filter(year <= par_max_yr)                                                                                                
  list(                                                                                                                           
    worker      = w,                                                                                                              
    tax         = calc_tax(tr2025, w) %>% select(id, year, age, tax),                                                             
    spouse_info = generate_spousal_info(tr2025, w)                                                                                
  )                                                                                                                               
})                                                                                                                                
names(cache) <- paste(type_combos$type, type_combos$birth_yr, sep = "-")                                                          

# ---- Cohort lookups ---------------------------------------------------------
le_at_65 <- tr2025 %>%                                                                                                            
  transmute(year_at_65 = year, death_age = round((le_m + le_f) / 2))                                                              

# Real factor: nominal flow at year y -> real PV at age 65 in $2026                                                               
make_real_factor <- function(birth_yr, par) {                                                                                     
  y65         <- as.integer(birth_yr) + 65L                                                                                       
  df_y65      <- par$df[par$year == y65]                                                                                          
  gdp_pi_y65  <- par$gdp_pi[par$year == y65]                                                                                      
  par %>% transmute(year, real_factor = (df_y65 / df) * (gdp_pi_2026 / gdp_pi_y65))                                               
}                                                                                                                                 

# ---- Helper: PV of household benefits with primary truncated at t -----------                                                   
# With the upstream NA-safe lookups in aime/pia/cola/worker_ben and the
# elig_age fix in spousal.R, calc_ben handles an uninsured truncated primary                                                      
# cleanly (own wrk_ben = 0, spousal_ben on partner's record runs through). The                                                    
# only remaining short-circuit is the truly-no-partner case where there's                                                         
# also no spousal benefit to compute.                                                                                             
pv_household_ben <- function(t, primary_baseline, partner_baseline,                                                               
                             partner_info, par, real_factor_lookup, death_age) {                                                  
  
  mod_primary <- primary_baseline %>%                     
    mutate(earnings = if_else(age <= t, earnings, 0))                                                                             
  
  pre <- mod_primary %>% join_all_assumptions(par) %>% eligibility()                                                              
  primary_eligible <- !is.na(pre$elig_age[1])                                                                                     
  
  if (is.null(partner_baseline)) {                                                                                                
    if (!primary_eligible) return(0)                                                                                              
    ben <- calc_ben(par, mod_primary, output = "skinny") %>%                                                                      
      transmute(year, age, household_ben = annual_ben)                                                                            
  } else {
    mod_primary <- mod_primary %>% mutate(spouse_id = partner_baseline$id[1])                                                     
    
    primary_ben <- calc_ben(par, mod_primary, partner_info, output = "skinny") %>%                                                
      transmute(year, age, ben_p = annual_ben)                                                                                    
    
    # Partner's spousal benefit depends on primary's (modified) PIA, so the                                                       
    # spouse-info pipeline has to be rerun on the modified primary.
    mod_primary_info <- generate_spousal_info(par, mod_primary)                                                                   
    mod_partner      <- partner_baseline %>%                                                                                      
      mutate(spouse_id = mod_primary$id[1])                                                                                       
    partner_ben      <- calc_ben(par, mod_partner, mod_primary_info, output = "skinny") %>%                                       
      transmute(year, age, ben_s = annual_ben)                                                                                    
    
    ben <- primary_ben %>%                                                                                                        
      full_join(partner_ben, by = c("year", "age")) %>%   
      mutate(household_ben = coalesce(ben_p, 0) + coalesce(ben_s, 0))                                                             
  }                                                                                                                               
  
  ben %>%                                                                                                                         
    filter(age <= death_age) %>%                          
    left_join(real_factor_lookup, by = "year") %>%
    summarise(pv = sum(household_ben * real_factor, na.rm = TRUE)) %>%                                                            
    pull(pv)                                                                                                                      
}                                                                                                                                 

# ---- Helper: net tax for one config ----------------------------------------
gen_net_tax <- function(worker_type, spouse_type, birth_yr, claim_age,
                        par, cache, le_at_65) {                                                                                   
  
  pri <- cache[[paste(worker_type, birth_yr, sep = "-")]]                                                                         
  if (spouse_type == "none") {                            
    partner_baseline <- NULL                                                                                                      
    partner_info     <- NULL                              
  } else {                                                                                                                        
    pcache           <- cache[[paste(spouse_type, birth_yr, sep = "-")]]
    partner_baseline <- pcache$worker                                                                                             
    partner_info     <- pcache$spouse_info                
  }                                                                                                                               
  
  y65       <- as.integer(birth_yr) + 65L                                                                                         
  death_age <- le_at_65$death_age[le_at_65$year_at_65 == y65]
  rfl       <- make_real_factor(birth_yr, par)                                                                                    
  
  pv_bens <- map_dbl(work_ages, \(t) pv_household_ben(                                                                            
    t, pri$worker, partner_baseline, partner_info, par, rfl, death_age))
  
  delta_pv_ben <- pv_bens - c(0, head(pv_bens, -1))                                                                               
  
  earn_tax <- pri$worker %>%                                                                                                      
    filter(age %in% work_ages) %>%                        
    select(age, year, earnings) %>%                                                                                               
    left_join(pri$tax %>% select(age, tax), by = "age") %>%
    left_join(rfl, by = "year")                                                                                                   
  
  tibble(                                                                                                                         
    age              = work_ages,                                                                                                 
    pv_ben_through_t = pv_bens,                           
    delta_pv_ben     = delta_pv_ben                                                                                               
  ) %>%
    left_join(earn_tax, by = "age") %>%                                                                                           
    mutate(                                                                                                                       
      tax_real      = tax      * real_factor,
      earnings_real = earnings * real_factor,                                                                                     
      net_tax = if_else(earnings_real > 0,                
                        (tax_real - delta_pv_ben) / earnings_real,                                                                
                        NA_real_),
      worker_type = worker_type,                                                                                                  
      spouse_type = spouse_type,                          
      birth_yr    = as.integer(birth_yr),                                                                                         
      claim_age   = as.integer(claim_age)
    ) %>%                                                                                                                         
    select(worker_type, spouse_type, birth_yr, claim_age, age, year,
           earnings, tax, pv_ben_through_t, delta_pv_ben, net_tax)                                                                
}                                                                                                                                 

# ---- Build the full grid (no dedup: asymmetric per primary) ----------------                                                    
grid <- expand_grid(                                      
  worker_type = worker_types,                                                                                                     
  spouse_type = spouse_types,                                                                                                     
  birth_yr    = birth_years
)                                                                                                                                 

# ---- Run --------------------------------------------------------------------                                                   
all_net_tax <- grid %>%
  pmap_dfr(\(worker_type, spouse_type, birth_yr)                                                                                  
           gen_net_tax(worker_type, spouse_type, birth_yr,                                                                        
                       claim_age = claim_age,
                       par       = tr2025,                                                                                        
                       cache     = cache,                 
                       le_at_65  = le_at_65),
           .progress = TRUE)                                                                                                      

# Post-hoc rule at age 21: primary can't be insured after one year of earnings,                                                   
# so the marginal effect of age-21 earnings on household PV is mechanically
# zero, and net_tax(21) = tax / earnings (gross OASDI rate).                                                                      
all_net_tax_fixed <- all_net_tax %>%                                                                                              
  mutate(                                                                                                                         
    delta_pv_ben = if_else(age == 21L, 0, delta_pv_ben),                                                                          
    net_tax      = if_else(age == 21L & earnings > 0,                                                                             
                           tax / earnings,
                           net_tax)                                                                                               
  )                                                       

# ---- Save -------------------------------------------------------------------
saveRDS(all_net_tax_fixed,  "./output/net_tax_on_earnings.rds")
write_csv(all_net_tax_fixed, "./output/net_tax_on_earnings.csv")