# create_pb_net_tax_on_earnings.R                         
# -----------------------------------------------------------------------------
# Marginal net tax on earnings, scheduled and payable scenarios in parallel.                                                      
#                                                                                                                                 
# Strategy: every cohort is recomputed fresh through gen_net_tax(). A single                                                      
# pv_household_ben() call returns both pv_sched and pv_pb from the same                                                           
# calc_ben result (one calc_ben per truncation age, two scenarios), so the                                                        
# payable side adds no extra spousal-benefit work — just a second weighted                                                        
# sum at the end.                                                                                                                 
#                                                                                                                                 
# At end of run, the recomputed _sched columns are cross-checked against the                                                      
# saved create_net_tax_on_earnings.rds file. A mismatch means saved_sched is                                                      
# stale (i.e., create_net_tax_on_earnings.R needs to be re-run first), and                                                        
# the script halts with an error rather than writing inconsistent output.                                                         
#                                                                                                                                 
# Output mirrors the original schema with parallel _sched / _pb columns.                                                          
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

le_at_65 <- tr2025 %>%                                                                                                            
  transmute(year_at_65 = year, death_age = round((le_m + le_f) / 2))

# ---- Pre-compute (type, birth_yr) baselines once ----------------------------                                                   
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

# ---- Real-factor + payable lookup (per cohort) -----------------------------                                                    
# Bundle real_factor and payable so pv_household_ben does a single join.                                                          
make_real_factor <- function(birth_yr, par) {                                                                                     
  y65         <- as.integer(birth_yr) + 65L                                                                                       
  df_y65      <- par$df[par$year == y65]                                                                                          
  gdp_pi_y65  <- par$gdp_pi[par$year == y65]                                                                                      
  par %>% transmute(                                                                                                              
    year,
    real_factor = (df_y65 / df) * (gdp_pi_2026 / gdp_pi_y65),                                                                     
    payable                                                                                                                       
  )
}                                                                                                                                 

# ---- Helper: PV of household benefits, both scenarios from one calc_ben ----                                                    
# Mirrors create_net_tax_on_earnings.R's pv_household_ben but returns both
# pv_sched and pv_pb in one tibble. The payable scenario applies the                                                              
# tr2025$payable factor year-by-year before discounting.  
pv_household_ben <- function(t, primary_baseline, partner_baseline,                                                               
                             partner_info, par, real_factor_lookup, death_age) {                                                  
  
  mod_primary <- primary_baseline %>%                                                                                             
    mutate(earnings = if_else(age <= t, earnings, 0))     
  
  pre <- mod_primary %>% join_all_assumptions(par) %>% eligibility()
  primary_eligible <- !is.na(pre$elig_age[1])
  
  if (is.null(partner_baseline)) {
    if (!primary_eligible) return(tibble(pv_sched = 0, pv_pb = 0))                                                                
    ben <- calc_ben(par, mod_primary, output = "skinny") %>%                                                                      
      transmute(year, age, household_ben = annual_ben)
  } else {                                                                                                                        
    mod_primary <- mod_primary %>% mutate(spouse_id = partner_baseline$id[1])
    
    primary_ben <- calc_ben(par, mod_primary, partner_info, output = "skinny") %>%                                                
      transmute(year, age, ben_p = annual_ben)
    
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
    summarise(                                            
      pv_sched = sum(household_ben           * real_factor, na.rm = TRUE),
      pv_pb    = sum(household_ben * payable * real_factor, na.rm = TRUE)                                                         
    )                                                                                                                             
}                                                                                                                                 

# ---- Helper: net tax for one config (both scenarios) -----------------------
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
  
  pv_bens <- map_dfr(work_ages, \(t) pv_household_ben(
    t, pri$worker, partner_baseline, partner_info, par, rfl, death_age))                                                          
  
  delta_sched <- pv_bens$pv_sched - c(0, head(pv_bens$pv_sched, -1))                                                              
  delta_pb    <- pv_bens$pv_pb    - c(0, head(pv_bens$pv_pb,    -1))                                                              
  
  earn_tax <- pri$worker %>%                              
    filter(age %in% work_ages) %>%                                                                                                
    select(age, year, earnings) %>%                       
    left_join(pri$tax %>% select(age, tax), by = "age") %>%
    left_join(rfl %>% select(year, real_factor), by = "year")                                                                     
  
  tibble(                                                                                                                         
    age                    = work_ages,                                                                                           
    pv_ben_through_t_sched = pv_bens$pv_sched,            
    pv_ben_through_t_pb    = pv_bens$pv_pb,                                                                                       
    delta_pv_ben_sched     = delta_sched,
    delta_pv_ben_pb        = delta_pb                                                                                             
  ) %>%                                                                                                                           
    left_join(earn_tax, by = "age") %>%
    mutate(                                                                                                                       
      tax_real      = tax      * real_factor,             
      earnings_real = earnings * real_factor,                                                                                     
      net_tax_sched = if_else(earnings_real > 0,
                              (tax_real - delta_pv_ben_sched) / earnings_real,                                                    
                              NA_real_),                                                                                          
      net_tax_pb    = if_else(earnings_real > 0,
                              (tax_real - delta_pv_ben_pb) / earnings_real,                                                       
                              NA_real_),                                                                                          
      worker_type   = worker_type,
      spouse_type   = spouse_type,                                                                                                
      birth_yr      = as.integer(birth_yr),               
      claim_age     = as.integer(claim_age)                                                                                       
    ) %>%
    select(worker_type, spouse_type, birth_yr, claim_age, age, year,                                                              
           earnings, tax,                                                                                                         
           pv_ben_through_t_sched, delta_pv_ben_sched, net_tax_sched,
           pv_ben_through_t_pb,    delta_pv_ben_pb,    net_tax_pb)                                                                
}                                                                                                                                 

# ---- Run for the full grid -------------------------------------------------                                                    
grid <- expand_grid(                                      
  worker_type = worker_types,                                                                                                     
  spouse_type = spouse_types,                             
  birth_yr    = birth_years                                                                                                       
)

all_part <- grid %>%                                      
  pmap_dfr(\(worker_type, spouse_type, birth_yr)
           gen_net_tax(worker_type, spouse_type, birth_yr,                                                                        
                       claim_age = claim_age,
                       par       = tr2025,                                                                                        
                       cache     = cache,                                                                                         
                       le_at_65  = le_at_65),
           .progress = TRUE)                                                                                                      

# Post-hoc age-21 rule, applied to both scenarios.                                                                                
all_net_tax <- all_part %>%
  mutate(                                                                                                                         
    delta_pv_ben_sched = if_else(age == 21L, 0, delta_pv_ben_sched),
    delta_pv_ben_pb    = if_else(age == 21L, 0, delta_pv_ben_pb),                                                                 
    net_tax_sched      = if_else(age == 21L & earnings > 0, tax / earnings, net_tax_sched),                                       
    net_tax_pb         = if_else(age == 21L & earnings > 0, tax / earnings, net_tax_pb)                                           
  ) %>%                                                                                                                           
  arrange(worker_type, spouse_type, birth_yr, claim_age, age)                                                                     

# ---- Cross-check: recomputed _sched must match saved sched everywhere ------                                                    
# This catches the case where create_net_tax_on_earnings.R hasn't been re-run                                                     
# after a code change. A mismatch means saved_sched is stale; halt rather than                                                    
# write a pb file inconsistent with the sched file.                                                                               
saved_sched <- readRDS("./output/net_tax_on_earnings.rds")                                                                        

sched_check <- saved_sched %>%                                                                                                    
  select(worker_type, spouse_type, birth_yr, claim_age, age,                                                                      
         net_tax_saved = net_tax)                                                                                                 

diffs <- all_net_tax %>%                                                                                                          
  left_join(sched_check,                                  
            by = c("worker_type", "spouse_type", "birth_yr", "claim_age", "age")) %>%                                             
  filter(!is.na(net_tax_sched), !is.na(net_tax_saved)) %>%                                                                        
  mutate(abs_diff = abs(net_tax_sched - net_tax_saved)) %>%
  filter(abs_diff > 1e-9)                                                                                                         

if (nrow(diffs) > 0) {                                                                                                            
  stop(sprintf(                                           
    "Recomputed scheduled net_tax differs from saved sched in %d rows (max abs diff = %s).\n%s",                                  
    nrow(diffs), signif(max(diffs$abs_diff), 3),                                                                                  
    "saved_sched is stale — re-run create_net_tax_on_earnings.R before this script."
  ), call. = FALSE)
}
message("Sanity check passed: recomputed scheduled net_tax matches saved sched on every row.")

# ---- Save -------------------------------------------------------------------
saveRDS(all_net_tax,  "./output/pb_net_tax_on_earnings.rds")
write_csv(all_net_tax, "./output/pb_net_tax_on_earnings.csv")