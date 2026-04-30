# patch_uninsured_spousal_sched.R                         
# ----------------------------------------------------------------------------- 
# SINGLE USE ONLY
# Targeted patch to output/net_tax_on_earnings.csv after the spousal_benefit()
# elig_age fix and the pv_household_ben short-circuit removal.                                                                    
#                                                                                                                                 
# Updates the rows the fix actually moves:                                                                                        
#   * pv_ben_through_t at pre-insurance ages of the 5 affected couple                                                             
#     configs — recomputed once per (worker, spouse, cohort) since the                                                            
#     pv is constant across the pre-insurance range.                                                                              
#   * delta_pv_ben + net_tax at each affected couple's year-of-insurance                                                          
#     row — the only post-fix value that changes when its preceding                                                               
#     pv_ben_through_t is updated.                                                                                                
#                                                         
# Untouched everywhere else:                                                                                                      
#   * Singles (no spousal calc invoked).                                                                                          
#   * 20 non-supplement couple types (own_PIA already >= 0.5 * spouse_PIA, so                                                     
#     the supplement is 0 either way and the on-disk values are correct).                                                         
#   * Post-insurance rows of the 5 affected couples (pv_household_ben(t)                                                          
#     for an insured truncated worker is unchanged by the fix; calc_ben                                                           
#     was already correct under the first-fix code).                                                                              
#   * The pb file — explicitly skipped; you'll regenerate that with step 7.                                                       
#                                                                                                                                 
# Affected couples: only those where 0.5 * spouse_PIA > own_PIA (verified                                                         
# empirically against the post-spousal-fix annual-benefits data):                                                                 
#   very_low | medium                                                                                                             
#   very_low | high                                                                                                               
#   very_low | max                                                                                                                
#   low      | high                                       
#   low      | max                                                                                                                
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

# ---- Pull pv_household_ben + make_real_factor from the sched script --------                                                    
extract_fn <- function(path, fn_name) {                                                                                           
  src   <- readLines(path)                                                                                                        
  start <- grep(paste0("^", fn_name, " <- function"), src)[1]
  end   <- grep("^\\}", src)[grep("^\\}", src) > start][1]
  paste(src[start:end], collapse = "\n")                                                                                          
}
sched_path <- "./R/create_net_tax_on_earnings.R"                                                                                  
eval(parse(text = extract_fn(sched_path, "make_real_factor")), envir = .GlobalEnv)                                                
eval(parse(text = extract_fn(sched_path, "pv_household_ben")), envir = .GlobalEnv)                                                

# ---- Configuration ---------------------------------------------------------                                                    
affected_pairs <- tribble(                                
  ~worker_type, ~spouse_type,                                                                                                     
  "very_low",   "medium",
  "very_low",   "high",                                                                                                           
  "very_low",   "max",                                                                                                            
  "low",        "high",
  "low",        "max"                                                                                                             
)                                                         
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

# ---- Cache only the worker types we need ----------------------------------                                                     
needed_types <- unique(c(affected_pairs$worker_type, affected_pairs$spouse_type))                                                 
type_combos  <- expand_grid(type = needed_types, birth_yr = birth_years)                                                          

cache <- type_combos %>% pmap(function(type, birth_yr) {                                                                          
  w <- generate_retired_worker(                                                                                                   
    sef = sef2025, par = tr2025,                                                                                                  
    birth_yr      = as.integer(birth_yr),                                                                                         
    claim_age     = claim_age,                                                                                                    
    type          = type,                                                                                                         
    career_length = career_length                                                                                                 
  ) %>% filter(year <= par_max_yr)                        
  list(                                                                                                                           
    worker      = w,
    spouse_info = generate_spousal_info(tr2025, w),                                                                               
    solo_ben    = calc_ben(tr2025, w, output = "skinny") %>% select(year, age, annual_ben)                                        
  )                                                                                                                               
})                                                                                                                                
names(cache) <- paste(type_combos$type, type_combos$birth_yr, sep = "-")                                                          

# ---- Per-config patch ------------------------------------------------------                                                    
patch_one_couple <- function(worker_type, spouse_type, birth_yr, rows) {                                                          
  pri              <- cache[[paste(worker_type, birth_yr, sep = "-")]]                                                            
  pcache           <- cache[[paste(spouse_type, birth_yr, sep = "-")]]                                                            
  partner_baseline <- pcache$worker                                                                                               
  partner_info     <- pcache$spouse_info                                                                                          
  partner_solo_ben <- pcache$solo_ben                                                                                             
  
  y65       <- as.integer(birth_yr) + 65L                                                                                         
  death_age <- le_at_65$death_age[le_at_65$year_at_65 == y65]
  rfl       <- make_real_factor(birth_yr, tr2025)                                                                                 
  
  # Insurance age = first row > 21 with a non-zero delta_pv_ben on disk.                                                          
  # Insurance status doesn't depend on the spousal calc, so this is robust                                                        
  # to either bug state.                                                                                                          
  ins_row <- rows %>%                                                                                                             
    filter(age >= 22, abs(delta_pv_ben) > 1e-9) %>%                                                                               
    slice_min(age, n = 1)                                                                                                         
  insurance_age <- if (nrow(ins_row) == 0) NA_integer_ else as.integer(ins_row$age[1])
  
  # New constant pre-insurance pv (one call — same value for every                                                                
  # pre-insurance age, since adding more zeroed-earnings years doesn't                                                            
  # change anything).                                                                                                             
  pv_pre <- pv_household_ben(                                                                                                     
    t = 21L, pri$worker, partner_baseline, partner_info, tr2025, rfl,                                                             
    death_age, partner_solo_ben                                                                                                   
  )                                                       
  
  pre_ages <- if (is.na(insurance_age)) work_ages else seq(21L, insurance_age - 1L)                                               
  
  patched <- rows %>%                                                                                                             
    mutate(pv_ben_through_t = if_else(age %in% pre_ages, pv_pre, pv_ben_through_t))
  
  if (!is.na(insurance_age)) {                            
    pv_at_ins <- patched$pv_ben_through_t[patched$age == insurance_age]                                                           
    new_delta <- pv_at_ins - pv_pre                                                                                               
    
    rf       <- rfl$real_factor[rfl$year == (birth_yr + insurance_age)]                                                           
    earn_real<- patched$earnings[patched$age == insurance_age] * rf                                                               
    tax_real <- patched$tax     [patched$age == insurance_age] * rf                                                               
    new_nt   <- if (earn_real > 0) (tax_real - new_delta) / earn_real else NA_real_                                               
    
    patched <- patched %>%                                                                                                        
      mutate(                                                                                                                     
        delta_pv_ben = if_else(age == insurance_age, new_delta, delta_pv_ben),
        net_tax      = if_else(age == insurance_age, new_nt,    net_tax)                                                          
      )                                                                                                                           
  }                                                                                                                               
  patched                                                                                                                         
}                                                         

# ---- Driver ----------------------------------------------------------------                                                    
nte_sched <- read_csv("./output/net_tax_on_earnings.csv", show_col_types = FALSE)

affected_grid <- affected_pairs %>% expand_grid(birth_yr = birth_years)                                                           

patched_couples <- affected_grid %>%                                                                                              
  pmap_dfr(function(worker_type, spouse_type, birth_yr) { 
    rows <- nte_sched %>% filter(                                                                                                 
      worker_type == !!worker_type,                                                                                               
      spouse_type == !!spouse_type,
      birth_yr    == !!birth_yr                                                                                                   
    )                                                                                                                             
    patch_one_couple(worker_type, spouse_type, birth_yr, rows)
  }, .progress = TRUE)                                                                                                            

patch_keys <- patched_couples %>% distinct(worker_type, spouse_type, birth_yr)                                                    

nte_sched_patched <- bind_rows(                                                                                                   
  nte_sched %>% anti_join(patch_keys, by = c("worker_type", "spouse_type", "birth_yr")),
  patched_couples                                                                                                                 
) %>%
  arrange(worker_type, spouse_type, birth_yr, age)                                                                                

stopifnot(nrow(nte_sched_patched) == nrow(nte_sched))

saveRDS(nte_sched_patched,  "./output/net_tax_on_earnings.rds")                                                                   
write_csv(nte_sched_patched, "./output/net_tax_on_earnings.csv")

cat(sprintf("Patched %d rows across %d (worker, spouse, cohort) configs.\n",                                                      
            nrow(patched_couples), nrow(patch_keys)))                                                                             
cat(sprintf("Total rows in sched file: %d (unchanged).\n", nrow(nte_sched_patched)))