# initial_replacement_rates.R
# -----------------------------------------------------------------------------
# Computes initial wage replacement rates at age 65 for each
# (worker_type, spouse_type, birth_yr) combination.
#
# Replacement rate = annual benefit at age 65 / AWI in the year worker turns 65
#
# Output columns:
#   id, worker_type, spouse_type, birth_yr, claim_age, year_at_65,
#   awi_at_65, ben_at_65, rep_rate
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
birth_years   <- c(1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
claim_age     <- 65L
career_length <- 44L

# ---- Helper: benefits for one couple (or single worker) ---------------------
gen_couple_ben <- function(worker_type, spouse_type, birth_yr, claim_age,
                           par, sef, career_length = 44L) {
  
  par_max_yr <- max(par$year)
  
  if (spouse_type == "none") {
    worker <- generate_retired_worker(
      sef = sef, par = par,
      birth_yr      = as.integer(birth_yr),
      claim_age     = as.integer(claim_age),
      type          = worker_type,
      career_length = career_length
    ) %>% filter(year <= par_max_yr)
    ben <- calc_ben(par, worker, output = "skinny")
  } else {
    spouse <- generate_retired_worker(
      sef = sef, par = par,
      birth_yr      = as.integer(birth_yr),
      claim_age     = as.integer(claim_age),
      type          = spouse_type,
      career_length = career_length
    ) %>% filter(year <= par_max_yr)
    s_id <- spouse$id[1]
    
    worker <- generate_retired_worker(
      sef = sef, par = par,
      birth_yr      = as.integer(birth_yr),
      claim_age     = as.integer(claim_age),
      type          = worker_type,
      spouse_id     = s_id,
      career_length = career_length
    ) %>% filter(year <= par_max_yr)
    
    spouse_info <- generate_spousal_info(par, spouse)
    ben <- calc_ben(par, worker, spouse_info, output = "skinny")
  }
  
  ben %>% mutate(
    worker_type = worker_type,
    spouse_type = spouse_type,
    birth_yr    = as.integer(birth_yr),
    claim_age   = as.integer(claim_age)
  )
}

# ---- Build the full grid -----------------------------------------------------
grid <- expand_grid(
  worker_type = worker_types,
  spouse_type = spouse_types,
  birth_yr    = birth_years
)

all_ben <- grid %>%
  pmap_dfr(\(worker_type, spouse_type, birth_yr)
           gen_couple_ben(worker_type, spouse_type, birth_yr,
                          claim_age     = claim_age,
                          par           = tr2025,
                          sef           = sef2025,
                          career_length = career_length))

# ---- Filter to age 65 and compute replacement rate --------------------------
awi_lookup <- tr2025 %>% transmute(year_at_65 = year, awi_at_65 = awi)

rep_rates <- all_ben %>%
  filter(age == 65L) %>%
  rename(year_at_65 = year, ben_at_65 = annual_ben) %>%
  left_join(awi_lookup, by = "year_at_65") %>%
  mutate(rep_rate = ben_at_65 / awi_at_65) %>%
  select(id, worker_type, spouse_type, birth_yr, claim_age,
         year_at_65, awi_at_65, ben_at_65, rep_rate)

# ---- Save --------------------------------------------------------------------
saveRDS(rep_rates,  "./output/initial_replacement_rates.rds")
write_csv(rep_rates, "./output/initial_replacement_rates.csv")
