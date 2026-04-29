# initial_replacement_rates.R
# -----------------------------------------------------------------------------
# Computes initial wage replacement rates at age 65 for each
# (worker_type, spouse_type, birth_yr) combination, using two denominators:
#
#   rep_rate_awi      = annual benefit at age 65 / AWI in year worker turns 65
#   rep_rate_career   = real benefit at 65 ($2026) / real career-average
#                       earnings ($2026), where career average is taken over
#                       all years from age 21 onward (zero-earning years
#                       included).
#
# Both rates are unitless. rep_rate_career rebases benefit and earnings to
# $2026 using gdp_pi before averaging.
#
# Output columns:
#   id, worker_type, spouse_type, birth_yr, claim_age, year_at_65,
#   awi_at_65, ben_at_65, real_ben_at_65, real_career_avg_earn,
#   rep_rate_awi, rep_rate_career
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
birth_years   <- c(1930L, 1935L, 1940L, 1945L, 1950L, 1955L, 1960L, 1965L, 1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)
claim_age     <- 65L
career_length <- 44L
ref_year      <- 2026L

par_max_yr  <- max(tr2025$year)
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == ref_year]

# ---- Helper: benefits for one couple (or single worker) ---------------------
gen_couple_ben <- function(worker_type, spouse_type, birth_yr, claim_age,
                           par, sef, career_length = 44L) {
  
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

# ---- Real career-average earnings (in $2026) -------------------------------
# Earnings are on the worker's own record, so the same value appears under
# every spouse pairing. Compute once per (worker_type, birth_yr) and join.
gdp_pi_lookup <- tr2025 %>% transmute(year, gdp_pi)

career_avg <- all_ben %>%
  distinct(worker_type, birth_yr, year, age, earnings) %>%
  left_join(gdp_pi_lookup, by = "year") %>%
  mutate(real_earn = earnings * gdp_pi_2026 / gdp_pi) %>%
  filter(age >= 21L, age <= 64L) %>%
  group_by(worker_type, birth_yr) %>%
  summarise(real_career_avg_earn = mean(real_earn), .groups = "drop")

# ---- AWI lookup and replacement rates --------------------------------------
awi_lookup <- tr2025 %>% transmute(year_at_65 = year,
                                   awi_at_65  = awi,
                                   gdp_pi_at_65 = gdp_pi)

rep_rates <- all_ben %>%
  filter(age == 65L) %>%
  rename(year_at_65 = year, ben_at_65 = annual_ben) %>%
  left_join(awi_lookup, by = "year_at_65") %>%
  left_join(career_avg, by = c("worker_type", "birth_yr")) %>%
  mutate(
    real_ben_at_65  = ben_at_65 * gdp_pi_2026 / gdp_pi_at_65,
    rep_rate_awi    = ben_at_65 / awi_at_65,
    rep_rate_career = real_ben_at_65 / real_career_avg_earn
  ) %>%
  select(id, worker_type, spouse_type, birth_yr, claim_age,
         year_at_65, awi_at_65, ben_at_65, real_ben_at_65,
         real_career_avg_earn, rep_rate_awi, rep_rate_career)

# ---- Save --------------------------------------------------------------------
saveRDS(rep_rates,  "./output/initial_replacement_rates.rds")
write_csv(rep_rates, "./output/initial_replacement_rates.csv")
