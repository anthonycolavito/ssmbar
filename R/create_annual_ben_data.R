# benefits_by_worker_age.R
# -----------------------------------------------------------------------------
# Long dataset of nominal and real annual Social Security benefits by
# worker-age, crossing worker type x spouse type x birth cohort.
# Spouse shares the worker's birth year and claim age.
#
# Output columns:
#   id, worker_type, spouse_type, birth_yr, claim_age, year, age,
#   earnings, nominal_ben, real_ben, death_age
#
# Each worker spans age 65 through expected death age (= round(65 + unisex
# LE-at-65) in the year the worker turns 65, averaging le_m and le_f).
#
# Real conversion uses gdp_pi from tr2025 with `base_year` as the reference.
# Requires:
#   - generate_retired_worker() to accept type = "max".
#   - calc_ben() to handle spouse = NULL (set spousal_ben = 0 when missing).
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
birth_years   <- c(1940L, 1945L, 1950L, 1955L, 1960L, 1965L, 1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)
claim_age     <- 65L
career_length <- 44L
base_year     <- 2026                 # reference year for real dollars

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
    )
    ben <- calc_ben(par, worker, output = "skinny")
  } else {
    # Build the spouse first so we know its id
    spouse <- generate_retired_worker(
      sef = sef, par = par,
      birth_yr      = as.integer(birth_yr),
      claim_age     = as.integer(claim_age),
      type          = spouse_type,
      career_length = career_length
    )
    s_id <- spouse$id[1]
    
    # Worker, pointed at the spouse
    worker <- generate_retired_worker(
      sef = sef, par = par,
      birth_yr      = as.integer(birth_yr),
      claim_age     = as.integer(claim_age),
      type          = worker_type,
      spouse_id     = s_id,
      career_length = career_length
    )
    
    # Run the PIA pipeline on the spouse (id -> s_id, age -> s_age, etc.)
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

# ---- Deflate to real dollars -------------------------------------------------
deflator <- tr2025 %>%
  transmute(year, price_factor = gdp_pi[year == base_year] / gdp_pi)

# ---- Unisex LE at 65 -> expected death age per cohort -----------------------
# Period LE-at-65 in the calendar year the worker turns 65, averaged across sex.
le_at_65 <- tr2025 %>%
  transmute(year_at_65 = year,
            death_age  = round((le_m + le_f) / 2))

ben_by_age <- all_ben %>%
  left_join(deflator, by = "year") %>%
  rename(nominal_ben = annual_ben) %>%
  mutate(real_ben   = nominal_ben * price_factor,
         year_at_65 = birth_yr + 65L) %>%
  left_join(le_at_65, by = "year_at_65") %>%
  filter(age >= 65L, age <= death_age, !is.na(real_ben)) %>%
  select(id, worker_type, spouse_type, birth_yr, claim_age,
         year, age, earnings, nominal_ben, real_ben, death_age)

# ---- Save ----------------------------------------------
dir.create("./output", showWarnings = FALSE)
saveRDS(ben_by_age,  "./output/benefits_by_worker_age.rds")
write_csv(ben_by_age, "./output/benefits_by_worker_age.csv")
