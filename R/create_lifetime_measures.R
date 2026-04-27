# pv_lifetime_taxes_benefits.R
# -----------------------------------------------------------------------------
# Real (2026-dollar) present value of lifetime OASDI taxes and benefits, and
# the benefit-tax ratio, for each (worker_type, spouse_type, birth_yr).
#
# Each worker's flows are discounted to their own age-65 year (y_65) using
# the nominal forward factor `df`, then expressed in 2026 dollars using the
# gdp_pi price index:
#     real PV at y_65, in $2026 =
#         sum( flow[y] * df[y_65] / df[y] * gdp_pi[2026] / gdp_pi[y_65] )
# Note: this rescaling does not affect ben_tax_ratio (the per-worker
# constant cancels), only the absolute pv_taxes and pv_benefits.
#
# For couples, taxes and benefits are split equally across the two members
# (each member's lifetime PV = (sum across both) / 2). Singles are treated
# as a single-member couple, so the same aggregation works.
#
# Two optimizations:
#   1. (type, birth_yr) worker frame, tax stream, and spouse-PIA pipeline
#      output are computed once per unique combination and cached. Only
#      calc_ben() runs per grid row, since it's the only piece that
#      depends on both members.
#   2. Couples grid is restricted to unordered pairs (worker_type position
#      <= spouse_type position), and homogamous couples skip the duplicate
#      build of the second member.
#
# Output columns:
#   worker_type, spouse_type, birth_yr, claim_age, death_age,
#   pv_taxes, pv_benefits, ben_tax_ratio
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
birth_years   <- c(1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
claim_age     <- 65L
career_length <- 44L
ref_year      <- 2026L

# ---- Pre-compute (type, birth_yr) frames once -------------------------------
# Each unique (type, birth_yr) appears in many grid rows. The worker frame,
# tax stream, and spouse-PIA pipeline all depend only on the worker, so we
# compute them once and look them up from `cache` keyed by "type-birth_yr".
par_max_yr <- max(tr2025$year)

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

# ---- Helper: lifetime flows for one couple (or single worker) ---------------
gen_couple_flows <- function(worker_type, spouse_type, birth_yr, claim_age,
                             par, cache) {
  
  build_flows <- function(w, spouse_info, tax_df) {
    ben <- calc_ben(par, w, spouse_info, output = "skinny")
    ben %>%
      left_join(tax_df, by = c("id", "year", "age")) %>%
      mutate(tax = coalesce(tax, 0))
  }
  
  key_a <- paste(worker_type, birth_yr, sep = "-")
  a <- cache[[key_a]]
  
  if (spouse_type == "none") {
    flows <- build_flows(a$worker, NULL, a$tax) %>% mutate(member = "self")
    
  } else if (worker_type == spouse_type) {
    # Homogamous: both members are identical, so just compute once.
    w <- a$worker %>% mutate(spouse_id = a$worker$id[1])
    flows <- build_flows(w, a$spouse_info, a$tax) %>% mutate(member = "self")
    
  } else {
    # Heterogamous: each member's flows uses the partner as their spouse.
    key_b <- paste(spouse_type, birth_yr, sep = "-")
    b <- cache[[key_b]]
    
    w_a <- a$worker %>% mutate(spouse_id = b$worker$id[1])
    w_b <- b$worker %>% mutate(spouse_id = a$worker$id[1])
    
    flows <- bind_rows(
      build_flows(w_a, b$spouse_info, a$tax) %>% mutate(member = "a"),
      build_flows(w_b, a$spouse_info, b$tax) %>% mutate(member = "b")
    )
  }
  
  flows %>% mutate(
    worker_type = worker_type,
    spouse_type = spouse_type,
    birth_yr    = as.integer(birth_yr),
    claim_age   = as.integer(claim_age)
  )
}

# ---- Build the grid: singles + unordered couple pairs -----------------------
type_pos <- function(t) match(t, worker_types)

singles_grid <- expand_grid(
  worker_type = worker_types,
  spouse_type = "none",
  birth_yr    = birth_years
)

couples_grid <- expand_grid(
  worker_type = worker_types,
  spouse_type = worker_types,
  birth_yr    = birth_years
) %>% filter(type_pos(worker_type) <= type_pos(spouse_type))

grid <- bind_rows(singles_grid, couples_grid)

# ---- Run all configs --------------------------------------------------------
all_flows <- grid %>%
  pmap_dfr(\(worker_type, spouse_type, birth_yr)
           gen_couple_flows(worker_type, spouse_type, birth_yr,
                            claim_age = claim_age,
                            par       = tr2025,
                            cache     = cache))

# ---- LE-based death age (truncation cap) ------------------------------------
le_at_65 <- tr2025 %>%
  transmute(year_at_65 = year,
            death_age  = round(65 + (le_m + le_f) / 2))

# ---- Real PV factor: discount to age 65, then express in 2026 dollars -------
# For a flow at year y belonging to a worker who turns 65 in year y_65:
#   nominal PV at y_65         = flow * df[y_65] / df[y]
#   real PV at y_65 in $2026   = flow * df[y_65] / df[y] * gdp_pi[2026] / gdp_pi[y_65]
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == ref_year]

flow_factors   <- tr2025 %>% transmute(year, df_y = df)
worker_factors <- tr2025 %>%
  transmute(year_at_65 = year, df_y65 = df, gdp_pi_y65 = gdp_pi)

# ---- Aggregate to PV per member, then average across members ----------------
pv_results <- all_flows %>%
  mutate(year_at_65 = birth_yr + 65L) %>%
  left_join(le_at_65,       by = "year_at_65") %>%
  left_join(flow_factors,   by = "year") %>%
  left_join(worker_factors, by = "year_at_65") %>%
  mutate(real_factor = (df_y65 / df_y) * (gdp_pi_2026 / gdp_pi_y65)) %>%
  filter(age <= death_age, !is.na(real_factor)) %>%
  group_by(worker_type, spouse_type, birth_yr, claim_age, death_age, member) %>%
  summarise(
    pv_taxes_member    = sum(tax        * real_factor, na.rm = TRUE),
    pv_benefits_member = sum(annual_ben * real_factor, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  summarise(
    pv_taxes    = mean(pv_taxes_member),
    pv_benefits = mean(pv_benefits_member),
    .groups = "drop"
  ) %>%
  mutate(ben_tax_ratio = pv_benefits / pv_taxes)

# ---- Save --------------------------------------------------------------------
saveRDS(pv_results,  "./output/pv_lifetime_taxes_benefits.rds")
write_csv(pv_results, "./output/pv_lifetime_taxes_benefits.csv")

