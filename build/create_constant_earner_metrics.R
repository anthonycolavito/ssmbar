# build/create_constant_earner_metrics.R
# -----------------------------------------------------------------------------
# Generates the eight cohort-comparison metrics (scheduled + payable scenarios)
# for a $50,000 custom-earner across all 17 birth cohorts, using the existing
# R/ helper functions (no modifications to any of them).
#
# A "constant earner" here means custom_avg_earnings = $50,000 — the worker's
# top-35 real-earnings average is held constant at $50K (in current-year
# dollars) across every cohort. The shape of the earnings profile follows
# generate_earnings()'s "custom" mode (raw scaled-earner shape, level scaled
# so the top-35 real average hits the target).
#
# Output: output/constant_earner_metrics.csv
#   one row per birth_yr with monthly_real_at_65_*, pv_benefits_*, pv_taxes,
#   ben_tax_ratio_*, rep_rate_career_*, rep_rate_awi_*, irr_*, marginal_irr_age64_*
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

# ---- Configuration ---------------------------------------------------------
custom_avg_earnings <- 50000
birth_years   <- c(1930L, 1935L, 1940L, 1945L, 1950L, 1955L, 1960L, 1965L,
                   1970L, 1975L, 1980L, 1985L, 1990L, 1995L, 2000L, 2005L, 2010L)
claim_age     <- 65L
career_length <- 44L
ref_year      <- 2026L
work_ages     <- 21:64

par_max_yr    <- max(tr2025$year)
gdp_pi_2026   <- tr2025$gdp_pi[tr2025$year == ref_year]

real_factor_lookup <- tr2025 %>%
  transmute(year, real_factor = gdp_pi_2026 / gdp_pi)

payable_lookup <- tr2025 %>% transmute(year, payable)

le_at_65 <- tr2025 %>%
  transmute(year_at_65 = year, death_age = round((le_m + le_f) / 2))

# ---- Helper: PV factor (nominal $year -> real $2026 at age 65) -------------
make_pv_factor <- function(birth_yr) {
  y65        <- as.integer(birth_yr) + 65L
  df_y65     <- tr2025$df[tr2025$year == y65]
  gdp_pi_y65 <- tr2025$gdp_pi[tr2025$year == y65]
  tr2025 %>% transmute(year, pv_factor = (df_y65 / df) * (gdp_pi_2026 / gdp_pi_y65))
}

# ---- Helper: build the custom $50K worker frame ----------------------------
build_worker <- function(birth_yr, earnings_override = NULL) {
  w <- generate_retired_worker(
    sef = sef2025, par = tr2025,
    birth_yr            = as.integer(birth_yr),
    claim_age           = claim_age,
    type                = "custom",
    custom_avg_earnings = custom_avg_earnings,
    career_length       = career_length
  ) %>% filter(year <= par_max_yr)

  if (!is.null(earnings_override)) {
    # earnings_override is a list of (age, earnings) pairs to substitute
    for (e in earnings_override) {
      w <- w %>% mutate(earnings = if_else(age == e$age, e$earnings, earnings))
    }
  }
  w
}

# ---- IRR solver: real annualized rate where NPV(real flows) = 0 ------------
solve_irr <- function(cf_df, lower = -0.20, upper = 0.30) {
  if (nrow(cf_df) == 0) return(NA_real_)
  t  <- cf_df$age - 21L
  cf <- cf_df$cf
  npv <- function(r) sum(cf / (1 + r)^t)
  if (sign(npv(lower)) == sign(npv(upper))) return(NA_real_)
  uniroot(npv, lower = lower, upper = upper, tol = 1e-6)$root
}

# ---- Compute all eight metrics for one cohort ------------------------------
compute_metrics <- function(birth_yr) {
  y65        <- as.integer(birth_yr) + 65L
  death_age  <- le_at_65$death_age[le_at_65$year_at_65 == y65]
  awi_at_65  <- tr2025$awi[tr2025$year == y65]
  gdp_pi_y65 <- tr2025$gdp_pi[tr2025$year == y65]
  payable_y65 <- tr2025$payable[tr2025$year == y65]

  worker <- build_worker(birth_yr)
  ben    <- calc_ben(tr2025, worker, output = "skinny")
  taxes  <- calc_tax(tr2025, worker)

  pv_factor <- make_pv_factor(birth_yr)

  # 1 / 5 / 6 — initial real benefit at age 65 + replacement rates
  ben_at_65_nominal <- ben$annual_ben[ben$age == 65L]
  real_ben_at_65    <- ben_at_65_nominal * gdp_pi_2026 / gdp_pi_y65
  monthly_real_sched <- real_ben_at_65 / 12
  monthly_real_pb    <- monthly_real_sched * payable_y65

  career_earn <- worker %>%
    filter(age %in% work_ages) %>%
    left_join(real_factor_lookup, by = "year") %>%
    mutate(real_earn = earnings * real_factor)
  real_career_avg <- mean(career_earn$real_earn)
  rep_rate_career_sched <- real_ben_at_65 / real_career_avg
  rep_rate_career_pb    <- rep_rate_career_sched * payable_y65

  rep_rate_awi_sched <- ben_at_65_nominal / awi_at_65
  rep_rate_awi_pb    <- rep_rate_awi_sched * payable_y65

  # 2 / 3 / 4 — PV of benefits and taxes (real $2026, anchored at age 65)
  ben_pv <- ben %>%
    filter(age >= 65L, age <= death_age) %>%
    left_join(payable_lookup, by = "year") %>%
    left_join(pv_factor,      by = "year") %>%
    summarise(
      pv_ben_sched = sum(annual_ben           * pv_factor, na.rm = TRUE),
      pv_ben_pb    = sum(annual_ben * payable * pv_factor, na.rm = TRUE)
    )
  pv_ben_sched <- ben_pv$pv_ben_sched
  pv_ben_pb    <- ben_pv$pv_ben_pb

  pv_tax <- taxes %>%
    filter(age %in% work_ages) %>%
    left_join(pv_factor, by = "year") %>%
    summarise(pv_tax = sum(tax * pv_factor, na.rm = TRUE)) %>%
    pull(pv_tax)

  ben_tax_ratio_sched <- pv_ben_sched / pv_tax
  ben_tax_ratio_pb    <- pv_ben_pb    / pv_tax

  # 7 — IRR (real, undiscounted cash flows)
  cf_work <- taxes %>%
    filter(age %in% work_ages) %>%
    left_join(real_factor_lookup, by = "year") %>%
    transmute(age, cf = -tax * real_factor)

  cf_ret_sched <- ben %>%
    filter(age >= 65L, age <= death_age) %>%
    left_join(real_factor_lookup, by = "year") %>%
    transmute(age, cf = annual_ben * real_factor)

  cf_ret_pb <- ben %>%
    filter(age >= 65L, age <= death_age) %>%
    left_join(payable_lookup,      by = "year") %>%
    left_join(real_factor_lookup,  by = "year") %>%
    transmute(age, cf = annual_ben * payable * real_factor)

  irr_sched <- solve_irr(bind_rows(cf_work, cf_ret_sched) %>% arrange(age))
  irr_pb    <- solve_irr(bind_rows(cf_work, cf_ret_pb)    %>% arrange(age))

  # 8 — Marginal IRR at age 64
  # delta_pv_ben_64 = PV(full career) - PV(career truncated at age 63 — i.e.,
  # age-64 earnings zeroed). Closed form: r = (delta / tax_real)^(1/1) - 1.
  worker_trunc63 <- worker %>% mutate(earnings = if_else(age == 64L, 0, earnings))
  ben_trunc63    <- calc_ben(tr2025, worker_trunc63, output = "skinny")

  pv_ben_trunc63 <- ben_trunc63 %>%
    filter(age >= 65L, age <= death_age) %>%
    left_join(payable_lookup, by = "year") %>%
    left_join(pv_factor,      by = "year") %>%
    summarise(
      pv_ben_sched_t = sum(annual_ben           * pv_factor, na.rm = TRUE),
      pv_ben_pb_t    = sum(annual_ben * payable * pv_factor, na.rm = TRUE)
    )
  delta_pv_sched <- pv_ben_sched - pv_ben_trunc63$pv_ben_sched_t
  delta_pv_pb    <- pv_ben_pb    - pv_ben_trunc63$pv_ben_pb_t

  tax_64_real <- taxes %>%
    filter(age == 64L) %>%
    left_join(real_factor_lookup, by = "year") %>%
    summarise(tr = sum(tax * real_factor)) %>%
    pull(tr)

  marginal_irr_64_sched <- if (delta_pv_sched > 0 && tax_64_real > 0) {
    delta_pv_sched / tax_64_real - 1
  } else NA_real_
  marginal_irr_64_pb    <- if (delta_pv_pb > 0 && tax_64_real > 0) {
    delta_pv_pb / tax_64_real - 1
  } else NA_real_

  tibble(
    birth_yr                     = as.integer(birth_yr),
    claim_age                    = claim_age,
    death_age                    = death_age,
    monthly_real_at_65_scheduled = monthly_real_sched,
    monthly_real_at_65_payable   = monthly_real_pb,
    pv_benefits_scheduled        = pv_ben_sched,
    pv_benefits_payable          = pv_ben_pb,
    pv_taxes                     = pv_tax,
    ben_tax_ratio_scheduled      = ben_tax_ratio_sched,
    ben_tax_ratio_payable        = ben_tax_ratio_pb,
    rep_rate_career_scheduled    = rep_rate_career_sched,
    rep_rate_career_payable      = rep_rate_career_pb,
    rep_rate_awi_scheduled       = rep_rate_awi_sched,
    rep_rate_awi_payable         = rep_rate_awi_pb,
    irr_scheduled                = irr_sched,
    irr_payable                  = irr_pb,
    marginal_irr_age64_scheduled = marginal_irr_64_sched,
    marginal_irr_age64_payable   = marginal_irr_64_pb
  )
}

# ---- Run --------------------------------------------------------------------
results <- map_dfr(birth_years, function(by) {
  cat(sprintf("Cohort %d ...\n", by))
  compute_metrics(by)
})

# ---- Save -------------------------------------------------------------------
saveRDS(results,  "./output/constant_earner_metrics.rds")
write_csv(results, "./output/constant_earner_metrics.csv")

cat(sprintf("\nWrote %d rows to output/constant_earner_metrics.csv\n", nrow(results)))
print(results, n = nrow(results))
