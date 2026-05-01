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

# Use the extended assumptions file so we can reach birth cohorts whose
# retirement years run past tr2025's standard 2150 horizon. The original
# tr2025.rda is left untouched.
load("./data/tr2025_extended.rda")
tr2025 <- tr2025_extended
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
# 1930 through 2200 in 5-year steps. Cohorts past 2035 retire after the
# Trustees Report's formal 2099 horizon, so values for those cohorts are
# based on extrapolated assumptions (see build/create_extended_assumptions.R).
birth_years <- as.integer(seq(1930, 2200, by = 5))
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
# Replicates generate_retired_worker()'s body but skips its
# `birth_yr <= 2036` assertion so we can build cohorts past 2035 without
# modifying the upstream function. Uses the unmodified generate_earnings()
# helper for the earnings vector.
build_worker <- function(birth_yr) {
  birth_yr <- as.integer(birth_yr)
  worker_type_label <- paste0("custom", custom_avg_earnings)
  id <- paste0("R-", worker_type_label, "-", birth_yr, "-", career_length, "-", claim_age)

  first_yr <- birth_yr + 21L
  last_yr  <- birth_yr + 119L
  worker <- data.frame(
    id        = id,
    age       = 21:119,
    year      = first_yr:last_yr,
    birth_yr  = birth_yr,
    claim_age = claim_age,
    dis_age   = NA_real_
  )

  earnings <- generate_earnings(
    sef                 = sef2025,
    par                 = tr2025,
    birth_yr            = birth_yr,
    type                = "custom",
    custom_avg_earnings = custom_avg_earnings,
    debugg              = FALSE
  )

  worker %>%
    left_join(earnings %>% select(age, earnings), by = "age") %>%
    mutate(earnings = if_else(is.na(earnings), 0, earnings)) %>%
    filter(year <= par_max_yr)
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
  if (length(ben_at_65_nominal) == 0) ben_at_65_nominal <- 0
  # For very-far-future cohorts the AWI-indexed QC threshold can grow past
  # the worker's fixed-real earnings, leaving the worker uninsured (40-QC
  # threshold not met). All benefit-side metrics are NA in that case;
  # tax-side metrics still have real meaning (taxes are still paid).
  is_insured <- isTRUE(ben_at_65_nominal > 0)

  real_ben_at_65    <- ben_at_65_nominal * gdp_pi_2026 / gdp_pi_y65
  monthly_real_sched <- if (is_insured) real_ben_at_65 / 12               else NA_real_
  monthly_real_pb    <- if (is_insured) monthly_real_sched * payable_y65  else NA_real_

  career_earn <- worker %>%
    filter(age %in% work_ages) %>%
    left_join(real_factor_lookup, by = "year") %>%
    mutate(real_earn = earnings * real_factor)
  real_career_avg <- mean(career_earn$real_earn)
  rep_rate_career_sched <- if (is_insured) real_ben_at_65 / real_career_avg     else NA_real_
  rep_rate_career_pb    <- if (is_insured) rep_rate_career_sched * payable_y65  else NA_real_

  rep_rate_awi_sched <- if (is_insured) ben_at_65_nominal / awi_at_65   else NA_real_
  rep_rate_awi_pb    <- if (is_insured) rep_rate_awi_sched * payable_y65 else NA_real_

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

  if (!is_insured) { pv_ben_sched <- NA_real_; pv_ben_pb <- NA_real_ }
  ben_tax_ratio_sched <- if (is_insured) pv_ben_sched / pv_tax else NA_real_
  ben_tax_ratio_pb    <- if (is_insured) pv_ben_pb    / pv_tax else NA_real_

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

  # The closed-form marginal IRR at age 64 reduces to delta / tax_real - 1.
  # When age-64 earnings are the marginal year that pushes the worker across
  # the 40-QC permanent-insurance threshold, delta jumps from 0 to the full
  # PV of benefits and the result spikes to several hundred percent. Clamp
  # such knife-edge values to NA — they're not a meaningful marginal return.
  clamp_extreme_mirr <- function(r) if (!is.na(r) && r > 1) NA_real_ else r
  marginal_irr_64_sched <- if (is_insured && isTRUE(delta_pv_sched > 0) && tax_64_real > 0) {
    clamp_extreme_mirr(delta_pv_sched / tax_64_real - 1)
  } else NA_real_
  marginal_irr_64_pb    <- if (is_insured && isTRUE(delta_pv_pb > 0) && tax_64_real > 0) {
    clamp_extreme_mirr(delta_pv_pb / tax_64_real - 1)
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

# ---- Lifetime profile: long-format earnings/benefits per cohort ------------
# Real earnings 21-64 are identical across cohorts by construction (the
# scalar normalising top-35 average to $50K cancels the level differences
# in real_AWI). We still emit them per cohort so the bundler can pick any
# representative profile and so a downstream consumer can verify the
# invariance directly.
compute_lifetime_long <- function(birth_yr) {
  birth_yr   <- as.integer(birth_yr)
  y65        <- birth_yr + 65L
  death_age  <- le_at_65$death_age[le_at_65$year_at_65 == y65]

  worker <- build_worker(birth_yr)
  ben    <- calc_ben(tr2025, worker, output = "skinny")

  earn_long <- worker %>%
    filter(age %in% work_ages) %>%
    left_join(real_factor_lookup, by = "year") %>%
    transmute(birth_yr, age, year,
              real_earnings    = earnings * real_factor,
              real_ben_sched   = NA_real_,
              real_ben_payable = NA_real_)

  ben_long <- ben %>%
    filter(age >= 65L, age <= death_age) %>%
    left_join(payable_lookup,     by = "year") %>%
    left_join(real_factor_lookup, by = "year") %>%
    transmute(birth_yr, age, year,
              real_earnings    = NA_real_,
              real_ben_sched   = annual_ben           * real_factor,
              real_ben_payable = annual_ben * payable * real_factor)

  bind_rows(earn_long, ben_long)
}

# ---- Run --------------------------------------------------------------------
results <- map_dfr(birth_years, function(by) {
  cat(sprintf("Cohort %d ...\n", by))
  compute_metrics(by)
})

lifetime <- map_dfr(birth_years, compute_lifetime_long)

# ---- Save -------------------------------------------------------------------
saveRDS(results,   "./output/constant_earner_metrics.rds")
write_csv(results,  "./output/constant_earner_metrics.csv")
saveRDS(lifetime,  "./output/constant_earner_lifetime.rds")
write_csv(lifetime, "./output/constant_earner_lifetime.csv")

cat(sprintf("\nWrote %d metric rows and %d lifetime rows.\n",
            nrow(results), nrow(lifetime)))
