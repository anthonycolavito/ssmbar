# create_payable_lifetime_measures.R
# -----------------------------------------------------------------------------
# Payable-scenario lifetime PV of taxes and benefits, derived from saved data:
#   - benefits_by_worker_age.rds:    nominal_ben (sched, year-level)
#   - pb_benefits_by_worker_age.rds: nominal_ben_pb (payable, year-level)
#   - pv_lifetime_taxes_benefits.rds: source of pv_taxes (scenario-invariant)
#                                     and a check value for sched recomputation
# No calc_ben() or calc_tax() reruns.
# -----------------------------------------------------------------------------

library(tidyverse)

load("./data/tr2025.rda")

# ---- Saved inputs ------------------------------------------------------------
sched_ben <- readRDS("./output/benefits_by_worker_age.rds")
pb_ben    <- readRDS("./output/pb_benefits_by_worker_age.rds")
pv_sched  <- readRDS("./output/pv_lifetime_taxes_benefits.rds")

# ---- Configuration -----------------------------------------------------------
worker_types <- c("very_low", "low", "medium", "high", "max")
ref_year     <- 2026L
gdp_pi_2026  <- tr2025$gdp_pi[tr2025$year == ref_year]
type_pos     <- function(t) match(t, worker_types)

# ---- Combine sched and pb annual benefits on row keys -----------------------
key_cols <- c("id", "worker_type", "spouse_type", "birth_yr", "claim_age",
              "year", "age")

ben_combined <- sched_ben %>%
  left_join(pb_ben %>% select(all_of(key_cols), nominal_ben_pb),
            by = key_cols)

# ---- Real PV factor lookups (same as create_lifetime_measures.R) ------------
flow_factors   <- tr2025 %>% transmute(year, df_y = df)
worker_factors <- tr2025 %>%
  transmute(year_at_65 = year, df_y65 = df, gdp_pi_y65 = gdp_pi)

# ---- Worker-level PV, both scenarios in one pass ----------------------------
worker_pv <- ben_combined %>%
  mutate(year_at_65 = birth_yr + 65L) %>%
  left_join(flow_factors,   by = "year") %>%
  left_join(worker_factors, by = "year_at_65") %>%
  mutate(real_factor = (df_y65 / df_y) * (gdp_pi_2026 / gdp_pi_y65)) %>%
  group_by(worker_type, spouse_type, birth_yr, claim_age) %>%
  summarise(
    pv_ben_sched_member = sum(nominal_ben    * real_factor, na.rm = TRUE),
    pv_ben_pb_member    = sum(nominal_ben_pb * real_factor, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Canonicalize couple direction, then mean across members ---------------
# Singles/homogamous: one row per (canonical key), mean of one value = self.
# Heterogamous: two rows (one per direction in saved annual data), mean = (a+b)/2.
pv_canonical <- worker_pv %>%
  mutate(
    type_lo = if_else(spouse_type == "none",
                      worker_type,
                      worker_types[pmin(type_pos(worker_type), type_pos(spouse_type))]),
    type_hi = if_else(spouse_type == "none",
                      "none",
                      worker_types[pmax(type_pos(worker_type), type_pos(spouse_type))])
  ) %>%
  group_by(birth_yr, claim_age, type_lo, type_hi) %>%
  summarise(
    pv_ben_sched = mean(pv_ben_sched_member),
    pv_ben_pb    = mean(pv_ben_pb_member),
    .groups = "drop"
  ) %>%
  rename(worker_type = type_lo, spouse_type = type_hi)

# ---- Join saved pv_taxes; compute ratios -----------------------------------
pv_results <- pv_canonical %>%
  left_join(
    pv_sched %>%
      transmute(worker_type, spouse_type, birth_yr, claim_age, death_age,
                pv_taxes, pv_ben_sched_saved = pv_benefits),
    by = c("worker_type", "spouse_type", "birth_yr", "claim_age")
  ) %>%
  mutate(
    ben_tax_ratio_sched = pv_ben_sched / pv_taxes,
    ben_tax_ratio_pb    = pv_ben_pb    / pv_taxes
  ) %>%
  select(worker_type, spouse_type, birth_yr, claim_age, death_age,
         pv_taxes, pv_ben_sched, pv_ben_pb,
         ben_tax_ratio_sched, ben_tax_ratio_pb,
         pv_ben_sched_saved)

# ---- Sanity check: recomputed sched PV should match saved ------------------
diffs <- pv_results %>%
  mutate(rel_diff = abs(pv_ben_sched - pv_ben_sched_saved) /
           pmax(abs(pv_ben_sched_saved), 1)) %>%
  filter(rel_diff > 1e-6)

if (nrow(diffs) > 0) {
  warning("Recomputed scheduled PV differs from saved in ", nrow(diffs),
          " rows; max rel diff = ", signif(max(diffs$rel_diff), 3))
} else {
  message("Sanity check passed: recomputed scheduled PV matches saved.")
}

pv_results <- pv_results %>% select(-pv_ben_sched_saved, -pv_ben_sched, -ben_tax_ratio_sched, pv_taxes_pb = pv_taxes)
  

# ---- Save ------------------------------------------------------------------
saveRDS(pv_results,  "./output/pb_pv_lifetime_taxes_benefits.rds")
write_csv(pv_results, "./output/pb_pv_lifetime_taxes_benefits.csv")
