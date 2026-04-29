# build_site_data.R
#
# Reshapes the seven output CSVs into a single nested JSON file consumed by
# the static dashboard at site/. Run by the GitHub Action on every push.
#
# Inputs:
#   Scheduled-benefits scenario:
#     output/benefits_by_worker_age.csv
#     output/initial_replacement_rates.csv
#     output/pv_lifetime_taxes_benefits.csv
#     output/net_tax_on_earnings.csv
#   Payable-benefits scenario:
#     output/pb_benefits_by_worker_age.csv
#     output/pb_initial_replacement_rates.csv
#     output/pb_pv_lifetime_taxes_benefits.csv
#     (pb_net_tax_on_earnings.csv: not yet computed; nmtr stays scenario-shared
#      until it lands and PB_NMTR_PENDING flips to FALSE.)
#
# Output: site/data/site_data.json

suppressPackageStartupMessages({
  library(jsonlite)
})

WORKER_TYPES <- c("very_low", "low", "medium", "high", "max")
SPOUSE_TYPES <- c("none", "very_low", "low", "medium", "high", "max")
BIRTH_YEARS  <- seq(1930, 2010, by = 5)
CLAIM_AGE    <- 65L

# Flip to TRUE if a future model bug forces the Net Tax Rate chart into its
# "data not yet available" empty state for every cohort while the scheduled
# CSV is regenerated. Normal operation: FALSE.
NMTR_VALUES_PENDING <- FALSE

# Flip to FALSE once pb_net_tax_on_earnings.csv lands. While TRUE, the Net Tax
# Rate chart shows only the scheduled line plus a "payable scenario coming
# soon" caption. Earnings columns stay shared across scenarios since worker
# earnings are exogenous to the scheduled/payable distinction.
PB_NMTR_PENDING <- TRUE

WORKER_LABELS <- c(
  very_low = "Very Low Earner",
  low      = "Low Earner",
  medium   = "Medium Earner",
  high     = "High Earner",
  max      = "Maximum Earner"
)
SPOUSE_LABELS <- c(
  none     = "None (Single)",
  very_low = "Very Low Earner",
  low      = "Low Earner",
  medium   = "Medium Earner",
  high     = "High Earner",
  max      = "Maximum Earner"
)

# ---- Load --------------------------------------------------------------------
read_csv_strict <- function(path) {
  stopifnot(file.exists(path))
  read.csv(path, stringsAsFactors = FALSE)
}

ben_age      <- read_csv_strict("output/benefits_by_worker_age.csv")
nmtr         <- read_csv_strict("output/net_tax_on_earnings.csv")
pv           <- read_csv_strict("output/pv_lifetime_taxes_benefits.csv")
rep_rates    <- read_csv_strict("output/initial_replacement_rates.csv")

ben_age_pb   <- read_csv_strict("output/pb_benefits_by_worker_age.csv")
pv_pb        <- read_csv_strict("output/pb_pv_lifetime_taxes_benefits.csv")
rep_rates_pb <- read_csv_strict("output/pb_initial_replacement_rates.csv")

# Period unisex life expectancy at age 65 by claim year, derived from tr2025.
load("data/tr2025.rda")
BASE_YEAR <- 2026L
le_at_65 <- data.frame(
  claim_year      = tr2025$year,
  le_age_at_death = round((tr2025$le_m + tr2025$le_f) / 2)
)

# Normal Retirement Age by birth cohort. tr2025$nra is keyed by the calendar
# year the cohort reaches age 62 (the EEA), so we look it up at birth_yr + 62.
nra_by_birth_year <- function(birth_yr) {
  hit <- tr2025$nra[tr2025$year == birth_yr + 62L]
  if (length(hit) != 1 || is.na(hit)) {
    stop(sprintf("No NRA in tr2025 for birth_yr %d (expected at year %d)",
                 birth_yr, birth_yr + 62L))
  }
  hit
}

# Price factor for converting nominal $year to real $2026 (GDP price index).
gdp_pi_2026 <- tr2025$gdp_pi[tr2025$year == BASE_YEAR]
stopifnot(length(gdp_pi_2026) == 1, !is.na(gdp_pi_2026))
price_factor_by_year <- setNames(gdp_pi_2026 / tr2025$gdp_pi, tr2025$year)

# ---- Validate dimensions -----------------------------------------------------
expected_combos <- expand.grid(
  worker_type = WORKER_TYPES,
  spouse_type = SPOUSE_TYPES,
  birth_yr    = BIRTH_YEARS,
  stringsAsFactors = FALSE
)
expected_keys <- with(expected_combos, paste(worker_type, spouse_type, birth_yr, sep = "|"))

assert_all_combos_present <- function(df, name) {
  uniq <- unique(df[c("worker_type", "spouse_type", "birth_yr")])
  uniq_keys <- with(uniq, paste(worker_type, spouse_type, birth_yr, sep = "|"))
  missing <- setdiff(expected_keys, uniq_keys)
  if (length(missing) > 0) {
    stop(sprintf("[%s] missing %d configurations; first: %s",
                 name, length(missing), missing[1]),
         call. = FALSE)
  }
  invisible(NULL)
}

report_missing_combos <- function(df, name) {
  uniq <- unique(df[c("worker_type", "spouse_type", "birth_yr")])
  uniq_keys <- with(uniq, paste(worker_type, spouse_type, birth_yr, sep = "|"))
  missing <- setdiff(expected_keys, uniq_keys)
  missing_years <- if (length(missing) == 0) integer(0)
                   else sort(unique(as.integer(sub(".*\\|", "", missing))))
  if (length(missing) > 0) {
    message(sprintf("[%s] %d configs missing across cohorts %s — emitting empty arrays.",
                    name, length(missing), paste(missing_years, collapse = ", ")))
  }
  missing_years
}

assert_all_combos_present(ben_age,      "benefits_by_worker_age")
assert_all_combos_present(rep_rates,    "initial_replacement_rates")
assert_all_combos_present(ben_age_pb,   "pb_benefits_by_worker_age")
assert_all_combos_present(rep_rates_pb, "pb_initial_replacement_rates")
nmtr_missing_years <- report_missing_combos(nmtr, "net_tax_on_earnings")

# pv (and pv_pb) are de-duplicated under primary/spouse swap; the union of
# direct and swapped keys must cover all 540 configs.
pv_check_coverage <- function(df, name) {
  direct  <- with(df, paste(worker_type, spouse_type, birth_yr, sep = "|"))
  swapped <- with(df, paste(spouse_type, worker_type, birth_yr, sep = "|"))
  uncovered <- setdiff(expected_keys, union(direct, swapped))
  if (length(uncovered) > 0) {
    warning(sprintf("[%s] %d configs uncovered after symmetric fallback; first: %s",
                    name, length(uncovered), uncovered[1]))
  }
  uncovered
}
pv_uncovered    <- pv_check_coverage(pv,    "pv_lifetime_taxes_benefits")
pv_pb_uncovered <- pv_check_coverage(pv_pb, "pb_pv_lifetime_taxes_benefits")
n_pv_have    <- length(expected_keys) - length(pv_uncovered)
n_pv_pb_have <- length(expected_keys) - length(pv_pb_uncovered)

# ---- Build configs map -------------------------------------------------------
# Look up a single (w, s, b) row from a df keyed on (worker_type, spouse_type,
# birth_yr, claim_age). Optionally falls back to the swapped pair (used only
# for the de-duplicated PV files).
lookup_one <- function(df, w, s, b, swap_fallback = FALSE) {
  hit <- df[df$worker_type == w & df$spouse_type == s &
            df$birth_yr    == b & df$claim_age   == CLAIM_AGE, ]
  if (nrow(hit) == 0 && swap_fallback && s != "none" && w != "none") {
    hit <- df[df$worker_type == s & df$spouse_type == w &
              df$birth_yr    == b & df$claim_age   == CLAIM_AGE, ]
  }
  hit
}

# Look up a panel (per-age rows) and order by age.
lookup_panel <- function(df, w, s, b) {
  hit <- df[df$worker_type == w & df$spouse_type == s &
            df$birth_yr    == b & df$claim_age   == CLAIM_AGE, ]
  hit[order(hit$age), ]
}

configs <- list()

for (w in WORKER_TYPES) {
  for (s in SPOUSE_TYPES) {
    for (b in BIRTH_YEARS) {
      key <- paste(w, s, b, sep = "|")

      ba    <- lookup_panel(ben_age,    w, s, b)
      ba_pb <- lookup_panel(ben_age_pb, w, s, b)
      nm    <- lookup_panel(nmtr,       w, s, b)
      stopifnot(nrow(ba_pb) == nrow(ba), all(ba_pb$age == ba$age))

      pvr    <- lookup_one(pv,    w, s, b, swap_fallback = TRUE)
      pvr_pb <- lookup_one(pv_pb, w, s, b, swap_fallback = TRUE)
      stopifnot(nrow(pvr)    %in% c(0, 1))
      stopifnot(nrow(pvr_pb) %in% c(0, 1))
      have_pv    <- nrow(pvr)    == 1
      have_pv_pb <- nrow(pvr_pb) == 1

      rr    <- lookup_one(rep_rates,    w, s, b)
      rr_pb <- lookup_one(rep_rates_pb, w, s, b)
      stopifnot(nrow(rr) == 1, nrow(rr_pb) == 1)

      ben_at_65    <- ba$real_ben[ba$age == 65]
      ben_at_65_pb <- ba_pb$real_ben_pb[ba_pb$age == 65]
      stopifnot(length(ben_at_65) == 1, length(ben_at_65_pb) == 1)

      claim_year <- b + CLAIM_AGE
      le_row     <- le_at_65[le_at_65$claim_year == claim_year, ]
      death_age  <- if (nrow(le_row) == 1) le_row$le_age_at_death else NA_integer_
      nra        <- nra_by_birth_year(b)

      # Household = primary + spouse-as-primary records (both members share
      # birth_yr / claim_age / death_age, so age vectors line up).
      if (s == "none") {
        ba_house_nom     <- ba$nominal_ben
        ba_house_real    <- ba$real_ben
        ba_pb_house_nom  <- ba_pb$nominal_ben_pb
        ba_pb_house_real <- ba_pb$real_ben_pb
        nm_house_earn    <- nm$earnings
      } else {
        ba_swap    <- lookup_panel(ben_age,    s, w, b)
        ba_pb_swap <- lookup_panel(ben_age_pb, s, w, b)
        stopifnot(nrow(ba_swap)    == nrow(ba), all(ba_swap$age    == ba$age))
        stopifnot(nrow(ba_pb_swap) == nrow(ba), all(ba_pb_swap$age == ba$age))
        ba_house_nom     <- ba$nominal_ben     + ba_swap$nominal_ben
        ba_house_real    <- ba$real_ben        + ba_swap$real_ben
        ba_pb_house_nom  <- ba_pb$nominal_ben_pb + ba_pb_swap$nominal_ben_pb
        ba_pb_house_real <- ba_pb$real_ben_pb    + ba_pb_swap$real_ben_pb

        nm_swap <- lookup_panel(nmtr, s, w, b)
        if (nrow(nm) > 0 && nrow(nm_swap) == nrow(nm) && all(nm_swap$age == nm$age)) {
          nm_house_earn <- nm$earnings + nm_swap$earnings
        } else {
          nm_house_earn <- numeric(0)
        }
      }

      configs[[key]] <- list(
        annual = list(
          ages      = ba$age,
          years     = ba$year,
          earnings  = round(ba$earnings, 2),
          scheduled = list(
            nominal           = round(ba$nominal_ben,     2),
            real              = round(ba$real_ben,        2),
            household_nominal = round(ba_house_nom,       2),
            household_real    = round(ba_house_real,      2)
          ),
          payable = list(
            nominal           = round(ba_pb$nominal_ben_pb, 2),
            real              = round(ba_pb$real_ben_pb,    2),
            household_nominal = round(ba_pb_house_nom,      2),
            household_real    = round(ba_pb_house_real,     2)
          )
        ),
        nmtr = list(
          ages                       = nm$age,
          years                      = nm$year,
          values                     = round(nm$net_tax, 6),
          earnings_nominal           = round(nm$earnings, 2),
          earnings_real              = round(nm$earnings * price_factor_by_year[as.character(nm$year)], 2),
          household_earnings_nominal = if (length(nm_house_earn) > 0) round(nm_house_earn, 2) else numeric(0),
          household_earnings_real    = if (length(nm_house_earn) > 0) round(nm_house_earn * price_factor_by_year[as.character(nm$year)], 2) else numeric(0)
        ),
        summary = list(
          death_age = death_age,
          nra       = round(nra, 4),
          pv_taxes  = if (have_pv) round(pvr$pv_taxes, 2) else NA_real_,
          scheduled = list(
            monthly_real_at_65 = round(ben_at_65 / 12, 2),
            pv_benefits        = if (have_pv) round(pvr$pv_benefits,   2) else NA_real_,
            ben_tax_ratio      = if (have_pv) round(pvr$ben_tax_ratio, 4) else NA_real_,
            rep_rate_career    = round(rr$rep_rate_career, 6),
            rep_rate_awi       = round(rr$rep_rate_awi,    6)
          ),
          payable = list(
            monthly_real_at_65 = round(ben_at_65_pb / 12, 2),
            pv_benefits        = if (have_pv_pb) round(pvr_pb$pv_ben_pb,        2) else NA_real_,
            ben_tax_ratio      = if (have_pv_pb) round(pvr_pb$ben_tax_ratio_pb, 4) else NA_real_,
            rep_rate_career    = round(rr_pb$rep_rate_career_pb, 6),
            rep_rate_awi       = round(rr_pb$rep_rate_awi_pb,    6)
          )
        )
      )
    }
  }
}

# ---- Write -------------------------------------------------------------------
out <- list(
  meta = list(
    data_mode                = "current_law_only",
    claim_age                = CLAIM_AGE,
    generated                = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    nmtr_missing_birth_years = as.list(nmtr_missing_years),
    nmtr_values_pending      = NMTR_VALUES_PENDING,
    pb_nmtr_pending          = PB_NMTR_PENDING
  ),
  dimensions = list(
    worker_types = lapply(WORKER_TYPES, function(k) list(key = k, label = unname(WORKER_LABELS[[k]]))),
    spouse_types = lapply(SPOUSE_TYPES, function(k) list(key = k, label = unname(SPOUSE_LABELS[[k]]))),
    birth_years  = BIRTH_YEARS
  ),
  configs = configs
)

dir.create("site/data", recursive = TRUE, showWarnings = FALSE)
write_json(out, "site/data/site_data.json", auto_unbox = TRUE, digits = NA, na = "null")

cat(sprintf("Wrote site/data/site_data.json with %d configs (sched PV %d, pb PV %d, NMTR missing for: %s, pb_nmtr_pending=%s)\n",
            length(configs), n_pv_have, n_pv_pb_have,
            if (length(nmtr_missing_years) == 0) "(none)"
            else paste(nmtr_missing_years, collapse = ", "),
            tolower(as.character(PB_NMTR_PENDING))))
