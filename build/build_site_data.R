# build_site_data.R
#
# Reshapes the four output CSVs into a single nested JSON file consumed by
# the static dashboard at site/. Run by the GitHub Action on every push.
#
# Inputs:  output/benefits_by_worker_age.csv
#          output/net_tax_on_earnings.csv
#          output/pv_lifetime_taxes_benefits.csv
#          output/initial_replacement_rates.csv
# Output:  site/data/site_data.json

suppressPackageStartupMessages({
  library(jsonlite)
})

WORKER_TYPES <- c("very_low", "low", "medium", "high", "max")
SPOUSE_TYPES <- c("none", "very_low", "low", "medium", "high", "max")
BIRTH_YEARS  <- seq(1940, 2010, by = 5)
CLAIM_AGE    <- 65L

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

ben_age   <- read_csv_strict("output/benefits_by_worker_age.csv")
nmtr      <- read_csv_strict("output/net_tax_on_earnings.csv")
pv        <- read_csv_strict("output/pv_lifetime_taxes_benefits.csv")
rep_rates <- read_csv_strict("output/initial_replacement_rates.csv")

# Period unisex life expectancy at age 65 by claim year, derived from tr2025.
# tr2025$le_m and le_f are the expected age at death given alive at 65, so the
# correct formula is the simple average — not (65 + (le_m + le_f) / 2), which is
# what the package's create_*_data.R scripts emit and produces values like 149+.
load("data/tr2025.rda")
BASE_YEAR <- 2026L
le_at_65 <- data.frame(
  claim_year      = tr2025$year,
  le_age_at_death = round((tr2025$le_m + tr2025$le_f) / 2)
)

# Price factor for converting nominal $year to real $2026 (GDP price index).
# Matches the convention in R/create_annual_ben_data.R that produced annual$real.
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

# ben_age, nmtr, and rep_rates are required to have every (worker, spouse, birth_yr).
# pv is de-duplicated: for two non-zero earners, the household's PV is symmetric
# under swapping primary/spouse, so the package stores only the canonical ordering.
# When the direct (worker, spouse) lookup is missing, we fall back to (spouse, worker).
# After the fallback, the union should cover all 450 configs.
assert_all_combos_present(ben_age,   "benefits_by_worker_age")
assert_all_combos_present(nmtr,      "net_tax_on_earnings")
assert_all_combos_present(rep_rates, "initial_replacement_rates")

pv_keys_direct  <- with(pv, paste(worker_type, spouse_type, birth_yr, sep = "|"))
pv_keys_swapped <- with(pv, paste(spouse_type, worker_type, birth_yr, sep = "|"))
pv_keys_all     <- union(pv_keys_direct, pv_keys_swapped)
pv_uncovered    <- setdiff(expected_keys, pv_keys_all)
n_pv_have       <- length(expected_keys) - length(pv_uncovered)

# ---- Build configs map -------------------------------------------------------
configs <- list()

for (w in WORKER_TYPES) {
  for (s in SPOUSE_TYPES) {
    for (b in BIRTH_YEARS) {
      key <- paste(w, s, b, sep = "|")

      ba <- ben_age[ben_age$worker_type == w &
                    ben_age$spouse_type == s &
                    ben_age$birth_yr    == b &
                    ben_age$claim_age   == CLAIM_AGE, ]
      ba <- ba[order(ba$age), ]

      nm <- nmtr[nmtr$worker_type == w &
                 nmtr$spouse_type == s &
                 nmtr$birth_yr    == b &
                 nmtr$claim_age   == CLAIM_AGE, ]
      nm <- nm[order(nm$age), ]

      pvr <- pv[pv$worker_type == w &
                pv$spouse_type == s &
                pv$birth_yr    == b &
                pv$claim_age   == CLAIM_AGE, ]
      if (nrow(pvr) == 0 && s != "none" && w != "none") {
        # Fall back to the swapped pair — household PV is symmetric.
        pvr <- pv[pv$worker_type == s &
                  pv$spouse_type == w &
                  pv$birth_yr    == b &
                  pv$claim_age   == CLAIM_AGE, ]
      }
      stopifnot(nrow(pvr) %in% c(0, 1))
      have_pv <- nrow(pvr) == 1

      rr <- rep_rates[rep_rates$worker_type == w &
                      rep_rates$spouse_type == s &
                      rep_rates$birth_yr    == b &
                      rep_rates$claim_age   == CLAIM_AGE, ]
      stopifnot(nrow(rr) == 1)

      ben_at_65 <- ba$real_ben[ba$age == 65]
      stopifnot(length(ben_at_65) == 1)

      claim_year <- b + CLAIM_AGE
      le_row     <- le_at_65[le_at_65$claim_year == claim_year, ]
      death_age  <- if (nrow(le_row) == 1) le_row$le_age_at_death else NA_integer_

      configs[[key]] <- list(
        annual = list(
          ages     = ba$age,
          years    = ba$year,
          nominal  = round(ba$nominal_ben, 2),
          real     = round(ba$real_ben,    2),
          earnings = round(ba$earnings,    2)
        ),
        nmtr = list(
          ages              = nm$age,
          values            = round(nm$net_tax, 6),
          earnings_nominal  = round(nm$earnings, 2),
          earnings_real     = round(nm$earnings * price_factor_by_year[as.character(nm$year)], 2)
        ),
        summary = list(
          monthly_real_at_65 = round(ben_at_65 / 12, 2),
          death_age          = death_age,
          pv_benefits        = if (have_pv) round(pvr$pv_benefits,    2) else NA_real_,
          pv_taxes           = if (have_pv) round(pvr$pv_taxes,       2) else NA_real_,
          ben_tax_ratio      = if (have_pv) round(pvr$ben_tax_ratio,  4) else NA_real_,
          rep_rate_career    = round(rr$rep_rate_career, 6),
          rep_rate_awi       = round(rr$rep_rate_awi,    6)
        )
      )
    }
  }
}

# ---- Write -------------------------------------------------------------------
out <- list(
  meta = list(
    data_mode = "current_law_only",
    claim_age = CLAIM_AGE,
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
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

cat(sprintf("Wrote site/data/site_data.json with %d configs (%d with PV, %d PV-missing)\n",
            length(configs), n_pv_have, length(pv_uncovered)))
if (length(pv_uncovered) > 0) {
  warning(sprintf("PV remains missing for %d configs after symmetric fallback; first: %s",
                  length(pv_uncovered), pv_uncovered[1]))
}
