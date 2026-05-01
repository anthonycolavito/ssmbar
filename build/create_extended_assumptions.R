# build/create_extended_assumptions.R
# -----------------------------------------------------------------------------
# Builds a CUSTOM Trustees-Report assumptions dataset that extends out to
# year `TARGET_YEAR` (default 2300). This is *only* for the Constant Earnings
# Comparisons tab so it can show birth cohorts beyond the standard tr2025
# horizon (which ends at 2150).
#
# The existing R/assumptions_prep.R prepares tr2025 from raw and internally
# extends to 2150 using fixed terminal growth rates and SSA indexing rules.
# This script:
#   1. Calls prep_assumptions() to get the standard 1951-2150 dataset.
#   2. Appends rows from 2151 to TARGET_YEAR using the same growth rates
#      (AWI 3.55%, GDP-PI 2.05%, CPI-W 2.4%, COLA 2.4%) and the same SSA
#      indexing rules for taxmax / bend points / QC threshold / RET amounts /
#      old-law base / special-minimum rate.
#   3. Saves to data/tr2025_extended.rda (without touching tr2025.rda).
#
# Discount factors `df` and `real_df` are copied forward from the terminal
# year, matching the behaviour of prep_assumptions' own 2101-2150 extension.
# This preserves methodological consistency with the rest of the pipeline.
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
})

source("./R/assumptions_prep.R")

TARGET_YEAR <- 2300L

# ---- 1. Standard prep ------------------------------------------------------
tr_raw   <- read.csv("./data-raw/2025TR_assumptions.csv")
tr2025_extended <- prep_assumptions(tr_raw)

# ---- 2. Append rows beyond 2150 -------------------------------------------
last_yr <- max(tr2025_extended$year)
if (last_yr < TARGET_YEAR) {
  ext_yrs  <- (last_yr + 1):TARGET_YEAR
  n_ext    <- length(ext_yrs)
  last_row <- tr2025_extended[tr2025_extended$year == last_yr, ]

  # Same extrapolation rates used inside prep_assumptions for the
  # 2101-2150 extension.
  extension <- last_row[rep(1, n_ext), ]
  extension$year   <- ext_yrs
  extension$awi    <- last_row$awi    * 1.0355^(ext_yrs - last_yr)
  extension$gdp_pi <- last_row$gdp_pi * 1.0205^(ext_yrs - last_yr)
  extension$cpi_w  <- last_row$cpi_w  * 1.024 ^(ext_yrs - last_yr)
  extension$cola   <- 0.024
  extension$payable <- last_row$payable  # held at terminal (~0.7143)

  # Derived parameters wiped so the indexing loop below fills them in.
  extension$taxmax           <- NA_real_
  extension$bp1              <- NA_real_
  extension$bp2              <- NA_real_
  extension$qc_rec           <- NA_real_
  extension$ret1             <- NA_real_
  extension$ret2             <- NA_real_
  extension$old_law_base     <- NA_real_
  extension$special_min_rate <- NA_real_

  tr2025_extended <- rbind(tr2025_extended, extension)

  # ---- 3. Project derived parameters via SSA indexing rules ---------------
  # AWI bases used for indexing (same as prep_assumptions).
  awi_1976 <- tr2025_extended[tr2025_extended$year == 1976, "awi"]
  awi_1977 <- tr2025_extended[tr2025_extended$year == 1977, "awi"]
  awi_1992 <- tr2025_extended[tr2025_extended$year == 1992, "awi"]
  awi_2000 <- tr2025_extended[tr2025_extended$year == 2000, "awi"]

  qc_base           <- tr2025_extended[tr2025_extended$year == 1978, "qc_rec"]
  bp1_base          <- tr2025_extended[tr2025_extended$year == 1979, "bp1"]
  bp2_base          <- tr2025_extended[tr2025_extended$year == 1979, "bp2"]
  taxmax_base       <- tr2025_extended[tr2025_extended$year == 1994, "taxmax"]
  ret1_base         <- tr2025_extended[tr2025_extended$year == 1994, "ret1"]
  ret2_base         <- tr2025_extended[tr2025_extended$year == 2002, "ret2"]
  old_law_base_base <- tr2025_extended[tr2025_extended$year == 1994, "old_law_base"]

  for (i in (last_yr + 1):TARGET_YEAR) {
    awi_end <- tr2025_extended[tr2025_extended$year == i - 2, "awi"]

    if (is.na(tr2025_extended[tr2025_extended$year == i, "taxmax"])) {
      tr2025_extended[tr2025_extended$year == i, "taxmax"] <-
        round((taxmax_base * awi_end / awi_1992) / 300) * 300
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "bp1"])) {
      tr2025_extended[tr2025_extended$year == i, "bp1"] <- round(bp1_base * awi_end / awi_1977)
      tr2025_extended[tr2025_extended$year == i, "bp2"] <- round(bp2_base * awi_end / awi_1977)
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "qc_rec"])) {
      prev <- tr2025_extended[tr2025_extended$year == i - 1, "qc_rec"]
      tr2025_extended[tr2025_extended$year == i, "qc_rec"] <-
        max(round(qc_base * awi_end / awi_1976 / 10) * 10, prev)
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "ret1"])) {
      prev <- tr2025_extended[tr2025_extended$year == i - 1, "ret1"]
      tr2025_extended[tr2025_extended$year == i, "ret1"] <-
        max(round(ret1_base * awi_end / awi_1992 / 120) * 120, prev)
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "ret2"])) {
      prev <- tr2025_extended[tr2025_extended$year == i - 1, "ret2"]
      tr2025_extended[tr2025_extended$year == i, "ret2"] <-
        max(round(ret2_base * awi_end / awi_2000 / 480) * 480, prev)
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "old_law_base"])) {
      tr2025_extended[tr2025_extended$year == i, "old_law_base"] <-
        round((old_law_base_base * awi_end / awi_1992) / 300) * 300
    }
    if (is.na(tr2025_extended[tr2025_extended$year == i, "special_min_rate"])) {
      prev   <- tr2025_extended$special_min_rate[tr2025_extended$year == i - 1]
      cola_i <- tr2025_extended$cola[tr2025_extended$year == i - 1]
      tr2025_extended[tr2025_extended$year == i, "special_min_rate"] <-
        floor(prev * (1 + cola_i) * 10) / 10
    }
  }

  # yoc_threshold depends on old_law_base, which we just filled in for the
  # extension years — recompute the column in full to pick those up.
  tr2025_extended$yoc_threshold <- ifelse(tr2025_extended$year < 1991,
                                          tr2025_extended$old_law_base * 0.25,
                                          tr2025_extended$old_law_base * 0.15)
}

# ---- 4. Save (separate file — does NOT replace data/tr2025.rda) -----------
save(tr2025_extended,
     file = "./data/tr2025_extended.rda",
     compress = "xz")

cat(sprintf("Saved tr2025_extended (year range %d–%d, %d rows) to data/tr2025_extended.rda\n",
            min(tr2025_extended$year), max(tr2025_extended$year),
            nrow(tr2025_extended)))
