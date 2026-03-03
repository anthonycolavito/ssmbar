#!/usr/bin/env Rscript
# =============================================================================
# Generate reform data for the Benefit Explorer
# =============================================================================
#
# Produces reform JSON files keyed by combo key (e.g., "reduce_fact3",
# "reduce_fact3+nra_to_68+chained_cpi"). Baseline data stays in current-law
# files. This script generates ONLY the reform deltas.
#
# Sex: "all" (unisex) only. Marital: single only. No spouse support.
#
# Skip logic:
#   PIA reforms (effective_year=2030, by eligibility cohort): skip 1940, 1950, 1960
#   NRA reforms (effective_year=2026, by eligibility cohort): skip 1940, 1950, 1960
#   COLA reforms (effective_year=2026, by calendar year): skip 1940 (dead before 2026)
#   For combos: skip birth years where NONE of the active reforms have any effect.
#
# Writes:
#   docs/data/reform/cohort/{type}.json      — summary metrics by combo key
#   docs/data/reform/individual/{type}.json   — by-age benefit series by combo key
#
# Usage:
#   Rscript scripts/generate_reform_data.R --categories pia
#   Rscript scripts/generate_reform_data.R --categories pia,nra
#   Rscript scripts/generate_reform_data.R --categories pia,nra,cola
#   Rscript scripts/generate_reform_data.R --categories pia --type medium
#   Rscript scripts/generate_reform_data.R --categories pia,nra,cola --cores 4
#
# =============================================================================

suppressMessages({
  library(dplyr)
  library(jsonlite)
  devtools::load_all(".", quiet = TRUE)
})

# =============================================================================
# CLI args
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
filter_type <- NULL
n_cores <- 6L
categories_arg <- NULL

for (i in seq_along(args)) {
  if (args[i] == "--type" && i < length(args)) filter_type <- args[i + 1]
  if (args[i] == "--cores" && i < length(args)) n_cores <- as.integer(args[i + 1])
  if (args[i] == "--categories" && i < length(args)) categories_arg <- args[i + 1]
}

if (is.null(categories_arg)) {
  stop("--categories is required. Example: --categories pia,nra,cola")
}

active_categories <- trimws(strsplit(categories_arg, ",")[[1]])
valid_cats <- c("pia", "nra", "cola")
bad <- setdiff(active_categories, valid_cats)
if (length(bad) > 0) stop("Unknown categories: ", paste(bad, collapse = ", "))

cat(sprintf("Active categories: %s\n", paste(active_categories, collapse = ", ")))

# =============================================================================
# Output directories
# =============================================================================

out_dir <- "docs/data/reform"
dir.create(file.path(out_dir, "cohort"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "individual"), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Reform definitions by category
# =============================================================================

reform_defs <- list(
  pia = list(
    effective_year = 2030L,
    phase_type = "cohort",
    reforms = list(
      reduce_fact3      = function() reform_reduce_fact3(0.05, 2030),
      flat_benefit      = function() reform_flat_benefit(effective_year = 2030),
      simpson_bowles_pia = function() reform_simpson_bowles(effective_year = 2030)
    )
  ),
  nra = list(
    effective_year = 2026L,
    phase_type = "cohort",
    reforms = list(
      nra_to_68     = function() reform_nra_to_68(2026),
      index_nra     = function() reform_index_nra(2026),
      nra_to_69_index = function() reform_nra_to_69_index(2026)
    )
  ),
  cola = list(
    effective_year = 2026L,
    phase_type = "calendar",
    reforms = list(
      chained_cpi = function() reform_chained_cpi(2026),
      cola_cap    = function() reform_cola_cap(2026),
      cpi_e       = function() reform_cpi_e(2026)
    )
  )
)

# =============================================================================
# Worker type configurations
# =============================================================================

worker_configs <- list(
  list(key = "very_low", label = "Very Low Earner", type = "very_low", custom_earnings = NULL),
  list(key = "low", label = "Low Earner", type = "low", custom_earnings = NULL),
  list(key = "medium", label = "Medium Earner", type = "medium", custom_earnings = NULL),
  list(key = "high", label = "High Earner", type = "high", custom_earnings = NULL),
  list(key = "max", label = "Maximum Earner", type = "max", custom_earnings = NULL),
  list(key = "custom_50k", label = "$50K Earner", type = "custom", custom_earnings = 50000)
)

if (!is.null(filter_type)) {
  worker_configs <- Filter(function(w) w$key == filter_type, worker_configs)
  if (length(worker_configs) == 0) stop("Unknown worker type: ", filter_type)
}

# =============================================================================
# Dimensions
# =============================================================================

all_birth_years <- c(1940L, 1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
claim_age <- 65L
gdp_pi_2025 <- tr2025$gdp_pi[tr2025$year == 2025]

# =============================================================================
# Build combo grid: expand.grid on active categories, exclude all-"none"
# =============================================================================

# Each category has "none" + its reform names
cat_options <- list()
for (cat in active_categories) {
  cat_options[[cat]] <- c("none", names(reform_defs[[cat]]$reforms))
}

combo_grid <- do.call(expand.grid, c(cat_options, stringsAsFactors = FALSE))
# Remove the all-"none" baseline row
all_none <- apply(combo_grid, 1, function(row) all(row == "none"))
combo_grid <- combo_grid[!all_none, , drop = FALSE]

# Build combo keys (non-"none" names joined with "+", in category order)
combo_keys <- apply(combo_grid, 1, function(row) {
  parts <- row[row != "none"]
  paste(parts, collapse = "+")
})

cat(sprintf("Combo keys: %d\n", length(combo_keys)))
for (ck in combo_keys) cat(sprintf("  %s\n", ck))

# =============================================================================
# Determine which birth years to compute for each combo
# =============================================================================

# For each combo, check which categories have an active (non-"none") reform
# Birth year 1940: skip for all (PIA/NRA: eligibility before effective_year; COLA: dead before 2026)
# Birth years 1950, 1960: skip if no COLA reform is active (PIA/NRA don't affect these)
# Birth years 1970+: always compute

get_birth_years_for_combo <- function(combo_row) {
  has_cola <- FALSE
  for (cat in active_categories) {
    if (cat == "cola" && combo_row[[cat]] != "none") has_cola <- TRUE
  }

  if (has_cola) {
    # COLA affects by calendar year — skip only 1940 (dead before 2026)
    all_birth_years[all_birth_years >= 1950L]
  } else {
    # PIA/NRA only — phase in by eligibility cohort
    # PIA effective_year=2030: first affected turns 62 in 2030, born 1968
    # NRA effective_year=2026: first affected turns 62 in 2026, born 1964
    # Our decadal BYs: 1970 is the first affected
    all_birth_years[all_birth_years >= 1970L]
  }
}

# =============================================================================
# Build reform objects for a given combo row
# =============================================================================

build_reforms <- function(combo_row) {
  reform_list <- list()
  for (cat in active_categories) {
    rname <- combo_row[[cat]]
    if (rname != "none") {
      reform_list[[length(reform_list) + 1]] <- reform_defs[[cat]]$reforms[[rname]]()
    }
  }
  reform_list
}

# =============================================================================
# Compute one config: combo × worker × birth year
# =============================================================================

compute_reform_config <- function(wtype, custom_earn, by, reformed_assumptions) {
  if (!is.null(custom_earn)) {
    worker <- calculate_benefits_reform(
      birth_yr = by, sex = "all", type = "custom",
      age_claim = claim_age, factors = sef2025, assumptions = reformed_assumptions,
      custom_avg_earnings = custom_earn, debugg = TRUE
    )
  } else {
    worker <- calculate_benefits_reform(
      birth_yr = by, sex = "all", type = wtype,
      age_claim = claim_age, factors = sef2025, assumptions = reformed_assumptions,
      debugg = TRUE
    )
  }
  worker
}

# =============================================================================
# Extract metrics (same logic as generate_currentlaw_data.R)
# =============================================================================

extract_metrics <- function(worker, by, assumptions) {
  claim_year <- by + claim_age

  mb <- worker$ben[worker$age == claim_age]
  mb <- if (length(mb) > 0) mb[1] else NA
  da <- worker$death_age[1]

  pvb <- tryCatch(pv_lifetime_benefits(worker, assumptions)$pv_benefits[1], error = function(e) NA)
  pvt <- tryCatch(pv_lifetime_taxes(worker, assumptions, include_employer = TRUE)$pv_taxes[1], error = function(e) NA)
  rat <- if (!is.na(pvb) && !is.na(pvt) && pvt > 0) pvb / pvt else NA

  irr_val <- tryCatch(internal_rate_of_return(worker, assumptions, include_employer = TRUE)$irr[1],
                       error = function(e) NA)

  rr <- tryCatch(rep_rates(worker, assumptions), error = function(e) NULL)
  rr_val <- NA
  if (!is.null(rr) && is.data.frame(rr)) {
    rr_row <- rr$rep_rate[rr$type == "real_all"]
    if (length(rr_row) > 0) rr_val <- rr_row[1]
  }

  gdp_pi_claim <- assumptions$gdp_pi[assumptions$year == claim_year]
  init_real <- NA
  if (length(gdp_pi_claim) > 0 && !is.na(gdp_pi_claim) && gdp_pi_claim > 0 && !is.na(mb)) {
    init_real <- mb * gdp_pi_2025 / gdp_pi_claim
  }

  list(
    monthly_benefit = mb, pv_benefits = pvb, pv_taxes = pvt,
    ratio = rat, irr = irr_val, repl_rate = rr_val,
    initial_real_benefit = init_real, death_age = da
  )
}

# =============================================================================
# Extract benefit series (same logic as generate_currentlaw_data.R)
# =============================================================================

extract_benefit_series <- function(worker, by, assumptions) {
  claim_year <- by + claim_age
  da <- worker$death_age[1]
  max_age <- if (!is.na(da)) floor(da) else 120

  rows <- worker[worker$age >= claim_age & worker$age <= max_age & !is.na(worker$annual_ind), ]
  if (nrow(rows) == 0) return(NULL)

  cpi_c <- assumptions$gdp_pi[assumptions$year == claim_year]
  if (length(cpi_c) > 0 && !is.na(cpi_c) && cpi_c > 0) {
    real_b <- sapply(seq_len(nrow(rows)), function(j) {
      cpi_y <- assumptions$gdp_pi[assumptions$year == rows$year[j]]
      if (length(cpi_y) > 0 && !is.na(cpi_y))
        round(rows$annual_ind[j] * cpi_c / cpi_y, 0)
      else rows$annual_ind[j]
    })
  } else {
    real_b <- rows$annual_ind
  }

  list(
    ages = as.integer(rows$age),
    nominal = round(rows$annual_ind, 0),
    real = round(real_b, 0)
  )
}

# =============================================================================
# Process one worker type: all combos × all applicable birth years
# =============================================================================

process_worker_type <- function(wconfig) {
  wkey <- wconfig$key
  wlabel <- wconfig$label
  wtype <- wconfig$type
  custom_earn <- wconfig$custom_earnings

  t0 <- Sys.time()
  cat(sprintf("[%s] Starting %s (%s)...\n", format(t0, "%H:%M:%S"), wkey, wlabel))

  # Output structures keyed by combo key
  coh_data <- list()   # combo_key -> cohort metrics
  ind_data <- list()   # combo_key -> {birth_year -> benefit series}

  total_combos <- nrow(combo_grid)
  for (ci in seq_len(total_combos)) {
    combo_row <- combo_grid[ci, , drop = FALSE]
    combo_key <- combo_keys[ci]
    birth_years <- get_birth_years_for_combo(combo_row)
    n_by <- length(birth_years)

    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("[%s] %s: combo %d/%d '%s' (%d BYs) %.1f min\n",
        format(Sys.time(), "%H:%M:%S"), wkey, ci, total_combos, combo_key, n_by, elapsed))

    # Build reform objects and apply to assumptions ONCE per combo
    reform_list <- build_reforms(combo_row)
    reformed_assumptions <- apply_reforms(tr2025, reform_list, check_exclusivity = FALSE)

    # Arrays for cohort metrics
    mb_arr <- numeric(n_by)
    pvb_arr <- numeric(n_by)
    pvt_arr <- numeric(n_by)
    rat_arr <- numeric(n_by)
    irr_arr <- numeric(n_by)
    rr_arr <- numeric(n_by)
    init_real_arr <- numeric(n_by)
    death_arr <- numeric(n_by)

    ben_by_year <- list()

    for (bi in seq_along(birth_years)) {
      by <- birth_years[bi]

      worker <- tryCatch(
        compute_reform_config(wtype, custom_earn, by, reformed_assumptions),
        error = function(e) {
          cat(sprintf("  ERROR: %s %s by=%d: %s\n", wkey, combo_key, by, e$message))
          NULL
        }
      )

      if (is.null(worker)) {
        mb_arr[bi] <- NA; pvb_arr[bi] <- NA; pvt_arr[bi] <- NA
        rat_arr[bi] <- NA; irr_arr[bi] <- NA; rr_arr[bi] <- NA
        init_real_arr[bi] <- NA; death_arr[bi] <- NA
        next
      }

      metrics <- extract_metrics(worker, by, reformed_assumptions)
      mb_arr[bi] <- metrics$monthly_benefit
      pvb_arr[bi] <- metrics$pv_benefits
      pvt_arr[bi] <- metrics$pv_taxes
      rat_arr[bi] <- metrics$ratio
      irr_arr[bi] <- metrics$irr
      rr_arr[bi] <- metrics$repl_rate
      init_real_arr[bi] <- metrics$initial_real_benefit
      death_arr[bi] <- metrics$death_age

      bseries <- extract_benefit_series(worker, by, reformed_assumptions)
      if (!is.null(bseries)) ben_by_year[[as.character(by)]] <- bseries
    }

    coh_data[[combo_key]] <- list(
      birth_years = as.integer(birth_years),
      monthly_benefit = round(mb_arr, 2),
      pv_benefits = round(pvb_arr, 0),
      pv_taxes = round(pvt_arr, 0),
      ratio = round(rat_arr, 4),
      irr = round(irr_arr, 6),
      repl_rate = round(rr_arr, 6),
      initial_real_benefit = round(init_real_arr, 2),
      death_age = round(death_arr, 1)
    )

    ind_data[[combo_key]] <- ben_by_year
  }

  # Write cohort file
  coh_out <- list(
    meta = list(
      worker_type = wkey,
      worker_label = wlabel,
      claim_age = claim_age,
      sex = "all",
      marital = "single",
      categories = active_categories,
      combo_keys = combo_keys
    ),
    data = coh_data
  )
  coh_file <- file.path(out_dir, "cohort", sprintf("%s.json", wkey))
  write(toJSON(coh_out, auto_unbox = TRUE, digits = 6), coh_file)
  cat(sprintf("  Wrote %s (%.1f KB)\n", coh_file, file.info(coh_file)$size / 1024))

  # Write individual file
  ind_out <- list(
    meta = list(worker_type = wkey, claim_age = claim_age, sex = "all"),
    data = ind_data
  )
  ind_file <- file.path(out_dir, "individual", sprintf("%s.json", wkey))
  write(toJSON(ind_out, auto_unbox = TRUE, digits = 0), ind_file)
  cat(sprintf("  Wrote %s (%.1f KB)\n", ind_file, file.info(ind_file)$size / 1024))

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("[%s] %s COMPLETE in %.1f minutes\n", format(Sys.time(), "%H:%M:%S"), wkey, elapsed))
  return(sprintf("%s: %.1f min", wkey, elapsed))
}

# =============================================================================
# Run
# =============================================================================

cat(sprintf("\n=== Reform Data Generation ===\n"))
cat(sprintf("Started at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Categories: %s\n", paste(active_categories, collapse = ", ")))
cat(sprintf("Combo keys: %d\n", length(combo_keys)))
cat(sprintf("Worker types: %d\n", length(worker_configs)))
cat(sprintf("Cores: %d\n\n", n_cores))

if (length(worker_configs) == 1 || n_cores == 1) {
  results <- lapply(worker_configs, process_worker_type)
} else {
  results <- parallel::mclapply(worker_configs, process_worker_type, mc.cores = n_cores)
}

cat("\n=== SUMMARY ===\n")
for (r in results) cat(" ", r, "\n")
cat(sprintf("Finished at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
