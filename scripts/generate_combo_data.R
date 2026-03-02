#!/usr/bin/env Rscript
# =============================================================================
# Generate pre-computed data for all 768 reform combinations
# =============================================================================
#
# Writes:
#   docs/data/cohort/{type}_65.json           — summary metrics per combo per birth year
#   docs/data/individual/{type}_65_benefits.json — by-age benefit series
#
# Usage:
#   Rscript scripts/generate_combo_data.R
#   Rscript scripts/generate_combo_data.R --cores 6
#   Rscript scripts/generate_combo_data.R --type medium   # single worker type
#
# =============================================================================

suppressWarnings(devtools::load_all(".", quiet = TRUE))
library(jsonlite)
library(parallel)

# Parse command line args
args <- commandArgs(trailingOnly = TRUE)
ncores <- 6L
filter_type <- NULL
for (i in seq_along(args)) {
  if (args[i] == "--cores" && i < length(args)) ncores <- as.integer(args[i + 1])
  if (args[i] == "--type" && i < length(args)) filter_type <- args[i + 1]
}
ncores <- min(ncores, detectCores() - 2L)
cat("Using", ncores, "cores\n")

data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

out_dir <- "docs/data"
dir.create(file.path(out_dir, "cohort"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "individual"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# Reform categories and combo generation
# =============================================================================

reform_factories <- list(
  reduce_fact3 = function() reform_reduce_fact3(0.05, 2030),
  flat_benefit = function() reform_flat_benefit(19300/12, 2030, 25),
  simpson_bowles_pia = function() reform_simpson_bowles(2030, 10),
  nra_to_68 = function() reform_nra_to_68(2030),
  index_nra = function() reform_index_nra(2030),
  nra_to_69_index = function() reform_nra_to_69_index(2030),
  chained_cpi = function() reform_chained_cpi(2030),
  cola_cap = function() reform_cola_cap(2030),
  cpi_e = function() reform_cpi_e(2030),
  taxmax_90_pct = function() reform_taxmax_90_pct(2030),
  eliminate_taxmax = function() reform_eliminate_taxmax(2030),
  eliminate_taxmax_no_credit = function() reform_eliminate_taxmax_no_credit(2030),
  forty_year_avg = function() reform_40_year_averaging(2030),
  mini_pia = function() reform_mini_pia(2030, 10)
)

categories <- list(
  pia = c("none", "reduce_fact3", "flat_benefit", "simpson_bowles_pia"),
  nra = c("none", "nra_to_68", "index_nra", "nra_to_69_index"),
  cola = c("none", "chained_cpi", "cola_cap", "cpi_e"),
  taxmax = c("none", "taxmax_90_pct", "eliminate_taxmax", "eliminate_taxmax_no_credit"),
  other = c("none", "forty_year_avg", "mini_pia")
)

combos <- expand.grid(categories, stringsAsFactors = FALSE)

make_combo_key <- function(row) {
  active <- as.character(row[row != "none"])
  if (length(active) == 0) return("baseline")
  paste(active, collapse = "+")
}
combo_keys <- apply(combos, 1, make_combo_key)

cat(nrow(combos), "reform combinations generated.\n")

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
# Birth years and deflation base
# =============================================================================

birth_years <- c(1940L, 1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
gdp_pi_2025 <- tr2025$gdp_pi[tr2025$year == 2025]
cat("GDP PI 2025:", gdp_pi_2025, "\n")

# =============================================================================
# Main processing function (one worker type)
# =============================================================================

process_worker_type <- function(wconfig) {
  wkey <- wconfig$key
  wlabel <- wconfig$label
  wtype <- wconfig$type
  custom_earn <- wconfig$custom_earnings

  t0 <- Sys.time()
  cat(sprintf("[%s] Starting %s (%s)...\n", format(t0, "%H:%M:%S"), wkey, wlabel))

  # Output files
  coh_file <- file.path(out_dir, "cohort", sprintf("%s_65.json", wkey))
  ben_file <- file.path(out_dir, "individual", sprintf("%s_65_benefits.json", wkey))

  # Check if already complete (incremental)
  if (file.exists(coh_file) && file.exists(ben_file)) {
    existing <- tryCatch(fromJSON(coh_file, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.null(existing) && length(names(existing$data)) >= nrow(combos)) {
      cat(sprintf("[%s] %s already complete (%d combos), skipping.\n",
          format(Sys.time(), "%H:%M:%S"), wkey, length(names(existing$data))))
      return(sprintf("%s: skipped (already complete)", wkey))
    }
  }

  coh_data <- list()
  ben_data <- list()
  n_combos <- nrow(combos)

  for (ci in seq_len(n_combos)) {
    combo <- combos[ci, ]
    combo_key <- combo_keys[ci]

    # Progress reporting
    if (ci %% 100 == 0 || ci == 1) {
      elapsed_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      rate <- if (ci > 1) elapsed_min / (ci - 1) else 0
      eta <- rate * (n_combos - ci)
      cat(sprintf("[%s] %s: combo %d/%d (%.1f min elapsed, ~%.0f min remaining) %s\n",
          format(Sys.time(), "%H:%M:%S"), wkey, ci, n_combos, elapsed_min, eta, combo_key))
    }

    # Build assumptions for this combo
    active_reforms <- as.character(unlist(combo[combo != "none"]))
    if (length(active_reforms) == 0) {
      assum <- tr2025
      is_baseline <- TRUE
    } else {
      reform_list <- lapply(active_reforms, function(r) reform_factories[[r]]())
      assum <- apply_reforms(tr2025, reform_list, check_exclusivity = FALSE)
      is_baseline <- FALSE
    }

    # Initialize arrays for cohort metrics
    mb <- numeric(length(birth_years))
    pvb <- numeric(length(birth_years))
    pvt <- numeric(length(birth_years))
    rat <- numeric(length(birth_years))
    irr_v <- numeric(length(birth_years))
    rr_real_all <- numeric(length(birth_years))
    init_real_ben <- numeric(length(birth_years))
    death_ages <- numeric(length(birth_years))

    # Benefits by birth year
    ben_by_year <- list()

    for (bi in seq_along(birth_years)) {
      by <- birth_years[bi]

      ben <- tryCatch({
        if (is_baseline) {
          if (!is.null(custom_earn)) {
            calculate_benefits(birth_yr = by, sex = "all", type = "custom",
              age_claim = 65L, factors = sef2025, assumptions = tr2025,
              custom_avg_earnings = custom_earn, debugg = TRUE)
          } else {
            calculate_benefits(birth_yr = by, sex = "all", type = wtype,
              age_claim = 65L, factors = sef2025, assumptions = tr2025, debugg = TRUE)
          }
        } else {
          if (!is.null(custom_earn)) {
            calculate_benefits_reform(birth_yr = by, sex = "all", type = "custom",
              age_claim = 65L, factors = sef2025, assumptions = assum,
              custom_avg_earnings = custom_earn, debugg = TRUE)
          } else {
            calculate_benefits_reform(birth_yr = by, sex = "all", type = wtype,
              age_claim = 65L, factors = sef2025, assumptions = assum, debugg = TRUE)
          }
        }
      }, error = function(e) {
        cat(sprintf("  ERROR: %s by=%d combo=%s: %s\n", wkey, by, combo_key, e$message))
        NULL
      })

      if (is.null(ben)) {
        mb[bi] <- NA; pvb[bi] <- NA; pvt[bi] <- NA
        rat[bi] <- NA; irr_v[bi] <- NA
        rr_real_all[bi] <- NA; init_real_ben[bi] <- NA
        death_ages[bi] <- NA
        next
      }

      # Monthly benefit at claim age
      b <- ben$ben[ben$age == 65]
      mb[bi] <- if (length(b) > 0) b[1] else NA

      # Death age
      death_ages[bi] <- ben$death_age[1]

      # PV calculations
      pb <- tryCatch(pv_lifetime_benefits(ben, assum)$pv_benefits[1], error = function(e) NA)
      pt <- tryCatch(pv_lifetime_taxes(ben, assum, include_employer = TRUE)$pv_taxes[1], error = function(e) NA)
      pvb[bi] <- pb; pvt[bi] <- pt
      rat[bi] <- if (!is.na(pb) && !is.na(pt) && pt > 0) pb / pt else NA

      # IRR
      irr_v[bi] <- tryCatch(internal_rate_of_return(ben, assum, include_employer = TRUE)$irr[1],
                             error = function(e) NA)

      # Replacement rate (real_all only)
      rr <- tryCatch(rep_rates(ben, assum), error = function(e) NULL)
      if (!is.null(rr) && is.data.frame(rr)) {
        rr_val <- rr$rep_rate[rr$type == "real_all"]
        rr_real_all[bi] <- if (length(rr_val) > 0) rr_val[1] else NA
      } else {
        rr_real_all[bi] <- NA
      }

      # Initial real benefit (deflated to 2025$)
      claim_year <- by + 65L
      gdp_pi_claim <- assum$gdp_pi[assum$year == claim_year]
      if (length(gdp_pi_claim) > 0 && !is.na(gdp_pi_claim) && gdp_pi_claim > 0 && !is.na(mb[bi])) {
        init_real_ben[bi] <- mb[bi] * gdp_pi_2025 / gdp_pi_claim
      } else {
        init_real_ben[bi] <- NA
      }

      # Benefit series (ages, nominal, real) — clip at death age
      da <- ben$death_age[1]
      max_age <- if (!is.na(da)) floor(da) else 120
      rows <- ben[ben$age >= 65 & ben$age <= max_age & !is.na(ben$annual_ind), ]
      if (nrow(rows) > 0) {
        cpi_c <- assum$gdp_pi[assum$year == claim_year]
        if (length(cpi_c) > 0 && !is.na(cpi_c) && cpi_c > 0) {
          real_b <- sapply(seq_len(nrow(rows)), function(j) {
            cpi_y <- assum$gdp_pi[assum$year == rows$year[j]]
            if (length(cpi_y) > 0 && !is.na(cpi_y))
              round(rows$annual_ind[j] * cpi_c / cpi_y, 0)
            else rows$annual_ind[j]
          })
        } else {
          real_b <- rows$annual_ind
        }

        ben_by_year[[as.character(by)]] <- list(
          ages = rows$age,
          nominal = round(rows$annual_ind, 0),
          real = round(real_b, 0)
        )
      }
    }

    # Store cohort data
    coh_data[[combo_key]] <- list(
      birth_years = as.integer(birth_years),
      monthly_benefit = round(mb, 2),
      pv_benefits = round(pvb, 0),
      pv_taxes = round(pvt, 0),
      ratio = round(rat, 4),
      irr = round(irr_v, 6),
      repl_rate_real_all = round(rr_real_all, 6),
      initial_real_benefit = round(init_real_ben, 2),
      death_age = round(death_ages, 2)
    )

    # Store benefits data
    ben_data[[combo_key]] <- ben_by_year
  }

  # Write cohort file
  result_coh <- list(
    meta = list(worker_type = wkey, worker_label = wlabel, claim_age = 65L),
    data = coh_data
  )
  writeLines(toJSON(result_coh, auto_unbox = TRUE, digits = 6, na = "null"), coh_file)

  # Write benefits file
  result_ben <- list(
    meta = list(worker_type = wkey, worker_label = wlabel, claim_age = 65L),
    data = ben_data
  )
  writeLines(toJSON(result_ben, auto_unbox = TRUE, digits = 6, na = "null"), ben_file)

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("[%s] %s DONE in %.1f minutes (cohort: %s, benefits: %s)\n",
      format(Sys.time(), "%H:%M:%S"), wkey, elapsed,
      basename(coh_file), basename(ben_file)))
  sprintf("%s: done in %.1f minutes", wkey, elapsed)
}

# =============================================================================
# Run
# =============================================================================

t_start <- Sys.time()
n_calcs <- length(worker_configs) * nrow(combos) * length(birth_years)
cat(sprintf("\nStarting %d worker types x %d combos x %d birth years = %d calculations\n",
    length(worker_configs), nrow(combos), length(birth_years), n_calcs))
cat(sprintf("Using %d cores.\n\n", ncores))

results <- mclapply(worker_configs, process_worker_type, mc.cores = ncores)

cat("\n=== Results ===\n")
for (r in results) {
  if (is.character(r)) cat(r, "\n")
  else cat("ERROR:", paste(r, collapse = " "), "\n")
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\n=== Done! Total time: %.1f minutes ===\n", elapsed_total))
cat("Cohort files:", length(list.files(file.path(out_dir, "cohort"), pattern = "_65\\.json$")), "\n")
cat("Individual files:", length(list.files(file.path(out_dir, "individual"), pattern = "_65_benefits\\.json$")), "\n")
