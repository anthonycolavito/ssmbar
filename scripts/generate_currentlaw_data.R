#!/usr/bin/env Rscript
# =============================================================================
# Generate current-law data for all worker configurations
# =============================================================================
#
# Single:  6 types × 2 sexes × 8 birth years = 96 configs
# Married: 6 types × 6 spouse types × 2 sexes × 8 birth years = 576 configs
# Total: 672 configurations
#
# Writes:
#   docs/data/cohort/{type}.json              — summary metrics by dimension key
#   docs/data/individual/{type}_benefits.json  — by-age benefit series
#   docs/data/individual/{type}_nmtr.json      — by-age NMTR series
#
# Dimension keys:
#   Single:  male_single, female_single
#   Married: male_married_very_low, male_married_low, ..., female_married_custom_50k
#
# Usage:
#   Rscript scripts/generate_currentlaw_data.R
#   Rscript scripts/generate_currentlaw_data.R --type medium
#   Rscript scripts/generate_currentlaw_data.R --cores 4
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

for (i in seq_along(args)) {
  if (args[i] == "--type" && i < length(args)) filter_type <- args[i + 1]
  if (args[i] == "--cores" && i < length(args)) n_cores <- as.integer(args[i + 1])
}

out_dir <- "docs/data"
dir.create(file.path(out_dir, "cohort"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "individual"), recursive = TRUE, showWarnings = FALSE)

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

# Lookup table for getting type/custom_earnings by key
worker_lookup <- setNames(worker_configs, sapply(worker_configs, `[[`, "key"))

if (!is.null(filter_type)) {
  worker_configs <- Filter(function(w) w$key == filter_type, worker_configs)
  if (length(worker_configs) == 0) stop("Unknown worker type: ", filter_type)
}

# =============================================================================
# Dimensions
# =============================================================================

birth_years <- c(1940L, 1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
sexes <- c("male", "female")
claim_age <- 65L
spouse_type_keys <- c("very_low", "low", "medium", "high", "max", "custom_50k")

gdp_pi_2025 <- tr2025$gdp_pi[tr2025$year == 2025]
cat("GDP PI 2025:", gdp_pi_2025, "\n")
cat("Configs per type: 2 single × 8 BY + 2 married × 6 spouse × 8 BY = 112\n")
cat("Total: 112 × 6 types = 672 configs\n")

# =============================================================================
# Helper: make dimension key
# =============================================================================

make_dim_key <- function(sex, marital, spouse_key = NULL) {
  if (marital == "single") {
    paste(sex, "single", sep = "_")
  } else {
    paste(sex, "married", spouse_key, sep = "_")
  }
}

# =============================================================================
# Helper: compute benefits for one configuration
# =============================================================================

compute_config <- function(wtype, custom_earn, sex, marital, by,
                           sp_type = NULL, sp_custom_earn = NULL) {
  spouse_sex <- if (sex == "male") "female" else "male"

  if (marital == "married") {
    # Build primary worker args
    primary_args <- list(
      birth_yr = by, sex = sex, age_claim = claim_age,
      factors = sef2025, assumptions = tr2025,
      spouse_sex = spouse_sex, spouse_birth_yr = by,
      spouse_age_claim = claim_age, debugg = TRUE
    )

    # Primary worker type
    if (!is.null(custom_earn)) {
      primary_args$type <- "custom"
      primary_args$custom_avg_earnings <- custom_earn
    } else {
      primary_args$type <- wtype
    }

    # Spouse type
    if (!is.null(sp_custom_earn)) {
      primary_args$spouse_type <- "custom"
      primary_args$spouse_custom_avg_earnings <- sp_custom_earn
    } else {
      primary_args$spouse_type <- sp_type
    }

    worker <- do.call(calculate_benefits, primary_args)

    # Spouse as primary (for couple_measures)
    spouse_args <- list(
      birth_yr = by, sex = spouse_sex, age_claim = claim_age,
      factors = sef2025, assumptions = tr2025,
      spouse_sex = sex, spouse_birth_yr = by,
      spouse_age_claim = claim_age, debugg = TRUE
    )

    if (!is.null(sp_custom_earn)) {
      spouse_args$type <- "custom"
      spouse_args$custom_avg_earnings <- sp_custom_earn
    } else {
      spouse_args$type <- sp_type
    }

    if (!is.null(custom_earn)) {
      spouse_args$spouse_type <- "custom"
      spouse_args$spouse_custom_avg_earnings <- custom_earn
    } else {
      spouse_args$spouse_type <- wtype
    }

    spouse_worker <- do.call(calculate_benefits, spouse_args)

  } else {
    # Single worker
    if (!is.null(custom_earn)) {
      worker <- calculate_benefits(
        birth_yr = by, sex = sex, type = "custom",
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        custom_avg_earnings = custom_earn, debugg = TRUE
      )
    } else {
      worker <- calculate_benefits(
        birth_yr = by, sex = sex, type = wtype,
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        debugg = TRUE
      )
    }
    spouse_worker <- NULL
  }

  list(worker = worker, spouse = spouse_worker)
}

# =============================================================================
# Helper: extract metrics from computed worker
# =============================================================================

extract_metrics <- function(worker, spouse, by, assumptions) {
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

  couple_pvb <- NA; couple_pvt <- NA; couple_rat <- NA
  if (!is.null(spouse)) {
    cm <- tryCatch(
      couple_measures(worker, spouse, assumptions, include_employer = TRUE, shared = TRUE),
      error = function(e) NULL
    )
    if (!is.null(cm)) {
      couple_pvb <- cm$couple_pv_benefits
      couple_pvt <- cm$couple_pv_taxes
      couple_rat <- cm$couple_ratio
    }
  }

  list(
    monthly_benefit = mb, pv_benefits = pvb, pv_taxes = pvt,
    ratio = rat, irr = irr_val, repl_rate = rr_val,
    initial_real_benefit = init_real, death_age = da,
    couple_pv_benefits = couple_pvb, couple_pv_taxes = couple_pvt,
    couple_ratio = couple_rat
  )
}

# =============================================================================
# Helper: extract benefit series (by age)
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
# Helper: compute NMTR for one worker
# =============================================================================

compute_nmtr <- function(worker, assumptions) {
  nmtr <- tryCatch(
    net_marginal_tax_rate(worker, assumptions, include_employer = TRUE),
    error = function(e) {
      cat(sprintf("  NMTR error: %s\n", e$message))
      NULL
    }
  )
  if (is.null(nmtr)) return(NULL)

  nmtr_rows <- nmtr[!is.na(nmtr$net_marginal_tax_rate) & nmtr$age >= 21 & nmtr$age <= 64, ]
  if (nrow(nmtr_rows) == 0) return(NULL)

  list(
    ages = as.integer(nmtr_rows$age),
    nmtr = round(nmtr_rows$net_marginal_tax_rate, 6),
    earnings = round(nmtr_rows$earnings, 0),
    ss_tax = round(nmtr_rows$ss_tax, 0),
    delta_pv = round(nmtr_rows$delta_pv_benefits, 0)
  )
}

# =============================================================================
# Process one dimension key (one combo of sex/marital/spouse_type across birth years)
# =============================================================================

process_dim_key <- function(wtype, custom_earn, sx, marital, sp_key, sp_type, sp_custom, dim_key) {
  mb_arr <- numeric(length(birth_years))
  pvb_arr <- numeric(length(birth_years))
  pvt_arr <- numeric(length(birth_years))
  rat_arr <- numeric(length(birth_years))
  irr_arr <- numeric(length(birth_years))
  rr_arr <- numeric(length(birth_years))
  init_real_arr <- numeric(length(birth_years))
  death_arr <- numeric(length(birth_years))
  cpvb_arr <- numeric(length(birth_years))
  cpvt_arr <- numeric(length(birth_years))
  crat_arr <- numeric(length(birth_years))

  ben_by_year <- list()
  nmtr_by_year <- list()

  for (bi in seq_along(birth_years)) {
    by <- birth_years[bi]

    result <- tryCatch(
      compute_config(wtype, custom_earn, sx, marital, by, sp_type, sp_custom),
      error = function(e) {
        cat(sprintf("  ERROR: %s %s by=%d: %s\n", dim_key, by, by, e$message))
        list(worker = NULL, spouse = NULL)
      }
    )

    if (is.null(result$worker)) {
      mb_arr[bi] <- NA; pvb_arr[bi] <- NA; pvt_arr[bi] <- NA
      rat_arr[bi] <- NA; irr_arr[bi] <- NA; rr_arr[bi] <- NA
      init_real_arr[bi] <- NA; death_arr[bi] <- NA
      cpvb_arr[bi] <- NA; cpvt_arr[bi] <- NA; crat_arr[bi] <- NA
      next
    }

    worker <- result$worker
    spouse <- result$spouse

    metrics <- extract_metrics(worker, spouse, by, tr2025)
    mb_arr[bi] <- metrics$monthly_benefit
    pvb_arr[bi] <- metrics$pv_benefits
    pvt_arr[bi] <- metrics$pv_taxes
    rat_arr[bi] <- metrics$ratio
    irr_arr[bi] <- metrics$irr
    rr_arr[bi] <- metrics$repl_rate
    init_real_arr[bi] <- metrics$initial_real_benefit
    death_arr[bi] <- metrics$death_age
    cpvb_arr[bi] <- metrics$couple_pv_benefits
    cpvt_arr[bi] <- metrics$couple_pv_taxes
    crat_arr[bi] <- metrics$couple_ratio

    bseries <- extract_benefit_series(worker, by, tr2025)
    if (!is.null(bseries)) ben_by_year[[as.character(by)]] <- bseries

    nmtr_result <- compute_nmtr(worker, tr2025)
    if (!is.null(nmtr_result)) nmtr_by_year[[as.character(by)]] <- nmtr_result
  }

  coh_entry <- list(
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

  if (marital == "married") {
    coh_entry$couple_pv_benefits <- round(cpvb_arr, 0)
    coh_entry$couple_pv_taxes <- round(cpvt_arr, 0)
    coh_entry$couple_ratio <- round(crat_arr, 4)
  }

  list(cohort = coh_entry, benefits = ben_by_year, nmtr = nmtr_by_year)
}

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

  coh_file <- file.path(out_dir, "cohort", sprintf("%s.json", wkey))
  ben_file <- file.path(out_dir, "individual", sprintf("%s_benefits.json", wkey))
  nmtr_file <- file.path(out_dir, "individual", sprintf("%s_nmtr.json", wkey))

  coh_data <- list()
  ben_data <- list()
  nmtr_data <- list()

  total_keys <- 2 + 2 * length(spouse_type_keys)  # 2 single + 12 married = 14
  key_num <- 0

  for (sx in sexes) {
    # Single
    key_num <- key_num + 1
    dim_key <- make_dim_key(sx, "single")
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    cat(sprintf("[%s] %s: dim %d/%d (%s) %.1f min\n",
        format(Sys.time(), "%H:%M:%S"), wkey, key_num, total_keys, dim_key, elapsed))

    res <- process_dim_key(wtype, custom_earn, sx, "single", NULL, NULL, NULL, dim_key)
    coh_data[[dim_key]] <- res$cohort
    ben_data[[dim_key]] <- res$benefits
    nmtr_data[[dim_key]] <- res$nmtr

    # Married — iterate over all spouse types
    for (sp_key in spouse_type_keys) {
      key_num <- key_num + 1
      dim_key <- make_dim_key(sx, "married", sp_key)
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      cat(sprintf("[%s] %s: dim %d/%d (%s) %.1f min\n",
          format(Sys.time(), "%H:%M:%S"), wkey, key_num, total_keys, dim_key, elapsed))

      sp_cfg <- worker_lookup[[sp_key]]
      sp_type <- sp_cfg$type
      sp_custom <- sp_cfg$custom_earnings

      res <- process_dim_key(wtype, custom_earn, sx, "married", sp_key, sp_type, sp_custom, dim_key)
      coh_data[[dim_key]] <- res$cohort
      ben_data[[dim_key]] <- res$benefits
      nmtr_data[[dim_key]] <- res$nmtr
    }
  }

  # Write cohort file
  coh_out <- list(
    meta = list(
      worker_type = wkey,
      worker_label = wlabel,
      claim_age = claim_age,
      sexes = sexes,
      spouse_types = spouse_type_keys,
      birth_years = as.integer(birth_years),
      dim_key_format = "Single: {sex}_single | Married: {sex}_married_{spouse_type}"
    ),
    data = coh_data
  )
  write(toJSON(coh_out, auto_unbox = TRUE, digits = 6), coh_file)
  cat(sprintf("  Wrote %s (%.1f KB)\n", coh_file, file.info(coh_file)$size / 1024))

  # Write benefits file
  ben_out <- list(
    meta = list(worker_type = wkey, claim_age = claim_age),
    data = ben_data
  )
  write(toJSON(ben_out, auto_unbox = TRUE, digits = 0), ben_file)
  cat(sprintf("  Wrote %s (%.1f KB)\n", ben_file, file.info(ben_file)$size / 1024))

  # Write NMTR file
  nmtr_out <- list(
    meta = list(
      worker_type = wkey, claim_age = claim_age,
      include_employer = TRUE,
      method = "Cumulative stopping-point, discounted to working year t"
    ),
    data = nmtr_data
  )
  write(toJSON(nmtr_out, auto_unbox = TRUE, digits = 6), nmtr_file)
  cat(sprintf("  Wrote %s (%.1f KB)\n", nmtr_file, file.info(nmtr_file)$size / 1024))

  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  cat(sprintf("[%s] %s COMPLETE in %.1f minutes\n", format(Sys.time(), "%H:%M:%S"), wkey, elapsed))
  return(sprintf("%s: %.1f min", wkey, elapsed))
}

# =============================================================================
# Run
# =============================================================================

cat(sprintf("Starting data generation at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Processing %d worker type(s) with %d cores\n", length(worker_configs), n_cores))

if (length(worker_configs) == 1 || n_cores == 1) {
  results <- lapply(worker_configs, process_worker_type)
} else {
  results <- parallel::mclapply(worker_configs, process_worker_type, mc.cores = n_cores)
}

cat("\n=== SUMMARY ===\n")
for (r in results) cat(" ", r, "\n")
cat(sprintf("Finished at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
