#!/usr/bin/env Rscript
# =============================================================================
# Generate current-law data for 192 worker configurations
# =============================================================================
#
# 6 worker types × 2 sexes × 8 birth cohorts × 2 marital statuses = 192 configs
#
# Writes:
#   docs/data/cohort/{type}.json              — summary metrics by sex/marital/birth year
#   docs/data/individual/{type}_benefits.json  — by-age benefit series
#   docs/data/individual/{type}_nmtr.json      — by-age NMTR series
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

if (!is.null(filter_type)) {
  worker_configs <- Filter(function(w) w$key == filter_type, worker_configs)
  if (length(worker_configs) == 0) stop("Unknown worker type: ", filter_type)
}

# =============================================================================
# Dimensions
# =============================================================================

birth_years <- c(1940L, 1950L, 1960L, 1970L, 1980L, 1990L, 2000L, 2010L)
sexes <- c("male", "female")
marital_statuses <- c("single", "married")
claim_age <- 65L

gdp_pi_2025 <- tr2025$gdp_pi[tr2025$year == 2025]
cat("GDP PI 2025:", gdp_pi_2025, "\n")
cat("Dimensions: 6 types × 2 sexes × 8 birth years × 2 marital = 192 configs\n")

# =============================================================================
# Helper: make dimension key
# =============================================================================

make_dim_key <- function(sex, marital) {
  paste(sex, marital, sep = "_")
}

# =============================================================================
# Helper: compute benefits for one configuration
# =============================================================================

compute_config <- function(wtype, custom_earn, sex, marital, by) {
  # Determine spouse parameters for married workers
  # Assumption: spouse is same worker type, opposite sex, same birth year, same claim age
  spouse_sex <- if (sex == "male") "female" else "male"

  if (marital == "married") {
    if (!is.null(custom_earn)) {
      worker <- calculate_benefits(
        birth_yr = by, sex = sex, type = "custom",
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        custom_avg_earnings = custom_earn,
        spouse_type = "custom", spouse_sex = spouse_sex,
        spouse_birth_yr = by, spouse_age_claim = claim_age,
        spouse_custom_avg_earnings = custom_earn,
        debugg = TRUE
      )
      # Spouse as primary for couple_measures
      spouse_worker <- calculate_benefits(
        birth_yr = by, sex = spouse_sex, type = "custom",
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        custom_avg_earnings = custom_earn,
        spouse_type = "custom", spouse_sex = sex,
        spouse_birth_yr = by, spouse_age_claim = claim_age,
        spouse_custom_avg_earnings = custom_earn,
        debugg = TRUE
      )
    } else {
      worker <- calculate_benefits(
        birth_yr = by, sex = sex, type = wtype,
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        spouse_type = wtype, spouse_sex = spouse_sex,
        spouse_birth_yr = by, spouse_age_claim = claim_age,
        debugg = TRUE
      )
      spouse_worker <- calculate_benefits(
        birth_yr = by, sex = spouse_sex, type = wtype,
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        spouse_type = wtype, spouse_sex = sex,
        spouse_birth_yr = by, spouse_age_claim = claim_age,
        debugg = TRUE
      )
    }
  } else {
    # Single worker
    if (!is.null(custom_earn)) {
      worker <- calculate_benefits(
        birth_yr = by, sex = sex, type = "custom",
        age_claim = claim_age, factors = sef2025, assumptions = tr2025,
        custom_avg_earnings = custom_earn,
        debugg = TRUE
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

  # Monthly benefit at claim age
  mb <- worker$ben[worker$age == claim_age]
  mb <- if (length(mb) > 0) mb[1] else NA

  # Death age
  da <- worker$death_age[1]

  # PV calculations (include employer taxes)
  pvb <- tryCatch(pv_lifetime_benefits(worker, assumptions)$pv_benefits[1], error = function(e) NA)
  pvt <- tryCatch(pv_lifetime_taxes(worker, assumptions, include_employer = TRUE)$pv_taxes[1], error = function(e) NA)
  rat <- if (!is.na(pvb) && !is.na(pvt) && pvt > 0) pvb / pvt else NA

  # IRR (include employer)
  irr_val <- tryCatch(internal_rate_of_return(worker, assumptions, include_employer = TRUE)$irr[1],
                       error = function(e) NA)

  # Replacement rate (real_all only)
  rr <- tryCatch(rep_rates(worker, assumptions), error = function(e) NULL)
  rr_val <- NA
  if (!is.null(rr) && is.data.frame(rr)) {
    rr_row <- rr$rep_rate[rr$type == "real_all"]
    if (length(rr_row) > 0) rr_val <- rr_row[1]
  }

  # Initial real benefit (deflated to 2025$)
  gdp_pi_claim <- assumptions$gdp_pi[assumptions$year == claim_year]
  init_real <- NA
  if (length(gdp_pi_claim) > 0 && !is.na(gdp_pi_claim) && gdp_pi_claim > 0 && !is.na(mb)) {
    init_real <- mb * gdp_pi_2025 / gdp_pi_claim
  }

  # Couple metrics (for married workers)
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
  # NMTR requires debugg=TRUE columns — already present
  nmtr <- tryCatch(
    net_marginal_tax_rate(worker, assumptions, include_employer = TRUE),
    error = function(e) {
      cat(sprintf("  NMTR error: %s\n", e$message))
      NULL
    }
  )
  if (is.null(nmtr)) return(NULL)

  # Filter to valid working years
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
  coh_file <- file.path(out_dir, "cohort", sprintf("%s.json", wkey))
  ben_file <- file.path(out_dir, "individual", sprintf("%s_benefits.json", wkey))
  nmtr_file <- file.path(out_dir, "individual", sprintf("%s_nmtr.json", wkey))

  coh_data <- list()
  ben_data <- list()
  nmtr_data <- list()

  n_configs <- length(sexes) * length(marital_statuses) * length(birth_years)
  config_num <- 0

  for (sx in sexes) {
    for (ms in marital_statuses) {
      dim_key <- make_dim_key(sx, ms)

      # Initialize cohort metric arrays
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
        config_num <- config_num + 1

        if (config_num %% 8 == 1 || config_num == 1) {
          elapsed <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
          cat(sprintf("[%s] %s: config %d/%d (%s %s by=%d) %.1f min elapsed\n",
              format(Sys.time(), "%H:%M:%S"), wkey, config_num, n_configs,
              dim_key, by, by, elapsed))
        }

        result <- tryCatch(
          compute_config(wtype, custom_earn, sx, ms, by),
          error = function(e) {
            cat(sprintf("  ERROR: %s %s %s by=%d: %s\n", wkey, sx, ms, by, e$message))
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

        # Extract summary metrics
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

        # Extract benefit series
        bseries <- extract_benefit_series(worker, by, tr2025)
        if (!is.null(bseries)) {
          ben_by_year[[as.character(by)]] <- bseries
        }

        # Compute NMTR (only for primary worker)
        nmtr_result <- compute_nmtr(worker, tr2025)
        if (!is.null(nmtr_result)) {
          nmtr_by_year[[as.character(by)]] <- nmtr_result
        }
      }

      # Store cohort data for this dimension key
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

      # Add couple metrics for married workers
      if (ms == "married") {
        coh_entry$couple_pv_benefits <- round(cpvb_arr, 0)
        coh_entry$couple_pv_taxes <- round(cpvt_arr, 0)
        coh_entry$couple_ratio <- round(crat_arr, 4)
      }

      coh_data[[dim_key]] <- coh_entry
      ben_data[[dim_key]] <- ben_by_year
      nmtr_data[[dim_key]] <- nmtr_by_year
    }
  }

  # Write cohort file
  coh_out <- list(
    meta = list(
      worker_type = wkey,
      worker_label = wlabel,
      claim_age = claim_age,
      sexes = sexes,
      marital_statuses = marital_statuses,
      birth_years = as.integer(birth_years),
      spouse_assumption = "Same worker type, opposite sex, same birth year, claim age 65"
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
  # Serial
  results <- lapply(worker_configs, process_worker_type)
} else {
  # Parallel
  results <- parallel::mclapply(worker_configs, process_worker_type, mc.cores = n_cores)
}

cat("\n=== SUMMARY ===\n")
for (r in results) cat(" ", r, "\n")
cat(sprintf("Finished at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
