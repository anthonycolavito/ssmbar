#!/usr/bin/env Rscript
# =============================================================================
# Pre-compute Static Data for GitHub Pages Benefit Explorer
# =============================================================================
#
# This script runs the ssmbar package across all input combinations and exports
# results as JSON files for the static site. Output goes to docs/data/.
#
# Usage:
#   Rscript scripts/precompute_static_data.R [--cores N] [--type TYPE] [--phase PHASE]
#
# Phases:
#   1 = Cohort data only (baseline + reforms)
#   2 = Individual metrics + benefits
#   3 = Individual marginal analysis (expensive)
#   all = Everything (default)
#
# =============================================================================

library(ssmbar)
library(jsonlite)
library(parallel)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
n_cores <- 1
filter_type <- NULL
phase <- "all"

i <- 1
while (i <= length(args)) {
  if (args[i] == "--cores" && i < length(args)) {
    n_cores <- as.integer(args[i + 1])
    i <- i + 2
  } else if (args[i] == "--type" && i < length(args)) {
    filter_type <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--phase" && i < length(args)) {
    phase <- args[i + 1]
    i <- i + 2
  } else {
    i <- i + 1
  }
}

cat("=== Pre-computation Configuration ===\n")
cat(sprintf("  Cores: %d\n", n_cores))
cat(sprintf("  Phase: %s\n", phase))
if (!is.null(filter_type)) cat(sprintf("  Filter type: %s\n", filter_type))
cat("\n")

# Load package data
data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

# Output directory — derive from script location or use working directory
# When run as: Rscript scripts/precompute_static_data.R (from package root)
pkg_root <- getwd()
# If running from scripts/ subdirectory, go up one level
if (basename(pkg_root) == "scripts") pkg_root <- dirname(pkg_root)
out_dir <- file.path(pkg_root, "docs", "data")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
dir.create(file.path(out_dir, "cohort"), showWarnings = FALSE)
dir.create(file.path(out_dir, "individual"), showWarnings = FALSE)
cat(sprintf("  Output directory: %s\n", out_dir))

# =============================================================================
# Configuration
# =============================================================================

# Worker types: 5 standard + 6 custom earnings levels
WORKER_CONFIGS <- list(
  list(type = "very_low", label = "Very Low Earner", custom = NULL),
  list(type = "low",      label = "Low Earner",      custom = NULL),
  list(type = "medium",   label = "Medium Earner",   custom = NULL),
  list(type = "high",     label = "High Earner",     custom = NULL),
  list(type = "max",      label = "Maximum Earner",  custom = NULL),
  list(type = "custom",   label = "$25K Earner",     custom = 25000),
  list(type = "custom",   label = "$50K Earner",     custom = 50000),
  list(type = "custom",   label = "$75K Earner",     custom = 75000),
  list(type = "custom",   label = "$100K Earner",    custom = 100000),
  list(type = "custom",   label = "$125K Earner",    custom = 125000),
  list(type = "custom",   label = "$150K Earner",    custom = 150000)
)

SEXES <- c("male", "female")
CLAIM_AGES <- 62:70
COHORT_BIRTH_YEARS <- 1960:2005
INDIVIDUAL_BIRTH_YEARS <- 1939:2010

# Reform scenarios
REFORM_SCENARIOS <- list(
  baseline = list(name = "baseline", label = "Current Law", fn = NULL),

  # PIA Formula (mutually exclusive)
  reduce_fact3 = list(
    name = "reduce_fact3",
    label = "Reduce Fact3 to 5%",
    fn = function() reform_reduce_fact3(0.05, 2030)
  ),
  flat_benefit = list(
    name = "flat_benefit",
    label = "Flat Benefit",
    fn = function() reform_flat_benefit(19300/12, 2030, 25)
  ),
  simpson_bowles_pia = list(
    name = "simpson_bowles_pia",
    label = "Simpson-Bowles PIA",
    fn = function() reform_simpson_bowles(2030, 10)
  ),

  # NRA Changes (mutually exclusive)
  nra_to_68 = list(
    name = "nra_to_68",
    label = "Raise NRA to 68",
    fn = function() reform_nra_to_68(2030)
  ),
  index_nra = list(
    name = "index_nra",
    label = "Index NRA to Longevity",
    fn = function() reform_index_nra(2030)
  ),
  nra_to_69_index = list(
    name = "nra_to_69_index",
    label = "NRA to 69, then Index",
    fn = function() reform_nra_to_69_index(2030)
  ),

  # COLA Indexing (mutually exclusive)
  chained_cpi = list(
    name = "chained_cpi",
    label = "Chained CPI",
    fn = function() reform_chained_cpi(2030)
  ),
  cola_cap = list(
    name = "cola_cap",
    label = "Cap COLAs at Median",
    fn = function() reform_cola_cap(2030)
  ),
  cpi_e = list(
    name = "cpi_e",
    label = "CPI-E (Higher)",
    fn = function() reform_cpi_e(2030)
  ),

  # Tax Max (mutually exclusive)
  taxmax_90_pct = list(
    name = "taxmax_90_pct",
    label = "90% Coverage + 5% Credit",
    fn = function() reform_taxmax_90_pct(2030)
  ),
  eliminate_taxmax = list(
    name = "eliminate_taxmax",
    label = "Eliminate Taxmax + 15% Credit",
    fn = function() reform_eliminate_taxmax(2030)
  ),
  eliminate_taxmax_no_credit = list(
    name = "eliminate_taxmax_no_credit",
    label = "Eliminate Taxmax, No Credit",
    fn = function() reform_eliminate_taxmax_no_credit(2030)
  ),

  # Other Reforms
  benefit_cut_5 = list(
    name = "benefit_cut_5",
    label = "5% Benefit Cut",
    fn = function() reform_reduce_benefits(0.95, 2030)
  ),
  benefit_cut_10 = list(
    name = "benefit_cut_10",
    label = "10% Benefit Cut",
    fn = function() reform_reduce_benefits(0.90, 2030)
  ),
  forty_year_avg = list(
    name = "forty_year_avg",
    label = "40-Year Averaging",
    fn = function() reform_40_year_averaging(2030)
  ),
  repeal_ret = list(
    name = "repeal_ret",
    label = "Repeal RET",
    fn = function() reform_repeal_ret(2030)
  ),
  phase_out_spousal = list(
    name = "phase_out_spousal",
    label = "Phase Out Spousal",
    fn = function() reform_phase_out_spousal(2030)
  ),
  mini_pia = list(
    name = "mini_pia",
    label = "Mini-PIA",
    fn = function() reform_mini_pia(2030, 10)
  ),

  # Benefit Enhancements
  basic_minimum = list(
    name = "basic_minimum",
    label = "Basic Minimum Benefit",
    fn = function() reform_basic_minimum(900, 1342, 2030)
  ),
  child_care_credit = list(
    name = "child_care_credit",
    label = "Child Care Credit",
    fn = function() reform_child_care_credit(2030, 5)
  ),
  widow_75_pct = list(
    name = "widow_75_pct",
    label = "75% Widow Benefit",
    fn = function() reform_widow_75_pct(2030)
  ),

  # Combined package
  simpson_bowles_combo = list(
    name = "simpson_bowles_combo",
    label = "Simpson-Bowles Package",
    fn = function() {
      list(
        reform_simpson_bowles(2030, 10),
        reform_index_nra(2030),
        reform_chained_cpi(2030),
        reform_taxmax_90_pct(2030)
      )
    }
  )
)

# Pre-compute reform assumptions (apply each reform to tr2025)
cat("Pre-computing reform assumptions...\n")
REFORM_ASSUMPTIONS <- list()
for (scenario_name in names(REFORM_SCENARIOS)) {
  scenario <- REFORM_SCENARIOS[[scenario_name]]
  if (is.null(scenario$fn)) {
    REFORM_ASSUMPTIONS[[scenario_name]] <- tr2025  # baseline
  } else {
    reform_obj <- scenario$fn()
    if (is.list(reform_obj) && !inherits(reform_obj, "Reform")) {
      # Combined package (list of reforms)
      REFORM_ASSUMPTIONS[[scenario_name]] <- apply_reforms(tr2025, reform_obj, check_exclusivity = FALSE)
    } else {
      REFORM_ASSUMPTIONS[[scenario_name]] <- apply_reform(tr2025, reform_obj)
    }
  }
}
cat(sprintf("  %d reform scenarios prepared.\n\n", length(REFORM_ASSUMPTIONS)))

# =============================================================================
# Helper: Get config key for file naming
# =============================================================================
get_config_key <- function(worker_config) {
  if (!is.null(worker_config$custom)) {
    paste0("custom_", worker_config$custom / 1000, "k")
  } else {
    worker_config$type
  }
}

# =============================================================================
# Helper: Safe metric computation with error handling
# =============================================================================
safe_compute <- function(expr, default = NA_real_) {
  tryCatch(expr, error = function(e) default)
}

# =============================================================================
# Phase 1: Cohort Data
# =============================================================================
compute_cohort_file <- function(worker_config, sex, claim_age) {
  config_key <- get_config_key(worker_config)
  filename <- sprintf("%s_%s_%d.json", config_key, sex, claim_age)
  filepath <- file.path(out_dir, "cohort", filename)

  # Check if already computed
  if (file.exists(filepath)) {
    cat(sprintf("  [SKIP] cohort/%s (exists)\n", filename))
    return(invisible(NULL))
  }

  cat(sprintf("  [COMPUTE] cohort/%s\n", filename))

  custom_earnings <- worker_config$custom
  data_out <- list()

  for (scenario_name in names(REFORM_SCENARIOS)) {
    assumptions <- REFORM_ASSUMPTIONS[[scenario_name]]
    is_baseline <- scenario_name == "baseline"

    birth_years <- numeric(0)
    monthly_benefit <- numeric(0)
    pv_benefits <- numeric(0)
    pv_taxes <- numeric(0)
    ratio <- numeric(0)
    irr <- numeric(0)
    repl_rate_pv <- numeric(0)
    repl_rate_wage_h35 <- numeric(0)
    repl_rate_real_h35 <- numeric(0)

    for (by in COHORT_BIRTH_YEARS) {
      # Calculate benefits
      ben_result <- if (is_baseline) {
        safe_compute(
          calculate_benefits(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = tr2025,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          ),
          default = NULL
        )
      } else {
        safe_compute(
          calculate_benefits_reform(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = assumptions,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          ),
          default = NULL
        )
      }

      if (is.null(ben_result)) {
        birth_years <- c(birth_years, by)
        monthly_benefit <- c(monthly_benefit, NA_real_)
        pv_benefits <- c(pv_benefits, NA_real_)
        pv_taxes <- c(pv_taxes, NA_real_)
        ratio <- c(ratio, NA_real_)
        irr <- c(irr, NA_real_)
        repl_rate_pv <- c(repl_rate_pv, NA_real_)
        repl_rate_wage_h35 <- c(repl_rate_wage_h35, NA_real_)
        repl_rate_real_h35 <- c(repl_rate_real_h35, NA_real_)
        next
      }

      birth_years <- c(birth_years, by)

      # Monthly benefit at claim age
      ben_at_claim <- ben_result$ben[ben_result$age == claim_age]
      monthly_benefit <- c(monthly_benefit,
                           if (length(ben_at_claim) > 0) ben_at_claim[1] else NA_real_)

      # PV metrics
      pv_b <- safe_compute(pv_lifetime_benefits(ben_result, assumptions)$pv_benefits[1])
      pv_t <- safe_compute(pv_lifetime_taxes(ben_result, assumptions, include_employer = TRUE)$pv_taxes[1])
      pv_benefits <- c(pv_benefits, pv_b)
      pv_taxes <- c(pv_taxes, pv_t)

      r <- if (!is.na(pv_b) && !is.na(pv_t) && pv_t > 0) pv_b / pv_t else NA_real_
      ratio <- c(ratio, r)

      # IRR
      irr_val <- safe_compute(
        internal_rate_of_return(ben_result, assumptions, include_employer = TRUE)$irr[1]
      )
      irr <- c(irr, irr_val)

      # Replacement rates
      rr <- safe_compute(rep_rates(ben_result, assumptions), default = NULL)
      if (!is.null(rr) && is.data.frame(rr)) {
        repl_rate_pv <- c(repl_rate_pv,
                          if ("pv_rr" %in% rr$type) rr$rep_rate[rr$type == "pv_rr"][1] else NA_real_)
        repl_rate_wage_h35 <- c(repl_rate_wage_h35,
                                if ("wage_h35" %in% rr$type) rr$rep_rate[rr$type == "wage_h35"][1] else NA_real_)
        repl_rate_real_h35 <- c(repl_rate_real_h35,
                                if ("real_h35" %in% rr$type) rr$rep_rate[rr$type == "real_h35"][1] else NA_real_)
      } else {
        repl_rate_pv <- c(repl_rate_pv, NA_real_)
        repl_rate_wage_h35 <- c(repl_rate_wage_h35, NA_real_)
        repl_rate_real_h35 <- c(repl_rate_real_h35, NA_real_)
      }
    }

    data_out[[scenario_name]] <- list(
      birth_years = birth_years,
      monthly_benefit = round(monthly_benefit, 2),
      pv_benefits = round(pv_benefits, 0),
      pv_taxes = round(pv_taxes, 0),
      ratio = round(ratio, 4),
      irr = round(irr, 6),
      repl_rate_pv = round(repl_rate_pv, 6),
      repl_rate_wage_h35 = round(repl_rate_wage_h35, 6),
      repl_rate_real_h35 = round(repl_rate_real_h35, 6)
    )
  }

  result <- list(
    meta = list(
      worker_type = config_key,
      worker_label = worker_config$label,
      sex = sex,
      claim_age = claim_age,
      custom_earnings = worker_config$custom
    ),
    data = data_out
  )

  write(toJSON(result, auto_unbox = TRUE, digits = 6, na = "null"), filepath)
}

# =============================================================================
# Phase 2: Individual Metrics + Benefits
# =============================================================================
compute_individual_files <- function(worker_config, sex, claim_age) {
  config_key <- get_config_key(worker_config)
  metrics_file <- sprintf("%s_%s_%d_metrics.json", config_key, sex, claim_age)
  benefits_file <- sprintf("%s_%s_%d_benefits.json", config_key, sex, claim_age)
  metrics_path <- file.path(out_dir, "individual", metrics_file)
  benefits_path <- file.path(out_dir, "individual", benefits_file)

  # Check if already computed
  if (file.exists(metrics_path) && file.exists(benefits_path)) {
    cat(sprintf("  [SKIP] individual/%s (exists)\n", metrics_file))
    return(invisible(NULL))
  }

  cat(sprintf("  [COMPUTE] individual/%s + benefits\n", metrics_file))

  custom_earnings <- worker_config$custom
  metrics_data <- list()
  benefits_data <- list()

  for (scenario_name in names(REFORM_SCENARIOS)) {
    assumptions <- REFORM_ASSUMPTIONS[[scenario_name]]
    is_baseline <- scenario_name == "baseline"

    # Metrics arrays
    m_birth_years <- numeric(0)
    m_monthly_benefit <- numeric(0)
    m_pv_benefits <- numeric(0)
    m_pv_taxes <- numeric(0)
    m_ratio <- numeric(0)
    m_irr <- numeric(0)

    # Benefits per birth year
    b_by_year <- list()

    for (by in INDIVIDUAL_BIRTH_YEARS) {
      ben_result <- if (is_baseline) {
        safe_compute(
          calculate_benefits(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = tr2025,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          ),
          default = NULL
        )
      } else {
        safe_compute(
          calculate_benefits_reform(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = assumptions,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          ),
          default = NULL
        )
      }

      m_birth_years <- c(m_birth_years, by)

      if (is.null(ben_result)) {
        m_monthly_benefit <- c(m_monthly_benefit, NA_real_)
        m_pv_benefits <- c(m_pv_benefits, NA_real_)
        m_pv_taxes <- c(m_pv_taxes, NA_real_)
        m_ratio <- c(m_ratio, NA_real_)
        m_irr <- c(m_irr, NA_real_)
        next
      }

      # Monthly benefit at claim age
      ben_at_claim <- ben_result$ben[ben_result$age == claim_age]
      m_monthly_benefit <- c(m_monthly_benefit,
                             if (length(ben_at_claim) > 0) ben_at_claim[1] else NA_real_)

      # PV metrics
      pv_b <- safe_compute(pv_lifetime_benefits(ben_result, assumptions)$pv_benefits[1])
      pv_t <- safe_compute(pv_lifetime_taxes(ben_result, assumptions, include_employer = TRUE)$pv_taxes[1])
      m_pv_benefits <- c(m_pv_benefits, pv_b)
      m_pv_taxes <- c(m_pv_taxes, pv_t)

      r <- if (!is.na(pv_b) && !is.na(pv_t) && pv_t > 0) pv_b / pv_t else NA_real_
      m_ratio <- c(m_ratio, r)

      irr_val <- safe_compute(
        internal_rate_of_return(ben_result, assumptions, include_employer = TRUE)$irr[1]
      )
      m_irr <- c(m_irr, irr_val)

      # Benefit series (only for claim age onward, where benefit > 0)
      benefit_rows <- ben_result[ben_result$age >= claim_age & !is.na(ben_result$annual_ind), ]
      if (nrow(benefit_rows) > 0) {
        # Compute real benefits using CPI deflation
        cpi_at_claim <- assumptions$gdp_pi[assumptions$year == (by + claim_age)]
        real_benefits <- if (length(cpi_at_claim) > 0 && !is.na(cpi_at_claim)) {
          sapply(seq_len(nrow(benefit_rows)), function(j) {
            yr <- benefit_rows$year[j]
            cpi_yr <- assumptions$gdp_pi[assumptions$year == yr]
            if (length(cpi_yr) > 0 && !is.na(cpi_yr) && cpi_at_claim > 0) {
              round(benefit_rows$annual_ind[j] * cpi_at_claim / cpi_yr, 0)
            } else {
              benefit_rows$annual_ind[j]
            }
          })
        } else {
          benefit_rows$annual_ind
        }

        b_by_year[[as.character(by)]] <- list(
          ages = benefit_rows$age,
          nominal = round(benefit_rows$annual_ind, 0),
          real = round(real_benefits, 0),
          earnings = round(benefit_rows$earnings, 0),
          bc = as.character(benefit_rows$bc)
        )
      }
    }

    metrics_data[[scenario_name]] <- list(
      birth_years = m_birth_years,
      monthly_benefit = round(m_monthly_benefit, 2),
      pv_benefits = round(m_pv_benefits, 0),
      pv_taxes = round(m_pv_taxes, 0),
      ratio = round(m_ratio, 4),
      irr = round(m_irr, 6)
    )

    benefits_data[[scenario_name]] <- b_by_year
  }

  # Write metrics file
  metrics_result <- list(
    meta = list(
      worker_type = config_key,
      worker_label = worker_config$label,
      sex = sex,
      claim_age = claim_age,
      custom_earnings = worker_config$custom
    ),
    data = metrics_data
  )
  write(toJSON(metrics_result, auto_unbox = TRUE, digits = 6, na = "null"), metrics_path)

  # Write benefits file
  benefits_result <- list(
    meta = list(
      worker_type = config_key,
      worker_label = worker_config$label,
      sex = sex,
      claim_age = claim_age,
      custom_earnings = worker_config$custom
    ),
    data = benefits_data
  )
  write(toJSON(benefits_result, auto_unbox = TRUE, digits = 6, na = "null"), benefits_path)
}

# =============================================================================
# Phase 3: Individual Marginal Analysis
# =============================================================================
compute_marginal_file <- function(worker_config, sex, claim_age) {
  config_key <- get_config_key(worker_config)
  filename <- sprintf("%s_%s_%d_marginal.json", config_key, sex, claim_age)
  filepath <- file.path(out_dir, "individual", filename)

  if (file.exists(filepath)) {
    cat(sprintf("  [SKIP] individual/%s (exists)\n", filename))
    return(invisible(NULL))
  }

  cat(sprintf("  [COMPUTE] individual/%s (marginal - slow)\n", filename))

  custom_earnings <- worker_config$custom
  marginal_data <- list()

  for (scenario_name in names(REFORM_SCENARIOS)) {
    assumptions <- REFORM_ASSUMPTIONS[[scenario_name]]
    is_baseline <- scenario_name == "baseline"

    m_by_year <- list()

    for (by in INDIVIDUAL_BIRTH_YEARS) {
      # Marginal analysis
      marg <- safe_compute({
        ben_result <- if (is_baseline) {
          calculate_benefits(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = tr2025,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          )
        } else {
          calculate_benefits_reform(
            birth_yr = by, sex = sex, type = worker_config$type,
            age_claim = claim_age, factors = sef2025, assumptions = assumptions,
            custom_avg_earnings = custom_earnings, debugg = TRUE
          )
        }

        mba <- marginal_benefit_analysis(ben_result, assumptions)
        nmtr <- net_marginal_tax_rate(ben_result, assumptions)

        # Marginal IRR
        mirr <- safe_compute(marginal_irr(ben_result, assumptions), default = NULL)

        list(mba = mba, nmtr = nmtr, mirr = mirr)
      }, default = NULL)

      if (!is.null(marg) && !is.null(marg$nmtr)) {
        m_by_year[[as.character(by)]] <- list(
          ages = marg$nmtr$age,
          earnings = round(marg$nmtr$earnings, 0),
          nmtr = round(marg$nmtr$nmtr, 6),
          delta_pv = if (!is.null(marg$mba)) round(marg$mba$delta_pv_benefits, 0) else NULL,
          marginal_irr = if (!is.null(marg$mirr)) round(marg$mirr$marginal_irr, 6) else NULL
        )
      }
    }

    marginal_data[[scenario_name]] <- m_by_year
  }

  result <- list(
    meta = list(
      worker_type = config_key,
      worker_label = worker_config$label,
      sex = sex,
      claim_age = claim_age,
      custom_earnings = worker_config$custom
    ),
    data = marginal_data
  )

  write(toJSON(result, auto_unbox = TRUE, digits = 6, na = "null"), filepath)
}

# =============================================================================
# Build configuration grid and run
# =============================================================================

# Build all (worker_config, sex, claim_age) combinations
configs <- list()
for (wc in WORKER_CONFIGS) {
  if (!is.null(filter_type) && get_config_key(wc) != filter_type) next
  for (sex in SEXES) {
    for (ca in CLAIM_AGES) {
      configs[[length(configs) + 1]] <- list(worker = wc, sex = sex, claim_age = ca)
    }
  }
}

cat(sprintf("Total configurations: %d\n\n", length(configs)))

# =============================================================================
# Execute phases
# =============================================================================

run_phase <- function(phase_num, phase_fn, phase_label) {
  cat(sprintf("========== Phase %d: %s ==========\n", phase_num, phase_label))
  start_time <- Sys.time()

  if (n_cores > 1) {
    mclapply(configs, function(cfg) {
      phase_fn(cfg$worker, cfg$sex, cfg$claim_age)
    }, mc.cores = n_cores)
  } else {
    for (cfg in configs) {
      phase_fn(cfg$worker, cfg$sex, cfg$claim_age)
    }
  }

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  cat(sprintf("\n  Phase %d completed in %.1f minutes.\n\n", phase_num, elapsed))
}

if (phase %in% c("all", "1")) {
  run_phase(1, compute_cohort_file, "Cohort Data")
}

if (phase %in% c("all", "2")) {
  run_phase(2, compute_individual_files, "Individual Metrics + Benefits")
}

if (phase %in% c("all", "3")) {
  run_phase(3, compute_marginal_file, "Individual Marginal Analysis")
}

# =============================================================================
# Write manifest
# =============================================================================
cat("Writing manifest.json...\n")

manifest <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  package_version = as.character(packageVersion("ssmbar")),
  data_source = "2025 Trustees Report",
  dimensions = list(
    worker_types = lapply(WORKER_CONFIGS, function(wc) {
      list(key = get_config_key(wc), label = wc$label,
           type = wc$type, custom_earnings = wc$custom)
    }),
    sexes = SEXES,
    claim_ages = CLAIM_AGES,
    cohort_birth_years = range(COHORT_BIRTH_YEARS),
    individual_birth_years = range(INDIVIDUAL_BIRTH_YEARS)
  ),
  reform_scenarios = lapply(REFORM_SCENARIOS, function(s) {
    list(name = s$name, label = s$label)
  }),
  reform_categories = list(
    pia_formula = list(
      label = "Slow Initial Benefit Growth",
      reforms = c("reduce_fact3", "flat_benefit", "simpson_bowles_pia"),
      exclusive = TRUE
    ),
    nra = list(
      label = "Increase Retirement Age",
      reforms = c("nra_to_68", "index_nra", "nra_to_69_index"),
      exclusive = TRUE
    ),
    cola = list(
      label = "Modify COLAs",
      reforms = c("chained_cpi", "cola_cap", "cpi_e"),
      exclusive = TRUE
    ),
    taxmax = list(
      label = "Increase Taxable Maximum",
      reforms = c("taxmax_90_pct", "eliminate_taxmax", "eliminate_taxmax_no_credit"),
      exclusive = TRUE
    ),
    other = list(
      label = "Other Reforms",
      reforms = c("benefit_cut_5", "benefit_cut_10", "forty_year_avg",
                   "repeal_ret", "phase_out_spousal", "mini_pia"),
      exclusive = FALSE
    ),
    enhancements = list(
      label = "Benefit Enhancements",
      reforms = c("basic_minimum", "child_care_credit", "widow_75_pct"),
      exclusive = FALSE
    ),
    combo = list(
      label = "Combined Package",
      reforms = c("simpson_bowles_combo"),
      exclusive = FALSE
    )
  ),
  file_patterns = list(
    cohort = "cohort/{type}_{sex}_{claimAge}.json",
    individual_metrics = "individual/{type}_{sex}_{claimAge}_metrics.json",
    individual_benefits = "individual/{type}_{sex}_{claimAge}_benefits.json",
    individual_marginal = "individual/{type}_{sex}_{claimAge}_marginal.json"
  )
)

write(toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), file.path(out_dir, "manifest.json"))

cat("\n=== Pre-computation complete! ===\n")
cat(sprintf("Output: %s\n", out_dir))
