#!/usr/bin/env Rscript
# =============================================================================
# Fast Pre-computation — Generate demo data for GitHub Pages site
# =============================================================================
# Generates cohort + individual data for a focused subset:
#   - 5 standard worker types (no custom earnings)
#   - 2 sexes
#   - 3 claim ages (62, 67, 70)
#   - Baseline + 5 key reforms (not all 22)
#   - Cohort: every 5th birth year (10 points instead of 46)
#   - Individual: every 5th birth year (15 points instead of 72)
#
# Run time estimate: ~15-30 min on 4 cores
# =============================================================================

# Load ssmbar
pkg_root <- getwd()
if (basename(pkg_root) == "scripts") pkg_root <- dirname(pkg_root)
if (requireNamespace("ssmbar", quietly = TRUE)) {
  library(ssmbar)
} else {
  suppressWarnings(devtools::load_all(pkg_root, quiet = TRUE))
}
library(jsonlite)
library(parallel)

data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

# Output
out_dir <- file.path(pkg_root, "docs", "data")
dir.create(file.path(out_dir, "cohort"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "individual"), showWarnings = FALSE, recursive = TRUE)

# Parse args
args <- commandArgs(trailingOnly = TRUE)
n_cores <- 1
i <- 1
while (i <= length(args)) {
  if (args[i] == "--cores" && i < length(args)) {
    n_cores <- as.integer(args[i + 1]); i <- i + 2
  } else { i <- i + 1 }
}

cat("=== Fast Pre-computation ===\n")
cat(sprintf("  Cores: %d\n\n", n_cores))

# =============================================================================
# Dimensions (reduced for speed)
# =============================================================================
WORKER_TYPES <- c("very_low", "low", "medium", "high", "max")
WORKER_LABELS <- c("Very Low Earner", "Low Earner", "Medium Earner", "High Earner", "Maximum Earner")
SEXES <- c("male", "female")
CLAIM_AGES <- 62:70  # All claim ages

# Cohort: all birth years (needed for charts to look good)
COHORT_BIRTH_YEARS <- 1960:2005

# Individual: all birth years
INDIVIDUAL_BIRTH_YEARS <- 1960:2005  # Narrower range for speed

# Key reforms (6 total including baseline)
REFORM_LIST <- list(
  baseline = list(name = "baseline", label = "Current Law", fn = NULL),
  chained_cpi = list(name = "chained_cpi", label = "Chained CPI",
    fn = function() reform_chained_cpi(2030)),
  nra_to_68 = list(name = "nra_to_68", label = "Raise NRA to 68",
    fn = function() reform_nra_to_68(2030)),
  reduce_fact3 = list(name = "reduce_fact3", label = "Reduce Fact3 to 5%",
    fn = function() reform_reduce_fact3(0.05, 2030)),
  taxmax_90_pct = list(name = "taxmax_90_pct", label = "90% Coverage + 5% Credit",
    fn = function() reform_taxmax_90_pct(2030)),
  simpson_bowles_pia = list(name = "simpson_bowles_pia", label = "Simpson-Bowles PIA",
    fn = function() reform_simpson_bowles(2030, 10)),
  cpi_e = list(name = "cpi_e", label = "CPI-E (Higher)",
    fn = function() reform_cpi_e(2030)),
  benefit_cut_5 = list(name = "benefit_cut_5", label = "5% Benefit Cut",
    fn = function() reform_reduce_benefits(0.95, 2030)),
  eliminate_taxmax_no_credit = list(name = "eliminate_taxmax_no_credit", label = "Eliminate Taxmax, No Credit",
    fn = function() reform_eliminate_taxmax_no_credit(2030)),
  flat_benefit = list(name = "flat_benefit", label = "Flat Benefit",
    fn = function() reform_flat_benefit(19300/12, 2030, 25)),
  index_nra = list(name = "index_nra", label = "Index NRA to Longevity",
    fn = function() reform_index_nra(2030)),
  nra_to_69_index = list(name = "nra_to_69_index", label = "NRA to 69, then Index",
    fn = function() reform_nra_to_69_index(2030)),
  cola_cap = list(name = "cola_cap", label = "Cap COLAs at Median",
    fn = function() reform_cola_cap(2030)),
  eliminate_taxmax = list(name = "eliminate_taxmax", label = "Eliminate Taxmax + 15% Credit",
    fn = function() reform_eliminate_taxmax(2030)),
  benefit_cut_10 = list(name = "benefit_cut_10", label = "10% Benefit Cut",
    fn = function() reform_reduce_benefits(0.90, 2030)),
  forty_year_avg = list(name = "forty_year_avg", label = "40-Year Averaging",
    fn = function() reform_40_year_averaging(2030)),
  repeal_ret = list(name = "repeal_ret", label = "Repeal RET",
    fn = function() reform_repeal_ret(2030)),
  phase_out_spousal = list(name = "phase_out_spousal", label = "Phase Out Spousal",
    fn = function() reform_phase_out_spousal(2030)),
  mini_pia = list(name = "mini_pia", label = "Mini-PIA",
    fn = function() reform_mini_pia(2030, 10)),
  basic_minimum = list(name = "basic_minimum", label = "Basic Minimum Benefit",
    fn = function() reform_basic_minimum(900, 1342, 2030)),
  child_care_credit = list(name = "child_care_credit", label = "Child Care Credit",
    fn = function() reform_child_care_credit(2030, 5)),
  widow_75_pct = list(name = "widow_75_pct", label = "75% Widow Benefit",
    fn = function() reform_widow_75_pct(2030)),
  simpson_bowles_combo = list(name = "simpson_bowles_combo", label = "Simpson-Bowles Package",
    fn = function() list(
      reform_simpson_bowles(2030, 10),
      reform_index_nra(2030),
      reform_chained_cpi(2030),
      reform_taxmax_90_pct(2030)
    ))
)

# Build reform assumptions
cat("Building reform assumptions...\n")
REFORM_ASSUMPTIONS <- list()
for (rn in names(REFORM_LIST)) {
  scenario <- REFORM_LIST[[rn]]
  if (is.null(scenario$fn)) {
    REFORM_ASSUMPTIONS[[rn]] <- tr2025
  } else {
    obj <- scenario$fn()
    if (is.list(obj) && !inherits(obj, "Reform")) {
      REFORM_ASSUMPTIONS[[rn]] <- apply_reforms(tr2025, obj, check_exclusivity = FALSE)
    } else {
      REFORM_ASSUMPTIONS[[rn]] <- apply_reform(tr2025, obj)
    }
  }
}
cat(sprintf("  %d reform scenarios ready.\n\n", length(REFORM_ASSUMPTIONS)))

# =============================================================================
# Compute one cohort file
# =============================================================================
compute_one_cohort <- function(worker_type, worker_label, sex, claim_age) {
  filename <- sprintf("%s_%s_%d.json", worker_type, sex, claim_age)
  filepath <- file.path(out_dir, "cohort", filename)

  if (file.exists(filepath)) return(invisible(NULL))

  cat(sprintf("  cohort/%s ...", filename))
  t0 <- Sys.time()

  data_out <- list()

  for (sn in names(REFORM_LIST)) {
    assumptions <- REFORM_ASSUMPTIONS[[sn]]
    is_base <- sn == "baseline"

    by_vec <- numeric(0)
    mb_vec <- numeric(0)
    pvb_vec <- numeric(0)
    pvt_vec <- numeric(0)
    rat_vec <- numeric(0)
    irr_vec <- numeric(0)
    rr_pv_vec <- numeric(0)
    rr_wh35_vec <- numeric(0)
    rr_rh35_vec <- numeric(0)

    for (by in COHORT_BIRTH_YEARS) {
      ben <- tryCatch({
        if (is_base) {
          calculate_benefits(birth_yr=by, sex=sex, type=worker_type,
            age_claim=claim_age, factors=sef2025, assumptions=tr2025,
            debugg=TRUE)
        } else {
          calculate_benefits_reform(birth_yr=by, sex=sex, type=worker_type,
            age_claim=claim_age, factors=sef2025, assumptions=assumptions,
            debugg=TRUE)
        }
      }, error = function(e) NULL)

      by_vec <- c(by_vec, by)

      if (is.null(ben)) {
        mb_vec <- c(mb_vec, NA_real_)
        pvb_vec <- c(pvb_vec, NA_real_)
        pvt_vec <- c(pvt_vec, NA_real_)
        rat_vec <- c(rat_vec, NA_real_)
        irr_vec <- c(irr_vec, NA_real_)
        rr_pv_vec <- c(rr_pv_vec, NA_real_)
        rr_wh35_vec <- c(rr_wh35_vec, NA_real_)
        rr_rh35_vec <- c(rr_rh35_vec, NA_real_)
        next
      }

      # Monthly benefit
      b <- ben$ben[ben$age == claim_age]
      mb_vec <- c(mb_vec, if (length(b) > 0) b[1] else NA_real_)

      # PV
      pvb <- tryCatch(pv_lifetime_benefits(ben, assumptions)$pv_benefits[1], error=function(e) NA_real_)
      pvt <- tryCatch(pv_lifetime_taxes(ben, assumptions, include_employer=TRUE)$pv_taxes[1], error=function(e) NA_real_)
      pvb_vec <- c(pvb_vec, pvb)
      pvt_vec <- c(pvt_vec, pvt)
      rat_vec <- c(rat_vec, if (!is.na(pvb) && !is.na(pvt) && pvt > 0) pvb/pvt else NA_real_)

      # IRR
      irr_val <- tryCatch(internal_rate_of_return(ben, assumptions, include_employer=TRUE)$irr[1], error=function(e) NA_real_)
      irr_vec <- c(irr_vec, irr_val)

      # Replacement rates
      rr <- tryCatch(rep_rates(ben, assumptions), error=function(e) NULL)
      if (!is.null(rr) && is.data.frame(rr)) {
        rr_pv_vec <- c(rr_pv_vec, if ("pv_rr" %in% rr$type) rr$rep_rate[rr$type=="pv_rr"][1] else NA_real_)
        rr_wh35_vec <- c(rr_wh35_vec, if ("wage_h35" %in% rr$type) rr$rep_rate[rr$type=="wage_h35"][1] else NA_real_)
        rr_rh35_vec <- c(rr_rh35_vec, if ("real_h35" %in% rr$type) rr$rep_rate[rr$type=="real_h35"][1] else NA_real_)
      } else {
        rr_pv_vec <- c(rr_pv_vec, NA_real_)
        rr_wh35_vec <- c(rr_wh35_vec, NA_real_)
        rr_rh35_vec <- c(rr_rh35_vec, NA_real_)
      }
    }

    data_out[[sn]] <- list(
      birth_years = by_vec,
      monthly_benefit = round(mb_vec, 2),
      pv_benefits = round(pvb_vec, 0),
      pv_taxes = round(pvt_vec, 0),
      ratio = round(rat_vec, 4),
      irr = round(irr_vec, 6),
      repl_rate_pv = round(rr_pv_vec, 6),
      repl_rate_wage_h35 = round(rr_wh35_vec, 6),
      repl_rate_real_h35 = round(rr_rh35_vec, 6)
    )
  }

  result <- list(
    meta = list(worker_type=worker_type, worker_label=worker_label,
                sex=sex, claim_age=claim_age, custom_earnings=NULL),
    data = data_out
  )

  writeLines(toJSON(result, auto_unbox=TRUE, digits=6, na="null"), filepath)
  cat(sprintf(" done (%.0fs)\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))
}

# =============================================================================
# Compute one individual metrics file
# =============================================================================
compute_one_individual <- function(worker_type, worker_label, sex, claim_age) {
  metrics_file <- sprintf("%s_%s_%d_metrics.json", worker_type, sex, claim_age)
  benefits_file <- sprintf("%s_%s_%d_benefits.json", worker_type, sex, claim_age)
  metrics_path <- file.path(out_dir, "individual", metrics_file)
  benefits_path <- file.path(out_dir, "individual", benefits_file)

  if (file.exists(metrics_path) && file.exists(benefits_path)) return(invisible(NULL))

  cat(sprintf("  individual/%s ...", metrics_file))
  t0 <- Sys.time()

  metrics_data <- list()
  benefits_data <- list()

  for (sn in names(REFORM_LIST)) {
    assumptions <- REFORM_ASSUMPTIONS[[sn]]
    is_base <- sn == "baseline"

    by_vec <- numeric(0)
    mb_vec <- numeric(0)
    pvb_vec <- numeric(0)
    pvt_vec <- numeric(0)
    rat_vec <- numeric(0)
    irr_vec <- numeric(0)
    b_by_year <- list()

    for (by in INDIVIDUAL_BIRTH_YEARS) {
      ben <- tryCatch({
        if (is_base) {
          calculate_benefits(birth_yr=by, sex=sex, type=worker_type,
            age_claim=claim_age, factors=sef2025, assumptions=tr2025,
            debugg=TRUE)
        } else {
          calculate_benefits_reform(birth_yr=by, sex=sex, type=worker_type,
            age_claim=claim_age, factors=sef2025, assumptions=assumptions,
            debugg=TRUE)
        }
      }, error = function(e) NULL)

      by_vec <- c(by_vec, by)

      if (is.null(ben)) {
        mb_vec <- c(mb_vec, NA_real_)
        pvb_vec <- c(pvb_vec, NA_real_)
        pvt_vec <- c(pvt_vec, NA_real_)
        rat_vec <- c(rat_vec, NA_real_)
        irr_vec <- c(irr_vec, NA_real_)
        next
      }

      b <- ben$ben[ben$age == claim_age]
      mb_vec <- c(mb_vec, if (length(b) > 0) b[1] else NA_real_)

      pvb <- tryCatch(pv_lifetime_benefits(ben, assumptions)$pv_benefits[1], error=function(e) NA_real_)
      pvt <- tryCatch(pv_lifetime_taxes(ben, assumptions, include_employer=TRUE)$pv_taxes[1], error=function(e) NA_real_)
      pvb_vec <- c(pvb_vec, pvb)
      pvt_vec <- c(pvt_vec, pvt)
      rat_vec <- c(rat_vec, if (!is.na(pvb) && !is.na(pvt) && pvt > 0) pvb/pvt else NA_real_)

      irr_val <- tryCatch(internal_rate_of_return(ben, assumptions, include_employer=TRUE)$irr[1], error=function(e) NA_real_)
      irr_vec <- c(irr_vec, irr_val)

      # Benefit series
      rows <- ben[ben$age >= claim_age & !is.na(ben$annual_ind), ]
      if (nrow(rows) > 0) {
        cpi_claim <- assumptions$gdp_pi[assumptions$year == (by + claim_age)]
        real_ben <- if (length(cpi_claim) > 0 && !is.na(cpi_claim)) {
          sapply(seq_len(nrow(rows)), function(j) {
            cpi_yr <- assumptions$gdp_pi[assumptions$year == rows$year[j]]
            if (length(cpi_yr) > 0 && !is.na(cpi_yr) && cpi_claim > 0)
              round(rows$annual_ind[j] * cpi_claim / cpi_yr, 0)
            else rows$annual_ind[j]
          })
        } else {
          rows$annual_ind
        }

        b_by_year[[as.character(by)]] <- list(
          ages = rows$age,
          nominal = round(rows$annual_ind, 0),
          real = round(real_ben, 0),
          earnings = round(rows$earnings, 0),
          bc = as.character(rows$bc)
        )
      }
    }

    metrics_data[[sn]] <- list(
      birth_years = by_vec,
      monthly_benefit = round(mb_vec, 2),
      pv_benefits = round(pvb_vec, 0),
      pv_taxes = round(pvt_vec, 0),
      ratio = round(rat_vec, 4),
      irr = round(irr_vec, 6)
    )
    benefits_data[[sn]] <- b_by_year
  }

  meta <- list(worker_type=worker_type, worker_label=worker_label,
               sex=sex, claim_age=claim_age, custom_earnings=NULL)

  writeLines(toJSON(list(meta=meta, data=metrics_data), auto_unbox=TRUE, digits=6, na="null"), metrics_path)
  writeLines(toJSON(list(meta=meta, data=benefits_data), auto_unbox=TRUE, digits=6, na="null"), benefits_path)

  cat(sprintf(" done (%.0fs)\n", as.numeric(difftime(Sys.time(), t0, units="secs"))))
}

# =============================================================================
# Build config grid and run
# =============================================================================
configs <- list()
for (wi in seq_along(WORKER_TYPES)) {
  for (sex in SEXES) {
    for (ca in CLAIM_AGES) {
      configs[[length(configs) + 1]] <- list(
        type = WORKER_TYPES[wi], label = WORKER_LABELS[wi],
        sex = sex, claim_age = ca
      )
    }
  }
}

cat(sprintf("Configurations: %d\n", length(configs)))
cat(sprintf("Scenarios per config: %d\n", length(REFORM_LIST)))
cat(sprintf("Cohort birth years: %d\n", length(COHORT_BIRTH_YEARS)))
cat(sprintf("Individual birth years: %d\n\n", length(INDIVIDUAL_BIRTH_YEARS)))

# Phase 1: Cohort
cat("=== Phase 1: Cohort Data ===\n")
t1 <- Sys.time()
if (n_cores > 1) {
  mclapply(configs, function(cfg) {
    compute_one_cohort(cfg$type, cfg$label, cfg$sex, cfg$claim_age)
  }, mc.cores = n_cores)
} else {
  for (cfg in configs) {
    compute_one_cohort(cfg$type, cfg$label, cfg$sex, cfg$claim_age)
  }
}
cat(sprintf("\nPhase 1 done in %.1f minutes.\n\n", as.numeric(difftime(Sys.time(), t1, units="mins"))))

# Phase 2: Individual metrics + benefits
cat("=== Phase 2: Individual Metrics + Benefits ===\n")
t2 <- Sys.time()
if (n_cores > 1) {
  mclapply(configs, function(cfg) {
    compute_one_individual(cfg$type, cfg$label, cfg$sex, cfg$claim_age)
  }, mc.cores = n_cores)
} else {
  for (cfg in configs) {
    compute_one_individual(cfg$type, cfg$label, cfg$sex, cfg$claim_age)
  }
}
cat(sprintf("\nPhase 2 done in %.1f minutes.\n\n", as.numeric(difftime(Sys.time(), t2, units="mins"))))

# Update manifest with actual computed dimensions
manifest <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  package_version = as.character(packageVersion("ssmbar")),
  data_source = "2025 Trustees Report",
  dimensions = list(
    worker_types = lapply(seq_along(WORKER_TYPES), function(i) {
      list(key=WORKER_TYPES[i], label=WORKER_LABELS[i], type=WORKER_TYPES[i], custom_earnings=NULL)
    }),
    sexes = SEXES,
    claim_ages = CLAIM_AGES,
    cohort_birth_years = range(COHORT_BIRTH_YEARS),
    individual_birth_years = range(INDIVIDUAL_BIRTH_YEARS)
  ),
  reform_scenarios = lapply(REFORM_LIST, function(s) list(name=s$name, label=s$label)),
  reform_categories = list(
    pia_formula = list(label="Slow Initial Benefit Growth",
      reforms=c("reduce_fact3","flat_benefit","simpson_bowles_pia"), exclusive=TRUE),
    nra = list(label="Increase Retirement Age",
      reforms=c("nra_to_68","index_nra","nra_to_69_index"), exclusive=TRUE),
    cola = list(label="Modify COLAs",
      reforms=c("chained_cpi","cola_cap","cpi_e"), exclusive=TRUE),
    taxmax = list(label="Increase Taxable Maximum",
      reforms=c("taxmax_90_pct","eliminate_taxmax","eliminate_taxmax_no_credit"), exclusive=TRUE),
    other = list(label="Other Reforms",
      reforms=c("benefit_cut_5","benefit_cut_10","forty_year_avg","repeal_ret","phase_out_spousal","mini_pia"), exclusive=FALSE),
    enhancements = list(label="Benefit Enhancements",
      reforms=c("basic_minimum","child_care_credit","widow_75_pct"), exclusive=FALSE),
    combo = list(label="Combined Package",
      reforms=c("simpson_bowles_combo"), exclusive=FALSE)
  ),
  file_patterns = list(
    cohort = "cohort/{type}_{sex}_{claimAge}.json",
    individual_metrics = "individual/{type}_{sex}_{claimAge}_metrics.json",
    individual_benefits = "individual/{type}_{sex}_{claimAge}_benefits.json",
    individual_marginal = "individual/{type}_{sex}_{claimAge}_marginal.json"
  )
)

writeLines(toJSON(manifest, auto_unbox=TRUE, pretty=TRUE), file.path(out_dir, "manifest.json"))
cat("Manifest updated.\n")
cat(sprintf("\n=== Total time: %.1f minutes ===\n", as.numeric(difftime(Sys.time(), t1, units="mins"))))
