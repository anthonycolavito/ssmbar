#!/usr/bin/env Rscript
# Parallel data generator - uses mclapply across configs
suppressWarnings(devtools::load_all(".", quiet = TRUE))
library(jsonlite)
library(parallel)

data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

ncores <- min(6L, detectCores() - 2L)
cat("Using", ncores, "cores\n")

out_dir <- "docs/data"
dir.create(file.path(out_dir, "cohort"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(out_dir, "individual"), showWarnings = FALSE, recursive = TRUE)

# Build reforms upfront (shared across all configs)
cat("Building reforms...\n")
reform_info <- list(
  baseline = list(label="Current Law", assumptions=tr2025),
  chained_cpi = list(label="Chained CPI", assumptions=apply_reform(tr2025, reform_chained_cpi(2030))),
  nra_to_68 = list(label="Raise NRA to 68", assumptions=apply_reform(tr2025, reform_nra_to_68(2030))),
  reduce_fact3 = list(label="Reduce Fact3 to 5%", assumptions=apply_reform(tr2025, reform_reduce_fact3(0.05, 2030))),
  taxmax_90_pct = list(label="90% Coverage + 5% Credit", assumptions=apply_reform(tr2025, reform_taxmax_90_pct(2030))),
  simpson_bowles_pia = list(label="Simpson-Bowles PIA", assumptions=apply_reform(tr2025, reform_simpson_bowles(2030, 10))),
  cpi_e = list(label="CPI-E (Higher)", assumptions=apply_reform(tr2025, reform_cpi_e(2030))),
  benefit_cut_5 = list(label="5% Benefit Cut", assumptions=apply_reform(tr2025, reform_reduce_benefits(0.95, 2030))),
  eliminate_taxmax_no_credit = list(label="Eliminate Taxmax, No Credit", assumptions=apply_reform(tr2025, reform_eliminate_taxmax_no_credit(2030))),
  flat_benefit = list(label="Flat Benefit", assumptions=apply_reform(tr2025, reform_flat_benefit(19300/12, 2030, 25))),
  index_nra = list(label="Index NRA to Longevity", assumptions=apply_reform(tr2025, reform_index_nra(2030))),
  nra_to_69_index = list(label="NRA to 69, then Index", assumptions=apply_reform(tr2025, reform_nra_to_69_index(2030))),
  cola_cap = list(label="Cap COLAs at Median", assumptions=apply_reform(tr2025, reform_cola_cap(2030))),
  eliminate_taxmax = list(label="Eliminate Taxmax + 15% Credit", assumptions=apply_reform(tr2025, reform_eliminate_taxmax(2030))),
  benefit_cut_10 = list(label="10% Benefit Cut", assumptions=apply_reform(tr2025, reform_reduce_benefits(0.90, 2030))),
  forty_year_avg = list(label="40-Year Averaging", assumptions=apply_reform(tr2025, reform_40_year_averaging(2030))),
  repeal_ret = list(label="Repeal RET", assumptions=apply_reform(tr2025, reform_repeal_ret(2030))),
  phase_out_spousal = list(label="Phase Out Spousal", assumptions=apply_reform(tr2025, reform_phase_out_spousal(2030))),
  mini_pia = list(label="Mini-PIA", assumptions=apply_reform(tr2025, reform_mini_pia(2030, 10))),
  basic_minimum = list(label="Basic Minimum Benefit", assumptions=apply_reform(tr2025, reform_basic_minimum(900, 1342, 2030))),
  child_care_credit = list(label="Child Care Credit", assumptions=apply_reform(tr2025, reform_child_care_credit(2030, 5))),
  widow_75_pct = list(label="75% Widow Benefit", assumptions=apply_reform(tr2025, reform_widow_75_pct(2030))),
  simpson_bowles_combo = list(label="Simpson-Bowles Package",
    assumptions=apply_reforms(tr2025, list(
      reform_simpson_bowles(2030, 10), reform_index_nra(2030),
      reform_chained_cpi(2030), reform_taxmax_90_pct(2030)
    ), check_exclusivity=FALSE))
)
scenario_names <- names(reform_info)
cat(length(scenario_names), "scenarios ready.\n")

# Build config list
worker_types <- c("very_low", "low", "medium", "high", "max")
worker_labels <- c("Very Low Earner", "Low Earner", "Medium Earner", "High Earner", "Maximum Earner")
sexes <- c("male", "female")
claim_ages <- 65L
cohort_bys <- 1960:2005
indiv_bys <- 1960:2005

configs <- list()
for (wi in seq_along(worker_types)) {
  for (sex in sexes) {
    for (claim_age in claim_ages) {
      configs[[length(configs) + 1]] <- list(
        wtype = worker_types[wi], wlabel = worker_labels[wi],
        sex = sex, claim_age = claim_age
      )
    }
  }
}

# Filter to only configs that still need work
needs_work <- sapply(configs, function(cfg) {
  coh_file <- file.path(out_dir, "cohort", sprintf("%s_%s_%d.json", cfg$wtype, cfg$sex, cfg$claim_age))
  met_file <- file.path(out_dir, "individual", sprintf("%s_%s_%d_metrics.json", cfg$wtype, cfg$sex, cfg$claim_age))
  !file.exists(coh_file) || !file.exists(met_file)
})
configs <- configs[needs_work]
cat(length(configs), "configs remaining (skipping already-generated files).\n\n")

if (length(configs) == 0) {
  cat("All files already exist. Nothing to do.\n")
  quit(save = "no")
}

t_start <- Sys.time()

process_config <- function(cfg) {
  wtype <- cfg$wtype; wlabel <- cfg$wlabel; sex <- cfg$sex; claim_age <- cfg$claim_age
  tag <- sprintf("%s_%s_%d", wtype, sex, claim_age)
  t0 <- Sys.time()

  # === COHORT FILE ===
  coh_file <- file.path(out_dir, "cohort", sprintf("%s.json", tag))
  if (!file.exists(coh_file)) {
    coh_data <- list()
    for (sn in scenario_names) {
      assum <- reform_info[[sn]]$assumptions
      is_base <- sn == "baseline"

      bys <- integer(0)
      mb <- numeric(0); pvb <- numeric(0); pvt <- numeric(0)
      rat <- numeric(0); irr_v <- numeric(0)
      rr1 <- numeric(0); rr2 <- numeric(0); rr3 <- numeric(0)

      for (by in cohort_bys) {
        bys <- c(bys, by)
        ben <- tryCatch({
          if (is_base)
            calculate_benefits(birth_yr=by, sex=sex, type=wtype, age_claim=claim_age,
              factors=sef2025, assumptions=tr2025, debugg=TRUE)
          else
            calculate_benefits_reform(birth_yr=by, sex=sex, type=wtype, age_claim=claim_age,
              factors=sef2025, assumptions=assum, debugg=TRUE)
        }, error = function(e) NULL)

        if (is.null(ben)) {
          mb <- c(mb, NA); pvb <- c(pvb, NA); pvt <- c(pvt, NA)
          rat <- c(rat, NA); irr_v <- c(irr_v, NA)
          rr1 <- c(rr1, NA); rr2 <- c(rr2, NA); rr3 <- c(rr3, NA)
          next
        }

        b <- ben$ben[ben$age == claim_age]
        mb <- c(mb, if (length(b) > 0) b[1] else NA)

        pb <- tryCatch(pv_lifetime_benefits(ben, assum)$pv_benefits[1], error=function(e) NA)
        pt <- tryCatch(pv_lifetime_taxes(ben, assum, include_employer=TRUE)$pv_taxes[1], error=function(e) NA)
        pvb <- c(pvb, pb); pvt <- c(pvt, pt)
        rat <- c(rat, if (!is.na(pb) && !is.na(pt) && pt > 0) pb/pt else NA)

        iv <- tryCatch(internal_rate_of_return(ben, assum, include_employer=TRUE)$irr[1], error=function(e) NA)
        irr_v <- c(irr_v, iv)

        rr <- tryCatch(rep_rates(ben, assum), error=function(e) NULL)
        if (!is.null(rr) && is.data.frame(rr)) {
          rr1 <- c(rr1, if ("pv_rr" %in% rr$type) rr$rep_rate[rr$type=="pv_rr"][1] else NA)
          rr2 <- c(rr2, if ("wage_h35" %in% rr$type) rr$rep_rate[rr$type=="wage_h35"][1] else NA)
          rr3 <- c(rr3, if ("real_h35" %in% rr$type) rr$rep_rate[rr$type=="real_h35"][1] else NA)
        } else {
          rr1 <- c(rr1, NA); rr2 <- c(rr2, NA); rr3 <- c(rr3, NA)
        }
      }

      coh_data[[sn]] <- list(
        birth_years=bys,
        monthly_benefit=round(mb, 2), pv_benefits=round(pvb, 0), pv_taxes=round(pvt, 0),
        ratio=round(rat, 4), irr=round(irr_v, 6),
        repl_rate_pv=round(rr1, 6), repl_rate_wage_h35=round(rr2, 6), repl_rate_real_h35=round(rr3, 6)
      )
    }

    result <- list(
      meta=list(worker_type=wtype, worker_label=wlabel, sex=sex, claim_age=claim_age),
      data=coh_data
    )
    writeLines(toJSON(result, auto_unbox=TRUE, digits=6, na="null"), coh_file)
  }

  # === INDIVIDUAL FILES ===
  met_file <- file.path(out_dir, "individual", sprintf("%s_metrics.json", tag))
  ben_file <- file.path(out_dir, "individual", sprintf("%s_benefits.json", tag))
  if (!file.exists(met_file)) {
    met_data <- list()
    ben_data <- list()

    for (sn in scenario_names) {
      assum <- reform_info[[sn]]$assumptions
      is_base <- sn == "baseline"

      bys <- integer(0)
      mb <- numeric(0); pvb <- numeric(0); pvt <- numeric(0)
      rat <- numeric(0); irr_v <- numeric(0)
      b_by_year <- list()

      for (by in indiv_bys) {
        bys <- c(bys, by)
        ben <- tryCatch({
          if (is_base)
            calculate_benefits(birth_yr=by, sex=sex, type=wtype, age_claim=claim_age,
              factors=sef2025, assumptions=tr2025, debugg=TRUE)
          else
            calculate_benefits_reform(birth_yr=by, sex=sex, type=wtype, age_claim=claim_age,
              factors=sef2025, assumptions=assum, debugg=TRUE)
        }, error = function(e) NULL)

        if (is.null(ben)) {
          mb <- c(mb, NA); pvb <- c(pvb, NA); pvt <- c(pvt, NA)
          rat <- c(rat, NA); irr_v <- c(irr_v, NA)
          next
        }

        b <- ben$ben[ben$age == claim_age]
        mb <- c(mb, if (length(b) > 0) b[1] else NA)

        pb <- tryCatch(pv_lifetime_benefits(ben, assum)$pv_benefits[1], error=function(e) NA)
        pt <- tryCatch(pv_lifetime_taxes(ben, assum, include_employer=TRUE)$pv_taxes[1], error=function(e) NA)
        pvb <- c(pvb, pb); pvt <- c(pvt, pt)
        rat <- c(rat, if (!is.na(pb) && !is.na(pt) && pt > 0) pb/pt else NA)

        iv <- tryCatch(internal_rate_of_return(ben, assum, include_employer=TRUE)$irr[1], error=function(e) NA)
        irr_v <- c(irr_v, iv)

        # Benefit series
        rows <- ben[ben$age >= claim_age & !is.na(ben$annual_ind), ]
        if (nrow(rows) > 0) {
          cpi_c <- assum$gdp_pi[assum$year == (by + claim_age)]
          real_b <- if (length(cpi_c) > 0 && !is.na(cpi_c)) {
            sapply(seq_len(nrow(rows)), function(j) {
              cpi_y <- assum$gdp_pi[assum$year == rows$year[j]]
              if (length(cpi_y) > 0 && !is.na(cpi_y) && cpi_c > 0)
                round(rows$annual_ind[j] * cpi_c / cpi_y, 0)
              else rows$annual_ind[j]
            })
          } else rows$annual_ind

          b_by_year[[as.character(by)]] <- list(
            ages=rows$age, nominal=round(rows$annual_ind, 0),
            real=round(real_b, 0), earnings=round(rows$earnings, 0),
            bc=as.character(rows$bc)
          )
        }
      }

      met_data[[sn]] <- list(
        birth_years=bys, monthly_benefit=round(mb, 2),
        pv_benefits=round(pvb, 0), pv_taxes=round(pvt, 0),
        ratio=round(rat, 4), irr=round(irr_v, 6)
      )
      ben_data[[sn]] <- b_by_year
    }

    meta <- list(worker_type=wtype, worker_label=wlabel, sex=sex, claim_age=claim_age)
    writeLines(toJSON(list(meta=meta, data=met_data), auto_unbox=TRUE, digits=6, na="null"), met_file)
    writeLines(toJSON(list(meta=meta, data=ben_data), auto_unbox=TRUE, digits=6, na="null"), ben_file)
  }

  elapsed <- as.numeric(difftime(Sys.time(), t0, units="secs"))
  sprintf("%s done in %.0fs", tag, elapsed)
}

cat(sprintf("Starting %d configs on %d cores...\n", length(configs), ncores))
results <- mclapply(configs, process_config, mc.cores = ncores)

# Print results
for (r in results) {
  if (is.character(r)) cat(r, "\n")
  else cat("ERROR:", paste(r, collapse=" "), "\n")
}

elapsed_total <- as.numeric(difftime(Sys.time(), t_start, units="mins"))
cat(sprintf("\n=== Done! Total time: %.1f minutes ===\n", elapsed_total))
cat("Cohort files:", length(list.files(file.path(out_dir, "cohort"), pattern="\\.json$")), "\n")
cat("Individual files:", length(list.files(file.path(out_dir, "individual"), pattern="\\.json$")), "\n")
