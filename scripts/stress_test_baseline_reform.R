# =============================================================================
# Stress Test: Baseline/Reform Separation
# =============================================================================
# This script tests whether the baseline/reform separation introduced any flaws
# by exercising all critical paths in the package.

devtools::load_all()
data(tr2025)
data(sef2025)

errors <- list()

cat("=============================================================================\n")
cat("STRESS TEST: Baseline/Reform Function Separation\n")
cat("=============================================================================\n\n")

# -----------------------------------------------------------------------------
# Test 1: Baseline pipeline (calculate_benefits)
# -----------------------------------------------------------------------------
cat("TEST 1: Baseline pipeline works correctly\n")
cat("-----------------------------------------------------------------------------\n")

tryCatch({
  worker <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  stopifnot("ben" %in% names(worker))
  stopifnot("wrk_ben" %in% names(worker))
  stopifnot(any(!is.na(worker$ben)))

  cat("  calculate_benefits() with medium worker - PASS\n")
  cat("TEST 1: PASSED\n\n")
}, error = function(e) {
  errors[["test1"]] <<- e$message
  cat("TEST 1: FAILED -", e$message, "\n\n")
})

# -----------------------------------------------------------------------------
# Test 2: Reform pipeline (calculate_benefits_reform)
# -----------------------------------------------------------------------------
cat("TEST 2: Reform pipeline works correctly\n")
cat("-----------------------------------------------------------------------------\n")

tryCatch({
  worker <- calculate_benefits_reform(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  stopifnot("ben" %in% names(worker))
  stopifnot("wrk_ben" %in% names(worker))
  stopifnot(any(!is.na(worker$ben)))

  cat("  calculate_benefits_reform() with medium worker - PASS\n")
  cat("TEST 2: PASSED\n\n")
}, error = function(e) {
  errors[["test2"]] <<- e$message
  cat("TEST 2: FAILED -", e$message, "\n\n")
})

# -----------------------------------------------------------------------------
# Test 3: calculate_benefits() vs calculate_benefits_reform() equivalence
# -----------------------------------------------------------------------------
cat("TEST 3: Baseline/Reform equivalence (no reform applied)\n")
cat("-----------------------------------------------------------------------------\n")

test_cases <- list(
  list(birth_yr = 1960, sex = "male", type = "low", age_claim = 62),
  list(birth_yr = 1965, sex = "female", type = "medium", age_claim = 65),
  list(birth_yr = 1970, sex = "male", type = "high", age_claim = 67),
  list(birth_yr = 1975, sex = "all", type = "max", age_claim = 70),
  list(birth_yr = 1980, sex = "female", type = "very_low", age_claim = 63),
  list(birth_yr = 1990, sex = "male", type = "medium", age_claim = 67),
  list(birth_yr = 2000, sex = "female", type = "high", age_claim = 68)
)

for (i in seq_along(test_cases)) {
  tc <- test_cases[[i]]
  tryCatch({
    baseline <- calculate_benefits(
      birth_yr = tc$birth_yr, sex = tc$sex, type = tc$type, age_claim = tc$age_claim,
      factors = sef2025, assumptions = tr2025
    )

    reform_null <- calculate_benefits_reform(
      birth_yr = tc$birth_yr, sex = tc$sex, type = tc$type, age_claim = tc$age_claim,
      factors = sef2025, assumptions = tr2025, reform = NULL
    )

    # Compare key columns
    if (!isTRUE(all.equal(baseline$ben, reform_null$ben, tolerance = 0.001))) {
      stop("ben mismatch")
    }
    if (!isTRUE(all.equal(baseline$wrk_ben, reform_null$wrk_ben, tolerance = 0.001))) {
      stop("wrk_ben mismatch")
    }

    cat(sprintf("  Case %d (birth=%d, type=%s, claim=%d) - PASS\n",
                i, tc$birth_yr, tc$type, tc$age_claim))
  }, error = function(e) {
    err_name <- paste0("test3_case", i)
    errors[[err_name]] <<- e$message
    cat(sprintf("  Case %d - FAILED: %s\n", i, e$message))
  })
}
cat("TEST 3: COMPLETED\n\n")

# -----------------------------------------------------------------------------
# Test 4: All reform templates work without errors
# -----------------------------------------------------------------------------
cat("TEST 4: All reform templates execute without errors\n")
cat("-----------------------------------------------------------------------------\n")

reform_templates <- list(
  "benefit_cut" = quote(reform_benefit_cut(cut_pct = 0.10, effective_year = 2030)),
  "reduce_fact3" = quote(reform_reduce_fact3(new_fact3 = 0.10, effective_year = 2030)),
  "nra_68" = quote(reform_raise_nra(target_nra = 68, start_cohort = 1960, end_cohort = 1970)),
  "chained_cpi" = quote(reform_chained_cpi(effective_year = 2025)),
  "40_year_avg" = quote(reform_40_year_averaging(effective_year = 2030)),
  "repeal_ret" = quote(reform_repeal_ret(effective_year = 2025)),
  "cola_cap" = quote(reform_cola_cap(cap_amount = 2000, effective_year = 2030)),
  "taxmax_90" = quote(reform_taxmax_90_pct(effective_year = 2025)),
  "basic_minimum" = quote(reform_basic_minimum(min_amount = 1000, effective_year = 2025)),
  "widow_75" = quote(reform_widow_75_pct(effective_year = 2025))
)

for (name in names(reform_templates)) {
  tryCatch({
    reform <- eval(reform_templates[[name]])

    result <- calculate_benefits_reform(
      birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
      factors = sef2025, assumptions = tr2025, reform = reform
    )

    stopifnot("ben" %in% names(result))
    stopifnot(any(!is.na(result$ben)))

    cat(sprintf("  %s - PASS\n", name))
  }, error = function(e) {
    err_name <- paste0("test4_", name)
    errors[[err_name]] <<- e$message
    cat(sprintf("  %s - FAILED: %s\n", name, e$message))
  })
}
cat("TEST 4: COMPLETED\n\n")

# -----------------------------------------------------------------------------
# Test 5: Spouse scenarios
# -----------------------------------------------------------------------------
cat("TEST 5: Spouse scenarios\n")
cat("-----------------------------------------------------------------------------\n")

spouse_cases <- list(
  list(type = "high", spouse_type = "low"),
  list(type = "medium", spouse_type = "medium"),
  list(type = "low", spouse_type = "high"),
  list(type = "max", spouse_type = "very_low")
)

for (i in seq_along(spouse_cases)) {
  sc <- spouse_cases[[i]]
  tryCatch({
    # Baseline
    baseline <- calculate_benefits(
      birth_yr = 1970, sex = "male", type = sc$type, age_claim = 67,
      factors = sef2025, assumptions = tr2025,
      spouse_type = sc$spouse_type, spouse_sex = "female",
      spouse_birth_yr = 1972, spouse_age_claim = 65
    )

    # Reform (no reform)
    reform_null <- calculate_benefits_reform(
      birth_yr = 1970, sex = "male", type = sc$type, age_claim = 67,
      factors = sef2025, assumptions = tr2025,
      spouse_type = sc$spouse_type, spouse_sex = "female",
      spouse_birth_yr = 1972, spouse_age_claim = 65
    )

    # Check equivalence
    stopifnot(isTRUE(all.equal(baseline$ben, reform_null$ben, tolerance = 0.001)))
    stopifnot(isTRUE(all.equal(baseline$spouse_ben, reform_null$spouse_ben, tolerance = 0.001)))

    cat(sprintf("  Case %d (worker=%s, spouse=%s) - PASS\n", i, sc$type, sc$spouse_type))
  }, error = function(e) {
    err_name <- paste0("test5_case", i)
    errors[[err_name]] <<- e$message
    cat(sprintf("  Case %d - FAILED: %s\n", i, e$message))
  })
}
cat("TEST 5: COMPLETED\n\n")

# -----------------------------------------------------------------------------
# Test 6: Edge cases
# -----------------------------------------------------------------------------
cat("TEST 6: Edge cases\n")
cat("-----------------------------------------------------------------------------\n")

edge_cases <- list(
  list(name = "Very early birth year", birth_yr = 1943, age_claim = 66),
  list(name = "Very late birth year", birth_yr = 2005, age_claim = 67),
  list(name = "Early claiming", birth_yr = 1970, age_claim = 62),
  list(name = "Late claiming", birth_yr = 1970, age_claim = 70),
  list(name = "Max earner early claim", birth_yr = 1965, age_claim = 62, type = "max"),
  list(name = "Very low earner late claim", birth_yr = 1975, age_claim = 70, type = "very_low")
)

for (ec in edge_cases) {
  tryCatch({
    type <- if (!is.null(ec$type)) ec$type else "medium"

    baseline <- calculate_benefits(
      birth_yr = ec$birth_yr, sex = "male", type = type, age_claim = ec$age_claim,
      factors = sef2025, assumptions = tr2025
    )

    reform_null <- calculate_benefits_reform(
      birth_yr = ec$birth_yr, sex = "male", type = type, age_claim = ec$age_claim,
      factors = sef2025, assumptions = tr2025
    )

    stopifnot(isTRUE(all.equal(baseline$ben, reform_null$ben, tolerance = 0.001)))

    cat(sprintf("  %s - PASS\n", ec$name))
  }, error = function(e) {
    err_name <- paste0("test6_", ec$name)
    errors[[err_name]] <<- e$message
    cat(sprintf("  %s - FAILED: %s\n", ec$name, e$message))
  })
}
cat("TEST 6: COMPLETED\n\n")

# -----------------------------------------------------------------------------
# Test 7: Analytical functions still work
# -----------------------------------------------------------------------------
cat("TEST 7: Analytical functions work with baseline output\n")
cat("-----------------------------------------------------------------------------\n")

tryCatch({
  worker <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  # Test calculate_taxes
  worker_taxes <- calculate_taxes(worker, tr2025)
  stopifnot("ss_tax" %in% names(worker_taxes))
  cat("  calculate_taxes() - PASS\n")

  # Test pv_lifetime_benefits
  pv_ben <- pv_lifetime_benefits(worker, tr2025)
  stopifnot(!is.na(pv_ben$pv_benefits))
  cat("  pv_lifetime_benefits() - PASS\n")

  # Test internal_rate_of_return
  irr <- internal_rate_of_return(worker, tr2025)
  stopifnot(!is.na(irr$irr))
  cat("  internal_rate_of_return() - PASS\n")

  # Test marginal_benefit_analysis
  if (!"claim_age" %in% names(worker)) worker$claim_age <- 67
  marginal <- marginal_benefit_analysis(worker, tr2025)
  stopifnot("delta_pv_benefits" %in% names(marginal))
  cat("  marginal_benefit_analysis() - PASS\n")

  cat("TEST 7: PASSED\n\n")
}, error = function(e) {
  errors[["test7"]] <<- e$message
  cat("TEST 7: FAILED -", e$message, "\n\n")
})

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("=============================================================================\n")
cat("STRESS TEST SUMMARY\n")
cat("=============================================================================\n")

if (length(errors) == 0) {
  cat("\nALL TESTS PASSED - No issues found with baseline/reform separation\n")
} else {
  cat(sprintf("\nFAILURES FOUND: %d errors\n", length(errors)))
  for (name in names(errors)) {
    cat(sprintf("  - %s: %s\n", name, errors[[name]]))
  }
}
cat("\n")
