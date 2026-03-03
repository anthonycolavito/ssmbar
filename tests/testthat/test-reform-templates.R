# =============================================================================
# Tests for Reform Template Functions
# =============================================================================
#
# These tests verify that each reform template function creates valid Reform
# objects with correct parameter specifications.
#
# =============================================================================

library(testthat)

# -----------------------------------------------------------------------------
# Test Tier 1 Reform Templates
# -----------------------------------------------------------------------------

test_that("reform_reduce_benefits() creates valid Reform", {
  reform <- reform_reduce_benefits(multiplier = 0.95, effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "5.0% Benefit Reduction")
  expect_equal(reform$effective_year, 2030)
  expect_length(reform$parameters, 1)
  expect_equal(reform$parameters[[1]]$param, "pia_multiplier")
  expect_equal(reform$parameters[[1]]$value, 0.95)
  expect_equal(reform$parameters[[1]]$type, "replace")
})

test_that("reform_reduce_benefits() handles increases", {
  reform <- reform_reduce_benefits(multiplier = 1.10, effective_year = 2030)

  expect_equal(reform$name, "10.0% Benefit Increase")
})

test_that("reform_reduce_benefits() validates multiplier", {
  expect_error(
    reform_reduce_benefits(multiplier = 0, effective_year = 2030),
    "'multiplier' must be positive"
  )

  expect_error(
    reform_reduce_benefits(multiplier = -0.5, effective_year = 2030),
    "'multiplier' must be positive"
  )
})

test_that("reform_reduce_fact3() creates valid Reform", {
  reform <- reform_reduce_fact3(target_fact3 = 0.05, effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_length(reform$parameters, 1)
  expect_equal(reform$parameters[[1]]$param, "fact3")
  expect_equal(reform$parameters[[1]]$value, 0.05)
  expect_equal(reform$phase_in_years, 10)  # Default
})

test_that("reform_nra_to_68() creates valid Reform with function value", {
  reform <- reform_nra_to_68(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Raise NRA to 68")
  expect_equal(reform$parameters[[1]]$param, "nra")
  expect_true(is.function(reform$parameters[[1]]$value))

  # Test the NRA function
  nra_func <- reform$parameters[[1]]$value

  # Before effective year
  expect_equal(nra_func(2029), 67)

  # At effective year
  expect_equal(nra_func(2030), 67)

  # After effective year - should increase by 1/12 per 2 years
  expect_equal(nra_func(2032), 67 + 1/12)  # 1 month increase after 2 cohorts
  expect_equal(nra_func(2034), 67 + 2/12)  # 2 months after 4 cohorts

  # Capped at 68
  expect_equal(nra_func(2060), 68)
})

test_that("reform_index_nra() creates valid Reform without cap", {
  reform <- reform_index_nra(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Index NRA to Longevity")

  nra_func <- reform$parameters[[1]]$value

  # Should not be capped at 68
  expect_true(nra_func(2100) > 68)
})

test_that("reform_nra_to_69_index() creates valid Reform with two phases", {
  reform <- reform_nra_to_69_index(effective_year = 2030)

  expect_s3_class(reform, "Reform")

  nra_func <- reform$parameters[[1]]$value

  # Phase 1: 2 months per year until 69
  expect_equal(nra_func(2030), 67)
  expect_equal(nra_func(2031), 67 + 2/12)  # +2 months
  expect_equal(nra_func(2032), 67 + 4/12)  # +4 months

  # After 12 years should reach 69
  expect_equal(nra_func(2042), 69)

  # Phase 2: 1 month per 2 years after 69
  expect_equal(nra_func(2044), 69 + 1/12)  # 1 month after 2 more cohorts
})

test_that("reform_chained_cpi() creates valid Reform", {
  reform <- reform_chained_cpi(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Index COLAs to Chained CPI")
  expect_equal(reform$parameters[[1]]$param, "cola")
  expect_equal(reform$parameters[[1]]$value, -0.3)
  expect_equal(reform$parameters[[1]]$type, "add")
})

test_that("reform_cpi_e() creates valid Reform", {
  reform <- reform_cpi_e(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Index COLAs to CPI-E")
  expect_equal(reform$parameters[[1]]$param, "cola")
  expect_equal(reform$parameters[[1]]$value, 0.2)
  expect_equal(reform$parameters[[1]]$type, "add")
})

test_that("reform_change_tax_rate() creates valid Reform", {
  reform <- reform_change_tax_rate(rate_change = 1.0, effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "Increase Tax Rate")
  expect_equal(reform$parameters[[1]]$param, "oasi_tr")
  expect_equal(reform$parameters[[1]]$value, 1.0)  # 1 percentage point (oasi_tr is stored as %)
  expect_equal(reform$parameters[[1]]$type, "add")
})

test_that("reform_40_year_averaging() creates valid Reform", {
  reform <- reform_40_year_averaging(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "40-Year Averaging")

  dropout_func <- reform$parameters[[1]]$value

  # Should reduce dropout years from 5 to 0
  expect_equal(dropout_func(2029), 5)  # Before effective year
  expect_equal(dropout_func(2030), 5)  # At effective year
  expect_equal(dropout_func(2031), 4)
  expect_equal(dropout_func(2032), 3)
  expect_equal(dropout_func(2033), 2)
  expect_equal(dropout_func(2034), 1)
  expect_equal(dropout_func(2035), 0)
  expect_equal(dropout_func(2040), 0)  # Stays at 0
})

test_that("reform_repeal_ret() creates valid Reform", {
  reform <- reform_repeal_ret(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Repeal Retirement Earnings Test")
  expect_equal(reform$parameters[[1]]$param, "ret_enabled")
  expect_equal(reform$parameters[[1]]$value, FALSE)
})

test_that("reform_phase_out_spousal() creates valid Reform", {
  reform <- reform_phase_out_spousal(effective_year = 2030, phase_in_years = 20)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Phase Out Spousal Benefits")
  expect_equal(reform$parameters[[1]]$param, "s_pia_share")
  expect_equal(reform$parameters[[1]]$value, 0)
  expect_equal(reform$phase_in_years, 20)
})


# -----------------------------------------------------------------------------
# Test Tier 2 Reform Templates
# -----------------------------------------------------------------------------

test_that("reform_cola_cap() creates valid Reform", {
  reform <- reform_cola_cap(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Cap COLAs at Median PIA")
})

test_that("reform_taxmax_90_pct() creates valid Reform without assumptions", {
  reform <- reform_taxmax_90_pct(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "90% Coverage")

  # Check expected parameters
  param_names <- sapply(reform$parameters, function(p) p$param)
  expect_true("bp3" %in% param_names)
  expect_true("fact4" %in% param_names)
  expect_true("taxmax" %in% param_names)
  expect_true("taxmax_benefit" %in% param_names)

  # fact4 = 5%
  fact4_param <- reform$parameters[[which(param_names == "fact4")]]
  expect_equal(fact4_param$value, 0.05)

  # bp3 function returns hardcoded taxmax / 12
  bp3_fn <- reform$parameters[[which(param_names == "bp3")]]$value
  expect_true(is.function(bp3_fn))
  expect_equal(bp3_fn(2026), 184500 / 12)
  expect_equal(bp3_fn(2030), 215400 / 12)

  # taxmax function returns new higher taxmax
  taxmax_fn <- reform$parameters[[which(param_names == "taxmax")]]$value
  expect_true(is.function(taxmax_fn))
  expect_true(taxmax_fn(2030) > 215400)  # New taxmax > old taxmax
})

test_that("reform_taxmax_90_pct() uses closure over assumptions", {
  # Create minimal assumptions with taxmax values
  assumptions <- data.frame(
    year = 2020:2060,
    taxmax = seq(150000, 400000, length.out = 41)
  )
  taxmax_2030 <- assumptions$taxmax[assumptions$year == 2030]

  reform <- reform_taxmax_90_pct(effective_year = 2026, assumptions = assumptions)

  param_names <- sapply(reform$parameters, function(p) p$param)

  # bp3 should use actual taxmax schedule
  bp3_fn <- reform$parameters[[which(param_names == "bp3")]]$value
  expect_equal(bp3_fn(2030), taxmax_2030 / 12)

  # New taxmax should be ratio × old taxmax
  taxmax_fn <- reform$parameters[[which(param_names == "taxmax")]]$value
  expected_ratio <- 330500 / 184500
  expect_equal(taxmax_fn(2030), taxmax_2030 * expected_ratio)

  # Before effective year should return NA
  expect_true(is.na(taxmax_fn(2025)))
})

test_that("reform_eliminate_taxmax() creates valid Reform without assumptions", {
  reform <- reform_eliminate_taxmax(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "Eliminate Taxmax with 15% Credit")

  param_names <- sapply(reform$parameters, function(p) p$param)

  # fact4 = 15%
  fact4_param <- reform$parameters[[which(param_names == "fact4")]]
  expect_equal(fact4_param$value, 0.15)

  # taxmax = 10M
  taxmax_param <- reform$parameters[[which(param_names == "taxmax")]]
  expect_equal(taxmax_param$value, 10000000)

  # bp3 returns NA without assumptions (relies on pia_reform fallback)
  bp3_fn <- reform$parameters[[which(param_names == "bp3")]]$value
  expect_true(is.function(bp3_fn))
  expect_true(is.na(bp3_fn(2030)))
})

test_that("reform_eliminate_taxmax() uses closure over assumptions for bp3", {
  assumptions <- data.frame(
    year = 2020:2060,
    taxmax = seq(150000, 400000, length.out = 41)
  )
  taxmax_2030 <- assumptions$taxmax[assumptions$year == 2030]

  reform <- reform_eliminate_taxmax(effective_year = 2026, assumptions = assumptions)

  param_names <- sapply(reform$parameters, function(p) p$param)
  bp3_fn <- reform$parameters[[which(param_names == "bp3")]]$value

  # bp3 should return actual taxmax / 12 when assumptions provided
  expect_equal(bp3_fn(2030), taxmax_2030 / 12)
  # Still NA for years outside the schedule
  expect_true(is.na(bp3_fn(1990)))
})

test_that("reform_eliminate_taxmax_no_credit() creates valid Reform", {
  reform <- reform_eliminate_taxmax_no_credit(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "without Credit")

  param_names <- sapply(reform$parameters, function(p) p$param)

  # taxmax_tax = 10M (scalar, not a function)
  taxmax_tax_param <- reform$parameters[[which(param_names == "taxmax_tax")]]
  expect_equal(taxmax_tax_param$value, 10000000)
  expect_false(is.function(taxmax_tax_param$value))

  # taxmax_benefit = function returning NA (keeps current law)
  taxmax_benefit_param <- reform$parameters[[which(param_names == "taxmax_benefit")]]
  expect_true(is.function(taxmax_benefit_param$value))
  expect_true(is.na(taxmax_benefit_param$value(2030)))
})

test_that("reform_basic_minimum() creates valid Reform", {
  reform <- reform_basic_minimum(
    individual_amount = 900,
    couple_amount = 1342,
    effective_year = 2030
  )

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Basic Minimum Benefit")

  # Check parameters
  bmb_ind_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "bmb_individual"))]]
  bmb_cpl_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "bmb_couple"))]]

  expect_equal(bmb_ind_param$value, 900)
  expect_equal(bmb_cpl_param$value, 1342)
})

test_that("reform_child_care_credit() creates valid Reform", {
  reform <- reform_child_care_credit(effective_year = 2030, max_years = 5)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Child Care Credit")

  # Check parameters
  cc_active_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "child_care_credit_active"))]]
  max_years_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "max_child_care_years"))]]

  expect_equal(cc_active_param$value, TRUE)
  expect_equal(max_years_param$value, 5)
})


# -----------------------------------------------------------------------------
# Test Tier 3 Reform Templates
# -----------------------------------------------------------------------------

test_that("reform_flat_benefit() creates valid Reform", {
  reform <- reform_flat_benefit(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "Flat Benefit")
  expect_equal(reform$phase_in_years, 25)

  # Should modify flat_benefit, fact2, and fact3
  param_names <- sapply(reform$parameters, function(p) p$param)
  expect_true("flat_benefit" %in% param_names)
  expect_true("fact2" %in% param_names)
  expect_true("fact3" %in% param_names)
})

test_that("reform_flat_benefit() without assumptions uses constant value", {
  reform <- reform_flat_benefit(effective_year = 2030)
  fb_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "flat_benefit"))]]
  # Without assumptions, value should be the constant flat_amount
  expect_equal(fb_param$value, 19300 / 12)
})

test_that("reform_flat_benefit() AWI-indexes flat benefit when assumptions provided", {
  # Create minimal assumptions with AWI values
  assumptions <- data.frame(
    year = 2020:2070,
    awi = seq(55000, 150000, length.out = 51)
  )
  # Set AWI(2023) as the base
  awi_2023 <- assumptions$awi[assumptions$year == 2023]

  reform <- reform_flat_benefit(effective_year = 2030, assumptions = assumptions)

  fb_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "flat_benefit"))]]
  expect_true(is.function(fb_param$value))

  flat_amount <- 19300 / 12

  # Value at 2025 should use AWI(2023) — i.e., exactly flat_amount
  val_2025 <- fb_param$value(2025)
  expect_equal(val_2025, flat_amount)

  # Value should scale with AWI (two-year lag)
  val_2040 <- fb_param$value(2040)  # Uses AWI(2038)
  val_2050 <- fb_param$value(2050)  # Uses AWI(2048)
  expect_true(val_2050 > val_2040)  # Grows with wages

  # Check exact computation: flat_amount * AWI(year-2) / AWI(2023)
  awi_2038 <- assumptions$awi[assumptions$year == 2038]
  expected_2040 <- flat_amount * awi_2038 / awi_2023
  expect_equal(val_2040, expected_2040)
})

test_that("reform_simpson_bowles() creates valid Reform", {
  reform <- reform_simpson_bowles(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "Simpson-Bowles")
  expect_equal(reform$phase_in_years, 10)

  # Should have bp3, fact2, fact3, fact4
  param_names <- sapply(reform$parameters, function(p) p$param)
  expect_true("bp3" %in% param_names)
  expect_true("fact2" %in% param_names)
  expect_true("fact3" %in% param_names)
  expect_true("fact4" %in% param_names)

  # Check values
  fact2_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "fact2"))]]
  fact3_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "fact3"))]]
  fact4_param <- reform$parameters[[which(sapply(reform$parameters, function(p) p$param == "fact4"))]]

  expect_equal(fact2_param$value, 0.30)
  expect_equal(fact3_param$value, 0.10)
  expect_equal(fact4_param$value, 0.05)
})

test_that("reform_mini_pia() creates valid Reform", {
  reform <- reform_mini_pia(effective_year = 2030, phase_in_years = 10)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "Mini-PIA")
  expect_equal(reform$phase_in_years, 10)

  # Should set mini_pia_blend to 1.0
  expect_equal(reform$parameters[[1]]$param, "mini_pia_blend")
  expect_equal(reform$parameters[[1]]$value, 1.0)
})

test_that("reform_widow_75_pct() creates valid Reform", {
  reform <- reform_widow_75_pct(effective_year = 2030)

  expect_s3_class(reform, "Reform")
  expect_match(reform$name, "75%")

  # Should set widow_75_pct_active to TRUE
  expect_equal(reform$parameters[[1]]$param, "widow_75_pct_active")
  expect_equal(reform$parameters[[1]]$value, TRUE)
})


# -----------------------------------------------------------------------------
# Test apply_reform() with new parameters
# -----------------------------------------------------------------------------

test_that("apply_reform() works with new reform parameters", {
  skip_if_not_installed("ssmbar")

  # Load package data
  data(tr2025, package = "ssmbar")

  # Test PIA multiplier reform
  reform <- reform_reduce_benefits(multiplier = 0.95, effective_year = 2030)
  reformed <- apply_reform(tr2025, reform)

  # Check that pia_multiplier was modified
  expect_true("pia_multiplier" %in% names(reformed))

  # Before effective year, should be 1.0
  pre_reform <- reformed[reformed$year < 2030, "pia_multiplier"]
  expect_true(all(pre_reform == 1.0))

  # After effective year, should be 0.95
  post_reform <- reformed[reformed$year >= 2030, "pia_multiplier"]
  expect_true(all(post_reform == 0.95))
})

test_that("apply_reform() works with phase-in", {
  skip_if_not_installed("ssmbar")

  data(tr2025, package = "ssmbar")

  # 10-year phase-in
  reform <- reform_reduce_benefits(multiplier = 0.90, effective_year = 2030, phase_in_years = 10)
  reformed <- apply_reform(tr2025, reform)

  # At 2030, should be ~0.99 (1 year into phase-in)
  year_2030 <- reformed[reformed$year == 2030, "pia_multiplier"]
  expect_true(year_2030 > 0.90 && year_2030 < 1.0)

  # At 2035, should be ~0.95 (halfway through phase-in)
  year_2035 <- reformed[reformed$year == 2035, "pia_multiplier"]
  expect_true(abs(year_2035 - 0.95) < 0.02)

  # At 2040 and after, should be fully phased in at 0.90
  year_2040 <- reformed[reformed$year == 2040, "pia_multiplier"]
  expect_equal(year_2040, 0.90)
})

test_that("apply_reform() works with RET repeal", {
  skip_if_not_installed("ssmbar")

  data(tr2025, package = "ssmbar")

  reform <- reform_repeal_ret(effective_year = 2030)
  reformed <- apply_reform(tr2025, reform)

  expect_true("ret_enabled" %in% names(reformed))

  # Before effective year
  pre_reform <- reformed[reformed$year < 2030, "ret_enabled"]
  expect_true(all(pre_reform == TRUE))

  # At/after effective year
  post_reform <- reformed[reformed$year >= 2030, "ret_enabled"]
  expect_true(all(post_reform == FALSE))
})


# -----------------------------------------------------------------------------
# Test Mutual Exclusivity Enforcement
# -----------------------------------------------------------------------------

test_that("check_reform_exclusivity() detects NRA conflicts", {
  reform1 <- reform_nra_to_68(effective_year = 2030)
  reform2 <- reform_index_nra(effective_year = 2030)

  result <- check_reform_exclusivity(list(reform1, reform2))
  expect_false(result$valid)
  expect_length(result$conflicts, 1)
  expect_match(result$conflicts[1], "nra")
})

test_that("check_reform_exclusivity() detects COLA index conflicts", {
  reform1 <- reform_chained_cpi(effective_year = 2030)
  reform2 <- reform_cpi_e(effective_year = 2030)

  result <- check_reform_exclusivity(list(reform1, reform2))
  expect_false(result$valid)
  expect_match(result$conflicts[1], "cola_index")
})

test_that("check_reform_exclusivity() detects taxmax conflicts", {
  reform1 <- reform_taxmax_90_pct(effective_year = 2030)
  reform2 <- reform_eliminate_taxmax(effective_year = 2030)

  result <- check_reform_exclusivity(list(reform1, reform2))
  expect_false(result$valid)
  expect_match(result$conflicts[1], "taxmax")
})

test_that("check_reform_exclusivity() allows non-conflicting reforms", {
  reform1 <- reform_nra_to_68(effective_year = 2030)
  reform2 <- reform_chained_cpi(effective_year = 2030)
  reform3 <- reform_reduce_benefits(multiplier = 0.95, effective_year = 2030)

  result <- check_reform_exclusivity(list(reform1, reform2, reform3))
  expect_true(result$valid)
  expect_length(result$conflicts, 0)
})

test_that("apply_reforms() errors on conflicting reforms", {
  skip_if_not_installed("ssmbar")
  data(tr2025, package = "ssmbar")

  reform1 <- reform_nra_to_68(effective_year = 2030)
  reform2 <- reform_nra_to_69_index(effective_year = 2030)

  expect_error(
    apply_reforms(tr2025, list(reform1, reform2)),
    "mutual exclusivity"
  )
})

test_that("apply_reforms() can bypass exclusivity check", {
  skip_if_not_installed("ssmbar")
  data(tr2025, package = "ssmbar")

  reform1 <- reform_nra_to_68(effective_year = 2030)
  reform2 <- reform_index_nra(effective_year = 2030)

  # Should not error when check_exclusivity = FALSE
  result <- apply_reforms(tr2025, list(reform1, reform2), check_exclusivity = FALSE)
  expect_true(is.data.frame(result))
})

test_that("apply_reforms() works with valid combinations", {
  skip_if_not_installed("ssmbar")
  data(tr2025, package = "ssmbar")

  reforms <- list(
    reform_reduce_benefits(multiplier = 0.95, effective_year = 2030),
    reform_nra_to_68(effective_year = 2030),
    reform_chained_cpi(effective_year = 2030),
    reform_change_tax_rate(rate_change = 1.0, effective_year = 2030)
  )

  reformed <- apply_reforms(tr2025, reforms)
  expect_s3_class(reformed, "data.frame")

  # Verify each reform took effect
  expect_equal(reformed[reformed$year == 2035, "pia_multiplier"], 0.95)
})


# -----------------------------------------------------------------------------
# Test calculate_taxes() with taxmax_tax
# -----------------------------------------------------------------------------

test_that("calculate_taxes() uses taxmax_tax when present in assumptions", {
  # Create worker with earnings above taxmax but below taxmax_tax
  worker <- data.frame(
    id = rep("test-worker", 3),
    year = 2025:2027,
    age = 40:42,
    earnings = c(200000, 200000, 200000)
  )

  # Assumptions without taxmax_tax (current law)
  assumptions_base <- data.frame(
    year = 2025:2027,
    oasi_tr = c(5.3, 5.3, 5.3),
    di_tr = c(0.9, 0.9, 0.9),
    taxmax = c(176100, 184500, 190800)
  )

  result_base <- calculate_taxes(worker, assumptions_base)
  # Taxable earnings should be capped at taxmax

  expect_equal(result_base$ss_taxable_earn[1], 176100)

  # Assumptions with taxmax_tax (reform #14 — unlimited taxes)
  assumptions_reform <- assumptions_base
  assumptions_reform$taxmax_tax <- c(10000000, 10000000, 10000000)

  result_reform <- calculate_taxes(worker, assumptions_reform)
  # Taxable earnings should use taxmax_tax (effectively uncapped)
  expect_equal(result_reform$ss_taxable_earn[1], 200000)

  # Tax should be higher under reform
  expect_true(result_reform$ss_tax[1] > result_base$ss_tax[1])
})
