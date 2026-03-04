# =============================================================================
# Tests for Baseline/Reform Function Equivalence
# =============================================================================
#
# These tests verify that baseline functions (aime, pia, cola, ret, widow_benefit)
# produce identical results to reform functions (aime_reform, pia_reform, etc.)
# when no reforms are applied.
#
# This ensures the baseline extraction was done correctly.
#
# =============================================================================

library(testthat)

# -----------------------------------------------------------------------------
# Test calculate_benefits() vs calculate_benefits_reform() equivalence
# -----------------------------------------------------------------------------

test_that("calculate_benefits() and calculate_benefits_reform() produce identical results with no reform", {
  # Calculate using baseline pipeline
  baseline <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025
  )

  # Calculate using reform pipeline with no reform
  reform_no_change <- calculate_benefits_reform(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025,
    reform = NULL
  )

  # Compare key benefit columns
  expect_equal(baseline$ben, reform_no_change$ben,
               info = "Monthly benefit (ben) should be identical")
  expect_equal(baseline$wrk_ben, reform_no_change$wrk_ben,
               info = "Worker benefit (wrk_ben) should be identical")
  expect_equal(baseline$annual_ind, reform_no_change$annual_ind,
               info = "Annual individual benefit should be identical")
})

test_that("Baseline and reform produce same results for low earner", {
  baseline <- calculate_benefits(
    birth_yr = 1965, sex = "female", type = "low", age_claim = 65,
    factors = sef2025, assumptions = tr2025
  )

  reform_no_change <- calculate_benefits_reform(
    birth_yr = 1965, sex = "female", type = "low", age_claim = 65,
    factors = sef2025, assumptions = tr2025
  )

  expect_equal(baseline$ben, reform_no_change$ben)
  expect_equal(baseline$wrk_ben, reform_no_change$wrk_ben)
})

test_that("Baseline and reform produce same results for high earner", {
  baseline <- calculate_benefits(
    birth_yr = 1980, sex = "male", type = "high", age_claim = 70,
    factors = sef2025, assumptions = tr2025
  )

  reform_no_change <- calculate_benefits_reform(
    birth_yr = 1980, sex = "male", type = "high", age_claim = 70,
    factors = sef2025, assumptions = tr2025
  )

  expect_equal(baseline$ben, reform_no_change$ben)
  expect_equal(baseline$wrk_ben, reform_no_change$wrk_ben)
})

test_that("Baseline and reform produce same results for max earner", {
  baseline <- calculate_benefits(
    birth_yr = 1975, sex = "all", type = "max", age_claim = 62,
    factors = sef2025, assumptions = tr2025
  )

  reform_no_change <- calculate_benefits_reform(
    birth_yr = 1975, sex = "all", type = "max", age_claim = 62,
    factors = sef2025, assumptions = tr2025
  )

  expect_equal(baseline$ben, reform_no_change$ben)
  expect_equal(baseline$wrk_ben, reform_no_change$wrk_ben)
})

test_that("Baseline and reform produce same results with spouse", {
  baseline <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "high", age_claim = 67,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female",
    spouse_birth_yr = 1972, spouse_age_claim = 65
  )

  reform_no_change <- calculate_benefits_reform(
    birth_yr = 1970, sex = "male", type = "high", age_claim = 67,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female",
    spouse_birth_yr = 1972, spouse_age_claim = 65
  )

  expect_equal(baseline$ben, reform_no_change$ben)
  expect_equal(baseline$wrk_ben, reform_no_change$wrk_ben)
  expect_equal(baseline$spouse_ben, reform_no_change$spouse_ben)
})


# -----------------------------------------------------------------------------
# Test individual function equivalence
# -----------------------------------------------------------------------------

test_that("aime() and aime_reform() produce identical results", {
  # Create test worker
  worker <- earnings_generator(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025
  )
  worker <- join_all_assumptions(worker, tr2025)

  baseline_result <- aime(worker, tr2025, debugg = TRUE)
  reform_result <- aime_reform(worker, tr2025, debugg = TRUE)

  expect_equal(baseline_result$aime, reform_result$aime,
               info = "AIME values should be identical")
})

test_that("pia() and pia_reform() produce identical results", {
  worker <- earnings_generator(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025
  )
  worker <- join_all_assumptions(worker, tr2025)
  worker <- aime(worker, tr2025, debugg = TRUE)

  baseline_result <- pia(worker, tr2025, debugg = TRUE)
  reform_result <- pia_reform(worker, tr2025, debugg = TRUE)

  expect_equal(baseline_result$basic_pia, reform_result$basic_pia,
               info = "PIA values should be identical")
})

test_that("cola() and cola_reform() produce identical results", {
  worker <- earnings_generator(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025
  )
  worker <- join_all_assumptions(worker, tr2025)
  worker <- aime(worker, tr2025, debugg = TRUE)
  worker <- pia(worker, tr2025, debugg = TRUE)

  baseline_result <- cola(worker, tr2025, debugg = TRUE)
  reform_result <- cola_reform(worker, tr2025, debugg = TRUE)

  expect_equal(baseline_result$cola_basic_pia, reform_result$cola_basic_pia,
               info = "COLA-adjusted PIA values should be identical")
})


# -----------------------------------------------------------------------------
# Test that reforms actually change results (sanity check)
# -----------------------------------------------------------------------------

test_that("Reforms produce different results than baseline", {
  # Calculate baseline
  baseline <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025
  )

  # Calculate with 20% benefit cut
  reform <- reform_reduce_benefits(multiplier = 0.80, effective_year = 2025)
  reformed <- calculate_benefits_reform(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, reform = reform
  )

  # Benefits should be different (lower)
  claim_year <- 1970 + 67
  baseline_ben <- baseline$ben[baseline$year == claim_year]
  reformed_ben <- reformed$ben[reformed$year == claim_year]

  expect_true(reformed_ben < baseline_ben,
              info = "Reformed benefits should be lower than baseline")
})
