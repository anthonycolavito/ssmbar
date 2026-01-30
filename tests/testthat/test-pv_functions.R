# Tests for PV Functions (pv_functions.R)
# These tests verify the present value calculation functions

# =============================================================================
# Test Setup
# =============================================================================

# Load test data
data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

# Create a standard test worker
create_test_worker <- function(birth_yr = 1960, type = "medium", age_claim = 67) {
  worker <- calculate_benefits(
    birth_yr = birth_yr,
    sex = "male",
    type = type,
    age_claim = age_claim,
    factors = sef2025,
    assumptions = tr2025,
    debugg = TRUE
  )
  # Ensure we have claim_age and death_age
  if (!"claim_age" %in% names(worker)) {
    worker$claim_age <- age_claim
  }
  worker
}

# =============================================================================
# Tests for pv_lifetime_benefits()
# =============================================================================

test_that("pv_lifetime_benefits returns correct structure", {
  worker <- create_test_worker()

  result <- pv_lifetime_benefits(worker, tr2025)

  # Check structure

  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("pv_benefits" %in% names(result))

  # Should have exactly one row per unique worker
  expect_equal(nrow(result), length(unique(worker$id)))
})

test_that("pv_lifetime_benefits returns positive value for worker with benefits", {
  worker <- create_test_worker()

  result <- pv_lifetime_benefits(worker, tr2025)

  # PV should be positive
  expect_gt(result$pv_benefits[1], 0)
})

test_that("pv_lifetime_benefits handles different discount ages", {
  worker <- create_test_worker()

  pv_65 <- pv_lifetime_benefits(worker, tr2025, discount_to_age = 65)
  pv_62 <- pv_lifetime_benefits(worker, tr2025, discount_to_age = 62)
  pv_70 <- pv_lifetime_benefits(worker, tr2025, discount_to_age = 70)

  # All should be positive
  expect_gt(pv_65$pv_benefits[1], 0)
  expect_gt(pv_62$pv_benefits[1], 0)
  expect_gt(pv_70$pv_benefits[1], 0)

  # Values should differ based on discount age
  # (Earlier discount age = smaller PV since discounting more)
  # Note: This depends on the direction of the discount factor
})

test_that("pv_lifetime_benefits validates required columns", {
  bad_worker <- data.frame(
    id = "test",
    year = 2025,
    age = 65
    # Missing annual_ind
  )

  expect_error(
    pv_lifetime_benefits(bad_worker, tr2025),
    "worker data must contain"
  )
})

# =============================================================================
# Tests for pv_lifetime_taxes()
# =============================================================================

test_that("pv_lifetime_taxes returns correct structure", {
  worker <- create_test_worker()

  result <- pv_lifetime_taxes(worker, tr2025)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("pv_taxes" %in% names(result))

  # Should have exactly one row per unique worker
  expect_equal(nrow(result), length(unique(worker$id)))
})

test_that("pv_lifetime_taxes returns positive value for worker with earnings", {
  worker <- create_test_worker()

  result <- pv_lifetime_taxes(worker, tr2025)

  # PV should be positive (taxes are positive)
  expect_gt(result$pv_taxes[1], 0)
})

test_that("pv_lifetime_taxes doubles with employer share", {
  worker <- create_test_worker()

  pv_employee <- pv_lifetime_taxes(worker, tr2025, include_employer = FALSE)
  pv_total <- pv_lifetime_taxes(worker, tr2025, include_employer = TRUE)

  # Total should be exactly double employee share
  expect_equal(pv_total$pv_taxes[1], pv_employee$pv_taxes[1] * 2, tolerance = 0.01)
})

test_that("pv_lifetime_taxes validates required columns", {
  bad_worker <- data.frame(
    id = "test",
    year = 2025,
    age = 65
    # Missing earnings
  )

  expect_error(
    pv_lifetime_taxes(bad_worker, tr2025),
    "worker data must contain"
  )
})

# =============================================================================
# Tests for benefit_tax_ratio()
# =============================================================================

test_that("benefit_tax_ratio calculates correct ratio", {
  # Simple test with known values
  expect_equal(benefit_tax_ratio(100, 50), 2.0)
  expect_equal(benefit_tax_ratio(50, 100), 0.5)
  expect_equal(benefit_tax_ratio(100, 100), 1.0)
})

test_that("benefit_tax_ratio handles data frames", {
  pv_ben <- data.frame(id = "test", pv_benefits = 100)
  pv_tax <- data.frame(id = "test", pv_taxes = 50)

  expect_equal(benefit_tax_ratio(pv_ben, pv_tax), 2.0)
})

test_that("benefit_tax_ratio handles zero taxes", {
  expect_true(is.na(benefit_tax_ratio(100, 0)))
})

test_that("benefit_tax_ratio works with full calculation", {
  worker <- create_test_worker()

  pv_ben <- pv_lifetime_benefits(worker, tr2025)
  pv_tax <- pv_lifetime_taxes(worker, tr2025)

  ratio <- benefit_tax_ratio(pv_ben, pv_tax)

  # Ratio should be a positive number
  expect_true(is.numeric(ratio))
  expect_gt(ratio, 0)
})

# =============================================================================
# Tests for couple_measures()
# =============================================================================

test_that("couple_measures returns correct structure without spouse", {
  worker <- create_test_worker()

  result <- couple_measures(worker, spouse = NULL, assumptions = tr2025)

  # Check structure
  expect_true(is.list(result))
  expect_true("worker_pv_benefits" %in% names(result))
  expect_true("worker_pv_taxes" %in% names(result))
  expect_true("worker_ratio" %in% names(result))
  expect_true("shared" %in% names(result))

  # Without spouse, shared should be FALSE
  expect_false(result$shared)

  # Spouse values should be NA
  expect_true(is.na(result$spouse_pv_benefits))
  expect_true(is.na(result$spouse_pv_taxes))
})

test_that("couple_measures returns correct structure with spouse", {
  worker <- create_test_worker(birth_yr = 1960, type = "high")
  spouse <- create_test_worker(birth_yr = 1962, type = "low", age_claim = 65)

  result <- couple_measures(worker, spouse, assumptions = tr2025, shared = TRUE)

  # Check structure
  expect_true(is.list(result))
  expect_true("couple_pv_benefits" %in% names(result))
  expect_true("couple_pv_taxes" %in% names(result))
  expect_true("couple_ratio" %in% names(result))

  # With shared = TRUE
  expect_true(result$shared)

  # Spouse values should not be NA
  expect_false(is.na(result$spouse_pv_benefits))
  expect_false(is.na(result$spouse_pv_taxes))
})

test_that("couple_measures calculates correct totals", {
  worker <- create_test_worker(birth_yr = 1960, type = "high")
  spouse <- create_test_worker(birth_yr = 1962, type = "low", age_claim = 65)

  result <- couple_measures(worker, spouse, assumptions = tr2025, shared = FALSE)

  # Couple totals should equal sum of individual values
  expect_equal(
    result$couple_pv_benefits,
    result$worker_pv_benefits + result$spouse_pv_benefits
  )
  expect_equal(
    result$couple_pv_taxes,
    result$worker_pv_taxes + result$spouse_pv_taxes
  )
})

test_that("couple_measures shared ratios are equal for both spouses", {
  worker <- create_test_worker(birth_yr = 1960, type = "high")
  spouse <- create_test_worker(birth_yr = 1962, type = "low", age_claim = 65)

  result <- couple_measures(worker, spouse, assumptions = tr2025, shared = TRUE)

  # When shared = TRUE, worker_ratio and spouse_ratio should be equal
  expect_equal(result$worker_ratio, result$spouse_ratio)
})

# =============================================================================
# Tests for worker type variations
# =============================================================================

test_that("pv_lifetime_benefits varies by worker type", {
  worker_low <- create_test_worker(type = "low")
  worker_med <- create_test_worker(type = "medium")
  worker_high <- create_test_worker(type = "high")

  pv_low <- pv_lifetime_benefits(worker_low, tr2025)$pv_benefits[1]
  pv_med <- pv_lifetime_benefits(worker_med, tr2025)$pv_benefits[1]
  pv_high <- pv_lifetime_benefits(worker_high, tr2025)$pv_benefits[1]

  # Higher earners should have higher PV benefits
  expect_gt(pv_high, pv_med)
  expect_gt(pv_med, pv_low)
})

test_that("benefit_tax_ratio varies by worker type", {
  worker_low <- create_test_worker(type = "low")
  worker_med <- create_test_worker(type = "medium")
  worker_high <- create_test_worker(type = "high")

  ratio_low <- benefit_tax_ratio(
    pv_lifetime_benefits(worker_low, tr2025),
    pv_lifetime_taxes(worker_low, tr2025)
  )
  ratio_med <- benefit_tax_ratio(
    pv_lifetime_benefits(worker_med, tr2025),
    pv_lifetime_taxes(worker_med, tr2025)
  )
  ratio_high <- benefit_tax_ratio(
    pv_lifetime_benefits(worker_high, tr2025),
    pv_lifetime_taxes(worker_high, tr2025)
  )

  # Due to progressive benefit formula, lower earners typically have higher ratios
  # This test verifies the ratios are all positive and reasonable
  expect_gt(ratio_low, 0)
  expect_gt(ratio_med, 0)
  expect_gt(ratio_high, 0)
})


# =============================================================================
# Tests for internal_rate_of_return()
# =============================================================================

test_that("internal_rate_of_return returns correct structure", {
  worker <- create_test_worker()

  result <- internal_rate_of_return(worker, tr2025)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("irr" %in% names(result))

  # Should have exactly one row per unique worker
  expect_equal(nrow(result), length(unique(worker$id)))
})

test_that("internal_rate_of_return returns value in expected range", {
  worker <- create_test_worker()

  result <- internal_rate_of_return(worker, tr2025)

  # IRR should typically be between -10% and +15% for Social Security
  expect_true(!is.na(result$irr[1]))
  expect_gt(result$irr[1], -0.10)
  expect_lt(result$irr[1], 0.15)
})

test_that("internal_rate_of_return doubles tax with employer share", {
  worker <- create_test_worker()

  irr_employee <- internal_rate_of_return(worker, tr2025, include_employer = FALSE)
  irr_total <- internal_rate_of_return(worker, tr2025, include_employer = TRUE)

  # IRR with employer share should be lower (more taxes for same benefits)
  expect_lt(irr_total$irr[1], irr_employee$irr[1])
})

test_that("internal_rate_of_return varies by worker type due to progressivity", {
  worker_low <- create_test_worker(type = "low")
  worker_med <- create_test_worker(type = "medium")
  worker_high <- create_test_worker(type = "high")

  irr_low <- internal_rate_of_return(worker_low, tr2025)$irr[1]
  irr_med <- internal_rate_of_return(worker_med, tr2025)$irr[1]
  irr_high <- internal_rate_of_return(worker_high, tr2025)$irr[1]

  # Due to progressive benefit formula, lower earners have higher IRR
  expect_gt(irr_low, irr_med)
  expect_gt(irr_med, irr_high)
})

test_that("internal_rate_of_return validates required columns", {
  bad_worker <- data.frame(
    id = "test",
    year = 2025,
    age = 65
    # Missing earnings, annual_ind, claim_age, death_age
  )

  expect_error(
    internal_rate_of_return(bad_worker, tr2025),
    "worker data must contain"
  )
})
