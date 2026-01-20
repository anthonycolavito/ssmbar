# Regression tests to verify calculate_benefits() output matches baseline fixtures
# These tests ensure refactoring doesn't change calculation results

# Helper function to compare key columns between current and expected results
compare_key_columns <- function(current, expected, tolerance = 1e-10) {
  # Columns to compare (core benefit calculation outputs)
  key_cols <- c("ben", "earnings", "aime", "basic_pia")

  # Add spouse columns if present in expected
  if ("spouse_ben" %in% names(expected)) {
    key_cols <- c(key_cols, "spouse_ben")
  }
  if ("spouse_pia" %in% names(expected)) {
    key_cols <- c(key_cols, "spouse_pia")
  }

  # Filter to columns that exist in both datasets
  key_cols <- key_cols[key_cols %in% names(current) & key_cols %in% names(expected)]

  # Compare each column
  for (col in key_cols) {
    expect_equal(
      current[[col]],
      expected[[col]],
      tolerance = tolerance,
      info = paste("Column:", col)
    )
  }
}

test_that("Medium earner (1960, claim 67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "medium_1960_claim67.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  # Check dimensions match

expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))

  # Compare key calculation columns
  compare_key_columns(current, expected)
})

test_that("Low earner (1960, claim 62) matches baseline", {
  expected <- readRDS(test_path("fixtures", "low_1960_claim62.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))
  compare_key_columns(current, expected)
})

test_that("High earner (1960, claim 70) matches baseline", {
  expected <- readRDS(test_path("fixtures", "high_1960_claim70.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))
  compare_key_columns(current, expected)
})

test_that("High earner with spouse (1960, claim 67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "high_1960_with_spouse.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female",
    spouse_birth_yr = 1962, spouse_age_claim = 65,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))
  compare_key_columns(current, expected)
})

test_that("Custom earner $50k (1970, claim 65) matches baseline", {
  expected <- readRDS(test_path("fixtures", "custom_50k_1970.rds"))

  current <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "custom", age_claim = 65,
    custom_avg_earnings = 50000,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))
  compare_key_columns(current, expected)
})

test_that("Max earner (1960, claim 67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "max_1960_claim67.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  expect_equal(ncol(current), ncol(expected))
  compare_key_columns(current, expected)
})
