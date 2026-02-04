# =============================================================================
# Tests for Reform Infrastructure
# =============================================================================

library(testthat)

# -----------------------------------------------------------------------------
# Test create_reform() validation
# -----------------------------------------------------------------------------

test_that("create_reform() validates name parameter", {
  expect_error(
    create_reform(
      name = "",
      description = "test",
      parameters = list(list(param = "nra", value = 69, type = "replace")),
      effective_year = 2030
    ),
    "'name' must be a non-empty character string"
  )

  expect_error(
    create_reform(
      name = c("a", "b"),
      description = "test",
      parameters = list(list(param = "nra", value = 69, type = "replace")),
      effective_year = 2030
    ),
    "'name' must be a non-empty character string"
  )
})

test_that("create_reform() validates description parameter", {
  expect_error(
    create_reform(
      name = "Test",
      description = 123,
      parameters = list(list(param = "nra", value = 69, type = "replace")),
      effective_year = 2030
    ),
    "'description' must be a character string"
  )
})

test_that("create_reform() validates parameters list", {
  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(),
      effective_year = 2030
    ),
    "'parameters' must be a non-empty list"
  )

  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(list(value = 69)),  # missing 'param'
      effective_year = 2030
    ),
    "must have a 'param' element"
  )

  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(list(param = "nra")),  # missing 'value'
      effective_year = 2030
    ),
    "must have a 'value' element"
  )

  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(list(param = "nra", value = 69, type = "invalid")),
      effective_year = 2030
    ),
    "must be 'replace', 'add', or 'multiply'"
  )
})

test_that("create_reform() validates effective_year", {
  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(list(param = "nra", value = 69, type = "replace")),
      effective_year = "2030"
    ),
    "'effective_year' must be a single numeric value"
  )
})

test_that("create_reform() validates phase_in_years", {
  expect_error(
    create_reform(
      name = "Test",
      description = "test",
      parameters = list(list(param = "nra", value = 69, type = "replace")),
      effective_year = 2030,
      phase_in_years = -1
    ),
    "'phase_in_years' must be a non-negative numeric value"
  )
})

test_that("create_reform() creates valid Reform object", {
  reform <- create_reform(
    name = "Test Reform",
    description = "A test reform",
    parameters = list(list(param = "nra", value = 69, type = "replace")),
    effective_year = 2030,
    phase_in_years = 10
  )

  expect_s3_class(reform, "Reform")
  expect_equal(reform$name, "Test Reform")
  expect_equal(reform$effective_year, 2030)
  expect_equal(reform$phase_in_years, 10)
})

test_that("create_reform() sets default type to 'replace'", {
  reform <- create_reform(
    name = "Test",
    description = "test",
    parameters = list(list(param = "nra", value = 69)),  # no type specified
    effective_year = 2030
  )

  expect_equal(reform$parameters[[1]]$type, "replace")
})


# -----------------------------------------------------------------------------
# Test apply_reform()
# -----------------------------------------------------------------------------

test_that("apply_reform() validates reform object", {
  expect_error(
    apply_reform(tr2025, list(name = "fake")),
    "'reform' must be a Reform object"
  )
})

test_that("apply_reform() validates parameters exist in assumptions", {
  reform <- create_reform(
    name = "Bad param",
    description = "test",
    parameters = list(list(param = "nonexistent_param", value = 100, type = "replace")),
    effective_year = 2030
  )

  expect_error(
    apply_reform(tr2025, reform),
    "Parameters not found in assumptions"
  )
})

test_that("apply_reform() handles immediate change correctly", {
  reform <- create_reform(
    name = "Immediate NRA change",
    description = "Set NRA to 68 immediately",
    parameters = list(list(param = "nra", value = 68, type = "replace")),
    effective_year = 2030,
    phase_in_years = 0
  )

  reformed <- apply_reform(tr2025, reform)

  # Before effective year: unchanged
  expect_equal(reformed$nra[reformed$year == 2029], tr2025$nra[tr2025$year == 2029])

  # At and after effective year: new value
  expect_equal(reformed$nra[reformed$year == 2030], 68)
  expect_equal(reformed$nra[reformed$year == 2040], 68)
})

test_that("apply_reform() handles phase-in correctly", {
  # Original NRA is 67 for years 2027+
  # Phase in to 69 over 10 years starting 2030
  reform <- create_reform(
    name = "Phase-in NRA",
    description = "Phase NRA from 67 to 69 over 10 years",
    parameters = list(list(param = "nra", value = 69, type = "replace")),
    effective_year = 2030,
    phase_in_years = 10
  )

  reformed <- apply_reform(tr2025, reform)

  # Before effective year: unchanged (67)
  expect_equal(reformed$nra[reformed$year == 2029], 67)

  # Year 1 of phase-in (2030): 67 + (1/10)*(69-67) = 67.2
  expect_equal(reformed$nra[reformed$year == 2030], 67.2)

  # Year 5 of phase-in (2034): 67 + (5/10)*(69-67) = 68
  expect_equal(reformed$nra[reformed$year == 2034], 68)

  # Year 10 (2039): fully phased in at 69
  expect_equal(reformed$nra[reformed$year == 2039], 69)

  # After full phase-in: stays at 69
  expect_equal(reformed$nra[reformed$year == 2050], 69)
})

test_that("apply_reform() handles 'add' type correctly", {
  # Add 0.5 years to NRA
  reform <- create_reform(
    name = "Add to NRA",
    description = "Add 0.5 to NRA",
    parameters = list(list(param = "nra", value = 0.5, type = "add")),
    effective_year = 2030,
    phase_in_years = 0
  )

  reformed <- apply_reform(tr2025, reform)

  # Before: unchanged
  expect_equal(reformed$nra[reformed$year == 2029], tr2025$nra[tr2025$year == 2029])

  # After: original + 0.5
  orig_2030 <- tr2025$nra[tr2025$year == 2030]
  expect_equal(reformed$nra[reformed$year == 2030], orig_2030 + 0.5)
})

test_that("apply_reform() handles 'multiply' type correctly", {
  # Multiply fact1 by 0.9 (10% cut to 90/32/15 formula)
  reform <- create_reform(
    name = "Cut fact1",
    description = "Reduce fact1 by 10%",
    parameters = list(list(param = "fact1", value = 0.9, type = "multiply")),
    effective_year = 2030,
    phase_in_years = 0
  )

  reformed <- apply_reform(tr2025, reform)

  # Before: unchanged
  expect_equal(reformed$fact1[reformed$year == 2029], tr2025$fact1[tr2025$year == 2029])

  # After: original * 0.9
  orig_2030 <- tr2025$fact1[tr2025$year == 2030]
  expect_equal(reformed$fact1[reformed$year == 2030], orig_2030 * 0.9)
})

test_that("apply_reform() stores reform as attribute", {
  reform <- create_reform(
    name = "Test",
    description = "test",
    parameters = list(list(param = "nra", value = 68, type = "replace")),
    effective_year = 2030
  )

  reformed <- apply_reform(tr2025, reform)

  expect_equal(attr(reformed, "reform")$name, "Test")
})


# -----------------------------------------------------------------------------
# Test compare_benefits()
# -----------------------------------------------------------------------------

test_that("compare_benefits() validates inputs", {
  expect_error(
    compare_benefits("not_df", data.frame()),
    "'baseline' and 'reformed' must be data frames"
  )
})

test_that("compare_benefits() calculates differences correctly", {
  # Create simple test data
  baseline <- data.frame(
    id = c(1, 1, 1),
    age = c(65, 66, 67),
    ben = c(1000, 1020, 1040),
    wrk_ben = c(1000, 1020, 1040)
  )

  reformed <- data.frame(
    id = c(1, 1, 1),
    age = c(65, 66, 67),
    ben = c(900, 918, 936),
    wrk_ben = c(900, 918, 936)
  )

  comparison <- compare_benefits(baseline, reformed, compare_cols = c("ben"))

  expect_true("ben_baseline" %in% names(comparison))
  expect_true("ben_reform" %in% names(comparison))
  expect_true("ben_diff" %in% names(comparison))

  expect_equal(comparison$ben_baseline, c(1000, 1020, 1040))
  expect_equal(comparison$ben_reform, c(900, 918, 936))
  expect_equal(comparison$ben_diff, c(-100, -102, -104))
})


# -----------------------------------------------------------------------------
# Test reform_impact_summary()
# -----------------------------------------------------------------------------

test_that("reform_impact_summary() calculates statistics correctly", {
  comparison <- data.frame(
    id = c(1, 1, 1, 1),
    age = c(65, 66, 67, 68),
    ben_baseline = c(1000, 1000, 1000, 1000),
    ben_reform = c(900, 950, 1000, 1100),
    ben_diff = c(-100, -50, 0, 100)
  )

  impact <- reform_impact_summary(comparison, metric = "ben")

  expect_s3_class(impact, "ReformImpact")
  expect_equal(impact$n_observations, 4)
  expect_equal(impact$n_winners, 1)  # +100
  expect_equal(impact$n_losers, 2)   # -100, -50
  expect_equal(impact$n_unchanged, 1) # 0
  expect_equal(impact$mean_diff, (-100 - 50 + 0 + 100) / 4)
  expect_equal(impact$total_baseline, 4000)
  expect_equal(impact$total_reform, 3950)
  expect_equal(impact$total_diff, -50)
})


# -----------------------------------------------------------------------------
# Test reform template functions
# -----------------------------------------------------------------------------

test_that("reform_raise_nra() creates valid reform", {
  reform <- reform_raise_nra(target_nra = 69, effective_year = 2030, phase_in_years = 10)

  expect_s3_class(reform, "Reform")
  expect_equal(reform$parameters[[1]]$param, "nra")
  expect_equal(reform$parameters[[1]]$value, 69)
  expect_equal(reform$effective_year, 2030)
  expect_equal(reform$phase_in_years, 10)
})

test_that("reform_benefit_formula() creates valid reform", {
  reform <- reform_benefit_formula(fact3 = 0.10, effective_year = 2027)

  expect_s3_class(reform, "Reform")
  expect_equal(length(reform$parameters), 1)
  expect_equal(reform$parameters[[1]]$param, "fact3")
  expect_equal(reform$parameters[[1]]$value, 0.10)
})

test_that("reform_benefit_formula() requires at least one factor", {
  expect_error(
    reform_benefit_formula(effective_year = 2027),
    "At least one of fact1, fact2, or fact3 must be specified"
  )
})

test_that("reform_benefit_cut() creates valid reform", {
  reform <- reform_benefit_cut(cut_pct = 0.20, effective_year = 2033)

  expect_s3_class(reform, "Reform")
  expect_equal(length(reform$parameters), 3)  # all three factors

  # Check that multiplier is 0.8 (1 - 0.20)
  expect_equal(reform$parameters[[1]]$value, 0.8)
  expect_equal(reform$parameters[[1]]$type, "multiply")
})

test_that("reform_benefit_cut() validates cut_pct range", {
  expect_error(reform_benefit_cut(cut_pct = 0, effective_year = 2033))
  expect_error(reform_benefit_cut(cut_pct = 1, effective_year = 2033))
  expect_error(reform_benefit_cut(cut_pct = 1.5, effective_year = 2033))
})


# -----------------------------------------------------------------------------
# Test integration with calculate_benefits_reform()
# -----------------------------------------------------------------------------

test_that("calculate_benefits_reform() accepts reform parameter", {
  reform <- create_reform(
    name = "NRA 68",
    description = "Raise NRA to 68",
    parameters = list(list(param = "nra", value = 68, type = "replace")),
    effective_year = 2025,
    phase_in_years = 0
  )

  # This should run without error
  result <- calculate_benefits_reform(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, reform = reform
  )

  expect_true(is.data.frame(result))
  expect_true("ben" %in% names(result))
})

test_that("Reform affects benefit calculations", {
  # Calculate baseline using baseline calculate_benefits()
  baseline <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025
  )

  # Create reform that cuts benefits by 20%
  reform <- reform_benefit_cut(cut_pct = 0.20, effective_year = 2025)

  # Calculate with reform using calculate_benefits_reform()
  reformed <- calculate_benefits_reform(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 67,
    factors = sef2025, assumptions = tr2025, reform = reform
  )

  # Benefits should be lower under reform for years after effective_year
  # Compare at claiming age (67 = 2037 for 1970 birth)
  claim_year <- 1970 + 67

  baseline_ben <- baseline$ben[baseline$year == claim_year]
  reformed_ben <- reformed$ben[reformed$year == claim_year]

  expect_true(reformed_ben < baseline_ben)
  # Should be approximately 20% less (may not be exact due to rounding in benefit calculations)
  expect_true(reformed_ben >= baseline_ben * 0.75)  # at least 75% of baseline
  expect_true(reformed_ben <= baseline_ben * 0.85)  # at most 85% of baseline
})
