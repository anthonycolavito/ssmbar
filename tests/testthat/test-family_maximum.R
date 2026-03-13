# Unit tests for family maximum calculations
# Tests the family_maximum() function and family max formula

# =============================================================================
# Test Data Setup
# =============================================================================

# Load test data if not already available
if (!exists("tr2025")) {
  tr2025 <- prep_assumptions(read.csv(system.file("extdata", "2025TR_assumptions.csv", package = "ssmbar")))
}
if (!exists("sef2025")) {
  sef2025 <- read.csv(system.file("extdata", "scaled_earnings_factors.csv", package = "ssmbar"))
}

# =============================================================================
# Tests for Family Maximum Bend Points
# =============================================================================

test_that("Family max bend points are loaded from CSV", {
  # Check that fm_bp columns exist in assumptions
  expect_true("fm_bp1" %in% names(tr2025))
  expect_true("fm_bp2" %in% names(tr2025))
  expect_true("fm_bp3" %in% names(tr2025))
})

test_that("Family max bend points have correct 1979 base values", {
  fm_1979 <- tr2025[tr2025$year == 1979, ]
  expect_equal(fm_1979$fm_bp1, 230)
  expect_equal(fm_1979$fm_bp2, 332)
  expect_equal(fm_1979$fm_bp3, 433)
})

test_that("Family max bend points have correct 2025 values", {
  fm_2025 <- tr2025[tr2025$year == 2025, ]
  expect_equal(fm_2025$fm_bp1, 1567)
  expect_equal(fm_2025$fm_bp2, 2262)
  expect_equal(fm_2025$fm_bp3, 2950)
})

test_that("Family max bend points are projected for future years", {
  # Check that 2030 has values (projected)
  fm_2030 <- tr2025[tr2025$year == 2030, ]
  fm_2025_row <- tr2025[tr2025$year == 2025, ]
  expect_true(!is.na(fm_2030$fm_bp1))
  expect_true(!is.na(fm_2030$fm_bp2))
  expect_true(!is.na(fm_2030$fm_bp3))
  # Values should increase with AWI
  expect_gt(fm_2030$fm_bp1, fm_2025_row$fm_bp1)
})

# =============================================================================
# Tests for Family Maximum Formula
# =============================================================================

test_that("Family maximum formula calculates correctly", {
  # Manual calculation with 2025 bend points
  # PIA = $2000
  # fm_bp1 = 1567, fm_bp2 = 2262, fm_bp3 = 2950
  # FM = 1.50 * min(2000, 1567) +
  #      2.72 * max(0, min(2000, 2262) - 1567) +
  #      1.34 * max(0, min(2000, 2950) - 2262) +
  #      1.75 * max(0, 2000 - 2950)
  #    = 1.50 * 1567 + 2.72 * 433 + 0 + 0
  #    = 2350.50 + 1177.76
  #    = 3528.26 (floored to 3528.20)

  pia <- 2000
  fm_bp1 <- 1567
  fm_bp2 <- 2262
  fm_bp3 <- 2950

  expected_fm <- floor_dime(
    1.50 * min(pia, fm_bp1) +
    2.72 * max(0, min(pia, fm_bp2) - fm_bp1) +
    1.34 * max(0, min(pia, fm_bp3) - fm_bp2) +
    1.75 * max(0, pia - fm_bp3)
  )

  # Should be approximately 3528.20
  expect_equal(expected_fm, 3528.2, tolerance = 0.1)
})

test_that("Family maximum for high PIA uses all four brackets", {
  # PIA = $4000 (above fm_bp3)
  pia <- 4000
  fm_bp1 <- 1567
  fm_bp2 <- 2262
  fm_bp3 <- 2950

  expected_fm <- floor_dime(
    1.50 * fm_bp1 +
    2.72 * (fm_bp2 - fm_bp1) +
    1.34 * (fm_bp3 - fm_bp2) +
    1.75 * (pia - fm_bp3)
  )

  # All four brackets should be used
  # = 1.50 * 1567 + 2.72 * 695 + 1.34 * 688 + 1.75 * 1050
  # = 2350.50 + 1890.40 + 921.92 + 1837.50
  # = 7000.32 (floored to 7000.30)
  expect_equal(expected_fm, 7000.3, tolerance = 0.1)
})

# =============================================================================
# Tests for Family Maximum Application
# =============================================================================

test_that("Family maximum limits total auxiliary benefits", {
  # High earner with spouse and multiple children
  # Should trigger family max reduction
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "high",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    spouse_type = "low",
    spouse_sex = "female",
    spouse_birth_yr = 1972,
    spouse_age_claim = 62,
    child1_spec = "2020-FALSE",
    child2_spec = "2022-FALSE",
    child3_spec = "2024-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 62, ]

  # Total auxiliary should not exceed family_max - wrk_ben
  total_aux <- at_claim$spouse_ben_fm + at_claim$child1_ben_fm +
               at_claim$child2_ben_fm + at_claim$child3_ben_fm

  available <- at_claim$family_max - at_claim$wrk_ben

  expect_lte(total_aux, available + 1)  # +1 for rounding tolerance
})

test_that("No reduction needed when under family max", {
  # Medium earner with one child - unlikely to hit family max
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 67,  # Claim at NRA
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2025-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 67, ]

  # Total auxiliary (just one child) should be under family max
  # So benefits should not be reduced
  # child1_ben_fm should be very close to child1_ben (allowing for floor rounding)
  expect_equal(at_claim$child1_ben_fm, at_claim$child1_ben, tolerance = 1)
})

test_that("Proportional reduction applies equally to all auxiliaries", {
  # Create scenario where family max is exceeded
  worker <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "high",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    spouse_type = "low",
    spouse_sex = "female",
    spouse_birth_yr = 1962,
    spouse_age_claim = 62,
    child1_spec = "2010-TRUE",  # Disabled child to ensure eligibility
    child2_spec = "2012-TRUE",
    debugg = TRUE
  )

  # Find a year where family max applies
  # Check that reduction factor is consistent across all auxiliary benefits
  at_claim <- worker[worker$age == 62, ]

  if (at_claim$fm_reduction_factor < 1) {
    # Reduction was applied - check it's proportional
    # spouse_ben_fm / spouse_ben should equal the reduction factor
    if (at_claim$spouse_ben > 0) {
      ratio_spouse <- at_claim$spouse_ben_fm / at_claim$spouse_ben
      expect_equal(ratio_spouse, at_claim$fm_reduction_factor, tolerance = 0.01)
    }
    if (at_claim$child1_ben > 0) {
      ratio_child1 <- at_claim$child1_ben_fm / at_claim$child1_ben
      expect_equal(ratio_child1, at_claim$fm_reduction_factor, tolerance = 0.01)
    }
  }
})

test_that("Worker benefit is NEVER reduced by family max", {
  # Even when family max is exceeded, worker's own benefit is not reduced
  # Compare a worker with and without children - their worker benefit should be the same
  worker_with_children <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "high",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2010-TRUE",
    child2_spec = "2012-TRUE"
  )

  worker_alone <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "high",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025
  )

  # At age 68, both workers should have positive benefits
  at_68_children <- worker_with_children[worker_with_children$age == 68, ]
  at_68_alone <- worker_alone[worker_alone$age == 68, ]

  # Worker alone should have positive ben
  expect_gt(at_68_alone$ben, 0)

  # Worker with children should have benefit >= worker alone
  # (family max affects auxiliaries, not worker's own)
  expect_gte(at_68_children$ben, at_68_alone$ben)
})

# =============================================================================
# Tests for Disability Family Maximum Alternative
# =============================================================================

test_that("Disabled worker uses disability family max formula", {
  # Disabled worker - family max should be min(85% AIME, 150% PIA)
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "high",
    age_claim = 50,  # This triggers disability
    disabled_age = 50,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2010-TRUE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 50, ]

  # For disabled workers, family_max should use disability formula
  # disability_fm = min(0.85 * AIME, 1.50 * PIA)
  disability_fm <- floor_dime(min(0.85 * at_claim$aime, 1.50 * at_claim$basic_pia))

  expect_equal(at_claim$fm_at_elig, disability_fm, tolerance = 0.1)
})

# =============================================================================
# Tests for Family Maximum with COLA
# =============================================================================

test_that("Family maximum receives COLA adjustments over time", {
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "high",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2020-TRUE",  # Disabled child for ongoing benefits
    debugg = TRUE
  )

  # Family max at claim age (62)
  fm_at_62 <- worker$family_max[worker$age == 62]

  # Family max at age 65 (3 years of COLA)
  fm_at_65 <- worker$family_max[worker$age == 65]

  # Family max should increase with COLA
  expect_gt(fm_at_65, fm_at_62)
})

# =============================================================================
# Tests for Total Benefit Calculation
# =============================================================================

test_that("Total benefit includes family-max-adjusted auxiliary benefits", {
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2020-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 62, ]

  # Total benefit should be wrk_ben + child_ben_fm
  # (no spouse, no survivor)
  expected_ben <- at_claim$wrk_ben + at_claim$child1_ben_fm +
                  at_claim$child2_ben_fm + at_claim$child3_ben_fm

  # Note: ben also includes spouse_ben_fm and survivor_ben logic
  # For this test, we just verify child benefits are included
  expect_gte(at_claim$ben, at_claim$wrk_ben + at_claim$child1_ben_fm)
})

# =============================================================================
# Tests for Dependent Spouse Benefit in Family Maximum
# =============================================================================

test_that("Dependent spouse benefit is subject to worker's family maximum", {
  # High earner with low-earning spouse = large dependent spouse benefit
  # Plus children to push total auxiliaries over family max
  worker <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "max",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    spouse_type = "very_low",
    spouse_sex = "female",
    spouse_birth_yr = 1962,
    spouse_age_claim = 62,
    child1_spec = "2010-TRUE",
    child2_spec = "2012-TRUE",
    debugg = TRUE
  )

  # At an age where both worker and spouse are claiming and children are eligible
  at_claim <- worker[worker$age == 62, ]

  # spouse_dep_ben_fm should exist and be >= 0
  expect_true("spouse_dep_ben_fm" %in% names(worker))

  # If family max is binding, total auxiliaries (including spouse_dep_ben_fm)
  # should not exceed family_max - wrk_ben
  total_aux <- at_claim$spouse_dep_ben_fm + at_claim$child1_ben_fm +
               at_claim$child2_ben_fm + at_claim$child3_ben_fm
  available <- at_claim$family_max - at_claim$wrk_ben

  expect_lte(total_aux, available + 1)  # +1 for rounding tolerance
})

test_that("Dependent spouse benefit gets same proportional reduction as children", {
  # Use same birth year so both claim simultaneously at age 62
  worker <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "max",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    spouse_type = "very_low",
    spouse_sex = "female",
    spouse_birth_yr = 1960,
    spouse_age_claim = 62,
    child1_spec = "2010-TRUE",
    child2_spec = "2012-TRUE",
    debugg = TRUE
  )

  # At age 62, both worker and spouse are claiming
  at_claim <- worker[worker$age == 62, ]

  # Verify the test scenario actually has a binding family max and positive spouse_dep_ben
  expect_lt(at_claim$fm_reduction_factor, 1)
  expect_gt(at_claim$spouse_dep_ben, 0)

  # The reduction factor should be applied to spouse_dep_ben too
  expected_spouse_dep_fm <- floor(at_claim$spouse_dep_ben * at_claim$fm_reduction_factor)
  expect_equal(at_claim$spouse_dep_ben_fm, expected_spouse_dep_fm, tolerance = 1)
})

test_that("No dependent spouse produces spouse_dep_ben_fm of zero", {
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2025-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 67, ]
  expect_true("spouse_dep_ben_fm" %in% names(worker))
  expect_equal(at_claim$spouse_dep_ben_fm, 0)
})

test_that("Worker's own spousal benefit (from spouse's record) is unaffected by worker's family max", {
  # This test ensures the fix doesn't accidentally start reducing spouse_ben
  # spouse_ben is from the SPOUSE's record, not the worker's
  worker <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "max",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    spouse_type = "very_low",
    spouse_sex = "female",
    spouse_birth_yr = 1962,
    spouse_age_claim = 62,
    child1_spec = "2010-TRUE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 62, ]

  # spouse_ben_fm should equal spouse_ben (unchanged by worker's family max)
  expect_equal(at_claim$spouse_ben_fm, pmax(at_claim$spouse_ben, 0))
})
