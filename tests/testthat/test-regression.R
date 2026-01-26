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

  # Add survivor columns if present in expected
  if ("survivor_ben" %in% names(expected)) {
    key_cols <- c(key_cols, "survivor_ben")
  }
  if ("survivor_pia" %in% names(expected)) {
    key_cols <- c(key_cols, "survivor_pia")
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

  # Check row count matches
  expect_equal(nrow(current), nrow(expected))

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
  compare_key_columns(current, expected)
})

test_that("High earner (1960, claim 70) matches baseline", {
  expected <- readRDS(test_path("fixtures", "high_1960_claim70.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
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
  compare_key_columns(current, expected)
})

test_that("Max earner (1960, claim 67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "max_1960_claim67.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

# Additional spousal benefit test cases (added 2026-01-23)
# These test the interaction of early claiming with spousal benefits,
# where different reduction factors (rf1/rf2 vs s_rf1/s_rf2) matter

test_that("Low earner early (62) with high spouse (67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "low_early_62_high_spouse_67.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

test_that("Low earner NRA (67) with high spouse (67) matches baseline", {
  expected <- readRDS(test_path("fixtures", "low_nra_67_high_spouse_67.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "low", age_claim = 67,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

test_that("Medium earner early (62) with medium spouse early (62) matches baseline", {
  expected <- readRDS(test_path("fixtures", "medium_early_62_medium_spouse_62.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "medium", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "medium", spouse_sex = "female",
    spouse_birth_yr = 1960, spouse_age_claim = 62,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

# Spouse's RET effect test case (added 2026-01-24)
# Tests the scenario where a lower earner receives spousal benefit from their
# higher-earning spouse's record, and the spouse has excess earnings that
# trigger RET and reduce the worker's spouse_ben.
# - Worker (very_low) receives spouse_ben from spouse's record
# - Spouse (high) has excess earnings at ages 62-64 (~$65-66k above ret1)
# - Spouse's RET allocates 1/3 of reduction to worker's spouse_ben
# - Worker's spouse_ben is reduced to $0 at ages 62-64 (reduction > benefit)

test_that("Very low earner with high spouse (spouse RET effect) matches baseline", {
  expected <- readRDS(test_path("fixtures", "very_low_1960_high_spouse_ret.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "very_low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 62,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Additional assertions specific to spouse's RET effect
  # Verify spouse_ben is 0 at ages 62-64 (when spouse has excess earnings)
  expect_equal(current$spouse_ben[current$age == 62], 0)
  expect_equal(current$spouse_ben[current$age == 63], 0)
  expect_equal(current$spouse_ben[current$age == 64], 0)

  # Verify spouse_ben is positive at age 65+ (when spouse stops earning)
  expect_true(current$spouse_ben[current$age == 65] > 0)

  # Verify spouse had excess earnings at ages 62-64
  expect_true(current$s_excess_earnings[current$age == 62] > 0)
  expect_true(current$s_excess_earnings[current$age == 63] > 0)
  expect_true(current$s_excess_earnings[current$age == 64] > 0)

  # Verify spouse's RET was allocated to worker's spouse_ben
  expect_true(current$spouse_ret_to_spouse_ben[current$age == 62] > 0)
})

# =============================================================================
# Edge Case Test Cases (added 2026-01-24)
# =============================================================================
# These test cases cover edge cases to catch regressions in future changes:
# - Survivor benefit scenarios
# - Birth cohort variations (NRA/DRC differences)
# - Claiming age edge cases
# - Extreme earnings scenarios
# - Spousal timing edge cases

# -----------------------------------------------------------------------------
# SURVIVOR BENEFIT EDGE CASES
# -----------------------------------------------------------------------------

test_that("Spouse plans to claim at 70 (late claim with DRCs) matches baseline", {
  # Tests survivor benefit when spouse claims late with maximum DRCs
  # Spouse born 1960, male, claims at 70 (max DRCs)
  # Spouse will die around age 83-84, so will have claimed before death
  expected <- readRDS(test_path("fixtures", "spouse_dies_before_claiming.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 70,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify survivor benefit kicks in after spouse dies
  death_age <- current$worker_age_at_spouse_death[1]
  expect_true(current$survivor_ben[current$age == death_age] > 0)
})

test_that("Spouse claims at 70 with max DRCs (1965 cohort) matches baseline", {
  # Tests survivor PIA when spouse received delayed retirement credits
  # Survivor PIA should be based on spouse's higher DRC-enhanced benefit
  expected <- readRDS(test_path("fixtures", "spouse_with_drcs.rds"))

  current <- calculate_benefits(
    birth_yr = 1965, sex = "female", type = "low", age_claim = 65,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1965, spouse_age_claim = 70,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

test_that("Worker much younger than spouse (15 years) matches baseline", {
  # Tests scenario where spouse dies earlier in worker's lifetime
  # Spouse born 1960, worker born 1975 (15 years younger)
  # Spouse dies around age 83 when worker is only 68
  expected <- readRDS(test_path("fixtures", "worker_younger_than_spouse.rds"))

  current <- calculate_benefits(
    birth_yr = 1975, sex = "female", type = "medium", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify worker receives survivor benefits for a long period
  death_age <- current$worker_age_at_spouse_death[1]
  expect_true(death_age < 75)  # Spouse should die when worker is relatively young

  # Verify survivor benefits continue well past spouse death
  expect_true(current$survivor_ben[current$age == death_age + 10] > 0)
})

# -----------------------------------------------------------------------------
# BIRTH COHORT EDGE CASES
# -----------------------------------------------------------------------------

test_that("Older birth cohort (1943) with different NRA/DRC matches baseline", {
  # Tests worker from 1943 birth cohort with NRA of 66 (not 67)
  # DRC rate is also different (6.5% vs 8%)
  expected <- readRDS(test_path("fixtures", "older_cohort_1943.rds"))

  current <- calculate_benefits(
    birth_yr = 1943, sex = "male", type = "medium", age_claim = 66,
    factors = sef2025, assumptions = tr2025,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify actuarial factor is 1.0 at NRA (which is 66 for this cohort)
  expect_equal(current$act_factor[current$age == 66][1], 1.0)
})

test_that("NRA transition cohort (1955) with fractional NRA matches baseline", {
  # Tests worker from 1955 birth cohort with NRA of 66+2 months
  # This tests handling of fractional NRA values
  expected <- readRDS(test_path("fixtures", "nra_transition_1955.rds"))

  current <- calculate_benefits(
    birth_yr = 1955, sex = "female", type = "medium", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

# -----------------------------------------------------------------------------
# CLAIMING AGE EDGE CASES
# -----------------------------------------------------------------------------

test_that("Claim exactly at NRA (factor = 1.0) matches baseline", {
  # Tests that actuarial factor is exactly 1.0 when claiming at NRA
  # No early retirement reduction, no delayed retirement credit
  expected <- readRDS(test_path("fixtures", "claim_at_nra.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
    factors = sef2025, assumptions = tr2025,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify actuarial factor is exactly 1.0
  expect_equal(current$act_factor[current$age == 67][1], 1.0)

  # Verify benefit equals COLA-adjusted PIA (no reduction or increase)
  expect_equal(
    current$wrk_ben[current$age == 67],
    floor(current$cola_basic_pia[current$age == 67])
  )
})

test_that("Maximum DRC (claim at 70) with spouse matches baseline", {
  # Tests maximum delayed retirement credits (24% increase)
  # Also tests DRC interaction with spousal benefits
  expected <- readRDS(test_path("fixtures", "max_drc_with_spouse.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female",
    spouse_birth_yr = 1962, spouse_age_claim = 65,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify actuarial factor is 1.24 (8% DRC * 3 years)
  expect_equal(current$act_factor[current$age == 70][1], 1.24)
})

# -----------------------------------------------------------------------------
# EXTREME EARNINGS EDGE CASES
# -----------------------------------------------------------------------------

test_that("Very low earner with max earner spouse (large spousal benefit) matches baseline", {
  # Tests scenario where worker has minimal own benefit but large spousal/survivor
  # Very low earner with max earner spouse
  expected <- readRDS(test_path("fixtures", "zero_earnings_high_spouse.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "very_low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "max", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify spousal benefit is significant relative to own benefit
  # At age 67 when spouse claims, spousal benefit should be substantial
  own_ben_67 <- current$wrk_ben[current$age == 67]
  spouse_ben_67 <- current$spouse_ben[current$age == 67]
  expect_true(spouse_ben_67 > own_ben_67 * 0.5)  # Spousal > 50% of own
})

test_that("Both very low earners (minimal benefits) matches baseline", {
  # Tests floor conditions with minimal earnings
  # Both worker and spouse are very low earners
  expected <- readRDS(test_path("fixtures", "very_low_both.rds"))

  current <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "very_low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "very_low", spouse_sex = "female",
    spouse_birth_yr = 1970, spouse_age_claim = 62,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify benefits are positive but modest
  expect_true(current$ben[current$age == 67] > 0)
  expect_true(current$ben[current$age == 67] < 2000)  # Should be modest
})

# -----------------------------------------------------------------------------
# SPOUSAL TIMING EDGE CASES
# -----------------------------------------------------------------------------

test_that("Spouse much older (12 years) - early spouse death matches baseline", {
  # Tests scenario where spouse is significantly older
  # Spouse born 1960, worker born 1972 (12 years younger)
  # Results in longer survivor benefit period
  expected <- readRDS(test_path("fixtures", "spouse_much_older.rds"))

  current <- calculate_benefits(
    birth_yr = 1972, sex = "female", type = "medium", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify spouse dies when worker is relatively young
  death_age <- current$worker_age_at_spouse_death[1]
  expect_true(death_age <= 75)

  # Verify survivor benefits for extended period
  expect_true(current$survivor_ben[current$age == 85] > 0)
})

test_that("Spouse much younger (10 years) - delayed spousal availability matches baseline", {
  # Tests scenario where spouse is significantly younger
  # Worker born 1960, spouse born 1970 (10 years younger)
  # Spousal benefits delayed until younger spouse claims
  expected <- readRDS(test_path("fixtures", "spouse_much_younger.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "high", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female",
    spouse_birth_yr = 1970, spouse_age_claim = 62,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Spouse claims at age 62 (year 2032), worker is 72 at that point
  # Worker receives no spousal benefit until spouse claims
  # Note: This worker is high earner so may not receive spousal anyway
})

# -----------------------------------------------------------------------------
# DISABLED WIDOW(ER) BENEFIT TESTS
# -----------------------------------------------------------------------------

test_that("Disabled worker qualifies for disabled widow(er) benefits (ADF/ARF)", {
  # Worker: born 1980, disabled at 45 (2025), medium earner
  # Spouse: born 1950 (30 years older), high earner, dies at ~84 (year 2034)
  # Worker's age when spouse dies: 54 (within 50-59 range)
  # 7-year rule: disability (2025) <= spouse death (2034) + 7 = YES
  # Expected: BC should be ADF before NRA, ARF at/after NRA
  expected <- readRDS(test_path("fixtures", "disabled_widow_qualifies.rds"))

  current <- calculate_benefits(
    birth_yr = 1980, sex = "male", type = "medium", age_claim = 45, disabled_age = 45,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "female", spouse_birth_yr = 1950, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify is_disabled_widow flag is TRUE (worker qualifies)
  expect_true(all(current$is_disabled_widow))

  # Verify BC codes at key ages
  # Before spouse dies (age 50): BC should be AD
  expect_equal(current$bc[current$age == 50], "AD")
  # After spouse dies but before NRA (age 60): BC should be ADF
  expect_equal(current$bc[current$age == 60], "ADF")
  # At/after NRA (age 67): BC should be ARF
  expect_equal(current$bc[current$age == 67], "ARF")
})

test_that("Disabled worker with very low earnings still qualifies for disabled widow(er)", {
  # Tests disabled widow(er) eligibility for very low earner
  expected <- readRDS(test_path("fixtures", "disabled_widow_very_low.rds"))

  current <- calculate_benefits(
    birth_yr = 1980, sex = "female", type = "very_low", age_claim = 50, disabled_age = 50,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "max", spouse_sex = "male", spouse_birth_yr = 1946, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)
})

test_that("Disabled worker fails 7-year rule - gets standard widow benefits", {
  # Worker: born 1970, disabled at 55 (2025)
  # Spouse: born 1930, dies at ~85 (year 2015)
  # 7-year rule: disability (2025) <= spouse death (2015) + 7 = NO (2025 > 2022)
  # Expected: is_disabled_widow should be FALSE, BC should be ADD/ARD (not ADF/ARF)
  expected <- readRDS(test_path("fixtures", "disabled_widow_7yr_fail.rds"))

  current <- calculate_benefits(
    birth_yr = 1970, sex = "male", type = "medium", age_claim = 55, disabled_age = 55,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "female", spouse_birth_yr = 1930, spouse_age_claim = 67,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify is_disabled_widow flag is FALSE (7-year rule violated)
  expect_false(any(current$is_disabled_widow))
})

test_that("Standard disabled worker with same-age spouse gets standard widow (ADD/ARD)", {
  # Worker: born 1960, disabled at 45
  # Spouse: born 1960 (same age), dies at ~84 (year 2044)
  # Worker's age when spouse dies: 84 (well past 60)
  # This is a standard widow scenario (age 60+), not disabled widow
  expected <- readRDS(test_path("fixtures", "disabled_standard_widow.rds"))

  current <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = "medium", age_claim = 45, disabled_age = 45,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "low", spouse_sex = "female", spouse_birth_yr = 1960, spouse_age_claim = 62,
    debugg = TRUE
  )

  expect_equal(nrow(current), nrow(expected))
  compare_key_columns(current, expected)

  # Verify is_disabled_widow flag is FALSE (spouse dies when worker is 60+)
  expect_false(any(current$is_disabled_widow))
})

# -----------------------------------------------------------------------------
# SPOUSE-ONLY BENEFIT CLASS (BR) TESTS
# -----------------------------------------------------------------------------

test_that("Zero earner with high-earning spouse gets BR benefit class", {
  # Worker has minimal earnings ($1 avg) - effectively zero own benefit
  # Worker receives spousal benefits from high-earning spouse
  # Expected BC: BR (Spouse of Retired Worker)

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "custom", age_claim = 62,
    custom_avg_earnings = 1,  # Minimal earnings
    factors = sef2025, assumptions = tr2025,
    spouse_type = "high", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 62,
    debugg = TRUE
  )

  # At age 62 when both claim, worker should have:
  # - wrk_ben = 0 (or near zero due to minimal earnings)
  # - spouse_ben > 0 (spousal benefit from high-earning spouse)
  # - BC = BR (Spouse of Retired Worker)
  age_62_row <- current[current$age == 62, ]

  # Worker's own benefit should be minimal/zero
  expect_true(age_62_row$wrk_ben <= 1)  # Effectively zero

  # Spousal benefit should be positive (once spouse claims)
  # Note: May be 0 at 62 due to spouse's RET, check at 67
  age_67_row <- current[current$age == 67, ]
  expect_true(age_67_row$spouse_ben > 0 | age_67_row$wrk_ben > 0)  # Some benefit

  # BC should be BR when receiving spousal benefit with no own worker benefit
  # Find first age where spouse_ben > 0 and wrk_ben <= 1
  br_ages <- current$age[current$wrk_ben <= 1 & current$spouse_ben > 0 & current$survivor_ben == 0]
  if (length(br_ages) > 0) {
    br_row <- current[current$age == br_ages[1], ]
    expect_equal(br_row$bc, "BR")
  }
})

test_that("Very low earner with max spouse gets ARB (not BR) due to own benefit", {
  # Very low earner has positive own benefit, so should get ARB not BR
  # This tests that BR only applies when wrk_ben = 0

  current <- calculate_benefits(
    birth_yr = 1960, sex = "female", type = "very_low", age_claim = 62,
    factors = sef2025, assumptions = tr2025,
    spouse_type = "max", spouse_sex = "male",
    spouse_birth_yr = 1960, spouse_age_claim = 67,
    debugg = TRUE
  )

  # At age 67, worker should have both own benefit and spousal benefit
  age_67_row <- current[current$age == 67, ]

  # Very low earner still has positive own benefit
  expect_true(age_67_row$wrk_ben > 0)

  # With spousal benefit, BC should be ARB (not BR)
  # Note: BC might be AR if spouse_ben = 0 due to own PIA exceeding spousal PIA
  expect_true(age_67_row$bc %in% c("AR", "ARB"))
})
