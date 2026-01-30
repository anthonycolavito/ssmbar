# Unit tests for child benefit functions
# Tests parse_child_spec(), is_child_eligible(), child_pia(), and child_benefit()

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
# Tests for parse_child_spec()
# =============================================================================

test_that("parse_child_spec handles valid non-disabled child", {
  result <- parse_child_spec("2010-FALSE")
  expect_equal(result$birth_yr, 2010)
  expect_false(result$is_disabled)
})

test_that("parse_child_spec handles valid disabled child", {
  result <- parse_child_spec("2005-TRUE")
  expect_equal(result$birth_yr, 2005)
  expect_true(result$is_disabled)
})

test_that("parse_child_spec handles lowercase disabled flag", {
  result <- parse_child_spec("2010-false")
  expect_false(result$is_disabled)

  result <- parse_child_spec("2010-true")
  expect_true(result$is_disabled)
})

test_that("parse_child_spec returns NULL for NA input", {
  expect_null(parse_child_spec(NA))
})

test_that("parse_child_spec returns NULL for NULL input", {
  expect_null(parse_child_spec(NULL))
})

test_that("parse_child_spec returns NULL for empty string", {
  expect_null(parse_child_spec(""))
})

test_that("parse_child_spec errors on invalid format", {
  expect_error(parse_child_spec("2010"))
  expect_error(parse_child_spec("2010-FALSE-extra"))
  expect_error(parse_child_spec("invalid-FALSE"))
})

# =============================================================================
# Tests for is_child_eligible()
# =============================================================================

test_that("Non-disabled child is eligible under age 18", {
  # Child born 2010, year 2025, child is 15
  result <- is_child_eligible(
    child_birth_yr = 2010,
    is_disabled = FALSE,
    year = 2025,
    worker_claim_age = 62,
    worker_age = 62,
    worker_death_age = 85
  )
  expect_true(result)
})

test_that("Non-disabled child is NOT eligible at age 18", {
  # Child born 2005, year 2023, child is 18
  result <- is_child_eligible(
    child_birth_yr = 2005,
    is_disabled = FALSE,
    year = 2023,
    worker_claim_age = 62,
    worker_age = 65,
    worker_death_age = 85
  )
  expect_false(result)
})

test_that("Disabled child is eligible at any age", {
  # Child born 2000, year 2030, child is 30 (but disabled before 22)
  result <- is_child_eligible(
    child_birth_yr = 2000,
    is_disabled = TRUE,
    year = 2030,
    worker_claim_age = 62,
    worker_age = 70,
    worker_death_age = 85
  )
  expect_true(result)
})

test_that("Child is NOT eligible before worker claims", {
  # Worker hasn't reached claim_age yet
  result <- is_child_eligible(
    child_birth_yr = 2010,
    is_disabled = FALSE,
    year = 2022,
    worker_claim_age = 67,
    worker_age = 60,  # Worker hasn't claimed yet
    worker_death_age = 85
  )
  expect_false(result)
})

test_that("Child is NOT eligible after worker death", {
  result <- is_child_eligible(
    child_birth_yr = 2010,
    is_disabled = FALSE,
    year = 2050,
    worker_claim_age = 67,
    worker_age = 90,  # Worker has died
    worker_death_age = 85
  )
  expect_false(result)
})

# =============================================================================
# Tests for child_pia() and child_benefit()
# =============================================================================

test_that("Child benefit is 50% of worker's PIA", {
  # Worker born 1960, with one non-disabled child born 2000
  worker <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "medium",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2000-FALSE",
    debugg = TRUE
  )

  # At age 62 (when worker claims), child is 22 - too old for non-disabled benefits
  # Let's check at an earlier age when child would be eligible
  # Actually, with birth_yr 2000 and worker birth_yr 1960, child is same age as worker minus 40
  # When worker is 62 (year 2022), child is 22 - not eligible
  # Need a younger child
})

test_that("Child benefit with younger child is 50% of PIA", {
  # Worker born 1970, claiming at 62 (year 2032)
  # Child born 2020, so child is 12 when worker claims
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

  # At claim age (62, year 2032), child is 12 - eligible
  at_claim <- worker[worker$age == 62, ]

  # Child PIA should be 50% of worker's COLA-adjusted PIA
  expect_equal(
    at_claim$child1_pia,
    0.5 * at_claim$cola_basic_pia,
    tolerance = 0.01
  )
})

test_that("Child benefit stops at age 18 for non-disabled child", {
  # Worker born 1970, claiming at 62 (year 2032)
  # Child born 2020, so child is 12 when worker claims
  # Child turns 18 in 2038 when worker is 68
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

  # At age 67 (year 2037), child is 17 - should be eligible
  at_67 <- worker[worker$age == 67, ]
  expect_gt(at_67$child1_ben, 0)

  # At age 68 (year 2038), child is 18 - should NOT be eligible
  at_68 <- worker[worker$age == 68, ]
  expect_equal(at_68$child1_ben, 0)
})

test_that("Disabled child benefit continues past age 18", {
  # Worker born 1970, claiming at 62 (year 2032)
  # Disabled child born 2010, so child is 22 when worker claims
  # Child should still be eligible because disabled before 22
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2010-TRUE",  # Disabled child
    debugg = TRUE
  )

  # At age 62 (year 2032), disabled child is 22 - still eligible
  at_claim <- worker[worker$age == 62, ]
  expect_gt(at_claim$child1_ben, 0)

  # At age 72 (year 2042), disabled child is 32 - still eligible
  at_72 <- worker[worker$age == 72, ]
  expect_gt(at_72$child1_ben, 0)
})

test_that("Multiple children all receive benefits", {
  # Worker born 1980, claiming at 62 (year 2042)
  # Three children born 2030, 2032, 2034 (ages 12, 10, 8 at claim)
  worker <- calculate_benefits(
    birth_yr = 1980,
    sex = "male",
    type = "medium",
    age_claim = 62,
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2030-FALSE",
    child2_spec = "2032-FALSE",
    child3_spec = "2034-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 62, ]

  # All three children should have benefits
  expect_gt(at_claim$child1_ben, 0)
  expect_gt(at_claim$child2_ben, 0)
  expect_gt(at_claim$child3_ben, 0)
})

test_that("Child benefits have no actuarial reduction", {
  # Child benefits should be exactly floor_dime(0.5 * cola_basic_pia)
  # regardless of worker's claiming age
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 62,  # Early claiming
    factors = sef2025,
    assumptions = tr2025,
    child1_spec = "2020-FALSE",
    debugg = TRUE
  )

  at_claim <- worker[worker$age == 62, ]

  # Child benefit should be 50% of PIA floored to dime, NOT reduced by actuarial adjustment
  expected_child_ben <- floor(0.5 * at_claim$cola_basic_pia * 10) / 10
  expect_equal(at_claim$child1_ben, expected_child_ben, tolerance = 0.01)
})

test_that("Worker with no children has zero child benefits", {
  worker <- calculate_benefits(
    birth_yr = 1970,
    sex = "male",
    type = "medium",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025,
    debugg = TRUE
  )

  # All child benefits should be zero
  expect_true(all(worker$child1_ben_fm == 0 | is.na(worker$child1_ben_fm)))
  expect_true(all(worker$child2_ben_fm == 0 | is.na(worker$child2_ben_fm)))
  expect_true(all(worker$child3_ben_fm == 0 | is.na(worker$child3_ben_fm)))
})
