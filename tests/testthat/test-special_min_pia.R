# Test file for Special Minimum PIA functionality
# Per 42 USC 415(a)(1)(C)

test_that("years_of_coverage counts qualifying years correctly", {
  # Create a test worker with known earnings history
  # Using very low earnings that stay below YOC threshold most years
  worker <- data.frame(
    id = rep(1, 50),
    age = 21:70,
    year = 1981:2030,
    earnings = c(
      rep(500, 10),    # Ages 21-30: Low earnings below threshold
      rep(15000, 20),  # Ages 31-50: Earnings above threshold
      rep(500, 10),    # Ages 51-60: Low earnings below threshold
      rep(0, 10)       # Ages 61-70: No earnings
    ),
    elig_age = 62
  )

  # Load package data
  data("tr2025", package = "ssmbar", envir = environment())

  # Join assumptions
  worker <- worker %>%
    dplyr::left_join(tr2025 %>% dplyr::select(year, yoc_threshold), by = "year")

  # Call years_of_coverage
  result <- years_of_coverage(worker, debugg = TRUE)

  # Check that years of coverage was computed
  expect_true("years_of_coverage" %in% names(result))
  expect_true("is_coverage_year" %in% names(result))

  # At age 50, should have 20 years of coverage (ages 31-50)
  yoc_at_50 <- result$years_of_coverage[result$age == 50]
  expect_equal(yoc_at_50, 20)

  # At age 30, should have 0 years of coverage (all earnings below threshold)
  yoc_at_30 <- result$years_of_coverage[result$age == 30]
  expect_equal(yoc_at_30, 0)
})

test_that("special minimum PIA is calculated for workers with 11+ years of coverage", {
  # Create a worker with consistent high earnings (will have many years of coverage)
  data("tr2025", package = "ssmbar", envir = environment())
  data("sef2025", package = "ssmbar", envir = environment())

  # Use earnings_generator to create a realistic worker
  worker <- earnings_generator(
    birth_yr = 1960,
    type = "very_low",
    assumptions = tr2025,
    factors = sef2025,
    age_claim = 67
  )

  # Calculate AIME and PIA with debugg=TRUE to see special_min_pia
  worker <- aime(worker, tr2025, debugg = FALSE)
  worker_debug <- pia(worker, tr2025, debugg = TRUE)

  # Check that special minimum PIA columns exist
  expect_true("special_min_pia" %in% names(worker_debug))
  expect_true("regular_pia" %in% names(worker_debug))
  expect_true("years_of_coverage" %in% names(worker_debug))

  # At eligibility age, special_min_pia should be calculated
  at_elig <- worker_debug[worker_debug$age == 62, ]
  expect_true(!is.na(at_elig$special_min_pia))
  expect_true(!is.na(at_elig$regular_pia))

  # basic_pia should be the max of regular and special minimum
  expect_equal(at_elig$basic_pia, max(at_elig$regular_pia, at_elig$special_min_pia))
})

test_that("special minimum PIA formula uses correct rate", {
  # Per 42 USC 415(a)(1)(C)(i): PIA = special_min_rate × (years_of_coverage - 10)
  # The rate is $11.50 in 1979, COLA-adjusted each year

  data("tr2025", package = "ssmbar", envir = environment())

  # Create a synthetic worker with exactly 20 years of coverage at eligibility
  # Manually construct to have precise control
  worker <- data.frame(
    id = rep(1, 50),
    age = 21:70,
    year = 1981:2030,
    earnings = c(
      rep(50000, 20),  # Ages 21-40: High earnings (above YOC threshold)
      rep(0, 30)       # Ages 41-70: No earnings
    ),
    elig_age = 62,
    aime = c(rep(0, 41), rep(1000, 9))  # Artificially low AIME to trigger special minimum
  )

  # Join assumptions
  worker <- worker %>%
    dplyr::left_join(tr2025 %>% dplyr::select(year, yoc_threshold, bp1, bp2, fact1, fact2, fact3,
                                               elig_age_retired, special_min_rate, min_yoc_for_special_min),
                     by = "year")

  # Calculate PIA
  result <- pia(worker, tr2025, debugg = TRUE)

  # Get values at eligibility age
  at_elig <- result[result$age == 62, ]

  # With 20 years of coverage and low AIME:
  # - special_min_pia = special_min_rate × (20 - 10) = special_min_rate × 10
  # This should be higher than the regular PIA for a low-AIME worker
  if (at_elig$years_of_coverage >= 11) {
    expected_special_min <- floor(at_elig$special_min_rate_elig * (at_elig$years_of_coverage - 10) * 10) / 10
    expect_equal(at_elig$special_min_pia, expected_special_min)
  }
})

test_that("workers with fewer than 11 years of coverage get regular PIA only", {
  data("tr2025", package = "ssmbar", envir = environment())

  # Create a worker with very few working years
  worker <- data.frame(
    id = rep(1, 50),
    age = 21:70,
    year = 1981:2030,
    earnings = c(
      rep(50000, 5),   # Ages 21-25: Only 5 years of high earnings
      rep(0, 45)       # Ages 26-70: No earnings
    ),
    elig_age = 62,
    aime = c(rep(0, 41), rep(1000, 9))
  )

  # Join assumptions
  worker <- worker %>%
    dplyr::left_join(tr2025 %>% dplyr::select(year, yoc_threshold, bp1, bp2, fact1, fact2, fact3,
                                               elig_age_retired, special_min_rate, min_yoc_for_special_min),
                     by = "year")

  # Calculate PIA
  result <- pia(worker, tr2025, debugg = TRUE)

  # Get values at eligibility age
  at_elig <- result[result$age == 62, ]

  # With fewer than 11 years of coverage, special_min_pia should be 0
  expect_equal(at_elig$special_min_pia, 0)

  # basic_pia should equal regular_pia
  expect_equal(at_elig$basic_pia, at_elig$regular_pia)
})

test_that("special_min_rate is COLA-adjusted correctly", {
  # The special minimum rate starts at $11.50 in 1979 and is adjusted by COLA each year
  data("tr2025", package = "ssmbar", envir = environment())

  # Check that special_min_rate in 1979 is the base $11.50
  rate_1979 <- tr2025$special_min_rate[tr2025$year == 1979]
  expect_equal(rate_1979, 11.50)

  # Check that rates increase over time (COLAs are generally positive)
  rate_2025 <- tr2025$special_min_rate[tr2025$year == 2025]
  expect_true(rate_2025 > rate_1979)

  # Verify the rate is always a multiple of $0.10 (floor to dime)
  rates <- tr2025$special_min_rate[!is.na(tr2025$special_min_rate)]
  expect_true(all((rates * 10) %% 1 == 0))
})

test_that("years of coverage only counts working years (21 to elig_age-1)", {
  data("tr2025", package = "ssmbar", envir = environment())

  # Create a worker with earnings at all ages
  worker <- data.frame(
    id = rep(1, 60),
    age = 16:75,  # Includes ages before 21 and after eligibility
    year = 1976:2035,
    earnings = rep(50000, 60),  # High earnings at all ages
    elig_age = 62
  )

  # Join assumptions
  worker <- worker %>%
    dplyr::left_join(tr2025 %>% dplyr::select(year, yoc_threshold), by = "year")

  # Calculate years of coverage
  result <- years_of_coverage(worker, debugg = TRUE)

  # Earnings before age 21 should not count
  # Check is_coverage_year is 0 for ages before 21
  under_21 <- result[result$age < 21, ]
  expect_true(all(under_21$is_coverage_year == 0))

  # Earnings at or after eligibility age should not count
  at_or_after_elig <- result[result$age >= 62, ]
  expect_true(all(at_or_after_elig$is_coverage_year == 0))

  # Only ages 21 through 61 should count (41 years maximum)
  yoc_at_elig <- result$years_of_coverage[result$age == 62]
  expect_true(yoc_at_elig <= 41)  # Maximum possible years: 62 - 21 = 41
})
