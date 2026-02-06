# Tests for Marginal Analysis Functions (analytic_functions.R)
# These tests verify the marginal benefit analysis, net marginal tax rate,
# and marginal IRR functions

# =============================================================================
# Test Setup
# =============================================================================

# Load test data
data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

# Create a standard test worker with debugg = TRUE (needed for marginal analysis)
create_debug_worker <- function(birth_yr = 1960, type = "medium", age_claim = 67) {
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
# Tests for marginal_benefit_analysis()
# =============================================================================

test_that("marginal_benefit_analysis returns correct structure", {

  worker <- create_debug_worker()

  result <- marginal_benefit_analysis(worker, tr2025)

  # Check structure - stopping-point method columns
  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("year" %in% names(result))
  expect_true("age" %in% names(result))
  expect_true("earnings" %in% names(result))
  expect_true("years_worked" %in% names(result))

  expect_true("qcs" %in% names(result))
  expect_true("eligible" %in% names(result))
  expect_true("cumulative_aime" %in% names(result))
  expect_true("cumulative_pia" %in% names(result))
  expect_true("cumulative_pv" %in% names(result))
  expect_true("delta_pv_benefits" %in% names(result))
})

test_that("marginal_benefit_analysis follows correct stopping-point pattern", {
  worker <- create_debug_worker()

  result <- marginal_benefit_analysis(worker, tr2025)

  # Filter to working years
  working_years <- result[result$age >= 21 & result$age <= 64, ]

  # Years 1-9 (age 21-29): Not yet eligible (< 40 QCs), delta_pv should be 0
  pre_eligible <- working_years[working_years$years_worked < 10, ]
  expect_true(all(pre_eligible$delta_pv_benefits == 0, na.rm = TRUE))
  expect_true(all(pre_eligible$eligible == FALSE, na.rm = TRUE))

  # Year 10 (age 30): Becomes eligible, should have large positive delta_pv
  year_10 <- working_years[working_years$years_worked == 10, ]
  expect_true(year_10$eligible)
  expect_gt(year_10$delta_pv_benefits, 0)

  # Cumulative values should be monotonically non-decreasing for eligible years
  eligible_years <- working_years[working_years$eligible == TRUE, ]
  expect_true(all(diff(eligible_years$cumulative_aime) >= -0.01, na.rm = TRUE))
  expect_true(all(diff(eligible_years$cumulative_pia) >= -0.01, na.rm = TRUE))
  expect_true(all(diff(eligible_years$cumulative_pv) >= -0.01, na.rm = TRUE))
})

test_that("marginal_benefit_analysis requires debugg = TRUE output", {
  # Create worker without debugg (missing indexed_earn, etc.)
  worker_no_debug <- calculate_benefits(
    birth_yr = 1960,
    sex = "male",
    type = "medium",
    age_claim = 67,
    factors = sef2025,
    assumptions = tr2025,
    debugg = FALSE
  )

  expect_error(
    marginal_benefit_analysis(worker_no_debug, tr2025),
    "worker data must contain"
  )
})

# =============================================================================
# Tests for net_marginal_tax_rate()
# =============================================================================

test_that("net_marginal_tax_rate returns correct structure", {
  worker <- create_debug_worker()

  result <- net_marginal_tax_rate(worker, tr2025)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("ss_tax" %in% names(result))
  expect_true("delta_pv_benefits" %in% names(result))
  expect_true("net_marginal_tax_rate" %in% names(result))
})

test_that("net_marginal_tax_rate follows expected patterns", {
  worker <- create_debug_worker()

  result <- net_marginal_tax_rate(worker, tr2025)

  # Filter to working years
  working_years <- result[result$age >= 21 & result$age <= 64, ]

  # With cumulative stopping-point method:
  # - Years 1-9: NMTR ≈ 6.2% (not eligible, pure tax with no benefit accrual)
  # - Year 10: Very large negative NMTR (eligibility transition, huge delta_pv)
  # - Years 11-35: Negative NMTR (benefit accrual exceeds tax)
  # - Years 36+: NMTR closer to full tax rate if year doesn't enter top 35

  # Pre-eligibility years should have positive NMTR (pure tax)
  pre_eligible <- working_years[working_years$years_worked < 10, ]
  expect_true(all(pre_eligible$net_marginal_tax_rate > 0, na.rm = TRUE))

  # Eligibility transition year should have negative NMTR
  # (benefit accrual exceeds tax at eligibility transition)
  year_10 <- working_years[working_years$years_worked == 10, ]
  expect_lt(year_10$net_marginal_tax_rate, 0)  # Should be negative

  # Max NMTR should not exceed full tax rate (6.2%)
  valid_rates <- working_years$net_marginal_tax_rate[!is.na(working_years$net_marginal_tax_rate)]
  expect_true(all(valid_rates <= 0.124))
})

test_that("net_marginal_tax_rate varies by worker type", {
  worker_low <- create_debug_worker(type = "low")
  worker_high <- create_debug_worker(type = "high")

  nmtr_low <- net_marginal_tax_rate(worker_low, tr2025)
  nmtr_high <- net_marginal_tax_rate(worker_high, tr2025)

  # Get mean NMTR for working years
  mean_nmtr_low <- mean(nmtr_low$net_marginal_tax_rate[nmtr_low$age >= 21 & nmtr_low$age <= 64], na.rm = TRUE)
  mean_nmtr_high <- mean(nmtr_high$net_marginal_tax_rate[nmtr_high$age >= 21 & nmtr_high$age <= 64], na.rm = TRUE)

  # Low earners should have lower NMTR (more benefit accrual relative to tax)
  expect_lt(mean_nmtr_low, mean_nmtr_high)
})

test_that("net_marginal_tax_rate handles employer share", {
  worker <- create_debug_worker()

  nmtr_employee <- net_marginal_tax_rate(worker, tr2025, include_employer = FALSE)
  nmtr_total <- net_marginal_tax_rate(worker, tr2025, include_employer = TRUE)

  # Get mean NMTR
  mean_employee <- mean(nmtr_employee$net_marginal_tax_rate[nmtr_employee$age >= 21 & nmtr_employee$age <= 64], na.rm = TRUE)
  mean_total <- mean(nmtr_total$net_marginal_tax_rate[nmtr_total$age >= 21 & nmtr_total$age <= 64], na.rm = TRUE)

  # NMTR with employer share should be higher (more tax, same benefit)
  expect_gt(mean_total, mean_employee)
})

# =============================================================================
# Tests for marginal_irr()
# =============================================================================

test_that("marginal_irr returns correct structure", {
  worker <- create_debug_worker()

  result <- marginal_irr(worker, tr2025)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("id" %in% names(result))
  expect_true("marginal_irr" %in% names(result))
})

test_that("marginal_irr values are reasonable for working years", {
  worker <- create_debug_worker()

  result <- marginal_irr(worker, tr2025)

  working <- result[result$age >= 21 & result$age <= 64, ]

  # Should have some valid IRR values (> -1, meaning benefit accrual occurred)
  valid_irrs <- working$marginal_irr[!is.na(working$marginal_irr) & working$marginal_irr > -1]
  expect_true(length(valid_irrs) > 0)

  # Years with positive delta_pv should have IRR > -1 (not total loss)
  eligible_positive <- working[!is.na(working$delta_pv_benefits) & working$delta_pv_benefits > 0, ]
  expect_true(all(eligible_positive$marginal_irr > -1, na.rm = TRUE))
})

test_that("marginal_irr varies by worker type due to progressivity", {
  worker_low <- create_debug_worker(type = "low")
  worker_high <- create_debug_worker(type = "high")

  mirr_low <- marginal_irr(worker_low, tr2025)
  mirr_high <- marginal_irr(worker_high, tr2025)

  # Get mean marginal IRR for working years with positive benefit accrual
  low_working <- mirr_low[mirr_low$age >= 21 & mirr_low$age <= 64 & !is.na(mirr_low$marginal_irr) & mirr_low$marginal_irr > -1, ]
  high_working <- mirr_high[mirr_high$age >= 21 & mirr_high$age <= 64 & !is.na(mirr_high$marginal_irr) & mirr_high$marginal_irr > -1, ]

  mean_mirr_low <- mean(low_working$marginal_irr, na.rm = TRUE)
  mean_mirr_high <- mean(high_working$marginal_irr, na.rm = TRUE)

  # Due to progressive benefit formula, low earners should have higher marginal IRR
  expect_gt(mean_mirr_low, mean_mirr_high)
})
