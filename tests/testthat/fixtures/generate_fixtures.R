# Generate baseline test fixtures for regression testing
# Run this script from the package root directory

library(devtools)
load_all()

cat("Generating test fixtures for regression testing...\n\n")

# Test case 1: Medium earner, no spouse, claiming at 67
baseline_medium_67 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_medium_67, "tests/testthat/fixtures/medium_1960_claim67.rds")
cat("Created medium_1960_claim67.rds\n")

# Test case 2: Low earner, early claiming at 62
baseline_low_62 <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_low_62, "tests/testthat/fixtures/low_1960_claim62.rds")
cat("Created low_1960_claim62.rds\n")

# Test case 3: High earner, delayed claiming at 70
baseline_high_70 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_high_70, "tests/testthat/fixtures/high_1960_claim70.rds")
cat("Created high_1960_claim70.rds\n")

# Test case 4: Worker with spouse
baseline_with_spouse <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "low", spouse_sex = "female",
  spouse_birth_yr = 1962, spouse_age_claim = 65,
  debugg = TRUE
)
saveRDS(baseline_with_spouse, "tests/testthat/fixtures/high_1960_with_spouse.rds")
cat("Created high_1960_with_spouse.rds\n")

# Test case 5: Custom earnings
baseline_custom <- calculate_benefits(
  birth_yr = 1970, sex = "male", type = "custom", age_claim = 65,
  custom_avg_earnings = 50000,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_custom, "tests/testthat/fixtures/custom_50k_1970.rds")
cat("Created custom_50k_1970.rds\n")

# Test case 6: Max earner
baseline_max <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "max", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
saveRDS(baseline_max, "tests/testthat/fixtures/max_1960_claim67.rds")
cat("Created max_1960_claim67.rds\n")

# Test case 7: Low earner claiming EARLY (62) with HIGH spouse at NRA (67)
baseline_low_early_high <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(baseline_low_early_high, "tests/testthat/fixtures/low_early_62_high_spouse_67.rds")
cat("Created low_early_62_high_spouse_67.rds\n")

# Test case 8: Low earner at NRA (67) with HIGH spouse at NRA (67)
baseline_low_nra_high <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "low", age_claim = 67,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(baseline_low_nra_high, "tests/testthat/fixtures/low_nra_67_high_spouse_67.rds")
cat("Created low_nra_67_high_spouse_67.rds\n")

# Test case 9: Medium earner claiming EARLY (62) with medium spouse claiming EARLY (62)
baseline_medium_early_both <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "medium", spouse_sex = "female",
  spouse_birth_yr = 1960, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_medium_early_both, "tests/testthat/fixtures/medium_early_62_medium_spouse_62.rds")
cat("Created medium_early_62_medium_spouse_62.rds\n")

# Test case 10: Very low earner with high spouse (spouse RET effect)
baseline_very_low_ret <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "very_low", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_very_low_ret, "tests/testthat/fixtures/very_low_1960_high_spouse_ret.rds")
cat("Created very_low_1960_high_spouse_ret.rds\n")

# =============================================================================
# SURVIVOR BENEFIT EDGE CASES
# =============================================================================

# Test case 11: Spouse plans to claim at 70 (late claim with DRCs)
baseline_spouse_dies <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "low", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 70,
  debugg = TRUE
)
saveRDS(baseline_spouse_dies, "tests/testthat/fixtures/spouse_dies_before_claiming.rds")
cat("Created spouse_dies_before_claiming.rds\n")

# Test case 12: Spouse claims at 70 with max DRCs (1965 cohort)
baseline_spouse_drcs <- calculate_benefits(
  birth_yr = 1965, sex = "female", type = "low", age_claim = 65,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1965, spouse_age_claim = 70,
  debugg = TRUE
)
saveRDS(baseline_spouse_drcs, "tests/testthat/fixtures/spouse_with_drcs.rds")
cat("Created spouse_with_drcs.rds\n")

# Test case 13: Worker much younger than spouse (15 years)
baseline_younger <- calculate_benefits(
  birth_yr = 1975, sex = "female", type = "medium", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(baseline_younger, "tests/testthat/fixtures/worker_younger_than_spouse.rds")
cat("Created worker_younger_than_spouse.rds\n")

# =============================================================================
# BIRTH COHORT EDGE CASES
# =============================================================================

# Test case 14: Older birth cohort (1943) with different NRA/DRC
baseline_older <- calculate_benefits(
  birth_yr = 1943, sex = "male", type = "medium", age_claim = 66,
  factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)
saveRDS(baseline_older, "tests/testthat/fixtures/older_cohort_1943.rds")
cat("Created older_cohort_1943.rds\n")

# Test case 15: NRA transition cohort (1955) with fractional NRA
baseline_nra_transition <- calculate_benefits(
  birth_yr = 1955, sex = "female", type = "medium", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)
saveRDS(baseline_nra_transition, "tests/testthat/fixtures/nra_transition_1955.rds")
cat("Created nra_transition_1955.rds\n")

# =============================================================================
# CLAIMING AGE EDGE CASES
# =============================================================================

# Test case 16: Claim exactly at NRA (factor = 1.0)
baseline_at_nra <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 67,
  factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)
saveRDS(baseline_at_nra, "tests/testthat/fixtures/claim_at_nra.rds")
cat("Created claim_at_nra.rds\n")

# Test case 17: Maximum DRC (claim at 70) with spouse
baseline_max_drc <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 70,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "low", spouse_sex = "female",
  spouse_birth_yr = 1962, spouse_age_claim = 65,
  debugg = TRUE
)
saveRDS(baseline_max_drc, "tests/testthat/fixtures/max_drc_with_spouse.rds")
cat("Created max_drc_with_spouse.rds\n")

# =============================================================================
# EXTREME EARNINGS EDGE CASES
# =============================================================================

# Test case 18: Very low earner with max earner spouse
baseline_very_low_max <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "very_low", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "max", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(baseline_very_low_max, "tests/testthat/fixtures/zero_earnings_high_spouse.rds")
cat("Created zero_earnings_high_spouse.rds\n")

# Test case 19: Both very low earners (minimal benefits)
baseline_both_very_low <- calculate_benefits(
  birth_yr = 1970, sex = "male", type = "very_low", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "very_low", spouse_sex = "female",
  spouse_birth_yr = 1970, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_both_very_low, "tests/testthat/fixtures/very_low_both.rds")
cat("Created very_low_both.rds\n")

# =============================================================================
# SPOUSAL TIMING EDGE CASES
# =============================================================================

# Test case 20: Spouse much older (12 years) - early spouse death
baseline_older_spouse <- calculate_benefits(
  birth_yr = 1972, sex = "female", type = "medium", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(baseline_older_spouse, "tests/testthat/fixtures/spouse_much_older.rds")
cat("Created spouse_much_older.rds\n")

# Test case 21: Spouse much younger (10 years)
baseline_younger_spouse <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "high", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "low", spouse_sex = "female",
  spouse_birth_yr = 1970, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_younger_spouse, "tests/testthat/fixtures/spouse_much_younger.rds")
cat("Created spouse_much_younger.rds\n")

# =============================================================================
# DISABLED WIDOW(ER) TEST CASES
# =============================================================================

# Test case 22: Disabled worker qualifies for disabled widow(er) benefits
disabled_widow_qualifies <- calculate_benefits(
  birth_yr = 1980, sex = "male", type = "medium", age_claim = 45, disabled_age = 45,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "female", spouse_birth_yr = 1950, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_qualifies, "tests/testthat/fixtures/disabled_widow_qualifies.rds")
cat("Created disabled_widow_qualifies.rds\n")

# Test case 23: Disabled worker with very low earnings
disabled_widow_very_low <- calculate_benefits(
  birth_yr = 1980, sex = "female", type = "very_low", age_claim = 50, disabled_age = 50,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "max", spouse_sex = "male", spouse_birth_yr = 1946, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_very_low, "tests/testthat/fixtures/disabled_widow_very_low.rds")
cat("Created disabled_widow_very_low.rds\n")

# Test case 24: Disabled worker fails 7-year rule
disabled_widow_7yr_fail <- calculate_benefits(
  birth_yr = 1970, sex = "male", type = "medium", age_claim = 55, disabled_age = 55,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "female", spouse_birth_yr = 1930, spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_7yr_fail, "tests/testthat/fixtures/disabled_widow_7yr_fail.rds")
cat("Created disabled_widow_7yr_fail.rds\n")

# Test case 25: Standard disabled worker with standard widow benefits
disabled_standard_widow <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 45, disabled_age = 45,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "low", spouse_sex = "female", spouse_birth_yr = 1960, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(disabled_standard_widow, "tests/testthat/fixtures/disabled_standard_widow.rds")
cat("Created disabled_standard_widow.rds\n")

cat("\nAll 25 baseline test fixtures generated successfully!\n")
