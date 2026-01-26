# Generate baseline test fixtures for regression testing
# Run this script from the package root directory

library(devtools)
load_all()

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

# ============================================================================
# Additional spousal benefit test cases (added 2026-01-23)
# These test the interaction of early claiming with spousal benefits,
# where different reduction factors (rf1/rf2 vs s_rf1/s_rf2) matter.
#
# Key insight: The spousal benefit calculation order matters because
# worker reduction factors differ from spousal reduction factors.
# See skill.md for detailed mathematical explanation.
# ============================================================================

# Test case 7: Low earner claiming EARLY (62) with HIGH spouse at NRA (67)
# This is the key case where different reduction factors matter
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
# Comparison case - no early reduction on worker benefit
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
# Both claiming early - tests interaction of two early claimers
baseline_medium_early_both <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 62,
  factors = sef2025, assumptions = tr2025,
  spouse_type = "medium", spouse_sex = "female",
  spouse_birth_yr = 1960, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_medium_early_both, "tests/testthat/fixtures/medium_early_62_medium_spouse_62.rds")
cat("Created medium_early_62_medium_spouse_62.rds\n")

# ============================================================================
# BR (Spouse of Retired Worker) test case (added 2026-01-25)
# This tests the scenario where a worker has no own earnings but receives
# spousal benefits based on their spouse's record.
# ============================================================================

# Test case 10: Zero earner with high-earning spouse (BR benefit class)
# Worker has $1 average earnings (effectively zero) to test BR classification
baseline_zero_with_spouse <- calculate_benefits(
  birth_yr = 1960, sex = "female", type = "custom", age_claim = 62,
  custom_avg_earnings = 1,  # Minimal earnings to avoid division by zero
  factors = sef2025, assumptions = tr2025,
  spouse_type = "high", spouse_sex = "male",
  spouse_birth_yr = 1960, spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(baseline_zero_with_spouse, "tests/testthat/fixtures/zero_earner_high_spouse_br.rds")
cat("Created zero_earner_high_spouse_br.rds\n")

cat("\nAll 10 baseline test cases generated successfully!\n")
