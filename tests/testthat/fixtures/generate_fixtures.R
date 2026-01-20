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

cat("\nAll 6 baseline test cases generated successfully!\n")
