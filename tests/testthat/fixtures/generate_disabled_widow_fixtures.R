# Generate test fixtures for disabled widow(er) benefits
# Run this script from the package root directory
#
# Test scenarios:
# 1. Disabled worker qualifies for disabled widow(er) benefits (ADF, ARF)
# 2. Disabled worker with widow-only benefit (F)
# 3. Disabled worker who does NOT qualify (7-year rule violated)
# 4. Standard disabled worker with standard widow benefits (ADD, ARD) for comparison

library(devtools)
load_all()

cat("Generating disabled widow(er) benefit test fixtures...\n\n")

# =============================================================================
# Test Case 1: Disabled worker qualifies for disabled widow(er) benefits
# =============================================================================
# Worker: born 1980, disabled at age 45 (2025), medium earner
# Spouse: born 1950 (30 years older), high earner, claims at 67
# Spouse dies at ~84 (life expectancy), which is year 2034
# Worker's age when spouse dies: 54 (within 50-59 range)
# Disability year: 2025, Spouse death year: 2034
# 7-year rule: 2025 <= 2034 + 7? YES (2025 <= 2041)
# Expected BC: ADF before NRA, ARF at/after NRA

cat("Test 1: Disabled worker qualifies for disabled widow(er) benefits (ADF/ARF)...\n")
disabled_widow_qualifies <- calculate_benefits(
  birth_yr = 1980,
  sex = "male",
  type = "medium",
  age_claim = 45,
  disabled_age = 45,
  factors = sef2025,
  assumptions = tr2025,
  spouse_type = "high",
  spouse_sex = "female",
  spouse_birth_yr = 1950,
  spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_qualifies, "tests/testthat/fixtures/disabled_widow_qualifies.rds")

# Check BC codes at key ages
cat("  BC codes by age:\n")
for (a in c(45, 50, 54, 55, 60, 67, 70)) {
  row <- disabled_widow_qualifies[disabled_widow_qualifies$age == a, ]
  if (nrow(row) > 0) {
    cat(sprintf("    Age %d: BC=%s, wrk_ben=%d, survivor_ben=%d, is_disabled_widow=%s\n",
                a, as.character(row$bc), row$wrk_ben, row$survivor_ben,
                as.character(row$is_disabled_widow)))
  }
}
cat("\n")

# =============================================================================
# Test Case 2: Disabled worker with very low earnings (mostly widow benefit)
# =============================================================================
# Similar setup but with very_low earner - should show F or ADF depending on wrk_ben
# Worker: born 1980, disabled at age 50, very_low earner
# Spouse: born 1945 (35 years older), high earner
# Spouse dies at ~84 (life expectancy), which is year 2029
# Worker's age when spouse dies: 49... need to adjust
# Let's use spouse born 1946, dies at 84 = year 2030, worker age 50

cat("Test 2: Disabled worker with very low earnings (may show F benefit class)...\n")
disabled_widow_very_low <- calculate_benefits(
  birth_yr = 1980,
  sex = "female",
  type = "very_low",
  age_claim = 50,
  disabled_age = 50,
  factors = sef2025,
  assumptions = tr2025,
  spouse_type = "max",
  spouse_sex = "male",
  spouse_birth_yr = 1946,
  spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_very_low, "tests/testthat/fixtures/disabled_widow_very_low.rds")

# Check BC codes at key ages
cat("  BC codes by age:\n")
for (a in c(50, 55, 60, 67, 70)) {
  row <- disabled_widow_very_low[disabled_widow_very_low$age == a, ]
  if (nrow(row) > 0) {
    cat(sprintf("    Age %d: BC=%s, wrk_ben=%d, survivor_ben=%d, is_disabled_widow=%s\n",
                a, as.character(row$bc), row$wrk_ben, row$survivor_ben,
                as.character(row$is_disabled_widow)))
  }
}
cat("\n")

# =============================================================================
# Test Case 3: Disabled worker does NOT qualify (7-year rule violated)
# =============================================================================
# Worker: born 1970, disabled at age 55 (2025)
# Spouse: born 1940, claims at 67, dies at ~84 = year 2024
# Worker's age when spouse dies: 54
# Disability year: 2025, Spouse death year: 2024
# 7-year rule: 2025 <= 2024 + 7? YES (2025 <= 2031) - Still passes!
# Need spouse to die MUCH earlier. Let's use an older spouse.

# Actually, let's try: worker disabled in 2025, spouse died in 2015
# Worker born 1970, disabled at 55 (2025)
# Spouse born 1930, dies at 85 = year 2015
# Worker's age when spouse dies: 45
# Disability year: 2025, Spouse death year: 2015
# 7-year rule: 2025 <= 2015 + 7? NO (2025 > 2022) - FAILS!

cat("Test 3: Disabled worker does NOT qualify (7-year rule violated)...\n")
disabled_widow_7yr_fail <- calculate_benefits(
  birth_yr = 1970,
  sex = "male",
  type = "medium",
  age_claim = 55,
  disabled_age = 55,
  factors = sef2025,
  assumptions = tr2025,
  spouse_type = "high",
  spouse_sex = "female",
  spouse_birth_yr = 1930,
  spouse_age_claim = 67,
  debugg = TRUE
)
saveRDS(disabled_widow_7yr_fail, "tests/testthat/fixtures/disabled_widow_7yr_fail.rds")

# Check BC codes - should be ADD/ARD (standard widow), not ADF/ARF
cat("  BC codes by age (should be ADD/ARD, not ADF/ARF):\n")
for (a in c(55, 60, 67, 70)) {
  row <- disabled_widow_7yr_fail[disabled_widow_7yr_fail$age == a, ]
  if (nrow(row) > 0) {
    cat(sprintf("    Age %d: BC=%s, wrk_ben=%d, survivor_ben=%d, is_disabled_widow=%s\n",
                a, as.character(row$bc), row$wrk_ben, row$survivor_ben,
                as.character(row$is_disabled_widow)))
  }
}
cat("\n")

# =============================================================================
# Test Case 4: Standard disabled worker with standard widow benefits
# =============================================================================
# Worker: born 1960, disabled at age 45
# Spouse: born 1960 (same age), dies at ~84
# Worker's age when spouse dies: 84 (well past 60)
# This is a standard widow scenario (age 60+), not disabled widow

cat("Test 4: Standard disabled worker with standard widow benefits (ADD/ARD)...\n")
disabled_standard_widow <- calculate_benefits(
  birth_yr = 1960,
  sex = "male",
  type = "medium",
  age_claim = 45,
  disabled_age = 45,
  factors = sef2025,
  assumptions = tr2025,
  spouse_type = "low",
  spouse_sex = "female",
  spouse_birth_yr = 1960,
  spouse_age_claim = 62,
  debugg = TRUE
)
saveRDS(disabled_standard_widow, "tests/testthat/fixtures/disabled_standard_widow.rds")

# Check BC codes - should transition AD -> ADD/ARD at spouse death (age 84+)
cat("  BC codes by age:\n")
for (a in c(45, 50, 60, 67, 70, 84, 85)) {
  row <- disabled_standard_widow[disabled_standard_widow$age == a, ]
  if (nrow(row) > 0) {
    cat(sprintf("    Age %d: BC=%s, wrk_ben=%d, survivor_ben=%d, is_disabled_widow=%s\n",
                a, as.character(row$bc), row$wrk_ben, row$survivor_ben,
                as.character(row$is_disabled_widow)))
  }
}
cat("\n")

cat("All 4 disabled widow(er) test fixtures generated successfully!\n")
