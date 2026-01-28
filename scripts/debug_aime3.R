# Compare AIME at different ages
library(devtools)
load_all()

# Calculate for 1960 medium earner with debug
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Check AIME at different ages
cat("AIME by age (62-65):\n")
for (a in 62:65) {
  row <- result[result$age == a, ]
  cat(sprintf("Age %d (year %d): AIME = %d\n", a, row$year, row$aime))
}

cat("\n\n=== Checking what years are included ===\n")
# At age 65 (claiming age), what indexed earnings are used?
# The aime() function uses indexed_earnings[1:i] at row i

# Get all indexed earnings
ie_df <- result[, c("age", "year", "indexed_earn")]
cat("All indexed earnings:\n")
print(ie_df[ie_df$age >= 21 & ie_df$age <= 65, ])

cat("\n\nTop 35 when including through age 64:\n")
through_64 <- result[result$age <= 64, ]
top35_64 <- head(through_64[order(-through_64$indexed_earn), c("age", "indexed_earn")], 35)
cat("Sum:", sum(top35_64$indexed_earn), "/ 420 =", floor(sum(top35_64$indexed_earn) / 420), "\n")

cat("\nTop 35 when including through age 61 (SSA method for age 62 claim):\n")
through_61 <- result[result$age <= 61, ]
top35_61 <- head(through_61[order(-through_61$indexed_earn), c("age", "indexed_earn")], 35)
cat("Sum:", sum(top35_61$indexed_earn), "/ 420 =", floor(sum(top35_61$indexed_earn) / 420), "\n")

cat("\n=== SSA Handbook says ===\n")
cat("Elapsed years: from age 22 through year BEFORE eligibility (age 61 for 62-eligible)\n")
cat("So AIME at age 62 should use earnings through age 61 only\n")
cat("Expected AIME at age 62:", floor(sum(top35_61$indexed_earn) / 420), "\n")
cat("Actual AIME at age 62:", result[result$age == 62, "aime"], "\n")

# What does V.C7 imply?
cat("\n=== Table V.C7 Comparison ===\n")
cat("V.C7 annual benefit at 65: 29283\n")
cat("V.C7 monthly: ", 29283/12, "\n")
# At age 65, COLA factor (from 62 to 65)
# 2022 COLA: 8.7%, 2023 COLA: 3.2%, 2024 COLA: 2.5%
cola_factor <- 1.087 * 1.032 * 1.025
cat("COLA factor from 62 to 65:", cola_factor, "\n")
cat("Implied basic_pia:", 29283 / 12 / cola_factor, "\n")
