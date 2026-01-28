# Debug AIME calculation to understand discrepancy
library(devtools)
load_all()

# Calculate for 1960 medium earner with debug
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get AIME details at age 62
age62 <- result[result$age == 62, ]
cat("AIME at age 62:", age62$aime, "\n")
cat("Comp period:", age62$comp_period, "\n\n")

# Get ALL rows up to and including age 61 (available for AIME at age 62)
years_for_aime <- result[result$age <= 61, c("age", "year", "earnings", "capped_earn", "indexed_earn")]

cat("Total years up to age 61:", nrow(years_for_aime), "\n")
cat("Years with positive indexed_earn:", sum(years_for_aime$indexed_earn > 0), "\n\n")

# Sort by indexed_earn descending and take top 35
years_sorted <- years_for_aime[order(-years_for_aime$indexed_earn), ]
top35 <- head(years_sorted, 35)

cat("Top 35 indexed earnings (by value):\n")
cat(sprintf("%4s %6s %12s %12s %12s\n", "Age", "Year", "Earnings", "Capped", "Indexed"))
cat("--------------------------------------------------------\n")
for (i in 1:35) {
  row <- top35[i, ]
  cat(sprintf("%4d %6d %12.0f %12.0f %12.0f\n",
              row$age, row$year, row$earnings, row$capped_earn, row$indexed_earn))
}

cat("\nSum of top 35:", sum(top35$indexed_earn), "\n")
cat("Sum / 420 =", sum(top35$indexed_earn) / 420, "\n")
cat("Floor =", floor(sum(top35$indexed_earn) / 420), "\n")
cat("Stored AIME:", age62$aime, "\n")
cat("Difference:", age62$aime - floor(sum(top35$indexed_earn) / 420), "\n")

# Check if there are years before age 21 with earnings
cat("\n\nEarnings by age (ages 0-25):\n")
early_years <- result[result$age <= 25, c("age", "year", "earnings", "indexed_earn")]
print(early_years)
