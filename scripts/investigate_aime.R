# Investigate AIME calculation for 1960 medium earner

library(devtools)
load_all()

cat("=============================================================================\n")
cat("AIME INVESTIGATION: 1960 Medium Earner\n")
cat("=============================================================================\n\n")

# Calculate with full debug output
result <- calculate_benefits(
  birth_yr = 1960,
  type = "medium",
  sex = "all",
  age_claim = 65,
  factors = sef2025,
  assumptions = tr2025,
  debugg = TRUE
)

# Get eligibility year data
age62 <- result[result$age == 62, ]

cat("=== Key Parameters ===\n")
cat("Birth year: 1960\n")
cat("Eligibility age: 62 (year 2022)\n")
cat("Index year: 2020 (age 60)\n")
cat("AWI for 2020:", tr2025$awi[tr2025$year == 2020], "\n\n")

cat("=== AIME Details ===\n")
cat("Computed AIME:", age62$aime, "\n")
cat("Computation period:", age62$comp_period, "years\n")
cat("Elapsed years:", age62$elapsed_years, "\n")
cat("Dropout years:", age62$dropout_years, "\n\n")

cat("=== Top 35 Indexed Earnings ===\n")
# Get all working years (ages 21-61 for this worker)
working_years <- result[result$age >= 21 & result$age <= 61,
                        c("age", "year", "earnings", "capped_earn", "indexed_earn")]

# Sort by indexed earnings descending
working_years <- working_years[order(-working_years$indexed_earn), ]

cat("Top 35 years (used for AIME):\n")
cat(sprintf("%4s %6s %12s %12s %12s\n", "Age", "Year", "Earnings", "Capped", "Indexed"))
cat("--------------------------------------------------------\n")

top35 <- head(working_years, 35)
for (i in 1:nrow(top35)) {
  row <- top35[i, ]
  cat(sprintf("%4d %6d %12.0f %12.0f %12.0f\n",
              row$age, row$year, row$earnings, row$capped_earn, row$indexed_earn))
}

cat("\nSum of top 35 indexed earnings:", sum(top35$indexed_earn), "\n")
cat("Divided by 420 months:", sum(top35$indexed_earn) / 420, "\n")
cat("Floored AIME:", floor(sum(top35$indexed_earn) / 420), "\n")
cat("Stored AIME:", age62$aime, "\n\n")

cat("=== AWI Used for Indexing ===\n")
cat("Index year (2020) AWI:", tr2025$awi[tr2025$year == 2020], "\n")
cat("\nSample AWI values for indexing:\n")
for (yr in c(1980, 1990, 2000, 2010, 2020)) {
  cat(sprintf("  Year %d: %.2f\n", yr, tr2025$awi[tr2025$year == yr]))
}

cat("\n=== Scaled Earnings Factor for Medium Earner ===\n")
medium_factors <- sef2025[sef2025$worker == "medium", ]
cat("Sample factors (ages 40, 50, 60):\n")
for (a in c(40, 50, 60)) {
  factor <- medium_factors$factor[medium_factors$age == a]
  awi <- tr2025$awi[tr2025$year == (1960 + a)]
  earn <- factor * awi
  cat(sprintf("  Age %d (year %d): factor=%.4f, AWI=%.2f, earnings=%.2f\n",
              a, 1960+a, factor, awi, earn))
}
