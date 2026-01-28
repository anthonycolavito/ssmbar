# Investigate earnings rounding - deeper analysis
library(devtools)
library(readxl)
library(dplyr)
load_all()

cat("=============================================================================\n")
cat("DEEPER ROUNDING INVESTIGATION\n")
cat("=============================================================================\n\n")

# Load Table V.C7 data and find maximum earner section
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

cat("=== SEARCHING FOR MAXIMUM EARNER IN V.C7 ===\n")
# Look for any row containing "max" (case insensitive)
max_rows <- which(grepl("max", raw_data[[1]], ignore.case = TRUE))
cat("Rows containing 'max':\n")
for (r in max_rows) {
  cat(sprintf("  Row %d: %s\n", r, raw_data[[1]][r]))
}

# Also check what sections exist
cat("\nAll section headers (rows with 'Scaled'):\n")
scaled_rows <- which(grepl("Scaled", raw_data[[1]], ignore.case = TRUE))
for (r in scaled_rows) {
  cat(sprintf("  Row %d: %s\n", r, raw_data[[1]][r]))
}

cat("\n\n=== CENT ROUNDING EFFECT ON ALL WORKER TYPES ===\n\n")

# Function to calculate with and without cent rounding
compare_rounding <- function(type) {
  result <- calculate_benefits(
    birth_yr = 1960, type = type, sex = "all",
    age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  # Get indexed earnings through age 64 (for age 65 AIME)
  data <- result[result$age >= 21 & result$age <= 64, ]

  # Current AIME (no cent rounding)
  top35_current <- head(sort(data$indexed_earn, decreasing = TRUE), 35)
  sum_current <- sum(top35_current)
  aime_current <- floor(sum_current / 420)

  # With cent rounding on indexed earnings
  indexed_rounded <- floor(data$indexed_earn * 100) / 100
  top35_rounded <- head(sort(indexed_rounded, decreasing = TRUE), 35)
  sum_rounded <- sum(top35_rounded)
  aime_rounded <- floor(sum_rounded / 420)

  # Also try rounding earnings BEFORE indexing
  # This would require recalculating indexed_earn
  earnings_rounded <- floor(data$earnings * 100) / 100
  # Can't easily recalculate indexed_earn here without the full pipeline

  list(
    type = type,
    sum_current = sum_current,
    sum_rounded = sum_rounded,
    aime_current = aime_current,
    aime_rounded = aime_rounded,
    diff = aime_current - aime_rounded
  )
}

cat(sprintf("%-10s %15s %15s %10s %10s %8s\n",
            "Type", "Sum (current)", "Sum (rounded)", "AIME curr", "AIME rnd", "Diff"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (type in c("very_low", "low", "medium", "high", "max")) {
  r <- compare_rounding(type)
  cat(sprintf("%-10s %15.2f %15.2f %10d %10d %8d\n",
              r$type, r$sum_current, r$sum_rounded, r$aime_current, r$aime_rounded, r$diff))
}

cat("\n\n=== CHECKING SCALED EARNINGS FACTORS ===\n")
cat("The remaining differences might be in the scaled earnings factors themselves.\n")
cat("Let's compare our factors to what might be implied by V.C7.\n\n")

# For a low earner, check the scaled factor pattern
cat("Low earner scaled factors (sample ages):\n")
low_factors <- sef2025[sef2025$worker == "low", ]
for (a in c(40, 50, 60)) {
  factor <- low_factors$factor[low_factors$age == a]
  awi <- tr2025$awi[tr2025$year == (1960 + a)]
  earnings <- factor * awi
  cat(sprintf("  Age %d: factor=%.4f, AWI=%.2f, earnings=%.2f\n", a, factor, awi, earnings))
}

cat("\nVery low earner scaled factors (sample ages):\n")
vlow_factors <- sef2025[sef2025$worker == "very_low", ]
for (a in c(40, 50, 60)) {
  factor <- vlow_factors$factor[vlow_factors$age == a]
  awi <- tr2025$awi[tr2025$year == (1960 + a)]
  earnings <- factor * awi
  cat(sprintf("  Age %d: factor=%.4f, AWI=%.2f, earnings=%.2f\n", a, factor, awi, earnings))
}

cat("\nHigh earner scaled factors (sample ages):\n")
high_factors <- sef2025[sef2025$worker == "high", ]
for (a in c(40, 50, 60)) {
  factor <- high_factors$factor[high_factors$age == a]
  awi <- tr2025$awi[tr2025$year == (1960 + a)]
  earnings <- factor * awi
  cat(sprintf("  Age %d: factor=%.4f, AWI=%.2f, earnings=%.2f\n", a, factor, awi, earnings))
}
