# Verify automatic recomputation fix
library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# Medium earner born 1960, claim at 65
w <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)

cat("=== Automatic Recomputation Verification ===\n\n")
cat(sprintf("%-4s %-6s %-6s %-12s %-14s\n", "Age", "Year", "AIME", "basic_pia", "cola_basic_pia"))
for (age in 62:70) {
  r <- w[w$age == age, ]
  cat(sprintf("%-4d %-6d %-6d %-12.2f %-14.2f\n",
              age, r$year, r$aime, r$basic_pia, r$cola_basic_pia))
}

cat(sprintf("\nBenefit at 65: $%.2f/month, $%.0f/year\n", w$ben[w$age == 65], w$ben[w$age == 65] * 12))
cat(sprintf("V.C7 target: $25,172/year\n"))
cat(sprintf("Difference: %.2f%%\n", (w$ben[w$age == 65] * 12 - 25172) / 25172 * 100))

# Also check that workers who don't earn past 62 are unaffected
cat("\n=== Control: Worker claiming at 62 (no post-62 earnings) ===\n")
w62 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 62, factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)
cat(sprintf("%-4s %-6s %-6s %-12s %-14s\n", "Age", "Year", "AIME", "basic_pia", "cola_basic_pia"))
for (age in 62:66) {
  r <- w62[w62$age == age, ]
  cat(sprintf("%-4d %-6d %-6d %-12.2f %-14.2f\n",
              age, r$year, r$aime, r$basic_pia, r$cola_basic_pia))
}

# Run V.C7 comparison for birth years 1960-1970
cat("\n=== Updated V.C7 Comparison (Birth 1960-1970) ===\n")
library(readxl)
vc7_path <- "C:/Users/AnthonyColavito/instructions/VC7.xlsx"
raw_data <- read_excel(vc7_path, col_names = FALSE)
ben_col <- ncol(raw_data) - 1
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]

vc7_patterns <- list(
  very_low = "Scaled very low",
  low = "Scaled low earnings",
  medium = "Scaled medium",
  high = "Scaled high",
  max = "Steady maximum"
)

get_vc7_reduced <- function(pattern, year_turn_65) {
  idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(idx) == 0) return(NA)
  start_idx <- idx[1]
  for (i in (start_idx + 1):min(start_idx + 200, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val)) {
      val_str <- as.character(val)
      if (nchar(val_str) == 4 && grepl("^[0-9]+$", val_str)) {
        if (as.numeric(val_str) == year_turn_65) {
          return(as.numeric(raw_data[[ben_col]][i]))
        }
      }
    }
  }
  return(NA)
}

worker_types <- c("very_low", "low", "medium", "high", "max")
results <- data.frame()

for (by in 1960:1970) {
  yr65 <- by + 65
  for (wtype in worker_types) {
    vc7_val <- get_vc7_reduced(vc7_patterns[[wtype]], yr65)
    if (is.na(vc7_val) || vc7_val == 0) next
    worker <- calculate_benefits(
      birth_yr = by, sex = "male", type = wtype,
      age_claim = 65, factors = sef2025, assumptions = tr2025
    )
    ben_65 <- worker$ben[worker$age == 65]
    cpi_yr <- tr2025$cpi_w[tr2025$year == yr65]
    annual_2025 <- ben_65 * 12 * (cpi_2025 / cpi_yr)
    diff_pct <- (annual_2025 - vc7_val) / vc7_val * 100
    results <- rbind(results, data.frame(
      birth_yr = by, type = wtype,
      ssmbar = round(annual_2025, 0), vc7 = vc7_val,
      diff_pct = round(diff_pct, 2)
    ))
  }
}

cat(sprintf("\n%-8s %-10s %-8s %-8s %-8s\n", "BirthYr", "Type", "ssmbar", "VC7", "Diff%"))
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("%-8d %-10s %-8d %-8.0f %-8.2f\n",
              r$birth_yr, r$type, r$ssmbar, r$vc7, r$diff_pct))
}

cat("\nSummary by worker type:\n")
for (wtype in worker_types) {
  sub <- results[results$type == wtype, ]
  cat(sprintf("  %-10s: avg=%.2f%%, range=[%.2f%%, %.2f%%]\n",
              wtype, mean(sub$diff_pct), min(sub$diff_pct), max(sub$diff_pct)))
}
cat(sprintf("\nOverall average |diff|: %.2f%%\n", mean(abs(results$diff_pct))))
