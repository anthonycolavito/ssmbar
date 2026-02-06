library(readxl)
library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# Read V.C7 data
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

# Extended range: birth years 1960-2000
birth_years <- seq(1960, 2000, by = 5)
worker_types <- c("very_low", "low", "medium", "high", "max")
results <- data.frame()

for (by in birth_years) {
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
      birth_yr = by, yr65 = yr65, type = wtype,
      ssmbar = round(annual_2025, 0), vc7 = vc7_val,
      diff_pct = round(diff_pct, 2)
    ))
  }
}

cat("\n=== Extended V.C7 Comparison (Birth Years 1960-2000) ===\n")
cat(sprintf("%-8s %-6s %-8s %-10s %-10s %-8s\n",
            "BirthYr", "Yr65", "Type", "ssmbar", "VC7", "Diff%"))
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("%-8d %-6d %-8s %-10d %-10.0f %-8.2f\n",
              r$birth_yr, r$yr65, r$type, r$ssmbar, r$vc7, r$diff_pct))
}

cat("\n=== Summary by Worker Type ===\n")
for (wtype in worker_types) {
  sub <- results[results$type == wtype, ]
  cat(sprintf("  %-10s: avg=%.2f%%, range=[%.2f%%, %.2f%%]\n",
              wtype, mean(sub$diff_pct), min(sub$diff_pct), max(sub$diff_pct)))
}
cat(sprintf("\nOverall average |diff|: %.2f%%\n", mean(abs(results$diff_pct))))

# Check specifically around the old discontinuity point (birth years 1969-1975)
cat("\n=== Zoom: Birth Years 1969-1975 (Around Former Discontinuity) ===\n")
cat(sprintf("%-8s %-8s %-10s %-10s %-8s\n", "BirthYr", "Type", "ssmbar", "VC7", "Diff%"))

for (by in 1969:1975) {
  yr65 <- by + 65
  for (wtype in c("medium")) {
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
    cat(sprintf("%-8d %-8s %-10d %-10.0f %-8.2f\n",
                by, wtype, round(annual_2025, 0), vc7_val, round(diff_pct, 2)))
  }
}
