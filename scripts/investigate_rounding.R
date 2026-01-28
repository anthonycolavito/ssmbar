# Investigate earnings rounding to nearest cent
library(devtools)
library(readxl)
library(dplyr)
load_all()

cat("=============================================================================\n")
cat("INVESTIGATION: Earnings Rounding and Maximum Earner Comparison\n")
cat("=============================================================================\n\n")

# Load Table V.C7 data
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

# Function to extract V.C7 value
get_vc7_value <- function(raw_data, worker_type) {
  pattern <- paste0("Scaled ", worker_type, " earnings")
  start_idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(start_idx) == 0) return(NA)

  for (i in (start_idx[1]+1):nrow(raw_data)) {
    if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
      return(as.numeric(raw_data[[3]][i]))
    }
  }
  return(NA)
}

# Get all V.C7 values including maximum
vc7_very_low <- get_vc7_value(raw_data, "very low")
vc7_low <- get_vc7_value(raw_data, "low")
vc7_medium <- get_vc7_value(raw_data, "medium")
vc7_high <- get_vc7_value(raw_data, "high")
vc7_max <- get_vc7_value(raw_data, "maximum")

cat("=== TABLE V.C7 VALUES (2025, born 1960, age 65) ===\n")
cat("Very Low:", vc7_very_low, "\n")
cat("Low:     ", vc7_low, "\n")
cat("Medium:  ", vc7_medium, "\n")
cat("High:    ", vc7_high, "\n")
cat("Maximum: ", vc7_max, "\n\n")

# Calculate for all worker types including max
cat("=== CURRENT SSMBAR RESULTS ===\n")
worker_types <- c("very_low", "low", "medium", "high", "max")
vc7_values <- c(vc7_very_low, vc7_low, vc7_medium, vc7_high, vc7_max)

results <- list()
for (i in seq_along(worker_types)) {
  type <- worker_types[i]
  result <- calculate_benefits(
    birth_yr = 1960, type = type, sex = "all",
    age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  age65 <- result[result$age == 65, ]
  annual <- age65$cola_basic_pia * 12
  vc7 <- vc7_values[i]
  diff <- annual - vc7
  pct <- round(diff / vc7 * 100, 2)

  results[[type]] <- list(
    basic_pia = age65$basic_pia,
    cola_basic_pia = age65$cola_basic_pia,
    annual = annual,
    vc7 = vc7,
    diff = diff,
    pct = pct
  )

  cat(sprintf("%-10s: basic_pia=%d, cola_pia=%d, annual=%d, V.C7=%d, diff=%d (%.2f%%)\n",
              type, age65$basic_pia, age65$cola_basic_pia, annual, vc7, diff, pct))
}

cat("\n\n=== INVESTIGATING EARNINGS ROUNDING ===\n\n")

# Check current earnings values for medium earner
result_med <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

cat("Sample earnings for medium earner (current - no cent rounding):\n")
sample_ages <- c(50, 55, 60, 61)
for (a in sample_ages) {
  row <- result_med[result_med$age == a, ]
  cat(sprintf("  Age %d: earnings=%.6f, indexed_earn=%.6f\n",
              a, row$earnings, row$indexed_earn))
}

cat("\n\nWith cent rounding (floor to nearest $0.01):\n")
for (a in sample_ages) {
  row <- result_med[result_med$age == a, ]
  earn_rounded <- floor(row$earnings * 100) / 100
  indexed_rounded <- floor(row$indexed_earn * 100) / 100
  cat(sprintf("  Age %d: earnings=%.2f, indexed_earn=%.2f\n",
              a, earn_rounded, indexed_rounded))
}

cat("\n\n=== SIMULATING CENT ROUNDING ON AIME ===\n")

# Get the indexed earnings and simulate cent rounding
indexed_earns <- result_med$indexed_earn[result_med$age >= 21 & result_med$age <= 64]
ages <- result_med$age[result_med$age >= 21 & result_med$age <= 64]

# Current method (no cent rounding)
top35_current <- head(sort(indexed_earns, decreasing = TRUE), 35)
aime_current <- floor(sum(top35_current) / 420)

# With cent rounding on indexed earnings
indexed_earns_rounded <- floor(indexed_earns * 100) / 100
top35_rounded <- head(sort(indexed_earns_rounded, decreasing = TRUE), 35)
aime_rounded <- floor(sum(top35_rounded) / 420)

cat("AIME comparison (medium earner at age 65):\n")
cat("  Current (no cent rounding): ", aime_current, "\n")
cat("  With cent rounding:         ", aime_rounded, "\n")
cat("  Difference:                 ", aime_current - aime_rounded, "\n")

# Calculate what basic_pia would be with rounded AIME
bp1 <- 1024
bp2 <- 6172
if (aime_rounded > bp1 && aime_rounded <= bp2) {
  basic_pia_rounded <- floor(0.90 * bp1 + 0.32 * (aime_rounded - bp1))
} else if (aime_rounded <= bp1) {
  basic_pia_rounded <- floor(0.90 * aime_rounded)
} else {
  basic_pia_rounded <- floor(0.90 * bp1 + 0.32 * (bp2 - bp1) + 0.15 * (aime_rounded - bp2))
}

cat("\nbasic_pia comparison:\n")
cat("  Current:      ", results$medium$basic_pia, "\n")
cat("  With rounding:", basic_pia_rounded, "\n")
