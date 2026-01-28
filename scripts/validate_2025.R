# Focused validation: Workers turning 65 in 2025 (born 1960)
# These should match Table V.C7 most closely since:
# 1. No CPI-W conversion needed (2025 is the reference year)
# 2. Only historical COLAs used (no projections)

library(devtools)
library(readxl)
library(dplyr)
load_all()

cat("=============================================================================\n")
cat("VALIDATION: Workers Born 1960, Claiming at Age 65 in 2025\n")
cat("=============================================================================\n\n")

# Confirm CPI-W assumption
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cat("CPI-W for 2025:", cpi_2025, "(confirms no conversion needed)\n\n")

# Load Table V.C7 data
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

# Extract 2025 values (year attain 65 = 2025, birth year = 1960)
# Row format: year_at_65, age_at_retirement, CPI-W_2025_benefit, ...
get_vc7_value <- function(raw_data, worker_type) {
  # Find section start
  pattern <- paste0("Scaled ", worker_type, " earnings")
  start_idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(start_idx) == 0) return(NA)

  # Find the row for year 2025
  for (i in (start_idx[1]+1):nrow(raw_data)) {
    if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
      return(as.numeric(raw_data[[3]][i]))
    }
  }
  return(NA)
}

# Get Table V.C7 values for each worker type
vc7_very_low <- get_vc7_value(raw_data, "very low")
vc7_low <- get_vc7_value(raw_data, "low")
vc7_medium <- get_vc7_value(raw_data, "medium")
vc7_high <- get_vc7_value(raw_data, "high")

cat("Table V.C7 Annual Benefits (2025, in 2025 CPI-W dollars):\n")
cat("  Very Low:", vc7_very_low, "\n")
cat("  Low:     ", vc7_low, "\n")
cat("  Medium:  ", vc7_medium, "\n")
cat("  High:    ", vc7_high, "\n\n")

# Calculate using ssmbar for each worker type
results <- list()

for (type in c("very_low", "low", "medium", "high")) {
  result <- calculate_benefits(
    birth_yr = 1960,
    type = type,
    sex = "all",
    age_claim = 65,
    factors = sef2025,
    assumptions = tr2025,
    debugg = TRUE
  )

  age65 <- result[result$age == 65, ]

  results[[type]] <- list(
    basic_pia = age65$basic_pia,
    cola_basic_pia = age65$cola_basic_pia,
    cola_cum_factor = age65$cola_cum_factor,
    wrk_ben = age65$wrk_ben,
    act_factor = age65$act_factor
  )
}

cat("ssmbar Calculated Values (Monthly):\n")
cat("-----------------------------------------\n")
cat(sprintf("%-10s %10s %15s %12s %10s\n",
            "Type", "basic_pia", "cola_basic_pia", "act_factor", "wrk_ben"))

for (type in c("very_low", "low", "medium", "high")) {
  r <- results[[type]]
  cat(sprintf("%-10s %10d %15d %12.4f %10d\n",
              type, r$basic_pia, r$cola_basic_pia, r$act_factor, r$wrk_ben))
}

cat("\n\nssmbar Calculated Annual Benefits:\n")
cat("-----------------------------------------\n")
cat(sprintf("%-10s %20s %20s\n", "Type", "Annual cola_basic_pia", "Annual wrk_ben"))

for (type in c("very_low", "low", "medium", "high")) {
  r <- results[[type]]
  cat(sprintf("%-10s %20d %20d\n",
              type, r$cola_basic_pia * 12, r$wrk_ben * 12))
}

# Comparison
cat("\n\n=============================================================================\n")
cat("COMPARISON: ssmbar cola_basic_pia vs Table V.C7\n")
cat("=============================================================================\n")
cat("(Table V.C7 shows full PIA at age 65, not reduced benefit)\n\n")

vc7_values <- c(very_low = vc7_very_low, low = vc7_low, medium = vc7_medium, high = vc7_high)

cat(sprintf("%-10s %12s %12s %10s %10s\n",
            "Type", "Table V.C7", "ssmbar", "Diff ($)", "Diff (%)"))
cat("---------------------------------------------------------\n")

for (type in c("very_low", "low", "medium", "high")) {
  expected <- vc7_values[[type]]
  calculated <- results[[type]]$cola_basic_pia * 12
  diff <- calculated - expected
  pct_diff <- (diff / expected) * 100

  cat(sprintf("%-10s %12.0f %12.0f %10.0f %9.2f%%\n",
              type, expected, calculated, diff, pct_diff))
}

cat("\n\n=============================================================================\n")
cat("COLA DETAILS FOR 1960 BIRTH COHORT\n")
cat("=============================================================================\n\n")

# Show COLA history for this worker
cat("Worker turns 62 in 2022, so COLAs start applying from age 62:\n")
cat("- Age 62 (2022): No COLA yet (basic_pia applies)\n")
cat("- Age 63 (2023): 2022 COLA of 8.7% applied\n")
cat("- Age 64 (2024): 2023 COLA of 3.2% applied\n")
cat("- Age 65 (2025): 2024 COLA of 2.5% applied\n\n")

# Get COLA values from assumptions
cat("Historical COLAs from tr2025:\n")
for (yr in 2022:2025) {
  cola_val <- tr2025$cola[tr2025$year == yr]
  cat(sprintf("  Year %d: %.1f%%\n", yr, cola_val))
}

cat("\nCumulative COLA factor at age 65:", results$medium$cola_cum_factor, "\n")
cat("Expected cumulative: 1 * 1.087 * 1.032 * 1.025 =", 1 * 1.087 * 1.032 * 1.025, "\n")
