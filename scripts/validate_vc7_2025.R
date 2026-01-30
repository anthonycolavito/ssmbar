# Validation script: Compare ssmbar benefits against Table V.C7 (2025 Trustees Report)
# Focus on workers turning 65 in 2025 and later
#
# V.C7 shows benefits in 2025 CPI-W indexed dollars
# The "reduced benefit at 65" is the second column from the right
# Each worker type has its own section in the table

library(readxl)
library(dplyr)

# Set working directory and load package
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# Read V.C7 data - use raw read with no assumed column names
vc7_path <- "C:/Users/AnthonyColavito/instructions/VC7.xlsx"
raw_data <- read_excel(vc7_path, col_names = FALSE)

# Show structure
cat("\n========================================\n")
cat("V.C7 Excel File Structure\n")
cat("========================================\n")
cat("Number of columns:", ncol(raw_data), "\n")
cat("Number of rows:", nrow(raw_data), "\n\n")

# Print first column to understand the structure
cat("First 30 values in column 1 (to find section headers):\n")
for (i in 1:min(30, nrow(raw_data))) {
  val <- raw_data[[1]][i]
  if (!is.na(val)) {
    cat(sprintf("Row %d: %s\n", i, as.character(val)))
  }
}
cat("\n")

# Worker type patterns for V.C7 - these identify each section
# Note: Need to use exact patterns to avoid "low" matching "very low"
vc7_patterns <- list(
  very_low = "Scaled very low",
  low = "Scaled low earnings",
  medium = "Scaled medium",
  high = "Scaled high",
  max = "Steady maximum"
)

# The benefit at 65 is second from right (user specification)
ben_col <- ncol(raw_data) - 1
cat("Using column", ben_col, "for benefit at 65\n\n")

# Function to extract reduced benefit at 65 for a given worker type and year turning 65
get_vc7_reduced <- function(pattern, year_turn_65) {
  # Find the section header for this worker type
  idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(idx) == 0) return(NA)
  start_idx <- idx[1]

  # Search for the year turning 65 in rows below the header
  for (i in (start_idx + 1):min(start_idx + 200, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val)) {
      # Check if this looks like a year (4 digits)
      val_str <- as.character(val)
      if (nchar(val_str) == 4 && grepl("^[0-9]+$", val_str)) {
        if (as.numeric(val_str) == year_turn_65) {
          # Return the benefit at 65 (second from right)
          result <- as.numeric(raw_data[[ben_col]][i])
          return(result)
        }
      }
    }
  }
  return(NA)
}

# Get 2025 CPI-W for deflation
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cat(sprintf("2025 CPI-W: %.3f\n\n", cpi_2025))

# Birth years to test (turning 65 in 2025-2035)
birth_years <- 1960:1970
worker_types <- c("very_low", "low", "medium", "high", "max")

results <- data.frame()

for (by in birth_years) {
  yr65 <- by + 65

  for (wtype in worker_types) {
    # Get V.C7 value for this worker type and year
    vc7_val <- get_vc7_reduced(vc7_patterns[[wtype]], yr65)

    if (is.na(vc7_val) || vc7_val == 0) {
      cat(sprintf("No V.C7 data for %s, year turning 65 = %d\n", wtype, yr65))
      next
    }

    # Calculate benefit at age 65
    worker <- calculate_benefits(
      birth_yr = by,
      sex = "male",
      type = wtype,
      age_claim = 65,
      factors = sef2025,
      assumptions = tr2025
    )

    # Get benefit at age 65 (monthly)
    ben_65 <- worker$ben[worker$age == 65]

    # Get CPI-W for benefit year
    cpi_yr <- tr2025$cpi_w[tr2025$year == yr65]

    # Convert to annual, deflate to 2025 CPI-W dollars
    annual_nominal <- ben_65 * 12
    annual_2025_dollars <- annual_nominal * (cpi_2025 / cpi_yr)

    # Calculate difference
    diff_pct <- (annual_2025_dollars - vc7_val) / vc7_val * 100

    results <- rbind(results, data.frame(
      year_turn_65 = yr65,
      birth_yr = by,
      worker_type = wtype,
      ssmbar_nominal = round(annual_nominal, 0),
      ssmbar_2025cpi = round(annual_2025_dollars, 0),
      vc7_2025cpi = vc7_val,
      diff_dollars = round(annual_2025_dollars - vc7_val, 0),
      diff_pct = round(diff_pct, 2)
    ))
  }
}

cat("\n========================================\n")
cat("ssmbar vs V.C7 Comparison (Benefits at Age 65)\n")
cat("All values in annual 2025 CPI-W dollars\n")
cat("========================================\n\n")

# Display by worker type
for (wtype in worker_types) {
  cat(paste0("\n--- ", toupper(wtype), " EARNER ---\n"))
  subset <- results %>%
    filter(worker_type == wtype) %>%
    select(year_turn_65, birth_yr, ssmbar_2025cpi, vc7_2025cpi, diff_dollars, diff_pct)
  print(subset)
}

# Summary statistics
cat("\n========================================\n")
cat("Summary Statistics by Worker Type\n")
cat("========================================\n\n")

summary_stats <- results %>%
  group_by(worker_type) %>%
  summarize(
    avg_diff_pct = mean(diff_pct),
    min_diff_pct = min(diff_pct),
    max_diff_pct = max(diff_pct),
    avg_diff_dollars = mean(diff_dollars),
    .groups = "drop"
  ) %>%
  arrange(factor(worker_type, levels = worker_types))

print(summary_stats)

cat("\nOverall average difference:", round(mean(abs(results$diff_pct)), 2), "%\n")
