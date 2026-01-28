# Validate all worker types against V.C7 for birth years 1960+
# (turning 65 in 2025 or later)
#
# V.C7 is organized by YEAR TURNING 65, not birth year
# Column structure:
#   1: Year turning 65
#   2: NRA (e.g., "67:0")
#   3: Full PIA at NRA
#   4: Replacement rate at NRA
#   5: Claiming age (e.g., "65:0")
#   6: Reduced benefit at age 65
#   7: Replacement rate at 65

library(devtools)
library(readxl)
load_all()

cat("=============================================================================\n")
cat("VALIDATION: ALL WORKER TYPES (Turning 65 in 2025+)\n")
cat("=============================================================================\n\n")

# Load V.C7 data
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, sheet = "V.C7", col_names = FALSE)

# Function to extract V.C7 reduced benefit at 65 for a given worker type and year turning 65
get_vc7_reduced <- function(pattern, year_turn_65) {
  idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(idx) == 0) return(NA)
  start_idx <- idx[1]

  # Find the row for the specified year turning 65
  for (i in (start_idx + 1):min(start_idx + 200, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val)) {
      if (as.character(val) == as.character(year_turn_65)) {
        # Column 6 is the reduced benefit at 65
        result <- as.numeric(raw_data[[6]][i])
        return(result)
      }
    }
  }
  return(NA)
}

# Worker type patterns for V.C7
vc7_patterns <- list(
  very_low = "Scaled very low",
  low = "Scaled low earnings",
  medium = "Scaled medium",
  high = "Scaled high",
  max = "Steady maximum"
)

# Birth years to test (turning 65 in 2025+)
# Year turning 65 = birth_year + 65
birth_years <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000)

# Store results
results <- list()
result_idx <- 0

cat("Calculating benefits for all worker types and birth years...\n\n")

for (by in birth_years) {
  year_turn_65 <- by + 65

  for (type in c("very_low", "low", "medium", "high", "max")) {
    # Get V.C7 value (using year turning 65)
    vc7_val <- get_vc7_reduced(vc7_patterns[[type]], year_turn_65)
    cat(sprintf("DEBUG: by=%d, year_65=%d, type=%s, vc7_val=%s\n",
                by, year_turn_65, type, as.character(vc7_val)))

    # Calculate with ssmbar
    result <- tryCatch({
      calculate_benefits(
        birth_yr = by, type = type, sex = "all",
        age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = FALSE
      )
    }, error = function(e) {
      cat(sprintf("Error calculating: birth_year=%d, type=%s: %s\n", by, type, e$message))
      return(NULL)
    })

    if (!is.null(result)) {
      age65 <- result[result$age == 65, ]
      if (nrow(age65) > 0) {
        # Use 'ben' column (final benefit) - wrk_ben is only in debug output
        our_val <- age65$ben[1] * 12

        # Ensure we have valid scalar values
        vc7_valid <- length(vc7_val) == 1 && !is.na(vc7_val) && vc7_val > 0
        our_valid <- length(our_val) == 1 && !is.na(our_val) && our_val > 0

        if (vc7_valid && our_valid) {
          diff <- our_val - vc7_val
          pct <- (diff / vc7_val) * 100

          result_idx <- result_idx + 1
          results[[result_idx]] <- data.frame(
            birth_year = by,
            year_65 = year_turn_65,
            type = type,
            vc7 = vc7_val,
            ssmbar = our_val,
            diff = diff,
            pct = pct
          )
        }
      }
    }
  }
}

# Combine results
if (length(results) > 0) {
  results_df <- do.call(rbind, results)

  # Display results by worker type
  cat("=============================================================================\n")
  cat("RESULTS BY WORKER TYPE\n")
  cat("=============================================================================\n\n")

  for (type in c("very_low", "low", "medium", "high", "max")) {
    type_results <- results_df[results_df$type == type, ]

    if (nrow(type_results) > 0) {
      cat(sprintf("=== %s ===\n", toupper(type)))
      cat(sprintf("%-10s %-8s %10s %10s %10s %8s\n", "Birth Yr", "Turn 65", "V.C7", "ssmbar", "Diff", "Pct"))
      cat(paste(rep("-", 62), collapse = ""), "\n")

      for (i in 1:nrow(type_results)) {
        cat(sprintf("%-10d %-8d %10.0f %10.0f %10.0f %7.2f%%\n",
                    type_results$birth_year[i],
                    type_results$year_65[i],
                    type_results$vc7[i],
                    type_results$ssmbar[i],
                    type_results$diff[i],
                    type_results$pct[i]))
      }

      avg_pct <- mean(type_results$pct)
      cat(sprintf("\nAverage difference: %.2f%%\n\n", avg_pct))
    }
  }

  # Summary table
  cat("=============================================================================\n")
  cat("SUMMARY: AVERAGE PERCENTAGE DIFFERENCE BY WORKER TYPE\n")
  cat("=============================================================================\n\n")

  cat(sprintf("%-12s %10s %10s %10s\n", "Worker Type", "Avg %", "Min %", "Max %"))
  cat(paste(rep("-", 45), collapse = ""), "\n")

  for (type in c("very_low", "low", "medium", "high", "max")) {
    type_results <- results_df[results_df$type == type, ]
    if (nrow(type_results) > 0) {
      cat(sprintf("%-12s %9.2f%% %9.2f%% %9.2f%%\n",
                  type,
                  mean(type_results$pct),
                  min(type_results$pct),
                  max(type_results$pct)))
    }
  }

  # Overall summary
  cat("\n")
  cat(sprintf("Overall average difference: %.2f%%\n", mean(results_df$pct)))
  cat(sprintf("Overall range: %.2f%% to %.2f%%\n", min(results_df$pct), max(results_df$pct)))

} else {
  cat("No results to display. Check V.C7 data extraction.\n")
}
