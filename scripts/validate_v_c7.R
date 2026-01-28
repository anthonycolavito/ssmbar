# Validation Script: Compare ssmbar calculations to Table V.C7
# Source: 2025 Trustees Report Table V.C7 - Annual Scheduled Benefit Amounts
#
# IMPORTANT: Table V.C7 shows FULL COLA-adjusted PIA at age 65
# (without early retirement reduction applied).
# Our calculator applies early retirement reduction when claiming before NRA.
# For validation, we compare cola_basic_pia (not wrk_ben) to Table V.C7.

library(devtools)
library(readxl)
library(dplyr)
library(tidyr)
load_all()

# =============================================================================
# 1. Load and Parse Validation Data from workers.xlsx
# =============================================================================

cat("Loading validation data from Table V.C7...\n")

workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

parse_worker_section <- function(raw_data, start_phrase) {
  start_idx <- which(grepl(start_phrase, raw_data[[1]], ignore.case = TRUE))
  if (length(start_idx) == 0) return(NULL)
  start_idx <- start_idx[1] + 1

  end_idx <- nrow(raw_data)

  section_data <- raw_data[start_idx:end_idx, ]
  section_data <- section_data[grepl("^\\d{4}$", section_data[[1]]), ]

  if (nrow(section_data) == 0) return(NULL)

  data.frame(
    year_at_65 = as.numeric(section_data[[1]]),
    birth_yr = as.numeric(section_data[[1]]) - 65,
    benefit_2025 = as.numeric(section_data[[3]])
  )
}

very_low_data <- parse_worker_section(raw_data, "Scaled very low earnings")
low_data <- parse_worker_section(raw_data, "Scaled low earnings")
medium_data <- parse_worker_section(raw_data, "Scaled medium earnings")
high_data <- parse_worker_section(raw_data, "Scaled high earnings")

if (!is.null(very_low_data)) very_low_data$type <- "very_low"
if (!is.null(low_data)) low_data$type <- "low"
if (!is.null(medium_data)) medium_data$type <- "medium"
if (!is.null(high_data)) high_data$type <- "high"

validation_data <- bind_rows(very_low_data, low_data, medium_data, high_data)
cat("  Parsed", nrow(validation_data), "rows from Table V.C7\n")

# =============================================================================
# 2. Get CPI-W for 2025 (reference year for conversion)
# =============================================================================

cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cat("\nCPI-W for 2025:", cpi_2025, "\n")

# =============================================================================
# 3. Calculate Benefits Using ssmbar and Compare cola_basic_pia
# =============================================================================

# Focus on cohorts from 1960-2000
test_cohorts <- validation_data %>%
  filter(birth_yr >= 1960 & birth_yr <= 2000) %>%
  distinct(birth_yr, type)

cat("\nCalculating and comparing", nrow(test_cohorts), "test cases...\n")
cat("NOTE: Comparing COLA-adjusted PIA (not reduced benefit) to Table V.C7\n\n")

results_list <- list()

for (i in 1:nrow(test_cohorts)) {
  birth_yr <- test_cohorts$birth_yr[i]
  type <- test_cohorts$type[i]

  expected_row <- validation_data %>%
    filter(birth_yr == !!birth_yr, type == !!type) %>%
    slice(1)

  if (nrow(expected_row) == 0) next
  expected <- expected_row$benefit_2025
  if (is.na(expected)) next

  # Calculate using ssmbar with debugg to get cola_basic_pia
  result <- calculate_benefits(
    birth_yr = birth_yr,
    type = type,
    sex = "all",
    age_claim = 65,  # Matches Table V.C7
    factors = sef2025,
    assumptions = tr2025,
    debugg = TRUE
  )

  age_65_row <- result[result$age == 65, ]

  if (nrow(age_65_row) > 0) {
    year_at_65 <- birth_yr + 65
    cpi_at_65 <- tr2025$cpi_w[tr2025$year == year_at_65]

    # Use cola_basic_pia (full PIA, no early retirement reduction)
    annual_pia_nominal <- age_65_row$cola_basic_pia * 12

    # Convert to 2025 dollars
    calculated_2025 <- if (length(cpi_at_65) > 0 && !is.na(cpi_at_65)) {
      annual_pia_nominal * (cpi_2025 / cpi_at_65)
    } else {
      NA
    }

    diff <- calculated_2025 - expected
    pct_diff <- (diff / expected) * 100

    results_list[[length(results_list) + 1]] <- data.frame(
      birth_yr = birth_yr,
      type = type,
      expected = expected,
      calculated = round(calculated_2025, 0),
      diff = round(diff, 0),
      pct_diff = round(pct_diff, 2)
    )
  }
}

comparison_results <- bind_rows(results_list)

# =============================================================================
# 4. Print Comparison Results
# =============================================================================

cat("=============================================================================\n")
cat("COMPARISON: cola_basic_pia vs Table V.C7 (2025 CPI-W Dollars)\n")
cat("=============================================================================\n\n")

for (wtype in c("very_low", "low", "medium", "high")) {
  cat("\n", toupper(wtype), "EARNERS:\n")
  cat("-----------------------------------------\n")
  type_results <- comparison_results %>% filter(type == wtype)
  if (nrow(type_results) > 0) {
    print(type_results %>% select(-type), row.names = FALSE)
  }
}

# =============================================================================
# 5. Summary Statistics
# =============================================================================

cat("\n\n=============================================================================\n")
cat("SUMMARY BY WORKER TYPE\n")
cat("=============================================================================\n\n")

summary_stats <- comparison_results %>%
  group_by(type) %>%
  summarise(
    n = n(),
    mean_diff = round(mean(diff, na.rm = TRUE), 0),
    mean_pct_diff = round(mean(pct_diff, na.rm = TRUE), 2),
    max_abs_diff = round(max(abs(diff), na.rm = TRUE), 0),
    within_1pct = sum(abs(pct_diff) <= 1),
    within_2pct = sum(abs(pct_diff) <= 2),
    within_5pct = sum(abs(pct_diff) <= 5)
  )

print(summary_stats, row.names = FALSE)

# =============================================================================
# 6. Overall Assessment
# =============================================================================

cat("\n\n=============================================================================\n")
cat("OVERALL ASSESSMENT\n")
cat("=============================================================================\n")

overall_mean_diff <- mean(abs(comparison_results$pct_diff), na.rm = TRUE)
max_diff <- max(abs(comparison_results$pct_diff), na.rm = TRUE)
within_2pct <- sum(abs(comparison_results$pct_diff) <= 2)
within_5pct <- sum(abs(comparison_results$pct_diff) <= 5)
total_cases <- nrow(comparison_results)

cat("\nTotal test cases:", total_cases, "\n")
cat("Mean absolute % difference:", round(overall_mean_diff, 2), "%\n")
cat("Max absolute % difference:", round(max_diff, 2), "%\n")
cat("Cases within 2% tolerance:", within_2pct, "/", total_cases,
    "(", round(within_2pct/total_cases*100, 1), "%)\n")
cat("Cases within 5% tolerance:", within_5pct, "/", total_cases,
    "(", round(within_5pct/total_cases*100, 1), "%)\n")

if (overall_mean_diff <= 5) {
  cat("\n*** VALIDATION PASSED: Mean difference is within 5% tolerance ***\n")
} else {
  cat("\n*** VALIDATION REQUIRES REVIEW: Mean difference exceeds 5% ***\n")
}

cat("\nNote: Small systematic differences may be due to:\n")
cat("- Rounding differences in intermediate calculations\n")
cat("- COLA timing assumptions (Q3 CPI-W measurement)\n")
cat("- AWI projection methodology differences\n")
