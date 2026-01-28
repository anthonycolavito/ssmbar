# Debug COLA calculation
library(devtools)
load_all()

cat("=== COLA VALUES IN tr2025 ===\n")
cola_years <- tr2025[tr2025$year >= 2020 & tr2025$year <= 2027, c("year", "cola", "cpi_w")]
print(cola_years)

cat("\n=== EXPECTED COLA CALCULATION FOR 1960 BIRTH YEAR ===\n")
cat("Worker born 1960:\n")
cat("- Age 62 in 2022: No COLA applied yet (eligibility year)\n")
cat("- Age 63 in 2023: Apply 2022 COLA (8.7%)\n")
cat("- Age 64 in 2024: Apply 2023 COLA (3.2%)\n")
cat("- Age 65 in 2025: Apply 2024 COLA (2.5%)\n\n")

# Calculate expected cumulative factor
expected_factor <- 1 * 1.087 * 1.032 * 1.025
cat("Expected cumulative COLA factor at 65:", expected_factor, "\n")

# Get actual values from result
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

cat("\n=== ACTUAL COLA FROM CALCULATION ===\n")
for (a in 62:65) {
  row <- result[result$age == a, ]
  cat(sprintf("Age %d (year %d): cola_factor=%.4f, cola_cum_factor=%.4f, basic_pia=%d, cola_basic_pia=%d\n",
              a, row$year, row$cola_factor, row$cola_cum_factor, row$basic_pia, row$cola_basic_pia))
}

# Check the raw COLA values that should be applied
cat("\n=== RAW COLA VALUES FROM ASSUMPTIONS ===\n")
for (yr in 2022:2025) {
  cola_val <- tr2025$cola[tr2025$year == yr]
  cat(sprintf("Year %d COLA: %s\n", yr, ifelse(is.na(cola_val), "NA", sprintf("%.1f%%", cola_val))))
}

# Check if cola column exists and has correct values
cat("\n=== COLA COLUMN IN RESULT ===\n")
for (a in 62:65) {
  row <- result[result$age == a, ]
  if ("cola" %in% names(row)) {
    cat(sprintf("Age %d: cola value = %s\n", a, ifelse(is.na(row$cola), "NA", sprintf("%.1f", row$cola))))
  }
}
