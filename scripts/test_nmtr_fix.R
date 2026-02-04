# Test NMTR fix for nominal discounting
# Run with: Rscript scripts/test_nmtr_fix.R

devtools::load_all()
data(tr2025)
data(sef2025)

cat("Testing marginal_benefit_analysis with nominal discounting...\n\n")

# Create test worker
w1960 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)

cat("Worker calculated successfully\n")
cat("Calling marginal_benefit_analysis...\n")

# Test marginal analysis
marginal <- marginal_benefit_analysis(w1960, tr2025)

cat("Marginal analysis completed!\n\n")

# Show key results
cat("Results for years 9-12 (eligibility transition):\n")
print(marginal[marginal$years_worked >= 9 & marginal$years_worked <= 12,
               c("age", "year", "years_worked", "eligible", "cumulative_pv", "delta_pv_benefits")])

cat("\n\nTesting net_marginal_tax_rate...\n")
nmtr <- net_marginal_tax_rate(w1960, tr2025)

cat("NMTR results for years 9-12:\n")
print(nmtr[nmtr$years_worked >= 9 & nmtr$years_worked <= 12,
           c("age", "years_worked", "earnings", "ss_tax", "delta_pv_benefits", "net_marginal_tax_rate")])

# Compare two cohorts
cat("\n\n=== Comparing Birth Cohorts ===\n")

w1990 <- calculate_benefits(
  birth_yr = 1990, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)

nmtr1960 <- net_marginal_tax_rate(w1960, tr2025)
nmtr1990 <- net_marginal_tax_rate(w1990, tr2025)

cat("\nBirth 1960 - Year 10 (eligibility):\n")
y10_1960 <- nmtr1960[nmtr1960$years_worked == 10, ]
cat(sprintf("  Earnings: $%.0f\n", y10_1960$earnings))
cat(sprintf("  SS Tax: $%.0f\n", y10_1960$ss_tax))
cat(sprintf("  Delta PV Benefits: $%.0f\n", y10_1960$delta_pv_benefits))
cat(sprintf("  NMTR: %.2f%%\n", y10_1960$net_marginal_tax_rate * 100))

cat("\nBirth 1990 - Year 10 (eligibility):\n")
y10_1990 <- nmtr1990[nmtr1990$years_worked == 10, ]
cat(sprintf("  Earnings: $%.0f\n", y10_1990$earnings))
cat(sprintf("  SS Tax: $%.0f\n", y10_1990$ss_tax))
cat(sprintf("  Delta PV Benefits: $%.0f\n", y10_1990$delta_pv_benefits))
cat(sprintf("  NMTR: %.2f%%\n", y10_1990$net_marginal_tax_rate * 100))

cat("\nYear 25 (age 45) comparison:\n")
y25_1960 <- nmtr1960[nmtr1960$years_worked == 25, ]
y25_1990 <- nmtr1990[nmtr1990$years_worked == 25, ]
cat(sprintf("  1960 cohort NMTR: %.2f%%\n", y25_1960$net_marginal_tax_rate * 100))
cat(sprintf("  1990 cohort NMTR: %.2f%%\n", y25_1990$net_marginal_tax_rate * 100))

cat("\nYear 40 (age 60) comparison:\n")
y40_1960 <- nmtr1960[nmtr1960$years_worked == 40, ]
y40_1990 <- nmtr1990[nmtr1990$years_worked == 40, ]
cat(sprintf("  1960 cohort NMTR: %.2f%%\n", y40_1960$net_marginal_tax_rate * 100))
cat(sprintf("  1990 cohort NMTR: %.2f%%\n", y40_1990$net_marginal_tax_rate * 100))

cat("\n=== Test Complete ===\n")
