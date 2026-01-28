# Compare our earnings with PDF Table 4
# Table 4 shows hypothetical earnings for 1960-born preliminary scaled worker

library(devtools)
load_all()

cat("=============================================================================\n")
cat("COMPARISON WITH PDF TABLE 4 EARNINGS\n")
cat("=============================================================================\n\n")

# Table 4 values from PDF (selected years for 1960-born preliminary scaled worker)
# These use PRELIMINARY ADJUSTED SCALED FACTORS, not final factors
# Final factor = preliminary × adjustment ratio
# For medium: adjustment ratio = 1.221

pdf_table4 <- data.frame(
  year = c(1981:2024),
  age = c(21:64),
  prelim_factor = c(0.227, 0.280, 0.357, 0.427, 0.481, 0.529, 0.573, 0.613, 0.649,
                    0.681, 0.709, 0.735, 0.757, 0.778, 0.796, 0.811, 0.825, 0.837,
                    0.847, 0.857, 0.865, 0.872, 0.879, 0.885, 0.889, 0.893, 0.896,
                    0.897, 0.896, 0.896, 0.894, 0.890, 0.885, 0.878, 0.869, 0.852,
                    0.833, 0.812, 0.788, 0.757, 0.715, 0.691, 0.667, 0.644),
  pdf_awi = c(13773.10, 14531.34, 15239.24, 16135.07, 16822.51, 17321.82, 18426.51,
              19334.04, 20099.55, 21027.98, 21811.60, 22935.42, 23132.67, 23753.53,
              24705.66, 25913.90, 27426.00, 28861.44, 30469.84, 32154.82, 32921.92,
              33252.09, 34064.95, 35648.55, 36952.94, 38651.41, 40405.48, 41334.97,
              40711.61, 41673.83, 42979.61, 44321.67, 44888.16, 46481.52, 48098.63,
              48642.15, 50321.89, 52145.80, 54099.99, 55628.60, 60575.07, 63795.13,
              66621.80, 69472.44),
  pdf_prelim_earnings = c(3127.58, 4065.56, 5447.48, 6889.79, 8097.01, 9159.92, 10558.32,
                          11858.11, 13038.86, 14309.58, 15471.56, 16857.69, 17518.53, 18474.93,
                          19655.90, 21016.38, 22613.48, 24146.89, 25806.10, 27544.69, 28476.67,
                          28989.64, 29932.39, 31532.07, 32847.32, 34515.55, 36186.30, 37059.65,
                          36486.68, 37351.50, 38444.14, 39442.83, 39704.03, 40792.27, 41776.26,
                          41451.14, 41912.55, 42331.13, 42630.31, 42104.34, 43337.34, 44072.11,
                          44442.72, 44751.21)
)

# Calculate what medium earner earnings should be (preliminary × 1.221)
pdf_table4$medium_factor <- pdf_table4$prelim_factor * 1.221
pdf_table4$pdf_medium_earnings <- pdf_table4$pdf_prelim_earnings * 1.221

# Now get our calculation
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get the medium factors from sef2025
med_factors <- sef2025[sef2025$worker == "medium", c("age", "factor")]

cat("Comparing MEDIUM EARNER scaled factors and earnings:\n\n")
cat(sprintf("%-5s %-5s %-10s %-10s %-12s %-12s %-10s\n",
            "Age", "Year", "Our Factor", "PDF Factor", "Our Earnings", "PDF Med Earn", "Diff"))
cat(paste(rep("-", 80), collapse = ""), "\n")

for (a in 21:64) {
  our_factor <- med_factors$factor[med_factors$age == a]
  pdf_factor <- pdf_table4$medium_factor[pdf_table4$age == a]

  our_earnings <- result$earnings[result$age == a]
  pdf_med_earn <- pdf_table4$pdf_medium_earnings[pdf_table4$age == a]

  diff <- our_earnings - pdf_med_earn

  cat(sprintf("%-5d %-5d %-10.3f %-10.3f %-12.2f %-12.2f %-10.2f\n",
              a, a + 1960, our_factor, pdf_factor, our_earnings, pdf_med_earn, diff))
}

cat("\n=== SUMMARY ===\n\n")

# Calculate totals
our_total <- sum(result$earnings[result$age >= 21 & result$age <= 64])
pdf_total <- sum(pdf_table4$pdf_medium_earnings)

cat("Total earnings (ages 21-64):\n")
cat("  Our calculation:", round(our_total, 2), "\n")
cat("  PDF medium:", round(pdf_total, 2), "\n")
cat("  Difference:", round(our_total - pdf_total, 2), "\n\n")

# Check AWI values
cat("=== AWI COMPARISON (Sample Years) ===\n\n")
cat(sprintf("%-6s %-12s %-12s %-10s\n", "Year", "Our AWI", "PDF AWI", "Diff"))
cat(paste(rep("-", 45), collapse = ""), "\n")

for (yr in c(2000, 2010, 2020, 2024)) {
  our_awi <- tr2025$awi[tr2025$year == yr]
  pdf_awi <- pdf_table4$pdf_awi[pdf_table4$year == yr]
  diff <- our_awi - pdf_awi

  cat(sprintf("%-6d %-12.2f %-12.2f %-10.2f\n", yr, our_awi, pdf_awi, diff))
}
