# Test script for new analytical functions
devtools::load_all()
data(tr2025)
data(sef2025)

cat("=== IRR Test - Single Worker ===\n\n")

# Test with a single medium worker
worker <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)

cat("Worker created successfully\n")
cat("Rows:", nrow(worker), "\n\n")

# Test IRR
cat("Testing internal_rate_of_return()...\n")
irr_result <- internal_rate_of_return(worker, tr2025, include_employer = FALSE)
cat("IRR (employee only):", round(irr_result$irr * 100, 2), "%\n")

irr_total <- internal_rate_of_return(worker, tr2025, include_employer = TRUE)
cat("IRR (with employer):", round(irr_total$irr * 100, 2), "%\n\n")

# Test marginal analysis
cat("Testing marginal_benefit_analysis()...\n")
marginal <- marginal_benefit_analysis(worker, tr2025)
cat("Rows returned:", nrow(marginal), "\n")
cat("Columns:", paste(names(marginal), collapse = ", "), "\n")

# Show sample of marginal analysis for working years
working_marginal <- marginal[marginal$age >= 21 & marginal$age <= 64, ]
cat("\nSample marginal analysis (ages 30, 40, 50, 60):\n")
print(working_marginal[working_marginal$age %in% c(30, 40, 50, 60),
                       c("age", "earnings", "in_top_35", "indexed_rank",
                         "marginal_pia_rate", "delta_pv_benefits")])

# Test net marginal tax rate
cat("\nTesting net_marginal_tax_rate()...\n")
nmtr <- net_marginal_tax_rate(worker, tr2025)
working_nmtr <- nmtr[nmtr$age >= 21 & nmtr$age <= 64, ]
cat("Mean NMTR:", round(mean(working_nmtr$net_marginal_tax_rate, na.rm = TRUE) * 100, 2), "%\n")
cat("Range:", round(min(working_nmtr$net_marginal_tax_rate, na.rm = TRUE) * 100, 2), "% to",
    round(max(working_nmtr$net_marginal_tax_rate, na.rm = TRUE) * 100, 2), "%\n")

# Test marginal IRR
cat("\nTesting marginal_irr()...\n")
mirr <- marginal_irr(worker, tr2025)
working_mirr <- mirr[mirr$age >= 21 & mirr$age <= 64, ]
top35_mirr <- working_mirr[working_mirr$in_top_35, ]
cat("Mean marginal IRR (top 35 years):", round(mean(top35_mirr$marginal_irr, na.rm = TRUE) * 100, 2), "%\n")

# Count years not in top 35 (should have IRR = -1)
not_top35 <- working_mirr[!working_mirr$in_top_35, ]
cat("Years not in top 35:", nrow(not_top35), "\n")
if (nrow(not_top35) > 0) {
  cat("All have IRR = -1:", all(not_top35$marginal_irr == -1, na.rm = TRUE), "\n")
}

cat("\n=== All tests completed ===\n")
