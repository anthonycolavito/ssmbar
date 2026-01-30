# Test progressivity of IRR and marginal measures across worker types
devtools::load_all()
data(tr2025)
data(sef2025)

cat("=== Progressivity Test Across Worker Types ===\n\n")

worker_types <- c("very_low", "low", "medium", "high", "max")
results <- data.frame()

for (type in worker_types) {
  worker <- calculate_benefits(
    birth_yr = 1960, sex = "male", type = type, age_claim = 67,
    factors = sef2025, assumptions = tr2025, debugg = TRUE
  )

  # IRR
  irr_emp <- internal_rate_of_return(worker, tr2025, include_employer = FALSE)
  irr_tot <- internal_rate_of_return(worker, tr2025, include_employer = TRUE)

  # Marginal analysis
  marginal <- marginal_benefit_analysis(worker, tr2025)
  working <- marginal[marginal$age >= 21 & marginal$age <= 64, ]

  # Net marginal tax rate
  nmtr <- net_marginal_tax_rate(worker, tr2025)
  working_nmtr <- nmtr[nmtr$age >= 21 & nmtr$age <= 64, ]

  # Marginal IRR
  mirr <- marginal_irr(worker, tr2025)
  working_mirr <- mirr[mirr$age >= 21 & mirr$age <= 64 & mirr$in_top_35, ]

  results <- rbind(results, data.frame(
    type = type,
    lifetime_irr_emp = round(irr_emp$irr * 100, 2),
    lifetime_irr_tot = round(irr_tot$irr * 100, 2),
    marginal_pia_rate = unique(working$marginal_pia_rate[!is.na(working$marginal_pia_rate)]),
    mean_nmtr = round(mean(working_nmtr$net_marginal_tax_rate, na.rm = TRUE) * 100, 2),
    mean_marginal_irr = round(mean(working_mirr$marginal_irr, na.rm = TRUE) * 100, 2)
  ))
}

cat("Results by Worker Type (1960 birth cohort, claim at 67):\n\n")
print(results)

cat("\n--- Verification Checks ---\n")
cat("\n1. Lifetime IRR decreases with earnings (progressivity): ")
cat(all(diff(results$lifetime_irr_emp) < 0), "\n")

cat("2. IRR with employer < IRR employee only: ")
cat(all(results$lifetime_irr_tot < results$lifetime_irr_emp), "\n")

cat("3. Marginal PIA rate decreases with earnings: ")
cat(all(diff(results$marginal_pia_rate) <= 0), "\n")

cat("4. Mean NMTR increases with earnings: ")
cat(all(diff(results$mean_nmtr) > 0), "\n")

cat("5. Mean marginal IRR decreases with earnings: ")
cat(all(diff(results$mean_marginal_irr) < 0), "\n")

cat("\n=== Progressivity test completed ===\n")
