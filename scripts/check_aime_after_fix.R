# Check AIME and COLA values after fixes
library(devtools)
load_all()

result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

cat("=== AIME and PIA at key ages ===\n")
for (a in c(62, 63, 64, 65)) {
  row <- result[result$age == a, ]
  cat(sprintf("Age %d: AIME=%d, basic_pia=%d, cola_factor=%.4f, cola_cum_factor=%.4f, cola_basic_pia=%d\n",
              a, row$aime, row$basic_pia, row$cola_factor, row$cola_cum_factor, row$cola_basic_pia))
}

# Check what earnings are now used for AIME at age 62
cat("\n=== Earnings at key ages ===\n")
for (a in c(60, 61, 62)) {
  row <- result[result$age == a, ]
  cat(sprintf("Age %d (year %d): indexed_earn = %.2f\n", a, row$year, row$indexed_earn))
}

cat("\n=== Verification ===\n")
cat("At age 62, AIME should now use earnings through age 61 only\n")
cat("This means age 62 earnings (", result$indexed_earn[result$age == 62], ") should NOT be in AIME calculation\n")

# Manual check: top 35 through age 61
through_61 <- result[result$age >= 21 & result$age <= 61, ]
top35 <- head(through_61[order(-through_61$indexed_earn), ], 35)
manual_aime <- floor(sum(top35$indexed_earn) / 420)
cat("\nManual AIME (through age 61):", manual_aime, "\n")
cat("Stored AIME at age 62:", result$aime[result$age == 62], "\n")
cat("Match:", manual_aime == result$aime[result$age == 62], "\n")
