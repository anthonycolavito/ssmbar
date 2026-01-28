# Final AIME analysis to understand V.C7 discrepancy
library(devtools)
load_all()

# Calculate for 1960 medium earner with debug
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

age65 <- result[result$age == 65, ]

cat("=== 1960 MEDIUM EARNER AT AGE 65 ===\n\n")
cat("AIME:", age65$aime, "\n")
cat("basic_pia:", age65$basic_pia, "\n")
cat("cola_basic_pia:", age65$cola_basic_pia, "\n")
cat("cola_cum_factor:", age65$cola_cum_factor, "\n")
cat("Annual cola_basic_pia:", age65$cola_basic_pia * 12, "\n")

cat("\n=== TABLE V.C7 COMPARISON ===\n")
vc7 <- 29283
cat("V.C7 annual benefit:", vc7, "\n")
cat("Our annual cola_basic_pia:", age65$cola_basic_pia * 12, "\n")
cat("Difference:", (age65$cola_basic_pia * 12) - vc7, "\n")
cat("Difference %:", round(((age65$cola_basic_pia * 12) - vc7) / vc7 * 100, 2), "%\n")

cat("\n=== WORKING BACKWARDS FROM V.C7 ===\n")
vc7_monthly <- vc7 / 12
cat("V.C7 monthly:", round(vc7_monthly, 2), "\n")
cat("Our cola_cum_factor:", age65$cola_cum_factor, "\n")
implied_basic_pia <- vc7_monthly / age65$cola_cum_factor
cat("V.C7 implied basic_pia:", round(implied_basic_pia, 2), "\n")
cat("Our basic_pia:", age65$basic_pia, "\n")
cat("Difference:", round(age65$basic_pia - implied_basic_pia, 2), "\n")

cat("\n=== AIME CHECK ===\n")
# Bend points for 2022 (eligibility year)
bp1 <- 1024
bp2 <- 6172
cat("Bend points (2022): bp1 =", bp1, ", bp2 =", bp2, "\n")

# Work backwards: what AIME gives the implied basic_pia?
# For AIME between bp1 and bp2: basic_pia = 0.90 * bp1 + 0.32 * (AIME - bp1)
implied_aime <- bp1 + (implied_basic_pia - 0.90 * bp1) / 0.32
cat("V.C7 implied AIME:", round(implied_aime, 0), "\n")
cat("Our AIME:", age65$aime, "\n")
cat("AIME difference:", round(age65$aime - implied_aime, 0), "\n")

cat("\n=== CORRECTLY CALCULATED AIME ===\n")
# What should AIME be using ONLY earnings through age 64 (year before 65)?
# (The current calculation might include age 65 which has 0 earnings anyway)

# Actually at age 65, AIME should include earnings through age 64
# Let's verify by manual calculation
through_64 <- result[result$age >= 21 & result$age <= 64, ]
top35 <- head(through_64[order(-through_64$indexed_earn), ], 35)
correct_aime <- floor(sum(top35$indexed_earn) / 420)
cat("Correct AIME (through age 64):", correct_aime, "\n")
cat("Stored AIME at 65:", age65$aime, "\n")

# What basic_pia does correct AIME give?
correct_basic_pia <- floor(0.90 * bp1 + 0.32 * (correct_aime - bp1))
cat("Correct basic_pia:", correct_basic_pia, "\n")

# What annual benefit does that give?
correct_annual <- correct_basic_pia * age65$cola_cum_factor * 12
cat("Correct annual benefit:", round(correct_annual, 0), "\n")
cat("V.C7 annual:", vc7, "\n")
cat("Remaining difference:", round(correct_annual - vc7, 0), "\n")
cat("Remaining % diff:", round((correct_annual - vc7) / vc7 * 100, 2), "%\n")

cat("\n=== SCALED EARNINGS FACTOR CHECK ===\n")
# Are our scaled earnings factors matching SSA's?
cat("Sample scaled earnings:\n")
sample_ages <- c(50, 55, 60)
for (a in sample_ages) {
  row <- result[result$age == a, ]
  cat(sprintf("  Age %d (year %d): earnings = %.2f, AWI = %.2f\n",
              a, row$year, row$earnings, tr2025$awi[tr2025$year == row$year]))
}
