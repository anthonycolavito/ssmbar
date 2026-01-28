# Investigate PIA calculation differences for 1960 medium earner

library(devtools)
load_all()

cat("=============================================================================\n")
cat("INVESTIGATING PIA CALCULATION: 1960 Medium Earner\n")
cat("=============================================================================\n\n")

# Calculate with full debug output
result <- calculate_benefits(
  birth_yr = 1960,
  type = "medium",
  sex = "all",
  age_claim = 65,
  factors = sef2025,
  assumptions = tr2025,
  debugg = TRUE
)

age62 <- result[result$age == 62, ]

cat("=== AIME Calculation (at eligibility age 62, year 2022) ===\n")
cat("AIME:", age62$aime, "\n")
cat("Index age:", age62$index_age, "\n")
cat("AWI at index age (60, year 2020):", tr2025$awi[tr2025$year == 2020], "\n\n")

cat("=== PIA Bend Points (for 2022 eligibility) ===\n")
cat("BP1 (at age 62):", age62$bp1_elig, "\n")
cat("BP2 (at age 62):", age62$bp2_elig, "\n")
cat("From tr2025 for 2022:", tr2025$bp1[tr2025$year == 2022], ",", tr2025$bp2[tr2025$year == 2022], "\n\n")

cat("=== PIA Factors ===\n")
cat("Factor 1:", age62$fact1_elig, "\n")
cat("Factor 2:", age62$fact2_elig, "\n")
cat("Factor 3:", age62$fact3_elig, "\n\n")

cat("=== Basic PIA Calculation ===\n")
aime <- age62$aime
bp1 <- age62$bp1_elig
bp2 <- age62$bp2_elig
f1 <- age62$fact1_elig
f2 <- age62$fact2_elig
f3 <- age62$fact3_elig

# Manual calculation
if (aime > bp2) {
  manual_pia <- (f1 * bp1) + (f2 * (bp2 - bp1)) + (f3 * (aime - bp2))
} else if (aime > bp1) {
  manual_pia <- (f1 * bp1) + (f2 * (aime - bp1))
} else {
  manual_pia <- f1 * aime
}

cat("Manual PIA calculation:\n")
cat("  90% of first", bp1, "=", f1 * bp1, "\n")
cat("  32% of next", bp2 - bp1, "=", f2 * (bp2 - bp1), "\n")
if (aime > bp2) {
  cat("  15% of remaining", aime - bp2, "=", f3 * (aime - bp2), "\n")
}
cat("  Total (before rounding):", manual_pia, "\n")
cat("  Total (floored):", floor(manual_pia), "\n")
cat("  Stored basic_pia:", age62$basic_pia, "\n\n")

cat("=== What Table V.C7 Implies ===\n")
vc7_annual <- 29283
vc7_monthly_cola_pia <- vc7_annual / 12
cola_factor <- 1.149829
vc7_implied_basic_pia <- vc7_monthly_cola_pia / cola_factor

cat("V.C7 annual:", vc7_annual, "\n")
cat("V.C7 monthly cola_pia:", round(vc7_monthly_cola_pia, 2), "\n")
cat("V.C7 implied basic_pia (using our COLA factor):", round(vc7_implied_basic_pia, 2), "\n")
cat("Our basic_pia:", age62$basic_pia, "\n")
cat("Difference:", round(age62$basic_pia - vc7_implied_basic_pia, 2), "\n")
cat("Difference %:", round((age62$basic_pia - vc7_implied_basic_pia) / vc7_implied_basic_pia * 100, 2), "%\n\n")

cat("=== Implied AIME from V.C7 ===\n")
# Work backwards: what AIME would give V.C7's implied basic_pia?
# PIA = 0.9 * bp1 + 0.32 * (bp2 - bp1) + 0.15 * (AIME - bp2)
# For our bend points (1024, 6172):
# PIA = 0.9 * 1024 + 0.32 * (6172 - 1024) + 0.15 * (AIME - 6172)
# PIA = 921.6 + 1647.36 + 0.15 * (AIME - 6172)
# 2122.65 = 2568.96 + 0.15 * (AIME - 6172)
# This gives negative, so AIME must be between bp1 and bp2

# If AIME <= bp2:
# PIA = 0.9 * 1024 + 0.32 * (AIME - 1024)
# 2122.65 = 921.6 + 0.32 * (AIME - 1024)
# 1201.05 = 0.32 * (AIME - 1024)
# AIME - 1024 = 3753.28
# AIME = 4777.28

cat("If V.C7 implied basic_pia =", round(vc7_implied_basic_pia, 2), "\n")
implied_aime <- 1024 + (vc7_implied_basic_pia - 921.6) / 0.32
cat("Then implied AIME =", round(implied_aime, 0), "\n")
cat("Our AIME =", aime, "\n")
cat("AIME difference:", round(aime - implied_aime, 0), "\n\n")

cat("=== Sample Earnings Check ===\n")
# Show a few years of earnings
cat("Sample indexed earnings (should peak at age 60 indexing):\n")
sample_ages <- c(40, 50, 55, 60, 61)
for (a in sample_ages) {
  row <- result[result$age == a, ]
  if (nrow(row) > 0 && "indexed_earn" %in% names(row)) {
    cat(sprintf("  Age %d: earnings=%d, indexed=%d\n",
                a, row$earnings, row$indexed_earn))
  }
}
