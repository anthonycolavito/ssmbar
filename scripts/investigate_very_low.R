# Investigate very_low earner PIA calculation
library(devtools)
load_all()

cat("=============================================================================\n")
cat("VERY LOW EARNER PIA INVESTIGATION\n")
cat("=============================================================================\n\n")

result <- calculate_benefits(
  birth_yr = 1960, type = "very_low", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

age62 <- result[result$age == 62, ]
age65 <- result[result$age == 65, ]

cat("=== AT ELIGIBILITY (AGE 62, YEAR 2022) ===\n")
cat("AIME:", age62$aime, "\n")
cat("Bend points: bp1 =", age62$bp1_elig, ", bp2 =", age62$bp2_elig, "\n")
cat("Factors: fact1 =", age62$fact1_elig, ", fact2 =", age62$fact2_elig, ", fact3 =", age62$fact3_elig, "\n")
cat("basic_pia:", age62$basic_pia, "\n\n")

cat("=== MANUAL PIA CALCULATION ===\n")
aime <- age62$aime
bp1 <- age62$bp1_elig
bp2 <- age62$bp2_elig

cat("AIME =", aime, "\n")
cat("Is AIME > bp1 (", bp1, ")?", aime > bp1, "\n")
cat("Is AIME > bp2 (", bp2, ")?", aime > bp2, "\n\n")

if (aime <= bp1) {
  manual_pia <- 0.9 * aime
  cat("AIME <= bp1, so PIA = 0.9 *", aime, "=", manual_pia, "\n")
} else if (aime <= bp2) {
  manual_pia <- 0.9 * bp1 + 0.32 * (aime - bp1)
  cat("bp1 < AIME <= bp2, so PIA = 0.9 *", bp1, "+ 0.32 * (", aime, "-", bp1, ")\n")
  cat("PIA = ", 0.9 * bp1, "+", 0.32 * (aime - bp1), "=", manual_pia, "\n")
} else {
  manual_pia <- 0.9 * bp1 + 0.32 * (bp2 - bp1) + 0.15 * (aime - bp2)
  cat("AIME > bp2, so full formula\n")
  cat("PIA =", manual_pia, "\n")
}

cat("Floored:", floor(manual_pia), "\n")
cat("Stored basic_pia:", age62$basic_pia, "\n")
cat("Match:", floor(manual_pia) == age62$basic_pia, "\n")

cat("\n\n=== CHECKING BEND POINTS FROM ASSUMPTIONS ===\n")
cat("Bend points in tr2025 for year 2022:\n")
cat("  bp1:", tr2025$bp1[tr2025$year == 2022], "\n")
cat("  bp2:", tr2025$bp2[tr2025$year == 2022], "\n")

cat("\n\n=== AT AGE 65 ===\n")
cat("AIME:", age65$aime, "\n")
cat("basic_pia:", age65$basic_pia, "\n")
cat("cola_basic_pia:", age65$cola_basic_pia, "\n")
cat("Annual:", age65$cola_basic_pia * 12, "\n")

cat("\n\n=== WHAT V.C7 IMPLIES ===\n")
vc7_annual <- 13485
vc7_monthly <- vc7_annual / 12
cola_factor <- 1.087 * 1.032 * 1.025
implied_basic_pia <- vc7_monthly / cola_factor

cat("V.C7 annual:", vc7_annual, "\n")
cat("V.C7 monthly:", round(vc7_monthly, 2), "\n")
cat("COLA factor:", cola_factor, "\n")
cat("Implied basic_pia:", round(implied_basic_pia, 2), "\n")

# What AIME gives that basic_pia?
# If basic_pia < 921.6 (0.9 * 1024), then AIME < 1024
if (implied_basic_pia < 0.9 * 1024) {
  implied_aime <- implied_basic_pia / 0.9
  cat("Implied AIME (all in first bracket):", round(implied_aime, 0), "\n")
} else {
  implied_aime <- 1024 + (implied_basic_pia - 0.9 * 1024) / 0.32
  cat("Implied AIME (in second bracket):", round(implied_aime, 0), "\n")
}

cat("\nOur AIME:", age65$aime, "\n")
cat("Our basic_pia:", age65$basic_pia, "\n")
cat("Difference in basic_pia:", age65$basic_pia - round(implied_basic_pia), "\n")
