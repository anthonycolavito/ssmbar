library(devtools)
load_all()

# Calculate for 1960 medium earner
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get values at age 65
age65 <- result[result$age == 65, ]
cat("=== 1960 MEDIUM EARNER AT AGE 65 ===\n")
cat("ben (total benefit):", age65$ben, "\n")
cat("wrk_ben:", age65$wrk_ben, "\n")
cat("cola_basic_pia (full PIA):", age65$cola_basic_pia, "\n")
cat("Actuarial factor:", age65$act_factor, "\n")
cat("basic_pia:", age65$basic_pia, "\n")
cat("\n")
cat("Annual ben * 12:", age65$ben * 12, "\n")
cat("Annual cola_basic_pia * 12:", age65$cola_basic_pia * 12, "\n")
cat("Table V.C7 expected: 29283\n")
cat("\nDifference from Table V.C7 (using full cola_basic_pia):",
    (age65$cola_basic_pia * 12) - 29283, "\n")
cat("Percent difference:", round(((age65$cola_basic_pia * 12) - 29283) / 29283 * 100, 2), "%\n")

# Also check the basic_pia (before COLA)
cat("\n\nBasic PIA (before COLA):", age65$basic_pia, "\n")
cat("AIME:", age65$aime, "\n")

# Check COLA factors
cat("\nCOLA details at age 65:\n")
cat("cola_factor:", age65$cola_factor, "\n")
cat("cola_cum_factor:", age65$cola_cum_factor, "\n")
