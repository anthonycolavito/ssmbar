# Check actuarial factor calculation
library(devtools)
load_all()

cat("=============================================================================\n")
cat("ACTUARIAL FACTOR INVESTIGATION\n")
cat("=============================================================================\n\n")

cat("=== V.C7 IMPLIED ACTUARIAL FACTOR ===\n")
vc7_full_pia <- 29283
vc7_reduced <- 25172
implied_factor <- vc7_reduced / vc7_full_pia
implied_reduction <- 1 - implied_factor

cat("V.C7 full PIA:", vc7_full_pia, "\n")
cat("V.C7 reduced:", vc7_reduced, "\n")
cat("Implied actuarial factor:", round(implied_factor, 6), "\n")
cat("Implied reduction:", round(implied_reduction * 100, 4), "%\n")

cat("\n=== OUR ACTUARIAL FACTOR ===\n")
# For 1960 birth year claiming at 65
# NRA = 67, so 24 months early
# Reduction = 5/9 of 1% per month for first 36 months

months_early <- 24
rf1 <- 5/9 / 100  # 5/9 of 1% = 0.005556 per month
reduction_pct <- months_early * rf1
our_factor <- 1 - reduction_pct

cat("Months early:", months_early, "\n")
cat("Reduction per month (rf1):", rf1, "\n")
cat("Total reduction:", round(reduction_pct * 100, 4), "%\n")
cat("Our actuarial factor:", round(our_factor, 6), "\n")

cat("\n=== COMPARISON ===\n")
cat("V.C7 implied factor:", round(implied_factor, 6), "\n")
cat("Our factor:", round(our_factor, 6), "\n")
cat("Difference:", round((our_factor - implied_factor) * 100, 4), "percentage points\n")

cat("\n=== CHECKING SSA's REDUCTION FORMULA ===\n")
cat("For claiming 24 months early (NRA 67, claim at 65):\n")
cat("Standard formula: 24 × (5/9 of 1%) = 24 × 0.005556 = 13.33%\n")
cat("Factor = 1 - 0.1333 = 0.8667\n\n")

cat("But V.C7 implies 14.03% reduction (factor 0.8597)\n")
cat("This suggests V.C7 might use a different calculation method.\n")

cat("\n=== CHECK ssmbar CALCULATION ===\n")
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

age65 <- result[result$age == 65, ]
cat("ssmbar act_factor:", age65$act_factor, "\n")
cat("ssmbar cola_basic_pia:", age65$cola_basic_pia, "\n")
cat("ssmbar wrk_ben:", age65$wrk_ben, "\n")
cat("Calculated: cola_basic_pia × act_factor =", age65$cola_basic_pia * age65$act_factor, "\n")
cat("Floor of that:", floor(age65$cola_basic_pia * age65$act_factor), "\n")

cat("\n=== WHAT FACTOR WOULD MATCH V.C7? ===\n")
# If our full PIA is 2440 monthly (29,280 annual)
# And V.C7 reduced is 25,172 annual (2097.67 monthly)
needed_factor <- 25172 / 29280
cat("Our full PIA annual:", 29280, "\n")
cat("V.C7 reduced annual:", 25172, "\n")
cat("Factor needed to match:", round(needed_factor, 6), "\n")

# What if V.C7 rounds differently?
cat("\n=== ROUNDING INVESTIGATION ===\n")
# V.C7 full PIA = 29283, which is 2440.25 monthly
# If they floor the monthly: 2440
# Then apply factor: 2440 × 0.8597 = 2097.67 → floor = 2097
# Annual = 2097 × 12 = 25164 (not 25172)

# Or if they use exact monthly PIA of 2440.25:
# 2440.25 × 0.8597 = 2098.02 → floor = 2098
# Annual = 25176 (closer but not exact)

cat("If V.C7 monthly PIA = 2440.25 (29283/12):\n")
cat("  × 0.8667 (our factor) = 2114.88 → annual = 25379\n")
cat("  × 0.8597 (implied) = 2097.87 → annual = 25174 (close to 25172)\n")
