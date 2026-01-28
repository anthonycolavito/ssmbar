# Check if V.C7 might round monthly then multiply by 12
library(devtools)
load_all()

cat("=============================================================================\n")
cat("MONTHLY ROUNDING INVESTIGATION\n")
cat("=============================================================================\n\n")

# V.C7 values for medium earner
vc7_full_annual <- 29283
vc7_reduced_annual <- 25172

cat("=== V.C7 VALUES ===\n")
cat("Full PIA annual:", vc7_full_annual, "\n")
cat("Reduced annual:", vc7_reduced_annual, "\n")

cat("\n=== TESTING DIFFERENT ROUNDING APPROACHES ===\n\n")

# Approach 1: Our current method
# floor(monthly_pia × factor) × 12
our_monthly_pia <- 2440  # floor(29280/12) or our calculated value
our_factor <- 0.8667
our_reduced_monthly <- floor(our_monthly_pia * our_factor)
our_reduced_annual <- our_reduced_monthly * 12
cat("Approach 1 (our method):\n")
cat("  Monthly PIA:", our_monthly_pia, "\n")
cat("  Factor:", our_factor, "\n")
cat("  Reduced monthly: floor(", our_monthly_pia, "×", our_factor, ") =", our_reduced_monthly, "\n")
cat("  Reduced annual:", our_reduced_annual, "\n\n")

# Approach 2: V.C7's monthly PIA (29283/12 = 2440.25)
vc7_monthly_pia <- vc7_full_annual / 12
cat("Approach 2 (using V.C7 exact monthly):\n")
cat("  V.C7 monthly PIA:", vc7_monthly_pia, "\n")

# What factor gives the exact V.C7 reduced amount?
# If annual reduced = monthly_reduced × 12
# Then monthly_reduced = 25172/12 = 2097.67
vc7_monthly_reduced <- vc7_reduced_annual / 12
implied_factor <- vc7_monthly_reduced / vc7_monthly_pia
cat("  V.C7 implied monthly reduced:", round(vc7_monthly_reduced, 2), "\n")
cat("  Implied factor:", round(implied_factor, 6), "\n\n")

# Check if they might floor monthly first, then apply factor
cat("Approach 3 (floor monthly PIA first):\n")
monthly_floored <- floor(vc7_monthly_pia)
cat("  Floored monthly PIA:", monthly_floored, "\n")
cat("  × our factor 0.8667 =", monthly_floored * 0.8667, "→ floor =", floor(monthly_floored * 0.8667), "\n")
cat("  Annual:", floor(monthly_floored * 0.8667) * 12, "\n\n")

# Check actuarial factor calculation more precisely
cat("=== PRECISE ACTUARIAL FACTOR CHECK ===\n")
cat("For NRA 67, claiming at 65 (24 months early):\n")
cat("  rf1 = 5/9 of 1% = 5/900 = 0.00555556 per month\n")
cat("  24 months × 0.00555556 = 0.13333333\n")
cat("  Factor = 1 - 0.13333333 = 0.86666667\n\n")

# What if SSA truncates or rounds differently?
cat("=== WHAT IF SSA ROUNDS THE FACTOR? ===\n")
# If they round to 4 decimal places: 0.8667
# If they truncate to 4 decimal places: 0.8666
cat("Factor rounded to 4 decimals: 0.8667\n")
cat("Factor truncated to 4 decimals: 0.8666\n")
cat("Neither gives 0.8596...\n\n")

cat("=== REVERSE ENGINEERING V.C7 ===\n")
# What monthly reduced gives exactly 25172 annual?
# It would need to be 25172/12 = 2097.666...
# But we floor, so monthly must be 2097 to give ≤25172
# 2097 × 12 = 25164 (not 25172)
# So they might NOT be flooring monthly

# If 2098 × 12 = 25176 (close to 25172)
# If they round monthly to nearest dollar:
#   2440.25 × 0.8596 = 2097.87 → round = 2098 → annual = 25176

cat("If monthly reduced = 2097, annual = 25164\n")
cat("If monthly reduced = 2098, annual = 25176\n")
cat("V.C7 shows 25172, which is between these.\n")
cat("This suggests V.C7 might NOT floor monthly, or uses different rounding.\n")
