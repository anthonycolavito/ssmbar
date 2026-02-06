# Trace the 0.17% V.C7 gap: factor precision from Actuarial Note 2025.3
#
# Key discovery: Table 6 publishes final scaled factors to 3 decimal places,
# but the actual computation uses preliminary adjusted factors (6+ digits)
# multiplied by the ratio (e.g., 1.221 for medium). The 3-digit rounding
# in Table 6 loses precision.
#
# Table 4 gives us the EXACT preliminary adjusted factors (to 3 decimals
# in the table, but the text says age 22 = 0.279779 rounded to 0.280).
# More importantly, Table 4 gives us the EXACT EARNINGS for each age,
# which we can compare to our factor × AWI computation.

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# =====================================================================
# Table 4 data: Preliminary adjusted scaled worker earnings (1960-born)
# These are the EXACT earnings SSA computes before applying the ratio
# =====================================================================
table4 <- data.frame(
  year = 1981:2024,
  age = 21:64,
  prelim_factor = c(
    0.227, 0.280, 0.357, 0.427, 0.481,  # ages 21-25
    0.529, 0.573, 0.613, 0.649, 0.681,  # ages 26-30
    0.709, 0.735, 0.757, 0.778, 0.796,  # ages 31-35
    0.811, 0.825, 0.837, 0.847, 0.857,  # ages 36-40
    0.865, 0.872, 0.879, 0.885, 0.889,  # ages 41-45
    0.893, 0.896, 0.897, 0.896, 0.896,  # ages 46-50
    0.894, 0.890, 0.885, 0.878, 0.869,  # ages 51-55
    0.852, 0.833, 0.812, 0.788, 0.757,  # ages 56-60
    0.715, 0.691, 0.667, 0.644           # ages 61-64
  ),
  awi = c(
    13773.10, 14531.34, 15239.24, 16135.07, 16822.51,
    17321.82, 18426.51, 19334.04, 20099.55, 21027.98,
    21811.60, 22935.42, 23132.67, 23753.53, 24705.66,
    25913.90, 27426.00, 28861.44, 30469.84, 32154.82,
    32921.92, 33252.09, 34064.95, 35648.55, 36952.94,
    38651.41, 40405.48, 41334.97, 40711.61, 41673.83,
    42979.61, 44321.67, 44888.16, 46481.52, 48098.63,
    48642.15, 50321.89, 52145.80, 54099.99, 55628.60,
    60575.07, 63795.13, 66621.80, 69472.44
  ),
  prelim_earnings = c(
    3127.58, 4065.56, 5447.48, 6889.79, 8097.01,
    9159.92, 10558.32, 11858.11, 13038.86, 14309.58,
    15471.56, 16857.69, 17518.53, 18474.93, 19655.90,
    21016.38, 22613.48, 24146.89, 25806.10, 27544.69,
    28476.67, 28989.64, 29932.39, 31532.07, 32847.32,
    34515.55, 36186.30, 37059.65, 36486.68, 37351.50,
    38444.14, 39442.83, 39704.03, 40792.27, 41776.26,
    41451.14, 41912.55, 42331.13, 42630.31, 42104.34,
    43337.34, 44072.11, 44442.72, 44751.21
  )
)

# =====================================================================
# Compare Table 4 earnings to our factor × AWI
# =====================================================================
cat("=== Compare Table 4 earnings to our computation ===\n")
cat("Medium ratio from Table 5: 1.221\n\n")

# Our factors from CSV
our_factors <- sef2025$factor[sef2025$worker == "medium"]
our_ages <- sef2025$age[sef2025$worker == "medium"]

cat(sprintf("%-4s %-8s %-8s %-8s %-12s %-12s %-12s %-8s\n",
            "Age", "Our_Fac", "Tbl6_Fac", "Prelim_F", "Our_Earn", "Tbl4_Earn", "Med_Earn", "Diff"))

total_diff <- 0
for (i in 1:nrow(table4)) {
  age <- table4$age[i]

  # Our factor (from CSV, 3 decimal places)
  our_fac <- our_factors[our_ages == age]

  # Table 6 factor (3 decimal places) — should match our CSV
  tbl6_fac_computed <- round(table4$prelim_factor[i] * 1.221, 3)

  # Exact medium earnings = prelim_earnings × 1.221
  med_earn_exact <- table4$prelim_earnings[i] * 1.221

  # Our earnings = our_factor × AWI
  our_earn <- our_fac * table4$awi[i]

  diff <- our_earn - med_earn_exact
  total_diff <- total_diff + diff

  if (abs(diff) > 0.01) {
    cat(sprintf("%-4d %-8.3f %-8.3f %-8.3f %-12.2f %-12.2f %-12.2f %-8.2f ***\n",
                age, our_fac, tbl6_fac_computed, table4$prelim_factor[i],
                our_earn, table4$prelim_earnings[i], med_earn_exact, diff))
  } else {
    cat(sprintf("%-4d %-8.3f %-8.3f %-8.3f %-12.2f %-12.2f %-12.2f %-8.2f\n",
                age, our_fac, tbl6_fac_computed, table4$prelim_factor[i],
                our_earn, table4$prelim_earnings[i], med_earn_exact, diff))
  }
}
cat(sprintf("\nTotal nominal earnings difference: $%.2f\n", total_diff))

# =====================================================================
# Now compute AIME using EXACT Table 4 earnings × 1.221
# vs our factor × AWI approach
# =====================================================================
cat("\n=== AIME comparison ===\n")

# Get indexing factors for born 1960 (index to age 60 year = 2020)
w <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                        age_claim = 65, factors = sef2025, assumptions = tr2025,
                        debugg = TRUE)

# Our earnings and indexed earnings
our_earn_all <- w$earnings[w$age >= 21 & w$age <= 64]
our_idx_fac <- w$index_factor[w$age >= 21 & w$age <= 64]
our_taxmax <- tr2025$taxmax[match(w$year[w$age >= 21 & w$age <= 64], tr2025$year)]

# SSA exact earnings (prelim × 1.221)
ssa_earn_all <- table4$prelim_earnings * 1.221

# Cap at taxmax
our_capped <- pmin(our_earn_all, our_taxmax)
ssa_capped <- pmin(ssa_earn_all, our_taxmax)

# Index
our_indexed <- our_capped * our_idx_fac
ssa_indexed <- ssa_capped * our_idx_fac

# Top 35
our_top35 <- sort(our_indexed, decreasing = TRUE)[1:35]
ssa_top35 <- sort(ssa_indexed, decreasing = TRUE)[1:35]

our_aime <- floor(sum(our_top35) / 420)
ssa_aime <- floor(sum(ssa_top35) / 420)

cat(sprintf("Our AIME:  %d (top-35 sum: %.2f)\n", our_aime, sum(our_top35)))
cat(sprintf("SSA AIME:  %d (top-35 sum: %.2f)\n", ssa_aime, sum(ssa_top35)))
cat(sprintf("Diff: %d\n", ssa_aime - our_aime))

# =====================================================================
# Even more precise: what if SSA doesn't round the prelim factor to 3dp?
# Table 4 text says "0.279779 (rounded to 0.280 in Table 4)"
# So the actual prelim factor has more precision.
# Back out exact prelim factors from Table 4 earnings:
# exact_prelim_factor = prelim_earnings / AWI
# =====================================================================
cat("\n=== Back out exact preliminary factors from Table 4 earnings ===\n")
exact_prelim <- table4$prelim_earnings / table4$awi
exact_medium <- exact_prelim * 1.221

cat(sprintf("%-4s %-12s %-12s %-10s %-12s %-12s\n",
            "Age", "Exact_Prelim", "Tbl4_Prelim", "Exact_Med", "Our_Med", "Diff"))
for (i in 1:nrow(table4)) {
  age <- table4$age[i]
  our_fac <- our_factors[our_ages == age]
  cat(sprintf("%-4d %-12.6f %-12.3f %-10.6f %-12.3f %-12.6f\n",
              age, exact_prelim[i], table4$prelim_factor[i],
              exact_medium[i], our_fac, exact_medium[i] - our_fac))
}

# =====================================================================
# AIME with exact prelim factors × 1.221 × AWI
# =====================================================================
cat("\n=== AIME with back-solved exact factors ===\n")
# Earnings = exact_prelim × 1.221 × AWI = prelim_earnings × 1.221
# (same as above since exact_prelim = prelim_earnings / AWI)
# But what if SSA rounds prelim_earnings to nearest cent first?

# Method A: prelim_earn × 1.221
earn_a <- table4$prelim_earnings * 1.221
# Method B: round(prelim_earn × 1.221, 2) — round medium earnings to cent
earn_b <- round(table4$prelim_earnings * 1.221, 2)
# Method C: round(prelim_earn × 1.221, 0) — round medium earnings to dollar
earn_c <- round(table4$prelim_earnings * 1.221, 0)

cap_a <- pmin(earn_a, our_taxmax)
cap_b <- pmin(earn_b, our_taxmax)
cap_c <- pmin(earn_c, our_taxmax)

idx_a <- cap_a * our_idx_fac
idx_b <- cap_b * our_idx_fac
idx_c <- cap_c * our_idx_fac

aime_a <- floor(sum(sort(idx_a, decreasing=TRUE)[1:35]) / 420)
aime_b <- floor(sum(sort(idx_b, decreasing=TRUE)[1:35]) / 420)
aime_c <- floor(sum(sort(idx_c, decreasing=TRUE)[1:35]) / 420)

cat(sprintf("Method A (raw multiply):    AIME = %d\n", aime_a))
cat(sprintf("Method B (round to cent):   AIME = %d\n", aime_b))
cat(sprintf("Method C (round to dollar): AIME = %d\n", aime_c))
cat(sprintf("Our current:                AIME = %d\n", our_aime))

# =====================================================================
# What about the exact factor precision?
# The text says "0.279779 (rounded to 0.280)"
# If exact prelim factor at age 22 = 0.279779:
# exact medium = 0.279779 × 1.221 = 0.341610
# Our CSV says 0.341
# Diff = 0.000610
# At AWI = 14531.34: diff = 0.000610 × 14531.34 = $8.87
# That's per year! Over 35 years that could add up.
# =====================================================================
cat("\n=== Factor precision analysis ===\n")
cat("Text says exact prelim at age 22 = 0.279779\n")
cat(sprintf("0.279779 × 1.221 = %.6f (Table 6 rounds to 0.341)\n", 0.279779 * 1.221))
cat(sprintf("Diff from 0.341: %.6f\n", 0.279779 * 1.221 - 0.341))
cat(sprintf("At AWI=14531.34: $%.2f per year\n\n", (0.279779 * 1.221 - 0.341) * 14531.34))

# Back out exact prelim factors from the exact earnings
# prelim_earn = exact_prelim_factor × AWI
# Since table 4 earnings are shown to 2 decimal places,
# the exact prelim factor is: earnings / AWI
# But Table 4 earnings may themselves be rounded to cent...
# The text confirms: "multiply ... 0.279779 ... by the 1982 AWI of $14,531.34
# to obtain annual earnings of $4,065.56"
# Check: 0.279779 × 14531.34 = 4065.525... ≈ $4,065.53 (NOT $4,065.56!)
cat("Verification of text claim:\n")
cat(sprintf("0.279779 × 14531.34 = %.4f (text says 4065.56)\n", 0.279779 * 14531.34))
cat("Hmm, doesn't match. So 0.279779 is itself rounded.\n")
cat(sprintf("Exact factor: 4065.56 / 14531.34 = %.8f\n", 4065.56 / 14531.34))
cat(sprintf("0.279779 is close but not exact.\n\n"))

# The TRUE exact factors are: Table4_earnings / AWI
# And TRUE exact medium factors are: (Table4_earnings × 1.221) / AWI
# = Table4_earnings / AWI × 1.221
# Let's compute AIME using these exact medium earnings

cat("=== Final: AIME using exact Table 4 medium earnings ===\n")
# SSA's actual medium earnings = prelim_earnings × 1.221
# This is equivalent to exact_medium_factor × AWI where
# exact_medium_factor = (prelim_earnings / AWI) × 1.221
ssa_medium_earn <- table4$prelim_earnings * 1.221
ssa_capped2 <- pmin(ssa_medium_earn, our_taxmax)
ssa_indexed2 <- ssa_capped2 * our_idx_fac
ssa_top35_2 <- sort(ssa_indexed2, decreasing=TRUE)[1:35]
ssa_aime2 <- floor(sum(ssa_top35_2) / 420)

cat(sprintf("SSA medium earnings AIME: %d\n", ssa_aime2))
cat(sprintf("Our AIME: %d\n", our_aime))
cat(sprintf("Diff: %d\n\n", ssa_aime2 - our_aime))

# Show which ages have the biggest earnings differences
cat("=== Earnings differences by age (SSA exact vs our 3dp) ===\n")
cat(sprintf("%-4s %-14s %-14s %-10s\n", "Age", "SSA_earn", "Our_earn", "Diff"))
diffs <- ssa_medium_earn - our_earn_all
for (i in order(abs(diffs), decreasing=TRUE)[1:15]) {
  age <- table4$age[i]
  cat(sprintf("%-4d $%-13.2f $%-13.2f $%-9.2f\n",
              age, ssa_medium_earn[i], our_earn_all[i], diffs[i]))
}
cat(sprintf("\nSum of all earnings diffs: $%.2f\n", sum(diffs)))
