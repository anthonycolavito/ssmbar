# The remaining AIME gap: Table 5 ratio precision
#
# Table 5 says medium ratio = 69472 / 56921 = 1.221 (3dp)
# But the EXACT ratio is 69472.44 / 56921.00 = 1.22056...
# Wait, let me check what the exact values are.
#
# Table 5:
#   Selected career-avg earnings = $69,472 (this is 100% of AWI 2024)
#   Prelim scaled career-avg = $56,921
#   Ratio = 69472 / 56921 = 1.221 (to 3dp)
#
# But AWI 2024 = $69,472.44 (from Table 4)
# So does the ratio use $69,472 or $69,472.44?

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

cat("=== Table 5 ratio precision ===\n")
awi_2024 <- 69472.44
prelim_avg <- 56921.00

# Ratio as shown in Table 5
ratio_3dp <- 1.221
# Exact ratio with round AWI
ratio_exact_round <- 69472 / 56921
# Exact ratio with precise AWI
ratio_exact_precise <- awi_2024 / prelim_avg
cat(sprintf("Ratio (3dp from table):  1.221\n"))
cat(sprintf("Ratio (69472/56921):     %.6f\n", ratio_exact_round))
cat(sprintf("Ratio (69472.44/56921):  %.6f\n", ratio_exact_precise))

# How much does this matter?
# With 1.221 vs 1.220562:
# At $44,751.21 (max earnings at age 64):
# 44751.21 × 1.221 = 54,641.23
# 44751.21 × 1.220562 = 54,621.61
# Diff = $19.62
cat(sprintf("\nExample age 64 earnings:\n"))
cat(sprintf("  With 1.221:     $%.2f\n", 44751.21 * 1.221))
cat(sprintf("  With 1.220562:  $%.2f\n", 44751.21 * ratio_exact_round))
cat(sprintf("  Diff: $%.2f\n", 44751.21 * 1.221 - 44751.21 * ratio_exact_round))

# Actually wait. Let me re-read the actuarial note more carefully.
# Table 5 says ratio = (1) / (2) = $69,472 / $56,921 = 1.221
# But this is ROUNDED to 3 dp for display.
# The actual computation might use the EXACT ratio: 69472.44 / 56921 = 1.22058...
# Or maybe 69472 / 56921 = 1.22009...

# Let me check: does SSA multiply prelim earnings by the exact ratio,
# or by the rounded 1.221?

# If exact ratio (69472/56921 = 1.220093...):
# medium earnings at each age = prelim_earnings × 1.220093
# If rounded ratio (1.221):
# medium earnings at each age = prelim_earnings × 1.221

# The career-average of the medium earner should equal exactly $69,472 (or $69,472.44)
# Let's check which ratio produces that.

# Table 4's career-average = $56,921
# If we multiply by 1.221: 56921 × 1.221 = $69,502.54 (too high!)
# If we multiply by exact ratio: 56921 × (69472/56921) = $69,472.00 (exact!)
# If we multiply by 69472.44/56921: 56921 × (69472.44/56921) = $69,472.44 (exact!)

cat("\n=== Career-average earnings check ===\n")
cat(sprintf("$56,921 × 1.221 = $%.2f (should be $69,472)\n", 56921 * 1.221))
cat(sprintf("$56,921 × (69472/56921) = $%.2f\n", 56921 * (69472/56921)))
cat(sprintf("$56,921 × (69472.44/56921) = $%.2f\n", 56921 * (69472.44/56921)))

# AH HA! Using 1.221 gives $69,502.54, NOT $69,472!
# That's $30.54 too high!
# So SSA does NOT use 1.221 — they use the EXACT ratio.
# But which denominator: $69,472 or $69,472.44?

# The note says: "Selected career-average earnings levels for hypothetical
# scaled workers: $69,472 (for medium)"
# And "the AWI for 2024 is $69,472.44"
# So the selected level is $69,472 (round to dollar), not $69,472.44

# Therefore the exact ratio is 69472 / 56921 = 1.220093...
exact_ratio <- 69472 / 56921
cat(sprintf("\nExact ratio: 69472 / 56921 = %.10f\n", exact_ratio))

# Now compute AIME with this exact ratio
table4_earnings <- c(
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

years <- 1981:2024
ages <- 21:64

# Get indexing factors and taxmax from our assumptions
w <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                        age_claim = 65, factors = sef2025, assumptions = tr2025,
                        debugg = TRUE)
idx_fac <- w$index_factor[w$age >= 21 & w$age <= 64]
taxmax_vals <- tr2025$taxmax[match(years, tr2025$year)]

# Method 1: prelim × 1.221 (rounded ratio)
earn_rounded_ratio <- table4_earnings * 1.221
# Method 2: prelim × exact ratio (69472/56921)
earn_exact_ratio <- table4_earnings * exact_ratio
# Method 3: prelim × (69472.44/56921)
earn_awi_ratio <- table4_earnings * (69472.44 / 56921)

compute_aime <- function(earnings) {
  capped <- pmin(earnings, taxmax_vals)
  indexed <- capped * idx_fac
  top35 <- sort(indexed, decreasing = TRUE)[1:35]
  list(aime = floor(sum(top35) / 420), top35_sum = sum(top35))
}

r1 <- compute_aime(earn_rounded_ratio)
r2 <- compute_aime(earn_exact_ratio)
r3 <- compute_aime(earn_awi_ratio)

# Also our current method
our_factors <- sef2025$factor[sef2025$worker == "medium"]
our_earn <- our_factors * tr2025$awi[match(years, tr2025$year)]
r_our <- compute_aime(our_earn)

cat("\n=== AIME with different ratio approaches ===\n")
cat(sprintf("Method 1 (prelim × 1.221):           AIME = %d (top35 = %.2f)\n", r1$aime, r1$top35_sum))
cat(sprintf("Method 2 (prelim × 69472/56921):     AIME = %d (top35 = %.2f)\n", r2$aime, r2$top35_sum))
cat(sprintf("Method 3 (prelim × 69472.44/56921):  AIME = %d (top35 = %.2f)\n", r3$aime, r3$top35_sum))
cat(sprintf("Our current (3dp factors × AWI):     AIME = %d (top35 = %.2f)\n", r_our$aime, r_our$top35_sum))

# Check career-average for each
compute_career_avg <- function(earnings) {
  indexed_2024 <- earnings * (69472.44 / tr2025$awi[match(years, tr2025$year)])
  top35 <- sort(indexed_2024, decreasing = TRUE)[1:35]
  mean(top35)
}

cat(sprintf("\nCareer-avg earnings:\n"))
cat(sprintf("  Method 1 (×1.221):         $%.2f\n", compute_career_avg(earn_rounded_ratio)))
cat(sprintf("  Method 2 (×69472/56921):   $%.2f\n", compute_career_avg(earn_exact_ratio)))
cat(sprintf("  Method 3 (×69472.44/56921):$%.2f\n", compute_career_avg(earn_awi_ratio)))
cat(sprintf("  Our current:               $%.2f\n", compute_career_avg(our_earn)))
cat(sprintf("  Target: $69,472.00 or $69,472.44\n"))

# Hmm. The career-avg earnings are computed with wage indexing to 2024.
# This is different from AIME indexing (which indexes to age 60 = 2020).
# The career-avg is just for calibrating the ratio. The actual AIME
# computation uses age-60 indexing.

# =====================================================================
# KEY QUESTION: What if SSA uses the EXACT ratio in their computation
# but we only have 3dp factors?
# =====================================================================
# The exact medium factor at each age = prelim_factor × exact_ratio
# = (table4_earnings / AWI_year) × (69472 / 56921)
# The earnings = exact_medium_factor × AWI_year
# = table4_earnings × (69472 / 56921)
# = table4_earnings × 1.22009...

# Wait, but we already computed this as Method 2 above: AIME = 4709
# That's the SAME as our current AIME!

# Hmm, something is off. Let me look more carefully.
# Method 1 (×1.221) gives AIME 4710
# Method 2 (×69472/56921 = ×1.22009) gives AIME 4709
# Our current gives AIME 4709
# But V.C7 implies ~4723...

# So the ratio precision actually works AGAINST us — the more precise
# ratio is SMALLER than 1.221, giving LESS earnings and a LOWER AIME.

# The gap of ~14 in AIME is NOT from factor precision or ratio precision.
# We need to look elsewhere.

# =====================================================================
# Wait — what if V.C7 actually targets AWI×1.0 for the CAREER-AVERAGE
# not for the RATIO? The career-avg = $69,472. The AIME is different.
# V.C7 might compute AIME differently than we do.
# =====================================================================

cat("\n=== What AIME does Table 4 produce directly? ===\n")
# The AIME uses:
# 1. Earnings from the medium worker (= prelim × exact_ratio × AWI for each year)
#    But actually, SSA probably just uses the final scaled factors from Table 6
#    No — the note explicitly shows the factors are computed from Table 4 × ratio
#    But V.C7 is computed by SSA's internal systems, which might use more precision

# Let me check: if V.C7 uses the career-average of $69,472 as the target,
# and career-avg = avg of top 35 years of earnings indexed to 2024...
# then the AIME (indexed to age-60 = 2020) would be:
# AIME = floor(career_avg × 12 × AWI_2020 / AWI_2024)
# where AWI indexing means we index to 2020 instead of 2024
# Actually no, AIME uses a completely different computation period

# Let me just check the AIME directly from Table 4 with the exact ratio
cat("Table 4 career-avg: $56,921\n")
cat(sprintf("Medium ratio (exact): %.10f\n", exact_ratio))
cat(sprintf("Medium career-avg target: $69,472\n"))
cat(sprintf("Check: 56921 × ratio = $%.2f\n", 56921 * exact_ratio))

# The target IS $69,472 exactly. So:
# medium earnings at each age = prelim_earnings × (69472 / 56921)

# But 69472 / 56921 = 1.220093...
# The 1.221 in Table 5 is rounded.
# Yet using 1.220093 gives AIME = 4709 (same as ours)
# And using 1.221 gives AIME = 4710

# V.C7 implies ~4723. Neither gets close.
# The gap must be elsewhere — maybe in the COLA application or CPI deflation.

cat("\n=== Check CPI deflation: does V.C7 deflate differently? ===\n")
# V.C7 for born 1960, age 65 (year 2025):
# The CPI deflator for year 2025 to 2025 = 1.0
# So for this specific case, CPI deflation doesn't matter.
# V.C7 shows $25,172 annual at age 65 in 2025 dollars.
# Since 2025 IS the reference year, this IS the nominal benefit.

# Our computation at age 65 (year 2025):
ben65 <- w$ben[w$age == 65]
cat(sprintf("Our monthly benefit at 65: $%.2f\n", ben65))
cat(sprintf("Our annual: $%.0f\n", ben65 * 12))
cat(sprintf("V.C7: $25,172\n"))
cat(sprintf("Gap: $%.0f\n\n", ben65 * 12 - 25172))

# The gap is $-56. This is about 4.67/month.
# If AIME were 4723 instead of 4709:
# basic_pia = floor_dime(0.9 × 1024 + 0.32 × (4723 - 1024))
# = floor_dime(921.6 + 1183.68)
# = floor_dime(2105.28) = 2105.2
# vs our 2100.8 (from AIME 4709: 0.9×1024 + 0.32×3685 = 921.6 + 1179.2 = 2100.8)
# Diff in basic_pia = 4.4

# Then COLAs would amplify:
# 2105.2 × 1.087 × ... = about 2421 cola'd PIA
# vs our 2415.4
# Monthly reduced = 2421 × 0.8667 = 2097.9 → floor_dime = 2097.9
# Annual = 2097.9 × 12 = 25174.8 → close to 25172

# But where does AIME 4723 come from?
# Need 14 × 420 = $5,880 more in top-35 indexed earnings sum.
# That's $168/year on average.

cat("=== What if V.C7 uses EXACT preliminary earnings (not rounded to cent)? ===\n")
# Table 4 shows earnings to the cent (2dp). But the EXACT earnings are
# prelim_factor (full precision) × AWI (full precision).
# The table 4 earnings might be rounded for display.
# Let's see if this makes a difference.

# If prelim factor at age 22 is exactly 0.279779:
# 0.279779 × 14531.34 = 4065.5638...
# Table 4 shows 4065.56 (rounded to cent)
# Medium earnings = 4065.5638 × exact_ratio = 4065.5638 × 1.220093 = 4960.03
# vs table4_earn × exact_ratio = 4065.56 × 1.220093 = 4960.00
# Diff = $0.03 — negligible

# The Table 4 earnings are rounded to the cent, and the error from this
# rounding is tiny (fractions of a cent per year × 44 years).
# This cannot explain the ~$5,880 gap.

cat("Table 4 cent-rounding effect is negligible.\n\n")

# =====================================================================
# FINAL CHECK: What if V.C7 doesn't use the scaled workers at all
# for the earnings computation? What if V.C7 uses a different
# methodology?
# =====================================================================
cat("=== Key finding ===\n")
cat("The 0.17% V.C7 gap CANNOT be explained by:\n")
cat("  1. Factor rounding (3dp vs exact): changes AIME by only 1\n")
cat("  2. Ratio rounding (1.221 vs exact): changes AIME by 0-1\n")
cat("  3. Earnings rounding (cent vs exact): changes AIME by 0\n")
cat("  4. Table 4 display rounding: negligible\n\n")
cat("The gap is 14 in AIME (4709 vs ~4723). All precision effects\n")
cat("combined can explain at most 1-2 of that 14.\n\n")
cat("Possible remaining explanations:\n")
cat("  A. V.C7 uses earnings from a different source than Actuarial Note\n")
cat("  B. Our AWI values differ from SSA's (even slightly)\n")
cat("  C. Our indexing factors differ from SSA's\n")
cat("  D. V.C7 has a different computation methodology\n\n")

# Check AWI values
cat("=== AWI comparison: ours vs Table 4 ===\n")
our_awi <- tr2025$awi[match(years, tr2025$year)]
tbl4_awi <- c(
  13773.10, 14531.34, 15239.24, 16135.07, 16822.51,
  17321.82, 18426.51, 19334.04, 20099.55, 21027.98,
  21811.60, 22935.42, 23132.67, 23753.53, 24705.66,
  25913.90, 27426.00, 28861.44, 30469.84, 32154.82,
  32921.92, 33252.09, 34064.95, 35648.55, 36952.94,
  38651.41, 40405.48, 41334.97, 40711.61, 41673.83,
  42979.61, 44321.67, 44888.16, 46481.52, 48098.63,
  48642.15, 50321.89, 52145.80, 54099.99, 55628.60,
  60575.07, 63795.13, 66621.80, 69472.44
)

awi_match <- all(abs(our_awi - tbl4_awi) < 0.01)
cat(sprintf("All AWI values match Table 4 within 1 cent: %s\n", awi_match))
if (!awi_match) {
  for (i in 1:length(years)) {
    if (abs(our_awi[i] - tbl4_awi[i]) >= 0.01) {
      cat(sprintf("  Year %d: ours=%.2f, table4=%.2f, diff=%.2f\n",
                  years[i], our_awi[i], tbl4_awi[i], our_awi[i] - tbl4_awi[i]))
    }
  }
}

# Check indexing factors
cat("\n=== Indexing factor check ===\n")
# For born 1960, indexing year = 2020 (age 60)
# Index factor for year Y = AWI_2020 / AWI_Y for Y <= 2020
# Index factor = 1.0 for Y > 2020
awi_2020 <- tr2025$awi[tr2025$year == 2020]
cat(sprintf("AWI 2020: $%.2f\n", awi_2020))
for (yr in c(1981, 1990, 2000, 2010, 2019, 2020, 2021, 2022)) {
  expected_idx <- if (yr <= 2020) awi_2020 / tr2025$awi[tr2025$year == yr] else 1.0
  our_idx <- w$index_factor[w$year == yr & w$age >= 21 & w$age <= 64]
  if (length(our_idx) > 0) {
    cat(sprintf("  Year %d: expected=%.6f, ours=%.6f, match=%s\n",
                yr, expected_idx, our_idx[1], abs(expected_idx - our_idx[1]) < 0.000001))
  }
}
