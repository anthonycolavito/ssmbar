# Trace the remaining 0.17% V.C7 difference
# Try to get it to zero by finding the exact source
library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# Focus on medium earner born 1960 (claim 65) — simplest case since
# CPI deflation factor = 1.0 (turning 65 in 2025, the reference year)
w <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 65,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Our result
our_monthly <- w$ben[w$age == 65]
our_annual <- our_monthly * 12
cat(sprintf("Our monthly benefit at 65: $%.2f\n", our_monthly))
cat(sprintf("Our annual benefit at 65: $%.0f\n", our_annual))
cat(sprintf("V.C7 annual at 65: $25172\n"))
cat(sprintf("Gap: $%.0f (%.3f%%)\n\n", our_annual - 25172, (our_annual - 25172) / 25172 * 100))

# =====================================================================
# HYPOTHESIS 1: SSA rounds earnings to nearest dollar
# =====================================================================
cat("=== Hypothesis 1: Round earnings to nearest dollar ===\n")
# Recompute with rounded earnings
w_rd <- w
work <- w_rd[w_rd$age >= 21 & w_rd$age <= 64, ]
for (age in 21:64) {
  idx <- which(w_rd$age == age)
  w_rd$earnings[idx] <- round(w_rd$earnings[idx], 0)
}
# Can't easily rerun pipeline with modified earnings, so let's compute
# the AIME difference from rounding
earn_unrounded <- w$earnings[w$age >= 21 & w$age <= 64]
earn_rounded <- round(earn_unrounded, 0)
cat(sprintf("Total earnings diff from rounding: $%.2f\n", sum(earn_rounded) - sum(earn_unrounded)))

# Compute indexed earnings with rounded
idx_fac <- w$index_factor[w$age >= 21 & w$age <= 64]
taxmax_vals <- tr2025$taxmax[match(w$year[w$age >= 21 & w$age <= 64], tr2025$year)]
capped_rd <- pmin(earn_rounded, taxmax_vals)
indexed_rd <- capped_rd * idx_fac
top35_rd <- sum(sort(indexed_rd, decreasing = TRUE)[1:35])
aime_rd <- floor(top35_rd / 420)

capped_orig <- pmin(earn_unrounded, taxmax_vals)
indexed_orig <- capped_orig * idx_fac
top35_orig <- sum(sort(indexed_orig, decreasing = TRUE)[1:35])
aime_orig <- floor(top35_orig / 420)

cat(sprintf("AIME without rounding: %d\n", aime_orig))
cat(sprintf("AIME with rounding to $1: %d\n", aime_rd))
cat(sprintf("Difference: %d\n\n", aime_rd - aime_orig))

# =====================================================================
# HYPOTHESIS 2: V.C7 uses different scaled earnings factors
# =====================================================================
cat("=== Hypothesis 2: Check factor precision ===\n")
# Compare our factors against what Actuarial Note 2025.3 Table 6 shows
# Show factors for key ages
cat(sprintf("%-4s %-12s %-14s %-14s\n", "Age", "Factor", "AWI", "Earnings"))
for (age in c(21, 25, 30, 35, 40, 45, 50, 55, 60, 64)) {
  fac <- sef2025$factor[sef2025$worker == "medium" & sef2025$age == age]
  awi <- tr2025$awi[tr2025$year == 1960 + age]
  earn <- fac * awi
  cat(sprintf("%-4d %-12.6f %-14.2f %-14.2f\n", age, fac, awi, earn))
}

# =====================================================================
# HYPOTHESIS 3: V.C7 shows benefit at FIRST month, not full year
# =====================================================================
cat("\n=== Hypothesis 3: V.C7 monthly amount interpretation ===\n")
# Maybe V.C7 rounds differently. Let's check all possible monthly amounts
# that could produce "25172" when multiplied by 12
# 25172 / 12 = 2097.6667
# If SSA rounds to nearest dollar at the annual level:
# monthly must be between 25171.5/12 = 2097.625 and 25172.5/12 = 2097.708
# Our monthly = 2093.00, so we're $4.67/month short
cat(sprintf("Our monthly: $%.2f\n", our_monthly))
cat(sprintf("V.C7-implied monthly: $%.4f\n", 25172/12))
cat(sprintf("Monthly gap: $%.2f\n\n", 25172/12 - our_monthly))

# =====================================================================
# HYPOTHESIS 4: Trace AIME difference precisely
# =====================================================================
cat("=== Hypothesis 4: What AIME does V.C7 imply? ===\n")
# V.C7 monthly = 2097.67 (approx)
# Actuarial reduction for 24 months: each month = 5/9 of 1%
# But SSA truncates the reduced benefit to the dime
# monthly_ben = floor(cola_pia * arf * 10) / 10
# So monthly_ben = 2097.X where X is the dime

# We need: monthly * 12 rounds to 25172
# 2097.7 * 12 = 25172.4 -> rounds to 25172 ✓
# So monthly = 2097.7
# floor(cola_pia * arf * 10) / 10 = 2097.7
# cola_pia * arf >= 2097.7 and < 2097.8
# cola_pia >= 2097.7 / 0.866667 = 2420.42
# cola_pia < 2097.8 / 0.866667 = 2420.54

arf <- 1 - 24 * (5/900)
cat(sprintf("V.C7 implied monthly: 2097.7\n"))
cat(sprintf("V.C7 implied COLA'd PIA: [2420.4, 2420.5]\n"))
cat(sprintf("Our COLA'd PIA at 65: %.2f\n", w$cola_basic_pia[w$age == 65]))
cat(sprintf("Our monthly: floor(%.2f * %.6f * 10)/10 = %.1f\n",
            w$cola_basic_pia[w$age == 65], arf,
            floor(w$cola_basic_pia[w$age == 65] * arf * 10) / 10))

# COLA'd PIA gap
cola_pia_gap <- 2420.5 - w$cola_basic_pia[w$age == 65]
cat(sprintf("COLA'd PIA gap: $%.2f\n\n", cola_pia_gap))

# Work backwards through COLAs to find implied basic PIA
cat("=== Working backwards through COLAs ===\n")
# Our COLA progression at age 65 with recomputation:
# basic_pia at 65 = 2100.80 (AIME=4709)
# COLAs applied: 2022->2023 (8.7%), 2023->2024 (3.2%), 2024->2025 (2.5%)
# Wait, check actual COLA timing
cola_2022 <- tr2025$cola[tr2025$year == 2022]  # applied at age 63
cola_2023 <- tr2025$cola[tr2025$year == 2023]  # applied at age 64
cola_2024 <- tr2025$cola[tr2025$year == 2024]  # applied at age 65
cat(sprintf("COLAs: 2022=%.1f%%, 2023=%.1f%%, 2024=%.1f%%\n", cola_2022, cola_2023, cola_2024))

# Forward from basic_pia
bp <- w$basic_pia[w$age == 65]  # 2100.80
p1 <- floor(bp * (1 + cola_2022/100) * 10) / 10
p2 <- floor(p1 * (1 + cola_2023/100) * 10) / 10
p3 <- floor(p2 * (1 + cola_2024/100) * 10) / 10
cat(sprintf("basic_pia=%.1f -> after 8.7%%: %.1f -> after 3.2%%: %.1f -> after 2.5%%: %.1f\n",
            bp, p1, p2, p3))
cat(sprintf("Our stored cola_basic_pia at 65: %.2f\n", w$cola_basic_pia[w$age == 65]))

# What basic PIA at age 65 would produce V.C7's result?
# Need cola_pia = ~2420.5
# Search
cat("\n=== Searching for target basic PIA ===\n")
target_monthly <- 2097.7
for (test_bp_10 in seq(21000, 21100, by = 1)) {
  test_bp <- test_bp_10 / 10
  p1 <- floor(test_bp * (1 + cola_2022/100) * 10) / 10
  p2 <- floor(p1 * (1 + cola_2023/100) * 10) / 10
  p3 <- floor(p2 * (1 + cola_2024/100) * 10) / 10
  monthly <- floor(p3 * arf * 10) / 10
  annual <- monthly * 12
  if (abs(annual - 25172) < 12) {
    cat(sprintf("  basic_pia=%.1f -> cola_pia=%.1f -> monthly=%.1f -> annual=%.0f %s\n",
                test_bp, p3, monthly, annual, ifelse(round(annual) == 25172, "<-- MATCH", "")))
  }
}

# What AIME produces each basic PIA?
bp1 <- tr2025$bp1[tr2025$year == 2022]  # 1024
bp2 <- tr2025$bp2[tr2025$year == 2022]  # 6172
cat(sprintf("\nBend points (2022): bp1=%d, bp2=%d\n", bp1, bp2))
cat(sprintf("Our AIME at 65: %d -> basic_pia = 0.9*%d + 0.32*(%d-%d) = %.2f -> floor_dime = %.1f\n",
            w$aime[w$age == 65], bp1, w$aime[w$age == 65], bp1,
            0.9*bp1 + 0.32*(w$aime[w$age == 65] - bp1),
            floor((0.9*bp1 + 0.32*(w$aime[w$age == 65] - bp1)) * 10) / 10))

# For the target basic_pia of ~2105 (from search above)
for (target_bp in c(2104.8, 2105.0, 2105.2, 2105.4, 2105.6)) {
  implied_aime <- (target_bp - 0.9*bp1) / 0.32 + bp1
  cat(sprintf("  target basic_pia=%.1f -> implied AIME=%.0f (ours=%d, diff=%d)\n",
              target_bp, implied_aime, w$aime[w$age == 65],
              round(implied_aime) - w$aime[w$age == 65]))
}

# =====================================================================
# HYPOTHESIS 5: Check V.C7 claim age / NRA column interpretation
# =====================================================================
cat("\n=== Hypothesis 5: Does V.C7 actually show benefit at age 65 with NRA reduction? ===\n")
cat("V.C7 column headers describe 'reduced benefit at age 65'\n")
cat(sprintf("Our actuarial reduction: 24 months * 5/900 = %.6f\n", 24 * 5/900))
cat(sprintf("Factor: 1 - %.6f = %.6f\n", 24 * 5/900, 1 - 24*5/900))

# What if V.C7 uses a slightly different reduction?
# SSA computes reduction month by month:
# First 36 months: 5/9 of 1% per month
# We have 24 months early (67-65=2 years)
# reduction = 24 * 5/9 * 1/100 = 0.13333
# factor = 0.86667
cat(sprintf("24-month reduction = 24 * 5/900 = %.10f\n", 24*5/900))
cat(sprintf("Factor = %.10f\n", 1 - 24*5/900))

# Try: what if SSA applies reduction as floor(PIA * months_early * 5/9 / 100 * 10)/10
# i.e., the reduction AMOUNT is truncated, not the result
cola_pia <- w$cola_basic_pia[w$age == 65]
reduction_pct <- 24 * 5 / 900
reduction_amount <- cola_pia * reduction_pct
cat(sprintf("\nOur COLA'd PIA: %.2f\n", cola_pia))
cat(sprintf("Reduction amount: %.2f * %.6f = %.4f\n", cola_pia, reduction_pct, reduction_amount))
cat(sprintf("Method A (our): floor(PIA * factor * 10)/10 = floor(%.4f * 10)/10 = %.1f\n",
            cola_pia * (1 - reduction_pct),
            floor(cola_pia * (1 - reduction_pct) * 10) / 10))
cat(sprintf("Method B: PIA - floor(reduction * 10)/10 = %.2f - %.1f = %.1f\n",
            cola_pia, floor(reduction_amount * 10) / 10,
            cola_pia - floor(reduction_amount * 10) / 10))

# =====================================================================
# HYPOTHESIS 6: NRA for 1960 cohort — is it exactly 67:0?
# =====================================================================
cat("\n=== Hypothesis 6: NRA precision ===\n")
nra <- w$nra[w$age == 65]
cat(sprintf("Our NRA: %.4f (should be exactly 67)\n", nra))
cat(sprintf("Months early: %d\n", round((nra - 65) * 12)))

# =====================================================================
# HYPOTHESIS 7: V.C7 uses NRA benefit, not age-65 benefit
# =====================================================================
cat("\n=== Hypothesis 7: Check V.C7's full PIA column (benefit at NRA) ===\n")
# V.C7 has both "full PIA at NRA" and "reduced at 65"
# Let's compute our benefit at NRA (67)
w67 <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 67,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)
cat(sprintf("Our monthly at 67 (NRA): $%.2f\n", w67$ben[w67$age == 67]))
cat(sprintf("Our annual at 67: $%.0f\n", w67$ben[w67$age == 67] * 12))

# =====================================================================
# HYPOTHESIS 8: Does V.C7 round the annual to nearest $12?
# =====================================================================
cat("\n=== Hypothesis 8: V.C7 annual rounding ===\n")
cat("If V.C7 shows 12 * floor(monthly_benefit), not round(12 * monthly):\n")
cat(sprintf("  Our monthly: $%.2f -> 12 * floor(%.2f) = $%d\n",
            our_monthly, our_monthly, 12 * floor(our_monthly)))
cat(sprintf("  vs 12 * monthly: $%.0f\n", 12 * our_monthly))

# What if V.C7 shows round(monthly * 12) not floor?
# Our: 2093.00 * 12 = 25116
# V.C7: 25172
# gap is 56

# =====================================================================
# HYPOTHESIS 9: Compare earnings directly to Actuarial Note Table 4
# =====================================================================
cat("\n=== Hypothesis 9: Compare total indexed earnings ===\n")
cat(sprintf("Our top-35 indexed earnings sum: $%.2f\n", top35_orig))
cat(sprintf("Our AIME (at 65): %d\n", w$aime[w$age == 65]))

# Target AIME that would produce V.C7's benefit
# From the search above, need basic_pia ~2105.4 -> AIME ~4724
target_aime <- 4724
target_top35 <- target_aime * 420 + 419  # upper bound
cat(sprintf("Target AIME for V.C7 match: ~%d\n", target_aime))
cat(sprintf("Target top-35 sum: ~$%.0f\n", target_aime * 420.0))
cat(sprintf("Our top-35 sum: $%.0f\n", top35_orig))
cat(sprintf("Indexed earnings gap: $%.0f\n", target_aime * 420.0 - top35_orig))
cat(sprintf("Per year (over 35): $%.0f\n", (target_aime * 420.0 - top35_orig) / 35))
