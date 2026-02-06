# Reverse-engineer V.C7's earnings from the replacement rate column
# Col 4 shows "percent of 35-year average earnings" for the NRA benefit
# This lets us back out the exact 35-year average earnings SSA used

library(dplyr)
library(readxl)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

vc7_path <- "C:/Users/AnthonyColavito/instructions/VC7.xlsx"
raw_data <- read_excel(vc7_path, col_names = FALSE)

# Get medium earner section
med_idx <- which(grepl("Scaled medium", raw_data[[1]], ignore.case = TRUE))[1]

cat("=== V.C7 Medium Earner: Full Data ===\n")
cat(sprintf("%-6s %-6s %-8s %-8s %-6s %-8s %-8s\n",
            "Yr65", "NRA", "NRA$", "NRA%", "Age", "Age65$", "Age65%"))

for (by in 1960:1970) {
  yr65 <- by + 65
  for (i in (med_idx + 1):min(med_idx + 200, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val) && as.character(val) == as.character(yr65)) {
      nra_age <- as.character(raw_data[[2]][i])
      nra_ben <- as.numeric(raw_data[[3]][i])
      nra_pct <- as.numeric(raw_data[[4]][i])
      age65_age <- as.character(raw_data[[5]][i])
      age65_ben <- as.numeric(raw_data[[6]][i])
      age65_pct <- as.numeric(raw_data[[7]][i])

      cat(sprintf("%-6d %-6s %-8.0f %-8.1f %-6s %-8.0f %-8.1f\n",
                  yr65, nra_age, nra_ben, nra_pct, age65_age, age65_ben, age65_pct))
      break
    }
  }
}

# For year 2025 (born 1960):
# NRA benefit = $29,283, NRA% = 40.8%
# Age-65 benefit = $25,172, Age-65% = 36.2%
# These are annual amounts in 2025 CPI dollars

# From NRA%: 29283 / (nra_pct/100) = 35-year avg real earnings in 2025$
# 29283 / 0.408 = 71,772
# From Age-65%: 25172 / 0.362 = 69,533
# Hmm, different denominators. The "35-year average earnings" is probably
# computed in real terms but might be defined differently.

cat("\n=== Reverse-engineer 35-year average earnings ===\n")
for (by in 1960:1970) {
  yr65 <- by + 65
  for (i in (med_idx + 1):min(med_idx + 200, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val) && as.character(val) == as.character(yr65)) {
      nra_ben <- as.numeric(raw_data[[3]][i])
      nra_pct <- as.numeric(raw_data[[4]][i])
      age65_ben <- as.numeric(raw_data[[6]][i])
      age65_pct <- as.numeric(raw_data[[7]][i])

      avg_earn_nra <- nra_ben / (nra_pct / 100)
      avg_earn_65 <- age65_ben / (age65_pct / 100)

      # Our 35-year average earnings (in real 2025 CPI terms)
      w <- calculate_benefits(birth_yr = by, sex = "male", type = "medium",
                              age_claim = 65, factors = sef2025, assumptions = tr2025,
                              debugg = TRUE)

      # Get CPI deflator
      cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
      cpi_yr65 <- tr2025$cpi_w[tr2025$year == yr65]

      # Compute our real 35-year average earnings
      # The V.C7 replacement rate denominator uses CPI-W indexed earnings
      # "Percent of 35-year average earnings" — this means:
      #   benefit / avg_35yr_real_earnings
      # where avg_35yr_real_earnings is in 2025 CPI dollars

      # Get our nominal earnings, deflate to 2025 CPI
      our_earn <- w$earnings[w$age >= 21 & w$age <= 64]
      our_years <- w$year[w$age >= 21 & w$age <= 64]
      our_cpi <- tr2025$cpi_w[match(our_years, tr2025$year)]
      our_real_earn <- our_earn * (cpi_2025 / our_cpi)
      our_avg_35 <- sum(sort(our_real_earn, decreasing = TRUE)[1:35]) / 35

      cat(sprintf("BY=%d: V.C7 avg_earn(NRA)=$%.0f, V.C7 avg_earn(65)=$%.0f, Ours=$%.0f, diff=%.1f%%\n",
                  by, avg_earn_nra, avg_earn_65, our_avg_35,
                  (our_avg_35 - avg_earn_nra) / avg_earn_nra * 100))
      break
    }
  }
}

# =====================================================================
# KEY TEST: Can we match V.C7 exactly by computing the monthly benefit
# directly from V.C7's implied earnings?
# =====================================================================
cat("\n=== Direct Test: Use V.C7's NRA benefit to verify our pipeline ===\n")
# For born 1960:
# V.C7 NRA benefit = $29,283/year in 2025 CPI dollars
# Since 2025 is the base year, no deflation needed
# Monthly NRA benefit = 29283/12 = 2440.25
# At NRA, actuarial factor = 1.0
# So cola_basic_pia at age 67 = 2440 or 2441 (depending on rounding)

# Our NRA benefit:
w67 <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                          age_claim = 67, factors = sef2025, assumptions = tr2025,
                          debugg = TRUE)
our_nra_monthly <- w67$ben[w67$age == 67]
our_nra_annual <- our_nra_monthly * 12
cat(sprintf("Our NRA monthly: $%.2f, annual: $%.0f\n", our_nra_monthly, our_nra_annual))
cat(sprintf("V.C7 NRA annual: $29283\n"))
cat(sprintf("NRA gap: $%.0f (%.3f%%)\n", our_nra_annual - 29283, (our_nra_annual - 29283) / 29283 * 100))

# The NRA benefit has no actuarial reduction, so:
# annual = 12 * floor(cola_basic_pia at 67)
# Our cola_basic_pia at 67:
cat(sprintf("Our cola_basic_pia at 67: $%.2f\n", w67$cola_basic_pia[w67$age == 67]))
cat(sprintf("floor -> $%.0f/month -> $%.0f/year\n",
            floor(w67$cola_basic_pia[w67$age == 67]),
            floor(w67$cola_basic_pia[w67$age == 67]) * 12))

# V.C7 implied cola_basic_pia at 67:
# 29283/12 = 2440.25, so monthly = 2440 (if floor) -> annual = 29280 (not 29283!)
# Hmm, 29283/12 = 2440.25 which doesn't give a clean floor
# 2440 * 12 = 29280 (not 29283)
# 2441 * 12 = 29292 (not 29283)
# So V.C7 must round differently...
# Maybe V.C7 shows round(monthly * 12), not floor(monthly) * 12

# Or maybe the benefit is truncated to dime, not dollar:
# floor(cola_pia * 10)/10 = monthly with dime precision
# 29283 / 12 = 2440.25
# So monthly could be 2440.20 (12 * 2440.20 = 29282.40, rounds to 29282)
# or 2440.30 (12 * 2440.30 = 29283.60, rounds to 29284)
# Neither gives exactly 29283!

# Unless V.C7 rounds to nearest dollar at the annual level:
# monthly * 12, then round to nearest dollar
# 2440.25 * 12 = 29283.0 exactly!? Let me check...
# No, we need monthly to be truncated to dime first.
# If monthly = 2440.2 -> 29282.4 -> round = 29282
# If monthly = 2440.3 -> 29283.6 -> round = 29284
# But 2440.25 * 12 = 29283.0 — if they DON'T truncate to dime first...

cat("\n=== V.C7 rounding: monthly * 12 without floor? ===\n")
# What if V.C7 computes: round(cola_basic_pia * 12) at NRA?
# i.e., they don't truncate to dime, they just multiply by 12 and round
# cola_pia * 12 = 29283 -> cola_pia = 2440.25
cat("If V.C7 does round(cola_pia * 12):\n")
cat(sprintf("  Implied cola_pia at 67: $%.4f\n", 29283/12))
cat(sprintf("  Our cola_pia at 67: $%.2f\n", w67$cola_basic_pia[w67$age == 67]))
cat(sprintf("  Gap: $%.2f\n", 29283/12 - w67$cola_basic_pia[w67$age == 67]))

# Same test for age 65:
# V.C7 = 25172
# If round(cola_pia * arf * 12):
# cola_pia * 0.866667 * 12 = 25172
# cola_pia = 25172 / (0.866667 * 12) = 25172 / 10.4 = 2420.38
cat(sprintf("\nIf V.C7 does round(cola_pia * arf * 12):\n"))
cat(sprintf("  Implied cola_pia at 65: $%.4f\n", 25172 / (1 - 24*5/900) / 12))
cat(sprintf("  But that's the same as round(monthly * 12) where monthly = cola_pia * arf\n"))

# Actually, let me think about this differently.
# What if V.C7 computes annual = round(monthly_benefit * 12)?
# And monthly_benefit = floor_dime(cola_pia * arf) for age 65
#                     = cola_pia for NRA (no reduction)
# At NRA: floor_dime(cola_pia) * 12
# floor_dime(x) * 12 can give non-multiple-of-12 only if the dime part
# contributes fractional cents... which it can:
# e.g., floor_dime(2440.28) = 2440.2, and 2440.2 * 12 = 29282.4

# So for NRA annual = 29283:
# We need floor_dime(cola_pia) * 12 to round to 29283
# floor_dime(cola_pia) must be 2440.25 -> but that's not a valid dime!
# Actually 2440.2 * 12 = 29282.4 -> round = 29282
# 2440.3 * 12 = 29283.6 -> round = 29284
# Neither works!

# So V.C7 does NOT truncate monthly to dime before multiplying by 12.
# V.C7 likely does: round(cola_pia * actuarial_factor * 12)
# i.e., annual = round(cola_pia * arf * 12)

# For NRA (arf=1): annual = round(cola_pia * 12) = 29283
# cola_pia * 12 must be in [29282.5, 29283.5)
# cola_pia must be in [2440.2083, 2440.2917)

# For age 65 (arf=0.866667): annual = round(cola_pia * 0.866667 * 12) = 25172
# cola_pia * 10.4 must be in [25171.5, 25172.5)
# cola_pia must be in [2420.337, 2420.433)

# Are these consistent? If cola_pia = 2440.25:
# NRA: round(2440.25 * 12) = round(29283.0) = 29283 ✓
# Age 65: round(2440.25 * 0.866667 * 12) = round(25375.06) = 25375 ✗ (not 25172)
# Wait, that can't be right. The cola_pia at 67 and 65 are different because
# of 2 more years of COLAs at 67.

# Let me be more careful. For claiming at 65, cola_pia is at year 2025 (age 65).
# For claiming at 67, cola_pia is at year 2027 (age 67) — 2 more COLAs.
# And V.C7 deflates the age-67 benefit back to 2025 dollars.

# So the test should be:
# Age 65 (year 2025): V.C7 = round(cola_pia_65 * arf * 12) = 25172
# Age 67 (year 2027): V.C7 = round(cola_pia_67 * 1.0 * 12 * cpi_2025/cpi_2027) = 29283

cpi_2027 <- tr2025$cpi_w[tr2025$year == 2027]
cat(sprintf("\nCPI deflation: cpi_2025/cpi_2027 = %.3f / %.3f = %.6f\n",
            tr2025$cpi_w[tr2025$year == 2025], cpi_2027,
            tr2025$cpi_w[tr2025$year == 2025] / cpi_2027))

# Test: does round(our_cola_pia_67 * 12 * cpi_2025/cpi_2027) = 29283?
our_cola_67 <- w67$cola_basic_pia[w67$age == 67]
deflated_67 <- our_cola_67 * 12 * tr2025$cpi_w[tr2025$year == 2025] / cpi_2027
cat(sprintf("Our cola_pia_67=%.2f * 12 * deflator = %.2f -> round = %.0f (V.C7=29283)\n",
            our_cola_67, deflated_67, round(deflated_67)))

# And for age 65:
our_cola_65 <- w$cola_basic_pia[w$age == 65]
# arf at 65 for NRA 67
arf <- 1 - 24 * 5 / 900
annual_65 <- our_cola_65 * arf * 12
cat(sprintf("Our cola_pia_65=%.2f * %.6f * 12 = %.2f -> round = %.0f (V.C7=25172)\n",
            our_cola_65, arf, annual_65, round(annual_65)))

# AH HA! Let me check if V.C7 does round() instead of floor_dime():
# What if V.C7's annual = round(cola_pia * arf * 12) where arf is precise?
# round(2415.40 * 0.866667 * 12) = round(25120.16) = 25120
# That's still not 25172. The gap is in the cola_pia itself.

# The gap must be in AIME. Let me check what AIME V.C7 implies
# via the NRA benefit (no actuarial reduction to confuse things):

# NRA annual in 2025$ = 29283
# NRA annual in 2027$ = 29283 * cpi_2027/cpi_2025
nra_nominal <- 29283 * cpi_2027 / tr2025$cpi_w[tr2025$year == 2025]
nra_monthly <- nra_nominal / 12
cat(sprintf("\nV.C7 NRA benefit in 2027 nominal: $%.2f/year, $%.2f/month\n",
            nra_nominal, nra_monthly))

# At NRA, monthly = floor(cola_pia at 67)
# So cola_pia_67 >= nra_monthly and < nra_monthly + 1
cat(sprintf("Implied cola_pia at 67: [%.2f, %.2f)\n", nra_monthly, nra_monthly + 1))
cat(sprintf("Our cola_pia at 67: %.2f\n", our_cola_67))
cat(sprintf("Gap: $%.2f\n", nra_monthly - our_cola_67))
