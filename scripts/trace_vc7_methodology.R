# The 0.17% gap is NOT in AIME computation.
# Let me look at the OTHER side: how V.C7 converts PIA to annual benefit.
#
# V.C7 shows "Annual scheduled benefit amounts in constant 2025 dollars"
# For born 1960, age 65:
#   NRA benefit (col 3): $29,283  (benefit at NRA, deflated to 2025$)
#   Age-65 benefit (col 6): $25,172  (benefit at 65, in 2025$)
#   NRA = 67, so age-65 has 24 months of actuarial reduction
#
# Our computation:
#   Monthly at 65 = $2,093.00
#   Annual = $25,116
#   Gap = $56

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

w65 <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                          age_claim = 65, factors = sef2025, assumptions = tr2025,
                          debugg = TRUE)
w67 <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                          age_claim = 67, factors = sef2025, assumptions = tr2025,
                          debugg = TRUE)

cat("=== Our computation chain ===\n")
cat(sprintf("AIME at 65: %d\n", w65$aime[w65$age == 65]))
cat(sprintf("AIME at 67: %d\n", w67$aime[w67$age == 67]))
cat(sprintf("basic_pia at 65: %.1f\n", w65$basic_pia[w65$age == 65]))
cat(sprintf("basic_pia at 67: %.1f\n", w67$basic_pia[w67$age == 67]))
cat(sprintf("cola_basic_pia at 65: %.2f\n", w65$cola_basic_pia[w65$age == 65]))
cat(sprintf("cola_basic_pia at 67: %.2f\n", w67$cola_basic_pia[w67$age == 67]))

# V.C7 NRA benefit = $29,283 (in 2025$)
# This is for claiming at NRA (67), in year 2027.
# At NRA, no actuarial reduction. Monthly benefit = floor_dime(cola_pia)
# Annual in nominal 2027$ = 12 × floor_dime(cola_pia_67)
# Annual in 2025$ = 12 × floor_dime(cola_pia_67) × CPI_2025 / CPI_2027

cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cpi_2027 <- tr2025$cpi_w[tr2025$year == 2027]
our_cola_67 <- w67$cola_basic_pia[w67$age == 67]
our_monthly_67 <- floor(our_cola_67 * 10) / 10  # floor to dime
our_annual_67_nominal <- our_monthly_67 * 12
our_annual_67_2025 <- our_annual_67_nominal * cpi_2025 / cpi_2027

cat(sprintf("\n=== NRA (67) benefit in 2025$ ===\n"))
cat(sprintf("cola_pia at 67: $%.2f\n", our_cola_67))
cat(sprintf("Monthly (floor_dime): $%.1f\n", our_monthly_67))
cat(sprintf("Annual nominal (2027$): $%.0f\n", our_annual_67_nominal))
cat(sprintf("CPI_2025: %.3f, CPI_2027: %.3f\n", cpi_2025, cpi_2027))
cat(sprintf("Annual in 2025$: $%.2f -> round = $%.0f\n",
            our_annual_67_2025, round(our_annual_67_2025)))
cat(sprintf("V.C7 NRA: $29,283\n"))
cat(sprintf("Gap: $%.0f\n", round(our_annual_67_2025) - 29283))

# =====================================================================
# AGE 65 benefit
# =====================================================================
our_cola_65 <- w65$cola_basic_pia[w65$age == 65]
arf <- 1 - 24 * 5 / 900  # 24 months early
our_monthly_65 <- floor(our_cola_65 * arf * 10) / 10
our_annual_65 <- our_monthly_65 * 12

cat(sprintf("\n=== Age 65 benefit ===\n"))
cat(sprintf("cola_pia at 65: $%.2f\n", our_cola_65))
cat(sprintf("ARF: %.6f\n", arf))
cat(sprintf("cola_pia × ARF: $%.4f\n", our_cola_65 * arf))
cat(sprintf("Monthly (floor_dime): $%.1f\n", our_monthly_65))
cat(sprintf("Annual: $%.0f\n", our_annual_65))
cat(sprintf("V.C7 age 65: $25,172\n"))
cat(sprintf("Gap: $%.0f\n", our_annual_65 - 25172))

# =====================================================================
# What if V.C7 does NOT floor to dime?
# What if V.C7 computes: round(cola_pia × arf × 12)?
# =====================================================================
cat(sprintf("\n=== Alternative: V.C7 = round(cola_pia × arf × 12) ===\n"))
alt_annual_65 <- round(our_cola_65 * arf * 12)
cat(sprintf("round(%.2f × %.6f × 12) = round(%.4f) = $%.0f\n",
            our_cola_65, arf, our_cola_65 * arf * 12, alt_annual_65))
cat(sprintf("V.C7: $25,172, gap: $%.0f\n", alt_annual_65 - 25172))

# And for NRA:
alt_annual_67 <- round(our_cola_67 * 12 * cpi_2025 / cpi_2027)
cat(sprintf("NRA: round(%.2f × 12 × %.6f) = $%.0f (V.C7=$29,283, gap=$%.0f)\n",
            our_cola_67, cpi_2025/cpi_2027,
            alt_annual_67, alt_annual_67 - 29283))

# =====================================================================
# What cola_pia would produce V.C7's exact values?
# =====================================================================
cat(sprintf("\n=== What cola_pia reproduces V.C7? ===\n"))
# For age 65: need monthly × 12 = 25172
# If V.C7 rounds: need cola_pia × arf × 12 in [25171.5, 25172.5)
# cola_pia × 0.866667 × 12 = [25171.5, 25172.5)
# cola_pia × 10.4 = [25171.5, 25172.5)
# cola_pia in [2420.337, 2420.433)
target_cola_65_lo <- 25171.5 / (arf * 12)
target_cola_65_hi <- 25172.5 / (arf * 12)
cat(sprintf("Age 65: need cola_pia in [%.3f, %.3f)\n", target_cola_65_lo, target_cola_65_hi))
cat(sprintf("Our cola_pia at 65: %.2f\n", our_cola_65))
cat(sprintf("Gap: $%.2f\n", target_cola_65_lo - our_cola_65))

# For NRA (67): need cola_pia × 12 × cpi_2025/cpi_2027 in [29282.5, 29283.5)
# cola_pia × 12 × deflator = [29282.5, 29283.5)
deflator <- cpi_2025 / cpi_2027
target_cola_67_lo <- 29282.5 / (12 * deflator)
target_cola_67_hi <- 29283.5 / (12 * deflator)
cat(sprintf("NRA: need cola_pia at 67 in [%.3f, %.3f)\n", target_cola_67_lo, target_cola_67_hi))
cat(sprintf("Our cola_pia at 67: %.2f\n", our_cola_67))

# =====================================================================
# What AIME produces the needed cola_pia?
# =====================================================================
cat(sprintf("\n=== Working backwards: what AIME produces V.C7? ===\n"))
# For age 65:
# cola_pia = 2420.4 (approximately)
# Need to work backwards through COLAs to get basic_pia,
# then from basic_pia to AIME.

# COLAs applied to basic_pia at age 62:
# age 62 -> 63: COLA from year 2022 (age 62)
# age 63 -> 64: COLA from year 2023 (age 63)
# age 64 -> 65: COLA from year 2024 (age 64)

# With recomputation, basic_pia changes each year.
# But the biggest AIME is at age 65 (last year of earnings at 64).
# Let me find what AIME at age 65 produces cola_pia = 2420.4

# Bend points at eligibility (2022):
bp1 <- tr2025$bp1[tr2025$year == 2022]
bp2 <- tr2025$bp2[tr2025$year == 2022]
cola_2022 <- tr2025$cola[tr2025$year == 2022]
cola_2023 <- tr2025$cola[tr2025$year == 2023]
cola_2024 <- tr2025$cola[tr2025$year == 2024]

cat(sprintf("Bend points (2022): bp1=%d, bp2=%d\n", bp1, bp2))
cat(sprintf("COLAs: 2022=%.1f%%, 2023=%.1f%%, 2024=%.1f%%\n", cola_2022, cola_2023, cola_2024))

# Search for AIME that produces cola_pia ≈ 2420.4 at age 65
# With full recomputation at each age
cat("\nSearching for target AIME...\n")
floor_dime <- function(x) floor(x * 10) / 10

for (test_aime in 4700:4740) {
  # basic_pia at 62 (initial)
  bp_62 <- floor_dime(0.9 * min(test_aime, bp1) + 0.32 * max(0, min(test_aime, bp2) - bp1) + 0.15 * max(0, test_aime - bp2))

  # With recomputation, AIME increases each year from 62-65 earnings
  # But for this search, assume AIME is the same at all ages (upper bound)
  # Actually, at age 65, the AIME includes ALL earnings through 64.
  # The recomputation at age 65 uses this AIME.

  # Apply COLAs from eligibility (62):
  cola_pia_62 <- bp_62
  cola_pia_63 <- floor_dime(cola_pia_62 * (1 + cola_2022/100))
  cola_pia_64 <- floor_dime(cola_pia_63 * (1 + cola_2023/100))
  cola_pia_65 <- floor_dime(cola_pia_64 * (1 + cola_2024/100))

  # Now check if recomputation at 65 gives higher:
  # At age 65 with full AIME = test_aime:
  bp_65 <- floor_dime(0.9 * min(test_aime, bp1) + 0.32 * max(0, min(test_aime, bp2) - bp1) + 0.15 * max(0, test_aime - bp2))
  # Replay COLAs on bp_65:
  recomp_63 <- floor_dime(bp_65 * (1 + cola_2022/100))
  recomp_64 <- floor_dime(recomp_63 * (1 + cola_2023/100))
  recomp_65 <- floor_dime(recomp_64 * (1 + cola_2024/100))
  final_cola_65 <- max(cola_pia_65, recomp_65)

  monthly <- floor_dime(final_cola_65 * arf)
  annual <- monthly * 12

  if (abs(annual - 25172) <= 24) {
    cat(sprintf("  AIME=%d: bp_62=%.1f, bp_65=%.1f, cola_65=%.1f, recomp_65=%.1f, final=%.1f, monthly=%.1f, annual=%.0f %s\n",
                test_aime, bp_62, bp_65, cola_pia_65, recomp_65, final_cola_65, monthly, annual,
                ifelse(round(annual) == 25172, "<-- MATCH!", "")))
  }
}

# =====================================================================
# Try with round() on annual instead of floor_dime monthly × 12
# =====================================================================
cat("\n=== With round(cola_pia × arf × 12) methodology ===\n")
for (test_aime in 4700:4740) {
  bp_val <- floor_dime(0.9 * min(test_aime, bp1) + 0.32 * max(0, min(test_aime, bp2) - bp1) + 0.15 * max(0, test_aime - bp2))
  cola_pia_62 <- bp_val
  cola_pia_63 <- floor_dime(cola_pia_62 * (1 + cola_2022/100))
  cola_pia_64 <- floor_dime(cola_pia_63 * (1 + cola_2023/100))
  cola_pia_65 <- floor_dime(cola_pia_64 * (1 + cola_2024/100))

  recomp_63 <- floor_dime(bp_val * (1 + cola_2022/100))
  recomp_64 <- floor_dime(recomp_63 * (1 + cola_2023/100))
  recomp_65 <- floor_dime(recomp_64 * (1 + cola_2024/100))
  final_cola_65 <- max(cola_pia_65, recomp_65)

  annual_method1 <- floor_dime(final_cola_65 * arf) * 12  # our method
  annual_method2 <- round(final_cola_65 * arf * 12)         # round annual

  if (abs(annual_method1 - 25172) <= 24 || abs(annual_method2 - 25172) <= 24) {
    cat(sprintf("  AIME=%d: cola_65=%.1f, m1=%d, m2=%d %s\n",
                test_aime, final_cola_65,
                annual_method1, annual_method2,
                ifelse(annual_method2 == 25172, "<-- MATCH!", "")))
  }
}

# =====================================================================
# What about the recomputation detail?
# Our code recomputes at each age 63, 64, 65 with updated AIME.
# The AIMEs at 63, 64, 65 are different because more earnings are included.
# Let me trace the EXACT recomputation in our pipeline.
# =====================================================================
cat("\n=== Exact recomputation trace from our pipeline ===\n")
for (age in 62:67) {
  if (age <= 64) {
    row <- w65[w65$age == age, ]
  } else if (age <= 65) {
    row <- w65[w65$age == age, ]
  } else {
    row <- w67[w67$age == age, ]
  }
  if (nrow(row) > 0) {
    cat(sprintf("Age %d: AIME=%d, basic_pia=%.1f, cola_pia=%.2f, ben=%.2f\n",
                age, row$aime, row$basic_pia, row$cola_basic_pia, row$ben))
  }
}

# =====================================================================
# Check recomputation at age 65 explicitly
# At age 65, AIME includes earnings from age 64 (last working year)
# =====================================================================
cat("\n=== Recomputation at age 65 detail ===\n")
# Our AIME at age 62 (from claim at 65):
aime_62 <- w65$aime[w65$age == 62]
aime_63 <- w65$aime[w65$age == 63]
aime_64 <- w65$aime[w65$age == 64]
aime_65 <- w65$aime[w65$age == 65]
cat(sprintf("AIME progression: 62=%d, 63=%d, 64=%d, 65=%d\n",
            aime_62, aime_63, aime_64, aime_65))

bp_62 <- floor_dime(0.9*min(aime_62,bp1) + 0.32*max(0,min(aime_62,bp2)-bp1))
bp_63 <- floor_dime(0.9*min(aime_63,bp1) + 0.32*max(0,min(aime_63,bp2)-bp1))
bp_64 <- floor_dime(0.9*min(aime_64,bp1) + 0.32*max(0,min(aime_64,bp2)-bp1))
bp_65 <- floor_dime(0.9*min(aime_65,bp1) + 0.32*max(0,min(aime_65,bp2)-bp1))
cat(sprintf("basic_pia: 62=%.1f, 63=%.1f, 64=%.1f, 65=%.1f\n", bp_62, bp_63, bp_64, bp_65))

# Full COLA chain with recomputation at each age
cola_62 <- bp_62
# Age 63: recompute with aime_63
bp_at_63 <- floor_dime(0.9*min(aime_63,bp1) + 0.32*max(0,min(aime_63,bp2)-bp1))
cola_63_forward <- floor_dime(cola_62 * (1 + cola_2022/100))
recomp_63 <- floor_dime(bp_at_63 * (1 + cola_2022/100))
cola_63 <- max(cola_63_forward, recomp_63)

# Age 64:
bp_at_64 <- floor_dime(0.9*min(aime_64,bp1) + 0.32*max(0,min(aime_64,bp2)-bp1))
cola_64_forward <- floor_dime(cola_63 * (1 + cola_2023/100))
recomp_64_from_bp64 <- floor_dime(floor_dime(bp_at_64 * (1 + cola_2022/100)) * (1 + cola_2023/100))
cola_64 <- max(cola_64_forward, recomp_64_from_bp64)

# Age 65:
bp_at_65 <- floor_dime(0.9*min(aime_65,bp1) + 0.32*max(0,min(aime_65,bp2)-bp1))
cola_65_forward <- floor_dime(cola_64 * (1 + cola_2024/100))
recomp_65_from_bp65_step1 <- floor_dime(bp_at_65 * (1 + cola_2022/100))
recomp_65_from_bp65_step2 <- floor_dime(recomp_65_from_bp65_step1 * (1 + cola_2023/100))
recomp_65_from_bp65_step3 <- floor_dime(recomp_65_from_bp65_step2 * (1 + cola_2024/100))
cola_65 <- max(cola_65_forward, recomp_65_from_bp65_step3)

cat(sprintf("\nAge 62: cola_pia = %.1f\n", cola_62))
cat(sprintf("Age 63: forward=%.1f, recomp(aime=%d)=%.1f, max=%.1f\n", cola_63_forward, aime_63, recomp_63, cola_63))
cat(sprintf("Age 64: forward=%.1f, recomp(aime=%d)=%.1f, max=%.1f\n", cola_64_forward, aime_64, recomp_64_from_bp64, cola_64))
cat(sprintf("Age 65: forward=%.1f, recomp(aime=%d)=%.1f, max=%.1f\n", cola_65_forward, aime_65, recomp_65_from_bp65_step3, cola_65))
cat(sprintf("Our pipeline cola_pia at 65: %.2f\n", w65$cola_basic_pia[w65$age == 65]))

# Monthly at 65
monthly_65 <- floor_dime(cola_65 * arf)
annual_65 <- monthly_65 * 12
cat(sprintf("\nMonthly at 65: floor_dime(%.1f × %.6f) = floor_dime(%.4f) = %.1f\n",
            cola_65, arf, cola_65 * arf, monthly_65))
cat(sprintf("Annual: %.1f × 12 = $%.0f (V.C7=$25,172, gap=$%.0f)\n",
            monthly_65, annual_65, annual_65 - 25172))
