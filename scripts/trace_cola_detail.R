# Deep trace of the COLA and PIA computation for medium earner born 1960
# Goal: Understand exactly what's happening at each step and why V.C7 differs

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

d <- tr2025

# Show COLA values for 2020-2030
cat("=== COLA Values in Assumptions (from cola.csv / projection) ===\n")
cat(sprintf("%-6s %-8s %-10s\n", "Year", "COLA%", "CPI-W"))
for (yr in 2020:2030) {
  cat(sprintf("%-6d %-8.2f %-10.3f\n", yr, d$cola[d$year == yr], d$cpi_w[d$year == yr]))
}

# Generate worker with full debug
w <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = d,
  debugg = TRUE
)

cat("\n=== AIME at Each Age (62-65) ===\n")
cat("Note: AIME changes each year because earnings from previous year are added\n")
cat(sprintf("%-4s %-6s %-6s %-14s\n", "Age", "Year", "AIME", "Earnings(yr-1)"))
for (age in 62:65) {
  r <- w[w$age == age, ]
  # Earnings from the previous year
  prev_earn <- w$earnings[w$age == age - 1]
  cat(sprintf("%-4d %-6d %-6d %-14.2f\n", age, r$year, r$aime, prev_earn))
}

cat("\n=== PIA (basic_pia) at Each Age ===\n")
cat("PIA = 0.90 * min(AIME, bp1) + 0.32 * min(max(AIME-bp1,0), bp2-bp1) + 0.15 * max(AIME-bp2,0)\n")
cat("Bend points determined at eligibility year (2022): bp1=1024, bp2=6172\n")
bp1 <- d$bp1[d$year == 2022]
bp2 <- d$bp2[d$year == 2022]
for (age in 62:65) {
  r <- w[w$age == age, ]
  aime <- r$aime
  raw_pia <- 0.90 * min(aime, bp1) + 0.32 * min(max(aime - bp1, 0), bp2 - bp1) + 0.15 * max(aime - bp2, 0)
  cat(sprintf("  Age %d: AIME=%d -> raw PIA=%.2f -> floor_dime=%.1f (stored=%.2f)\n",
              age, aime, raw_pia, floor(raw_pia * 10) / 10, r$basic_pia))
}

cat("\n=== COLA Application Step-by-Step ===\n")
cat("Worker eligible in 2022. COLA timing:\n")
cat("  - Age 62 (2022): No COLA - this is eligibility year\n")
cat("  - Age 63 (2023): Apply 2022 COLA (announced Oct 2022, effective Jan 2023)\n")
cat("  - Age 64 (2024): Apply 2023 COLA (announced Oct 2023, effective Jan 2024)\n")
cat("  - Age 65 (2025): Apply 2024 COLA (announced Oct 2024, effective Jan 2025)\n\n")

cat(sprintf("%-4s %-6s %-12s %-10s %-12s %-14s\n",
            "Age", "Year", "basic_pia", "cola_fac", "lag_cola%", "cola_basic_pia"))
for (age in 62:65) {
  r <- w[w$age == age, ]
  cola_in_row <- d$cola[d$year == r$year]
  prev_cola <- if (age > 62) d$cola[d$year == r$year - 1] else NA
  cat(sprintf("%-4d %-6d %-12.2f %-10.4f %-12s %-14.2f\n",
              age, r$year, r$basic_pia, r$cola_factor,
              ifelse(is.na(prev_cola), "N/A", sprintf("%.1f", prev_cola)),
              r$cola_basic_pia))
}

# Now manually trace COLA application
cat("\n=== Manual COLA Trace (how cola_basic_pia is built) ===\n")
pia_62 <- w$basic_pia[w$age == 62]
cat(sprintf("Age 62: cola_basic_pia = basic_pia = %.2f\n", pia_62))

# What COLA applies at age 63?
# cola_factor at age 63 = 1 + lag(cola)/100
# lag(cola) for age 63 is the cola at age 62 = cola for year 2022
cola_yr_2022 <- d$cola[d$year == 2022]
cat(sprintf("  cola in year 2022: %.2f%%\n", cola_yr_2022))
cat(sprintf("  cola_factor at age 63 = 1 + %.2f/100 = %.6f\n", cola_yr_2022, 1 + cola_yr_2022/100))

# But wait: the basic_pia at age 63 might differ from age 62 (because AIME changed)
# The COLA is applied to the PREVIOUS YEAR'S cola_basic_pia, not the current basic_pia
basic_pia_63 <- w$basic_pia[w$age == 63]
cola_factor_63 <- w$cola_factor[w$age == 63]
new_cola_pia_63 <- floor(pia_62 * cola_factor_63 * 10) / 10
cat(sprintf("  Age 63: prev_cola_pia(%.2f) * cola_factor(%.6f) = %.4f -> floor_dime = %.1f\n",
            pia_62, cola_factor_63, pia_62 * cola_factor_63, new_cola_pia_63))
cat(sprintf("  But basic_pia at age 63 = %.2f (AIME increased from %d to %d)\n",
            basic_pia_63, w$aime[w$age == 62], w$aime[w$age == 63]))
cat(sprintf("  Stored cola_basic_pia at age 63: %.2f\n", w$cola_basic_pia[w$age == 63]))

# KEY QUESTION: Does the COLA'd PIA track the INCREASING basic PIA, or just
# apply COLAs to the age-62 PIA?
# From the code (line 499): cola_basic_pia_vals[i] = floor_dime(cola_basic_pia_vals[i-1] * cola_factor[i])
# This applies COLA to the PREVIOUS cola_basic_pia, ignoring the new (higher) basic_pia

cat("\n=== CRITICAL: Does V.C7 use recalculated PIA at each age? ===\n")
cat("Our code COLAs the age-62 PIA forward (never re-bases to higher AIME).\n")
cat("But SSA actually recalculates PIA at each year with new earnings,\n")
cat("then takes the MAX of (COLA'd old PIA, new PIA).\n\n")

# What would happen if we used the highest basic_pia at each age?
cat("Alternative: Use basic_pia recalculated at each age, then COLA from there:\n")
for (age in 62:65) {
  r <- w[w$age == age, ]
  cat(sprintf("  Age %d: basic_pia=%.2f (AIME=%d)\n", age, r$basic_pia, r$aime))
}

# Actually per SSA rules, the PIA is recalculated when new earnings are added
# and the beneficiary gets the HIGHER of:
# (a) the old COLA'd PIA
# (b) the new recalculated PIA (with new earnings) + applicable COLAs
# This is called an "automatic recomputation" (SSA Handbook 715)

cat("\n=== Testing Automatic Recomputation Theory ===\n")
# Approach: At each age, compute PIA from that age's AIME, then COLA from eligibility year
for (age in 62:65) {
  r <- w[w$age == age, ]
  aime_at_age <- r$aime
  raw_pia <- 0.90 * min(aime_at_age, bp1) + 0.32 * min(max(aime_at_age - bp1, 0), bp2 - bp1) + 0.15 * max(aime_at_age - bp2, 0)
  basic_pia <- floor(raw_pia * 10) / 10

  # Apply COLAs from eligibility year to current year
  cola_pia <- basic_pia
  if (age > 62) {
    for (yr in 2023:(2022 + (age - 62))) {
      prev_cola <- d$cola[d$year == yr - 1]
      cola_pia <- floor(cola_pia * (1 + prev_cola / 100) * 10) / 10
    }
  }
  cat(sprintf("  Age %d: AIME=%d, basic_pia=%.1f, cola_pia=%.1f\n",
              age, aime_at_age, basic_pia, cola_pia))
}

# Compare: our approach (COLA from age-62 PIA) vs recomputation (COLA from latest PIA)
cat("\n=== Side-by-Side: Our COLA'd PIA vs Recomputed ===\n")
cat(sprintf("%-4s %-12s %-12s %-10s\n", "Age", "Our_cola_pia", "Recomp_pia", "Diff"))

prev_cola_pia_recomp <- 0
for (age in 62:65) {
  r <- w[w$age == age, ]
  aime_at_age <- r$aime
  raw_pia <- 0.90 * min(aime_at_age, bp1) + 0.32 * min(max(aime_at_age - bp1, 0), bp2 - bp1) + 0.15 * max(aime_at_age - bp2, 0)
  basic_pia <- floor(raw_pia * 10) / 10

  if (age == 62) {
    recomp_cola_pia <- basic_pia
  } else {
    # Use the MAX of COLA'd-forward vs new basic PIA with COLAs
    # Method: new PIA + all applicable COLAs
    new_pia_with_colas <- basic_pia
    for (yr in 2023:(2022 + (age - 62))) {
      prev_cola <- d$cola[d$year == yr - 1]
      new_pia_with_colas <- floor(new_pia_with_colas * (1 + prev_cola / 100) * 10) / 10
    }
    # COLA'd forward from previous
    prev_cola_pct <- d$cola[d$year == 2022 + (age - 62) - 1]
    old_cola_forward <- floor(prev_cola_pia_recomp * (1 + prev_cola_pct / 100) * 10) / 10

    recomp_cola_pia <- max(old_cola_forward, new_pia_with_colas)
  }
  prev_cola_pia_recomp <- recomp_cola_pia

  our_pia <- r$cola_basic_pia
  cat(sprintf("%-4d %-12.2f %-12.2f %-10.2f\n", age, our_pia, recomp_cola_pia, recomp_cola_pia - our_pia))
}

# Now compute the benefit at 65 with the recomputed PIA
recomp_pia_65 <- prev_cola_pia_recomp
arf <- 1 - 24 * (5/900)
recomp_monthly_65 <- floor(recomp_pia_65 * arf * 10) / 10
cat(sprintf("\nRecomputed COLA'd PIA at 65: %.2f\n", recomp_pia_65))
cat(sprintf("Recomputed monthly at 65: %.1f\n", recomp_monthly_65))
cat(sprintf("Recomputed annual at 65: %.0f\n", recomp_monthly_65 * 12))
cat(sprintf("V.C7 annual at 65: 25172\n"))
cat(sprintf("Our annual at 65: %.0f\n", w$ben[w$age == 65] * 12))
