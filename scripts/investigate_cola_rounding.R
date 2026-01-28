# Investigate COLA rounding: year-by-year vs cumulative
library(devtools)
load_all()

cat("=============================================================================\n")
cat("COLA ROUNDING INVESTIGATION: 1960 Medium Earner\n")
cat("=============================================================================\n\n")

result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get basic_pia at eligibility
basic_pia <- result$basic_pia[result$age == 62]
cat("basic_pia at age 62:", basic_pia, "\n\n")

# Get COLA values
cola_2022 <- tr2025$cola[tr2025$year == 2022]  # 8.7%
cola_2023 <- tr2025$cola[tr2025$year == 2023]  # 3.2%
cola_2024 <- tr2025$cola[tr2025$year == 2024]  # 2.5%

cat("COLA values:\n")
cat("  2022 COLA (applies to 2023):", cola_2022, "%\n")
cat("  2023 COLA (applies to 2024):", cola_2023, "%\n")
cat("  2024 COLA (applies to 2025):", cola_2024, "%\n\n")

cat("=== METHOD 1: Current approach (cumulative factor on base PIA) ===\n")
cola_cum_factor <- (1 + cola_2022/100) * (1 + cola_2023/100) * (1 + cola_2024/100)
current_cola_pia_65 <- floor(basic_pia * cola_cum_factor)
cat("cola_cum_factor:", cola_cum_factor, "\n")
cat("cola_basic_pia at 65 = floor(", basic_pia, "*", cola_cum_factor, ") =", current_cola_pia_65, "\n")
cat("Annual:", current_cola_pia_65 * 12, "\n\n")

cat("=== METHOD 2: Year-by-year rounding (SSA method?) ===\n")
# Age 62: No COLA yet
pia_62 <- basic_pia
cat("Age 62: PIA =", pia_62, "(no COLA)\n")

# Age 63: Apply 2022 COLA (8.7%)
pia_63_raw <- pia_62 * (1 + cola_2022/100)
pia_63 <- floor(pia_63_raw)
cat("Age 63: PIA = floor(", pia_62, "* 1.087) = floor(", pia_63_raw, ") =", pia_63, "\n")

# Age 64: Apply 2023 COLA (3.2%)
pia_64_raw <- pia_63 * (1 + cola_2023/100)
pia_64 <- floor(pia_64_raw)
cat("Age 64: PIA = floor(", pia_63, "* 1.032) = floor(", pia_64_raw, ") =", pia_64, "\n")

# Age 65: Apply 2024 COLA (2.5%)
pia_65_raw <- pia_64 * (1 + cola_2024/100)
pia_65 <- floor(pia_65_raw)
cat("Age 65: PIA = floor(", pia_64, "* 1.025) = floor(", pia_65_raw, ") =", pia_65, "\n")
cat("Annual:", pia_65 * 12, "\n\n")

cat("=== COMPARISON ===\n")
cat("Method 1 (current - cumulative):", current_cola_pia_65, "monthly,", current_cola_pia_65 * 12, "annual\n")
cat("Method 2 (year-by-year rounding):", pia_65, "monthly,", pia_65 * 12, "annual\n")
cat("Difference:", current_cola_pia_65 - pia_65, "monthly,", (current_cola_pia_65 - pia_65) * 12, "annual\n\n")

cat("=== TABLE V.C7 COMPARISON ===\n")
vc7_annual <- 29283
vc7_monthly <- vc7_annual / 12
cat("Table V.C7 annual:", vc7_annual, "\n")
cat("Table V.C7 monthly:", round(vc7_monthly, 2), "\n\n")

cat("Method 1 vs V.C7:", current_cola_pia_65 * 12 - vc7_annual,
    "(", round((current_cola_pia_65 * 12 - vc7_annual) / vc7_annual * 100, 2), "%)\n")
cat("Method 2 vs V.C7:", pia_65 * 12 - vc7_annual,
    "(", round((pia_65 * 12 - vc7_annual) / vc7_annual * 100, 2), "%)\n")

cat("\n=== WHAT PIA WOULD MATCH V.C7? ===\n")
# V.C7 shows 29283 annual, which is 2440.25 monthly
# If that's the COLA-adjusted PIA, what basic_pia would produce it?
cat("V.C7 implies monthly PIA of:", vc7_monthly, "\n")
cat("If using year-by-year rounding, working backwards:\n")
# PIA_65 = floor(PIA_64 * 1.025) = 2440
# So PIA_64 * 1.025 must be in [2440, 2441)
# PIA_64 in [2440/1.025, 2441/1.025) = [2380.49, 2381.46)
# So PIA_64 = 2380 or 2381
cat("  If monthly = 2440, then PIA_64 must be ~2380-2381\n")
