# Proper validation against V.C7 accounting for deflation
# V.C7 shows all benefits in 2025 CPI-W adjusted dollars
#
# Key insight from user:
# - "Full PIA at NRA" column: Benefits at NRA (2027), received in 2027, deflated to 2025 dollars
# - "Reduced at 65" column: Benefits claimed at 65 (2025), already in 2025 dollars

library(devtools)
load_all()

cat("=============================================================================\n")
cat("V.C7 VALIDATION WITH PROPER DEFLATION\n")
cat("=============================================================================\n\n")

# Worker born 1960, turning 65 in 2025
# NRA = 67, so benefits at NRA are in 2027

# Get CPI-W values from assumptions
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cpi_2027 <- tr2025$cpi_w[tr2025$year == 2027]

cat("CPI-W values from tr2025:\n")
cat("  2025:", cpi_2025, "\n")
cat("  2027:", cpi_2027, "\n")
cat("  Deflation factor (2025/2027):", cpi_2025 / cpi_2027, "\n\n")

# Calculate benefits for medium earner
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get benefits at different ages
age65 <- result[result$age == 65, ]
age67 <- result[result$age == 67, ]

cat("=== MEDIUM EARNER CALCULATION ===\n\n")

cat("At Age 65 (claiming year 2025):\n")
cat("  basic_pia:", age65$basic_pia, "\n")
cat("  cola_basic_pia:", age65$cola_basic_pia, "\n")
cat("  act_factor:", age65$act_factor, "\n")
cat("  wrk_ben (monthly reduced):", age65$wrk_ben, "\n")
cat("  annual reduced:", age65$wrk_ben * 12, "(nominal 2025 dollars)\n\n")

cat("At Age 67 (NRA, year 2027):\n")
cat("  cola_basic_pia:", age67$cola_basic_pia, "\n")
cat("  annual full PIA:", age67$cola_basic_pia * 12, "(nominal 2027 dollars)\n")

# Deflate the 2027 benefit to 2025 dollars
full_pia_2027_annual <- age67$cola_basic_pia * 12
full_pia_2025_deflated <- full_pia_2027_annual * (cpi_2025 / cpi_2027)

cat("  deflated to 2025 dollars:", round(full_pia_2025_deflated, 0), "\n\n")

cat("=== COMPARISON TO V.C7 ===\n\n")

# V.C7 values for medium earner (from user's investigation)
vc7_full_pia <- 29283
vc7_reduced_65 <- 25172

cat("V.C7 values (medium earner born 1960):\n")
cat("  Full PIA (at NRA, in 2025 dollars):", vc7_full_pia, "\n")
cat("  Reduced at 65 (in 2025 dollars):", vc7_reduced_65, "\n\n")

cat("Our calculations:\n")
cat("  Full PIA at NRA (deflated to 2025):", round(full_pia_2025_deflated, 0), "\n")
cat("  Reduced at 65:", age65$wrk_ben * 12, "\n\n")

cat("Differences:\n")
cat("  Full PIA: ssmbar =", round(full_pia_2025_deflated, 0), "vs V.C7 =", vc7_full_pia,
    "  Diff =", round(full_pia_2025_deflated - vc7_full_pia, 0),
    "(", round((full_pia_2025_deflated - vc7_full_pia) / vc7_full_pia * 100, 2), "%)\n")
cat("  Reduced:  ssmbar =", age65$wrk_ben * 12, "vs V.C7 =", vc7_reduced_65,
    "  Diff =", age65$wrk_ben * 12 - vc7_reduced_65,
    "(", round((age65$wrk_ben * 12 - vc7_reduced_65) / vc7_reduced_65 * 100, 2), "%)\n\n")

# Verify the actuarial factor
cat("=== ACTUARIAL FACTOR VERIFICATION ===\n\n")

# Compare benefits in the SAME year's dollars to get actuarial factor
# Use benefits at age 65 (2025)
full_benefit_65_if_at_nra <- age65$cola_basic_pia * 12  # What full benefit would be at 65 if no reduction
reduced_benefit_65 <- age65$wrk_ben * 12

cat("At age 65 (2025), comparing full vs reduced in same year's dollars:\n")
cat("  cola_basic_pia × 12 (full):", full_benefit_65_if_at_nra, "\n")
cat("  wrk_ben × 12 (reduced):", reduced_benefit_65, "\n")
cat("  Actual actuarial factor used:", age65$act_factor, "\n")
cat("  Implied factor from our calc:", round(reduced_benefit_65 / full_benefit_65_if_at_nra, 6), "\n\n")

cat("=== COLA PROGRESSION ===\n\n")
cat("Year   Age   COLA%   cola_basic_pia   cum_factor\n")
cat("----   ---   -----   --------------   ----------\n")
for (a in 62:67) {
  row <- result[result$age == a, ]
  cat(sprintf("%d   %d   %5.2f   %14d   %10.6f\n",
              row$year, a, row$cola, row$cola_basic_pia, row$cola_cum_factor))
}
