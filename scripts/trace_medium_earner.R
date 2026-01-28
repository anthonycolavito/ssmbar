# Detailed trace of medium earner calculation
# Compare step-by-step with what V.C7 implies

library(devtools)
load_all()

cat("=============================================================================\n")
cat("DETAILED TRACE: MEDIUM EARNER BORN 1960\n")
cat("=============================================================================\n\n")

result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# =============================================================================
# Step 1: Earnings and AIME
# =============================================================================
cat("STEP 1: EARNINGS AND AIME\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

age62 <- result[result$age == 62, ]
cat("At age 62 (2022, eligibility year):\n")
cat("  AIME:", age62$aime, "\n\n")

# Show a few earnings records
cat("Sample earnings (ages 50-64):\n")
for (a in 50:64) {
  row <- result[result$age == a, ]
  cat(sprintf("  Age %d (year %d): earnings = %.2f, indexed = %.2f\n",
              a, row$year, row$earn, row$indexed_earn))
}
cat("\n")

# =============================================================================
# Step 2: Basic PIA at Eligibility
# =============================================================================
cat("STEP 2: BASIC PIA AT ELIGIBILITY (2022)\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("Bend points for 2022 (year turning 62):\n")
cat("  bp1:", age62$bp1_elig, "\n")
cat("  bp2:", age62$bp2_elig, "\n")
cat("  AIME:", age62$aime, "\n\n")

# Manual PIA calculation
aime <- age62$aime
bp1 <- age62$bp1_elig
bp2 <- age62$bp2_elig

if (aime <= bp1) {
  pia_calc <- 0.9 * aime
  cat("AIME <= bp1, so PIA = 0.9 ×", aime, "=", pia_calc, "\n")
} else if (aime <= bp2) {
  part1 <- 0.9 * bp1
  part2 <- 0.32 * (aime - bp1)
  pia_calc <- part1 + part2
  cat("bp1 < AIME <= bp2, so:\n")
  cat("  Part 1: 0.9 ×", bp1, "=", part1, "\n")
  cat("  Part 2: 0.32 × (", aime, "-", bp1, ") =", part2, "\n")
  cat("  Total:", pia_calc, "\n")
} else {
  part1 <- 0.9 * bp1
  part2 <- 0.32 * (bp2 - bp1)
  part3 <- 0.15 * (aime - bp2)
  pia_calc <- part1 + part2 + part3
  cat("AIME > bp2, so:\n")
  cat("  Part 1: 0.9 ×", bp1, "=", part1, "\n")
  cat("  Part 2: 0.32 × (", bp2, "-", bp1, ") =", part2, "\n")
  cat("  Part 3: 0.15 × (", aime, "-", bp2, ") =", part3, "\n")
  cat("  Total:", pia_calc, "\n")
}

cat("  Floor:", floor(pia_calc), "\n")
cat("  Stored basic_pia:", age62$basic_pia, "\n\n")

# =============================================================================
# Step 3: COLA Progression
# =============================================================================
cat("STEP 3: COLA PROGRESSION\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

cat("Year  Age  COLA%  cola_factor  cola_basic_pia  Manual calculation\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

prev_cola_pia <- 0
for (a in 62:67) {
  row <- result[result$age == a, ]

  if (a == 62) {
    manual_cola_pia <- row$basic_pia
    calc_note <- "= basic_pia (no COLA yet)"
  } else {
    # COLA from previous year applies to current year
    prev_row <- result[result$age == a - 1, ]
    cola_pct <- prev_row$cola
    manual_cola_pia <- floor(prev_cola_pia * (1 + cola_pct / 100))
    calc_note <- sprintf("= floor(%d × %.5f)", prev_cola_pia, 1 + cola_pct / 100)
  }

  cat(sprintf("%d  %d   %5.2f  %10.6f  %14d  %s\n",
              row$year, a, row$cola, row$cola_factor, row$cola_basic_pia, calc_note))

  prev_cola_pia <- row$cola_basic_pia
}

cat("\n")

# =============================================================================
# Step 4: Actuarial Factor and Worker Benefit
# =============================================================================
cat("STEP 4: ACTUARIAL FACTOR AND WORKER BENEFIT\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

age65 <- result[result$age == 65, ]

cat("At age 65 (claiming year 2025):\n")
cat("  NRA:", age65$nra_ind, "\n")
cat("  Months early: (", age65$nra_ind, "- 65) × 12 =", (age65$nra_ind - 65) * 12, "\n")
cat("  rf1:", age65$rf1_ind, "\n")
cat("  Reduction: 24 ×", age65$rf1_ind, "=", 24 * age65$rf1_ind, "\n")
cat("  Actuarial factor: 1 -", 24 * age65$rf1_ind, "=", 1 - 24 * age65$rf1_ind, "\n")
cat("  Stored act_factor:", age65$act_factor, "\n\n")

cat("Worker benefit calculation:\n")
cat("  cola_basic_pia:", age65$cola_basic_pia, "\n")
cat("  act_factor:", age65$act_factor, "\n")
cat("  wrk_ben = floor(", age65$cola_basic_pia, "×", age65$act_factor, ") =",
    floor(age65$cola_basic_pia * age65$act_factor), "\n")
cat("  Stored wrk_ben:", age65$wrk_ben, "\n\n")

cat("Annual reduced benefit:", age65$wrk_ben * 12, "\n\n")

# =============================================================================
# Step 5: Comparison with V.C7
# =============================================================================
cat("STEP 5: COMPARISON WITH V.C7\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

# V.C7 values
vc7_full_pia <- 29283
vc7_reduced <- 25172

# Our full PIA at NRA, deflated
age67 <- result[result$age == 67, ]
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cpi_2027 <- tr2025$cpi_w[tr2025$year == 2027]

our_full_annual_2027 <- age67$cola_basic_pia * 12
our_full_deflated <- our_full_annual_2027 * (cpi_2025 / cpi_2027)

cat("Full PIA at NRA (age 67, 2027):\n")
cat("  Our cola_basic_pia_67:", age67$cola_basic_pia, "\n")
cat("  Our annual (2027 dollars):", our_full_annual_2027, "\n")
cat("  CPI-W 2025:", cpi_2025, "\n")
cat("  CPI-W 2027:", cpi_2027, "\n")
cat("  Deflation factor:", round(cpi_2025 / cpi_2027, 6), "\n")
cat("  Our annual deflated to 2025:", round(our_full_deflated, 0), "\n")
cat("  V.C7 full PIA:", vc7_full_pia, "\n")
cat("  Difference:", round(our_full_deflated - vc7_full_pia, 0),
    "(", round((our_full_deflated - vc7_full_pia) / vc7_full_pia * 100, 2), "%)\n\n")

cat("Reduced at 65 (2025):\n")
cat("  Our wrk_ben × 12:", age65$wrk_ben * 12, "\n")
cat("  V.C7 reduced:", vc7_reduced, "\n")
cat("  Difference:", age65$wrk_ben * 12 - vc7_reduced,
    "(", round((age65$wrk_ben * 12 - vc7_reduced) / vc7_reduced * 100, 2), "%)\n\n")

# =============================================================================
# Step 6: Reverse Engineering V.C7
# =============================================================================
cat("STEP 6: REVERSE ENGINEERING V.C7\n")
cat(paste(rep("-", 70), collapse = ""), "\n\n")

# What would V.C7's monthly reduced benefit be?
vc7_monthly_reduced <- vc7_reduced / 12
cat("V.C7 monthly reduced: $", round(vc7_monthly_reduced, 2), "\n")

# If actuarial factor is 0.8667, what cola_basic_pia would give this?
implied_cola_pia_65 <- vc7_monthly_reduced / 0.866666667
cat("If act_factor = 0.8667, implied cola_basic_pia_65 = $", round(implied_cola_pia_65, 2), "\n")
cat("Our cola_basic_pia_65: $", age65$cola_basic_pia, "\n")
cat("Difference in cola_basic_pia: $", round(age65$cola_basic_pia - implied_cola_pia_65, 2), "\n\n")

# What monthly PIA difference would explain the annual difference?
annual_diff <- age65$wrk_ben * 12 - vc7_reduced
monthly_diff <- annual_diff / 12
cat("Annual difference:", annual_diff, "\n")
cat("Monthly difference:", round(monthly_diff, 2), "\n")
cat("This is approximately", round(monthly_diff), "dollars per month difference.\n")
