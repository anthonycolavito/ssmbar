# Investigate January 1 vs January 2 birth date effect
#
# SSA rule: You "attain" an age the day BEFORE your birthday
# - Born January 1, 1960: Attains age 60 on December 31, 2019 → index year 2019
# - Born January 2, 1960: Attains age 60 on January 1, 2020 → index year 2020
#
# This affects the AWI used for indexing earnings in the AIME calculation.

library(devtools)
load_all()

cat("=============================================================================\n")
cat("JANUARY 1 VS JANUARY 2 BIRTH DATE INVESTIGATION\n")
cat("=============================================================================\n\n")

cat("SSA Rule: You 'attain' an age the day BEFORE your birthday\n\n")

cat("For someone born January 1, 1960:\n")
cat("  Attains age 60 on December 31, 2019\n")
cat("  Indexing year = 2019\n\n")

cat("For someone born January 2, 1960 (SSA hypothetical worker):\n")
cat("  Attains age 60 on January 1, 2020\n")
cat("  Indexing year = 2020\n\n")

# Get AWI values
awi_2019 <- tr2025$awi[tr2025$year == 2019]
awi_2020 <- tr2025$awi[tr2025$year == 2020]

cat("AWI values:\n")
cat("  2019:", awi_2019, "\n")
cat("  2020:", awi_2020, "\n")
cat("  Ratio (2020/2019):", awi_2020 / awi_2019, "\n\n")

# Our current calculation uses which indexing year?
cat("=== OUR CURRENT CALCULATION ===\n\n")

result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Check the indexing factor used at age 60
age60 <- result[result$age == 60, ]
cat("At age 60 (2020):\n")
cat("  Year:", age60$year, "\n")
cat("  awi_index (indexing factor):", age60$awi_index, "\n")

# The awi_index should be awi_indexing_year / awi_current_year
# If indexing to 2020: awi_index at 2020 should be 1.0
# If indexing to 2019: awi_index at 2020 should be awi_2019 / awi_2020
expected_index_if_2020 <- 1.0
expected_index_if_2019 <- awi_2019 / awi_2020

cat("  Expected if indexing to 2020:", expected_index_if_2020, "\n")
cat("  Expected if indexing to 2019:", expected_index_if_2019, "\n\n")

# Check what indexing year our code uses
cat("=== CHECKING OUR INDEXING LOGIC ===\n\n")

# The index factor at age 60 should tell us
# If it's 1.0, we're indexing to 2020
# If it's less than 1, we might be indexing to an earlier year

age59 <- result[result$age == 59, ]
age61 <- result[result$age == 61, ]

cat("Indexing factors by age:\n")
cat("  Age 59 (2019): awi_index =", age59$awi_index, "\n")
cat("  Age 60 (2020): awi_index =", age60$awi_index, "\n")
cat("  Age 61 (2021): awi_index =", age61$awi_index, "\n\n")

# The indexing year is the year in which awi_index = 1.0
# Let's find it
for (a in 58:64) {
  row <- result[result$age == a, ]
  if (abs(row$awi_index - 1.0) < 0.001) {
    cat("Found indexing year: age", a, "= year", row$year, "\n")
    cat("  (awi_index =", row$awi_index, ")\n\n")
    break
  }
}

cat("=== IMPACT ON AIME ===\n\n")

# If we indexed to 2019 instead of 2020, how would AIME change?
# Higher AWI for indexing = lower indexed earnings = lower AIME

cat("If indexing to 2020 (current):\n")
cat("  AIME:", result$aime[result$age == 62], "\n\n")

# Calculate what AIME would be with 2019 indexing
# This requires rerunning the calculation, but we can estimate
aime_current <- result$aime[result$age == 62]
# If indexing year changes from 2020 to 2019, all indexed earnings would be scaled by:
scale_factor <- awi_2019 / awi_2020

cat("If indexing to 2019:\n")
cat("  Scale factor:", scale_factor, "\n")
cat("  Estimated AIME:", round(aime_current * scale_factor), "\n\n")

# What would the difference in PIA be?
bp1 <- 1115  # 2022 bend point
bp2 <- 6721

aime_2020 <- aime_current
aime_2019 <- round(aime_current * scale_factor)

pia_2020 <- floor(0.9 * bp1 + 0.32 * (aime_2020 - bp1))
pia_2019 <- floor(0.9 * bp1 + 0.32 * (aime_2019 - bp1))

cat("=== IMPACT ON BASIC PIA ===\n\n")
cat("With 2020 indexing:\n")
cat("  AIME:", aime_2020, "\n")
cat("  basic_pia:", pia_2020, "\n\n")

cat("With 2019 indexing (estimate):\n")
cat("  AIME:", aime_2019, "\n")
cat("  basic_pia:", pia_2019, "\n\n")

cat("Difference in basic_pia:", pia_2020 - pia_2019, "\n\n")

# How does this flow through to the reduced benefit?
# COLA progression: 2124 → 2308 → 2381 → 2440
# If basic_pia were lower by X, cola_basic_pia_65 would be approximately:
# lower_basic_pia × 1.149829 (cumulative COLA factor)

lower_basic_pia <- pia_2019
lower_cola_pia_65 <- lower_basic_pia * 1.149829
lower_wrk_ben <- floor(lower_cola_pia_65 * 0.866667)
lower_annual <- lower_wrk_ben * 12

cat("=== ESTIMATED BENEFIT WITH 2019 INDEXING ===\n\n")
cat("basic_pia:", lower_basic_pia, "\n")
cat("cola_basic_pia at 65 (approx):", round(lower_cola_pia_65), "\n")
cat("wrk_ben (monthly):", lower_wrk_ben, "\n")
cat("Annual reduced:", lower_annual, "\n\n")

cat("V.C7 reduced:", 25172, "\n")
cat("Our current:", 25368, "\n")
cat("With 2019 indexing:", lower_annual, "\n\n")

cat("=== SUMMARY ===\n\n")
cat("The January 1 vs January 2 birth date affects the AIME indexing year.\n")
cat("Born January 2 (SSA hypothetical) → index to 2020\n")
cat("Born January 1 (our assumption) → index to 2019 (if we follow SSA attainment rule)\n\n")
cat("However, we need to verify what indexing year our code actually uses.\n")
