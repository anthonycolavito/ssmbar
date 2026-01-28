# Check which earnings years are being used in AIME calculation
# Compare with what SSA would use for January 2 birth date

library(devtools)
load_all()

cat("=============================================================================\n")
cat("EARNINGS YEARS SELECTION CHECK\n")
cat("=============================================================================\n\n")

result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get earnings at eligibility (when AIME is first calculated)
age62 <- result[result$age == 62, ]

cat("At age 62 (2022, eligibility year):\n")
cat("  AIME:", age62$aime, "\n")
cat("  comp_period:", age62$comp_period, "\n")
cat("  elig_age:", age62$elig_age, "\n\n")

# Show all indexed earnings from age 21 to 61 (years available for AIME at age 62)
cat("=== INDEXED EARNINGS AVAILABLE FOR AIME AT AGE 62 ===\n")
cat("(Using earnings through age 61, since claim is January 1 at age 62)\n\n")

cat(sprintf("%-5s %-6s %15s %15s %15s\n", "Age", "Year", "Raw Earnings", "Capped", "Indexed"))
cat(paste(rep("-", 60), collapse = ""), "\n")

# Extract indexed earnings for ages 21-61
earnings_data <- result[result$age >= 21 & result$age <= 61,
                        c("age", "year", "earnings", "capped_earn", "indexed_earn")]

for (i in 1:nrow(earnings_data)) {
  cat(sprintf("%-5d %-6d %15.2f %15.2f %15.2f\n",
              earnings_data$age[i], earnings_data$year[i],
              earnings_data$earnings[i], earnings_data$capped_earn[i],
              earnings_data$indexed_earn[i]))
}

cat("\n=== TOP 35 INDEXED EARNINGS ===\n\n")

indexed_earnings <- earnings_data$indexed_earn
top_35 <- sort(indexed_earnings, decreasing = TRUE)[1:35]

cat("Rank  Indexed Earnings\n")
cat(paste(rep("-", 25), collapse = ""), "\n")
for (i in 1:35) {
  cat(sprintf("%4d  %15.2f\n", i, top_35[i]))
}

total_top_35 <- sum(top_35)
calculated_aime <- floor(total_top_35 / (35 * 12))

cat("\n")
cat("Sum of top 35:", round(total_top_35, 2), "\n")
cat("Divided by (35 × 12):", round(total_top_35 / (35 * 12), 2), "\n")
cat("Floor (AIME):", calculated_aime, "\n")
cat("Stored AIME:", age62$aime, "\n")
cat("Match:", calculated_aime == age62$aime, "\n\n")

# What would AIME need to be to produce V.C7's implied basic_pia?
# V.C7 reduced = 25172, act_factor = 0.8667
# implied wrk_ben = 25172/12 = 2097.67
# implied cola_pia_65 = 2097.67 / 0.8667 = 2420.38
# implied basic_pia = 2420.38 / 1.149829 = 2105 (approximately)

cat("=== REVERSE ENGINEERING V.C7's IMPLIED AIME ===\n\n")

# If basic_pia = 2105 (estimated from V.C7)
# PIA = 0.9 × bp1 + 0.32 × (AIME - bp1)
# 2105 = 0.9 × 1115 + 0.32 × (AIME - 1115)
# 2105 = 1003.5 + 0.32 × (AIME - 1115)
# 1101.5 = 0.32 × (AIME - 1115)
# 3442.19 = AIME - 1115
# AIME = 4557

implied_basic_pia <- round(2420.38 / 1.149829)
implied_aime <- round(1115 + (implied_basic_pia - 0.9 * 1115) / 0.32)

cat("V.C7 implied cola_basic_pia_65:", 2420.38, "\n")
cat("V.C7 implied basic_pia (÷ COLA factor):", implied_basic_pia, "\n")
cat("V.C7 implied AIME:", implied_aime, "\n\n")

cat("Our AIME:", age62$aime, "\n")
cat("Our basic_pia:", age62$basic_pia, "\n")
cat("Difference in AIME:", age62$aime - implied_aime, "\n")
cat("Difference in basic_pia:", age62$basic_pia - implied_basic_pia, "\n\n")

# What earnings difference would explain this AIME difference?
aime_diff <- age62$aime - implied_aime
annual_indexed_diff <- aime_diff * 12 * 35  # Total indexed earnings difference

cat("=== EARNINGS IMPACT ===\n")
cat("AIME difference:", aime_diff, "per month\n")
cat("Total indexed earnings difference (over 35 years):", annual_indexed_diff, "\n")
cat("Average per year:", round(annual_indexed_diff / 35, 2), "\n")
