# Trace AIME computation step-by-step for medium earner born 1960
# Goal: Identify exactly where rounding differences arise vs SSA methodology
#
# SSA's AIME computation (per POMS RS 00605.360 and Handbook 700-701):
# 1. Compute nominal earnings = scaled_factor × AWI
# 2. Cap at taxable maximum: capped = min(earnings, taxmax)
# 3. Index: indexed_earn = capped × (AWI_index_year / AWI_earn_year)
#    --> SSA rounds indexed earnings to nearest CENT at each year
# 4. Select top 35 years of indexed earnings
# 5. Sum them, divide by 420, truncate to dollar = AIME

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# Generate medium earner born 1960 with debug mode
w <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium",
  age_claim = 65, factors = sef2025, assumptions = tr2025,
  debugg = TRUE
)

# Extract the earnings/indexing data for working years
work <- w %>%
  filter(age >= 21, age <= 64) %>%
  select(age, year, earnings, capped_earn, awi, index_factor, indexed_earn, aime)

# Show our unrounded vs rounded-to-cent indexed earnings
cat("=== AIME Trace: Medium Earner Born 1960 ===\n\n")
cat(sprintf("%-4s %-6s %-14s %-14s %-12s %-14s %-14s %-14s\n",
            "Age", "Year", "Earnings", "Capped", "Idx_Factor", "Indexed(ours)", "Earn_Rounded", "Indexed_Rnd"))

total_indexed_ours <- 0
total_indexed_earn_rounded <- 0
total_indexed_both_rounded <- 0

for (i in seq_len(nrow(work))) {
  r <- work[i, ]

  # Our method: no rounding at any intermediate step
  our_indexed <- r$indexed_earn

  # Alternative 1: Round earnings to nearest dollar before capping/indexing
  earn_rounded <- round(r$earnings, 0)
  capped_rounded <- pmin(earn_rounded, tr2025$taxmax[tr2025$year == r$year])
  indexed_from_rounded_earn <- capped_rounded * r$index_factor

  # Alternative 2: Round indexed earnings to nearest cent (POMS RS 00605.360)
  indexed_rounded_cent <- round(r$indexed_earn, 2)

  # Alternative 3: Both - round earnings to dollar, then round indexed to cent
  indexed_both <- round(indexed_from_rounded_earn, 2)

  cat(sprintf("%-4d %-6d %-14.2f %-14.2f %-12.6f %-14.2f %-14.0f %-14.2f\n",
              r$age, r$year, r$earnings, r$capped_earn, r$index_factor,
              our_indexed, earn_rounded, indexed_both))

  total_indexed_ours <- total_indexed_ours + our_indexed
  total_indexed_earn_rounded <- total_indexed_earn_rounded + indexed_from_rounded_earn
  total_indexed_both_rounded <- total_indexed_both_rounded + indexed_both
}

# Now compute AIME under different rounding approaches
comp_period <- 35
months <- comp_period * 12  # 420

# Get top 35 for each approach
top35_ours <- sum(sort(work$indexed_earn, decreasing = TRUE)[1:35])

# Recalculate with earnings rounded to nearest dollar
earn_rounded_all <- round(work$earnings, 0)
capped_rounded_all <- pmin(earn_rounded_all, tr2025$taxmax[match(work$year, tr2025$year)])
indexed_rounded_all <- capped_rounded_all * work$index_factor
top35_earn_rounded <- sum(sort(indexed_rounded_all, decreasing = TRUE)[1:35])

# Recalculate with indexed earnings rounded to nearest cent
indexed_cent_rounded <- round(work$indexed_earn, 2)
top35_cent_rounded <- sum(sort(indexed_cent_rounded, decreasing = TRUE)[1:35])

# Both: round earnings to dollar, then round indexed to cent
indexed_both_all <- round(indexed_rounded_all, 2)
top35_both <- sum(sort(indexed_both_all, decreasing = TRUE)[1:35])

cat("\n=== AIME Comparison Under Different Rounding ===\n\n")
cat(sprintf("%-40s %-15s %-8s\n", "Method", "Top-35 Sum", "AIME"))
cat(sprintf("%-40s %-15.2f %-8d\n", "Ours (no intermediate rounding)",
            top35_ours, floor(top35_ours / months)))
cat(sprintf("%-40s %-15.2f %-8d\n", "Earnings rounded to $1",
            top35_earn_rounded, floor(top35_earn_rounded / months)))
cat(sprintf("%-40s %-15.2f %-8d\n", "Indexed rounded to $0.01",
            top35_cent_rounded, floor(top35_cent_rounded / months)))
cat(sprintf("%-40s %-15.2f %-8d\n", "Both: earn→$1, indexed→$0.01",
            top35_both, floor(top35_both / months)))

# What AIME does V.C7 imply for this worker?
# V.C7 shows annual benefit at 65 = $25,172 (2025 CPI-W dollars)
# For birth year 1960, turning 65 in 2025, CPI deflation factor = 1.0
# Monthly benefit = 25172 / 12 = $2097.67
# With actuarial reduction for 24 months early: factor = 1 - 24*(5/900) = 0.8667
# So PIA = monthly_ben / 0.8667
# And PIA comes from the bend point formula on AIME

cat("\n=== Reverse-Engineering V.C7's Implied AIME ===\n")
vc7_annual_65 <- 25172  # From V.C7 for medium earner born 1960
vc7_monthly_65 <- vc7_annual_65 / 12
cat(sprintf("V.C7 annual benefit at 65: $%d\n", vc7_annual_65))
cat(sprintf("V.C7 monthly benefit at 65: $%.2f\n", vc7_monthly_65))

# Actuarial reduction: 24 months early (NRA=67, claim=65)
# First 36 months: 5/9 of 1% per month
rf <- 24 * (5/900)
factor_65 <- 1 - rf
cat(sprintf("Actuarial reduction factor at 65: %.6f\n", factor_65))

implied_pia <- vc7_monthly_65 / factor_65
cat(sprintf("Implied PIA: $%.2f\n", implied_pia))

# Reverse the PIA formula to get AIME
# PIA = 0.90 * min(aime, bp1) + 0.32 * min(max(aime-bp1,0), bp2-bp1) + 0.15 * max(aime-bp2, 0)
# For 1960 born, eligibility year 2022, bend points:
bp1 <- tr2025$bp1[tr2025$year == 2022]
bp2 <- tr2025$bp2[tr2025$year == 2022]
cat(sprintf("Bend points for eligibility year 2022: bp1=%d, bp2=%d\n", bp1, bp2))

# Our PIA and AIME at age 65
our_aime <- w$aime[w$age == 65]
our_pia <- w$basic_pia[w$age == 65]
our_ben <- w$ben[w$age == 65]
cat(sprintf("\nOur AIME: %d\n", our_aime))
cat(sprintf("Our basic PIA: %.2f\n", our_pia))
cat(sprintf("Our monthly benefit at 65: %.2f\n", our_ben))
cat(sprintf("Our annual benefit: %.2f\n", our_ben * 12))

# For a medium earner, AIME is likely between bp1 and bp2
# PIA = 0.90 * bp1 + 0.32 * (AIME - bp1)
# implied_pia = 0.90 * bp1 + 0.32 * (implied_aime - bp1)
# implied_aime = (implied_pia - 0.90 * bp1) / 0.32 + bp1
if (implied_pia > 0.90 * bp1) {
  implied_aime <- (implied_pia - 0.90 * bp1) / 0.32 + bp1
  cat(sprintf("Implied AIME (reverse-engineered): %.0f\n", implied_aime))
  cat(sprintf("AIME difference (ours - implied): %d\n", our_aime - round(implied_aime)))
}

# Show difference in PIA from each AIME approach
cat("\n=== PIA from Each AIME Approach ===\n")
for (method_aime in c(
  floor(top35_ours / months),
  floor(top35_earn_rounded / months),
  floor(top35_cent_rounded / months),
  floor(top35_both / months)
)) {
  pia <- 0.90 * min(method_aime, bp1) +
    0.32 * min(max(method_aime - bp1, 0), bp2 - bp1) +
    0.15 * max(method_aime - bp2, 0)
  pia <- floor(pia * 10) / 10  # Truncate to dime
  cat(sprintf("AIME=%d -> PIA=%.1f -> monthly@65=%.2f -> annual=%.0f\n",
              method_aime, pia, pia * factor_65, pia * factor_65 * 12))
}

# Also check: do SSA's published earnings for hypothetical workers match ours?
cat("\n=== Earnings Comparison: Selected Years ===\n")
cat("(SSA Actuarial Note Table 4 shows earnings for hypothetical workers)\n")
cat(sprintf("%-4s %-6s %-10s %-14s %-14s %-10s\n",
            "Age", "Year", "Factor", "AWI", "Our_Earnings", "Rounded$1"))
for (age in c(21, 30, 40, 50, 60, 64)) {
  r <- work[work$age == age, ]
  if (nrow(r) == 1) {
    fac <- sef2025$factor[sef2025$worker == "medium" & sef2025$age == age]
    cat(sprintf("%-4d %-6d %-10.6f %-14.2f %-14.2f %-10.0f\n",
                age, r$year, fac, r$awi, r$earnings, round(r$earnings, 0)))
  }
}

# Check how much difference rounding earnings to the dollar makes for TOTAL earnings
total_earn_unrounded <- sum(work$earnings)
total_earn_rounded <- sum(round(work$earnings, 0))
cat(sprintf("\nTotal lifetime earnings (unrounded): $%.2f\n", total_earn_unrounded))
cat(sprintf("Total lifetime earnings (rounded $1): $%.0f\n", total_earn_rounded))
cat(sprintf("Difference: $%.2f\n", total_earn_unrounded - total_earn_rounded))
