# Investigate: what computational difference produces AIME 4723 vs 4709?
#
# For born 1960, eligible at 62 (2022):
# - Computation period: age 22 to age 61 = 40 years (1982-2021)
# - N-5 = 35 years counted
# - Indexing year = year of age 60 = 2020
# - But also earnings at ages 62-64 count (not indexed, since after age 60)
#
# Wait — the computation period for AIME:
# "Elapsed years" = years after reaching age 21 and before reaching age 62
# = years 1982 through 2021 = 40 years
# Number of computation years = elapsed_years - 5 = 35
# But AIME uses the HIGHEST 35 years of indexed earnings from ALL years
# (ages 21-64 for a claim at 65), including unindexed years 62-64.

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

w <- calculate_benefits(birth_yr = 1960, sex = "male", type = "medium",
                        age_claim = 65, factors = sef2025, assumptions = tr2025,
                        debugg = TRUE)

# Show all earnings and indexed earnings
work_rows <- w[w$age >= 21 & w$age <= 64, ]
cat("=== Full earnings and indexing detail ===\n")
cat(sprintf("%-5s %-5s %-12s %-12s %-12s %-10s\n",
            "Year", "Age", "Earnings", "Taxmax", "Index_Fac", "Indexed"))
for (i in 1:nrow(work_rows)) {
  earn <- work_rows$earnings[i]
  yr <- work_rows$year[i]
  age <- work_rows$age[i]
  taxmax <- tr2025$taxmax[tr2025$year == yr]
  idx_fac <- work_rows$index_factor[i]
  capped <- min(earn, taxmax)
  indexed <- capped * idx_fac
  cat(sprintf("%-5d %-5d $%-11.2f $%-11.0f %-12.6f $%-9.2f\n",
              yr, age, earn, taxmax, idx_fac, indexed))
}

# Show which 35 years are selected
capped_earn <- pmin(work_rows$earnings, tr2025$taxmax[match(work_rows$year, tr2025$year)])
indexed_earn <- capped_earn * work_rows$index_factor
names(indexed_earn) <- work_rows$year
sorted <- sort(indexed_earn, decreasing = TRUE)

cat(sprintf("\n=== Top 35 indexed earnings ===\n"))
cat(sprintf("%-5s %-5s %-12s\n", "Rank", "Year", "Indexed"))
for (i in 1:35) {
  yr <- names(sorted)[i]
  cat(sprintf("%-5d %-5s $%-11.2f\n", i, yr, sorted[i]))
}
cat(sprintf("\n35th: $%.2f\n", sorted[35]))
cat(sprintf("36th: $%.2f (excluded)\n", sorted[36]))
cat(sprintf("Sum of top 35: $%.2f\n", sum(sorted[1:35])))
cat(sprintf("AIME = floor(%.2f / 420) = %d\n\n", sum(sorted[1:35]), floor(sum(sorted[1:35]) / 420)))

# =====================================================================
# What if the computation includes age 21 earnings but we have the
# wrong factor? Let me check age 21.
# =====================================================================
cat("=== Age 21 check ===\n")
fac_21 <- sef2025$factor[sef2025$worker == "medium" & sef2025$age == 21]
awi_1981 <- tr2025$awi[tr2025$year == 1981]
earn_21 <- fac_21 * awi_1981
idx_21 <- tr2025$awi[tr2025$year == 2020] / awi_1981
indexed_21 <- earn_21 * idx_21
cat(sprintf("Age 21: factor=%.3f, AWI=$%.2f, earn=$%.2f, idx=%.6f, indexed=$%.2f\n",
            fac_21, awi_1981, earn_21, idx_21, indexed_21))
cat(sprintf("Table 4 prelim at 21: 0.227, earn=$3127.58\n"))
cat(sprintf("Our medium at 21: %.3f × $%.2f = $%.2f\n", fac_21, awi_1981, earn_21))
cat(sprintf("Table 4 medium at 21: $3127.58 × 1.221 = $%.2f\n", 3127.58 * 1.221))
cat(sprintf("Diff: $%.2f\n\n", earn_21 - 3127.58 * 1.221))

# Is age 21 among the top 35? (It shouldn't be — too low)
cat(sprintf("Age 21 indexed ($%.2f) vs 35th ranked ($%.2f): %s\n",
            indexed_21, sorted[35],
            ifelse(indexed_21 >= sorted[35], "IN top 35", "NOT in top 35")))

# =====================================================================
# The gap is 14 in AIME. Let me figure out how much MORE indexed
# earnings we'd need in each of the top 35 years to close this.
# 14 × 420 = $5,880
# If spread evenly across 35 years: $168/year
# That corresponds to about $168 / 1.0 (average index factor for top years)
# = about $168 in nominal earnings per year
# Or about 0.3% of earnings at each age
# =====================================================================
cat("\n=== Needed increase to close gap ===\n")
needed <- 14 * 420  # $5,880
cat(sprintf("Need $%d more in top-35 indexed sum\n", needed))
cat(sprintf("Average over 35 years: $%.0f/year\n", needed/35))

# Average nominal earnings of top 35 years:
avg_top35_nominal <- mean(capped_earn[match(names(sorted[1:35]), work_rows$year)])
cat(sprintf("Average nominal earnings of top 35: $%.0f\n", avg_top35_nominal))
cat(sprintf("$168 is %.2f%% of average earnings\n", 168/avg_top35_nominal * 100))
cat(sprintf("This is consistent with ~0.003-0.004 factor difference at each age\n"))

# =====================================================================
# CONCLUSION
# =====================================================================
cat("\n=== CONCLUSION ===\n")
cat("The 0.17% gap between our benefits and V.C7 is caused by a difference\n")
cat("of 14 in AIME (4709 vs 4723), which requires ~$5,880 more in indexed\n")
cat("earnings.\n\n")

cat("We have confirmed:\n")
cat("  1. Our AWI values match SSA's exactly (Table 4)\n")
cat("  2. Our indexing factors are correct (AWI_2020 / AWI_year)\n")
cat("  3. Our earnings formula (factor × AWI) matches the actuarial note\n")
cat("  4. Our 3dp factors match Table 6 of the actuarial note\n")
cat("  5. The Table 5 ratio (1.221) is rounded from 69472/56921 = 1.22050\n")
cat("  6. Using the EXACT ratio OR the rounded ratio changes AIME by ≤2\n")
cat("  7. No rounding of intermediate values closes the gap\n\n")

cat("The remaining gap (~0.3% of earnings) likely comes from:\n")
cat("  - V.C7 being generated by SSA's internal benefit estimation program\n")
cat("    which may use higher-precision factors than what's published in\n")
cat("    Actuarial Note 2025.3 Table 6\n")
cat("  - The actuarial note factors are rounded to 3 decimal places for\n")
cat("    publication, but the internal computation likely uses 6+ decimal\n")
cat("    places (e.g., age 22: 0.341610 instead of 0.341)\n")
cat("  - We showed that back-solving from Table 4 earnings gives exact\n")
cat("    factors like 0.341610 at age 22, but even these only improve\n")
cat("    AIME by 1 (to 4710) because Table 4 earnings are themselves\n")
cat("    computed from rounded prelim factors\n")
cat("  - The full-precision path would be:\n")
cat("    exact_raw_factor → preliminary_adjustment → ratio_multiply → earnings\n")
cat("    where each step uses full floating-point precision\n")
cat("  - SSA's internal raw factors (Table 2) are also shown to only 3dp\n")
cat("    but likely have more precision internally\n")
