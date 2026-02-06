# Final analysis: V.C7 implies AIME=4723 for medium born 1960
# Our AIME is 4709. Gap = 14.
# This requires 14 × 420 = $5,880 more in top-35 indexed earnings.
#
# Key question: Does AIME=4723 also match the NRA column of V.C7?

library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

floor_dime <- function(x) floor(x * 10) / 10

# Parameters
bp1 <- 1024  # 2022 bend points (eligibility year for born 1960)
bp2 <- 6172
cola_2022 <- 8.7
cola_2023 <- 3.2
cola_2024 <- 2.5
cola_2025 <- tr2025$cola[tr2025$year == 2025]
cola_2026 <- tr2025$cola[tr2025$year == 2026]
arf <- 1 - 24 * 5 / 900
cpi_2025 <- tr2025$cpi_w[tr2025$year == 2025]
cpi_2027 <- tr2025$cpi_w[tr2025$year == 2027]

cat(sprintf("COLAs: 2022=%.1f%%, 2023=%.1f%%, 2024=%.1f%%, 2025=%.1f%%, 2026=%.1f%%\n",
            cola_2022, cola_2023, cola_2024, cola_2025, cola_2026))
cat(sprintf("CPI deflator (2025/2027): %.6f\n\n", cpi_2025/cpi_2027))

# =====================================================================
# Test: Does AIME=4723 match BOTH V.C7 columns?
# =====================================================================
test_aime <- 4723
basic_pia <- floor_dime(0.9 * min(test_aime, bp1) + 0.32 * max(0, min(test_aime, bp2) - bp1))
cat(sprintf("AIME = %d -> basic_pia = %.1f\n", test_aime, basic_pia))

# COLA chain from 62 to 65
c62 <- basic_pia
c63 <- floor_dime(c62 * (1 + cola_2022/100))
c64 <- floor_dime(c63 * (1 + cola_2023/100))
c65 <- floor_dime(c64 * (1 + cola_2024/100))
cat(sprintf("COLA chain: 62=%.1f -> 63=%.1f -> 64=%.1f -> 65=%.1f\n", c62, c63, c64, c65))

# Age 65 benefit
monthly_65 <- floor_dime(c65 * arf)
annual_65 <- as.integer(monthly_65 * 12)
cat(sprintf("Age 65: monthly=%.1f, annual=%d (V.C7=25172) %s\n",
            monthly_65, annual_65, ifelse(annual_65 == 25172, "MATCH!", "")))

# NRA (67) benefit in 2025$
c66 <- floor_dime(c65 * (1 + cola_2025/100))
c67 <- floor_dime(c66 * (1 + cola_2026/100))
monthly_67 <- floor_dime(c67)  # at NRA, no reduction
annual_67_nominal <- as.integer(monthly_67 * 12)
annual_67_2025 <- round(monthly_67 * 12 * cpi_2025 / cpi_2027)
cat(sprintf("NRA (67): cola_pia=%.1f, monthly=%.1f, annual_nominal=%d\n",
            c67, monthly_67, annual_67_nominal))
cat(sprintf("  Annual in 2025$: %d (V.C7=29283) %s\n",
            annual_67_2025, ifelse(annual_67_2025 == 29283, "MATCH!", "")))

# If not matching at NRA, try alternative deflation
# V.C7 might use a slightly different CPI deflation
if (annual_67_2025 != 29283) {
  # What deflator produces 29283?
  # monthly_67 * 12 * deflator = 29283
  needed_deflator <- 29283 / (monthly_67 * 12)
  cat(sprintf("  Needed deflator: %.6f (ours=%.6f, diff=%.6f)\n",
              needed_deflator, cpi_2025/cpi_2027,
              needed_deflator - cpi_2025/cpi_2027))

  # What if V.C7 doesn't floor to dime at the NRA?
  # V.C7 annual = round(cola_pia_67 * 12 * deflator)
  alt_annual <- round(c67 * 12 * cpi_2025 / cpi_2027)
  cat(sprintf("  Without floor_dime at NRA: round(%.1f * 12 * %.6f) = %d\n",
              c67, cpi_2025/cpi_2027, alt_annual))
}

# =====================================================================
# Summary of the gap
# =====================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("V.C7 implies AIME = 4723 for medium earner born 1960\n"))
cat(sprintf("Our computation produces AIME = 4709\n"))
cat(sprintf("Gap = 14 AIME units = $5,880 in indexed earnings\n"))
cat(sprintf("= $168/year average over 35 years\n\n"))

cat("Confirmed NOT the source:\n")
cat("  - Factor × AWI rounding (changes AIME by 0-1)\n")
cat("  - Table 5 ratio precision (changes AIME by 0-1)\n")
cat("  - AWI values (match Table 4 exactly)\n")
cat("  - Indexing factors (correct per AWI series)\n")
cat("  - COLA application (correct chain)\n")
cat("  - Actuarial reduction factor (correct)\n\n")

# =====================================================================
# Could V.C7 use a different set of earnings years?
# Our computation: earnings at ages 21-64 (ages 21-64 only)
# What if V.C7 includes age 65 earnings too?
# Born Jan 2, 1960 → turns 65 on Jan 2, 2025
# If they work through age 65 (year 2025), that's an additional year
# =====================================================================
cat("=== What if V.C7 includes age 65 earnings? ===\n")
# If the worker has earnings at age 65:
# factor at 65 would be... Table 6 only goes to age 64!
# So no, the actuarial note defines earnings from 21 to 64.
cat("Table 6 scaled factors only defined for ages 21-64.\n")
cat("V.C7 workers earn from 21 to 64 (same as us).\n\n")

# =====================================================================
# Could V.C7 use the ROUNDED ratio (1.221) instead of exact?
# =====================================================================
cat("=== Effect of using 1.221 vs exact ratio ===\n")
# Table 4 prelim earnings
table4_earnings <- c(
  3127.58, 4065.56, 5447.48, 6889.79, 8097.01,
  9159.92, 10558.32, 11858.11, 13038.86, 14309.58,
  15471.56, 16857.69, 17518.53, 18474.93, 19655.90,
  21016.38, 22613.48, 24146.89, 25806.10, 27544.69,
  28476.67, 28989.64, 29932.39, 31532.07, 32847.32,
  34515.55, 36186.30, 37059.65, 36486.68, 37351.50,
  38444.14, 39442.83, 39704.03, 40792.27, 41776.26,
  41451.14, 41912.55, 42331.13, 42630.31, 42104.34,
  43337.34, 44072.11, 44442.72, 44751.21
)

years <- 1981:2024
idx_fac_vals <- sapply(years, function(yr) {
  awi_2020 <- tr2025$awi[tr2025$year == 2020]
  awi_yr <- tr2025$awi[tr2025$year == yr]
  if (yr <= 2020) awi_2020 / awi_yr else 1.0
})
taxmax_vals <- tr2025$taxmax[match(years, tr2025$year)]

compute_aime <- function(earnings) {
  capped <- pmin(earnings, taxmax_vals)
  indexed <- capped * idx_fac_vals
  top35 <- sort(indexed, decreasing = TRUE)[1:35]
  floor(sum(top35) / 420)
}

# Method: prelim × 1.221 (rounded ratio from Table 5)
earn_1221 <- table4_earnings * 1.221
aime_1221 <- compute_aime(earn_1221)

# Method: prelim × exact ratio
exact_ratio <- 69472 / 56921
earn_exact <- table4_earnings * exact_ratio
aime_exact <- compute_aime(earn_exact)

# Method: our 3dp factors × AWI
our_factors <- sef2025$factor[sef2025$worker == "medium"]
our_awi <- tr2025$awi[match(years, tr2025$year)]
earn_ours <- our_factors * our_awi
aime_ours <- compute_aime(earn_ours)

cat(sprintf("prelim × 1.221:      AIME = %d\n", aime_1221))
cat(sprintf("prelim × exact:      AIME = %d\n", aime_exact))
cat(sprintf("our 3dp × AWI:       AIME = %d\n", aime_ours))
cat(sprintf("V.C7 implied:        AIME = 4723\n"))
cat(sprintf("Gap (best case):     %d\n\n", 4723 - aime_1221))

# =====================================================================
# What if there's a DIFFERENT source of the +14?
# What if SSA's internal computation rounds INDEXED earnings to cents?
# Or rounds at some intermediate step we're not doing?
# =====================================================================
cat("=== Try: round indexed earnings to nearest cent ===\n")
earn_med <- table4_earnings * 1.221  # best AIME so far
capped <- pmin(earn_med, taxmax_vals)
indexed <- capped * idx_fac_vals
indexed_cent <- round(indexed, 2)

aime_raw_idx <- compute_aime(earn_med)
top35_raw <- sort(indexed, decreasing=TRUE)[1:35]
top35_cent <- sort(indexed_cent, decreasing=TRUE)[1:35]
aime_cent_idx <- floor(sum(top35_cent) / 420)

cat(sprintf("Raw indexed:     sum=%.2f, AIME=%d\n", sum(top35_raw), aime_raw_idx))
cat(sprintf("Cent indexed:    sum=%.2f, AIME=%d\n", sum(top35_cent), aime_cent_idx))

# Try: round nominal earnings to dollar BEFORE capping
earn_dollar <- round(earn_med, 0)
aime_dollar <- compute_aime(earn_dollar)
cat(sprintf("Dollar nominal:  AIME=%d\n", aime_dollar))

# Try: round nominal earnings to cent
earn_cent <- round(earn_med, 2)
aime_cent_earn <- compute_aime(earn_cent)
cat(sprintf("Cent nominal:    AIME=%d\n", aime_cent_earn))

# =====================================================================
# How about: what if SSA rounds factor × AWI BEFORE multiplying by ratio?
# i.e., earnings = round(prelim_factor × AWI) × ratio
# The table 4 values ARE round(prelim_factor × AWI, 2)
# So this is already captured.
# =====================================================================

# =====================================================================
# Final check: our earnings vs Table 4 × 1.221 — which years differ?
# =====================================================================
cat("\n=== Where are the biggest differences between approaches? ===\n")
diff_vs_1221 <- earn_1221 - earn_ours
cat(sprintf("%-4s %-14s %-14s %-10s %-14s\n", "Age", "Tbl4×1.221", "Our_3dp×AWI", "NomDiff", "Indexed_Diff"))
for (i in order(abs(diff_vs_1221), decreasing=TRUE)[1:10]) {
  cat(sprintf("%-4d $%-13.2f $%-13.2f $%-9.2f $%-13.2f\n",
              i+20, earn_1221[i], earn_ours[i], diff_vs_1221[i],
              diff_vs_1221[i] * idx_fac_vals[i]))
}

# =====================================================================
# Could the gap be in the taxmax cap?
# If SSA uses a slightly different taxmax for any year...
# =====================================================================
cat("\n=== Taxmax cap check ===\n")
# For medium earner, are any years capped?
capped_years <- which(earn_ours > taxmax_vals)
if (length(capped_years) > 0) {
  cat("Years where medium earner hits taxmax:\n")
  for (i in capped_years) {
    cat(sprintf("  Year %d (age %d): earn=$%.2f, taxmax=$%.2f\n",
                years[i], 20+i, earn_ours[i], taxmax_vals[i]))
  }
} else {
  cat("No years where medium earner hits taxmax.\n")
  cat("(This means the taxmax cap is not a factor in the gap.)\n")
}
