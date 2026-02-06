# Compare our earnings to SSA's published hypothetical worker earnings
# The key question: do our scaled earnings factors × AWI match what SSA uses?
library(dplyr)
setwd("C:/Users/AnthonyColavito/ssmbar")
devtools::load_all(".")

# =====================================================================
# Check: what if SSA rounds factor * AWI to nearest CENT?
# =====================================================================
cat("=== Round factor * AWI to nearest cent ===\n")
w <- calculate_benefits(
  birth_yr = 1960, sex = "male", type = "medium", age_claim = 65,
  factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Get working-age data
work_ages <- 21:64
earn_raw <- numeric(length(work_ages))
earn_cent <- numeric(length(work_ages))
earn_dollar <- numeric(length(work_ages))

for (i in seq_along(work_ages)) {
  age <- work_ages[i]
  yr <- 1960 + age
  fac <- sef2025$factor[sef2025$worker == "medium" & sef2025$age == age]
  awi <- tr2025$awi[tr2025$year == yr]
  taxmax <- tr2025$taxmax[tr2025$year == yr]

  earn_raw[i] <- fac * awi
  earn_cent[i] <- round(fac * awi, 2)
  earn_dollar[i] <- round(fac * awi, 0)
}

# Cap at taxmax
taxmax_vals <- tr2025$taxmax[match(1960 + work_ages, tr2025$year)]
capped_raw <- pmin(earn_raw, taxmax_vals)
capped_cent <- pmin(earn_cent, taxmax_vals)
capped_dollar <- pmin(earn_dollar, taxmax_vals)

# Index
idx_fac <- w$index_factor[w$age >= 21 & w$age <= 64]
indexed_raw <- capped_raw * idx_fac
indexed_cent <- capped_cent * idx_fac
indexed_dollar <- capped_dollar * idx_fac

# Also try rounding indexed to cent
indexed_raw_then_cent <- round(indexed_raw, 2)
indexed_cent_then_cent <- round(indexed_cent, 2)
indexed_dollar_then_cent <- round(indexed_dollar, 2)

# Compute AIME for each approach
compute_aime <- function(indexed) {
  top35 <- sum(sort(indexed, decreasing = TRUE)[1:35])
  floor(top35 / 420)
}

cat(sprintf("%-35s %-8s %-14s\n", "Method", "AIME", "Top-35 Sum"))
cat(sprintf("%-35s %-8d %-14.2f\n", "Raw (no rounding)", compute_aime(indexed_raw), sum(sort(indexed_raw, decreasing=TRUE)[1:35])))
cat(sprintf("%-35s %-8d %-14.2f\n", "Earnings to cent", compute_aime(indexed_cent), sum(sort(indexed_cent, decreasing=TRUE)[1:35])))
cat(sprintf("%-35s %-8d %-14.2f\n", "Earnings to dollar", compute_aime(indexed_dollar), sum(sort(indexed_dollar, decreasing=TRUE)[1:35])))
cat(sprintf("%-35s %-8d %-14.2f\n", "Raw then indexed to cent", compute_aime(indexed_raw_then_cent), sum(sort(indexed_raw_then_cent, decreasing=TRUE)[1:35])))
cat(sprintf("%-35s %-8d %-14.2f\n", "Earn cent then indexed cent", compute_aime(indexed_cent_then_cent), sum(sort(indexed_cent_then_cent, decreasing=TRUE)[1:35])))
cat(sprintf("%-35s %-8d %-14.2f\n", "Earn dollar then indexed cent", compute_aime(indexed_dollar_then_cent), sum(sort(indexed_dollar_then_cent, decreasing=TRUE)[1:35])))

# Target AIME is ~4723
cat(sprintf("\nTarget AIME for V.C7 match: ~4723\n"))

# =====================================================================
# Check: what if the scaled factors have more decimal places?
# =====================================================================
cat("\n=== Check factor precision effect ===\n")
# SSA publishes factors to 4 decimal places in some tables
# What if the true factor has more precision?
# For example, if age-64 factor is 0.7864 instead of 0.786
# That's a difference of 0.0004 * AWI_2024 = 0.0004 * 69472.44 = $27.79

# Let's check: how much would we need to increase total indexed earnings
# to get AIME from 4709 to 4723?
# Need: (4723 * 420) - (4709 * 420) = 14 * 420 = $5,880 more in top-35 indexed sum
needed_increase <- (4723 - 4709) * 420
cat(sprintf("Need $%.0f more in top-35 indexed earnings sum\n", needed_increase))
cat(sprintf("That's $%.0f per year across 35 years\n", needed_increase / 35))

# =====================================================================
# What if V.C7 doesn't use age-65 claim? What if column is different?
# =====================================================================
cat("\n=== Check: Read V.C7 raw data to verify column ===\n")
library(readxl)
vc7_path <- "C:/Users/AnthonyColavito/instructions/VC7.xlsx"
raw_data <- read_excel(vc7_path, col_names = FALSE)

# Print all column values for the medium earner, year 2025
# Find medium earner section
med_idx <- which(grepl("Scaled medium", raw_data[[1]], ignore.case = TRUE))
cat(sprintf("Medium earner section starts at row %d\n", med_idx[1]))

# Find row for year 2025
for (i in (med_idx[1] + 1):min(med_idx[1] + 100, nrow(raw_data))) {
  val <- raw_data[[1]][i]
  if (!is.na(val) && as.character(val) == "2025") {
    cat(sprintf("\nRow %d (year 2025):\n", i))
    for (j in 1:ncol(raw_data)) {
      cat(sprintf("  Col %d: %s\n", j, as.character(raw_data[[j]][i])))
    }
    break
  }
}

# Also show the header row to understand column meanings
cat("\nColumn headers (rows near section start):\n")
for (i in max(1, med_idx[1]-5):med_idx[1]) {
  vals <- sapply(1:ncol(raw_data), function(j) {
    v <- raw_data[[j]][i]
    if (is.na(v)) "NA" else as.character(v)
  })
  cat(sprintf("  Row %d: %s\n", i, paste(vals, collapse=" | ")))
}

# Let's also look at the very top header rows
cat("\nTop header rows:\n")
for (i in 1:10) {
  vals <- sapply(1:ncol(raw_data), function(j) {
    v <- raw_data[[j]][i]
    if (is.na(v)) "NA" else as.character(v)
  })
  cat(sprintf("  Row %d: %s\n", i, paste(vals, collapse=" | ")))
}
