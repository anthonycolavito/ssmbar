# Investigate scaled earnings factors
library(devtools)
load_all()

cat("=============================================================================\n")
cat("SCALED EARNINGS FACTORS INVESTIGATION\n")
cat("=============================================================================\n\n")

# The issue: our AIMEs are higher than V.C7 implies for non-medium earners
# This suggests our scaled earnings factors might be different

cat("=== MEDIUM EARNER (MATCHES V.C7) ===\n")
med_factors <- sef2025[sef2025$worker == "medium", ]
cat("Medium earner factor at age 50:", med_factors$factor[med_factors$age == 50], "\n")
cat("Medium earner factor at age 60:", med_factors$factor[med_factors$age == 60], "\n")

# Calculate total indexed earnings for medium
result_med <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)
sum_indexed_med <- sum(head(sort(result_med$indexed_earn[result_med$age <= 64], decreasing = TRUE), 35))
cat("Sum of top 35 indexed earnings:", round(sum_indexed_med, 0), "\n")
cat("AIME at 65:", result_med$aime[result_med$age == 65], "\n\n")

cat("=== VERY LOW EARNER (OFF BY 3.76%) ===\n")
vlow_factors <- sef2025[sef2025$worker == "very_low", ]
cat("Very low earner factor at age 50:", vlow_factors$factor[vlow_factors$age == 50], "\n")
cat("Very low earner factor at age 60:", vlow_factors$factor[vlow_factors$age == 60], "\n")

# Ratio of very_low to medium factors
ratio_50 <- vlow_factors$factor[vlow_factors$age == 50] / med_factors$factor[med_factors$age == 50]
ratio_60 <- vlow_factors$factor[vlow_factors$age == 60] / med_factors$factor[med_factors$age == 60]
cat("Ratio (very_low/medium) at age 50:", round(ratio_50, 4), "\n")
cat("Ratio (very_low/medium) at age 60:", round(ratio_60, 4), "\n")

# Calculate for very_low
result_vlow <- calculate_benefits(
  birth_yr = 1960, type = "very_low", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)
sum_indexed_vlow <- sum(head(sort(result_vlow$indexed_earn[result_vlow$age <= 64], decreasing = TRUE), 35))
cat("Sum of top 35 indexed earnings:", round(sum_indexed_vlow, 0), "\n")
cat("AIME at 65:", result_vlow$aime[result_vlow$age == 65], "\n")

# What ratio of AIME would match V.C7?
# V.C7 implies AIME of about 1086 for very_low
# Our AIME is 1177
# Medium AIME is 4709
# Expected ratio if very_low follows same pattern: 1086/4709 = 0.2306
# Actual ratio: 1177/4709 = 0.2499
cat("\nExpected very_low/medium AIME ratio (from V.C7):", round(1086/4709, 4), "\n")
cat("Actual very_low/medium AIME ratio:", round(1177/4709, 4), "\n")

cat("\n\n=== HIGH EARNER (OFF BY 3.59%) ===\n")
high_factors <- sef2025[sef2025$worker == "high", ]
cat("High earner factor at age 50:", high_factors$factor[high_factors$age == 50], "\n")
cat("High earner factor at age 60:", high_factors$factor[high_factors$age == 60], "\n")

result_high <- calculate_benefits(
  birth_yr = 1960, type = "high", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)
cat("AIME at 65:", result_high$aime[result_high$age == 65], "\n")

# What does V.C7 imply for high earner?
# V.C7 annual = 38574, monthly = 3214.5, basic_pia = 3214.5/1.149829 = 2795.63
# For bp1=1115, bp2=6721:
# PIA = 0.9*1115 + 0.32*(6721-1115) + 0.15*(AIME-6721) if AIME > 6721
# 2795.63 = 1003.5 + 1793.92 + 0.15*(AIME-6721)
# -1.79 = 0.15*(AIME-6721) -- this gives negative, so AIME must be < 6721
# PIA = 1003.5 + 0.32*(AIME-1115) = 2795.63
# AIME = 1115 + (2795.63-1003.5)/0.32 = 1115 + 5600.4 = 6715.4

cat("V.C7 implies high earner AIME:", round(1115 + (2795.63-1003.5)/0.32, 0), "\n")
cat("Our high earner AIME:", result_high$aime[result_high$age == 65], "\n")

cat("\n\n=== PATTERN ANALYSIS ===\n")
cat("For medium earner, our calculation matches V.C7 perfectly.\n")
cat("For other earner types, our AIMEs are consistently HIGHER.\n")
cat("This suggests the scaled earnings FACTORS might differ from SSA's.\n\n")

cat("=== CHECKING FACTOR SOURCE ===\n")
cat("The scaled earnings factors should come from the Trustees Report.\n")
cat("Let's check the factor values at key ages for all types:\n\n")

cat(sprintf("%-10s %10s %10s %10s\n", "Type", "Age 40", "Age 50", "Age 60"))
cat(paste(rep("-", 45), collapse = ""), "\n")
for (type in c("very_low", "low", "medium", "high", "max")) {
  factors <- sef2025[sef2025$worker == type, ]
  f40 <- factors$factor[factors$age == 40]
  f50 <- factors$factor[factors$age == 50]
  f60 <- factors$factor[factors$age == 60]
  cat(sprintf("%-10s %10.4f %10.4f %10.4f\n", type, f40, f50, f60))
}
