# Deeper debug of AIME calculation
library(devtools)
load_all()

# Calculate for 1960 medium earner with debug
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Check the structure around ages 60-62
cat("Data around ages 60-62:\n")
check_ages <- result[result$age >= 60 & result$age <= 62,
                     c("age", "year", "earnings", "capped_earn", "indexed_earn", "aime")]
print(check_ages)

# Check if age 62 has earnings (it shouldn't be included in AIME for age 62)
cat("\n\nAge 62 details:\n")
age62 <- result[result$age == 62, ]
cat("Has indexed_earn:", age62$indexed_earn, "\n")
cat("AIME:", age62$aime, "\n")

# Manually trace through what aime() function does
# At age 62, it takes earnings_subset = indexed_earnings[1:i]
# Let's replicate this

cat("\n\n=== Replicating aime() logic ===\n")
# Get indexed earnings for ages up to and including 62
indexed_earn_vec <- result$indexed_earn[1:(which(result$age == 62))]
cat("Length of indexed_earn vector at age 62:", length(indexed_earn_vec), "\n")

# comp_period at age 62
comp_period <- 35
years_to_use <- min(length(indexed_earn_vec), comp_period)
cat("years_to_use:", years_to_use, "\n")

# Take top years_to_use earnings
top_earnings <- -sort(-indexed_earn_vec, partial = 1:years_to_use)[1:years_to_use]
cat("Sum of top", years_to_use, "earnings:", sum(top_earnings), "\n")
cat("AIME calculation:", floor(sum(top_earnings) / 420), "\n")

# Check what ages are included
cat("\n\nAges included in indexed_earn_vec:\n")
ages_included <- result$age[1:(which(result$age == 62))]
cat("Min age:", min(ages_included), "Max age:", max(ages_included), "Count:", length(ages_included), "\n")

# Check for any ages before 21
cat("Ages before 21 included:", sum(ages_included < 21), "\n")
