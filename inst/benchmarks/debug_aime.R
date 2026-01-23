# Debug the aime issue
library(devtools)
load_all()

# Test the specific failing case
cat("Testing problematic worker: medium-female-1983-69\n")

# Generate a single worker with the problematic config
tryCatch({
  result <- calculate_benefits(
    birth_yr = 1983,
    sex = "female",
    type = "medium",
    age_claim = 69,
    factors = sef2025,
    assumptions = tr2025
  )
  cat("Worker 1983-69: OK\n")
}, error = function(e) {
  cat("Error:", conditionMessage(e), "\n")
})

# Check assumptions data for duplicates
cat("\nChecking assumptions for duplicate years:\n")
yr_counts <- table(tr2025$year)
dups <- yr_counts[yr_counts > 1]
if (length(dups) > 0) {
  cat("Found duplicate years:\n")
  print(dups)
} else {
  cat("No duplicate years in assumptions\n")
}

# Test with birth years that definitely have AWI data
cat("\nTesting with safe birth year range (1955-1975):\n")
set.seed(42)
result <- calculate_benefits(
  birth_yr = sample(1955:1975, 50, replace = TRUE),
  sex = sample(c("male", "female"), 50, replace = TRUE),
  type = sample(c("low", "medium", "high"), 50, replace = TRUE),
  age_claim = sample(62:70, 50, replace = TRUE),
  factors = sef2025,
  assumptions = tr2025
)
cat("50 workers (1955-1975): OK, rows =", nrow(result), "\n")
