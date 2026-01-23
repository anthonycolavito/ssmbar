# Test duplicate IDs
library(devtools)
load_all()

cat("Testing 2 workers with SAME configuration (duplicate IDs):\n")
tryCatch({
  result <- calculate_benefits(
    birth_yr = c(1966, 1966),  # Same birth year
    sex = c("male", "male"),    # Same sex
    type = c("low", "low"),     # Same type
    age_claim = c(63, 63),      # Same claim age
    factors = sef2025,
    assumptions = tr2025
  )
  cat("Result rows:", nrow(result), "\n")
  cat("Unique IDs:", length(unique(result$id)), "\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})

cat("\nTesting 2 workers with DIFFERENT configurations:\n")
tryCatch({
  result <- calculate_benefits(
    birth_yr = c(1966, 1970),
    sex = c("male", "female"),
    type = c("low", "high"),
    age_claim = c(63, 67),
    factors = sef2025,
    assumptions = tr2025
  )
  cat("Result rows:", nrow(result), "\n")
  cat("Unique IDs:", length(unique(result$id)), "\n")
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
})
