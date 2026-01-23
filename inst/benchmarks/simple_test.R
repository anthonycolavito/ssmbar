# Simple benchmark test
library(devtools)
load_all()

cat("Testing single worker...\n")
result1 <- calculate_benefits(
  birth_yr = 1960,
  sex = "male",
  type = "medium",
  age_claim = 67,
  factors = sef2025,
  assumptions = tr2025
)
cat("Single worker: OK, rows =", nrow(result1), "\n")

cat("\nTesting 3 workers...\n")
result3 <- calculate_benefits(
  birth_yr = c(1960, 1970, 1980),
  sex = "male",
  type = c("low", "medium", "high"),
  age_claim = c(62, 67, 70),
  factors = sef2025,
  assumptions = tr2025
)
cat("3 workers: OK, rows =", nrow(result3), "\n")

cat("\nTiming 10 workers...\n")
start <- Sys.time()
result10 <- calculate_benefits(
  birth_yr = sample(1960:1980, 10, replace = TRUE),
  sex = sample(c("male", "female"), 10, replace = TRUE),
  type = sample(c("low", "medium", "high"), 10, replace = TRUE),
  age_claim = sample(62:70, 10, replace = TRUE),
  factors = sef2025,
  assumptions = tr2025
)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat("10 workers: OK, rows =", nrow(result10), ", time =", round(elapsed, 2), "sec\n")

cat("\nTiming 25 workers...\n")
start <- Sys.time()
result25 <- calculate_benefits(
  birth_yr = sample(1960:1980, 25, replace = TRUE),
  sex = sample(c("male", "female"), 25, replace = TRUE),
  type = sample(c("low", "medium", "high"), 25, replace = TRUE),
  age_claim = sample(62:70, 25, replace = TRUE),
  factors = sef2025,
  assumptions = tr2025
)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat("25 workers: OK, rows =", nrow(result25), ", time =", round(elapsed, 2), "sec\n")
