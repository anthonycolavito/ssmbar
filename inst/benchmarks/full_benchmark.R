# Full benchmark test
library(devtools)
load_all()

set.seed(42)  # For reproducibility

run_timing <- function(n_workers, reps = 3) {
  times <- numeric(reps)

  for (r in 1:reps) {
    # Generate unique worker configurations to avoid duplicate ID bug
    # Use expand.grid to create unique combinations
    configs <- expand.grid(
      type = c("low", "medium", "high"),
      sex = c("male", "female"),
      birth_yr = 1950:1980,
      claim_age = 62:70,
      stringsAsFactors = FALSE
    )
    # Sample n_workers unique configurations
    sampled_idx <- sample(nrow(configs), min(n_workers, nrow(configs)))
    config <- configs[sampled_idx, ]

    start <- Sys.time()
    result <- calculate_benefits(
      birth_yr = config$birth_yr,
      sex = config$sex,
      type = config$type,
      age_claim = config$claim_age,
      factors = sef2025,
      assumptions = tr2025
    )
    times[r] <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    cat(sprintf("  n=%d rep=%d: %.2f sec\n", n_workers, r, times[r]))
  }

  return(c(mean = mean(times), sd = sd(times)))
}

cat("=== Baseline Benchmark Results ===\n\n")

worker_counts <- c(10, 25, 50, 100)
results <- data.frame(
  n_workers = worker_counts,
  mean_sec = numeric(length(worker_counts)),
  sd_sec = numeric(length(worker_counts))
)

for (i in seq_along(worker_counts)) {
  cat(sprintf("\nTesting %d workers:\n", worker_counts[i]))
  timing <- run_timing(worker_counts[i], reps = 3)
  results$mean_sec[i] <- timing["mean"]
  results$sd_sec[i] <- timing["sd"]
}

results$sec_per_worker <- results$mean_sec / results$n_workers

cat("\n\n=== Summary ===\n")
print(results)

# Calculate scaling exponent
log_n <- log(results$n_workers)
log_t <- log(results$mean_sec)
fit <- lm(log_t ~ log_n)
scaling_exp <- coef(fit)[2]
cat(sprintf("\nScaling exponent: %.2f (1.0=linear, 2.0=quadratic)\n", scaling_exp))

# Save results
saveRDS(results, "inst/benchmarks/baseline_results.rds")
cat("\nResults saved to inst/benchmarks/baseline_results.rds\n")
