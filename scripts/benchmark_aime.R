# Benchmark AIME calculation performance
# Tests performance across different worker counts

devtools::load_all()
data(tr2025)
data(sef2025)

cat("=== AIME Performance Benchmark ===\n\n")

# Helper function to time calculate_benefits
benchmark_calculate <- function(n_workers, birth_years = NULL, types = NULL) {
  if (is.null(birth_years)) birth_years <- sample(1960:1990, n_workers, replace = TRUE)
  if (is.null(types)) types <- sample(c("very_low", "low", "medium", "high", "max"), n_workers, replace = TRUE)

  start_time <- Sys.time()

  for (i in seq_len(n_workers)) {
    calculate_benefits(
      birth_yr = birth_years[i],
      sex = "male",
      type = types[i],
      age_claim = 67,
      factors = sef2025,
      assumptions = tr2025,
      debugg = FALSE
    )
  }

  end_time <- Sys.time()
  as.numeric(difftime(end_time, start_time, units = "secs"))
}

# Test different worker counts
worker_counts <- c(10, 25, 50, 100)
results <- data.frame()

cat("Running benchmarks...\n\n")

for (n in worker_counts) {
  cat(sprintf("Testing %d workers: ", n))

  # Run 3 trials and take median
  times <- replicate(3, benchmark_calculate(n))
  median_time <- median(times)
  time_per_worker <- median_time / n * 1000  # ms per worker

  cat(sprintf("%.2f sec total, %.1f ms/worker\n", median_time, time_per_worker))

  results <- rbind(results, data.frame(
    n_workers = n,
    total_sec = round(median_time, 3),
    ms_per_worker = round(time_per_worker, 1)
  ))
}

cat("\n--- Summary ---\n")
print(results)

# Check scaling
cat("\n--- Scaling Analysis ---\n")
if (nrow(results) >= 2) {
  scaling_factor <- results$total_sec[nrow(results)] / results$total_sec[1]
  expected_linear <- results$n_workers[nrow(results)] / results$n_workers[1]
  cat(sprintf("Actual scaling from %d to %d workers: %.2fx\n",
              results$n_workers[1], results$n_workers[nrow(results)], scaling_factor))
  cat(sprintf("Expected linear scaling: %.2fx\n", expected_linear))
  cat(sprintf("Efficiency: %.1f%% (100%% = perfect linear)\n",
              (expected_linear / scaling_factor) * 100))
}

cat("\n=== Benchmark completed ===\n")
