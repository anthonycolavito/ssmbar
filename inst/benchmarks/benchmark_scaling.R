# =============================================================================
# Benchmark Script: Performance Scaling Analysis
# =============================================================================
#
# This script benchmarks the calculate_benefits() function with varying numbers
# of workers to understand performance scaling characteristics.
#
# Usage:
#   source("inst/benchmarks/benchmark_scaling.R")
#   results <- run_benchmarks()
#   print(results)
#
# =============================================================================

library(devtools)
load_all()

#' Run Scaling Benchmarks
#'
#' Tests calculate_benefits() performance with 10, 100, 500, and 1000 workers.
#'
#' @param worker_counts Vector of worker counts to test. Default: c(10, 100, 500, 1000)
#' @param repetitions Number of repetitions per test. Default: 3
#' @param verbose Print progress messages. Default: TRUE
#'
#' @return Data frame with benchmark results
#'
#' @examples
#' \dontrun{
#' results <- run_benchmarks()
#' print(results)
#' }
run_benchmarks <- function(worker_counts = c(10, 100, 500, 1000),
                           repetitions = 3,
                           verbose = TRUE) {

  results <- data.frame(
    n_workers = integer(),
    rep = integer(),
    elapsed_sec = numeric(),
    stringsAsFactors = FALSE
  )

  for (n in worker_counts) {
    if (verbose) cat(sprintf("Benchmarking %d workers...\n", n))

    # Generate varied worker configurations
    birth_years <- sample(1950:1990, n, replace = TRUE)
    types <- sample(c("low", "medium", "high"), n, replace = TRUE)
    claim_ages <- sample(62:70, n, replace = TRUE)
    sexes <- sample(c("male", "female"), n, replace = TRUE)

    for (r in seq_len(repetitions)) {
      if (verbose) cat(sprintf("  Rep %d/%d... ", r, repetitions))

      start_time <- Sys.time()

      result <- calculate_benefits(
        birth_yr = birth_years,
        sex = sexes,
        type = types,
        age_claim = claim_ages,
        factors = sef2025,
        assumptions = tr2025,
        debugg = FALSE
      )

      end_time <- Sys.time()
      elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))

      if (verbose) cat(sprintf("%.2f sec\n", elapsed))

      results <- rbind(results, data.frame(
        n_workers = n,
        rep = r,
        elapsed_sec = elapsed
      ))
    }
  }

  # Calculate summary statistics
  summary_results <- aggregate(
    elapsed_sec ~ n_workers,
    data = results,
    FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
  )

  # Flatten the matrix column
  summary_df <- data.frame(
    n_workers = summary_results$n_workers,
    mean_sec = summary_results$elapsed_sec[, "mean"],
    sd_sec = summary_results$elapsed_sec[, "sd"],
    min_sec = summary_results$elapsed_sec[, "min"],
    max_sec = summary_results$elapsed_sec[, "max"]
  )

  # Calculate scaling factor (time per worker)
  summary_df$sec_per_worker <- summary_df$mean_sec / summary_df$n_workers

  # Calculate scaling exponent (log-log slope)
  if (nrow(summary_df) >= 2) {
    log_n <- log(summary_df$n_workers)
    log_t <- log(summary_df$mean_sec)
    scaling_fit <- lm(log_t ~ log_n)
    scaling_exponent <- coef(scaling_fit)[2]
    attr(summary_df, "scaling_exponent") <- scaling_exponent
  }

  if (verbose) {
    cat("\n=== Benchmark Summary ===\n")
    print(summary_df)
    if (!is.null(attr(summary_df, "scaling_exponent"))) {
      cat(sprintf("\nScaling exponent: %.2f (1.0 = linear, 2.0 = quadratic)\n",
                  attr(summary_df, "scaling_exponent")))
    }
  }

  return(summary_df)
}


#' Profile AIME Calculation
#'
#' Runs profvis on the aime() function to identify bottlenecks.
#' Requires the profvis package to be installed.
#'
#' @param n_workers Number of workers to profile. Default: 100
#'
#' @return profvis object (if profvis is installed)
#'
#' @examples
#' \dontrun{
#' prof <- profile_aime(100)
#' print(prof)
#' }
profile_aime <- function(n_workers = 100) {
  if (!requireNamespace("profvis", quietly = TRUE)) {
    stop("profvis package required for profiling. Install with: install.packages('profvis')")
  }

  # Generate test workers
  birth_years <- sample(1960:1980, n_workers, replace = TRUE)
  types <- sample(c("low", "medium", "high"), n_workers, replace = TRUE)
  claim_ages <- rep(67, n_workers)
  sexes <- sample(c("male", "female"), n_workers, replace = TRUE)

  # Generate earnings first (not profiled)
  cat("Generating earnings data...\n")
  workers <- earnings_generator(
    birth_yr = birth_years,
    sex = sexes,
    type = types,
    age_claim = claim_ages,
    age_elig = 62,
    factors = sef2025,
    assumptions = tr2025
  )

  cat(sprintf("Profiling aime() with %d workers...\n", n_workers))

  prof <- profvis::profvis({
    result <- aime(workers, tr2025, debugg = FALSE)
  })

  return(prof)
}


#' Benchmark Individual Pipeline Steps
#'
#' Measures time taken by each step in the benefit calculation pipeline.
#'
#' @param n_workers Number of workers to benchmark. Default: 100
#'
#' @return Data frame with timing for each pipeline step
#'
#' @examples
#' \dontrun{
#' pipeline_times <- benchmark_pipeline(100)
#' print(pipeline_times)
#' }
benchmark_pipeline <- function(n_workers = 100) {

  # Generate test workers
  birth_years <- sample(1960:1980, n_workers, replace = TRUE)
  types <- sample(c("low", "medium", "high"), n_workers, replace = TRUE)
  claim_ages <- rep(67, n_workers)
  sexes <- sample(c("male", "female"), n_workers, replace = TRUE)

  timings <- list()

  # Step 1: Generate earnings
  start <- Sys.time()
  workers <- earnings_generator(
    birth_yr = birth_years,
    sex = sexes,
    type = types,
    age_claim = claim_ages,
    age_elig = 62,
    factors = sef2025,
    assumptions = tr2025
  )
  timings$earnings <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Step 2: AIME
  start <- Sys.time()
  workers <- aime(workers, tr2025, debugg = FALSE)
  timings$aime <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Step 3: PIA
  start <- Sys.time()
  workers <- pia(workers, tr2025, debugg = FALSE)
  timings$pia <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Step 4: COLA
  start <- Sys.time()
  workers <- cola(workers, tr2025, debugg = FALSE)
  timings$cola <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Step 5: Worker benefit
  start <- Sys.time()
  workers <- worker_benefit(workers, tr2025, debugg = FALSE)
  timings$worker_benefit <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Step 6: Final benefit (spousal steps skipped for single workers)
  start <- Sys.time()
  workers <- final_benefit(workers, debugg = FALSE)
  timings$final_benefit <- as.numeric(difftime(Sys.time(), start, units = "secs"))

  # Create results data frame
  results <- data.frame(
    step = names(timings),
    elapsed_sec = unlist(timings),
    row.names = NULL
  )

  results$pct_total <- 100 * results$elapsed_sec / sum(results$elapsed_sec)

  cat(sprintf("\n=== Pipeline Benchmark (%d workers) ===\n", n_workers))
  print(results)
  cat(sprintf("\nTotal: %.2f sec\n", sum(results$elapsed_sec)))

  return(results)
}


# =============================================================================
# Main execution (when sourced directly)
# =============================================================================

if (interactive()) {
  cat("Benchmark functions loaded. Available functions:\n")
  cat("  - run_benchmarks(): Test scaling with 10-1000 workers\n")
  cat("  - benchmark_pipeline(): Time each pipeline step\n")
  cat("  - profile_aime(): Profile AIME calculation (requires profvis)\n")
}
