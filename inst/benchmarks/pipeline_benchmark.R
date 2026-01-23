# Pipeline benchmark - identify which steps take the most time
library(devtools)
load_all()

set.seed(123)
n_workers <- 50

# Generate unique worker configurations
configs <- expand.grid(
  type = c("low", "medium", "high"),
  sex = c("male", "female"),
  birth_yr = 1950:1980,
  claim_age = 62:70,
  stringsAsFactors = FALSE
)
sampled_idx <- sample(nrow(configs), n_workers)
config <- configs[sampled_idx, ]

timings <- list()

# Step 1: Generate earnings
cat("Step 1: earnings_generator...\n")
start <- Sys.time()
workers <- earnings_generator(
  birth_yr = config$birth_yr,
  sex = config$sex,
  type = config$type,
  age_claim = config$claim_age,
  age_elig = 62,
  factors = sef2025,
  assumptions = tr2025
)
timings$earnings <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$earnings))

# Step 2: AIME
cat("Step 2: aime...\n")
start <- Sys.time()
workers <- aime(workers, tr2025, debugg = FALSE)
timings$aime <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$aime))

# Step 3: PIA
cat("Step 3: pia...\n")
start <- Sys.time()
workers <- pia(workers, tr2025, debugg = FALSE)
timings$pia <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$pia))

# Step 4: COLA
cat("Step 4: cola...\n")
start <- Sys.time()
workers <- cola(workers, tr2025, debugg = FALSE)
timings$cola <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$cola))

# Step 5: Worker benefit
cat("Step 5: worker_benefit...\n")
start <- Sys.time()
workers <- worker_benefit(workers, tr2025, debugg = FALSE)
timings$worker_benefit <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$worker_benefit))

# Step 6: Spousal PIA (no spouse for these workers)
cat("Step 6: spousal_pia...\n")
start <- Sys.time()
workers <- spousal_pia(workers, spouse_data = NULL, tr2025, factors = sef2025, debugg = FALSE)
timings$spousal_pia <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$spousal_pia))

# Step 7: Spouse benefit
cat("Step 7: spouse_benefit...\n")
start <- Sys.time()
workers <- spouse_benefit(workers, spouse_data = NULL, tr2025, debugg = FALSE)
timings$spouse_benefit <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$spouse_benefit))

# Step 8: RET
cat("Step 8: ret...\n")
start <- Sys.time()
workers <- ret(workers, tr2025, spouse_data = NULL, factors = sef2025, debugg = FALSE)
timings$ret <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$ret))

# Step 9: Final benefit
cat("Step 9: final_benefit...\n")
start <- Sys.time()
workers <- final_benefit(workers, debugg = FALSE)
timings$final_benefit <- as.numeric(difftime(Sys.time(), start, units = "secs"))
cat(sprintf("  %.3f sec\n", timings$final_benefit))

# Summary
results <- data.frame(
  step = names(timings),
  elapsed_sec = unlist(timings),
  row.names = NULL
)
results$pct_total <- 100 * results$elapsed_sec / sum(results$elapsed_sec)

cat(sprintf("\n=== Pipeline Benchmark Summary (%d workers) ===\n", n_workers))
print(results)
cat(sprintf("\nTotal: %.2f sec\n", sum(results$elapsed_sec)))

# Save for comparison
saveRDS(results, "inst/benchmarks/pipeline_baseline.rds")
