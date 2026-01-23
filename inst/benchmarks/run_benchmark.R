# Run benchmark script
library(devtools)
load_all()
source("inst/benchmarks/benchmark_scaling.R")

cat("\n=== Pipeline Benchmark (50 workers) ===\n")
pipeline_times <- benchmark_pipeline(50)

cat("\n=== Scaling Benchmark ===\n")
results <- run_benchmarks(worker_counts = c(10, 25, 50), repetitions = 2)
