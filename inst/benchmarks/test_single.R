# Test single worker
library(devtools)
load_all()
result <- calculate_benefits(1966, "male", "low", 63, sef2025, tr2025)
cat("low-male-1966-63: OK, rows =", nrow(result), "\n")
