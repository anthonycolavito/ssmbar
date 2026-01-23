# Debug ID uniqueness issue
library(devtools)
load_all()

set.seed(42)
n <- 50

birth_yrs <- sample(1950:1975, n, replace = TRUE)
sexes <- sample(c("male", "female"), n, replace = TRUE)
types <- sample(c("low", "medium", "high"), n, replace = TRUE)
claims <- sample(62:70, n, replace = TRUE)

# Create IDs manually to check for duplicates
ids <- paste(types, sexes, birth_yrs, claims, sep = "-")
cat("Generated", length(ids), "workers\n")
cat("Unique IDs:", length(unique(ids)), "\n")

# Show duplicates if any
dup_ids <- ids[duplicated(ids)]
if (length(dup_ids) > 0) {
  cat("\nDuplicate IDs found:\n")
  print(table(ids[ids %in% dup_ids]))
  cat("\nThis is the source of the bug - multiple workers with same ID\n")
} else {
  cat("No duplicate IDs\n")
}
