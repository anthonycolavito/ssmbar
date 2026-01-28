# Debug lag namespace issue
library(devtools)
load_all()

cat("=== Testing lag() namespace ===\n\n")

# Create test data
test_df <- data.frame(
  id = 1,
  age = 62:65,
  cola = c(8.7, 3.2, 2.5, 2.8)
)

cat("Test data:\n")
print(test_df)

# Using lag() without namespace (like the package does)
cat("\n\n=== Using lag() without namespace prefix ===\n")
result1 <- test_df %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    lagged = lag(cola, default = 0)
  ) %>%
  dplyr::ungroup()
print(result1)

# Using dplyr::lag() explicitly
cat("\n\n=== Using dplyr::lag() explicitly ===\n")
result2 <- test_df %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    lagged = dplyr::lag(cola, default = 0)
  ) %>%
  dplyr::ungroup()
print(result2)

# Check what lag function is being used
cat("\n\n=== Which lag() is being used? ===\n")
cat("stats::lag signature: lag(x, k = 1, ...)\n")
cat("dplyr::lag signature: lag(x, n = 1L, default = NULL, order_by = NULL, ...)\n")
cat("\nIn stats::lag, there is no 'default' parameter.\n")
cat("If stats::lag is being used, 'default = 0' is ignored!\n")

# Check what the package imports
cat("\n\n=== Package NAMESPACE check ===\n")
ns_file <- "C:/Users/AnthonyColavito/ssmbar/NAMESPACE"
if (file.exists(ns_file)) {
  cat("NAMESPACE contents:\n")
  cat(readLines(ns_file), sep = "\n")
}
