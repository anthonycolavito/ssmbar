# Debug lag() behavior in COLA calculation
library(dplyr)
library(devtools)
load_all()

# Create minimal test case
test_df <- data.frame(
  id = rep(1, 5),
  age = 62:66,
  year = 2022:2026,
  basic_pia = 2000,
  elig_age = 62
)

# Join cola from assumptions
test_df <- test_df %>%
  left_join(tr2025 %>% select(year, cola), by = "year")

cat("=== Input data ===\n")
print(test_df)

cat("\n=== Testing lag() behavior ===\n")
test_result <- test_df %>%
  group_by(id) %>%
  arrange(id, age) %>%
  mutate(
    lagged_cola = lag(cola, default = 0),
    raw_factor = 1 + pmax(lag(cola, default = 0), 0) / 100,
    condition = age == elig_age,
    cola_factor_test = if_else(
      age == elig_age,
      1,
      1 + pmax(lag(cola, default = 0), 0) / 100
    )
  ) %>%
  ungroup()

print(test_result[, c("age", "year", "cola", "lagged_cola", "raw_factor", "condition", "cola_factor_test")])

cat("\n\n=== What the calculation SHOULD be ===\n")
cat("Age 62 (2022): cola=8.7, lagged=0 (default), factor should be 1 (eligibility)\n")
cat("Age 63 (2023): cola=3.2, lagged=8.7, factor should be 1.087\n")
cat("Age 64 (2024): cola=2.5, lagged=3.2, factor should be 1.032\n")
cat("Age 65 (2025): cola=2.8, lagged=2.5, factor should be 1.025\n")
cat("Age 66 (2026): cola=2.49, lagged=2.8, factor should be 1.028\n")

cat("\n\n=== Expected cumulative at age 65 ===\n")
cat("1 * 1.087 * 1.032 * 1.025 =", 1 * 1.087 * 1.032 * 1.025, "\n")
