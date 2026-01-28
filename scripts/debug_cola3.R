# Debug COLA - check data BEFORE cola() is called
library(devtools)
load_all()

cat("=== Running calculate_benefits() with interception ===\n\n")

# Instead of using calculate_benefits, let's manually step through
# to see what data looks like before cola()

worker <- earnings_generator(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025
)

# Join all assumptions (like calculate_benefits does)
worker <- join_all_assumptions(worker, tr2025)

# Run aime and pia (like calculate_benefits does)
worker <- worker %>%
  aime(tr2025, TRUE) %>%
  pia(tr2025, TRUE)

cat("=== Data before cola() - checking ages 60-66 ===\n")
pre_cola <- worker[worker$age >= 60 & worker$age <= 66,
                   c("id", "age", "year", "basic_pia", "elig_age", "cola")]
print(pre_cola)

cat("\n\n=== Now running cola() ===\n")
worker <- worker %>% cola(tr2025, TRUE)

cat("\nData after cola() - ages 60-66:\n")
post_cola <- worker[worker$age >= 60 & worker$age <= 66,
                    c("age", "year", "cola", "cola_factor", "cola_cum_factor", "cola_basic_pia")]
print(post_cola)

cat("\n\n=== Checking if lag() works correctly on FULL dataset ===\n")
# Re-run the cola logic on the full dataset BEFORE cola() modified it
test <- worker[, c("id", "age", "year", "basic_pia", "elig_age")]
test <- test %>%
  dplyr::left_join(tr2025 %>% dplyr::select(year, cola), by = "year")

# Check row ordering
cat("Row count:", nrow(test), "\n")
cat("Ages in data:", min(test$age), "to", max(test$age), "\n")

# Apply lag manually to see what happens
test_result <- test %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(id, age) %>%
  dplyr::mutate(
    row_num = dplyr::row_number(),
    lagged_cola = dplyr::lag(cola, default = 0)
  ) %>%
  dplyr::ungroup()

cat("\nLag result for ages 60-66:\n")
print(test_result[test_result$age >= 60 & test_result$age <= 66,
                  c("row_num", "age", "year", "cola", "lagged_cola")])
