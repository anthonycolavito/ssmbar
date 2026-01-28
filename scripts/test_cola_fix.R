# Test year-by-year COLA rounding implementation
library(dplyr)
library(devtools)
load_all()

cat("=== Testing year-by-year COLA implementation ===\n\n")

# Create test data similar to what cola() receives
worker <- earnings_generator(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025
)
worker <- join_all_assumptions(worker, tr2025)
worker <- worker %>% aime(tr2025, TRUE) %>% pia(tr2025, TRUE)

# Extract relevant data for testing
test_df <- worker[worker$age >= 60 & worker$age <= 70,
                  c("id", "age", "year", "basic_pia", "elig_age", "cola")]

cat("Input data:\n")
print(test_df[1:8, ])

cat("\n\n=== Implementing year-by-year rounding with accumulate ===\n")

# Use purrr::accumulate to build up COLA-adjusted PIA with rounding at each step
library(purrr)

result <- test_df %>%
  group_by(id) %>%
  arrange(id, age) %>%
  mutate(
    # Get the COLA factor for each year (using lagged COLA)
    cola_factor = if_else(
      age == elig_age,
      1,
      1 + pmax(lag(cola, default = 0), 0) / 100
    ),
    cola_factor = if_else(age >= elig_age, cola_factor, 1)
  ) %>%
  ungroup()

# Now use accumulate to apply year-by-year rounding
# The key insight: at each step, multiply previous rounded value by current factor, then floor
result <- result %>%
  group_by(id) %>%
  arrange(id, age) %>%
  mutate(
    # Start with basic_pia at eligibility, then compound with rounding
    cola_basic_pia_new = {
      # Get vectors
      bp <- basic_pia
      cf <- cola_factor
      ea <- first(elig_age)
      ages <- age

      # Initialize result vector
      result_vec <- numeric(length(bp))

      for (i in seq_along(bp)) {
        if (ages[i] < ea) {
          result_vec[i] <- 0
        } else if (ages[i] == ea) {
          result_vec[i] <- bp[i]  # No COLA at eligibility
        } else {
          # Multiply previous rounded value by current COLA factor, then floor
          result_vec[i] <- floor(result_vec[i-1] * cf[i])
        }
      }
      result_vec
    }
  ) %>%
  ungroup()

cat("\nResult with year-by-year rounding:\n")
print(result[result$age >= 62 & result$age <= 66,
             c("age", "year", "basic_pia", "cola", "cola_factor", "cola_basic_pia_new")])

cat("\n\n=== Comparison with V.C7 ===\n")
pia_65 <- result$cola_basic_pia_new[result$age == 65]
cat("Year-by-year cola_basic_pia at 65:", pia_65, "\n")
cat("Annual:", pia_65 * 12, "\n")
cat("V.C7:", 29283, "\n")
cat("Difference:", pia_65 * 12 - 29283, "(", round((pia_65 * 12 - 29283) / 29283 * 100, 2), "%)\n")
