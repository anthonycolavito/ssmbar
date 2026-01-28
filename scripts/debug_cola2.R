# Debug COLA in actual calculate_benefits flow
library(devtools)
load_all()

# Run calculate_benefits but intercept at cola() step
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

# Check if cola values are present and what they are
cat("=== COLA values in result at ages 60-66 ===\n")
for (a in 60:66) {
  row <- result[result$age == a, ]
  if (nrow(row) > 0) {
    cola_val <- if ("cola" %in% names(row)) row$cola else NA
    cola_factor <- if ("cola_factor" %in% names(row)) row$cola_factor else NA
    cat(sprintf("Age %d (year %d): cola=%s, cola_factor=%s\n",
                a, row$year,
                ifelse(is.na(cola_val), "NA", sprintf("%.2f", cola_val)),
                ifelse(is.na(cola_factor), "NA", sprintf("%.4f", cola_factor))))
  }
}

# Now let's manually step through the cola() function
cat("\n\n=== Manually stepping through cola() ===\n")

# First, simulate what happens before cola() is called
# Worker should have basic_pia at this point
worker_pre_cola <- result[, c("id", "age", "year", "basic_pia", "elig_age")]
worker_pre_cola <- worker_pre_cola[worker_pre_cola$age >= 60 & worker_pre_cola$age <= 66, ]

cat("Worker data before cola (ages 60-66):\n")
print(worker_pre_cola)

# Join cola from assumptions
cat("\n\nJoining cola from assumptions:\n")
worker_with_cola <- worker_pre_cola %>%
  dplyr::left_join(tr2025 %>% dplyr::select(year, cola), by = "year")
print(worker_with_cola)

# Now apply the cola() logic
cat("\n\nApplying cola() logic:\n")
worker_result <- worker_with_cola %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(id, age) %>%
  dplyr::mutate(
    lagged_cola = dplyr::lag(cola, default = 0),
    cola_factor = dplyr::if_else(
      age == elig_age,
      1,
      1 + pmax(dplyr::lag(cola, default = 0), 0) / 100
    ),
    cola_factor = dplyr::if_else(age >= elig_age, cola_factor, 1),
    cola_cum_factor = cumprod(cola_factor),
    cola_basic_pia = floor(basic_pia * cola_cum_factor)
  ) %>%
  dplyr::ungroup()

print(worker_result)
