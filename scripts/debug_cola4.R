# Debug COLA - check if cola_factor already exists
library(devtools)
load_all()

cat("=== Step by step trace ===\n\n")

# Generate worker data
worker <- earnings_generator(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025
)

cat("After earnings_generator - columns:\n")
cat(paste(names(worker), collapse = ", "), "\n")
cat("cola_factor exists?", "cola_factor" %in% names(worker), "\n")

# Join all assumptions
worker <- join_all_assumptions(worker, tr2025)
cat("\nAfter join_all_assumptions - columns:\n")
cat(paste(names(worker), collapse = ", "), "\n")
cat("cola_factor exists?", "cola_factor" %in% names(worker), "\n")

# Run aime
worker <- worker %>% aime(tr2025, TRUE)
cat("\nAfter aime - columns:\n")
cat(paste(names(worker), collapse = ", "), "\n")
cat("cola_factor exists?", "cola_factor" %in% names(worker), "\n")

# Run pia
worker <- worker %>% pia(tr2025, TRUE)
cat("\nAfter pia - columns:\n")
cat(paste(names(worker), collapse = ", "), "\n")
cat("cola_factor exists?", "cola_factor" %in% names(worker), "\n")

# Now let's trace what happens inside cola()
cat("\n\n=== Inside cola() trace ===\n")

# Check if cola column already present
cat("cola column exists before cola()?", "cola" %in% names(worker), "\n")
if ("cola" %in% names(worker)) {
  cat("cola values at ages 62-65:\n")
  print(worker[worker$age >= 62 & worker$age <= 65, c("age", "year", "cola")])
}

# This is what cola() does internally:
cols_needed <- c("cola", "elig_age_retired")
cols_missing <- cols_needed[!cols_needed %in% names(worker)]
cat("\ncols_missing:", paste(cols_missing, collapse = ", "), "\n")

if (length(cols_missing) > 0) {
  cat("Would need to join:", paste(cols_missing, collapse = ", "), "\n")
  dataset <- worker %>% dplyr::left_join(tr2025 %>% dplyr::select(year, dplyr::all_of(cols_missing)),
                                         by = "year")
} else {
  cat("Using worker as-is (all columns present)\n")
  dataset <- worker
}

# Check data before mutate
cat("\nData at ages 62-65 before mutate:\n")
print(dataset[dataset$age >= 62 & dataset$age <= 65, c("age", "year", "basic_pia", "elig_age", "cola")])

# Apply the mutate
dataset_result <- dataset %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(id, age) %>%
  dplyr::mutate(
    debug_lagged = dplyr::lag(cola, default = 0),
    debug_raw_calc = 1 + pmax(dplyr::lag(cola, default = 0), 0) / 100,
    debug_condition = age == elig_age,
    cola_factor = dplyr::if_else(
      age == elig_age,
      1,
      1 + pmax(dplyr::lag(cola, default = 0), 0) / 100
    ),
    cola_factor = dplyr::if_else(age >= elig_age, cola_factor, 1)
  ) %>%
  dplyr::ungroup()

cat("\nResult with debug columns at ages 62-65:\n")
print(dataset_result[dataset_result$age >= 62 & dataset_result$age <= 65,
                     c("age", "year", "cola", "debug_lagged", "debug_raw_calc", "debug_condition", "cola_factor")])
