# Debug COLA - compare actual cola() vs manual calculation
library(devtools)
load_all()

cat("=== Comparing actual cola() function vs manual ===\n\n")

# Generate worker data through pia step
worker <- earnings_generator(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025
)
worker <- join_all_assumptions(worker, tr2025)
worker <- worker %>% aime(tr2025, TRUE) %>% pia(tr2025, TRUE)

cat("Columns before cola():", paste(names(worker)[1:10], collapse = ", "), "...\n")
cat("Total columns:", length(names(worker)), "\n\n")

# Make a copy for comparison
worker_copy <- worker

# Run ACTUAL cola() function
worker_after_cola <- worker %>% cola(tr2025, TRUE)

cat("=== ACTUAL cola() result at ages 62-65 ===\n")
print(worker_after_cola[worker_after_cola$age >= 62 & worker_after_cola$age <= 65,
                        c("age", "year", "cola", "cola_factor", "cola_cum_factor", "cola_basic_pia")])

# Now do manual calculation on the copy
cat("\n\n=== MANUAL calculation on same input ===\n")

# Replicate cola() logic exactly
dataset <- worker_copy  # cola column already present

dataset_manual <- dataset %>%
  dplyr::group_by(id) %>%
  dplyr::arrange(id, age) %>%
  dplyr::mutate(
    cola_factor_manual = dplyr::if_else(
      age == elig_age,
      1,
      1 + pmax(dplyr::lag(cola, default = 0), 0) / 100
    ),
    cola_factor_manual = dplyr::if_else(age >= elig_age, cola_factor_manual, 1),
    cola_cum_factor_manual = cumprod(cola_factor_manual),
    cola_basic_pia_manual = floor(basic_pia * cola_cum_factor_manual)
  ) %>%
  dplyr::ungroup()

cat("Manual calculation at ages 62-65:\n")
print(dataset_manual[dataset_manual$age >= 62 & dataset_manual$age <= 65,
                     c("age", "year", "cola", "cola_factor_manual", "cola_cum_factor_manual", "cola_basic_pia_manual")])

# Compare
cat("\n\n=== COMPARISON ===\n")
cat("Age 63 - Actual cola_factor:", worker_after_cola$cola_factor[worker_after_cola$age == 63], "\n")
cat("Age 63 - Manual cola_factor:", dataset_manual$cola_factor_manual[dataset_manual$age == 63], "\n")
