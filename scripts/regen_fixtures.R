# Regenerate ALL regression test fixtures — exact params from test-regression.R
# Run from repo root: Rscript scripts/regen_fixtures.R
devtools::load_all(".")

fixture_dir <- "tests/testthat/fixtures"

gen <- function(name, ...) {
  cat(sprintf("Generating %s... ", name))
  w <- calculate_benefits(..., factors = sef2025, assumptions = tr2025, debugg = TRUE)
  saveRDS(w, file.path(fixture_dir, paste0(name, ".rds")))
  cat("done\n")
}

# Single workers
gen("medium_1960_claim67", birth_yr=1960, sex="male", type="medium", age_claim=67)
gen("low_1960_claim62", birth_yr=1960, sex="female", type="low", age_claim=62)
gen("high_1960_claim70", birth_yr=1960, sex="male", type="high", age_claim=70)
gen("max_1960_claim67", birth_yr=1960, sex="male", type="max", age_claim=67)
gen("custom_50k_1970", birth_yr=1970, sex="male", type="custom", age_claim=65, custom_avg_earnings=50000)
gen("claim_at_nra", birth_yr=1960, sex="male", type="high", age_claim=67)
gen("nra_transition_1955", birth_yr=1955, sex="female", type="medium", age_claim=62)
gen("older_cohort_1943", birth_yr=1943, sex="male", type="medium", age_claim=66)

# Spouse combinations
gen("high_1960_with_spouse", birth_yr=1960, sex="male", type="high", age_claim=67,
    spouse_type="low", spouse_sex="female", spouse_birth_yr=1962, spouse_age_claim=65)
gen("low_early_62_high_spouse_67", birth_yr=1960, sex="female", type="low", age_claim=62,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=67)
gen("low_nra_67_high_spouse_67", birth_yr=1960, sex="female", type="low", age_claim=67,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=67)
gen("medium_early_62_medium_spouse_62", birth_yr=1960, sex="male", type="medium", age_claim=62,
    spouse_type="medium", spouse_sex="female", spouse_birth_yr=1960, spouse_age_claim=62)
gen("very_low_1960_high_spouse_ret", birth_yr=1960, sex="female", type="very_low", age_claim=62,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=62)
gen("max_drc_with_spouse", birth_yr=1960, sex="male", type="high", age_claim=70,
    spouse_type="low", spouse_sex="female", spouse_birth_yr=1962, spouse_age_claim=65)
gen("very_low_both", birth_yr=1970, sex="male", type="very_low", age_claim=62,
    spouse_type="very_low", spouse_sex="female", spouse_birth_yr=1970, spouse_age_claim=62)
gen("zero_earnings_high_spouse", birth_yr=1960, sex="female", type="very_low", age_claim=62,
    spouse_type="max", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=67)
gen("spouse_much_older", birth_yr=1972, sex="female", type="medium", age_claim=62,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=67)
gen("spouse_much_younger", birth_yr=1960, sex="male", type="high", age_claim=62,
    spouse_type="low", spouse_sex="female", spouse_birth_yr=1970, spouse_age_claim=62)
gen("worker_younger_than_spouse", birth_yr=1975, sex="female", type="medium", age_claim=62,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=67)
gen("spouse_with_drcs", birth_yr=1965, sex="female", type="low", age_claim=65,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1965, spouse_age_claim=70)
gen("spouse_dies_before_claiming", birth_yr=1960, sex="female", type="low", age_claim=62,
    spouse_type="high", spouse_sex="male", spouse_birth_yr=1960, spouse_age_claim=70)

# Disabled widow(er) cases
gen("disabled_widow_qualifies", birth_yr=1980, sex="male", type="medium", age_claim=45, disabled_age=45,
    spouse_type="high", spouse_sex="female", spouse_birth_yr=1950, spouse_age_claim=67)
gen("disabled_widow_very_low", birth_yr=1980, sex="female", type="very_low", age_claim=50, disabled_age=50,
    spouse_type="max", spouse_sex="male", spouse_birth_yr=1946, spouse_age_claim=67)
gen("disabled_widow_7yr_fail", birth_yr=1970, sex="male", type="medium", age_claim=55, disabled_age=55,
    spouse_type="high", spouse_sex="female", spouse_birth_yr=1930, spouse_age_claim=67)
gen("disabled_standard_widow", birth_yr=1960, sex="male", type="medium", age_claim=45, disabled_age=45,
    spouse_type="low", spouse_sex="female", spouse_birth_yr=1960, spouse_age_claim=62)

cat("\nAll fixtures regenerated.\n")
