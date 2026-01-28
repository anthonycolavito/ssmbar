# Check COLA timing for medium earner
# Key question: How many COLAs are applied between eligibility and claiming?

library(devtools)
load_all()

cat("=============================================================================\n")
cat("COLA TIMING CHECK\n")
cat("=============================================================================\n\n")

# Historical COLAs
cat("Historical COLAs from cola.csv:\n")
cat("  2021 COLA: 5.9% (applied to benefits Jan 2022)\n")
cat("  2022 COLA: 8.7% (applied to benefits Jan 2023)\n")
cat("  2023 COLA: 3.2% (applied to benefits Jan 2024)\n")
cat("  2024 COLA: 2.5% (applied to benefits Jan 2025)\n")
cat("  2025 COLA: 2.8% (applied to benefits Jan 2026)\n\n")

# For worker born 1960, claiming at 65:
# - Eligibility: Age 62 in 2022
# - Claiming: Age 65 in 2025

cat("For worker born 1960 claiming at 65:\n")
cat("  Age 62 in 2022: basic_pia calculated, no COLA yet\n")
cat("  Age 63 in 2023: Apply 2022 COLA (8.7%)\n")
cat("  Age 64 in 2024: Apply 2023 COLA (3.2%)\n")
cat("  Age 65 in 2025: Apply 2024 COLA (2.5%)\n\n")

# Calculate cumulative COLA factor
cola_2022 <- 8.7
cola_2023 <- 3.2
cola_2024 <- 2.5

cum_factor <- (1 + cola_2022/100) * (1 + cola_2023/100) * (1 + cola_2024/100)
cat("Cumulative COLA factor (exact multiplication):", cum_factor, "\n")
cat("As percentage increase:", (cum_factor - 1) * 100, "%\n\n")

# Now verify with actual calculation
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

age62 <- result[result$age == 62, ]
age65 <- result[result$age == 65, ]

cat("From ssmbar calculation:\n")
cat("  basic_pia at 62:", age62$basic_pia, "\n")
cat("  cola_basic_pia at 65:", age65$cola_basic_pia, "\n")
cat("  Actual ratio:", age65$cola_basic_pia / age62$basic_pia, "\n")
cat("  Expected from cumulative:", cum_factor, "\n\n")

# The difference is due to year-by-year flooring
# Let's trace the flooring effect
cat("Year-by-year flooring trace:\n")
pia <- age62$basic_pia
cat(sprintf("  Age 62 (2022): basic_pia = %d\n", pia))

pia_63_exact <- pia * (1 + cola_2022/100)
pia_63_floored <- floor(pia_63_exact)
cat(sprintf("  Age 63 (2023): %d × 1.087 = %.2f → floor = %d\n", pia, pia_63_exact, pia_63_floored))

pia_64_exact <- pia_63_floored * (1 + cola_2023/100)
pia_64_floored <- floor(pia_64_exact)
cat(sprintf("  Age 64 (2024): %d × 1.032 = %.2f → floor = %d\n", pia_63_floored, pia_64_exact, pia_64_floored))

pia_65_exact <- pia_64_floored * (1 + cola_2024/100)
pia_65_floored <- floor(pia_65_exact)
cat(sprintf("  Age 65 (2025): %d × 1.025 = %.2f → floor = %d\n", pia_64_floored, pia_65_exact, pia_65_floored))

cat("\n  ssmbar cola_basic_pia at 65:", age65$cola_basic_pia, "\n")
cat("  Manual trace result:", pia_65_floored, "\n")
cat("  Match:", pia_65_floored == age65$cola_basic_pia, "\n\n")

# What V.C7 implies
vc7_reduced <- 25172
act_factor <- 0.866666667
vc7_implied_cola_pia_65 <- (vc7_reduced / 12) / act_factor

cat("=== What V.C7 implies ===\n")
cat("  V.C7 reduced annual:", vc7_reduced, "\n")
cat("  V.C7 reduced monthly:", vc7_reduced / 12, "\n")
cat("  If actuarial factor = 0.8667\n")
cat("  Implied cola_basic_pia at 65:", round(vc7_implied_cola_pia_65, 2), "\n")
cat("  Our cola_basic_pia at 65:", age65$cola_basic_pia, "\n")
cat("  Difference:", round(age65$cola_basic_pia - vc7_implied_cola_pia_65, 2), "\n\n")

# Check if V.C7 might floor monthly benefit differently
cat("=== Rounding investigation ===\n")
# What if V.C7 rounds to dime?
wrk_ben_exact <- age65$cola_basic_pia * act_factor
wrk_ben_floored <- floor(wrk_ben_exact)
wrk_ben_dimed <- floor(wrk_ben_exact * 10) / 10

cat("  cola_basic_pia × act_factor =", age65$cola_basic_pia, "×", act_factor, "=", wrk_ben_exact, "\n")
cat("  Floor to dollar:", wrk_ben_floored, "→ annual =", wrk_ben_floored * 12, "\n")
cat("  Floor to dime:", wrk_ben_dimed, "→ annual =", wrk_ben_dimed * 12, "\n")
cat("  V.C7 annual:", vc7_reduced, "\n")
