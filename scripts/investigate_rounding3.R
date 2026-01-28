# Get maximum earner V.C7 value and investigate differences
library(devtools)
library(readxl)
library(dplyr)
load_all()

cat("=============================================================================\n")
cat("MAXIMUM EARNER AND DIFFERENCE INVESTIGATION\n")
cat("=============================================================================\n\n")

# Load Table V.C7 data
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

# Get maximum earner value (row 659 starts the section)
cat("=== MAXIMUM EARNER FROM V.C7 ===\n")
# Find the row for year 2025 after "Steady maximum earnings"
start_idx <- 659
for (i in (start_idx+1):min(start_idx+200, nrow(raw_data))) {
  if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
    vc7_max <- as.numeric(raw_data[[3]][i])
    cat("Maximum earner V.C7 value (2025):", vc7_max, "\n")
    break
  }
}

# Calculate max earner with ssmbar
result_max <- calculate_benefits(
  birth_yr = 1960, type = "max", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)
age65_max <- result_max[result_max$age == 65, ]
annual_max <- age65_max$cola_basic_pia * 12

cat("ssmbar max earner annual:", annual_max, "\n")
cat("Difference:", annual_max - vc7_max, "(", round((annual_max - vc7_max) / vc7_max * 100, 2), "%)\n")

cat("\n\n=== ALL WORKER TYPES COMPARISON (UPDATED) ===\n")

# All V.C7 values
get_vc7 <- function(pattern, start_row = 1) {
  idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(idx) == 0) return(NA)
  start_idx <- idx[1]

  for (i in (start_idx+1):min(start_idx+200, nrow(raw_data))) {
    if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
      return(as.numeric(raw_data[[3]][i]))
    }
  }
  return(NA)
}

vc7 <- list(
  very_low = get_vc7("Scaled very low"),
  low = get_vc7("Scaled low earnings"),
  medium = get_vc7("Scaled medium"),
  high = get_vc7("Scaled high"),
  max = get_vc7("Steady maximum")
)

cat(sprintf("\n%-10s %10s %10s %10s %8s\n", "Type", "V.C7", "ssmbar", "Diff", "Pct"))
cat(paste(rep("-", 55), collapse = ""), "\n")

for (type in c("very_low", "low", "medium", "high", "max")) {
  result <- calculate_benefits(
    birth_yr = 1960, type = type, sex = "all",
    age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
  )
  age65 <- result[result$age == 65, ]
  annual <- age65$cola_basic_pia * 12
  v <- vc7[[type]]
  diff <- annual - v
  pct <- round(diff / v * 100, 2)

  cat(sprintf("%-10s %10d %10d %10d %7.2f%%\n", type, v, annual, diff, pct))
}

cat("\n\n=== INVESTIGATING WHY OTHER WORKERS DIFFER ===\n")
cat("Medium earner matches (-0.01%), so the COLA and AIME logic is correct.\n")
cat("The differences must be in AIME/PIA calculations for these specific worker types.\n\n")

# Let's work backwards from V.C7 to see what AIME would be needed
cat("Working backwards from V.C7 to implied AIME:\n")
cat("Bend points for 2022: bp1=1024, bp2=6172\n")
cat("Factors: 90%, 32%, 15%\n\n")

# COLA factor for age 65
cola_factor <- 1.087 * 1.032 * 1.025  # 1.149829

for (type in c("very_low", "low", "high")) {
  v <- vc7[[type]]
  monthly_cola_pia <- v / 12
  basic_pia_implied <- monthly_cola_pia / cola_factor

  # Work backwards to get implied AIME
  # PIA = 0.9 * min(AIME, 1024) + 0.32 * min(max(AIME-1024,0), 6172-1024) + 0.15 * max(AIME-6172,0)
  # For very_low and low, AIME is likely < 6172
  # If AIME < 1024: PIA = 0.9 * AIME, so AIME = PIA / 0.9
  # If 1024 < AIME < 6172: PIA = 0.9*1024 + 0.32*(AIME-1024) = 921.6 + 0.32*(AIME-1024)
  #   so AIME = 1024 + (PIA - 921.6) / 0.32

  if (basic_pia_implied < 921.6) {
    # All in first bracket
    implied_aime <- basic_pia_implied / 0.9
  } else if (basic_pia_implied < 921.6 + 0.32 * (6172 - 1024)) {
    # In second bracket
    implied_aime <- 1024 + (basic_pia_implied - 921.6) / 0.32
  } else {
    # In third bracket
    implied_aime <- 6172 + (basic_pia_implied - 921.6 - 0.32 * (6172 - 1024)) / 0.15
  }

  # Get actual ssmbar values
  result <- calculate_benefits(
    birth_yr = 1960, type = type, sex = "all",
    age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
  )
  age65 <- result[result$age == 65, ]

  cat(sprintf("%s earner:\n", type))
  cat(sprintf("  V.C7 annual: %d, monthly cola_pia: %.2f\n", v, monthly_cola_pia))
  cat(sprintf("  Implied basic_pia: %.2f, Implied AIME: %.0f\n", basic_pia_implied, implied_aime))
  cat(sprintf("  ssmbar basic_pia: %d, ssmbar AIME: %d\n", age65$basic_pia, age65$aime))
  cat(sprintf("  AIME difference: %d\n\n", age65$aime - round(implied_aime)))
}
