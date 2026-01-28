# Check V.C7 columns and compare reduced benefits
library(devtools)
library(readxl)
library(dplyr)
load_all()

cat("=============================================================================\n")
cat("V.C7 COLUMN INVESTIGATION\n")
cat("=============================================================================\n\n")

# Load Table V.C7 data
workers_file <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(workers_file, col_names = FALSE)

# Find the medium earner section and look at column headers
cat("=== EXAMINING V.C7 STRUCTURE ===\n")
medium_start <- which(grepl("Scaled medium earnings", raw_data[[1]], ignore.case = TRUE))[1]
cat("Medium earner section starts at row:", medium_start, "\n\n")

# Look at a few rows around the header
cat("Rows around medium earner header:\n")
for (i in (medium_start-2):(medium_start+5)) {
  row_vals <- sapply(1:7, function(j) {
    val <- raw_data[[j]][i]
    if (is.na(val)) "NA" else as.character(val)
  })
  cat(sprintf("Row %d: %s\n", i, paste(row_vals, collapse = " | ")))
}

# Find the 2025 row for medium earner
cat("\n\nLooking for year 2025 in medium earner section:\n")
for (i in (medium_start+1):min(medium_start+100, nrow(raw_data))) {
  if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
    cat("Found 2025 at row", i, "\n")
    cat("All column values:\n")
    for (j in 1:7) {
      val <- raw_data[[j]][i]
      cat(sprintf("  Column %d: %s\n", j, ifelse(is.na(val), "NA", as.character(val))))
    }
    break
  }
}

cat("\n\n=== COMPARING REDUCED BENEFITS ===\n")
cat("User says medium earner reduced benefit (second from right) = 25,172\n\n")

# Calculate medium earner with ssmbar
result <- calculate_benefits(
  birth_yr = 1960, type = "medium", sex = "all",
  age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
)

age65 <- result[result$age == 65, ]
cat("ssmbar results for medium earner at age 65:\n")
cat("  cola_basic_pia (full PIA):", age65$cola_basic_pia, "monthly,", age65$cola_basic_pia * 12, "annual\n")
cat("  act_factor:", age65$act_factor, "\n")
cat("  wrk_ben (reduced):", age65$wrk_ben, "monthly,", age65$wrk_ben * 12, "annual\n")

cat("\n\nComparison:\n")
vc7_reduced <- 25172
cat("  V.C7 reduced benefit:", vc7_reduced, "\n")
cat("  ssmbar wrk_ben annual:", age65$wrk_ben * 12, "\n")
cat("  Difference:", age65$wrk_ben * 12 - vc7_reduced, "(",
    round((age65$wrk_ben * 12 - vc7_reduced) / vc7_reduced * 100, 2), "%)\n")

cat("\n\n=== ALL WORKER TYPES - REDUCED BENEFITS ===\n")
# Let's get the reduced benefit column for all worker types
# User says it's the second column from the right

get_vc7_reduced <- function(pattern) {
  idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
  if (length(idx) == 0) return(NA)
  start_idx <- idx[1]

  for (i in (start_idx+1):min(start_idx+200, nrow(raw_data))) {
    if (!is.na(raw_data[[1]][i]) && raw_data[[1]][i] == "2025") {
      # Second from right - assuming 7 columns, that's column 6
      return(as.numeric(raw_data[[6]][i]))
    }
  }
  return(NA)
}

vc7_reduced_vals <- list(
  very_low = get_vc7_reduced("Scaled very low"),
  low = get_vc7_reduced("Scaled low earnings"),
  medium = get_vc7_reduced("Scaled medium"),
  high = get_vc7_reduced("Scaled high"),
  max = get_vc7_reduced("Steady maximum")
)

cat(sprintf("\n%-10s %12s %12s %10s %8s\n", "Type", "V.C7 reduced", "ssmbar", "Diff", "Pct"))
cat(paste(rep("-", 60), collapse = ""), "\n")

for (type in c("very_low", "low", "medium", "high", "max")) {
  result <- calculate_benefits(
    birth_yr = 1960, type = type, sex = "all",
    age_claim = 65, factors = sef2025, assumptions = tr2025, debugg = TRUE
  )
  age65 <- result[result$age == 65, ]
  annual_reduced <- age65$wrk_ben * 12
  v <- vc7_reduced_vals[[type]]

  if (!is.na(v)) {
    diff <- annual_reduced - v
    pct <- round(diff / v * 100, 2)
    cat(sprintf("%-10s %12d %12d %10d %7.2f%%\n", type, v, annual_reduced, diff, pct))
  } else {
    cat(sprintf("%-10s %12s %12d %10s %8s\n", type, "NA", annual_reduced, "NA", "NA"))
  }
}
