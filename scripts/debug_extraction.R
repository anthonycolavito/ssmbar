# Debug V.C7 extraction for specific cases
library(readxl)

file_path <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
raw_data <- read_excel(file_path, sheet = "V.C7", col_names = FALSE)

# Find medium earner section
pattern <- "Scaled medium"
medium_idx <- which(grepl(pattern, raw_data[[1]], ignore.case = TRUE))
cat("Medium section starts at row:", medium_idx, "\n\n")

# Show some rows
if (length(medium_idx) > 0) {
  start <- medium_idx[1]
  cat("Looking for year 2025 after row", start, "...\n")

  for (i in (start + 1):min(start + 100, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    cat(sprintf("Row %d: col1 = '%s', class = %s, is.na = %s\n",
                i, as.character(val), class(val), is.na(val)))

    if (!is.na(val) && as.character(val) == "2025") {
      cat("  FOUND! Column 6 value:", raw_data[[6]][i], "\n")
      break
    }

    if (i > start + 95) break
  }
}

# Try a direct test
cat("\n\n=== Direct test ===\n")
cat("Looking for row where col1 == 2025 in medium section...\n")

# Check data types
cat("First few rows after medium header:\n")
for (i in (medium_idx[1] + 1):(medium_idx[1] + 5)) {
  val <- raw_data[[1]][i]
  cat(sprintf("  Row %d: value='%s', numeric_val=%s, class=%s\n",
              i, val, as.numeric(val), class(val)))
}

# Try numeric comparison
cat("\n\nTrying numeric comparison for 2025...\n")
for (i in (medium_idx[1] + 1):min(medium_idx[1] + 100, nrow(raw_data))) {
  val <- raw_data[[1]][i]
  if (!is.na(val)) {
    num_val <- suppressWarnings(as.numeric(val))
    if (!is.na(num_val) && num_val == 2025) {
      cat(sprintf("Found 2025 at row %d!\n", i))
      cat("  Full row:", sapply(1:7, function(j) as.character(raw_data[[j]][i])), "\n")
      cat("  Column 6 value:", raw_data[[6]][i], "\n")
      break
    }
  }
}
