# Debug V.C7 data structure
library(readxl)

raw_data <- read_excel("C:/Users/AnthonyColavito/Downloads/workers.xlsx", col_names = FALSE)

cat("Number of rows:", nrow(raw_data), "\n")
cat("Number of cols:", ncol(raw_data), "\n\n")

# Find medium earner section
medium_idx <- which(grepl("Scaled medium", raw_data[[1]], ignore.case = TRUE))
cat("Medium earner section starts at row:", medium_idx, "\n\n")

# Show some rows around the header
if (length(medium_idx) > 0) {
  start <- medium_idx[1]
  cat("Rows around medium earner section:\n")
  for (i in start:min(start+15, nrow(raw_data))) {
    row_vals <- sapply(1:min(7, ncol(raw_data)), function(j) {
      val <- raw_data[[j]][i]
      if (is.na(val)) "NA" else substr(as.character(val), 1, 15)
    })
    cat(sprintf("Row %d: %s\n", i, paste(row_vals, collapse = " | ")))
  }
}

# Try to find 1960 in medium earner section
cat("\n\nSearching for '1960' in medium earner section...\n")
if (length(medium_idx) > 0) {
  start <- medium_idx[1]
  for (i in (start+1):min(start+100, nrow(raw_data))) {
    val <- raw_data[[1]][i]
    if (!is.na(val) && grepl("1960", as.character(val))) {
      cat(sprintf("Found at row %d: ", i))
      row_vals <- sapply(1:min(7, ncol(raw_data)), function(j) {
        v <- raw_data[[j]][i]
        if (is.na(v)) "NA" else as.character(v)
      })
      cat(paste(row_vals, collapse = " | "), "\n")
      break
    }
  }
}
