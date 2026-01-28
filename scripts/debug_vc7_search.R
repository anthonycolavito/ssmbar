# Debug V.C7 - search for values around 25000 and 29000
library(readxl)

file_path <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"
data <- read_excel(file_path, sheet = "V.C7", col_names = FALSE)

cat("Searching for values between 24000 and 30000...\n\n")

# Search for values in that range
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    val <- data[[j]][i]
    if (!is.na(val) && is.numeric(val) && val > 24000 && val < 30000) {
      # Show the row
      row_vals <- sapply(1:ncol(data), function(k) {
        v <- data[[k]][i]
        if (is.na(v)) "NA" else as.character(v)
      })
      cat(sprintf("Row %d, Col %d, Value %.0f\n", i, j, val))
      cat("  Full row:", paste(row_vals, collapse = " | "), "\n\n")
    }
  }
}

# Also show the full row for 1960 in medium earner section
cat("\n=== Full row for 1960 in medium earner section ===\n")
medium_idx <- which(grepl("Scaled medium", data[[1]], ignore.case = TRUE))
if (length(medium_idx) > 0) {
  start <- medium_idx[1]
  for (i in (start+1):min(start+100, nrow(data))) {
    val <- data[[1]][i]
    if (!is.na(val) && grepl("^1960$", as.character(val))) {
      row_vals <- sapply(1:ncol(data), function(k) {
        v <- data[[k]][i]
        if (is.na(v)) "NA" else as.character(v)
      })
      cat("Row", i, ":", paste(row_vals, collapse = " | "), "\n")
      break
    }
  }
}

# Check what row 2025 shows (turning 65 means birth year = 2025 - 65 = 1960)
# But maybe the table is organized by the year they turn 65?
cat("\n=== Searching for '2025' anywhere ===\n")
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    val <- data[[j]][i]
    if (!is.na(val) && as.character(val) == "2025") {
      row_vals <- sapply(1:ncol(data), function(k) {
        v <- data[[k]][i]
        if (is.na(v)) "NA" else as.character(v)
      })
      cat(sprintf("Found '2025' at row %d, col %d\n", i, j))
      cat("  Full row:", paste(row_vals, collapse = " | "), "\n")
    }
  }
}
