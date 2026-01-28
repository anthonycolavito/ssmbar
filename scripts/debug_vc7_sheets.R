# Debug V.C7 - check all sheets
library(readxl)

file_path <- "C:/Users/AnthonyColavito/Downloads/workers.xlsx"

# Get sheet names
sheets <- excel_sheets(file_path)
cat("Available sheets:", paste(sheets, collapse = ", "), "\n\n")

# Try reading each sheet and look for the 25172 value
for (sheet in sheets) {
  cat("=== Sheet:", sheet, "===\n")
  data <- read_excel(file_path, sheet = sheet, col_names = FALSE)
  cat("  Rows:", nrow(data), "Cols:", ncol(data), "\n")

  # Search for 25172
  found <- FALSE
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      val <- data[[j]][i]
      if (!is.na(val) && is.numeric(val) && abs(val - 25172) < 1) {
        cat(sprintf("  Found 25172 at row %d, col %d\n", i, j))
        # Show the row
        row_vals <- sapply(1:ncol(data), function(k) {
          v <- data[[k]][i]
          if (is.na(v)) "NA" else as.character(round(as.numeric(v), 0))
        })
        cat("    Row:", paste(row_vals, collapse = " | "), "\n")
        found <- TRUE
      }
    }
  }
  if (!found) cat("  Value 25172 not found\n")
  cat("\n")
}
