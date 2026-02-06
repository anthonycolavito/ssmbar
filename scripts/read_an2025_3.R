# Extract text from Actuarial Note 2025.3 PDF
library(pdftools)
txt <- pdf_text("C:/Users/AnthonyColavito/Downloads/an2025-3.pdf")
cat(sprintf("Total pages: %d\n\n", length(txt)))
for (i in seq_along(txt)) {
  cat(sprintf("=== PAGE %d ===\n", i))
  cat(txt[i])
  cat("\n\n")
}
