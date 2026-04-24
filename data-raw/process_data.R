# Data Processing Script for ssmbar
# This script reads raw data files, processes them, and saves them as package data.
#
# When new Trustees Report or scaled earnings factor data become available:
# 1. Place the new raw CSV files in data-raw/
# 2. Update the year suffix below
# 3. Run this script to generate new .rda files

library(dplyr)
library(here)

# Source the package functions needed for processing
source("R/assumptions_prep.R")

# =============================================================================
# Configuration
# =============================================================================

# Update these values when new data becomes available
DATA_YEAR <- 2025

# Input file paths (raw data in inst/extdata/)
TR_RAW_FILE <- here("data-raw", "2025TR_assumptions.csv")
SEF_RAW_FILE <- here("data-raw", "scaled_earnings_factors.csv")

# =============================================================================
# Process Trustees Report Assumptions
# =============================================================================

cat("Processing Trustees Report assumptions...\n")

# Read raw assumptions data
tr_raw <- read.csv(TR_RAW_FILE)

# Process through prep_assumptions()
tr_processed <- prep_assumptions(tr_raw)

# Assign to named object with year suffix
assign(paste0("tr", DATA_YEAR), tr_processed)

cat(paste0("  Created: tr", DATA_YEAR, "\n"))

# =============================================================================
# Process Scaled Earnings Factors
# =============================================================================

cat("Processing scaled earnings factors...\n")

# Read raw scaled earnings factors
sef_raw <- read.csv(SEF_RAW_FILE)

# -----------------------------------------------------------------------------
# Placeholder for future processing
# When processing is needed, add a prep_sef() function and call it here:
#
# source("R/sef_prep.R")  # Create this file when needed
# sef_processed <- prep_sef(sef_raw)
#
# For now, use the raw data as-is
# -----------------------------------------------------------------------------

sef_processed <- sef_raw

# Clean up the row names column if present (artifact from R's read.csv)
if ("X" %in% names(sef_processed)) {
  sef_processed <- sef_processed %>% select(-X)
}

assign(paste0("sef", DATA_YEAR), sef_processed)

cat(paste0("  Created: sef", DATA_YEAR, "\n"))

# =============================================================================
# Save Processed Data
# =============================================================================

cat("Saving processed data to data/ directory...\n")

save(
  list = paste0("tr", DATA_YEAR),
  file = here("data", paste0("tr", DATA_YEAR, ".rda")),
  compress = "xz"
)
cat(paste0("  Saved: data/tr", DATA_YEAR, ".rda\n"))

save(
  list = paste0("sef", DATA_YEAR),
  file = here("data", paste0("sef", DATA_YEAR, ".rda")),
  compress = "xz"
)
cat(paste0("  Saved: data/sef", DATA_YEAR, ".rda\n"))

cat("\nData processing complete!\n")

