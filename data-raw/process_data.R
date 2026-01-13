# Data Processing Script for ssmbar
# This script reads raw data files, processes them, and saves them as package data.
#
# When new Trustees Report or scaled earnings factor data become available:
# 1. Place the new raw CSV files in inst/extdata/
# 2. Update the file paths and year suffix below
# 3. Run this script to generate new .rda files
# 4. The new data objects will be available to users after package rebuild

library(dplyr)

# Source the package functions needed for processing
source("R/assumptions_prep.R")

# =============================================================================
# Configuration
# =============================================================================

# Update these values when new data becomes available
DATA_YEAR <- 2025

# Input file paths (raw data in inst/extdata/)
TR_RAW_FILE <- "inst/extdata/2025TR assumptions.csv"
SEF_RAW_FILE <- "inst/extdata/scaled_earnings_factors.csv"

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

# Assign to named object with year suffix
assign(paste0("sef", DATA_YEAR), sef_processed)

cat(paste0("  Created: sef", DATA_YEAR, "\n"))

# =============================================================================
# Save Processed Data
# =============================================================================

cat("Saving processed data to data/ directory...\n")

# Save Trustees Report data
save(
  list = paste0("tr", DATA_YEAR),
  file = paste0("data/tr", DATA_YEAR, ".rda"),
  compress = "xz"
)
cat(paste0("  Saved: data/tr", DATA_YEAR, ".rda\n"))

# Save Scaled Earnings Factors data
save(
  list = paste0("sef", DATA_YEAR),
  file = paste0("data/sef", DATA_YEAR, ".rda"),
  compress = "xz"
)
cat(paste0("  Saved: data/sef", DATA_YEAR, ".rda\n"))

cat("\nData processing complete!\n")
cat(paste0("Users can now access tr", DATA_YEAR, " and sef", DATA_YEAR,
           " after loading the package.\n"))
