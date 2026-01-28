# =============================================================================
# Global Configuration for Benefit Explorer App
# =============================================================================

# Load required packages
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(scales)
library(ssmbar)

# Load package data
data(tr2025, package = "ssmbar")
data(sef2025, package = "ssmbar")

# App configuration constants
APP_TITLE <- "Social Security Benefit Explorer"
APP_VERSION <- "0.1.0"

# Worker type options
WORKER_TYPES <- c(

"Very Low" = "very_low",
"Low" = "low",
"Medium" = "medium",
"High" = "high",
"Maximum" = "max",
"Custom" = "custom"
)

# Sex options
SEX_OPTIONS <- c(
"Male" = "male",
"Female" = "female",
"Gender-neutral" = "all"
)

# Birth year range
BIRTH_YEAR_MIN <- 1940
BIRTH_YEAR_MAX <- 2010
BIRTH_YEAR_DEFAULT <- 1970

# Claim age range
CLAIM_AGE_MIN <- 62
CLAIM_AGE_MAX <- 70
CLAIM_AGE_DEFAULT <- 67

# Default custom earnings amount (in current dollars)
CUSTOM_EARNINGS_DEFAULT <- 50000

# Theme configuration
app_theme <- bs_theme(
bootswatch = "flatly",
primary = "#0d6efd",
secondary = "#6c757d",
success = "#198754",
info = "#0dcaf0",
warning = "#ffc107",
danger = "#dc3545",
font_scale = 0.95,
`enable-rounded` = TRUE
)

# Chart theme for ggplot
chart_theme <- theme_minimal(base_size = 12) +
theme(
  plot.title = element_text(face = "bold", size = 14),
  plot.subtitle = element_text(color = "gray50"),
  legend.position = "bottom",
  panel.grid.minor = element_blank(),
  axis.title = element_text(size = 11),
  axis.text = element_text(size = 10)
)

# Color palette for charts
CHART_COLORS <- c(
"#0d6efd",  # Primary blue
"#198754",  # Success green
"#dc3545",  # Danger red
"#ffc107",  # Warning yellow
"#6f42c1",  # Purple
"#20c997"   # Teal
)

# Helper function to format currency
format_currency <- function(x, prefix = "$", suffix = "", big.mark = ",", digits = 0) {
paste0(prefix, format(round(x, digits), big.mark = big.mark, scientific = FALSE), suffix)
}

# Helper function to format percentages
format_percent <- function(x, digits = 1) {
paste0(format(round(x * 100, digits), nsmall = digits), "%")
}

# Load Shiny modules
source("modules/mod_worker_input.R")
source("modules/mod_benefits.R")
source("modules/mod_replacement.R")
source("modules/mod_lifetime.R")
source("modules/mod_ratios.R")
