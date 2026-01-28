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

# CRFB Color Palette
CRFB_DARK_BLUE <- "#003477"
CRFB_LIGHT_BLUE <- "#9ACDFF"
CRFB_ORANGE <- "#F36107"
CRFB_RED <- "#EE3224"
CRFB_PURPLE <- "#2A368A"
CRFB_TEAL <- "#0A81A8"
CRFB_GRAY <- "#666666"

# Theme configuration
app_theme <- bs_theme(
  bootswatch = "flatly",
  primary = CRFB_DARK_BLUE,
  secondary = CRFB_GRAY,
  success = CRFB_TEAL,
  info = CRFB_LIGHT_BLUE,
  warning = CRFB_ORANGE,
  danger = CRFB_RED,
  font_scale = 0.95,
  `enable-rounded` = TRUE
)

# Chart theme for ggplot
chart_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = CRFB_DARK_BLUE),
    plot.subtitle = element_text(color = CRFB_GRAY),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11, color = CRFB_GRAY),
    axis.text = element_text(size = 10, color = CRFB_GRAY)
  )

# Color palette for charts (CRFB brand colors)
CHART_COLORS <- c(
  CRFB_DARK_BLUE,  # Primary - CRFB Dark Blue
  CRFB_ORANGE,     # Secondary - Contrast Orange
  CRFB_TEAL,       # Third - Website Teal
  CRFB_RED,        # Fourth - FTD Red
  CRFB_PURPLE,     # Fifth - SSDI Purple
  CRFB_LIGHT_BLUE  # Sixth - CRFB Light Blue
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
