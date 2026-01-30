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
BIRTH_YEAR_DEFAULT <- 1960

# Claim age range
CLAIM_AGE_MIN <- 62
CLAIM_AGE_MAX <- 70
CLAIM_AGE_DEFAULT <- 65

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

# Dark mode colors
DARK_BG <- "#1a1a2e"
DARK_CARD <- "#16213e"
DARK_TEXT <- "#e8e8e8"
DARK_MUTED <- "#a0a0a0"

# Theme configuration - Dark mode with CRFB accents
app_theme <- bs_theme(
  bg = DARK_BG,
  fg = DARK_TEXT,
  primary = CRFB_LIGHT_BLUE,
  secondary = CRFB_GRAY,
  success = CRFB_TEAL,
  info = CRFB_LIGHT_BLUE,
  warning = CRFB_ORANGE,
  danger = CRFB_RED,
  base_font = font_google("Open Sans"),
  font_scale = 0.95,
  `enable-rounded` = TRUE
) |>
  bs_add_rules("
    .card { background-color: #16213e; border: 1px solid #2a3f5f; }
    .card-header { background-color: #1f3460 !important; border-bottom: 1px solid #2a3f5f; }
    .card-header.bg-primary { background-color: #003477 !important; }
    .card-header.bg-success { background-color: #0A81A8 !important; }
    .card-header.bg-danger { background-color: #EE3224 !important; }
    .card-header.bg-info { background-color: #2A368A !important; }
    .card-header.bg-secondary { background-color: #444444 !important; }
    .form-control, .form-select { background-color: #1f3460; border-color: #2a3f5f; color: #e8e8e8; }
    .form-control:focus, .form-select:focus { background-color: #1f3460; color: #e8e8e8; }
    .btn-primary { background-color: #F36107; border-color: #F36107; }
    .btn-primary:hover { background-color: #d45506; border-color: #d45506; }
    .text-muted { color: #a0a0a0 !important; }
    .nav-tabs .nav-link { color: #a0a0a0; }
    .nav-tabs .nav-link.active { background-color: #16213e; color: #9ACDFF; border-color: #2a3f5f #2a3f5f #16213e; }
  ")

# Chart theme for ggplot - Dark mode
chart_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = DARK_CARD, color = NA),
    panel.background = element_rect(fill = DARK_CARD, color = NA),
    plot.title = element_text(face = "bold", size = 14, color = CRFB_LIGHT_BLUE),
    plot.subtitle = element_text(color = DARK_MUTED, size = 10),
    legend.position = "right",
    legend.background = element_rect(fill = DARK_CARD, color = NA),
    legend.text = element_text(color = DARK_TEXT, size = 9),
    legend.title = element_text(color = CRFB_LIGHT_BLUE, size = 10),
    legend.key = element_rect(fill = DARK_CARD, color = NA),
    panel.grid.major = element_line(color = "#2a3f5f", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 11, color = DARK_MUTED),
    axis.text = element_text(size = 10, color = DARK_MUTED)
  )

# Color palette for charts (bright colors for dark background)
CHART_COLORS <- c(
  CRFB_LIGHT_BLUE, # Primary - Light Blue (visible on dark)
  CRFB_ORANGE,     # Secondary - Contrast Orange
  CRFB_TEAL,       # Third - Website Teal
  CRFB_RED,        # Fourth - FTD Red
  "#9b7ed9",       # Fifth - Lighter purple (more visible)
  "#50c878"        # Sixth - Emerald green
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
source("modules/mod_marginal.R")
