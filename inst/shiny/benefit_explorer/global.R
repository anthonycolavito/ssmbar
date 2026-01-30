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

# =============================================================================
# Reform Configuration
# =============================================================================

# Available reforms organized by category
# Each reform is a function that creates a Reform object
AVAILABLE_REFORMS <- list(
  "Benefit Changes" = list(
    "5% Benefit Cut" = list(
      fn = function() reform_reduce_benefits(0.95, 2030),
      desc = "Reduce all benefits by 5%"
    ),
    "10% Benefit Cut" = list(
      fn = function() reform_reduce_benefits(0.90, 2030),
      desc = "Reduce all benefits by 10%"
    ),
    "Reduce Fact3 to 5%" = list(
      fn = function() reform_reduce_fact3(0.05, 2030),
      desc = "Reduce top PIA bracket from 15% to 5%",
      group = "pia_formula"
    ),
    "Flat Benefit" = list(
      fn = function() reform_flat_benefit(19300/12, 2030, 25),
      desc = "Phase in flat benefit replacing formula",
      group = "pia_formula"
    ),
    "Simpson-Bowles PIA" = list(
      fn = function() reform_simpson_bowles(2030, 10),
      desc = "4-bracket PIA (30/10/5% phase-in)",
      group = "pia_formula"
    ),
    "Mini-PIA" = list(
      fn = function() reform_mini_pia(2030, 10),
      desc = "Average yearly PIAs instead of AIME",
      group = "pia_formula"
    )
  ),
  "NRA Changes" = list(
    "Raise NRA to 68" = list(
      fn = function() reform_nra_to_68(2030),
      desc = "Gradually raise Normal Retirement Age to 68",
      group = "nra"
    ),
    "Index NRA to Longevity" = list(
      fn = function() reform_index_nra(2030),
      desc = "Index NRA to life expectancy (no cap)",
      group = "nra"
    ),
    "NRA to 69, then Index" = list(
      fn = function() reform_nra_to_69_index(2030),
      desc = "Raise to 69, then index to longevity",
      group = "nra"
    )
  ),
  "COLA Indexing" = list(
    "Chained CPI" = list(
      fn = function() reform_chained_cpi(2030),
      desc = "Index COLAs to Chained CPI (-0.3pp)",
      group = "cola"
    ),
    "Cap COLAs at Median" = list(
      fn = function() reform_cola_cap(2030),
      desc = "Cap dollar COLA increase at median PIA",
      group = "cola"
    ),
    "CPI-E (Higher)" = list(
      fn = function() reform_cpi_e(2030),
      desc = "Index COLAs to CPI-E (+0.2pp)",
      group = "cola"
    )
  ),
  "Tax Changes" = list(
    "90% Coverage + 5% Credit" = list(
      fn = function() reform_taxmax_90_pct(2030),
      desc = "Raise taxmax to cover 90% of earnings",
      group = "taxmax"
    ),
    "Eliminate Taxmax + 15% Credit" = list(
      fn = function() reform_eliminate_taxmax(2030),
      desc = "Eliminate taxmax with 15% benefit credit",
      group = "taxmax"
    ),
    "Eliminate Taxmax, No Credit" = list(
      fn = function() reform_eliminate_taxmax_no_credit(2030),
      desc = "Eliminate taxmax for taxes only",
      group = "taxmax"
    ),
    "Increase Tax Rate +1pp" = list(
      fn = function() reform_change_tax_rate(1.0, 2030),
      desc = "Increase payroll tax rate by 1 percentage point"
    ),
    "Increase Tax Rate +2pp" = list(
      fn = function() reform_change_tax_rate(2.0, 2030),
      desc = "Increase payroll tax rate by 2 percentage points"
    )
  ),
  "Benefit Enhancements" = list(
    "Basic Minimum Benefit" = list(
      fn = function() reform_basic_minimum(900, 1342, 2030),
      desc = "Add minimum benefit floor ($900/$1,342)"
    ),
    "Child Care Credit" = list(
      fn = function() reform_child_care_credit(2030, 5),
      desc = "Credit earnings for child care years"
    ),
    "75% Widow Benefit" = list(
      fn = function() reform_widow_75_pct(2030),
      desc = "75% of combined benefits for widows"
    )
  ),
  "Other Reforms" = list(
    "40-Year Averaging" = list(
      fn = function() reform_40_year_averaging(2030),
      desc = "Phase out dropout years (35 to 40 year average)"
    ),
    "Repeal RET" = list(
      fn = function() reform_repeal_ret(2030),
      desc = "Repeal the Retirement Earnings Test"
    ),
    "Phase Out Spousal" = list(
      fn = function() reform_phase_out_spousal(2030),
      desc = "Phase out spousal benefits over 10 years"
    )
  )
)

# Flatten reform list for easy lookup
REFORM_LOOKUP <- list()
for (category in names(AVAILABLE_REFORMS)) {
  for (reform_name in names(AVAILABLE_REFORMS[[category]])) {
    REFORM_LOOKUP[[reform_name]] <- AVAILABLE_REFORMS[[category]][[reform_name]]
  }
}

# Get all reform names as choices for UI
get_reform_choices <- function() {
  choices <- list()
  for (category in names(AVAILABLE_REFORMS)) {
    choices[[category]] <- names(AVAILABLE_REFORMS[[category]])
  }
  choices
}

# Check for mutual exclusivity conflicts
check_ui_exclusivity <- function(selected_reforms) {
  groups <- list(
    nra = c("Raise NRA to 68", "Index NRA to Longevity", "NRA to 69, then Index"),
    cola = c("Chained CPI", "Cap COLAs at Median", "CPI-E (Higher)"),
    taxmax = c("90% Coverage + 5% Credit", "Eliminate Taxmax + 15% Credit", "Eliminate Taxmax, No Credit"),
    pia_formula = c("Reduce Fact3 to 5%", "Flat Benefit", "Simpson-Bowles PIA", "Mini-PIA")
  )

  conflicts <- list()
  for (group_name in names(groups)) {
    group_reforms <- groups[[group_name]]
    selected_in_group <- intersect(selected_reforms, group_reforms)
    if (length(selected_in_group) > 1) {
      conflicts[[group_name]] <- selected_in_group
    }
  }
  conflicts
}

# =============================================================================
# Load Shiny modules
# =============================================================================

source("modules/mod_worker_input.R")
source("modules/mod_benefits.R")
source("modules/mod_replacement.R")
source("modules/mod_lifetime.R")
source("modules/mod_ratios.R")
source("modules/mod_marginal.R")
source("modules/mod_reform_summary.R")
