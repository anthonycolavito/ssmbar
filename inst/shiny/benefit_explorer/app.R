# =============================================================================
# Benefit Explorer App - Main App File
# =============================================================================

source("global.R")

# =============================================================================
# UI Definition
# =============================================================================

ui <- page_navbar(
  title = APP_TITLE,
  theme = app_theme,
  id = "main_nav",

  # Sidebar for worker configuration
  sidebar = sidebar(
    width = 320,
    title = "Worker Configuration",

    # Worker input module
    worker_input_ui("worker")
  ),

  # Main content tabs
  nav_panel(
    title = "Benefits",
    icon = icon("chart-line"),
    benefits_ui("benefits")
  ),

  nav_panel(
    title = "Replacement Rates",
    icon = icon("percent"),
    replacement_ui("replacement")
  ),

  nav_panel(
    title = "Lifetime Value",
    icon = icon("piggy-bank"),
    lifetime_ui("lifetime")
  ),

  nav_panel(
    title = "Ratios",
    icon = icon("scale-balanced"),
    ratios_ui("ratios")
  ),

  # Footer with package info
  nav_spacer(),
  nav_item(
    tags$span(
      class = "text-muted small",
      paste0("ssmbar v", packageVersion("ssmbar"), " | Data: 2025 Trustees Report")
    )
  )
)

# =============================================================================
# Server Logic
# =============================================================================

server <- function(input, output, session) {

  # Worker input module - returns reactive with calculated benefits
  worker_data <- worker_input_server("worker")

  # Pass worker data to visualization modules
  benefits_server("benefits", worker_data)
  replacement_server("replacement", worker_data)
  lifetime_server("lifetime", worker_data)
  ratios_server("ratios", worker_data)

}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui = ui, server = server)
