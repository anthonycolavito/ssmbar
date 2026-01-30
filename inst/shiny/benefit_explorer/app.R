# =============================================================================
# Benefit Explorer App - Main App File (Redesigned 2-Tab Architecture)
# =============================================================================

source("global.R")

# =============================================================================
# UI Definition
# =============================================================================

ui <- page_navbar(
  title = APP_TITLE,
  theme = app_theme,
  id = "main_nav",

  # Sidebar with shared reform selector
  sidebar = sidebar(
    width = 300,
    title = "Reform Comparison",

    # Shared reform selector (mandatory)
    reform_selector_ui("reforms"),

    # Divider
    tags$hr(class = "my-3"),

    # App info
    tags$div(
      class = "text-muted small",
      tags$p(
        icon("info-circle"), " ",
        "Select reforms above to compare against baseline. ",
        "All visualizations show baseline vs reform comparison."
      ),
      tags$p(
        class = "mt-2",
        icon("user"), " ",
        tags$strong("Individual Tab: "),
        "Configure a specific worker and see detailed benefits/marginal analysis."
      ),
      tags$p(
        class = "mt-2",
        icon("users"), " ",
        tags$strong("Cohort Tab: "),
        "Compare metrics across birth years (1960-2005)."
      )
    )
  ),

  # Tab 1: Individual Worker
  nav_panel(
    title = "Individual Worker",
    icon = icon("user"),
    individual_tab_ui("individual")
  ),

  # Tab 2: Cohort Comparison
  nav_panel(
    title = "Cohort Comparison",
    icon = icon("users"),
    cohort_tab_ui("cohort")
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

  # Shared reform selector state
  reform_state <- reform_selector_server("reforms")

  # Tab servers receive shared reform state
  individual_tab_server("individual", reform_state)
  cohort_tab_server("cohort", reform_state)

}

# =============================================================================
# Run App
# =============================================================================

shinyApp(ui = ui, server = server)
