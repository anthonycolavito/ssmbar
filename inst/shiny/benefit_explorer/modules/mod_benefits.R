# =============================================================================
# Benefits Module - Redesigned for cleaner UX
# =============================================================================

# Beneficiary class labels
BC_LABELS <- c(
  "AR" = "Retired Worker", "ARB" = "Retired + Spousal", "ARD" = "Retired + Widow(er)",
  "ARF" = "Retired + Disabled Widow(er)", "AD" = "Disabled Worker",
  "ADB" = "Disabled + Spousal", "ADD" = "Disabled + Widow(er)",
  "ADF" = "Disabled + Disabled Widow(er)", "BR" = "Spouse Only (Retired)",
 "BD" = "Spouse Only (Disabled)", "D" = "Widow(er) Only", "F" = "Disabled Widow(er) Only"
)

# Module UI
benefits_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main chart - full width, prominent
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Annual Benefits by Age"),
        tags$div(
          radioButtons(
            ns("chart_type"), NULL,
            choices = c("Nominal $" = "nominal", "Real 2025 $" = "real"),
            selected = "nominal", inline = TRUE
          )
        )
      ),
      card_body(
        class = "p-2",
        plotOutput(ns("benefit_chart"), height = "420px")
      )
    ),

    # Compact metrics row
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_claim"))),
      column(3, uiOutput(ns("metric_age70"))),
      column(3, uiOutput(ns("metric_pv"))),
      column(3,
        tags$div(
          class = "text-end",
          actionButton(ns("toggle_table"), "Show Data", icon = icon("table"),
                       class = "btn-sm btn-outline-secondary")
        )
      )
    ),

    # Collapsible data table
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_table")),
      card(
        class = "mt-2",
        card_body(
          class = "p-2",
          div(
            class = "d-flex justify-content-between mb-2",
            downloadButton(ns("download_data"), "Export CSV", class = "btn-sm btn-outline-primary"),
            actionButton(ns("toggle_table2"), "Hide", icon = icon("times"), class = "btn-sm btn-outline-secondary")
          ),
          DTOutput(ns("benefit_table"))
        )
      )
    )
  )
}

# Module Server
benefits_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare chart data
    chart_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      if (!"gdp_pi" %in% names(primary)) {
        primary <- primary %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      primary <- primary %>%
        mutate(
          annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
          annual_nominal = annual_ind
        )

      if (!is.null(data$comparisons)) {
        comparisons <- data$comparisons
        if (!"gdp_pi" %in% names(comparisons)) {
          comparisons <- comparisons %>%
            left_join(assumptions %>% select(year, gdp_pi), by = "year")
        }
        comparisons <- comparisons %>%
          mutate(
            annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
            annual_nominal = annual_ind
          )
        primary <- bind_rows(primary, comparisons)
      }

      primary
    })

    # Main benefit chart
    output$benefit_chart <- renderPlot({
      data <- chart_data()
      if (is.null(data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see benefits",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      data_filtered <- data %>%
        filter(annual_ind > 0 & age < death_age)

      if (nrow(data_filtered) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No benefits to display",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      y_var <- if (input$chart_type == "nominal") "annual_nominal" else "annual_real"
      y_label <- if (input$chart_type == "nominal") "Annual Benefit ($)" else "Annual Benefit (2025 $)"

      data_filtered <- data_filtered %>%
        mutate(bc_label = if ("bc" %in% names(.)) bc else "AR")

      unique_scenarios <- unique(data_filtered$scenario)
      show_legend <- length(unique_scenarios) > 1

      # Get baseline data for BC transitions
      baseline_data <- data_filtered %>% filter(scenario == "Baseline" | scenario == unique_scenarios[1])

      p <- ggplot(data_filtered, aes(x = age, y = .data[[y_var]],
                                      color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 2.5, alpha = 0.8) +
        scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.02, 0.1))) +
        scale_x_continuous(breaks = seq(60, 100, by = 5)) +
        scale_color_manual(values = CHART_COLORS) +
        labs(x = "Age", y = y_label, color = NULL) +
        chart_theme +
        theme(
          legend.position = if (show_legend) "top" else "none",
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(10, 15, 10, 10)
        )

      p
    })

    # Compact metrics
    output$metric_claim <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      claim_age <- unique(primary$claim_age)[1]
      ben_at_claim <- primary$annual_ind[primary$age == claim_age]

      tags$div(
        class = "text-center p-2 rounded",
        style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", paste0("At Claim (", claim_age, ")")),
        tags$strong(class = "text-info", format_currency(ben_at_claim))
      )
    })

    output$metric_age70 <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      ben_at_70 <- if (70 <= max(primary$age)) primary$annual_ind[primary$age == 70] else NA

      tags$div(
        class = "text-center p-2 rounded",
        style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "At Age 70"),
        tags$strong(class = "text-info", if (!is.na(ben_at_70)) format_currency(ben_at_70) else "N/A")
      )
    })

    output$metric_pv <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      pv_ben <- tryCatch({
        pv_lifetime_benefits(data$primary, data$assumptions)$pv_benefits[1]
      }, error = function(e) NA_real_)

      tags$div(
        class = "text-center p-2 rounded",
        style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "PV Lifetime"),
        tags$strong(class = "text-success", format_currency(pv_ben))
      )
    })

    # Data table
    output$benefit_table <- renderDT({
      data <- chart_data()
      if (is.null(data)) return(NULL)

      table_data <- data %>%
        filter(annual_ind > 0 & age < death_age) %>%
        mutate(
          bc_display = if ("bc" %in% names(.)) BC_LABELS[bc] else "Retired Worker",
          annual_nominal = round(annual_nominal, 0),
          annual_real = round(annual_real, 0)
        ) %>%
        select(scenario, year, age, bc_display, annual_nominal, annual_real) %>%
        rename(
          Scenario = scenario, Year = year, Age = age, Class = bc_display,
          `Nominal` = annual_nominal, `Real 2025` = annual_real
        )

      datatable(table_data,
        options = list(pageLength = 15, scrollX = TRUE, dom = 'tip'),
        rownames = FALSE, class = "compact"
      ) %>%
        formatCurrency(c("Nominal", "Real 2025"), currency = "$", digits = 0)
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() paste0("benefits_", Sys.Date(), ".csv"),
      content = function(file) {
        data <- chart_data()
        if (!is.null(data)) {
          export_data <- data %>%
            filter(annual_ind > 0 & age < death_age) %>%
            select(scenario, year, age, annual_nominal, annual_real)
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )
  })
}
