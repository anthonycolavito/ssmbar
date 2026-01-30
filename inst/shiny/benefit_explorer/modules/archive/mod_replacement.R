# =============================================================================
# Replacement Rates Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
replacement_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main chart
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Replacement Rates"),
        tags$div(
          class = "d-flex align-items-center gap-2",
          selectInput(ns("rate_preset"), NULL,
            choices = c(
              "Key Rates (3)" = "key",
              "All Real" = "real",
              "All Wage-Indexed" = "wage",
              "Comprehensive" = "all"
            ),
            selected = "key", width = "150px"
          )
        )
      ),
      card_body(
        class = "p-2",
        plotOutput(ns("rr_chart"), height = "400px")
      )
    ),

    # Compact metrics and toggle
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_pv"))),
      column(3, uiOutput(ns("metric_wage35"))),
      column(3, uiOutput(ns("metric_real35"))),
      column(3,
        tags$div(
          class = "text-end",
          actionButton(ns("toggle_info"), "Info", icon = icon("info-circle"),
                       class = "btn-sm btn-outline-secondary me-1"),
          actionButton(ns("toggle_table"), "Data", icon = icon("table"),
                       class = "btn-sm btn-outline-secondary")
        )
      )
    ),

    # Collapsible info panel
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_info")),
      card(
        class = "mt-2",
        card_body(
          class = "p-2",
          tags$small(
            tags$strong("PV Annuity:"), " Benefit / constant payment with same PV as career earnings.", tags$br(),
            tags$strong("Wage-Indexed:"), " Earnings adjusted for wage growth (SSA method).", tags$br(),
            tags$strong("Real:"), " Earnings adjusted for inflation.", tags$br(),
            tags$strong("High-35:"), " Uses highest 35 years (matches AIME).", tags$br(),
            tags$strong("Last-N:"), " Uses final N years before claiming."
          )
        )
      )
    ),

    # Collapsible table
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_table")),
      card(
        class = "mt-2",
        card_body(class = "p-2", DTOutput(ns("rr_table")))
      )
    )
  )
}

# Module Server
replacement_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get selected rate types based on preset
    selected_types <- reactive({
      switch(input$rate_preset,
        "key" = c("pv_rr", "wage_h35", "real_h35"),
        "real" = c("real_all", "real_h35", "real_h10", "real_l10"),
        "wage" = c("wage_all", "wage_h35", "wage_h10", "wage_l10"),
        "all" = c("pv_rr", "real_h35", "real_l10", "wage_h35", "wage_l10")
      )
    })

    # Calculate replacement rates
    rr_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      required_cols <- c("id", "year", "age", "earnings", "annual_ind")
      if (!all(required_cols %in% names(primary))) return(NULL)

      claim_age <- unique(primary$claim_age)[1]
      ben_at_claim <- primary$annual_ind[primary$age == claim_age]
      if (length(ben_at_claim) == 0 || is.na(ben_at_claim[1]) || ben_at_claim[1] == 0) return(NULL)

      tryCatch({
        rr <- ssmbar:::rep_rates(primary, assumptions)
        rr$scenario <- "Baseline"

        if (!is.null(data$comparisons)) {
          for (sc_name in unique(data$comparisons$scenario)) {
            sc_data <- data$comparisons %>% filter(scenario == sc_name)
            if (nrow(sc_data) > 0 && "annual_ind" %in% names(sc_data)) {
              sc_claim_age <- unique(sc_data$claim_age)[1]
              sc_ben_claim <- sc_data$annual_ind[sc_data$age == sc_claim_age]
              if (length(sc_ben_claim) > 0 && !is.na(sc_ben_claim[1]) && sc_ben_claim[1] > 0) {
                sc_rr <- ssmbar:::rep_rates(sc_data, assumptions)
                sc_rr$scenario <- sc_name
                rr <- bind_rows(rr, sc_rr)
              }
            }
          }
        }
        rr
      }, error = function(e) NULL)
    })

    # Type labels
    type_labels <- c(
      "pv_rr" = "PV Annuity", "real_all" = "Real All", "wage_all" = "Wage All",
      "real_h35" = "Real H35", "wage_h35" = "Wage H35",
      "real_h10" = "Real H10", "wage_h10" = "Wage H10",
      "real_h5" = "Real H5", "wage_h5" = "Wage H5",
      "real_l35" = "Real L35", "wage_l35" = "Wage L35",
      "real_l10" = "Real L10", "wage_l10" = "Wage L10",
      "real_l5" = "Real L5", "wage_l5" = "Wage L5"
    )

    # Main chart
    output$rr_chart <- renderPlot({
      data <- rr_data()
      if (is.null(data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see replacement rates",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      data_filtered <- data %>%
        filter(type %in% selected_types()) %>%
        mutate(type_label = type_labels[type])

      if (nrow(data_filtered) == 0) return(NULL)

      n_scenarios <- length(unique(data_filtered$scenario))

      p <- ggplot(data_filtered, aes(x = type_label, y = rep_rate, fill = scenario)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_text(aes(label = scales::percent(rep_rate, accuracy = 0.1)),
                  position = position_dodge(width = 0.8),
                  vjust = -0.5, size = 3.5, color = DARK_TEXT) +
        scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
        scale_fill_manual(values = CHART_COLORS) +
        labs(x = NULL, y = "Replacement Rate", fill = NULL) +
        chart_theme +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
          legend.position = if (n_scenarios > 1) "top" else "none"
        )

      p
    })

    # Compact metrics
    output$metric_pv <- renderUI({
      data <- rr_data()
      if (is.null(data)) return(NULL)
      pv_val <- data$rep_rate[data$type == "pv_rr" & data$scenario == "Baseline"][1]
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "PV Annuity"),
        tags$strong(class = "text-info", scales::percent(pv_val, accuracy = 0.1))
      )
    })

    output$metric_wage35 <- renderUI({
      data <- rr_data()
      if (is.null(data)) return(NULL)
      val <- data$rep_rate[data$type == "wage_h35" & data$scenario == "Baseline"][1]
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Wage H35"),
        tags$strong(class = "text-info", scales::percent(val, accuracy = 0.1))
      )
    })

    output$metric_real35 <- renderUI({
      data <- rr_data()
      if (is.null(data)) return(NULL)
      val <- data$rep_rate[data$type == "real_h35" & data$scenario == "Baseline"][1]
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Real H35"),
        tags$strong(class = "text-info", scales::percent(val, accuracy = 0.1))
      )
    })

    # Table
    output$rr_table <- renderDT({
      data <- rr_data()
      if (is.null(data)) return(NULL)

      table_data <- data %>%
        mutate(type_label = type_labels[type]) %>%
        select(scenario, type_label, rep_rate) %>%
        pivot_wider(names_from = scenario, values_from = rep_rate)

      datatable(table_data,
        options = list(pageLength = 15, dom = 't', ordering = FALSE),
        rownames = FALSE, colnames = c("Measure" = "type_label")
      ) %>%
        formatPercentage(names(table_data)[-1], digits = 1)
    })
  })
}
