# =============================================================================
# Reform Summary Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
reform_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main comparison chart
    card(
      card_header(
        class = "bg-warning text-dark d-flex justify-content-between align-items-center",
        tags$span("Reform Impact Comparison"),
        selectInput(ns("metric_select"), NULL,
          choices = c(
            "Monthly Benefit" = "monthly_benefit",
            "PV Lifetime Benefits" = "pv_benefits",
            "Benefit-Tax Ratio" = "benefit_tax_ratio"
          ),
          selected = "pv_benefits", width = "180px"
        )
      ),
      card_body(
        class = "p-2",
        conditionalPanel(
          condition = sprintf("!output['%s']", ns("has_reforms")),
          tags$div(
            class = "text-center py-5",
            tags$p(class = "text-muted", "Enable 'Compare Reforms' in the sidebar to see comparisons.")
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("has_reforms")),
          plotOutput(ns("comparison_chart"), height = "380px")
        )
      )
    ),

    # Percent change chart
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_reforms")),
      card(
        class = "mt-2",
        card_header(class = "bg-secondary text-white", "Percent Change from Baseline"),
        card_body(
          class = "p-2",
          plotOutput(ns("change_chart"), height = "250px")
        )
      )
    ),

    # Summary table toggle
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_reforms")),
      fluidRow(
        class = "mt-2",
        column(12,
          tags$div(
            class = "text-end",
            actionButton(ns("toggle_table"), "Show Summary Table", icon = icon("table"),
                         class = "btn-sm btn-outline-secondary")
          )
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_table")),
        card(
          class = "mt-2",
          card_body(class = "p-2", DTOutput(ns("summary_table")))
        )
      )
    )
  )
}

# Module Server
reform_summary_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$has_reforms <- reactive({
      data <- worker_data()
      !is.null(data) && !is.null(data$has_reforms) && data$has_reforms
    })
    outputOptions(output, "has_reforms", suspendWhenHidden = FALSE)

    reform_summary <- reactive({
      data <- worker_data()
      if (is.null(data) || !data$has_reforms || is.null(data$reform_summary)) return(NULL)
      data$reform_summary
    })

    # Main comparison bar chart
    output$comparison_chart <- renderPlot({
      summary_data <- reform_summary()
      if (is.null(summary_data)) return(NULL)

      metric <- input$metric_select
      metric_label <- switch(metric,
        "monthly_benefit" = "Monthly Benefit ($)",
        "pv_benefits" = "PV Lifetime Benefits ($)",
        "benefit_tax_ratio" = "Benefit-Tax Ratio"
      )

      summary_data$scenario <- factor(summary_data$scenario,
        levels = c("Baseline", setdiff(unique(summary_data$scenario), "Baseline")))

      n_scenarios <- nrow(summary_data)
      colors <- c(CRFB_LIGHT_BLUE, CHART_COLORS[2:min(n_scenarios, length(CHART_COLORS))])

      # Format y-axis based on metric
      y_formatter <- if (metric == "benefit_tax_ratio") {
        scales::number_format(accuracy = 0.01)
      } else {
        scales::dollar_format()
      }

      p <- ggplot(summary_data, aes(x = scenario, y = .data[[metric]], fill = scenario)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = if (metric == "benefit_tax_ratio") {
          sprintf("%.2f", .data[[metric]])
        } else {
          paste0("$", format(round(.data[[metric]]), big.mark = ","))
        }),
        vjust = -0.5, size = 3.5, color = DARK_TEXT) +
        scale_fill_manual(values = colors, guide = "none") +
        scale_y_continuous(labels = y_formatter, expand = expansion(mult = c(0, 0.15))) +
        labs(x = NULL, y = metric_label) +
        chart_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

      p
    })

    # Percent change chart
    output$change_chart <- renderPlot({
      summary_data <- reform_summary()
      if (is.null(summary_data)) return(NULL)

      plot_data <- summary_data %>%
        filter(scenario != "Baseline") %>%
        mutate(
          scenario = factor(scenario, levels = scenario),
          is_positive = pct_change_pv >= 0
        )

      if (nrow(plot_data) == 0) return(NULL)

      p <- ggplot(plot_data, aes(x = scenario, y = pct_change_pv, fill = is_positive)) +
        geom_col(width = 0.6) +
        geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
        geom_text(aes(label = sprintf("%+.1f%%", pct_change_pv)),
                  vjust = ifelse(plot_data$pct_change_pv >= 0, -0.5, 1.5),
                  size = 3.5, color = DARK_TEXT) +
        scale_fill_manual(values = c("TRUE" = CRFB_TEAL, "FALSE" = CRFB_RED), guide = "none") +
        scale_y_continuous(labels = function(x) paste0(x, "%"),
                           expand = expansion(mult = c(0.15, 0.15))) +
        labs(x = NULL, y = "% Change in PV Benefits") +
        chart_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))

      p
    })

    # Summary table
    output$summary_table <- renderDT({
      summary_data <- reform_summary()
      if (is.null(summary_data)) return(NULL)

      display_data <- summary_data %>%
        mutate(
          monthly_benefit = paste0("$", format(round(monthly_benefit), big.mark = ",")),
          pv_benefits = paste0("$", format(round(pv_benefits), big.mark = ",")),
          pv_taxes = paste0("$", format(round(pv_taxes), big.mark = ",")),
          benefit_tax_ratio = round(benefit_tax_ratio, 2),
          pct_change_pv = ifelse(scenario == "Baseline", "-",
            sprintf("%+.1f%%", pct_change_pv))
        ) %>%
        select(scenario, monthly_benefit, pv_benefits, benefit_tax_ratio, pct_change_pv) %>%
        rename(
          Scenario = scenario, `Monthly Ben` = monthly_benefit,
          `PV Benefits` = pv_benefits, Ratio = benefit_tax_ratio, `% Change` = pct_change_pv
        )

      datatable(display_data,
        options = list(pageLength = 10, dom = 't', ordering = FALSE),
        rownames = FALSE, class = "compact"
      )
    })
  })
}
