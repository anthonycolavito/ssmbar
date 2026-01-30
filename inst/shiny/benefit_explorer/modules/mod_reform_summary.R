# =============================================================================
# Reform Summary Module
# =============================================================================
# Displays comparison dashboard for policy reform scenarios

# Module UI
reform_summary_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12, 6, 6),

    # Top row - summary comparison table
    card(
      card_header(
        class = "bg-warning text-dark",
        "Reform Comparison Summary"
      ),
      card_body(
        conditionalPanel(
          condition = sprintf("!output['%s']", ns("has_reforms")),
          tags$div(
            class = "text-center py-4",
            tags$p(class = "text-muted",
                   "Enable 'Compare Reforms' in the sidebar and select reforms to see comparison.")
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("has_reforms")),
          DTOutput(ns("summary_table"))
        )
      )
    ),

    # Bottom left - metric comparison bar chart
    card(
      card_header("Key Metrics by Scenario"),
      card_body(
        selectInput(
          ns("metric_select"),
          NULL,
          choices = c(
            "Monthly Benefit at Claim" = "monthly_benefit",
            "PV Lifetime Benefits" = "pv_benefits",
            "PV Lifetime Taxes" = "pv_taxes",
            "Benefit-Tax Ratio" = "benefit_tax_ratio"
          ),
          selected = "monthly_benefit"
        ),
        plotOutput(ns("metric_chart"), height = "320px")
      )
    ),

    # Bottom right - percent change chart
    card(
      card_header("Percent Change from Baseline"),
      card_body(
        selectInput(
          ns("change_metric"),
          NULL,
          choices = c(
            "Monthly Benefit" = "pct_change_benefit",
            "PV Lifetime Benefits" = "pct_change_pv"
          ),
          selected = "pct_change_pv"
        ),
        plotOutput(ns("change_chart"), height = "320px")
      )
    ),

    # Additional row - benefit trajectory comparison
    card(
      card_header("Benefit Trajectory Comparison"),
      card_body(
        radioButtons(
          ns("trajectory_type"),
          NULL,
          choices = c(
            "Nominal Dollars" = "nominal",
            "Real Dollars (2025)" = "real"
          ),
          selected = "nominal",
          inline = TRUE
        ),
        plotOutput(ns("trajectory_chart"), height = "350px")
      )
    )
  )
}

# Module Server
reform_summary_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Output flag for conditional panels
    output$has_reforms <- reactive({
      data <- worker_data()
      !is.null(data) && !is.null(data$has_reforms) && data$has_reforms
    })
    outputOptions(output, "has_reforms", suspendWhenHidden = FALSE)

    # Get reform summary data
    reform_summary <- reactive({
      data <- worker_data()
      if (is.null(data) || !data$has_reforms || is.null(data$reform_summary)) {
        return(NULL)
      }
      data$reform_summary
    })

    # Summary comparison table
    output$summary_table <- renderDT({
      summary_data <- reform_summary()
      if (is.null(summary_data)) return(NULL)

      # Format for display
      display_data <- summary_data %>%
        mutate(
          monthly_benefit = paste0("$", format(round(monthly_benefit), big.mark = ",")),
          pv_benefits = paste0("$", format(round(pv_benefits), big.mark = ",")),
          pv_taxes = paste0("$", format(round(pv_taxes), big.mark = ",")),
          benefit_tax_ratio = round(benefit_tax_ratio, 2),
          pct_change_benefit = ifelse(
            scenario == "Baseline", "-",
            paste0(ifelse(pct_change_benefit >= 0, "+", ""), round(pct_change_benefit, 1), "%")
          ),
          pct_change_pv = ifelse(
            scenario == "Baseline", "-",
            paste0(ifelse(pct_change_pv >= 0, "+", ""), round(pct_change_pv, 1), "%")
          )
        ) %>%
        rename(
          Scenario = scenario,
          `Monthly Benefit` = monthly_benefit,
          `PV Benefits` = pv_benefits,
          `PV Taxes` = pv_taxes,
          `Benefit/Tax Ratio` = benefit_tax_ratio,
          `Benefit Change` = pct_change_benefit,
          `PV Change` = pct_change_pv
        )

      datatable(
        display_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't',  # Table only, no search/pagination
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    # Metric comparison bar chart
    output$metric_chart <- renderPlot({
      summary_data <- reform_summary()
      if (is.null(summary_data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No reform data available",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      metric <- input$metric_select
      metric_label <- switch(
        metric,
        "monthly_benefit" = "Monthly Benefit at Claim ($)",
        "pv_benefits" = "Present Value of Lifetime Benefits ($)",
        "pv_taxes" = "Present Value of Lifetime Taxes ($)",
        "benefit_tax_ratio" = "Benefit-to-Tax Ratio"
      )

      # Order scenarios: Baseline first, then reforms
      summary_data$scenario <- factor(
        summary_data$scenario,
        levels = c("Baseline", setdiff(unique(summary_data$scenario), "Baseline"))
      )

      # Create color vector
      n_scenarios <- nrow(summary_data)
      colors <- c(CRFB_LIGHT_BLUE, CHART_COLORS[2:min(n_scenarios, length(CHART_COLORS))])

      p <- ggplot(summary_data, aes(x = scenario, y = .data[[metric]], fill = scenario)) +
        geom_col(width = 0.7) +
        scale_fill_manual(values = colors, guide = "none") +
        scale_y_continuous(
          labels = if (metric == "benefit_tax_ratio") {
            scales::number_format(accuracy = 0.01)
          } else {
            scales::dollar_format()
          },
          expand = expansion(mult = c(0, 0.1))
        ) +
        labs(
          title = metric_label,
          x = NULL,
          y = NULL
        ) +
        chart_theme +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        )

      p
    })

    # Percent change chart
    output$change_chart <- renderPlot({
      summary_data <- reform_summary()
      if (is.null(summary_data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No reform data available",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      metric <- input$change_metric
      metric_label <- switch(
        metric,
        "pct_change_benefit" = "% Change in Monthly Benefit",
        "pct_change_pv" = "% Change in PV Lifetime Benefits"
      )

      # Filter out baseline (0% change)
      plot_data <- summary_data %>%
        filter(scenario != "Baseline") %>%
        mutate(
          scenario = factor(scenario, levels = scenario),
          is_positive = .data[[metric]] >= 0
        )

      if (nrow(plot_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Select reforms to compare",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      p <- ggplot(plot_data, aes(x = scenario, y = .data[[metric]], fill = is_positive)) +
        geom_col(width = 0.7) +
        geom_hline(yintercept = 0, linetype = "solid", color = DARK_MUTED, linewidth = 0.5) +
        scale_fill_manual(
          values = c("TRUE" = CRFB_TEAL, "FALSE" = CRFB_RED),
          guide = "none"
        ) +
        scale_y_continuous(
          labels = scales::percent_format(scale = 1),
          expand = expansion(mult = c(0.1, 0.1))
        ) +
        labs(
          title = metric_label,
          subtitle = "Relative to Baseline (Current Law)",
          x = NULL,
          y = NULL
        ) +
        chart_theme +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
        )

      p
    })

    # Benefit trajectory comparison chart
    output$trajectory_chart <- renderPlot({
      data <- worker_data()
      if (is.null(data) || !data$has_reforms) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No reform data available",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      primary <- data$primary
      reform_data <- data$reform_data
      assumptions <- data$assumptions

      # Calculate real benefits
      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      # Join gdp_pi if not present
      if (!"gdp_pi" %in% names(primary)) {
        primary <- primary %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      primary <- primary %>%
        mutate(
          annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
          annual_nominal = annual_ind,
          scenario = "Baseline"
        )

      if (!"gdp_pi" %in% names(reform_data)) {
        reform_data <- reform_data %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      reform_data <- reform_data %>%
        mutate(
          annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
          annual_nominal = annual_ind
        )

      # Combine data
      combined <- bind_rows(primary, reform_data)

      # Filter to benefit-receiving years
      death_age <- unique(primary$death_age)[1]
      plot_data <- combined %>%
        filter(annual_ind > 0 & age < death_age)

      if (nrow(plot_data) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No benefits to display",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      y_var <- if (input$trajectory_type == "nominal") "annual_nominal" else "annual_real"
      y_label <- if (input$trajectory_type == "nominal") {
        "Annual Benefit (Nominal $)"
      } else {
        "Annual Benefit (2025 $)"
      }

      # Create color vector for scenarios
      unique_scenarios <- unique(plot_data$scenario)
      n_scenarios <- length(unique_scenarios)
      colors <- CHART_COLORS[1:min(n_scenarios, length(CHART_COLORS))]
      names(colors) <- unique_scenarios

      p <- ggplot(plot_data, aes(x = age, y = .data[[y_var]],
                                  color = scenario, group = scenario)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 1.5, alpha = 0.7) +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_x_continuous(breaks = seq(60, 100, by = 5)) +
        scale_color_manual(values = colors) +
        labs(
          title = "Annual Benefits by Age: Baseline vs Reforms",
          subtitle = if (input$trajectory_type == "nominal") {
            "Nominal dollars including COLA adjustments"
          } else {
            "Real dollars (deflated to 2025 using GDP price index)"
          },
          x = "Age",
          y = y_label,
          color = "Scenario"
        ) +
        chart_theme +
        theme(
          legend.position = "bottom",
          legend.direction = "horizontal"
        )

      p
    })

  })
}
