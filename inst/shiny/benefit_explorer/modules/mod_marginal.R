# =============================================================================
# Marginal Analysis Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
marginal_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main chart - NMTR by age
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Net Marginal Tax Rate by Working Year"),
        checkboxInput(ns("include_employer_marginal"), "Incl. Employer", value = FALSE, width = "120px")
      ),
      card_body(
        class = "p-2",
        plotOutput(ns("nmtr_chart"), height = "350px")
      )
    ),

    # Key metrics row
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_pia_bracket"))),
      column(3, uiOutput(ns("metric_mean_nmtr"))),
      column(3, uiOutput(ns("metric_mean_mirr"))),
      column(3, uiOutput(ns("metric_top35_years")))
    ),

    # Info and table toggles
    fluidRow(
      class = "mt-2",
      column(12,
        tags$div(
          class = "text-end",
          actionButton(ns("toggle_info"), "Info", icon = icon("info-circle"),
                       class = "btn-sm btn-outline-secondary me-1"),
          actionButton(ns("toggle_table"), "Data", icon = icon("table"),
                       class = "btn-sm btn-outline-secondary")
        )
      )
    ),

    # Collapsible info
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_info")),
      card(
        class = "mt-2",
        card_body(
          class = "p-2",
          tags$small(
            tags$strong("NMTR:"), " Net Marginal Tax Rate = (Tax - PV Marginal Benefits) / Earnings.", tags$br(),
            tags$strong("12.4%:"), " No benefit accrual (years outside top 35).", tags$br(),
            tags$strong("Near 0%:"), " Benefits roughly offset taxes.", tags$br(),
            tags$strong("Negative:"), " Benefits exceed taxes (subsidy for low earners).", tags$br(),
            tags$strong("PIA Bracket:"), " Marginal rate in PIA formula (90%, 32%, or 15%)."
          )
        )
      )
    ),

    # Collapsible table
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_table")),
      card(
        class = "mt-2",
        card_body(class = "p-2", DTOutput(ns("marginal_table")))
      )
    )
  )
}

# Module Server
marginal_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate marginal measures (baseline and reform)
    marginal_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      tryCatch({
        marginal <- marginal_benefit_analysis(primary, assumptions)
        nmtr <- net_marginal_tax_rate(primary, assumptions,
                                       include_employer = input$include_employer_marginal)
        mirr <- marginal_irr(primary, assumptions,
                             include_employer = input$include_employer_marginal)

        working_marginal <- marginal[marginal$age >= 21 & marginal$age <= 64, ]
        working_nmtr <- nmtr[nmtr$age >= 21 & nmtr$age <= 64, ]
        working_mirr <- mirr[mirr$age >= 21 & mirr$age <= 64, ]

        pia_rate <- unique(working_marginal$marginal_pia_rate[!is.na(working_marginal$marginal_pia_rate)])
        mean_nmtr <- mean(working_nmtr$net_marginal_tax_rate, na.rm = TRUE)
        mean_mirr_top35 <- mean(working_mirr$marginal_irr[working_mirr$in_top_35], na.rm = TRUE)
        n_top_35 <- sum(working_marginal$in_top_35, na.rm = TRUE)

        table_data <- data.frame(
          age = working_marginal$age,
          earnings = working_marginal$earnings,
          in_top_35 = working_marginal$in_top_35,
          indexed_rank = working_marginal$indexed_rank,
          delta_pv_benefits = working_marginal$delta_pv_benefits,
          net_marginal_tax_rate = working_nmtr$net_marginal_tax_rate,
          marginal_irr = working_mirr$marginal_irr
        )

        result <- list(
          marginal_pia_rate = pia_rate[1],
          mean_nmtr = mean_nmtr,
          mean_mirr_top35 = mean_mirr_top35,
          n_top_35 = n_top_35,
          table_data = table_data,
          working_nmtr = working_nmtr,
          has_reforms = FALSE
        )

        # Calculate reform marginal measures if enabled
        if (!is.null(data$has_reforms) && data$has_reforms && !is.null(data$reform_data)) {
          reform <- data$reform_data
          reform_assumptions <- if (!is.null(data$reform_assumptions)) data$reform_assumptions else assumptions

          reform_marginal <- marginal_benefit_analysis(reform, reform_assumptions)
          reform_nmtr <- net_marginal_tax_rate(reform, reform_assumptions,
                                                include_employer = input$include_employer_marginal)

          reform_working_marginal <- reform_marginal[reform_marginal$age >= 21 & reform_marginal$age <= 64, ]
          reform_working_nmtr <- reform_nmtr[reform_nmtr$age >= 21 & reform_nmtr$age <= 64, ]

          reform_mean_nmtr <- mean(reform_working_nmtr$net_marginal_tax_rate, na.rm = TRUE)
          reform_pia_rate <- unique(reform_working_marginal$marginal_pia_rate[!is.na(reform_working_marginal$marginal_pia_rate)])

          result$reform_mean_nmtr <- reform_mean_nmtr
          result$reform_pia_rate <- reform_pia_rate[1]
          result$reform_working_nmtr <- reform_working_nmtr
          result$reform_scenario <- unique(reform$scenario)[1]
          result$has_reforms <- TRUE
        }

        result
      }, error = function(e) NULL)
    })

    # NMTR chart by age with reform comparison
    output$nmtr_chart <- renderPlot({
      mdata <- marginal_data()
      if (is.null(mdata) || is.null(mdata$working_nmtr)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see marginal analysis",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      # If reforms are enabled, show comparison line chart
      if (mdata$has_reforms && !is.null(mdata$reform_working_nmtr)) {
        baseline_nmtr <- mdata$table_data %>%
          filter(!is.na(net_marginal_tax_rate)) %>%
          mutate(
            nmtr_pct = net_marginal_tax_rate * 100,
            scenario = "Baseline"
          ) %>%
          select(age, nmtr_pct, scenario)

        reform_nmtr <- data.frame(
          age = mdata$reform_working_nmtr$age,
          nmtr_pct = mdata$reform_working_nmtr$net_marginal_tax_rate * 100,
          scenario = mdata$reform_scenario
        )

        combined <- bind_rows(baseline_nmtr, reform_nmtr) %>%
          filter(!is.na(nmtr_pct))

        if (nrow(combined) == 0) return(NULL)

        p <- ggplot(combined, aes(x = age, y = nmtr_pct, color = scenario, linetype = scenario)) +
          geom_line(linewidth = 1.5) +
          geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
          geom_hline(yintercept = 12.4, color = CRFB_ORANGE, linewidth = 0.8, linetype = "dashed") +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          scale_x_continuous(breaks = seq(25, 65, by = 5)) +
          scale_color_manual(values = c("Baseline" = CRFB_LIGHT_BLUE,
                                         setNames(CRFB_TEAL, mdata$reform_scenario))) +
          scale_linetype_manual(values = c("Baseline" = "solid",
                                            setNames("dashed", mdata$reform_scenario))) +
          labs(x = "Age", y = "Net Marginal Tax Rate", color = NULL, linetype = NULL) +
          annotate("text", x = 63, y = 13.5, label = "12.4% (no accrual)",
                   color = CRFB_ORANGE, size = 3, hjust = 1) +
          chart_theme +
          theme(legend.position = "top")

      } else {
        # Standard bar chart colored by top 35
        nmtr_data <- mdata$table_data %>%
          filter(!is.na(net_marginal_tax_rate)) %>%
          mutate(
            nmtr_pct = net_marginal_tax_rate * 100,
            in_top_35_label = ifelse(in_top_35, "In Top 35", "Outside Top 35")
          )

        if (nrow(nmtr_data) == 0) return(NULL)

        p <- ggplot(nmtr_data, aes(x = age, y = nmtr_pct, fill = in_top_35_label)) +
          geom_col(width = 0.8, alpha = 0.85) +
          geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
          geom_hline(yintercept = 12.4, color = CRFB_ORANGE, linewidth = 0.8, linetype = "dashed") +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          scale_x_continuous(breaks = seq(25, 65, by = 5)) +
          scale_fill_manual(values = c("In Top 35" = CRFB_TEAL, "Outside Top 35" = CRFB_RED)) +
          labs(x = "Age", y = "Net Marginal Tax Rate", fill = NULL) +
          annotate("text", x = 63, y = 13.5, label = "12.4% (no accrual)",
                   color = CRFB_ORANGE, size = 3, hjust = 1) +
          chart_theme +
          theme(legend.position = "top")
      }

      p
    })

    # Compact metrics with reform comparison
    output$metric_pia_bracket <- renderUI({
      mdata <- marginal_data()
      if (is.null(mdata)) return(NULL)

      if (mdata$has_reforms) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Reform Scenario"),
          tags$strong(class = "text-warning", mdata$reform_scenario)
        )
      } else {
        bracket <- if (!is.na(mdata$marginal_pia_rate)) sprintf("%.0f%%", mdata$marginal_pia_rate * 100) else "N/A"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PIA Bracket"),
          tags$strong(class = "text-info", bracket)
        )
      }
    })

    output$metric_mean_nmtr <- renderUI({
      mdata <- marginal_data()
      if (is.null(mdata)) return(NULL)

      if (mdata$has_reforms && !is.na(mdata$reform_mean_nmtr)) {
        baseline_color <- if (!is.na(mdata$mean_nmtr) && mdata$mean_nmtr < 6.2) "text-success" else "text-warning"
        reform_color <- if (!is.na(mdata$reform_mean_nmtr) && mdata$reform_mean_nmtr < 6.2) "text-success" else "text-warning"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Mean NMTR"),
          tags$div(
            tags$span(class = "text-muted",
              if (!is.na(mdata$mean_nmtr)) sprintf("%.1f%%", mdata$mean_nmtr * 100) else "N/A"),
            tags$span(" → "),
            tags$strong(class = reform_color,
              if (!is.na(mdata$reform_mean_nmtr)) sprintf("%.1f%%", mdata$reform_mean_nmtr * 100) else "N/A")
          )
        )
      } else {
        nmtr_color <- if (!is.na(mdata$mean_nmtr) && mdata$mean_nmtr < 6.2) "text-success" else "text-warning"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Mean NMTR"),
          tags$strong(class = nmtr_color,
            if (!is.na(mdata$mean_nmtr)) sprintf("%.1f%%", mdata$mean_nmtr * 100) else "N/A"
          )
        )
      }
    })

    output$metric_mean_mirr <- renderUI({
      mdata <- marginal_data()
      if (is.null(mdata)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Marg IRR (Top 35)"),
        tags$strong(class = "text-success",
          if (!is.na(mdata$mean_mirr_top35)) sprintf("%.1f%%", mdata$mean_mirr_top35 * 100) else "N/A"
        )
      )
    })

    output$metric_top35_years <- renderUI({
      mdata <- marginal_data()
      if (is.null(mdata)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Years in Top 35"),
        tags$strong(mdata$n_top_35)
      )
    })

    # Data table
    output$marginal_table <- renderDT({
      mdata <- marginal_data()
      if (is.null(mdata) || is.null(mdata$table_data)) return(NULL)

      df <- mdata$table_data %>%
        mutate(
          earnings = round(earnings, 0),
          delta_pv_benefits = round(delta_pv_benefits * 100, 2),
          net_marginal_tax_rate = round(net_marginal_tax_rate * 100, 1),
          marginal_irr = ifelse(marginal_irr == -1, NA, round(marginal_irr * 100, 1))
        ) %>%
        rename(
          Age = age, Earnings = earnings, `Top 35` = in_top_35, Rank = indexed_rank,
          `Delta PV %` = delta_pv_benefits, `NMTR %` = net_marginal_tax_rate, `Marg IRR %` = marginal_irr
        )

      datatable(df,
        options = list(pageLength = 15, dom = 'tip', scrollX = TRUE),
        rownames = FALSE, class = "compact"
      ) %>%
        formatCurrency("Earnings", currency = "$", digits = 0) %>%
        formatStyle("Top 35", backgroundColor = styleEqual(c(TRUE, FALSE), c("#2d5a3d", "#5a2d2d")))
    })
  })
}
