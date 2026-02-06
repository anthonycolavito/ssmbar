# =============================================================================
# Cohort Comparison Tab Module - Birth year analysis with multiple charts
# =============================================================================

# Module UI
cohort_tab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Configuration row
    fluidRow(
      column(12,
        card(
          card_header(
            class = "bg-primary text-white py-2",
            tags$span("Cohort Configuration")
          ),
          card_body(
            class = "p-3",
            fluidRow(
              column(2,
                selectInput(ns("worker_type"), "Worker Type",
                            choices = WORKER_TYPES,
                            selected = "medium")
              ),
              column(2,
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'custom'", ns("worker_type")),
                  numericInput(ns("custom_earnings"), "Avg Earnings ($)",
                               value = 50000, min = 10000, max = 500000, step = 5000)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] != 'custom'", ns("worker_type")),
                  tags$div(class = "form-group", style = "visibility: hidden;",
                    numericInput(ns("placeholder"), "Placeholder", value = 0)
                  )
                )
              ),
              column(2,
                selectInput(ns("sex"), "Sex", choices = SEX_OPTIONS, selected = "all")
              ),
              column(2,
                numericInput(ns("claim_age"), "Claim Age",
                             value = 65, min = CLAIM_AGE_MIN, max = CLAIM_AGE_MAX, step = 1)
              ),
              column(2,
                selectInput(ns("repl_rate_type"), "Replacement Rate",
                            choices = c(
                              "PV Replacement Rate" = "pv_rr",
                              "High-35 Wage-Indexed" = "wage_h35",
                              "High-35 Price-Indexed" = "real_h35"
                            ),
                            selected = "pv_rr")
              )
            ),
            fluidRow(
              class = "mt-2",
              column(2,
                tags$div(
                  class = "pt-2",
                  actionButton(
                    ns("calculate"),
                    "Calculate",
                    icon = icon("play"),
                    class = "btn-primary w-100"
                  )
                )
              ),
              column(2,
                tags$div(
                  class = "pt-2",
                  downloadButton(ns("download_data"), "Export CSV",
                                 class = "btn-outline-secondary w-100")
                )
              )
            ),
            fluidRow(
              class = "mt-2",
              column(12,
                sliderInput(
                  ns("birth_year_range"),
                  "Birth Year Range",
                  min = BIRTH_YEAR_MIN,
                  max = BIRTH_YEAR_MAX,
                  value = c(1960, 2005),
                  step = 1,
                  sep = ""
                )
              )
            )
          )
        )
      )
    ),

    # Progress indicator
    fluidRow(
      class = "mt-2",
      column(12, uiOutput(ns("progress_indicator")))
    ),

    # Charts row 1: Replacement Rate & PV Benefits
    fluidRow(
      class = "mt-3",
      column(6,
        card(
          card_header(
            class = "bg-info text-white py-2",
            "Replacement Rate by Birth Year"
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("chart_replacement"), height = "320px")
          )
        )
      ),
      column(6,
        card(
          card_header(
            class = "bg-info text-white py-2",
            "PV Lifetime Benefits by Birth Year"
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("chart_pv_benefits"), height = "320px")
          )
        )
      )
    ),

    # Charts row 2: Benefit-Tax Ratio & IRR
    fluidRow(
      class = "mt-3",
      column(6,
        card(
          card_header(
            class = "bg-info text-white py-2",
            "Benefit-Tax Ratio by Birth Year"
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("chart_ratio"), height = "320px")
          )
        )
      ),
      column(6,
        card(
          card_header(
            class = "bg-info text-white py-2",
            "Internal Rate of Return by Birth Year"
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("chart_irr"), height = "320px")
          )
        )
      )
    ),

    # Summary statistics
    fluidRow(
      class = "mt-3",
      column(3, uiOutput(ns("metric_avg_repl"))),
      column(3, uiOutput(ns("metric_avg_pv"))),
      column(3, uiOutput(ns("metric_avg_ratio"))),
      column(3, uiOutput(ns("metric_avg_irr")))
    ),

    # Collapsible data table
    fluidRow(
      class = "mt-3",
      column(12,
        tags$div(
          class = "text-end mb-2",
          actionButton(ns("toggle_table"), "Show Data Table",
                       icon = icon("table"), class = "btn-sm btn-outline-secondary")
        )
      )
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_table")),
      fluidRow(
        column(12,
          card(
            card_header(class = "py-2", "Cohort Data"),
            card_body(
              class = "p-2",
              DTOutput(ns("cohort_table"))
            )
          )
        )
      )
    ),

    # Info panel
    fluidRow(
      class = "mt-3",
      column(12,
        card(
          card_body(
            class = "p-2",
            tags$small(
              class = "text-muted",
              tags$strong("Notes: "),
              "All metrics calculated for specified worker configuration across birth years. ",
              "PV values discounted to age 65. ",
              "Replacement Rate = benefit / wage-indexed highest 35 years earnings. ",
              "IRR = internal rate of return on lifetime SS taxes."
            )
          )
        )
      )
    )
  )
}

# Module Server
cohort_tab_server <- function(id, reform_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to track calculation status
    calc_status <- reactiveVal(list(running = FALSE, current = 0, total = 0))

    # Calculate cohort data when button is clicked
    cohort_data <- eventReactive(input$calculate, {
      birth_years <- seq(input$birth_year_range[1], input$birth_year_range[2])
      n_years <- length(birth_years)

      calc_status(list(running = TRUE, current = 0, total = n_years))

      showNotification(
        paste0("Calculating ", n_years, " birth cohorts..."),
        id = "cohort_calc_notify",
        duration = NULL
      )
      on.exit({
        removeNotification("cohort_calc_notify")
        calc_status(list(running = FALSE, current = 0, total = 0))
      })

      tryCatch({
        # Get reform assumptions
        has_reforms <- reform_state$has_reforms()
        reform_assumptions <- reform_state$reform_assumptions()
        reform_label <- if (has_reforms) reform_state$reform_label() else "Reform"

        # Get custom earnings if applicable
        custom_earnings <- if (input$worker_type == "custom") input$custom_earnings else NULL

        results <- lapply(seq_along(birth_years), function(i) {
          by <- birth_years[i]

          # Update progress
          calc_status(list(running = TRUE, current = i, total = n_years))

          # Calculate baseline
          baseline <- calculate_benefits(
            birth_yr = by,
            sex = input$sex,
            type = input$worker_type,
            age_claim = input$claim_age,
            factors = sef2025,
            assumptions = tr2025,
            custom_avg_earnings = custom_earnings,
            debugg = TRUE
          )
          baseline$scenario <- "Baseline"

          # Extract baseline metrics
          claim_age <- input$claim_age
          baseline_ben <- baseline$ben[baseline$age == claim_age][1]

          baseline_pv_ben <- tryCatch({
            pv_lifetime_benefits(baseline, tr2025)$pv_benefits[1]
          }, error = function(e) NA_real_)

          baseline_pv_tax <- tryCatch({
            pv_lifetime_taxes(baseline, tr2025, include_employer = TRUE)$pv_taxes[1]
          }, error = function(e) NA_real_)

          baseline_ratio <- if (!is.na(baseline_pv_ben) && !is.na(baseline_pv_tax) && baseline_pv_tax > 0) {
            baseline_pv_ben / baseline_pv_tax
          } else NA_real_

          # Replacement rate (using selected type)
          baseline_repl <- tryCatch({
            rr <- rep_rates(baseline, tr2025)
            rr$rep_rate[rr$type == input$repl_rate_type][1]
          }, error = function(e) NA_real_)

          # Internal rate of return
          baseline_irr <- tryCatch({
            internal_rate_of_return(baseline, tr2025, include_employer = TRUE)$irr[1]
          }, error = function(e) NA_real_)

          # Calculate reform if selected
          reform_ben <- NA_real_
          reform_pv_ben <- NA_real_
          reform_pv_tax <- NA_real_
          reform_ratio <- NA_real_
          reform_repl <- NA_real_
          reform_irr <- NA_real_

          if (has_reforms && !is.null(reform_assumptions)) {
            reform <- calculate_benefits_reform(
              birth_yr = by,
              sex = input$sex,
              type = input$worker_type,
              age_claim = input$claim_age,
              factors = sef2025,
              assumptions = reform_assumptions,
              custom_avg_earnings = custom_earnings,
              debugg = TRUE
            )
            reform$scenario <- reform_label

            reform_ben <- reform$ben[reform$age == claim_age][1]

            reform_pv_ben <- tryCatch({
              pv_lifetime_benefits(reform, reform_assumptions)$pv_benefits[1]
            }, error = function(e) NA_real_)

            reform_pv_tax <- tryCatch({
              pv_lifetime_taxes(reform, reform_assumptions, include_employer = TRUE)$pv_taxes[1]
            }, error = function(e) NA_real_)

            reform_ratio <- if (!is.na(reform_pv_ben) && !is.na(reform_pv_tax) && reform_pv_tax > 0) {
              reform_pv_ben / reform_pv_tax
            } else NA_real_

            reform_repl <- tryCatch({
              rr <- rep_rates(reform, reform_assumptions)
              rr$rep_rate[rr$type == input$repl_rate_type][1]
            }, error = function(e) NA_real_)

            reform_irr <- tryCatch({
              internal_rate_of_return(reform, reform_assumptions, include_employer = TRUE)$irr[1]
            }, error = function(e) NA_real_)
          }

          list(
            birth_year = by,
            benefit_baseline = baseline_ben,
            benefit_reform = reform_ben,
            pv_benefits_baseline = baseline_pv_ben,
            pv_benefits_reform = reform_pv_ben,
            pv_taxes_baseline = baseline_pv_tax,
            pv_taxes_reform = reform_pv_tax,
            ratio_baseline = baseline_ratio,
            ratio_reform = reform_ratio,
            repl_rate_baseline = baseline_repl,
            repl_rate_reform = reform_repl,
            irr_baseline = baseline_irr,
            irr_reform = reform_irr
          )
        })

        cohort_df <- bind_rows(results)

        list(
          data = cohort_df,
          has_reforms = has_reforms,
          reform_label = reform_label,
          worker_type = input$worker_type,
          custom_earnings = custom_earnings,
          sex = input$sex,
          claim_age = input$claim_age
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    }, ignoreNULL = FALSE)

    # Progress indicator
    output$progress_indicator <- renderUI({
      status <- calc_status()
      if (!status$running) return(NULL)

      progress_pct <- (status$current / status$total) * 100

      tags$div(
        class = "alert alert-info py-2",
        tags$div(class = "d-flex justify-content-between mb-1",
          tags$small(paste0("Calculating birth year ", status$current, " of ", status$total)),
          tags$small(sprintf("%.0f%%", progress_pct))
        ),
        tags$div(
          class = "progress", style = "height: 6px;",
          tags$div(
            class = "progress-bar bg-info",
            role = "progressbar",
            style = paste0("width: ", progress_pct, "%"),
            `aria-valuenow` = progress_pct,
            `aria-valuemin` = 0,
            `aria-valuemax` = 100
          )
        )
      )
    })

    # Helper function to create chart data in long format
    prepare_chart_data <- reactive({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) return(NULL)

      df <- result$data
      has_reforms <- result$has_reforms
      reform_label <- result$reform_label

      # Create long format for plotting
      if (has_reforms) {
        baseline_df <- data.frame(
          birth_year = df$birth_year,
          repl_rate = df$repl_rate_baseline * 100,
          pv_benefits = df$pv_benefits_baseline / 1000,
          ratio = df$ratio_baseline,
          irr = df$irr_baseline * 100,
          scenario = "Baseline"
        )
        reform_df <- data.frame(
          birth_year = df$birth_year,
          repl_rate = df$repl_rate_reform * 100,
          pv_benefits = df$pv_benefits_reform / 1000,
          ratio = df$ratio_reform,
          irr = df$irr_reform * 100,
          scenario = reform_label
        )
        chart_df <- bind_rows(baseline_df, reform_df)
      } else {
        chart_df <- data.frame(
          birth_year = df$birth_year,
          repl_rate = df$repl_rate_baseline * 100,
          pv_benefits = df$pv_benefits_baseline / 1000,
          ratio = df$ratio_baseline,
          irr = df$irr_baseline * 100,
          scenario = "Baseline"
        )
      }

      list(
        data = chart_df,
        has_reforms = has_reforms,
        reform_label = reform_label
      )
    })

    # Chart: Replacement Rate
    output$chart_replacement <- renderPlot({
      chart_info <- prepare_chart_data()
      if (is.null(chart_info)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see results",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      df <- chart_info$data
      show_legend <- chart_info$has_reforms

      p <- ggplot(df, aes(x = birth_year, y = repl_rate, color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3, alpha = 0.8) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = seq(1960, 2010, by = 5)) +
        scale_color_manual(values = c("Baseline" = CRFB_LIGHT_BLUE,
                                       setNames(CRFB_ORANGE, chart_info$reform_label))) +
        labs(x = "Birth Year", y = "Replacement Rate (%)", color = NULL) +
        chart_theme +
        theme(legend.position = if (show_legend) "top" else "none")

      p
    })

    # Chart: PV Benefits
    output$chart_pv_benefits <- renderPlot({
      chart_info <- prepare_chart_data()
      if (is.null(chart_info)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see results",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      df <- chart_info$data
      show_legend <- chart_info$has_reforms

      p <- ggplot(df, aes(x = birth_year, y = pv_benefits, color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3, alpha = 0.8) +
        scale_y_continuous(labels = dollar_format(suffix = "K")) +
        scale_x_continuous(breaks = seq(1960, 2010, by = 5)) +
        scale_color_manual(values = c("Baseline" = CRFB_LIGHT_BLUE,
                                       setNames(CRFB_ORANGE, chart_info$reform_label))) +
        labs(x = "Birth Year", y = "PV Lifetime Benefits ($K)", color = NULL) +
        chart_theme +
        theme(legend.position = if (show_legend) "top" else "none")

      p
    })

    # Chart: Benefit-Tax Ratio
    output$chart_ratio <- renderPlot({
      chart_info <- prepare_chart_data()
      if (is.null(chart_info)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see results",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      df <- chart_info$data
      show_legend <- chart_info$has_reforms

      p <- ggplot(df, aes(x = birth_year, y = ratio, color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3, alpha = 0.8) +
        geom_hline(yintercept = 1.0, color = DARK_MUTED, linetype = "dashed", linewidth = 0.5) +
        scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
        scale_x_continuous(breaks = seq(1960, 2010, by = 5)) +
        scale_color_manual(values = c("Baseline" = CRFB_LIGHT_BLUE,
                                       setNames(CRFB_ORANGE, chart_info$reform_label))) +
        labs(x = "Birth Year", y = "Benefit-Tax Ratio", color = NULL) +
        annotate("text", x = 2007, y = 1.02, label = "Break-even",
                 color = DARK_MUTED, size = 3, hjust = 1) +
        chart_theme +
        theme(legend.position = if (show_legend) "top" else "none")

      p
    })

    # Chart: IRR
    output$chart_irr <- renderPlot({
      chart_info <- prepare_chart_data()
      if (is.null(chart_info)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see results",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      df <- chart_info$data
      show_legend <- chart_info$has_reforms

      p <- ggplot(df, aes(x = birth_year, y = irr, color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 3, alpha = 0.8) +
        geom_hline(yintercept = 0, color = DARK_MUTED, linetype = "dashed", linewidth = 0.5) +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        scale_x_continuous(breaks = seq(1960, 2010, by = 5)) +
        scale_color_manual(values = c("Baseline" = CRFB_LIGHT_BLUE,
                                       setNames(CRFB_ORANGE, chart_info$reform_label))) +
        labs(x = "Birth Year", y = "Internal Rate of Return (%)", color = NULL) +
        chart_theme +
        theme(legend.position = if (show_legend) "top" else "none")

      p
    })

    # Summary metrics
    output$metric_avg_repl <- renderUI({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Replacement Rate"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_baseline <- mean(result$data$repl_rate_baseline, na.rm = TRUE) * 100

      if (result$has_reforms) {
        avg_reform <- mean(result$data$repl_rate_reform, na.rm = TRUE) * 100
        pct_change <- (avg_reform / avg_baseline - 1) * 100
        change_color <- if (pct_change < 0) "text-danger" else "text-success"

        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Replacement Rate"),
          tags$div(
            tags$span(class = "text-muted", sprintf("%.1f%%", avg_baseline)),
            tags$span(" \u2192 "),
            tags$strong(class = change_color, sprintf("%.1f%%", avg_reform))
          ),
          tags$small(class = change_color, sprintf("(%+.1f%%)", pct_change))
        )
      } else {
        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Replacement Rate"),
          tags$strong(class = "text-info", sprintf("%.1f%%", avg_baseline))
        )
      }
    })

    output$metric_avg_pv <- renderUI({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg PV Benefits"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_baseline <- mean(result$data$pv_benefits_baseline, na.rm = TRUE) / 1000

      if (result$has_reforms) {
        avg_reform <- mean(result$data$pv_benefits_reform, na.rm = TRUE) / 1000
        pct_change <- (avg_reform / avg_baseline - 1) * 100
        change_color <- if (pct_change < 0) "text-danger" else "text-success"

        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg PV Benefits"),
          tags$div(
            tags$span(class = "text-muted", format_currency(avg_baseline, suffix = "K")),
            tags$span(" \u2192 "),
            tags$strong(class = change_color, format_currency(avg_reform, suffix = "K"))
          ),
          tags$small(class = change_color, sprintf("(%+.1f%%)", pct_change))
        )
      } else {
        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg PV Benefits"),
          tags$strong(class = "text-success", format_currency(avg_baseline, suffix = "K"))
        )
      }
    })

    output$metric_avg_ratio <- renderUI({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Benefit-Tax Ratio"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_baseline <- mean(result$data$ratio_baseline, na.rm = TRUE)

      if (result$has_reforms) {
        avg_reform <- mean(result$data$ratio_reform, na.rm = TRUE)
        pct_change <- (avg_reform / avg_baseline - 1) * 100
        change_color <- if (pct_change < 0) "text-danger" else "text-success"

        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Benefit-Tax Ratio"),
          tags$div(
            tags$span(class = "text-muted", sprintf("%.2f", avg_baseline)),
            tags$span(" \u2192 "),
            tags$strong(class = change_color, sprintf("%.2f", avg_reform))
          ),
          tags$small(class = change_color, sprintf("(%+.1f%%)", pct_change))
        )
      } else {
        ratio_color <- if (avg_baseline >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Benefit-Tax Ratio"),
          tags$strong(class = ratio_color, sprintf("%.2f", avg_baseline))
        )
      }
    })

    output$metric_avg_irr <- renderUI({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg IRR"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_baseline <- mean(result$data$irr_baseline, na.rm = TRUE) * 100

      if (result$has_reforms) {
        avg_reform <- mean(result$data$irr_reform, na.rm = TRUE) * 100
        diff <- avg_reform - avg_baseline
        change_color <- if (diff < 0) "text-danger" else "text-success"

        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg IRR"),
          tags$div(
            tags$span(class = "text-muted", sprintf("%.1f%%", avg_baseline)),
            tags$span(" \u2192 "),
            tags$strong(class = change_color, sprintf("%.1f%%", avg_reform))
          ),
          tags$small(class = change_color, sprintf("(%+.1f pp)", diff))
        )
      } else {
        irr_color <- if (avg_baseline >= 0) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg IRR"),
          tags$strong(class = irr_color, sprintf("%.1f%%", avg_baseline))
        )
      }
    })

    # Data table (collapsible)
    output$cohort_table <- renderDT({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(datatable(
          data.frame(Message = "Click 'Calculate' to generate results"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- result$data
      has_reforms <- result$has_reforms

      if (has_reforms) {
        display_df <- data.frame(
          `Birth Year` = df$birth_year,
          `Repl% (Base)` = round(df$repl_rate_baseline * 100, 1),
          `Repl% (Reform)` = round(df$repl_rate_reform * 100, 1),
          `PV Ben (Base)` = round(df$pv_benefits_baseline / 1000, 0),
          `PV Ben (Reform)` = round(df$pv_benefits_reform / 1000, 0),
          `Ratio (Base)` = round(df$ratio_baseline, 2),
          `Ratio (Reform)` = round(df$ratio_reform, 2),
          `IRR% (Base)` = round(df$irr_baseline * 100, 2),
          `IRR% (Reform)` = round(df$irr_reform * 100, 2),
          check.names = FALSE
        )
      } else {
        display_df <- data.frame(
          `Birth Year` = df$birth_year,
          `Repl Rate %` = round(df$repl_rate_baseline * 100, 1),
          `PV Benefits (K)` = round(df$pv_benefits_baseline / 1000, 0),
          `Benefit-Tax Ratio` = round(df$ratio_baseline, 2),
          `IRR %` = round(df$irr_baseline * 100, 2),
          check.names = FALSE
        )
      }

      datatable(
        display_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'tip'
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        result <- cohort_data()
        type_label <- if (!is.null(result)) {
          if (result$worker_type == "custom") {
            paste0("custom_", result$custom_earnings)
          } else {
            result$worker_type
          }
        } else "medium"
        paste0("cohort_comparison_", type_label, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        result <- cohort_data()
        if (!is.null(result) && !is.null(result$data)) {
          export_df <- result$data
          export_df$worker_type <- result$worker_type
          export_df$custom_earnings <- result$custom_earnings
          export_df$sex <- result$sex
          export_df$claim_age <- result$claim_age
          export_df$reform_scenario <- if (result$has_reforms) result$reform_label else "None"

          export_df <- export_df %>%
            select(birth_year, worker_type, custom_earnings, sex, claim_age, reform_scenario, everything())

          write.csv(export_df, file, row.names = FALSE)
        }
      }
    )

  })
}
