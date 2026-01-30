# =============================================================================
# Cohort Comparison Tab Module - Birth year analysis across cohorts
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
              column(3,
                selectInput(ns("worker_type"), "Worker Type",
                            choices = WORKER_TYPES[names(WORKER_TYPES) != "Custom"],
                            selected = "medium")
              ),
              column(3,
                selectInput(ns("sex"), "Sex", choices = SEX_OPTIONS, selected = "all")
              ),
              column(3,
                numericInput(ns("claim_age"), "Claim Age",
                             value = 67, min = CLAIM_AGE_MIN, max = CLAIM_AGE_MAX, step = 1)
              ),
              column(3,
                tags$div(
                  class = "pt-4",
                  actionButton(
                    ns("calculate"),
                    "Calculate Cohort",
                    icon = icon("play"),
                    class = "btn-primary w-100"
                  )
                )
              )
            ),
            fluidRow(
              class = "mt-2",
              column(12,
                sliderInput(
                  ns("birth_year_range"),
                  "Birth Year Range",
                  min = 1955,
                  max = 2010,
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

    # Results table
    fluidRow(
      class = "mt-3",
      column(12,
        card(
          card_header(
            class = "bg-info text-white py-2 d-flex justify-content-between align-items-center",
            tags$span("Cohort Comparison Results"),
            downloadButton(ns("download_data"), "Export CSV", class = "btn-sm btn-light")
          ),
          card_body(
            class = "p-2",
            DTOutput(ns("cohort_table"))
          )
        )
      )
    ),

    # Summary statistics
    fluidRow(
      class = "mt-3",
      column(3, uiOutput(ns("metric_avg_benefit_change"))),
      column(3, uiOutput(ns("metric_avg_pv_change"))),
      column(3, uiOutput(ns("metric_avg_ratio_change"))),
      column(3, uiOutput(ns("metric_avg_repl_change")))
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
              "All metrics are calculated for the specified worker type, sex, and claim age. ",
              "PV values are discounted to age 65 using real discount rates. ",
              "Replacement Rate uses wage-indexed highest 35 years earnings. ",
              "% Change shows the reform impact relative to baseline for each birth year."
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
            pv_lifetime_taxes(baseline, tr2025)$pv_taxes[1]
          }, error = function(e) NA_real_)

          baseline_ratio <- if (!is.na(baseline_pv_ben) && !is.na(baseline_pv_tax) && baseline_pv_tax > 0) {
            baseline_pv_ben / baseline_pv_tax
          } else NA_real_

          # Replacement rate (wage-indexed highest 35)
          baseline_repl <- tryCatch({
            rr <- rep_rates(baseline, tr2025)
            rr$rep_rate[rr$type == "wage_h35"][1]
          }, error = function(e) NA_real_)

          # Calculate reform if selected
          reform_ben <- NA_real_
          reform_pv_ben <- NA_real_
          reform_pv_tax <- NA_real_
          reform_ratio <- NA_real_
          reform_repl <- NA_real_

          if (has_reforms && !is.null(reform_assumptions)) {
            reform <- calculate_benefits(
              birth_yr = by,
              sex = input$sex,
              type = input$worker_type,
              age_claim = input$claim_age,
              factors = sef2025,
              assumptions = reform_assumptions,
              debugg = TRUE
            )
            reform$scenario <- reform_label

            reform_ben <- reform$ben[reform$age == claim_age][1]

            reform_pv_ben <- tryCatch({
              pv_lifetime_benefits(reform, reform_assumptions)$pv_benefits[1]
            }, error = function(e) NA_real_)

            reform_pv_tax <- tryCatch({
              pv_lifetime_taxes(reform, reform_assumptions)$pv_taxes[1]
            }, error = function(e) NA_real_)

            reform_ratio <- if (!is.na(reform_pv_ben) && !is.na(reform_pv_tax) && reform_pv_tax > 0) {
              reform_pv_ben / reform_pv_tax
            } else NA_real_

            reform_repl <- tryCatch({
              rr <- rep_rates(reform, reform_assumptions)
              rr$rep_rate[rr$type == "wage_h35"][1]
            }, error = function(e) NA_real_)
          }

          # Calculate percent changes
          pct_ben <- if (!is.na(reform_ben) && !is.na(baseline_ben) && baseline_ben > 0) {
            (reform_ben / baseline_ben - 1) * 100
          } else NA_real_

          pct_pv_ben <- if (!is.na(reform_pv_ben) && !is.na(baseline_pv_ben) && baseline_pv_ben > 0) {
            (reform_pv_ben / baseline_pv_ben - 1) * 100
          } else NA_real_

          pct_pv_tax <- if (!is.na(reform_pv_tax) && !is.na(baseline_pv_tax) && baseline_pv_tax > 0) {
            (reform_pv_tax / baseline_pv_tax - 1) * 100
          } else NA_real_

          pct_ratio <- if (!is.na(reform_ratio) && !is.na(baseline_ratio) && baseline_ratio > 0) {
            (reform_ratio / baseline_ratio - 1) * 100
          } else NA_real_

          pct_repl <- if (!is.na(reform_repl) && !is.na(baseline_repl) && baseline_repl > 0) {
            (reform_repl / baseline_repl - 1) * 100
          } else NA_real_

          list(
            birth_year = by,
            benefit_baseline = baseline_ben,
            benefit_reform = reform_ben,
            benefit_pct_change = pct_ben,
            pv_benefits_baseline = baseline_pv_ben,
            pv_benefits_reform = reform_pv_ben,
            pv_benefits_pct_change = pct_pv_ben,
            pv_taxes_baseline = baseline_pv_tax,
            pv_taxes_reform = reform_pv_tax,
            pv_taxes_pct_change = pct_pv_tax,
            ratio_baseline = baseline_ratio,
            ratio_reform = reform_ratio,
            ratio_pct_change = pct_ratio,
            repl_rate_baseline = baseline_repl,
            repl_rate_reform = reform_repl,
            repl_rate_pct_change = pct_repl
          )
        })

        cohort_df <- bind_rows(results)

        list(
          data = cohort_df,
          has_reforms = has_reforms,
          reform_label = reform_label,
          worker_type = input$worker_type,
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

    # Main cohort table
    output$cohort_table <- renderDT({
      result <- cohort_data()
      if (is.null(result) || is.null(result$data)) {
        return(datatable(
          data.frame(Message = "Click 'Calculate Cohort' to generate results"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- result$data
      has_reforms <- result$has_reforms

      if (has_reforms) {
        # Full table with baseline, reform, and % change columns
        display_df <- data.frame(
          `Birth Year` = df$birth_year,
          `Benefit (Base)` = round(df$benefit_baseline, 0),
          `Benefit (Reform)` = round(df$benefit_reform, 0),
          `Benefit %Chg` = round(df$benefit_pct_change, 1),
          `PV Ben (Base)` = round(df$pv_benefits_baseline / 1000, 0),
          `PV Ben (Reform)` = round(df$pv_benefits_reform / 1000, 0),
          `PV Ben %Chg` = round(df$pv_benefits_pct_change, 1),
          `PV Tax (Base)` = round(df$pv_taxes_baseline / 1000, 0),
          `PV Tax (Reform)` = round(df$pv_taxes_reform / 1000, 0),
          `PV Tax %Chg` = round(df$pv_taxes_pct_change, 1),
          `Ratio (Base)` = round(df$ratio_baseline, 2),
          `Ratio (Reform)` = round(df$ratio_reform, 2),
          `Ratio %Chg` = round(df$ratio_pct_change, 1),
          `Repl% (Base)` = round(df$repl_rate_baseline * 100, 1),
          `Repl% (Reform)` = round(df$repl_rate_reform * 100, 1),
          `Repl% Chg` = round(df$repl_rate_pct_change, 1),
          check.names = FALSE
        )

        dt <- datatable(
          display_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'tip',
            columnDefs = list(
              list(className = 'dt-right', targets = 1:15)
            )
          ),
          rownames = FALSE,
          class = "compact stripe"
        ) %>%
          formatCurrency(c("Benefit (Base)", "Benefit (Reform)"), currency = "$", digits = 0) %>%
          formatCurrency(c("PV Ben (Base)", "PV Ben (Reform)", "PV Tax (Base)", "PV Tax (Reform)"),
                         currency = "$", digits = 0, before = FALSE) %>%
          formatStyle(
            c("Benefit %Chg", "PV Ben %Chg", "Ratio %Chg", "Repl% Chg"),
            color = styleInterval(0, c(CRFB_RED, CRFB_TEAL))
          ) %>%
          formatStyle(
            c("PV Tax %Chg"),
            color = styleInterval(0, c(CRFB_TEAL, CRFB_ORANGE))
          )

      } else {
        # Baseline-only table
        display_df <- data.frame(
          `Birth Year` = df$birth_year,
          `Monthly Benefit` = round(df$benefit_baseline, 0),
          `PV Benefits (K)` = round(df$pv_benefits_baseline / 1000, 0),
          `PV Taxes (K)` = round(df$pv_taxes_baseline / 1000, 0),
          `Benefit/Tax Ratio` = round(df$ratio_baseline, 2),
          `Replacement Rate` = round(df$repl_rate_baseline * 100, 1),
          check.names = FALSE
        )

        dt <- datatable(
          display_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE,
            dom = 'tip',
            columnDefs = list(
              list(className = 'dt-right', targets = 1:5)
            )
          ),
          rownames = FALSE,
          class = "compact stripe"
        ) %>%
          formatCurrency("Monthly Benefit", currency = "$", digits = 0) %>%
          formatCurrency(c("PV Benefits (K)", "PV Taxes (K)"), currency = "$", digits = 0)
      }

      dt
    })

    # Summary statistics
    output$metric_avg_benefit_change <- renderUI({
      result <- cohort_data()
      if (is.null(result) || !result$has_reforms) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Benefit Change"),
          tags$strong(class = "text-muted", "Select reforms to compare")
        ))
      }

      avg_change <- mean(result$data$benefit_pct_change, na.rm = TRUE)
      change_color <- if (avg_change < 0) "text-danger" else "text-success"

      tags$div(
        class = "text-center p-3 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Avg Benefit Change"),
        tags$strong(class = change_color, sprintf("%+.1f%%", avg_change))
      )
    })

    output$metric_avg_pv_change <- renderUI({
      result <- cohort_data()
      if (is.null(result) || !result$has_reforms) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg PV Benefits Change"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_change <- mean(result$data$pv_benefits_pct_change, na.rm = TRUE)
      change_color <- if (avg_change < 0) "text-danger" else "text-success"

      tags$div(
        class = "text-center p-3 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Avg PV Benefits Change"),
        tags$strong(class = change_color, sprintf("%+.1f%%", avg_change))
      )
    })

    output$metric_avg_ratio_change <- renderUI({
      result <- cohort_data()
      if (is.null(result) || !result$has_reforms) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Ratio Change"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_change <- mean(result$data$ratio_pct_change, na.rm = TRUE)
      change_color <- if (avg_change < 0) "text-danger" else "text-success"

      tags$div(
        class = "text-center p-3 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Avg Ratio Change"),
        tags$strong(class = change_color, sprintf("%+.1f%%", avg_change))
      )
    })

    output$metric_avg_repl_change <- renderUI({
      result <- cohort_data()
      if (is.null(result) || !result$has_reforms) {
        return(tags$div(
          class = "text-center p-3 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Avg Repl Rate Change"),
          tags$strong(class = "text-muted", "-")
        ))
      }

      avg_change <- mean(result$data$repl_rate_pct_change, na.rm = TRUE)
      change_color <- if (avg_change < 0) "text-danger" else "text-success"

      tags$div(
        class = "text-center p-3 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Avg Repl Rate Change"),
        tags$strong(class = change_color, sprintf("%+.1f%%", avg_change))
      )
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        result <- cohort_data()
        type_label <- if (!is.null(result)) result$worker_type else "medium"
        paste0("cohort_comparison_", type_label, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        result <- cohort_data()
        if (!is.null(result) && !is.null(result$data)) {
          # Add metadata columns
          export_df <- result$data
          export_df$worker_type <- result$worker_type
          export_df$sex <- result$sex
          export_df$claim_age <- result$claim_age
          export_df$reform_scenario <- if (result$has_reforms) result$reform_label else "None"

          # Reorder columns
          export_df <- export_df %>%
            select(birth_year, worker_type, sex, claim_age, reform_scenario, everything())

          write.csv(export_df, file, row.names = FALSE)
        }
      }
    )

  })
}
