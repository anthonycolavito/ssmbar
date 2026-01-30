# =============================================================================
# Lifetime Value Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
lifetime_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main chart
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Lifetime Cash Flows (Real 2025 $)"),
        tags$div(
          class = "d-flex align-items-center gap-2",
          radioButtons(ns("flow_type"), NULL,
            choices = c("Annual" = "annual", "Cumulative" = "cumulative"),
            selected = "annual", inline = TRUE
          ),
          checkboxInput(ns("include_employer"), "Incl. Employer", value = TRUE, width = "120px")
        )
      ),
      card_body(
        class = "p-2",
        plotOutput(ns("flow_chart"), height = "380px")
      )
    ),

    # Compact metrics row - 4 key values
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_pv_benefits"))),
      column(3, uiOutput(ns("metric_pv_taxes"))),
      column(3, uiOutput(ns("metric_ratio"))),
      column(3, uiOutput(ns("metric_real_benefits")))
    ),

    # Second metrics row - supporting values
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_real_earnings"))),
      column(3, uiOutput(ns("metric_pv_earnings"))),
      column(3, uiOutput(ns("metric_ben_earn_ratio"))),
      column(3,
        tags$div(
          class = "text-end pt-2",
          actionButton(ns("toggle_info"), "Info", icon = icon("info-circle"),
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
            tags$strong("PV Benefits/Taxes:"), " Present value discounted to age 65 using Trustees real rate.", tags$br(),
            tags$strong("Real Values:"), " Sum of price-deflated amounts (no discounting).", tags$br(),
            tags$strong("Benefit-Tax Ratio:"), " PV Benefits / PV Taxes. >1 means net benefit.", tags$br(),
            tags$strong("Taxes:"), if (TRUE) " Include employee + employer (12.4% total)." else " Employee share only (6.2%)."
          )
        )
      )
    )
  )
}

# Module Server
lifetime_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate all lifetime measures (baseline and reform)
    lifetime_measures <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      req_cols <- c("id", "year", "age", "annual_ind", "earnings", "claim_age", "death_age")
      if (!all(req_cols %in% names(primary))) return(NULL)

      tryCatch({
        pv_ben <- pv_lifetime_benefits(primary, assumptions, discount_to_age = 65)
        pv_tax <- pv_lifetime_taxes(primary, assumptions, discount_to_age = 65,
                                     include_employer = input$include_employer)
        real_ben <- real_lifetime_benefits(primary, assumptions, base_year = 2025)
        real_earn <- real_lifetime_earnings(primary, assumptions, base_year = 2025)
        pv_earn <- pv_lifetime_earnings(primary, assumptions, discount_to_age = 65)

        result <- list(
          pv_benefits = pv_ben$pv_benefits[1],
          pv_taxes = pv_tax$pv_taxes[1],
          real_benefits = real_ben$real_benefits[1],
          real_earnings = real_earn$real_earnings[1],
          pv_earnings = pv_earn$pv_earnings[1],
          benefit_tax_ratio = benefit_tax_ratio(pv_ben$pv_benefits[1], pv_tax$pv_taxes[1]),
          real_benefit_earnings_ratio = real_benefit_earnings_ratio(real_ben$real_benefits[1], real_earn$real_earnings[1]),
          pv_benefit_earnings_ratio = pv_benefit_earnings_ratio(pv_ben$pv_benefits[1], pv_earn$pv_earnings[1]),
          has_reforms = FALSE
        )

        # Calculate reform measures if reforms are enabled
        if (!is.null(data$has_reforms) && data$has_reforms && !is.null(data$reform_data)) {
          reform <- data$reform_data
          reform_assumptions <- if (!is.null(data$reform_assumptions)) data$reform_assumptions else assumptions

          reform_pv_ben <- pv_lifetime_benefits(reform, reform_assumptions, discount_to_age = 65)
          reform_pv_tax <- pv_lifetime_taxes(reform, reform_assumptions, discount_to_age = 65,
                                              include_employer = input$include_employer)

          result$reform_pv_benefits <- reform_pv_ben$pv_benefits[1]
          result$reform_pv_taxes <- reform_pv_tax$pv_taxes[1]
          result$reform_ratio <- benefit_tax_ratio(reform_pv_ben$pv_benefits[1], reform_pv_tax$pv_taxes[1])
          result$pv_benefits_pct_change <- (result$reform_pv_benefits / result$pv_benefits - 1) * 100
          result$has_reforms <- TRUE
          result$reform_scenario <- unique(reform$scenario)[1]
        }

        result
      }, error = function(e) NULL)
    })

    # Flow data for chart (baseline and reform)
    flow_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      calc_flow <- function(worker_df, assumptions_df, scenario_name) {
        worker_with_taxes <- tryCatch({
          calculate_taxes(worker_df, assumptions_df)
        }, error = function(e) {
          worker_df %>% mutate(ss_tax = 0)
        })

        death_age <- unique(worker_df$death_age)[1]
        if (is.null(death_age) || is.na(death_age)) death_age <- 85

        gdp_pi_2025 <- assumptions_df$gdp_pi[assumptions_df$year == 2025]

        if (!"gdp_pi" %in% names(worker_with_taxes)) {
          worker_with_taxes <- worker_with_taxes %>%
            left_join(assumptions_df %>% select(year, gdp_pi), by = "year")
        }

        flow <- worker_with_taxes %>%
          filter(age >= 21 & age < death_age) %>%
          mutate(
            scenario = scenario_name,
            price_deflator = gdp_pi_2025 / gdp_pi,
            tax_nominal = if (input$include_employer) {
              ifelse(age <= 64, ss_tax * 2, 0)
            } else {
              ifelse(age <= 64, ss_tax, 0)
            },
            benefit_nominal = ifelse(!is.na(annual_ind) & annual_ind > 0, annual_ind, 0),
            tax_amount = tax_nominal * price_deflator,
            benefit_amount = benefit_nominal * price_deflator
          ) %>%
          select(scenario, year, age, tax_amount, benefit_amount) %>%
          mutate(net_flow = benefit_amount - tax_amount) %>%
          arrange(age) %>%
          mutate(
            cum_taxes = cumsum(tax_amount),
            cum_benefits = cumsum(benefit_amount),
            cum_net = cumsum(net_flow)
          )

        flow
      }

      # Calculate baseline flow
      baseline_flow <- calc_flow(primary, assumptions, "Baseline")

      # Calculate reform flow if enabled
      if (!is.null(data$has_reforms) && data$has_reforms && !is.null(data$reform_data)) {
        reform <- data$reform_data
        reform_assumptions <- if (!is.null(data$reform_assumptions)) data$reform_assumptions else assumptions
        reform_scenario <- unique(reform$scenario)[1]
        reform_flow <- calc_flow(reform, reform_assumptions, reform_scenario)

        list(
          baseline = baseline_flow,
          reform = reform_flow,
          has_reforms = TRUE
        )
      } else {
        list(
          baseline = baseline_flow,
          reform = NULL,
          has_reforms = FALSE
        )
      }
    })

    # Compact metric outputs with reform comparison
    output$metric_pv_benefits <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)

      if (m$has_reforms) {
        pct_change <- m$pv_benefits_pct_change
        change_color <- if (pct_change >= 0) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Benefits"),
          tags$div(
            tags$span(class = "text-muted", format_currency(m$pv_benefits)),
            tags$span(" → "),
            tags$strong(class = "text-success", format_currency(m$reform_pv_benefits))
          ),
          tags$small(class = change_color, sprintf("(%+.1f%%)", pct_change))
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Benefits"),
          tags$strong(class = "text-success", format_currency(m$pv_benefits))
        )
      }
    })

    output$metric_pv_taxes <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)

      if (m$has_reforms && !is.null(m$reform_pv_taxes)) {
        tax_change <- (m$reform_pv_taxes / m$pv_taxes - 1) * 100
        change_color <- if (tax_change <= 0) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Taxes"),
          tags$div(
            tags$span(class = "text-muted", format_currency(m$pv_taxes)),
            tags$span(" → "),
            tags$strong(class = "text-danger", format_currency(m$reform_pv_taxes))
          ),
          tags$small(class = change_color, sprintf("(%+.1f%%)", tax_change))
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Taxes"),
          tags$strong(class = "text-danger", format_currency(m$pv_taxes))
        )
      }
    })

    output$metric_ratio <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)

      if (m$has_reforms && !is.null(m$reform_ratio)) {
        baseline_color <- if (!is.na(m$benefit_tax_ratio) && m$benefit_tax_ratio >= 1) "text-success" else "text-danger"
        reform_color <- if (!is.na(m$reform_ratio) && m$reform_ratio >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Benefit/Tax Ratio"),
          tags$div(
            tags$span(class = "text-muted", sprintf("%.2f", m$benefit_tax_ratio)),
            tags$span(" → "),
            tags$strong(class = reform_color, sprintf("%.2f", m$reform_ratio))
          )
        )
      } else {
        ratio_color <- if (!is.na(m$benefit_tax_ratio) && m$benefit_tax_ratio >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Benefit/Tax"),
          tags$strong(class = ratio_color, sprintf("%.2f", m$benefit_tax_ratio))
        )
      }
    })

    output$metric_real_benefits <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)

      if (m$has_reforms) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Reform Scenario"),
          tags$strong(class = "text-warning", m$reform_scenario)
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Real Benefits"),
          tags$strong(class = "text-info", format_currency(m$real_benefits))
        )
      }
    })

    output$metric_real_earnings <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Real Earnings"),
        tags$strong(format_currency(m$real_earnings))
      )
    })

    output$metric_pv_earnings <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "PV Earnings"),
        tags$strong(format_currency(m$pv_earnings))
      )
    })

    output$metric_ben_earn_ratio <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Ben/Earn (PV)"),
        tags$strong(sprintf("%.1f%%", m$pv_benefit_earnings_ratio * 100))
      )
    })

    # Flow chart with reform comparison
    output$flow_chart <- renderPlot({
      flow_list <- flow_data()
      if (is.null(flow_list) || is.null(flow_list$baseline) || nrow(flow_list$baseline) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see cash flows",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      baseline <- flow_list$baseline
      has_reforms <- flow_list$has_reforms && !is.null(flow_list$reform)

      if (input$flow_type == "cumulative") {
        # Cumulative view - show benefits comparison (baseline vs reform)
        if (has_reforms) {
          reform <- flow_list$reform
          reform_scenario <- unique(reform$scenario)[1]

          combined <- bind_rows(
            baseline %>% select(age, cum_benefits, scenario) %>% mutate(type = "Baseline Benefits"),
            reform %>% select(age, cum_benefits, scenario) %>% mutate(type = paste0(reform_scenario, " Benefits"))
          ) %>%
            filter(cum_benefits > 0)

          if (nrow(combined) == 0) return(NULL)

          p <- ggplot(combined, aes(x = age, y = cum_benefits, color = type, linetype = type)) +
            geom_line(linewidth = 1.5) +
            scale_y_continuous(labels = dollar_format()) +
            scale_x_continuous(breaks = seq(20, 100, by = 10)) +
            scale_color_manual(values = c("Baseline Benefits" = CRFB_LIGHT_BLUE,
                                          setNames(CRFB_ORANGE, paste0(reform_scenario, " Benefits")))) +
            scale_linetype_manual(values = c("Baseline Benefits" = "solid",
                                              setNames("dashed", paste0(reform_scenario, " Benefits")))) +
            labs(x = "Age", y = "Cumulative Benefits (2025 $)", color = NULL, linetype = NULL) +
            chart_theme +
            theme(legend.position = "top")
        } else {
          flow_long <- baseline %>%
            filter(cum_taxes > 0 | cum_benefits > 0) %>%
            select(age, cum_taxes, cum_benefits) %>%
            pivot_longer(cols = c(cum_taxes, cum_benefits),
                         names_to = "type", values_to = "amount") %>%
            mutate(type = case_when(
              type == "cum_taxes" ~ "Cumulative Taxes",
              type == "cum_benefits" ~ "Cumulative Benefits"
            ))

          if (nrow(flow_long) == 0) return(NULL)

          p <- ggplot(flow_long, aes(x = age, y = amount, color = type)) +
            geom_line(linewidth = 1.5) +
            scale_y_continuous(labels = dollar_format()) +
            scale_x_continuous(breaks = seq(20, 100, by = 10)) +
            scale_color_manual(values = c("Cumulative Taxes" = CRFB_RED, "Cumulative Benefits" = CRFB_TEAL)) +
            labs(x = "Age", y = "Cumulative (2025 $)", color = NULL) +
            chart_theme +
            theme(legend.position = "top")
        }
      } else {
        # Annual view - compare benefits at each age
        if (has_reforms) {
          reform <- flow_list$reform
          reform_scenario <- unique(reform$scenario)[1]

          combined <- bind_rows(
            baseline %>% select(age, benefit_amount) %>% mutate(scenario = "Baseline"),
            reform %>% select(age, benefit_amount) %>% mutate(scenario = reform_scenario)
          ) %>%
            filter(benefit_amount > 0)

          if (nrow(combined) == 0) return(NULL)

          p <- ggplot(combined, aes(x = age, y = benefit_amount, fill = scenario)) +
            geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.85) +
            scale_y_continuous(labels = dollar_format()) +
            scale_x_continuous(breaks = seq(60, 100, by = 5)) +
            scale_fill_manual(values = c("Baseline" = CRFB_LIGHT_BLUE, setNames(CRFB_ORANGE, reform_scenario))) +
            labs(x = "Age", y = "Annual Benefit (2025 $)", fill = NULL) +
            chart_theme +
            theme(legend.position = "top")
        } else {
          flow_active <- baseline %>% filter(tax_amount > 0 | benefit_amount > 0)
          if (nrow(flow_active) == 0) return(NULL)

          flow_long <- flow_active %>%
            select(age, tax_amount, benefit_amount) %>%
            pivot_longer(cols = c(tax_amount, benefit_amount),
                         names_to = "type", values_to = "amount") %>%
            mutate(
              type = case_when(
                type == "tax_amount" ~ "Taxes",
                type == "benefit_amount" ~ "Benefits"
              ),
              amount = ifelse(type == "Taxes", -amount, amount)
            )

          y_max <- max(abs(flow_long$amount), na.rm = TRUE) * 1.1

          p <- ggplot(flow_long, aes(x = age, y = amount, fill = type)) +
            geom_col(position = "identity", alpha = 0.85, width = 0.8) +
            geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
            scale_y_continuous(labels = dollar_format(), limits = c(-y_max, y_max)) +
            scale_x_continuous(breaks = seq(20, 100, by = 10)) +
            scale_fill_manual(values = c("Taxes" = CRFB_RED, "Benefits" = CRFB_TEAL)) +
            labs(x = "Age", y = "Amount (2025 $)", fill = NULL) +
            chart_theme +
            theme(legend.position = "top")
        }
      }

      p
    })
  })
}
