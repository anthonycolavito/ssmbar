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

    # Calculate all lifetime measures
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

        list(
          pv_benefits = pv_ben$pv_benefits[1],
          pv_taxes = pv_tax$pv_taxes[1],
          real_benefits = real_ben$real_benefits[1],
          real_earnings = real_earn$real_earnings[1],
          pv_earnings = pv_earn$pv_earnings[1],
          benefit_tax_ratio = benefit_tax_ratio(pv_ben$pv_benefits[1], pv_tax$pv_taxes[1]),
          real_benefit_earnings_ratio = real_benefit_earnings_ratio(real_ben$real_benefits[1], real_earn$real_earnings[1]),
          pv_benefit_earnings_ratio = pv_benefit_earnings_ratio(pv_ben$pv_benefits[1], pv_earn$pv_earnings[1])
        )
      }, error = function(e) NULL)
    })

    # Flow data for chart
    flow_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      primary_with_taxes <- tryCatch({
        calculate_taxes(primary, assumptions)
      }, error = function(e) {
        primary %>% mutate(ss_tax = 0)
      })

      death_age <- unique(primary$death_age)[1]
      if (is.null(death_age) || is.na(death_age)) death_age <- 85

      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      if (!"gdp_pi" %in% names(primary_with_taxes)) {
        primary_with_taxes <- primary_with_taxes %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      flow <- primary_with_taxes %>%
        filter(age >= 21 & age < death_age) %>%
        mutate(
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
        select(year, age, tax_amount, benefit_amount) %>%
        mutate(net_flow = benefit_amount - tax_amount) %>%
        arrange(age) %>%
        mutate(
          cum_taxes = cumsum(tax_amount),
          cum_benefits = cumsum(benefit_amount),
          cum_net = cumsum(net_flow)
        )

      flow
    })

    # Compact metric outputs
    output$metric_pv_benefits <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "PV Benefits"),
        tags$strong(class = "text-success", format_currency(m$pv_benefits))
      )
    })

    output$metric_pv_taxes <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "PV Taxes"),
        tags$strong(class = "text-danger", format_currency(m$pv_taxes))
      )
    })

    output$metric_ratio <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      ratio_color <- if (!is.na(m$benefit_tax_ratio) && m$benefit_tax_ratio >= 1) "text-success" else "text-danger"
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Benefit/Tax"),
        tags$strong(class = ratio_color, sprintf("%.2f", m$benefit_tax_ratio))
      )
    })

    output$metric_real_benefits <- renderUI({
      m <- lifetime_measures()
      if (is.null(m)) return(NULL)
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Real Benefits"),
        tags$strong(class = "text-info", format_currency(m$real_benefits))
      )
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

    # Flow chart
    output$flow_chart <- renderPlot({
      flow <- flow_data()
      if (is.null(flow) || nrow(flow) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see cash flows",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      if (input$flow_type == "annual") {
        flow_active <- flow %>% filter(tax_amount > 0 | benefit_amount > 0)
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

      } else {
        flow_long <- flow %>%
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

      p
    })
  })
}
