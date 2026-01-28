# =============================================================================
# Lifetime Value Module
# =============================================================================
# Displays present value of lifetime benefits and taxes, real values, and ratios

# Module UI
lifetime_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Top row - Chart (most prominent)
    fluidRow(
      column(12,
        card(
          card_header(
            class = "bg-primary text-white",
            "Benefits vs Taxes Over Time (Real 2025 $)"
          ),
          card_body(
            fluidRow(
              column(6,
                radioButtons(
                  ns("flow_type"),
                  NULL,
                  choices = c(
                    "Annual Flow" = "annual",
                    "Cumulative" = "cumulative"
                  ),
                  selected = "annual",
                  inline = TRUE
                )
              ),
              column(6,
                checkboxInput(
                  ns("include_employer"),
                  "Include Employer Share of Taxes",
                  value = TRUE
                )
              )
            ),
            plotOutput(ns("flow_chart"), height = "300px")
          )
        )
      )
    ),

    # Row 2: PV measures (all in real 2025 dollars)
    fluidRow(
      column(4,
        card(
          card_header(class = "bg-success text-white", "PV Benefits (2025$)"),
          card_body(uiOutput(ns("pv_benefits_display")))
        )
      ),
      column(4,
        card(
          card_header(class = "bg-danger text-white", "PV Taxes (2025$)"),
          card_body(uiOutput(ns("pv_taxes_display")))
        )
      ),
      column(4,
        card(
          card_header("Benefit-Tax Ratio"),
          card_body(uiOutput(ns("benefit_tax_ratio_display")))
        )
      )
    ),

    # Row 3: Real (undiscounted) measures
    fluidRow(
      column(3,
        card(
          card_header(class = "bg-info text-white", "Real Benefits (2025$)"),
          card_body(uiOutput(ns("real_benefits_display")))
        )
      ),
      column(3,
        card(
          card_header(class = "bg-secondary text-white", "Real Earnings (2025$)"),
          card_body(uiOutput(ns("real_earnings_display")))
        )
      ),
      column(3,
        card(
          card_header(class = "bg-info text-white", "PV Earnings (2025$)"),
          card_body(uiOutput(ns("pv_earnings_display")))
        )
      ),
      column(3,
        card(
          card_header("Ratios"),
          card_body(uiOutput(ns("ratios_display")))
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

      # Debug: Check required columns
      req_cols <- c("id", "year", "age", "annual_ind", "earnings", "claim_age", "death_age")
      missing_cols <- req_cols[!req_cols %in% names(primary)]

      if (length(missing_cols) > 0) {
        message("Lifetime module - Missing columns: ", paste(missing_cols, collapse = ", "))
        return(NULL)
      }

      tryCatch({
        # Calculate PV of benefits
        pv_ben <- pv_lifetime_benefits(primary, assumptions, discount_to_age = 65)

        # Calculate PV of taxes
        pv_tax <- pv_lifetime_taxes(primary, assumptions, discount_to_age = 65,
                                     include_employer = input$include_employer)

        # Calculate real (price-deflated) benefits in 2025 dollars
        real_ben <- real_lifetime_benefits(primary, assumptions, base_year = 2025)

        # Calculate real earnings in 2025 dollars
        real_earn <- real_lifetime_earnings(primary, assumptions, base_year = 2025)

        # Calculate PV of earnings
        pv_earn <- pv_lifetime_earnings(primary, assumptions, discount_to_age = 65)

        # Get individual values (first row since all have same id)
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
      }, error = function(e) {
        message("Lifetime module error: ", e$message)
        NULL
      })
    })

    # Legacy alias for compatibility
    pv_data <- reactive({
      measures <- lifetime_measures()
      if (is.null(measures)) return(NULL)
      list(
        pv_benefits = measures$pv_benefits,
        pv_taxes = measures$pv_taxes,
        ratio = measures$benefit_tax_ratio
      )
    })

    # Flow data for chart (in real 2025 dollars)
    flow_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      # Calculate taxes
      primary_with_taxes <- tryCatch({
        calculate_taxes(primary, assumptions)
      }, error = function(e) {
        primary %>% mutate(ss_tax = 0)
      })

      # Get death age for filtering
      death_age <- unique(primary$death_age)[1]
      if (is.null(death_age) || is.na(death_age)) death_age <- 85

      # Get GDP price index for 2025 to convert to real dollars
      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      # Join gdp_pi if not present
      if (!"gdp_pi" %in% names(primary_with_taxes)) {
        primary_with_taxes <- primary_with_taxes %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      # Prepare flow data in real 2025 dollars
      flow <- primary_with_taxes %>%
        filter(age >= 21 & age < death_age) %>%
        mutate(
          # Convert to real 2025 dollars
          price_deflator = gdp_pi_2025 / gdp_pi,
          tax_nominal = if (input$include_employer) {
            ifelse(age <= 64, ss_tax * 2, 0)
          } else {
            ifelse(age <= 64, ss_tax, 0)
          },
          benefit_nominal = ifelse(!is.na(annual_ind) & annual_ind > 0, annual_ind, 0),
          # Real values
          tax_amount = tax_nominal * price_deflator,
          benefit_amount = benefit_nominal * price_deflator
        ) %>%
        select(year, age, tax_amount, benefit_amount) %>%
        mutate(
          net_flow = benefit_amount - tax_amount
        )

      # Add cumulative totals
      flow <- flow %>%
        arrange(age) %>%
        mutate(
          cum_taxes = cumsum(tax_amount),
          cum_benefits = cumsum(benefit_amount),
          cum_net = cumsum(net_flow)
        )

      flow
    })

    # PV Benefits display
    output$pv_benefits_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Click Calculate"))
      }

      tags$div(
        tags$h3(class = "text-success mb-1", format_currency(measures$pv_benefits)),
        tags$p(class = "text-muted small mb-0", "Discounted to age 65")
      )
    })

    # PV Taxes display
    output$pv_taxes_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Click Calculate"))
      }

      employer_note <- if (input$include_employer) "(w/ employer)" else "(employee only)"

      tags$div(
        tags$h3(class = "text-danger mb-1", format_currency(measures$pv_taxes)),
        tags$p(class = "text-muted small mb-0", employer_note)
      )
    })

    # Benefit-Tax Ratio display
    output$benefit_tax_ratio_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("..."))
      }

      ratio <- measures$benefit_tax_ratio
      ratio_class <- if (!is.na(ratio) && ratio >= 1) "text-success" else "text-danger"

      tags$div(
        tags$h3(class = paste(ratio_class, "mb-1"), sprintf("%.2f", ratio)),
        tags$p(class = "text-muted small mb-0", "PV Benefits / PV Taxes")
      )
    })

    # Real Benefits display
    output$real_benefits_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Click Calculate"))
      }

      tags$div(
        tags$h4(class = "text-info mb-1", format_currency(measures$real_benefits)),
        tags$p(class = "text-muted small mb-0", "Undiscounted sum")
      )
    })

    # Real Earnings display (separate)
    output$real_earnings_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Click Calculate"))
      }

      tags$div(
        tags$h4(class = "mb-1", format_currency(measures$real_earnings)),
        tags$p(class = "text-muted small mb-0", "Ages 21-64")
      )
    })

    # PV Earnings display (separate)
    output$pv_earnings_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Click Calculate"))
      }

      tags$div(
        tags$h4(class = "text-info mb-1", format_currency(measures$pv_earnings)),
        tags$p(class = "text-muted small mb-0", "Discounted to age 65")
      )
    })

    # Combined ratios display
    output$ratios_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("..."))
      }

      tags$div(
        tags$p(class = "mb-1",
          tags$strong("Real Ben/Earn: "),
          sprintf("%.1f%%", measures$real_benefit_earnings_ratio * 100)
        ),
        tags$p(class = "mb-0",
          tags$strong("PV Ben/Earn: "),
          sprintf("%.1f%%", measures$pv_benefit_earnings_ratio * 100)
        )
      )
    })

    # Flow chart
    output$flow_chart <- renderPlot({
      flow <- flow_data()
      if (is.null(flow) || nrow(flow) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click 'Calculate Benefits' to see cash flows",
                          size = 6, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      if (input$flow_type == "annual") {
        # Annual flow chart - filter to ages with activity
        flow_active <- flow %>%
          filter(tax_amount > 0 | benefit_amount > 0)

        if (nrow(flow_active) == 0) {
          return(ggplot() +
                   annotate("text", x = 0.5, y = 0.5,
                            label = "No cash flow data available",
                            size = 6, color = DARK_MUTED) +
                   theme_void() +
                   theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
        }

        flow_long <- flow_active %>%
          select(age, tax_amount, benefit_amount) %>%
          pivot_longer(cols = c(tax_amount, benefit_amount),
                       names_to = "type",
                       values_to = "amount") %>%
          mutate(
            type = case_when(
              type == "tax_amount" ~ "Taxes Paid",
              type == "benefit_amount" ~ "Benefits Received"
            ),
            amount = ifelse(type == "Taxes Paid", -amount, amount)
          )

        # Calculate y-axis limits with padding
        y_max <- max(abs(flow_long$amount), na.rm = TRUE) * 1.1

        p <- ggplot(flow_long, aes(x = age, y = amount, fill = type)) +
          geom_col(position = "identity", alpha = 0.8, width = 0.8) +
          geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
          scale_y_continuous(labels = dollar_format(), limits = c(-y_max, y_max)) +
          scale_x_continuous(breaks = seq(20, 100, by = 10)) +
          scale_fill_manual(values = c("Taxes Paid" = CRFB_RED, "Benefits Received" = CRFB_TEAL)) +
          labs(
            title = "Annual Social Security Cash Flows (Real 2025$)",
            subtitle = "Taxes paid (negative) vs benefits received (positive)",
            x = "Age",
            y = "Amount (2025$)",
            fill = NULL
          ) +
          chart_theme

      } else {
        # Cumulative chart
        flow_long <- flow %>%
          filter(cum_taxes > 0 | cum_benefits > 0) %>%
          select(age, cum_taxes, cum_benefits) %>%
          pivot_longer(cols = c(cum_taxes, cum_benefits),
                       names_to = "type",
                       values_to = "amount") %>%
          mutate(
            type = case_when(
              type == "cum_taxes" ~ "Cumulative Taxes",
              type == "cum_benefits" ~ "Cumulative Benefits"
            )
          )

        if (nrow(flow_long) == 0) {
          return(ggplot() +
                   annotate("text", x = 0.5, y = 0.5,
                            label = "No cumulative data available",
                            size = 6, color = DARK_MUTED) +
                   theme_void() +
                   theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
        }

        p <- ggplot(flow_long, aes(x = age, y = amount, color = type)) +
          geom_line(linewidth = 1.2) +
          scale_y_continuous(labels = dollar_format()) +
          scale_x_continuous(breaks = seq(20, 100, by = 10)) +
          scale_color_manual(values = c("Cumulative Taxes" = CRFB_RED,
                                         "Cumulative Benefits" = CRFB_TEAL)) +
          labs(
            title = "Cumulative Social Security Cash Flows (Real 2025$)",
            subtitle = "Running total of taxes paid vs benefits received",
            x = "Age",
            y = "Cumulative Amount (2025$)",
            color = NULL
          ) +
          chart_theme
      }

      p
    })

  })
}
