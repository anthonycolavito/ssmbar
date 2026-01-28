# =============================================================================
# Lifetime Value Module
# =============================================================================
# Displays present value of lifetime benefits and taxes, real values, and ratios

# Module UI
lifetime_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6, 6, 6, 4, 4, 4, 12),

    # Row 1: Present Value measures
    # Left - Benefits PV
    card(
      card_header(
        class = "bg-success text-white",
        "PV Lifetime Benefits"
      ),
      card_body(
        uiOutput(ns("pv_benefits_display"))
      )
    ),

    # Right - Taxes PV
    card(
      card_header(
        class = "bg-danger text-white",
        "PV Lifetime Taxes"
      ),
      card_body(
        checkboxInput(
          ns("include_employer"),
          "Include Employer Share of Taxes",
          value = FALSE
        ),
        uiOutput(ns("pv_taxes_display"))
      )
    ),

    # Row 2: Real (price-deflated) measures
    # Left - Real Benefits
    card(
      card_header(
        class = "bg-info text-white",
        "Real Lifetime Benefits"
      ),
      card_body(
        uiOutput(ns("real_benefits_display"))
      )
    ),

    # Right - Real Earnings and PV Earnings
    card(
      card_header(
        class = "bg-secondary text-white",
        "Lifetime Earnings"
      ),
      card_body(
        uiOutput(ns("earnings_display"))
      )
    ),

    # Row 3: Ratios
    card(
      card_header("Benefit-Tax Ratio"),
      card_body(
        uiOutput(ns("benefit_tax_ratio_display"))
      )
    ),

    card(
      card_header("Real Benefit-Earnings Ratio"),
      card_body(
        uiOutput(ns("real_ratio_display"))
      )
    ),

    card(
      card_header("PV Benefit-Earnings Ratio"),
      card_body(
        uiOutput(ns("pv_ratio_display"))
      )
    ),

    # Bottom - Comparison chart
    card(
      card_header("Benefits vs Taxes Over Time"),
      card_body(
        radioButtons(
          ns("flow_type"),
          NULL,
          choices = c(
            "Annual Flow" = "annual",
            "Cumulative (Undiscounted)" = "cumulative"
          ),
          selected = "annual",
          inline = TRUE
        ),
        plotOutput(ns("flow_chart"), height = "350px")
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

    # Flow data for chart
    flow_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      # Calculate taxes
      primary_with_taxes <- calculate_taxes(primary, assumptions)

      # Prepare flow data
      flow <- primary_with_taxes %>%
        mutate(
          tax_amount = if (input$include_employer) ss_tax * 2 else ss_tax,
          benefit_amount = annual_ind
        ) %>%
        select(year, age, earnings, tax_amount, benefit_amount) %>%
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
      pv <- pv_data()
      if (is.null(pv)) {
        return(helpText("Calculate benefits to see present values"))
      }

      tags$div(
        tags$h2(class = "text-success mb-3",
                format_currency(pv$pv_benefits)),
        tags$p(class = "text-muted",
               "Discounted to age 65 using nominal effective interest rate from Trustees Report")
      )
    })

    # PV Taxes display
    output$pv_taxes_display <- renderUI({
      pv <- pv_data()
      if (is.null(pv)) {
        return(helpText("Calculate benefits to see present values"))
      }

      employer_note <- if (input$include_employer) {
        " (employee + employer share)"
      } else {
        " (employee share only)"
      }

      tags$div(
        tags$h2(class = "text-danger mb-3",
                format_currency(pv$pv_taxes)),
        tags$p(class = "text-muted",
               paste0("Discounted to age 65", employer_note))
      )
    })

    # Real Benefits display
    output$real_benefits_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Calculate benefits to see real values"))
      }

      tags$div(
        tags$h2(class = "text-info mb-3",
                format_currency(measures$real_benefits)),
        tags$p(class = "text-muted",
               "Sum of benefits in constant 2025 dollars (GDP price index)")
      )
    })

    # Earnings display (both real and PV)
    output$earnings_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("Calculate benefits to see earnings values"))
      }

      tags$div(
        tags$p(
          tags$strong("Real Earnings (2025$): "),
          format_currency(measures$real_earnings)
        ),
        tags$p(
          tags$strong("PV Earnings: "),
          format_currency(measures$pv_earnings)
        ),
        tags$p(class = "text-muted small",
               "Ages 21-64 (PV discounted to age 65)")
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
        tags$h3(class = paste(ratio_class, "mb-2"),
                sprintf("%.2f", ratio)),
        tags$p(class = "text-muted small",
               "PV Benefits / PV Taxes")
      )
    })

    # Real Benefit-Earnings Ratio display
    output$real_ratio_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("..."))
      }

      tags$div(
        tags$h3(class = "text-primary mb-2",
                sprintf("%.2f%%", measures$real_benefit_earnings_ratio * 100)),
        tags$p(class = "text-muted small",
               "Real Benefits / Real Earnings")
      )
    })

    # PV Benefit-Earnings Ratio display
    output$pv_ratio_display <- renderUI({
      measures <- lifetime_measures()
      if (is.null(measures)) {
        return(helpText("..."))
      }

      tags$div(
        tags$h3(class = "text-primary mb-2",
                sprintf("%.2f%%", measures$pv_benefit_earnings_ratio * 100)),
        tags$p(class = "text-muted small",
               "PV Benefits / PV Earnings")
      )
    })

    # Flow chart
    output$flow_chart <- renderPlot({
      flow <- flow_data()
      if (is.null(flow)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click 'Calculate Benefits' to see cash flows",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      if (input$flow_type == "annual") {
        # Annual flow chart
        flow_long <- flow %>%
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

        p <- ggplot(flow_long, aes(x = age, y = amount, fill = type)) +
          geom_col(position = "identity", alpha = 0.8) +
          geom_hline(yintercept = 0, color = "gray30") +
          scale_y_continuous(labels = dollar_format()) +
          scale_fill_manual(values = c("Taxes Paid" = "#dc3545", "Benefits Received" = "#198754")) +
          labs(
            title = "Annual Social Security Cash Flows",
            subtitle = "Taxes paid (negative) vs benefits received (positive)",
            x = "Age",
            y = "Annual Amount",
            fill = NULL
          ) +
          chart_theme

      } else {
        # Cumulative chart
        flow_long <- flow %>%
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

        p <- ggplot(flow_long, aes(x = age, y = amount, color = type)) +
          geom_line(linewidth = 1.2) +
          scale_y_continuous(labels = dollar_format()) +
          scale_color_manual(values = c("Cumulative Taxes" = "#dc3545",
                                         "Cumulative Benefits" = "#198754")) +
          labs(
            title = "Cumulative Social Security Cash Flows",
            subtitle = "Running total of taxes paid vs benefits received (undiscounted)",
            x = "Age",
            y = "Cumulative Amount",
            color = NULL
          ) +
          chart_theme
      }

      p
    })

  })
}
