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
          value = TRUE
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
      primary_with_taxes <- tryCatch({
        calculate_taxes(primary, assumptions)
      }, error = function(e) {
        primary %>% mutate(ss_tax = 0)
      })

      # Get death age for filtering
      death_age <- unique(primary$death_age)[1]
      if (is.null(death_age) || is.na(death_age)) death_age <- 85

      # Prepare flow data - filter to working years (21-64) for taxes, claim to death for benefits
      flow <- primary_with_taxes %>%
        filter(age >= 21 & age < death_age) %>%
        mutate(
          tax_amount = if (input$include_employer) {
            ifelse(age <= 64, ss_tax * 2, 0)
          } else {
            ifelse(age <= 64, ss_tax, 0)
          },
          benefit_amount = ifelse(!is.na(annual_ind) & annual_ind > 0, annual_ind, 0)
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
