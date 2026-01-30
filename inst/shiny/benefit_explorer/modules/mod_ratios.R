# =============================================================================
# Ratios Module
# =============================================================================
# Displays benefit-tax ratios for workers and couples

# Module UI
ratios_ui <- function(id) {

  ns <- NS(id)

  layout_columns(
    col_widths = c(6, 6, 6, 6),

    # Top left - Main ratio display
    card(
      card_header(
        class = "bg-primary text-white",
        "Benefit-Tax Ratios"
      ),
      card_body(
        checkboxInput(
          ns("include_employer_ratio"),
          "Include Employer Share of Taxes in Calculation",
          value = TRUE
        ),
        uiOutput(ns("ratio_display"))
      )
    ),

    # Top right - IRR display
    card(
      card_header(
        class = "bg-info text-white",
        "Internal Rate of Return"
      ),
      card_body(
        uiOutput(ns("irr_display"))
      )
    ),

    # Bottom left - interpretation
    card(
      card_header("Interpreting the Ratio"),
      card_body(
        tags$dl(
          tags$dt("Ratio > 1.0"),
          tags$dd("Worker receives more in benefits than they paid in taxes (in present value terms). This is common for lower-income workers and those with longer life expectancies."),

          tags$dt("Ratio = 1.0"),
          tags$dd("Worker receives exactly what they paid (actuarially fair). This is the break-even point."),

          tags$dt("Ratio < 1.0"),
          tags$dd("Worker pays more in taxes than they receive in benefits. This is common for higher-income workers and those with shorter life expectancies.")
        ),
        tags$hr(),
        tags$p(
          class = "text-muted small",
          "Note: All values are in real 2025 dollars, discounted to age 65 using ",
          "the real discount factor from the Trustees Report. Different assumptions ",
          "about interest rates, mortality, or claiming behavior would change these results."
        )
      )
    ),

    # Bottom right - couple measures (if applicable)
    card(
      card_header("Couple Analysis"),
      card_body(
        uiOutput(ns("couple_display"))
      )
    )
  )
}

# Module Server
ratios_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate ratio measures
    ratio_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      spouse <- data$spouse
      assumptions <- data$assumptions

      tryCatch({
        # Worker individual measures
        worker_pv_ben <- pv_lifetime_benefits(primary, assumptions)
        worker_pv_tax <- pv_lifetime_taxes(primary, assumptions,
                                            include_employer = input$include_employer_ratio)

        worker_benefits <- worker_pv_ben$pv_benefits[1]
        worker_taxes <- worker_pv_tax$pv_taxes[1]
        worker_ratio <- benefit_tax_ratio(worker_benefits, worker_taxes)

        result <- list(
          worker_benefits = worker_benefits,
          worker_taxes = worker_taxes,
          worker_ratio = worker_ratio,
          has_spouse = data$has_spouse
        )

        # Add spouse measures if applicable
        if (data$has_spouse && !is.null(spouse)) {
          # Spouse individual measures
          spouse_pv_ben <- pv_lifetime_benefits(spouse, assumptions)
          spouse_pv_tax <- pv_lifetime_taxes(spouse, assumptions,
                                              include_employer = input$include_employer_ratio)

          spouse_benefits <- spouse_pv_ben$pv_benefits[1]
          spouse_taxes <- spouse_pv_tax$pv_taxes[1]
          spouse_ratio <- benefit_tax_ratio(spouse_benefits, spouse_taxes)

          # Couple totals
          couple_benefits <- worker_benefits + spouse_benefits
          couple_taxes <- worker_taxes + spouse_taxes
          couple_ratio <- benefit_tax_ratio(couple_benefits, couple_taxes)

          # Shared (50/50) measures
          shared_benefits <- couple_benefits / 2
          shared_taxes <- couple_taxes / 2
          shared_ratio <- benefit_tax_ratio(shared_benefits, shared_taxes)

          result$spouse_benefits <- spouse_benefits
          result$spouse_taxes <- spouse_taxes
          result$spouse_ratio <- spouse_ratio
          result$couple_benefits <- couple_benefits
          result$couple_taxes <- couple_taxes
          result$couple_ratio <- couple_ratio
          result$shared_benefits <- shared_benefits
          result$shared_taxes <- shared_taxes
          result$shared_ratio <- shared_ratio
        }

        # Calculate IRR for worker
        worker_irr <- tryCatch({
          irr_result <- internal_rate_of_return(primary, assumptions,
                                                 include_employer = input$include_employer_ratio)
          irr_result$irr[1]
        }, error = function(e) NA)

        result$worker_irr <- worker_irr

        # Add spouse IRR if applicable
        if (data$has_spouse && !is.null(spouse)) {
          spouse_irr <- tryCatch({
            irr_result <- internal_rate_of_return(spouse, assumptions,
                                                   include_employer = input$include_employer_ratio)
            irr_result$irr[1]
          }, error = function(e) NA)
          result$spouse_irr <- spouse_irr
        }

        result
      }, error = function(e) {
        NULL
      })
    })

    # IRR display
    output$irr_display <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios)) {
        return(helpText("Calculate benefits to see IRR"))
      }

      # Format IRR with color
      format_irr <- function(r) {
        if (is.na(r)) return(tags$span(class = "text-muted", "N/A"))
        color <- if (r >= 0) "text-success" else "text-danger"
        tags$span(class = color, sprintf("%.2f%%", r * 100))
      }

      # Worker IRR card
      worker_irr_card <- tags$div(
        class = "text-center",
        tags$h6(class = "text-muted", "Primary Worker"),
        tags$h2(format_irr(ratios$worker_irr)),
        tags$p(
          class = "small text-muted mb-0",
          "Real rate of return on SS taxes"
        )
      )

      if (!ratios$has_spouse || is.null(ratios$spouse_irr)) {
        return(tagList(
          worker_irr_card,
          tags$hr(),
          tags$p(
            class = "small text-muted",
            "IRR = discount rate where PV(benefits) = PV(taxes). ",
            "Higher values indicate better returns. ",
            "Progressive benefit formula leads to higher IRR for lower earners."
          )
        ))
      }

      # With spouse
      spouse_irr_card <- tags$div(
        class = "text-center",
        tags$h6(class = "text-muted", "Spouse"),
        tags$h2(format_irr(ratios$spouse_irr))
      )

      tagList(
        tags$div(
          class = "row",
          tags$div(class = "col-6", worker_irr_card),
          tags$div(class = "col-6", spouse_irr_card)
        ),
        tags$hr(),
        tags$p(
          class = "small text-muted",
          "IRR = discount rate where PV(benefits) = PV(taxes). Higher = better returns."
        )
      )
    })

    # Main ratio display
    output$ratio_display <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios)) {
        return(helpText("Calculate benefits to see ratios"))
      }

      # Format ratio with color
      format_ratio <- function(r) {
        color <- if (r >= 1) "text-success" else "text-danger"
        tags$span(class = color, sprintf("%.2f", r))
      }

      # Worker ratio card
      worker_card <- tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body text-center",
          tags$h6(class = "text-muted", "Primary Worker"),
          tags$h1(format_ratio(ratios$worker_ratio)),
          tags$p(
            class = "small text-muted mb-0",
            paste0("$", format(round(ratios$worker_benefits), big.mark = ","),
                   " benefits / $", format(round(ratios$worker_taxes), big.mark = ","),
                   " taxes")
          )
        )
      )

      if (!ratios$has_spouse) {
        return(worker_card)
      }

      # With spouse - show all ratios
      spouse_card <- tags$div(
        class = "card mb-3",
        tags$div(
          class = "card-body text-center",
          tags$h6(class = "text-muted", "Spouse (Individual)"),
          tags$h1(format_ratio(ratios$spouse_ratio)),
          tags$p(
            class = "small text-muted mb-0",
            paste0("$", format(round(ratios$spouse_benefits), big.mark = ","),
                   " / $", format(round(ratios$spouse_taxes), big.mark = ","))
          )
        )
      )

      couple_card <- tags$div(
        class = "card mb-3 border-primary",
        tags$div(
          class = "card-body text-center",
          tags$h6(class = "text-primary", "Couple Combined"),
          tags$h1(class = "text-primary", sprintf("%.2f", ratios$couple_ratio)),
          tags$p(
            class = "small text-muted mb-0",
            paste0("$", format(round(ratios$couple_benefits), big.mark = ","),
                   " / $", format(round(ratios$couple_taxes), big.mark = ","))
          )
        )
      )

      tagList(
        tags$div(
          class = "row",
          tags$div(class = "col-6", worker_card),
          tags$div(class = "col-6", spouse_card)
        ),
        couple_card
      )
    })

    # Couple display (shared measures)
    output$couple_display <- renderUI({
      ratios <- ratio_data()

      if (is.null(ratios)) {
        return(helpText("Calculate benefits to see couple analysis"))
      }

      if (!ratios$has_spouse) {
        return(tags$div(
          class = "text-muted text-center",
          tags$p("Add a spouse in the sidebar to see couple analysis"),
          tags$p(class = "small", "Couple analysis shows how Social Security benefits and taxes are distributed when viewed as a shared household resource.")
        ))
      }

      # Shared (50/50) analysis
      tags$div(
        tags$h5("Shared Household Perspective"),
        tags$p(
          class = "text-muted small",
          "When viewing Social Security as a shared household resource, ",
          "the couple's total benefits and taxes are split 50/50."
        ),
        tags$table(
          class = "table table-sm",
          tags$thead(
            tags$tr(
              tags$th("Measure"),
              tags$th("Worker"),
              tags$th("Spouse"),
              tags$th("Total")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Shared Benefits"),
              tags$td(format_currency(ratios$shared_benefits)),
              tags$td(format_currency(ratios$shared_benefits)),
              tags$td(format_currency(ratios$couple_benefits))
            ),
            tags$tr(
              tags$td("Shared Taxes"),
              tags$td(format_currency(ratios$shared_taxes)),
              tags$td(format_currency(ratios$shared_taxes)),
              tags$td(format_currency(ratios$couple_taxes))
            ),
            tags$tr(
              class = "table-primary",
              tags$td(tags$strong("Shared Ratio")),
              tags$td(sprintf("%.2f", ratios$shared_ratio)),
              tags$td(sprintf("%.2f", ratios$shared_ratio)),
              tags$td(sprintf("%.2f", ratios$couple_ratio))
            )
          )
        ),
        tags$hr(),
        tags$h5("Individual vs Shared"),
        tags$p(
          class = "small",
          sprintf(
            "The worker's individual ratio is %.2f, while the spouse's is %.2f. ",
            ratios$worker_ratio, ratios$spouse_ratio
          ),
          sprintf(
            "Under the shared perspective, both have a ratio of %.2f.",
            ratios$shared_ratio
          )
        )
      )
    })

  })
}
