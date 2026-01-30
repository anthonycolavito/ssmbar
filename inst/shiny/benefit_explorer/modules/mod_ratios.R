# =============================================================================
# Ratios Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
ratios_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Main visualization - ratio comparison
    card(
      card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        tags$span("Benefit-Tax Analysis"),
        checkboxInput(ns("include_employer_ratio"), "Incl. Employer Tax", value = TRUE, width = "140px")
      ),
      card_body(
        class = "p-2",
        plotOutput(ns("ratio_chart"), height = "350px")
      )
    ),

    # Key metrics row
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_worker_ratio"))),
      column(3, uiOutput(ns("metric_worker_irr"))),
      column(3, uiOutput(ns("metric_spouse_ratio"))),
      column(3, uiOutput(ns("metric_couple_ratio")))
    ),

    # Info toggle
    fluidRow(
      class = "mt-2",
      column(12,
        tags$div(
          class = "text-end",
          actionButton(ns("toggle_info"), "Interpretation Guide", icon = icon("info-circle"),
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
            tags$strong("Ratio > 1.0:"), " Worker receives more in benefits than paid in taxes (net gain).", tags$br(),
            tags$strong("Ratio = 1.0:"), " Break-even (actuarially fair).", tags$br(),
            tags$strong("Ratio < 1.0:"), " Worker pays more than they receive (net loss).", tags$br(),
            tags$strong("IRR:"), " Internal rate of return - the discount rate where PV(benefits) = PV(taxes).", tags$br(),
            tags$em(class = "text-muted", "All values in real 2025 dollars, discounted to age 65.")
          )
        )
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
        worker_pv_ben <- pv_lifetime_benefits(primary, assumptions)
        worker_pv_tax <- pv_lifetime_taxes(primary, assumptions,
                                            include_employer = input$include_employer_ratio)

        worker_benefits <- worker_pv_ben$pv_benefits[1]
        worker_taxes <- worker_pv_tax$pv_taxes[1]
        worker_ratio <- benefit_tax_ratio(worker_benefits, worker_taxes)

        worker_irr <- tryCatch({
          irr_result <- internal_rate_of_return(primary, assumptions,
                                                 include_employer = input$include_employer_ratio)
          irr_result$irr[1]
        }, error = function(e) NA)

        result <- list(
          worker_benefits = worker_benefits,
          worker_taxes = worker_taxes,
          worker_ratio = worker_ratio,
          worker_irr = worker_irr,
          has_spouse = data$has_spouse
        )

        if (data$has_spouse && !is.null(spouse)) {
          spouse_pv_ben <- pv_lifetime_benefits(spouse, assumptions)
          spouse_pv_tax <- pv_lifetime_taxes(spouse, assumptions,
                                              include_employer = input$include_employer_ratio)

          spouse_benefits <- spouse_pv_ben$pv_benefits[1]
          spouse_taxes <- spouse_pv_tax$pv_taxes[1]
          spouse_ratio <- benefit_tax_ratio(spouse_benefits, spouse_taxes)

          couple_benefits <- worker_benefits + spouse_benefits
          couple_taxes <- worker_taxes + spouse_taxes
          couple_ratio <- benefit_tax_ratio(couple_benefits, couple_taxes)

          spouse_irr <- tryCatch({
            irr_result <- internal_rate_of_return(spouse, assumptions,
                                                   include_employer = input$include_employer_ratio)
            irr_result$irr[1]
          }, error = function(e) NA)

          result$spouse_benefits <- spouse_benefits
          result$spouse_taxes <- spouse_taxes
          result$spouse_ratio <- spouse_ratio
          result$spouse_irr <- spouse_irr
          result$couple_benefits <- couple_benefits
          result$couple_taxes <- couple_taxes
          result$couple_ratio <- couple_ratio
        }

        result
      }, error = function(e) NULL)
    })

    # Main ratio chart
    output$ratio_chart <- renderPlot({
      ratios <- ratio_data()
      if (is.null(ratios)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see ratios",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      # Build data for chart
      chart_data <- data.frame(
        category = c("Benefits", "Taxes"),
        Worker = c(ratios$worker_benefits, ratios$worker_taxes),
        stringsAsFactors = FALSE
      )

      if (ratios$has_spouse) {
        chart_data$Spouse <- c(ratios$spouse_benefits, ratios$spouse_taxes)
        chart_data$Couple <- c(ratios$couple_benefits, ratios$couple_taxes)
      }

      chart_long <- chart_data %>%
        pivot_longer(cols = -category, names_to = "person", values_to = "amount") %>%
        mutate(
          person = factor(person, levels = c("Worker", "Spouse", "Couple")),
          fill_color = case_when(
            category == "Benefits" ~ "Benefits",
            category == "Taxes" ~ "Taxes"
          )
        )

      p <- ggplot(chart_long, aes(x = person, y = amount / 1000, fill = category)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_y_continuous(labels = function(x) paste0("$", x, "K")) +
        scale_fill_manual(values = c("Benefits" = CRFB_TEAL, "Taxes" = CRFB_RED)) +
        labs(x = NULL, y = "Present Value (thousands)", fill = NULL) +
        chart_theme +
        theme(legend.position = "top")

      # Add ratio annotations
      ratio_labels <- data.frame(
        person = "Worker",
        y_pos = max(chart_long$amount) / 1000 * 1.05,
        label = sprintf("Ratio: %.2f", ratios$worker_ratio)
      )

      if (ratios$has_spouse) {
        ratio_labels <- rbind(ratio_labels,
          data.frame(person = "Spouse", y_pos = max(chart_long$amount) / 1000 * 1.05,
                     label = sprintf("Ratio: %.2f", ratios$spouse_ratio)),
          data.frame(person = "Couple", y_pos = max(chart_long$amount) / 1000 * 1.05,
                     label = sprintf("Ratio: %.2f", ratios$couple_ratio))
        )
      }

      p <- p +
        geom_text(data = ratio_labels, aes(x = person, y = y_pos, label = label),
                  inherit.aes = FALSE, color = DARK_TEXT, size = 4, fontface = "bold") +
        coord_cartesian(ylim = c(0, max(chart_long$amount) / 1000 * 1.15))

      p
    })

    # Compact metrics
    output$metric_worker_ratio <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios)) return(NULL)
      ratio_color <- if (ratios$worker_ratio >= 1) "text-success" else "text-danger"
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Worker Ratio"),
        tags$strong(class = ratio_color, sprintf("%.2f", ratios$worker_ratio))
      )
    })

    output$metric_worker_irr <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios)) return(NULL)
      irr_color <- if (!is.na(ratios$worker_irr) && ratios$worker_irr >= 0) "text-success" else "text-danger"
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Worker IRR"),
        tags$strong(class = irr_color,
          if (!is.na(ratios$worker_irr)) sprintf("%.2f%%", ratios$worker_irr * 100) else "N/A"
        )
      )
    })

    output$metric_spouse_ratio <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios) || !ratios$has_spouse) {
        return(tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Spouse Ratio"),
          tags$strong(class = "text-muted", "N/A")
        ))
      }
      ratio_color <- if (ratios$spouse_ratio >= 1) "text-success" else "text-danger"
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Spouse Ratio"),
        tags$strong(class = ratio_color, sprintf("%.2f", ratios$spouse_ratio))
      )
    })

    output$metric_couple_ratio <- renderUI({
      ratios <- ratio_data()
      if (is.null(ratios) || !ratios$has_spouse) {
        return(tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Couple Ratio"),
          tags$strong(class = "text-muted", "N/A")
        ))
      }
      ratio_color <- if (ratios$couple_ratio >= 1) "text-success" else "text-danger"
      tags$div(
        class = "text-center p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-muted d-block", "Couple Ratio"),
        tags$strong(class = ratio_color, sprintf("%.2f", ratios$couple_ratio))
      )
    })
  })
}
