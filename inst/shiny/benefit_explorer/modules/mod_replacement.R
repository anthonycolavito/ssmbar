# =============================================================================
# Replacement Rates Module
# =============================================================================
# Displays multiple replacement rate calculations from rep_rates()

# Module UI
replacement_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12, 6, 6),

    # Top row - comparison chart
    card(
      card_header(
        class = "bg-primary text-white",
        "Replacement Rate Comparison"
      ),
      card_body(
        # Organized selection by category
        tags$div(
          class = "row mb-3",
          # Present Value method
          tags$div(
            class = "col-md-4",
            tags$strong(class = "d-block mb-2", "Present Value Method"),
            checkboxGroupInput(
              ns("rate_pv"),
              NULL,
              choices = c("PV Annuity" = "pv_rr"),
              selected = "pv_rr"
            )
          ),
          # Real (inflation-adjusted) methods
          tags$div(
            class = "col-md-4",
            tags$strong(class = "d-block mb-2", "Real (Inflation-Adjusted)"),
            checkboxGroupInput(
              ns("rate_real"),
              NULL,
              choices = c(
                "All Years" = "real_all",
                "Highest 35 Years" = "real_h35",
                "Highest 10 Years" = "real_h10",
                "Highest 5 Years" = "real_h5",
                "Last 35 Years" = "real_l35",
                "Last 10 Years" = "real_l10",
                "Last 5 Years" = "real_l5"
              ),
              selected = "real_h35"
            )
          ),
          # Wage-indexed methods
          tags$div(
            class = "col-md-4",
            tags$strong(class = "d-block mb-2", "Wage-Indexed"),
            checkboxGroupInput(
              ns("rate_wage"),
              NULL,
              choices = c(
                "All Years" = "wage_all",
                "Highest 35 Years" = "wage_h35",
                "Highest 10 Years" = "wage_h10",
                "Highest 5 Years" = "wage_h5",
                "Last 35 Years" = "wage_l35",
                "Last 10 Years" = "wage_l10",
                "Last 5 Years" = "wage_l5"
              ),
              selected = "wage_h35"
            )
          )
        ),
        plotOutput(ns("rr_chart"), height = "350px")
      )
    ),

    # Bottom left - explanation
    card(
      card_header("Understanding Replacement Rates"),
      card_body(
        tags$dl(
          tags$dt("PV Annuity"),
          tags$dd("Initial benefit divided by a constant real payment with the same present value as career earnings. Most comprehensive measure."),

          tags$dt("Real (Inflation-Adjusted)"),
          tags$dd("Compares benefit to earnings adjusted for inflation (purchasing power). Shows what percentage of real income is replaced."),

          tags$dt("Wage-Indexed"),
          tags$dd("Compares benefit to earnings adjusted for wage growth. This is the methodology used by SSA in official calculations."),

          tags$dt("Highest N Years"),
          tags$dd("Uses only the highest N years of earnings. High-35 matches the AIME calculation period."),

          tags$dt("Last N Years"),
          tags$dd("Uses only the final N years of earnings before retirement. Reflects pre-retirement living standard.")
        )
      )
    ),

    # Bottom right - data table
    card(
      card_header("Replacement Rate Values"),
      card_body(
        DTOutput(ns("rr_table"))
      )
    )
  )
}

# Module Server
replacement_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate replacement rates
    rr_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      # Need to prepare data for rep_rates function
      # rep_rates needs: id, year, age, earnings, annual_ind
      # Check if required columns exist
      required_cols <- c("id", "year", "age", "earnings")
      if (!all(required_cols %in% names(primary))) return(NULL)

      # rep_rates also needs annual_ind - check it exists
      if (!"annual_ind" %in% names(primary)) return(NULL)

      tryCatch({
        rr <- ssmbar:::rep_rates(primary, assumptions)
        rr$scenario <- "Primary"

        # Calculate for comparisons if available
        if (!is.null(data$comparisons)) {
          # Process each comparison scenario
          comparison_ids <- unique(data$comparisons$scenario)
          for (sc_name in comparison_ids) {
            sc_data <- data$comparisons %>% filter(scenario == sc_name)
            if (nrow(sc_data) > 0 && "annual_ind" %in% names(sc_data)) {
              sc_rr <- ssmbar:::rep_rates(sc_data, assumptions)
              sc_rr$scenario <- sc_name
              rr <- bind_rows(rr, sc_rr)
            }
          }
        }

        rr
      }, error = function(e) {
        NULL
      })
    })

    # Replacement rate chart
    output$rr_chart <- renderPlot({
      data <- rr_data()
      if (is.null(data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click 'Calculate Benefits' to see replacement rates",
                          size = 6, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      # Combine selected rate types from all three checkbox groups
      selected_types <- c(input$rate_pv, input$rate_real, input$rate_wage)
      if (length(selected_types) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Select at least one rate type",
                          size = 6, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      data_filtered <- data %>%
        filter(type %in% selected_types)

      if (nrow(data_filtered) == 0) return(NULL)

      # Create nice labels for rate types
      type_labels <- c(
        "pv_rr" = "PV Annuity",
        "real_all" = "Real All",
        "wage_all" = "Wage All",
        "real_h35" = "Real High-35",
        "wage_h35" = "Wage High-35",
        "real_h10" = "Real High-10",
        "wage_h10" = "Wage High-10",
        "real_h5" = "Real High-5",
        "wage_h5" = "Wage High-5",
        "real_l35" = "Real Last-35",
        "wage_l35" = "Wage Last-35",
        "real_l10" = "Real Last-10",
        "wage_l10" = "Wage Last-10",
        "real_l5" = "Real Last-5",
        "wage_l5" = "Wage Last-5"
      )

      data_filtered <- data_filtered %>%
        mutate(type_label = type_labels[type])

      p <- ggplot(data_filtered, aes(x = type_label, y = rep_rate, fill = scenario)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_text(aes(label = percent(rep_rate, accuracy = 0.1)),
                  position = position_dodge(width = 0.8),
                  vjust = -0.5, size = 3.5) +
        scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.15))) +
        scale_fill_manual(values = CHART_COLORS) +
        labs(
          title = "Social Security Replacement Rates",
          subtitle = "Initial annual benefit as percentage of prior earnings",
          x = "Replacement Rate Measure",
          y = "Replacement Rate",
          fill = "Worker"
        ) +
        chart_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      p
    })

    # Replacement rate table
    output$rr_table <- renderDT({
      data <- rr_data()
      if (is.null(data)) return(NULL)

      # Create nice labels
      type_labels <- c(
        "pv_rr" = "PV Annuity",
        "real_all" = "Real - All Years",
        "wage_all" = "Wage-Indexed - All Years",
        "real_h35" = "Real - Highest 35 Years",
        "wage_h35" = "Wage-Indexed - Highest 35 Years",
        "real_h10" = "Real - Highest 10 Years",
        "wage_h10" = "Wage-Indexed - Highest 10 Years",
        "real_h5" = "Real - Highest 5 Years",
        "wage_h5" = "Wage-Indexed - Highest 5 Years",
        "real_l35" = "Real - Last 35 Years",
        "wage_l35" = "Wage-Indexed - Last 35 Years",
        "real_l10" = "Real - Last 10 Years",
        "wage_l10" = "Wage-Indexed - Last 10 Years",
        "real_l5" = "Real - Last 5 Years",
        "wage_l5" = "Wage-Indexed - Last 5 Years"
      )

      # Pivot wider for nice display
      table_data <- data %>%
        mutate(type_label = type_labels[type]) %>%
        select(scenario, type_label, rep_rate) %>%
        pivot_wider(names_from = scenario, values_from = rep_rate)

      datatable(
        table_data,
        options = list(
          pageLength = 15,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c("Measure" = "type_label")
      ) %>%
        formatPercentage(names(table_data)[-1], digits = 1)
    })

  })
}
