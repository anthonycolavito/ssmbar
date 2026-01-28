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
          tags$dd("Initial benefit at claim age divided by a constant real payment with the same present value as career earnings (ages 21 through year before claiming)."),

          tags$dt("Real (Inflation-Adjusted)"),
          tags$dd("Compares benefit at claim age to earnings adjusted for inflation to the year before claiming. Shows what percentage of real income is replaced."),

          tags$dt("Wage-Indexed"),
          tags$dd("Compares benefit at claim age to earnings adjusted for wage growth to the year before claiming. This methodology matches SSA's official calculations."),

          tags$dt("Highest N Years"),
          tags$dd("Uses only the highest N years of earnings (from ages 21 through year before claiming). High-35 matches the AIME calculation period."),

          tags$dt("Last N Years"),
          tags$dd("Uses only the final N years of earnings before claiming. Reflects pre-claiming living standard.")
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
      required_cols <- c("id", "year", "age", "earnings", "annual_ind")
      missing_cols <- required_cols[!required_cols %in% names(primary)]

      if (length(missing_cols) > 0) {
        message("Replacement rate module - Missing columns: ", paste(missing_cols, collapse = ", "))
        return(NULL)
      }

      # Check if there's a benefit at claim age
      claim_age <- unique(primary$claim_age)[1]
      ben_at_claim <- primary$annual_ind[primary$age == claim_age]
      if (length(ben_at_claim) == 0 || is.na(ben_at_claim[1]) || ben_at_claim[1] == 0) {
        message("Replacement rate module - No benefit at claim age ", claim_age)
        return(NULL)
      }

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
              sc_claim_age <- unique(sc_data$claim_age)[1]
              sc_ben_claim <- sc_data$annual_ind[sc_data$age == sc_claim_age]
              if (length(sc_ben_claim) > 0 && !is.na(sc_ben_claim[1]) && sc_ben_claim[1] > 0) {
                sc_rr <- ssmbar:::rep_rates(sc_data, assumptions)
                sc_rr$scenario <- sc_name
                rr <- bind_rows(rr, sc_rr)
              }
            }
          }
        }

        rr
      }, error = function(e) {
        message("Replacement rate module error: ", e$message)
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

      # Count scenarios
      n_scenarios <- length(unique(data$scenario))

      # Warning for too many selections
      if (length(selected_types) > 6 || n_scenarios > 4) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = paste0("Too many selections for clear display.\n",
                                        "Selected: ", length(selected_types), " rate types, ",
                                        n_scenarios, " scenarios.\n",
                                        "Please select at most 6 rate types and 4 scenarios."),
                          size = 5, color = CRFB_ORANGE, lineheight = 1.3) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      data_filtered <- data %>%
        filter(type %in% selected_types)

      if (nrow(data_filtered) == 0) return(NULL)

      # Create nice labels for rate types and assign categories
      type_labels <- c(
        "pv_rr" = "PV Annuity",
        "real_all" = "All Years",
        "wage_all" = "All Years",
        "real_h35" = "High-35",
        "wage_h35" = "High-35",
        "real_h10" = "High-10",
        "wage_h10" = "High-10",
        "real_h5" = "High-5",
        "wage_h5" = "High-5",
        "real_l35" = "Last-35",
        "wage_l35" = "Last-35",
        "real_l10" = "Last-10",
        "wage_l10" = "Last-10",
        "real_l5" = "Last-5",
        "wage_l5" = "Last-5"
      )

      # Category mapping for faceting
      type_categories <- c(
        "pv_rr" = "Present Value",
        "real_all" = "Real (Inflation-Adj)",
        "wage_all" = "Wage-Indexed",
        "real_h35" = "Real (Inflation-Adj)",
        "wage_h35" = "Wage-Indexed",
        "real_h10" = "Real (Inflation-Adj)",
        "wage_h10" = "Wage-Indexed",
        "real_h5" = "Real (Inflation-Adj)",
        "wage_h5" = "Wage-Indexed",
        "real_l35" = "Real (Inflation-Adj)",
        "wage_l35" = "Wage-Indexed",
        "real_l10" = "Real (Inflation-Adj)",
        "wage_l10" = "Wage-Indexed",
        "real_l5" = "Real (Inflation-Adj)",
        "wage_l5" = "Wage-Indexed"
      )

      data_filtered <- data_filtered %>%
        mutate(
          type_label = type_labels[type],
          category = factor(type_categories[type],
                           levels = c("Present Value", "Real (Inflation-Adj)", "Wage-Indexed"))
        )

      # Count active categories for faceting
      active_categories <- unique(data_filtered$category)

      p <- ggplot(data_filtered, aes(x = type_label, y = rep_rate, fill = scenario)) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7) +
        geom_text(aes(label = percent(rep_rate, accuracy = 0.1)),
                  position = position_dodge(width = 0.8),
                  vjust = -0.5, size = 3) +
        scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.2))) +
        scale_fill_manual(values = CHART_COLORS) +
        labs(
          title = "Social Security Replacement Rates",
          subtitle = "Initial annual benefit as percentage of prior earnings",
          x = NULL,
          y = "Replacement Rate",
          fill = "Worker"
        ) +
        chart_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))

      # Add faceting if multiple categories
      if (length(active_categories) > 1) {
        p <- p + facet_wrap(~ category, scales = "free_x", nrow = 1)
      }

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
