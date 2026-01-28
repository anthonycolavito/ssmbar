# =============================================================================
# Benefits Module
# =============================================================================
# Displays benefit amounts over time in real and nominal dollars

# Beneficiary class labels for display
BC_LABELS <- c(
  "AR" = "Retired Worker",
  "ARB" = "Retired + Spousal",
  "ARD" = "Retired + Widow(er)",
  "ARF" = "Retired + Disabled Widow(er)",
  "AD" = "Disabled Worker",
  "ADB" = "Disabled + Spousal",
  "ADD" = "Disabled + Widow(er)",
  "ADF" = "Disabled + Disabled Widow(er)",
  "BR" = "Spouse Only (Retired)",
  "BD" = "Spouse Only (Disabled)",
  "D" = "Widow(er) Only",
  "F" = "Disabled Widow(er) Only"
)

# Module UI
benefits_ui <- function(id) {
  ns <- NS(id)

  layout_columns(
    col_widths = c(12, 6, 6),

    # Top row - main chart
    card(
      card_header(
        class = "bg-primary text-white",
        "Benefit Trajectory Over Time"
      ),
      card_body(
        radioButtons(
          ns("chart_type"),
          NULL,
          choices = c(
            "Nominal Dollars" = "nominal",
            "Real Dollars (2025)" = "real"
          ),
          selected = "nominal",
          inline = TRUE
        ),
        plotOutput(ns("benefit_chart"), height = "400px")
      )
    ),

    # Bottom left - summary statistics
    card(
      card_header("Key Statistics"),
      card_body(
        uiOutput(ns("summary_stats"))
      )
    ),

    # Bottom right - data table
    card(
      card_header("Benefit Data"),
      card_body(
        downloadButton(ns("download_data"), "Export CSV", class = "btn-sm mb-2"),
        DTOutput(ns("benefit_table"))
      )
    )
  )
}

# Module Server
benefits_server <- function(id, worker_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare data for charts
    chart_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) return(NULL)

      primary <- data$primary
      assumptions <- data$assumptions

      # Calculate real benefits (deflated to 2025 dollars using GDP price index)
      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      # Join gdp_pi if not already present (may exist from join_all_assumptions)
      if (!"gdp_pi" %in% names(primary)) {
        primary <- primary %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      primary <- primary %>%
        mutate(
          annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
          annual_nominal = annual_ind
        )

      # Add comparison scenarios if available
      if (!is.null(data$comparisons)) {
        comparisons <- data$comparisons

        # Join gdp_pi if not already present
        if (!"gdp_pi" %in% names(comparisons)) {
          comparisons <- comparisons %>%
            left_join(assumptions %>% select(year, gdp_pi), by = "year")
        }

        comparisons <- comparisons %>%
          mutate(
            annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
            annual_nominal = annual_ind
          )

        primary <- bind_rows(primary, comparisons)
      }

      primary
    })

    # Main benefit chart
    output$benefit_chart <- renderPlot({
      data <- chart_data()
      if (is.null(data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click 'Calculate Benefits' to see results",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      # Filter to benefit-receiving years up to death age
      data_filtered <- data %>%
        filter(annual_ind > 0 & age < death_age)

      if (nrow(data_filtered) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No benefits calculated",
                          size = 6, color = "gray50") +
                 theme_void())
      }

      y_var <- if (input$chart_type == "nominal") "annual_nominal" else "annual_real"
      y_label <- if (input$chart_type == "nominal") {
        "Annual Benefit (Nominal $)"
      } else {
        "Annual Benefit (2025 $)"
      }

      # Create BC label for display (use short codes for chart)
      data_filtered <- data_filtered %>%
        mutate(bc_label = if ("bc" %in% names(.)) bc else "AR")

      # Calculate y-axis range with padding to show trajectory clearly
      y_values <- data_filtered[[y_var]]
      y_min <- min(y_values, na.rm = TRUE)
      y_max <- max(y_values, na.rm = TRUE)
      y_range <- y_max - y_min
      # Add 15% padding at top for labels, reasonable padding at bottom
      y_axis_min <- max(0, y_min - y_range * 0.05)
      y_axis_max <- y_max + y_range * 0.15

      # Check if we have multiple scenarios
      unique_scenarios <- unique(data_filtered$scenario)
      show_scenario_legend <- length(unique_scenarios) > 1

      # Find benefit class transitions for primary scenario (vertical dividers)
      primary_data <- data_filtered %>% filter(scenario == "Primary" | scenario == unique_scenarios[1])
      bc_transitions <- primary_data %>%
        arrange(age) %>%
        mutate(bc_change = bc_label != lag(bc_label, default = first(bc_label))) %>%
        filter(bc_change | age == min(age))

      # Create labels for each BC region
      bc_regions <- primary_data %>%
        arrange(age) %>%
        group_by(bc_label) %>%
        summarize(
          start_age = min(age),
          end_age = max(age),
          mid_age = (min(age) + max(age)) / 2,
          .groups = "drop"
        )

      p <- ggplot(data_filtered, aes(x = age, y = .data[[y_var]],
                                      color = scenario, group = scenario)) +
        # Add vertical lines at BC transitions (except first)
        geom_vline(
          data = bc_transitions %>% filter(age > min(primary_data$age)),
          aes(xintercept = age - 0.5),
          linetype = "dashed",
          color = DARK_MUTED,
          alpha = 0.7
        ) +
        # Add BC labels at top of chart
        geom_label(
          data = bc_regions,
          aes(x = mid_age, y = y_axis_max - y_range * 0.03, label = bc_label),
          inherit.aes = FALSE,
          fill = DARK_CARD,
          color = CRFB_LIGHT_BLUE,
          size = 3,
          label.padding = unit(0.15, "lines"),
          label.size = 0
        ) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2, alpha = 0.8) +
        scale_y_continuous(labels = dollar_format(), limits = c(y_axis_min, y_axis_max)) +
        scale_x_continuous(breaks = seq(60, 100, by = 5)) +
        scale_color_manual(values = CHART_COLORS, guide = if (show_scenario_legend) "legend" else "none") +
        labs(
          title = "Annual Social Security Benefits by Age",
          subtitle = if (input$chart_type == "nominal") {
            "Nominal dollars including COLA adjustments"
          } else {
            "Real dollars (deflated to 2025 using GDP price index)"
          },
          x = "Age",
          y = y_label,
          color = "Scenario"
        ) +
        chart_theme

      p
    })

    # Summary statistics
    output$summary_stats <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$primary)) {
        return(helpText("No data available"))
      }

      primary <- data$primary
      assumptions <- data$assumptions

      # Get key values
      claim_age <- unique(primary$claim_age)[1]
      death_age <- unique(primary$death_age)[1]

      # Benefit at claim
      ben_at_claim <- primary$annual_ind[primary$age == claim_age]

      # Benefit at 65
      ben_at_65 <- primary$annual_ind[primary$age == 65]

      # Benefit at 70
      ben_at_70 <- if (70 <= max(primary$age)) {
        primary$annual_ind[primary$age == 70]
      } else NA

      # PV of lifetime benefits (discounted to age 65)
      pv_ben <- tryCatch({
        pv_lifetime_benefits(primary, assumptions, discount_to_age = 65)$pv_benefits[1]
      }, error = function(e) NA_real_)

      # Real benefit at claim (deflated using GDP price index)
      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]
      claim_year <- primary$year[primary$age == claim_age]
      gdp_pi_claim <- assumptions$gdp_pi[assumptions$year == claim_year]
      ben_at_claim_real <- ben_at_claim * (gdp_pi_2025 / gdp_pi_claim)

      tags$div(
        class = "row",
        tags$div(
          class = "col-6",
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "Claim Age"),
                   tags$h4(claim_age)
          ),
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "Annual Benefit at Claim"),
                   tags$h4(format_currency(ben_at_claim))
          ),
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "Benefit at Claim (2025 Real $)"),
                   tags$h4(format_currency(ben_at_claim_real))
          )
        ),
        tags$div(
          class = "col-6",
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "Expected Death Age"),
                   tags$h4(round(death_age))
          ),
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "Annual Benefit at 70"),
                   tags$h4(if (!is.na(ben_at_70)) format_currency(ben_at_70) else "N/A")
          ),
          tags$div(class = "mb-3",
                   tags$small(class = "text-muted", "PV Lifetime Benefits (age 65)"),
                   tags$h4(format_currency(pv_ben))
          )
        )
      )
    })

    # Benefit data table
    output$benefit_table <- renderDT({
      data <- chart_data()
      if (is.null(data)) return(NULL)

      # Select and format relevant columns (filter to death age)
      # Include BC column with human-readable labels
      table_data <- data %>%
        filter(annual_ind > 0 & age < death_age) %>%
        mutate(
          bc_display = if ("bc" %in% names(.)) {
            ifelse(is.na(bc), "", BC_LABELS[bc])
          } else "Retired Worker",
          annual_nominal = round(annual_nominal, 0),
          annual_real = round(annual_real, 0)
        ) %>%
        select(scenario, year, age, bc_display, annual_nominal, annual_real) %>%
        rename(
          Scenario = scenario,
          Year = year,
          Age = age,
          `Benefit Class` = bc_display,
          `Nominal ($)` = annual_nominal,
          `Real 2025 ($)` = annual_real
        )

      datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        rownames = FALSE
      ) %>%
        formatCurrency(c("Nominal ($)", "Real 2025 ($)"), currency = "", digits = 0)
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("benefits_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- chart_data()
        if (!is.null(data)) {
          export_data <- data %>%
            filter(annual_ind > 0 & age < death_age) %>%
            mutate(
              bc_label = if ("bc" %in% names(.)) BC_LABELS[bc] else "Retired Worker"
            )
          # Select columns that exist
          export_cols <- c("scenario", "id", "year", "age", "bc", "bc_label",
                           "earnings", "annual_nominal", "annual_real")
          export_cols <- intersect(export_cols, names(export_data))
          export_data <- export_data %>% select(all_of(export_cols))
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )

  })
}
