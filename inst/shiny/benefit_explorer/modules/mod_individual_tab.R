# =============================================================================
# Individual Worker Tab Module - Combined input + benefits + marginal analysis
# =============================================================================

# Beneficiary class labels
BC_LABELS <- c(
  "AR" = "Retired Worker", "ARB" = "Retired + Spousal", "ARD" = "Retired + Widow(er)",
  "ARF" = "Retired + Disabled Widow(er)", "AD" = "Disabled Worker",
  "ADB" = "Disabled + Spousal", "ADD" = "Disabled + Widow(er)",
  "ADF" = "Disabled + Disabled Widow(er)", "BR" = "Spouse Only (Retired)",
  "BD" = "Spouse Only (Disabled)", "D" = "Widow(er) Only", "F" = "Disabled Widow(er) Only"
)

# Module UI
individual_tab_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Shared parameters row (birth year and claim age apply to both workers)
    fluidRow(
      column(3,
        numericInput(ns("birth_year"), "Birth Year",
                     value = BIRTH_YEAR_DEFAULT,
                     min = BIRTH_YEAR_MIN, max = BIRTH_YEAR_MAX, step = 1)
      ),
      column(3,
        numericInput(ns("claim_age"), "Claim Age",
                     value = CLAIM_AGE_DEFAULT,
                     min = CLAIM_AGE_MIN, max = CLAIM_AGE_MAX, step = 1)
      )
    ),

    # Row 1: Worker configuration cards
    fluidRow(
      # Primary worker config
      column(6,
        card(
          card_header(
            class = "bg-primary text-white py-2",
            tags$span("Worker Configuration")
          ),
          card_body(
            class = "p-3",
            fluidRow(
              column(6,
                selectInput(ns("worker_type"), "Worker Type",
                            choices = WORKER_TYPES, selected = "medium")
              ),
              column(6,
                selectInput(ns("sex"), "Sex", choices = SEX_OPTIONS, selected = "all")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'custom'", ns("worker_type")),
              numericInput(ns("custom_earnings"), "Avg Real Earnings ($)",
                           value = CUSTOM_EARNINGS_DEFAULT, min = 1000, max = 500000, step = 1000)
            )
          )
        )
      ),

      # Spouse config (optional)
      column(6,
        card(
          card_header(
            class = "bg-success text-white py-2 d-flex justify-content-between align-items-center",
            tags$span("Spouse Configuration"),
            checkboxInput(ns("add_spouse"), "Include", value = FALSE, width = "80px")
          ),
          card_body(
            class = "p-3",
            conditionalPanel(
              condition = sprintf("input['%s']", ns("add_spouse")),
              fluidRow(
                column(6,
                  selectInput(ns("spouse_type"), "Spouse Type",
                              choices = WORKER_TYPES, selected = "low")
                ),
                column(6,
                  selectInput(ns("spouse_sex"), "Sex",
                              choices = SEX_OPTIONS, selected = "all")
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'custom'", ns("spouse_type")),
                numericInput(ns("spouse_custom_earnings"), "Spouse Earnings ($)",
                             value = CUSTOM_EARNINGS_DEFAULT, min = 1000, max = 500000, step = 1000)
              )
            ),
            conditionalPanel(
              condition = sprintf("!input['%s']", ns("add_spouse")),
              tags$div(
                class = "text-center text-muted py-4",
                tags$small("Check 'Include' to add a spouse")
              )
            )
          )
        )
      )
    ),

    # Calculate button
    fluidRow(
      class = "mt-3 mb-3",
      column(12,
        actionButton(
          ns("calculate"),
          "Calculate Benefits",
          icon = icon("play"),
          class = "btn-primary btn-lg w-100"
        )
      )
    ),

    # Row 2: Charts side by side
    fluidRow(
      # Benefits by age chart
      column(6,
        card(
          card_header(
            class = "bg-info text-white d-flex justify-content-between align-items-center py-2",
            tags$span("Annual Benefits by Age"),
            radioButtons(
              ns("chart_type"), NULL,
              choices = c("Nominal $" = "nominal", "Real 2025 $" = "real"),
              selected = "real", inline = TRUE
            )
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("benefit_chart"), height = "380px")
          )
        )
      ),

      # NMTR by age chart
      column(6,
        card(
          card_header(
            class = "bg-info text-white d-flex justify-content-between align-items-center py-2",
            tags$span("Net Marginal Tax Rate by Age")
          ),
          card_body(
            class = "p-2",
            plotOutput(ns("nmtr_chart"), height = "380px")
          )
        )
      )
    ),

    # Row 3: Key metrics
    fluidRow(
      class = "mt-3",
      column(3, uiOutput(ns("metric_monthly"))),
      column(3, uiOutput(ns("metric_pv_benefits"))),
      column(3, uiOutput(ns("metric_pv_taxes"))),
      column(3, uiOutput(ns("metric_ratio")))
    ),

    # Row 4: Additional metrics (IRR, Marginal IRR, Marginal BTR)
    fluidRow(
      class = "mt-2",
      column(3, uiOutput(ns("metric_irr"))),
      column(3, uiOutput(ns("metric_marginal_irr"))),
      column(3, uiOutput(ns("metric_marginal_btr"))),
      column(3,
        tags$div(
          class = "text-end pt-3",
          actionButton(ns("toggle_data"), "Show Data Tables",
                       icon = icon("table"), class = "btn-sm btn-outline-secondary")
        )
      )
    ),

    # Collapsible data tables
    conditionalPanel(
      condition = sprintf("input['%s'] %% 2 == 1", ns("toggle_data")),
      fluidRow(
        class = "mt-3",
        column(6,
          card(
            card_header(class = "py-2", "Benefits Data"),
            card_body(
              class = "p-2",
              downloadButton(ns("download_benefits"), "Export CSV",
                             class = "btn-sm btn-outline-primary mb-2"),
              DTOutput(ns("benefits_table"))
            )
          )
        ),
        column(6,
          card(
            card_header(class = "py-2", "Marginal Analysis Data"),
            card_body(
              class = "p-2",
              downloadButton(ns("download_marginal"), "Export CSV",
                             class = "btn-sm btn-outline-primary mb-2"),
              DTOutput(ns("marginal_table"))
            )
          )
        )
      )
    )
  )
}

# Module Server
individual_tab_server <- function(id, reform_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Calculate benefits when button is clicked
    worker_data <- eventReactive(input$calculate, {
      showNotification("Calculating benefits...", id = "calc_notify", duration = NULL)
      on.exit(removeNotification("calc_notify"))

      tryCatch({
        # Prepare primary worker parameters
        custom_avg <- if (input$worker_type == "custom") input$custom_earnings else NULL

        # Prepare spouse parameters
        spouse_type <- if (input$add_spouse) input$spouse_type else NULL
        spouse_sex <- if (input$add_spouse) input$spouse_sex else NULL
        spouse_birth_yr <- if (input$add_spouse) input$birth_year else NULL
        spouse_claim_age <- if (input$add_spouse) input$claim_age else NULL
        spouse_custom <- if (input$add_spouse && input$spouse_type == "custom") {
          input$spouse_custom_earnings
        } else NULL

        # Calculate BASELINE benefits
        baseline <- calculate_benefits(
          birth_yr = input$birth_year,
          sex = input$sex,
          type = input$worker_type,
          age_claim = input$claim_age,
          factors = sef2025,
          assumptions = tr2025,
          custom_avg_earnings = custom_avg,
          spouse_type = spouse_type,
          spouse_sex = spouse_sex,
          spouse_birth_yr = spouse_birth_yr,
          spouse_age_claim = spouse_claim_age,
          spouse_custom_avg_earnings = spouse_custom,
          debugg = TRUE
        )
        baseline$scenario <- "Baseline"

        # Calculate spouse benefits independently (if enabled)
        spouse_data <- NULL
        if (input$add_spouse) {
          spouse_custom_ind <- if (input$spouse_type == "custom") {
            input$spouse_custom_earnings
          } else NULL

          spouse_data <- calculate_benefits(
            birth_yr = input$birth_year,
            sex = input$spouse_sex,
            type = input$spouse_type,
            age_claim = input$claim_age,
            factors = sef2025,
            assumptions = tr2025,
            custom_avg_earnings = spouse_custom_ind,
            debugg = TRUE
          )
          spouse_data$scenario <- "Spouse"
        }

        # Calculate REFORM benefits (if reforms selected)
        # Uses calculate_benefits_reform() which has reform-capable functions
        # (aime_reform, pia_reform, cola_reform, etc.) that process reform parameters
        reform_data <- NULL
        reform_assumptions <- reform_state$reform_assumptions()

        if (reform_state$has_reforms() && !is.null(reform_assumptions)) {
          reform_data <- calculate_benefits_reform(
            birth_yr = input$birth_year,
            sex = input$sex,
            type = input$worker_type,
            age_claim = input$claim_age,
            factors = sef2025,
            assumptions = reform_assumptions,
            custom_avg_earnings = custom_avg,
            spouse_type = spouse_type,
            spouse_sex = spouse_sex,
            spouse_birth_yr = spouse_birth_yr,
            spouse_age_claim = spouse_claim_age,
            spouse_custom_avg_earnings = spouse_custom,
            debugg = TRUE
          )
          reform_data$scenario <- reform_state$reform_label()
          reform_data$is_reform <- TRUE
        }

        # Calculate REFORM spouse benefits independently (if spouse + reforms)
        reform_spouse_data <- NULL
        if (input$add_spouse && reform_state$has_reforms() && !is.null(reform_assumptions)) {
          reform_spouse_data <- tryCatch({
            rs <- calculate_benefits_reform(
              birth_yr = input$birth_year,
              sex = input$spouse_sex,
              type = input$spouse_type,
              age_claim = input$claim_age,
              factors = sef2025,
              assumptions = reform_assumptions,
              custom_avg_earnings = spouse_custom_ind,
              debugg = TRUE
            )
            rs$scenario <- "Reform Spouse"
            rs
          }, error = function(e) NULL)
        }

        list(
          baseline = baseline,
          reform = reform_data,
          spouse = spouse_data,
          reform_spouse = reform_spouse_data,
          has_spouse = input$add_spouse,
          has_reforms = reform_state$has_reforms(),
          reform_label = reform_state$reform_label(),
          assumptions = tr2025,
          reform_assumptions = reform_assumptions,
          factors = sef2025
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    }, ignoreNULL = FALSE)

    # Couple IRR helper: combine primary + spouse tax/benefit streams, solve with uniroot
    compute_couple_irr <- function(worker, spouse, assumptions, include_employer = TRUE) {
      tryCatch({
        w_taxes <- calculate_taxes(worker, assumptions)
        s_taxes <- calculate_taxes(spouse, assumptions)

        claim_age_val <- unique(worker$claim_age)[1]
        death_age_val <- unique(worker$death_age)[1]
        floor_death <- floor(death_age_val)
        frac_death <- death_age_val - floor_death

        # Combined tax stream (ages 21-64)
        w_tax <- w_taxes %>%
          filter(age >= 21 & age <= 64) %>%
          mutate(tax_amount = if (include_employer) ss_tax * 2 else ss_tax) %>%
          select(age, tax_amount)
        s_tax <- s_taxes %>%
          filter(age >= 21 & age <= 64) %>%
          mutate(tax_amount = if (include_employer) ss_tax * 2 else ss_tax) %>%
          select(age, tax_amount)

        combined_tax <- merge(w_tax, s_tax, by = "age", all = TRUE, suffixes = c("_w", "_s"))
        combined_tax$tax_amount_w[is.na(combined_tax$tax_amount_w)] <- 0
        combined_tax$tax_amount_s[is.na(combined_tax$tax_amount_s)] <- 0
        combined_tax$total_tax <- combined_tax$tax_amount_w + combined_tax$tax_amount_s

        # Combined benefit stream (claim_age to death_age, with partial year)
        w_ben <- worker %>%
          filter(age >= claim_age_val & age <= floor_death & annual_ind > 0) %>%
          mutate(ben_amount = if_else(age == floor_death, annual_ind * frac_death, annual_ind)) %>%
          select(age, ben_amount)
        s_ben <- spouse %>%
          filter(age >= claim_age_val & age <= floor_death & annual_ind > 0) %>%
          mutate(ben_amount = if_else(age == floor_death, annual_ind * frac_death, annual_ind)) %>%
          select(age, ben_amount)

        combined_ben <- merge(w_ben, s_ben, by = "age", all = TRUE, suffixes = c("_w", "_s"))
        combined_ben$ben_amount_w[is.na(combined_ben$ben_amount_w)] <- 0
        combined_ben$ben_amount_s[is.na(combined_ben$ben_amount_s)] <- 0
        combined_ben$total_ben <- combined_ben$ben_amount_w + combined_ben$ben_amount_s

        total_taxes <- sum(combined_tax$total_tax, na.rm = TRUE)
        total_benefits <- sum(combined_ben$total_ben, na.rm = TRUE)
        if (total_taxes == 0 || total_benefits == 0) return(NA_real_)

        npv_func <- function(r) {
          base_age <- 21
          pv_taxes <- sum(combined_tax$total_tax / (1 + r)^(combined_tax$age - base_age), na.rm = TRUE)
          pv_benefits <- sum(combined_ben$total_ben / (1 + r)^(combined_ben$age - base_age), na.rm = TRUE)
          pv_benefits - pv_taxes
        }

        result <- uniroot(npv_func, interval = c(-0.99, 1.0), tol = 1e-8)
        result$root
      }, error = function(e) NA_real_)
    }

    # Couple statistics reactive: shared (50/50) measures when spouse is present
    couple_stats <- reactive({
      data <- worker_data()
      if (is.null(data) || !data$has_spouse || is.null(data$spouse)) return(NULL)

      tryCatch({
        baseline <- data$baseline
        spouse <- data$spouse
        assumptions <- data$assumptions

        # Couple measures using existing function
        cm <- couple_measures(baseline, spouse, assumptions,
                              include_employer = TRUE, shared = TRUE)

        # Shared monthly = (primary_ben + spouse_ben) / 2
        claim_age_val <- unique(baseline$claim_age)[1]
        primary_monthly <- baseline$ben[baseline$age == claim_age_val][1]
        spouse_monthly <- spouse$ben[spouse$age == claim_age_val][1]

        # Couple IRR
        couple_irr <- compute_couple_irr(baseline, spouse, assumptions)

        result <- list(
          shared_monthly = (primary_monthly + spouse_monthly) / 2,
          shared_pv_benefits = cm$couple_pv_benefits / 2,
          shared_pv_taxes = cm$couple_pv_taxes / 2,
          shared_btr = cm$couple_ratio,
          shared_irr = couple_irr
        )

        # Reform shared values (if reforms active)
        if (data$has_reforms && !is.null(data$reform) && !is.null(data$reform_spouse)) {
          reform <- data$reform
          reform_spouse <- data$reform_spouse
          reform_assumptions <- data$reform_assumptions

          cm_reform <- couple_measures(reform, reform_spouse, reform_assumptions,
                                       include_employer = TRUE, shared = TRUE)

          reform_primary_monthly <- reform$ben[reform$age == claim_age_val][1]
          reform_spouse_monthly <- reform_spouse$ben[reform_spouse$age == claim_age_val][1]

          reform_couple_irr <- compute_couple_irr(reform, reform_spouse, reform_assumptions)

          result$reform_shared_monthly <- (reform_primary_monthly + reform_spouse_monthly) / 2
          result$reform_shared_pv_benefits <- cm_reform$couple_pv_benefits / 2
          result$reform_shared_pv_taxes <- cm_reform$couple_pv_taxes / 2
          result$reform_shared_btr <- cm_reform$couple_ratio
          result$reform_shared_irr <- reform_couple_irr
          result$has_reforms <- TRUE
        } else {
          result$has_reforms <- FALSE
        }

        result
      }, error = function(e) NULL)
    })

    # Prepare chart data (combined baseline + reform)
    chart_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)

      baseline <- data$baseline
      assumptions <- data$assumptions
      gdp_pi_2025 <- assumptions$gdp_pi[assumptions$year == 2025]

      # Add GDP deflator for real calculations
      if (!"gdp_pi" %in% names(baseline)) {
        baseline <- baseline %>%
          left_join(assumptions %>% select(year, gdp_pi), by = "year")
      }

      baseline <- baseline %>%
        mutate(
          annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
          annual_nominal = annual_ind
        )

      # Add reform data if present
      if (!is.null(data$reform)) {
        reform <- data$reform
        if (!"gdp_pi" %in% names(reform)) {
          reform <- reform %>%
            left_join(assumptions %>% select(year, gdp_pi), by = "year")
        }
        reform <- reform %>%
          mutate(
            annual_real = annual_ind * (gdp_pi_2025 / gdp_pi),
            annual_nominal = annual_ind
          )
        baseline <- bind_rows(baseline, reform)
      }

      baseline
    })

    # Benefits by age chart
    output$benefit_chart <- renderPlot({
      data <- chart_data()
      if (is.null(data)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see benefits",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      data_filtered <- data %>%
        filter(annual_ind > 0 & age < death_age)

      if (nrow(data_filtered) == 0) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "No benefits to display",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      y_var <- if (input$chart_type == "nominal") "annual_nominal" else "annual_real"
      y_label <- if (input$chart_type == "nominal") "Annual Benefit ($)" else "Annual Benefit (2025 $)"

      unique_scenarios <- unique(data_filtered$scenario)
      show_legend <- length(unique_scenarios) > 1

      p <- ggplot(data_filtered, aes(x = age, y = .data[[y_var]],
                                      color = scenario, group = scenario)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 2.5, alpha = 0.8) +
        scale_y_continuous(labels = dollar_format(), expand = expansion(mult = c(0.02, 0.1))) +
        scale_x_continuous(breaks = seq(60, 100, by = 5)) +
        scale_color_manual(values = c("Baseline" = APP_LIGHT_BLUE,
                                       setNames(APP_ORANGE, setdiff(unique_scenarios, "Baseline")))) +
        labs(x = "Age", y = y_label, color = NULL) +
        chart_theme +
        theme(
          legend.position = if (show_legend) "top" else "none",
          legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(10, 15, 10, 10)
        )

      # Add benefit-type labels at transition points (baseline only)
      if ("bc" %in% names(data_filtered)) {
        baseline_data <- data_filtered %>% filter(scenario == "Baseline")
        if (nrow(baseline_data) > 0) {
          # Find transition points where bc changes
          bc_runs <- rle(baseline_data$bc)
          transition_idx <- cumsum(bc_runs$lengths)
          # Label at the midpoint of each run
          start_idx <- c(1, cumsum(bc_runs$lengths[-length(bc_runs$lengths)]) + 1)
          mid_idx <- floor((start_idx + transition_idx) / 2)

          for (i in seq_along(bc_runs$values)) {
            bc_val <- bc_runs$values[i]
            if (!is.na(bc_val) && bc_val %in% names(BC_LABELS)) {
              label_row <- baseline_data[mid_idx[i], ]
              p <- p + annotate("text",
                x = label_row$age, y = label_row[[y_var]],
                label = bc_val, size = 3.5, color = DARK_MUTED,
                vjust = -1.5, fontface = "italic")
            }
          }
        }
      }

      p
    })

    # Calculate marginal data
    marginal_data <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)

      baseline <- data$baseline
      assumptions <- data$assumptions

      tryCatch({
        # Baseline marginal analysis
        marginal <- marginal_benefit_analysis(baseline, assumptions)
        nmtr <- net_marginal_tax_rate(baseline, assumptions,
                                       include_employer = TRUE)
        mirr <- marginal_irr(baseline, assumptions,
                             include_employer = TRUE)

        working_marginal <- marginal[marginal$age >= 21 & marginal$age <= 64, ]
        working_nmtr <- nmtr[nmtr$age >= 21 & nmtr$age <= 64, ]
        working_mirr <- mirr[mirr$age >= 21 & mirr$age <= 64, ]

        mean_nmtr <- mean(working_nmtr$net_marginal_tax_rate, na.rm = TRUE)
        mean_mirr <- mean(working_mirr$marginal_irr[!is.na(working_mirr$marginal_irr) & working_mirr$marginal_irr > -1], na.rm = TRUE)

        table_data <- data.frame(
          age = working_marginal$age,
          earnings = working_marginal$earnings,
          delta_pv_benefits = working_marginal$delta_pv_benefits,
          net_marginal_tax_rate = working_nmtr$net_marginal_tax_rate,
          marginal_irr = working_mirr$marginal_irr
        )

        result <- list(
          mean_nmtr = mean_nmtr,
          mean_mirr = mean_mirr,
          table_data = table_data,
          working_nmtr = working_nmtr,
          has_reforms = FALSE
        )

        # Calculate reform marginal if reforms are enabled
        if (!is.null(data$reform) && !is.null(data$reform_assumptions)) {
          reform <- data$reform
          reform_assumptions <- data$reform_assumptions

          reform_nmtr <- net_marginal_tax_rate(reform, reform_assumptions,
                                                include_employer = TRUE)
          reform_working_nmtr <- reform_nmtr[reform_nmtr$age >= 21 & reform_nmtr$age <= 64, ]
          reform_mean_nmtr <- mean(reform_working_nmtr$net_marginal_tax_rate, na.rm = TRUE)

          result$reform_mean_nmtr <- reform_mean_nmtr
          result$reform_working_nmtr <- reform_working_nmtr
          result$reform_scenario <- data$reform_label
          result$has_reforms <- TRUE
        }

        result
      }, error = function(e) NULL)
    })

    # NMTR chart
    output$nmtr_chart <- renderPlot({
      mdata <- marginal_data()
      if (is.null(mdata) || is.null(mdata$working_nmtr)) {
        return(ggplot() +
                 annotate("text", x = 0.5, y = 0.5,
                          label = "Click Calculate to see marginal analysis",
                          size = 5, color = DARK_MUTED) +
                 theme_void() +
                 theme(plot.background = element_rect(fill = DARK_CARD, color = NA)))
      }

      if (mdata$has_reforms && !is.null(mdata$reform_working_nmtr)) {
        # Reform comparison: overlapping bar chart
        baseline_nmtr <- mdata$table_data %>%
          filter(!is.na(net_marginal_tax_rate)) %>%
          mutate(
            nmtr_pct = net_marginal_tax_rate * 100,
            nmtr_pct_display = pmax(pmin(nmtr_pct, 20), -50),
            scenario = "Baseline"
          ) %>%
          select(age, nmtr_pct_display, scenario)

        reform_nmtr <- data.frame(
          age = mdata$reform_working_nmtr$age,
          nmtr_pct = mdata$reform_working_nmtr$net_marginal_tax_rate * 100
        ) %>%
          mutate(
            nmtr_pct_display = pmax(pmin(nmtr_pct, 20), -50),
            scenario = mdata$reform_scenario
          ) %>%
          select(age, nmtr_pct_display, scenario)

        combined <- bind_rows(baseline_nmtr, reform_nmtr) %>%
          filter(!is.na(nmtr_pct_display))

        if (nrow(combined) == 0) return(NULL)

        # Baseline bars in back (wider), reform bars in front (narrower)
        p <- ggplot(combined, aes(x = age, y = nmtr_pct_display, fill = scenario)) +
          geom_col(data = combined %>% filter(scenario == "Baseline"),
                   width = 0.8, alpha = 0.5) +
          geom_col(data = combined %>% filter(scenario != "Baseline"),
                   width = 0.5, alpha = 0.85) +
          geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
          geom_hline(yintercept = 12.4, color = APP_RED, linewidth = 0.8, linetype = "dashed") +
          scale_y_continuous(labels = function(x) paste0(x, "%"),
                             limits = c(-50, 20), oob = scales::squish) +
          scale_x_continuous(breaks = seq(25, 65, by = 5)) +
          scale_fill_manual(values = c("Baseline" = APP_TEAL,
                                        setNames(APP_ORANGE, mdata$reform_scenario))) +
          labs(x = "Age", y = "Net Marginal Tax Rate", fill = NULL) +
          annotate("text", x = 63, y = 13.5, label = "12.4% (no accrual)",
                   color = APP_RED, size = 3, hjust = 1) +
          chart_theme +
          theme(legend.position = "top")

      } else {
        # Standard bar chart (baseline only)
        nmtr_data <- mdata$table_data %>%
          filter(!is.na(net_marginal_tax_rate)) %>%
          mutate(
            nmtr_pct = net_marginal_tax_rate * 100,
            nmtr_pct_display = pmax(pmin(nmtr_pct, 20), -50)
          )

        if (nrow(nmtr_data) == 0) return(NULL)

        p <- ggplot(nmtr_data, aes(x = age, y = nmtr_pct_display)) +
          geom_col(width = 0.8, alpha = 0.85, fill = APP_TEAL) +
          geom_hline(yintercept = 0, color = DARK_MUTED, linewidth = 0.5) +
          geom_hline(yintercept = 12.4, color = APP_RED, linewidth = 0.8, linetype = "dashed") +
          scale_y_continuous(labels = function(x) paste0(x, "%"),
                             limits = c(-50, 20), oob = scales::squish) +
          scale_x_continuous(breaks = seq(25, 65, by = 5)) +
          labs(x = "Age", y = "Net Marginal Tax Rate") +
          annotate("text", x = 63, y = 13.5, label = "12.4% (no accrual)",
                   color = APP_RED, size = 3, hjust = 1) +
          chart_theme
      }

      p
    })

    # Helper: build a value row with optional reform arrow
    format_value_row <- function(label, baseline_val, reform_val, formatter, has_reform,
                                  positive_class = "text-success", negative_class = "text-danger") {
      if (has_reform && !is.na(reform_val)) {
        tags$div(
          tags$small(class = "text-muted", paste0(label, ": ")),
          tags$span(class = "text-muted", formatter(baseline_val)),
          tags$span(" \u2192 "),
          tags$strong(class = if (reform_val < baseline_val) negative_class else positive_class,
                      formatter(reform_val))
        )
      } else {
        tags$div(
          tags$small(class = "text-muted", paste0(label, ": ")),
          tags$strong(class = "text-info", formatter(baseline_val))
        )
      }
    }

    # Metric: Monthly benefit at claim
    output$metric_monthly <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)
      couple <- couple_stats()

      baseline <- data$baseline
      claim_age <- unique(baseline$claim_age)[1]
      baseline_ben <- baseline$ben[baseline$age == claim_age][1]
      has_reform <- data$has_reforms && !is.null(data$reform)
      reform_ben <- if (has_reform) data$reform$ben[data$reform$age == claim_age][1] else NA_real_

      if (!is.null(couple)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", paste0("Monthly at ", claim_age)),
          format_value_row("Individual", baseline_ben, reform_ben, format_currency, has_reform),
          format_value_row("Shared", couple$shared_monthly,
                           if (isTRUE(couple$has_reforms)) couple$reform_shared_monthly else NA_real_,
                           format_currency, isTRUE(couple$has_reforms))
        )
      } else if (has_reform) {
        pct_change <- (reform_ben / baseline_ben - 1) * 100
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", paste0("Monthly at ", claim_age)),
          tags$div(
            tags$span(class = "text-muted", format_currency(baseline_ben)),
            tags$span(" \u2192 "),
            tags$strong(class = if (pct_change < 0) "text-danger" else "text-success",
                        format_currency(reform_ben))
          ),
          tags$small(class = if (pct_change < 0) "text-danger" else "text-success",
                     sprintf("%+.1f%%", pct_change))
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", paste0("Monthly at ", claim_age)),
          tags$strong(class = "text-info", format_currency(baseline_ben))
        )
      }
    })

    # Metric: PV Benefits
    output$metric_pv_benefits <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)
      couple <- couple_stats()

      baseline_pv <- tryCatch({
        pv_lifetime_benefits(data$baseline, data$assumptions)$pv_benefits[1]
      }, error = function(e) NA_real_)
      has_reform <- data$has_reforms && !is.null(data$reform)
      reform_pv <- if (has_reform) tryCatch({
        pv_lifetime_benefits(data$reform, data$reform_assumptions)$pv_benefits[1]
      }, error = function(e) NA_real_) else NA_real_

      fmt_k <- function(x) format_currency(x / 1000, suffix = "K")

      if (!is.null(couple)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Benefits"),
          format_value_row("Individual", baseline_pv, reform_pv, fmt_k, has_reform),
          format_value_row("Shared", couple$shared_pv_benefits,
                           if (isTRUE(couple$has_reforms)) couple$reform_shared_pv_benefits else NA_real_,
                           fmt_k, isTRUE(couple$has_reforms))
        )
      } else if (has_reform) {
        pct_change <- if (!is.na(baseline_pv) && !is.na(reform_pv) && baseline_pv > 0) {
          (reform_pv / baseline_pv - 1) * 100
        } else NA_real_
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Benefits"),
          tags$div(
            tags$span(class = "text-muted", fmt_k(baseline_pv)),
            tags$span(" \u2192 "),
            tags$strong(class = if (!is.na(pct_change) && pct_change < 0) "text-danger" else "text-success",
                        fmt_k(reform_pv))
          ),
          if (!is.na(pct_change)) {
            tags$small(class = if (pct_change < 0) "text-danger" else "text-success",
                       sprintf("%+.1f%%", pct_change))
          }
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Benefits"),
          tags$strong(class = "text-success", fmt_k(baseline_pv))
        )
      }
    })

    # Metric: PV Taxes
    output$metric_pv_taxes <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)
      couple <- couple_stats()

      baseline_pv_tax <- tryCatch({
        pv_lifetime_taxes(data$baseline, data$assumptions,
                          include_employer = TRUE)$pv_taxes[1]
      }, error = function(e) NA_real_)
      has_reform <- data$has_reforms && !is.null(data$reform)
      reform_pv_tax <- if (has_reform) tryCatch({
        pv_lifetime_taxes(data$reform, data$reform_assumptions,
                          include_employer = TRUE)$pv_taxes[1]
      }, error = function(e) NA_real_) else NA_real_

      fmt_k <- function(x) format_currency(x / 1000, suffix = "K")

      if (!is.null(couple)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Taxes"),
          format_value_row("Individual", baseline_pv_tax, reform_pv_tax, fmt_k, has_reform,
                           positive_class = "text-warning", negative_class = "text-warning"),
          format_value_row("Shared", couple$shared_pv_taxes,
                           if (isTRUE(couple$has_reforms)) couple$reform_shared_pv_taxes else NA_real_,
                           fmt_k, isTRUE(couple$has_reforms),
                           positive_class = "text-warning", negative_class = "text-warning")
        )
      } else if (has_reform) {
        pct_change <- if (!is.na(baseline_pv_tax) && !is.na(reform_pv_tax) && baseline_pv_tax > 0) {
          (reform_pv_tax / baseline_pv_tax - 1) * 100
        } else NA_real_
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Taxes"),
          tags$div(
            tags$span(class = "text-muted", fmt_k(baseline_pv_tax)),
            tags$span(" \u2192 "),
            tags$strong(class = "text-warning", fmt_k(reform_pv_tax))
          ),
          if (!is.na(pct_change) && abs(pct_change) > 0.01) {
            tags$small(class = "text-warning", sprintf("%+.1f%%", pct_change))
          }
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "PV Taxes"),
          tags$strong(class = "text-warning", fmt_k(baseline_pv_tax))
        )
      }
    })

    # Metric: Benefit-Tax Ratio
    output$metric_ratio <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)
      couple <- couple_stats()

      baseline_pv <- tryCatch({
        pv_lifetime_benefits(data$baseline, data$assumptions)$pv_benefits[1]
      }, error = function(e) NA_real_)
      baseline_pv_tax <- tryCatch({
        pv_lifetime_taxes(data$baseline, data$assumptions,
                          include_employer = TRUE)$pv_taxes[1]
      }, error = function(e) NA_real_)
      baseline_ratio <- if (!is.na(baseline_pv) && !is.na(baseline_pv_tax) && baseline_pv_tax > 0) {
        baseline_pv / baseline_pv_tax
      } else NA_real_

      has_reform <- data$has_reforms && !is.null(data$reform)
      reform_ratio <- NA_real_
      if (has_reform) {
        reform_pv <- tryCatch({
          pv_lifetime_benefits(data$reform, data$reform_assumptions)$pv_benefits[1]
        }, error = function(e) NA_real_)
        reform_pv_tax <- tryCatch({
          pv_lifetime_taxes(data$reform, data$reform_assumptions,
                            include_employer = TRUE)$pv_taxes[1]
        }, error = function(e) NA_real_)
        reform_ratio <- if (!is.na(reform_pv) && !is.na(reform_pv_tax) && reform_pv_tax > 0) {
          reform_pv / reform_pv_tax
        } else NA_real_
      }

      fmt_ratio <- function(x) if (!is.na(x)) sprintf("%.2f", x) else "N/A"

      if (!is.null(couple)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Benefit/Tax Ratio"),
          format_value_row("Individual", baseline_ratio, reform_ratio, fmt_ratio, has_reform),
          format_value_row("Shared", couple$shared_btr,
                           if (isTRUE(couple$has_reforms)) couple$reform_shared_btr else NA_real_,
                           fmt_ratio, isTRUE(couple$has_reforms))
        )
      } else if (has_reform) {
        pct_change <- if (!is.na(baseline_ratio) && !is.na(reform_ratio) && baseline_ratio > 0) {
          (reform_ratio / baseline_ratio - 1) * 100
        } else NA_real_
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Benefit/Tax Ratio"),
          tags$div(
            tags$span(class = "text-muted", fmt_ratio(baseline_ratio)),
            tags$span(" \u2192 "),
            tags$strong(class = if (!is.na(reform_ratio) && reform_ratio >= 1) "text-success" else "text-danger",
                        fmt_ratio(reform_ratio))
          ),
          if (!is.na(pct_change)) {
            tags$small(class = if (pct_change < 0) "text-danger" else "text-success",
                       sprintf("%+.1f%%", pct_change))
          }
        )
      } else {
        ratio_color <- if (!is.na(baseline_ratio) && baseline_ratio >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Benefit/Tax Ratio"),
          tags$strong(class = ratio_color, fmt_ratio(baseline_ratio))
        )
      }
    })

    # Compute marginal metrics for one additional year of work
    # Uses avg of last 5 nonzero earnings years as the hypothetical additional earnings
    marginal_extra_year <- reactive({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)

      tryCatch({
        baseline <- data$baseline
        assumptions <- data$assumptions

        # Find last 5 non-zero earnings years
        working <- baseline %>%
          filter(age >= 21 & age <= 64 & earnings > 0) %>%
          arrange(desc(age))

        if (nrow(working) < 1) return(NULL)

        avg_last5 <- mean(head(working$earnings, 5))
        last_work_age <- max(working$age)

        # The hypothetical additional year is at age last_work_age + 1
        extra_age <- last_work_age + 1
        if (extra_age > 64) extra_age <- 64  # cap at working age

        # Get the marginal data — find delta_pv and tax at the last working year
        mdata <- marginal_data()
        if (is.null(mdata) || is.null(mdata$table_data)) return(NULL)

        # Use the last working year's marginal IRR as proxy
        last_row <- mdata$table_data %>%
          filter(!is.na(net_marginal_tax_rate) & earnings > 0) %>%
          arrange(desc(age)) %>%
          head(1)

        if (nrow(last_row) == 0) return(NULL)

        # Marginal IRR from last working year
        mirr_val <- last_row$marginal_irr
        delta_pv <- last_row$delta_pv_benefits

        # Marginal BTR: delta_pv_benefits / tax on that year's earnings
        # Tax = 12.4% (employee + employer) of min(earnings, taxmax)
        tax_on_year <- last_row$earnings * 0.124  # approximate employee+employer
        if (!is.na(delta_pv) && tax_on_year > 0) {
          marginal_btr <- delta_pv / tax_on_year
        } else {
          marginal_btr <- NA_real_
        }

        # Also compute for reform if available
        reform_mirr <- NA_real_
        reform_btr <- NA_real_
        if (mdata$has_reforms && !is.null(data$reform) && !is.null(data$reform_assumptions)) {
          reform_marginal <- tryCatch({
            marginal_benefit_analysis(data$reform, data$reform_assumptions)
          }, error = function(e) NULL)

          if (!is.null(reform_marginal)) {
            reform_working <- reform_marginal %>%
              filter(age >= 21 & age <= 64 & !is.na(delta_pv_benefits) & earnings > 0) %>%
              arrange(desc(age)) %>%
              head(1)

            if (nrow(reform_working) > 0) {
              reform_delta_pv <- reform_working$delta_pv_benefits
              reform_tax <- reform_working$earnings * 0.124
              if (reform_tax > 0) {
                reform_btr <- reform_delta_pv / reform_tax
              }
              # For reform IRR, use the marginal_irr function
              reform_mirr_data <- tryCatch({
                marginal_irr(data$reform, data$reform_assumptions, include_employer = TRUE)
              }, error = function(e) NULL)
              if (!is.null(reform_mirr_data)) {
                reform_mirr_row <- reform_mirr_data %>%
                  filter(age >= 21 & age <= 64 & !is.na(marginal_irr)) %>%
                  arrange(desc(age)) %>%
                  head(1)
                if (nrow(reform_mirr_row) > 0) {
                  reform_mirr <- reform_mirr_row$marginal_irr
                }
              }
            }
          }
        }

        list(
          mirr = if (!is.na(mirr_val)) mirr_val else NA_real_,
          btr = marginal_btr,
          reform_mirr = reform_mirr,
          reform_btr = reform_btr,
          has_reforms = mdata$has_reforms
        )
      }, error = function(e) NULL)
    })

    # Metric: IRR
    output$metric_irr <- renderUI({
      data <- worker_data()
      if (is.null(data) || is.null(data$baseline)) return(NULL)
      couple <- couple_stats()

      baseline_irr <- tryCatch({
        internal_rate_of_return(data$baseline, data$assumptions,
                                include_employer = TRUE)$irr[1]
      }, error = function(e) NA_real_)
      has_reform <- data$has_reforms && !is.null(data$reform)
      reform_irr <- if (has_reform) tryCatch({
        internal_rate_of_return(data$reform, data$reform_assumptions,
                                include_employer = TRUE)$irr[1]
      }, error = function(e) NA_real_) else NA_real_

      fmt_irr <- function(x) if (!is.na(x)) sprintf("%.1f%%", x * 100) else "N/A"

      if (!is.null(couple)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Internal Rate of Return"),
          format_value_row("Individual", baseline_irr, reform_irr, fmt_irr, has_reform),
          format_value_row("Shared", couple$shared_irr,
                           if (isTRUE(couple$has_reforms)) couple$reform_shared_irr else NA_real_,
                           fmt_irr, isTRUE(couple$has_reforms))
        )
      } else if (has_reform) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Internal Rate of Return"),
          tags$div(
            tags$span(class = "text-muted", fmt_irr(baseline_irr)),
            tags$span(" \u2192 "),
            tags$strong(class = "text-success", fmt_irr(reform_irr))
          )
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Internal Rate of Return"),
          tags$strong(class = "text-success", fmt_irr(baseline_irr))
        )
      }
    })

    # Metric: Marginal IRR (at last working year)
    output$metric_marginal_irr <- renderUI({
      extra <- marginal_extra_year()
      if (is.null(extra)) return(NULL)

      if (!is.null(extra$has_reforms) && extra$has_reforms && !is.na(extra$reform_mirr)) {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Marginal IRR"),
          tags$div(
            tags$span(class = "text-muted",
                      if (!is.na(extra$mirr)) sprintf("%.1f%%", extra$mirr * 100) else "N/A"),
            tags$span(" \u2192 "),
            tags$strong(class = "text-success",
                        sprintf("%.1f%%", extra$reform_mirr * 100))
          )
        )
      } else {
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Marginal IRR"),
          tags$strong(class = "text-success",
                      if (!is.na(extra$mirr)) sprintf("%.1f%%", extra$mirr * 100) else "N/A")
        )
      }
    })

    # Metric: Marginal Benefit-Tax Ratio
    output$metric_marginal_btr <- renderUI({
      extra <- marginal_extra_year()
      if (is.null(extra)) return(NULL)

      if (!is.null(extra$has_reforms) && extra$has_reforms && !is.na(extra$reform_btr)) {
        btr_color <- if (!is.na(extra$reform_btr) && extra$reform_btr >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Marginal Benefit-Tax Ratio"),
          tags$div(
            tags$span(class = "text-muted",
                      if (!is.na(extra$btr)) sprintf("%.2f", extra$btr) else "N/A"),
            tags$span(" \u2192 "),
            tags$strong(class = btr_color,
                        sprintf("%.2f", extra$reform_btr))
          )
        )
      } else {
        btr_color <- if (!is.na(extra$btr) && extra$btr >= 1) "text-success" else "text-danger"
        tags$div(
          class = "text-center p-2 rounded", style = "background: #1f3460;",
          tags$small(class = "text-muted d-block", "Marginal Benefit-Tax Ratio"),
          tags$strong(class = btr_color,
                      if (!is.na(extra$btr)) sprintf("%.2f", extra$btr) else "N/A")
        )
      }
    })

    # Benefits data table
    output$benefits_table <- renderDT({
      data <- chart_data()
      if (is.null(data)) return(NULL)

      table_data <- data %>%
        filter(annual_ind > 0 & age < death_age) %>%
        mutate(
          bc_display = if ("bc" %in% names(.)) BC_LABELS[bc] else "Retired Worker",
          annual_nominal = round(annual_nominal, 0),
          annual_real = round(annual_real, 0)
        ) %>%
        select(scenario, year, age, bc_display, annual_nominal, annual_real) %>%
        rename(
          Scenario = scenario, Year = year, Age = age, Class = bc_display,
          Nominal = annual_nominal, `Real 2025` = annual_real
        )

      datatable(table_data,
                options = list(pageLength = 10, scrollX = TRUE, dom = 'tip'),
                rownames = FALSE, class = "compact"
      ) %>%
        formatCurrency(c("Nominal", "Real 2025"), currency = "$", digits = 0)
    })

    # Marginal data table
    output$marginal_table <- renderDT({
      mdata <- marginal_data()
      if (is.null(mdata) || is.null(mdata$table_data)) return(NULL)

      df <- mdata$table_data %>%
        mutate(
          earnings = round(earnings, 0),
          delta_pv_benefits = round(delta_pv_benefits, 0),
          net_marginal_tax_rate = round(net_marginal_tax_rate * 100, 1),
          marginal_irr = round(marginal_irr * 100, 1)
        ) %>%
        rename(
          Age = age, Earnings = earnings,
          `Delta PV` = delta_pv_benefits, `NMTR %` = net_marginal_tax_rate, `Marg IRR %` = marginal_irr
        )

      datatable(df,
                options = list(pageLength = 10, dom = 'tip', scrollX = TRUE),
                rownames = FALSE, class = "compact"
      ) %>%
        formatCurrency("Earnings", currency = "$", digits = 0) %>%
        formatCurrency("Delta PV", currency = "$", digits = 0)
    })

    # Download handlers
    output$download_benefits <- downloadHandler(
      filename = function() paste0("individual_benefits_", Sys.Date(), ".csv"),
      content = function(file) {
        data <- chart_data()
        if (!is.null(data)) {
          export_data <- data %>%
            filter(annual_ind > 0 & age < death_age) %>%
            select(scenario, year, age, annual_nominal, annual_real)
          write.csv(export_data, file, row.names = FALSE)
        }
      }
    )

    output$download_marginal <- downloadHandler(
      filename = function() paste0("individual_marginal_", Sys.Date(), ".csv"),
      content = function(file) {
        mdata <- marginal_data()
        if (!is.null(mdata) && !is.null(mdata$table_data)) {
          write.csv(mdata$table_data, file, row.names = FALSE)
        }
      }
    )

  })
}
