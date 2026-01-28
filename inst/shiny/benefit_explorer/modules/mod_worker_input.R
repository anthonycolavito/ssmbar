# =============================================================================
# Worker Input Module
# =============================================================================
# Provides UI controls for configuring worker parameters and calculates benefits

# Module UI
worker_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Primary worker configuration
    h5("Primary Worker", class = "text-primary"),

    selectInput(
      ns("worker_type"),
      "Worker Type",
      choices = WORKER_TYPES,
      selected = "medium"
    ),

    conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("worker_type")),
      numericInput(
        ns("custom_earnings"),
        "Average Real Earnings ($)",
        value = CUSTOM_EARNINGS_DEFAULT,
        min = 1000,
        max = 500000,
        step = 1000
      )
    ),

    sliderInput(
      ns("birth_year"),
      "Birth Year",
      min = BIRTH_YEAR_MIN,
      max = BIRTH_YEAR_MAX,
      value = BIRTH_YEAR_DEFAULT,
      step = 1,
      sep = ""
    ),

    selectInput(
      ns("sex"),
      "Sex (for life expectancy)",
      choices = SEX_OPTIONS,
      selected = "all"
    ),

    sliderInput(
      ns("claim_age"),
      "Claim Age",
      min = CLAIM_AGE_MIN,
      max = CLAIM_AGE_MAX,
      value = CLAIM_AGE_DEFAULT,
      step = 1
    ),

    hr(),

    # Spouse configuration
    checkboxInput(
      ns("add_spouse"),
      "Add Spouse",
      value = FALSE
    ),

    conditionalPanel(
      condition = sprintf("input['%s']", ns("add_spouse")),

      h5("Spouse", class = "text-success"),

      selectInput(
        ns("spouse_type"),
        "Spouse Type",
        choices = WORKER_TYPES,
        selected = "low"
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'custom'", ns("spouse_type")),
        numericInput(
          ns("spouse_custom_earnings"),
          "Spouse Avg Real Earnings ($)",
          value = CUSTOM_EARNINGS_DEFAULT,
          min = 1000,
          max = 500000,
          step = 1000
        )
      ),

      sliderInput(
        ns("spouse_birth_year"),
        "Spouse Birth Year",
        min = BIRTH_YEAR_MIN,
        max = BIRTH_YEAR_MAX,
        value = BIRTH_YEAR_DEFAULT,
        step = 1,
        sep = ""
      ),

      selectInput(
        ns("spouse_sex"),
        "Spouse Sex",
        choices = SEX_OPTIONS,
        selected = "all"
      ),

      sliderInput(
        ns("spouse_claim_age"),
        "Spouse Claim Age",
        min = CLAIM_AGE_MIN,
        max = CLAIM_AGE_MAX,
        value = CLAIM_AGE_DEFAULT,
        step = 1
      )
    ),

    hr(),

    # Scenario comparison
    h5("Scenarios", class = "text-info"),

    checkboxInput(
      ns("enable_comparison"),
      "Compare Scenarios",
      value = FALSE
    ),

    conditionalPanel(
      condition = sprintf("input['%s']", ns("enable_comparison")),

      helpText("Add up to 4 additional scenarios to compare"),

      actionButton(
        ns("add_scenario"),
        "Add Scenario",
        icon = icon("plus"),
        class = "btn-sm btn-outline-primary mb-2"
      ),

      actionButton(
        ns("clear_scenarios"),
        "Clear All",
        icon = icon("trash"),
        class = "btn-sm btn-outline-danger mb-2"
      ),

      uiOutput(ns("scenario_list"))
    ),

    hr(),

    # Calculate button
    actionButton(
      ns("calculate"),
      "Calculate Benefits",
      icon = icon("calculator"),
      class = "btn-primary w-100"
    ),

    # Status message
    uiOutput(ns("status"))
  )
}

# Module Server
worker_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for scenarios
    scenarios <- reactiveVal(list())
    scenario_counter <- reactiveVal(0)

    # Add scenario
    observeEvent(input$add_scenario, {
      current <- scenarios()
      if (length(current) < 4) {
        counter <- scenario_counter() + 1
        scenario_counter(counter)

        new_scenario <- list(
          id = counter,
          type = input$worker_type,
          birth_year = input$birth_year,
          sex = input$sex,
          claim_age = input$claim_age,
          custom_earnings = input$custom_earnings
        )
        current[[length(current) + 1]] <- new_scenario
        scenarios(current)
      }
    })

    # Clear scenarios
    observeEvent(input$clear_scenarios, {
      scenarios(list())
    })

    # Remove individual scenario
    observeEvent(input$remove_scenario, {
      current <- scenarios()
      # Extract scenario id from button id
      btn_id <- input$remove_scenario
      if (length(current) > 0) {
        # Remove last scenario for simplicity
        scenarios(current[-length(current)])
      }
    })

    # Render scenario list
    output$scenario_list <- renderUI({
      current <- scenarios()
      if (length(current) == 0) {
        return(helpText("No additional scenarios added"))
      }

      scenario_tags <- lapply(seq_along(current), function(i) {
        s <- current[[i]]
        type_label <- names(WORKER_TYPES)[WORKER_TYPES == s$type]
        tags$div(
          class = "card card-body p-2 mb-2",
          tags$small(
            tags$strong(paste0("Scenario ", i, ": ")),
            paste(type_label, s$birth_year, "claim at", s$claim_age)
          )
        )
      })

      do.call(tagList, scenario_tags)
    })

    # Calculate benefits reactively
    worker_data <- eventReactive(input$calculate, {

      # Show loading indicator
      showNotification("Calculating benefits...", id = "calc_notify", duration = NULL)
      on.exit(removeNotification("calc_notify"))

      tryCatch({
        # Prepare primary worker parameters
        custom_avg <- if (input$worker_type == "custom") input$custom_earnings else NULL

        # Prepare spouse parameters
        spouse_type <- if (input$add_spouse) input$spouse_type else NULL
        spouse_sex <- if (input$add_spouse) input$spouse_sex else NULL
        spouse_birth_yr <- if (input$add_spouse) input$spouse_birth_year else NULL
        spouse_claim_age <- if (input$add_spouse) input$spouse_claim_age else NULL
        spouse_custom <- if (input$add_spouse && input$spouse_type == "custom") {
          input$spouse_custom_earnings
        } else NULL

        # Calculate primary worker benefits
        primary <- calculate_benefits(
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
          debugg = TRUE  # Need full data for analytics
        )

        primary$scenario <- "Primary"

        # Calculate spouse benefits independently (for couple_measures)
        spouse_data <- NULL
        if (input$add_spouse) {
          spouse_custom_ind <- if (input$spouse_type == "custom") {
            input$spouse_custom_earnings
          } else NULL

          spouse_data <- calculate_benefits(
            birth_yr = input$spouse_birth_year,
            sex = input$spouse_sex,
            type = input$spouse_type,
            age_claim = input$spouse_claim_age,
            factors = sef2025,
            assumptions = tr2025,
            custom_avg_earnings = spouse_custom_ind,
            debugg = TRUE
          )
          spouse_data$scenario <- "Spouse"
        }

        # Calculate comparison scenarios
        comparison_data <- NULL
        current_scenarios <- scenarios()
        if (input$enable_comparison && length(current_scenarios) > 0) {
          comparison_list <- lapply(seq_along(current_scenarios), function(i) {
            s <- current_scenarios[[i]]
            s_custom <- if (s$type == "custom") s$custom_earnings else NULL

            sc_data <- calculate_benefits(
              birth_yr = s$birth_year,
              sex = s$sex,
              type = s$type,
              age_claim = s$claim_age,
              factors = sef2025,
              assumptions = tr2025,
              custom_avg_earnings = s_custom,
              debugg = TRUE
            )
            sc_data$scenario <- paste0("Scenario ", i)
            sc_data
          })

          comparison_data <- bind_rows(comparison_list)
        }

        # Return all data
        list(
          primary = primary,
          spouse = spouse_data,
          comparisons = comparison_data,
          has_spouse = input$add_spouse,
          assumptions = tr2025,
          factors = sef2025
        )

      }, error = function(e) {
        showNotification(
          paste("Error calculating benefits:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
    }, ignoreNULL = FALSE)

    # Status output
    output$status <- renderUI({
      data <- worker_data()
      if (is.null(data)) {
        return(tags$div(
          class = "alert alert-info mt-3",
          icon("info-circle"),
          " Click 'Calculate Benefits' to generate results"
        ))
      }

      primary <- data$primary
      n_scenarios <- if (!is.null(data$comparisons)) {
        length(unique(data$comparisons$scenario))
      } else 0

      tags$div(
        class = "alert alert-success mt-3",
        icon("check-circle"),
        sprintf(" Calculated benefits for %s%s",
                unique(primary$id)[1],
                if (data$has_spouse) " with spouse" else ""),
        if (n_scenarios > 0) sprintf(" + %d scenarios", n_scenarios) else ""
      )
    })

    # Return reactive data
    return(worker_data)
  })
}
