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

    # Policy Reform Comparison
    h5("Policy Reforms", class = "text-warning"),

    checkboxInput(
      ns("enable_reforms"),
      "Compare Reforms",
      value = FALSE
    ),

    conditionalPanel(
      condition = sprintf("input['%s']", ns("enable_reforms")),

      helpText("Select reforms to compare against current law"),

      # Reform selection by category
      selectizeInput(
        ns("selected_reforms"),
        "Select Reforms",
        choices = get_reform_choices(),
        multiple = TRUE,
        options = list(
          placeholder = "Choose reforms...",
          plugins = list("remove_button")
        )
      ),

      # Warning for mutual exclusivity
      uiOutput(ns("reform_warnings")),

      # Selected reform descriptions
      uiOutput(ns("reform_descriptions"))
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

    # Reform warnings for mutual exclusivity
    output$reform_warnings <- renderUI({
      selected <- input$selected_reforms
      if (is.null(selected) || length(selected) < 2) return(NULL)

      conflicts <- check_ui_exclusivity(selected)
      if (length(conflicts) == 0) return(NULL)

      warning_msgs <- lapply(names(conflicts), function(group) {
        tags$div(
          class = "alert alert-warning py-1 px-2 mb-1",
          tags$small(
            icon("exclamation-triangle"),
            paste0(" ", toupper(group), ": Only one allowed - ",
                   paste(conflicts[[group]], collapse = ", "))
          )
        )
      })

      do.call(tagList, warning_msgs)
    })

    # Reform descriptions
    output$reform_descriptions <- renderUI({
      selected <- input$selected_reforms
      if (is.null(selected) || length(selected) == 0) return(NULL)

      desc_tags <- lapply(selected, function(reform_name) {
        reform_info <- REFORM_LOOKUP[[reform_name]]
        desc <- if (!is.null(reform_info$desc)) reform_info$desc else ""
        tags$div(
          class = "small text-muted mb-1",
          tags$strong(reform_name), ": ", desc
        )
      })

      tags$div(class = "mt-2", do.call(tagList, desc_tags))
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

        # Calculate comparison scenarios (different worker configs)
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

        # Calculate reform scenarios
        reform_data <- NULL
        reform_summary <- NULL
        selected_reforms <- input$selected_reforms

        if (input$enable_reforms && !is.null(selected_reforms) && length(selected_reforms) > 0) {
          # Check for conflicts and filter to valid reforms
          conflicts <- check_ui_exclusivity(selected_reforms)

          # If conflicts, keep only first reform from each conflicting group
          valid_reforms <- selected_reforms
          if (length(conflicts) > 0) {
            for (group in names(conflicts)) {
              conflicting <- conflicts[[group]]
              # Keep only the first one selected
              to_remove <- conflicting[-1]
              valid_reforms <- setdiff(valid_reforms, to_remove)
            }
          }

          if (length(valid_reforms) > 0) {
            reform_list <- lapply(valid_reforms, function(reform_name) {
              reform_info <- REFORM_LOOKUP[[reform_name]]
              reform_obj <- reform_info$fn()

              # Apply reform to assumptions
              reformed_assumptions <- apply_reform(tr2025, reform_obj)

              # Calculate benefits under reform
              reform_worker <- calculate_benefits(
                birth_yr = input$birth_year,
                sex = input$sex,
                type = input$worker_type,
                age_claim = input$claim_age,
                factors = sef2025,
                assumptions = reformed_assumptions,
                custom_avg_earnings = custom_avg,
                spouse_type = spouse_type,
                spouse_sex = spouse_sex,
                spouse_birth_yr = spouse_birth_yr,
                spouse_age_claim = spouse_claim_age,
                spouse_custom_avg_earnings = spouse_custom,
                debugg = TRUE
              )

              reform_worker$scenario <- reform_name
              reform_worker$is_reform <- TRUE
              reform_worker
            })

            reform_data <- bind_rows(reform_list)

            # Create summary comparison
            reform_summary <- create_reform_summary(primary, reform_data, tr2025)
          }
        }

        # Combine all comparisons
        all_comparisons <- NULL
        if (!is.null(comparison_data) || !is.null(reform_data)) {
          all_comparisons <- bind_rows(
            comparison_data,
            reform_data
          )
        }

        # Return all data
        list(
          primary = primary,
          spouse = spouse_data,
          comparisons = all_comparisons,
          reform_data = reform_data,
          reform_summary = reform_summary,
          has_spouse = input$add_spouse,
          has_reforms = input$enable_reforms && !is.null(reform_data),
          selected_reforms = if (input$enable_reforms) selected_reforms else NULL,
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
      n_scenarios <- if (!is.null(data$comparisons) && !is.null(data$comparisons$is_reform)) {
        sum(!data$comparisons$is_reform[!duplicated(data$comparisons$scenario)], na.rm = TRUE)
      } else if (!is.null(data$comparisons)) {
        sum(is.na(data$comparisons$is_reform[!duplicated(data$comparisons$scenario)]))
      } else 0

      n_reforms <- if (!is.null(data$reform_data)) {
        length(unique(data$reform_data$scenario))
      } else 0

      msg_parts <- c(
        sprintf("Calculated benefits for %s", unique(primary$id)[1]),
        if (data$has_spouse) " with spouse" else "",
        if (n_scenarios > 0) sprintf(" + %d scenarios", n_scenarios) else "",
        if (n_reforms > 0) sprintf(" + %d reforms", n_reforms) else ""
      )

      tags$div(
        class = "alert alert-success mt-3",
        icon("check-circle"),
        paste(msg_parts, collapse = "")
      )
    })

    # Return reactive data
    return(worker_data)
  })
}


#' Create Reform Summary Table
#'
#' Helper function to create a summary comparison table for reforms
#' @keywords internal
create_reform_summary <- function(baseline, reform_data, assumptions) {
  # Get baseline metrics
  claim_age <- unique(baseline$claim_age)[1]
  death_age <- unique(baseline$death_age)[1]

  baseline_ben <- baseline$ben[baseline$age == claim_age][1]
  baseline_pv <- tryCatch({
    pv_lifetime_benefits(baseline, assumptions)$pv_benefits[1]
  }, error = function(e) NA_real_)
  baseline_pv_tax <- tryCatch({
    pv_lifetime_taxes(baseline, assumptions)$pv_taxes[1]
  }, error = function(e) NA_real_)
  baseline_ratio <- if (!is.na(baseline_pv) && !is.na(baseline_pv_tax) && baseline_pv_tax > 0) {
    baseline_pv / baseline_pv_tax
  } else NA_real_

  # Start with baseline row
  summary_df <- data.frame(
    scenario = "Baseline",
    monthly_benefit = baseline_ben,
    pv_benefits = baseline_pv,
    pv_taxes = baseline_pv_tax,
    benefit_tax_ratio = baseline_ratio,
    pct_change_benefit = 0,
    pct_change_pv = 0,
    stringsAsFactors = FALSE
  )

  # Add reform rows
  reform_scenarios <- unique(reform_data$scenario)
  for (scenario_name in reform_scenarios) {
    reform_subset <- reform_data[reform_data$scenario == scenario_name, ]

    reform_ben <- reform_subset$ben[reform_subset$age == claim_age][1]
    reform_pv <- tryCatch({
      pv_lifetime_benefits(reform_subset, assumptions)$pv_benefits[1]
    }, error = function(e) NA_real_)
    reform_pv_tax <- tryCatch({
      pv_lifetime_taxes(reform_subset, assumptions)$pv_taxes[1]
    }, error = function(e) NA_real_)
    reform_ratio <- if (!is.na(reform_pv) && !is.na(reform_pv_tax) && reform_pv_tax > 0) {
      reform_pv / reform_pv_tax
    } else NA_real_

    pct_benefit <- if (!is.na(reform_ben) && !is.na(baseline_ben) && baseline_ben > 0) {
      (reform_ben / baseline_ben - 1) * 100
    } else NA_real_

    pct_pv <- if (!is.na(reform_pv) && !is.na(baseline_pv) && baseline_pv > 0) {
      (reform_pv / baseline_pv - 1) * 100
    } else NA_real_

    summary_df <- rbind(summary_df, data.frame(
      scenario = scenario_name,
      monthly_benefit = reform_ben,
      pv_benefits = reform_pv,
      pv_taxes = reform_pv_tax,
      benefit_tax_ratio = reform_ratio,
      pct_change_benefit = pct_benefit,
      pct_change_pv = pct_pv,
      stringsAsFactors = FALSE
    ))
  }

  summary_df
}
