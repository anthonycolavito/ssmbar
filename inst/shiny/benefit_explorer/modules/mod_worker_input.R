# =============================================================================
# Worker Input Module - Redesigned for cleaner UX
# =============================================================================

# Module UI
worker_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Primary worker - compact layout
    tags$div(
      class = "mb-3",
      tags$label(class = "form-label fw-bold text-primary", "Worker Profile"),

      fluidRow(
        column(6,
          selectInput(ns("worker_type"), NULL, choices = WORKER_TYPES,
                      selected = "medium")
        ),
        column(6,
          selectInput(ns("sex"), NULL, choices = SEX_OPTIONS, selected = "all")
        )
      ),

      conditionalPanel(
        condition = sprintf("input['%s'] == 'custom'", ns("worker_type")),
        numericInput(ns("custom_earnings"), "Avg Real Earnings ($)",
                     value = CUSTOM_EARNINGS_DEFAULT, min = 1000, max = 500000, step = 1000)
      ),

      fluidRow(
        column(6,
          numericInput(ns("birth_year"), "Birth Year", value = BIRTH_YEAR_DEFAULT,
                       min = BIRTH_YEAR_MIN, max = BIRTH_YEAR_MAX, step = 1)
        ),
        column(6,
          numericInput(ns("claim_age"), "Claim Age", value = CLAIM_AGE_DEFAULT,
                       min = CLAIM_AGE_MIN, max = CLAIM_AGE_MAX, step = 1)
        )
      )
    ),

    # Spouse toggle - collapsible
    tags$div(
      class = "mb-3",
      tags$div(
        class = "d-flex align-items-center",
        checkboxInput(ns("add_spouse"), NULL, value = FALSE, width = "30px"),
        tags$label(class = "form-label mb-0 fw-bold text-success", "Add Spouse")
      ),

      conditionalPanel(
        condition = sprintf("input['%s']", ns("add_spouse")),
        tags$div(
          class = "ps-2 border-start border-success",
          fluidRow(
            column(6,
              selectInput(ns("spouse_type"), NULL, choices = WORKER_TYPES, selected = "low")
            ),
            column(6,
              selectInput(ns("spouse_sex"), NULL, choices = SEX_OPTIONS, selected = "all")
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("spouse_type")),
            numericInput(ns("spouse_custom_earnings"), "Spouse Earnings ($)",
                         value = CUSTOM_EARNINGS_DEFAULT, min = 1000, max = 500000, step = 1000)
          ),
          fluidRow(
            column(6,
              numericInput(ns("spouse_birth_year"), "Birth Yr", value = BIRTH_YEAR_DEFAULT,
                           min = BIRTH_YEAR_MIN, max = BIRTH_YEAR_MAX, step = 1)
            ),
            column(6,
              numericInput(ns("spouse_claim_age"), "Claim", value = CLAIM_AGE_DEFAULT,
                           min = CLAIM_AGE_MIN, max = CLAIM_AGE_MAX, step = 1)
            )
          )
        )
      )
    ),

    # Reform comparison - collapsible
    tags$div(
      class = "mb-3",
      tags$div(
        class = "d-flex align-items-center",
        checkboxInput(ns("enable_reforms"), NULL, value = FALSE, width = "30px"),
        tags$label(class = "form-label mb-0 fw-bold text-warning", "Compare Reforms")
      ),

      conditionalPanel(
        condition = sprintf("input['%s']", ns("enable_reforms")),
        tags$div(
          class = "ps-2 border-start border-warning",
          selectizeInput(
            ns("selected_reforms"),
            NULL,
            choices = get_reform_choices(),
            multiple = TRUE,
            options = list(
              placeholder = "Select reforms...",
              plugins = list("remove_button"),
              maxItems = 5
            )
          ),
          uiOutput(ns("reform_warnings"))
        )
      )
    ),

    # Calculate button - prominent
    actionButton(
      ns("calculate"),
      "Calculate",
      icon = icon("play"),
      class = "btn-primary w-100 btn-lg mb-2"
    ),

    # Compact status
    uiOutput(ns("status"))
  )
}

# Module Server
worker_input_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reform warnings for mutual exclusivity
    output$reform_warnings <- renderUI({
      selected <- input$selected_reforms
      if (is.null(selected) || length(selected) < 2) return(NULL)

      conflicts <- check_ui_exclusivity(selected)
      if (length(conflicts) == 0) return(NULL)

      warning_msgs <- lapply(names(conflicts), function(group) {
        tags$small(
          class = "text-warning d-block",
          icon("exclamation-triangle"),
          paste0(toupper(group), ": ", paste(conflicts[[group]], collapse = " / "))
        )
      })

      tags$div(class = "mt-1", do.call(tagList, warning_msgs))
    })

    # Calculate benefits reactively
    worker_data <- eventReactive(input$calculate, {

      showNotification("Calculating...", id = "calc_notify", duration = NULL)
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
          debugg = TRUE
        )

        primary$scenario <- "Baseline"

        # Calculate spouse benefits independently
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

        # Calculate reform scenarios
        reform_data <- NULL
        reform_summary <- NULL
        combined_assumptions <- NULL
        selected_reforms <- input$selected_reforms

        if (input$enable_reforms && !is.null(selected_reforms) && length(selected_reforms) > 0) {
          conflicts <- check_ui_exclusivity(selected_reforms)

          valid_reforms <- selected_reforms
          if (length(conflicts) > 0) {
            for (group in names(conflicts)) {
              conflicting <- conflicts[[group]]
              to_remove <- conflicting[-1]
              valid_reforms <- setdiff(valid_reforms, to_remove)
            }
          }

          if (length(valid_reforms) > 0) {
            # Create reform objects for all valid reforms
            reform_objects <- lapply(valid_reforms, function(reform_name) {
              REFORM_LOOKUP[[reform_name]]$fn()
            })

            # Apply ALL reforms together to get combined effect
            combined_assumptions <- apply_reforms(tr2025, reform_objects, check_exclusivity = FALSE)

            # Calculate benefits under combined reforms
            combined_worker <- calculate_benefits(
              birth_yr = input$birth_year,
              sex = input$sex,
              type = input$worker_type,
              age_claim = input$claim_age,
              factors = sef2025,
              assumptions = combined_assumptions,
              custom_avg_earnings = custom_avg,
              spouse_type = spouse_type,
              spouse_sex = spouse_sex,
              spouse_birth_yr = spouse_birth_yr,
              spouse_age_claim = spouse_claim_age,
              spouse_custom_avg_earnings = spouse_custom,
              debugg = TRUE
            )

            # Name the scenario based on number of reforms
            scenario_name <- if (length(valid_reforms) == 1) {
              valid_reforms[1]
            } else {
              paste0("Combined (", length(valid_reforms), " reforms)")
            }

            combined_worker$scenario <- scenario_name
            combined_worker$is_reform <- TRUE
            reform_data <- combined_worker

            reform_summary <- create_reform_summary(primary, reform_data, combined_assumptions)
          }
        }

        # Combine all comparisons
        all_comparisons <- NULL
        if (!is.null(reform_data)) {
          all_comparisons <- reform_data
        }

        list(
          primary = primary,
          spouse = spouse_data,
          comparisons = all_comparisons,
          reform_data = reform_data,
          reform_summary = reform_summary,
          has_spouse = input$add_spouse,
          has_reforms = input$enable_reforms && !is.null(reform_data),
          selected_reforms = if (input$enable_reforms) valid_reforms else NULL,
          assumptions = tr2025,
          reform_assumptions = combined_assumptions,
          factors = sef2025
        )

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        return(NULL)
      })
    }, ignoreNULL = FALSE)

    # Compact status output
    output$status <- renderUI({
      data <- worker_data()
      if (is.null(data)) {
        return(tags$small(class = "text-muted", "Configure worker and click Calculate"))
      }

      primary <- data$primary
      claim_age <- unique(primary$claim_age)[1]
      ben_at_claim <- primary$ben[primary$age == claim_age][1]

      n_reforms <- if (!is.null(data$reform_data)) length(unique(data$reform_data$scenario)) else 0

      tags$div(
        class = "alert alert-success py-2 px-3 mb-0",
        tags$strong(format_currency(ben_at_claim * 12), "/yr"),
        tags$small(class = "d-block text-muted",
          paste0("at age ", claim_age),
          if (data$has_spouse) " w/ spouse" else "",
          if (n_reforms > 0) paste0(" + ", n_reforms, " reforms") else ""
        )
      )
    })

    return(worker_data)
  })
}


#' Create Reform Summary Table
#' @keywords internal
create_reform_summary <- function(baseline, reform_data, assumptions) {
  claim_age <- unique(baseline$claim_age)[1]

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
