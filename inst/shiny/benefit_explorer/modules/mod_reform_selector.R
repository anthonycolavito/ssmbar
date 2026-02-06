# =============================================================================
# Reform Selector Module - Organized by category with checkbox inputs
# =============================================================================

# Module UI
reform_selector_ui <- function(id) {

  ns <- NS(id)

  tagList(
    tags$div(
      class = "reform-selector",

      # Header
      tags$label(class = "form-label fw-bold text-warning mb-2", "Select Reforms"),
      tags$small(class = "text-muted d-block mb-3",
                 "Choose reforms to compare against baseline"),

      # Slow Initial Benefit Growth (mutually exclusive via server)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_pia")),
          icon("calculator", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Slow Initial Benefit Growth"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_pia"),
          class = "ps-3",
          checkboxGroupInput(
            ns("pia_formula"),
            NULL,
            choices = c(
              "Slow Benefit Growth for Top 20% of Earners" = "Reduce Fact3 to 5%",
              "Transition to a Flat Benefit" = "Flat Benefit",
              "Slow Benefit Growth for Top Half of Earners" = "Simpson-Bowles PIA"
            )
          )
        )
      ),

      # Increase Retirement Age (mutually exclusive via server)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_nra")),
          icon("clock", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Increase Retirement Age"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_nra"),
          class = "ps-3",
          checkboxGroupInput(
            ns("nra"),
            NULL,
            choices = c(
              "Raise NRA to 68" = "Raise NRA to 68",
              "Index NRA to Longevity" = "Index NRA to Longevity",
              "NRA to 69, then Index" = "NRA to 69, then Index"
            )
          )
        )
      ),

      # Modify COLAs (mutually exclusive via server)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_cola")),
          icon("chart-line", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Modify Cost-of-Living Adjustments (COLAs)"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_cola"),
          class = "ps-3",
          checkboxGroupInput(
            ns("cola"),
            NULL,
            choices = c(
              "Index COLAs to \"Chained CPI\"" = "Chained CPI",
              "Cap COLAs for Top Half of Beneficiaries" = "Cap COLAs at Median",
              "Index COLAs to \"CPI-E\"" = "CPI-E (Higher)"
            )
          )
        )
      ),

      # Increase Taxable Maximum (mutually exclusive via server)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_taxmax")),
          icon("money-bill", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Increase $184,500 Taxable Maximum"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_taxmax"),
          class = "ps-3",
          checkboxGroupInput(
            ns("taxmax"),
            NULL,
            choices = c(
              "Raise Tax Max to Cover 90% of Wages ($330,500 in 2026)" = "90% Coverage + 5% Credit",
              "Eliminate Tax Max with Benefit Credit for Additional Payments" = "Eliminate Taxmax + 15% Credit",
              "Eliminate Tax Max without Benefit Credit" = "Eliminate Taxmax, No Credit"
            )
          )
        )
      ),

      # Other Reforms (checkboxes, can combine)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_other")),
          icon("cog", class = "me-2 text-muted"),
          tags$span(class = "fw-bold small", "Other Reforms"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_other"),
          class = "ps-3",
          checkboxGroupInput(
            ns("other_reforms"),
            NULL,
            choices = c(
              "Calculate Benefits Based on Highest 40 Years" = "40-Year Averaging",
              "Phase Out Non-Widow(er) Spousal Benefit" = "Phase Out Spousal",
              "Apply the Benefit Formula to Annual Earnings" = "Mini-PIA",
              "Repeal the Retirement Earnings Test" = "Repeal RET"
            )
          )
        )
      ),

      # Enact Benefit Enhancements (checkboxes, can combine) — last section
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_enhance")),
          icon("plus-circle", class = "me-2 text-success"),
          tags$span(class = "fw-bold small", "Enact Benefit Enhancements"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_enhance"),
          class = "ps-3",
          checkboxGroupInput(
            ns("enhancements"),
            NULL,
            choices = c(
              "Establish a Basic Minimum Benefit" = "Basic Minimum Benefit",
              "Provide Earnings Credit for Child Care" = "Child Care Credit",
              "Expand Widow(er) Benefits to 75% of a Couple's Combined Benefit" = "75% Widow Benefit"
            )
          )
        )
      ),

      # Clear all button
      tags$div(
        class = "mt-3 mb-2",
        actionButton(ns("clear_all"), "Clear All",
                     icon = icon("times"), class = "btn-sm btn-outline-secondary w-100")
      ),

      # Selected reforms summary
      uiOutput(ns("selected_summary"))
    ),

    # Custom CSS for compact styling
    tags$style(HTML(sprintf("
      #%s .reform-section {
        border-left: 2px solid #2a3f5f;
        padding-left: 8px;
      }
      #%s .reform-section:hover {
        border-left-color: #9ACDFF;
      }
      #%s .checkbox label {
        font-size: 0.78rem;
        padding: 2px 0;
        white-space: normal;
        word-wrap: break-word;
      }
      #%s .form-check {
        margin-bottom: 2px;
      }
    ", ns(""), ns(""), ns(""), ns(""))))
  )
}

# Module Server
reform_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Mutual exclusivity enforcement for checkbox groups ---
    # PIA Formula: max 1 selection
    observeEvent(input$pia_formula, {
      if (length(input$pia_formula) > 1) {
        # Keep only the most recently added (last element)
        updateCheckboxGroupInput(session, "pia_formula",
                                 selected = tail(input$pia_formula, 1))
      }
    }, ignoreNULL = FALSE)

    # NRA: max 1 selection
    observeEvent(input$nra, {
      if (length(input$nra) > 1) {
        updateCheckboxGroupInput(session, "nra",
                                 selected = tail(input$nra, 1))
      }
    }, ignoreNULL = FALSE)

    # COLA: max 1 selection
    observeEvent(input$cola, {
      if (length(input$cola) > 1) {
        updateCheckboxGroupInput(session, "cola",
                                 selected = tail(input$cola, 1))
      }
    }, ignoreNULL = FALSE)

    # Taxmax: max 1 selection
    observeEvent(input$taxmax, {
      if (length(input$taxmax) > 1) {
        updateCheckboxGroupInput(session, "taxmax",
                                 selected = tail(input$taxmax, 1))
      }
    }, ignoreNULL = FALSE)

    # Clear all selections
    observeEvent(input$clear_all, {
      updateCheckboxGroupInput(session, "pia_formula", selected = character(0))
      updateCheckboxGroupInput(session, "nra", selected = character(0))
      updateCheckboxGroupInput(session, "cola", selected = character(0))
      updateCheckboxGroupInput(session, "taxmax", selected = character(0))
      updateCheckboxGroupInput(session, "enhancements", selected = character(0))
      updateCheckboxGroupInput(session, "other_reforms", selected = character(0))
    })

    # Collect all selected reforms
    all_selected <- reactive({
      selected <- character(0)

      # Mutually exclusive checkbox group selections
      if (!is.null(input$pia_formula) && length(input$pia_formula) > 0) {
        selected <- c(selected, input$pia_formula)
      }
      if (!is.null(input$nra) && length(input$nra) > 0) {
        selected <- c(selected, input$nra)
      }
      if (!is.null(input$cola) && length(input$cola) > 0) {
        selected <- c(selected, input$cola)
      }
      if (!is.null(input$taxmax) && length(input$taxmax) > 0) {
        selected <- c(selected, input$taxmax)
      }

      # Multi-select checkbox selections
      if (!is.null(input$enhancements)) {
        selected <- c(selected, input$enhancements)
      }
      if (!is.null(input$other_reforms)) {
        selected <- c(selected, input$other_reforms)
      }

      selected
    })

    # Valid reforms (same as selected since we enforce exclusivity via server)
    valid_reforms <- reactive({
      all_selected()
    })

    # Create reform objects
    reform_objects <- reactive({
      valid <- valid_reforms()
      if (length(valid) == 0) return(NULL)

      lapply(valid, function(reform_name) {
        REFORM_LOOKUP[[reform_name]]$fn()
      })
    })

    # Create combined assumptions
    reform_assumptions <- reactive({
      objects <- reform_objects()
      if (is.null(objects) || length(objects) == 0) return(NULL)

      apply_reforms(tr2025, objects, check_exclusivity = FALSE)
    })

    # Create display label
    reform_label <- reactive({
      valid <- valid_reforms()
      if (length(valid) == 0) return(NULL)

      if (length(valid) == 1) {
        return(valid[1])
      } else {
        return(paste0("Combined (", length(valid), " reforms)"))
      }
    })

    # Selected summary
    output$selected_summary <- renderUI({
      selected <- valid_reforms()
      if (length(selected) == 0) {
        return(tags$div(
          class = "text-center text-muted small p-2",
          "No reforms selected (showing baseline only)"
        ))
      }

      tags$div(
        class = "mt-2 p-2 rounded", style = "background: #1f3460;",
        tags$small(class = "text-warning d-block mb-1",
                   icon("check-circle"),
                   paste0(" ", length(selected), " reform(s) selected:")),
        lapply(selected, function(r) {
          desc <- REFORM_LOOKUP[[r]]$desc
          tags$small(
            class = "text-muted d-block",
            paste0("\u2022 ", r)
          )
        })
      )
    })

    # Return reactive state
    list(
      selected_reforms = all_selected,
      valid_reforms = valid_reforms,
      reform_objects = reform_objects,
      reform_assumptions = reform_assumptions,
      reform_label = reform_label,
      has_reforms = reactive(length(valid_reforms()) > 0)
    )
  })
}
