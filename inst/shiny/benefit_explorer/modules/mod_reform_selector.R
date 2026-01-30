# =============================================================================
# Reform Selector Module - Organized by category with radio/checkbox inputs
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

      # PIA Formula Changes (mutually exclusive)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_pia")),
          icon("calculator", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "PIA Formula"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_pia"),
          class = "ps-3",
          radioButtons(
            ns("pia_formula"),
            NULL,
            choices = c(
              "None" = "none",
              "Reduce Fact3 to 5%" = "Reduce Fact3 to 5%",
              "Flat Benefit" = "Flat Benefit",
              "Simpson-Bowles PIA" = "Simpson-Bowles PIA",
              "Mini-PIA" = "Mini-PIA"
            ),
            selected = "none"
          )
        )
      ),

      # NRA Changes (mutually exclusive)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_nra")),
          icon("clock", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Retirement Age"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_nra"),
          class = "ps-3",
          radioButtons(
            ns("nra"),
            NULL,
            choices = c(
              "None" = "none",
              "Raise NRA to 68" = "Raise NRA to 68",
              "Index NRA to Longevity" = "Index NRA to Longevity",
              "NRA to 69, then Index" = "NRA to 69, then Index"
            ),
            selected = "none"
          )
        )
      ),

      # COLA Indexing (mutually exclusive)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_cola")),
          icon("chart-line", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "COLA Indexing"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_cola"),
          class = "ps-3",
          radioButtons(
            ns("cola"),
            NULL,
            choices = c(
              "None" = "none",
              "Chained CPI (-0.3pp)" = "Chained CPI",
              "Cap COLAs at Median" = "Cap COLAs at Median",
              "CPI-E (+0.2pp)" = "CPI-E (Higher)"
            ),
            selected = "none"
          )
        )
      ),

      # Tax Changes - Taxmax (mutually exclusive)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_taxmax")),
          icon("money-bill", class = "me-2 text-info"),
          tags$span(class = "fw-bold small", "Taxable Maximum"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_taxmax"),
          class = "ps-3",
          radioButtons(
            ns("taxmax"),
            NULL,
            choices = c(
              "None" = "none",
              "90% Coverage + Credit" = "90% Coverage + 5% Credit",
              "Eliminate + 15% Credit" = "Eliminate Taxmax + 15% Credit",
              "Eliminate, No Credit" = "Eliminate Taxmax, No Credit"
            ),
            selected = "none"
          )
        )
      ),

      # Benefit Cuts (checkboxes, can combine)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_cuts")),
          icon("scissors", class = "me-2 text-danger"),
          tags$span(class = "fw-bold small", "Benefit Cuts"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_cuts"),
          class = "ps-3",
          checkboxGroupInput(
            ns("benefit_cuts"),
            NULL,
            choices = c(
              "5% Benefit Cut" = "5% Benefit Cut",
              "10% Benefit Cut" = "10% Benefit Cut"
            )
          )
        )
      ),

      # Tax Rate Changes (checkboxes)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_taxrate")),
          icon("percent", class = "me-2 text-warning"),
          tags$span(class = "fw-bold small", "Tax Rate"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_taxrate"),
          class = "ps-3",
          checkboxGroupInput(
            ns("tax_rate"),
            NULL,
            choices = c(
              "+1pp Tax Rate" = "Increase Tax Rate +1pp",
              "+2pp Tax Rate" = "Increase Tax Rate +2pp"
            )
          )
        )
      ),

      # Benefit Enhancements (checkboxes)
      tags$div(
        class = "reform-section mb-3",
        tags$div(
          class = "reform-section-header d-flex align-items-center mb-2",
          style = "cursor: pointer;",
          onclick = sprintf("$('#%s').slideToggle(200)", ns("section_enhance")),
          icon("plus-circle", class = "me-2 text-success"),
          tags$span(class = "fw-bold small", "Benefit Enhancements"),
          tags$span(class = "ms-auto text-muted small", icon("chevron-down"))
        ),
        tags$div(
          id = ns("section_enhance"),
          class = "ps-3",
          checkboxGroupInput(
            ns("enhancements"),
            NULL,
            choices = c(
              "Basic Minimum Benefit" = "Basic Minimum Benefit",
              "Child Care Credit" = "Child Care Credit",
              "75% Widow Benefit" = "75% Widow Benefit"
            )
          )
        )
      ),

      # Other Reforms (checkboxes)
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
              "40-Year Averaging" = "40-Year Averaging",
              "Repeal RET" = "Repeal RET",
              "Phase Out Spousal" = "Phase Out Spousal"
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
      #%s .radio label, #%s .checkbox label {
        font-size: 0.85rem;
        padding: 2px 0;
      }
      #%s .form-check {
        margin-bottom: 2px;
      }
    ", ns(""), ns(""), ns(""), ns(""), ns(""))))
  )
}

# Module Server
reform_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Clear all selections
    observeEvent(input$clear_all, {
      updateRadioButtons(session, "pia_formula", selected = "none")
      updateRadioButtons(session, "nra", selected = "none")
      updateRadioButtons(session, "cola", selected = "none")
      updateRadioButtons(session, "taxmax", selected = "none")
      updateCheckboxGroupInput(session, "benefit_cuts", selected = character(0))
      updateCheckboxGroupInput(session, "tax_rate", selected = character(0))
      updateCheckboxGroupInput(session, "enhancements", selected = character(0))
      updateCheckboxGroupInput(session, "other_reforms", selected = character(0))
    })

    # Collect all selected reforms
    all_selected <- reactive({
      selected <- character(0)

      # Radio button selections (exclude "none")
      if (!is.null(input$pia_formula) && input$pia_formula != "none") {
        selected <- c(selected, input$pia_formula)
      }
      if (!is.null(input$nra) && input$nra != "none") {
        selected <- c(selected, input$nra)
      }
      if (!is.null(input$cola) && input$cola != "none") {
        selected <- c(selected, input$cola)
      }
      if (!is.null(input$taxmax) && input$taxmax != "none") {
        selected <- c(selected, input$taxmax)
      }

      # Checkbox selections
      if (!is.null(input$benefit_cuts)) {
        selected <- c(selected, input$benefit_cuts)
      }
      if (!is.null(input$tax_rate)) {
        selected <- c(selected, input$tax_rate)
      }
      if (!is.null(input$enhancements)) {
        selected <- c(selected, input$enhancements)
      }
      if (!is.null(input$other_reforms)) {
        selected <- c(selected, input$other_reforms)
      }

      selected
    })

    # Valid reforms (same as selected since we enforce exclusivity via UI)
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
