# =============================================================================
# Reform Selector Module - Shared mandatory reform selection for sidebar
# =============================================================================

# Module UI
reform_selector_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "mb-3",
      tags$label(class = "form-label fw-bold text-warning", "Select Reforms"),
      tags$small(class = "text-muted d-block mb-2",
                 "Compare baseline to selected reform(s)"),

      selectizeInput(
        ns("selected_reforms"),
        NULL,
        choices = get_reform_choices(),
        multiple = TRUE,
        options = list(
          placeholder = "Select reforms to compare...",
          plugins = list("remove_button"),
          maxItems = 5
        )
      ),

      # Reform warnings for mutual exclusivity
      uiOutput(ns("reform_warnings")),

      # Reform descriptions
      uiOutput(ns("reform_descriptions"))
    )
  )
}

# Module Server
# Returns a reactive list with:
#   - selected_reforms: character vector of selected reform names
#   - valid_reforms: character vector after exclusivity filtering
#   - reform_objects: list of Reform objects
#   - reform_assumptions: modified assumptions data frame
#   - reform_label: combined name string for display
#   - has_reforms: boolean indicating if any reforms are selected
reform_selector_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Track valid reforms after exclusivity check
    valid_reforms <- reactive({
      selected <- input$selected_reforms
      if (is.null(selected) || length(selected) == 0) {
        return(character(0))
      }

      conflicts <- check_ui_exclusivity(selected)
      valid <- selected

      if (length(conflicts) > 0) {
        for (group in names(conflicts)) {
          conflicting <- conflicts[[group]]
          # Keep only the first selected reform from each conflicting group
          to_remove <- conflicting[-1]
          valid <- setdiff(valid, to_remove)
        }
      }

      valid
    })

    # Create reform objects
    reform_objects <- reactive({
      valid <- valid_reforms()
      if (length(valid) == 0) return(NULL)

      lapply(valid, function(reform_name) {
        REFORM_LOOKUP[[reform_name]]$fn()
      })
    })

    # Create combined assumptions with all reforms applied
    reform_assumptions <- reactive({
      objects <- reform_objects()
      if (is.null(objects) || length(objects) == 0) return(NULL)

      apply_reforms(tr2025, objects, check_exclusivity = FALSE)
    })

    # Create display label for combined reforms
    reform_label <- reactive({
      valid <- valid_reforms()
      if (length(valid) == 0) return(NULL)

      if (length(valid) == 1) {
        return(valid[1])
      } else {
        return(paste0("Combined (", length(valid), " reforms)"))
      }
    })

    # Reform warnings UI
    output$reform_warnings <- renderUI({
      selected <- input$selected_reforms
      if (is.null(selected) || length(selected) < 2) return(NULL)

      conflicts <- check_ui_exclusivity(selected)
      if (length(conflicts) == 0) return(NULL)

      warning_msgs <- lapply(names(conflicts), function(group) {
        kept <- conflicts[[group]][1]
        removed <- conflicts[[group]][-1]
        tags$small(
          class = "text-warning d-block",
          icon("exclamation-triangle"),
          paste0(toupper(group), ": Using '", kept, "', ignoring: ",
                 paste(removed, collapse = ", "))
        )
      })

      tags$div(class = "mt-1 mb-2", do.call(tagList, warning_msgs))
    })

    # Reform descriptions
    output$reform_descriptions <- renderUI({
      valid <- valid_reforms()
      if (length(valid) == 0) return(NULL)

      desc_items <- lapply(valid, function(reform_name) {
        desc <- REFORM_LOOKUP[[reform_name]]$desc
        tags$small(
          class = "text-muted d-block",
          tags$strong(paste0(reform_name, ": ")),
          desc
        )
      })

      tags$div(class = "mt-2 p-2 rounded", style = "background: #1f3460;",
               do.call(tagList, desc_items))
    })

    # Return reactive state
    list(
      selected_reforms = reactive(input$selected_reforms),
      valid_reforms = valid_reforms,
      reform_objects = reform_objects,
      reform_assumptions = reform_assumptions,
      reform_label = reform_label,
      has_reforms = reactive(length(valid_reforms()) > 0)
    )
  })
}
