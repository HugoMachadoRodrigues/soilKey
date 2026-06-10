# =============================================================================
# soilKey Pro -- Settings module (v0.9.97).
#
# Controls the diagnostic engine, the WRB Tier-3 strict-mode toggle, and the
# missing-data policy. Engine and strict mode are pushed to package options
# (soilKey.diagnostic_engine, soilKey.rsg_strict) so every classifier picks
# them up; on_missing is returned as a reactive for the Classify module.
# =============================================================================

settings_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = 1 / 2,
    bslib::card(
      bslib::card_header("Diagnostic engine"),
      bslib::card_body(
        shinyWidgets::radioGroupButtons(
          ns("engine"), "Threshold engine",
          choices = c("soilKey (strict WRB)" = "soilkey",
                      "aqp (regional tolerance)" = "aqp"),
          selected = "soilkey", justified = TRUE
        ),
        shiny::helpText(
          "soilKey applies canonical WRB 2022 thresholds; aqp relaxes a few ",
          "horizon thresholds to match KSSL / regional lab methodology."
        ),
        shiny::tags$hr(),
        shinyWidgets::materialSwitch(
          ns("strict"), "WRB Tier-3 strict mode",
          value = FALSE, status = "danger"
        ),
        shiny::helpText(
          "Strengthens per-RSG numerical gates (e.g. Ferralsol CEC, Vertisol ",
          "clay). Borderline profiles may fall through to a different RSG."
        ),
        shinyWidgets::materialSwitch(
          ns("specifiers"), "WRB depth specifiers",
          value = FALSE, status = "primary"
        ),
        shiny::helpText(
          "Auto-attach Epi-/Endo-/Bathy-/Amphi- to depth-anchored qualifiers ",
          "from the feature's depth (e.g. Gleyic -> Endogleyic)."
        )
      )
    ),
    bslib::card(
      bslib::card_header("Missing-data policy"),
      bslib::card_body(
        shinyWidgets::radioGroupButtons(
          ns("on_missing"), "When an attribute the key needs is absent",
          choices = c("Warn" = "warn", "Silent" = "silent", "Error" = "error"),
          selected = "silent", justified = TRUE
        ),
        shiny::helpText(
          "Silent is recommended in the app -- missing attributes surface in ",
          "the Classify tab as 'measurements that would refine the result'."
        ),
        shiny::tags$hr(),
        shiny::checkboxInput(
          ns("include_familia"), "Resolve SiBCS 5th level (familia)",
          value = TRUE
        ),
        shiny::checkboxInput(
          ns("include_family"), "Resolve USDA 5th level (family)",
          value = FALSE
        ),
        shiny::helpText(
          "USDA family prepends class modifiers (particle-size, mineralogy, ",
          "CEC-activity, temperature regime, ...) to the subgroup name."
        )
      )
    )
  )
}

settings_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    # Push engine + strict mode into package options whenever they change.
    shiny::observeEvent(input$engine, {
      options(soilKey.diagnostic_engine = input$engine)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$strict, {
      options(soilKey.rsg_strict = isTRUE(input$strict))
    }, ignoreInit = FALSE)

    shiny::reactive({
      list(
        engine          = input$engine %||% "soilkey",
        strict          = isTRUE(input$strict),
        on_missing      = input$on_missing %||% "silent",
        include_familia = isTRUE(input$include_familia),
        include_family  = isTRUE(input$include_family),
        specifiers      = isTRUE(input$specifiers)
      )
    })
  })
}
