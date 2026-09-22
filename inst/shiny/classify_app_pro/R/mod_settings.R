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
      bslib::card_header(i18n("settings.diagnostic_engine")),
      bslib::card_body(
        shiny::helpText(
          "These options set how every classifier reads horizon data. ",
          "They apply globally to all tabs of this session."
        ),

        sk_section(
          i18n("settings.threshold_engine"),
          desc = "Which set of numeric thresholds decides whether a horizon meets each diagnostic criterion.",
          icon = "gear",
          shinyWidgets::radioGroupButtons(
            ns("engine"),
            sk_label(i18n("settings.threshold_engine"),
                     "soilKey uses the package's built-in thresholds; aqp defers to the aqp package where it has a matching rule."),
            choices = stats::setNames(
              c("soilkey", "aqp"),
              c(i18n("settings.engine_soilkey"),
                i18n("settings.engine_aqp"))
            ),
            selected = "soilkey", justified = TRUE
          ),
          shiny::helpText(
            i18n("settings.engine_help")
          )
        ),

        sk_section(
          i18n("settings.strict_mode"),
          desc = "How deep the classification goes below the reference group or order.",
          icon = "sliders",
          shinyWidgets::materialSwitch(
            ns("strict"),
            sk_label(i18n("settings.strict_mode"),
                     "When on, a class is only assigned if every required diagnostic is met; borderline profiles stay unclassified rather than being forced."),
            value = FALSE, status = "danger"
          ),
          shiny::helpText(
            i18n("settings.strict_help")
          ),
          shinyWidgets::materialSwitch(
            ns("specifiers"),
            sk_label(i18n("settings.specifiers"),
                     "Add WRB principal and supplementary qualifiers (the words before and after the reference group) to the result."),
            value = FALSE, status = "primary"
          ),
          shiny::helpText(
            i18n("settings.specifiers_help")
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header(i18n("settings.missing_data_policy")),
      bslib::card_body(
        shiny::helpText(
          "Control how missing measurements are handled and how much taxonomic ",
          "detail the classifiers report."
        ),

        sk_section(
          i18n("settings.on_missing_label"),
          desc = "What the classifier does when a horizon lacks a value a rule needs.",
          icon = "flask",
          shinyWidgets::radioGroupButtons(
            ns("on_missing"),
            sk_label(i18n("settings.on_missing_label"),
                     "Warn keeps going but flags gaps; Silent skips the affected rules quietly; Error stops so nothing is guessed."),
            choices = stats::setNames(
              c("warn", "silent", "error"),
              c(i18n("settings.on_missing_warn"),
                i18n("settings.on_missing_silent"),
                i18n("settings.on_missing_error"))
            ),
            selected = "silent", justified = TRUE
          ),
          shiny::helpText(
            i18n("settings.on_missing_help")
          )
        ),

        sk_section(
          i18n("settings.include_familia"),
          desc = "Whether to resolve the deepest, lowest-level categories in each taxonomy.",
          icon = "layer-group",
          shiny::checkboxInput(
            ns("include_familia"),
            sk_label(i18n("settings.include_familia"),
                     "Also derive the SiBCS 'família' level (texture, mineralogy and other family attributes) beneath the subgroup."),
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("include_family"),
            sk_label(i18n("settings.include_family"),
                     "Also derive the USDA Soil Taxonomy family level (particle-size, mineralogy and temperature classes) beneath the subgroup."),
            value = FALSE
          ),
          shiny::helpText(
            i18n("settings.family_help")
          )
        )
      )
    )
  )
}

settings_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Push engine + strict mode into package options whenever they change.
    shiny::observeEvent(input$engine, {
      options(soilKey.diagnostic_engine = input$engine)
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$strict, {
      options(soilKey.rsg_strict = isTRUE(input$strict))
    }, ignoreInit = FALSE)

    # ---- two-way sync of the depth-level toggles with the shared rv ---------
    # rv is the single source of truth (the Classify tab mirrors the same two
    # switches). The `identical()` guards make the round-trip idempotent, so
    # updating the widget from rv never bounces back and writes rv again.
    shiny::observeEvent(input$include_family, {
      v <- isTRUE(input$include_family)
      if (!identical(v, isTRUE(rv$include_family))) rv$include_family <- v
    }, ignoreInit = TRUE)
    shiny::observeEvent(rv$include_family, {
      v <- isTRUE(rv$include_family)
      if (!identical(v, isTRUE(input$include_family)))
        shiny::updateCheckboxInput(session, "include_family", value = v)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$specifiers, {
      v <- isTRUE(input$specifiers)
      if (!identical(v, isTRUE(rv$specifiers))) rv$specifiers <- v
    }, ignoreInit = TRUE)
    shiny::observeEvent(rv$specifiers, {
      v <- isTRUE(rv$specifiers)
      if (!identical(v, isTRUE(input$specifiers)))
        shinyWidgets::updateMaterialSwitch(session, "specifiers", value = v)
    }, ignoreInit = TRUE)

    shiny::reactive({
      list(
        engine          = input$engine %||% "soilkey",
        strict          = isTRUE(input$strict),
        on_missing      = input$on_missing %||% "silent",
        include_familia = isTRUE(input$include_familia),
        # Read the depth-level flags from rv so Settings and Classify agree.
        include_family  = isTRUE(rv$include_family),
        specifiers      = isTRUE(rv$specifiers)
      )
    })
  })
}
