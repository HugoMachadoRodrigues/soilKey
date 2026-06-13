# =============================================================================
# soilKey Pro -- Classify module (v0.9.97).
#
# Runs WRB 2022 / SiBCS 5 / USDA ST 13 on the shared pedon and shows the three
# results side-by-side, the deterministic key trace per system, the close-call
# ambiguities, and the measurements that would refine the result.
# =============================================================================

classify_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h5(i18n("classify.run_classification")),
      shiny::checkboxGroupInput(
        ns("systems"), i18n("classify.systems"),
        choices  = c("WRB 2022" = "wrb2022", "SiBCS 5" = "sibcs",
                     "USDA ST 13" = "usda"),
        selected = c("wrb2022", "sibcs", "usda")
      ),
      shiny::actionButton(ns("run"), i18n("classify.run"),
                          icon = shiny::icon("play"),
                          class = "btn-primary w-100"),
      shiny::tags$hr(),
      # The two deepest-level options live on the Settings tab, but they are
      # surfaced here too so the user can discover and flip them without
      # leaving Classify. Both switches two-way-sync with the shared rv, so
      # they stay identical to the Settings tab's controls.
      shiny::h6(i18n("classify.deepest_level")),
      shinyWidgets::materialSwitch(
        ns("include_family"), i18n("classify.usda_family"),
        value = FALSE, status = "primary"),
      shinyWidgets::materialSwitch(
        ns("specifiers"), i18n("classify.wrb_depth_specifiers"),
        value = FALSE, status = "primary"),
      shiny::tags$hr(),
      shiny::helpText(
        i18n("classify.key_deterministic")
      ),
      shiny::uiOutput(ns("engine_note"))
    ),
    shiny::uiOutput(ns("body"))
  )
}

classify_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- mirror the depth-level switches onto the shared rv -----------------
    # Same guarded two-way sync as the Settings module: rv is the source of
    # truth, the identical() guards keep the round-trip from looping. Flipping
    # the switch here therefore also moves the matching Settings switch (and
    # feeds settings(), which the classification below reads).
    shiny::observeEvent(input$include_family, {
      v <- isTRUE(input$include_family)
      if (!identical(v, isTRUE(rv$include_family))) rv$include_family <- v
    }, ignoreInit = TRUE)
    shiny::observeEvent(rv$include_family, {
      v <- isTRUE(rv$include_family)
      if (!identical(v, isTRUE(input$include_family)))
        shinyWidgets::updateMaterialSwitch(session, "include_family", value = v)
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

    results <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      cfg <- settings()
      sys <- input$systems
      if (length(sys) == 0L) {
        shiny::showNotification(i18n("classify.pick_one_system"), type = "warning")
        return(NULL)
      }
      shiny::withProgress(message = i18n("classify.classifying"), value = 0.5, {
        soilKey::classify_all(
          rv$pedon,
          systems         = sys,
          on_missing      = cfg$on_missing,
          include_familia = cfg$include_familia,
          include_family  = isTRUE(cfg$include_family),
          specifiers      = isTRUE(cfg$specifiers)
        )
      })
    })

    output$engine_note <- shiny::renderUI({
      cfg <- settings()
      shiny::div(
        class = "small text-muted mt-2",
        i18n("classify.engine_note", cfg$engine,
             if (isTRUE(cfg$strict)) i18n("classify.tier3_strict_on") else "")
      )
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      if (is.null(results())) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("play"),
                          i18n("classify.press_classify")))
      }
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          pro_result_card(results()$wrb,   "WRB 2022"),
          pro_result_card(results()$sibcs, "SiBCS 5"),
          pro_result_card(results()$usda,  "USDA ST 13")
        ),
        bslib::navset_card_tab(
          title = i18n("classify.decision_detail"),
          bslib::nav_panel(
            i18n("classify.key_trace"),
            shiny::selectInput(ns("trace_sys"), i18n("classify.system"),
                               choices = c("WRB" = "wrb", "SiBCS" = "sibcs",
                                           "USDA" = "usda"),
                               selected = "wrb"),
            DT::DTOutput(ns("trace_table"))
          ),
          bslib::nav_panel(
            i18n("classify.ambiguities"),
            shiny::uiOutput(ns("ambiguities"))
          ),
          bslib::nav_panel(
            i18n("classify.missing_data"),
            shiny::helpText(i18n("classify.measuring_refine")),
            shiny::verbatimTextOutput(ns("missing"))
          )
        )
      )
    })

    output$trace_table <- DT::renderDT({
      res <- results()
      shiny::req(res)
      r <- res[[input$trace_sys %||% "wrb"]]
      if (is.null(r) || is.null(r$trace) || length(r$trace) == 0L) {
        return(DT::datatable(
          stats::setNames(data.frame(i18n("classify.no_trace_available")),
                          i18n("classify.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      }
      pass_lbl <- i18n("classify.status_pass")
      fail_lbl <- i18n("classify.status_fail")
      tr <- do.call(rbind, lapply(r$trace, function(t) {
        data.frame(
          code    = t$code   %||% "?",
          name    = t$name   %||% "?",
          status  = {
            p <- t$passed %||% t$status
            if (isTRUE(p)) pass_lbl
            else if (isFALSE(p)) fail_lbl
            else as.character(p %||% i18n("classify.status_indeterminate"))
          },
          missing = paste(t$missing %||% character(0), collapse = ", "),
          stringsAsFactors = FALSE
        )
      }))
      DT::datatable(tr, rownames = FALSE,
                    colnames = c(i18n("classify.col_code"),
                                 i18n("classify.col_name"),
                                 i18n("classify.col_status"),
                                 i18n("classify.col_missing")),
                    options = list(pageLength = 15, dom = "tip")) |>
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            c(pass_lbl, fail_lbl), c("#d1e7dd", "#f8d7da"))
        )
    })

    output$ambiguities <- shiny::renderUI({
      res <- results()
      shiny::req(res)
      amb <- res$wrb$ambiguities %||% list()
      if (length(amb) == 0L) {
        return(shiny::div(class = "text-muted",
                          i18n("classify.no_close_calls")))
      }
      shiny::tags$ul(lapply(amb, function(a) {
        shiny::tags$li(
          shiny::strong(a$name %||% a$code %||% "?"), i18n("classify.amb_sep"),
          a$reason %||% a$note %||% i18n("classify.near_miss")
        )
      }))
    })

    output$missing <- shiny::renderText({
      res <- results()
      shiny::req(res)
      miss <- character(0)
      for (nm in c("wrb", "sibcs", "usda")) {
        r <- res[[nm]]
        if (!is.null(r) && !inherits(r, "error"))
          miss <- unique(c(miss, r$missing_data %||% character(0)))
      }
      if (length(miss) == 0L) i18n("classify.no_missing_complete")
      else paste(sort(miss), collapse = "\n")
    })

    # Expose results so the Report module can reuse them.
    results
  })
}
