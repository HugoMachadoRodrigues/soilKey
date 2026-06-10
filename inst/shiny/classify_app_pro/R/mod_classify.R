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
      shiny::h5("Run classification"),
      shiny::checkboxGroupInput(
        ns("systems"), "Systems",
        choices  = c("WRB 2022" = "wrb2022", "SiBCS 5" = "sibcs",
                     "USDA ST 13" = "usda"),
        selected = c("wrb2022", "sibcs", "usda")
      ),
      shiny::actionButton(ns("run"), "Classify",
                          icon = shiny::icon("play"),
                          class = "btn-primary w-100"),
      shiny::tags$hr(),
      shiny::helpText(
        "The taxonomic key is deterministic -- identical inputs always give ",
        "the same class. Engine and strict mode are set on the Settings tab."
      ),
      shiny::uiOutput(ns("engine_note"))
    ),
    shiny::uiOutput(ns("body"))
  )
}

classify_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    results <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      cfg <- settings()
      sys <- input$systems
      if (length(sys) == 0L) {
        shiny::showNotification("Pick at least one system.", type = "warning")
        return(NULL)
      }
      shiny::withProgress(message = "Classifying...", value = 0.5, {
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
        sprintf("Engine: %s%s", cfg$engine,
                if (isTRUE(cfg$strict)) " | Tier-3 strict ON" else "")
      )
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      if (is.null(results())) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("play"),
                          " Press Classify to run the keys."))
      }
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          pro_result_card(results()$wrb,   "WRB 2022"),
          pro_result_card(results()$sibcs, "SiBCS 5"),
          pro_result_card(results()$usda,  "USDA ST 13")
        ),
        bslib::navset_card_tab(
          title = "Decision detail",
          bslib::nav_panel(
            "Key trace",
            shiny::selectInput(ns("trace_sys"), "System",
                               choices = c("WRB" = "wrb", "SiBCS" = "sibcs",
                                           "USDA" = "usda"),
                               selected = "wrb"),
            DT::DTOutput(ns("trace_table"))
          ),
          bslib::nav_panel(
            "Ambiguities",
            shiny::uiOutput(ns("ambiguities"))
          ),
          bslib::nav_panel(
            "Missing data",
            shiny::helpText("Measuring these would refine or resolve the result."),
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
        return(DT::datatable(data.frame(Note = "No trace available"),
                             rownames = FALSE, options = list(dom = "t")))
      }
      tr <- do.call(rbind, lapply(r$trace, function(t) {
        data.frame(
          code    = t$code   %||% "?",
          name    = t$name   %||% "?",
          status  = {
            p <- t$passed %||% t$status
            if (isTRUE(p)) "pass"
            else if (isFALSE(p)) "fail"
            else as.character(p %||% "indeterminate")
          },
          missing = paste(t$missing %||% character(0), collapse = ", "),
          stringsAsFactors = FALSE
        )
      }))
      DT::datatable(tr, rownames = FALSE,
                    options = list(pageLength = 15, dom = "tip")) |>
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            c("pass", "fail"), c("#d1e7dd", "#f8d7da"))
        )
    })

    output$ambiguities <- shiny::renderUI({
      res <- results()
      shiny::req(res)
      amb <- res$wrb$ambiguities %||% list()
      if (length(amb) == 0L) {
        return(shiny::div(class = "text-muted", "No close calls -- the WRB ",
                          "result is unambiguous."))
      }
      shiny::tags$ul(lapply(amb, function(a) {
        shiny::tags$li(
          shiny::strong(a$name %||% a$code %||% "?"), " -- ",
          a$reason %||% a$note %||% "near miss"
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
      if (length(miss) == 0L) "(no missing data -- profile is complete)"
      else paste(sort(miss), collapse = "\n")
    })

    # Expose results so the Report module can reuse them.
    results
  })
}
