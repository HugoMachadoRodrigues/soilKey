# =============================================================================
# soilKey Pro -- Classify module (v0.9.97).
#
# Runs WRB 2022 / SiBCS 5 / USDA ST 13 on the shared pedon and shows the three
# results side-by-side, the deterministic key trace per system, the close-call
# ambiguities, and the measurements that would refine the result.
# =============================================================================

# Turn a raw horizon/site attribute name into a readable label with its unit,
# for the "Missing data" list (e.g. "clay_pct" -> "Clay (%)").
.classify_pretty_attr <- function(x) {
  y <- x
  y <- gsub("_pct$",     " (%)",           y)
  y <- gsub("_cmol$",    " (cmol_c/kg)",   y)
  y <- gsub("_cmol_kg$", " (cmol_c/kg)",   y)
  y <- gsub("_mg_kg$",   " (mg/kg)",       y)
  y <- gsub("_g_cm3$",   " (g/cm3)",       y)
  y <- gsub("_temp_C$",  " temperature (C)", y)
  y <- gsub("_",         " ",              y)
  substr(y, 1, 1) <- toupper(substr(y, 1, 1))
  y
}

classify_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      # ---- Which systems to run, and the run trigger --------------------
      sk_section(
        i18n("classify.run_classification"),
        icon = "play",
        desc = "Choose which classification systems to run on the current pedon, then start the key.",
        shiny::checkboxGroupInput(
          ns("systems"),
          sk_label(
            i18n("classify.systems"),
            "Tick every system you want a name for; each is scored independently from the same pedon."
          ),
          choices  = c("WRB 2022" = "wrb2022", "SiBCS 5" = "sibcs",
                       "USDA ST 13" = "usda"),
          selected = c("wrb2022", "sibcs", "usda")
        ),
        bslib::tooltip(
          shiny::actionButton(ns("run"), i18n("classify.run"),
                              icon = shiny::icon("play"),
                              class = "btn-primary w-100"),
          "Run the deterministic keys and show the WRB, SiBCS and USDA names with their decision traces."
        ),
        # Tells the user whether the shown results reflect the current settings,
        # or whether an input changed and they must press Classify again.
        shiny::uiOutput(ns("run_status"))
      ),
      shiny::tags$hr(),
      # ---- Complete a partial profile before classifying ------------------
      sk_section(
        "Complete missing data",
        icon = "wand-magic-sparkles",
        desc = paste("Optional. Fill blank attributes so an incomplete profile",
                     "can still be classified. Filled values are flagged as",
                     "predicted, so the evidence grade drops honestly (A to B/C)."),
        shiny::checkboxGroupInput(
          ns("gapfill_methods"),
          sk_label(
            "Fill missing attributes from",
            paste("Applied in order, to blank cells only, on a copy of the",
                  "pedon -- your entered values are never overwritten.")),
          choices = c(
            "Interpolation within the profile"      = "interp",
            "SoilGrids at the coordinates (online)" = "soilgrids",
            "Attached Vis-NIR spectra"              = "spectra"),
          selected = character(0)),
        shiny::helpText(shiny::icon("arrow-up"), " ",
                        i18n("classify.applies_on_run"))
      ),
      shiny::tags$hr(),
      # The two deepest-level options live on the Settings tab, but they are
      # surfaced here too so the user can discover and flip them without
      # leaving Classify. Both switches two-way-sync with the shared rv, so
      # they stay identical to the Settings tab's controls.
      sk_section(
        i18n("classify.deepest_level"),
        icon = "sliders",
        desc = "Optional finer levels. These stay in sync with the same switches on the Settings tab.",
        shinyWidgets::materialSwitch(
          ns("include_family"),
          sk_label(
            i18n("classify.usda_family"),
            "Add the USDA family level (texture, mineralogy, temperature) below the subgroup when data allow."
          ),
          value = FALSE, status = "primary"),
        shinyWidgets::materialSwitch(
          ns("specifiers"),
          sk_label(
            i18n("classify.wrb_depth_specifiers"),
            "Append WRB depth specifiers (e.g. Epi-, Endo-) that record where a qualifier occurs in the profile."
          ),
          value = FALSE, status = "primary"),
        shiny::helpText(shiny::icon("arrow-up"), " ",
                        i18n("classify.applies_on_run"))
      ),
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

    # ---- "results current vs out of date" tracking --------------------------
    # Changing the systems, gap-fill options, depth switches or the pedon after
    # a run means the shown results no longer reflect the settings -> prompt the
    # user to press Classify again. Reset to fresh on every run.
    # has_run gates every read of results(): an eventReactive is in a "pending"
    # state before its first event, and reading it there suspends the output
    # (endless spinner). has_run only becomes TRUE once Classify is pressed.
    has_run <- shiny::reactiveVal(FALSE)
    stale   <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$run, { has_run(TRUE); stale(FALSE) })
    shiny::observeEvent(
      list(input$systems, input$gapfill_methods, input$include_family,
           input$specifiers, rv$pedon),
      { if (has_run()) stale(TRUE) }, ignoreInit = TRUE)

    results <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      cfg <- settings()
      sys <- input$systems
      if (length(sys) == 0L) {
        shiny::showNotification(i18n("classify.pick_one_system"), type = "warning")
        return(NULL)
      }
      gf <- input$gapfill_methods
      run_all <- function(gapfill_arg) soilKey::classify_all(
        rv$pedon,
        systems         = sys,
        on_missing      = cfg$on_missing,
        include_familia = cfg$include_familia,
        include_family  = isTRUE(cfg$include_family),
        specifiers      = isTRUE(cfg$specifiers),
        gapfill         = gapfill_arg)
      shiny::withProgress(message = i18n("classify.classifying"), value = 0.5, {
        if (length(gf) == 0L) return(run_all(FALSE))
        # Gap-fill can fail (no internet for SoilGrids, no attached spectra):
        # fall back to classifying as-is rather than erroring the whole tab.
        tryCatch(
          run_all(list(method = gf)),
          error = function(e) {
            shiny::showNotification(
              sprintf("Gap-fill could not run (%s) - classified without it.",
                      conditionMessage(e)),
              type = "warning", duration = 8)
            run_all(FALSE)
          })
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

    # ---- run-state hint under the Classify button ---------------------------
    output$run_status <- shiny::renderUI({
      if (is.null(rv$pedon))
        return(shiny::div(class = "small text-muted mt-2",
                          shiny::icon("circle-info"), " ",
                          i18n("classify.hint_need_pedon")))
      if (!has_run())
        return(shiny::div(class = "small text-muted mt-2",
                          shiny::icon("hand-pointer"), " ",
                          i18n("classify.hint_press")))
      if (isTRUE(stale()))
        return(shiny::div(class = "small mt-2",
                          style = "color:#8a5a00;font-weight:600;",
                          shiny::icon("triangle-exclamation"), " ",
                          i18n("classify.hint_stale")))
      shiny::div(class = "small mt-2", style = "color:#3f6024;",
                 shiny::icon("circle-check"), " ", i18n("classify.hint_current"))
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      if (!has_run() || is.null(results())) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("play"),
                          i18n("classify.press_classify")))
      }
      shiny::tagList(
        if (isTRUE(stale())) shiny::div(
          class = "alert alert-warning py-2 px-3 small mb-2 d-flex align-items-center gap-2",
          shiny::icon("triangle-exclamation"),
          shiny::span(i18n("classify.stale_banner"))),
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
            shiny::helpText(i18n("classify.trace_intro")),
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
            shiny::uiOutput(ns("missing"))
          )
        )
      )
    })

    output$trace_table <- DT::renderDT({
      res <- results()
      shiny::req(res)
      r <- res[[input$trace_sys %||% "wrb"]]
      # v0.9.165: the trace shape differs by system (flat for WRB, nested phases
      # for SiBCS/USDA). key_trace_table() normalises every shape to one ordered
      # data frame, so this renderer no longer crashes on the SiBCS/USDA trace
      # ("$ operator is invalid for atomic vectors").
      tr <- if (is.null(r)) NULL
            else tryCatch(soilKey::key_trace_table(r), error = function(e) NULL)
      if (is.null(tr) || nrow(tr) == 0L) {
        return(DT::datatable(
          stats::setNames(data.frame(i18n("classify.no_trace_available")),
                          i18n("classify.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      }
      pass_lbl <- i18n("classify.status_pass")
      fail_lbl <- i18n("classify.status_fail")
      lbl <- c(passed        = pass_lbl,
               failed        = fail_lbl,
               indeterminate = i18n("classify.status_indeterminate"),
               selected      = i18n("classify.status_selected"),
               info          = i18n("classify.status_info"))
      disp <- data.frame(
        code    = tr$code,
        name    = tr$name,
        status  = unname(lbl[tr$status]),
        missing = tr$missing,
        stringsAsFactors = FALSE)
      # Show the phase / level column only when it carries information: the
      # hierarchical SiBCS / USDA keys fill it; the flat WRB trace leaves it
      # blank, so WRB keeps the original four-column table.
      has_phase <- any(nzchar(tr$phase))
      if (has_phase)
        disp <- cbind(phase = tr$phase, disp, stringsAsFactors = FALSE)
      colnames_loc <- c(if (has_phase) i18n("classify.col_phase"),
                        i18n("classify.col_code"), i18n("classify.col_name"),
                        i18n("classify.col_status"), i18n("classify.col_missing"))
      DT::datatable(disp, rownames = FALSE, colnames = colnames_loc,
                    options = list(pageLength = 15, dom = "tip")) |>
        # "not met" is the NORMAL case (most candidate classes don't apply), so
        # colour it neutral grey -- not alarming red. Only the assigned class and
        # a met criterion are highlighted; "needs data" is a soft amber.
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            c(pass_lbl, fail_lbl, lbl[["selected"]], lbl[["indeterminate"]]),
            c("#d1e7dd", "#eef1f3", "#cfe2ff", "#fff3cd")))
    })

    output$ambiguities <- shiny::renderUI({
      res <- results()
      shiny::req(res)
      amb <- res$wrb$ambiguities %||% list()
      if (length(amb) == 0L) {
        return(shiny::div(class = "text-muted p-2",
                          shiny::icon("circle-check"), " ",
                          i18n("classify.no_close_calls")))
      }
      shiny::tagList(
        shiny::helpText(i18n("classify.amb_intro")),
        shiny::tags$ul(class = "sk-amb-list", lapply(amb, function(a) {
          shiny::tags$li(
            shiny::strong(a$name %||% a$code %||% "?"),
            i18n("classify.amb_sep"),
            a$reason %||% a$note %||% i18n("classify.near_miss"))
        }))
      )
    })

    output$missing <- shiny::renderUI({
      res <- results()
      shiny::req(res)
      # Per-system so the user sees which measurement each key still wants.
      blocks <- list()
      for (nm in c("wrb", "sibcs", "usda")) {
        r <- res[[nm]]
        if (is.null(r) || inherits(r, "error")) next
        m <- sort(unique(r$missing_data %||% character(0)))
        if (!length(m)) next
        blocks[[length(blocks) + 1L]] <- shiny::div(
          class = "mb-3",
          shiny::tags$strong(c(wrb = "WRB 2022", sibcs = "SiBCS 5",
                               usda = "USDA ST 13")[[nm]]),
          shiny::tags$ul(class = "sk-missing-list", lapply(m, function(a)
            shiny::tags$li(
              .classify_pretty_attr(a),
              shiny::tags$code(class = "ms-2", a)))))
      }
      if (length(blocks) == 0L)
        return(shiny::div(class = "text-muted p-2",
                          shiny::icon("circle-check"), " ",
                          i18n("classify.no_missing_complete")))
      shiny::tagList(shiny::helpText(i18n("classify.measuring_refine")), blocks)
    })

    # Expose results so the Report module can reuse them.
    results
  })
}
