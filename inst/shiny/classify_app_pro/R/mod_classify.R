# =============================================================================
# soilKey Pro -- Classify module (v0.9.97).
#
# Runs WRB 2022 / SiBCS 5 / USDA ST 13 on the shared pedon and shows the three
# results side-by-side, the deterministic key trace per system, the close-call
# ambiguities, and the measurements that would refine the result.
# =============================================================================

# How much of a horizon attribute the pedon actually carries, as "5 / 7".
#
# The "Missing data" list names an attribute as soon as ONE predicate could not
# read it in ONE horizon. A user who filled the attribute in every horizon but
# the bottom two then sees its name listed and reasonably concludes the upload
# was rejected -- reported from ISRIC on a profile whose lowest two horizons had
# no laboratory data. Returns NULL when the attribute is not a horizon column
# (site fields, composite hints such as "al_sat_pct (or ca+mg+...)") or when the
# pedon carries none of it, in which case "missing" is the whole story.
.classify_attr_coverage <- function(attr, pedon) {
  if (is.null(pedon) || is.null(pedon$horizons)) return(NULL)
  h <- pedon$horizons
  if (!attr %in% names(h)) return(NULL)
  n <- nrow(h)
  if (!n) return(NULL)
  v <- h[[attr]]
  filled <- sum(!is.na(v) & !(is.character(v) & !nzchar(as.character(v))))
  if (filled == 0L) return(NULL)
  list(filled = filled, n = n)
}

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
        desc = i18n("classify.desc_run"),
        shiny::checkboxGroupInput(
          ns("systems"),
          sk_label(
            i18n("classify.systems"),
            i18n("classify.help_systems")
          ),
          choices  = c("WRB 2022" = "wrb2022", "SiBCS 5" = "sibcs",
                       "USDA ST 13" = "usda"),
          selected = c("wrb2022", "sibcs", "usda")
        ),
        bslib::tooltip(
          bslib::input_task_button(ns("run"), i18n("classify.run"),
                                   icon = shiny::icon("play"),
                                   label_busy = i18n("classify.classifying"),
                                   type = "primary", class = "w-100"),
          i18n("classify.tip_run")
        ),
        # Tells the user whether the shown results reflect the current settings,
        # or whether an input changed and they must press Classify again.
        shiny::uiOutput(ns("run_status"))
      ),
      shiny::tags$hr(),
      # ---- Complete a partial profile before classifying ------------------
      sk_section(
        i18n("classify.complete_missing"),
        icon = "wand-magic-sparkles",
        desc = i18n("classify.desc_complete"),
        shiny::checkboxGroupInput(
          ns("gapfill_methods"),
          sk_label(
            i18n("classify.fill_from"),
            i18n("classify.help_fill_from")),
          choices = .classify_gapfill_choices(),
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
        desc = i18n("classify.desc_deepest"),
        shinyWidgets::materialSwitch(
          ns("include_family"),
          sk_label(
            i18n("classify.usda_family"),
            i18n("classify.help_usda_family")
          ),
          value = FALSE, status = "primary"),
        shinyWidgets::materialSwitch(
          ns("specifiers"),
          sk_label(
            i18n("classify.wrb_depth_specifiers"),
            i18n("classify.help_specifiers")
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

# Where the Classify tab may fill missing attributes from. Spectra only when a
# real OSSL reference library is configured (.spectra_gapfill_available()):
# without one, fill_from_spectra() writes placeholder values, and a profile
# classified on them would carry an invented class.
.classify_gapfill_choices <- function() {
  ch <- c(interp = i18n("classify.gapfill_interp"),
          soilgrids = i18n("classify.gapfill_soilgrids"),
          spectra = i18n("classify.gapfill_spectra"))
  if (!.spectra_gapfill_available()) ch <- ch[names(ch) != "spectra"]
  stats::setNames(names(ch), unname(ch))
}

# The classification run, done in a background worker (.sk_async()): it may use
# only its arguments and package functions. Gap-fill can fail (no internet for
# SoilGrids, no attached spectra). classify_all() turns that into a warning and
# a NULL result for the system, so the error never reached the tryCatch that
# was meant to catch it, and the cards came back empty. The failures are now
# read from those warnings: a system lost to gap-fill is classified again
# without it, and the reason rides along as attr(, "gapfill_error").
.classify_job <- function(pedon, systems, on_missing, include_familia,
                          include_family, specifiers, gapfill_methods) {
  run_all <- function(gapfill_arg) soilKey::classify_all(
    pedon,
    systems         = systems,
    on_missing      = on_missing,
    include_familia = include_familia,
    include_family  = include_family,
    specifiers      = specifiers,
    gapfill         = gapfill_arg)
  if (length(gapfill_methods) == 0L) return(run_all(FALSE))
  why <- character(0)
  res <- tryCatch(
    withCallingHandlers(
      run_all(list(method = gapfill_methods)),
      warning = function(w) {
        m <- conditionMessage(w)
        if (grepl("^classify_[a-z0-9]+ failed: ", m)) {
          why <<- c(why, sub("^classify_[a-z0-9]+ failed: ", "", m))
          invokeRestart("muffleWarning")
        }
      }),
    error = function(e) { why <<- c(why, conditionMessage(e)); NULL })
  keys <- c(wrb2022 = "wrb", sibcs = "sibcs", usda = "usda")[systems]
  lost <- if (is.null(res)) keys else keys[vapply(keys, function(k) is.null(res[[k]]),
                                                    logical(1))]
  if (!length(lost)) return(res)
  plain <- run_all(FALSE)
  if (is.null(res)) res <- plain
  for (k in lost) res[k] <- list(plain[[k]])
  res$summary <- plain$summary
  attr(res, "gapfill_error") <- why[1] %||% "gap-fill failed"
  res
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
    shiny::observeEvent(
      list(input$systems, input$gapfill_methods, input$include_family,
           input$specifiers, rv$pedon),
      { if (has_run()) stale(TRUE) }, ignoreInit = TRUE)

    # The keys run in a background worker (utils_async.R): with SoilGrids
    # gap-fill a run reads the network for over a minute, and in this process it
    # froze every other session on the instance. The button stays busy while it
    # runs; results() waits for it.
    classify_task <- shiny::ExtendedTask$new(function(args)
      .sk_async(.classify_job, args))
    bslib::bind_task_button(classify_task, "run")
    shiny::observeEvent(input$run, {
      shiny::req(rv$pedon)
      cfg <- settings()
      if (length(input$systems) == 0L) {
        shiny::showNotification(i18n("classify.pick_one_system"), type = "warning")
        return()
      }
      has_run(TRUE); stale(FALSE)
      classify_task$invoke(list(
        pedon           = rv$pedon,
        systems         = input$systems,
        on_missing      = cfg$on_missing,
        include_familia = cfg$include_familia,
        include_family  = isTRUE(cfg$include_family),
        specifiers      = isTRUE(cfg$specifiers),
        gapfill_methods = input$gapfill_methods))
    })
    results <- shiny::reactive(.sk_task_value(classify_task))
    # Gap-fill can fail (no internet for SoilGrids, no attached spectra); the
    # job then classifies as-is and says why.
    shiny::observe({
      msg <- attr(tryCatch(classify_task$result(), error = function(e) NULL),
                  "gapfill_error")
      if (!is.null(msg))
        shiny::showNotification(
          i18n("classify.gapfill_fallback", msg),
          type = "warning", duration = 8)
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
      shiny::div(class = "small mt-2 sk-ok",
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
      if (inherits(results(), "error"))
        return(shiny::div(class = "alert alert-danger m-3",
                          shiny::icon("triangle-exclamation"), " ",
                          conditionMessage(results())))
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
      shiny::req(res, !inherits(res, "error"))
      r <- res[[input$trace_sys %||% "wrb"]]
      # v0.9.165: the trace shape differs by system (flat for WRB, nested phases
      # for SiBCS/USDA). key_trace_table() normalises every shape to one ordered
      # data frame, so this renderer no longer crashes on the SiBCS/USDA trace
      # ("$ operator is invalid for atomic vectors").
      tr <- if (is.null(r)) NULL
            else tryCatch(soilKey::key_trace_table(r), error = function(e) NULL)
      if (is.null(tr) || nrow(tr) == 0L) {
        return(sk_datatable(
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
      sk_datatable(disp, rownames = FALSE, colnames = colnames_loc,
                    options = list(pageLength = 15, dom = "tip")) |>
        # "not met" is the NORMAL case (most candidate classes don't apply), so
        # colour it neutral grey -- not alarming red. Only the assigned class and
        # a met criterion are highlighted; "needs data" is a soft amber.
        DT::formatStyle(
          "status",
          backgroundColor = DT::styleEqual(
            c(pass_lbl, fail_lbl, lbl[["selected"]], lbl[["indeterminate"]]),
            # palette tokens (soilkey.css), so dark mode has its own shades
            c("var(--sk-st-met)", "var(--sk-st-fail)", "var(--sk-st-sel)",
              "var(--sk-st-na)")))
    })

    output$ambiguities <- shiny::renderUI({
      res <- results()
      shiny::req(res, !inherits(res, "error"))
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
      shiny::req(res, !inherits(res, "error"))
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
          shiny::tags$ul(class = "sk-missing-list", lapply(m, function(a) {
            cov <- .classify_attr_coverage(a, rv$pedon)
            shiny::tags$li(
              .classify_pretty_attr(a),
              shiny::tags$code(class = "ms-2", a),
              # Say WHERE it is missing when the pedon has it somewhere, so a
              # partially-filled column never reads as a rejected upload.
              if (!is.null(cov)) shiny::tags$span(
                class = "text-muted small ms-2",
                sprintf(i18n("classify.attr_coverage"), cov$filled, cov$n)))
          })))
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
