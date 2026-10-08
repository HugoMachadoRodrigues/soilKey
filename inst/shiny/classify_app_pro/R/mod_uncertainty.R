# =============================================================================
# soilKey Pro -- Uncertainty module (v0.9.100).
#
# Provenance-weighted Monte-Carlo uncertainty: classify_with_uncertainty()
# perturbs each horizon cell by an amount scaled to its evidence grade, then
# reports the posterior distribution over classes, the Shannon entropy, and a
# leave-one-attribute-out sensitivity ranking.
# =============================================================================

uncertainty_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,

      # Analyse the single active profile, or the whole group of points entered
      # on the Map tab (Batch mode). The group option lights up only when a
      # batch has been loaded; a note reports how many points are available.
      sk_section(
        i18n("uncert.source_title"), icon = "layer-group",
        desc = i18n("uncert.desc_source"),
        shinyWidgets::radioGroupButtons(
          ns("source"),
          choices = stats::setNames(c("active", "group"),
                                    c(i18n("uncert.source_active"),
                                      i18n("uncert.source_group"))),
          selected = "active", justified = TRUE, size = "sm"),
        shiny::uiOutput(ns("group_note"))
      ),

      sk_section(
        i18n("uncert.analysis_title"),
        desc = i18n("uncert.desc_analysis"),
        icon = "sliders",
        shiny::selectInput(
          ns("system"),
          sk_label(i18n("uncert.system"),
                   i18n("uncert.help_system")),
          choices = c("WRB 2022" = "wrb2022",
                      "SiBCS 5" = "sibcs",
                      "USDA ST 13" = "usda"),
          selected = "wrb2022"),
        shiny::radioButtons(
          ns("level"),
          sk_label(i18n("uncert.compare_at"),
                   i18n("uncert.help_level")),
          choices = stats::setNames(
            c("rsg", "name"),
            c(i18n("uncert.level_rsg_order"),
              i18n("uncert.level_full_name"))),
          selected = "rsg")
      ),

      sk_section(
        i18n("uncert.mc_runs"),
        desc = i18n("uncert.desc_mc"),
        icon = "dice",
        shiny::sliderInput(
          ns("n"),
          sk_label(i18n("uncert.mc_runs"),
                   i18n("uncert.help_n")),
          min = 25, max = 500, value = 50, step = 25),
        shiny::checkboxInput(
          ns("sensitivity"),
          sk_label(i18n("uncert.compute_sensitivity"),
                   i18n("uncert.help_sensitivity")),
          value = TRUE)
      ),

      sk_section(
        i18n("uncert.run_analysis"),
        desc = i18n("uncert.desc_run"),
        icon = "play",
        bslib::tooltip(
          bslib::input_task_button(ns("run"), i18n("uncert.run_analysis"),
                                   icon = shiny::icon("dice"),
                                   label_busy = i18n("uncert.running_mc"),
                                   type = "primary", class = "w-100"),
          i18n("uncert.tip_run")),
        shiny::helpText(
          i18n("uncert.perturb_help")
        )
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

# The Monte-Carlo jobs, run in a background worker (.sk_async()): they may use
# only their arguments and package functions.
.uncert_job <- function(pedon, n, system, level, sensitivity) {
  soilKey::classify_with_uncertainty(pedon, n = n, system = system,
                                     level = level, sensitivity = sensitivity)
}

# One row per point: most likely class, its probability, entropy.
.uncert_group_job <- function(pedons, n, system, level) {
  rows <- lapply(seq_along(pedons), function(i) {
    p  <- pedons[[i]]
    id <- tryCatch(p$site$id, error = function(e) NULL)
    if (is.null(id)) id <- sprintf("p%02d", i)
    u  <- tryCatch(soilKey::classify_with_uncertainty(
      p, n = n, system = system, level = level, sensitivity = FALSE),
      error = function(e) NULL)
    if (is.null(u) || (length(u$posterior) == 1L && is.na(u$posterior[[1L]])))
      data.frame(id = id, top1 = NA_character_, prob = NA_real_,
                 entropy = NA_real_, stringsAsFactors = FALSE)
    else
      data.frame(id = id, top1 = if (is.null(u$top1)) NA_character_ else u$top1,
                 prob = as.numeric(u$posterior[1L]), entropy = u$entropy,
                 stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

uncertainty_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- group of points (from the Map Batch tab, shared via rv) ----------
    group_pedons <- shiny::reactive(tryCatch(rv$batch_pedons, error = function(e) NULL))
    n_group <- shiny::reactive({
      bp <- group_pedons(); if (is.null(bp)) 0L else length(bp)
    })

    output$group_note <- shiny::renderUI({
      n <- n_group()
      if (n == 0L)
        shiny::helpText(shiny::icon("circle-info"), " ", i18n("uncert.group_none"))
      else
        shiny::helpText(shiny::icon("layer-group"), " ",
                        sprintf(i18n("uncert.group_available"), n))
    })

    # The Monte-Carlo runs (15-20 s for one profile, as long again over a group)
    # run in a background worker (utils_async.R), so other sessions on the
    # instance are not frozen while they do. One button, two tasks: the source
    # picked when Run is pressed decides which one runs.
    unc_task   <- shiny::ExtendedTask$new(function(args) .sk_async(.uncert_job, args))
    group_task <- shiny::ExtendedTask$new(function(args)
      .sk_async(.uncert_group_job, args))
    bslib::bind_task_button(unc_task, "run")
    bslib::bind_task_button(group_task, "run")
    ran <- shiny::reactiveValues(single = FALSE, group = FALSE)
    shiny::observeEvent(input$run, {
      if (identical(input$source, "group")) {
        bp <- group_pedons()
        if (is.null(bp) || !length(bp)) return()
        ran$group <- TRUE
        # Sensitivity is skipped (it is a per-point extra pass -> too slow x N);
        # n is capped for a responsive table.
        group_task$invoke(list(
          pedons = bp, n = min(as.integer(input$n %||% 50L), 100L),
          system = input$system, level = input$level))
      } else {
        shiny::req(rv$pedon)
        ran$single <- TRUE
        unc_task$invoke(list(
          pedon = rv$pedon, n = input$n, system = input$system,
          level = input$level, sensitivity = isTRUE(input$sensitivity)))
      }
    })
    unc <- shiny::reactive(if (isTRUE(ran$single)) .sk_task_value(unc_task))
    group_unc <- shiny::reactive({
      bp <- group_pedons()
      if (is.null(bp) || !length(bp))
        return(simpleError(i18n("uncert.group_none")))
      if (isTRUE(ran$group)) .sk_task_value(group_task)
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      # -- group mode: a per-point uncertainty table + summary ---------------
      if (identical(input$source, "group")) {
        if (n_group() == 0L)
          return(shiny::div(class = "text-muted p-4 text-center",
                            shiny::icon("layer-group"), " ",
                            i18n("uncert.group_none_body")))
        g <- group_unc()
        if (is.null(g))
          return(shiny::div(class = "text-muted p-4 text-center",
                            shiny::icon("dice"), " ", i18n("uncert.press_run")))
        if (inherits(g, "error"))
          return(bslib::card(bslib::card_header(i18n("uncert.analysis_failed")),
                             bslib::card_body(shiny::tags$p(class = "text-danger",
                                                            conditionMessage(g)))))
        ok <- g[is.finite(g$prob), , drop = FALSE]
        mean_conf <- if (nrow(ok)) mean(ok$prob) else NA_real_
        mean_ent  <- if (nrow(ok)) mean(ok$entropy) else NA_real_
        n_stable  <- sum(ok$prob >= 0.8, na.rm = TRUE)
        return(shiny::tagList(
          bslib::layout_column_wrap(
            width = 1 / 3,
            bslib::value_box(
              title = i18n("uncert.group_n_points"), value = nrow(g),
              showcase = shiny::icon("layer-group"), theme = "primary"),
            bslib::value_box(
              title = i18n("uncert.group_mean_conf"),
              value = if (is.na(mean_conf)) i18n("uncert.na") else sprintf("%.2f%%", 100 * mean_conf),
              showcase = shiny::icon("percent"),
              theme = if (isTRUE(mean_conf >= 0.8)) "success"
                      else if (isTRUE(mean_conf >= 0.5)) "warning" else "danger"),
            bslib::value_box(
              title = i18n("uncert.group_stable"),
              value = sprintf("%d / %d", n_stable, nrow(g)),
              showcase = shiny::icon("shield-halved"),
              theme = "secondary")),
          bslib::card(
            bslib::card_header(i18n("uncert.group_per_point")),
            bslib::card_body(
              shiny::helpText(shiny::icon("hand-pointer"), " ",
                              i18n("uncert.click_row_hint")),
              DT::DTOutput(ns("group_table")))),
          # per-point drill-in: full distribution + sensitivity for the row
          shiny::uiOutput(ns("drill_detail"))
        ))
      }
      # -- single active-profile mode (unchanged) ----------------------------
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      u <- unc()
      if (is.null(u)) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("dice"),
                          i18n("uncert.press_run")))
      }
      if (inherits(u, "error")) {
        return(bslib::card(
          bslib::card_header(i18n("uncert.analysis_failed")),
          bslib::card_body(shiny::tags$p(class = "text-danger",
                                         conditionMessage(u)))))
      }
      if (length(u$posterior) == 1L && is.na(u$posterior[[1L]])) {
        return(bslib::card(
          bslib::card_header(i18n("uncert.not_enough_data")),
          bslib::card_body(i18n("uncert.no_perturbable"))))
      }
      p_top <- as.numeric(u$posterior[1L])
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box(
            title = i18n("uncert.most_likely_class"),
            value = u$top1 %||% i18n("uncert.na"),
            showcase = shiny::icon("flag"),
            theme = "primary"),
          bslib::value_box(
            title = i18n("uncert.posterior_probability"),
            value = sprintf("%.2f%%", 100 * p_top),
            showcase = shiny::icon("percent"),
            theme = if (p_top >= 0.8) "success"
                    else if (p_top >= 0.5) "warning" else "danger"),
          bslib::value_box(
            title = i18n("uncert.entropy"),
            value = sprintf("%.2f", u$entropy),
            showcase = shiny::icon("wave-square"),
            theme = if (u$entropy < 0.5) "success"
                    else if (u$entropy < 1) "warning" else "danger")
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(
            bslib::card_header(i18n("uncert.posterior_distribution")),
            bslib::card_body(plotly::plotlyOutput(ns("posterior"),
                                                  height = "320px"))),
          bslib::card(
            bslib::card_header(i18n("uncert.attribute_sensitivity")),
            bslib::card_body(DT::DTOutput(ns("sensitivity"))))
        )
      )
    })

    # per-point uncertainty table (group mode)
    output$group_table <- DT::renderDT({
      g <- group_unc(); shiny::req(g)
      shiny::validate(shiny::need(!inherits(g, "error"),
        if (inherits(g, "error")) conditionMessage(g) else i18n("uncert.na")))
      show <- data.frame(
        id = g$id, top1 = g$top1 %||% NA_character_,
        prob = g$prob, entropy = round(g$entropy, 2),
        stringsAsFactors = FALSE)
      show <- show[order(-show$prob), , drop = FALSE]
      names(show) <- c(i18n("uncert.col_point"), i18n("uncert.most_likely_class"),
                       i18n("uncert.posterior_probability"), i18n("uncert.entropy"))
      sk_datatable(show, rownames = FALSE, selection = "single",
                    options = list(dom = "tp", pageLength = 12, scrollX = TRUE)) |>
        DT::formatPercentage(i18n("uncert.posterior_probability"), 2) |>
        DT::formatRound(i18n("uncert.entropy"), 2) |>
        DT::formatStyle(
          i18n("uncert.posterior_probability"),
          color = DT::styleInterval(c(0.5, 0.8),
                                    c("var(--sk-prob-low)", "var(--sk-prob-mid)",
                                      "var(--sk-prob-high)")),
          fontWeight = "bold")
    })

    # ---- per-point drill-in: click a row -> full analysis for that point ----
    drill_task <- shiny::ExtendedTask$new(function(args) .sk_async(.uncert_job, args))
    drill_id   <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$group_table_rows_selected, {
      sel <- input$group_table_rows_selected
      g   <- group_unc()
      if (is.null(sel) || is.null(g) || inherits(g, "error")) return(drill_id(NULL))
      gs  <- g[order(-g$prob), , drop = FALSE]        # same order the table shows
      if (sel > nrow(gs)) return(drill_id(NULL))
      id  <- gs$id[sel]
      bp  <- group_pedons()
      ped <- NULL
      for (p in bp) {
        pid <- tryCatch(p$site$id, error = function(e) NULL) %||% ""
        if (identical(as.character(pid), as.character(id))) { ped <- p; break }
      }
      if (is.null(ped) && sel <= length(bp)) ped <- bp[[sel]]   # index fallback
      if (is.null(ped)) return(drill_id(NULL))
      drill_id(id)
      drill_task$invoke(list(
        pedon = ped, n = input$n, system = input$system, level = input$level,
        sensitivity = isTRUE(input$sensitivity)))
    }, ignoreNULL = FALSE)
    drill <- shiny::reactive({
      id <- drill_id()
      if (is.null(id)) return(NULL)
      list(id = id, u = .sk_task_value(drill_task))
    })

    output$drill_detail <- shiny::renderUI({
      ns <- session$ns
      d <- drill()
      if (is.null(d))
        return(shiny::div(class = "text-muted small p-2",
                          shiny::icon("hand-pointer"), " ",
                          i18n("uncert.click_row_hint")))
      u <- d$u
      if (inherits(u, "error"))
        return(bslib::card(class = "mt-2",
          bslib::card_header(i18n("uncert.analysis_failed")),
          bslib::card_body(shiny::tags$p(class = "text-danger", conditionMessage(u)))))
      bslib::card(
        class = "mt-2",
        bslib::card_header(shiny::icon("magnifying-glass"), " ",
                           sprintf(i18n("uncert.drill_title"), d$id,
                                   u$top1 %||% i18n("uncert.na"))),
        bslib::card_body(bslib::layout_column_wrap(
          width = 1 / 2,
          plotly::plotlyOutput(ns("drill_posterior"), height = "300px"),
          DT::DTOutput(ns("drill_sensitivity")))))
    })

    output$drill_posterior <- plotly::renderPlotly({
      d <- drill(); shiny::req(d, !inherits(d$u, "error"))
      post <- d$u$posterior
      shiny::validate(shiny::need(
        !(length(post) == 1L && is.na(post[[1L]])), i18n("uncert.no_posterior")))
      df <- data.frame(class = names(post), prob = as.numeric(post),
                       stringsAsFactors = FALSE)
      df <- utils::head(df[order(-df$prob), ], 8L)
      plotly::plot_ly(df, x = ~prob, y = ~stats::reorder(class, prob),
                      type = "bar", orientation = "h",
                      marker = list(color = "#6a51a3")) |>
        plotly::layout(
          xaxis = list(title = i18n("uncert.p_class"), range = c(0, 1),
                       tickformat = ".0%"),
          yaxis = list(title = ""), margin = list(l = 140, t = 20, b = 40))
    })

    output$drill_sensitivity <- DT::renderDT({
      d <- drill(); shiny::req(d, !inherits(d$u, "error"))
      s <- d$u$sensitivity
      if (is.null(s) || nrow(s) == 0L)
        return(sk_datatable(
          stats::setNames(data.frame(i18n("uncert.sensitivity_not_computed")),
                          i18n("uncert.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      df <- as.data.frame(s); df$importance <- round(df$importance, 2)
      sk_datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8))
    })

    output$posterior <- plotly::renderPlotly({
      u <- unc()
      shiny::req(u, !inherits(u, "error"))
      post <- u$posterior
      shiny::validate(shiny::need(
        !(length(post) == 1L && is.na(post[[1L]])), i18n("uncert.no_posterior")))
      df <- data.frame(class = names(post), prob = as.numeric(post),
                       stringsAsFactors = FALSE)
      df <- utils::head(df[order(-df$prob), ], 8L)
      plotly::plot_ly(
        df, x = ~prob, y = ~stats::reorder(class, prob),
        type = "bar", orientation = "h",
        marker = list(color = "#6a51a3")) |>
        plotly::layout(
          xaxis = list(title = i18n("uncert.p_class"), range = c(0, 1),
                       tickformat = ".0%"),
          yaxis = list(title = ""),
          margin = list(l = 140, t = 20, b = 40))
    })

    output$sensitivity <- DT::renderDT({
      u <- unc()
      shiny::req(u, !inherits(u, "error"))
      s <- u$sensitivity
      if (is.null(s) || nrow(s) == 0L) {
        return(sk_datatable(
          stats::setNames(data.frame(i18n("uncert.sensitivity_not_computed")),
                          i18n("uncert.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      }
      df <- as.data.frame(s)
      df$importance <- round(df$importance, 2)
      sk_datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8))
    })
  })
}
