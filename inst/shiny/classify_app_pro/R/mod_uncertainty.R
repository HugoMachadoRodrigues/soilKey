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
      width = 300,
      shiny::h5(i18n("uncert.analysis_title")),
      shiny::selectInput(ns("system"), i18n("uncert.system"),
                         choices = c("WRB 2022" = "wrb2022",
                                     "SiBCS 5" = "sibcs",
                                     "USDA ST 13" = "usda"),
                         selected = "wrb2022"),
      shiny::radioButtons(ns("level"), i18n("uncert.compare_at"),
                          choices = stats::setNames(
                            c("rsg", "name"),
                            c(i18n("uncert.level_rsg_order"),
                              i18n("uncert.level_full_name"))),
                          selected = "rsg"),
      shiny::sliderInput(ns("n"), i18n("uncert.mc_runs"), min = 25, max = 500,
                         value = 50, step = 25),
      shiny::checkboxInput(ns("sensitivity"),
                           i18n("uncert.compute_sensitivity"), value = TRUE),
      shiny::actionButton(ns("run"), i18n("uncert.run_analysis"),
                          icon = shiny::icon("dice"),
                          class = "btn-primary w-100"),
      shiny::helpText(
        i18n("uncert.perturb_help")
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

uncertainty_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    unc <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      shiny::withProgress(message = i18n("uncert.running_mc"),
                          value = 0.4, {
        tryCatch(
          soilKey::classify_with_uncertainty(
            rv$pedon,
            n           = input$n,
            system      = input$system,
            level       = input$level,
            sensitivity = isTRUE(input$sensitivity)
          ),
          error = function(e) e)
      })
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
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
            value = sprintf("%.0f%%", 100 * p_top),
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
        return(DT::datatable(
          stats::setNames(data.frame(i18n("uncert.sensitivity_not_computed")),
                          i18n("uncert.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      }
      df <- as.data.frame(s)
      df$importance <- round(df$importance, 3)
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8))
    })
  })
}
