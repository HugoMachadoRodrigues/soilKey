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
      shiny::h5("Uncertainty analysis"),
      shiny::selectInput(ns("system"), "System",
                         choices = c("WRB 2022" = "wrb2022",
                                     "SiBCS 5" = "sibcs",
                                     "USDA ST 13" = "usda"),
                         selected = "wrb2022"),
      shiny::radioButtons(ns("level"), "Compare at",
                          choices = c("RSG / Order" = "rsg",
                                      "Full name" = "name"),
                          selected = "rsg"),
      shiny::sliderInput(ns("n"), "Monte-Carlo runs", min = 25, max = 500,
                         value = 50, step = 25),
      shiny::checkboxInput(ns("sensitivity"),
                           "Compute attribute sensitivity", value = TRUE),
      shiny::actionButton(ns("run"), "Run uncertainty analysis",
                          icon = shiny::icon("dice"),
                          class = "btn-primary w-100"),
      shiny::helpText(
        "Each cell is perturbed by an amount scaled to its provenance ",
        "evidence grade: measured values barely move, assumed values move ",
        "a lot. The posterior reflects how trustworthy the inputs are."
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

uncertainty_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    unc <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      shiny::withProgress(message = "Running provenance-weighted Monte-Carlo...",
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
                          " Press 'Run uncertainty analysis' to start."))
      }
      if (inherits(u, "error")) {
        return(bslib::card(
          bslib::card_header("Analysis failed"),
          bslib::card_body(shiny::tags$p(class = "text-danger",
                                         conditionMessage(u)))))
      }
      if (length(u$posterior) == 1L && is.na(u$posterior[[1L]])) {
        return(bslib::card(
          bslib::card_header("Not enough data"),
          bslib::card_body("This pedon has no perturbable attributes.")))
      }
      p_top <- as.numeric(u$posterior[1L])
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box(
            title = "Most likely class",
            value = u$top1 %||% "n/a",
            showcase = shiny::icon("flag"),
            theme = "primary"),
          bslib::value_box(
            title = "Posterior probability",
            value = sprintf("%.0f%%", 100 * p_top),
            showcase = shiny::icon("percent"),
            theme = if (p_top >= 0.8) "success"
                    else if (p_top >= 0.5) "warning" else "danger"),
          bslib::value_box(
            title = "Entropy",
            value = sprintf("%.2f", u$entropy),
            showcase = shiny::icon("wave-square"),
            theme = if (u$entropy < 0.5) "success"
                    else if (u$entropy < 1) "warning" else "danger")
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(
            bslib::card_header("Posterior distribution"),
            bslib::card_body(plotly::plotlyOutput(ns("posterior"),
                                                  height = "320px"))),
          bslib::card(
            bslib::card_header("Attribute sensitivity"),
            bslib::card_body(DT::DTOutput(ns("sensitivity"))))
        )
      )
    })

    output$posterior <- plotly::renderPlotly({
      u <- unc()
      shiny::req(u, !inherits(u, "error"))
      post <- u$posterior
      shiny::validate(shiny::need(
        !(length(post) == 1L && is.na(post[[1L]])), "no posterior"))
      df <- data.frame(class = names(post), prob = as.numeric(post),
                       stringsAsFactors = FALSE)
      df <- utils::head(df[order(-df$prob), ], 8L)
      plotly::plot_ly(
        df, x = ~prob, y = ~stats::reorder(class, prob),
        type = "bar", orientation = "h",
        marker = list(color = "#6a51a3")) |>
        plotly::layout(
          xaxis = list(title = "P(class)", range = c(0, 1),
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
          data.frame(Note = "Sensitivity not computed"),
          rownames = FALSE, options = list(dom = "t")))
      }
      df <- as.data.frame(s)
      df$importance <- round(df$importance, 3)
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8))
    })
  })
}
