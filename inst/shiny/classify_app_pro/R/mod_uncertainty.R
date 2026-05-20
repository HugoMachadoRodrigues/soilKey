# =============================================================================
# soilKey Pro -- Uncertainty module (v0.9.97).
#
# Monte-Carlo robustness of the classification: perturb the horizon attributes
# within plausible analytical error and measure how often the class holds.
# v0.9.97 wires the existing classification_robustness(); v0.9.100 upgrades
# this tab to the provenance-weighted posterior classify_with_uncertainty().
# =============================================================================

uncertainty_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h5("Robustness analysis"),
      shiny::selectInput(ns("system"), "System",
                         choices = c("WRB 2022" = "wrb2022",
                                     "SiBCS 5" = "sibcs",
                                     "USDA ST 13" = "usda"),
                         selected = "wrb2022"),
      shiny::radioButtons(ns("level"), "Compare at",
                          choices = c("RSG / Order" = "order",
                                      "Full name" = "name"),
                          selected = "order"),
      shiny::sliderInput(ns("n"), "Monte-Carlo runs", min = 25, max = 500,
                         value = 50, step = 25),
      shiny::actionButton(ns("run"), "Run robustness analysis",
                          icon = shiny::icon("dice"),
                          class = "btn-primary w-100"),
      shiny::helpText(
        "Each run perturbs texture, pH and organic carbon within typical ",
        "analytical error, then re-classifies."
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

uncertainty_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    rob <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      shiny::withProgress(message = "Running Monte-Carlo perturbations...",
                          value = 0.5, {
        tryCatch(
          soilKey::classification_robustness(
            rv$pedon,
            system = input$system,
            level  = input$level,
            n      = input$n
          ),
          error = function(e) e)
      })
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      r <- rob()
      if (is.null(r)) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("dice"),
                          " Press 'Run robustness analysis' to start."))
      }
      if (inherits(r, "error")) {
        return(bslib::card(
          bslib::card_header("Analysis failed"),
          bslib::card_body(shiny::tags$p(class = "text-danger",
                                         conditionMessage(r)))
        ))
      }
      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box(
            title = "Baseline class",
            value = r$baseline %||% "n/a",
            showcase = shiny::icon("flag"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Robustness",
            value = sprintf("%.0f%%", 100 * (r$robustness %||% 0)),
            showcase = shiny::icon("shield-halved"),
            theme = if ((r$robustness %||% 0) >= 0.8) "success"
                    else if ((r$robustness %||% 0) >= 0.5) "warning"
                    else "danger"
          ),
          bslib::value_box(
            title = "Monte-Carlo runs",
            value = r$n %||% 0L,
            showcase = shiny::icon("repeat"),
            theme = "secondary"
          )
        ),
        bslib::card(
          bslib::card_header("Alternative classes when the result flipped"),
          bslib::card_body(plotly::plotlyOutput(ns("flip_plot"),
                                                height = "300px"))
        )
      )
    })

    output$flip_plot <- plotly::renderPlotly({
      r <- rob()
      shiny::req(r)
      shiny::validate(shiny::need(!inherits(r, "error"), "n/a"))
      ft <- r$flipped_to
      if (is.null(ft) || length(ft) == 0L) {
        return(plotly::plotly_empty(type = "scatter", mode = "markers") |>
                 plotly::layout(title = list(
                   text = "No flips -- the classification held in every run",
                   font = list(size = 13))))
      }
      df <- as.data.frame(ft, stringsAsFactors = FALSE)
      names(df) <- c("class", "count")
      df$count <- as.numeric(df$count)
      plotly::plot_ly(
        df, x = ~count, y = ~stats::reorder(class, count),
        type = "bar", orientation = "h",
        marker = list(color = "#fd8d3c")
      ) |>
        plotly::layout(
          xaxis = list(title = "Times reached"),
          yaxis = list(title = ""),
          margin = list(l = 120, t = 20, b = 40)
        )
    })
  })
}
