# =============================================================================
# soilKey Shiny demo -- one-screen, no-code soil-classification GUI
#
# Drag-and-drop a small horizons table OR pick one of the 31 canonical
# fixtures, click Classify, and see the deterministic key trace + the
# WRB / SiBCS / USDA names + the evidence grade. Designed for
# pedologists who want to validate the package on a profile they
# already know without writing R code.
#
# Launched via:   soilKey::run_demo()
# =============================================================================

library(shiny)
library(soilKey)

CANONICAL_FIXTURES <- c(
  "Ferralsol (Latossolo Vermelho)" = "make_ferralsol_canonical",
  "Acrisol"                        = "make_acrisol_canonical",
  "Alisol"                         = "make_alisol_canonical",
  "Andosol (volcanic ash)"         = "make_andosol_canonical",
  "Anthrosol"                      = "make_anthrosol_canonical",
  "Arenosol"                       = "make_arenosol_canonical",
  "Calcisol"                       = "make_calcisol_canonical",
  "Cambisol"                       = "make_cambisol_canonical",
  "Chernozem"                      = "make_chernozem_canonical",
  "Cryosol (permafrost)"           = "make_cryosol_canonical",
  "Durisol"                        = "make_durisol_canonical",
  "Fluvisol"                       = "make_fluvisol_canonical",
  "Gleysol"                        = "make_gleysol_canonical",
  "Gypsisol"                       = "make_gypsisol_canonical",
  "Histosol"                       = "make_histosol_canonical",
  "Kastanozem"                     = "make_kastanozem_canonical",
  "Leptosol"                       = "make_leptosol_canonical",
  "Lixisol"                        = "make_lixisol_canonical",
  "Luvisol"                        = "make_luvisol_canonical",
  "Nitisol"                        = "make_nitisol_canonical",
  "Phaeozem"                       = "make_phaeozem_canonical",
  "Planosol"                       = "make_planosol_canonical",
  "Plinthosol"                     = "make_plinthosol_canonical",
  "Podzol"                         = "make_podzol_canonical",
  "Retisol"                        = "make_retisol_canonical",
  "Solonchak"                      = "make_solonchak_canonical",
  "Solonetz"                       = "make_solonetz_canonical",
  "Stagnosol"                      = "make_stagnosol_canonical",
  "Technosol"                      = "make_technosol_canonical",
  "Umbrisol"                       = "make_umbrisol_canonical",
  "Vertisol"                       = "make_vertisol_canonical"
)

ui <- fluidPage(
  titlePanel("soilKey - automated soil profile classification"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Choose input"),
      radioButtons("source", NULL,
                     choices = c("Canonical fixture" = "fixture",
                                 "Upload CSV"        = "upload"),
                     selected = "fixture"),
      conditionalPanel(
        "input.source == 'fixture'",
        selectInput("fixture", "Pick a canonical profile:",
                      choices  = CANONICAL_FIXTURES,
                      selected = "make_ferralsol_canonical")
      ),
      conditionalPanel(
        "input.source == 'upload'",
        fileInput("csv", "CSV with horizons (top_cm, bottom_cm, designation, clay_pct, silt_pct, sand_pct, ph_h2o, oc_pct, cec_cmol, bs_pct):",
                    accept = ".csv"),
        helpText("Optional columns are accepted. See horizon_column_spec() for the full canonical schema.")
      ),
      hr(),
      h4("2. Classify"),
      checkboxGroupInput("systems", "Classification systems:",
                            choices  = c("WRB 2022" = "wrb",
                                         "SiBCS 5"  = "sibcs",
                                         "USDA 13ed" = "usda"),
                            selected = c("wrb", "sibcs", "usda")),
      actionButton("go", "Classify",
                     class = "btn-primary", width = "100%"),
      hr(),
      tags$small(
        "Local & deterministic. The taxonomic key is not delegated to a model -- ",
        "VLM extraction fills the PedonRecord; ",
        "the key itself is canonical YAML + R."
      )
    ),
    mainPanel(
      width = 8,
      h3(textOutput("title")),
      tableOutput("hzn"),
      hr(),
      h4("Classification results"),
      uiOutput("results"),
      hr(),
      h4("Key trace (WRB)"),
      verbatimTextOutput("trace_wrb"),
      h4("Missing data (across all systems)"),
      verbatimTextOutput("missing")
    )
  )
)


server <- function(input, output, session) {
  pedon_rv <- reactive({
    if (input$source == "fixture") {
      fn <- get(input$fixture, envir = asNamespace("soilKey"))
      fn()
    } else {
      req(input$csv)
      raw <- read.csv(input$csv$datapath)
      PedonRecord$new(
        site     = list(id = sub("\\.csv$", "", input$csv$name)),
        horizons = ensure_horizon_schema(raw)
      )
    }
  })

  output$title <- renderText({
    p <- pedon_rv()
    sprintf("Profile: %s", p$site$id %||% "(unnamed)")
  })

  output$hzn <- renderTable({
    p <- pedon_rv()
    h <- as.data.frame(p$horizons)
    cols <- intersect(c("top_cm","bottom_cm","designation","clay_pct",
                          "silt_pct","sand_pct","ph_h2o","oc_pct",
                          "cec_cmol","bs_pct"), names(h))
    h[, cols, drop = FALSE]
  }, striped = TRUE, hover = TRUE)

  results_rv <- eventReactive(input$go, {
    p <- pedon_rv()
    out <- list()
    if ("wrb"   %in% input$systems)
      out$wrb   <- tryCatch(classify_wrb2022(p, on_missing = "silent"),
                              error = function(e) e)
    if ("sibcs" %in% input$systems)
      out$sibcs <- tryCatch(classify_sibcs(p),
                              error = function(e) e)
    if ("usda"  %in% input$systems)
      out$usda  <- tryCatch(classify_usda(p, on_missing = "silent"),
                              error = function(e) e)
    out
  })

  output$results <- renderUI({
    res <- results_rv()
    if (length(res) == 0L) return(p(em("Click Classify to see results.")))
    tagList(lapply(names(res), function(nm) {
      r <- res[[nm]]
      if (inherits(r, "error"))
        return(div(strong(toupper(nm)), ": ",
                    span(class = "text-danger",
                          paste("error -", conditionMessage(r)))))
      div(
        strong(toupper(nm)), ": ",
        span(r$name %||% "(no name)"),
        br(),
        tags$small("evidence grade: ", r$evidence_grade %||% "(none)")
      )
    }))
  })

  output$trace_wrb <- renderPrint({
    res <- results_rv()
    if (is.null(res$wrb)) return(invisible())
    if (inherits(res$wrb, "error")) return(cat("(error)"))
    if (is.null(res$wrb$trace)) return(cat("(no trace)"))
    cat(sprintf("(%d RSGs tested before assignment)\n", length(res$wrb$trace)))
    for (i in seq_along(res$wrb$trace)) {
      t <- res$wrb$trace[[i]]
      cat(sprintf("%2d. %-3s %-15s -- %s\n",
                    i, t$code %||% "?", t$name %||% "?",
                    t$status %||% "?"))
    }
  })

  output$missing <- renderPrint({
    res <- results_rv()
    if (length(res) == 0L) return(cat("(none yet)"))
    miss <- character(0)
    for (r in res) {
      if (inherits(r, "error")) next
      m <- r$missing_data %||% character(0)
      if (length(m)) miss <- unique(c(miss, m))
    }
    if (length(miss) == 0L) cat("(none)")
    else cat(paste(sort(miss), collapse = "\n"))
  })
}


shinyApp(ui, server)
