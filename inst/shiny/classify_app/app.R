# =============================================================================
# soilKey Shiny app -- interactive WRB / SiBCS / USDA classification (v0.9.39).
#
# Drag-and-drop a CSV (one row per horizon, columns matching the soilKey
# horizon schema) and get all three classifications side-by-side, with a
# downloadable HTML report.
#
# Run via:
#   shiny::runApp(system.file("shiny", "classify_app", package = "soilKey"))
# OR
#   soilKey::run_classify_app()  # convenience wrapper
# =============================================================================

# Soft-fail if shiny/DT not installed.
if (!requireNamespace("shiny", quietly = TRUE))
  stop("Package 'shiny' is required to run this app. ",
       "Install with `install.packages(\"shiny\")`.")
if (!requireNamespace("DT", quietly = TRUE))
  stop("Package 'DT' is required to run this app. ",
       "Install with `install.packages(\"DT\")`.")

library(shiny)
library(soilKey)

# Sample CSV content (downloadable as a starter template).
.SAMPLE_CSV <- paste(
  "top_cm,bottom_cm,designation,clay_pct,sand_pct,silt_pct,ph_h2o,oc_pct,bs_pct,cec_cmol",
  "0,15,A,50,35,15,4.8,2.0,24,8",
  "15,35,AB,52,34,14,4.7,1.2,17,6.5",
  "35,65,BA,55,35,10,4.7,0.6,14,5.5",
  "65,130,Bw1,60,32,8,4.8,0.3,13,5.0",
  "130,200,Bw2,60,32,8,4.9,0.2,13,4.8",
  sep = "\n"
)

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("soilKey -- Interactive soil profile classification"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Load horizons"),
      tags$p("Either upload a CSV with one row per horizon, or paste",
              "tabular data into the area below. Columns required:",
              tags$code("top_cm"), ",", tags$code("bottom_cm"), ",",
              tags$code("designation"), ", plus any soilKey horizon",
              "columns (clay_pct, sand_pct, silt_pct, ph_h2o, oc_pct,",
              "bs_pct, cec_cmol, etc.)."),

      fileInput("horizons_csv", "CSV file", accept = c(".csv", ".tsv", ".txt")),

      downloadButton("download_template", "Download starter template"),

      tags$hr(),
      h4("2. Site metadata"),
      textInput("site_id", "Profile ID", "demo-pedon-01"),
      numericInput("site_lat", "Latitude (decimal)", -22.5, step = 0.1),
      numericInput("site_lon", "Longitude (decimal)", -43.7, step = 0.1),
      textInput("site_country", "Country code (ISO-2)", "BR"),
      textInput("site_pm", "Parent material", "gneiss"),

      tags$hr(),
      h4("3. Classify"),
      actionButton("classify", "Classify across all 3 systems",
                     class = "btn-primary"),

      tags$hr(),
      h4("4. Download report"),
      tags$p("Renders all 3 results to a self-contained HTML file."),
      downloadButton("download_report", "Download HTML report",
                       icon = icon("file-download"))
    ),

    mainPanel(
      width = 8,
      tabsetPanel(
        id = "tabs",
        tabPanel("Horizons",
                   h4("Loaded horizons"),
                   DT::DTOutput("horizons_table")),
        tabPanel("Results",
                   h4("WRB 2022"),
                   verbatimTextOutput("res_wrb"),
                   h4("SiBCS 5a ed."),
                   verbatimTextOutput("res_sibcs"),
                   h4("USDA Soil Taxonomy 13ed"),
                   verbatimTextOutput("res_usda")),
        tabPanel("Trace",
                   h4("Selected key trace"),
                   selectInput("trace_system", "System",
                                 choices = c("WRB" = "wrb",
                                               "SiBCS" = "sibcs",
                                               "USDA" = "usda"),
                                 selected = "wrb"),
                   verbatimTextOutput("trace"))
      )
    )
  )
)


# ----------------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Reactive: parsed horizons data.frame
  horizons_df <- reactive({
    f <- input$horizons_csv
    if (is.null(f)) {
      # Default sample
      return(read.csv(text = .SAMPLE_CSV, stringsAsFactors = FALSE))
    }
    sep <- if (grepl("\\.tsv$", f$name)) "\t" else ","
    read.csv(f$datapath, sep = sep, stringsAsFactors = FALSE)
  })

  output$horizons_table <- DT::renderDT({
    DT::datatable(horizons_df(),
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
  })

  # Reactive: PedonRecord built from horizons + site
  pedon <- eventReactive(input$classify, {
    h <- horizons_df()
    h_dt <- soilKey:::ensure_horizon_schema(data.table::as.data.table(h))
    soilKey::PedonRecord$new(
      site = list(
        id              = input$site_id,
        lat             = input$site_lat,
        lon             = input$site_lon,
        country         = input$site_country,
        parent_material = input$site_pm
      ),
      horizons = h_dt
    )
  })

  # Reactive: classify_all output
  cls <- reactive({
    req(pedon())
    soilKey::classify_all(pedon(), on_missing = "silent")
  })

  format_result <- function(r) {
    if (is.null(r)) return("(classification failed; see Trace tab)")
    out <- character(0)
    if (!is.null(r$name))
      out <- c(out, sprintf("Name:           %s", r$name))
    if (!is.null(r$rsg_or_order))
      out <- c(out, sprintf("RSG / Order:    %s", r$rsg_or_order))
    if (!is.null(r$evidence_grade))
      out <- c(out, sprintf("Evidence grade: %s", r$evidence_grade))
    if (!is.null(r$qualifiers$principal) && length(r$qualifiers$principal) > 0L)
      out <- c(out, sprintf("Principals:     %s",
                              paste(r$qualifiers$principal, collapse = ", ")))
    if (!is.null(r$qualifiers$supplementary) &&
          length(r$qualifiers$supplementary) > 0L)
      out <- c(out, sprintf("Supplementary:  %s",
                              paste(r$qualifiers$supplementary, collapse = ", ")))
    paste(out, collapse = "\n")
  }

  output$res_wrb   <- renderText(format_result(cls()$wrb))
  output$res_sibcs <- renderText(format_result(cls()$sibcs))
  output$res_usda  <- renderText(format_result(cls()$usda))

  output$trace <- renderPrint({
    res <- cls()
    sys <- input$trace_system
    r <- res[[sys]]
    if (is.null(r)) {
      cat("(no result for", sys, ")\n")
    } else {
      cat("System:", sys, "\n\n")
      print(r)
    }
  })

  # Downloads
  output$download_template <- downloadHandler(
    filename = function() "soilKey_horizons_template.csv",
    content  = function(file) writeLines(.SAMPLE_CSV, file)
  )

  output$download_report <- downloadHandler(
    filename = function() {
      sprintf("soilKey_report_%s.html", input$site_id %||% "pedon")
    },
    content = function(file) {
      withProgress(message = "Rendering report...", value = 0.5, {
        if (!exists("report", envir = asNamespace("soilKey"))) {
          # If the package's report() is not available, write a minimal HTML.
          html <- sprintf(
            paste0("<!doctype html><html><head><meta charset='utf-8'>",
                     "<title>soilKey report -- %s</title></head>",
                     "<body><h1>soilKey classification</h1>",
                     "<p>Profile: <code>%s</code></p>",
                     "<h2>WRB 2022</h2><pre>%s</pre>",
                     "<h2>SiBCS 5a</h2><pre>%s</pre>",
                     "<h2>USDA ST 13</h2><pre>%s</pre>",
                     "</body></html>"),
            input$site_id, input$site_id,
            format_result(cls()$wrb),
            format_result(cls()$sibcs),
            format_result(cls()$usda)
          )
          writeLines(html, file)
        } else {
          soilKey::report(list(cls()$wrb, cls()$sibcs, cls()$usda),
                            file = file, pedon = pedon())
        }
      })
    }
  )
}

# ----------------------------------------------------------------------------
# Launch
# ----------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
