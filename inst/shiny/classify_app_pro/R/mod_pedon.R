# =============================================================================
# soilKey Pro -- Pedon builder module (v0.9.97).
#
# Three ways to seed a profile: a canonical fixture, a CSV upload, or a blank
# template. The horizon table is editable cell-by-cell (DT). "Build pedon"
# assembles a PedonRecord from the edited table plus the site metadata and
# stores it in the shared rv$pedon.
# =============================================================================

# Blank horizon template -- one empty row in canonical column order.
.pedon_blank_template <- function() {
  data.frame(
    top_cm = 0, bottom_cm = 20, designation = "A",
    clay_pct = NA_real_, silt_pct = NA_real_, sand_pct = NA_real_,
    ph_h2o = NA_real_, oc_pct = NA_real_, cec_cmol = NA_real_,
    bs_pct = NA_real_, stringsAsFactors = FALSE
  )
}

.pedon_starter_csv <- paste(
  "top_cm,bottom_cm,designation,clay_pct,silt_pct,sand_pct,ph_h2o,oc_pct,cec_cmol,bs_pct",
  "0,15,A,50,15,35,4.8,2.0,8.0,24",
  "15,35,AB,52,14,34,4.7,1.2,6.5,17",
  "35,65,BA,55,10,35,4.7,0.6,5.5,14",
  "65,130,Bw1,60,8,32,4.8,0.3,5.0,13",
  "130,200,Bw2,60,8,32,4.9,0.2,4.8,13",
  sep = "\n"
)

pedon_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5("1. Seed the profile"),
      shinyWidgets::radioGroupButtons(
        ns("source"), NULL,
        choices = c("Fixture" = "fixture", "Upload CSV" = "upload",
                    "Blank" = "blank"),
        selected = "fixture", justified = TRUE, size = "sm"
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'fixture'", ns("source")),
        shinyWidgets::pickerInput(
          ns("fixture"), "Canonical profile",
          choices  = pro_fixture_catalog(),
          selected = "make_ferralsol_canonical",
          options  = list(`live-search` = TRUE)
        )
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'upload'", ns("source")),
        shiny::fileInput(ns("csv"), "Horizons CSV / TSV",
                         accept = c(".csv", ".tsv", ".txt")),
        shiny::downloadLink(ns("template"), "Download a starter CSV")
      ),
      shiny::actionButton(ns("load"), "Load horizons",
                          icon = shiny::icon("upload"),
                          class = "btn-secondary w-100"),
      shiny::tags$hr(),
      shiny::h5("2. Site metadata"),
      shiny::textInput(ns("site_id"), "Profile ID", "demo-pedon-01"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("lat"), "Latitude", -22.5,
                                             step = 0.01)),
        shiny::column(6, shiny::numericInput(ns("lon"), "Longitude", -43.7,
                                             step = 0.01))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("country"), "Country (ISO-2)", "BR")),
        shiny::column(6, shiny::textInput(ns("pm"), "Parent material", "gneiss"))
      ),
      shiny::tags$hr(),
      shiny::actionButton(ns("build"), "Build / update pedon",
                          icon = shiny::icon("hammer"),
                          class = "btn-primary w-100"),
      shiny::uiOutput(ns("status"))
    ),
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong("Horizons (click a cell to edit)"),
                     shiny::actionButton(ns("add_row"), "Add row",
                                         icon = shiny::icon("plus"),
                                         class = "btn-sm btn-outline-secondary"))
        ),
        bslib::card_body(DT::DTOutput(ns("hz_table")))
      ),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong("Depth profile"),
                     shiny::selectInput(ns("plot_attr"), NULL,
                                        choices = pro_numeric_attrs(),
                                        selected = "clay_pct", width = "180px"))
        ),
        bslib::card_body(plotly::plotlyOutput(ns("profile"), height = "320px"))
      )
    )
  )
}

pedon_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    hz        <- shiny::reactiveVal(NULL)  # editable horizon data.frame
    hz_reload <- shiny::reactiveVal(0L)    # bumps only on load/add -> re-render

    output$template <- shiny::downloadHandler(
      filename = function() "soilKey_horizons_template.csv",
      content  = function(file) writeLines(.pedon_starter_csv, file)
    )

    # ---- load horizons from the chosen source -----------------------------
    shiny::observeEvent(input$load, {
      df <- switch(
        input$source,
        fixture = {
          p <- tryCatch(pro_load_fixture(input$fixture),
                        error = function(e) NULL)
          if (is.null(p)) {
            shiny::showNotification("Could not load that fixture.",
                                    type = "error")
            return(invisible())
          }
          as.data.frame(p$horizons)
        },
        upload = {
          f <- input$csv
          if (is.null(f)) {
            shiny::showNotification("Choose a CSV first.", type = "warning")
            return(invisible())
          }
          sep <- if (grepl("\\.tsv$", f$name, ignore.case = TRUE)) "\t" else ","
          tryCatch(utils::read.csv(f$datapath, sep = sep,
                                   stringsAsFactors = FALSE),
                   error = function(e) {
                     shiny::showNotification(
                       paste("CSV parse failed:", conditionMessage(e)),
                       type = "error")
                     NULL
                   })
        },
        blank = .pedon_blank_template()
      )
      if (is.null(df)) return(invisible())
      # Keep only columns soilKey understands, in canonical order.
      spec  <- names(soilKey:::horizon_column_spec())
      keep  <- intersect(spec, names(df))
      extra <- setdiff(names(df), spec)
      df    <- df[, c(keep, extra), drop = FALSE]
      hz(df)
      hz_reload(hz_reload() + 1L)
      shiny::showNotification(
        sprintf("Loaded %d horizon(s).", nrow(df)), type = "message")
    })

    shiny::observeEvent(input$add_row, {
      cur <- hz()
      if (is.null(cur)) cur <- .pedon_blank_template()[0, , drop = FALSE]
      new <- .pedon_blank_template()
      # Align columns of the blank row to the current table.
      for (cn in setdiff(names(cur), names(new))) new[[cn]] <- NA
      new <- new[, names(cur), drop = FALSE]
      if (nrow(cur) > 0L) {
        new$top_cm    <- max(cur$bottom_cm, na.rm = TRUE)
        new$bottom_cm <- new$top_cm + 20
      }
      hz(rbind(cur, new))
      hz_reload(hz_reload() + 1L)
    })

    # ---- editable table ---------------------------------------------------
    output$hz_table <- DT::renderDT({
      hz_reload()                          # re-render only on load / add
      df <- shiny::isolate(hz())
      if (is.null(df)) df <- .pedon_blank_template()[0, , drop = FALSE]
      DT::datatable(
        df,
        editable  = list(target = "cell"),
        rownames  = FALSE,
        selection = "none",
        options   = list(pageLength = 12, scrollX = TRUE, dom = "tip")
      )
    })

    shiny::observeEvent(input$hz_table_cell_edit, {
      df <- hz()
      if (is.null(df)) return(invisible())
      hz(DT::editData(df, input$hz_table_cell_edit, rownames = FALSE))
    })

    # ---- depth profile ----------------------------------------------------
    output$profile <- plotly::renderPlotly({
      pro_profile_plot(hz(), input$plot_attr %||% "clay_pct")
    })

    # ---- build the PedonRecord -------------------------------------------
    shiny::observeEvent(input$build, {
      df <- hz()
      if (is.null(df) || nrow(df) == 0L) {
        shiny::showNotification("Load or add at least one horizon first.",
                                type = "warning")
        return(invisible())
      }
      built <- tryCatch({
        h_dt <- soilKey:::ensure_horizon_schema(data.table::as.data.table(df))
        soilKey::PedonRecord$new(
          site = list(
            id              = input$site_id %||% "pedon",
            lat             = input$lat,
            lon             = input$lon,
            country         = input$country,
            parent_material = input$pm
          ),
          horizons = h_dt
        )
      }, error = function(e) e)

      if (inherits(built, "error")) {
        shiny::showNotification(
          paste("Could not build pedon:", conditionMessage(built)),
          type = "error", duration = 8)
        return(invisible())
      }
      rv$pedon <- built
      shiny::showNotification("Pedon built -- ready to classify.",
                              type = "message")
    })

    output$status <- shiny::renderUI({
      if (is.null(rv$pedon)) {
        shiny::div(class = "text-muted small mt-2",
                   shiny::icon("circle-info"), " No pedon built yet.")
      } else {
        shiny::div(class = "text-success small mt-2",
                   shiny::icon("circle-check"), " ",
                   sprintf("Pedon '%s' ready (%d horizons).",
                           rv$pedon$site$id %||% "pedon",
                           nrow(rv$pedon$horizons)))
      }
    })
  })
}
