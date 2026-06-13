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
      shiny::h5(i18n("pedon.seed_profile")),
      shinyWidgets::radioGroupButtons(
        ns("source"), NULL,
        choices = stats::setNames(
          c("fixture", "upload", "blank"),
          c(i18n("pedon.source_fixture"), i18n("pedon.source_upload"),
            i18n("pedon.source_blank"))),
        selected = "fixture", justified = TRUE, size = "sm"
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'fixture'", ns("source")),
        shinyWidgets::pickerInput(
          ns("fixture"), i18n("pedon.canonical_profile"),
          choices  = pro_fixture_catalog(),
          selected = "make_ferralsol_canonical",
          options  = list(`live-search` = TRUE)
        )
      ),
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'upload'", ns("source")),
        shiny::fileInput(ns("csv"), i18n("pedon.horizons_csv_tsv"),
                         accept = c(".csv", ".tsv", ".txt")),
        shiny::downloadLink(ns("template"), i18n("pedon.download_starter_csv"))
      ),
      shiny::actionButton(ns("load"), i18n("pedon.load_horizons"),
                          icon = shiny::icon("upload"),
                          class = "btn-secondary w-100"),
      shiny::tags$hr(),
      shiny::h5(i18n("pedon.site_metadata")),
      shiny::textInput(ns("site_id"), i18n("pedon.profile_id"), "demo-pedon-01"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("lat"), i18n("pedon.latitude"), -22.5,
                                             step = 0.01)),
        shiny::column(6, shiny::numericInput(ns("lon"), i18n("pedon.longitude"), -43.7,
                                             step = 0.01))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(ns("country"), i18n("pedon.country_iso2"), "BR")),
        shiny::column(6, shiny::textInput(ns("pm"), i18n("pedon.parent_material"), "gneiss"))
      ),
      shiny::tags$hr(),
      shiny::actionButton(ns("build"), i18n("pedon.build_update_pedon"),
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
                     shiny::strong(i18n("pedon.horizons_click_edit")),
                     shiny::div(
                       class = "d-flex gap-2",
                       shiny::downloadButton(ns("download_hz"), i18n("pedon.csv"),
                                             icon = shiny::icon("download"),
                                             class = "btn-sm btn-outline-secondary"),
                       shiny::actionButton(ns("add_row"), i18n("pedon.add_row"),
                                           icon = shiny::icon("plus"),
                                           class = "btn-sm btn-outline-secondary")))
        ),
        bslib::card_body(
          DT::DTOutput(ns("hz_table")),
          shiny::uiOutput(ns("geom_status")))
      ),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong(i18n("pedon.depth_profile")),
                     shiny::selectInput(ns("plot_attr"), NULL,
                                        choices = pro_numeric_attrs(),
                                        selected = "clay_pct", width = "180px"))
        ),
        bslib::card_body(plotly::plotlyOutput(ns("profile"), height = "320px"))
      )
    )
  )
}

# Turn validate_horizon_geometry() details into localized lines for the Pedon
# builder (the package function returns English; the app renders pt/en).
.pedon_geom_lines <- function(geom) {
  d <- geom$details; err <- character(0); warn <- character(0)
  jn <- function(x) paste(x, collapse = ", ")
  if (!is.null(d$missing_depth))  err  <- c(err,  i18n("pedon.geom_missing_depth", jn(d$missing_depth)))
  if (!is.null(d$negative_depth)) err  <- c(err,  i18n("pedon.geom_negative",      jn(d$negative_depth)))
  if (!is.null(d$inverted))       err  <- c(err,  i18n("pedon.geom_inverted",      jn(d$inverted)))
  if (!is.null(d$overlap))        err  <- c(err,  i18n("pedon.geom_overlap",       jn(d$overlap)))
  if (!is.null(d$gap))            warn <- c(warn, i18n("pedon.geom_gap",           jn(d$gap)))
  if (!is.null(d$surface_gap))    warn <- c(warn, i18n("pedon.geom_surface_gap",   d$surface_gap))
  if (isTRUE(d$non_monotonic))    warn <- c(warn, i18n("pedon.geom_non_monotonic"))
  if (!is.null(d$duplicate_designation))
    warn <- c(warn, i18n("pedon.geom_duplicate", jn(d$duplicate_designation)))
  # structural errors (no columns / empty) carry no details -> fall back
  if (length(err) == 0L && length(geom$errors) > 0L) err <- geom$errors
  list(errors = err, warnings = warn)
}

pedon_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    hz        <- shiny::reactiveVal(NULL)  # editable horizon data.frame
    hz_reload <- shiny::reactiveVal(0L)    # bumps only on load/add -> re-render

    output$template <- shiny::downloadHandler(
      filename = function() "soilKey_horizons_template.csv",
      content  = function(file) writeLines(.pedon_starter_csv, file)
    )

    # Download the current horizon table as CSV (edit in a spreadsheet, archive,
    # or re-upload later).
    output$download_hz <- shiny::downloadHandler(
      filename = function() sprintf("soilKey_horizons_%s.csv",
                                    input$site_id %||% "pedon"),
      content  = function(file) {
        cur <- hz()
        shiny::validate(shiny::need(!is.null(cur) && nrow(cur) > 0L,
                                    i18n("pedon.no_horizons_download")))
        utils::write.csv(cur, file, row.names = FALSE)
      }
    )

    # ---- one-click example profile (bumped by the Help modal / ribbon) -----
    # The canonical Ferralsol fixture is a complete PedonRecord; loading it
    # populates the editor AND builds rv$pedon, so every tab is immediately
    # usable -- the app's on-ramp for first-time users.
    shiny::observeEvent(rv$example_request, {
      p <- tryCatch(pro_load_fixture("make_ferralsol_canonical"),
                    error = function(e) NULL)
      if (is.null(p)) return(invisible())
      hz(as.data.frame(p$horizons))
      hz_reload(hz_reload() + 1L)
      shiny::updateTextInput(session, "site_id", value = p$site$id %||% "ferralsol-demo")
      shiny::updateNumericInput(session, "lat", value = p$site$lat %||% -22.5)
      shiny::updateNumericInput(session, "lon", value = p$site$lon %||% -43.7)
      rv$pedon <- p
      shiny::showNotification(
        i18n("pedon.loaded_example_ferralsol"),
        type = "message", duration = 6)
    }, ignoreInit = TRUE)

    # ---- load horizons from the chosen source -----------------------------
    shiny::observeEvent(input$load, {
      df <- switch(
        input$source,
        fixture = {
          p <- tryCatch(pro_load_fixture(input$fixture),
                        error = function(e) NULL)
          if (is.null(p)) {
            shiny::showNotification(i18n("pedon.could_not_load_fixture"),
                                    type = "error")
            return(invisible())
          }
          as.data.frame(p$horizons)
        },
        upload = {
          f <- input$csv
          if (is.null(f)) {
            shiny::showNotification(i18n("pedon.choose_csv_first"), type = "warning")
            return(invisible())
          }
          sep <- if (grepl("\\.tsv$", f$name, ignore.case = TRUE)) "\t" else ","
          tryCatch(utils::read.csv(f$datapath, sep = sep,
                                   stringsAsFactors = FALSE),
                   error = function(e) {
                     shiny::showNotification(
                       i18n("pedon.csv_parse_failed", conditionMessage(e)),
                       type = "error")
                     NULL
                   })
        },
        blank = .pedon_blank_template()
      )
      if (is.null(df)) return(invisible())
      # Keep only columns soilKey understands, in canonical order.
      spec  <- names(soilKey::horizon_column_spec())
      keep  <- intersect(spec, names(df))
      extra <- setdiff(names(df), spec)
      df    <- df[, c(keep, extra), drop = FALSE]
      hz(df)
      hz_reload(hz_reload() + 1L)
      shiny::showNotification(
        i18n("pedon.loaded_n", nrow(df)), type = "message")
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

    # ---- live horizon-geometry feedback under the table -------------------
    # Reacts to every cell edit so problems (overlaps, gaps, inverted depths)
    # surface immediately, in the chosen language. AA-contrast colours.
    output$geom_status <- shiny::renderUI({
      df <- hz()
      if (is.null(df) || nrow(df) == 0L) return(NULL)
      lines <- .pedon_geom_lines(validate_horizon_geometry(df))
      if (length(lines$errors) == 0L && length(lines$warnings) == 0L) {
        return(shiny::div(class = "small mt-2", style = "color:#3f6024;",
                          shiny::icon("circle-check"), " ", i18n("pedon.geom_ok")))
      }
      shiny::tagList(
        lapply(lines$errors, function(m)
          shiny::div(class = "small mt-1", style = "color:#b02a37;",
                     shiny::icon("triangle-exclamation"), " ", m)),
        lapply(lines$warnings, function(m)
          shiny::div(class = "small mt-1", style = "color:#7a5b00;",
                     shiny::icon("circle-exclamation"), " ", m))
      )
    })

    # ---- build the PedonRecord -------------------------------------------
    shiny::observeEvent(input$build, {
      df <- hz()
      if (is.null(df) || nrow(df) == 0L) {
        shiny::showNotification(i18n("pedon.load_add_horizon_first"),
                                type = "warning")
        return(invisible())
      }
      # Guard the horizon geometry before it reaches the key: overlaps, inverted
      # or missing depths would build a nonsensical profile. Errors block;
      # warnings (gaps, surface offset, ...) are surfaced but allowed.
      geom <- validate_horizon_geometry(df)
      glines <- .pedon_geom_lines(geom)
      if (!geom$valid) {
        shiny::showNotification(
          i18n("pedon.geom_errors_block", paste(glines$errors, collapse = " ")),
          type = "error", duration = 8)
        return(invisible())
      }
      if (length(glines$warnings)) {
        shiny::showNotification(
          i18n("pedon.geom_warnings", paste(glines$warnings, collapse = " ")),
          type = "warning", duration = 6)
      }
      # Guard the coordinates before they reach the key: an out-of-range
      # lat/lon would silently poison the SoilGrids prior and the inferred
      # temperature regime. Blank is allowed (coords are optional).
      lat <- suppressWarnings(as.numeric(input$lat))
      lon <- suppressWarnings(as.numeric(input$lon))
      if (!is.na(lat) && (lat < -90 || lat > 90)) {
        shiny::showNotification(
          i18n("pedon.latitude_range"), type = "error")
        return(invisible())
      }
      if (!is.na(lon) && (lon < -180 || lon > 180)) {
        shiny::showNotification(
          i18n("pedon.longitude_range"), type = "error")
        return(invisible())
      }
      built <- tryCatch({
        h_dt <- soilKey::ensure_horizon_schema(data.table::as.data.table(df))
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
          i18n("pedon.could_not_build_pedon", conditionMessage(built)),
          type = "error", duration = 8)
        return(invisible())
      }
      rv$pedon <- built
      shiny::showNotification(i18n("pedon.pedon_built_ready"),
                              type = "message")
    })

    output$status <- shiny::renderUI({
      if (is.null(rv$pedon)) {
        shiny::div(class = "text-muted small mt-2",
                   shiny::icon("circle-info"), i18n("pedon.no_pedon_yet"))
      } else {
        shiny::div(class = "text-success small mt-2",
                   shiny::icon("circle-check"), " ",
                   i18n("pedon.pedon_ready",
                        rv$pedon$site$id %||% "pedon",
                        nrow(rv$pedon$horizons)))
      }
    })
  })
}
