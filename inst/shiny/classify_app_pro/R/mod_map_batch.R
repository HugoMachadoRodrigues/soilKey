# =============================================================================
# soilKey Pro -- Batch classification map module (v0.9.102).
#
# Phase 2 of the mapping roadmap. Ingests MANY profiles (a point set), builds
# one PedonRecord per point, classifies each under all three systems with
# classify_all(), and plots them on a leaflet map coloured by class. This is
# the genuine *pedon-scale soil map*: every mapped class is backed by a
# described, classified profile -- not a pixel covariate model.
#
# Two input sources:
#   * "Demo (fixtures)" -- N canonical fixtures spread across Brazil, so the
#     tab is demonstrable with no data (mirrors the Phase-1 test raster).
#   * "Upload CSV" -- a long-format table (one row per horizon) with an id
#     column, lat/lon, and horizon attributes. Grouped by id; PedonRecord$new()
#     normalises each horizon table via ensure_horizon_schema().
#
# Export: the classified point set is written to a GeoPackage with sf, using
# the same st_as_sf()/st_write() idiom as report_to_qgis().
# =============================================================================

# A varied set of canonical fixtures so demo points get distinct colours.
.batch_demo_fixtures <- function() {
  c("make_ferralsol_canonical", "make_cambisol_canonical",
    "make_gleysol_canonical",   "make_histosol_canonical",
    "make_vertisol_canonical",  "make_arenosol_canonical",
    "make_acrisol_canonical",   "make_luvisol_canonical",
    "make_podzol_canonical",    "make_nitisol_canonical",
    "make_planosol_canonical",  "make_leptosol_canonical")
}

# Build N demo PedonRecords from fixtures, spread deterministically across
# Brazil via the R2 low-discrepancy sequence (no RNG -> reproducible). The two
# multipliers are 1/g and 1/g^2 for the plastic number g = 1.3247179572...,
# which give two *independent* dimensions -- unlike a golden-ratio pair
# (0.618 / 0.382 = 1 - 0.618), whose points collapse onto a single line.
.batch_demo_pedons <- function(n = 12L, loader = pro_load_fixture) {
  fx <- .batch_demo_fixtures()
  n  <- max(1L, as.integer(n))
  out <- vector("list", n)
  for (i in seq_len(n)) {
    p   <- loader(fx[[((i - 1L) %% length(fx)) + 1L]])
    # R2 sequence over a Brazil-ish land box (lat -8..-28, lon -42..-58).
    lat <- -8  - 20 * ((i * 0.7548776662) %% 1)   # dim 1 (1/g)
    lon <- -42 - 16 * ((i * 0.5698402910) %% 1)   # dim 2 (1/g^2)
    p$site$id  <- sprintf("demo-%02d", i)
    p$site$lat <- round(lat, 4)
    p$site$lon <- round(lon, 4)
    out[[i]] <- p
  }
  out
}

# Long-format starter table (two profiles, a few horizons each) so the upload
# path is self-documenting: id + lat/lon repeated per horizon, then depths and
# the canonical horizon attributes.
.batch_starter_csv <- paste(
  "profile_id,lat,lon,top_cm,bottom_cm,designation,clay_pct,silt_pct,sand_pct,ph_h2o,oc_pct,cec_cmol,bs_pct",
  "P01,-22.75,-43.68,0,15,A,50,15,35,4.8,2.0,8.0,24",
  "P01,-22.75,-43.68,15,45,Bw,58,12,30,4.7,0.7,5.5,15",
  "P01,-22.75,-43.68,45,120,Bo,62,10,28,4.9,0.3,4.8,12",
  "P02,-12.40,-45.10,0,20,A,18,10,72,5.6,1.1,4.0,40",
  "P02,-12.40,-45.10,20,80,Bt,32,12,56,5.4,0.4,6.0,55",
  sep = "\n"
)

# Find the first present name (case-insensitive) from a set of candidates.
.batch_pick_col <- function(nms, candidates) {
  hit <- which(tolower(nms) %in% tolower(candidates))
  if (length(hit)) nms[hit[1L]] else NA_character_
}

# Parse a long-format data.frame (one row per horizon) into PedonRecords.
.batch_parse_csv <- function(df) {
  nms     <- names(df)
  id_col  <- .batch_pick_col(nms, c("profile_id", "pedon_id", "id", "perfil",
                                    "site_id"))
  lat_col <- .batch_pick_col(nms, c("lat", "latitude", "lat_decimal", "y"))
  lon_col <- .batch_pick_col(nms, c("lon", "lng", "longitude", "lon_decimal",
                                    "x"))
  if (is.na(id_col))
    stop(i18n("mbatch.err_no_id"))
  if (is.na(lat_col) || is.na(lon_col))
    stop(i18n("mbatch.err_no_latlon"))

  # Horizon attribute columns = everything that is part of the canonical spec.
  spec_cols <- names(soilKey::horizon_column_spec())
  hz_cols   <- intersect(nms, spec_cols)
  if (!all(c("top_cm", "bottom_cm") %in% hz_cols))
    stop(i18n("mbatch.err_no_depth"))

  ids  <- as.character(df[[id_col]])
  uids <- unique(ids[!is.na(ids) & nzchar(ids)])
  out  <- vector("list", length(uids))
  for (k in seq_along(uids)) {
    rows <- df[ids %in% uids[k], , drop = FALSE]
    site <- list(
      id  = uids[k],
      lat = suppressWarnings(as.numeric(rows[[lat_col]][1L])),
      lon = suppressWarnings(as.numeric(rows[[lon_col]][1L])),
      crs = 4326
    )
    out[[k]] <- soilKey::PedonRecord$new(
      site = site, horizons = rows[, hz_cols, drop = FALSE])
  }
  out
}

# Classify a list of pedons; return one row per pedon with the three class
# names, their RSG/order (for colouring) and evidence grades.
.batch_classify <- function(pedons, on_missing = "silent", bump = NULL) {
  rows <- vector("list", length(pedons))
  for (i in seq_along(pedons)) {
    p <- pedons[[i]]
    r <- tryCatch(
      soilKey::classify_all(p, on_missing = on_missing),
      error = function(e) NULL)
    grab <- function(res, field) if (is.null(res)) NA_character_
                                 else as.character(res[[field]] %||% NA)
    rows[[i]] <- data.frame(
      id          = as.character(p$site$id %||% sprintf("p%02d", i)),
      lat         = suppressWarnings(as.numeric(p$site$lat %||% NA)),
      lon         = suppressWarnings(as.numeric(p$site$lon %||% NA)),
      wrb_name    = grab(r$wrb, "name"),
      wrb_class   = grab(r$wrb, "rsg_or_order"),
      wrb_grade   = grab(r$wrb, "evidence_grade"),
      sibcs_name  = grab(r$sibcs, "name"),
      sibcs_class = grab(r$sibcs, "rsg_or_order"),
      sibcs_grade = grab(r$sibcs, "evidence_grade"),
      usda_name   = grab(r$usda, "name"),
      usda_class  = grab(r$usda, "rsg_or_order"),
      usda_grade  = grab(r$usda, "evidence_grade"),
      stringsAsFactors = FALSE
    )
    if (is.function(bump)) bump(i, length(pedons))
  }
  df <- do.call(rbind, rows)
  df[!is.na(df$lat) & !is.na(df$lon), , drop = FALSE]
}


map_batch_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,

      # ---- 1. Where the profiles come from ----------------------------------
      sk_section(
        i18n("mbatch.point_source"),
        icon = "map-location-dot",
        desc = "Choose the set of described profiles to classify and map.",
        shinyWidgets::radioGroupButtons(
          ns("source"),
          sk_label(
            i18n("mbatch.point_source"),
            paste("Demo spreads canonical fixture profiles across Brazil;",
                  "Upload reads your own long-format CSV of horizons.")),
          choices = stats::setNames(
            c("demo", "upload"),
            c(i18n("mbatch.source_demo"), i18n("mbatch.source_upload"))),
          selected = "demo", justified = TRUE, size = "sm"),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'demo'", ns("source")),
          shiny::numericInput(
            ns("n_demo"),
            sk_label(
              i18n("mbatch.n_demo"),
              paste("How many demo profiles to place. They are spread",
                    "deterministically over Brazil, so the map is reproducible.")),
            12, min = 1, max = 60, step = 1)),
        shiny::conditionalPanel(
          sprintf("input['%s'] == 'upload'", ns("source")),
          shiny::fileInput(
            ns("csv"),
            sk_label(
              i18n("mbatch.long_csv"),
              paste("Long-format CSV: one row per horizon, with an id column,",
                    "lat/lon, and top_cm/bottom_cm plus horizon attributes.")),
            accept = ".csv"),
          shiny::helpText(i18n("mbatch.csv_help")),
          shiny::downloadLink(ns("template"), i18n("mbatch.download_template")))
      ),

      # ---- 2. How to colour the map -----------------------------------------
      sk_section(
        i18n("mbatch.colour_by_system"),
        icon = "sliders",
        desc = "Pick which classification system drives the map colours and legend.",
        shiny::selectInput(
          ns("system"),
          sk_label(
            i18n("mbatch.colour_by_system"),
            paste("Every point is classified under all three systems;",
                  "this only chooses which one colours the markers.")),
          choices = c("WRB 2022"  = "wrb",
                      "SiBCS 5"    = "sibcs",
                      "USDA ST 13" = "usda"),
          selected = "wrb")
      ),

      # ---- 3. Run + export ---------------------------------------------------
      sk_section(
        i18n("mbatch.run"),
        icon = "play",
        desc = "Classify the point set, then export the result for GIS.",
        bslib::tooltip(
          shiny::actionButton(
            ns("run"), i18n("mbatch.run"),
            icon = shiny::icon("layer-group"),
            class = "btn-primary w-100"),
          "Build one profile per point, classify each under all three systems, and plot them on the map."),
        bslib::tooltip(
          shiny::downloadButton(
            ns("export"), i18n("mbatch.export"),
            class = "btn-outline-secondary w-100 mt-2"),
          "Save the classified points as a GeoPackage (.gpkg) you can open in QGIS or ArcGIS."),
        bslib::tooltip(
          shiny::downloadButton(
            ns("report"), i18n("mbatch.report"),
            icon = shiny::icon("file-lines"),
            class = "btn-outline-secondary w-100 mt-2"),
          "Download a multi-profile HTML report: a map of all profiles on the first page, then one page per profile."),
        shiny::helpText(i18n("mbatch.deterministic_help"))
      )
    ),
    bslib::layout_column_wrap(
      width = 1, heights_equal = "row",
      bslib::card(
        bslib::card_header(i18n("mbatch.soil_map")),
        bslib::card_body(
          padding = 0,
          shiny::helpText(
            class = "px-3 pt-2 mb-1",
            paste("Each marker is a classified profile, coloured by the chosen",
                  "system. Click a point to see its WRB, SiBCS and USDA names.")),
          leaflet::leafletOutput(ns("map"), height = "440px"))
      ),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
                     shiny::strong(i18n("mbatch.classified_points")),
                     shiny::uiOutput(ns("count"), inline = TRUE))),
        bslib::card_body(
          shiny::helpText(
            paste("One row per mapped profile, with the class name it received",
                  "in each of the three systems.")),
          DT::DTOutput(ns("table")))
      )
    )
  )
}

map_batch_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    # Class column for the chosen system (wrb_class / sibcs_class / usda_class).
    class_col <- shiny::reactive(paste0(input$system %||% "wrb", "_class"))
    name_col  <- shiny::reactive(paste0(input$system %||% "wrb", "_name"))

    # The built PedonRecords behind the current point set, kept so the
    # multi-profile report can be generated from them.
    batch_pedons <- shiny::reactiveVal(NULL)

    # ---- build + classify the point set on demand ---------------------------
    results <- shiny::eventReactive(input$run, {
      on_missing <- tryCatch(settings()$on_missing, error = function(e) NULL)
      on_missing <- on_missing %||% "silent"
      shiny::withProgress(message = i18n("mbatch.classifying"), value = 0, {
        pedons <- tryCatch({
          if (identical(input$source, "upload")) {
            f <- input$csv
            if (is.null(f)) return(simpleError(i18n("mbatch.upload_first")))
            raw <- tryCatch(
              utils::read.csv(f$datapath, stringsAsFactors = FALSE),
              error = function(e)
                stop(i18n("mbatch.csv_read_failed", conditionMessage(e))))
            .batch_parse_csv(raw)
          } else {
            .batch_demo_pedons(input$n_demo %||% 12L)
          }
        }, error = function(e) e)
        if (inherits(pedons, "error")) return(pedons)
        batch_pedons(pedons)          # keep for the multi-profile report
        n <- length(pedons)
        bump <- function(i, total)
          shiny::incProgress(1 / total,
                             detail = sprintf("%d / %d", i, total))
        .batch_classify(pedons, on_missing = on_missing, bump = bump)
      })
    })

    # Publish the classified points to shared state so the Grid-prediction
    # tab's "Interpolate points" method can reuse them.
    shiny::observeEvent(results(), {
      res <- results()
      if (!inherits(res, "error") && is.data.frame(res)) rv$batch_points <- res
    })

    # ---- base map -----------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = -51, lat = -14, zoom = 4)
    })

    # ---- redraw markers + legend whenever results or system change ----------
    shiny::observe({
      res    <- results()
      sysc   <- class_col()
      sysn   <- name_col()
      proxy  <- leaflet::leafletProxy("map", session) |>
        leaflet::clearMarkers() |>
        leaflet::clearControls()
      if (inherits(res, "error") || is.null(res) || nrow(res) == 0L) return()

      classes <- res[[sysc]]
      pal <- leaflet::colorFactor("Set3", domain = sort(unique(classes)),
                                  na.color = "#bdbdbd")
      popups <- sprintf(
        "<b>%s</b><br/>WRB: %s <i>(%s)</i><br/>SiBCS: %s <i>(%s)</i><br/>USDA: %s <i>(%s)</i>",
        res$id,
        res$wrb_name,   res$wrb_grade,
        res$sibcs_name, res$sibcs_grade,
        res$usda_name,  res$usda_grade)
      proxy |>
        leaflet::addCircleMarkers(
          lng = res$lon, lat = res$lat,
          radius = 7, weight = 1, color = "#333", fillOpacity = 0.85,
          fillColor = pal(classes), label = res$id, popup = popups) |>
        leaflet::addLegend(
          position = "bottomright", pal = pal, values = classes,
          title = sysn, opacity = 0.9) |>
        leaflet::fitBounds(
          lng1 = min(res$lon), lat1 = min(res$lat),
          lng2 = max(res$lon), lat2 = max(res$lat))
    })

    # ---- count badge + table ------------------------------------------------
    output$count <- shiny::renderUI({
      res <- results()
      if (inherits(res, "error") || is.null(res)) return(NULL)
      shiny::tags$span(class = "badge bg-secondary",
                       i18n("mbatch.n_points", nrow(res)))
    })

    output$table <- DT::renderDT({
      res <- results()
      shiny::req(res)
      shiny::validate(shiny::need(!inherits(res, "error"),
                                  if (inherits(res, "error"))
                                    conditionMessage(res) else "n/a"))
      shiny::validate(shiny::need(nrow(res) > 0L,
                                  i18n("mbatch.no_classifiable")))
      show <- res[, c("id", "lat", "lon", "wrb_name", "sibcs_name",
                      "usda_name"), drop = FALSE]
      names(show) <- c(i18n("mbatch.col_id"), i18n("mbatch.col_lat"),
                       i18n("mbatch.col_lon"), "WRB 2022", "SiBCS 5",
                       "USDA ST 13")
      DT::datatable(show, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })

    # ---- long-format CSV template (self-documents the upload layout) --------
    output$template <- shiny::downloadHandler(
      filename = function() "soilKey_batch_template.csv",
      content  = function(file) writeLines(.batch_starter_csv, file)
    )

    # ---- multi-profile HTML report ------------------------------------------
    output$report <- shiny::downloadHandler(
      filename = function() "soilkey_profiles_report.html",
      content = function(file) {
        peds <- batch_pedons()
        if (is.null(peds) || length(peds) == 0L)
          stop(i18n("mbatch.no_profiles_report"))
        soilKey::report_html(peds, file = file)
      }
    )

    # ---- GeoPackage export --------------------------------------------------
    output$export <- shiny::downloadHandler(
      filename = function() "soilkey_soil_map.gpkg",
      content = function(file) {
        res <- results()
        if (inherits(res, "error") || is.null(res) || nrow(res) == 0L)
          stop(i18n("mbatch.nothing_to_export"))
        if (!requireNamespace("sf", quietly = TRUE))
          stop(i18n("mbatch.sf_required"))
        pts <- sf::st_as_sf(res, coords = c("lon", "lat"),
                            crs = 4326, remove = FALSE)
        sf::st_write(pts, file, layer = "soil_points",
                     delete_dsn = TRUE, quiet = TRUE)
      }
    )
  })
}
