# =============================================================================
# soilKey Pro -- professional multi-tab Shiny app (v0.9.97).
#
# A complete graphical front-end to the soilKey pipeline:
#   * Pedon    -- build a profile from a canonical fixture, a CSV upload, or
#                 from scratch, with an interactive horizon editor.
#   * Classify -- run WRB 2022 / SiBCS 5 / USDA ST 13 side-by-side, with the
#                 full deterministic key trace and missing-data hints.
#   * Photo    -- VLM extraction of Munsell colour and site metadata from
#                 field photographs (mock provider by default; no data leaves
#                 the machine unless a live provider is configured).
#   * Spectra  -- gap-fill horizon attributes from a Vis-NIR spectrum (OSSL).
#   * Spatial  -- SoilGrids spatial prior at the profile coordinates.
#   * Map      -- interactive leaflet maps. "Point prior": click to place a
#                 point and query the SoilGrids class prior there. "Batch
#                 classify": classify many profiles at once and map them by
#                 class (with GeoPackage export).
#   * Uncertainty -- Monte-Carlo robustness of the classification.
#   * Report   -- download a self-contained HTML or PDF cross-system report.
#   * Settings -- diagnostic engine, Tier-3 strict mode, missing-data policy.
#
# Helper modules live in the R/ sub-directory and are auto-sourced by Shiny.
#
# Launch with:
#   soilKey::run_classify_app(ui = "pro")
# or directly:
#   shiny::runApp(system.file("shiny", "classify_app_pro", package = "soilKey"))
# =============================================================================

# ---- dependency soft-fail ---------------------------------------------------
.pro_require <- function(pkgs) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(miss)) {
    stop("soilKey Pro needs these packages: ", paste(miss, collapse = ", "),
         ".\n  Install with: install.packages(c(",
         paste0('"', miss, '"', collapse = ", "), "))",
         call. = FALSE)
  }
  invisible(TRUE)
}
.pro_require(c("shiny", "bslib", "DT", "plotly", "shinyWidgets", "leaflet"))

library(shiny)
library(soilKey)

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------

# A soil-science palette layered on flatly (see www/soilkey.css for the rest).
sk_theme <- bslib::bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#6B4423", secondary = "#A0522D", success = "#4F772D",
  "navbar-bg" = "#6B4423"
)

ui <- bslib::page_navbar(
  title  = tags$span(class = "navbar-brand-inner",
                     "soil", tags$span(class = "sk-mark", "Key"), " Pro"),
  id     = "main_nav",
  theme  = sk_theme,
  fillable = TRUE,
  # Stylesheet + the global pedon ribbon render above the tab content.
  header = tagList(
    tags$head(tags$link(rel = "stylesheet", type = "text/css",
                        href = "soilkey.css")),
    uiOutput("pedon_ribbon")
  ),
  bslib::nav_panel("Pedon",       icon = icon("layer-group"),  pedon_ui("pedon")),
  bslib::nav_panel("Classify",    icon = icon("sitemap"),      classify_ui("classify")),
  bslib::nav_panel("Photo",       icon = icon("camera"),       photo_ui("photo")),
  bslib::nav_panel("Spectra",     icon = icon("wave-square"),  spectra_ui("spectra")),
  bslib::nav_panel("Spatial",     icon = icon("location-dot"), spatial_ui("spatial")),
  bslib::nav_panel(
    "Map", icon = icon("map-location-dot"),
    bslib::navset_card_tab(
      bslib::nav_panel("Point prior",     map_ui("map")),
      bslib::nav_panel("Batch classify",  map_batch_ui("map_batch")),
      bslib::nav_panel("Grid prediction", map_grid_ui("map_grid"))
    )
  ),
  bslib::nav_panel("Uncertainty", icon = icon("dice"),         uncertainty_ui("uncertainty")),
  bslib::nav_panel("Report",      icon = icon("file-arrow-down"), report_ui("report")),
  bslib::nav_spacer(),
  bslib::nav_panel("Settings",    icon = icon("gear"),         settings_ui("settings")),
  bslib::nav_item(
    actionLink("about", label = tagList(icon("circle-question"), "Help"),
               class = "nav-link")
  ),
  bslib::nav_item(
    tags$a(icon("book"), "Docs",
           href   = "https://hugomachadorodrigues.github.io/soilKey/",
           target = "_blank")
  ),
  footer = tags$div(
    class = "text-muted small px-3 py-2",
    sprintf("soilKey %s -- deterministic keys; the taxonomic key is never ",
            as.character(utils::packageVersion("soilKey"))),
    "delegated to a language model."
  )
)

# ----------------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------------

server <- function(input, output, session) {

  # Shared, mutable application state. `pedon` is a PedonRecord (R6, reference
  # semantics) -- modules that enrich it MUST reassign rv$pedon afterwards so
  # downstream reactives invalidate. `example_request` is a counter the Help
  # modal / ribbon bump to ask the Pedon tab to load the demo profile.
  # `include_family` / `specifiers` are the two depth-level options: rv is their
  # single source of truth, so the Settings switch and the Classify-sidebar
  # switch stay in lock-step (each module two-way-syncs its widget to rv).
  rv <- reactiveValues(pedon = NULL, example_request = 0L,
                       include_family = FALSE, specifiers = FALSE)

  settings <- settings_server("settings", rv)

  pedon_server("pedon",            rv)
  classify_server("classify",      rv, settings)
  photo_server("photo",            rv)
  spectra_server("spectra",        rv)
  spatial_server("spatial",        rv, settings)
  map_server("map",                rv, settings)
  map_batch_server("map_batch",    rv, settings)
  map_grid_server("map_grid",      rv, settings)
  uncertainty_server("uncertainty", rv, settings)
  report_server("report",          rv, settings)

  # ---- global pedon ribbon (persistent context across every tab) ----------
  output$pedon_ribbon <- renderUI({
    p <- rv$pedon
    if (is.null(p)) {
      return(tags$div(
        class = "sk-ribbon",
        tags$span(class = "sk-empty",
                  icon("circle-info"), " No pedon yet."),
        actionButton("ribbon_example", "Load example",
                     icon = icon("flask"),
                     class = "btn-sm btn-primary")))
    }
    lat <- p$site$lat %||% NA; lon <- p$site$lon %||% NA
    coord <- if (!is.na(lat) && !is.na(lon))
      sprintf("%.3f, %.3f", lat, lon) else "no coords"
    tags$div(
      class = "sk-ribbon",
      tags$span(class = "sk-built", icon("circle-check"), " Pedon built"),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", "ID"), p$site$id %||% "(unnamed)"),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", "Horizons"), nrow(p$horizons)),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", "Site"), coord))
  })

  # ---- one-click example: ask the Pedon tab to load the demo profile ------
  load_example <- function() {
    rv$example_request <- rv$example_request + 1L
    bslib::nav_select("main_nav", "Pedon")
  }
  observeEvent(input$ribbon_example, load_example())

  # ---- "Help / Getting started" modal -------------------------------------
  observeEvent(input$about, {
    showModal(modalDialog(
      title = tagList(icon("seedling"), " Welcome to soilKey Pro"),
      easyClose = TRUE, size = "l",
      tags$p("Classify a soil profile under WRB 2022, SiBCS 5 and USDA Soil ",
             "Taxonomy 13 -- the deterministic key is never delegated to a ",
             "language model."),
      tags$p(tags$strong("Workflow:")),
      tags$ol(
        class = "sk-steps",
        tags$li(tags$strong("Pedon"), " -- build a profile from a fixture, a ",
                "CSV, or by hand."),
        tags$li(tags$strong("Classify"), " -- run all three systems with the ",
                "full key trace."),
        tags$li(tags$strong("Photo / Spectra / Spatial"), " -- enrich the ",
                "pedon from images, Vis-NIR, or SoilGrids."),
        tags$li(tags$strong("Map"), " -- point prior, batch soil map, or a ",
                "gridded prediction."),
        tags$li(tags$strong("Uncertainty / Report"), " -- robustness and a ",
                "downloadable cross-system report.")
      ),
      tags$p(class = "text-muted small",
             sprintf("soilKey %s",
                     as.character(utils::packageVersion("soilKey")))),
      footer = tagList(
        modalButton("Close"),
        actionButton("about_example", "Load example & classify",
                     icon = icon("flask"), class = "btn-primary"))
    ))
  })
  observeEvent(input$about_example, {
    removeModal()
    rv$example_request <- rv$example_request + 1L
    bslib::nav_select("main_nav", "Classify")
  })
}

shinyApp(ui = ui, server = server)
