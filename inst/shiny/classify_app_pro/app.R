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

ui <- bslib::page_navbar(
  title  = "soilKey Pro",
  id     = "main_nav",
  theme  = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,
  bslib::nav_panel("Pedon",       icon = icon("layer-group"),  pedon_ui("pedon")),
  bslib::nav_panel("Classify",    icon = icon("sitemap"),      classify_ui("classify")),
  bslib::nav_panel("Photo",       icon = icon("camera"),       photo_ui("photo")),
  bslib::nav_panel("Spectra",     icon = icon("wave-square"),  spectra_ui("spectra")),
  bslib::nav_panel("Spatial",     icon = icon("location-dot"), spatial_ui("spatial")),
  bslib::nav_panel(
    "Map", icon = icon("map-location-dot"),
    bslib::navset_card_tab(
      bslib::nav_panel("Point prior",    map_ui("map")),
      bslib::nav_panel("Batch classify", map_batch_ui("map_batch"))
    )
  ),
  bslib::nav_panel("Uncertainty", icon = icon("dice"),         uncertainty_ui("uncertainty")),
  bslib::nav_panel("Report",      icon = icon("file-arrow-down"), report_ui("report")),
  bslib::nav_spacer(),
  bslib::nav_panel("Settings",    icon = icon("gear"),         settings_ui("settings")),
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
  # downstream reactives invalidate.
  rv <- reactiveValues(pedon = NULL)

  settings <- settings_server("settings")

  pedon_server("pedon",            rv)
  classify_server("classify",      rv, settings)
  photo_server("photo",            rv)
  spectra_server("spectra",        rv)
  spatial_server("spatial",        rv, settings)
  map_server("map",                rv, settings)
  map_batch_server("map_batch",    rv, settings)
  uncertainty_server("uncertainty", rv, settings)
  report_server("report",          rv, settings)
}

shinyApp(ui = ui, server = server)
