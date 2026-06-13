# =============================================================================
# soilKey Pro -- professional multi-tab Shiny app (v0.9.97; i18n v0.9.114).
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
# The interface is bilingual (English / Portuguese). UI strings come from the
# i18n() helper (R/i18n.R, catalogue in inst/i18n/translations.yaml); the
# navbar EN/PT selector flips the `soilKey.app_lang` option and reloads. The UI
# is a per-session function so it rebuilds in the chosen language. Helper
# modules live in the R/ sub-directory and are auto-sourced by Shiny.
#
# Launch with:
#   soilKey::run_classify_app(ui = "pro")             # English (default)
#   soilKey::run_classify_app(ui = "pro", lang = "pt") # Portuguese
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
# UI -- a per-session function so i18n() picks up the current language.
# ----------------------------------------------------------------------------

# A soil-science palette layered on flatly (see www/soilkey.css for the rest).
sk_theme <- bslib::bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#6B4423", secondary = "#A0522D", success = "#4F772D",
  "navbar-bg" = "#6B4423"
)

ui <- function(request) {
  bslib::page_navbar(
    title  = tags$span(class = "navbar-brand-inner",
                       "soil", tags$span(class = "sk-mark", "Key"),
                       i18n("app.brand_suffix")),
    id     = "main_nav",
    theme  = sk_theme,
    fillable = TRUE,
    # a11y: the document language follows the chosen interface language so
    # screen readers use the right pronunciation rules.
    lang   = .sk_app_lang(),
    # Stylesheet + the global pedon ribbon render above the tab content.
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "soilkey.css"),
        # a11y: announce transient showNotification() toasts to screen readers
        # (the panel is created on demand, so tag it once it appears).
        tags$script(htmltools::HTML(
          "document.addEventListener('DOMContentLoaded',function(){",
          "new MutationObserver(function(){",
          "var p=document.getElementById('shiny-notification-panel');",
          "if(p&&!p.getAttribute('aria-live')){p.setAttribute('aria-live','polite');p.setAttribute('role','status');}",
          "}).observe(document.body,{childList:true,subtree:true});});"))
      ),
      uiOutput("pedon_ribbon")
    ),
    bslib::nav_panel(i18n("nav.pedon"),    icon = icon("layer-group"),  pedon_ui("pedon")),
    bslib::nav_panel(i18n("nav.classify"), icon = icon("sitemap"),      classify_ui("classify")),
    bslib::nav_panel(i18n("nav.photo"),    icon = icon("camera"),       photo_ui("photo")),
    bslib::nav_panel(i18n("nav.spectra"),  icon = icon("wave-square"),  spectra_ui("spectra")),
    bslib::nav_panel(i18n("nav.spatial"),  icon = icon("location-dot"), spatial_ui("spatial")),
    bslib::nav_panel(
      i18n("nav.map"), icon = icon("map-location-dot"),
      bslib::navset_card_tab(
        bslib::nav_panel(i18n("map.tab_point"), map_ui("map")),
        bslib::nav_panel(i18n("map.tab_batch"), map_batch_ui("map_batch")),
        bslib::nav_panel(i18n("map.tab_grid"),  map_grid_ui("map_grid"))
      )
    ),
    bslib::nav_panel(i18n("nav.uncertainty"), icon = icon("dice"),         uncertainty_ui("uncertainty")),
    bslib::nav_panel(i18n("nav.report"),      icon = icon("file-arrow-down"), report_ui("report")),
    bslib::nav_spacer(),
    bslib::nav_panel(i18n("nav.settings"),    icon = icon("gear"),         settings_ui("settings")),
    bslib::nav_item(
      htmltools::tagAppendAttributes(
        shinyWidgets::radioGroupButtons(
          "app_lang_sel", label = NULL,
          choices  = c("EN" = "en", "PT" = "pt"),
          selected = .sk_app_lang(), size = "sm"),
        role = "group", "aria-label" = i18n("a11y.language"))
    ),
    bslib::nav_item(
      actionLink("about", label = tagList(icon("circle-question"), i18n("nav.help")),
                 class = "nav-link")
    ),
    bslib::nav_item(
      tags$a(icon("book"), i18n("nav.docs"),
             href   = "https://hugomachadorodrigues.github.io/soilKey/",
             target = "_blank")
    ),
    footer = tags$div(
      class = "text-muted small px-3 py-2",
      i18n("app.footer", as.character(utils::packageVersion("soilKey")))
    )
  )
}

# ----------------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- language selector: flip the option + reload so ui() rebuilds --------
  observeEvent(input$app_lang_sel, {
    sel <- input$app_lang_sel
    if (!is.null(sel) && sel %in% c("en", "pt") && !identical(sel, .sk_app_lang())) {
      options(soilKey.app_lang = sel)
      session$reload()
    }
  }, ignoreInit = TRUE)

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
                  icon("circle-info"), paste0(" ", i18n("ribbon.no_pedon"))),
        actionButton("ribbon_example", i18n("ribbon.load_example"),
                     icon = icon("flask"),
                     class = "btn-sm btn-primary")))
    }
    lat <- p$site$lat %||% NA; lon <- p$site$lon %||% NA
    coord <- if (!is.na(lat) && !is.na(lon))
      sprintf("%.3f, %.3f", lat, lon) else i18n("ribbon.no_coords")
    tags$div(
      class = "sk-ribbon",
      tags$span(class = "sk-built", icon("circle-check"), paste0(" ", i18n("ribbon.built"))),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", i18n("ribbon.id")), p$site$id %||% i18n("ribbon.unnamed")),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", i18n("ribbon.horizons")), nrow(p$horizons)),
      tags$span(class = "sk-chip",
                tags$span(class = "sk-key", i18n("ribbon.site")), coord))
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
      title = tagList(icon("seedling"), paste0(" ", i18n("help.title"))),
      easyClose = TRUE, size = "l",
      tags$p(i18n("help.intro")),
      tags$p(tags$strong(i18n("help.workflow"))),
      tags$ol(
        class = "sk-steps",
        tags$li(tags$strong(i18n("nav.pedon")), i18n("help.step_pedon")),
        tags$li(tags$strong(i18n("nav.classify")), i18n("help.step_classify")),
        tags$li(tags$strong(i18n("help.step_enrich_b")), i18n("help.step_enrich")),
        tags$li(tags$strong(i18n("nav.map")), i18n("help.step_map")),
        tags$li(tags$strong(i18n("help.step_robust_b")), i18n("help.step_robust"))
      ),
      tags$p(class = "text-muted small",
             sprintf("soilKey %s",
                     as.character(utils::packageVersion("soilKey")))),
      footer = tagList(
        modalButton(i18n("help.close")),
        actionButton("about_example", i18n("help.load_classify"),
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
