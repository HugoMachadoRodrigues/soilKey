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

# A refined soil-science palette layered on flatly (see www/soilkey.css for the
# full design system). Earth tones (topsoil brown, terracotta subsoil, moss)
# with a cool slate `info` accent for measurement/data cues.
sk_theme <- bslib::bs_theme(
  version = 5, bootswatch = "flatly",
  primary   = "#7A5230",   # topsoil brown (buttons, active nav)
  secondary = "#B5652E",   # terracotta subsoil (accents)
  success   = "#5E7B3B",   # vegetation moss (positive states)
  info      = "#4E6E81",   # slate (data / measurement cues)
  warning   = "#C9962F",   # ochre
  danger    = "#A63D40",   # oxidised red
  "navbar-bg"          = "#4A3226",   # deep espresso
  "border-radius"      = "0.55rem",
  "border-radius-lg"   = "0.8rem",
  "card-cap-bg"        = "#F3ECE2"
)

ui <- function(request) {
  bslib::page_navbar(
    title  = tags$span(class = "navbar-brand-inner",
                       tags$img(src = "logo.png", class = "sk-logo",
                                alt = "soilKey", height = "34"),
                       # one wordmark node built as a single HTML string so
                       # htmltools does not insert indentation whitespace
                       # between the pieces (which collapsed to "soil Key Pro").
                       tags$span(class = "sk-wordmark", htmltools::HTML(paste0(
                         "soil<span class=\"sk-mark\">Key</span>",
                         htmltools::htmlEscape(i18n("app.brand_suffix")))))),
    id     = "main_nav",
    theme  = sk_theme,
    fillable = TRUE,
    # Explicit browser-tab title: the `title` arg is an HTML wordmark, so without
    # this the document <title> renders the mangled tag text ("soil Key Pro").
    window_title = "soilKey Pro",
    # a11y: the document language follows the chosen interface language so
    # screen readers use the right pronunciation rules.
    lang   = .sk_app_lang(),
    # Stylesheet + the global pedon ribbon render above the tab content.
    header = tagList(
      tags$head(
        # Browser-tab icon (favicon): the soilKey logo.
        tags$link(rel = "icon", type = "image/png", href = "logo.png"),
        tags$link(rel = "apple-touch-icon", href = "logo.png"),
        # version query-string busts the browser cache when the CSS changes
        tags$link(rel = "stylesheet", type = "text/css",
                  href = paste0("soilkey.css?v=",
                                as.character(utils::packageVersion("soilKey")))),
        # a11y: announce transient showNotification() toasts to screen readers
        # (the panel is created on demand, so tag it once it appears).
        tags$script(htmltools::HTML(
          "document.addEventListener('DOMContentLoaded',function(){",
          "new MutationObserver(function(){",
          "var p=document.getElementById('shiny-notification-panel');",
          "if(p&&!p.getAttribute('aria-live')){p.setAttribute('aria-live','polite');p.setAttribute('role','status');}",
          "}).observe(document.body,{childList:true,subtree:true});});")),
        # Welcome tour: on the first connection from this browser (no
        # localStorage flag yet) ask the server to open the guided tour. The
        # server marks the browser as welcomed via a custom message so the tour
        # never auto-opens again -- users can replay it from Help.
        tags$script(htmltools::HTML(paste0(
          "$(document).on('shiny:connected',function(){try{",
          "if(!window.localStorage.getItem('soilkey_welcomed_v2')){",
          "Shiny.setInputValue('show_welcome',(new Date()).getTime(),{priority:'event'});",
          "}}catch(e){}});",
          "Shiny.addCustomMessageHandler('soilkey_mark_welcomed',function(x){",
          "try{window.localStorage.setItem('soilkey_welcomed_v2','1');}catch(e){}});",
          # clearing the flag re-arms the auto-open (used by 'Take the tour')
          "Shiny.addCustomMessageHandler('soilkey_clear_welcomed',function(x){",
          "try{window.localStorage.removeItem('soilkey_welcomed_v2');}catch(e){}});",
          # scroll the chat transcript to the newest message
          "Shiny.addCustomMessageHandler('sk_chat_scroll',function(id){",
          "try{var d=document.getElementById(id);if(d)d.scrollTop=d.scrollHeight;}catch(e){}});",
          # the Assistant drawer: a right-side panel that opens on ANY tab. Pure
          # client-side toggle (a FAB opens it, the header X / Esc / backdrop
          # closes it) so it is available everywhere without a server round-trip.
          "document.addEventListener('DOMContentLoaded',function(){",
          "var open=function(){document.body.classList.add('sk-assistant-open');};",
          "var close=function(){document.body.classList.remove('sk-assistant-open');};",
          "document.addEventListener('click',function(e){",
          "if(e.target.closest('#sk_assistant_fab')){open();}",
          "else if(e.target.closest('#sk_assistant_close')||e.target.closest('#sk_assistant_backdrop')){close();}});",
          "document.addEventListener('keydown',function(e){if(e.key==='Escape')close();});});")))
      ),
      uiOutput("pedon_ribbon"),
      # ---- the Assistant: a floating button + a right-side slide-out drawer,
      # persistent across every tab (position:fixed, so outside the nav flow).
      tags$button(id = "sk_assistant_fab", class = "sk-assistant-fab",
                  type = "button", `aria-label` = i18n("chat.open_assistant"),
                  tags$img(src = "logo.png", class = "sk-assistant-fab-logo",
                           alt = "soilKey"),
                  tags$span(i18n("chat.assistant"))),
      tags$div(id = "sk_assistant_backdrop", class = "sk-assistant-backdrop"),
      tags$aside(class = "sk-assistant-drawer", `aria-label` = i18n("chat.assistant"),
                 chat_ui("chat"))
    ),
    bslib::nav_panel(i18n("nav.pedon"),    icon = icon("layer-group"),  pedon_ui("pedon")),
    bslib::nav_panel(i18n("nav.classify"), icon = icon("sitemap"),      classify_ui("classify")),
    bslib::nav_panel(i18n("nav.photo"),    icon = icon("camera"),       photo_ui("photo")),
    bslib::nav_panel(i18n("nav.spectra"),  icon = icon("wave-square"),  spectra_ui("spectra")),
    # v0.9.174: the three former sub-tabs (Point prior / Batch / Grid) are now
    # ONE square map driven by a mode selector, all centred on the same point,
    # with a shared SoilGrids overlay -- so the point, its neighbours and the
    # SoilGrids prior are seen together instead of on three unsynced maps.
    bslib::nav_panel(
      i18n("nav.map"), icon = icon("map-location-dot"),
      map_ui("map")
    ),
    bslib::nav_panel(i18n("nav.uncertainty"), icon = icon("dice"),         uncertainty_ui("uncertainty")),
    bslib::nav_panel(i18n("nav.report"),      icon = icon("file-arrow-down"), report_ui("report")),
    bslib::nav_panel(i18n("nav.thanks"),      icon = icon("heart"),        acknowledgements_ui("thanks")),
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
    # Light/dark theme toggle (sun/moon). Defaults to following the user's OS
    # colour scheme; the dark palette + contrast live in www/soilkey.css under
    # [data-bs-theme="dark"].
    bslib::nav_item(
      htmltools::tagAppendAttributes(
        bslib::input_dark_mode(id = "color_mode"),
        title = i18n("a11y.theme_toggle"),
        "aria-label" = i18n("a11y.theme_toggle"))
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
    bslib::nav_item(
      # "Support": opens an in-app modal with contact guidance and a button that
      # composes an email. The address is assembled in JS at click time, so it
      # is never rendered as visible text or a hoverable href.
      actionLink("support",
                 label = tagList(icon("life-ring"), i18n("nav.support")),
                 class = "nav-link", title = i18n("nav.support_tip"))
    ),
    footer = tags$div(
      class = "sk-footer",
      tags$img(src = "logo.png", height = "22", alt = "", class = "sk-footer-logo"),
      tags$span(class = "sk-footer-title", "soilKey ",
                tags$span(class = "sk-footer-ver",
                          as.character(utils::packageVersion("soilKey")))),
      tags$span(class = "sk-footer-tag", i18n("app.footer_tag")),
      tags$span(
        class = "sk-footer-links",
        tags$a(href = "https://hugomachadorodrigues.github.io/soilKey/",
               target = "_blank", rel = "noopener", i18n("nav.docs")),
        tags$a(href = "https://github.com/HugoMachadoRodrigues/soilKey",
               target = "_blank", rel = "noopener", "GitHub"),
        tags$a(href = "https://CRAN.R-project.org/package=soilKey",
               target = "_blank", rel = "noopener", "CRAN"),
        tags$span(class = "sk-footer-lic", i18n("app.footer_license")))
    )
  )
}

# ----------------------------------------------------------------------------
# Welcome tour -- a dependency-free, step-by-step onboarding modal shown on the
# first open (localStorage-gated) and replayable from the Help menu. Each step
# is one facet of the workflow; the final step offers the two on-ramps (load
# the example and classify, or start a blank profile).
# ----------------------------------------------------------------------------
.PRO_WELCOME_STEPS <- 4L

.pro_welcome_modal <- function(step) {
  step <- max(1L, min(.PRO_WELCOME_STEPS, as.integer(step)))
  head_line <- switch(
    step,
    tagList(icon("hand-sparkles"), " ", i18n("welcome.title")),
    tagList(icon("layer-group"),   " ", i18n("welcome.s2_title")),
    tagList(icon("sitemap"),       " ", i18n("welcome.s3_title")),
    tagList(icon("compass"),       " ", i18n("welcome.s4_title")))
  body <- switch(
    step,
    tags$p(i18n("welcome.s1_body")),
    tags$p(i18n("welcome.s2_body")),
    tags$p(i18n("welcome.s3_body")),
    tags$p(i18n("welcome.s4_body")))
  dots <- tags$div(
    class = "sk-welcome-dots",
    lapply(seq_len(.PRO_WELCOME_STEPS), function(i)
      tags$span(class = if (i == step) "sk-dot sk-dot-on" else "sk-dot")))
  back <- if (step > 1L)
    actionButton("welcome_back", i18n("welcome.back"),
                 icon = icon("arrow-left"), class = "btn-outline-secondary")
  nxt  <- if (step < .PRO_WELCOME_STEPS)
    actionButton("welcome_next", i18n("welcome.next"),
                 icon = icon("arrow-right"), class = "btn-primary")
  ctas <- if (step == .PRO_WELCOME_STEPS)
    tagList(
      actionButton("welcome_scratch", i18n("welcome.start_scratch"),
                   icon = icon("pen"), class = "btn-outline-secondary"),
      actionButton("welcome_example", i18n("welcome.load_example"),
                   icon = icon("flask"), class = "btn-primary"))
  modalDialog(
    title = head_line, easyClose = FALSE, size = "l",
    tags$div(class = "text-muted small mb-2",
             i18n("welcome.step_of", step, .PRO_WELCOME_STEPS)),
    body, dots,
    footer = tagList(
      actionButton("welcome_skip", i18n("welcome.skip"),
                   class = "btn-link text-muted"),
      back, nxt, ctas))
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
  chat_server("chat",              rv, settings)
  photo_server("photo",            rv)
  spectra_server("spectra",        rv)
  map_server("map",                rv, settings)  # unified: point / batch / grid
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
        actionButton("about_tour", i18n("welcome.take_tour"),
                     icon = icon("compass"), class = "btn-outline-primary"),
        actionButton("about_example", i18n("help.load_classify"),
                     icon = icon("flask"), class = "btn-primary"))
    ))
  })
  observeEvent(input$about_example, {
    removeModal()
    rv$example_request <- rv$example_request + 1L
    bslib::nav_select("main_nav", "Classify")
  })

  # ---- welcome tour (first open + replay from Help) -----------------------
  welcome_step <- reactiveVal(1L)
  mark_welcomed <- function() session$sendCustomMessage("soilkey_mark_welcomed", TRUE)
  open_welcome <- function() { welcome_step(1L); showModal(.pro_welcome_modal(1L)) }

  observeEvent(input$show_welcome, open_welcome())          # first open (JS)
  observeEvent(input$about_tour, {                          # replay + re-arm
    session$sendCustomMessage("soilkey_clear_welcomed", TRUE)
    removeModal(); open_welcome()
  })
  observeEvent(input$welcome_next, {
    s <- min(.PRO_WELCOME_STEPS, welcome_step() + 1L)
    welcome_step(s); showModal(.pro_welcome_modal(s))
  })
  observeEvent(input$welcome_back, {
    s <- max(1L, welcome_step() - 1L)
    welcome_step(s); showModal(.pro_welcome_modal(s))
  })
  observeEvent(input$welcome_skip, { removeModal(); mark_welcomed() })
  observeEvent(input$welcome_scratch, {
    removeModal(); mark_welcomed(); bslib::nav_select("main_nav", "Pedon")
  })
  observeEvent(input$welcome_example, {
    removeModal(); mark_welcomed()
    rv$example_request <- rv$example_request + 1L
    bslib::nav_select("main_nav", "Classify")
  })

  # ---- Support modal ------------------------------------------------------
  # Opens an in-app dialog (so the click always does something visible) with a
  # "Compose email" button. The support address is assembled in JS at click
  # time, so it never appears as visible text or a hoverable href in the DOM.
  observeEvent(input$support, {
    showModal(modalDialog(
      title = tagList(icon("life-ring"), " ", i18n("support.title")),
      easyClose = TRUE, size = "m",
      tags$p(i18n("support.body")),
      tags$ul(
        tags$li(i18n("support.bullet_email")),
        tags$li(HTML(sprintf(
          '%s <a href="https://github.com/HugoMachadoRodrigues/soilKey/issues" target="_blank" rel="noopener">GitHub Issues</a>.',
          i18n("support.bullet_issues"))))
      ),
      footer = tagList(
        modalButton(i18n("support.close")),
        tags$a(
          class = "btn btn-primary",
          icon("envelope"), " ", i18n("support.compose"),
          href = "#",
          onclick = paste0(
            "var a=['rodrigues.h','ufl.edu'].join(String.fromCharCode(64));",
            "var s=encodeURIComponent('soilKey Pro — support request');",
            "var b=encodeURIComponent('Please describe your question or the ",
            "problem and what you were doing when it happened:",
            "\\n\\n\\n\\n--- soilKey Pro');",
            "window.location.href='mailto:'+a+'?subject='+s+'&body='+b;",
            "return false;"))
      )
    ))
  })
}

shinyApp(ui = ui, server = server)
