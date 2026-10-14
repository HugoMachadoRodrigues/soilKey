# =============================================================================
# soilKey Pro -- the visitor counter in the footer (v0.9.261): how many people
# are on the site now, how many have come and from which countries. The
# numbers take one line of the footer; the map of the countries opens above
# it on a click. The counting is in R/utils_visitors.R.
# =============================================================================

# The countries the map can place (ISO code, a point, the name in en and pt);
# built by data-raw/visitor_countries.R.
.vis_countries_table <- function() {
  if (is.null(.vis_env$countries)) {
    f <- file.path(getwd(), "data", "countries.csv")
    .vis_env$countries <- if (file.exists(f))
      utils::read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "")
    else data.frame(iso2 = character(0), lon = numeric(0), lat = numeric(0),
                    name_en = character(0), name_pt = character(0))
  }
  .vis_env$countries
}

# A number written the way the language writes it (1,234 / 1.234).
.vis_fmt <- function(x, lang) {
  pt <- identical(lang, "pt")
  formatC(as.numeric(x), format = "d", big.mark = if (pt) "." else ",",
          decimal.mark = if (pt) "," else ".")
}

visitors_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "sk-visitors",
    uiOutput(ns("numbers"), inline = TRUE),
    # The map waits, hidden, above the footer: the footer is on screen in every
    # tab, and a map in it took the height the tabs' own tables need (with a
    # 790 px window the key trace of Classify had none left and was not drawn).
    tags$div(
      id = ns("panel"), class = "sk-visitors-panel", hidden = NA,
      role = "region", `aria-label` = i18n("visitors.map_title"),
      tags$div(class = "sk-visitors-panel-title", i18n("visitors.map_title")),
      leaflet::leafletOutput(ns("map"), height = "210px"),
      tags$div(class = "sk-visitors-note", i18n("visitors.note"), " ",
               tags$span(class = "sk-visitors-credit", HTML(sprintf(
                 '%s <a href="https://db-ip.com" target="_blank" rel="noopener">DB-IP</a>.',
                 htmltools::htmlEscape(i18n("visitors.credit"))))))),
    # A first visit tells the server once; a later one only opens a session.
    # The mark is kept in this browser and carries no identifier. The button of
    # the numbers opens and closes the map; a click elsewhere or Escape closes
    # it, and so does scrolling the page. Shiny draws the map when it is shown
    # ('shown'), at its size.
    tags$script(HTML(sprintf(
      "(function() {
         var panel = '#%s', btn = '.sk-visitors-toggle';
         function isOpen() { return !$(panel).is('[hidden]'); }
         function setOpen(open) {
           var p = $(panel), b = $(btn).first(); if (!p.length || !b.length) return;
           if (open) {
             // above the button, or below it when there is no room above (a
             // phone scrolls the page, so the footer is not always at the bottom)
             p.removeAttr('hidden');
             var r = b[0].getBoundingClientRect(), h = p.outerHeight();
             if (r.top - h - 8 >= 8) p.css({top: 'auto', bottom: (window.innerHeight - r.top + 8) + 'px'});
             else p.css({bottom: 'auto', top: Math.max(8, Math.min(r.bottom + 8, window.innerHeight - h - 8)) + 'px'});
             p.trigger('shown');
             setTimeout(function() { $(window).trigger('resize'); }, 60);
           } else { p.attr('hidden', ''); p.trigger('hidden'); }
           $(btn).attr('aria-expanded', open ? 'true' : 'false');
         }
         $(document).on('click', btn, function(e) { e.stopPropagation(); setOpen(!isOpen()); });
         $(document).on('click', function(e) {
           if (isOpen() && !$(e.target).closest(panel + ',' + btn).length) setOpen(false);
         });
         $(window).on('scroll', function() { if (isOpen()) setOpen(false); });
         $(document).on('keydown', function(e) {
           if (e.key === 'Escape' && isOpen()) { setOpen(false); $(btn).trigger('focus'); }
         });
         $(document).on('shiny:value', function(e) {
           if (e.name === '%s') setTimeout(function() {
             $(btn).attr('aria-expanded', isOpen() ? 'true' : 'false'); }, 0);
         });
         $(document).on('shiny:connected', function() {
           var first = false;
           try { if (!window.localStorage.getItem('soilkey_seen')) {
                   first = true; window.localStorage.setItem('soilkey_seen', '1'); } }
           catch (e) { first = false; }
           Shiny.setInputValue('%s', first, {priority: 'event'});
         });
       })();", ns("panel"), ns("numbers"), ns("first_visit"))))
  )
}

visitors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    req0 <- session$request
    host <- req0$HTTP_HOST %||% ""
    coll <- .vis_collection(host)
    lang <- .sk_app_lang()
    counted <- FALSE

    # the session is counted once, when its page has run (a crawler that
    # never runs the page's script is not), and not when the browser is an
    # automated one
    observeEvent(input$first_visit, {
      if (counted || .vis_is_automated(req0$HTTP_USER_AGENT)) return()
      counted <<- TRUE
      ip <- .vis_client_ip(req0$HTTP_X_FORWARDED_FOR, req0$REMOTE_ADDR)
      cc <- if (isTRUE(input$first_visit)) .vis_country(ip) else NA_character_
      .vis_record(coll, isTRUE(input$first_visit), cc)
      .vis_session_open(coll)
      session$onSessionEnded(function() .vis_session_close(coll))
    }, once = TRUE)

    # the numbers of this process, refreshed by its ticks
    stats <- reactivePoll(5000, session,
                          checkFunc = function() .vis_env$version %||% 0L,
                          valueFunc = function() .vis_env$stats[[coll]])

    output$numbers <- renderUI({
      s <- stats()
      if (is.null(s)) return(NULL)
      tagList(
        tags$span(class = "sk-visitors-online", tags$span(class = "sk-visitors-dot"),
                  i18n("visitors.online", .vis_fmt(max(s$online, 1L), lang))),
        tags$span(i18n("visitors.people", .vis_fmt(s$visitors, lang))),
        tags$button(type = "button", class = "sk-visitors-toggle",
                    `aria-controls` = session$ns("panel"), `aria-expanded` = "false",
                    title = i18n("visitors.map_show"),
                    i18n("visitors.countries", .vis_fmt(length(s$countries), lang)),
                    shiny::icon("earth-americas", `aria-hidden` = "true")),
        if (!is.na(s$since)) tags$span(class = "sk-visitors-since",
                                        i18n("visitors.since", format(as.Date(s$since),
                                             if (identical(lang, "pt")) "%d/%m/%Y" else "%Y-%m-%d"))))
    })
    # the line is hidden while it is empty, so it must be drawn while hidden
    outputOptions(output, "numbers", suspendWhenHidden = FALSE)

    output$map <- leaflet::renderLeaflet({
      s <- stats()
      ct <- .vis_countries_table()
      n <- if (is.null(s)) numeric(0) else s$countries
      d <- ct[ct$iso2 %in% names(n) & !is.na(ct$lon), , drop = FALSE]
      d$n <- as.numeric(n[d$iso2])
      nm <- if (identical(lang, "pt")) d$name_pt else d$name_en
      # the app's own leaflet and provider helpers (R/utils_ui.R), so the map's
      # scripts are those every instance registered at start-up; Esri's gray
      # canvas, with its attribution (CARTO's tiles now ask for a key)
      m <- sk_leaflet(options = leaflet::leafletOptions(
             zoomControl = FALSE, worldCopyJump = TRUE, minZoom = 0,
             zoomSnap = 0.25)) |>
        sk_provider_tiles("Esri.WorldGrayCanvas") |>
        leaflet::fitBounds(lng1 = -160, lat1 = -55, lng2 = 180, lat2 = 72)
      if (nrow(d)) m <- leaflet::addCircleMarkers(
        m, lng = d$lon, lat = d$lat, radius = 3 + 2.5 * sqrt(d$n),
        stroke = FALSE, fillColor = "#9E5626", fillOpacity = 0.75,
        label = sprintf("%s: %s", nm, .vis_fmt(d$n, lang)))
      m
    })
  })
}
