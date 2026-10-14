# =============================================================================
# soilKey Pro -- the visitor counter in the footer (v0.9.260): how many people
# are on the site now, how many have come and from which countries, with a
# small map. The counting is in R/utils_visitors.R.
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
  formatC(as.numeric(x), format = "d", big.mark = if (identical(lang, "pt")) "." else ",")
}

visitors_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "sk-visitors",
    tags$div(class = "sk-visitors-text",
             uiOutput(ns("numbers"), inline = TRUE),
             tags$span(class = "sk-visitors-note", i18n("visitors.note")),
             tags$span(class = "sk-visitors-credit", HTML(sprintf(
               '%s <a href="https://db-ip.com" target="_blank" rel="noopener">DB-IP</a>',
               htmltools::htmlEscape(i18n("visitors.credit")))))),
    tags$div(class = "sk-visitors-map",
             leaflet::leafletOutput(ns("map"), height = "130px")),
    # A first visit tells the server once; a later one only opens a session.
    # The mark is kept in this browser and carries no identifier.
    tags$script(HTML(sprintf(
      "$(document).on('shiny:connected', function() {
         var first = false;
         try { if (!window.localStorage.getItem('soilkey_seen')) {
                 first = true; window.localStorage.setItem('soilkey_seen', '1'); } }
         catch (e) { first = false; }
         Shiny.setInputValue('%s', first, {priority: 'event'});
       });", ns("first_visit"))))
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
        tags$span(i18n("visitors.countries", .vis_fmt(length(s$countries), lang))),
        if (!is.na(s$since)) tags$span(class = "sk-visitors-since",
                                        i18n("visitors.since", format(as.Date(s$since),
                                             if (identical(lang, "pt")) "%d/%m/%Y" else "%Y-%m-%d"))))
    })

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
             zoomControl = FALSE, worldCopyJump = TRUE, minZoom = 0)) |>
        sk_provider_tiles("Esri.WorldGrayCanvas") |>
        leaflet::setView(lng = 0, lat = 20, zoom = 0)
      if (nrow(d)) m <- leaflet::addCircleMarkers(
        m, lng = d$lon, lat = d$lat, radius = 3 + 2.5 * sqrt(d$n),
        stroke = FALSE, fillColor = "#9E5626", fillOpacity = 0.75,
        label = sprintf("%s: %s", nm, .vis_fmt(d$n, lang)))
      m
    })
  })
}
