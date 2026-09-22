# =============================================================================
# v0.9.39  -- run_classify_app(): convenience wrapper for the Shiny app.
# v0.9.97  -- added the `ui` argument ("pro" / "classic").
# v0.9.117 -- retired the legacy single-page "classic" app; `ui = "classic"`
#             now warns once and launches the Pro app instead.
# =============================================================================


#' Launch the soilKey interactive classification Shiny app
#'
#' Opens a local Shiny app ("Pro") that drives the soilKey pipeline from a
#' browser -- no R code required: build a pedon from a canonical fixture, a CSV
#' upload, or an interactive horizon editor; classify under WRB 2022 / SiBCS 5 /
#' USDA ST 13 with the full key trace; run VLM photo extraction, OSSL spectral
#' gap-fill, the SoilGrids spatial prior, an interactive \pkg{leaflet} map that
#' queries the class prior at a clicked point, and a Monte-Carlo robustness
#' analysis; and download a cross-system HTML or PDF report. The interface is
#' bilingual (English / Portuguese; see \code{lang}).
#'
#' Needs the optional packages \pkg{bslib}, \pkg{shinyWidgets}, \pkg{plotly}
#' and \pkg{leaflet} (all in \code{Suggests}); the function raises a clear,
#' copy-pasteable error if any are missing.
#'
#' @param ui Kept for back-compatibility. \code{"pro"} (default) launches the
#'   professional multi-tab app. \code{"classic"} -- the original single-page
#'   uploader -- was \strong{retired in v0.9.117}; passing it now emits a
#'   deprecation warning and launches the Pro app instead.
#' @param lang Initial interface language: \code{"en"} (default) or \code{"pt"}
#'   (Brazilian Portuguese). Can also be switched live from the app's navbar.
#' @param port Port for the local server. Default lets Shiny choose.
#' @param launch.browser Whether to open the app in the default
#'        browser (default \code{TRUE}).
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return Invisibly the value returned by \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' run_classify_app()              # professional multi-tab app (English)
#' run_classify_app(lang = "pt")   # interface em portugues
#' }
#' @export
run_classify_app <- function(ui = c("pro", "classic"),
                             lang = c("en", "pt"),
                             port = NULL, launch.browser = TRUE, ...) {
  ui   <- match.arg(ui)
  lang <- match.arg(lang)
  if (ui == "classic") {
    warning("The 'classic' single-page app was retired in soilKey 0.9.117; ",
            "launching the Pro app instead.", call. = FALSE)
    ui <- "pro"
  }
  # The pro app reads getOption("soilKey.app_lang") when it builds its UI; set
  # it for this launch and restore the previous value once the app closes.
  old_lang <- getOption("soilKey.app_lang")
  options(soilKey.app_lang = lang)
  on.exit(options(soilKey.app_lang = old_lang), add = TRUE)

  needed <- c("shiny", "bslib", "DT", "plotly", "shinyWidgets", "leaflet")
  missing_pkgs <- needed[!vapply(needed, requireNamespace,
                                 logical(1L), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop("The Pro app needs these package(s): ",
         paste(missing_pkgs, collapse = ", "),
         ".\n  Install with: install.packages(c(",
         paste0("\"", missing_pkgs, "\"", collapse = ", "), "))",
         call. = FALSE)
  }

  app_dir <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # Development checkout fallback.
    app_dir <- file.path("inst", "shiny", "classify_app_pro")
  }
  if (!dir.exists(app_dir))
    stop("Could not locate the Shiny app at inst/shiny/classify_app_pro.",
         call. = FALSE)
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
