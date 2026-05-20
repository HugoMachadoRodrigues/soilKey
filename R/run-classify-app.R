# =============================================================================
# v0.9.39 -- run_classify_app(): convenience wrapper for the Shiny app.
# v0.9.97 -- adds the `ui` argument to choose the professional multi-tab app
#            ("pro", default) or the legacy single-page uploader ("classic").
# =============================================================================


#' Launch the soilKey interactive classification Shiny app
#'
#' Opens a local Shiny app that drives the soilKey pipeline from a browser --
#' no R code required. Two interfaces are available:
#'
#' \describe{
#'   \item{\code{"pro"} (default)}{A professional multi-tab app: build a pedon
#'     from a canonical fixture, a CSV upload, or an interactive horizon
#'     editor; classify under WRB 2022 / SiBCS 5 / USDA ST 13 with the full
#'     key trace; run VLM photo extraction, OSSL spectral gap-fill, the
#'     SoilGrids spatial prior, and a Monte-Carlo robustness analysis; and
#'     download a cross-system HTML or PDF report. Needs the optional
#'     packages \pkg{bslib}, \pkg{shinyWidgets} and \pkg{plotly}.}
#'   \item{\code{"classic"}}{The original single-page uploader (v0.9.39):
#'     drag-and-drop a CSV and get the three classifications side-by-side.
#'     Needs only \pkg{shiny} and \pkg{DT}.}
#' }
#'
#' All optional packages are listed in \code{Suggests}; the function raises a
#' clear, copy-pasteable error if any are missing.
#'
#' @param ui One of \code{"pro"} (default) or \code{"classic"}. See above.
#' @param port Port for the local server. Default lets Shiny choose.
#' @param launch.browser Whether to open the app in the default
#'        browser (default \code{TRUE}).
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return Invisibly the value returned by \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' run_classify_app()                 # professional multi-tab app
#' run_classify_app(ui = "classic")   # legacy single-page uploader
#' }
#' @export
run_classify_app <- function(ui = c("pro", "classic"),
                             port = NULL, launch.browser = TRUE, ...) {
  ui <- match.arg(ui)

  needed <- if (ui == "pro")
    c("shiny", "bslib", "DT", "plotly", "shinyWidgets")
  else
    c("shiny", "DT")
  missing_pkgs <- needed[!vapply(needed, requireNamespace,
                                 logical(1L), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop("The ", ui, " app needs these package(s): ",
         paste(missing_pkgs, collapse = ", "),
         ".\n  Install with: install.packages(c(",
         paste0("\"", missing_pkgs, "\"", collapse = ", "), "))",
         call. = FALSE)
  }

  app_subdir <- if (ui == "pro") "classify_app_pro" else "classify_app"
  app_dir <- system.file("shiny", app_subdir, package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # Development checkout fallback.
    app_dir <- file.path("inst", "shiny", app_subdir)
  }
  if (!dir.exists(app_dir))
    stop("Could not locate the Shiny app at inst/shiny/", app_subdir, ".",
         call. = FALSE)
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
