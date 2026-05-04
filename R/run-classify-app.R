# =============================================================================
# v0.9.39 -- run_classify_app(): convenience wrapper for the Shiny app.
# =============================================================================


#' Launch the soilKey interactive classification Shiny app
#'
#' Drag-and-drop a CSV (one row per horizon) and get all three
#' classifications side-by-side, with a downloadable HTML report.
#' Designed for non-R users (agronomists, students, field workers).
#'
#' Requires the optional packages \code{shiny} and \code{DT} (both
#' listed in Suggests). The function raises a clear error if either
#' is missing.
#'
#' @param port Port for the local server. Default lets Shiny choose.
#' @param launch.browser Whether to open the app in the default
#'        browser (default \code{TRUE}).
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return Invisibly the value returned by \code{shiny::runApp()}.
#' @examples
#' \dontrun{
#' run_classify_app()
#' }
#' @export
run_classify_app <- function(port = NULL, launch.browser = TRUE, ...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required. Install with ",
         "`install.packages(\"shiny\")`.")
  if (!requireNamespace("DT", quietly = TRUE))
    stop("Package 'DT' is required. Install with ",
         "`install.packages(\"DT\")`.")
  app_dir <- system.file("shiny", "classify_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # Development checkout fallback.
    app_dir <- file.path("inst", "shiny", "classify_app")
  }
  if (!dir.exists(app_dir))
    stop("Could not locate the Shiny app at inst/shiny/classify_app.")
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
