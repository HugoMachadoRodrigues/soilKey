#' Launch the soilKey Shiny demo (one-screen GUI)
#'
#' Opens a Shiny app that lets a non-coder pick one of the 31
#' canonical profiles or upload a small horizons CSV, click
#' \strong{Classify}, and read the WRB / SiBCS / USDA names plus the
#' deterministic key trace and the evidence grade. Useful for live
#' demos, classroom teaching, and for pedologists who want to verify
#' the package on a profile they already know without writing R code.
#'
#' Requires the \code{shiny} package. The taxonomic key is still
#' deterministic: no VLM is invoked from the GUI.
#'
#' @param ... Forwarded to \code{shiny::runApp()} (e.g.
#'        \code{port = 4321}, \code{launch.browser = FALSE},
#'        \code{host = "0.0.0.0"}).
#' @return Invisibly, the value returned by
#'         \code{shiny::runApp()}.
#' @export
#' @examples
#' \dontrun{
#'   soilKey::run_demo()
#' }
run_demo <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'shiny' is required for run_demo() but is not installed. ",
      "Install it with install.packages('shiny')."
    ))
  }
  app_dir <- system.file("shiny-demo", package = "soilKey")
  if (!nzchar(app_dir)) {
    # When loaded via pkgload::load_all() during development.
    app_dir <- file.path("inst", "shiny-demo")
  }
  if (!file.exists(file.path(app_dir, "app.R"))) {
    rlang::abort(sprintf(
      "soilKey demo app not found at %s. Reinstall the package.",
      app_dir
    ))
  }
  invisible(shiny::runApp(app_dir, ...))
}
