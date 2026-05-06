# =============================================================================
# v0.9.65 -- run_agent_app(): launches the soilKey "Agente Pedometrista".
# =============================================================================


#' Launch the soilKey "Agente Pedometrista" Shiny app
#'
#' A modern bslib-themed Shiny UI for end-to-end soil profile
#' classification driven by a local Gemma 4 (or any cloud VLM) for
#' multimodal extraction:
#'
#' \enumerate{
#'   \item Upload a profile photo, PDF report, field-sheet image or
#'         Vis-NIR spectrum.
#'   \item The VLM extracts schema-validated structured data into a
#'         \code{\link{PedonRecord}} with explicit per-attribute
#'         provenance (\code{source = "extracted_vlm"}).
#'   \item The deterministic R taxonomic key classifies the pedon
#'         under WRB 2022, SiBCS 5a edicao and USDA Soil Taxonomy
#'         13ed -- never the LLM.
#'   \item A free-form chat tab lets the user ask the local Gemma
#'         (with the soilKey "pedometrista" persona) about the loaded
#'         profile in PT-BR or English.
#' }
#'
#' Requires the optional packages \code{shiny}, \code{bslib},
#' \code{bsicons} and \code{DT} (all in Suggests). For local Gemma
#' inference, also requires Ollama -- see \code{\link{setup_local_vlm}}
#' for one-shot bootstrap from inside R.
#'
#' @param port Port for the local server. Default (\code{NULL}) lets
#'        Shiny choose.
#' @param launch.browser Whether to open the app in the default
#'        browser (default \code{TRUE}).
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return Invisibly the value returned by \code{shiny::runApp()}.
#'
#' @examples
#' \dontrun{
#' # First-time setup (download Gemma 4 edge):
#' setup_local_vlm("light")   # gemma4:e2b, ~1.5 GB
#'
#' # Launch the agent UI:
#' run_agent_app()
#' }
#'
#' @seealso \code{\link{setup_local_vlm}}, \code{\link{vlm_provider}},
#'   \code{\link{extract_munsell_from_photo}},
#'   \code{\link{extract_horizons_from_pdf}},
#'   \code{\link{extract_site_from_fieldsheet}},
#'   \code{\link{classify_from_documents}},
#'   \code{\link{run_classify_app}} (the simpler CSV-only UI).
#' @export
run_agent_app <- function(port = NULL, launch.browser = TRUE, ...) {
  needed <- c("shiny", "bslib", "bsicons", "DT")
  missing <- needed[!vapply(needed, requireNamespace, logical(1L),
                              quietly = TRUE)]
  if (length(missing) > 0L) {
    stop(sprintf(
      "Packages required for run_agent_app() are missing: %s. Install with `install.packages(c(%s))`.",
      paste(missing, collapse = ", "),
      paste(sprintf('"%s"', missing), collapse = ", ")
    ))
  }
  app_dir <- system.file("shiny", "agent_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    app_dir <- file.path("inst", "shiny", "agent_app")
  }
  if (!dir.exists(app_dir)) {
    stop("Could not locate the Shiny app at inst/shiny/agent_app.")
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}
