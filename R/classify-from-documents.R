# =============================================================================
# soilKey -- High-level "one-liner" entry point for VLM-driven
# classification.
#
# `classify_from_documents()` chains: VLM extraction (PDFs / photos /
# field sheets) -> deterministic classification -> optional report
# rendering. This is the simple/intuitive surface promised in
# ARCHITECTURE.md sec. 10. Pre-v0.9.11, users had to compose
# `vlm_provider()`, `extract_horizons_from_pdf()`,
# `extract_munsell_from_photo()`, `classify_wrb2022()` and
# `report_html()` by hand -- five function calls and ~20 lines of
# glue for the canonical case.
#
# After this:
#
#   classify_from_documents(
#     pdf       = "perfil_042_descricao.pdf",
#     image     = "perfil_042_parede.jpg",
#     provider  = "ollama",                         # local Gemma 4
#     report    = "perfil_042.html"                 # one-pager out
#   )
#
# Returns a list of three ClassificationResults (WRB / SiBCS / USDA)
# plus the populated PedonRecord plus the report path.
# =============================================================================


#' Build a fully-classified `PedonRecord` from documents in one call
#'
#' Highest-level entry point of the soilKey VLM pipeline. Given a
#' soil-description PDF and / or a profile-wall photograph, this
#' function:
#'
#' \enumerate{
#'   \item Constructs a vision-language provider chat object via
#'         \code{\link{vlm_provider}} (defaults to local Ollama with
#'         Gemma 4 edge for institutional independence and data
#'         sovereignty).
#'   \item Extracts horizons from \code{pdf} via
#'         \code{\link{extract_horizons_from_pdf}}, Munsell colours
#'         from \code{image} via
#'         \code{\link{extract_munsell_from_photo}}, and site
#'         metadata from \code{fieldsheet} via
#'         \code{\link{extract_site_from_fieldsheet}}. Every
#'         extracted attribute is stamped \code{source =
#'         "extracted_vlm"} in the PedonRecord's provenance log.
#'   \item Runs the three deterministic keys
#'         (\code{\link{classify_wrb2022}},
#'         \code{\link{classify_sibcs}},
#'         \code{\link{classify_usda}}). The VLM never classifies --
#'         the package's architectural invariant is preserved.
#'   \item Optionally renders a one-pager HTML / PDF report via
#'         \code{\link{report}}.
#' }
#'
#' At least one of \code{pdf}, \code{image} or \code{fieldsheet}
#' must be supplied; you can also pass an existing partially-filled
#' \code{PedonRecord} via \code{pedon} and let this function fill
#' the gaps.
#'
#' @section Why local-first by default:
#' The default \code{provider = "ollama"} runs the entire VLM pipeline
#' on the user's machine via Gemma 4 (edge variant, ~3 GB, multimodal
#' text+image). No part of the soil description, photograph or
#' field sheet ever leaves the local network. This is the
#' recommended configuration for governmental surveys, indigenous
#' land studies, and unpublished research data; it also makes the
#' pipeline reproducible without an internet connection. Cloud
#' providers (\code{"anthropic"}, \code{"openai"}, \code{"google"})
#' remain one argument away when they are the right call.
#'
#' @param pdf        Optional path to a soil-description PDF.
#' @param image      Optional path to a profile-wall image (JPG /
#'                   PNG); if supplied, Munsell extraction is
#'                   attempted with the configured provider.
#' @param fieldsheet Optional path to a site-metadata field sheet
#'                   (image or PDF).
#' @param pedon      Optional existing \code{\link{PedonRecord}};
#'                   when supplied, the function fills only the
#'                   fields VLM extraction can fill (subject to the
#'                   provenance-authority order).
#' @param provider   Either a provider name passed to
#'                   \code{\link{vlm_provider}} (default
#'                   \code{"ollama"}) OR a pre-built ellmer chat
#'                   object (when you want full control over
#'                   \code{system_prompt}, \code{api_key}, ...).
#' @param model      Optional model identifier; passed through to
#'                   \code{vlm_provider()} when \code{provider} is a
#'                   string. Defaults to the per-provider default
#'                   from \code{\link{default_model}}.
#' @param systems    Character vector listing which classification
#'                   systems to run; subset of
#'                   \code{c("wrb", "sibcs", "usda")}. Default: all
#'                   three.
#' @param report     Optional output path for a self-contained
#'                   report (\code{.html} or \code{.pdf}). When
#'                   supplied, \code{\link{report}} is called on the
#'                   classification results + pedon. Default
#'                   \code{NULL} (no report file).
#' @param overwrite  When merging extracted values into an existing
#'                   pedon, allow VLM-extracted attributes to clobber
#'                   already-recorded ones. Default \code{FALSE} --
#'                   the provenance authority order
#'                   (\code{measured > extracted_vlm}) is enforced
#'                   by \code{PedonRecord$add_measurement()}.
#' @param verbose    Emit cli progress messages. Default
#'                   \code{TRUE}.
#' @return A list with elements:
#'   \describe{
#'     \item{\code{pedon}}{The (mutated) \code{\link{PedonRecord}}.}
#'     \item{\code{classifications}}{Named list with up to three
#'           \code{\link{ClassificationResult}} objects keyed by
#'           \code{wrb}, \code{sibcs}, \code{usda}.}
#'     \item{\code{report}}{Path to the rendered report file (if
#'           \code{report = ...} was supplied), else \code{NULL}.}
#'     \item{\code{provider}}{The chat-provider object actually used
#'           (useful for downstream debugging or cost accounting).}
#'   }
#'
#' @section Architectural invariants preserved:
#' \itemize{
#'   \item The VLM never classifies. Every extracted value carries
#'         \code{source = "extracted_vlm"}; the deterministic keys
#'         consume the resulting \code{PedonRecord} unaware of how
#'         each value was obtained.
#'   \item Provenance is preserved end-to-end. The
#'         \code{evidence_grade} on each
#'         \code{ClassificationResult} reflects whether decisive
#'         attributes came from \code{measured},
#'         \code{predicted_spectra}, \code{extracted_vlm},
#'         \code{inferred_prior}, or \code{user_assumed} -- so a
#'         caller always knows how robust the classification is.
#'   \item Authority order is enforced. A pre-existing
#'         \code{measured} value is never silently overwritten by a
#'         later \code{extracted_vlm} value (unless
#'         \code{overwrite = TRUE}).
#' }
#'
#' @examplesIf requireNamespace("ellmer", quietly = TRUE)
#' \dontrun{
#' # The simplest possible end-to-end call -- local Gemma 4 edge.
#' res <- classify_from_documents(
#'   pdf      = "perfil_042_descricao.pdf",
#'   image    = "perfil_042_parede.jpg",
#'   report   = "perfil_042.html"
#' )
#' res$classifications$wrb$name
#' #> "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"
#'
#' # Cloud provider for a one-shot, production run
#' res <- classify_from_documents(
#'   pdf      = "perfil_042_descricao.pdf",
#'   provider = "anthropic"
#' )
#'
#' # Different Gemma 4 size on Ollama
#' res <- classify_from_documents(
#'   pdf      = "perfil_042_descricao.pdf",
#'   provider = "ollama",
#'   model    = "gemma4:31b"
#' )
#' }
#' @seealso \code{\link{vlm_provider}},
#'          \code{\link{extract_horizons_from_pdf}},
#'          \code{\link{classify_wrb2022}},
#'          \code{\link{report}}.
#' @export
classify_from_documents <- function(pdf        = NULL,
                                      image      = NULL,
                                      fieldsheet = NULL,
                                      pedon      = NULL,
                                      provider   = "auto",
                                      model      = NULL,
                                      systems    = c("wrb", "sibcs", "usda"),
                                      report     = NULL,
                                      overwrite  = FALSE,
                                      verbose    = TRUE) {

  if (is.null(pdf) && is.null(image) && is.null(fieldsheet) &&
        is.null(pedon)) {
    rlang::abort(paste0(
      "classify_from_documents(): supply at least one of `pdf`, ",
      "`image`, `fieldsheet`, or an existing `pedon`."
    ))
  }
  systems <- match.arg(systems, c("wrb", "sibcs", "usda"),
                          several.ok = TRUE)

  # ---- 1. Resolve provider -------------------------------------------------
  if (inherits(provider, "Chat") ||
        inherits(provider, "MockVLMProvider")) {
    chat <- provider
    provider_label <- attr(provider, "name") %||% "(custom)"
  } else if (is.character(provider) && length(provider) == 1L) {
    # provider = "auto" picks Ollama if reachable, else falls back to
    # any cloud provider whose API key is set. provider = "ollama"
    # still hard-fails if Ollama is down -- the explicit choice is
    # respected.
    resolved <- if (identical(provider, "auto"))
                  vlm_pick_provider(verbose = verbose)
                else provider
    chat <- vlm_provider(resolved, model = model)
    provider_label <- if (is.null(model)) resolved
                       else sprintf("%s (model=%s)", resolved, model)
  } else {
    rlang::abort(paste0(
      "`provider` must be either a provider name (e.g. \"ollama\", ",
      "\"auto\") or a pre-built ellmer chat object."))
  }

  # ---- 2. Seed the pedon ---------------------------------------------------
  if (is.null(pedon)) {
    pedon <- PedonRecord$new(
      site = list(id = paste0("vlm-", format(Sys.time(),
                                                 "%Y%m%dT%H%M%S")))
    )
  } else if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord (or NULL).")
  }

  if (verbose)
    cli::cli_alert_info("Using provider {.field {provider_label}}.")

  # ---- 3. Extract from each available source ------------------------------
  if (!is.null(pdf)) {
    if (!file.exists(pdf))
      rlang::abort(sprintf("PDF not found: %s", pdf))
    if (verbose) cli::cli_alert_info("Extracting horizons from {.path {pdf}}.")
    pedon <- extract_horizons_from_pdf(pedon, pdf_path = pdf,
                                         provider = chat,
                                         overwrite = overwrite)
  }
  if (!is.null(image)) {
    if (!file.exists(image))
      rlang::abort(sprintf("Image not found: %s", image))
    if (verbose) cli::cli_alert_info("Extracting Munsell from {.path {image}}.")
    pedon <- extract_munsell_from_photo(pedon, image_path = image,
                                          provider = chat,
                                          overwrite = overwrite)
  }
  if (!is.null(fieldsheet)) {
    if (!file.exists(fieldsheet))
      rlang::abort(sprintf("Field sheet not found: %s", fieldsheet))
    if (verbose) cli::cli_alert_info("Extracting site metadata from {.path {fieldsheet}}.")
    pedon <- extract_site_from_fieldsheet(pedon, image_path = fieldsheet,
                                            provider = chat,
                                            overwrite = overwrite)
  }

  # ---- 4. Classify ---------------------------------------------------------
  classifications <- list()
  if ("wrb"   %in% systems)
    classifications$wrb   <- tryCatch(
      classify_wrb2022(pedon, on_missing = "silent"),
      error = function(e) {
        if (verbose) cli::cli_alert_warning("WRB classification failed: {conditionMessage(e)}")
        NULL
      })
  if ("sibcs" %in% systems)
    classifications$sibcs <- tryCatch(
      classify_sibcs(pedon, include_familia = TRUE),
      error = function(e) {
        if (verbose) cli::cli_alert_warning("SiBCS classification failed: {conditionMessage(e)}")
        NULL
      })
  if ("usda"  %in% systems)
    classifications$usda  <- tryCatch(
      classify_usda(pedon),
      error = function(e) {
        if (verbose) cli::cli_alert_warning("USDA classification failed: {conditionMessage(e)}")
        NULL
      })

  # ---- 5. Report (optional) -----------------------------------------------
  report_path <- NULL
  if (!is.null(report)) {
    results_list <- Filter(Negate(is.null), classifications)
    if (length(results_list) > 0L) {
      report_path <- report(results_list, file = report,
                              pedon = pedon)
      if (verbose)
        cli::cli_alert_success("Report written to {.path {report_path}}")
    } else if (verbose) {
      cli::cli_alert_warning("No classification succeeded; report not written.")
    }
  }

  invisible(list(
    pedon           = pedon,
    classifications = classifications,
    report          = report_path,
    provider        = chat
  ))
}
