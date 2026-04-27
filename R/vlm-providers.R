# ================================================================
# Module 2 -- VLM provider abstraction
#
# Thin wrapper over `ellmer` so that downstream extraction code does
# not need to care about which provider is used. The VLM never
# classifies; it only extracts schema-validated structured data from
# unstructured documents and photos. See ARCHITECTURE.md, section 7.
#
# Providers supported (via ellmer):
#   anthropic -> ellmer::chat_anthropic
#   openai    -> ellmer::chat_openai
#   google    -> ellmer::chat_google_gemini
#   ollama    -> ellmer::chat_ollama   (local, default for institutional
#                                       independence and sensitive data)
# ================================================================


#' Default VLM model per provider
#'
#' Returns a sensible default model name for the requested provider.
#' These defaults are chosen for vision capability (multimodal) and
#' general structured-extraction reliability; users can override via
#' the \code{model} argument of \code{\link{vlm_provider}}.
#'
#' @param name Provider name; one of \code{"anthropic"}, \code{"openai"},
#'        \code{"google"}, \code{"ollama"}.
#' @return Character scalar with the default model identifier.
#' @keywords internal
default_model <- function(name) {
  name <- match.arg(name, c("anthropic", "openai", "google", "ollama"))
  switch(name,
    anthropic = "claude-sonnet-4-5",
    openai    = "gpt-4o",
    google    = "gemini-1.5-pro",
    ollama    = "gemma3:27b"
  )
}


#' Construct a VLM provider chat object
#'
#' Returns an \code{ellmer} chat object configured for the given
#' provider, ready to be passed to the extraction functions
#' (\code{\link{extract_horizons_from_pdf}}, etc.). The chat object
#' wraps API credentials and model selection; it does not itself send
#' any request.
#'
#' This is purely a convenience wrapper: it picks a default model per
#' provider and forwards remaining arguments (e.g.
#' \code{system_prompt}, \code{api_key}) to the underlying ellmer
#' constructor. \code{ellmer} must be installed.
#'
#' @section Local-first option:
#' Passing \code{name = "ollama"} runs every extraction locally via
#' an Ollama server (default \code{gemma3:27b}). No data leaves the
#' machine, which is the recommended setting for sensitive field
#' descriptions (e.g. governmental surveys, indigenous land studies)
#' where institutional independence and data sovereignty matter.
#'
#' @param name Provider name. One of \code{"anthropic"} (Claude),
#'        \code{"openai"} (GPT-4o family), \code{"google"} (Gemini),
#'        \code{"ollama"} (local).
#' @param model Optional model identifier; defaults to
#'        \code{default_model(name)}.
#' @param ... Additional arguments forwarded to the corresponding
#'        \code{ellmer::chat_*} constructor (e.g. \code{system_prompt},
#'        \code{api_key}, \code{base_url}, \code{params}).
#' @return An \code{ellmer} \code{Chat} object exposing a \code{$chat()}
#'         method for sending prompts.
#' @export
#' @examplesIf requireNamespace("ellmer", quietly = TRUE)
#' \dontrun{
#' # Cloud provider (needs ANTHROPIC_API_KEY)
#' provider <- vlm_provider("anthropic")
#'
#' # Local provider (needs a running Ollama server)
#' provider <- vlm_provider("ollama", model = "gemma3:27b")
#' }
vlm_provider <- function(name = c("anthropic", "openai", "google", "ollama"),
                          model = NULL, ...) {

  name  <- match.arg(name)
  model <- model %||% default_model(name)

  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'ellmer' is required for vlm_provider() but is not ",
      "installed. Install it with install.packages('ellmer')."
    ))
  }

  switch(name,
    anthropic = ellmer::chat_anthropic(model = model, ...),
    openai    = ellmer::chat_openai(   model = model, ...),
    google    = ellmer::chat_google_gemini(model = model, ...),
    ollama    = ellmer::chat_ollama(   model = model, ...)
  )
}
