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
#' These defaults are picked for **vision capability** (multimodal)
#' AND **structured-extraction reliability** -- the two things the
#' soilKey extraction layer needs.
#'
#' Defaults (as of v0.9.11):
#' \itemize{
#'   \item \code{anthropic = "claude-sonnet-4-7"} -- the strongest
#'         Claude vision model at our 2026 cutoff for document /
#'         photo extraction.
#'   \item \code{openai = "gpt-4o"} -- text + vision.
#'   \item \code{google = "gemini-2.0-pro"} -- successor to 1.5
#'         with longer context + better multimodal grounding.
#'   \item \code{ollama = "gemma4:e4b"} -- Gemma 4 edge
#'         multimodal (text + image; audio also). For larger
#'         contexts use \code{"gemma4:31b"}; for cloud-only
#'         offload via Ollama, \code{"gemma4-cloud:31b"}. Pull the
#'         desired size first with \code{ollama pull gemma4:e4b}.
#' }
#'
#' Users can override at any time:
#' \preformatted{
#' vlm_provider("ollama", model = "gemma4:31b")
#' vlm_provider("ollama", model = "gemma3:27b")  # back-compat
#' vlm_provider("ollama", model = "qwen2.5vl:32b")  # any pulled model
#' }
#'
#' @param name Provider name; one of \code{"anthropic"}, \code{"openai"},
#'        \code{"google"}, \code{"ollama"}.
#' @return Character scalar with the default model identifier.
#' @keywords internal
default_model <- function(name) {
  name <- match.arg(name, c("auto", "anthropic", "openai", "google", "ollama"))
  if (identical(name, "auto")) name <- vlm_pick_provider(verbose = FALSE)
  switch(name,
    anthropic = "claude-sonnet-4-7",
    openai    = "gpt-4o",
    google    = "gemini-2.0-pro",
    ollama    = "gemma4:e4b"
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
#' an Ollama server (default \code{gemma4:e4b}, Gemma 4 edge with
#' multimodal text+image+audio support). No data leaves the
#' machine, which is the recommended setting for sensitive field
#' descriptions (e.g. governmental surveys, indigenous land studies)
#' where institutional independence and data sovereignty matter.
#' Pull the model first:
#' \preformatted{
#'   ollama pull gemma4:e4b      # ~3 GB edge variant (default)
#'   ollama pull gemma4:31b      # frontier dense variant
#'   ollama pull gemma3:27b      # earlier generation, still solid
#' }
#' Then start an Ollama server (\code{ollama serve}) and the chat
#' object returned here will dispatch over HTTP locally.
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
#' # Local Gemma 4 edge model -- default, ~3 GB, runs anywhere
#' provider <- vlm_provider("ollama")
#'
#' # Local Gemma 4 frontier dense model -- best quality
#' provider <- vlm_provider("ollama", model = "gemma4:31b")
#'
#' # Any other multimodal model the user has pulled
#' provider <- vlm_provider("ollama", model = "qwen2.5vl:32b")
#' }
vlm_provider <- function(name = c("auto", "anthropic", "openai", "google", "ollama"),
                          model = NULL, ...) {

  name  <- match.arg(name)

  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(paste0(
      "Package 'ellmer' is required for vlm_provider() but is not ",
      "installed. Install it with install.packages('ellmer')."
    ))
  }

  if (identical(name, "auto")) {
    name <- vlm_pick_provider(verbose = TRUE)
  }
  model <- model %||% default_model(name)

  switch(name,
    anthropic = ellmer::chat_anthropic(model = model, ...),
    openai    = ellmer::chat_openai(   model = model, ...),
    google    = ellmer::chat_google_gemini(model = model, ...),
    ollama    = ellmer::chat_ollama(   model = model, ...)
  )
}


#' Pick the best available VLM provider
#'
#' Selects a provider based on what is reachable in the user's
#' environment, in this preference order: local Ollama (if
#' \code{ollama_is_running()}), then Anthropic, OpenAI, and Google
#' (each requires the relevant \code{*_API_KEY} environment variable).
#' Errors with an actionable installation / API-key hint when no
#' provider is reachable.
#'
#' @param verbose If \code{TRUE} (default), emits a one-line
#'        \code{cli} message explaining the chosen provider.
#' @return Character scalar: one of \code{"ollama"}, \code{"anthropic"},
#'         \code{"openai"}, \code{"google"}.
#' @export
vlm_pick_provider <- function(verbose = TRUE) {
  if (ollama_is_running()) {
    if (verbose)
      cli::cli_alert_info("VLM provider {.field auto}: using local Ollama (preferred -- no data leaves the machine).")
    return("ollama")
  }
  has_key <- function(env) nzchar(Sys.getenv(env, ""))
  if (has_key("ANTHROPIC_API_KEY")) {
    if (verbose)
      cli::cli_alert_info("VLM provider {.field auto}: Ollama not reachable; falling back to Anthropic ({.field ANTHROPIC_API_KEY} detected).")
    return("anthropic")
  }
  if (has_key("OPENAI_API_KEY")) {
    if (verbose)
      cli::cli_alert_info("VLM provider {.field auto}: Ollama not reachable; falling back to OpenAI ({.field OPENAI_API_KEY} detected).")
    return("openai")
  }
  if (has_key("GOOGLE_API_KEY") || has_key("GEMINI_API_KEY")) {
    if (verbose)
      cli::cli_alert_info("VLM provider {.field auto}: Ollama not reachable; falling back to Google Gemini.")
    return("google")
  }
  rlang::abort(c(
    "No VLM provider is reachable.",
    i = "To run a fully local pipeline, install and start Ollama:",
    " " = "  https://ollama.com  -- then  ollama pull gemma4:e4b && ollama serve",
    i = "Or set one of the cloud-provider API keys:",
    " " = "  Sys.setenv(ANTHROPIC_API_KEY = 'sk-...')   # Claude",
    " " = "  Sys.setenv(OPENAI_API_KEY    = 'sk-...')   # GPT-4o",
    " " = "  Sys.setenv(GOOGLE_API_KEY    = '...')      # Gemini"
  ))
}


#' Is the local Ollama HTTP API reachable?
#'
#' Probes \code{http://127.0.0.1:11434/api/tags} (the standard Ollama
#' endpoint) with a short HTTP HEAD-style GET. Returns \code{TRUE}
#' only if the request returns HTTP 200 in under \code{timeout_s}
#' seconds. Used by \code{\link{vlm_pick_provider}} for the
#' \code{provider = "auto"} fallback chain. Override the URL via
#' \code{options(soilKey.ollama_url = "http://host:port")}.
#'
#' @param url Override URL to probe (default reads
#'        \code{getOption("soilKey.ollama_url",
#'        default = "http://127.0.0.1:11434/api/tags")}).
#' @param timeout_s Request timeout in seconds (default 1.5).
#' @return Logical scalar.
#' @export
ollama_is_running <- function(url = NULL, timeout_s = 1.5) {
  url <- url %||% getOption("soilKey.ollama_url",
                              default = "http://127.0.0.1:11434/api/tags")
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(FALSE)
  }
  ok <- tryCatch({
    resp <- httr::GET(url, httr::timeout(timeout_s))
    isTRUE(httr::status_code(resp) == 200L)
  }, error = function(e) FALSE)
  isTRUE(ok)
}
