# =============================================================================
# v0.9.70 -- ellmer structured-output bridge.
#
# When a provider supports structured outputs (Anthropic tool calls,
# OpenAI response_format = json_schema, Ollama 0.5+ format = json_schema,
# Gemini structured output), we can ask the model to emit JSON that is
# *guaranteed* to validate against our soilKey schemas. That removes
# the entire class of "model returned prose / wrong shape" errors that
# the v0.9.66/v0.9.68 retry loop was working around.
#
# ellmer 0.3+ exposes a uniform API for this:
#
#   chat$chat_structured(prompt, type = type_object(...))
#
# `type_from_schema()` (also ellmer) reads a JSON schema file and
# returns the matching ellmer type tree. This lets us reuse our
# existing inst/schemas/*.json verbatim.
# =============================================================================


#' ellmer type tree for a soilKey extraction schema
#'
#' Reads `inst/schemas/<name>.json` and converts it to an ellmer
#' `type_object()` via `ellmer::type_from_schema()`. Cached per call
#' (lightweight; the schema files are < 5 KB each).
#'
#' Used by [validate_or_retry()] when `use_structured = TRUE`: instead
#' of calling `provider$chat()` and parsing JSON manually, the
#' provider gets called via `chat_structured(prompt, type = <this>)`
#' and returns an R list whose shape is provider-validated.
#'
#' @param name Schema base name -- one of `"horizon"`, `"site"`,
#'   `"pedon-schema"`. Without `.json`.
#'
#' @return An ellmer type object (class inheriting from
#'   `ellmer::Type`).
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("ellmer", quietly = TRUE)) {
#'   t <- vlm_type_from_soilkey_schema("horizon")
#'   t  # prints the type tree
#' }
#' }
#' @seealso [validate_or_retry()] (which uses this when `use_structured = TRUE`),
#'   [`ellmer::type_from_schema`].
#' @export
vlm_type_from_soilkey_schema <- function(name) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    rlang::abort("'name' must be a non-empty character scalar.")
  }
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    rlang::abort(paste0(
      "ellmer is required for structured-output extraction. Install ",
      "with install.packages('ellmer')."
    ))
  }
  if (!exists("type_from_schema", envir = asNamespace("ellmer"),
                inherits = FALSE)) {
    rlang::abort(paste0(
      "Your ellmer version does not export type_from_schema(). ",
      "Install ellmer >= 0.3.0 with ",
      "remotes::install_github('tidyverse/ellmer')."
    ))
  }
  schema_file <- system.file("schemas", paste0(name, ".json"),
                                package = "soilKey")
  if (!nzchar(schema_file) || !file.exists(schema_file)) {
    schema_file <- file.path("inst", "schemas", paste0(name, ".json"))
  }
  if (!file.exists(schema_file)) {
    rlang::abort(sprintf("Schema not found: %s.json", name))
  }
  ellmer::type_from_schema(path = schema_file)
}


#' Does a provider support `chat_structured()`?
#'
#' Quick capability probe. Returns TRUE when the provider exposes a
#' `chat_structured` method (ellmer Chat object built for an LLM that
#' supports structured outputs). Used internally by
#' [validate_or_retry()] to decide whether the structured-output path
#' is available.
#'
#' Mock providers and any non-ellmer chat objects return FALSE here,
#' so the structured-output flag degrades gracefully to the legacy
#' chat-and-validate loop.
#'
#' @param provider The provider to probe.
#' @return Logical scalar.
#' @keywords internal
.provider_supports_structured <- function(provider) {
  if (is.null(provider)) return(FALSE)
  # ellmer Chat objects expose chat_structured as a method via R6.
  has_method <- tryCatch(
    is.function(provider$chat_structured),
    error = function(e) FALSE
  )
  isTRUE(has_method)
}
