# ================================================================
# Module 2 -- schema-validated extraction loop with retry
#
# Centralises the "call the model -> validate JSON -> on failure,
# show the error and retry" loop used by all extraction functions.
# Returns a parsed R list on success; aborts after `max_retries`
# attempts.
# ================================================================


#' Coerce a chat response to a single character scalar
#'
#' \code{ellmer} chat objects' \code{$chat()} method returns a
#' character vector (possibly with class attributes for ANSI). The
#' \code{\link{MockVLMProvider}} returns a plain string. This helper
#' normalises both shapes.
#'
#' @keywords internal
as_chat_text <- function(x) {
  if (is.character(x)) {
    paste(as.character(x), collapse = "")
  } else if (!is.null(attr(x, "text"))) {
    as.character(attr(x, "text"))
  } else {
    as.character(x)
  }
}


#' Strip surrounding code fences from a model response
#'
#' Some models wrap JSON output in \code{```json ... ```} fences
#' despite being told not to. This helper removes a single leading
#' and trailing fence pair if present, leaving the inner content.
#'
#' @keywords internal
strip_code_fence <- function(text) {
  text <- trimws(text)
  text <- sub("^```(?:json)?\\s*\\n?", "", text, perl = TRUE)
  text <- sub("\\n?```\\s*$", "", text, perl = TRUE)
  trimws(text)
}


#' Call a provider, validate JSON output, retry on failure
#'
#' Sends \code{prompt} to \code{provider}, parses the response as
#' JSON, and validates it against \code{schema} (a short schema name
#' resolved via \code{\link{load_schema}}). If validation fails, the
#' error message is appended to the prompt and the call is retried
#' up to \code{max_retries} times.
#'
#' On success, returns a list with the parsed JSON, the raw text, and
#' the number of attempts taken. On terminal failure, throws.
#'
#' This is the single place where the VLM-call -> validate -> retry
#' contract is implemented; every user-facing extractor delegates
#' here.
#'
#' @param provider An \code{ellmer} chat object (from
#'        \code{\link{vlm_provider}}) or a \code{\link{MockVLMProvider}}
#'        instance. Must expose a \code{$chat(prompt, ...)} method
#'        returning text (or a character vector of length 1).
#' @param prompt Character scalar with the initial prompt.
#' @param schema Short schema name (\code{"horizon"}, \code{"site"}).
#' @param max_retries Integer; total attempts will be at most
#'        \code{1 + max_retries}.
#' @param image Optional \code{ellmer} image content object (e.g.
#'        from \code{ellmer::content_image_file}) to pass alongside
#'        the prompt for multimodal calls.
#' @return A list with elements \code{data} (parsed R object),
#'         \code{raw} (character scalar), \code{attempts} (integer).
#' @keywords internal
validate_or_retry <- function(provider,
                                prompt,
                                schema,
                                max_retries = 3L,
                                image       = NULL) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    rlang::abort("Package 'jsonlite' is required for VLM extraction.")
  }

  if (is.null(provider) || !is.function(provider$chat)) {
    rlang::abort(paste0(
      "`provider` must expose a `$chat()` method (e.g. an ellmer chat ",
      "object or a MockVLMProvider)."
    ))
  }

  current_prompt <- prompt
  last_error     <- NULL
  attempts       <- 0L
  max_retries    <- as.integer(max_retries)
  total_calls    <- max(1L, 1L + max_retries)

  for (i in seq_len(total_calls)) {
    attempts <- i

    raw_response <- tryCatch(
      if (is.null(image)) provider$chat(current_prompt)
      else                provider$chat(current_prompt, image),
      error = function(e) {
        rlang::abort(sprintf(
          "VLM provider call failed on attempt %d: %s",
          i, conditionMessage(e)
        ))
      }
    )

    raw_text <- strip_code_fence(as_chat_text(raw_response))

    # 1. Must be parseable JSON.
    parsed <- tryCatch(
      jsonlite::fromJSON(raw_text, simplifyVector = FALSE),
      error = function(e) {
        last_error <<- sprintf("Response is not valid JSON: %s",
                                 conditionMessage(e))
        NULL
      }
    )
    if (is.null(parsed)) {
      current_prompt <- paste0(
        prompt,
        "\n\n---\nYour previous response failed validation:\n",
        last_error,
        "\nPlease return ONLY valid JSON conforming to the schema. ",
        "Do not wrap the JSON in code fences or add any prose."
      )
      next
    }

    # 2. Must conform to the schema.
    val <- validate_against_schema(raw_text, schema)
    if (val$valid) {
      return(list(data = parsed, raw = raw_text, attempts = i))
    }

    last_error <- paste(val$errors, collapse = "; ")
    current_prompt <- paste0(
      prompt,
      "\n\n---\nYour previous response failed schema validation:\n",
      last_error,
      "\nPlease correct the issues and return ONLY valid JSON conforming ",
      "to the schema."
    )
  }

  rlang::abort(sprintf(
    "VLM extraction failed after %d attempt(s). Last validation error: %s",
    attempts, last_error %||% "<unknown>"
  ))
}
