# ================================================================
# Module 2 -- mock provider for offline / CI testing
#
# A drop-in stand-in for an ellmer chat object that does not call any
# real model. It returns canned responses from a queue and -- crucially
# for testing the retry path -- can be configured to return an
# intentionally invalid response on a specific attempt before falling
# back to a valid one.
#
# Tests in tests/testthat/test-vlm-extract.R use this exclusively, so
# the test suite never depends on API keys or network access.
# ================================================================


#' Mock VLM provider for testing
#'
#' A stand-in for an \code{ellmer} chat object. Exposes the same
#' \code{$chat(prompt, ...)} method, but instead of calling a model
#' it pops the next response from a pre-loaded queue. Designed for
#' \pkg{testthat} unit tests that exercise extraction logic without
#' API keys or network access.
#'
#' Each call to \code{$chat()} returns the next element of the
#' \code{responses} list. If the call number matches
#' \code{validation_error_at}, that response is replaced with a
#' deliberately malformed JSON string, allowing tests to exercise the
#' retry-on-validation-failure path implemented in
#' \code{\link{validate_or_retry}}.
#'
#' @section Example:
#' \preformatted{
#' good_json <- '{"horizons": [...]}'
#' mock <- MockVLMProvider$new(responses = list(good_json))
#' result <- mock$chat("any prompt")  # returns good_json
#'
#' # Simulate one validation error before success.
#' mock <- MockVLMProvider$new(
#'   responses = list("not really json", good_json),
#'   validation_error_at = NULL  # already invalid as-is
#' )
#'
#' # Or force an attempt to be invalid via the helper.
#' mock <- MockVLMProvider$new(
#'   responses = list(good_json, good_json),
#'   validation_error_at = 1L
#' )
#' }
#'
#' @section Inspection:
#' After use, the mock exposes \code{$call_count} (integer) and
#' \code{$prompts_received} (list of every prompt passed to
#' \code{$chat()}), which lets tests assert that retry prompts include
#' the previous validation error.
#'
#' @field responses    List of canned responses (character scalars or
#'                     R objects to be JSON-serialised).
#' @field validation_error_at Optional integer; when the call number
#'                     matches, the returned text is replaced with a
#'                     malformed JSON string.
#' @field call_count   Integer counter (0 before any call).
#' @field prompts_received List recording every prompt passed to
#'                     \code{$chat()}.
#'
#' @keywords internal
#' @export
MockVLMProvider <- R6::R6Class("MockVLMProvider",
  public = list(

    responses           = NULL,
    validation_error_at = NULL,
    call_count          = 0L,
    prompts_received    = NULL,

    #' @description Construct a mock provider.
    #' @param responses List of canned responses. Strings are returned
    #'        verbatim; non-string elements are JSON-serialised via
    #'        \code{jsonlite::toJSON}.
    #' @param validation_error_at Optional integer giving the 1-based
    #'        index of an attempt that should return malformed JSON
    #'        (to test the retry path). Use \code{NULL} (default) to
    #'        always return the queued response unchanged.
    initialize = function(responses           = list(),
                          validation_error_at = NULL) {
      if (!is.list(responses)) {
        rlang::abort("`responses` must be a list")
      }
      self$responses           <- responses
      self$validation_error_at <- if (is.null(validation_error_at)) {
                                     NULL
                                  } else {
                                     as.integer(validation_error_at)
                                  }
      self$call_count       <- 0L
      self$prompts_received <- list()
    },

    #' @description Send a prompt; returns the next queued response.
    #' @param prompt Character scalar (the rendered prompt). Stored in
    #'        \code{$prompts_received}.
    #' @param ... Additional arguments are accepted (and ignored) so
    #'        the signature matches multimodal calls that pass an
    #'        image content object after the prompt.
    #' @return Character scalar with the response text.
    chat = function(prompt, ...) {
      self$call_count       <- self$call_count + 1L
      self$prompts_received[[self$call_count]] <- prompt

      idx <- self$call_count
      if (idx > length(self$responses)) {
        rlang::abort(sprintf(
          "MockVLMProvider exhausted: %d calls, only %d responses queued",
          idx, length(self$responses)
        ))
      }

      if (!is.null(self$validation_error_at) &&
          idx == self$validation_error_at) {
        # Deliberately malformed JSON so the retry path engages.
        return("{ this is not valid json !!! ")
      }

      resp <- self$responses[[idx]]
      if (is.character(resp)) {
        return(resp)
      }
      jsonlite::toJSON(resp, auto_unbox = TRUE, null = "null", na = "null")
    },

    #' @description Reset the mock (call count and prompt log).
    reset = function() {
      self$call_count       <- 0L
      self$prompts_received <- list()
      invisible(self)
    }
  )
)
