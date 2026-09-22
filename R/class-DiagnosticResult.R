#' DiagnosticResult: structured outcome of a diagnostic test
#'
#' Returned by every WRB or SiBCS diagnostic function (e.g.
#' \code{\link{argic}}, \code{\link{ferralic}}, \code{\link{mollic}}). A
#' DiagnosticResult never reduces to a bare TRUE/FALSE — it always carries
#' (a) which layers satisfied the criteria, (b) the per-sub-test evidence,
#' (c) which attributes would have been required but are missing, and
#' (d) the literature reference for the diagnostic definition.
#'
#' \code{passed} is \code{TRUE}/\code{FALSE}/\code{NA}; \code{NA} means the
#' test could not be evaluated because critical attributes were missing.
#' This three-valued semantics propagates through the rule engine — an
#' indeterminate test does not silently fail.
#'
#' @field name        Character. Name of the diagnostic (e.g. \code{"argic"}).
#' @field passed      Logical. \code{TRUE}, \code{FALSE}, or \code{NA}.
#' @field layers      Integer vector. Indices of horizons that satisfy the
#'                    diagnostic.
#' @field evidence    Named list. Sub-test results, each itself a list with
#'                    at least \code{passed}, \code{layers}, and \code{missing}.
#' @field missing     Character vector. Attribute names that would have been
#'                    needed but were NA.
#' @field reference   Character. Literature citation for this diagnostic.
#' @field notes       Character. Free-form notes (interpretation choices,
#'                    edge cases hit).
#'
#' @export
DiagnosticResult <- R6::R6Class("DiagnosticResult",
  public = list(

    name      = NULL,
    passed    = NULL,
    layers    = NULL,
    evidence  = NULL,
    missing   = NULL,
    reference = NULL,
    notes     = NULL,

    #' @description Build a DiagnosticResult.
    #' @param name Diagnostic name.
    #' @param passed \code{TRUE}/\code{FALSE}/\code{NA}.
    #' @param layers Integer vector of horizon indices that satisfied.
    #' @param evidence Named list of sub-test results.
    #' @param missing Character vector of missing attribute names.
    #' @param reference Citation string.
    #' @param notes Free-form notes.
    initialize = function(name,
                          passed    = NA,
                          layers    = integer(0),
                          evidence  = list(),
                          missing   = character(0),
                          reference = NA_character_,
                          notes     = NA_character_) {
      self$name      <- name
      self$passed    <- passed
      self$layers    <- as.integer(layers)
      self$evidence  <- evidence
      self$missing   <- as.character(missing)
      self$reference <- reference
      self$notes     <- notes
    },

    #' @description Pretty-print the result with sub-test breakdown.
    #' @param ... Ignored (S3 print signature compatibility).
    print = function(...) {
      cli::cli_h3(sprintf("DiagnosticResult: %s", self$name))
      status <- if (isTRUE(self$passed)) {
        "PASSED"
      } else if (isFALSE(self$passed)) {
        "failed"
      } else {
        "NA (insufficient data)"
      }
      cli::cli_text("Status: {status}")
      if (length(self$layers) > 0) {
        cli::cli_text("Layers satisfying: {paste(self$layers, collapse = ', ')}")
      }
      if (length(self$missing) > 0) {
        cli::cli_text("Missing attributes ({length(self$missing)}): {paste(self$missing, collapse = ', ')}")
      }
      if (length(self$evidence) > 0) {
        cli::cli_text("Sub-tests:")
        for (n in names(self$evidence)) {
          subtest <- self$evidence[[n]]
          if (is.list(subtest) && "passed" %in% names(subtest)) {
            sym <- if (isTRUE(subtest$passed)) "[PASS]"
                   else if (isFALSE(subtest$passed)) "[fail]"
                   else "[ NA ]"
            cli::cli_text(sprintf("  %s %s", sym, n))
          }
        }
      }
      if (!is.na(self$reference)) {
        cli::cli_text("Reference: {self$reference}")
      }
      if (!is.na(self$notes)) {
        cli::cli_text("Notes: {self$notes}")
      }
      invisible(self)
    },

    #' @description Return the result as a plain list (for serialization).
    as_list = function() {
      list(
        name      = self$name,
        passed    = self$passed,
        layers    = self$layers,
        evidence  = self$evidence,
        missing   = self$missing,
        reference = self$reference,
        notes     = self$notes
      )
    }
  )
)
