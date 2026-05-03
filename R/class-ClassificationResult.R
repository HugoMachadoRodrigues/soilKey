#' ClassificationResult: structured outcome of running a key
#'
#' Returned by \code{\link{classify_wrb2022}} (and the future
#' \code{classify_sibcs}). Carries the full decision trace — which RSGs
#' were tested, which passed, which failed, which were indeterminate
#' because of missing data — plus the assigned class, qualifiers,
#' ambiguities (RSGs that nearly satisfied), missing data that would
#' refine the result, the provenance-aware evidence grade, and any
#' biogeographical or prior-based warnings.
#'
#' @field system         Character. \code{"WRB 2022"} or \code{"SiBCS 5"}.
#' @field name           Character. Full classification name with
#'                       qualifiers (e.g.
#'                       \code{"Rhodic Ferralsol (Clayic, Humic, Dystric)"}).
#' @field rsg_or_order   Character. Bare RSG (WRB) or order (SiBCS), e.g.
#'                       \code{"Ferralsols"}.
#' @field qualifiers     List. Principal and supplementary qualifiers in
#'                       canonical order.
#' @field trace          List. One element per RSG tested (in key order),
#'                       each with \code{code}, \code{name}, \code{passed},
#'                       \code{evidence}, \code{missing}.
#' @field ambiguities    List. RSGs that came close to passing — useful
#'                       hints for follow-up measurements.
#' @field missing_data   Character vector. Attributes whose measurement
#'                       would refine or resolve the result.
#' @field evidence_grade Character. \code{"A"}, \code{"B"}, \code{"C"},
#'                       \code{"D"}, or \code{NA_character_}.
#' @field prior_check    List or NULL. Result of the spatial-prior sanity
#'                       check (consistent / inconsistent / not run).
#' @field warnings       Character vector. Free-form warnings.
#'
#' @export
ClassificationResult <- R6::R6Class("ClassificationResult",
  public = list(

    system         = NULL,
    name           = NULL,
    rsg_or_order   = NULL,
    qualifiers     = NULL,
    trace          = NULL,
    ambiguities    = NULL,
    missing_data   = NULL,
    evidence_grade = NULL,
    prior_check    = NULL,
    warnings       = NULL,

    #' @description Build a ClassificationResult.
    #' @param system System name.
    #' @param name Classification name.
    #' @param rsg_or_order RSG (WRB) or order (SiBCS).
    #' @param qualifiers List of qualifier names.
    #' @param trace List of per-RSG test entries.
    #' @param ambiguities List of close-call RSGs.
    #' @param missing_data Character vector.
    #' @param evidence_grade Single character A/B/C/D or NA.
    #' @param prior_check List or NULL.
    #' @param warnings Character vector.
    initialize = function(system,
                          name,
                          rsg_or_order   = NA_character_,
                          qualifiers     = list(),
                          trace          = list(),
                          ambiguities    = list(),
                          missing_data   = character(0),
                          evidence_grade = NA_character_,
                          prior_check    = NULL,
                          warnings       = character(0)) {
      self$system         <- system
      self$name           <- name
      self$rsg_or_order   <- rsg_or_order
      self$qualifiers     <- qualifiers
      self$trace          <- trace
      self$ambiguities    <- ambiguities
      self$missing_data   <- as.character(missing_data)
      self$evidence_grade <- evidence_grade
      self$prior_check    <- prior_check
      self$warnings       <- as.character(warnings)
    },

    #' @description Pretty-print the result with key trace, ambiguities, and
    #'              warnings.
    #' @param ... Ignored (S3 print signature compatibility).
    print = function(...) {
      cli::cli_h2(sprintf("ClassificationResult (%s)", self$system))
      cli::cli_text("Name: {self$name}")
      if (!is.na(self$rsg_or_order)) {
        cli::cli_text("RSG/Order: {self$rsg_or_order}")
      }
      if (length(self$qualifiers) > 0) {
        cli::cli_text("Qualifiers: {paste(unlist(self$qualifiers), collapse = ', ')}")
      }
      if (!is.na(self$evidence_grade)) {
        cli::cli_text("Evidence grade: {self$evidence_grade}")
      }
      if (!is.null(self$prior_check)) {
        cli::cli_text("Prior check: {self$prior_check$status %||% 'not run'}")
      }
      if (length(self$ambiguities) > 0) {
        cli::cli_h3("Ambiguities")
        for (a in self$ambiguities) {
          cli::cli_text(sprintf("  - %s: %s",
                                 a$rsg_code %||% "?",
                                 a$reason %||% ""))
        }
      }
      if (length(self$missing_data) > 0) {
        cli::cli_h3("Missing data that would refine result")
        cli::cli_text(paste(self$missing_data, collapse = ", "))
      }
      if (length(self$warnings) > 0) {
        cli::cli_h3("Warnings")
        for (w in self$warnings) cli::cli_alert_warning(w)
      }
      cli::cli_h3("Key trace")
      n_tested <- length(self$trace)
      cli::cli_text("({n_tested} RSGs tested before assignment)")
      for (i in seq_along(self$trace)) {
        t <- self$trace[[i]]
        sym <- if (isTRUE(t$passed)) "PASSED"
               else if (isFALSE(t$passed)) "failed"
               else "NA"
        n_missing <- length(t$missing %||% character(0))
        suffix <- if (n_missing > 0L && !isTRUE(t$passed)) {
          sprintf(" (%d attrs missing)", n_missing)
        } else ""
        cli::cli_text(sprintf("  %2d. %-3s %-14s -- %s%s",
                               i,
                               t$code %||% "??",
                               t$name %||% "",
                               sym,
                               suffix))
      }
      invisible(self)
    },

    #' @description Compact summary list.
    #' @param ... Ignored (S3 summary signature compatibility).
    summary = function(...) {
      list(
        system         = self$system,
        name           = self$name,
        rsg_or_order   = self$rsg_or_order,
        evidence_grade = self$evidence_grade,
        n_trace_steps  = length(self$trace),
        n_ambiguities  = length(self$ambiguities),
        n_missing      = length(self$missing_data),
        n_warnings     = length(self$warnings)
      )
    },

    #' @description Render this classification as a self-contained
    #'              report (delegates to the package-level
    #'              \code{\link{report}} generic). HTML output is
    #'              dependency-free; PDF requires \code{rmarkdown}
    #'              and a working LaTeX engine.
    #' @param file Output path. Format is inferred from the
    #'             extension.
    #' @param format One of "html" or "pdf" (defaults to "auto",
    #'               which infers from the extension).
    #' @param pedon Optional \code{PedonRecord} whose horizons /
    #'              provenance are added to the report.
    #' @param ... Forwarded to \code{\link{report_html}} or
    #'            \code{\link{report_pdf}}.
    report = function(file,
                      format = c("auto", "html", "pdf"),
                      pedon  = NULL,
                      ...) {
      format <- match.arg(format)
      report(self, file = file, format = format,
              pedon = pedon, ...)
    }
  )
)
