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
#'                       \code{"Rhodic Ferralsol (Clayic, Dystric, Humic)"}).
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
#' @field evidence_grade Character. \code{"A"} (measured), \code{"B"}
#'                       (spectra-predicted), \code{"C"} (prior-inferred),
#'                       \code{"D"} (VLM-extracted), \code{"E"}
#'                       (user-assumed), or \code{NA_character_} when no
#'                       horizon carries a soil property (only depths and
#'                       designations; since v0.9.213), so nothing in the
#'                       class was verified.
#' @field prior_check    List or NULL. Result of the spatial-prior sanity
#'                       check (consistent / inconsistent / not run).
#' @field warnings       Character vector. Free-form warnings.
#' @field system_id      Character. Registry id of the system (see
#'                       \code{\link{soilkey_systems}}), e.g.
#'                       \code{"wrb2022"}; since v0.9.265.
#' @field levels         \code{data.frame} with one row per level of the
#'                       key that assigned a class (\code{level},
#'                       \code{code}, \code{name}), top level first; set by
#'                       the generic classifier of ordered keys and
#'                       \code{NULL} for WRB, SiBCS and USDA, whose levels
#'                       are in \code{trace}.
#' @field provisional_because Character vector. In a first-match key a class
#'                       that could not be decided for lack of data, tested
#'                       before the one assigned, makes the assignment
#'                       provisional; these are those classes, as
#'                       \code{"<level>:<code>"}. Empty when the assignment
#'                       is firm or the classifier does not report it.
#' @field phases         Soil phases of the systems that have them (the SCSJ
#'                       2017: temperature, man-made and forest phases), a
#'                       \code{data.frame} with \code{kind}, \code{code},
#'                       \code{name}, \code{name_native} and \code{basis};
#'                       \code{NULL} otherwise. Since v0.9.324.
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
    system_id      = NULL,
    levels         = NULL,
    provisional_because = NULL,
    phases         = NULL,

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
    #' @param system_id Registry id of the system, or NULL.
    #' @param levels Assigned class per level (data.frame), or NULL.
    #' @param provisional_because Character vector of undecided classes
    #'        tested before the assigned ones.
    initialize = function(system,
                          name,
                          rsg_or_order   = NA_character_,
                          qualifiers     = list(),
                          trace          = list(),
                          ambiguities    = list(),
                          missing_data   = character(0),
                          evidence_grade = NA_character_,
                          prior_check    = NULL,
                          warnings       = character(0),
                          system_id      = NULL,
                          levels         = NULL,
                          provisional_because = character(0)) {
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
      self$system_id      <- system_id
      self$levels         <- levels
      self$provisional_because <- as.character(provisional_because)
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
      if (is.data.frame(self$levels) && nrow(self$levels) > 1L) {
        cli::cli_text("Levels: {paste(sprintf('%s %s', self$levels$code,
                                                self$levels$name),
                                       collapse = ' > ')}")
      }
      if (length(self$provisional_because) > 0L) {
        cli::cli_text("Provisional: undecided before it, {paste(self$provisional_because, collapse = ', ')}")
      }
      if (length(self$qualifiers) > 0) {
        cli::cli_text("Qualifiers: {paste(unlist(self$qualifiers), collapse = ', ')}")
      }
      if (!is.na(self$evidence_grade)) {
        cli::cli_text("Evidence grade: {self$evidence_grade}")
      } else {
        cli::cli_text("Evidence grade: none (no soil property measured)")
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
      # v0.9.165: the trace is flat for WRB but nested for SiBCS/USDA (phases of
      # candidate steps, assigned records, family attributes, bare labels).
      # .flatten_key_trace() normalises every shape to one ordered table, so the
      # per-step dump no longer produces "?? -- NA" rows (or crashes) for the
      # hierarchical systems. WRB output is unchanged.
      tt <- .flatten_key_trace(self$trace)
      # v0.9.265: count the classes actually tested (for a nested trace,
      # length(trace) counted phases and assigned records), and call them
      # RSGs only for WRB.
      n_tested <- sum(tt$status %in% c("passed", "failed", "indeterminate"))
      tested_what <- if (identical(self$system_id, "wrb2022") ||
                         (is.null(self$system_id) &&
                          startsWith(self$system %||% "", "WRB"))) "RSGs"
                     else "classes"
      cli::cli_text("({n_tested} {tested_what} tested before assignment)")
      sym_of <- c(passed = "PASSED", failed = "failed",
                  indeterminate = "NA", selected = "assigned", info = "info")
      for (i in seq_len(nrow(tt))) {
        sym    <- sym_of[[tt$status[i]]]
        suffix <- if (tt$n_missing[i] > 0L &&
                      tt$status[i] %in% c("failed", "indeterminate"))
                    sprintf(" (%d attrs missing)", tt$n_missing[i]) else ""
        cli::cli_text(sprintf("  %2d. %-3s %-14s -- %s%s",
                              i,
                              if (nzchar(tt$code[i])) tt$code[i] else "??",
                              tt$name[i],
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
