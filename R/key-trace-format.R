# =============================================================================
# soilKey -- canonical key-trace flattener (v0.9.165).
#
# The decision trace on a ClassificationResult has a SYSTEM-DEPENDENT shape:
#
#   * WRB 2022  -- a FLAT list of reference-soil-group test steps, each a list
#                  list(code, name, passed, evidence, missing, notes).
#   * SiBCS 5 / -- a NESTED, named list of phases (ordens/subordens/grandes_
#     USDA ST13    grupos/subgrupos/familia/... ; orders/suborders/...). Each
#                  phase is heterogeneous: a group of candidate steps, an
#                  assigned-taxon record list(code, name, tests), a group of
#                  FamilyAttribute R6 objects, a bare atomic label
#                  (e.g. familia_label = "argilosa"), or NULL.
#
# Every consumer that walked the trace as a flat list of steps therefore
# either crashed on the atomic label ("$ operator is invalid for atomic
# vectors") or produced garbled "?? -- NA" rows for the nested systems. This
# helper normalises ANY of those shapes into one ordered data frame of display
# rows, so print(), the HTML/PDF reports and the Shiny app all render the trace
# the same way and none has to special-case a system.
# =============================================================================

#' Flatten a classification key trace into a tabular form
#'
#' Normalises the system-dependent decision trace carried by a
#' \code{\link{ClassificationResult}} into a single, ordered data frame of
#' display rows. WRB 2022 stores a flat list of reference-soil-group test
#' steps; the hierarchical SiBCS and USDA keys store a nested list of phases
#' (orders, suborders, great groups, subgroups, family, ...), each holding
#' candidate steps, an assigned-taxon record, family attributes, or a bare
#' label. This function walks all of those shapes and returns one row per step,
#' in the order the key was evaluated, so every consumer (\code{print()}, the
#' HTML and PDF reports, the Shiny app) can render the trace uniformly.
#'
#' @param x A \code{\link{ClassificationResult}}, or the \code{trace} list
#'   taken from one.
#' @return A \code{data.frame} with one row per trace step and columns:
#'   \describe{
#'     \item{\code{phase}}{Key phase / level the step belongs to (e.g.
#'       \code{"orders"}, \code{"subgrupos"}); empty for the flat WRB trace.}
#'     \item{\code{code}}{Taxon or attribute code.}
#'     \item{\code{name}}{Taxon or attribute name (or attribute value).}
#'     \item{\code{status}}{One of \code{"passed"}, \code{"failed"},
#'       \code{"indeterminate"} (a test that could not be evaluated for want of
#'       data), \code{"selected"} (the taxon assigned at a level), or
#'       \code{"info"} (a family attribute or label, not a pass/fail test).}
#'     \item{\code{missing}}{Comma-separated attributes that were missing for
#'       the step (empty when none).}
#'     \item{\code{n_missing}}{Integer count of missing attributes.}
#'   }
#'   A zero-row data frame with those columns when the trace is empty.
#' @examples
#' res <- classify_sibcs(make_ferralsol_canonical())
#' head(key_trace_table(res))
#' @export
key_trace_table <- function(x) {
  trace <- if (inherits(x, "ClassificationResult")) x$trace else x
  .flatten_key_trace(trace)
}

# Internal worker -- see file header for the shapes handled.
.flatten_key_trace <- function(trace) {
  empty <- data.frame(
    phase = character(0), code = character(0), name = character(0),
    status = character(0), missing = character(0), n_missing = integer(0),
    stringsAsFactors = FALSE)
  if (is.null(trace) || length(trace) == 0L) return(empty)

  rows <- list()

  as_disp <- function(v) {
    if (is.null(v) || length(v) == 0L) return("")
    paste(as.character(v), collapse = ", ")
  }
  clean_missing <- function(m) {
    m <- as.character(m %||% character(0))
    m[nzchar(m) & !is.na(m)]
  }
  emit <- function(phase, code, name, status, missing) {
    m <- clean_missing(missing)
    rows[[length(rows) + 1L]] <<- data.frame(
      phase = as_disp(phase), code = as_disp(code), name = as_disp(name),
      status = status, missing = paste(m, collapse = ", "),
      n_missing = length(m), stringsAsFactors = FALSE)
  }
  step_status <- function(t) {
    pv <- t$passed
    if (isTRUE(pv))            "passed"
    else if (isFALSE(pv))      "failed"
    else if (!is.null(t$tests)) "selected"   # an assigned-taxon record
    else                       "indeterminate"
  }

  # Recursively walk a node. `phase` is the level label it belongs to ("" for
  # the flat WRB trace). Leaves (atomic labels, R6 family attributes, step
  # lists, NULL) emit a row or nothing; a group of children recurses one level.
  walk <- function(node, phase) {
    if (length(phase) != 1L || is.na(phase)) phase <- ""
    if (is.null(node) || length(node) == 0L) return(invisible())
    if (is.atomic(node)) {                       # bare label (e.g. familia_label)
      emit(phase, "", node, "info", NULL)
      return(invisible())
    }
    if (inherits(node, "R6")) {                  # FamilyAttribute and friends
      emit(phase, node$code %||% node$name, node$value %||% node$name,
           "info", node$missing)
      return(invisible())
    }
    if (is.list(node) && !inherits(node, "data.frame")) {
      if (!is.null(node$code) && is.atomic(node$code)) {   # a single step / assigned record
        # The flat WRB trace is a list NAMED BY RSG code, so a top-level step
        # arrives with phase == its own code; that is not a level, so blank it.
        # SiBCS/USDA assigned records (phase e.g. "subordem_assigned" != code)
        # and grouped candidates keep their meaningful level label.
        code_disp <- as_disp(node$code)
        ph <- if (identical(phase, code_disp)) "" else phase
        emit(ph, node$code, node$name, step_status(node), node$missing)
      } else {                                             # a group of children
        nm <- names(node); if (is.null(nm)) nm <- rep("", length(node))
        for (j in seq_along(node)) {
          child_phase <- if (nzchar(phase)) phase else nm[[j]]
          walk(node[[j]], child_phase)
        }
      }
    }
    invisible()
  }

  top <- names(trace); if (is.null(top)) top <- rep("", length(trace))
  for (i in seq_along(trace)) walk(trace[[i]], top[[i]] %||% "")

  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
