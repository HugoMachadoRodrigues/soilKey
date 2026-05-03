# =============================================================================
# v0.9.28 -- classify_all(): single-call wrapper across the three systems.
#
# Saves callers from typing three separate classify_*() calls when they want
# the multi-system view. Parallel-friendly: the three classifiers are
# independent (they share the PedonRecord but neither writes back to it),
# so future versions could parallelise via `future` if profiling shows it
# helps.
# =============================================================================


#' Classify a pedon across all three taxonomic systems
#'
#' Convenience wrapper that runs \code{\link{classify_wrb2022}},
#' \code{\link{classify_sibcs}}, and \code{\link{classify_usda}} on the same
#' \code{\link{PedonRecord}} and returns a single named list with one entry
#' per system (plus a \code{summary} table that's handy for reports).
#'
#' Each classifier still produces its own \code{\link{ClassificationResult}}
#' with the full key trace and evidence grade -- nothing is collapsed or
#' homogenised. The wrapper exists for ergonomics, not abstraction.
#'
#' @section Selecting a subset of systems:
#'
#' Pass \code{systems = c("wrb2022", "sibcs")} (or any other subset) to skip
#' systems you don't need. Default \code{systems = "all"} runs all three.
#'
#' @section Errors and partial results:
#'
#' If a single classifier raises an error, the corresponding slot of the
#' returned list is set to \code{NULL} and a one-line warning is emitted (so
#' you can rerun the offender on its own to see the full traceback). The
#' other classifiers still run and their results are returned. This matches
#' the spirit of \code{on_missing = "warn"} on the individual classifiers.
#'
#' @section Side effects:
#'
#' None. The classifiers do not mutate \code{pedon}; the wrapper does not
#' attach any side-channel state.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param systems Character vector. Any subset of \code{c("wrb2022",
#'        "sibcs", "usda")}, or the literal \code{"all"} (default) to run
#'        every system.
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}. Forwarded verbatim to each classifier.
#' @param include_familia Forwarded to \code{\link{classify_sibcs}} (default
#'        \code{TRUE}). Has no effect on the other systems.
#' @param ... Additional named arguments are silently ignored.
#' @return A named list with elements:
#'   \itemize{
#'     \item \code{wrb} -- \code{\link{ClassificationResult}} from
#'           \code{classify_wrb2022()} (or \code{NULL} if the system was
#'           skipped or errored).
#'     \item \code{sibcs} -- as above, from \code{classify_sibcs()}.
#'     \item \code{usda} -- as above, from \code{classify_usda()}.
#'     \item \code{summary} -- a 1-row \code{data.frame} with one column
#'           per system, holding the resulting \code{$name} (or \code{NA}
#'           when the system was skipped / errored). Useful for tabulating
#'           many pedons in one shot.
#'   }
#' @seealso \code{\link{classify_wrb2022}}, \code{\link{classify_sibcs}},
#'   \code{\link{classify_usda}}.
#' @examples
#' \dontrun{
#' pr <- make_ferralsol_canonical()
#' all_three <- classify_all(pr)
#' all_three$summary
#' #>                              wrb                                       sibcs
#' #> 1 Geric Ferric Rhodic Chromic ... Latossolos Vermelhos Distroficos tipicos...
#' #>             usda
#' #> 1 Rhodic Hapludox
#'
#' # WRB + USDA only (skip SiBCS):
#' classify_all(pr, systems = c("wrb2022", "usda"))$summary
#' }
#' @export
classify_all <- function(pedon,
                            systems        = "all",
                            on_missing     = c("warn", "silent", "error"),
                            include_familia = TRUE,
                            ...) {
  on_missing <- match.arg(on_missing)

  if (length(systems) == 1L && identical(systems, "all"))
    systems <- c("wrb2022", "sibcs", "usda")
  systems <- match.arg(systems, c("wrb2022", "sibcs", "usda"),
                          several.ok = TRUE)

  out <- list(wrb = NULL, sibcs = NULL, usda = NULL)

  if ("wrb2022" %in% systems) {
    out$wrb <- tryCatch(
      classify_wrb2022(pedon, on_missing = on_missing),
      error = function(e) {
        warning(sprintf("classify_wrb2022 failed: %s", conditionMessage(e)),
                  call. = FALSE)
        NULL
      }
    )
  }
  if ("sibcs" %in% systems) {
    out$sibcs <- tryCatch(
      classify_sibcs(pedon, on_missing = on_missing,
                       include_familia = include_familia),
      error = function(e) {
        warning(sprintf("classify_sibcs failed: %s", conditionMessage(e)),
                  call. = FALSE)
        NULL
      }
    )
  }
  if ("usda" %in% systems) {
    out$usda <- tryCatch(
      classify_usda(pedon, on_missing = on_missing),
      error = function(e) {
        warning(sprintf("classify_usda failed: %s", conditionMessage(e)),
                  call. = FALSE)
        NULL
      }
    )
  }

  pick_name <- function(res) if (is.null(res)) NA_character_
                              else res$name %||% NA_character_
  out$summary <- data.frame(
    wrb   = pick_name(out$wrb),
    sibcs = pick_name(out$sibcs),
    usda  = pick_name(out$usda),
    stringsAsFactors = FALSE
  )

  out
}
