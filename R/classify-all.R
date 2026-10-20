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
#' The systems and their aliases come from \code{\link{soilkey_systems}}.
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
#' @param systems Character vector. Any subset of the registered systems
#'        (\code{"wrb2022"}, \code{"sibcs"}, \code{"usda"}; ids and aliases
#'        as listed by \code{\link{soilkey_systems}}), or the literal
#'        \code{"all"} (default) to run every default system.
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}. Forwarded verbatim to each classifier.
#' @param include_familia Forwarded to \code{\link{classify_sibcs}} (default
#'        \code{TRUE}). Has no effect on the other systems.
#' @param include_family Forwarded to \code{\link{classify_usda}} (default
#'        \code{FALSE}) to derive the USDA 5th-level family. No effect on the
#'        other systems.
#' @param specifiers Forwarded to \code{\link{classify_wrb2022}} (default
#'        \code{FALSE}) to auto-attach WRB depth specifiers. No effect on the
#'        other systems.
#' @param gapfill Forwarded to all three classifiers (default \code{FALSE} =>
#'        byte-identical). Opt-in within-pedon depth gap-fill; see
#'        \code{\link{gapfill_within_pedon}}. Applied independently per system
#'        on a deep copy, so the caller's pedon is never mutated.
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
#' pr <- make_ferralsol_canonical()
#' all_three <- classify_all(pr)
#' all_three$summary
#'
#' # WRB + USDA only (skip SiBCS):
#' classify_all(pr, systems = c("wrb2022", "usda"))$summary
#' @export
classify_all <- function(pedon,
                            systems        = "all",
                            on_missing     = c("warn", "silent", "error"),
                            include_familia = TRUE,
                            include_family = FALSE,
                            specifiers = FALSE,
                            gapfill    = FALSE,
                            ...) {
  on_missing <- match.arg(on_missing)

  if (length(systems) == 1L && identical(systems, "all"))
    systems <- .system_ids(default_only = TRUE)
  systems <- .system_resolve(systems, several.ok = TRUE)

  # One slot per default system (NULL when skipped), plus any other system
  # asked for, in registry order.
  run   <- .system_ids()[.system_ids() %in% systems]
  shown <- .system_ids()[.system_ids() %in%
                           union(.system_ids(default_only = TRUE), run)]
  slots <- vapply(shown, function(id) .system_get(id)$result_slot,
                  character(1), USE.NAMES = FALSE)
  out <- stats::setNames(vector("list", length(slots)), slots)

  env <- environment()
  for (id in run) {
    sys <- .system_get(id)
    # The call reads classify_<id>(pedon, on_missing = on_missing,
    # <option> = <option>, gapfill = gapfill), evaluated here, so each
    # classifier receives exactly the arguments it always did.
    opts <- sys$classify_all_options
    cl <- as.call(c(list(as.name(sys$classifier), quote(pedon),
                         on_missing = quote(on_missing)),
                    stats::setNames(lapply(unlist(opts), as.name), names(opts)),
                    list(gapfill = quote(gapfill))))
    out[[sys$result_slot]] <- tryCatch(
      eval(cl, envir = env),
      error = function(e) {
        warning(sprintf("%s failed: %s", sys$classifier, conditionMessage(e)),
                  call. = FALSE)
        NULL
      }
    )
  }

  pick_name <- function(res) if (is.null(res)) NA_character_
                              else res$name %||% NA_character_
  out$summary <- do.call(data.frame, c(
    stats::setNames(lapply(slots, function(s) pick_name(out[[s]])), slots),
    list(stringsAsFactors = FALSE, check.names = FALSE)
  ))

  out
}
