# ================================================================
# Module 3 -- Prior sanity check
#
# Compares the deterministic key's assignment against the spatial
# prior's distribution. This NEVER overrides the key -- it only
# annotates the ClassificationResult with a `prior_check` entry that
# downstream consumers (the print method, reports, the user) can use
# to flag implausible classifications.
#
# Decision logic (consistent with ARCHITECTURE.md sec. 8.2):
#   - If the assigned RSG has p >= threshold (default 0.01) in the
#     prior, the assignment is considered "consistent" with the
#     local soil landscape.
#   - If p < threshold (or the RSG is wholly absent), we mark
#     "inconsistent" and emit a warning. The classic example is a
#     Cryosol returned for a tropical site.
#   - If the prior is empty (no pixels in buffer, or extraction
#     failed), we mark "no_data" rather than fabricating a verdict.
# ================================================================


#' Check consistency between a deterministic RSG assignment and a
#' spatial prior
#'
#' Returns a list describing whether the assigned RSG is plausible
#' under the given prior. The deterministic classification is never
#' overridden -- this is purely a sanity-check signal.
#'
#' @param rsg_code Two-letter RSG code (e.g. \code{"FR"}). Either the
#'        \code{rsg_or_order} from a \code{\link{ClassificationResult}}
#'        (in which case it must be the RSG name; we try to translate
#'        via the trace) or the bare code from a key trace entry.
#' @param prior A spatial-prior data.table from
#'        \code{\link{spatial_prior}}.
#' @param threshold Probability below which an assignment is flagged
#'        inconsistent (default 0.01).
#' @return A list with elements:
#'   \itemize{
#'     \item \code{consistent}: \code{TRUE} / \code{FALSE} / \code{NA}.
#'     \item \code{p}: probability of the assigned RSG in the prior
#'           (or \code{NA_real_} if not found).
#'     \item \code{threshold}: the threshold used.
#'     \item \code{status}: a short status string -- \code{"consistent"},
#'           \code{"inconsistent"}, or \code{"no_data"}.
#'     \item \code{note}: human-readable explanation.
#'     \item \code{top_prior}: \code{data.table} with the top three
#'           classes from the prior (for messages).
#'   }
#' @export
prior_consistency_check <- function(rsg_code, prior, threshold = 0.01) {
  prior <- normalize_prior(prior)

  out <- list(
    consistent = NA,
    p          = NA_real_,
    threshold  = threshold,
    status     = "no_data",
    note       = "Prior is empty; cannot evaluate consistency.",
    top_prior  = data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    )
  )

  if (is.null(prior) || nrow(prior) == 0L) {
    return(out)
  }

  out$top_prior <- prior[seq_len(min(3L, nrow(prior))), ]

  if (is.null(rsg_code) || length(rsg_code) != 1L || is.na(rsg_code) ||
      !nzchar(rsg_code)) {
    out$status <- "no_data"
    out$note   <- "rsg_code is missing; cannot evaluate consistency."
    return(out)
  }

  # NB: inside data.table's `[.data.table` the argument name `rsg_code`
  # shadows the column of the same name. Use vector subsetting instead.
  hit_idx <- which(prior$rsg_code == rsg_code)
  p   <- if (length(hit_idx) > 0L) prior$probability[hit_idx[1]] else 0
  out$p <- p

  if (p >= threshold) {
    out$consistent <- TRUE
    out$status     <- "consistent"
    out$note       <- sprintf(
      "Assigned RSG '%s' has prior probability %.3f at this location (>= threshold %.3f).",
      rsg_code, p, threshold
    )
  } else {
    out$consistent <- FALSE
    out$status     <- "inconsistent"
    out$note       <- sprintf(
      "Assigned RSG '%s' has prior probability %.3f at this location (< threshold %.3f). Top prior classes: %s. The deterministic classification is NOT overridden but the result is biogeographically unusual; verify the input data and site coordinates.",
      rsg_code, p, threshold,
      paste(sprintf("%s=%.2f", out$top_prior$rsg_code, out$top_prior$probability),
            collapse = ", ")
    )
  }
  out
}


#' Resolve the assigned RSG code from a ClassificationResult
#'
#' Walks the trace looking for the entry whose name matches
#' \code{rsg_or_order} and returns its \code{code}. Used internally by
#' \code{\link{classify_wrb2022}} to wire the prior check.
#'
#' @keywords internal
resolve_assigned_rsg_code <- function(result) {
  trace <- result$trace
  if (length(trace) == 0L) return(NA_character_)
  for (entry in trace) {
    if (isTRUE(entry$passed) &&
        identical(entry$name, result$rsg_or_order)) {
      return(entry$code %||% NA_character_)
    }
  }
  # fallback: the last passed entry, or the last entry if none passed
  passed_entries <- Filter(function(e) isTRUE(e$passed), trace)
  if (length(passed_entries) > 0L) {
    return(passed_entries[[length(passed_entries)]]$code %||% NA_character_)
  }
  trace[[length(trace)]]$code %||% NA_character_
}
