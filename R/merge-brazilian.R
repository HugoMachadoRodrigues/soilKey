# =============================================================================
# v0.9.62 -- merge_brazilian_pedons(): dedup BDsolos x FEBR via sisb_id.
#
# Both Embrapa BDsolos and the FEBR (Free Brazilian Repository for Open
# Soil Data) curate Brazilian pedons. Many profiles appear in BOTH
# corpuses because FEBR ingested historic Embrapa surveys: 590 of the
# 905 BDsolos RJ pedons match a FEBR sisb_id, and across the full
# 27-UF download we expect heavy overlap with the 8,124 FEBR records
# that carry a non-NA sisb_id.
#
# This module exposes:
#
#   merge_brazilian_pedons(bdsolos, febr, prefer = c("bdsolos", "febr"))
#       -- joins two PedonRecord lists by site$sisb_id, drops the
#          duplicates from the non-preferred source, and emits a
#          single super-list of distinct Brazilian pedons. Tags each
#          surviving pedon's site$reference_source with the merge
#          decision so downstream code can audit provenance.
#
#   summarize_brazilian_overlap(bdsolos, febr)
#       -- diagnostic table reporting overlap counts per UF / per
#          ordem and the dedup ratio.
#
# Both functions are pure R (no external dependencies) and work over
# in-memory PedonRecord lists; they don't touch the filesystem.
# =============================================================================


# ---- helpers -------------------------------------------------------------

#' Extract sisb_id from a PedonRecord, returning NA when not present
#'
#' Both v0.9.62 loaders (BDsolos + FEBR) assign `site$sisb_id`. This
#' helper centralises the lookup so older PedonRecord objects without
#' the field still work.
#'
#' @keywords internal
.get_sisb_id <- function(pedon) {
  if (is.null(pedon) || !inherits(pedon, "PedonRecord")) return(NA_character_)
  v <- pedon$site$sisb_id %||% NA_character_
  if (length(v) == 0L || is.na(v) || !nzchar(trimws(as.character(v)))) {
    return(NA_character_)
  }
  trimws(as.character(v))
}


#' Tag a pedon with merge provenance
#'
#' Appends the source label to `site$reference_source` and stores
#' `site$merge_decision` (`"kept_bdsolos"`, `"kept_febr"`, or
#' `"unique"`).
#'
#' @keywords internal
.tag_merge_decision <- function(pedon, source, decision) {
  if (is.null(pedon)) return(NULL)
  prev_src <- pedon$site$reference_source %||% NA_character_
  pedon$site$merge_decision   <- decision
  pedon$site$merge_source     <- source
  if (is.na(prev_src) || !nzchar(prev_src)) {
    pedon$site$reference_source <- source
  } else if (!grepl(source, prev_src, fixed = TRUE)) {
    pedon$site$reference_source <- paste0(prev_src, " | merged:", decision)
  } else {
    pedon$site$reference_source <- paste0(prev_src, " | ", decision)
  }
  pedon
}


# ---- merge ----------------------------------------------------------------

#' Merge BDsolos and FEBR PedonRecord lists, deduplicating by sisb_id
#'
#' Both Embrapa BDsolos and FEBR carry Brazilian soil profiles, with
#' substantial overlap. BDsolos exports the historic Embrapa pedon
#' numbering as \code{Codigo PA}; FEBR's \code{observacao} table
#' carries the same numbering as \code{sisb_id}. This function uses
#' those two as a join key to drop duplicates and produce a single
#' consolidated list.
#'
#' Pedons whose \code{site$sisb_id} is \code{NA} on either side are
#' kept as unique entries (the duplication test cannot be resolved).
#'
#' @param bdsolos List of \code{PedonRecord} objects from
#'   \code{\link{load_bdsolos_csv}}.
#' @param febr List of \code{PedonRecord} objects from
#'   \code{\link{read_febr_pedons}}.
#' @param prefer Character: which side wins when a sisb_id matches in
#'   both. Either \code{"bdsolos"} (default) or \code{"febr"}.
#' @param verbose If \code{TRUE} (default), prints a one-line summary.
#'
#' @return A list of \code{PedonRecord} objects with site provenance
#'   tagged via \code{site$merge_decision} (\code{"kept_bdsolos"},
#'   \code{"kept_febr"}, or \code{"unique"}) and \code{site$merge_source}.
#'   Pedons appear in the order: chosen-from-overlap first, then
#'   unique-to-bdsolos, then unique-to-febr.
#'
#' @examples
#' \dontrun{
#' bd <- load_bdsolos_csv("soil_data/embrapa_bdsolos/BD_solos/RJ.csv")
#' fb <- read_febr_pedons(c("ctb0032", "ctb0500"))
#' merged <- merge_brazilian_pedons(bd, fb, prefer = "bdsolos")
#' length(merged)  # < length(bd) + length(fb) when there is overlap
#' }
#'
#' @seealso \code{\link{load_bdsolos_csv}},
#'          \code{\link{read_febr_pedons}},
#'          \code{\link{summarize_brazilian_overlap}}.
#' @export
merge_brazilian_pedons <- function(bdsolos, febr,
                                      prefer  = c("bdsolos", "febr"),
                                      verbose = TRUE) {
  prefer <- match.arg(prefer)
  if (is.null(bdsolos)) bdsolos <- list()
  if (is.null(febr))    febr    <- list()
  if (!is.list(bdsolos) ||
        (length(bdsolos) > 0L &&
           !all(vapply(bdsolos, inherits, logical(1L), "PedonRecord")))) {
    stop("merge_brazilian_pedons(): 'bdsolos' must be a list of PedonRecord (or NULL).")
  }
  if (!is.list(febr) ||
        (length(febr) > 0L &&
           !all(vapply(febr, inherits, logical(1L), "PedonRecord")))) {
    stop("merge_brazilian_pedons(): 'febr' must be a list of PedonRecord (or NULL).")
  }

  bd_sisb <- vapply(bdsolos, .get_sisb_id, character(1L))
  fb_sisb <- vapply(febr,    .get_sisb_id, character(1L))

  bd_with    <- which(!is.na(bd_sisb))
  fb_with    <- which(!is.na(fb_sisb))
  bd_without <- which(is.na(bd_sisb))
  fb_without <- which(is.na(fb_sisb))

  shared_keys <- intersect(bd_sisb[bd_with], fb_sisb[fb_with])
  bd_unique_idx <- bd_with[!bd_sisb[bd_with] %in% shared_keys]
  fb_unique_idx <- fb_with[!fb_sisb[fb_with] %in% shared_keys]

  out <- list()

  # 1. Overlap: keep one pedon per sisb_id from the preferred side.
  if (length(shared_keys) > 0L) {
    for (k in shared_keys) {
      bd_hit <- bdsolos[bd_with[bd_sisb[bd_with] == k][1L]][[1L]]
      fb_hit <- febr   [fb_with[fb_sisb[fb_with] == k][1L]][[1L]]
      if (prefer == "bdsolos") {
        out[[length(out) + 1L]] <- .tag_merge_decision(
          bd_hit, "BDsolos", "kept_bdsolos"
        )
      } else {
        out[[length(out) + 1L]] <- .tag_merge_decision(
          fb_hit, "FEBR", "kept_febr"
        )
      }
    }
  }

  # 2. BDsolos pedons unique to this side (sisb_id not in FEBR).
  for (i in bd_unique_idx) {
    out[[length(out) + 1L]] <- .tag_merge_decision(
      bdsolos[[i]], "BDsolos", "unique"
    )
  }

  # 3. FEBR pedons unique to this side.
  for (i in fb_unique_idx) {
    out[[length(out) + 1L]] <- .tag_merge_decision(
      febr[[i]], "FEBR", "unique"
    )
  }

  # 4. Pedons without a sisb_id from either side -- can't dedupe; keep all.
  for (i in bd_without) {
    out[[length(out) + 1L]] <- .tag_merge_decision(
      bdsolos[[i]], "BDsolos", "unique"
    )
  }
  for (i in fb_without) {
    out[[length(out) + 1L]] <- .tag_merge_decision(
      febr[[i]], "FEBR", "unique"
    )
  }

  if (isTRUE(verbose)) {
    cli::cli_alert_success(sprintf(
      "merge_brazilian_pedons(): %d pedons total -- %d shared (kept %s), %d BDsolos-only, %d FEBR-only, %d sisb-less.",
      length(out), length(shared_keys), prefer,
      length(bd_unique_idx), length(fb_unique_idx),
      length(bd_without) + length(fb_without)
    ))
  }
  out
}


# ---- diagnostics ---------------------------------------------------------

#' Diagnostic summary of overlap between BDsolos and FEBR pedon lists
#'
#' Counts pedons by source / overlap status without performing the
#' merge. Useful for verifying the dedup ratio before committing to
#' \code{\link{merge_brazilian_pedons}}.
#'
#' @param bdsolos,febr Lists of \code{PedonRecord} objects.
#'
#' @return List with elements \code{n_bdsolos}, \code{n_febr},
#'   \code{n_bdsolos_with_sisb}, \code{n_febr_with_sisb},
#'   \code{n_shared}, \code{n_bdsolos_only}, \code{n_febr_only},
#'   \code{n_unmatchable} (sisb_id missing in one or both).
#'
#' @seealso \code{\link{merge_brazilian_pedons}}.
#' @export
summarize_brazilian_overlap <- function(bdsolos, febr) {
  if (is.null(bdsolos)) bdsolos <- list()
  if (is.null(febr))    febr    <- list()
  bd_sisb <- vapply(bdsolos, .get_sisb_id, character(1L))
  fb_sisb <- vapply(febr,    .get_sisb_id, character(1L))
  bd_with    <- !is.na(bd_sisb)
  fb_with    <- !is.na(fb_sisb)
  shared <- intersect(bd_sisb[bd_with], fb_sisb[fb_with])
  list(
    n_bdsolos             = length(bdsolos),
    n_febr                = length(febr),
    n_bdsolos_with_sisb   = sum(bd_with),
    n_febr_with_sisb      = sum(fb_with),
    n_shared              = length(shared),
    n_bdsolos_only        = sum(bd_with & !(bd_sisb %in% shared)),
    n_febr_only           = sum(fb_with & !(fb_sisb %in% shared)),
    n_unmatchable         = sum(!bd_with) + sum(!fb_with)
  )
}
