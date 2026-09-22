# =============================================================================
# v0.9.116 -- validate_horizon_geometry(): a pure depth-geometry check on a
# horizon table, used by the Pro app's Pedon builder for real-time feedback
# before a profile is classified. It complements PedonRecord$validate() (which
# also checks chemistry) with a fuller set of geometry rules, and works on a
# plain data frame, so the app can validate the editable table directly.
# =============================================================================

#' Validate horizon depth geometry
#'
#' A pure, side-effect-free check of a horizon table's depth geometry,
#' independent of any \code{\link{PedonRecord}}. The Pro app's Pedon builder
#' calls it to give immediate feedback while horizons are edited, and it is a
#' handy guard before constructing a profile from an untrusted CSV.
#'
#' It reports two severities:
#' \describe{
#'   \item{errors (these make a sane classification impossible)}{a missing or
#'     non-numeric \code{top_cm}/\code{bottom_cm}; a negative depth; a horizon
#'     whose \code{top_cm >= bottom_cm} (inverted or zero thickness); two
#'     horizons whose depths overlap.}
#'   \item{warnings (allowed, but worth surfacing)}{the shallowest horizon not
#'     starting at the surface (0 cm); a gap between consecutive horizons;
#'     horizons entered out of increasing-depth order; a duplicated horizon
#'     designation.}
#' }
#'
#' This complements \code{PedonRecord$validate()}, which additionally checks
#' chemistry (texture sums, pH, CEC vs bases, Munsell ranges); use that for a
#' built record and this for a raw table.
#'
#' @param horizons A data frame with at least numeric \code{top_cm} and
#'   \code{bottom_cm} columns (and optionally a \code{designation} column).
#' @return A list with \code{valid} (logical; \code{TRUE} when there are no
#'   errors), \code{errors} and \code{warnings} (character vectors of
#'   human-readable English messages), and \code{details} -- a named list of
#'   the offending row indices (or values) per check, so a caller can compose
#'   its own (e.g. localised) messages.
#' @examples
#' h <- data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 90),
#'                 designation = c("A", "AB", "Bt"))
#' validate_horizon_geometry(h)$valid          # TRUE
#'
#' bad <- data.frame(top_cm = c(0, 40), bottom_cm = c(50, 30))  # overlap+inverted
#' validate_horizon_geometry(bad)$errors
#' @export
validate_horizon_geometry <- function(horizons) {
  errors <- character(0); warnings <- character(0); details <- list()

  if (is.null(horizons) || !is.data.frame(horizons) || nrow(horizons) == 0L) {
    return(list(valid = FALSE, errors = "No horizons to validate.",
                warnings = character(0), details = list()))
  }
  if (!all(c("top_cm", "bottom_cm") %in% names(horizons))) {
    return(list(valid = FALSE,
                errors = "Horizon table needs 'top_cm' and 'bottom_cm' columns.",
                warnings = character(0), details = list()))
  }

  top <- suppressWarnings(as.numeric(horizons$top_cm))
  bot <- suppressWarnings(as.numeric(horizons$bottom_cm))

  # --- errors --------------------------------------------------------------
  na_rows <- which(is.na(top) | is.na(bot))
  if (length(na_rows)) {
    errors <- c(errors, sprintf("Missing or non-numeric depth in row(s) %s.",
                                paste(na_rows, collapse = ", ")))
    details$missing_depth <- na_rows
  }
  neg <- which((!is.na(top) & top < 0) | (!is.na(bot) & bot < 0))
  if (length(neg)) {
    errors <- c(errors, sprintf("Negative depth in row(s) %s.",
                                paste(neg, collapse = ", ")))
    details$negative_depth <- neg
  }
  inv <- which(!is.na(top) & !is.na(bot) & top >= bot)
  if (length(inv)) {
    errors <- c(errors, sprintf(
      "top_cm >= bottom_cm (inverted or zero-thickness) in row(s) %s.",
      paste(inv, collapse = ", ")))
    details$inverted <- inv
  }

  # --- ordering / overlaps / gaps (on the well-formed rows) ----------------
  ok  <- which(!is.na(top) & !is.na(bot) & top < bot)
  if (length(ok) >= 1L) {
    ord <- ok[order(top[ok])]

    if (length(ok) > 1L && is.unsorted(top[ok])) {
      warnings <- c(warnings, "Horizons are not entered in increasing-depth order.")
      details$non_monotonic <- TRUE
    }
    if (top[ord[1L]] > 0) {
      warnings <- c(warnings, sprintf(
        "Shallowest horizon starts at %g cm, not the surface (0 cm).", top[ord[1L]]))
      details$surface_gap <- top[ord[1L]]
    }
    if (length(ord) > 1L) {
      for (i in seq_len(length(ord) - 1L)) {
        a <- ord[i]; b <- ord[i + 1L]
        delta <- top[b] - bot[a]
        if (delta < -0.01) {
          errors <- c(errors, sprintf("Horizons in rows %d and %d overlap by %g cm.",
                                      a, b, -delta))
          details$overlap <- c(details$overlap, b)
        } else if (delta > 0.01) {
          warnings <- c(warnings, sprintf("Gap of %g cm between rows %d and %d.",
                                          delta, a, b))
          details$gap <- c(details$gap, b)
        }
      }
    }
  }

  # --- duplicate designations ----------------------------------------------
  if ("designation" %in% names(horizons)) {
    d <- as.character(horizons$designation)
    d <- trimws(d[!is.na(d)]); d <- d[nzchar(d)]
    dup <- unique(d[duplicated(d)])
    if (length(dup)) {
      warnings <- c(warnings, sprintf("Duplicate horizon designation(s): %s.",
                                      paste(dup, collapse = ", ")))
      details$duplicate_designation <- dup
    }
  }

  list(valid = length(errors) == 0L, errors = errors,
       warnings = warnings, details = details)
}
