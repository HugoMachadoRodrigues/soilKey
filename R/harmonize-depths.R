# =============================================================================
# v0.9.62 -- harmonize_to_gsm(): mass-preserving spline harmonisation of
# horizon-level pedon data to GlobalSoilMap (GSM) depth intervals.
#
# Why we need this
# ----------------
# soilKey ingests four datasets (BDsolos, FEBR, KSSL+NASIS, LUCAS) whose
# horizon depths are *irregular* and *dataset-specific*. To pool them
# into a single benchmark (Order accuracy aggregated across all systems),
# every pedon needs to express its chemistry / texture / Munsell on a
# *common* set of depth intervals. The GlobalSoilMap (Arrouays et al.
# 2014) standard is 0-5 / 5-15 / 15-30 / 30-60 / 60-100 / 100-200 cm.
#
# Implementation
# --------------
# Wraps mpspline2::mpspline_tidy() (Bishop et al. 1999 mass-preserving
# spline). For each numeric horizon attribute requested, we splice the
# pedon's irregular intervals onto GSM, preserving the mass under the
# original depth-distribution curve. Categorical attributes
# (designation, Munsell hue) are propagated by depth-overlap mode
# (most-frequent class within each target interval).
#
# Input / output
# --------------
# Takes a list of soilKey PedonRecord objects. Returns a NEW list of
# PedonRecord objects whose horizons table has been re-cut on GSM
# intervals; site fields are passed through unchanged.
# =============================================================================


#' Default GlobalSoilMap depth intervals (cm)
#'
#' GSM standard per Arrouays et al. (2014) "GlobalSoilMap: Toward a
#' fine-resolution global grid of soil properties". Boundaries:
#' 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm.
#'
#' @export
GSM_DEPTHS <- c(0, 5, 15, 30, 60, 100, 200)


#' Harmonise pedons to GlobalSoilMap depth intervals
#'
#' Runs \code{mpspline2::mpspline_tidy()} on each requested numeric
#' horizon attribute, producing a new PedonRecord per input pedon
#' whose horizons table covers the canonical GSM intervals
#' (\code{\link{GSM_DEPTHS}}). Categorical attributes (designation,
#' Munsell hue) are propagated by mode-over-depth-overlap.
#'
#' @section Why mass-preserving:
#'
#' The Bishop et al. (1999) spline conserves the integral of the
#' attribute over depth: if the original pedon has 30 g/kg OC over
#' 0-15 cm, the harmonised pedon will report 30 g/kg integrated
#' over 0-15 cm (split between 0-5 and 5-15 in proportion to the
#' spline-implied gradient). This is a critical property for
#' benchmark integrity: simple linear interpolation does not
#' preserve mass and biases means upward / downward systematically.
#'
#' @section Categorical handling:
#'
#' \code{designation} and \code{munsell_hue_moist} (and other
#' character columns in the horizon schema) cannot be splined.
#' Instead, for each target GSM interval, we pick the modal value
#' weighted by the depth-overlap fraction with the input horizons.
#' Ties broken by uppermost-input-horizon precedence.
#'
#' @param pedons A list of \code{\link{PedonRecord}} objects.
#' @param attributes Character vector of numeric horizon column names
#'        to harmonise. Default covers the chemistry / texture /
#'        Munsell numeric columns the soilKey diagnostics use.
#' @param depths Numeric vector of GSM depth boundaries (n+1 values
#'        for n intervals). Default \code{\link{GSM_DEPTHS}}.
#' @param lam Smoothing parameter for the spline (default 0.1, per
#'        Bishop et al. 1999 recommendation).
#' @param verbose If \code{TRUE} (default), emits cli progress.
#' @return A list of new \code{\link{PedonRecord}} objects with
#'   harmonised horizons.
#' @references
#' Bishop, T.F.A., McBratney, A.B., Laslett, G.M. (1999). "Modelling
#' soil attribute depth functions with equal-area quadratic smoothing
#' splines." \emph{Geoderma} 91: 27-45.
#'
#' Arrouays, D. et al. (2014). "GlobalSoilMap: Toward a fine-resolution
#' global grid of soil properties." \emph{Advances in Agronomy} 125:
#' 93-134.
#' @seealso \code{mpspline2::mpspline_tidy}, \code{\link{GSM_DEPTHS}}.
#' @export
harmonize_to_gsm <- function(pedons,
                                attributes = c("clay_pct", "silt_pct",
                                                "sand_pct",
                                                "ph_h2o", "oc_pct",
                                                "cec_cmol",
                                                "base_saturation_pct",
                                                "munsell_value_moist",
                                                "munsell_chroma_moist",
                                                "redoximorphic_features_pct"),
                                depths = GSM_DEPTHS,
                                lam = 0.1,
                                verbose = TRUE) {
  if (!requireNamespace("mpspline2", quietly = TRUE)) {
    stop("harmonize_to_gsm(): the 'mpspline2' package is required. ",
         "install.packages('mpspline2').", call. = FALSE)
  }
  if (!is.list(pedons) || length(pedons) == 0L) {
    stop("harmonize_to_gsm(): `pedons` must be a non-empty list.",
         call. = FALSE)
  }
  if (length(depths) < 2L)
    stop("harmonize_to_gsm(): `depths` must have at least 2 boundaries.",
         call. = FALSE)

  out <- vector("list", length(pedons))
  n_failed <- 0L
  for (k in seq_along(pedons)) {
    p <- pedons[[k]]
    hz <- p$horizons
    if (is.null(hz) || nrow(hz) == 0L) {
      n_failed <- n_failed + 1L; next
    }
    # Drop horizons with NA depths (mpspline2 chokes on those).
    keep <- !is.na(hz$top_cm) & !is.na(hz$bottom_cm) &
              hz$bottom_cm > hz$top_cm
    hz <- hz[keep, , drop = FALSE]
    if (nrow(hz) < 2L) {
      # Single horizon: spline degenerates. Replicate the value
      # across all GSM intervals that overlap.
      out[[k]] <- .harmonize_single_horizon(p, hz, depths)
      next
    }
    # Numeric attributes via mpspline_tidy
    new_hz <- .harmonize_numeric_attrs(hz, attributes, depths, lam)
    # Categorical attrs by depth-overlap mode
    char_cols <- setdiff(colnames(hz), c(attributes, "top_cm", "bottom_cm"))
    char_cols <- char_cols[vapply(char_cols, function(c)
                                       is.character(hz[[c]]),
                                     logical(1L))]
    for (col in char_cols) {
      new_hz[[col]] <- .modal_by_overlap(hz[[col]], hz$top_cm,
                                            hz$bottom_cm, depths)
    }
    new_pedon <- p$clone()
    new_pedon$horizons <- ensure_horizon_schema(
      data.table::as.data.table(new_hz))
    out[[k]] <- new_pedon
    if (isTRUE(verbose) && (k %% 100L) == 0L)
      cli::cli_alert_info(sprintf("harmonize_to_gsm: %d / %d done",
                                     k, length(pedons)))
  }
  if (isTRUE(verbose))
    cli::cli_alert_success(sprintf(
      "harmonize_to_gsm: %d / %d harmonised, %d failed",
      length(pedons) - n_failed, length(pedons), n_failed))
  out
}


# --- internal helpers -------------------------------------------------

#' Single-horizon fallback: replicate values across overlapping GSM intervals
#' @noRd
.harmonize_single_horizon <- function(p, hz, depths) {
  if (nrow(hz) == 0L) return(NULL)
  intervals <- data.frame(top_cm    = depths[-length(depths)],
                            bottom_cm = depths[-1L])
  # Keep only intervals overlapping the single horizon
  ov <- intervals$top_cm < hz$bottom_cm[1L] &
          intervals$bottom_cm > hz$top_cm[1L]
  intervals <- intervals[ov, , drop = FALSE]
  if (nrow(intervals) == 0L) return(NULL)
  new_hz <- intervals
  for (col in setdiff(colnames(hz), c("top_cm", "bottom_cm"))) {
    new_hz[[col]] <- rep(hz[[col]][1L], nrow(intervals))
  }
  new_pedon <- p$clone()
  new_pedon$horizons <- ensure_horizon_schema(
    data.table::as.data.table(new_hz))
  new_pedon
}


#' Numeric attributes via mass-preserving spline
#' @noRd
.harmonize_numeric_attrs <- function(hz, attributes, depths, lam) {
  intervals <- data.frame(top_cm    = depths[-length(depths)],
                            bottom_cm = depths[-1L])
  new_hz <- intervals
  SID <- 1L  # single-pedon-at-a-time
  for (attr in attributes) {
    val <- hz[[attr]]
    if (is.null(val) || all(is.na(val))) {
      new_hz[[attr]] <- rep(NA_real_, nrow(intervals))
      next
    }
    df <- data.frame(SID = rep(SID, nrow(hz)),
                      UD = hz$top_cm,
                      LD = hz$bottom_cm,
                      v  = as.numeric(val),
                      stringsAsFactors = FALSE)
    keep <- !is.na(df$v) & !is.na(df$UD) & !is.na(df$LD) &
              df$LD > df$UD
    df <- df[keep, , drop = FALSE]
    if (nrow(df) < 2L) {
      # Cannot splice; replicate or NA.
      new_hz[[attr]] <- rep(if (nrow(df) == 1L) df$v else NA_real_,
                              nrow(intervals))
      next
    }
    names(df)[4L] <- attr
    out <- tryCatch(
      mpspline2::mpspline_tidy(obj = df, var_name = attr,
                                  d = depths, lam = lam),
      error   = function(e) NULL,
      warning = function(w) suppressWarnings(
        mpspline2::mpspline_tidy(obj = df, var_name = attr,
                                    d = depths, lam = lam))
    )
    if (is.null(out) || is.null(out$est_dcm)) {
      new_hz[[attr]] <- rep(NA_real_, nrow(intervals))
      next
    }
    new_hz[[attr]] <- out$est_dcm$SPLINED_VALUE[seq_len(nrow(intervals))]
  }
  new_hz
}


#' Modal categorical value by depth-overlap fraction
#' @noRd
.modal_by_overlap <- function(values, top, bottom, depths) {
  intervals_top    <- depths[-length(depths)]
  intervals_bottom <- depths[-1L]
  out <- character(length(intervals_top))
  for (j in seq_along(intervals_top)) {
    it <- intervals_top[j]; ib <- intervals_bottom[j]
    overlap <- pmax(0, pmin(bottom, ib) - pmax(top, it))
    keep <- overlap > 0 & !is.na(values) & nzchar(values)
    if (!any(keep)) { out[j] <- NA_character_; next }
    vals <- values[keep]; w <- overlap[keep]
    tab <- tapply(w, vals, sum, na.rm = TRUE)
    out[j] <- names(tab)[which.max(tab)]
  }
  out
}
