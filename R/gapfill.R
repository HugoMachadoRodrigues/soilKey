# =============================================================================
# v0.9.120 -- Within-pedon depth gap-fill.
#
# Many reference profiles measure clay / CEC / base saturation only in a
# subset of horizons, leaving NA cells *between* measured layers. A
# deterministic key that needs a continuous depth trend (e.g. the argic
# clay-increase test, the WRB/SiBCS/USDA discrimination of Acrisol / Lixisol /
# Alisol / Luvisol) then stalls on an artefact of incomplete reporting, not of
# the soil. gapfill_within_pedon() fills those *interior* NA cells by linearly
# interpolating each attribute from the pedon's OWN measured horizons,
# recording every fill as an "inferred_prior" provenance entry so the evidence
# grade honestly drops to "C".
#
# Two deliberate honesty guards:
#   * INTERPOLATION ONLY -- a horizon's mid-depth must fall strictly between
#     the shallowest and deepest *measured* mid-depth for that attribute. We
#     never extrapolate a property above the top or below the bottom measured
#     layer (that would be an assumption about the soil, not a reading of it).
#   * AUTHORITY ORDER -- writes go through PedonRecord$add_measurement(), so an
#     interpolated value can never displace a measured, spectra-predicted or
#     VLM-extracted one.
#
# This is the within-pedon companion to apply_soilgrids_depth_prior() (which
# fills from an *external* SoilGrids profile); both share .interp_depth_profile().
# =============================================================================

# Continuous, depth-trending numeric horizon attributes a within-pedon linear
# interpolation can reasonably estimate. Categorical / geometry / spike-like
# attributes (Munsell, structure, slickensides, carbonate nodules, ...) are
# excluded on purpose -- they do not vary smoothly with depth and linear
# interpolation of them would invent values, not recover them.
.GAPFILL_DEFAULT_ATTRS <- c(
  "clay_pct", "silt_pct", "sand_pct",
  "ph_h2o", "ph_kcl", "ph_cacl2",
  "oc_pct",
  "cec_cmol", "ecec_cmol", "bs_pct", "al_sat_pct",
  "bulk_density_g_cm3"
)

#' Fill interior missing horizon attributes by within-pedon depth interpolation
#'
#' For each requested attribute, builds a depth profile from the horizons in
#' which that attribute is \emph{measured} (non-\code{NA}) and linearly
#' interpolates the value at the mid-depth of every horizon where it is missing
#' -- but only for horizons whose mid-depth falls strictly between the
#' shallowest and deepest measured layer. Cells above the top or below the
#' bottom measured layer are left \code{NA}: the function interpolates, it never
#' extrapolates. Each fill is written with \code{source = "inferred_prior"}, so
#' the \code{\link{PedonRecord}} authority order keeps it from displacing a
#' measured, spectra-predicted or VLM-extracted value, and any downstream
#' \code{\link{compute_evidence_grade}} call reports grade \code{"C"}.
#'
#' This is the within-pedon companion to
#' \code{\link{apply_soilgrids_depth_prior}} (which fills from an external
#' SoilGrids profile rather than from the profile's own measured layers). It is
#' the mechanism behind the opt-in \code{gapfill} argument of
#' \code{\link{classify_wrb2022}}, \code{\link{classify_sibcs}},
#' \code{\link{classify_usda}} and \code{\link{classify_all}}.
#'
#' Note that this mutates \code{pedon} in place (as
#' \code{apply_soilgrids_depth_prior} does). The \code{gapfill} argument of the
#' classifiers operates on a deep copy instead, so a classification call never
#' alters the caller's pedon.
#'
#' @param pedon A \code{\link{PedonRecord}} with at least two horizons.
#' @param attrs Character vector of horizon columns to fill. Defaults to the
#'        continuous depth-trending attributes a linear interpolation can
#'        reasonably estimate (clay/silt/sand, pH, organic carbon, CEC/ECEC,
#'        base/aluminium saturation, bulk density).
#' @param confidence Numeric in \[0, 1\] recorded as the provenance confidence
#'        of each interpolated cell. Defaults to \code{0.6} -- below a measured
#'        value but anchored on the profile's own data, hence above the
#'        \code{0.5} used for an external SoilGrids prior.
#' @param overwrite If \code{FALSE} (default) only \code{NA} cells are filled.
#'        If \code{TRUE}, non-measured cells are re-interpolated (measured cells
#'        are still never overwritten, and the provenance authority order is
#'        always respected).
#' @return Invisibly, the mutated \code{pedon}. An attribute
#'         \code{"gapfill_within_pedon"} on the return value records how many
#'         cells were filled and for which attributes.
#' @examples
#' h <- data.frame(
#'   top_cm    = c(0, 20, 40, 60),
#'   bottom_cm = c(20, 40, 60, 90),
#'   clay_pct  = c(15, NA, 35, 40)
#' )
#' p <- PedonRecord$new(horizons = h)
#' gapfill_within_pedon(p, attrs = "clay_pct")
#' p$horizons$clay_pct   # second horizon filled to ~25 by interpolation
#' @seealso \code{\link{apply_soilgrids_depth_prior}}, \code{\link{classify_all}}
#' @export
gapfill_within_pedon <- function(pedon,
                                 attrs      = NULL,
                                 confidence = 0.6,
                                 overwrite  = FALSE) {
  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  h <- pedon$horizons
  if (is.null(h) || nrow(h) < 2L) {
    # Need at least two horizons to bracket an interior gap.
    attr(pedon, "gapfill_within_pedon") <- list(n_filled = 0L,
                                                attrs = character(0))
    return(invisible(pedon))
  }

  if (is.null(attrs)) attrs <- .GAPFILL_DEFAULT_ATTRS
  attrs <- intersect(attrs, names(h))

  mids_all <- (h$top_cm + h$bottom_cm) / 2

  n_filled     <- 0L
  filled_attrs <- character(0)
  for (a in attrs) {
    vals <- h[[a]]
    if (!is.numeric(vals)) next
    obs <- !is.na(vals) & !is.na(mids_all)
    if (sum(obs) < 2L) next               # need >= 2 measured points to interpolate

    # Measured profile, sorted ascending by mid-depth (stats::approx needs it).
    ord      <- order(mids_all[obs])
    obs_mids <- mids_all[obs][ord]
    obs_vals <- vals[obs][ord]
    lo <- obs_mids[1L]
    hi <- obs_mids[length(obs_mids)]

    any_a <- FALSE
    for (i in seq_len(nrow(h))) {
      if (obs[i]) next                    # measured -- leave it
      if (!overwrite && !is.na(vals[i])) next
      mid <- mids_all[i]
      if (is.na(mid)) next
      if (mid <= lo || mid >= hi) next    # INTERPOLATION ONLY: skip extrapolation
      val <- .interp_depth_profile(mid, obs_mids, obs_vals)
      if (is.na(val)) next
      pedon$add_measurement(
        i, a, value = val,
        source     = "inferred_prior",
        confidence = confidence,
        notes      = "within-pedon depth interpolation",
        overwrite  = overwrite
      )
      n_filled <- n_filled + 1L
      any_a    <- TRUE
    }
    if (any_a) filled_attrs <- c(filled_attrs, a)
  }

  attr(pedon, "gapfill_within_pedon") <- list(n_filled = n_filled,
                                              attrs = filled_attrs)
  invisible(pedon)
}

# -----------------------------------------------------------------------------
# Classifier hook.
#
# Resolves the `gapfill` argument of classify_wrb2022/sibcs/usda/all and, when
# it asks for any fill, applies it to a DEEP COPY of the pedon so the caller's
# object is never mutated. Returns the (possibly filled) pedon to classify.
#
# `gapfill` accepts:
#   * FALSE / NULL  -> no-op, the original pedon is returned unchanged
#                      (the default; classification stays byte-identical).
#   * TRUE          -> gapfill_within_pedon() with default attributes.
#   * character     -> gapfill_within_pedon(attrs = <character>).
#   * list          -> do.call(gapfill_within_pedon, <list>) for full control
#                      (e.g. list(attrs = "clay_pct", confidence = 0.5)).
# -----------------------------------------------------------------------------
.classify_apply_gapfill <- function(pedon, gapfill) {
  if (is.null(gapfill) || isFALSE(gapfill)) return(pedon)
  if (!inherits(pedon, "PedonRecord")) return(pedon)

  # Deep copy so the caller's pedon is never altered. R6's deep clone does not
  # copy data.table fields, so copy horizons + provenance explicitly.
  p <- pedon$clone(deep = TRUE)
  p$horizons   <- data.table::copy(pedon$horizons)
  p$provenance <- if (is.null(pedon$provenance)) pedon$provenance
                  else data.table::copy(pedon$provenance)

  if (isTRUE(gapfill)) {
    gapfill_within_pedon(p)
  } else if (is.character(gapfill)) {
    gapfill_within_pedon(p, attrs = gapfill)
  } else if (is.list(gapfill)) {
    do.call(gapfill_within_pedon, c(list(pedon = p), gapfill))
  } else {
    rlang::abort(paste0("`gapfill` must be FALSE, TRUE, a character vector of ",
                        "attribute names, or a named list of ",
                        "gapfill_within_pedon() arguments"))
  }
  p
}
