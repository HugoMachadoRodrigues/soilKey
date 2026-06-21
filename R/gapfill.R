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
#' \code{compute_evidence_grade} call reports grade \code{"C"}.
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

# =============================================================================
# v0.9.140 -- Definitional-closure gap-fill (in-horizon).
#
# A measured diagnostic on the local SiBCS benchmarks (Redape n=94, BDsolos RJ
# n=722) showed the missing cells are WHOLE-HORIZON: clay is NA only when sand
# AND silt are also NA, and base saturation is NA only when CEC and the
# exchangeable bases are also NA -- so a within-horizon proxy is essentially
# never available where it is needed (0 texture-closure-fillable, ~1
# bs-closure-fillable in each dataset). The single non-trivial case is al_sat,
# which Redape never reports (0%) yet is definitionally derivable from the
# measured exchange complex (al + bases): see gapfill_measurement_v09140.md.
#
# gapfill_derive_horizon() fills the cells that follow by DEFINITION (closure)
# from other measured columns in the SAME horizon -- not a statistical estimate:
#   * the texture third (clay/silt/sand) when the other two are measured;
#   * ecec   = sum(bases) + al;
#   * al_sat = 100 * al / ecec;
#   * bs     = 100 * sum(bases) / cec.
# Each write goes through add_measurement(source = "inferred_prior") so it never
# displaces a measured value and the evidence grade honestly drops to "C". This
# is a DATA-RECOVERY tool: the same Redape benchmark measured the al_sat closure
# as accuracy-NEUTRAL (carater_alitico already keys on the measured V<50 branch),
# so it is off by default like the other gap-fill methods.
# =============================================================================

#' Fill horizon attributes derivable BY DEFINITION from the same horizon
#'
#' Recovers cells that are exact closures of other measured columns in the same
#' horizon (not statistical estimates): the texture third (clay/silt/sand) when
#' the other two are present and sum to \\< 100; effective CEC as
#' \code{sum(bases) + al}; aluminium saturation as \code{100 * al / ecec}; and
#' base saturation as \code{100 * sum(bases) / cec}. Every fill is written with
#' \code{source = "inferred_prior"} so the \code{\link{PedonRecord}} authority
#' order keeps it from displacing a measured value and the evidence grade drops
#' to \code{"C"}. Companion to \code{\link{gapfill_within_pedon}} (depth
#' interpolation) and \code{\link{apply_soilgrids_depth_prior}} (external prior);
#' reachable via the \code{gapfill = list(method = "derive")} argument of the
#' classifiers.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param overwrite If \code{FALSE} (default) only \code{NA} target cells are
#'        filled.
#' @return Invisibly, the mutated \code{pedon}; attribute
#'         \code{"gapfill_derive_horizon"} records the count filled.
#' @seealso \code{\link{gapfill_within_pedon}}, \code{\link{apply_soilgrids_depth_prior}}
#' @export
gapfill_derive_horizon <- function(pedon, overwrite = FALSE) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("`pedon` must be a PedonRecord")
  h <- pedon$horizons
  if (is.null(h) || nrow(h) == 0L) {
    attr(pedon, "gapfill_derive_horizon") <- list(n_filled = 0L)
    return(invisible(pedon))
  }
  g  <- function(col) if (col %in% names(h)) h[[col]] else rep(NA_real_, nrow(h))
  na <- function(x) is.na(x)
  bases <- rowSums(cbind(g("ca_cmol"), g("mg_cmol"), g("k_cmol"), g("na_cmol")),
                   na.rm = TRUE)
  # bases is only trustworthy when at least Ca and Mg are measured.
  bases_ok <- !na(g("ca_cmol")) & !na(g("mg_cmol"))
  cla <- g("clay_pct"); sil <- g("silt_pct"); san <- g("sand_pct")
  al  <- g("al_cmol");  cec <- g("cec_cmol"); ecec <- g("ecec_cmol")

  fill <- function(col, idx, val, note) {
    idx <- idx[!na(val[idx]) & is.finite(val[idx])]
    for (i in idx) pedon$add_measurement(i, col, value = val[i],
        source = "inferred_prior", confidence = 0.7,
        notes = note, overwrite = overwrite)
    length(idx)
  }

  n <- 0L
  if (all(c("clay_pct","silt_pct","sand_pct") %in% names(h))) {
    third <- function(target_col, target_vals, a, b) {
      v   <- 100 - a - b
      idx <- which((na(target_vals) | overwrite) & !na(a) & !na(b) &
                     v >= 0 & v <= 100)
      fill(target_col, idx, v, "texture closure (100 - other two)")
    }
    n <- n + third("clay_pct", cla, sil, san)
    n <- n + third("silt_pct", sil, cla, san)
    n <- n + third("sand_pct", san, cla, sil)
  }
  if ("ecec_cmol" %in% names(h)) {
    v <- bases + al
    idx <- which((na(ecec) | overwrite) & bases_ok & !na(al) & v >= 0)
    n <- n + fill("ecec_cmol", idx, v, "ECEC closure (sum bases + Al)")
    ecec <- g("ecec_cmol")
  }
  if ("al_sat_pct" %in% names(h)) {
    denom <- ifelse(!na(ecec) & ecec > 0, ecec, al + bases)
    v <- 100 * al / denom
    idx <- which((na(g("al_sat_pct")) | overwrite) & !na(al) & bases_ok &
                   !na(denom) & denom > 0 & v >= 0 & v <= 100)
    n <- n + fill("al_sat_pct", idx, v, "Al-saturation closure (100 Al / ECEC)")
  }
  if ("bs_pct" %in% names(h)) {
    v <- 100 * bases / cec
    idx <- which((na(g("bs_pct")) | overwrite) & bases_ok & !na(cec) & cec > 0 &
                   v >= 0 & v <= 100)
    n <- n + fill("bs_pct", idx, v, "base-saturation closure (100 sum-bases / CEC)")
  }
  attr(pedon, "gapfill_derive_horizon") <- list(n_filled = n)
  invisible(pedon)
}

# =============================================================================
# v0.9.144 -- Predicted-taxon gap-fill (non-circular external prior).
#
# The within-pedon interp / definitional-closure fills cannot touch the
# WHOLE-HORIZON gaps of the reference datasets (clay NA only when sand+silt also
# NA, etc.). An external prior keyed on the soil's CLASS can: build a mean depth
# profile per taxon from a calibration set, then fill a test pedon's missing
# cells from the profile of the taxon the deterministic key assigns it WITHOUT
# fill. This is NON-CIRCULAR: the fill is keyed on the model's own provisional
# prediction (not the reference label), and the profiles are calibrated on a
# SEPARATE set (e.g. a train split). It is the class-prior companion to
# apply_soilgrids_depth_prior() (coordinate prior) and shares the same six-slice
# depth grid + .interp_depth_profile().
# =============================================================================

# Normalise an order/taxon label to a comparison key (lowercase, accent-free,
# first word, de-pluralised) so a reference label and a predicted RSG match.
.taxon_key <- function(label) {
  if (is.null(label) || length(label) == 0L || is.na(label[1L])) return(NA_character_)
  x <- tolower(trimws(as.character(label[1L])))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- strsplit(x, "[ ,;/]")[[1L]][1L]
  sub("s$", "", x %||% "")
}

#' Build per-taxon mean depth profiles for predicted-taxon gap-fill
#'
#' For each taxon (the first word of the reference label at the requested level),
#' averages each attribute across the calibration pedons into the six standard
#' depth slices (0-5 ... 100-200 cm). The result feeds
#' \code{\link{gapfill_by_predicted_taxon}}. Calibrate on a set DISJOINT from the
#' pedons you will fill (e.g. a train split) to keep the fill non-circular.
#'
#' @param pedons A list of \code{\link{PedonRecord}} with a reference label.
#' @param ref_field Site field holding the reference label (default
#'        \code{"reference_sibcs"}; e.g. \code{"reference_usda"} / \code{"reference_wrb"}).
#' @param attrs Attributes to profile (default the continuous gap-fill set).
#' @return A named list \code{taxon -> attr -> numeric(6)} (NA where a taxon has
#'         no measured value in a slice).
#' @seealso \code{\link{gapfill_by_predicted_taxon}}
#' @export
build_taxon_profiles <- function(pedons, ref_field = "reference_sibcs",
                                 attrs = NULL) {
  if (is.null(attrs)) attrs <- .GAPFILL_DEFAULT_ATTRS
  mids <- .SOILGRIDS_DEPTH_MIDS
  flat <- list()                                   # "tax\001attr\001slice" -> values
  taxa <- character(0); attrset <- character(0)
  for (p in pedons) {
    if (!inherits(p, "PedonRecord")) next
    tax <- .taxon_key(p$site[[ref_field]] %||% NA_character_)
    if (is.na(tax) || !nzchar(tax)) next
    h <- p$horizons
    if (is.null(h) || nrow(h) == 0L) next
    midh <- (h$top_cm + h$bottom_cm) / 2
    for (a in intersect(attrs, names(h))) {
      v <- h[[a]]
      if (!is.numeric(v)) next
      for (i in seq_len(nrow(h))) {
        if (is.na(v[i]) || is.na(midh[i])) next
        k   <- which.min(abs(mids - midh[i]))
        key <- paste(tax, a, k, sep = "\001")
        flat[[key]] <- c(flat[[key]], v[i])
        taxa <- union(taxa, tax); attrset <- union(attrset, a)
      }
    }
  }
  out <- list()
  for (tax in taxa) {
    byattr <- list()
    for (a in attrset) {
      prof <- vapply(seq_along(mids), function(k) {
        x <- flat[[paste(tax, a, k, sep = "\001")]]
        if (is.null(x) || !length(x)) NA_real_ else mean(x, na.rm = TRUE)
      }, numeric(1))
      if (!all(is.na(prof))) byattr[[a]] <- prof
    }
    out[[tax]] <- byattr
  }
  out
}

#' Fill missing horizon attributes from the predicted taxon's mean profile
#'
#' Classifies \code{pedon} with NO fill to get a provisional taxon, then fills
#' its missing cells from \code{taxon_profiles[[<that taxon>]]} (built by
#' \code{\link{build_taxon_profiles}}). Non-circular: the fill is keyed on the
#' model's own prediction, not the reference. Each fill is written with
#' \code{source = "inferred_prior"} (grade C). Reachable via
#' \code{gapfill = list(method = "taxon", taxon_profiles = <...>)}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param taxon_profiles Output of \code{\link{build_taxon_profiles}}.
#' @param system One of \code{"sibcs"} (default), \code{"wrb2022"}, \code{"usda"}.
#' @param attrs Attributes to fill (default: those present in the matched profile).
#' @param confidence Provenance confidence (default 0.55, below a coordinate prior).
#' @return Invisibly, the mutated \code{pedon}; attribute
#'         \code{"gapfill_by_predicted_taxon"} records the taxon + cells filled.
#' @seealso \code{\link{build_taxon_profiles}}, \code{\link{apply_soilgrids_depth_prior}}
#' @export
gapfill_by_predicted_taxon <- function(pedon, taxon_profiles,
                                       system = c("sibcs", "wrb2022", "usda"),
                                       attrs = NULL, confidence = 0.55) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("`pedon` must be a PedonRecord")
  system <- match.arg(system)
  classer <- switch(system, sibcs = classify_sibcs,
                    wrb2022 = classify_wrb2022, usda = classify_usda)
  prov <- tryCatch(classer(pedon, on_missing = "silent")$rsg_or_order,
                   error = function(e) NA_character_)
  tax  <- .taxon_key(prov)
  prof <- if (!is.na(tax)) taxon_profiles[[tax]] else NULL
  n <- 0L
  if (!is.null(prof)) {
    h <- pedon$horizons
    use <- intersect(if (is.null(attrs)) names(prof) else attrs, names(h))
    for (a in use) {
      pr <- prof[[a]]
      if (is.null(pr) || all(is.na(pr))) next
      for (i in seq_len(nrow(h))) {
        if (!is.na(h[[a]][i])) next
        mid <- (h$top_cm[i] + h$bottom_cm[i]) / 2
        if (is.na(mid)) next
        val <- .interp_depth_profile(mid, .SOILGRIDS_DEPTH_MIDS, pr)
        if (is.na(val)) next
        pedon$add_measurement(i, a, value = val, source = "inferred_prior",
            confidence = confidence,
            notes = sprintf("predicted-taxon prior (%s)", tax))
        n <- n + 1L
      }
    }
  }
  attr(pedon, "gapfill_by_predicted_taxon") <- list(taxon = tax, n_filled = n)
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
#   * character     -> gapfill_within_pedon(attrs = <character>) (back-compat).
#   * list          -> if it carries a `method` key, dispatch to one or more of
#                      "interp" (gapfill_within_pedon), "derive"
#                      (gapfill_derive_horizon) or "soilgrids"
#                      (apply_soilgrids_depth_prior), applied in the given order;
#                      remaining list elements are passed to the method(s).
#                      Without a `method` key it is do.call'd on
#                      gapfill_within_pedon for back-compat.
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
    if (!is.null(gapfill$method)) {
      methods <- gapfill$method
      args    <- gapfill[setdiff(names(gapfill), "method")]
      for (m in methods) {
        if (identical(m, "interp")) {
          do.call(gapfill_within_pedon, c(list(pedon = p), args))
        } else if (identical(m, "derive")) {
          ow <- args[intersect(names(args), "overwrite")]
          do.call(gapfill_derive_horizon, c(list(pedon = p), ow))
        } else if (identical(m, "soilgrids")) {
          do.call(apply_soilgrids_depth_prior, c(list(pedon = p), args))
        } else if (identical(m, "taxon")) {
          do.call(gapfill_by_predicted_taxon, c(list(pedon = p), args))
        } else if (identical(m, "spectra")) {
          # `method` is consumed as the dispatcher key, so fill_from_spectra's
          # own model choice is passed as `fill_method` (mbl/plsr_local/pretrained).
          sargs <- args
          if (!is.null(sargs$fill_method)) {
            sargs$method <- sargs$fill_method; sargs$fill_method <- NULL
          }
          do.call(fill_from_spectra, c(list(pedon = p), sargs))
        } else {
          rlang::abort(paste0("unknown gapfill method '", m,
                              "'; use interp / derive / soilgrids / taxon / spectra"))
        }
      }
    } else {
      do.call(gapfill_within_pedon, c(list(pedon = p), gapfill))
    }
  } else {
    rlang::abort(paste0("`gapfill` must be FALSE, TRUE, a character vector of ",
                        "attribute names, or a named list (optionally with a ",
                        "`method` of interp / derive / soilgrids / taxon / spectra)"))
  }
  p
}
