# =============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5) MISSING in v0.9.62 -- v0.9.63 batch.
#
# This file implements the Tier-1 missing qualifiers identified by the
# v0.9.62 audit (`audit_wrb_canonical_v0962_2026-05-08.md`):
#
#   - 33 missing principal qualifiers (PQs)
#   - 68 missing supplementary qualifiers (SQs)
#
# v0.9.63 ships a batch of ~20 PQs + SQs that map cleanly to existing
# soilKey horizon attributes or to existing diagnostics. The remaining
# qualifiers (composite -- multiple existing primitives -- and
# new-primitive ones) are tracked for v0.9.64+.
#
# Each function returns a `DiagnosticResult` per the established
# `qual_<Name>` convention from `R/qualifiers-wrb2022.R`.
#
# References: IUSS Working Group WRB (2022). World Reference Base for
# Soil Resources, 4th edition. Chapter 5 (qualifiers).
# =============================================================================


# ---------- Helpers (private) ------------------------------------------------

#' Test "X within depth d cm" given an existing diagnostic
#'
#' Many WRB sub-qualifiers (Endo-, Bathy-, Hyper-, Pano-, Ortho-,
#' Ano-, etc.) are depth-bounded modifiers of an existing principal
#' qualifier or diagnostic horizon. This helper tests whether the
#' base diagnostic fires AND has any of its passing layers in the
#' given depth window.
#'
#' @keywords internal
.q_within_depth <- function(name, base_diag,
                                pedon, top_cm, bottom_cm) {
  if (!isTRUE(base_diag$passed)) {
    return(DiagnosticResult$new(
      name = name, passed = FALSE, layers = integer(0),
      evidence = list(base = base_diag,
                        depth_window = c(top_cm, bottom_cm)),
      missing = base_diag$missing %||% character(0),
      reference = sprintf("WRB (2022) Ch 5, %s", name)
    ))
  }
  h <- pedon$horizons
  in_window <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                       h$bottom_cm > top_cm & h$top_cm < bottom_cm)
  ok_layers <- intersect(base_diag$layers, in_window)
  passed <- length(ok_layers) > 0L
  DiagnosticResult$new(
    name = name, passed = passed, layers = ok_layers,
    evidence = list(base = base_diag,
                      depth_window = c(top_cm, bottom_cm),
                      n_layers_in_window = length(ok_layers)),
    missing = if (length(ok_layers) == 0L && length(base_diag$missing) > 0L)
                base_diag$missing else character(0),
    reference = sprintf("WRB (2022) Ch 5, %s", name)
  )
}


#' Volume-weighted mean of a horizon attribute over a depth window
#' @keywords internal
.q_weighted_mean <- function(values, top, bottom,
                                window_top = 0, window_bottom = 100) {
  ok <- !is.na(values) & !is.na(top) & !is.na(bottom) & bottom > top
  if (!any(ok)) return(NA_real_)
  values <- values[ok]; top <- top[ok]; bottom <- bottom[ok]
  overlap <- pmax(0, pmin(bottom, window_bottom) - pmax(top, window_top))
  if (sum(overlap) == 0) return(NA_real_)
  sum(values * overlap) / sum(overlap)
}


# ============================================================================
# PRINCIPAL QUALIFIERS (PQ) -- v0.9.63 batch
# ============================================================================


#' Coarsic qualifier (cr): >= 70\% coarse fragments by volume in upper 100 cm
#'
#' WRB 2022 Ch 5: "Containing layers (in total >= 30 cm thick) with >=
#' 70\% by volume coarse fragments and/or technic hard material averaged
#' over a depth of 100 cm from the soil surface."
#'
#' Applies to: HISTOSOLS, TECHNOSOLS, CRYOSOLS, LEPTOSOLS, PODZOLS,
#' PLINTHOSOLS, DURISOLS, GYPSISOLS, CALCISOLS.
#'
#' Implementation: weighted mean of \code{coarse_fragments_pct} over
#' the upper 100 cm; passes if \\>= 70 (or NA if no measurements).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_coarsic <- function(pedon) {
  h <- pedon$horizons
  cf <- h$coarse_fragments_pct
  if (is.null(cf) || all(is.na(cf))) {
    return(DiagnosticResult$new(
      name = "Coarsic", passed = NA, layers = integer(0),
      evidence = list(reason = "no coarse_fragments_pct data"),
      missing = "coarse_fragments_pct",
      reference = "WRB (2022) Ch 5, Coarsic"
    ))
  }
  wmean <- .q_weighted_mean(cf, h$top_cm, h$bottom_cm,
                                window_top = 0, window_bottom = 100)
  passed <- isTRUE(is.finite(wmean) && wmean >= 70)
  layers <- if (passed)
              which(!is.na(cf) & cf >= 70 & h$top_cm < 100)
            else integer(0)
  DiagnosticResult$new(
    name = "Coarsic", passed = passed, layers = layers,
    evidence = list(weighted_mean_cf_pct = wmean,
                      threshold = 70, depth_window_cm = c(0, 100)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Coarsic"
  )
}


#' Fractic qualifier (fc): fractures (cracks) within 100 cm
#'
#' WRB 2022 Ch 5 (Durisols / Gypsisols / Calcisols): "Showing
#' fractures within 100 cm of the soil surface" (a duripan, gypsic,
#' or calcic horizon that has cracked / fractured).
#'
#' Implementation: positive \code{cracks_width_cm} or
#' \code{cracks_depth_cm} on any layer with top <= 100 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_fractic <- function(pedon) {
  h <- pedon$horizons
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  cw <- h$cracks_width_cm[upper];  cd <- h$cracks_depth_cm[upper]
  has_cracks <- (!is.na(cw) & cw > 0) | (!is.na(cd) & cd > 0)
  if (length(has_cracks) == 0L || all(is.na(cw) & is.na(cd))) {
    return(DiagnosticResult$new(
      name = "Fractic", passed = NA, layers = integer(0),
      evidence = list(reason = "no cracks data"),
      missing = c("cracks_width_cm", "cracks_depth_cm"),
      reference = "WRB (2022) Ch 5, Fractic"
    ))
  }
  passed <- any(has_cracks, na.rm = TRUE)
  layers <- if (passed) upper[which(has_cracks)] else integer(0)
  DiagnosticResult$new(
    name = "Fractic", passed = passed, layers = layers,
    evidence = list(n_cracked_layers = sum(has_cracks, na.rm = TRUE)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Fractic"
  )
}


#' Gibbsic qualifier (gi): high gibbsite (>= 25\%) in fine earth
#'
#' WRB 2022 Ch 5 (Plinthosols / Ferralsols): "Containing layers with
#' >= 25\% gibbsite by mass averaged over a depth of 100 cm".
#'
#' soilKey schema does not currently carry direct gibbsite percent.
#' The closest proxy is \code{al_ox_pct} (oxalate-extractable Al, \%),
#' but gibbsite is poorly extracted by oxalate. The sulfuric attack
#' \code{al2o3_sulfuric_pct} captures crystalline Al-oxides (gibbsite
#' + boehmite + diaspore + Al-substitution in goethite). This
#' implementation uses Al2O3 by sulfuric attack >= 25\% as a proxy
#' (slight over-estimate, since not all crystalline Al is gibbsite).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_gibbsic <- function(pedon) {
  h <- pedon$horizons
  al2o3 <- h$al2o3_sulfuric_pct
  if (is.null(al2o3) || all(is.na(al2o3))) {
    return(DiagnosticResult$new(
      name = "Gibbsic", passed = NA, layers = integer(0),
      evidence = list(reason = "no al2o3_sulfuric_pct (proxy)"),
      missing = "al2o3_sulfuric_pct",
      reference = "WRB (2022) Ch 5, Gibbsic"
    ))
  }
  wmean <- .q_weighted_mean(al2o3, h$top_cm, h$bottom_cm, 0, 100)
  passed <- isTRUE(is.finite(wmean) && wmean >= 25)
  layers <- if (passed)
              which(!is.na(al2o3) & al2o3 >= 25 & h$top_cm < 100)
            else integer(0)
  DiagnosticResult$new(
    name = "Gibbsic", passed = passed, layers = layers,
    evidence = list(weighted_mean_al2o3 = wmean,
                      threshold = 25, proxy = "Al2O3 sulfuric attack",
                      caveat = "true gibbsite percent requires XRD"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Gibbsic"
  )
}


#' Ferritic qualifier (fr): high free-Fe in fine earth
#'
#' WRB 2022 Ch 5 (Nitisols / Ferralsols): "Containing layers with
#' >= 18\% Fe2O3 (or 12.6\% Fe) in fine earth, averaged over upper
#' 100 cm or to a contact / petroplinthic / pisoplinthic / R."
#'
#' Implementation: weighted mean of \code{fe_dcb_pct} (DCB-extractable
#' Fe2O3, the canonical Fe-pool for Ferralic / Nitic chemistry) over
#' the upper 100 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_ferritic <- function(pedon) {
  h <- pedon$horizons
  fe <- h$fe_dcb_pct
  if (is.null(fe) || all(is.na(fe))) {
    return(DiagnosticResult$new(
      name = "Ferritic", passed = NA, layers = integer(0),
      evidence = list(reason = "no fe_dcb_pct data"),
      missing = "fe_dcb_pct",
      reference = "WRB (2022) Ch 5, Ferritic"
    ))
  }
  wmean <- .q_weighted_mean(fe, h$top_cm, h$bottom_cm, 0, 100)
  passed <- isTRUE(is.finite(wmean) && wmean >= 18)
  layers <- if (passed)
              which(!is.na(fe) & fe >= 18 & h$top_cm < 100)
            else integer(0)
  DiagnosticResult$new(
    name = "Ferritic", passed = passed, layers = layers,
    evidence = list(weighted_mean_fe2o3_pct = wmean, threshold = 18),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Ferritic"
  )
}


#' Greyzemic qualifier (gz): mollic / umbric overlain by albic-like layer
#'
#' WRB 2022 Ch 5 (Chernozems / Phaeozems / Umbrisols): "Having a
#' mollic / umbric horizon overlain by a thin (<= 10 cm) albic-like
#' layer with low chroma and high value (Munsell value >= 4 moist
#' AND chroma <= 2)."
#'
#' Implementation: presence of mollic OR umbric (we have
#' \code{\link{mollic}} but not yet \code{umbric}) AND an overlying
#' bleached layer (\code{munsell_value_moist >= 4} and
#' \code{munsell_chroma_moist <= 2}, thickness <= 10 cm).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_greyzemic <- function(pedon) {
  h <- pedon$horizons
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(DiagnosticResult$new(
      name = "Greyzemic", passed = FALSE, layers = integer(0),
      evidence = list(mollic = mol),
      missing = mol$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Greyzemic"
    ))
  }
  # Find horizons above the mollic/umbric layers that look bleached
  surface_idx <- which(h$bottom_cm <= min(h$top_cm[mol$layers], na.rm = TRUE))
  if (length(surface_idx) == 0L) {
    return(DiagnosticResult$new(
      name = "Greyzemic", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no overlying horizon", mollic = mol),
      reference = "WRB (2022) Ch 5, Greyzemic"
    ))
  }
  v <- h$munsell_value_moist[surface_idx]
  c <- h$munsell_chroma_moist[surface_idx]
  thk <- h$bottom_cm[surface_idx] - h$top_cm[surface_idx]
  bleached <- !is.na(v) & v >= 4 & !is.na(c) & c <= 2 &
                !is.na(thk) & thk <= 10
  passed <- any(bleached, na.rm = TRUE)
  layers <- if (passed) c(surface_idx[which(bleached)], mol$layers)
            else integer(0)
  DiagnosticResult$new(
    name = "Greyzemic", passed = passed, layers = layers,
    evidence = list(mollic = mol, n_bleached = sum(bleached, na.rm = TRUE)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Greyzemic"
  )
}


#' Profundihumic qualifier (ph): SOC >= 1.4\% to depth >= 100 cm
#'
#' WRB 2022 Ch 5 (Nitisols / Ferralsols): "Containing >= 1.4\% organic
#' carbon (by weight, excluding live fine roots) as a weighted average
#' from the soil surface down to 100 cm."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_profundihumic <- function(pedon) {
  h <- pedon$horizons
  oc <- h$oc_pct
  if (is.null(oc) || all(is.na(oc))) {
    return(DiagnosticResult$new(
      name = "Profundihumic", passed = NA, layers = integer(0),
      evidence = list(reason = "no oc_pct data"),
      missing = "oc_pct",
      reference = "WRB (2022) Ch 5, Profundihumic"
    ))
  }
  wmean <- .q_weighted_mean(oc, h$top_cm, h$bottom_cm, 0, 100)
  passed <- isTRUE(is.finite(wmean) && wmean >= 1.4)
  layers <- if (passed)
              which(!is.na(oc) & oc >= 1.4 & h$top_cm < 100)
            else integer(0)
  DiagnosticResult$new(
    name = "Profundihumic", passed = passed, layers = layers,
    evidence = list(weighted_mean_oc_pct = wmean,
                      threshold = 1.4, depth_window_cm = c(0, 100)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Profundihumic"
  )
}


#' Wapnic qualifier (wp): soft, moist limnic material >= 80\% CaCO3
#'
#' WRB 2022 Ch 5 (Calcisols / Gleysols / Cryosols): "Having soft,
#' moist limnic material that contains >= 80\% by mass CaCO3
#' equivalent within 100 cm of the soil surface."
#'
#' Implementation: \code{caco3_pct} >= 80 in any layer with top <= 100.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_wapnic <- function(pedon) {
  h <- pedon$horizons
  cc <- h$caco3_pct
  if (is.null(cc) || all(is.na(cc))) {
    return(DiagnosticResult$new(
      name = "Wapnic", passed = NA, layers = integer(0),
      evidence = list(reason = "no caco3_pct data"),
      missing = "caco3_pct",
      reference = "WRB (2022) Ch 5, Wapnic"
    ))
  }
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  qualifying <- upper[!is.na(cc[upper]) & cc[upper] >= 80]
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Wapnic", passed = passed, layers = qualifying,
    evidence = list(threshold_caco3_pct = 80,
                      n_qualifying = length(qualifying)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Wapnic"
  )
}


#' Mawic qualifier (mw): moss-fibre-dominant peat
#'
#' WRB 2022 Ch 5 (Histosols): "Containing >= 40\% by volume moss
#' fibres in organic material >= 40 cm thick within 100 cm."
#'
#' Implementation: any horizon with \code{fiber_content_unrubbed_pct}
#' \\>= 40 AND \code{layer_origin} matches "moss" pattern, OR fall
#' back to \code{histic_horizon} OK + fibre threshold (the moss-
#' specific test is over-permissive without explicit moss flag).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_mawic <- function(pedon) {
  h <- pedon$horizons
  fiber <- h$fiber_content_unrubbed_pct
  origin <- h$layer_origin
  if (is.null(fiber) || all(is.na(fiber))) {
    return(DiagnosticResult$new(
      name = "Mawic", passed = NA, layers = integer(0),
      evidence = list(reason = "no fiber_content data"),
      missing = "fiber_content_unrubbed_pct",
      reference = "WRB (2022) Ch 5, Mawic"
    ))
  }
  is_moss <- !is.na(origin) & grepl("moss|sphagnum|musgo",
                                        tolower(origin), perl = TRUE)
  qualifying <- which(!is.na(fiber) & fiber >= 40 & is_moss)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Mawic", passed = passed, layers = qualifying,
    evidence = list(threshold_fiber_pct = 40, layer_origin_pattern = "moss"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Mawic"
  )
}


#' Muusic qualifier (mu): high-fibre peat (non-moss-specific)
#'
#' WRB 2022 Ch 5 (Histosols): "Containing >= 75\% by volume rubbed
#' fibres in organic material >= 40 cm thick within 100 cm."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_muusic <- function(pedon) {
  h <- pedon$horizons
  fiber_r <- h$fiber_content_rubbed_pct
  if (is.null(fiber_r) || all(is.na(fiber_r))) {
    return(DiagnosticResult$new(
      name = "Muusic", passed = NA, layers = integer(0),
      evidence = list(reason = "no fiber_content_rubbed_pct data"),
      missing = "fiber_content_rubbed_pct",
      reference = "WRB (2022) Ch 5, Muusic"
    ))
  }
  qualifying <- which(!is.na(fiber_r) & fiber_r >= 75 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Muusic", passed = passed, layers = qualifying,
    evidence = list(threshold_rubbed_fiber_pct = 75),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Muusic"
  )
}


#' Murshic qualifier (mr): partly drained organic with strong decomposition
#'
#' WRB 2022 Ch 5 (Histosols): "Drained organic soils with sapric
#' decomposition (rubbed fibres < 17\%) and von Post >= 7 in upper 50
#' cm." Proxy via low rubbed fibre + von Post (when present).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_murshic <- function(pedon) {
  h <- pedon$horizons
  fiber_r <- h$fiber_content_rubbed_pct
  vp <- h$von_post_index
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  fr_ok <- !is.na(fiber_r[upper]) & fiber_r[upper] < 17
  vp_ok <- !is.na(vp[upper])      & vp[upper] >= 7
  qualifying <- upper[fr_ok | vp_ok]
  if (length(qualifying) == 0L &&
        all(is.na(fiber_r[upper])) && all(is.na(vp[upper]))) {
    return(DiagnosticResult$new(
      name = "Murshic", passed = NA, layers = integer(0),
      evidence = list(reason = "no fiber + von Post data in upper 50 cm"),
      missing = c("fiber_content_rubbed_pct", "von_post_index"),
      reference = "WRB (2022) Ch 5, Murshic"
    ))
  }
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Murshic", passed = passed, layers = qualifying,
    evidence = list(threshold_rubbed_fiber_pct = 17,
                      threshold_von_post = 7,
                      depth_window_cm = c(0, 50)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Murshic"
  )
}


#' Rockic qualifier (rk): rock-dominated organic horizon
#'
#' WRB 2022 Ch 5 (Histosols): "Having a continuous rock or rock-like
#' material starting <= 25 cm from the soil surface AND >= 50\% by
#' volume coarse fragments in the upper 50 cm." Reuses
#' \code{\link{leptic_features}} (max_depth = 25) AND coarse-frag check.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_rockic <- function(pedon) {
  lep <- leptic_features(pedon, max_depth = 25)
  h <- pedon$horizons
  cf <- h$coarse_fragments_pct
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  cf_ok <- !is.na(cf[upper]) & cf[upper] >= 50
  has_rocky <- isTRUE(lep$passed) || any(cf_ok, na.rm = TRUE)
  passed <- isTRUE(lep$passed) && any(cf_ok, na.rm = TRUE)
  layers <- if (passed) c(lep$layers, upper[which(cf_ok)])
            else integer(0)
  DiagnosticResult$new(
    name = "Rockic", passed = passed, layers = unique(layers),
    evidence = list(leptic = lep, n_coarse_layers = sum(cf_ok, na.rm = TRUE)),
    missing = lep$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Rockic"
  )
}


#' Thyric qualifier (ty): organic technic material in upper 100 cm
#'
#' WRB 2022 Ch 5 (Leptosols / Technosols): "Containing >= 20\% by
#' volume technic hard material with organic origin (waste organic
#' refuse, peat-like industrial residues) in upper 100 cm."
#' Implementation: \code{artefacts_industrial_pct} populated AND
#' organic-rich (oc_pct >= 5\%).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_thyric <- function(pedon) {
  h <- pedon$horizons
  art <- h$artefacts_industrial_pct
  oc  <- h$oc_pct
  if (is.null(art) || all(is.na(art))) {
    return(DiagnosticResult$new(
      name = "Thyric", passed = NA, layers = integer(0),
      evidence = list(reason = "no artefacts_industrial_pct data"),
      missing = "artefacts_industrial_pct",
      reference = "WRB (2022) Ch 5, Thyric"
    ))
  }
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  qualifying <- upper[!is.na(art[upper]) & art[upper] >= 20 &
                          !is.na(oc[upper]) & oc[upper] >= 5]
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Thyric", passed = passed, layers = qualifying,
    evidence = list(threshold_artefacts = 20, threshold_oc = 5),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Thyric"
  )
}


#' Anthromollic qualifier (am): anthric horizon overlying spodic
#'
#' WRB 2022 Ch 5 (Podzols): "Having an anthric (irrigation /
#' Plaggic-like) surface horizon directly over spodic / albic /
#' diagnostic horizon." Combines \code{\link{anthric_horizons}} +
#' overlying-spodic check.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_anthromollic <- function(pedon) {
  ant <- tryCatch(anthric_horizons(pedon),
                    error = function(e) NULL)
  if (is.null(ant) || !isTRUE(ant$passed)) {
    return(DiagnosticResult$new(
      name = "Anthromollic", passed = FALSE, layers = integer(0),
      evidence = list(anthric = ant),
      missing = if (is.null(ant)) "anthric_horizons" else
                  ant$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Anthromollic"
    ))
  }
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  if (is.null(spo) || !isTRUE(spo$passed)) {
    return(DiagnosticResult$new(
      name = "Anthromollic", passed = FALSE, layers = integer(0),
      evidence = list(anthric = ant, spodic = spo),
      missing = if (is.null(spo)) "spodic" else
                  spo$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Anthromollic"
    ))
  }
  DiagnosticResult$new(
    name = "Anthromollic", passed = TRUE,
    layers = unique(c(ant$layers, spo$layers)),
    evidence = list(anthric = ant, spodic = spo),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Anthromollic"
  )
}


#' Endocalcaric qualifier (cae): calcaric only at depth >= 50 cm
#'
#' WRB 2022 Ch 5 (Umbrisols / Retisols): "Calcaric material starting
#' >= 50 cm from the soil surface." Modifier of
#' \code{\link{calcaric_material}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endocalcaric <- function(pedon) {
  base <- calcaric_material(pedon)
  .q_within_depth("Endocalcaric", base, pedon,
                    top_cm = 50, bottom_cm = 200)
}


#' Endodolomitic qualifier (dme): dolomitic only at depth >= 50 cm
#'
#' WRB 2022 Ch 5 (Umbrisols / Retisols): "Dolomitic material starting
#' >= 50 cm from the soil surface." Modifier of
#' \code{\link{dolomitic_material}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endodolomitic <- function(pedon) {
  base <- dolomitic_material(pedon)
  .q_within_depth("Endodolomitic", base, pedon,
                    top_cm = 50, bottom_cm = 200)
}


#' Anofluvic qualifier (af): fluvic material only at depth >= 50 cm
#'
#' WRB 2022 Ch 5 (Fluvisols): "Fluvic material starting >= 50 cm
#' from the soil surface." Depth modifier of
#' \code{\link{fluvic_material}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_anofluvic <- function(pedon) {
  base <- fluvic_material(pedon)
  .q_within_depth("Anofluvic", base, pedon, 50, 200)
}


#' Pantofluvic qualifier (pf): fluvic material throughout 0-100 cm
#'
#' WRB 2022 Ch 5 (Fluvisols): "Fluvic material continuously from the
#' soil surface to >= 100 cm depth."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pantofluvic <- function(pedon) {
  base <- fluvic_material(pedon)
  if (!isTRUE(base$passed)) {
    return(DiagnosticResult$new(
      name = "Pantofluvic", passed = FALSE, layers = integer(0),
      evidence = list(base = base),
      missing = base$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Pantofluvic"
    ))
  }
  h <- pedon$horizons
  # All horizons covering 0-100 cm must have fluvic
  cover <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                   h$top_cm < 100)
  passed <- length(setdiff(cover, base$layers)) == 0L &&
              length(cover) > 0L
  DiagnosticResult$new(
    name = "Pantofluvic", passed = passed, layers = base$layers,
    evidence = list(base = base, n_uncovered = length(setdiff(cover, base$layers))),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Pantofluvic"
  )
}


#' Orthofluvic qualifier (of): fluvic material 50-100 cm
#'
#' WRB 2022 Ch 5 (Fluvisols): "Fluvic material with its upper boundary
#' between 50 and 100 cm of the soil surface." (default Fluvisol qualifier
#' when neither Ano- nor Panto- applies.)
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_orthofluvic <- function(pedon) {
  base <- fluvic_material(pedon)
  .q_within_depth("Orthofluvic", base, pedon, 50, 100)
}


#' Oxyaquic qualifier (oa): saturation regime without reduction
#'
#' WRB 2022 Ch 5: "Saturated with water for >= 30 consecutive days
#' or 90 cumulative days but not concurrently showing reductimorphic
#' features." Proxy: stagnic_pattern OR redox below threshold + low
#' depth_to_water_table indicator (when available). For BDsolos /
#' FEBR (no permafrost / aquic conditions tracked), checks
#' redoximorphic features WITHOUT gleyic-hue reduction.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_oxyaquic <- function(pedon) {
  h <- pedon$horizons
  redox <- h$redoximorphic_features_pct
  hue   <- h$munsell_hue_moist
  chroma <- h$munsell_chroma_moist
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  has_redox  <- !is.na(redox[upper]) & redox[upper] >= 5
  is_gleyic  <- !is.na(hue[upper]) &
                  grepl(.GLEYIC_HUE_REGEX, trimws(hue[upper]),
                          perl = TRUE) &
                  !is.na(chroma[upper]) & chroma[upper] <= 2
  qualifying <- upper[has_redox & !is_gleyic]
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Oxyaquic", passed = passed, layers = qualifying,
    evidence = list(threshold_redox_pct = 5,
                      n_redox_no_gleyic = length(qualifying)),
    missing = if (all(is.na(redox))) "redoximorphic_features_pct"
              else character(0),
    reference = "WRB (2022) Ch 5, Oxyaquic"
  )
}


#' Oxygleyic qualifier (og): gleyic regime with predominant oxidation
#'
#' WRB 2022 Ch 5 (Gleysols): "Gleyic properties dominated by oxidation
#' (redox concentrations >> reductive depletions)." Heuristic: gleyic
#' fires AND redoximorphic_features_pct >= 10 in upper 50 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_oxygleyic <- function(pedon) {
  gl <- gleyic_properties(pedon)
  if (!isTRUE(gl$passed)) {
    return(DiagnosticResult$new(
      name = "Oxygleyic", passed = FALSE, layers = integer(0),
      evidence = list(gleyic = gl),
      missing = gl$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Oxygleyic"
    ))
  }
  h <- pedon$horizons
  redox <- h$redoximorphic_features_pct
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  qualifying <- upper[!is.na(redox[upper]) & redox[upper] >= 10]
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Oxygleyic", passed = passed,
    layers = unique(c(gl$layers, qualifying)),
    evidence = list(gleyic = gl, n_redox_concentrations = length(qualifying)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Oxygleyic"
  )
}


#' Reductaquic qualifier (ra): aquic + reductive at depth
#'
#' WRB 2022 Ch 5 (Cryosols): "Saturation + reductimorphic features
#' (chroma <= 1, low value) at >= 50 cm depth."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_reductaquic <- function(pedon) {
  h <- pedon$horizons
  chroma <- h$munsell_chroma_moist
  hue    <- h$munsell_hue_moist
  deep <- which(!is.na(h$top_cm) & h$top_cm >= 50)
  is_gleyic <- !is.na(hue[deep]) &
                 grepl(.GLEYIC_HUE_REGEX, trimws(hue[deep]),
                         perl = TRUE) &
                 !is.na(chroma[deep]) & chroma[deep] <= 1
  qualifying <- deep[is_gleyic]
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Reductaquic", passed = passed, layers = qualifying,
    evidence = list(threshold_chroma = 1, depth_floor_cm = 50),
    missing = if (all(is.na(chroma))) "munsell_chroma_moist"
              else character(0),
    reference = "WRB (2022) Ch 5, Reductaquic"
  )
}


#' Reductigleyic qualifier (rg): gleyic + reductive
#'
#' WRB 2022 Ch 5 (Gleysols): "Gleyic dominated by reduction
#' (gleyic-hue layers occupying \\>= 50\% of the upper 50 cm)."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_reductigleyic <- function(pedon) {
  gl <- gleyic_properties(pedon)
  if (!isTRUE(gl$passed)) {
    return(DiagnosticResult$new(
      name = "Reductigleyic", passed = FALSE, layers = integer(0),
      evidence = list(gleyic = gl),
      missing = gl$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Reductigleyic"
    ))
  }
  h <- pedon$horizons
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  in_upper <- intersect(gl$layers, upper)
  thk <- if (length(in_upper) > 0L)
           sum(pmax(0, pmin(h$bottom_cm[in_upper], 50) -
                     pmax(h$top_cm[in_upper], 0)),
               na.rm = TRUE)
         else 0
  passed <- thk >= 25  # >= half of 50 cm = >= 25 cm
  DiagnosticResult$new(
    name = "Reductigleyic", passed = passed, layers = in_upper,
    evidence = list(gleyic = gl, gleyic_thickness_in_upper50 = thk,
                      threshold_cm = 25),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Reductigleyic"
  )
}


#' Transportic qualifier (tr): transported material (Technosols / Regosols)
#'
#' WRB 2022 Ch 5: "Soil material that has been moved by humans (mining
#' spoils, dredged sediments, roadside fill) covering >= 100 cm of
#' the upper soil." Detection via \code{layer_origin} matching
#' \code{transport|fill|spoil|dredge|aterro|antropico}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_transportic <- function(pedon) {
  h <- pedon$horizons
  origin <- h$layer_origin
  if (is.null(origin) || all(is.na(origin))) {
    return(DiagnosticResult$new(
      name = "Transportic", passed = NA, layers = integer(0),
      evidence = list(reason = "no layer_origin data"),
      missing = "layer_origin",
      reference = "WRB (2022) Ch 5, Transportic"
    ))
  }
  pat <- "transport|fill|spoil|dredge|aterro|antropic|antrop"
  is_transported <- !is.na(origin) &
                       grepl(pat, tolower(origin), perl = TRUE)
  qualifying <- which(is_transported & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Transportic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat, n_transported = length(qualifying)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Transportic"
  )
}


#' Relocatic qualifier (rl): relocated material (Arenosols / Regosols)
#'
#' WRB 2022 Ch 5: "Soil material that has been relocated within the
#' same site (cut-and-fill, terracing) covering >= 100 cm of the
#' upper soil." Implementation parallels \code{\link{qual_transportic}}
#' but matches \code{relocat|terraced|cut.fill}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_relocatic <- function(pedon) {
  h <- pedon$horizons
  origin <- h$layer_origin
  if (is.null(origin) || all(is.na(origin))) {
    return(DiagnosticResult$new(
      name = "Relocatic", passed = NA, layers = integer(0),
      evidence = list(reason = "no layer_origin data"),
      missing = "layer_origin",
      reference = "WRB (2022) Ch 5, Relocatic"
    ))
  }
  pat <- "relocat|terrac|cut.?fill|aterrad"
  is_reloc <- !is.na(origin) & grepl(pat, tolower(origin), perl = TRUE)
  qualifying <- which(is_reloc & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Relocatic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Relocatic"
  )
}


#' Isolatic qualifier (il): isolated technic material
#'
#' WRB 2022 Ch 5 (Technosols): "Containing isolated bodies of
#' technic hard material (concrete blocks, asphalt slabs, brick
#' walls) but NOT covering the full surface." Detection via
#' \code{artefacts_urbic_pct} or \code{artefacts_industrial_pct}
#' between 5 and 50\%.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_isolatic <- function(pedon) {
  h <- pedon$horizons
  art <- pmax(h$artefacts_urbic_pct %||% rep(NA_real_, nrow(h)),
                h$artefacts_industrial_pct %||% rep(NA_real_, nrow(h)),
                na.rm = TRUE)
  if (all(is.na(art))) {
    return(DiagnosticResult$new(
      name = "Isolatic", passed = NA, layers = integer(0),
      evidence = list(reason = "no artefact percent data"),
      missing = c("artefacts_urbic_pct", "artefacts_industrial_pct"),
      reference = "WRB (2022) Ch 5, Isolatic"
    ))
  }
  qualifying <- which(!is.na(art) & art >= 5 & art <= 50 &
                          h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Isolatic", passed = passed, layers = qualifying,
    evidence = list(threshold_artefact_low = 5,
                      threshold_artefact_high = 50),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Isolatic"
  )
}


# ============================================================================
# SUPPLEMENTARY QUALIFIERS (SQ) -- v0.9.63 batch
# ============================================================================
#
# SQs are typically modifiers (Endo-, Epi-, Bathy-, Hyper-, Hypo-,
# Proto-) of an existing diagnostic. The patterns are stereotyped:
#
#   Endo-X     : X applies only at depth >= 50 cm
#   Epi-X      : X applies only in upper 50 cm
#   Bathy-X    : X applies very deep (100-200 cm)
#   Hyper-X    : X with extreme intensity (chemistry > threshold + N)
#   Hypo-X     : X with weak intensity
#   Proto-X    : early-stage / borderline X
# =============================================================================


#' Endodystric supplementary qualifier (eds): dystric only at depth
#'
#' WRB 2022 Ch 5: "Distric (BS < 50\%) at >= 50 cm depth."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endodystric <- function(pedon) {
  base <- distrofico(pedon)
  .q_within_depth("Endodystric", base, pedon, 50, 200)
}


#' Epidystric supplementary qualifier (epd): dystric only in upper 50 cm
#'
#' WRB 2022 Ch 5: "Dystric (BS < 50\%) in upper 50 cm and eutric below."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_epidystric <- function(pedon) {
  base <- distrofico(pedon)
  .q_within_depth("Epidystric", base, pedon, 0, 50)
}


#' Endoeutric supplementary qualifier (eee): eutric only at depth
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endoeutric <- function(pedon) {
  base <- eutrofico(pedon)
  .q_within_depth("Endoeutric", base, pedon, 50, 200)
}


#' Epieutric supplementary qualifier (eee): eutric only in upper 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_epieutric <- function(pedon) {
  base <- eutrofico(pedon)
  .q_within_depth("Epieutric", base, pedon, 0, 50)
}


#' Endoabruptic supplementary qualifier (eea): abrupt textural change deep
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endoabruptic <- function(pedon) {
  base <- tryCatch(abrupt_textural_difference(pedon),
                     error = function(e) NULL)
  if (is.null(base)) {
    return(DiagnosticResult$new(
      name = "Endoabruptic", passed = NA, layers = integer(0),
      evidence = list(reason = "abrupt_textural_difference unavailable"),
      missing = "abrupt_textural_difference",
      reference = "WRB (2022) Ch 5, Endoabruptic"
    ))
  }
  .q_within_depth("Endoabruptic", base, pedon, 50, 200)
}


#' Endoleptic supplementary qualifier (lle): rock contact 50-100 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endoleptic <- function(pedon) {
  # leptic_features tests for rock <= 25 cm (max_depth=25 default).
  # Endoleptic redefines depth window to 50-100 cm.
  lep_deep <- leptic_features(pedon, max_depth = 100)
  if (!isTRUE(lep_deep$passed)) {
    return(DiagnosticResult$new(
      name = "Endoleptic", passed = FALSE, layers = integer(0),
      evidence = list(leptic_deep = lep_deep),
      missing = lep_deep$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Endoleptic"
    ))
  }
  h <- pedon$horizons
  in_window <- which(!is.na(h$top_cm) & h$top_cm >= 50 &
                       h$top_cm <= 100)
  ok_layers <- intersect(lep_deep$layers, in_window)
  passed <- length(ok_layers) > 0L
  DiagnosticResult$new(
    name = "Endoleptic", passed = passed, layers = ok_layers,
    evidence = list(leptic_deep = lep_deep,
                      depth_window = c(50, 100)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Endoleptic"
  )
}


#' Endothionic supplementary qualifier (etn): thionic at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endothionic <- function(pedon) {
  base <- tryCatch(carater_tionico(pedon),
                     error = function(e) NULL)
  if (is.null(base)) {
    return(DiagnosticResult$new(
      name = "Endothionic", passed = NA, layers = integer(0),
      evidence = list(reason = "carater_tionico unavailable"),
      missing = "carater_tionico",
      reference = "WRB (2022) Ch 5, Endothionic"
    ))
  }
  .q_within_depth("Endothionic", base, pedon, 50, 200)
}


#' Hypernatric supplementary qualifier (hyna): very high Na (>= 70\% ESP)
#'
#' WRB 2022 Ch 5: "Sodic with exchangeable sodium percentage >= 70\%."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hypernatric <- function(pedon) {
  h <- pedon$horizons
  na <- h$na_cmol; cec <- h$cec_cmol
  if (is.null(na) || is.null(cec)) {
    return(DiagnosticResult$new(
      name = "Hypernatric", passed = NA, layers = integer(0),
      evidence = list(reason = "no na_cmol / cec_cmol data"),
      missing = c("na_cmol", "cec_cmol"),
      reference = "WRB (2022) Ch 5, Hypernatric"
    ))
  }
  esp <- 100 * na / cec
  qualifying <- which(!is.na(esp) & esp >= 70 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Hypernatric", passed = passed, layers = qualifying,
    evidence = list(threshold_esp = 70,
                      n_qualifying_layers = length(qualifying)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Hypernatric"
  )
}


#' Sulfatic supplementary qualifier (su): high sulfate content
#' WRB 2022 Ch 5: "Containing >= 25\% gypsum or >= 5\% sulfate by mass."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_sulfatic <- function(pedon) {
  h <- pedon$horizons
  # Try sulfate / sulphate horizon column (oxidising regime; not always present)
  s <- h$so4_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(s))) {
    return(DiagnosticResult$new(
      name = "Sulfatic", passed = NA, layers = integer(0),
      evidence = list(reason = "no so4_pct data"),
      missing = "so4_pct",
      reference = "WRB (2022) Ch 5, Sulfatic"
    ))
  }
  qualifying <- which(!is.na(s) & s >= 5 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Sulfatic", passed = passed, layers = qualifying,
    evidence = list(threshold_so4_pct = 5),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Sulfatic"
  )
}


#' Carbonic supplementary qualifier (cb): high SOC content (>= 6\%)
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_carbonic <- function(pedon) {
  h <- pedon$horizons
  oc <- h$oc_pct
  if (is.null(oc) || all(is.na(oc))) {
    return(DiagnosticResult$new(
      name = "Carbonic", passed = NA, layers = integer(0),
      evidence = list(reason = "no oc_pct data"),
      missing = "oc_pct",
      reference = "WRB (2022) Ch 5, Carbonic"
    ))
  }
  qualifying <- which(!is.na(oc) & oc >= 6 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Carbonic", passed = passed, layers = qualifying,
    evidence = list(threshold_oc_pct = 6),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Carbonic"
  )
}


#' Carbonatic supplementary qualifier (cn): >= 50\% carbonates
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_carbonatic <- function(pedon) {
  h <- pedon$horizons
  cc <- h$caco3_pct
  if (is.null(cc) || all(is.na(cc))) {
    return(DiagnosticResult$new(
      name = "Carbonatic", passed = NA, layers = integer(0),
      evidence = list(reason = "no caco3_pct data"),
      missing = "caco3_pct",
      reference = "WRB (2022) Ch 5, Carbonatic"
    ))
  }
  qualifying <- which(!is.na(cc) & cc >= 50 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Carbonatic", passed = passed, layers = qualifying,
    evidence = list(threshold_caco3_pct = 50),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Carbonatic"
  )
}


#' Hydrophobic supplementary qualifier (hf): water-repellent surface
#'
#' WRB 2022 Ch 5: "Surface horizon (0-5 cm) with hydrophobic
#' character measurable as MED (Molarity of an Ethanol Droplet) >= 1
#' or WDPT (Water Drop Penetration Time) >= 60 s."
#' Implementation: textual flag in \code{vesicular_pores} (BDsolos:
#' "hidrofóbico", "water repellent") OR a \code{water_repellence}
#' field if the loader supplies it.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hydrophobic <- function(pedon) {
  h <- pedon$horizons
  flag <- h$vesicular_pores %||% rep(NA_character_, nrow(h))
  if (all(is.na(flag))) {
    return(DiagnosticResult$new(
      name = "Hydrophobic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface vesicular_pores / hydrophobic flag"),
      missing = "vesicular_pores",
      reference = "WRB (2022) Ch 5, Hydrophobic"
    ))
  }
  pat <- "hidrofobic|water.?repellent|hydrophob"
  qualifying <- which(!is.na(flag) & grepl(pat, tolower(flag), perl = TRUE) &
                          h$top_cm < 5)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Hydrophobic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Hydrophobic"
  )
}


#' Pyric supplementary qualifier (py): fire-affected horizon
#' WRB 2022 Ch 5: "Containing layers with charcoal / soot / fire-baked
#' material (visual or chemical evidence)."
#' Implementation: \code{layer_origin} or designation matching fire-related text.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pyric <- function(pedon) {
  h <- pedon$horizons
  origin <- h$layer_origin %||% rep(NA_character_, nrow(h))
  desg   <- h$designation  %||% rep(NA_character_, nrow(h))
  pat <- "pyric|burn|charcoal|fogo|incendio|carvao"
  is_pyric <- (!is.na(origin) & grepl(pat, tolower(origin), perl = TRUE)) |
              (!is.na(desg) & grepl(pat, tolower(desg), perl = TRUE))
  if (!any(!is.na(origin)) && !any(!is.na(desg))) {
    return(DiagnosticResult$new(
      name = "Pyric", passed = NA, layers = integer(0),
      evidence = list(reason = "no origin or designation data"),
      missing = c("layer_origin", "designation"),
      reference = "WRB (2022) Ch 5, Pyric"
    ))
  }
  qualifying <- which(is_pyric)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Pyric", passed = passed, layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Pyric"
  )
}


#' Lignic supplementary qualifier (lg): wood content in organic horizon
#' WRB 2022 Ch 5: "Containing recognisable wood remains (>= 25\% by
#' volume or weight) in organic material."
#' Implementation: \code{woody_fragments_pct} or layer_origin matching wood.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_lignic <- function(pedon) {
  h <- pedon$horizons
  wf <- h$woody_fragments_pct %||% rep(NA_real_, nrow(h))
  origin <- h$layer_origin   %||% rep(NA_character_, nrow(h))
  has_wf <- !is.na(wf) & wf >= 25
  has_word <- !is.na(origin) &
                grepl("lignic|wood|madeir|raiz_grossa",
                        tolower(origin), perl = TRUE)
  qualifying <- which(has_wf | has_word)
  passed <- length(qualifying) > 0L
  if (all(is.na(wf)) && all(is.na(origin))) {
    return(DiagnosticResult$new(
      name = "Lignic", passed = NA, layers = integer(0),
      evidence = list(reason = "no woody_fragments / layer_origin"),
      missing = c("woody_fragments_pct", "layer_origin"),
      reference = "WRB (2022) Ch 5, Lignic"
    ))
  }
  DiagnosticResult$new(
    name = "Lignic", passed = passed, layers = qualifying,
    evidence = list(threshold_woody_pct = 25),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Lignic"
  )
}


#' Bathyspodic supplementary qualifier (bs): spodic at 100-200 cm depth
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_bathyspodic <- function(pedon) {
  base <- spodic(pedon)
  .q_within_depth("Bathyspodic", base, pedon, 100, 200)
}


#' Cohesic supplementary qualifier (co): cohesive horizon (extra-firm dry)
#' WRB 2022 Ch 5: "Containing layers with extreme dry consistence
#' AND moist consistence very firm." Implementation: matches via
#' \code{consistence_dry} ("extremely hard") OR
#' \code{consistence_moist} ("very firm"), within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_cohesic <- function(pedon) {
  h <- pedon$horizons
  cd <- h$consistence_dry  %||% rep(NA_character_, nrow(h))
  cm <- h$consistence_moist %||% rep(NA_character_, nrow(h))
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  hard <- !is.na(cd[upper]) & grepl("extremely.?hard|extr.+dura",
                                        tolower(cd[upper]), perl = TRUE)
  firm <- !is.na(cm[upper]) & grepl("very.?firm|muito.?firme",
                                        tolower(cm[upper]), perl = TRUE)
  qualifying <- upper[hard | firm]
  if (length(qualifying) == 0L &&
        all(is.na(cd[upper])) && all(is.na(cm[upper]))) {
    return(DiagnosticResult$new(
      name = "Cohesic", passed = NA, layers = integer(0),
      evidence = list(reason = "no consistence data"),
      missing = c("consistence_dry", "consistence_moist"),
      reference = "WRB (2022) Ch 5, Cohesic"
    ))
  }
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Cohesic", passed = passed, layers = qualifying,
    evidence = list(pattern_dry = "extremely.?hard",
                      pattern_moist = "very.?firm"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Cohesic"
  )
}


#' Inclinic supplementary qualifier (in): tilted / inclined position
#' WRB 2022 Ch 5: site has a slope >= 10\% (relevo declivoso).
#' Implementation: site$slope_pct (when populated) >= 10 OR
#' parent_material / forma_relevo flagging steep terrain.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_inclinic <- function(pedon) {
  slope <- pedon$site$slope_pct %||% NA_real_
  relief <- pedon$site$forma_relevo %||% pedon$site$drainage %||% ""
  if (!is.na(slope) && is.numeric(slope)) {
    passed <- isTRUE(slope >= 10)
    return(DiagnosticResult$new(
      name = "Inclinic", passed = passed,
      layers = integer(0),
      evidence = list(slope_pct = slope, threshold = 10),
      missing = character(0),
      reference = "WRB (2022) Ch 5, Inclinic"
    ))
  }
  # Heuristic: site description mentions "ondulado / forte ondulado /
  # montanhoso / escarpado" -> declivous.
  passed <- grepl("forte.?ondulad|montanhos|escarpad",
                    tolower(relief), perl = TRUE)
  DiagnosticResult$new(
    name = "Inclinic", passed = passed, layers = integer(0),
    evidence = list(slope_pct = slope, relief = relief),
    missing = if (is.na(slope) && !nzchar(relief))
                c("slope_pct", "forma_relevo") else character(0),
    reference = "WRB (2022) Ch 5, Inclinic"
  )
}


#' Gelic supplementary qualifier (gl): permafrost or strong frost activity
#' WRB 2022 Ch 5: "Permafrost within 200 cm of the soil surface OR
#' gelic materials." Modifier of cryic_conditions for non-Cryosols.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_gelic <- function(pedon) {
  base <- tryCatch(cryic_conditions(pedon), error = function(e) NULL)
  if (is.null(base)) {
    return(DiagnosticResult$new(
      name = "Gelic", passed = NA, layers = integer(0),
      evidence = list(reason = "cryic_conditions unavailable"),
      missing = "cryic_conditions",
      reference = "WRB (2022) Ch 5, Gelic"
    ))
  }
  passed <- isTRUE(base$passed)
  DiagnosticResult$new(
    name = "Gelic", passed = passed, layers = base$layers,
    evidence = list(cryic = base),
    missing = base$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Gelic"
  )
}
