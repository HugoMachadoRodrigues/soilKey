# =============================================================
# USDA Soil Taxonomy 13th edition (2022) -- Diagnostic Epipedons
# Chapter 3, pp 13-21
# =============================================================
#
# An epipedon is a diagnostic surface horizon that meets specific
# requirements of color, organic carbon content, base saturation,
# and thickness. The eight epipedons defined in the KST 13ed are:
#
#  Anthropic   -- ancient anthropogenic surface (rare)
#  Folistic    -- well-drained organic surface
#  Histic      -- saturated organic surface
#  Melanic     -- thick, andic, very dark organic-rich surface
#  Mollic      -- thick, dark, base-rich mineral surface
#  Ochric      -- catch-all (fails all other tests)
#  Plaggen     -- ancient anthropogenic raised landform (Europe)
#  Umbric      -- thick, dark, base-poor mineral surface
#
# This file (v0.8.x) implements the 6 most common epipedons used in
# the Order/Suborder keys. Anthropic and Plaggen are deferred to
# v0.9 because they require anthropogenic-landform criteria not
# yet captured in the schema.
# =============================================================


# Helper: check if a layer (row index) has texture finer than
# loamy fine sand. Returns TRUE if clay >= 18 OR (clay+silt > 50).
# An approximation of the USDA "loamy fine sand or coarser" cut-off
# used in mollic/umbric thickness rules.
.is_finer_than_loamy_fine_sand <- function(clay_pct, silt_pct, sand_pct) {
  if (is.na(clay_pct) || is.na(sand_pct)) return(NA)
  # Approximation: loamy fine sand has sand 70-91, clay <=15.
  # "Finer than loamy fine sand" -> clay > 15 OR sand <= 70.
  clay_pct > 15 || sand_pct <= 70
}


# Helper: top-of-mineral-soil. KST measures depths from "the mineral
# soil surface" -- the top of the first non-organic horizon.
.mineral_soil_surface_cm <- function(h) {
  if (nrow(h) == 0L) return(NA_real_)
  ord <- order(h$top_cm, na.last = TRUE)
  for (i in ord) {
    des <- h$designation[i]
    if (is.na(des)) next
    # Mineral horizon = anything not starting with O or H.
    if (!grepl("^[OH]", des)) return(h$top_cm[i])
  }
  NA_real_
}


# Helper: cumulative thickness of organic horizons (designation
# starting with O or H) restricted to layers whose top_cm is below a
# given depth (default 0 = whole profile).
.cumulative_organic_thickness <- function(h, max_top_cm = Inf) {
  if (nrow(h) == 0L) return(0)
  org_idx <- which(!is.na(h$designation) &
                       grepl("^[OH]", h$designation) &
                       !is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(org_idx) == 0L) return(0)
  thk <- h$bottom_cm[org_idx] - h$top_cm[org_idx]
  thk[is.na(thk)] <- 0
  sum(pmax(thk, 0))
}


# ---- Histic Epipedon (KST 13ed, Cap 3 pp 13-15) ---------------------

#' Histic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A surface horizon (or layers within 40 cm of the surface) that is
#' periodically saturated with water and has sufficiently high
#' organic carbon to be considered organic soil material. Diagnostic
#' for the Histosols order, the Histels suborder of Gelisols, and
#' the Hist- modifier in many other taxa.
#'
#' KST 13ed required characteristics (Ch. 3, pp 13-15):
#' \itemize{
#'   \item Saturated 30+ days/year (or artificially drained); AND
#'   \item Organic soil material that is either:
#'     \itemize{
#'       \item 20-60 cm thick AND (Sphagnum >= 75 percent OR
#'             bulk_density < 0.1 g/cm3); OR
#'       \item 20-40 cm thick (general); OR
#'     }
#'   \item OR Ap horizon mixed to 25 cm with OC >= 8 percent by weight.
#' }
#'
#' Implementation notes (v0.8.x):
#' \itemize{
#'   \item Saturation is detected via a horizon designation
#'         starting with H (per KST notation) or via the WRB
#'         \code{horizonte_glei} as fallback when redoximorphic
#'         features are present.
#'   \item Sphagnum content uses the WRB \code{fiber_content_rubbed_pct}
#'         column (>= 75 means very fibrous); refinement to a true
#'         Sphagnum-specific column is deferred.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_pct Minimum organic carbon percent for organic soil
#'        material (default 12; equivalent to ~20\% organic matter
#'        per KST conversion factor 0.58).
#' @param min_thickness_cm Minimum thickness (default 20 cm).
#' @param min_ap_oc_pct Minimum OC for the Ap-horizon shortcut
#'        (default 8 percent).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022). \emph{Keys to Soil Taxonomy},
#'   13th edition, USDA-NRCS, Washington DC. Ch. 3, pp. 13-15.
#' @export
histic_epipedon_usda <- function(pedon,
                                    min_oc_pct       = 12,
                                    min_thickness_cm = 20,
                                    min_ap_oc_pct    = 8) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "histic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  # Path 1: H-designated organic layers totaling >= min_thickness.
  h_layers <- which(!is.na(h$designation) & grepl("^H", h$designation))
  thk_h <- if (length(h_layers) > 0L)
             sum(pmax(h$bottom_cm[h_layers] - h$top_cm[h_layers], 0),
                   na.rm = TRUE)
           else 0
  oc_h <- if (length(h_layers) > 0L)
            h$oc_pct[h_layers]
          else numeric(0)
  oc_pass <- if (length(oc_h) > 0L)
               !is.na(oc_h) & oc_h >= min_oc_pct
             else logical(0)
  path1_ok <- thk_h >= min_thickness_cm && any(oc_pass)

  # Path 2: Ap horizon with OC >= 8% mixed to 25 cm.
  ap_layers <- which(!is.na(h$designation) & grepl("^Ap", h$designation) &
                       !is.na(h$top_cm) & h$top_cm <= 25)
  path2_ok <- length(ap_layers) > 0L &&
                any(!is.na(h$oc_pct[ap_layers]) &
                      h$oc_pct[ap_layers] >= min_ap_oc_pct)

  passed <- path1_ok || path2_ok
  layers <- if (path1_ok) h_layers[oc_pass] else ap_layers
  miss <- character(0)
  if (length(h_layers) > 0L && all(is.na(oc_h))) miss <- c(miss, "oc_pct")
  DiagnosticResult$new(
    name = "histic_epipedon_usda", passed = passed,
    layers = layers,
    evidence = list(h_layers = h_layers, organic_thickness_cm = thk_h,
                      ap_layers = ap_layers,
                      path1_organic = path1_ok,
                      path2_ap = path2_ok),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 14-15"
  )
}


# ---- Folistic Epipedon (KST 13ed, Ch 3, p 13-14) ---------------------

#' Folistic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A freely-drained surface organic horizon. Differs from the histic
#' epipedon in that it is saturated for less than 30 days per year.
#' Diagnostic for the Folists suborder of Histosols and the Folistels
#' great group of Histels.
#'
#' KST 13ed required characteristics (Ch. 3, pp 13-14):
#' \itemize{
#'   \item Saturated < 30 days/year (and not artificially drained); AND
#'   \item Organic soil material: 15+ cm thick (with Sphagnum-rich
#'         exception 20-60 cm) OR Ap with OC >= 8 percent.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_pct Minimum OC for organic soil material (default 12).
#' @param min_thickness_cm Minimum thickness (default 15 cm).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 13-14.
#' @export
folistic_epipedon_usda <- function(pedon,
                                      min_oc_pct       = 12,
                                      min_thickness_cm = 15) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "folistic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  # Folistic = O horizons (drained), thickness >= 15 cm with OC >= 12%.
  o_layers <- which(!is.na(h$designation) & grepl("^O", h$designation))
  thk_o <- if (length(o_layers) > 0L)
             sum(pmax(h$bottom_cm[o_layers] - h$top_cm[o_layers], 0),
                   na.rm = TRUE)
           else 0
  oc_o <- if (length(o_layers) > 0L)
            h$oc_pct[o_layers]
          else numeric(0)
  oc_pass <- if (length(oc_o) > 0L)
               !is.na(oc_o) & oc_o >= min_oc_pct
             else logical(0)
  passed <- thk_o >= min_thickness_cm && any(oc_pass)
  miss <- character(0)
  if (length(o_layers) > 0L && all(is.na(oc_o))) miss <- c(miss, "oc_pct")
  DiagnosticResult$new(
    name = "folistic_epipedon_usda", passed = passed,
    layers = if (passed) o_layers[oc_pass] else integer(0),
    evidence = list(o_layers = o_layers,
                      organic_thickness_cm = thk_o,
                      threshold_cm = min_thickness_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 13-14"
  )
}


# ---- Mollic Epipedon (KST 13ed, Ch 3, pp 15-17) ----------------------

#' Mollic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A thick, dark-colored, base-rich mineral surface horizon. The
#' principal diagnostic horizon of the Mollisols order; also
#' qualifies many subgroups of other orders as "Mollic" or
#' "Pachic".
#'
#' KST 13ed required characteristics (Ch. 3, pp 15-17):
#' \itemize{
#'   \item Color: dominant color value <= 3 (moist) AND <= 5 (dry)
#'         AND chroma <= 3 (moist), with adjustments for CaCO3
#'         content (deferred to v0.9);
#'   \item Base saturation (NH4OAc, pH 7) >= 50 percent throughout;
#'   \item Organic carbon >= 0.6 percent (or 2.5 percent if value
#'         is 4-5 moist; or 0.6 absolute > C horizon);
#'   \item Thickness: 18 cm general, 25 cm if texture is loamy fine
#'         sand or coarser, 10 cm if directly above lithic/densic/
#'         paralithic contact (\\code{thin_lithic_overlay} branch);
#'   \item Structure: peds <= 30 cm OR rupture-resistance <=
#'         moderately hard;
#'   \item Some part moist 90+ days when soil temp at 50 cm is
#'         >= 5 C (deferred -- requires climatic data).
#' }
#'
#' Implementation notes (v0.8.x):
#' \itemize{
#'   \item Thickness rule is computed dynamically based on texture
#'         and presence of underlying lithic/paralithic contact.
#'   \item N value < 0.7 / fluidity nonfluid is assumed (laboratory
#'         tests rarely available);
#'   \item 90-day moisture condition is deferred to v0.9.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Default 50 percent.
#' @param min_oc_pct Default 0.6 percent.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-17.
#' @export
mollic_epipedon_usda <- function(pedon,
                                    min_bs     = 50,
                                    min_oc_pct = 0.6) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "mollic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-17"
    ))
  }
  # Candidate: A or Ap horizons starting at the mineral soil surface.
  ms_top <- .mineral_soil_surface_cm(h)
  if (is.na(ms_top)) ms_top <- 0
  cand <- which(!is.na(h$designation) & grepl("^A", h$designation) &
                  !is.na(h$top_cm) & h$top_cm <= ms_top + 5)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "mollic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface A horizon"),
      missing = "designation",
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  # v0.9.10: KST 13ed Ch. 3 (mollic epipedon) explicitly excludes
  # "anthropic" / artefact-rich horizons -- the dark colour from urban
  # / industrial fill is not a pedogenic signal. We screen out
  # candidates whose `artefacts_pct` is >= 20 % (the WRB Technic
  # threshold; KST uses qualitative "human-altered material" wording
  # but the same threshold is the de-facto practice). Without this
  # screen, the canonical Technosol fixture (Au with artefacts_pct =
  # 30, dark anthropogenic colour, high BS) was being assigned mollic
  # and routed to Hapludolls instead of the canonical Entisols
  # (Arents).
  art_pct <- if ("artefacts_pct" %in% names(h)) h$artefacts_pct[cand]
             else rep(NA_real_, length(cand))
  artefact_layers <- cand[!is.na(art_pct) & art_pct >= 20]
  cand <- setdiff(cand, artefact_layers)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "mollic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "all candidate A horizons are artefact-rich",
                       artefact_layers = artefact_layers),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  tests <- list()
  tests$color           <- test_mollic_color(h, candidate_layers = cand,
                                                 max_value_moist = 3,
                                                 max_chroma_moist = 3,
                                                 max_value_dry = 5)
  tests$organic_carbon  <- test_mollic_organic_carbon(h,
                                                          min_pct = min_oc_pct,
                                                          candidate_layers = cand)
  tests$base_saturation <- test_mollic_base_saturation(h,
                                                          min_pct = min_bs,
                                                          candidate_layers = cand)
  # Thickness is dynamic: choose the appropriate threshold.
  passing_so_far <- intersect(intersect(tests$color$layers,
                                            tests$organic_carbon$layers),
                                  tests$base_saturation$layers)
  thk_threshold <- 18
  if (length(passing_so_far) > 0L) {
    sample_i <- passing_so_far[1]
    sandy <- isTRUE(.is_finer_than_loamy_fine_sand(h$clay_pct[sample_i],
                                                       h$silt_pct[sample_i],
                                                       h$sand_pct[sample_i])) == FALSE
    if (sandy) thk_threshold <- 25
  }
  tests$thickness <- test_minimum_thickness(h, min_cm = thk_threshold,
                                                candidate_layers = passing_so_far)
  tests$structure <- test_mollic_structure(h, candidate_layers = cand)

  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "mollic_epipedon_usda", passed = agg$passed,
    layers = agg$layers,
    evidence = c(tests, list(thickness_threshold_cm = thk_threshold,
                                  candidate_layers = cand)),
    missing = agg$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-17"
  )
}


# ---- Umbric Epipedon (KST 13ed, Ch 3, pp 18-20) ----------------------

#' Umbric epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A thick, dark-colored, base-poor (BS < 50 percent) mineral
#' surface horizon. Differs from mollic in low base saturation;
#' qualifies the Humults / Humic / Umbric subgroups in many orders.
#'
#' KST 13ed required characteristics (Ch. 3, pp 18-20):
#' \itemize{
#'   \item Color: same as mollic (V<=3 moist, V<=5 dry, chroma<=3);
#'   \item Base saturation (NH4OAc) < 50 percent in some part;
#'   \item Organic carbon >= 0.6 percent (or 0.6 absolute > C);
#'   \item Thickness: same rules as mollic (18 / 25 / 10 cm);
#'   \item Structure: peds <= 30 cm OR rupture-resistance <= moderately
#'         hard.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_bs Maximum BS (default 50 -- "less than 50 percent").
#' @param min_oc_pct Minimum OC (default 0.6).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 18-20.
#' @export
umbric_epipedon_usda <- function(pedon,
                                    max_bs     = 50,
                                    min_oc_pct = 0.6) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "umbric_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 18-20"
    ))
  }
  ms_top <- .mineral_soil_surface_cm(h)
  if (is.na(ms_top)) ms_top <- 0
  cand <- which(!is.na(h$designation) & grepl("^A", h$designation) &
                  !is.na(h$top_cm) & h$top_cm <= ms_top + 5)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "umbric_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface A horizon"),
      missing = "designation",
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  tests <- list()
  tests$color          <- test_mollic_color(h, candidate_layers = cand)
  tests$organic_carbon <- test_mollic_organic_carbon(h,
                                                         min_pct = min_oc_pct,
                                                         candidate_layers = cand)
  # BS test inverted: pass when bs_pct < max_bs in some part.
  bs_layers <- integer(0); bs_missing <- character(0)
  for (i in cand) {
    bs <- h$bs_pct[i]
    if (is.na(bs)) { bs_missing <- c(bs_missing, "bs_pct"); next }
    if (bs < max_bs) bs_layers <- c(bs_layers, i)
  }
  bs_passed <- if (length(bs_layers) > 0L) TRUE
               else if (length(cand) == length(bs_missing) &&
                          length(bs_missing) > 0L) NA
               else FALSE
  tests$base_saturation_low <- list(
    passed = bs_passed, layers = bs_layers, missing = unique(bs_missing)
  )

  passing_so_far <- intersect(intersect(tests$color$layers,
                                            tests$organic_carbon$layers),
                                  bs_layers)
  thk_threshold <- 18
  if (length(passing_so_far) > 0L) {
    sample_i <- passing_so_far[1]
    sandy <- isTRUE(.is_finer_than_loamy_fine_sand(h$clay_pct[sample_i],
                                                       h$silt_pct[sample_i],
                                                       h$sand_pct[sample_i])) == FALSE
    if (sandy) thk_threshold <- 25
  }
  tests$thickness <- test_minimum_thickness(h, min_cm = thk_threshold,
                                                candidate_layers = passing_so_far)
  tests$structure <- test_mollic_structure(h, candidate_layers = cand)

  passed <- isTRUE(tests$color$passed) &&
              isTRUE(tests$organic_carbon$passed) &&
              isTRUE(tests$base_saturation_low$passed) &&
              isTRUE(tests$thickness$passed) &&
              isTRUE(tests$structure$passed)
  DiagnosticResult$new(
    name = "umbric_epipedon_usda", passed = passed,
    layers = if (passed) tests$thickness$layers else integer(0),
    evidence = c(tests, list(thickness_threshold_cm = thk_threshold,
                                  candidate_layers = cand)),
    missing = unique(c(tests$color$missing %||% character(0),
                          tests$organic_carbon$missing %||% character(0),
                          tests$base_saturation_low$missing %||% character(0),
                          tests$thickness$missing %||% character(0),
                          tests$structure$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 18-20"
  )
}


# ---- Melanic Epipedon (KST 13ed, Ch 3, pp 15-16) ---------------------

#' Melanic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A thick, very dark, andic, organic-rich surface horizon
#' associated with volcanic-ash-derived soils in cool, humid
#' environments. Diagnostic for the Melanists / Melanudands great
#' groups of Andisols.
#'
#' KST 13ed required characteristics (Ch. 3, pp 15-16):
#' \itemize{
#'   \item Upper boundary at or within 30 cm of the mineral soil
#'         surface (or organic layer with andic properties);
#'   \item Cumulative thickness >= 30 cm within 40 cm with all of:
#'     \itemize{
#'       \item Andic soil properties throughout;
#'       \item Color value <= 2.5 moist AND chroma <= 2 throughout;
#'       \item Melanic index <= 1.70 (deferred -- specialized lab
#'             measurement);
#'       \item OC >= 6 percent (weighted) AND >= 4 percent (each
#'             layer).
#'     }
#' }
#'
#' Implementation notes (v0.8.x):
#' \itemize{
#'   \item Andic soil properties are tested via \code{andic_properties_usda}
#'         (v0.9; for v0.8 we approximate with bulk_density <=
#'         0.9 g/cm3 AND phosphate_retention >= 85\%).
#'   \item Melanic index (= 100 / (OC * 100 + 1) per KST appendix)
#'         is deferred -- requires UV-Vis spectroscopy.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-16.
#' @export
melanic_epipedon_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "melanic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-16"
    ))
  }
  # Surface candidate: top_cm <= 30 cm.
  cand <- which(!is.na(h$top_cm) & h$top_cm <= 30 &
                  !is.na(h$designation) & grepl("^A|^O", h$designation))
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "melanic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface A/O horizon within 30 cm"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-16"
    ))
  }
  miss <- character(0); details <- list(); passing <- integer(0)
  for (i in cand) {
    bd <- h$bulk_density_g_cm3[i]
    pret <- h$phosphate_retention_pct[i]
    vm <- h$munsell_value_moist[i]
    cm <- h$munsell_chroma_moist[i]
    oc <- h$oc_pct[i]
    # Andic approximation
    andic_ok <- !is.na(bd) && bd <= 0.9 &&
                  !is.na(pret) && pret >= 85
    # Color
    color_ok <- !is.na(vm) && vm <= 2.5 && !is.na(cm) && cm <= 2
    # OC: each layer >= 4%
    oc_ok <- !is.na(oc) && oc >= 4
    if (is.na(bd)) miss <- c(miss, "bulk_density_g_cm3")
    if (is.na(pret)) miss <- c(miss, "phosphate_retention_pct")
    if (is.na(oc)) miss <- c(miss, "oc_pct")
    if (is.na(vm)) miss <- c(miss, "munsell_value_moist")
    layer_ok <- isTRUE(andic_ok) && isTRUE(color_ok) && isTRUE(oc_ok)
    details[[as.character(i)]] <- list(idx = i,
                                          andic = andic_ok,
                                          color = color_ok,
                                          oc = oc_ok)
    if (layer_ok) passing <- c(passing, i)
  }
  # Cumulative thickness >= 30 cm in 40 cm
  thk_pass <- if (length(passing) > 0L)
                sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                      na.rm = TRUE)
              else 0
  # Weighted OC >= 6%
  if (length(passing) > 0L && thk_pass > 0) {
    oc_weighted <- sum(h$oc_pct[passing] *
                         (h$bottom_cm[passing] - h$top_cm[passing]),
                          na.rm = TRUE) / thk_pass
  } else {
    oc_weighted <- NA_real_
  }
  passed <- thk_pass >= 30 && !is.na(oc_weighted) && oc_weighted >= 6
  DiagnosticResult$new(
    name = "melanic_epipedon_usda", passed = passed,
    layers = if (passed) passing else integer(0),
    evidence = list(layer_details = details,
                      thickness_passing_cm = thk_pass,
                      oc_weighted_pct = oc_weighted,
                      candidate_layers = cand,
                      note = "v0.8: melanic_index deferred; andic approx via bd<=0.9 and Pret>=85"),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 15-16"
  )
}


# ---- Ochric Epipedon (KST 13ed, Ch 3, p 17) --------------------------

#' Ochric epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' The catch-all surface epipedon: any A horizon (or surface
#' horizon with pedogenic alteration) that does NOT meet the
#' specific requirements of histic, folistic, melanic, mollic,
#' umbric, anthropic or plaggen.
#'
#' KST 13ed (Ch 3, p 17): "The ochric epipedon fails to meet the
#' definitions for any of the other seven epipedons because it is
#' too thin or too dry, has too high a color value or chroma,
#' contains too little organic carbon, has too high an n value, has
#' too high a fluidity class or melanic index, or is both massive and
#' hard or harder when dry."
#'
#' Implementation: pass when none of the 6 implemented epipedons
#' (histic, folistic, melanic, mollic, umbric -- v0.8 implements 5;
#' anthropic / plaggen are deferred to v0.9 but rare) pass AND the
#' profile has at least one surface A horizon.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 17.
#' @export
ochric_epipedon_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "ochric_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 17"
    ))
  }
  ms_top <- .mineral_soil_surface_cm(h)
  if (is.na(ms_top)) ms_top <- 0
  surf_a <- which(!is.na(h$designation) & grepl("^A", h$designation) &
                    !is.na(h$top_cm) & h$top_cm <= ms_top + 5)
  if (length(surf_a) == 0L) {
    return(DiagnosticResult$new(
      name = "ochric_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface A horizon"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 17"
    ))
  }
  others <- list(
    histic   = histic_epipedon_usda(pedon),
    folistic = folistic_epipedon_usda(pedon),
    melanic  = melanic_epipedon_usda(pedon),
    mollic   = mollic_epipedon_usda(pedon),
    umbric   = umbric_epipedon_usda(pedon)
  )
  any_passes <- any(vapply(others,
                              function(d) isTRUE(d$passed),
                              logical(1)))
  passed <- !any_passes
  DiagnosticResult$new(
    name = "ochric_epipedon_usda", passed = passed,
    layers = if (passed) surf_a else integer(0),
    evidence = list(other_epipedons = vapply(others,
                                                function(d) isTRUE(d$passed),
                                                logical(1)),
                      surface_a_layers = surf_a),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 17"
  )
}
