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


# ---- Histic Epipedon (KST 13th ed., p. 14) --------------------------

# v0.9.418: the run of adjoining organic layers from the soil surface, for
# the histic and folistic epipedons: the layers surely organic (`yes`) and
# those that are or may be (`may`), with their thicknesses.
.usda_organic_surface_run <- function(pedon, org) {
  h <- pedon$horizons
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & !is.na(h$bottom_cm[ord])]
  run <- function(keep) {
    ly <- integer(0); bot <- NA_real_
    for (i in ord) {
      if (!keep[i]) break
      if (length(ly) && abs(h$top_cm[i] - bot) > 1e-6) break
      if (!length(ly) && h$top_cm[i] > min(h$top_cm, na.rm = TRUE) + 1e-6) break
      ly <- c(ly, i); bot <- h$bottom_cm[i]
    }
    list(layers = ly, thickness = if (length(ly)) bot - h$top_cm[ly[1]] else 0)
  }
  list(yes = run(org %in% TRUE), may = run(!(org %in% FALSE)))
}

# v0.9.418: the Ap path of the histic and folistic epipedons (KST 13th ed.,
# pp. 13-14): an Ap horizon at the mineral soil surface that, mixed to a
# depth of 25 cm, has 8 percent organic carbon or more. Returns the
# weighted organic carbon test (TRUE / FALSE / NA) and the layers mixed.
.usda_ap_mixed <- function(pedon, org, min_oc = 8) {
  h <- pedon$horizons
  ms <- .usda_mineral_surface(pedon, org)
  d <- .desg_master(h$designation, site = pedon$site)
  first <- which(!is.na(h$top_cm) & abs(h$top_cm - ms) < 1e-6 & !(org %in% TRUE))
  if (!length(first) || !any(grepl("^Ap", d[first]))) return(list(passed = FALSE, layers = integer(0)))
  k <- which(!is.na(h$top_cm) & h$top_cm < ms + 25 & h$bottom_cm > ms)
  w <- pmin(h$bottom_cm[k], ms + 25) - pmax(h$top_cm[k], ms)
  oc <- suppressWarnings(as.numeric(h$oc_pct[k]))
  # bounds with organic carbon not recorded (0 to 100 %)
  lo <- sum(ifelse(is.na(oc), 0, oc) * w) / 25
  hi <- sum(ifelse(is.na(oc), 100, oc) * w) / 25
  passed <- if (lo >= min_oc) TRUE else if (hi < min_oc) FALSE else NA
  list(passed = passed, layers = k)
}

#' Histic epipedon (USDA Soil Taxonomy, 13th edition, p. 14)
#'
#' A layer (one or more horizons) characterized by saturation for 30 days or
#' more (cumulative) and reduction in normal years, or artificially drained,
#' that either:
#' \enumerate{
#'   \item consists of organic soil material 20 to 60 cm thick with 75
#'     percent or more Sphagnum fibres or a moist bulk density under 0.1, or
#'     20 to 40 cm thick otherwise; or
#'   \item is an Ap horizon that, mixed to a depth of 25 cm, has 8 percent
#'     organic carbon or more.
#' }
#' v0.9.418: organic soil material is read layer by layer (12 percent
#' organic carbon, or an O or H designation without a measurement, reading
#' L10), as the run of organic layers from the soil surface; saturation is
#' read by \code{.usda_saturated30()} (drainage recorded, saturation days,
#' an H designation, or aquic conditions in a mineral layer); the Ap path
#' needs that saturation and mixes the upper 25 cm (fix A13 of the Andisols
#' audit). Up to v0.9.417 any H layers summing 20 cm with one measured
#' organic carbon of 12 percent passed, and any Ap within 25 cm with 8
#' percent, with no saturation.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_pct Kept for callers; organic soil material is 12 percent.
#' @param min_thickness_cm Minimum thickness (default 20 cm).
#' @param min_ap_oc_pct Minimum mixed organic carbon of the Ap path (8).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022). \emph{Keys to Soil Taxonomy},
#'   13th edition, USDA-NRCS, Washington DC, p. 14.
#' @noRd
histic_epipedon_usda <- function(pedon,
                                    min_oc_pct       = 12,
                                    min_thickness_cm = 20,
                                    min_ap_oc_pct    = 8) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 14"
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "histic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  org <- .usda_organic_status(pedon)
  r <- .usda_organic_surface_run(pedon, org)
  sat <- .usda_saturated30(pedon, org)
  bd <- suppressWarnings(as.numeric(h$bulk_density_g_cm3))
  sph <- suppressWarnings(as.numeric(h$sphagnum_fibres_pct %||% rep(NA, nrow(h))))
  light <- length(r$may$layers) > 0L &&
    all((!is.na(sph[r$may$layers]) & sph[r$may$layers] >= 75) |
          (!is.na(bd[r$may$layers]) & bd[r$may$layers] < 0.1))
  max_thk <- if (light) 60 else 40
  ty <- r$yes$thickness; tm <- r$may$thickness
  thick <- if (ty >= min_thickness_cm && ty <= max_thk && tm <= max_thk) TRUE
           else if (tm < min_thickness_cm || ty > max_thk) FALSE else NA
  path1 <- thick & (if (length(r$may$layers)) .any3u(sat[r$may$layers]) else FALSE)
  ap <- .usda_ap_mixed(pedon, org, min_ap_oc_pct)
  path2 <- ap$passed & (if (length(ap$layers)) .any3u(sat[ap$layers]) else FALSE)
  passed <- path1 | path2
  miss <- character(0)
  if (is.na(passed)) {
    if (anyNA(org[r$may$layers])) miss <- c(miss, "oc_pct")
    if (anyNA(sat[c(r$may$layers, ap$layers)])) miss <- c(miss, "water_saturation_days")
    if (is.na(ap$passed)) miss <- c(miss, "oc_pct")
  }
  DiagnosticResult$new(
    name = "histic_epipedon_usda", passed = passed,
    layers = if (isTRUE(path1)) r$yes$layers else if (isTRUE(path2)) ap$layers else integer(0),
    evidence = list(organic_layers = r$yes$layers, organic_thickness_cm = ty,
                    possible_thickness_cm = tm, max_thickness_cm = max_thk,
                    saturated = sat, path1_organic = path1, path2_ap = path2),
    missing = unique(miss), reference = ref)
}


# ---- Folistic Epipedon (KST 13th ed., p. 13) ------------------------

#' Folistic epipedon (USDA Soil Taxonomy, 13th edition, p. 13)
#'
#' A layer saturated for less than 30 days (cumulative) in normal years and
#' not artificially drained that either consists of organic soil material
#' 15 cm or more thick (20 cm or more for Sphagnum-rich or very light
#' material, which the 15 cm path already covers) or is an Ap horizon that,
#' mixed to 25 cm, has 8 percent organic carbon or more.
#' v0.9.418: organic soil material as for the histic epipedon (reading L10);
#' the saturation is the complement of \code{.usda_saturated30()}, so a site
#' recorded as drained fails and a layer with no saturation record is
#' undecided. Up to v0.9.417 any O layers summing 15 cm with one measured
#' organic carbon of 12 percent passed, with no saturation and no Ap path.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc_pct Kept for callers; organic soil material is 12 percent.
#' @param min_thickness_cm Minimum thickness (default 15 cm).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13th ed., p. 13.
#' @noRd
folistic_epipedon_usda <- function(pedon,
                                      min_oc_pct       = 12,
                                      min_thickness_cm = 15) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 13"
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "folistic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  org <- .usda_organic_status(pedon)
  r <- .usda_organic_surface_run(pedon, org)
  dry <- .usda_unsaturated30(pedon, org)
  ty <- r$yes$thickness; tm <- r$may$thickness
  thick <- if (ty >= min_thickness_cm) TRUE else if (tm < min_thickness_cm) FALSE else NA
  path1 <- thick & (if (length(r$may$layers)) .all3u(dry[r$may$layers]) else FALSE)
  ap <- .usda_ap_mixed(pedon, org, 8)
  path2 <- ap$passed & (if (length(ap$layers)) .all3u(dry[ap$layers]) else FALSE)
  passed <- path1 | path2
  miss <- character(0)
  if (is.na(passed)) {
    if (anyNA(org[r$may$layers]) || is.na(ap$passed)) miss <- c(miss, "oc_pct")
    if (anyNA(dry[c(r$may$layers, ap$layers)])) miss <- c(miss, "water_saturation_days")
  }
  DiagnosticResult$new(
    name = "folistic_epipedon_usda", passed = passed,
    layers = if (isTRUE(path1)) r$yes$layers else if (isTRUE(path2)) ap$layers else integer(0),
    evidence = list(organic_layers = r$yes$layers, organic_thickness_cm = ty,
                    possible_thickness_cm = tm, threshold_cm = min_thickness_cm,
                    under_30_days = dry, path1_organic = path1, path2_ap = path2),
    missing = unique(miss), reference = ref)
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
#'         paralithic contact (\code{thin_lithic_overlay} branch);
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
#' @noRd
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
#' @noRd
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


# ---- Melanic Epipedon (KST 13th ed., p. 14) --------------------------

#' Melanic epipedon (USDA Soil Taxonomy, 13th edition)
#'
#' A thick, very dark, andic, organic-rich horizon at or near the surface,
#' mostly in volcanic materials in cool, humid climates. Diagnostic for the
#' Melan- great groups of the Andisols.
#'
#' KST 13th ed., p. 14: an upper boundary at, or within 30 cm of, the mineral
#' soil surface or the upper boundary of an organic layer with andic soil
#' properties, whichever is shallower; and, in layers with a cumulative
#' thickness of 30 cm or more within a total thickness of 40 cm, andic soil
#' properties throughout, a moist colour value of 2.5 or less and chroma of 2
#' or less throughout, a melanic index of 1.70 or less throughout, and 6\%
#' or more organic carbon as a weighted average and 4\% or more in all layers.
#'
#' v0.9.417: the andic soil properties are those of
#' \code{andic_soil_properties_usda()} (up to v0.9.416 bulk density and
#' phosphate retention only, without Al + 1/2 Fe); no A or O designation is
#' required; the 30 cm are counted within a 40 cm span starting within 30 cm
#' of the top; and the answer is three-valued (a colour, carbon or andic
#' status not recorded leaves a layer undecided). soilKey records no melanic
#' index, so it is not tested.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13th ed., p. 14.
#' @noRd
melanic_epipedon_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 14"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "melanic_epipedon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref
    ))
  }
  st <- .andic_layer_status_usda(h)
  vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist
  ok <- st$status & (vm <= 2.5) & (cm <= 2)
  tops <- .andic_top_usda(pedon, st$status)
  r <- .melanic_span_usda(h, ok, tops$top)
  if (tops$alt < tops$top) {
    r2 <- .melanic_span_usda(h, ok, tops$alt)
    if (!identical(r$passed, r2$passed)) r$passed <- NA
  }
  miss <- character(0)
  if (is.na(r$passed)) {
    und <- which(is.na(ok) | is.na(h$oc_pct))
    miss <- unique(c(unlist(st$missing[und]),
                     if (any(is.na(vm[und]))) "munsell_value_moist",
                     if (any(is.na(cm[und]))) "munsell_chroma_moist",
                     if (any(is.na(h$oc_pct[und]))) "oc_pct"))
  }
  DiagnosticResult$new(
    name = "melanic_epipedon_usda", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = c(r, list(top_cm = tops$top,
                         note = "melanic index not recorded in soilKey; not tested")),
    missing = miss,
    reference = ref
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
#' @noRd
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


# ---- Anthropic and plaggen epipedons (KST 13th ed., pp. 12-13 and 17) ------
# v0.9.419 (audit I11). Both rest on field records of human-made material;
# not recorded, they are absent.

# Layers of human-transported material (KST 13th ed., Ch. 3): a caret in the
# designation (^Ap, ^C), a layer_origin or anthropic_material naming
# transported, dumped, dredged or plaggen material, or artifacts recorded in
# the layer (the field evidence KST gives for such material). TRUE / FALSE
# per layer.
.usda_htm_layers <- function(pedon) {
  h <- pedon$horizons
  d <- .usda_desig(h$designation)
  lo <- tolower(as.character(.usda_col(h, "layer_origin", NA_character_)))
  am <- tolower(as.character(.usda_col(h, "anthropic_material", NA_character_)))
  re <- "transport|plagg|dredg|spoil|landfill|dump|made ground|fill"
  d$caret | (!is.na(lo) & grepl(re, lo)) | (!is.na(am) & grepl(paste0(re, "|-tp\\b|tp$"), am)) |
    .usda_artifact_layers(pedon)
}

# Artifacts in each layer: artefacts_pct above 0, or a u (human-manufactured
# materials) in the designation. TRUE / FALSE per layer.
.usda_artifact_layers <- function(pedon) {
  h <- pedon$horizons
  d <- .usda_desig(h$designation)
  art <- .usda_col(h, "artefacts_pct", NA_real_)
  (!is.na(art) & art > 0) | (!is.na(d$suffix) & grepl("u", d$suffix))
}

# An anthropogenic landform or microfeature recorded at the site (landform,
# landform_element, an artificial terrace or plaggen cultivation). TRUE /
# FALSE: a field observation, absent when not recorded.
.usda_anthropogenic_landform <- function(pedon) {
  s <- pedon$site
  txt <- tolower(paste(as.character(s$landform %||% ""),
                       as.character(s$landform_element %||% ""), collapse = " "))
  grepl("anthropogen|man-made|manmade|human-made|artificial|plaggen|spoil|landfill|dump|midden|embankment",
        txt) || isTRUE(s$terraced) ||
    identical(tolower(as.character(s$deep_cultivation_kind %||% NA_character_))[1], "plaggen")
}

# The layers from the mineral soil surface down that all meet `flag`, in
# depth order (the surface layers of an epipedon).
.usda_surface_run <- function(h, flag) {
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$bottom_cm[ord] > ms]
  out <- integer(0)
  for (i in ord) { if (!isTRUE(flag[i])) break; out <- c(out, i) }
  out
}


#' Plaggen epipedon (USDA Soil Taxonomy, KST 13th ed., p. 17)
#'
#' A human-made mineral surface layer produced by long-continued manuring:
#' (1) on a locally raised landform, with artifacts or spade marks below 30
#' cm; (2) a value of 4 or less moist and 5 or less dry and chroma of 2 or
#' less; (3) 0.6 \% organic carbon or more; (4) 50 cm or more of
#' human-transported material; (5) moist 90 days or more a year (not met
#' under a recorded aridic regime unless irrigated). The plaggen material
#' is read from the field records: layers of human-transported material
#' (\code{.usda_htm_layers()}), a plaggen layer_origin, or
#' \code{site$deep_cultivation_kind} "plaggen" (then the A horizons from the
#' surface); a recorded plaggen cultivation also stands for the raised
#' landform and the spade marks. Nothing recorded: no plaggen epipedon.
#' Colour and organic carbon not recorded leave it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
plaggen_epipedon_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, plaggen epipedon (p. 17)"
  h <- pedon$horizons
  if (is.null(h) || !nrow(h))
    return(DiagnosticResult$new(name = "plaggen_epipedon_usda", passed = FALSE, reference = ref))
  d <- .usda_desig(h$designation)
  lo <- tolower(as.character(.usda_col(h, "layer_origin", NA_character_)))
  am <- tolower(as.character(.usda_col(h, "anthropic_material", NA_character_)))
  plagg_l <- (!is.na(lo) & grepl("plagg", lo)) | (!is.na(am) & grepl("plagg", am))
  plagg_s <- identical(tolower(as.character(pedon$site$deep_cultivation_kind %||% NA_character_))[1],
                       "plaggen")
  flag <- .usda_htm_layers(pedon) | plagg_l
  if (!any(flag) && plagg_s) flag <- !is.na(d$master) & grepl("A", d$master)
  L <- .usda_surface_run(h, flag)
  if (!length(L))
    return(DiagnosticResult$new(name = "plaggen_epipedon_usda", passed = FALSE,
             evidence = list(reason = "no human-transported or plaggen material recorded at the surface"),
             reference = ref))
  recorded <- plagg_s || any(plagg_l[L])
  i1 <- (recorded || .usda_anthropogenic_landform(pedon)) &&
        (recorded || any(.usda_artifact_layers(pedon)[L]))
  vm <- h$munsell_value_moist[L]; vd <- h$munsell_value_dry[L]; cm <- h$munsell_chroma_moist[L]
  i2 <- .usda_all3(c(ifelse(is.na(vm), NA, vm <= 4), ifelse(is.na(vd), NA, vd <= 5),
                     ifelse(is.na(cm), NA, cm <= 2)))
  oc <- h$oc_pct[L]
  i3 <- .usda_all3(ifelse(is.na(oc), NA, oc >= 0.6))
  i4 <- .usda_run_thickness(h, L) >= 50
  smr <- tolower(as.character(pedon$site$soil_moisture_regime %||% NA_character_))[1]
  i5 <- !(!is.na(smr) && smr %in% c("aridic", "torric") && !isTRUE(pedon$site$irrigated))
  passed <- .usda_all3(c(i1, i2, i3, i4, i5))
  DiagnosticResult$new(
    name = "plaggen_epipedon_usda", passed = passed, layers = L,
    evidence = list(raised_artifacts = i1, colour = i2, organic_carbon = i3,
                    thickness_50 = i4, moist = i5),
    missing = if (is.na(passed)) c("munsell_value_moist", "munsell_value_dry",
                                   "munsell_chroma_moist", "oc_pct") else character(0),
    reference = ref)
}


#' Anthropic epipedon (USDA Soil Taxonomy, KST 13th ed., pp. 12-13)
#'
#' A mineral surface layer formed in human-transported (or human-altered)
#' material on an anthropogenic landform or microfeature: (1) structural
#' units of 30 cm or less when dry (a recorded structure other than massive
#' or single grain; not recorded, undecided); (2) rock structure in less
#' than half the volume; (3) an n value under 0.7 or a nonfluid class (not
#' recorded, nonfluid, audit L16); (4) either directly over a root-limiting
#' layer or over horizons not derived from human-made material, or with
#' artifacts throughout; (5) 25 cm or more thick, or the whole soil above a
#' root-limiting layer within 25 cm. The human-transported material is read
#' from the field records (\code{.usda_htm_layers()}); at the surface it
#' makes a constructional anthropogenic landform or microfeature. Human-
#' altered material in place is not recorded by soilKey and is not read.
#' Nothing recorded: no anthropic epipedon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
anthropic_epipedon_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, anthropic epipedon (pp. 12-13)"
  h <- pedon$horizons
  if (is.null(h) || !nrow(h))
    return(DiagnosticResult$new(name = "anthropic_epipedon_usda", passed = FALSE, reference = ref))
  d <- .usda_desig(h$designation)
  htm <- .usda_htm_layers(pedon) & !(d$master %in% c("O", "H", "R"))
  L <- .usda_surface_run(h, htm)
  if (!length(L))
    return(DiagnosticResult$new(name = "anthropic_epipedon_usda", passed = FALSE,
             evidence = list(reason = "no human-transported material recorded at the surface"),
             reference = ref))
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  upper <- L[h$top_cm[L] < ms + 25]
  st <- tolower(as.character(.usda_col(h, "structure_type", NA_character_)))[upper]
  i1 <- .usda_all3(ifelse(is.na(st), NA, !grepl("massive|single", st)))
  rsp <- .usda_col(h, "rock_structure_pct", NA_real_)[upper]
  i2 <- .usda_all3(ifelse(is.na(rsp), !(!is.na(st) & grepl("rock|stratif", st)), rsp < 50))
  nv <- .usda_col(h, "n_value", NA_real_)[upper]
  fl <- tolower(as.character(.usda_col(h, "fluidity_class", NA_character_)))[upper]
  i3 <- .usda_all3(ifelse(!is.na(nv), nv < 0.7, ifelse(!is.na(fl), grepl("non", fl), TRUE)))
  ord <- order(h$top_cm)
  last <- L[length(L)]
  nxt <- ord[match(last, ord) + 1L]
  over <- !is.na(nxt)          # over a root-limiting layer or natural horizons
  art_all <- all(.usda_artifact_layers(pedon)[L])
  i4 <- over || art_all
  rl <- .usda_contact_top_cm(h)
  thk <- .usda_run_thickness(h, L)
  i5 <- thk >= 25 || (!is.na(rl) && rl - ms < 25 && max(h$bottom_cm[L]) >= rl)
  passed <- .usda_all3(c(i1, i2, i3, i4, i5))
  DiagnosticResult$new(
    name = "anthropic_epipedon_usda", passed = passed, layers = L,
    evidence = list(structure = i1, rock_structure = i2, nonfluid = i3,
                    anthropogenic_influence = i4, thickness = i5),
    missing = if (is.na(passed)) "structure_type" else character(0),
    reference = ref)
}
