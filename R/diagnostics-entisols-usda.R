# =============================================================
# USDA Soil Taxonomy 13ed -- Entisols helpers (Cap 8, pp 165-188)
# =============================================================
#
# Entisols are catch-all weakly-developed soils -- usually little
# or no profile development beyond a recently formed A horizon.
# 5 Suborders: Wassents (subaqueous), Aquents, Fluvents (flood
# plain irregular OC), Psamments (sandy), Orthents (catch-all).
# =============================================================


#' Wassent Suborder qualifier (subaqueous Entisol).
#' Pass when site$water_table_cm_above_surface > 0 (water column
#' permanently above the surface).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
wassent_qualifying_usda <- function(pedon) {
  wt <- pedon$site$water_table_cm_above_surface %||% NA_real_
  passed <- !is.na(wt) && wt > 0
  DiagnosticResult$new(
    name = "wassent_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(water_table_cm_above_surface = wt),
    missing = if (is.na(wt)) "site$water_table_cm_above_surface"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
  )
}


# ---- v0.9.419: the Entisol suborders of KST 13th ed., p. 165 ---------------

# Layers of the particle-size control section of a soil with no diagnostic
# subsurface horizon (KST 13th ed., Ch. 17): from 25 to 100 cm below the
# mineral soil surface, or to a densic, lithic or paralithic contact if
# shallower; from the mineral soil surface to the contact when the contact
# is within 36 cm.
.usda_pscs_layers <- function(h) {
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  rl <- .usda_contact_top_cm(h)
  if (!is.na(rl) && rl - ms <= 36) { lo <- ms; hi <- rl }
  else { lo <- ms + 25; hi <- if (!is.na(rl)) min(rl, ms + 100) else ms + 100 }
  d <- .usda_desig(h$designation)
  which(!is.na(h$top_cm) & h$bottom_cm > lo & h$top_cm < hi &
          !(d$master %in% c("R", "O", "H")) &
          !(d$master %in% "C" & grepl("[rd]", d$suffix)))
}

# Less than 35 % rock fragments and a texture class of loamy fine sand or
# coarser in all layers of the particle-size control section (KST 13th ed.,
# Psamments LC, p. 165; Psammaquents LBE; the Psammentic subgroups).
# Rock fragments not recorded: absent (a field estimate). TRUE / FALSE / NA.
.usda_psamment_texture <- function(pedon) {
  h <- pedon$horizons
  L <- .usda_pscs_layers(h)
  if (!length(L)) return(list(passed = NA, layers = integer(0), sandy = logical(0)))
  s <- .usda_texture_grades(h)$lfs_or_coarser[L]
  rf <- .usda_col(h, "coarse_fragments_pct", NA_real_)[L]
  v <- ifelse(!is.na(rf) & rf >= 35, FALSE, s)
  list(passed = .usda_all3(v), layers = L, sandy = s, rock_fragments = rf)
}

# The organic-carbon criteria of the Fluvents (LD.3, p. 165), the
# Fluvaquents (LBF) and the Fluventic subgroups: (a) 0.2 % or more organic
# carbon at 125 cm below the mineral soil surface; (b) an irregular decrease
# between 25 cm and 125 cm (or a densic, lithic or paralithic contact if
# shallower). An irregular decrease is a rise of more than 0.05 percentage
# points from one layer to the next one down (audit L15). TRUE / FALSE / NA
# each; (a) is undecided where the profile ends above 125 cm, (b) where a
# pair of adjoining layers lacks its organic carbon.
.usda_fluvent_oc <- function(pedon, rise = 0.05) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  rl <- .usda_contact_top_cm(h)
  end <- if (!is.na(rl) && rl < ms + 125) rl else ms + 125
  at <- which(!is.na(h$top_cm) & h$top_cm <= ms + 125 & h$bottom_cm > ms + 125)
  a <- if (!is.na(rl) && rl <= ms + 125) FALSE
       else if (!length(at)) NA
       else if (is.na(h$oc_pct[at[1]])) NA else h$oc_pct[at[1]] >= 0.2
  band <- which(!is.na(h$top_cm) & h$bottom_cm > ms + 25 & h$top_cm < end)
  band <- band[order(h$top_cm[band])]
  oc <- h$oc_pct[band]
  b <- if (length(band) < 2L) FALSE else .usda_any3(vapply(seq_len(length(band) - 1L),
         function(k) if (is.na(oc[k]) || is.na(oc[k + 1L])) NA else oc[k + 1L] - oc[k] > rise, NA))
  list(at_125 = a, irregular = b, layers = band)
}

# Human-transported material at the surface 50 cm or more thick, or an
# anthropogenic landform recorded (field observations: not recorded,
# absent).
.usda_htm_surface_cm <- function(pedon) {
  h <- pedon$horizons
  htm <- .usda_htm_layers(pedon)
  ord <- order(h$top_cm)
  thk <- 0
  for (i in ord) { if (!isTRUE(htm[i])) break; thk <- thk + max(h$bottom_cm[i] - h$top_cm[i], 0, na.rm = TRUE) }
  thk
}


#' Aquent Suborder (KST 13th ed., item LB, p. 165)
#'
#' LB.1: aquic conditions and sulfidic materials within 50 cm; LB.2:
#' permanent saturation and a reduced matrix in all horizons below 25 cm
#' (\code{water_saturation_days} of 365 with chroma of 2 or less or a g
#' designation; otherwise not met, since the saturation leaves the colours
#' LB.3 reads); LB.3: in the layer at 40 to 50 cm (or the layer above a
#' shallower densic, lithic or paralithic contact), aquic conditions (or
#' artificial drainage) and colours by texture class: finer than loamy fine
#' sand, neutral colours with zero chroma, chroma of 1 or less with a value
#' of 4 or more, or chroma of 2 or less with redox concentrations; loamy
#' fine sand or coarser, the six colour sets of LB.3.b; or a positive
#' alpha,alpha-dipyridyl reaction. Up to v0.9.418 the aquic proxy alone.
#' Redox concentrations and the dipyridyl test are field observations: not
#' recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquent_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  aqr <- aquic_conditions_usda(pedon, max_top_cm = ms + 50)
  aq <- .usda_pv(aqr)
  if (isTRUE(pedon$site$artificially_drained)) aq <- TRUE
  sulf <- .usda_pv(.usda_try(sulfidic_materials_usda(pedon, max_top_cm = ms + 50)))
  lb1 <- .and3v(aq, sulf)
  # LB.2
  sd <- .usda_col(h, "water_saturation_days", NA_real_)
  deep <- which(!is.na(h$top_cm) & h$bottom_cm > ms + 25)
  dg <- .usda_desig(h$designation)
  red <- (!is.na(h$munsell_chroma_moist) & h$munsell_chroma_moist <= 2) |
         (!is.na(dg$suffix) & grepl("g", dg$suffix))
  lb2 <- length(deep) > 0L && all(!is.na(sd[deep]) & sd[deep] >= 365 & red[deep])
  # LB.3
  rl <- .usda_contact_top_cm(h)
  L <- if (!is.na(rl) && rl < ms + 50) {
         up <- which(!is.na(h$bottom_cm) & h$bottom_cm <= rl)
         if (length(up)) up[which.max(h$bottom_cm[up])] else integer(0)
       } else which(!is.na(h$top_cm) & h$top_cm < ms + 50 & h$bottom_cm > ms + 40)
  sandy <- .usda_texture_grades(h)$lfs_or_coarser
  hr <- .usda_hue_rank(h$munsell_hue_moist)
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)
  col <- if (!length(L)) NA else .usda_any3(vapply(L, function(i) {
    cm <- h$munsell_chroma_moist[i]; v <- h$munsell_value_moist[i]
    hu <- toupper(as.character(h$munsell_hue_moist[i]))
    if (is.na(cm)) return(NA)
    rc <- !is.na(rdx[i]) && rdx[i] > 0
    neutral <- !is.na(hu) && grepl("^N", hu) && cm == 0
    v4 <- if (is.na(v)) NA else v >= 4
    fine <- .usda_any3(c(neutral, if (cm <= 1) v4 else FALSE, cm <= 2 && rc))
    red10 <- if (is.na(hr[i])) NA else hr[i] <= 20     # 10YR or redder
    gley <- !is.na(hu) && grepl("^5(GY|G|BG|B)$", hu)
    coarse <- .usda_any3(c(neutral,
      .and3v(red10, if (cm == 1) v4 else FALSE),
      .and3v(red10, cm <= 2 && rc),
      .and3v(.usda_not3(red10), cm <= 3 && rc),
      .and3v(.usda_not3(red10), cm == 1),
      gley))
    if (is.na(sandy[i])) { if (identical(fine, coarse)) fine else NA }
    else if (sandy[i]) coarse else fine
  }, NA))
  ct <- tolower(as.character(.usda_col(h, "childs_test", NA_character_)))
  dip <- any(!is.na(h$top_cm) & h$top_cm < ms + 50 & grepl("^pos", ct))
  lb3 <- .and3v(aq, .usda_any3(c(col, dip)))
  passed <- .usda_any3(c(lb1, lb2, lb3))
  DiagnosticResult$new(
    name = "aquent_qualifying_usda", passed = passed, layers = L,
    evidence = list(aquic = aq, sulfidic_50 = sulf, lb1 = lb1,
                    permanent_saturation = lb2, colour = col,
                    dipyridyl = dip, lb3 = lb3),
    missing = if (is.na(passed)) unique(c("munsell_chroma_moist", "munsell_value_moist",
                                          "munsell_hue_moist", aqr$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, item LB (p. 165)")
}


#' Fluvent Suborder (KST 13th ed., item LD, p. 165)
#'
#' No densic, lithic or paralithic contact within 25 cm of the mineral soil
#' surface, less than 50 cm of human-transported material at the surface,
#' not on an anthropogenic landform, a slope of less than 25 \% (not
#' recorded, read as met, as in the Cumulic subgroups of the Mollisols), the
#' organic-carbon criteria (0.2 \% or more at 125 cm, or an irregular
#' decrease from 25 cm: a rise of more than 0.05 points, audit L15), and a
#' soil temperature regime warmer than cryic, or a gelic or cryic one with
#' no gelic materials and a slope under 5 \% or under 15 \% volcanic glass
#' in some part of the particle-size control section (either of these two
#' settles item 4 whatever the regime). Up to v0.9.418 any rise of organic
#' carbon in the layers starting between 25 and 125 cm, a missing value read
#' as no rise, and a fluvic layer origin alone also passed. A surface mantle
#' of new non-alluvial material is not recorded and not tested.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
fluvent_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  rl <- .usda_contact_top_cm(h)
  no_contact <- is.na(rl) || rl - ms >= 25
  no_htm <- .usda_htm_surface_cm(pedon) < 50
  no_landform <- !.usda_anthropogenic_landform(pedon)
  sl <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA_real_))[1]
  slope25 <- is.na(sl) || sl < 25
  oc <- .usda_fluvent_oc(pedon)
  item3 <- .usda_any3(c(oc$at_125, oc$irregular))
  cold <- .usda_any3(c(.usda_pv(.usda_try(str_cryic_usda(pedon))),
                       .usda_pv(.usda_try(str_gelic_usda(pedon)))))
  gl <- .usda_col(h, "volcanic_glass_pct", NA_real_)[.usda_pscs_layers(h)]
  glass <- if (any(gl < 15, na.rm = TRUE)) TRUE else if (!length(gl) || anyNA(gl)) NA else FALSE
  bcond <- .usda_any3(c(if (is.na(sl)) NA else sl < 5, glass))
  item4 <- if (isTRUE(bcond)) TRUE else .usda_any3(c(.usda_not3(cold), .and3v(cold, bcond)))
  passed <- .usda_all3(c(no_contact, no_htm, no_landform, slope25, item3, item4))
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(item3)) miss <- c(miss, "oc_pct")
    if (is.na(item4)) miss <- c(miss, "site$soil_temperature_regime", "site$slope_pct")
  }
  DiagnosticResult$new(
    name = "fluvent_qualifying_usda", passed = passed, layers = oc$layers,
    evidence = list(no_contact_25 = no_contact, no_htm_50 = no_htm,
                    no_anthropogenic_landform = no_landform, slope_under_25 = slope25,
                    oc_125 = oc$at_125, oc_irregular = oc$irregular,
                    temperature_item4 = item4),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, item LD (p. 165)")
}


#' Fluvaquents (KST 13th ed., item LBF, p. 166)
#'
#' Aquents with less than 50 cm of human-transported material at the
#' surface, not on an anthropogenic landform, a slope under 25 \% (not
#' recorded, read as met) and 0.2 \% organic carbon or more at 125 cm or an
#' irregular decrease from 25 cm (a rise of more than 0.05 points, audit
#' L15). v0.9.419: up to v0.9.418 \code{fluvaquentic_usda()}, which also
#' asked for aquic conditions (already met by every Aquent) and read only
#' the layers starting at 25 to 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
fluvaquent_qualifying_usda <- function(pedon) {
  no_htm <- .usda_htm_surface_cm(pedon) < 50
  no_landform <- !.usda_anthropogenic_landform(pedon)
  sl <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA_real_))[1]
  slope25 <- is.na(sl) || sl < 25
  oc <- .usda_fluvent_oc(pedon)
  item3 <- .usda_any3(c(oc$at_125, oc$irregular))
  passed <- .usda_all3(c(no_htm, no_landform, slope25, item3))
  DiagnosticResult$new(
    name = "fluvaquent_qualifying_usda", passed = passed, layers = oc$layers,
    evidence = list(no_htm_50 = no_htm, no_anthropogenic_landform = no_landform,
                    slope_under_25 = slope25, oc_125 = oc$at_125,
                    oc_irregular = oc$irregular),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, item LBF (p. 166)")
}


#' Psamment Suborder (KST 13th ed., item LC, p. 165)
#'
#' Less than 35 \% rock fragments and a texture class of loamy fine sand or
#' coarser in all layers of the particle-size control section. Up to
#' v0.9.418 half of the layers starting above 100 cm, by the laboratory
#' texture alone; the Psamments are now keyed before the Fluvents, as in
#' the text.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
psamment_qualifying_usda <- function(pedon) {
  r <- .usda_psamment_texture(pedon)
  DiagnosticResult$new(
    name = "psamment_qualifying_usda", passed = r$passed, layers = r$layers,
    evidence = list(lfs_or_coarser = r$sandy, rock_fragments = r$rock_fragments),
    missing = if (is.na(r$passed)) c("clay_pct", "sand_pct", "texture_class_usda") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, item LC (p. 165)")
}


#' Quartzipsamment helper (Quartzipsamments: >= 95\% resistant minerals)
#'
#' KST 13ed Ch 8 (p 357) defines Quartzipsamments as Psamments where
#' "a weighted average of the resistant minerals in the 0.02-2.0 mm
#' fraction is at least 95 percent". Resistant minerals are dominated
#' by quartz; the practical proxy is a profile that is uniformly
#' sandy with very little clay AND minimal coarse fragments AND no
#' explicit mineralogical evidence of weatherable minerals.
#'
#' v0.9.31 broadens the proxy from "clay <= 5 % AND coarse_fragments
#' <= 5 %" (which under-detected; only 0/14 KSSL Quartzipsamments
#' were caught) to:
#'
#' \itemize{
#'   \item \code{clay_pct <= 10} (loamy sands and finer sands all
#'         qualify -- the 5 % cutoff was too strict);
#'   \item \code{sand_pct >= 80} (sand-dominated texture -- a NEW
#'         requirement, since clay alone is not sufficient);
#'   \item \code{coarse_fragments_pct <= 15} (some coarse fragments
#'         tolerated; 5 % was overly strict);
#'   \item at least 50 % of in-range layers must satisfy all three
#'         (preserved from v0.8).
#' }
#'
#' This still excludes Loamy Psamments and Sandy-Loamy Psamments
#' (Udipsamments / Ustipsamments fallthroughs) by requiring sand >=
#' 80 %; it captures the mineralogical signal indirectly via the
#' near-pure-sand texture.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
quartzipsamment_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "quartzipsamment_qualifying_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8 (p 357)"
    ))
  }
  cl <- h$clay_pct[cand]
  sd <- h$sand_pct[cand]
  cf <- h$coarse_fragments_pct[cand]

  # Layer is Quartzipsamment-compatible if all three conditions met.
  layer_ok <- !is.na(cl) & cl <= 10 &
                !is.na(sd) & sd >= 80 &
                (is.na(cf) | cf <= 15)
  passing <- cand[layer_ok]
  passed <- length(passing) >= 0.5 * length(cand)

  DiagnosticResult$new(
    name = "quartzipsamment_qualifying_usda", passed = passed,
    layers = passing,
    evidence = list(
      threshold_clay_max = 10,
      threshold_sand_min = 80,
      threshold_cf_max   = 15,
      total_layers       = length(cand),
      passing_layers     = length(passing)
    ),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8 (p 357)"
  )
}


#' Hydric Aquent helper (Hydraquents)
#' Pass when surface 0-50 has high water content (n value high).
#' v0.8 proxy: water_content_1500kpa >= 80\% in surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
hydraquent_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  # KST 13ed Ch. 8 (p. 168): in ALL horizons between 20 and 50 cm, BOTH an
  # n value > 0.7 (proxied here by water_content_1500kpa >= 80%) AND 8% or
  # more clay in the fine-earth fraction. (Corrected: window is 20-50 cm not
  # 0-50; the clay condition was missing; the logic is "all", not "any".)
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 20 & h$top_cm < 50)
  wr <- h$water_content_1500kpa[cand]
  cl <- h$clay_pct[cand]
  miss <- character(0)
  if (length(cand) == 0L || all(is.na(wr))) miss <- c(miss, "water_content_1500kpa")
  if (length(cand) == 0L || all(is.na(cl))) miss <- c(miss, "clay_pct")
  each_ok <- (!is.na(wr) & wr >= 80) & (!is.na(cl) & cl >= 8)
  passed  <- length(cand) > 0L && all(each_ok)
  DiagnosticResult$new(
    name = "hydraquent_qualifying_usda", passed = passed,
    layers = cand[each_ok],
    evidence = list(n_value_proxy_pct = 80, min_clay_pct = 8,
                    depth_window = "20-50 cm", n_layers = length(cand)),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8, p 168"
  )
}
