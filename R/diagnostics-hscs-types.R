# =============================================================================
# v0.9.305 -- the gates of the 15 soil types of the Hungarian diagnostic
# classification (HSCS 2024, Micheli et al., second approximation, section
# 7). Each type after the first is "other soils that ...": a gate tests only
# its own requirements, and the order of the key (inst/rules/hscs2024/key.yaml)
# supplies the exclusions. The last type, Foldeskopar ("other soils"), is the
# key's default.
# =============================================================================

# LAP Laptalajok: an organic horizon 40 cm or more thick, its top at the
# surface or within 40 cm, of bulk density 1 g/cm3 or less (Hugo's decision,
# 2026-10-29: an organic horizon with no bulk density measured counts as 1 or
# less; a measured density above 1 fails).
peat_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h
  bd <- h$bulk_density_g_cm3
  ok <- .and3v(x$organic, ifelse(is.na(bd), TRUE, bd <= 1))
  v <- if (is.na(x$surface)) NA else .hscs_run(x, ok, top_max = x$surface + 40, min_th = 40)
  .hscs_result("peat_type_hscs", v, which(ok %in% TRUE), list(organic_rows = which(x$organic %in% TRUE)),
               c("oc_pct", "bulk_density_g_cm3", "designation"), "Key, Laptalajok")
}

# ANT Antropogen talajok: ploughing or turning to 50 cm or more; or, within
# 100 cm, 20 % or more artefacts by volume (weighted average), a continuous
# artificial material that water does not pass (or passes very slowly), or
# an anthropogenic horizon 50 cm or more thick (a surface horizon, 8.1: a run
# from the surface). Features a describer records when met: absent when not
# recorded, and below the description.
anthropogenic_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h; n <- x$n
  if (is.na(x$surface)) return(.hscs_result("anthropogenic_type_hscs", NA, where = "Key, Antropogen talajok"))
  zb <- x$surface + 100
  # 1. ploughing or turning to 50 cm or more: the base of the ploughed
  # horizons (p, not buried) in a run from the surface, or the depth of
  # mechanical mixing
  base <- x$surface
  for (i in x$ord) {
    if (abs(h$top_cm[i] - base) > 0.5 || !x$des$p[i] || x$des$buried[i] || is.na(h$bottom_cm[i])) break
    base <- h$bottom_cm[i]
  }
  mix <- suppressWarnings(as.numeric(.hscs_site1(x, "mechanical_mixing_depth_cm")))
  c1 <- base >= x$surface + 50 || (!is.na(mix) && mix >= 50)
  # 2a. artefacts, weighted average to 100 cm (or to shallower rock)
  art <- .hscs_artefacts_wmean(x)
  c2a <- !is.na(art) && art >= 20
  # 2b. a continuous artificial material
  r <- .hscs_rows_in(x, x$surface, zb)
  gm <- h$geomembrane_present %||% rep(NA, n)
  thm <- h$technic_hardmaterial_pct %||% rep(NA_real_, n)
  sgm <- .hscs_truthy(.hscs_site1(x, "geomembrane"))
  sgd <- suppressWarnings(as.numeric(.hscs_site1(x, "geomembrane_depth_cm")))
  c2b <- any(gm[r] %in% TRUE) || any(!is.na(thm[r]) & thm[r] >= 95) ||
         (isTRUE(sgm) && (is.na(sgd) || sgd < 100))
  # 2c. an anthropogenic horizon 50 cm or more thick from the surface
  an <- .hscs_anthropic_rows(x)
  c2c <- isTRUE(.hscs_run(x, an, top_max = x$surface + 0.5, min_th = 50, below_open = FALSE))
  v <- c1 || c2a || c2b || c2c
  .hscs_result("anthropogenic_type_hscs", v, which(an),
               list(deep_ploughing = c1, artefacts_pct = art, artificial_layer = c2b, anthropogenic_horizon = c2c),
               c("artefacts_pct", "layer_origin", "geomembrane_present"), "Key, Antropogen talajok")
}

# KOV Koves-sziklas vaztalajok: a continuous soil cover not deeper than 10 cm
# over continuous hard rock (rock outcrops may break it).
rocky_skeletal_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  rt <- .hscs_rock_top(x)
  deepest <- suppressWarnings(max(x$h$bottom_cm, x$h$top_cm, na.rm = TRUE))
  v <- if (is.na(x$surface)) NA else if (!is.na(rt)) rt <= x$surface + 10
       else if (is.finite(deepest) && deepest <= x$surface + 10) NA else FALSE
  .hscs_result("rocky_skeletal_type_hscs", v, evidence = list(rock_top = rt), missing = "designation",
               where = "Key, Koves-sziklas vaztalajok")
}

# KOZ Kozethatasu talajok: continuous hard rock within 25 cm (the type's
# description: not deeper than 25 cm); or, to 75 cm or to shallower hard
# rock, less than 20 % fine earth by volume (coarse fragments above 80 %),
# as a weighted average over that depth (as the WRB Leptosols read it);
# coarse fragments not recorded read as none.
rock_influenced_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  if (is.na(x$surface)) return(.hscs_result("rock_influenced_type_hscs", NA, where = "Key, Kozethatasu talajok"))
  rt <- .hscs_rock_top(x)
  c1 <- !is.na(rt) && rt <= x$surface + 25
  zb <- if (!is.na(rt)) min(rt, x$surface + 75) else x$surface + 75
  cf <- x$h$coarse_fragments_pct; cf <- ifelse(is.na(cf), 0, cf)
  w <- if (zb > x$surface) .hscs_wmean(x, cf, x$surface, zb) else c(lo = NA, hi = NA)
  c2 <- if (anyNA(w)) NA else if (w[["lo"]] > 80) TRUE else if (w[["hi"]] <= 80) FALSE else NA
  v <- if (c1) TRUE else c2
  .hscs_result("rock_influenced_type_hscs", v, evidence = list(rock_top = rt, coarse_fragments = w),
               missing = c("designation", "coarse_fragments_pct"), where = "Key, Kozethatasu talajok")
}

# SZN Szolonyec talajok: a solonetz horizon within 100 cm.
solonetz_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  ok <- .hscs_solonetz_rows(x)
  v <- if (is.na(x$surface)) NA else .hscs_run(x, ok, top_max = x$surface + 100, min_th = 0)
  .hscs_result("solonetz_type_hscs", v, which(ok %in% TRUE), list(ploughed = x$ploughed),
               c("na_cmol", "mg_cmol", "cec_cmol", "esp_pct", "structure_type"), "Key, Szolonyec talajok")
}

# SZK Szoloncsak talajok: a solonchak horizon (15 cm or more thick) within
# 50 cm.
solonchak_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  ok <- .hscs_solonchak_rows(x)
  v <- if (is.na(x$surface)) NA else .hscs_run(x, ok, top_max = x$surface + 50, min_th = 15)
  .hscs_result("solonchak_type_hscs", v, which(ok %in% TRUE), missing = "soluble_salts_pct",
               where = "Key, Szoloncsak talajok")
}

# DUZ Duzzado agyagtalajok: a swelling horizon (25 cm or more thick) within
# 100 cm; 30 % clay or more from the surface down to it; and cracks that open
# and close (recorded cracks; Hugo's decision: otherwise the slickensides of
# the swelling horizon stand for them). Each row that may begin a swelling
# horizon is read with the clay above it; below a description that does not
# end on rock, one may begin too.
swelling_clay_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h; r <- x$ord
  if (is.na(x$surface)) return(.hscs_result("swelling_clay_type_hscs", NA, where = "Key, Duzzado agyagtalajok"))
  sw <- .hscs_swelling_rows(x)
  clay30 <- .hscs_clay_at_least(x, 30)
  clay_to <- function(z) if (z <= x$surface + 0.5) TRUE else .hscs_all_in(x, clay30, x$surface, z)
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  verdicts <- c()
  for (k in seq_along(r)) {
    s <- r[k]
    if (sw[s] %in% FALSE || h$top_cm[s] >= x$surface + 100 - 0.25) next
    # the run of rows from s that hold (sure) or may hold (possible)
    sure_th <- 0; poss_th <- 0; sure_on <- TRUE; prev <- h$top_cm[s]
    for (i in r[k:length(r)]) {
      if (abs(h$top_cm[i] - prev) > 0.5) { poss_th <- Inf; break }
      if (sw[i] %in% FALSE) break
      th <- x$thick[i]
      if (sure_on && sw[i] %in% TRUE) sure_th <- sure_th + (if (is.na(th)) 0 else th) else sure_on <- FALSE
      poss_th <- poss_th + (if (is.na(th)) Inf else th)
      prev <- h$bottom_cm[i]
      if (is.na(prev)) break
    }
    # a run that reaches the end of a description not ending on rock may go on
    if (!is.na(prev) && is.finite(deepest) && abs(prev - deepest) <= 0.5 && !x$ends_on_rock) poss_th <- Inf
    run <- if (sure_th >= 25) TRUE else if (poss_th >= 25) NA else FALSE
    verdicts <- c(verdicts, .and3(c(run, clay_to(h$top_cm[s]))))
  }
  # a swelling horizon may begin below the description
  if (!x$ends_on_rock && is.finite(deepest) && deepest < x$surface + 100 - 0.25 &&
      !isFALSE(clay_to(deepest))) verdicts <- c(verdicts, NA)
  c12 <- if (!length(verdicts)) FALSE else .any3(verdicts)
  cw <- h$cracks_width_cm %||% rep(NA_real_, x$n)
  cracks <- if (any(!is.na(cw) & cw > 0)) TRUE else c12
  v <- .and3(c(c12, cracks))
  .hscs_result("swelling_clay_type_hscs", v, which(sw %in% TRUE),
               list(swelling_horizon = c12, cracks = cracks),
               c("clay_pct", "slickensides", "structure_type"), "Key, Duzzado agyagtalajok")
}

# RET Reti talajok: reductive conditions within 50 cm for part of the year
# (Hugo's decision, 2026-10-29: a positive dipyridyl test, methane, iron
# sulfides, or an l or r suffix on a horizon starting within 50 cm; none
# recorded: undecided; negative tests over the whole window: no); and a
# groundwater gley colour pattern in at least half of the soil matrix
# between 50 and 100 cm (the matrix ends at continuous hard rock).
meadow_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h
  if (is.na(x$surface)) return(.hscs_result("meadow_type_hscs", NA, where = "Key, Reti talajok"))
  rt <- .hscs_rock_top(x)
  red <- .hscs_reductive_rows(x)
  r50 <- x$ord[h$top_cm[x$ord] < x$surface + 50 - 0.25 & !x$rock[x$ord]]
  meth <- .hscs_truthy(.hscs_site1(x, "methane_odour"))
  z50 <- min(c(x$surface + 50, rt), na.rm = TRUE)
  c1 <- if (isTRUE(meth) || any(red[r50] %in% TRUE)) TRUE else
        if (z50 > x$surface && isTRUE(.hscs_all_in(x, ifelse(red %in% FALSE, TRUE, NA), x$surface, z50))) FALSE else NA
  zb <- min(c(x$surface + 100, rt), na.rm = TRUE)
  c2 <- if (zb <= x$surface + 50 + 0.25) FALSE else {
    s <- .hscs_share(x, .hscs_gw_gley_rows(x), x$surface + 50, zb)
    if (s[["lo"]] >= 0.5) TRUE else if (s[["hi"]] < 0.5) FALSE else NA
  }
  v <- .and3(c(c1, c2))
  .hscs_result("meadow_type_hscs", v, which(.hscs_gw_gley_rows(x) %in% TRUE),
               list(reductive_within_50 = c1, gley_pattern_50_100 = c2),
               c("childs_test", "designation", "munsell_hue_moist", "munsell_chroma_moist",
                 "redoximorphic_features_pct"), "Key, Reti talajok")
}

# KAR Karbonat talajok: within 50 cm, a horizon 15 cm or more thick with 25 %
# or more carbonate as CaCO3.
carbonate_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  ok <- .and3v(.hscs_calcareous_rows(x, min_pct = 25), x$mineral3)
  v <- if (is.na(x$surface)) NA else .hscs_run(x, ok, top_max = x$surface + 50, min_th = 15)
  .hscs_result("carbonate_type_hscs", v, which(ok %in% TRUE), missing = c("caco3_pct", "effervescence_class"),
               where = "Key, Karbonat talajok")
}

# MEZ Mezosegi talajok: 1. a humic horizon with 1 % organic carbon or more,
# sandy loam or finer, at most 20 % coarse fragments; 2. within 100 cm a
# layer 15 cm or more thick with secondary carbonates, at least weakly
# calcareous; 3. no clay accumulation horizon and 4. no continuous hard rock
# within 100 cm; 5. base saturation 50 % or more from the surface (every row
# from the surface to 100 cm; a calcareous row reads as saturated).
steppe_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h
  if (is.na(x$surface) || is.na(x$ms)) return(.hscs_result("steppe_type_hscs", NA, where = "Key, Mezosegi talajok"))
  sandy <- .hscs_sandy_rows(x)
  fine <- ifelse(is.na(sandy), NA, !sandy)
  cf <- h$coarse_fragments_pct
  cfok <- ifelse(is.na(cf), TRUE, cf <= 20)
  hum <- .and3v(.and3v(.hscs_humic_rows(x, oc_min = 1), fine), cfok)
  c1 <- .hscs_humic_horizon(x, hum)
  sec <- .and3v(.and3v(.hscs_secondary_carbonate_rows(x), .hscs_calcareous_rows(x)), x$mineral3)
  c2 <- .hscs_run(x, sec, top_max = x$surface + 100, min_th = 15)
  ca <- .hscs_clay_accum(x, 100)
  c3 <- if (is.na(ca)) NA else !ca
  rt <- .hscs_rock_top(x)
  c4 <- is.na(rt) || rt >= x$surface + 100
  bs <- ifelse(x$rock, TRUE, .hscs_bs50_rows(x))
  c5 <- .hscs_all_in(x, bs, x$surface, x$surface + 100)
  # an undescribed part below a description that ends (closed) above 100 cm
  # does not undo base saturation shown down to its end
  if (is.na(c5) && !anyNA(h$bottom_cm)) {
    deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
    if (is.finite(deepest) && deepest < x$surface + 100 &&
        isTRUE(.hscs_all_in(x, bs, x$surface, deepest))) c5 <- TRUE
  }
  v <- .and3(c(c1, c2, c3, c4, c5))
  .hscs_result("steppe_type_hscs", v, which(hum %in% TRUE),
               list(humic = c1, secondary_carbonates = c2, no_clay_accumulation = c3, no_rock = c4,
                    base_saturation = c5),
               c("oc_pct", "structure_type", "munsell_value_moist", "munsell_chroma_moist", "bs_pct",
                 "caco3_pct", "secondary_carbonates_pct", "clay_pct"), "Key, Mezosegi talajok")
}

# AGY Agyagbemosodasos talajok: a clay accumulation horizon with its top
# within 100 cm.
clay_illuvial_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  v <- if (is.na(x$surface)) NA else .hscs_clay_accum(x, 100)
  .hscs_result("clay_illuvial_type_hscs", v, which(.hscs_clay_accum_rows(x) %in% TRUE),
               list(ploughed = x$ploughed), c("clay_pct", "clay_films_amount", "designation"),
               "Key, Agyagbemosodasos talajok")
}

# HOM Homoktalajok: coarse sand, sand or loamy sand to 100 cm or to
# shallower hard rock or a cemented layer (gravel, limestone bank,
# sandstone); finer layers 15 cm thick or less in all; and some sandy
# material (a cemented layer near the surface leaves none).
sandy_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h
  if (is.na(x$surface)) return(.hscs_result("sandy_type_hscs", NA, where = "Key, Homoktalajok"))
  zb <- min(c(x$surface + 100, .hscs_rock_top(x), .hscs_cemented_top(x)), na.rm = TRUE)
  r <- .hscs_rows_in(x, x$surface, zb); r <- r[x$mineral[r]]
  ov <- .asc_overlap(h$top_cm[r], ifelse(is.na(h$bottom_cm[r]), zb, h$bottom_cm[r]), x$surface, zb)
  sandy <- .hscs_sandy_rows(x)[r]
  finer_lo <- sum(ov[sandy %in% FALSE]); finer_hi <- sum(ov[!(sandy %in% TRUE)])
  sandy_lo <- sum(ov[sandy %in% TRUE])
  # the part of the window the description does not reach may be finer
  org <- x$ord[x$organic[x$ord] %in% TRUE]
  org_ov <- sum(.asc_overlap(h$top_cm[org], ifelse(is.na(h$bottom_cm[org]), zb, h$bottom_cm[org]), x$surface, zb))
  gap <- if (zb > x$surface) max(0, (zb - x$surface) - sum(ov) - org_ov) else 0
  sandy_hi <- sum(ov[!(sandy %in% FALSE)]) + gap
  v <- if (finer_lo > 15 || sandy_hi <= 0) FALSE else if (finer_hi + gap <= 15 && sandy_lo > 0) TRUE else NA
  .hscs_result("sandy_type_hscs", v, r[sandy %in% TRUE], list(window_to = zb, finer_cm = c(finer_lo, finer_hi + gap)),
               c("field_texture", "clay_pct", "silt_pct", "sand_pct"), "Key, Homoktalajok")
}

# BAR Barnafoldek: a cambic horizon (15 cm or more thick) within 100 cm.
brown_earth_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon)
  ok <- .hscs_cambic_rows(x)
  v <- if (is.na(x$surface)) NA else .hscs_run(x, ok, top_max = x$surface + 100, min_th = 15)
  .hscs_result("brown_earth_type_hscs", v, which(ok %in% TRUE), missing = c("designation", "structure_grade",
               "munsell_value_moist", "munsell_chroma_moist", "caco3_pct"), where = "Key, Barnafoldek")
}

# HOR Hordalektalajok: stratified material within 50 cm (or to shallower
# hard rock); or, within 100 cm, a mineral layer with 0.2 % more organic
# carbon than the layer directly over it (a gap in the description above a
# layer leaves it open).
deposit_type_hscs <- function(pedon) {
  x <- .hscs_horizons(pedon); h <- x$h
  if (is.na(x$surface)) return(.hscs_result("deposit_type_hscs", NA, where = "Key, Hordalektalajok"))
  rt <- .hscs_rock_top(x)
  z50 <- min(c(x$surface + 50, rt), na.rm = TRUE)
  c1 <- if (z50 > x$surface) .hscs_stratified(x, x$surface, z50) else FALSE
  ab <- .hscs_above(x); oc <- h$oc_pct
  first <- x$ord[1L]
  r <- .hscs_rows_in(x, x$surface, x$surface + 100); r <- r[x$mineral[r]]
  inc <- vapply(r, function(i) {
    j <- ab[i]
    if (is.na(j)) return(if (identical(i, first)) FALSE else NA)
    if (!x$mineral[j]) return(FALSE)
    if (is.na(oc[i]) || is.na(oc[j])) NA else oc[i] - oc[j] >= 0.2 - 1e-9
  }, NA)
  # below a description that ends above 100 cm (not on rock) such a layer
  # may lie
  deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  below <- if (!x$ends_on_rock && (anyNA(h$bottom_cm) || !is.finite(deepest) || deepest < x$surface + 100 - 0.25)) NA else FALSE
  c2 <- .any3(c(inc, below))
  v <- .any3(c(c1, c2))
  .hscs_result("deposit_type_hscs", v, evidence = list(stratified_within_50 = c1, carbon_rise_within_100 = c2),
               missing = c("coarse_fragments_pct", "sand_pct", "oc_pct"), where = "Key, Hordalektalajok")
}
