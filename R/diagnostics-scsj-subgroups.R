# =============================================================================
# v0.9.323 -- the subgroups of the Soil Classification System of Japan (SCSJ
# 2017, chapter III, pp. 7-16): 120 subgroups under the 26 groups. Within a
# group each subgroup after the first is "soils other than the above", so a
# gate tests only its own requirements; the Haplic subgroup (and Mineral,
# Lowland, Low-moor, Mottled) is the default. Most gates are generic, with
# their depths and features as parameters in inst/rules/scsj2017/subgroups/.
# Three-valued.
# =============================================================================

# ---- generic helpers ------------------------------------------------------------------

# Feature row flags for the generic wetness tests.
#   gw: the groundwater wetness feature (on paddy land not in the paddy
#   horizons); sw: the surface-water one; wet: either (the kind not needed);
#   gley: the gley feature; gley_layer: gley layers (10 cm or more); gw_gley:
#   groundwater gley layers; pseudogley: the pseudogley horizon. When both gw
#   and sw are asked, a wetness feature of an unknown kind counts too (the
#   key accepts either).
.scsj_feature_rows <- function(x, features, f = .scsj_horizon_flags(x)) {
  wet <- .scsj_wetness_rows(x)
  out <- rep(FALSE, x$n)
  for (k in features) {
    v <- switch(k,
      gw = .scsj_groundwater_rows(x, f), sw = wet$surface, wet = wet$any, gley = wet$gley,
      gley_layer = f$gley_layer, pseudogley = f$pseudogley,
      gw_gley = .scsj_groundwater_gley_rows(x, f),
      stop("unknown feature ", k))
    out <- .scsj_or(out, v)
  }
  if (all(c("gw", "sw") %in% features)) {
    gw <- .scsj_groundwater_rows(x, f)
    # the kind is unknown but either counts; a paddy horizon on paddy land
    # counts only by its surface-water feature
    either <- ifelse(gw %in% FALSE & wet$any %in% TRUE & !(wet$surface %in% c(TRUE, NA)) & isTRUE(x$paddy),
                     wet$surface, wet$any)
    out <- .scsj_or(out, either)
  }
  out
}

# The rows of groundwater gley layers: gley layers (adjacent gley horizons,
# 10 cm or more) reaching 75 cm or deeper.
.scsj_groundwater_gley_rows <- function(x, f = .scsj_horizon_flags(x)) {
  rows <- f$gley
  out <- ifelse(rows %in% FALSE, FALSE, NA)
  for (r in .scsj_runs(x, rows, bridge = TRUE)) {
    bottom <- if (r$at_end) Inf else r$bottom
    if (r$poss < 10 || bottom < 75) out[r$rows] <- FALSE
  }
  for (r in .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE)))
    if (r$sure >= 10 && r$deep >= 75) out[r$rows] <- TRUE
  out
}

# Generic: the top of a layer with any of `features` within [from, to] cm,
# the layer `thick` cm or more. With overlap = TRUE, a layer with the
# feature anywhere between `from` and `to` (the Epi-gray subgroup, whose
# text names no top).
wet_top_scsj <- function(pedon, features = "gw", from = -Inf, to = 50, thick = 0, overlap = FALSE) {
  x <- .scsj_profile(pedon)
  name <- "wet_top_scsj"
  if (is.na(x$surface)) return(.scsj_no_surface(name, x, "III"))
  features <- unlist(features)
  rows <- .scsj_feature_rows(x, features)
  passed <- if (isTRUE(overlap)) {
    cm <- .scsj_cum(x, rows, max(from, 0), to)
    if (cm$sure > 0) TRUE else if (cm$poss <= 0) FALSE else NA
  } else .scsj_top_in(x, rows, from, to, thick)
  .scsj_result(name, passed, which(rows %in% TRUE),
               list(features = paste(features, collapse = "+"), from = from, to = to, thick = thick),
               if (is.na(passed)) c("redoximorphic_features_pct", "redox_segregations_location", "childs_test") else character(0),
               "III (subgroups)")
}

# Generic: no layer with any of `features` (`thick` cm or more) whose top is
# within `to` cm, and no layer of 10 cm or more with the groundwater wetness
# feature (the Aeric Pseudogley soils). A wetness feature of an unknown kind,
# 10 cm or more, is one of the two; on paddy land a paddy greyed horizon
# within the depth also rules it out (Hugo, 2026-10-31).
no_wet_top_scsj <- function(pedon, features = c("pseudogley"), to = 30, thick = 0, gw_thick = 10) {
  x <- .scsj_profile(pedon)
  name <- "no_wet_top_scsj"
  if (is.na(x$surface)) return(.scsj_no_surface(name, x, "III G, pp. 13-14"))
  f <- .scsj_horizon_flags(x)
  a <- .scsj_top_in(x, .scsj_feature_rows(x, unlist(features), f), -Inf, to, thick)
  b <- .scsj_top_in(x, .scsj_feature_rows(x, "gw", f), -Inf, to, gw_thick)
  wet <- .scsj_wetness_rows(x)
  any_rows <- ifelse(isTRUE(x$paddy) & f$paddy_grey %in% TRUE, FALSE, wet$any)
  c <- .scsj_top_in(x, any_rows, -Inf, to, 10)
  d <- .scsj_top_in(x, f$paddy_grey, -Inf, to, 0)
  passed <- .scsj_not(.scsj_or3(a, b, c, d))
  .scsj_result(name, passed, integer(0), list(pseudogley_within = a, groundwater_10_within = b,
                                              wetness_10_within = c, paddy_greyed_within = d),
               if (is.na(passed)) c("redoximorphic_features_pct", "redox_segregations_location") else character(0),
               "III G, pp. 13-14")
}

# Generic: `cum` cm or more (cumulative) of peat material within `within` cm.
peat_within_scsj <- function(pedon, within = 100, cum = 25) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("peat_within_scsj", x, "III"))
  passed <- .scsj_at_least(.scsj_cum(x, x$peat, 0, within), cum)
  .scsj_result("peat_within_scsj", passed, which(x$peat %in% TRUE),
               list(peat_cm = .scsj_cum(x, x$peat, 0, within)$sure, within = within),
               if (is.na(passed)) "oc_pct" else character(0), "III (subgroups)")
}

# ---- surface horizons (IV.1) -----------------------------------------------------------

# The humic (3 % carbon) or polyhumic (6 %) surface horizon: from the soil
# surface down, surface horizons (A, or undesignated) of moist value 3 or
# less and chroma 3 or less (not 3/3), with the carbon, mineral (not peat or
# organic material), `thick` cm or more in all; layers failing the colour or
# carbon sandwiched inside it count as long as they total less than 10 cm
# (the note to the definitions); a failing layer at the top ends it. colour
# = "brown": every requirement but the dark colour, with moist value and
# chroma both 3 or more (the Brown-humic subgroups; chapter VI, Hugo,
# 2026-10-31). Three-valued: gaps, a horizon without a bottom and the end of
# a description (not on rock) count as possible.
.scsj_humic_surface <- function(x, oc_min = 3, thick = 25, colour = TRUE) {
  if (is.na(x$surface)) return(NA)
  v <- x$colour$value; c <- x$colour$chroma
  dark <- ifelse(is.na(v) | is.na(c), NA, v <= 3 & c <= 3 & !(v == 3 & c == 3))
  brown <- .scsj_and(ifelse(is.na(v), NA, v >= 3), ifelse(is.na(c), NA, c >= 3))
  ocv <- ifelse(is.na(x$oc), NA, x$oc >= oc_min)
  surface_role <- is.na(x$role) | grepl("^A", x$role %||% "")
  meet <- .scsj_and(.scsj_and(ocv, x$mineral), surface_role)
  meet <- if (identical(colour, "brown")) .scsj_and(meet, brown) else if (isTRUE(colour)) .scsj_and(meet, dark) else meet
  rows <- x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= -0.5]
  e <- .scsj_ends(x)
  walk <- function(possible) {
    got <- 0; fail <- 0; pend <- 0; prev <- 0; started <- FALSE
    for (i in rows) {
      gap <- x$dtop[i] - prev
      if (gap > 0.5) { if (!possible) return(got); got <- got + gap; started <- TRUE }
      ok <- if (possible) !(meet[i] %in% FALSE) else meet[i] %in% TRUE
      th <- x$thick[i]
      if (ok) {
        if (is.na(th)) return(if (possible) Inf else got)
        got <- got + th; fail <- fail + pend; pend <- 0; started <- TRUE
      } else {
        if (!started) return(got)
        if (is.na(th)) return(got)
        pend <- pend + th
        if (fail + pend >= 10) return(got)
      }
      prev <- x$dbot[i]
    }
    # the walk reached the end of the description
    if (possible && !e$on_rock) Inf else got
  }
  sure <- walk(FALSE); poss <- walk(TRUE)
  if (sure >= thick) TRUE else if (poss < thick) FALSE else NA
}

# Generic: a humic or polyhumic surface horizon `thick` cm or more (Humic
# 25; Cumulic 50); colour = "brown" for Brown-humic.
humic_surface_scsj <- function(pedon, thick = 25, colour = TRUE, oc_min = 3) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("humic_surface_scsj", x, "IV.1, p. 23"))
  col <- if (isFALSE(colour) || identical(colour, "brown")) "brown" else TRUE
  passed <- .scsj_humic_surface(x, oc_min, thick, col)
  .scsj_result("humic_surface_scsj", passed, integer(0), list(thick = thick, colour = col),
               if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist") else character(0),
               "IV.1, pp. 23-24")
}

# The buried humic layer (IV.1): moist value 3 or less and chroma 3 or less
# (not 3/3), 3 % carbon or more, 10 cm or more (a run of such horizons),
# under 10 cm or more of layers (below the soil surface) with a value at
# least 1 higher and at least 1 % less carbon than the layer's top horizon;
# its top within `within` cm.
buried_humic_scsj <- function(pedon, within = 50) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("buried_humic_scsj", x, "IV.1, p. 24"))
  v <- x$colour$value; c <- x$colour$chroma
  dark <- ifelse(is.na(v) | is.na(c), NA, v <= 3 & c <= 3 & !(v == 3 & c == 3))
  cand <- .scsj_and(.scsj_and(dark, ifelse(is.na(x$oc), NA, x$oc >= 3)), .scsj_not(x$organic))
  ord <- x$ord[!is.na(x$dtop[x$ord]) & x$dtop[x$ord] >= -0.5]
  over_ok <- function(i) {
    # the 10 cm directly above row i, lighter and leaner than it
    k <- match(i, ord); if (is.na(k) || k == 1L) return(FALSE)
    th <- 0; res <- TRUE
    for (j in rev(ord[seq_len(k - 1L)])) {
      if (is.na(x$thick[j])) return(NA)
      lighter <- if (is.na(v[j]) || is.na(v[i])) NA else v[j] >= v[i] + 1
      leaner <- if (is.na(x$oc[j]) || is.na(x$oc[i])) NA else x$oc[j] <= x$oc[i] - 1
      cond <- .scsj_and(lighter, leaner)
      if (isFALSE(cond)) return(FALSE)
      if (is.na(cond)) res <- NA
      th <- th + x$thick[j]
      if (th >= 10) return(res)
    }
    FALSE
  }
  flags <- rep(FALSE, x$n)
  for (r in .scsj_runs(x, cand, bridge = FALSE)) {
    o <- over_ok(r$rows[1L])
    flags[r$rows] <- if (isFALSE(o)) FALSE else ifelse(cand[r$rows] %in% TRUE & isTRUE(o), TRUE, NA)
  }
  passed <- .scsj_top_in(x, flags, -Inf, within, 10)
  .scsj_result("buried_humic_scsj", passed, which(flags %in% TRUE), list(within = within),
               if (is.na(passed)) c("oc_pct", "munsell_value_moist") else character(0), "IV.1, p. 24")
}

# ---- materials and features of the subgroups ---------------------------------------

# Generic: the top of layers of alluvial deposits within `within` cm,
# `cum` cm or more of them. mode "window": cumulative within the depth (the
# Wet Andosols); "top": the top within the depth, cumulative within the depth
# the system classifies (1 m: the Allophanic Andosols).
endofluvic_scsj <- function(pedon, within = 75, cum = 25, mode = "window") {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("endofluvic_scsj", x, "III C"))
  rows <- x$orig$alluvial
  passed <- if (identical(mode, "window")) .scsj_at_least(.scsj_cum(x, rows, 0, within), cum) else
    .scsj_and3(.scsj_top_in(x, rows, -Inf, within, 0), .scsj_at_least(.scsj_cum(x, rows, 0, 100), cum))
  .scsj_result("endofluvic_scsj", passed, which(rows), list(within = within, mode = mode),
               character(0), "III C, pp. 8-10")
}

# The buried non-kuroboku upland soil (IV.4): a buried soil other than one of
# the Organic Soils, Andosols or Fluvic soils, its top within `within` cm.
# Read as layers buried (a b of the notation, or a lithologic number deeper
# than the surface's) that have neither kuroboku feature, are not peat
# material and not alluvial deposits; a row whose designation cannot be read
# may be one.
thapto_upland_scsj <- function(pedon, within = 50) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("thapto_upland_scsj", x, "IV.4, p. 30"))
  first <- if (length(x$ord)) x$des$lith[x$ord[1L]] else NA
  buried <- x$des$buried | (!is.na(x$des$lith) & !is.na(first) & x$des$lith > first)
  buried <- ifelse(x$role %in% "other", NA, buried)
  mat <- .scsj_and(.scsj_and(.scsj_not(.scsj_or(x$kb, x$ikb_raw)), .scsj_not(x$peat)), !x$orig$alluvial)
  rows <- .scsj_and(buried, mat)
  rows <- ifelse(x$mineral %in% FALSE, FALSE, rows)
  passed <- .scsj_top_in(x, rows, -Inf, within, 0)
  .scsj_result("thapto_upland_scsj", passed, which(rows %in% TRUE), list(within = within),
               if (is.na(passed)) c("phosphate_absorption_coef", "designation") else character(0), "IV.4, p. 30")
}

# Generic: `cum` cm or more (cumulative) of layers with the kuroboku feature
# within `within` cm (Thapto-andic).
kuroboku_within_scsj <- function(pedon, within = 100, cum = 25) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("kuroboku_within_scsj", x, "III E"))
  passed <- .scsj_at_least(.scsj_cum(x, x$kb, 0, within), cum)
  .scsj_result("kuroboku_within_scsj", passed, which(x$kb %in% TRUE), list(within = within),
               if (is.na(passed)) "phosphate_absorption_coef" else character(0), "III E, p. 12")
}

# Andic subgroups: within 50 cm, 25 cm or more of layers with a phosphate
# absorption coefficient of 1000 mg P2O5/100 g or more (P retention 60 % or
# more, or Alo + 1/2 Feo 1.2 % or more), read with the precedence of the
# kuroboku feature.
andic_subgroup_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("andic_subgroup_scsj", x, "III F, I"))
  h <- x$h
  pac <- .scsj_col(h, "phosphate_absorption_coef"); pr <- .scsj_col(h, "phosphate_retention_pct")
  alo <- .scsj_col(h, "al_ox_pct"); feo <- .scsj_col(h, "fe_ox_pct")
  af <- ifelse(is.na(alo) | is.na(feo), NA_real_, alo + feo / 2)
  sub_pass <- (!is.na(pr) & pr >= 60) | (!is.na(af) & af >= 1.2)
  rows <- ifelse(!is.na(pac), pac >= 1000, ifelse(sub_pass, TRUE, ifelse(!is.na(pr) | !is.na(af), FALSE, NA)))
  passed <- .scsj_at_least(.scsj_cum(x, rows, 0, 50), 25)
  .scsj_result("andic_subgroup_scsj", passed, which(rows %in% TRUE), list(),
               if (is.na(passed)) c("phosphate_absorption_coef", "phosphate_retention_pct") else character(0),
               "III F (p. 13), I (p. 14)")
}

# The sulfuric feature (IV.2) or potential sulfuric material (IV.3) rows:
# jarosite mottles with pH(H2O) under 4.0; or pH(H2O) 4.0 or more with a pH
# after hydrogen peroxide (ph_h2o2) under 3.0 (saturation is that of the
# Gley and Gray Fluvic soils, where the subgroup is keyed).
.scsj_sulfuric_rows <- function(x) {
  h <- x$h
  jar <- as.logical(.scsj_col(h, "jarosite_present", NA))
  ph <- .scsj_col(h, "ph_h2o"); p2 <- .scsj_col(h, "ph_h2o2")
  sulf <- .scsj_and(jar, ifelse(is.na(ph), NA, ph < 4.0))
  pot <- .scsj_and(ifelse(is.na(ph), NA, ph >= 4.0), ifelse(is.na(p2), NA, p2 < 3.0))
  # neither recorded: none (recorded when present)
  sulf <- ifelse(is.na(jar), FALSE, sulf); pot <- ifelse(is.na(p2), FALSE, pot)
  .scsj_or(sulf, pot)
}

# Thionic subgroups: the top within 75 cm of layers, 15 cm or more
# (cumulative, within the 1 m the system classifies), with the sulfuric
# feature or potential sulfuric material.
thionic_scsj <- function(pedon, within = 75, cum = 15) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("thionic_scsj", x, "III E, p. 11"))
  rows <- .scsj_sulfuric_rows(x)
  passed <- .scsj_and3(.scsj_top_in(x, rows, -Inf, within, 0), .scsj_at_least(.scsj_cum(x, rows, 0, 100), cum))
  .scsj_result("thionic_scsj", passed, which(rows %in% TRUE), list(), character(0), "III E, p. 11")
}

# The paddy reverse gley layer (IV.1): on paddy land, a gley layer starting
# directly under the plough layer (the plough layer itself not counted) and
# ending above 75 cm.
paddy_reverse_gley_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("paddy_reverse_gley_scsj", x, "IV.1, p. 20"))
  f <- .scsj_horizon_flags(x)
  ap <- .scsj_plough_rows(x)
  pad <- x$paddy
  res <- function(v) .scsj_result("paddy_reverse_gley_scsj", v, integer(0), list(paddy = pad),
                                  if (is.na(v)) c("land_use", "childs_test") else character(0), "IV.1, p. 20")
  if (!any(ap) || isFALSE(pad)) return(res(FALSE))
  if (anyNA(x$dbot[ap])) return(res(NA))
  base <- max(x$dbot[ap])
  gley <- ifelse(ap, FALSE, f$gley)
  under <- x$ord[!is.na(x$dtop[x$ord]) & abs(x$dtop[x$ord] - base) <= 0.5 & !ap[x$ord]]
  if (!length(under)) {
    # nothing (or a gap) directly under the plough layer
    e <- .scsj_ends(x)
    return(res(if (!is.na(e$depth) && e$depth <= base + 0.5 && e$on_rock) FALSE else NA))
  }
  sure <- Filter(function(r) abs(r$top - base) <= 0.5 && r$sure >= 10,
                 .scsj_runs(x, ifelse(gley %in% TRUE, TRUE, FALSE)))
  poss <- Filter(function(r) r$top <= base + 0.5 && (if (r$open) Inf else r$bottom) >= base - 0.5 && r$poss >= 10,
                 .scsj_runs(x, gley, bridge = TRUE))
  ends <- function(r) if (r$at_end) Inf else r$bottom
  v <- if (length(sure)) {
    # the bridged run holding it must end above 75 cm
    b <- Filter(function(r) any(sure[[1L]]$rows %in% r$rows), .scsj_runs(x, gley, bridge = TRUE))
    if (length(b) && ends(b[[1L]]) < 75) TRUE else if (sure[[1L]]$deep >= 75) FALSE else NA
  } else if (!length(poss) || all(vapply(poss, function(r) r$deep >= 75, NA))) FALSE else NA
  res(if (isTRUE(pad)) v else if (isFALSE(v)) FALSE else NA)
}

# Generic: a paddy iron accumulation horizon (Anthraquic subgroups).
paddy_fe_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("paddy_fe_scsj", x, "IV.1, p. 20"))
  f <- .scsj_horizon_flags(x)
  passed <- .scsj_any(f$paddy_fe)
  .scsj_result("paddy_fe_scsj", passed, which(f$paddy_fe %in% TRUE), list(),
               if (is.na(passed)) c("land_use", "fe_dcb_pct") else character(0), "IV.1, p. 20")
}

# The bleached paddy surface horizon (Albic Fluvic Paddy soils).
bleached_paddy_surface_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("bleached_paddy_surface_scsj", x, "IV.1, p. 22"))
  ps <- .scsj_paddy_surface_rows(x)
  passed <- .scsj_any(ps$bleached)
  .scsj_result("bleached_paddy_surface_scsj", passed, which(ps$bleached %in% TRUE), list(),
               if (is.na(passed)) c("fe_dcb_pct", "redoximorphic_features_pct") else character(0), "IV.1, p. 22")
}

# A bleached horizon, 1 cm or more (Albic Red-Yellow soils).
bleached_horizon_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("bleached_horizon_scsj", x, "IV.1, p. 23"))
  pz <- .scsj_podzolic_rows(x)
  bl <- .scsj_and(.scsj_bleached_rows(x), .scsj_not(.scsj_or(pz$morph, pz$analysis)))
  passed <- .scsj_horizon_within(x, bl, 1)$passed
  .scsj_result("bleached_horizon_scsj", passed, which(bl %in% TRUE), list(),
               if (is.na(passed)) "munsell_value_moist" else character(0), "IV.1, p. 23")
}

# A podzolic accumulation horizon, by morphology or by analysis (Podzolic
# Brown Forest soils).
podzolic_horizon_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("podzolic_horizon_scsj", x, "IV.1, p. 24"))
  f <- .scsj_horizon_flags(x)
  passed <- .scsj_any(f$podzolic)
  .scsj_result("podzolic_horizon_scsj", passed, which(f$podzolic %in% TRUE), list(),
               if (is.na(passed)) c("munsell_hue_moist", "al_ox_pct") else character(0), "IV.1, p. 24")
}

# The top within `within` cm of a brown layer `thick` cm or more, below the
# plough layer and the surface horizons (the Endoaeric Fluvic Paddy soils:
# a brown subsoil; Hugo, 2026-10-31).
brown_layer_scsj <- function(pedon, within = 75, thick = 15) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("brown_layer_scsj", x, "III E, p. 10"))
  surface <- .scsj_plough_rows(x) | (!is.na(x$role) & grepl("^A", x$role))
  rows <- ifelse(surface, FALSE, x$colour$brown)
  passed <- .scsj_top_in(x, rows, -Inf, within, thick)
  .scsj_result("brown_layer_scsj", passed, which(rows %in% TRUE), list(),
               if (is.na(passed)) "munsell_hue_moist" else character(0), "III E, p. 10")
}

# Strong Gley Fluvic soils: no iron mottles of the class 'contains' or more
# (5 % or more: Hugo, 2026-10-31) in the gley layers deeper than 25 cm. A Gr
# of the notation (strongly reduced) has none, a Go (oxidised) has them, when
# the mottles are not recorded. A description that stops above 1 m (not on
# rock) leaves a Strong reading undecided.
strong_gley_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("strong_gley_scsj", x, "III E, p. 11"))
  f <- .scsj_horizon_flags(x)
  rows <- which(!(f$gley_layer %in% FALSE) & (is.na(x$dbot) | x$dbot > 25))
  rf <- .scsj_col(x$h, "redoximorphic_features_pct")
  tok <- x$des$tokens
  mott <- vapply(rows, function(i) {
    if (!is.na(rf[i])) return(rf[i] >= 5)
    if (x$role[i] %in% "G" && "r" %in% tok[[i]]) return(FALSE)
    if (x$role[i] %in% "G" && "o" %in% tok[[i]]) return(TRUE)
    NA
  }, NA)
  passed <- if (!length(rows)) NA else .scsj_not(.scsj_any(.scsj_and(f$gley_layer[rows], mott)))
  e <- .scsj_ends(x)
  if (isTRUE(passed) && !e$on_rock && !is.na(e$depth) && e$depth < 100) passed <- NA
  .scsj_result("strong_gley_scsj", passed, rows, list(),
               if (is.na(passed)) "redoximorphic_features_pct" else character(0), "III E, p. 11")
}

# Protoanthraquic Brown Fluvic soils: greyed down below the plough layer,
# with iron mottles (the horizon directly under the Ap grey, with mottles of
# 2 % or more or a g). Without designations, undecided.
protoanthraquic_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("protoanthraquic_scsj", x, "III E, p. 12"))
  res <- function(v, i = integer(0)) .scsj_result("protoanthraquic_scsj", v, i, list(),
    if (is.na(v)) c("designation", "munsell_chroma_moist", "redoximorphic_features_pct") else character(0), "III E, p. 12")
  ap <- .scsj_plough_rows(x)
  if (!any(ap)) return(res(if (any(x$des$known)) FALSE else NA))
  if (anyNA(x$dbot[ap])) return(res(NA))
  base <- max(x$dbot[ap])
  nxt <- x$ord[!is.na(x$dtop[x$ord]) & abs(x$dtop[x$ord] - base) <= 0.5 & !ap[x$ord]]
  if (!length(nxt)) return(res(NA))
  i <- nxt[1L]
  rf <- .scsj_col(x$h, "redoximorphic_features_pct")[i]
  mott <- if (!is.na(rf)) rf >= 2 else if (x$des$g[i]) TRUE else if (x$des$known[i]) FALSE else NA
  res(.scsj_and3(x$colour$grey[i], mott), i)
}

# Aquic Regosolic Fluvic soils: groundwater within 50 cm for half the year
# or more. Read from site$groundwater_depth_cm (50 cm or less), else the
# drainage (poorly or very poorly drained: yes; moderately well drained or
# better: no).
shallow_groundwater_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  gd <- suppressWarnings(as.numeric(x$site$groundwater_depth_cm %||% NA)[1L])
  dr <- .scsj_drainage(x$site)
  passed <- if (!is.na(gd)) gd <= 50 else if (is.na(dr)) NA else
    if (grepl("^(very )?poor", dr)) TRUE else if (grepl("^somewhat poor|^imperfect", dr)) NA else FALSE
  .scsj_result("shallow_groundwater_scsj", passed, integer(0), list(groundwater_depth_cm = gd, drainage = dr),
               if (is.na(passed)) c("groundwater_depth_cm", "drainage_class") else character(0), "III E, p. 12")
}

# Reddish subgroups: an argic (or, horizon = "weathered", a weathered)
# horizon of a red moist colour.
reddish_scsj <- function(pedon, horizon = "argic") {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("reddish_scsj", x, "III F"))
  f <- .scsj_horizon_flags(x)
  hz <- if (identical(horizon, "weathered")) f$weathered else f$argic
  passed <- .scsj_any(.scsj_and(hz, x$colour$red))
  .scsj_result("reddish_scsj", passed, which(hz %in% TRUE), list(horizon = horizon),
               if (is.na(passed)) "munsell_hue_moist" else character(0), "III F, pp. 12-13")
}

# Thapto-red-yellow Brown Forest soils: within 75 cm a weathered or argic
# horizon with the red-yellow feature.
thapto_red_yellow_scsj <- function(pedon, within = 75) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("thapto_red_yellow_scsj", x, "III I, p. 14"))
  f <- .scsj_horizon_flags(x)
  ry <- .scsj_red_yellow_rows(x)
  rows <- .scsj_and(.scsj_or(f$weathered, f$argic), ry)
  passed <- .scsj_top_in(x, rows, -Inf, within, 0)
  .scsj_result("thapto_red_yellow_scsj", passed, which(rows %in% TRUE), list(),
               if (is.na(passed)) "munsell_hue_moist" else character(0), "III I, p. 14")
}

# Eutric Brown Forest soils: a subhorizon of the subsurface with pH(H2O) 6.5
# or more, or base saturation 50 % or more. No section (gravel or rock at 20
# cm): no.
eutric_subsurface_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("eutric_subsurface_scsj", x, "III I, p. 15"))
  cs <- .scsj_control(x)
  rows <- cs$rows
  if (!length(rows)) return(.scsj_result("eutric_subsurface_scsj", if (isTRUE(cs$covered)) FALSE else NA,
                                         integer(0), list(), c("ph_h2o", "bs_pct"), "III I, p. 15"))
  ph <- .scsj_col(x$h, "ph_h2o")[rows]; bs <- .scsj_col(x$h, "bs_pct")[rows]
  r <- .scsj_or(ifelse(is.na(ph), NA, ph >= 6.5), ifelse(is.na(bs), NA, bs >= 50))
  passed <- .scsj_any(r)
  if (isFALSE(passed) && !isTRUE(cs$covered)) passed <- NA
  .scsj_result("eutric_subsurface_scsj", passed, rows, list(),
               if (is.na(passed)) c("ph_h2o", "bs_pct") else character(0), "III I, p. 15")
}

# ---- Regosols -------------------------------------------------------------------------

# Calcaric subgroups: carbonates of 2.0 % or more (CaCO3 equivalent) in a
# horizon between 20 and 50 cm (above = "rock": in the horizon on the
# bedrock). caco3_pct decides; no effervescence reads as no carbonate; a part
# of the window not described (or a gap above the rock) leaves a negative
# reading undecided.
calcaric_scsj <- function(pedon, from = 20, to = 50, above = "window") {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("calcaric_scsj", x, "III J, pp. 15-16"))
  h <- x$h
  cc <- .scsj_col(h, "caco3_pct"); ef <- .scsj_txt(h, "effervescence_class")
  r <- ifelse(!is.na(cc), cc >= 2.0, ifelse(!is.na(ef) & grepl("^(none|non)", ef), FALSE, NA))
  if (identical(above, "rock")) {
    rt <- x$rock_top
    if (!is.finite(rt %||% NA)) {
      passed <- FALSE; rows <- integer(0)
    } else {
      rows <- which(!is.na(x$dbot) & abs(x$dbot - rt) <= 0.5 & !x$rock)
      passed <- if (!length(rows)) NA else .scsj_any(r[rows])
    }
  } else {
    rows <- which(!is.na(x$dtop) & x$dtop < to & (is.na(x$dbot) | x$dbot > from) & !x$rock)
    passed <- if (!length(rows)) NA else .scsj_any(r[rows])
    if (isFALSE(passed) && .scsj_cum(x, rep(FALSE, x$n), from, to)$poss > 0) passed <- NA
  }
  .scsj_result("calcaric_scsj", passed, rows, list(), if (is.na(passed)) "caco3_pct" else character(0), "III J, pp. 15-16")
}

# Aquic Lithosols: the top of a layer with the gley feature or the
# groundwater wetness feature between the soil surface and the bedrock.
aquic_above_rock_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("aquic_above_rock_scsj", x, "III J, p. 15"))
  rows <- ifelse(x$rock, FALSE, .scsj_feature_rows(x, c("gley", "gw")))
  rt <- if (is.finite(x$rock_top %||% NA)) x$rock_top else Inf
  passed <- .scsj_top_in(x, rows, -Inf, rt, 0)
  .scsj_result("aquic_above_rock_scsj", passed, which(rows %in% TRUE), list(),
               if (is.na(passed)) "redoximorphic_features_pct" else character(0), "III J, p. 15")
}

.SCSJ_MARL_RE <- "marl|\u6ce5\u7070\u5ca9"
.SCSJ_GRANITE_RE <- "granit|\u82b1\u5d17|\u307e\u3055\u571f|masa"

# Generic: the soil derives from a parent rock named by `rock` (marl or
# granite), from the layers' recorded origin or the site parent material;
# for marl also 30 % clay or more in every horizon to 100 cm or the bedrock
# (a part not described: undecided).
parent_rock_scsj <- function(pedon, rock = "granite") {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("parent_rock_scsj", x, "III J, p. 16"))
  h <- x$h
  txt <- paste(c(.scsj_txt(h, "layer_origin"), .scsj_txt(h, "rock_origin"),
                 tolower(as.character(unlist(x$site$parent_material %||% character(0))))), collapse = " ")
  re <- if (identical(rock, "marl")) .SCSJ_MARL_RE else .SCSJ_GRANITE_RE
  from_rock <- grepl(re, txt, perl = TRUE)
  passed <- from_rock
  if (identical(rock, "marl") && from_rock) {
    lim <- min(100, if (is.finite(x$rock_top %||% NA)) x$rock_top else 100)
    rows <- which(!is.na(x$dtop) & x$dtop < lim & !x$rock & !(x$mineral %in% FALSE))
    clay <- .scsj_col(h, "clay_pct")[rows]
    passed <- .scsj_all(ifelse(is.na(clay), NA, clay >= 30))
    if (isTRUE(passed) && .scsj_cum(x, rep(FALSE, x$n), 0, lim)$poss > 0) passed <- NA
  }
  .scsj_result("parent_rock_scsj", passed, integer(0), list(rock = rock),
               if (is.na(passed)) "clay_pct" else character(0), "III J, p. 16")
}

# Para-lithic Terrestrial Regosols: gravel or bedrock that a trowel or
# shovel can cut: a Cr (semi-weathered bedrock) of the notation, or a rock
# row whose rupture resistance is soft or slightly hard.
para_lithic_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("para_lithic_scsj", x, "III J, p. 16"))
  rr <- .scsj_txt(x$h, "rupture_resistance")
  soft <- x$des$weathered_rock | (x$rock & !is.na(rr) & grepl("^(loose|soft|slightly hard)", rr))
  passed <- if (any(soft)) TRUE else if (any(x$rock_maybe)) NA else FALSE
  .scsj_result("para_lithic_scsj", passed, which(soft), list(), character(0), "III J, p. 16")
}

# ---- Man-made and Organic soils ------------------------------------------------------

# Organic Artifactual soils: the artificial material is mainly organic
# (artefacts_organic): the layers with 20 % or more artificial material, by
# thickness, more than half organic; read from the site when no layer
# records it. Only a continuous impermeable layer (concrete, asphalt): no.
organic_artefacts_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  ao <- as.logical(.scsj_col(x$h, "artefacts_organic", NA))
  art <- .scsj_col(x$h, "artefacts_pct")
  rows <- which(!is.na(art) & art >= 20)
  s <- as.logical(x$site$artefacts_organic %||% NA)[1L]
  passed <- if (!length(rows)) {
    if (!is.na(s)) s else if (any(.scsj_impermeable_rows(x))) FALSE else NA
  } else if (all(is.na(ao[rows]))) {
    if (!is.na(s)) s else NA
  } else {
    th <- ifelse(is.na(x$thick[rows]), 0, x$thick[rows])
    org <- sum(th[ao[rows] %in% TRUE]); inorg <- sum(th[ao[rows] %in% FALSE]); unk <- sum(th[is.na(ao[rows])])
    if (org > inorg + unk) TRUE else if (org + unk <= inorg) FALSE else NA
  }
  .scsj_result("organic_artefacts_scsj", passed, rows, list(), if (is.na(passed)) "artefacts_organic" else character(0),
               "III A, p. 7")
}

# Ekranic Artifactual soils: a continuous impermeable artificial layer.
ekranic_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  imp <- .scsj_impermeable_rows(x)
  passed <- any(imp) || isTRUE(as.logical(x$site$geomembrane %||% NA)[1L])
  .scsj_result("ekranic_scsj", passed, which(imp), list(), character(0), "III A, p. 7")
}

# Upland Reformed soils: on a terrace, hill or mountain (site$landform);
# lowland words: no; both or neither: undecided.
upland_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  lf <- tolower(paste(as.character(unlist(x$site$landform %||% character(0))), collapse = " "))
  up <- grepl("terrace|hill|mountain|upland|plateau|slope|ridge|\u53f0\u5730|\u4e18\u9675|\u5c71\u5730", lf)
  low <- grepl("lowland|floodplain|flood plain|delta|valley floor|backswamp|\\bplain\\b|\u4f4e\u5730", lf)
  passed <- if (!nzchar(trimws(lf)) || identical(trimws(lf), "na")) NA else
    if (up && !low) TRUE else if (low && !up) FALSE else NA
  .scsj_result("upland_scsj", passed, integer(0), list(landform = lf), if (is.na(passed)) "landform" else character(0),
               "III A, p. 7")
}

# Peat material by decomposition: sapric (rubbed fibre under a sixth of the
# volume), fibric (three quarters or more), hemic (between); from the
# decomposition letter of the notation when fibre is not recorded (a, e, i).
.scsj_peat_kind <- function(x) {
  fib <- .scsj_col(x$h, "fiber_content_rubbed_pct")
  tok <- x$des$tokens
  letter <- vapply(seq_len(x$n), function(i) {
    t <- tok[[i]]
    if ("a" %in% t) "sapric" else if ("e" %in% t) "hemic" else if ("i" %in% t) "fibric" else NA_character_
  }, "")
  ifelse(!is.na(fib), ifelse(fib < 100 / 6, "sapric", ifelse(fib >= 75, "fibric", "hemic")), letter)
}

# Sapric Peat soils: among the peat layers to 50 cm, sapric peat material has
# the largest share (thickness). Layers whose peat status or decomposition is
# not known, the part of an open horizon below its top, and undescribed
# depth count as unknown.
sapric_peat_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("sapric_peat_scsj", x, "III B, p. 7"))
  k <- .scsj_peat_kind(x)
  rows <- which(!(x$peat %in% FALSE) & !is.na(x$dtop) & x$dtop < 50)
  if (!length(rows)) return(.scsj_result("sapric_peat_scsj", NA, integer(0), list(), "oc_pct", "III B, p. 7"))
  closed <- !is.na(x$dbot[rows])
  th <- ifelse(closed, pmin(x$dbot[rows], 50) - pmax(x$dtop[rows], 0), 0)
  sure_peat <- x$peat[rows] %in% TRUE & closed
  share <- function(kind) sum(th[sure_peat & k[rows] %in% kind])
  unk <- sum(th[!sure_peat | is.na(k[rows])]) +
         sum(ifelse(closed, 0, 50 - pmax(x$dtop[rows], 0))) +
         .scsj_cum(x, rep(FALSE, x$n), 0, 50)$poss
  s <- share("sapric"); o <- max(share("hemic"), share("fibric"))
  passed <- if (s > o + unk) TRUE else if (s + unk <= o) FALSE else NA
  .scsj_result("sapric_peat_scsj", passed, rows, list(sapric_cm = s, other_cm = o, unknown_cm = unk),
               if (is.na(passed)) "fiber_content_rubbed_pct" else character(0), "III B, p. 7")
}

# High-moor and Transitional-moor Peat soils: the uppermost 25 cm of the peat
# layers is that peat (peat_moor_type: high-moor, transitional, low-moor, or
# the Japanese words). A layer whose peat status is undecided counts as
# unknown.
moor_type_scsj <- function(pedon, type = "high-moor") {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("moor_type_scsj", x, "III B, p. 7"))
  mt <- .scsj_txt(x$h, "peat_moor_type")
  mt <- ifelse(is.na(mt), NA, ifelse(grepl("high|\u9ad8\u4f4d", mt), "high-moor",
               ifelse(grepl("transit|intermed|\u4e2d\u9593", mt), "transitional",
               ifelse(grepl("low|\u4f4e\u4f4d", mt), "low-moor", NA))))
  rows <- x$ord[!(x$peat[x$ord] %in% FALSE)]
  if (!any(x$peat %in% TRUE)) return(.scsj_result("moor_type_scsj", NA, integer(0), list(), "oc_pct", "III B, p. 7"))
  top <- x$dtop[rows[1L]]
  in25 <- rows[x$dtop[rows] < top + 25]
  want <- if (identical(type, "transitional")) "transitional" else "high-moor"
  v <- ifelse(is.na(mt[in25]), NA, mt[in25] == want)
  v <- ifelse(x$peat[in25] %in% TRUE, v, ifelse(v %in% FALSE, NA, v))
  passed <- .scsj_all(v)
  .scsj_result("moor_type_scsj", passed, in25, list(type = want),
               if (is.na(passed)) "peat_moor_type" else character(0), "III B, p. 7")
}
