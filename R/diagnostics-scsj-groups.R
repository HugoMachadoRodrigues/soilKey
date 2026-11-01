# =============================================================================
# v0.9.322 -- the 26 groups of the Soil Classification System of Japan (SCSJ
# 2017, chapter III, pp. 7-15). Within a great group each group after the
# first is "soils other than the above", so a gate tests only its own
# requirements; the last group of each great group is the default.
# Three-valued.
# =============================================================================

# ---- gley layers (IV.1, the subdivisions of the gley layer) -------------------

# The gley layers (gley feature, 10 cm or more): runs of adjacent gley
# horizons (Hugo, 2026-10-31: "several gley layers" are adjacent gley
# horizons taken together). The sure ones (runs of TRUE rows) and the
# possible ones (bridged runs of rows that may have the feature), each with
# its top, its certain bottom (deep) and its possible bottom (Inf when the
# run has no bottom or reaches the end of a description that stops above,
# not on rock).
.scsj_gley_layers <- function(x, f = .scsj_horizon_flags(x)) {
  rows <- f$gley
  one <- function(r) c(top = r$top, deep = r$deep, bottom = if (r$at_end) Inf else r$bottom)
  sure <- Filter(function(r) r$sure >= 10, .scsj_runs(x, ifelse(rows %in% TRUE, TRUE, FALSE)))
  poss <- Filter(function(r) r$poss >= 10, .scsj_runs(x, rows, bridge = TRUE))
  list(sure = lapply(sure, one), poss = lapply(poss, one))
}

# The top of a groundwater gley layer (a gley layer reaching 75 cm or deeper)
# within `within` cm. Undecided when rows that may have the gley feature,
# gaps and undescribed depth together could make one (a span without a break
# from `within` cm or shallower to 75 cm or deeper).
.scsj_groundwater_gley_within <- function(x, within = 50, f = .scsj_horizon_flags(x)) {
  if (is.na(x$surface)) return(NA)
  gl <- .scsj_gley_layers(x, f)
  if (any(vapply(gl$sure, function(v) v[["top"]] <= within && v[["deep"]] >= 75, NA))) return(TRUE)
  sp <- .scsj_possible_spans(x, f$gley)
  if (any(sp$a <= within & sp$b >= 75)) return(NA)
  FALSE
}

# The groundwater wetness rows the groups read: on paddy land the paddy
# surface, paddy iron accumulation and paddy greyed horizons are left out,
# their mottles coming from the irrigation water (Hugo, 2026-10-31; chapter
# VI: paddy fields on well-drained Andosols are not Wet Andosols). Land use
# not recorded: those horizons' wetness is undecided.
.scsj_groundwater_rows <- function(x, f = .scsj_horizon_flags(x)) {
  gw <- .scsj_wetness_rows(x)$groundwater
  if (isFALSE(x$paddy)) return(gw)
  ps <- .scsj_paddy_surface_rows(x)
  paddy_rows <- .scsj_or(.scsj_or(ps$surface, f$paddy_fe), f$paddy_grey)
  if (isTRUE(x$paddy)) return(ifelse(paddy_rows %in% TRUE, FALSE, ifelse(is.na(paddy_rows) & gw %in% TRUE, NA, gw)))
  ifelse(!(paddy_rows %in% FALSE) & gw %in% TRUE, NA, gw)
}

# The top of a layer with the groundwater wetness feature within `within`
# cm (any thickness).
.scsj_groundwater_wet_within <- function(x, within = 50, f = .scsj_horizon_flags(x)) {
  .scsj_top_in(x, .scsj_groundwater_rows(x, f), -Inf, within, 0)
}

# ---- A. Man-made soils ------------------------------------------------------------

# Artifactual soils (A1): 25 cm or more (cumulative) of layers with 20 % or
# more artificial material within 50 cm, or a continuous impermeable
# artificial layer within 30 cm.
artifactual_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("artifactual_soils_scsj", x, "III A, p. 7"))
  h <- x$h
  art <- .scsj_col(h, "artefacts_pct")
  art20 <- ifelse(is.na(art), FALSE, art >= 20)
  a <- .scsj_at_least(.scsj_cum(x, art20, 0, 50), 25)
  imp <- .scsj_impermeable_rows(x)
  b <- .scsj_impermeable_within(x, 30)
  passed <- .scsj_or3(a, b)
  .scsj_result("artifactual_soils_scsj", passed, which(art20 | imp),
               list(artefacts_20_cm = .scsj_cum(x, art20, 0, 50)$sure, impermeable_within_30 = b),
               character(0), "III A, p. 7")
}

# ---- C. Andosols -------------------------------------------------------------------

# Podzolic Andosols (C1): a podzolic accumulation horizon (morphology) under
# a bleached horizon, or one by analysis under a bleached horizon or with one
# by morphology; when there is a bleached horizon, it and the accumulation
# horizon come from one parent material (criterion 2; the bleached horizon
# above, as for the Podzols).
podzolic_andosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("podzolic_andosols_scsj", x, "III C, p. 7"))
  ag <- .scsj_argic_rows(x)
  pz <- .scsj_podzolic_rows(x, argic = ag)
  bl <- .scsj_and(.scsj_bleached_rows(x), .scsj_not(.scsj_or(pz$morph, pz$analysis)))
  r_mb <- .scsj_pair(x, pz$morph, 2.5, bl, 1)
  r_ab <- .scsj_pair(x, pz$analysis, 2.5, bl, 1)
  r_am <- .scsj_pair(x, pz$analysis, 2.5, pz$morph, 2.5, above = FALSE, same_material = FALSE)
  # the analysis-with-morphology route: with a bleached horizon, the unit
  # must be of one parent material
  am <- r_am$passed
  if (!isFALSE(am)) {
    bl_h <- .scsj_horizon_rows(x, bl, 1)
    if (any(bl_h %in% TRUE)) {
      li <- unique(stats::na.omit(x$des$lith[c(r_am$rows, which(bl_h %in% TRUE))]))
      am <- .scsj_and3(am, length(li) <= 1L)
    } else if (anyNA(bl_h)) {
      am <- NA
    }
  }
  passed <- .scsj_or3(r_mb$passed, r_ab$passed, am)
  .scsj_result("podzolic_andosols_scsj", passed, unique(c(r_mb$rows, r_ab$rows, r_am$rows)),
               list(morphology_under_bleached = r_mb$passed, analysis_under_bleached = r_ab$passed,
                    analysis_with_morphology = am),
               if (is.na(passed)) c("munsell_hue_moist", "al_ox_pct", "fe_ox_pct") else character(0),
               "III C, pp. 7-8")
}

# Regosolic Andosols (C2): 25 cm or more (cumulative) of layers with the
# immature kuroboku feature within 50 cm.
regosolic_andosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("regosolic_andosols_scsj", x, "III C, p. 8"))
  passed <- .scsj_at_least(.scsj_cum(x, x$ikb, 0, 50), 25)
  .scsj_result("regosolic_andosols_scsj", passed, which(x$ikb %in% TRUE),
               list(immature_kuroboku_cm_within_50 = .scsj_cum(x, x$ikb, 0, 50)$sure),
               if (is.na(passed)) c("layer_origin", "phosphate_absorption_coef") else character(0), "III C, p. 8")
}

# Gleyed soils (C3 Gleyed Andosols, E2 Gley Fluvic soils): the top of a
# groundwater gley layer within 50 cm.
groundwater_gley_group_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("groundwater_gley_group_scsj", x, "III C, E"))
  passed <- .scsj_groundwater_gley_within(x, 50)
  .scsj_result("groundwater_gley_group_scsj", passed, which(.scsj_gley_feature_rows(x) %in% TRUE),
               list(groundwater_gley_within_50 = passed),
               if (is.na(passed)) c("childs_test", "designation") else character(0), "III C (p. 8), E (p. 11)")
}

# Wet soils (C4 Wet Andosols, E3 Gray Fluvic soils): the top of a layer with
# the groundwater wetness feature within 50 cm.
groundwater_wet_group_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("groundwater_wet_group_scsj", x, "III C, E"))
  f <- .scsj_horizon_flags(x)
  gw <- .scsj_groundwater_rows(x, f)
  passed <- .scsj_top_in(x, gw, -Inf, 50, 0)
  .scsj_result("groundwater_wet_group_scsj", passed, which(gw %in% TRUE),
               list(groundwater_wetness_within_50 = passed),
               if (is.na(passed)) c("redoximorphic_features_pct", "redox_segregations_location") else character(0),
               "III C (p. 8), E (p. 11)")
}

# Farmland (a site under cultivation), for the y1 rule of the non-allophanic
# kuroboku feature: a recorded land use decides (paddy land, or words for
# crops, fields, orchards, tea gardens, vegetables, pasture or cultivation;
# not 'uncultivated', not a forest or natural grassland); with none
# recorded, a plough layer (not buried) says farmland. TRUE / FALSE / NA.
.scsj_farmland <- function(x) {
  if (isTRUE(x$paddy)) return(TRUE)
  lu <- tolower(paste(as.character(unlist(x$site$land_use %||% character(0))), collapse = " "))
  if (nzchar(trimws(lu)) && !identical(trimws(lu), "na")) {
    lu2 <- gsub("\\b(un|non[- ]?)(cultivat\\w*|farm\\w*|arable)", " ", lu, perl = TRUE)
    farm <- grepl(paste0("\\b(crops?|cropping|cropland|fields?|arable|cultivat\\w*|farm|farmland|orchards?|",
                         "tea (garden|field|plantation)s?|vegetables?|pastures?|hay ?fields?|upland rice)\\b|",
                         "\u7551|\u6a39\u5712|\u679c\u6a39\u5712|\u8336\u5712|\u8fb2\u5730|\u8015\u5730|\u7267\u8349\u5730|\u9678\u7a32"),
                  lu2, perl = TRUE)
    return(farm)
  }
  ap <- .scsj_plough_rows(x) & !x$des$buried
  if (any(ap)) return(TRUE)
  NA
}

# The non-allophanic kuroboku feature (IV.2) of each row: the kuroboku
# feature, and an exchange acidity y1 of 5 or more, or Sio under 0.6 %, or an
# Alp/Alo ratio of 0.5 or more. On farmland, where fertilising or liming can
# change the acidity, y1 is not read.
.scsj_non_allophanic_rows <- function(x) {
  h <- x$h
  y1 <- .scsj_col(h, "exchange_acidity_y1")
  sio <- .scsj_col(h, "si_ox_pct")
  alp <- .scsj_col(h, "al_pyro_pct"); alo <- .scsj_col(h, "al_ox_pct")
  ratio <- ifelse(is.na(alp) | is.na(alo) | alo <= 0, NA_real_, alp / alo)
  farm <- .scsj_farmland(x)
  b <- .scsj_or(ifelse(is.na(sio), NA, sio < 0.6), ifelse(is.na(ratio), NA, ratio >= 0.5))
  a <- if (isTRUE(farm)) rep(FALSE, x$n) else ifelse(is.na(y1), NA, y1 >= 5)
  if (is.na(farm)) a <- ifelse(a %in% TRUE, NA, a)      # y1 may not apply
  .scsj_and(x$kb, .scsj_or(a, b))
}

# Non-allophanic Andosols (C5): 25 cm or more (cumulative) of layers with
# the non-allophanic kuroboku feature within 50 cm.
non_allophanic_andosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("non_allophanic_andosols_scsj", x, "III C, p. 9"))
  rows <- .scsj_non_allophanic_rows(x)
  passed <- .scsj_at_least(.scsj_cum(x, rows, 0, 50), 25)
  .scsj_result("non_allophanic_andosols_scsj", passed, which(rows %in% TRUE),
               list(non_allophanic_cm_within_50 = .scsj_cum(x, rows, 0, 50)$sure, farmland = .scsj_farmland(x)),
               if (is.na(passed)) c("exchange_acidity_y1", "si_ox_pct", "al_pyro_pct", "al_ox_pct") else character(0),
               "III C, p. 9")
}

# ---- E. Fluvic soils ----------------------------------------------------------------

# Fluvic Paddy soils (E1): a paddy iron accumulation horizon, or a paddy
# greyed horizon whose lower boundary is 50 cm or deeper. A description that
# stops at or inside the paddy horizons (not on rock) leaves it undecided.
fluvic_paddy_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("fluvic_paddy_soils_scsj", x, "III E, p. 10"))
  f <- .scsj_horizon_flags(x)
  ps <- .scsj_paddy_surface_rows(x)
  e <- .scsj_ends(x)
  fe <- .scsj_any(f$paddy_fe)
  gr_rows <- f$paddy_grey
  grey <- FALSE
  if (any(gr_rows %in% c(TRUE, NA))) {
    sel <- which(gr_rows %in% c(TRUE, NA))
    open <- anyNA(x$dbot[sel])
    deep <- if (open) Inf else max(x$dbot[sel])
    reach <- if (!open && deep >= 50) TRUE else
             if (open || (!e$on_rock && !is.na(e$depth) && deep >= e$depth - 0.5)) NA else FALSE
    grey <- .scsj_and3(.scsj_any(gr_rows), reach)
  }
  passed <- .scsj_or3(fe, grey)
  # a possible paddy surface with nothing described under it
  if (isFALSE(passed) && any(ps$surface %in% c(TRUE, NA)) && !e$on_rock) {
    surf <- which(ps$surface %in% c(TRUE, NA))
    base <- if (anyNA(x$dbot[surf])) NA else max(x$dbot[surf])
    if (is.na(base) || (!is.na(e$depth) && base >= e$depth - 0.5)) passed <- NA
  }
  .scsj_result("fluvic_paddy_soils_scsj", passed, which(f$paddy_fe %in% TRUE | gr_rows %in% TRUE),
               list(paddy_iron_accumulation = fe, paddy_greyed_to_50 = grey),
               if (is.na(passed)) c("land_use", "fe_dcb_pct", "redoximorphic_features_pct") else character(0),
               "III E, p. 10")
}

# The subsurface colour (IV.4): the colour words of the thickest horizon of
# the subsurface section (the text: several horizons in 20-60 cm are
# represented by the thickest). No section (gravel or rock at 20 cm, the
# description complete): not brown.
.scsj_subsurface_colour <- function(x) {
  # the colour word: brown (SCSJ), yellowish brown without 5YR (CSCJ)
  word <- if (identical(x$edition, "cscj2011")) x$colour$yellowish_brown else x$colour$brown
  cs <- .scsj_control(x)
  if (!length(cs$rows)) return(list(rows = integer(0), brown3 = if (isTRUE(cs$covered)) FALSE else NA))
  db <- ifelse(is.na(x$dbot), Inf, x$dbot)
  ov <- pmin(db[cs$rows], cs$to) - pmax(x$dtop[cs$rows], cs$from)
  best <- cs$rows[ov == max(ov)]
  b <- if (all(word[best] %in% TRUE)) TRUE else
       if (all(word[best] %in% FALSE)) FALSE else NA
  # a section not described in full, or an open thickest horizon, may hide a
  # thicker horizon of another colour, unless the best overlap exceeds what
  # is undescribed
  if (!isTRUE(cs$covered) || any(is.na(x$dbot[best]))) {
    undesc <- .scsj_cum(x, rep(FALSE, x$n), cs$from, cs$to)$poss
    if (!(all(!is.na(x$dbot[best])) && max(ov) > undesc)) b <- NA
  }
  list(rows = best, brown3 = b, covered = cs$covered)
}

# Brown Fluvic soils (E4): the subsurface is brown.
brown_fluvic_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("brown_fluvic_soils_scsj", x, "III E, p. 11"))
  sc <- .scsj_subsurface_colour(x)
  passed <- sc$brown3
  .scsj_result("brown_fluvic_soils_scsj", passed, sc$rows %||% integer(0),
               list(subsurface_brown = passed),
               if (is.na(passed)) "munsell_hue_moist" else character(0), "III E, p. 11")
}

# ---- F. Red-Yellow soils -------------------------------------------------------------

# Argic Red-Yellow soils (F1): an argic horizon, read within the depth the
# system classifies (about 1 m, chapter I).
argic_group_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("argic_group_scsj", x, "III F, p. 12"))
  ag <- .scsj_argic_horizon_rows(x)
  passed <- .scsj_top_in(x, ag, -Inf, 100, 0)
  .scsj_result("argic_group_scsj", passed, which(ag %in% TRUE), list(argic = passed),
               if (is.na(passed)) c("clay_pct", "clay_films_amount") else character(0), "III F, p. 12")
}

# ---- G. Stagnic soils -----------------------------------------------------------------

# Stagnogley soils (G1): the top of a gley layer within 50 cm, the layer
# reaching 50 cm or deeper, or the bedrock.
stagnogley_soils_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("stagnogley_soils_scsj", x, "III G, p. 13"))
  gl <- .scsj_gley_layers(x)
  rt <- x$rock_top
  on_rock <- function(b) is.finite(b) && is.finite(rt %||% NA) && abs(b - rt) <= 0.5
  # undecided: a span that may hold a gley layer of 10 cm starting within 50
  # cm and reaching 50 cm or the bedrock (rows, gaps, undescribed depth)
  sp <- .scsj_possible_spans(x, .scsj_horizon_flags(x)$gley)
  poss <- any(sp$a <= 50 & sp$b - sp$a >= 10 & (sp$b >= 50 | vapply(sp$b, on_rock, NA)))
  passed <- if (any(vapply(gl$sure, function(v) v[["top"]] <= 50 && (v[["deep"]] >= 50 || on_rock(v[["deep"]])), NA))) TRUE else
            if (poss) NA else FALSE
  .scsj_result("stagnogley_soils_scsj", passed, which(.scsj_gley_feature_rows(x) %in% TRUE),
               list(gley_layer_to_50 = passed),
               if (is.na(passed)) c("childs_test", "designation") else character(0), "III G, p. 13")
}

# ---- H. Eutrosols -------------------------------------------------------------------

# Magnesian Eutrosols (H1): a subhorizon of the subsurface with an
# exchangeable Ca/Mg ratio under 0.1, or every subhorizon under 1.0.
magnesian_eutrosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("magnesian_eutrosols_scsj", x, "III H, p. 14"))
  cs <- .scsj_control(x)
  rows <- cs$rows
  if (!length(rows)) {
    return(.scsj_result("magnesian_eutrosols_scsj", NA, integer(0), list(), c("ca_cmol", "mg_cmol"), "III H, p. 14"))
  }
  ca <- .scsj_col(x$h, "ca_cmol")[rows]; mg <- .scsj_col(x$h, "mg_cmol")[rows]
  ratio <- ifelse(is.na(ca) | is.na(mg), NA_real_, ifelse(mg <= 0, Inf, ca / mg))
  low01 <- .scsj_any(ifelse(is.na(ratio), NA, ratio < 0.1))
  all1 <- .scsj_all(ifelse(is.na(ratio), NA, ratio < 1.0))
  if (!isTRUE(cs$covered)) { if (isFALSE(low01)) low01 <- NA; if (isTRUE(all1)) all1 <- NA }
  passed <- .scsj_or3(low01, all1)
  .scsj_result("magnesian_eutrosols_scsj", passed, rows,
               list(ca_mg = ratio, any_under_0.1 = low01, all_under_1 = all1),
               if (is.na(passed)) c("ca_cmol", "mg_cmol") else character(0), "III H, p. 14")
}

# ---- J. Regosols ----------------------------------------------------------------------

# Volcanogenous Regosols (J1): horizons undeveloped (no weathered or argic
# horizon) and 25 cm or more (cumulative) of volcanic ejecta within 50 cm.
volcanogenous_regosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("volcanogenous_regosols_scsj", x, "III J, p. 15"))
  f <- .scsj_horizon_flags(x)
  undeveloped <- .scsj_not(.scsj_any(.scsj_or(f$weathered, f$argic)))
  ej <- .scsj_at_least(.scsj_cum(x, x$orig$ejecta, 0, 50), 25)
  passed <- .scsj_and3(undeveloped, ej)
  .scsj_result("volcanogenous_regosols_scsj", passed, which(x$orig$ejecta),
               list(ejecta_cm_within_50 = .scsj_cum(x, x$orig$ejecta, 0, 50)$sure, undeveloped = undeveloped),
               character(0), "III J, p. 15")
}

# Sandy Regosols (J2): every horizon to 100 cm with coarse fragments under
# 35 % and a texture of loamy sand or coarser (calcareous and coral sands
# included). Bedrock or an organic layer within 100 cm is not sand; a gap or
# the end of the description above 100 cm leaves it undecided; coarse
# fragments not recorded (nor gravel) read as under 35 %. A part keyed on
# its own is read to its last horizon.
sandy_regosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("sandy_regosols_scsj", x, "III J, p. 15"))
  h <- x$h
  tx <- .nzsc_texture(h)
  coarse <- ifelse(!is.na(tx$class), tx$class %in% c("sand", "loamy sand"),
            ifelse(is.na(tx$group), NA, tx$group == "sandy"))
  cf <- .scsj_col(h, "coarse_fragments_pct"); gv <- .scsj_col(h, "gravel_pct")
  cfu <- ifelse(!is.na(cf), cf, gv)
  few <- ifelse(is.na(cfu), TRUE, cfu < 35)
  ok <- .scsj_and(coarse, few)
  ok <- ifelse(x$rock | x$organic %in% TRUE, FALSE, ifelse(is.na(x$organic), .scsj_and(ok, NA), ok))
  inside <- !is.na(x$dtop) & x$dtop < 100 & (is.na(x$dbot) | x$dbot > 0)
  passed <- if (!any(inside)) NA else .scsj_all(ok[inside])
  # the whole window must be described (not a gap, not below the end)
  if (isTRUE(passed) && .scsj_cum(x, rep(FALSE, x$n), 0, 100)$poss > 0) passed <- NA
  .scsj_result("sandy_regosols_scsj", passed, which(inside),
               list(sandy_to_100 = passed),
               if (is.na(passed)) c("field_texture", "clay_pct", "coarse_fragments_pct") else character(0),
               "III J, p. 15")
}

# Lithosols (J3): consolidated (hard) bedrock, an R horizon, within 30 cm.
lithosols_scsj <- function(pedon) {
  x <- .scsj_profile(pedon)
  if (is.na(x$surface)) return(.scsj_no_surface("lithosols_scsj", x, "III J, p. 15"))
  hard <- x$role %in% "R"
  passed <- if (any(hard & !is.na(x$dtop) & x$dtop <= 30)) TRUE else
            if (any(x$rock_maybe & !is.na(x$dtop) & x$dtop <= 30)) NA else {
              dep <- suppressWarnings(max(x$dbot, na.rm = TRUE))
              if ((is.finite(dep) && dep > 30) || isTRUE(x$complete)) FALSE else NA
            }
  .scsj_result("lithosols_scsj", passed, which(hard), list(hard_rock_within_30 = passed),
               if (is.na(passed)) "designation" else character(0), "III J, p. 15")
}

# The Regosol groups a part of a profile may key to (for the heterogeneous
# fill of Man-made soils): those of the gates up to the first that passes,
# the undecided ones included.
.scsj_regosol_possible <- function(p) {
  if (is.null(p)) return(c("J1", "J2", "J3", "J4"))
  out <- character(0)
  for (k in c("J1", "J2", "J3")) {
    fn <- switch(k, J1 = volcanogenous_regosols_scsj, J2 = sandy_regosols_scsj, J3 = lithosols_scsj)
    r <- fn(p)$passed
    if (isTRUE(r)) return(c(out, k))
    if (is.na(r)) out <- c(out, k)
  }
  c(out, "J4")
}
