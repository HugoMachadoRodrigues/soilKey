# =============================================================================
# v0.9.361 -- the horizons of the German KA5 (Bodenkundliche Kartieranleitung,
# 5th ed., Ad-hoc-AG Boden 2005; 5.6.3.3, pp. 88-108) identified on the
# recorded data, layer by layer.
#
# The KA5 defines its horizons by criteria partly relative (the Bv against
# the horizon below, the Bs against both neighbours, the Sw against the Sd
# below, the Bt against the Al above within 30 cm), partly field-only
# (bleaching, rust, marbling, bioturbation) and partly measured (organic
# substance, clay, Cp/Fep, base saturation). It gives no key to them: soilKey
# reads them by a fixed order of tests of its own (Hugo's decision Q9):
#   1 F, the subaqueous and intertidal horizons;
#   2 the organic horizons (30 % organic substance or more, Q10): H, then L,
#     Of, Oh;
#   3 the anthropogenic and colluvial ones: Y, R, E, M;
#   4 the groundwater horizons G (Gr, Gor, Gro, Go, Gw; Q13);
#   5 the perched-water horizons S (Sq, Sg, Srw, Srd, Sew, Sw, Sd);
#   6 the topsoils A (Aa, Ae, Ael, Al, Acxh, Axh, Ap, Ahe, Ai, Ah) in the
#     run of topsoils at the GOF;
#   7 the subsoils B (Bh, Bsh, Bhs, Bs; Bt; Bu; Bfv; Bv by Q12), P and T;
#   8 the substratum C (lC, mC, xC with i, e, c by Q15; Cv, Cn, Cc);
#   9 the prefixes and suffixes every row may carry (a, tm, tb, tp, z, e, o;
#     c, p);
#  10 the transitions the text gives rules for (Q9): Bv-Sw, Al-Sw (S features
#     10 to < 80 area-%), Sw-Bv (< 10), Bv-Sd (10 to < 50), Bv-Go (rust 5 or
#     more in the fluctuation zone), Go-Bv (under 5), and the transitions with
#     the Ah humus minimum (Ah-Bv, Ah-Go, Ah-Sw).
# Every test is three-valued. The first that holds gives the code; an
# undecided test before it in its step leaves the layer undecided with both
# as candidates; an undecided organic, G or S test leaves the layer
# undecided against whatever follows (they dominate a layer's reading); an
# undecided test of another earlier step goes to the candidates. The other
# codes that hold go to the candidates too.
#
# A missing field descriptor may be read through a soilKey WRB diagnostic,
# flagged, never decisive alone (the plan's stand-in rule): argic() for the
# clay coatings of the Bt, spodic() for the Bh and Bs, albic() for the Ae,
# gleyic_properties() and stagnic_properties() for the water of a G or an S,
# mollic() and chernic() for the Axh, cambic() for the Bv, vertic_horizon()
# for the structure of the P, fluvic_material() for the stratification of an
# M, plaggic() and hortic() for the E.
#
# A layer written in the KA5 notation keeps its horizon (route "notation");
# the order of tests runs on its data as well, and when it reaches the same
# horizon the route is "both", otherwise its reading goes to the evidence
# (the written designation gives the horizon, the data the properties: the
# Russian and RP rule). A row of a profile that declares the FAO notation
# is read within the KA5 horizons its FAO letters allow, with the field
# observations they state (v0.9.380, R/ka5-fao.R). .ka5_identify() is the
# identification of the namespace ka5 (identify_horizons(pedon, "ka5") once
# the registry has it).
# =============================================================================

.KA5_STAND_INS <- c("argic", "spodic", "albic", "gleyic_properties", "stagnic_properties", "mollic",
                    "chernic", "cambic", "vertic_horizon", "fluvic_material", "plaggic", "hortic",
                    "limnic_material")

# A WRB diagnostic standing in, flagged: its verdict on the row and a note.
.ka5_stand_in <- function(x, i, name, what) {
  w <- .kidpr_wrb_layer(x, name, i)
  list(p = w, tag = name, note = sprintf("%s not recorded: soilKey's WRB %s() %s, flagged", what, name,
                                         if (is.na(w)) "undecided" else if (w) "holds on the layer" else "does not hold"))
}

# A row's site in a floodplain (the prefix a, p. 84): landform or valley
# position, flooding, or fluvial material.
.ka5_floodplain <- function(x, i) {
  lf <- paste(c(.ka5_site_txt(x, "landform"), .ka5_site_txt(x, "valley_position")), collapse = " ")
  ff <- .ka5_site_txt(x, "flood_frequency")
  lo <- .ka5_txt(x, i, "layer_origin")
  isTRUE(grepl("floodplain|flood plain|aue|levee|backswamp", lf)) ||
    (!is.na(ff) && !ff %in% c("none", "never")) || isTRUE(grepl("fluvi|alluvi", lo))
}

# The tidal prefix of a row from its layer_origin (tm, tb, tp), else NA.
.ka5_tidal_prefix <- function(x, i) {
  lo <- .ka5_txt(x, i, "layer_origin")
  if (is.na(lo)) return(NA_character_)
  if (grepl("tidal-marine", lo)) "tm" else if (grepl("tidal-brackish", lo)) "tb" else if (grepl("tidal-fluvial", lo)) "tp" else NA_character_
}

# A row described as rock: rock_diggable, weathering_stage or the carbonate
# of the whole rock recorded, or coarse fragments of 75 % or more (soilKey's
# reading, Hugo, 2026-11-06: the surveyor who records these
# describes the substratum C, not a B).
.ka5_rock_described <- function(x, i) {
  !is.na(.ka5_lgl(x, i, "rock_diggable")) || !is.na(.ka5_txt(x, i, "weathering_stage")) ||
    !is.na(.ka5_num(x, i, "carbonate_whole_soil_pct")) || isTRUE(.ka5_num(x, i, "coarse_fragments_pct") >= .ka5_thr("b_coarse_max_pct"))
}

# The first mineral row in depth order (the top of the run of topsoils).
.ka5_first_mineral <- function(x) {
  .ka5_memo(x, "first_mineral", function() {
    for (i in x$ord) if (!isTRUE(.ka5_organic(x, i)$passed)) return(i)
    NA_integer_
  })
}

# The site is under water (inland) or in the tidal zone between MNW and MHW.
.ka5_subaqueous <- function(x) {
  sa <- .ka5_site_lgl(x, "subaqueous")
  tz <- .ka5_site_txt(x, "tidal_zone")
  if (isTRUE(sa)) return("inland")
  if (isTRUE(tz %in% c("subtidal", "intertidal"))) return("tidal")
  NA_character_
}


# --- 1 the F horizons (p. 89) -------------------------------------------------------------

.ka5_t_F <- function(x, i) {
  where <- .ka5_subaqueous(x)
  if (is.na(where)) {
    # a lake or river bottom that is not recorded: limnic material is a hint
    w <- .ka5_stand_in(x, i, "limnic_material", "the site under water")
    if (isTRUE(w$p)) return(.ka5_r(NA, missing = c("subaqueous", "tidal_zone"), note = w$note, tag = w$tag))
    return(.ka5_r(FALSE, note = "the site is not recorded under water or in the tidal zone"))
  }
  if (isTRUE(.ka5_organic(x, i)$passed) && isTRUE(.ka5_peat(x, i)$passed))
    return(.ka5_r(FALSE, note = "a peat under water is an H"))
  sh <- .ka5_shares(x, i)
  col <- .ka5_colour(x, i)
  os <- .ka5_os(x, i)$range[1L]
  dark <- if (is.na(col$value) || is.na(col$chroma)) NA else col$value <= 3 && col$chroma <= 2
  brown <- if (is.na(col$family)) NA else col$family %in% c("YR") && isTRUE(col$chroma >= 2) && isTRUE(col$value <= 4)
  # the kinds of the F (p. 89), soilKey's order and readings (for Hugo to
  # confirm, 2026-11-05): clear oxidation features (rust 5 % or more, the
  # limit of the Go) make an Fo; dark brown humic an Fh; black to dark grey
  # or a sulfide smell an Fr; in the tidal zone without features an Fw; no
  # visible humus (organic substance under 1 %) an Fi
  code <- if (isTRUE(sh$rust[1L] >= .ka5_thr("go_rust_min_pct"))) "Fo"
          else if (isTRUE(brown) && isTRUE(os >= .ka5_thr("f_os_min_pct"))) "Fh"
          else if (isTRUE(dark) || isTRUE(.ka5_lgl(x, i, "sulfide_odour"))) "Fr"
          else if (identical(where, "tidal")) "Fw"
          else if (isTRUE(os < .ka5_thr("f_os_min_pct"))) "Fi"
          else NA_character_
  if (is.na(code)) return(.ka5_r(NA, missing = c("munsell_value_moist", "oc_pct", "redoximorphic_features_pct"),
                                 note = "an F horizon (under water); its kind undecided", code = "F"))
  .ka5_r(TRUE, code = code, tag = if (code %in% c("Fh", "Fr")) "f_colour_cut",
         note = sprintf("%s: the site is %s; %s", code,
                                           if (where == "tidal") "in the tidal zone" else "under water", .kidpr_colour_text(col)))
}


# --- 2 the organic horizons (pp. 89-92) -----------------------------------------------------

# The suffixes of an H on the data (p. 90-91): m, v, a, t, w, r, p.
.ka5_h_suffix <- function(x, i) {
  st <- .ka5_txt(x, i, "structure_type"); sz <- .ka5_txt(x, i, "structure_size")
  vp <- .ka5_num(x, i, "von_post_index"); col <- .ka5_colour(x, i)
  wr <- .ka5_txt(x, i, "water_repellence")
  drained <- isTRUE(.ka5_site_lgl(x, "artificially_drained"))
  top <- identical(i, x$ord[1L]) || isTRUE(x$rtop[i] <= 0)
  sh <- .ka5_shares(x, i)
  days <- .ka5_num(x, i, "water_saturation_days")
  lu <- .ka5_site_txt(x, "land_use")
  notes <- character(0)
  s <- ""
  if (top && isTRUE(vp >= 7) && isTRUE(col$value <= 2) && (isTRUE(grepl("repellent", wr)) || isTRUE(grepl("single|powder", st)))) {
    s <- "m"; notes <- "black, strongly decomposed, powdery or water-repellent: vermulmt (Hm)"
  } else if (top && isTRUE(grepl("granul|crumb", st) || (grepl("subangular", st) && isTRUE(grepl("fine", sz)))) && drained) {
    s <- "v"; notes <- "crumb or fine subangular structure in a drained moor: vererdet (Hv)"
  } else if (!top && isTRUE(grepl("subangular|angular|block", st))) {
    s <- "a"; notes <- "subangular blocky separation fabric of a drained moor (Ha)"
  } else if (isTRUE(grepl("prism", st))) {
    s <- "t"; notes <- "coarse prisms of shrinkage (Ht)"
  } else if (isTRUE(sh$rust[1L] > 0)) {
    s <- "w"; notes <- "oxidation features: the fluctuation zone (Hw)"
  } else if (isTRUE(days > 300) || isTRUE(sh$reduction[1L] > 50)) {
    s <- "r"; notes <- "permanently saturated, reduced (Hr)"
  }
  if (top && isTRUE(grepl("arable|crop|acker|tillage", lu))) { s <- paste0(s, "p"); notes <- c(notes, "tilled (p)") }
  list(suffix = s, notes = notes)
}

.ka5_t_H <- function(x, i) {
  og <- .ka5_organic(x, i)
  if (!isTRUE(og$passed)) return(.ka5_r(og$passed, missing = og$missing, note = og$note, tag = og$tag, code = "H"))
  pt <- .ka5_peat(x, i)
  if (!isTRUE(pt$passed)) return(.ka5_r(pt$passed, missing = pt$missing, note = c(og$note, pt$note), tag = pt$tag, code = "H"))
  pm <- .ka5_txt(x, i, "peat_moor_type")
  pre <- if (is.na(pm)) "" else switch(pm, "high-moor" = "h", "transitional" = "u", "low-moor" = "n", "")
  sf <- .ka5_h_suffix(x, i)
  .ka5_r(TRUE, code = paste0(pre, "H", sf$suffix), note = c(og$note, pt$note, sf$notes,
         if (nzchar(pre)) sprintf("peat of the %s: prefix %s", pm, pre)), tag = unique(c(og$tag, pt$tag)))
}

.ka5_t_O <- function(x, i) {
  og <- .ka5_organic(x, i)
  if (!isTRUE(og$passed)) return(.ka5_r(og$passed, missing = og$missing, note = og$note, code = "O"))
  pt <- .ka5_peat(x, i)
  if (isTRUE(pt$passed)) return(.ka5_r(FALSE, note = "a peat: H", code = "O"))
  fo <- .ka5_num(x, i, "fine_organic_matter_pct")
  lim <- .ka5_thr("fine_substance_pct")
  if (is.na(fo)) return(.ka5_r(if (isFALSE(pt$passed)) TRUE else NA, code = "O", missing = c("fine_organic_matter_pct", pt$missing),
                               note = c(og$note, pt$note, "the fine substance not recorded: L, Of or Oh undecided (an O)")))
  code <- if (fo < lim[1L]) "L" else if (fo < lim[2L]) "Of" else "Oh"
  .ka5_r(if (isFALSE(pt$passed) || code == "L") TRUE else NA, code = code, missing = pt$missing,
         note = c(og$note, sprintf("fine substance %s vol-%%: %s (pp. 90-91)", format(fo), code)))
}


# --- 3 the anthropogenic and colluvial horizons (pp. 106-108) -------------------------------

.ka5_t_Y <- function(x, i) {
  gas <- .ka5_num(x, i, "soil_gas_ch4_co2_pct")
  src <- .ka5_site_txt(x, "reductgas_source")
  mo <- .ka5_site_lgl(x, "methane_odour")
  lim <- .ka5_thr("y_gas_min_pct")
  if (is.na(gas)) {
    # the gas is measured only where the site gives a reason to: a reducing-gas
    # source or a methane smell (soilKey's reading, Hugo,
    # 2026-11-06)
    if (isTRUE(src == "natural_wetness")) return(.ka5_r(FALSE, note = "the reducing gas comes from natural wetness: no Y (Q20)"))
    if (is.na(src) && !isTRUE(mo)) return(.ka5_r(FALSE, note = "no reducing gas recorded at the site"))
    p <- NA
  } else p <- gas >= lim
  if (isFALSE(p)) return(.ka5_r(FALSE, note = sprintf("CH4 and CO2 %s vol-%% (under 10)", format(gas))))
  sh <- .ka5_shares(x, i)
  code <- if (isTRUE(sh$reduction[1L] > 50)) "Yr" else if (isTRUE(sh$rust[1L] > 0)) "Yo" else "Y"
  .ka5_r(p, code = code, missing = "soil_gas_ch4_co2_pct",
         note = c(if (!is.na(gas)) sprintf("CH4 and CO2 %s vol-%% (10 or more, p. 108)", format(gas))
                  else sprintf("a reducing-gas source recorded (%s), the gas not measured", if (is.na(src)) "methane smell" else src)))
}

# The mixed zone of deep cultivation (R): the kind recorded and its depth.
.ka5_t_R <- function(x, i) {
  k <- .ka5_site_txt(x, "deep_cultivation_kind")
  dep <- .ka5_site_num(x, "mechanical_mixing_depth_cm")
  if (is.na(k)) {
    pu <- .ka5_site_txt(x, "mechanical_mixing_purpose")
    if (is.na(dep) || dep <= .ka5_thr("r_min_cm")) return(.ka5_r(FALSE, note = "no deep cultivation recorded"))
    k <- if (is.na(pu)) NA_character_ else pu
  }
  if (!is.na(k) && !k %in% c("repeated_trenching", "single_deep_ploughing") && !grepl("trench|plough|rigol", k))
    return(.ka5_r(FALSE, note = sprintf("deep cultivation of the kind '%s': no R", k)))
  if (is.na(dep)) return(.ka5_r(NA, missing = "mechanical_mixing_depth_cm",
                                note = "deep cultivation recorded, its depth not: the R undecided"))
  if (dep <= .ka5_thr("r_min_cm")) return(.ka5_r(FALSE, note = sprintf("mixed to %s cm, not over 4 dm (p. 107)", format(dep))))
  if (!isTRUE(x$rtop[i] < dep)) return(.ka5_r(FALSE, note = sprintf("below the mixed zone (%s cm)", format(dep))))
  lu <- .ka5_site_txt(x, "land_use")
  first <- identical(i, .ka5_first_mineral(x))
  code <- if (first && isTRUE(grepl("arable|crop|acker|tillage|vine", lu))) "R-Ap" else "R"
  .ka5_r(TRUE, code = code, note = sprintf("within the zone mixed by deep cultivation to %s cm (over 4 dm, p. 107)", format(dep)))
}

.ka5_t_E <- function(x, i) {
  k <- .ka5_site_txt(x, "deep_cultivation_kind")
  first <- identical(i, .ka5_first_mineral(x))
  if (first) return(.ka5_r(FALSE, note = "the topsoil: an Ah or Ap, not the E below it"))
  if (!isTRUE(k %in% c("plaggen", "garden"))) {
    w <- .kidpr_or(.kidpr_wrb_layer(x, "plaggic", i), .kidpr_wrb_layer(x, "hortic", i))
    if (is.na(k) && isTRUE(w))
      return(.ka5_r(NA, missing = "deep_cultivation_kind", tag = c("plaggic", "hortic"),
                    note = "no plaggen or garden culture recorded: soilKey's WRB plaggic or hortic horizon holds, flagged (a candidate)"))
    return(.ka5_r(FALSE, note = "no plaggen or garden culture recorded"))
  }
  # in the run under the topsoil
  prev <- .ka5_prev(x, i)
  above_ok <- !is.na(prev) && (identical(prev, .ka5_first_mineral(x)) || isTRUE(grepl("^E", .ka5_data_layer(x, prev)$code)))
  if (!above_ok) return(.ka5_r(FALSE, note = "not in the run under the topsoil"))
  am <- .ka5_ah_min(x, i)
  dep <- .ka5_site_num(x, "mechanical_mixing_depth_cm")
  # thicker than the plough depth: mechanical_mixing_depth_cm read as the
  # plough depth; not recorded, undecided (Hugo's decision Q23)
  plough <- if (is.na(dep)) NA else x$rbot[i] > dep
  cult <- .kidpr_or(isTRUE(.ka5_num(x, i, "artefacts_pct") > 0), if (is.na(.ka5_num(x, i, "p2o5_al_mg_kg"))) NA else TRUE)
  p <- .kidpr_and(am$passed, plough, cult)
  bio <- .ka5_txt(x, i, "bioturbation_density"); wh <- .ka5_num(x, i, "worm_holes_pct")
  code <- if (identical(k, "garden") && (isTRUE(bio %in% c("common", "many")) || isTRUE(wh > 0))) "Ex" else "E"
  .ka5_r(p, code = code, missing = unique(c(am$missing, if (is.na(plough)) "mechanical_mixing_depth_cm", if (is.na(cult)) "p2o5_al_mg_kg")),
         note = c(sprintf("%s culture recorded", k), am$note,
                  if (is.na(plough)) "the plough depth not recorded: thicker than it undecided (Q23)",
                  if (code == "Ex") "marked bioturbation: Ex"))
}

.ka5_t_M <- function(x, i) {
  if (identical(i, .ka5_first_mineral(x))) return(.ka5_r(FALSE, note = "the topsoil"))
  lo <- .ka5_txt(x, i, "layer_origin")
  colluvial <- isTRUE(grepl("colluvi|solimov|fluvi|alluvi", lo))
  if (!colluvial) return(.ka5_r(FALSE, note = "no colluvial or fluvial solum material recorded"))
  am <- .ka5_ah_min(x, i)
  col <- .ka5_colour(x, i)
  ch3 <- if (is.na(col$chroma)) NA else col$chroma >= 3
  # (Ah humus OR chroma 3 or more) AND stratification AND no dominant other
  # features (slip 22, default 18)
  sp <- .ka5_txt(x, i, "stratification_pattern")
  strat <- if (!is.na(sp)) sp %in% c("continuous", "interrupted", "lithologic_break") else NA
  tag <- character(0); snote <- character(0)
  if (is.na(strat)) {
    w <- .ka5_stand_in(x, i, "fluvic_material", "the stratification")
    strat <- if (isTRUE(w$p)) TRUE else FALSE
    if (isTRUE(w$p)) { tag <- w$tag; snote <- w$note } else snote <- "no stratification recorded: absent"
  }
  sh <- .ka5_shares(x, i)
  dom <- isTRUE(sh$s[1L] >= .ka5_thr("s_transition_pct")[2L]) || isTRUE(sh$rust[1L] >= .ka5_thr("go_rust_min_pct"))
  p <- .kidpr_and(.kidpr_or(am$passed, ch3), strat, !dom)
  code <- if (.ka5_floodplain(x, i)) "aM" else "M"
  .ka5_r(p, code = code, missing = am$missing, tag = tag,
         note = c(sprintf("solum material of '%s' origin", lo), am$note, snote, if (dom) "dominant hydromorphic features"))
}


# --- 4 the groundwater horizons (pp. 104-106; Q13) ------------------------------------------

.ka5_t_G <- function(x, i) {
  sh <- .ka5_shares(x, i)
  rust <- sh$rust; red <- sh$reduction
  r5 <- .ka5_thr("go_rust_min_pct")
  gw <- .ka5_groundwater(x, i)
  base_note <- c(sprintf("rust %s %%, reduction colours %s %%", .ka5_range_txt(rust), .ka5_range_txt(red)), sh$notes)
  if (isFALSE(gw$passed)) return(.ka5_r(FALSE, code = "G", note = c(base_note, gw$note)))
  if (isTRUE(.ka5_organic(x, i)$passed)) return(.ka5_r(FALSE, code = "G", note = "organic"))
  ox <- .ka5_site_lgl(x, "groundwater_oxygen_rich")
  rh <- .ka5_num(x, i, "rh_value"); ch <- .ka5_txt(x, i, "childs_test")
  red_test <- .kidpr_or(if (is.na(rh)) NA else rh <= .ka5_thr("rh_max"),
                        if (isTRUE(ch == "positive")) TRUE else if (isTRUE(ch == "negative")) FALSE else NA)
  # Go, Gro, Gor (Q13): [rust 5 % or more] and reduction colours under 10,
  # 10 to 50, over 50, and [the fluctuation zone or oxygen-rich water]
  if (isTRUE(.ka5_at_least(rust, r5))) {
    kind <- if (isTRUE(.ka5_at_least(red, .ka5_thr("gor_reduction_min_pct") + 1e-9))) "Gor"
            else if (isTRUE(.ka5_at_least(red, .ka5_thr("gro_reduction_pct")[1L])) &&
                     isTRUE(.ka5_below(red, .ka5_thr("gro_reduction_pct")[2L] + 1e-9))) "Gro"
            else if (isTRUE(.ka5_below(red, .ka5_thr("go_reduction_max_pct")))) "Go" else NA_character_
    zone <- .kidpr_or(gw$passed, if (isTRUE(ox)) TRUE else NA)
    if (is.na(kind)) return(.ka5_r(.kidpr_and(zone, NA), code = "Go", value = c("Go", "Gro", "Gor"),
                                   missing = c("reduction_colours_pct", gw$missing), tag = gw$tag,
                                   note = c(base_note, gw$note, "the share of reduction colours undecided: Go, Gro or Gor")))
    return(.ka5_r(zone, code = kind, missing = gw$missing, tag = gw$tag,
                  note = c(base_note, gw$note, sprintf("the %s (pp. 104-105)", kind))))
  }
  if (!isTRUE(.ka5_below(rust, r5))) return(.ka5_r(NA, code = "G", missing = "redoximorphic_features_pct", note = base_note))
  # Gr: rust under 5, reduction colours over 95, rH 19 or less or a positive
  # dipyridyl test (p. 105)
  if (isTRUE(.ka5_at_least(red, .ka5_thr("gr_reduction_min_pct") + 1e-9))) {
    p <- .kidpr_and(red_test, gw$passed)
    return(.ka5_r(p, code = "Gr", missing = unique(c(if (is.na(red_test)) c("rh_value", "childs_test"), gw$missing)),
                  tag = unique(c(gw$tag, sh$tags)),
                  note = c(base_note, gw$note, if (is.na(red_test)) "a Gr if rH 19 or less or the dipyridyl test positive (undecided)"
                           else if (isTRUE(red_test)) "rH 19 or less or a positive dipyridyl test: the Gr (p. 105)"
                           else "rH over 19 and a negative dipyridyl test: no Gr")))
  }
  if (anyNA(red) && isTRUE(gw$passed))
    return(.ka5_r(NA, code = "Gr", missing = c("reduction_colours_pct", "munsell_hue_moist"), note = c(base_note, gw$note,
                  "groundwater recorded; the reduction colours not: a Gr undecided")))
  # Gw: above a Gr, no features, the dipyridyl test negative (p. 105)
  nx <- .ka5_next(x, i)
  nofeat <- isTRUE(rust[2L] == 0) && isTRUE(red[2L] == 0) && isTRUE(sh$bleached[2L] == 0)
  if (nofeat && isTRUE(gw$passed) && !identical(i, .ka5_first_mineral(x)) && !is.na(nx) && isTRUE(.ka5_t_G_raw_gr(x, nx))) {
    p <- if (isTRUE(ch == "negative")) TRUE else if (isTRUE(ch == "positive")) FALSE else NA
    return(.ka5_r(p, code = "Gw", missing = "childs_test", note = "no features above a Gr: a Gw if the dipyridyl test is negative (p. 105)"))
  }
  .ka5_r(FALSE, code = "G", note = c(base_note, "no G horizon"))
}

# The matrix hue of the Gr set used for the reduction colours read from the
# matrix: N, 5Y to 10Y, GY, G, BG, B, PB, with a moist value of 4 or more
# (dark humus colours are not read as reduction colours: soilKey's reading,
# Hugo, 2026-11-06).
.ka5_gr_set_hue <- function(x, i) {
  if (!is.na(.ka5_num(x, i, "reduction_colours_pct"))) return(TRUE)
  col <- .ka5_colour(x, i)
  if (is.na(col$family) || is.na(col$value)) return(NA)
  (col$family %in% c("N", "GY", "G", "BG", "B", "PB") || (col$family == "Y" && isTRUE(col$number >= 5))) && col$value >= 4
}

# A Gr on the raw tests of a row (for the Gw above it).
.ka5_t_G_raw_gr <- function(x, i) {
  sh <- .ka5_shares(x, i)
  .kidpr_and(.ka5_below(sh$rust, .ka5_thr("go_rust_min_pct")), .ka5_at_least(sh$reduction, 95 + 1e-9))
}


# --- 5 the perched-water horizons (pp. 101-103) ---------------------------------------------

.ka5_t_S <- function(x, i) {
  sh <- .ka5_shares(x, i)
  s <- sh$s
  kd <- .ka5_txt(x, i, "knick_degree")
  if (!is.na(kd)) return(.ka5_r(TRUE, code = "Sq", note = sprintf("a %s Knick horizon recorded (p. 103)", kd)))
  gw <- .ka5_groundwater(x, i)
  s80 <- .ka5_at_least(s, .ka5_thr("sw_shares_min_pct"))
  s50 <- .ka5_at_least(s, .ka5_thr("sd_shares_min_pct"))
  base_note <- c(sprintf("bleaching and rust %s %% of the area", .ka5_range_txt(s)), sh$notes)
  if (isFALSE(s50)) return(.ka5_r(FALSE, note = c(base_note, "under 50 %: no S horizon")))
  perched <- .kidpr_not(gw$passed)
  if (isFALSE(perched)) return(.ka5_r(FALSE, note = c(base_note, gw$note, "groundwater: a G, not an S")))
  ks <- .ka5_num(x, i, "ksat_mm_h"); ac <- .ka5_num(x, i, "air_capacity_pct")
  kl <- .ka5_thr("ksat_s_mm_h")
  rh <- .ka5_num(x, i, "rh_value"); ch <- .ka5_txt(x, i, "childs_test")
  red_test <- .kidpr_or(if (is.na(rh)) NA else rh <= .ka5_thr("rh_max"), if (isTRUE(ch == "positive")) TRUE else NA)
  days <- .ka5_num(x, i, "water_saturation_days")
  loc <- .ka5_txt(x, i, "redox_segregations_location")
  col <- .ka5_colour(x, i)
  miss <- unique(c(gw$missing, "ksat_mm_h", "air_capacity_pct"))
  tag <- gw$tag
  # Sg: 80 % or more with Sd features, air shortage at field capacity, silt
  # and finest sand 50 % or more (Q11)
  si <- .ka5_num(x, i, "silt_pct"); ff <- .ka5_num(x, i, "sand_ffs_pct")
  sf <- if (!is.na(si) && !is.na(ff)) si + ff >= .ka5_thr("sg_silt_ffs_min_pct")
        else { s50s <- .ka5_silt50(x, i)$p; if (isTRUE(s50s)) TRUE else NA }
  sg <- .kidpr_and(s80, perched, if (is.na(ac)) NA else ac < .ka5_thr("air_capacity_max_pct"), sf)
  if (isTRUE(sg)) {
    cl <- .ka5_num(x, i, "clay_pct"); b <- .ka5_thr("sdg_clay_pct")
    return(.ka5_r(TRUE, code = if (isTRUE(cl >= b[1L] && cl < b[2L])) "Sdg" else "Sg", tag = tag,
                  note = c(base_note, "air shortage at field capacity, rich in silt and finest sand: the Sg (p. 103)")))
  }
  # Srw: wet 200 to 300 days, rust only on the root channels
  dd <- .ka5_thr("srw_days")
  if (isTRUE(s80) && isTRUE(days >= dd[1L] && days <= dd[2L]) && isTRUE(loc == "voids") && !isFALSE(perched))
    return(.ka5_r(perched, code = if (isTRUE(.ka5_pale(x, i)$passed)) "Serw" else "Srw", missing = gw$missing, tag = tag,
                  note = c(base_note, sprintf("wet %s days a year, rust on the root channels: the Srw (p. 102)", format(days)))))
  # Srd: Sd features with reduction features and permanent air shortage
  # (the Srd's numbers and the Srw's voids are soilKey's conventions, D15)
  if (isTRUE(s50) && isTRUE(sh$reduction[1L] > .ka5_thr("srd_reduction_min_pct")) && isTRUE(ac < .ka5_thr("air_capacity_max_pct")) &&
      isTRUE(days > .ka5_thr("srd_days_min")))
    return(.ka5_r(perched, code = "Srd", missing = gw$missing, tag = tag, note = c(base_note, "reduction features, permanent air shortage: the Srd")))
  # the Sw over the Sd: permeability (kf 10 cm/d) and the S below
  nx <- .ka5_next(x, i)
  ks_nx <- if (!is.na(nx) && .ka5_same_layer(x, i, nx) || (!is.na(nx) && x$layer[nx] > x$layer[i])) .ka5_num(x, nx, "ksat_mm_h") else NA
  sw <- .kidpr_and(s80, perched, red_test, if (is.na(ks)) NA else ks >= kl, if (is.na(ks) || is.na(ks_nx)) NA else ks > ks_nx)
  sd <- .kidpr_and(s50, perched, if (is.na(ks)) NA else ks < kl, if (is.na(ac)) NA else ac < .ka5_thr("air_capacity_max_pct"))
  sew <- .kidpr_and(s80, .ka5_pale(x, i)$passed, .ka5_below(sh$rust, .ka5_thr("sew_rust_max_pct")))
  if (isTRUE(sw)) return(.ka5_r(TRUE, code = if (isTRUE(sew)) "Sew" else "Sw", tag = tag,
                                note = c(base_note, sprintf("kf %s mm/h, more than the horizon below: the Sw (p. 102)", format(ks)))))
  if (isTRUE(sd)) return(.ka5_r(TRUE, code = "Sd", tag = tag,
                                note = c(base_note, sprintf("kf %s mm/h, air capacity %s vol-%%: the Sd (p. 103)", format(ks), format(ac)))))
  if (isFALSE(sw) && isFALSE(sd)) return(.ka5_r(FALSE, note = c(base_note, "neither the Sw nor the Sd")))
  # undecided between the Sw and the Sd
  cands <- c(if (!isFALSE(sw)) (if (isTRUE(sew)) "Sew" else "Sw"), if (!isFALSE(sd)) "Sd")
  .ka5_r(NA, code = cands[1L], missing = miss, tag = tag, value = cands,
         note = c(base_note, gw$note, sprintf("an S horizon: %s undecided (permeability, air capacity and rH not recorded)",
                                              paste(cands, collapse = " or "))))
}


# --- 6 the topsoils (pp. 92-95) ---------------------------------------------------------------

# Is a row in the run of topsoils: the first mineral row, or under a row the
# data read as an A (the same layer).
.ka5_in_topsoil <- function(x, i) {
  f <- .ka5_first_mineral(x)
  if (is.na(f)) return(FALSE)
  # no run of topsoils under 30 cm or more of H (D19)
  hrows <- x$ord[x$rank < x$rank[f]]
  # the organic rows above that are or may be peat (not a forest floor)
  hth <- sum(vapply(hrows, function(j) if (!isFALSE(.ka5_peat(x, j)$passed) && isTRUE(.ka5_organic(x, j)$passed)) x$thick[j] %||% 0 else 0, 0), na.rm = TRUE)
  if (hth >= .ka5_thr("h_topsoil_block_cm")) return(FALSE)
  if (identical(i, f)) return(TRUE)
  # an FAO A or E at the GOF (under an FAO O) starts the run; under an FAO
  # A or E of the run, an undecided row is still in it (v0.9.380)
  fi <- .ka5_fao_at(x, i)
  if (!is.null(fi) && any(fi$masters %in% c("A", "E")) && identical(i, x$gof_row)) return(TRUE)
  p <- .ka5_prev(x, i)
  if (is.na(p) || !.ka5_same_layer(x, i, p) || x$rank[p] < x$rank[f]) return(FALSE)
  pc <- .ka5_data_layer(x, p)
  if (isTRUE(substr(pc$carrying %||% "", 1L, 1L) == "A")) return(TRUE)
  fp <- .ka5_fao_at(x, p)
  !is.null(fi) && !is.null(fp) && all(fp$masters %in% c("A", "E")) && any(fi$masters %in% c("A", "E")) &&
    .ka5_in_topsoil(x, p)
}

.ka5_t_A <- function(x, i) {
  if (isTRUE(.ka5_organic(x, i)$passed)) return(.ka5_r(FALSE, note = "organic"))
  top <- .ka5_in_topsoil(x, i)
  first <- identical(i, .ka5_first_mineral(x))
  # v0.9.364 (Hugo, 2026-11-06): the eluvial Al of a Parabraunerde under a
  # podzolic B (the Ae/B(h)s/Al/Bt of the Parabraunerde-Podsol, p. 223)
  pv <- .ka5_prev(x, i)
  under_pz <- !top && !first && !is.na(pv) && isTRUE(.ka5_fao_gate(x, pv, .ka5_t_Bsp(x, pv))$passed)
  if (!top && !under_pz) return(.ka5_r(FALSE, note = "not in the run of topsoils"))
  if (.ka5_rock_described(x, i)) return(.ka5_r(FALSE, note = "described as rock"))
  os <- .ka5_os(x, i)
  am <- .ka5_ah_min(x, i)
  th <- x$thick[i]
  # Aa: organic substance 15 to < 30 %, under ground or perched water
  aa <- .ka5_thr("aa_os_pct")
  if (isTRUE(.ka5_at_least(os$range, aa[1L])) && isTRUE(.ka5_below(os$range, aa[2L]))) {
    wet <- !is.na(.ka5_water_regime(x)) || isTRUE(.ka5_shares(x, i)$s[1L] > 0)
    return(.ka5_r(if (wet) TRUE else NA, code = "Aa", missing = "groundwater_kind",
                  note = c(os$note, "organic substance 15 to < 30 %: anmoorig (Aa, p. 94)",
                           if (!wet) "ground or perched water not recorded")))
  }
  pale <- .ka5_pale(x, i)
  # under an FAO E: the horizon below its sub-horizons and the E-B
  # transitions (E1/E2/EB/Bt: the Bt; v0.9.380)
  nx <- .ka5_fao_under_eluvial(x, i)
  # Ae: pale, over a Bh or Bs (p. 94)
  if (!under_pz && (!first || isTRUE(pale$passed))) {
    bsp <- if (!is.na(nx)) .ka5_fao_gate(x, nx, .ka5_t_Bsp(x, nx)) else .ka5_r(FALSE)
    # v0.9.364 (Hugo, 2026-11-06): the first row, pale and humic, over a pale
    # Ae over a Bh or Bs is the Ahe of the Ahe/Ae/B(h)s (p. 94)
    if (first && isFALSE(bsp$passed) && !is.na(nx) && isTRUE(.ka5_pale(x, nx)$passed)) {
      n2 <- .ka5_next(x, nx)
      b2 <- if (!is.na(n2)) .ka5_fao_gate(x, n2, .ka5_t_Bsp(x, n2)) else .ka5_r(FALSE)
      if (isTRUE(b2$passed)) bsp <- .ka5_r(b2$passed, code = sprintf("pale Ae over a %s", b2$code %||% "Bh or Bs"))
    }
    if (isTRUE(bsp$passed) || is.na(bsp$passed)) {
      p <- .kidpr_and(pale$passed, bsp$passed)
      if (is.na(pale$passed)) {
        w <- .ka5_stand_in(x, i, "albic", "the colour")
        if (isFALSE(w$p)) p <- FALSE
      }
      if (!isFALSE(p)) {
        code <- if (isTRUE(am$passed) && first) "Ahe" else "Ae"
        return(.ka5_r(p, code = code, missing = pale$missing,
                      note = c(pale$note, sprintf("over a %s", bsp$code %||% "Bh or Bs"), if (code == "Ahe") am$note)))
      }
    }
  }
  # Al, Ael: over a Bt, lighter than it (p. 95)
  if (!first && !is.na(nx)) {
    bt <- .ka5_fao_gate(x, nx, .ka5_t_Bt(x, nx))
    if (!isFALSE(bt$passed)) {
      cn <- .ka5_colour(x, i); cb <- .ka5_colour(x, nx)
      lighter <- if (is.na(cn$value) || is.na(cb$value)) NA else cn$value > cb$value
      p <- .kidpr_and(bt$passed, lighter)
      if (!isFALSE(p)) {
        pale_ael <- if (is.na(cn$value) || is.na(cn$chroma)) NA else cn$value > .ka5_thr("ael_value_moist_min") && cn$chroma < .ka5_thr("ael_chroma_moist_max")
        lf <- .ka5_clay_diff(x, i, nx, "lf")
        code <- if (isTRUE(pale_ael) && isTRUE(lf$passed)) "Ael" else "Al"
        # the colour not recorded and the Fahlerde difference not failed: Al
        # or Ael undecided (D11)
        if (is.na(pale_ael) && !isFALSE(lf$passed))
          return(.ka5_r(.kidpr_and(p, NA), code = "Al", value = c("Al", "Ael"), missing = c("munsell_value_moist", "munsell_chroma_moist"),
                        note = c(sprintf("over a Bt (%s): Al or Ael, the colour not recorded", paste(bt$note, collapse = "; ")), lf$note)))
        return(.ka5_r(p, code = code, missing = c("munsell_value_moist", bt$missing),
                      note = c(sprintf("over a Bt (%s), lighter: %s", paste(bt$note, collapse = "; "), code),
                               if (code == "Ael") c("pale grey (p. 219)", lf$note),
                               if (under_pz) "under a podzolic B (p. 223)")))
      }
    }
  }
  if (under_pz) return(.ka5_r(FALSE, note = "under a podzolic B, over no Bt: not an Al"))
  # Axh, Acxh: 1 dm or more, base saturation 50 % or more, marked
  # bioturbation and stable aggregates (p. 93)
  bs <- .ka5_num(x, i, "bs_pct")
  bio <- .ka5_txt(x, i, "bioturbation_density"); wh <- .ka5_num(x, i, "worm_holes_pct")
  bioturb <- if (!is.na(wh)) wh > 0 else if (!is.na(bio)) bio %in% c("common", "many") else NA
  st <- .ka5_txt(x, i, "structure_type")
  stable <- if (is.na(st)) NA else grepl("granul|crumb", st)
  tag <- character(0); bnote <- character(0)
  if (is.na(bioturb)) {
    w <- .ka5_stand_in(x, i, "mollic", "the bioturbation")
    if (isTRUE(w$p)) { bioturb <- NA; tag <- w$tag; bnote <- w$note } else { bioturb <- FALSE; bnote <- "no bioturbation recorded: absent" }
  }
  axh <- .kidpr_and(am$passed, if (is.na(th)) NA else th >= .ka5_thr("axh_min_cm"), if (is.na(bs)) NA else bs >= .ka5_thr("axh_min_bs_pct"),
                    bioturb, stable)
  lu <- .ka5_site_txt(x, "land_use")
  ploughed <- first && isTRUE(grepl("arable|crop|acker|tillage|cultivat", lu))
  # the FAO p states the tillage of the Ap (WRB 2022 Annex 3, p. 227), on
  # any row of the topsoil (v0.9.380)
  if (.ka5_fao_has(x, i, "p", "A")) ploughed <- TRUE
  sc <- isTRUE(.ka5_num(x, i, "secondary_carbonates_pct") > 0)
  if (!isFALSE(axh) && (isTRUE(bioturb) || is.na(bioturb))) {
    code <- paste0("A", if (sc) "c" else "", "x", if (ploughed) "p" else "h")
    return(.ka5_r(axh, code = code, missing = c("bs_pct", "worm_holes_pct", "structure_type"), tag = tag,
                  note = c(am$note, bnote, sprintf("bioturbated topsoil: %s (p. 93)", code))))
  }
  if (ploughed && !isFALSE(am$passed)) {
    return(.ka5_r(am$passed, code = if (sc) "Acp" else "Ap", missing = am$missing,
                  note = c(am$note, sprintf("the topsoil of land used as '%s': ploughed (Ap)", lu))))
  }
  if (isTRUE(am$passed)) {
    # under the first row, a humic layer browner than the one below is a Bv
    # with the Ah minimum (Ah-Bv), not an Ah
    # under the first row, a humic layer that is a Bh, Bs, Bt or Bv is that B
    # (with the Ah minimum: Ah-Bv), not an Ah
    if (!first) {
      bb <- vapply(list(.ka5_t_Bsp(x, i), .ka5_t_Bt(x, i), .ka5_t_Bv(x, i)), function(r) r$passed, NA)
      # an FAO A (not AB, BA, AC ...) answers the open question: no B
      # horizon; a B measured on the row stays (a conflict, H3; v0.9.380)
      if (.ka5_fao_excludes(x, i, "Bv") && .ka5_fao_excludes(x, i, "Bt") && .ka5_fao_excludes(x, i, "Bs")) bb[is.na(bb)] <- FALSE
      if (any(bb %in% TRUE))
        return(.ka5_r(FALSE, code = "Ah", note = c(am$note, "a B horizon with the Ah humus minimum, not an Ah")))
      if (any(is.na(bb)))
        return(.ka5_r(NA, code = "Ah", missing = am$missing, note = c(am$note, "an Ah or a B horizon with the Ah minimum undecided")))
    }
    return(.ka5_r(TRUE, code = if (sc) "Ach" else "Ah", note = am$note))
  }
  # Ai: the first mineral row, under the Ah minimum (or patchy and thin)
  if (first) {
    hc <- .ka5_txt(x, i, "horizon_continuity")
    if (isFALSE(am$passed) || (isTRUE(hc == "patchy") && isTRUE(th < .ka5_thr("ai_patchy_max_cm"))))
      return(.ka5_r(TRUE, code = "Ai", note = c(am$note, "an initial A: humus under the Ah minimum (p. 92)")))
    return(.ka5_r(NA, code = "Ah", value = c("Ah", "Ai"), missing = am$missing, note = c(am$note, "Ah or Ai undecided")))
  }
  .ka5_r(if (is.na(am$passed)) NA else FALSE, code = "Ah", missing = am$missing, note = am$note)
}


# --- 7 the subsoils B, P, T (pp. 95-101) ---------------------------------------------------

# A B horizon at all: not the first mineral row, no lithogenic carbonate in
# the fine earth, coarse fragments under 75 % (p. 95).
.ka5_b_gate <- function(x, i) {
  if (identical(i, .ka5_first_mineral(x)) || isTRUE(.ka5_organic(x, i)$passed)) return(.ka5_r(FALSE, note = "not a subsoil"))
  if (.ka5_rock_described(x, i)) return(.ka5_r(FALSE, note = "described as rock (diggable or not, its weathering, or the carbonate of the whole rock recorded): a C"))
  cf <- .ka5_num(x, i, "coarse_fragments_pct")
  if (isTRUE(cf >= .ka5_thr("b_coarse_max_pct"))) return(.ka5_r(FALSE, note = sprintf("coarse fragments %s %%", format(cf))))
  .ka5_no_lithogenic_carbonate(x, i)
}

# Bh, Bsh, Bhs, Bs: Cp/Fep over 10, 3 to 10, under 3 (p. 97); the Bs is a
# page redder than above and below; cemented: B(h)ms.
.ka5_t_Bsp <- function(x, i) {
  .ka5_memo(x, sprintf("bsp_%d", i), function() {
    if (identical(i, .ka5_first_mineral(x)) || isTRUE(.ka5_organic(x, i)$passed)) return(.ka5_r(FALSE, code = "Bs", note = "not a subsoil"))
    cp <- .ka5_num(x, i, "c_pyro_pct"); fp <- .ka5_num(x, i, "fe_pyro_pct")
    cm <- .ka5_txt(x, i, "cementation_class")
    m <- if (isTRUE(cm %in% c("strongly", "indurated", "moderately"))) "m" else ""
    if (is.na(cp) || is.na(fp) || fp <= 0) {
      w <- .ka5_stand_in(x, i, "spodic", "Cp/Fep")
      p <- if (isTRUE(w$p)) NA else FALSE
      # v0.9.364 (Hugo, 2026-11-06): humus or oxalate Fe and Al accumulated
      # under a pale horizon leave a Bh or Bs open without its Cp/Fep
      films <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
      if (isFALSE(p) && !isTRUE(films >= 3L) && isTRUE(.ka5_podzolic_signs(x, i)$passed)) {
        p <- NA
        w$note <- c(w$note, "podzolic signs under a pale horizon")
      }
      return(.ka5_r(p, code = paste0("B", if (m == "m") "hms" else "hs"), missing = c("c_pyro_pct", "fe_pyro_pct"), tag = w$tag,
                    note = c(w$note, if (is.na(p)) "a Bh or Bs (a candidate, never decided by the stand-in alone)")))
    }
    r <- cp / fp
    lims <- .ka5_thr("bhs_cp_fep")
    pr <- .ka5_prev(x, i); nx <- .ka5_next(x, i)
    if (r > .ka5_thr("bh_cp_fep_min")) {
      up <- if (!is.na(pr)) .ka5_num(x, pr, "oc_pct") else NA
      p <- if (is.na(up) || is.na(.ka5_num(x, i, "oc_pct"))) NA else .ka5_num(x, i, "oc_pct") > up
      return(.ka5_r(p, code = paste0("B", "h", m), missing = "oc_pct",
                    note = sprintf("Cp/Fep %s, over 10, humus more than above: the Bh (p. 97)", format(signif(r, 3)))))
    }
    if (r >= lims[1L]) return(.ka5_r(TRUE, code = paste0("Bh", m, "s"),
                                     note = sprintf("Cp/Fep %s, 3 to 10: the Bhs or Bsh (pp. 97, 222)", format(signif(r, 3)))))
    redder <- function(j) if (is.na(j)) NA else { a <- .ka5_hue_page(.ka5_colour(x, i)); b <- .ka5_hue_page(.ka5_colour(x, j))
      if (is.na(a) || is.na(b)) NA else b - a >= 1 - 1e-9 }
    p <- .kidpr_and(redder(pr), if (is.na(nx)) TRUE else redder(nx))
    .ka5_r(p, code = paste0("B", m, "s"), missing = "munsell_hue_moist",
           note = sprintf("Cp/Fep %s, under 3, %s: the Bs (p. 97)", format(signif(r, 3)),
                          if (is.na(p)) "a page redder than above and below undecided" else if (p) "a page redder than above and below" else "not redder"))
  })
}

# The signs of a podzolic illuvial horizon in a B (v0.9.364; Hugo,
# 2026-11-06): under a pale horizon (the Ae colour, p. 94), humus or oxalate
# Fe and Al (Al + 1/2 Fe) accumulated against it. TRUE (a sign measured),
# FALSE (none, or no pale horizon above), NA (the pale horizon above, the
# signs not measured).
.ka5_podzolic_signs <- function(x, i) {
  pr <- .ka5_prev(x, i)
  pale <- if (is.na(pr)) FALSE else .ka5_pale(x, pr)$passed
  if (!isTRUE(pale)) return(.ka5_r(FALSE, note = "no pale horizon above: no podzolic signs read"))
  if (isFALSE(.ka5_b_gate(x, i)$passed)) return(.ka5_r(FALSE, note = "not a B"))
  ox <- function(j) { a <- .ka5_num(x, j, "al_ox_pct"); f <- .ka5_num(x, j, "fe_ox_pct"); if (is.na(a) || is.na(f)) NA else a + f / 2 }
  oo <- c(ox(i), ox(pr))
  # illuvial humus: the B at the humus minimum of an Ah (p. 93), the pale
  # horizon above under it
  ha <- .ka5_ah_min(x, i)$passed; hp <- .ka5_ah_min(x, pr)$passed
  hum <- if (is.na(ha) || is.na(hp)) NA else ha && !hp
  oxa <- if (anyNA(oo)) NA else oo[1L] > oo[2L]
  p <- if (isTRUE(hum) || isTRUE(oxa)) TRUE else if (is.na(hum) && is.na(oxa)) NA else FALSE
  .ka5_r(p, missing = c(if (is.na(hum)) "oc_pct", if (is.na(oxa)) c("al_ox_pct", "fe_ox_pct")),
         note = sprintf("podzolic signs under the pale horizon above: %s",
                        if (is.na(p)) "not measured" else if (p) "humus or oxalate Fe and Al accumulated" else "none"))
}

# Bt: clay coatings (common or many clay films, over 1 % in thin section, or
# clay bridges) and the clay difference to the eluvial horizon above (p. 97).
.ka5_t_Bt <- function(x, i) {
  .ka5_memo(x, sprintf("bt_%d", i), function() {
    g <- .ka5_b_gate(x, i)
    if (isFALSE(g$passed)) return(.ka5_r(FALSE, code = "Bt", note = g$note))
    cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
    ts <- .ka5_num(x, i, "clay_coatings_thin_section_pct"); br <- .ka5_lgl(x, i, "clay_bridges")
    obs <- c(if (is.na(cf)) NA else cf >= 3L, if (is.na(ts)) NA else ts > .ka5_thr("bt_thin_section_min_pct"), br)
    # one of the three recorded decides; the others not recorded are absent
    coat <- if (any(obs %in% TRUE)) TRUE else if (any(obs %in% FALSE)) FALSE else NA
    tag <- character(0); cnote <- character(0)
    if (is.na(coat) && .ka5_fao_has(x, i, "t", "B")) {
      # the FAO t states clay coatings, lamellae or bridges (WRB 2022 Annex
      # 3, p. 227): the field observation of the Bt where none is recorded
      # (v0.9.380)
      coat <- TRUE; cnote <- "no clay coatings recorded: the FAO t states them present (not their strength, p. 97); the clay difference decides"
    } else if (is.na(coat)) {
      w <- .ka5_stand_in(x, i, "argic", "the clay coatings")
      # a field observation not recorded is absent; soilKey's argic horizon
      # stands in for it, flagged, beside the measured clay difference
      coat <- isTRUE(w$p); tag <- if (isTRUE(w$p)) w$tag else character(0)
      cnote <- if (isTRUE(w$p)) w$note else "no clay coatings recorded: absent"
    } else cnote <- if (coat) "clay coatings recorded" else "no clay coatings recorded"
    bands <- .ka5_num(x, i, "bands_area_pct")
    # a sub-horizon of an FAO Bt (Bt2 under Bt1) has the clay difference of
    # the run's first row against the eluvial horizon above (v0.9.380)
    # (its own clay against the eluvial horizon above the run: the
    # enrichment runs through the whole Bt, p. 97; D4)
    top <- .ka5_fao_run_top(x, i)
    pr <- .ka5_prev(x, top)
    cd <- .ka5_clay_diff(x, pr, i, "bt")
    if (top != i) cd$note <- c(cd$note, sprintf("a sub-horizon of the FAO %s: its clay against the eluvial horizon above the run", .ka5_fao_at(x, i)$stem))
    p <- .kidpr_and(coat, cd$passed, g$passed)
    # v0.9.364 (Hugo, 2026-11-06): with no clay coatings recorded, a B with
    # the signs of a podzolic illuvial horizon (a Bh or Bs, or humus or oxalate
    # Fe and Al accumulated against the pale horizon above) is no Bt; signs
    # not measured leave the Bt undecided beside the podzolic B
    if (!isFALSE(p) && !any(obs %in% TRUE)) {
      bsp <- .ka5_t_Bsp(x, i)
      if (isTRUE(bsp$passed)) {
        p <- FALSE
        cnote <- c(cnote, sprintf("the podzolic %s (p. 97), no clay coatings recorded: no Bt", bsp$code))
      } else if (!is.na(pr) && isTRUE(.ka5_fao_gate(x, pr, .ka5_t_Bsp(x, pr))$passed)) {
        # the clay difference of the Bt is against the eluvial horizon above
        # (p. 97): under a podzolic B it is no clay illuviation
        p <- FALSE
        cnote <- c(cnote, "under a podzolic B, not an eluvial horizon, no clay coatings recorded: no Bt")
      } else if (isTRUE(.ka5_podzolic_signs(x, i)$passed)) {
        p <- NA
        cnote <- c(cnote, "podzolic signs under a pale horizon, no clay coatings recorded: a Bt or a podzolic B undecided")
      }
    }
    code <- if (isTRUE(bands > .ka5_thr("bands_area_pct")[2L])) "Bbt" else "Bt"
    .ka5_r(p, code = code, missing = unique(c("clay_films_amount", cd$missing, g$missing)), tag = tag,
           note = c(cnote, cd$note, if (code == "Bbt") sprintf("clay bands %s area-%%", format(bands))), value = cd$value)
  })
}

# Bu: no rock structure, rock remnants under 5 %, two pages redder than the
# horizon further below, moist value 4 or less, weatherable minerals under 3
# %, clay over 17 %, clay CEC under 16, ECEC under 10 (p. 98).
.ka5_t_Bu <- function(x, i) {
  g <- .ka5_b_gate(x, i)
  if (isFALSE(g$passed)) return(.ka5_r(FALSE, code = "Bu", note = g$note))
  cl <- .ka5_num(x, i, "clay_pct"); wm <- .ka5_num(x, i, "weatherable_minerals_pct")
  cec <- .ka5_num(x, i, "cec_cmol"); ecec <- .ka5_num(x, i, "ecec_cmol"); cf <- .ka5_num(x, i, "coarse_fragments_pct")
  if (isTRUE(cl <= .ka5_thr("bu_clay_min_pct")) || isTRUE(wm >= .ka5_thr("bu_weatherable_max_pct")))
    return(.ka5_r(FALSE, code = "Bu", note = "clay 17 % or less, or weatherable minerals 3 % or more: no Bu"))
  cc <- if (!is.na(cec) && isTRUE(cl > 0)) 100 * cec / cl else NA
  ec <- if (!is.na(ecec) && isTRUE(cl > 0)) 100 * ecec / cl else NA
  col <- .ka5_colour(x, i)
  nx <- .ka5_next(x, i)
  red2 <- if (is.na(nx)) NA else { a <- .ka5_hue_page(col); b <- .ka5_hue_page(.ka5_colour(x, nx))
    if (is.na(a) || is.na(b)) NA else b - a >= .ka5_thr("bu_hue_pages") - 1e-9 }
  p <- .kidpr_and(if (is.na(cl)) NA else TRUE, if (is.na(wm)) NA else TRUE,
                  if (is.na(cc)) NA else cc < .ka5_thr("bu_cec_clay_max"), if (is.na(ec)) NA else ec < .ka5_thr("bu_ecec_clay_max"),
                  if (is.na(cf)) NA else cf < .ka5_thr("bu_rock_max_pct"), if (is.na(col$value)) NA else col$value <= .ka5_thr("bu_value_moist_max"),
                  red2, g$passed)
  # the Bu does not form in Germany today: never a candidate on a profile
  # that gives no sign of it (clay CEC not recorded)
  if (is.na(p) && is.na(cc) && is.na(ec)) p <- FALSE
  .ka5_r(p, code = "Bu", missing = c("cec_cmol", "ecec_cmol", "weatherable_minerals_pct"),
         note = sprintf("clay %s %%, clay CEC %s, clay ECEC %s: the Bu (p. 98)", format(cl), format(signif(cc, 3)), format(signif(ec, 3))))
}

# Bfv: dry bulk density under 0.8 or pore volume 60 % or more (p. 95).
.ka5_t_Bfv <- function(x, i) {
  g <- .ka5_b_gate(x, i)
  if (isFALSE(g$passed)) return(.ka5_r(FALSE, code = "Bfv", note = g$note))
  bd <- .ka5_num(x, i, "bulk_density_g_cm3"); po <- .ka5_num(x, i, "total_porosity_pct")
  loose <- .kidpr_or(if (is.na(bd)) NA else bd < .ka5_thr("bfv_bd_max"), if (is.na(po)) NA else po >= .ka5_thr("bfv_porosity_min_pct"))
  # tested only where the bulk density or porosity is recorded (soilKey's
  # reading, Hugo, 2026-11-06: else every B would be a Bfv
  # candidate)
  if (!isTRUE(loose)) return(.ka5_r(FALSE, code = "Bfv",
                                    note = if (isFALSE(loose)) "not loose" else "bulk density and porosity not recorded: no Bfv"))
  .ka5_r(g$passed, code = "Bfv", missing = g$missing, note = c(g$note, sprintf("bulk density %s, porosity %s: loose (Bfv, p. 95)", format(bd), format(po))))
}

# Bv (Hugo's decision Q12): a B free of lithogenic carbonate, browner than
# the horizon below in the same layer (a page redder or two chroma steps),
# not a Bt, Bs, Bh, Bu or Bj (the order of tests); the pH, the clay CEC,
# muscovite, weatherable minerals and texture only supporting.
.ka5_t_Bv <- function(x, i) {
  .ka5_memo(x, sprintf("bv_%d", i), function() {
    g <- .ka5_b_gate(x, i)
    if (isFALSE(g$passed)) return(.ka5_r(FALSE, code = "Bv", note = g$note))
    # a sub-horizon of an FAO Bw (Bw1 over Bw2) is compared with the horizon
    # under the run (v0.9.380)
    # the FAO w states the change of colour (or structure) against the
    # material below (WRB 2022 Annex 3, p. 227): the browning of the Bv
    # where the colours do not decide it (v0.9.380)
    fao_w <- .ka5_fao_has(x, i, "w", "B")
    nx <- if (fao_w) .ka5_next(x, .ka5_fao_run_bottom(x, i)) else .ka5_next(x, i)
    if (fao_w && (is.na(nx) || !.ka5_same_layer(x, i, nx)))
      return(.ka5_r(g$passed, code = "Bv", missing = g$missing,
                    note = c("no horizon below in the same layer: the browning stated by the FAO w", g$note)))
    if (is.na(nx) || !.ka5_same_layer(x, i, nx)) {
      w <- .ka5_stand_in(x, i, "cambic", "the horizon below in the same layer")
      return(.ka5_r(if (isFALSE(g$passed)) FALSE else NA, code = "Bv", missing = g$missing, tag = if (isTRUE(w$p)) w$tag,
                    note = c("no horizon below in the same layer: browner than it undecided (Q12)", w$note)))
    }
    br <- .ka5_browner(x, i, nx)
    tag <- br$tag; note <- br$note
    if (is.na(br$passed) && fao_w) {
      br$passed <- TRUE
      note <- c(note, "the browning stated by the FAO w")
    } else if (is.na(br$passed)) {
      w <- .ka5_stand_in(x, i, "cambic", "the colours")
      if (isTRUE(w$p)) tag <- c(tag, w$tag)
      note <- c(note, w$note)
    }
    p <- .kidpr_and(br$passed, g$passed)
    .ka5_r(p, code = "Bv", missing = unique(c(br$missing, g$missing)), tag = tag, note = c(note, g$note))
  })
}

# P: clay 45 % or more, prisms or blocks with slickensides or deep cracks,
# from clay or clay-marl stone, S features subordinate (p. 100).
.ka5_t_P <- function(x, i) {
  if (identical(i, .ka5_first_mineral(x))) return(.ka5_r(FALSE, code = "P", note = "the topsoil"))
  cl <- .ka5_num(x, i, "clay_pct")
  pr0 <- .ka5_v_raw(x, i, "parent_rock_ka5")
  pm0 <- paste(c(pr0, .ka5_site_txt(x, "parent_material")), collapse = " ")
  # clay not recorded: a P candidate only on a recorded clay or clay-marl
  # stone (soilKey's reading, Hugo 2026-11-06)
  if (is.na(cl)) return(.ka5_r(if (grepl("ton|clay|mergel|marl|shale|schiefer|pelit", pm0, ignore.case = TRUE)) NA else FALSE,
                               code = "P", missing = "clay_pct", note = "clay not recorded"))
  if (cl < .ka5_thr("p_clay_min_pct")) return(.ka5_r(FALSE, code = "P", note = sprintf("clay %s %%: under 45", format(cl))))
  sl <- .ka5_txt(x, i, "slickensides"); cr <- .ka5_num(x, i, "cracks_depth_cm"); st <- .ka5_txt(x, i, "structure_type")
  shrink <- .kidpr_or(if (is.na(sl)) NA else !sl %in% c("absent", "none"), if (is.na(cr)) NA else cr > .ka5_thr("p_cracks_min_cm"))
  tag <- character(0)
  if (is.na(shrink)) {
    w <- .ka5_stand_in(x, i, "vertic_horizon", "the shrink-swell signs")
    shrink <- w$p; tag <- w$tag
  }
  struct <- if (is.na(st)) NA else grepl("prism|angular|block|wedge", st)
  pr <- .ka5_v_raw(x, i, "parent_rock_ka5")
  pm <- paste(c(pr, .ka5_site_txt(x, "parent_material")), collapse = " ")
  rock <- if (all(is.na(c(pr, .ka5_site_txt(x, "parent_material"))))) NA else grepl("ton|clay|mergel|marl|shale|schiefer|pelit", pm, ignore.case = TRUE)
  below <- x$ord[x$rank > x$rank[i]]
  if (is.na(rock) && length(below) &&
      any(vapply(below, function(j) isTRUE(.ka5_at_least(.ka5_carbonate(x, j)$range, .ka5_thr("carbonate_rock_pct")[2L])), NA)))
    rock <- FALSE
  sub <- .ka5_below(.ka5_shares(x, i)$s, 10)
  p <- .kidpr_and(shrink, struct, rock, sub)
  .ka5_r(p, code = "P", missing = c("slickensides", "structure_type", "parent_rock_ka5"), tag = tag,
         note = sprintf("clay %s %%; parent rock %s: the P (p. 100)", format(cl), if (is.na(rock)) "not recorded" else pm))
}

# T: clay 65 % or more, subangular blocky, no primary carbonate, over a
# carbonate rock (p. 101); Tv 5YR or yellower, Tu redder.
.ka5_t_T <- function(x, i) {
  if (identical(i, .ka5_first_mineral(x))) return(.ka5_r(FALSE, code = "T", note = "the topsoil"))
  cl <- .ka5_num(x, i, "clay_pct")
  pm0 <- paste(c(.ka5_v_raw(x, i, "parent_rock_ka5"), .ka5_site_txt(x, "parent_material")), collapse = " ")
  # clay not recorded: a T candidate only on a recorded carbonate rock
  if (is.na(cl)) return(.ka5_r(if (grepl("kalk|lime|dolom|carbonat|chalk|kreide", pm0, ignore.case = TRUE)) NA else FALSE,
                               code = "T", missing = "clay_pct", note = "clay not recorded"))
  if (cl < .ka5_thr("t_clay_min_pct")) return(.ka5_r(FALSE, code = "T", note = sprintf("clay %s %%: under 65", format(cl))))
  st <- .ka5_txt(x, i, "structure_type")
  struct <- if (is.na(st)) NA else grepl("angular|block|polyhed", st)
  nc <- .ka5_no_lithogenic_carbonate(x, i)
  # the carbonate rock: below in the profile (75 % or more) or the parent rock
  below <- x$ord[x$rank > x$rank[i]]
  cb <- vapply(below, function(j) .ka5_at_least(.ka5_carbonate(x, j)$range, .ka5_thr("carbonate_rock_pct")[2L]), NA)
  pr <- paste(c(.ka5_v_raw(x, i, "parent_rock_ka5"), .ka5_site_txt(x, "parent_material")), collapse = " ")
  lime <- .kidpr_or(if (length(cb)) .kidpr_or(cb) else NA, if (grepl("NA NA", pr) || !nzchar(pr)) NA else grepl("kalk|lime|dolom|carbonat|chalk|kreide", pr, ignore.case = TRUE))
  col <- .ka5_colour(x, i)
  pos <- .rp_hue_pos(col)
  code <- if (is.na(pos)) "Tv" else if (pos < 15 - 1e-9) "Tu" else "Tv"
  p <- .kidpr_and(struct, nc$passed, lime)
  .ka5_r(p, code = code, missing = c("structure_type", "caco3_pct", "parent_rock_ka5"),
         note = c(sprintf("clay %s %%, over a carbonate rock: the T (p. 101); %s", format(cl), .kidpr_colour_text(col)), nc$note))
}


# --- 8 the substratum C (pp. 98-100; Q15) -----------------------------------------------------

.ka5_t_C <- function(x, i) {
  if (isTRUE(.ka5_organic(x, i)$passed)) return(.ka5_r(FALSE, note = "organic"))
  d <- .ka5_des(x, i)
  cf <- .ka5_num(x, i, "coarse_fragments_pct")
  dg <- .ka5_lgl(x, i, "rock_diggable")
  cm <- .ka5_txt(x, i, "cementation_class")
  fine <- any(!is.na(c(.ka5_num(x, i, "clay_pct"), .ka5_num(x, i, "sand_pct"), .ka5_num(x, i, "silt_pct"))))
  hard_hint <- !x$ka5 && isTRUE(grepl("^[0-9]*R[0-9]*$", d))
  # m, l, x (pp. 98-99): not diggable, diggable, fine earth under 5 vol-%
  lmx <- if (isTRUE(cf > 100 - .ka5_thr("xc_fine_earth_max_pct"))) "x"
         else if (isFALSE(dg) || isTRUE(cm %in% c("strongly", "indurated")) || hard_hint) "m"
         else if (isTRUE(dg) || fine || isTRUE(cm %in% c("none", "weakly"))) "l"
         else NA_character_
  carb <- .ka5_carbonate(x, i)
  pre <- .ka5_carbonate_prefix(carb$range)
  ws <- .ka5_txt(x, i, "weathering_stage")
  sc2 <- .ka5_num(x, i, "secondary_carbonates_pct")
  suf <- if (isTRUE(sc2 > 0) || (is.na(sc2) && .ka5_fao_has(x, i, "k"))) "c"
         else if (isTRUE(grepl("fresh", ws))) "n" else if (isTRUE(grepl("weathered|saprolite", ws))) "v" else ""
  code <- paste0(if (is.na(pre)) "" else pre, if (is.na(lmx)) "" else lmx, "C", suf)
  .ka5_r(TRUE, code = code, tag = carb$tag, value = list(prefix_options = attr(pre, "options"), lmx = lmx),
         note = c(carb$note, if (hard_hint) sprintf("the designation '%s' names hard rock", d),
                  if (is.na(lmx)) "solid or loose rock undecided (rock_diggable not recorded)",
                  if (is.na(pre)) sprintf("the carbonate of the rock undecided (%s)", paste(attr(pre, "options"), collapse = ", "))))
}


# --- the order (Q9) --------------------------------------------------------------------------

.ka5_steps <- function() {
  t <- function(name, f) list(name = name, f = f)
  list(
    fsub = list(t("F", .ka5_t_F)),
    organic = list(t("H", .ka5_t_H), t("O", .ka5_t_O)),
    anthropic = list(t("Y", .ka5_t_Y), t("R", .ka5_t_R), t("E", .ka5_t_E), t("M", .ka5_t_M)),
    groundwater = list(t("G", .ka5_t_G)),
    perched = list(t("S", .ka5_t_S)),
    topsoil = list(t("A", .ka5_t_A)),
    subsoil = list(t("Bsp", .ka5_t_Bsp), t("Bt", .ka5_t_Bt), t("Bu", .ka5_t_Bu), t("T", .ka5_t_T),
                   t("P", .ka5_t_P), t("Bfv", .ka5_t_Bfv), t("Bv", .ka5_t_Bv)),
    substratum = list(t("C", .ka5_t_C))
  )
}

# The steps whose undecided test leaves the layer undecided against what
# follows (they dominate the reading of a layer).
.KA5_DOMINANT_STEPS <- c("fsub", "organic", "groundwater", "perched")

# A test asked once per row.
.ka5_ask <- function(x, i, step, t) {
  key <- sprintf("t_%s_%s_%d", step, t$name, i)
  r <- x$cache[[key]]
  if (is.null(r)) {
    r <- tryCatch(suppressWarnings(t$f(x, i)), error = function(e)
      .ka5_r(NA, note = sprintf("%s: %s", t$name, conditionMessage(e))))
    r$name <- t$name
    r$code <- r$code %||% t$name
    assign(key, r, envir = x$cache)
  }
  r
}

# The prefixes a data code takes (step 9): a in a floodplain, tm, tb or tp
# from a tidal layer origin, z for salt, o for sedimentary organic matter,
# e for a marl (A and G), i, e, c of the rock (C, already in its code).
.ka5_prefixes <- function(x, i, code) {
  m <- regmatches(code, regexec("^([a-z]*)([A-Z])", code))[[1L]]
  if (!length(m)) return(code)
  master <- m[3L]
  out <- character(0)
  tp <- .ka5_tidal_prefix(x, i)
  if (!is.na(tp)) out <- c(out, tp)
  else if (master %in% c("A", "M", "C", "G", "S") && .ka5_floodplain(x, i) && !startsWith(code, "a")) out <- c(out, "a")
  ec <- .ka5_num(x, i, "ec_dS_m")
  if (isTRUE(ec >= .ka5_thr("ec_z_dS_m")) && master %in% c("F", "G", "H", "A", "S", "C", "M")) out <- c(out, "z")
  ol <- .ka5_num(x, i, "oc_lithogenic_pct")
  if (isTRUE(ol * .ka5_thr("os_factor_mineral") >= .ka5_thr("o_prefix_os_pct")) && master %in% c("A", "G", "C", "F")) out <- c(out, "o")
  if (master %in% c("A", "G", "F") && length(out) && any(out %in% c("a", "tm", "tb", "tp"))) {
    pre <- .ka5_carbonate_prefix(.ka5_carbonate(x, i)$range)
    if (identical(as.character(pre), "e")) out <- c(out, "e")
  }
  paste0(paste(out, collapse = ""), code)
}

# The secondary carbonate suffix c on the A, B, G, S, H and T (step 9): after
# the master (Bcv, Gco, Scw, Hcw, Tcv); the A and C codes carry it already.
.ka5_suffix_c <- function(x, i, code) {
  sc <- .ka5_num(x, i, "secondary_carbonates_pct")
  # the FAO k states an accumulation of pedogenic carbonates (WRB 2022
  # Annex 3, p. 227): the KA5 c where none is recorded (v0.9.380)
  if (!isTRUE(sc > 0) && !(is.na(sc) && .ka5_fao_has(x, i, "k"))) return(code)
  m <- regmatches(code, regexec("^([a-z]*)([BGSHT])([a-z]*)$", code))[[1L]]
  if (!length(m) || grepl("c", m[4L], fixed = TRUE)) return(code)
  paste0(m[2L], m[3L], "c", m[4L])
}

# The transitions of step 10 (Q9) around a decided data code: the code to
# write and a note.
.ka5_transition <- function(x, i, code, ans) {
  m <- regmatches(code, regexec("^([a-z]*)([A-Z])", code))[[1L]]
  if (!length(m)) return(list(code = code, note = character(0)))
  master <- m[3L]
  sh <- .ka5_shares(x, i)
  gw <- .ka5_groundwater(x, i)
  hold <- function(step, names = NULL) {
    rs <- ans[[step]]
    k <- which(vapply(rs, function(r) isTRUE(r$passed) && (is.null(names) || r$name %in% names), NA))
    if (length(k)) rs[[k[1L]]]$code else NA_character_
  }
  topsoil <- .ka5_in_topsoil(x, i)
  am <- .ka5_ah_min(x, i)
  if (master %in% c("G", "S")) {
    # a G or S with the Ah humus minimum in the run of topsoils: Ah-Go, Ah-Sw;
    # with a non-hydromorphic horizon that also holds: Bv-Go, Bt-Sd ...
    # (v0.9.364: not the illuvial humus of a podzolic B, which gives Bhs-Go;
    # Hugo, 2026-11-06)
    if (topsoil && isTRUE(am$passed) && !startsWith(sub("^[a-z]*", "", code), "Gr") && is.na(hold("subsoil", "Bsp")))
      return(list(code = paste0("Ah-", code), note = "the Ah humus minimum: Ah- (p. 105)"))
    other <- hold("subsoil", c("Bt", "Bv", "Bsp"))
    # a Bv against a hydromorphic horizon below is the colour of the redox
    # features, not of weathering: no Bv- part (soilKey's reading)
    nx <- .ka5_next(x, i)
    if (identical(other, "Bv") && !is.na(nx)) {
      sn <- .ka5_shares(x, nx)
      if (isTRUE(sn$rust[1L] > 0) || isTRUE(sn$reduction[1L] > 0) || isTRUE(sn$bleached[1L] > 0)) other <- NA_character_
    }
    if (!is.na(other)) return(list(code = paste0(other, "-", code), note = sprintf("a %s as well: %s-%s", other, other, code)))
    return(list(code = code, note = character(0)))
  }
  if (!master %in% c("A", "B", "C", "M", "E", "P", "T", "R")) return(list(code = code, note = character(0)))
  out <- code; notes <- character(0)
  rust <- sh$rust; s <- sh$s
  if (isTRUE(gw$passed) && isTRUE(rust[1L] > 0) && isTRUE(.ka5_below(rust, .ka5_thr("go_rust_min_pct")))) {
    out <- paste0("Go-", out)
    notes <- "rust under 5 % in the groundwater zone: Go- (p. 105)"
  } else if (isFALSE(gw$passed) && isTRUE(s[1L] > 0)) {
    st <- .ka5_thr("s_transition_pct")
    if (isTRUE(s[1L] >= st[1L]) && isTRUE(.ka5_below(s, st[2L]))) {
      out <- paste0(out, "-Sw")
      notes <- sprintf("S features %s %% (10 to < 80): %s (p. 102)", .ka5_range_txt(s), out)
    } else if (isTRUE(.ka5_below(s, st[1L]))) {
      out <- paste0("Sw-", out)
      notes <- sprintf("S features %s %% (under 10): %s (p. 102)", .ka5_range_txt(s), out)
    }
  }
  # a B with the Ah humus minimum under the topsoil: Ah-Bv, Ah-Bfv (p. 96)
  if (master == "B" && !grepl("-", out, fixed = TRUE) && code %in% c("Bv", "Bfv")) {
    p <- .ka5_prev(x, i)
    if (!is.na(p) && isTRUE(substr(.ka5_data_layer(x, p)$carrying %||% "", 1L, 1L) == "A") && isTRUE(am$passed)) {
      out <- paste0("Ah-", out)
      notes <- c(notes, "the Ah humus minimum under the topsoil: Ah- (p. 96)")
    }
  }
  list(code = out, note = notes)
}

#' The order of tests on the data of one row: the code, the candidates, the
#' evidence. Memoised.
#' @noRd
.ka5_data_layer <- function(x, i) {
  key <- sprintf("datalayer_%d", i)
  if (!is.null(x$cache[[key]])) return(x$cache[[key]])
  if (isTRUE(x$busy[i])) return(list(code = NA_character_, carrying = NA_character_, passed = NA, candidates = character(0)))
  if (is.null(x$busy)) x$busy <- logical(x$n)
  x$busy[i] <- TRUE
  on.exit(x$busy[i] <- FALSE, add = TRUE)
  steps <- .ka5_steps()
  ans <- list()
  code <- NA_character_; dec <- NULL; undecided <- FALSE; cands <- character(0); pending <- character(0)
  block <- FALSE
  for (s in names(steps)) {
    rs <- lapply(steps[[s]], function(t) .ka5_ask(x, i, s, t))
    ans[[s]] <- rs
    if (!is.na(code) || undecided) next
    v <- vapply(rs, function(r) r$passed, NA)
    lab <- vapply(rs, function(r) r$code %||% r$name, "")
    labs_na <- function(k) unlist(lapply(rs[k], function(r) if (length(r$value) && is.character(r$value)) r$value else r$code))
    if (any(v %in% TRUE)) {
      k <- which(v %in% TRUE)[1L]
      nas <- which(is.na(v[seq_len(k - 1L)]))
      if (length(nas) || block) { undecided <- TRUE; cands <- unique(c(cands, labs_na(nas), lab[k])) }
      else if (s == "substratum" && length(pending)) {
        # the C is the fallback: it does not win over an undecided soil horizon
        undecided <- TRUE; cands <- unique(c(pending, lab[k]))
      } else { code <- lab[k]; dec <- rs[[k]] }
    } else if (any(is.na(v))) {
      nas <- which(is.na(v))
      if (s %in% .KA5_DOMINANT_STEPS) { block <- TRUE; cands <- unique(c(cands, labs_na(nas))) }
      else if (length(nas) >= 2L) { undecided <- TRUE; cands <- unique(c(cands, labs_na(nas))) }
      else pending <- unique(c(pending, labs_na(nas)))
    }
  }
  if (block && !undecided && is.na(code)) undecided <- TRUE
  # an FAO designation bounds the horizons the row may be (v0.9.380,
  # R/ka5-fao.R): the row is read within its family
  fao <- .ka5_fao_at(x, i)
  fz <- NULL
  if (!is.null(fao)) {
    fz <- .ka5_fao_combine(x, i, ans, fao, code, unique(c(cands, pending)))
    code <- fz$code; dec <- fz$dec; pending <- character(0)
    cands <- fz$cands; undecided <- is.na(code)
  }
  notes <- character(0); missing <- character(0); tags <- character(0)
  all_r <- unlist(ans, recursive = FALSE)
  if (!is.na(code)) {
    tr <- if (!is.null(fz)) fz$tr else .ka5_transition(x, i, code, ans)
    code <- .ka5_suffix_c(x, i, .ka5_prefixes(x, i, tr$code))
    notes <- c(dec$note, tr$note, fz$note); tags <- dec$tag
    holds <- unique(vapply(Filter(function(r) isTRUE(r$passed) && r$name != "C", all_r), function(r) r$code, ""))
    if (!is.null(fao)) holds <- holds[vapply(holds, function(h) .ka5_fao_compatible(fao$rule, h), NA)]
    cands <- unique(c(pending, setdiff(holds, c(dec$code, sub("^[a-z]*", "", code)))))
  } else {
    nr <- Filter(function(r) is.na(r$passed) || (isTRUE(r$passed) && r$code %in% cands), all_r)
    missing <- unique(unlist(lapply(nr, `[[`, "missing")))
    notes <- unique(c(fz$note, unlist(lapply(nr, `[[`, "note"))))
    tags <- unique(unlist(lapply(nr, `[[`, "tag")))
    cands <- unique(c(cands, pending))
  }
  passed <- if (!is.na(code)) TRUE else if (undecided || length(cands)) NA else FALSE
  rd <- if (!is.na(code)) .ka5_parse_one(code) else NULL
  if (!is.null(rd) && !isTRUE(rd$known)) {
    notes <- c(notes, sprintf("soilKey wrote %s, which does not read; kept as written", code))
    rd <- NULL
  }
  # the rock prefix the data leave open
  pre_na <- character(0)
  cc <- Filter(function(r) identical(r$name, "C"), ans$substratum)
  if (!is.na(code) && length(cc) && grepl("C", code, fixed = TRUE) && is.list(cc[[1L]]$value))
    pre_na <- as.character(cc[[1L]]$value$prefix_options %||% character(0))
  all_tags <- unique(c(tags, unlist(lapply(Filter(function(r) isTRUE(r$passed), all_r), `[[`, "tag"))))
  r <- list(code = code, carrying = if (!is.null(rd)) rd$code else NA_character_, row = rd, passed = passed,
            candidates = as.character(setdiff(cands, code)), missing = as.character(unique(missing)), notes = unique(notes[nzchar(notes)]),
            stand_ins = as.character(intersect(c(tags, if (!is.na(code)) dec$tag), .KA5_STAND_INS)),
            tags = all_tags, prefixes_na = if (length(pre_na) > 1L) pre_na else character(0), answers = ans,
            fao = if (!is.null(fz)) fz$kind else NA_character_)
  # a stand-in, or the F kind read from soilKey's colour cuts (R3), lowers the
  # grade
  r$lower_grade <- length(r$stand_ins) > 0L || (!is.na(code) && "f_colour_cut" %in% c(tags, dec$tag))
  # a row the FAO letter decides (the data leave only its translation open)
  # has a lower grade, which the key reports (H2, D13)
  if (identical(r$fao, "fao")) {
    r$lower_grade <- TRUE
    r$tags <- unique(c(r$tags, "fao_designation"))
  }
  assign(key, r, envir = x$cache)
  r
}


# --- the profile ----------------------------------------------------------------------------

.ka5_evidence <- function(parts) {
  parts <- unique(parts[!is.na(parts) & nzchar(parts)])
  if (!length(parts)) NA_character_ else paste(parts, collapse = "; ")
}

# A prefix the KA5 notation leaves out of a C (the rock carbonate i, e, c)
# or of an H (the moor type h, n, u), filled from the recorded data or left
# open (D3, Q15; Hugo, 2026-11-06): list(add, open, note).
.ka5_notation_prefix_fill <- function(x, i, master, prefixes) {
  out <- list(add = character(0), open = character(0), note = character(0))
  if (identical(master, "C") && !any(prefixes %in% c("i", "e", "c"))) {
    cb <- .ka5_carbonate(x, i)
    pre <- .ka5_carbonate_prefix(cb$range)
    if (!is.na(pre)) {
      out$add <- as.character(pre)
      out$note <- sprintf("the rock prefix %s filled from the data (%s)", pre, cb$note)
    } else {
      out$open <- attr(pre, "options") %||% c("i", "e", "c")
      out$note <- sprintf("the rock prefix not written and the carbonate %s: %s open", if (anyNA(cb$range)) "not recorded" else "undecided",
                          paste(out$open, collapse = ", "))
    }
  }
  if (identical(master, "H") && !any(prefixes %in% c("h", "n", "u"))) {
    pm <- .ka5_txt(x, i, "peat_moor_type")
    pre <- if (is.na(pm)) NA_character_ else switch(pm, "high-moor" = "h", "transitional" = "u", "low-moor" = "n", NA_character_)
    if (!is.na(pre)) {
      out$add <- c(out$add, pre)
      out$note <- c(out$note, sprintf("the moor prefix %s filled from the peat of the %s", pre, pm))
    } else {
      out$open <- c(out$open, "h", "n", "u")
      out$note <- c(out$note, "the moor prefix not written nor the moor type recorded: h, n or u open")
    }
  }
  out
}

#' The KA5 horizons of a pedon
#'
#' The horizon identification of the namespace ka5: every row read as one of
#' the KA5 horizons. A row written in the KA5 notation keeps its horizon
#' (route "notation"); the order of tests runs on its data as well, and when
#' it reaches the same carrying horizon the route is "both", otherwise its
#' reading goes to the evidence. Any other row is read by the order of tests
#' (route "data").
#'
#' @param pedon A PedonRecord.
#' @param ... Not used.
#' @return The frame of identify_horizons(), one row per layer: \code{code}
#'   (the symbol without numeral and digits, the sequence engine's token:
#'   Ah-Bv, elC, tmzeGo), with the KA5 columns \code{carrying} (the horizon
#'   the last part carries: Bv for Ah-Bv), \code{master}, \code{prefixes} and
#'   \code{with} (list columns), \code{layer}, \code{fossil}, \code{relict},
#'   \code{bs_mark}, \code{options} (the readings a letter in parentheses
#'   gives), \code{top_gof} and \code{bottom_gof} (depths from the GOF, p.
#'   79), \code{notation_code} and \code{data_code} (the two readings),
#'   \code{prefixes_na} (rock prefixes the data leave open), \code{stand_ins}
#'   (soilKey's WRB diagnostics that stood in, flagged) and
#'   \code{lower_grade}.
#' @noRd
.ka5_identify <- function(pedon, ...) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
  fr <- .horizon_frame(pedon)
  n <- nrow(fr)
  chr <- rep(NA_character_, n); lst <- rep(list(character(0)), n)
  fr$carrying <- chr; fr$master <- chr; fr$prefixes <- lst; fr$with <- lst
  fr$layer <- rep(1L, n); fr$fossil <- rep(FALSE, n); fr$relict <- rep(FALSE, n); fr$bs_mark <- chr
  fr$options <- lst; fr$top_gof <- rep(NA_real_, n); fr$bottom_gof <- rep(NA_real_, n)
  fr$notation_code <- chr; fr$data_code <- chr; fr$prefixes_na <- lst; fr$stand_ins <- lst
  fr$lower_grade <- rep(FALSE, n)
  if (!n) return(fr)
  x <- .ka5_profile(pedon)
  d <- x$des
  fr$top_gof <- x$rtop; fr$bottom_gof <- x$rbot
  fr$layer <- as.integer(x$layer)
  ok <- x$ka5
  buried <- .ka5_buried_rows(ok, d$layer, d$fossil)
  for (i in x$ord) {
    k <- .ka5_data_layer(x, i)
    fr$data_code[i] <- k$code
    dn <- if (identical(i, x$gof_row) || (is.na(x$gof_row) && identical(i, x$ord[1L]))) x$gof_note else character(0)
    if (ok && isTRUE(d$known[i])) {
      rr <- list(known = TRUE, code = d$code[i], parts = d$parts[[i]], part_prefixes = d$part_prefixes[[i]],
                 joiners = d$joiners[[i]], prefixes = d$prefixes[[i]], bs_mark = d$bs_mark[i])
      sym <- .ka5_row_symbol(rr)
      fr$code[i] <- sym
      fr$notation_code[i] <- sym
      fr$carrying[i] <- d$code[i]
      fr$master[i] <- d$master[i]
      fr$prefixes[[i]] <- d$prefixes[[i]]
      # a prefix the notation leaves out is filled from the data, else left
      # open (D3, Q15): the rock carbonate of a C, the moor type of an H
      fill <- .ka5_notation_prefix_fill(x, i, d$master[i], d$prefixes[[i]])
      if (length(fill$add)) fr$prefixes[[i]] <- c(d$prefixes[[i]], fill$add)
      fr$prefixes_na[[i]] <- fill$open
      fr$with[[i]] <- d$with[[i]]
      fr$fossil[i] <- isTRUE(d$fossil[i]); fr$relict[i] <- isTRUE(d$relict[i])
      fr$bs_mark[i] <- d$bs_mark[i]
      fr$features[[i]] <- d$features[[i]]
      fr$overlay[[i]] <- setdiff(unique(c(d$code[i], d$master[i], if (identical(d$code[i], "Ap")) "Ah")), sym)
      fr$options[[i]] <- if (length(d$options[[i]]) > 1L) d$options[[i]] else character(0)
      fr$buried[i] <- buried[i]
      fr$passed[i] <- TRUE
      fr$transitional[i] <- length(d$with[[i]]) > 0L
      agree <- identical(k$carrying, d$code[i])
      fr$route[i] <- if (agree) "both" else "notation"
      fr$candidates[[i]] <- if (agree) character(0) else unique(c(k$carrying[!is.na(k$carrying)], k$candidates))
      fr$stand_ins[[i]] <- if (agree) k$stand_ins else character(0)
      dis <- if (agree) "the order of tests on the data reaches it too"
             else if (!is.na(k$code)) sprintf("the data read %s; the notation's %s is kept (the designation gives the horizon, the data the properties)", k$code, sym)
             else if (length(k$candidates)) sprintf("the data leave it undecided (%s); the notation's %s is kept", paste(k$candidates, collapse = ", "), sym)
             else sprintf("the data read no KA5 horizon; the notation's %s is kept", sym)
      fr$evidence[i] <- .ka5_evidence(c(sprintf("the KA5 notation: %s", fr$designation[i]), dis, d$notes[[i]], fill$note, dn))
    } else {
      rd <- k$row
      fr$code[i] <- if (!is.null(rd)) .ka5_row_symbol(rd) else k$code
      fr$carrying[i] <- k$carrying
      fr$master[i] <- if (!is.null(rd)) rd$master else NA_character_
      fr$prefixes[[i]] <- if (!is.null(rd)) rd$prefixes else character(0)
      fr$with[[i]] <- if (!is.null(rd)) vapply(seq_len(max(0L, length(rd$parts) - 1L)), function(j)
        paste0(rd$part_prefixes[j], rd$parts[j]), "") else character(0)
      fr$features[[i]] <- if (!is.null(rd)) .ka5_row_features(rd) else character(0)
      fr$overlay[[i]] <- if (!is.null(rd)) setdiff(unique(c(rd$code, rd$master, if (identical(rd$code, "Ap")) "Ah")), fr$code[i]) else character(0)
      fr$passed[i] <- k$passed
      fr$transitional[i] <- length(fr$with[[i]]) > 0L
      fr$candidates[[i]] <- k$candidates
      fr$missing[[i]] <- k$missing
      fr$prefixes_na[[i]] <- k$prefixes_na
      fr$stand_ins[[i]] <- k$stand_ins
      fr$lower_grade[i] <- isTRUE(k$lower_grade)
      fr$route[i] <- "data"
      head <- if (!is.na(k$code)) sprintf("[soilKey's order of tests] %s", k$code)
              else if (length(k$candidates)) sprintf("[soilKey's order of tests] undecided between %s", paste(k$candidates, collapse = ", "))
              else "[soilKey's order of tests] no KA5 horizon"
      und <- if (!ok && !is.na(d$read_as_ka5[i])) d$notes[[i]] else character(0)
      fr$evidence[i] <- .ka5_evidence(c(head, k$notes,
                                        if (length(k$stand_ins)) sprintf("soilKey's WRB stand-in, flagged: %s", paste0(k$stand_ins, "()", collapse = ", ")),
                                        und, dn))
    }
  }
  fr
}

#' The horizon panel of a KA5 result: one row per layer with its depths from
#' the GOF, the designation, the horizon read, the route (notation, data or
#' both), the candidates and the evidence.
#' @noRd
.ka5_horizon_panel <- function(frame) {
  if (!is.data.frame(frame) || !nrow(frame)) return(NULL)
  fmt <- function(v) ifelse(is.na(v), "?", format(round(v, 1), trim = TRUE))
  data.frame(
    depth = sprintf("%s-%s", fmt(frame$top_gof), fmt(frame$bottom_gof)),
    designation = frame$designation,
    horizon = ifelse(is.na(frame$code), "[undecided]", frame$code),
    route = frame$route,
    candidates = vapply(frame$candidates, function(z) paste(z, collapse = ", "), ""),
    stand_ins = vapply(frame$stand_ins, function(z) paste(z, collapse = ", "), ""),
    evidence = ifelse(is.na(frame$evidence), "", frame$evidence),
    stringsAsFactors = FALSE)
}
