# =============================================================================
# v0.9.343 -- the reference horizons of the French Referentiel pedologique
# 2008 (AFES; Baize and Girard coord.; Quae, 2009) identified on the recorded
# data, layer by layer.
#
# The RP has no key to its horizons: they are concepts the pedologist
# interprets with the whole sequence (p. XII). soilKey reads them by a fixed
# order of tests of its own (Hugo's decision Q10), built from the book's order
# of presentation (p. 6) and its statements of dominance (the reductic G
# dominates every other character of a layer, p. 280):
#   1 layers and materials: R, M, D, P; TH, Z and its kinds, Mli, Mt;
#   2 holorganic: H (Hf, Hm, Hs, Ha), then O (OL, OF, OH and their kinds,
#     OHta);
#   3 coarse constitution: ND (intertropical), X (Xgr, Xc, Xp);
#   4 hydromorphy: G (Gr, Go), AG for an A with reductic characters;
#   5 indurated and accumulation horizons: U; Km, Kc, K; Sim, Si; Fem,
#     Femp, Fe; Ym, Ys, Yp; OXm, OXc (intertropical);
#   6 surface organo-mineral horizons: An, Ach, Aso, Aca, Ado, Aci, Amg,
#     Alu, And, Avi (andosol domain), Av, Anod, Ae, A, then Js;
#   7 subsurface: E, BP (BPh, BPs), BT (BTss), V, SV, Snd, Slu, F, OX, RT, FE
#     (intertropical), Sn, FS, Sp, Sca, Sdo, Sci, Smg, Sal, S, Jp, C;
#   8 features on every layer (g, h, ho, k, t, v, x, y, d, cn, j, cry, nod,
#     c, b, a), and the overlays L (ploughed), Na and Sa.
# Every test is three-valued. The first that holds gives the code; an
# undecided test before it in the same step leaves the layer undecided with
# both as candidates; an undecided test alone in its step does not stop the
# order and goes to the candidates; the other codes that hold go to the
# candidates too (Q10, with the Russian rule Q4). With no code, a functional
# redox feature makes the layer g.
#
# A missing field descriptor is read through a soilKey WRB diagnostic,
# flagged, with a lower grade (default 6): argic() for the clay coatings of
# BT, spodic() for BP, calcic() and petrocalcic() for K, duric_horizon() and
# petroduric() for Si, ferric() and petroplinthic() for Fe, plinthic() for
# RT, vertic_horizon() and vertic_properties() for V, SV and Av,
# nitic_horizon() for the pressure faces of Sn, ferralic() for F and OX,
# chernic() and mollic() for Ach, umbric_horizon() for Aso (never sombric(),
# a name clash), albic() for E, stagnic_properties() for g, andic_properties()
# and vitric_properties() for the andosol domain, retic_properties() and
# albeluvic_glossae() for BTd, continuous_rock() for R. The faux amis of p.
# 392 (Andic, Calcaric, Calcic, Ferric, Leptic, Luvic, Vertic as qualifiers)
# never stand in for the RP qualifiers of the same name.
#
# A layer written in the RP notation keeps its code (route "notation"); the
# data run as well, and a different reading goes to the evidence ("both" when
# they agree; default 7). A layer in another notation is read on the data,
# its designation narrowing the candidates through Annex 6 and the FAO
# letters (Q9).
#
# .rp_identify() is the horizon identification of the namespace rp; until
# the registry entry rp2008 exists (the next part), identify_horizons() does
# not reach it, and it is called directly.
# =============================================================================


# --- the profile around a layer ---------------------------------------------------

# The positions of the layers, once per profile: holorganic, the first
# mineral layer, and the surface organo-mineral run (the first mineral layer
# and the A layers that continue it: an A family designation of another
# notation, else organic carbon 1 % or more with a moist value 4 or less).
.rp_positions <- function(x) {
  .rp_memo(x, "positions", function() {
    n <- x$n
    org <- vapply(seq_len(n), function(i) isTRUE(.rp_organic(x, i)$passed), NA)
    surf <- logical(n); first <- NA_integer_; open <- TRUE; last <- NA_integer_
    for (i in x$ord) {
      if (org[i]) next
      if (is.na(first)) { first <- i; surf[i] <- TRUE; last <- i; next }
      if (!open) break
      # the run continues only under a layer of A material
      if (!isTRUE(.rp_a_material(x, last)$passed)) break
      # an A or L designation of another notation (not a transition that
      # starts with B, E or C, as BA), else carbon over 1 % with a moist value
      # of 4 or less (the review's D2)
      a_first <- isTRUE(grepl("^[0-9]*(A|Ap|L)", .rp_des(x, i)))
      cont <- if (length(x$hint_fam[[i]])) a_first && ("A" %in% x$hint_fam[[i]] || "L" %in% x$hint_fam[[i]])
              else if (x$rp && isTRUE(x$des$known[i])) isTRUE(x$des$family[i] %in% c("A", "L"))
              else {
                oc <- .rp_num(x, i, "oc_pct"); col <- .rp_colour(x, i)
                isTRUE(oc > 1) && !isTRUE(col$value > 4)
              }
      if (isTRUE(cont) && !isTRUE("b" %in% x$des$hint_features[[i]])) { surf[i] <- TRUE; last <- i } else open <- FALSE
    }
    list(organic = org, first = first, surface = surf)
  })
}
.rp_is_surface <- function(x, i) isTRUE(.rp_positions(x)$surface[i])
.rp_is_first <- function(x, i) identical(.rp_positions(x)$first, i)
# The surface run of a layer: the layers of the run, in depth order.
.rp_surface_run <- function(x) { pos <- .rp_positions(x); x$ord[pos$surface[x$ord]] }
.rp_is_mineral <- function(x, i) !isTRUE(.rp_positions(x)$organic[i])

# The thickness of a layer and of the run of adjacent layers for which
# `pred(j)` is TRUE.
.rp_run_thickness <- function(x, i, pred) {
  r <- x$rank[i]; th <- x$thick[i]
  if (is.na(th)) return(NA_real_)
  k <- r - 1L
  while (k >= 1L && isTRUE(pred(x$ord[k]))) { th <- th + (x$thick[x$ord[k]] %||% 0); k <- k - 1L }
  k <- r + 1L
  while (k <= x$n && isTRUE(pred(x$ord[k]))) { th <- th + (x$thick[x$ord[k]] %||% 0); k <- k + 1L }
  th
}

# No particle size, carbon or structure recorded (carbonates may be: a chalk).
.rp_no_fine_earth_but <- function(x, i) {
  all(is.na(c(.rp_num(x, i, "clay_pct"), .rp_num(x, i, "sand_pct"), .rp_num(x, i, "silt_pct"),
              .rp_num(x, i, "oc_pct")))) && is.na(.kidpr_structure(x, i)$type)
}

# Is there no fine earth recorded in a layer (a rock)?
.rp_no_fine_earth <- function(x, i) {
  all(is.na(c(.rp_num(x, i, "clay_pct"), .rp_num(x, i, "sand_pct"), .rp_num(x, i, "silt_pct"),
              .rp_num(x, i, "oc_pct")))) && is.na(.kidpr_structure(x, i)$type)
}

# A pedogenic sign in a layer (organic carbon 0.1 % or more, a pedological
# structure, roots): an X, not a layer D (map 4, the default of Q11).
.rp_pedogenic_sign <- function(x, i) {
  oc <- .rp_num(x, i, "oc_pct")
  st <- .rp_structured(x, i)$passed
  ro <- .rp_txt(x, i, "roots_abundance")
  if (isTRUE(oc >= 0.1) || isTRUE(st) || isTRUE(!is.na(ro) && ro != "none")) return(TRUE)
  if (is.na(oc) && is.na(st)) NA else FALSE
}

# More clay than a reference layer: 3 points more under 15 % clay, 1.2
# times from 15 to 40 %, 8 points more from 40 % (soilKey's reading of "more
# clay", the increase of the WRB argic horizon).
.rp_more_clay <- function(ci, cref) {
  if (is.na(ci) || is.na(cref)) return(NA)
  if (cref < 15) ci >= cref + 3 else if (cref < 40) ci >= 1.2 * cref else ci >= cref + 8
}

# The least clay of the mineral layers above a layer (NA when none is
# recorded).
.rp_clay_above <- function(x, i) {
  a <- .rp_above(x, i)
  a <- a[vapply(a, function(j) .rp_is_mineral(x, j), NA)]
  cl <- vapply(a, function(j) .rp_clay(x, j), 0)
  if (!length(cl) || all(is.na(cl))) NA_real_ else min(cl, na.rm = TRUE)
}


# --- step 1: layers and materials (pp. 26-28) ----------------------------------------

.rp_t_R <- function(x, i) {
  hc <- x$hint_codes[[i]]
  rr <- .rp_txt(x, i, "rupture_resistance")
  hard <- if (is.na(rr)) NA else grepl("very hard|extremely hard|indurat", rr)
  if (identical(hc, "R")) return(.rp_r(TRUE, tag = "designation_hint", note = sprintf("the designation '%s' is hard rock", .rp_des(x, i))))
  if (length(hc) && all(hc %in% c("R", "M", "D"))) {
    if (!is.na(hard)) return(.rp_r(hard, note = sprintf("rupture resistance %s", rr)))
    return(.rp_stand_in(x, i, "continuous_rock", "the hardness of the rock"))
  }
  if (isTRUE(hard) && .rp_no_fine_earth(x, i))
    return(.rp_r(TRUE, note = sprintf("rupture resistance %s and no fine earth recorded", rr)))
  .rp_r(FALSE)
}

.rp_t_M <- function(x, i) {
  hc <- x$hint_codes[[i]]
  rr <- .rp_txt(x, i, "rupture_resistance")
  soft <- !is.na(rr) && !grepl("very hard|extremely hard|indurat|loose", rr)
  # without a designation of a rock: a soft or slightly hard layer with no
  # fine earth and its carbonates recorded (chalk, marl), or an FAO Cr, a
  # weathered rock (the review's d4)
  if (soft && .rp_no_fine_earth_but(x, i) &&
      (!is.na(.rp_num(x, i, "caco3_pct")) || isTRUE(grepl("^[0-9]*Cr[0-9]*$", .rp_des(x, i)))))
    return(.rp_r(TRUE, tag = if (isTRUE(grepl("Cr", .rp_des(x, i)))) "designation_hint",
                 note = sprintf("rupture resistance %s, no fine earth: a coherent soft rock", rr)))
  if (!length(hc) || !all(hc %in% c("R", "M", "D")) || identical(hc, "R")) return(.rp_r(FALSE))
  rr <- .rp_txt(x, i, "rupture_resistance")
  if (is.na(rr)) return(.rp_r(NA, missing = "rupture_resistance", note = "a rock layer: hard, loose or fragmented undecided"))
  cw <- .rp_coarse_wt(x, i, 60)
  if (isTRUE(cw$passed)) return(.rp_r(FALSE, note = "coarse elements dominant: a layer D"))
  .rp_r(!grepl("very hard|extremely hard|indurat|loose", rr), note = sprintf("rupture resistance %s: coherent, easy to work", rr))
}

.rp_t_D <- function(x, i) {
  hc <- x$hint_codes[[i]]
  if (identical(hc, "R")) return(.rp_r(FALSE))
  if (length(hc) && !any(hc %in% c("R", "M", "D")))
    return(.rp_r(FALSE, note = "the designation names a horizon, not a layer"))
  cw <- .rp_coarse_wt(x, i, 60)
  if (isFALSE(cw$passed)) return(.rp_r(FALSE))
  ps <- .rp_pedogenic_sign(x, i)
  r <- .rp_all(cw, .rp_r(if (is.na(ps)) NA else !ps, missing = c("oc_pct", "structure_type"),
                         note = if (isTRUE(ps)) "a pedogenic sign: a peyric horizon, not a layer" else "no pedogenic sign"))
  r$note <- c(r$note, "coarse material with no pedogenic sign: a layer D (map 4)")
  r
}

.rp_t_P <- function(x, i) {
  t <- .rp_num(x, i, "permafrost_temp_C")
  if (!is.na(t)) return(.rp_r(t < 0, note = sprintf("temperature %s C", format(t))))
  if ("*" %in% x$des$hint_features[[i]]) return(.rp_r(NA, missing = "permafrost_temp_C", note = "a frozen layer by its designation"))
  fr <- .rp_site_txt(x, "frost_regime")
  if (isTRUE(grepl("permafrost|pergel", fr)) && isTRUE(x$rtop[i] >= 30))
    return(.rp_r(NA, missing = "permafrost_temp_C", note = sprintf("frost regime %s: the layer may be permafrost", fr)))
  .rp_r(FALSE)
}

# TH: a waterlogged mineral or organo-mineral material with 0.75 % S or more
# (default 18: at least 0.75 for TH), mostly sulfides.
.rp_t_TH <- function(x, i) {
  st <- .rp_num(x, i, "s_total_pct")
  sf <- .rp_num(x, i, "sulfidic_s_pct")
  sign <- !is.na(st) || !is.na(sf) || isTRUE(.rp_lgl(x, i, "sulfide_odour")) ||
    !is.na(.rp_num(x, i, "ph_h2o2")) || !is.na(.rp_num(x, i, "incubation_ph"))
  if (!sign) return(.rp_r(FALSE))
  # an oxidised, sulfated layer is no sulfidic material: jarosite, or a pH
  # under 4 (the review's D8)
  ph <- .rp_num(x, i, "ph_h2o")
  if (isTRUE(.rp_lgl(x, i, "jarosite_present")) || isTRUE(ph < 4))
    return(.rp_r(FALSE, note = "jarosite or a pH under 4: oxidised, not a sulfidic material"))
  s <- if (!is.na(st)) .rp_r(st >= 0.75, note = sprintf("total S %s %%", format(st)))
       else if (isTRUE(sf >= 0.75)) .rp_r(TRUE, tag = "sulfidic_s_lower_bound", note = sprintf("sulfidic S %s %% (a lower bound of the total S)", format(sf)))
       else .rp_r(NA, missing = "s_total_pct", note = "the total sulfur is not recorded")
  wet <- .rp_any(.rp_saturated(x, i), .rp_reduced(x, i),
                 .rp_r(if (isTRUE(.rp_site_lgl(x, "tidal_zone")) || isTRUE(.rp_site_lgl(x, "tidal_inundation"))) TRUE else NA,
                       missing = "water_saturation_days"))
  .rp_all(s, wet)
}

# Z: an anthropic material (anthropic_material, else artefacts 50 % or more).
.rp_t_Z <- function(x, i) {
  am <- .rp_txt(x, i, "anthropic_material")
  kinds <- c(earthy = "Ztr", technological = "Ztc", holorganic = "ZO", archaeological = "Zar", transported = "Z")
  if (!is.na(am)) {
    k <- if (am %in% names(kinds)) kinds[[am]] else "Z"
    return(.rp_r(TRUE, code = k, note = sprintf("anthropic material: %s", am)))
  }
  amp <- .rp_num(x, i, "anthropic_material_pct")
  ar <- .rp_num(x, i, "artefacts_pct")
  v <- if (!is.na(amp)) amp else ar
  if (is.na(v) || v < 50) return(.rp_r(FALSE))
  ind <- .rp_num(x, i, "artefacts_industrial_pct")
  org <- .rp_lgl(x, i, "artefacts_organic")
  k <- if (isTRUE(ind >= v / 2)) "Ztc" else if (isTRUE(org)) "ZO" else "Z"
  .rp_r(TRUE, code = k, note = sprintf("anthropic material %s %% of the volume", format(v)))
}

# Mli: limnic material (layer_origin) in a profile with organic layers.
.rp_t_Mli <- function(x, i) {
  lo <- .rp_txt(x, i, "layer_origin")
  if (!isTRUE(grepl("limnic|coprogen|diatom|gyttja|marl", lo))) return(.rp_r(FALSE))
  any_org <- any(.rp_positions(x)$organic)
  .rp_r(any_org, note = sprintf("layer origin %s%s", lo, if (any_org) " in a profile with holorganic layers" else " with no holorganic layer"))
}

# Mt: a mineral layer between holorganic layers, or over one (p. 207).
.rp_t_Mt <- function(x, i) {
  pos <- .rp_positions(x)
  if (pos$organic[i]) return(.rp_r(FALSE))
  pv <- .rp_prev(x, i); nx <- .rp_next(x, i)
  under <- !is.na(nx) && pos$organic[nx]
  over <- !is.na(pv) && pos$organic[pv]
  .rp_r(under && (over || is.na(pv)), note = if (under) "a mineral layer in contact with holorganic layers" else character(0))
}


# --- step 2: holorganic horizons (pp. 7-9, 205-206; Q19) -----------------------------------

# H or O by the designation of another notation (the FAO H is histic, O is
# not), else by the water saturation.
.rp_histic_side <- function(x, i) {
  f <- x$hint_fam[[i]]
  if ("H" %in% f && !"O" %in% f) return(.rp_r(TRUE, tag = "designation_hint", note = "the designation is an H horizon"))
  if ("O" %in% f && !"H" %in% f) return(.rp_r(FALSE, tag = "designation_hint", note = "the designation is an O horizon"))
  .rp_saturated(x, i)
}

.rp_t_H <- function(x, i) {
  o <- .rp_organic(x, i)
  if (isFALSE(o$passed)) return(.rp_r(FALSE))
  # without a carbon or an ash measurement, only the notation gives an H (Q19)
  if (isTRUE(o$passed) && "designation_hint" %in% o$tag)
    o <- .rp_r(NA, missing = c("loi_pct", "oc_pct"), note = "no carbon or ash measured: an H only by the notation of the RP (Q19)")
  r <- .rp_all(o, .rp_histic_side(x, i))
  if (!isTRUE(r$passed)) return(r)
  fib <- .rp_num(x, i, "fiber_content_rubbed_pct"); vp <- .rp_num(x, i, "von_post_index")
  code <- "H"; variants <- character(0); note <- character(0)
  if (!is.na(fib)) {
    code <- if (fib >= 40) "Hf" else if (fib >= 10) "Hm" else "Hs"
    note <- sprintf("rubbed fibres %s %%", format(fib))
  } else if (!is.na(vp)) {
    if (vp <= 4) code <- "Hf" else if (vp == 5) variants <- c("Hf", "Hm")
    else if (vp <= 7) code <- "Hm" else if (vp == 8) variants <- c("Hm", "Hs") else code <- "Hs"
    note <- sprintf("von Post %s%s", format(vp), if (length(variants)) ": between two kinds" else "")
  } else {
    variants <- c("Hf", "Hm", "Hs")
    note <- "no fibre content or von Post degree: the kind of H is undecided"
  }
  if (isTRUE(.rp_site_lgl(x, "artificially_drained")) && code == "H") {
    code <- "Ha"; variants <- character(0); note <- c(note, "a drained site: H assaini")
  }
  r$code <- code; r$variants <- variants; r$note <- c(r$note, note)
  r
}

# The O kind by the fine organic matter (% vol): OL under 10, OF 10-70 (OFr
# under 30, OFm 30-70), OH over 70 (OHr 70-90, OHf over 90); else 100 minus
# the plant residues, flagged; else a designation of another notation.
.rp_o_kind <- function(x, i) {
  fom <- .rp_num(x, i, "fine_organic_matter_pct")
  tag <- character(0)
  if (is.na(fom)) {
    pr <- .rp_num(x, i, "plant_residues_pct")
    if (!is.na(pr)) { fom <- 100 - pr; tag <- "fom_from_residues" }
  }
  if (!is.na(fom)) {
    k <- if (fom < 10) "OL" else if (fom < 30) "OFr" else if (fom <= 70) "OFm" else if (fom <= 90) "OHr" else "OHf"
    return(list(code = k, tag = tag, note = sprintf("fine organic matter %s %%%s", format(fom),
                                                     if (length(tag)) " (100 minus the plant residues, flagged)" else "")))
  }
  hc <- intersect(x$hint_codes[[i]], c("OL", "OF", "OH"))
  if (length(hc) == 1L) return(list(code = hc, tag = "designation_hint", note = sprintf("the kind of O by the designation '%s'", .rp_des(x, i))))
  list(code = "O", variants = c("OL", "OF", "OH"), note = "no fine organic matter recorded: the kind of O is undecided")
}

# OHta (p. 9): an OH saturated over 80 % by Ca and Mg, pH 5-7, greasy.
.rp_tangel <- function(x, i) {
  sm <- .rp_txt(x, i, "sensitivity_class"); cw <- .rp_txt(x, i, "consistence_wet")
  greasy <- isTRUE(grepl("smear|greas|sensitive", paste(sm, cw)))
  if (!greasy) return(FALSE)
  ca <- .rp_num(x, i, "ca_cmol"); mg <- .rp_num(x, i, "mg_cmol"); cec <- .rp_num(x, i, "cec_cmol")
  ph <- .rp_num(x, i, "ph_h2o")
  sat <- if (!is.na(ca) && !is.na(mg) && isTRUE(cec > 0)) 100 * (ca + mg) / cec else NA
  .kidpr_and(if (is.na(sat)) NA else sat > 80, if (is.na(ph)) NA else ph >= 5 - .RP_TOL$ph && ph <= 7 + .RP_TOL$ph)
}

.rp_t_O <- function(x, i) {
  o <- .rp_organic(x, i)
  if (isFALSE(o$passed)) return(.rp_r(FALSE))
  hs <- .rp_histic_side(x, i)
  side <- .rp_r(if (is.na(hs$passed)) NA else !hs$passed, missing = hs$missing, note = hs$note, tag = hs$tag)
  r <- .rp_all(o, side)
  if (!isTRUE(r$passed)) return(r)
  k <- .rp_o_kind(x, i)
  if (k$code %in% c("OHr", "OHf") && isTRUE(.rp_tangel(x, i))) k <- list(code = "OHta", note = "greasy, saturated by Ca and Mg, pH 5-7: tangel")
  r$code <- k$code; r$variants <- k$variants %||% character(0)
  r$note <- c(r$note, k$note); r$tag <- unique(c(r$tag, k$tag))
  r
}


# --- step 3: coarse constitution (p. 24, 249-251; Q11) ---------------------------------------

.rp_t_ND <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  nw <- .rp_num(x, i, "nodules_wt_pct")
  r <- if (!is.na(nw)) .rp_r(nw > 60, note = sprintf("sesquioxidic nodules %s %% by weight", format(nw)))
       else {
         nv <- sum(c(.rp_num(x, i, "ferric_nodules_pct"), .rp_num(x, i, "bauxitic_nodules_pct"),
                     .rp_num(x, i, "manganiferous_nodules_pct")), na.rm = TRUE)
         if (nv >= 60) .rp_r(TRUE, tag = "weight_from_volume_band", note = sprintf("nodules %s %% by volume: over 60 %% by weight, flagged", format(nv)))
         else if (nv < 35) .rp_r(FALSE)
         else .rp_r(NA, missing = "nodules_wt_pct", tag = "weight_from_volume_band", note = sprintf("nodules %s %% by volume: undecided by weight", format(nv)))
       }
  if (isFALSE(r$passed)) return(r)
  .rp_all(r, .rp_tropical(x))
}

.rp_t_X <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  cw <- .rp_coarse_wt(x, i, 60)
  if (isFALSE(cw$passed)) return(.rp_r(FALSE))
  th <- .rp_tol(x$thick[i], 10, "min", .RP_TOL$d10, "thickness (cm)", "bottom_cm")
  ps <- .rp_pedogenic_sign(x, i)
  r <- .rp_all(cw, th, .rp_r(ps, missing = c("oc_pct", "structure_type"),
                             note = if (isTRUE(ps)) "with a pedogenic sign" else character(0)))
  if (!isTRUE(r$passed)) return(r)
  k <- .rp_peyric_kind(x, i)
  r$code <- k$code; r$variants <- k$variants %||% character(0)
  r$note <- c(r$note, k$note); r$tag <- unique(c(r$tag, k$tag))
  r
}


# --- step 4: hydromorphy (pp. 21-22, 280-281; Q13, Q17, default 18) ---------------------------

# G dominates every other character of the layer (no SG, BTG, CG, p. 280);
# an A with reductic characters is AG. Gr: a reduced matrix with redox
# features of 10 % or less (uniform over more than 90 %, Q17); Go: redox
# segregations at the voids, roots or ped faces; inside the peds the layer is
# a g; with more than 10 % and no location, Go and g are undecided.
.rp_t_G <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  red <- .rp_reduced(x, i)
  if (!isTRUE(red$passed)) return(red)
  rx <- .rp_redox(x, i)
  code <- NA_character_; r <- red
  # Gr with redox features of 5 % or less; 5-10 % with no location leaves Go
  # and g undecided (the review's D1)
  if (identical(rx$kind, "Go") && isTRUE(rx$pct > 0 || rx$mottles)) code <- "Go"
  else if (!is.na(rx$pct) && rx$pct <= 5) code <- "Gr"
  else if (!rx$mottles) code <- "Gr"
  else if (identical(rx$kind, "g")) return(.rp_r(FALSE, note = c(red$note, rx$note, "segregations inside the peds: redox g, not G (default 18)")))
  else return(.rp_r(NA, missing = "redox_segregations_location", note = c(red$note, rx$note, "a grey matrix with mottles and no location: Go or g undecided (default 18)")))
  r$note <- c(red$note, rx$note)
  pos <- .rp_positions(x)
  a <- if (.rp_is_surface(x, i)) .rp_a_material(x, i) else .rp_r(FALSE)
  if (isTRUE(a$passed)) {
    # an A is reductic only in a bluish or greenish hue (the review's D1)
    if (!.rp_gley_hue(.rp_colour(x, i)))
      return(.rp_r(FALSE, note = c(red$note, "an A: reductic only in a bluish or greenish hue")))
    code <- "AG"; r$note <- c(r$note, "an A with reductic characters (p. 13)")
  }
  r$code <- code
  r
}


# --- step 5: indurated and accumulation horizons ----------------------------------------------

# Indurated (cementation strongly or indurated; rupture very or extremely hard).
.rp_indurated <- function(x, i) {
  cc <- .rp_txt(x, i, "cementation_class"); rr <- .rp_txt(x, i, "rupture_resistance")
  if (is.na(cc) && is.na(rr)) return(NA)
  isTRUE(grepl("strong|indurat", cc)) || isTRUE(grepl("very hard|extremely hard", rr))
}
.rp_cemented_any <- function(x, i) {
  cc <- .rp_txt(x, i, "cementation_class")
  if (is.na(cc)) NA else !grepl("^(none|not|no)", cc)
}

.rp_t_U <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  ph <- .rp_num(x, i, "ph_h2o")
  jar <- .rp_lgl(x, i, "jarosite_present")
  st <- .rp_num(x, i, "s_total_pct")
  if (!isTRUE(jar) && !isTRUE(ph < 4) && !isTRUE(st > 0.75)) return(.rp_r(FALSE))
  p <- .rp_tol(ph, 3.5, "max", .RP_TOL$ph, "pH in water (1:1, p. 25)", "ph_h2o")
  if (!is.na(ph)) p$note <- c(p$note, "the pH as recorded; the book reads it 1:1 (Q12)")
  s <- .rp_any(.rp_r(if (is.na(jar)) NA else jar, missing = "jarosite_present", note = if (isTRUE(jar)) "jarosite"),
               .rp_r(if (is.na(st)) NA else st > 0.75, missing = "s_total_pct", note = if (!is.na(st)) sprintf("total S %s %%", format(st))))
  .rp_all(p, s)
}

# K, Kc, Km (p. 20): secondary carbonates in discontinuous forms over 15 %
# of the volume (K), continuous and not indurated (Kc), continuous and
# indurated (Km). Not recorded: calcic() and petrocalcic() stand in.
.rp_t_K <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  sc <- .rp_num(x, i, "secondary_carbonates_pct")
  soft <- .rp_num(x, i, "carbonate_soft_pct"); hard <- .rp_num(x, i, "carbonate_hard_pct")
  if (is.na(sc) && (!is.na(soft) || !is.na(hard))) sc <- sum(c(soft, hard), na.rm = TRUE)
  form <- .rp_txt(x, i, "secondary_carbonates_form")
  ind <- .rp_indurated(x, i); cem <- .rp_cemented_any(x, i)
  if (!is.na(sc)) {
    if (sc <= 15) return(.rp_r(FALSE, note = sprintf("secondary carbonates %s %% of the volume", format(sc))))
    code <- if (isTRUE(ind)) "Km" else if (isTRUE(cem) || isTRUE(grepl("mass", form))) "Kc" else "K"
    return(.rp_r(TRUE, code = code, note = sprintf("secondary carbonates %s %% of the volume%s", format(sc),
                                                   if (!is.na(form)) paste0(", ", form) else "")))
  }
  cc <- .rp_num(x, i, "caco3_pct")
  if (!isTRUE(cc >= 15)) return(.rp_r(FALSE))
  # the stand-in only for an accumulation: below the surface layer, with
  # 5 points of CaCO3 more than every layer below (soilKey's reading)
  if (identical(.rp_positions(x)$first, i)) return(.rp_r(FALSE, note = "carbonate forms not recorded; the surface layer: no stand-in"))
  cb <- vapply(.rp_below(x, i), function(j) .rp_num(x, j, "caco3_pct"), 0)
  if (!length(cb) || all(is.na(cb)) || cc < max(cb, na.rm = TRUE) + 5)
    return(.rp_r(FALSE, note = "carbonate forms not recorded; no more CaCO3 than the layers below: not an accumulation"))
  pc <- .kidpr_wrb_layer(x, "petrocalcic", i)
  if (isTRUE(pc)) return(.rp_r(TRUE, code = "Km", tag = "petrocalcic", note = "the carbonate forms not recorded: soilKey's petrocalcic() stands in"))
  r <- .rp_stand_in(x, i, "calcic", "the secondary carbonate forms")
  if (is.na(r$passed)) r$passed <- FALSE
  if (isTRUE(r$passed)) r$code <- if (isTRUE(cem)) "Kc" else "K"
  r
}

# Si, Sim (pp. 20-21): cemented by silica (durinodes or a duripan);
# continuous and indurated with no roots, Sim.
.rp_t_Si <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  dp <- .rp_num(x, i, "duripan_pct")
  pt <- .rp_txt(x, i, "pan_type")
  hint <- isTRUE(grepl("duripan|silcrete", pt))
  if (is.na(dp) && !hint) return(.rp_r(FALSE))
  r <- if (!is.na(dp)) .rp_r(dp >= 10, note = sprintf("durinodes or duripan %s %%", format(dp)))
       else .rp_r(TRUE, note = sprintf("pan %s", pt))
  if (!isTRUE(r$passed)) return(r)
  ind <- .rp_indurated(x, i)
  ro <- .rp_txt(x, i, "roots_abundance")
  pd <- if (is.na(ind)) .kidpr_wrb_layer(x, "petroduric", i) else ind
  r$code <- if (isTRUE(pd) && !isTRUE(ro %in% c("common", "many"))) "Sim" else "Si"
  if (is.na(ind) && !is.na(pd)) r$tag <- "petroduric"
  r
}

# Fe, Fem, Femp (p. 20): an iron accumulation, discontinuous (Fe), in
# indurated banks (Fem), in thin indurated layers (Femp). In the
# intertropical domain the indurated sesquioxide accumulations read OXm and
# OXc (soilKey's reading).
.rp_t_Fe <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  pt <- .rp_txt(x, i, "pan_type")
  if (isTRUE(grepl("thin ironpan|placic", pt))) return(.rp_r(TRUE, code = "Femp", note = sprintf("pan %s", pt)))
  fn <- .rp_num(x, i, "ferric_nodules_pct")
  ind <- .rp_indurated(x, i)
  r <- if (!is.na(fn)) .rp_r(fn >= 15, note = sprintf("ferric nodules %s %%", format(fn)))
       else if (isTRUE(grepl("ferricrete|ironstone", pt))) .rp_r(TRUE, note = sprintf("pan %s", pt))
       else .rp_r(FALSE)
  if (is.na(fn) && isFALSE(r$passed)) {
    pl <- .rp_num(x, i, "plinthite_pct")
    if (!isTRUE(pl > 0)) return(r)
    r <- .rp_stand_in(x, i, "ferric", "the iron accumulation")
    if (is.na(r$passed)) r$passed <- FALSE
  }
  if (!isTRUE(r$passed)) return(r)
  r$code <- if (isTRUE(ind)) "Fem" else "Fe"
  r$value <- fn
  if (isTRUE(ind)) {
    th <- x$thick[i]
    if (!is.na(th) && th < 1) r$code <- "Femp"
  }
  tr <- .rp_tropical(x)
  if (isTRUE(tr$passed) && r$code == "Fem") return(.rp_r(FALSE, note = "indurated iron in the intertropical domain: OXm or OXc"))
  if (is.na(tr$passed) && r$code == "Fem") return(.rp_r(NA, missing = tr$missing, note = c(r$note, "Fem outside the intertropical domain, OXm or OXc in it")))
  r
}

# Y, Ym, Ys, Yp (pp. 197-198): secondary gypsum. Ys from the surface or
# under Ym (top within 10 +/- 5 cm), gypsum 25 % or more, CaCO3 under 20 %;
# Yp deeper, gypsum 15 % or more, CaCO3 under 40 %; Ym indurated over a Ys.
.rp_t_Y <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  gy <- .rp_num(x, i, "caso4_pct")
  if (is.na(gy) || gy < 15) {
    if (is.na(gy) && isTRUE(.rp_num(x, i, "gypsum_visible_pct") >= 15))
      return(.rp_r(NA, missing = "caso4_pct", note = "visible gypsum; its content is not recorded"))
    return(.rp_r(FALSE))
  }
  cc <- .rp_num(x, i, "caco3_pct")
  ind <- .rp_indurated(x, i)
  top <- x$rtop[i]
  if (isTRUE(ind)) return(.rp_r(TRUE, code = "Ym", note = sprintf("gypsum %s %%, indurated", format(gy))))
  pv <- .rp_prev(x, i)
  under_ym <- !is.na(pv) && isTRUE(.rp_indurated(x, pv)) && isTRUE(.rp_num(x, pv, "caso4_pct") >= 15)
  near <- isTRUE(top <= 10 + .RP_TOL$d10) || under_ym
  if (near && gy >= 25 && !isTRUE(cc >= 20))
    return(.rp_r(TRUE, code = "Ys", note = sprintf("gypsum %s %% from near the surface", format(gy))))
  if (!near && !isTRUE(cc >= 40)) return(.rp_r(TRUE, code = "Yp", note = sprintf("gypsum %s %% at depth", format(gy))))
  .rp_r(TRUE, code = "Y", variants = c("Ys", "Yp"), note = sprintf("gypsum %s %%: the kind of Y is undecided", format(gy)))
}

# OXm, OXc (pp. 152-154, intertropical): a continuous accumulation of metal
# oxides, very hard (OXm, a cuirasse) or less (OXc, a carapace).
.rp_t_OXh <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  pt <- .rp_txt(x, i, "pan_type")
  pl <- .rp_num(x, i, "plinthite_pct"); fn <- .rp_num(x, i, "ferric_nodules_pct")
  oxide <- isTRUE(grepl("ferricrete|ironstone|laterit|cuirass|alcrete|bauxit", pt)) || isTRUE(pl >= 15) || isTRUE(fn >= 15)
  cem <- .rp_cemented_any(x, i); ind <- .rp_indurated(x, i)
  if (!oxide || !isTRUE(cem)) return(.rp_r(FALSE))
  r <- .rp_all(.rp_r(TRUE, note = "a cemented accumulation of oxides"), .rp_tropical(x))
  if (isTRUE(r$passed)) r$code <- if (isTRUE(ind)) "OXm" else "OXc"
  r
}


# --- step 6: surface organo-mineral horizons (pp. 9-14) -----------------------------------------

#' Organo-mineral A material at the surface: organic carbon 0.5 % or more
#' (p. 10), not a bleached layer (moist value 5 or more and chroma 2 or less
#' under 2 % carbon reads E-like); with no carbon recorded, a designation of
#' the A family in another notation, or a moist value of 4 or less darker
#' than the layer below, flagged.
#' @noRd
.rp_a_material <- function(x, i) {
  .rp_memo(x, sprintf("amat_%d", i), function() {
    oc <- .rp_num(x, i, "oc_pct")
    col <- .rp_colour(x, i)
    if (is.na(oc)) {
      if (.rp_hint_in(x, i, c("A", "L"))) return(.rp_r(TRUE, tag = "designation_hint", note = "no carbon recorded: an A by its designation"))
      nx <- .rp_next(x, i)
      if (!is.na(nx) && !is.na(col$value)) {
        cn <- .rp_colour(x, nx)
        if (col$value <= 4 && isTRUE(cn$value >= col$value + 1))
          return(.rp_r(TRUE, tag = "a_from_colour", note = "no carbon recorded: darker than the layer below, flagged"))
      }
      return(.rp_r(NA, missing = "oc_pct", note = "the organic carbon is not recorded"))
    }
    if (oc < 0.5) return(.rp_r(FALSE, note = sprintf("organic carbon %s %%: too little for an A", format(oc))))
    if (oc > 30) return(.rp_r(FALSE, note = sprintf("organic carbon %s %%: an A is never over 30 %% (p. 10)", format(oc))))
    if (isTRUE(col$value >= 5 && col$chroma <= 2) && oc < 2)
      return(.rp_r(FALSE, note = sprintf("%s with %s %% carbon: bleached, not an A", .rp_colour_text(col), format(oc))))
    # a light layer low in carbon directly under the O horizons is an E (the
    # review's d6)
    pv <- .rp_prev(x, i)
    if (!is.na(pv) && isTRUE(.rp_organic(x, pv)$passed) && isTRUE(col$value >= 5) && oc < 1)
      return(.rp_r(FALSE, note = sprintf("%s with %s %% carbon: light and low in carbon, not an A", .rp_colour_text(col), format(oc))))
    .rp_r(TRUE, note = sprintf("organic carbon %s %%", format(oc)))
  })
}

# The surface gate of the A family: a surface layer with A material.
.rp_a_gate <- function(x, i) {
  if (!.rp_is_mineral(x, i) || !.rp_is_surface(x, i)) return(.rp_r(FALSE))
  .rp_a_material(x, i)
}

# An (p. 13, Annex 2): black, organic carbon over 8 % (8 +/- 2), prolonged
# waterlogging (saturation, or a reduced or wet layer).
.rp_t_An <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  oc <- .rp_oc_min(x, i, 8, .RP_TOL$oc8)
  if (isFALSE(oc$passed)) return(oc)
  col <- .rp_colour(x, i); dw <- .rp_dark_word(col)
  black <- .rp_r(if (is.na(dw)) NA else dw == "noir", missing = col$missing, tag = col$tag,
                 note = sprintf("%s (%s)", .rp_colour_text(col), dw))
  if (isFALSE(black$passed)) return(black)
  wet <- .rp_any(.rp_saturated(x, i), .rp_r(if (.rp_wet_sign(x, i)) TRUE else FALSE))
  .rp_all(g, oc, black, wet)
}

# Ach (pp. 12-13, 126): moist value 3.5 or less (the chapter, Q17), chroma
# 2 or less; organic carbon 3 % or more (the 30 g/kg of p. 12, read for every
# texture); CaCO3 under 5 %; pH in water 6.0-8.3; a fine biological
# structure, chernic() or mollic() standing in when not recorded.
.rp_t_Ach <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  col <- .rp_colour(x, i)
  # value 3.5 or less: the chapter (p. 126) over the general list (Q17)
  cv <- .rp_all(.rp_tol(col$value, 3.5, "max", 0, "moist value", "munsell_value_moist"),
                .rp_tol(col$chroma, 2, "max", 0, "moist chroma", "munsell_chroma_moist"))
  if (isFALSE(cv$passed)) return(cv)
  # 3 % carbon in the top 10 cm of the A run (the review's D13)
  run <- .rp_surface_run(x)
  top <- run[1L]
  oc <- .rp_oc_min(x, top, 3)
  if (top != i) oc$note <- c(oc$note, "in the top 10 cm of the A run")
  if (isFALSE(oc$passed)) return(oc)
  # darker than a mineral layer below, with a gradual decrease of the carbon
  # (pp. 12, 126; the review's d1)
  last <- run[length(run)]
  below <- .rp_next(x, last)
  lb <- if (is.na(below) || !.rp_is_mineral(x, below) || .rp_no_fine_earth(x, below) || .rp_hint_in(x, below, c("R", "M", "D")))
          .rp_r(FALSE, note = "no mineral layer below the A run: not a chernic A")
        else {
          cb <- .rp_colour(x, below); cl <- .rp_colour(x, last)
          .rp_r(if (is.na(cb$value) || is.na(cl$value)) NA else cb$value > cl$value, missing = "munsell_value_moist",
                note = if (!is.na(cb$value) && !is.na(cl$value)) sprintf("the layer below %s", .rp_colour_text(cb)))
        }
  if (isFALSE(lb$passed)) return(lb)
  ocs <- vapply(c(run, below), function(j) .rp_num(x, j, "oc_pct"), 0)
  bd <- .rp_txt(x, last, "boundary_distinctness")
  grad <- if (isTRUE(grepl("gradual|diffuse", bd))) TRUE
          else if (length(run) >= 2L && !anyNA(ocs[seq_along(run)]) &&
                   all(ocs[seq_along(run)][-1L] / ocs[seq_along(run)][-length(run)] >= 0.4)) TRUE
          else if (isTRUE(ocs[length(ocs)] >= 0.6)) TRUE
          else if (!is.na(bd) && !is.na(ocs[length(ocs)])) FALSE else NA
  gd <- .rp_r(grad, missing = "boundary_distinctness", note = if (isTRUE(grad)) "carbon decreasing gradually with depth")
  if (isFALSE(gd$passed)) return(gd)
  cc <- .rp_num(x, i, "caco3_pct")
  carb <- .rp_r(if (is.na(cc)) TRUE else cc < 5, note = if (!is.na(cc)) sprintf("CaCO3 %s %%", format(cc)))
  if (isFALSE(carb$passed)) return(carb)
  ph <- .rp_between(.rp_num(x, i, "ph_h2o"), 6.0, 8.3, .RP_TOL$ph, "pH in water", "ph_h2o")
  if (isFALSE(ph$passed)) return(ph)
  st <- .rp_biomacro(x, i, meso = TRUE)
  if (is.na(st$passed)) {
    w <- .kidpr_wrb_layer(x, "chernic", i)
    st <- .rp_r(w, missing = st$missing, tag = "chernic",
                note = sprintf("the biological structure not recorded: soilKey's chernic() %s",
                               if (is.na(w)) "undecided" else if (w) "stands in (yes)" else "stands in (no)"))
  }
  .rp_all(g, cv, oc, lb, gd, carb, ph, st)
}

# Aso (p. 13; grisols p. 193): not calcareous; organic carbon over 1.2 %;
# moist chroma under 4, value under 4 and their sum under 6 (the general
# list: the test); value under 4.5, chroma under 4, not 4/3 (the grisols:
# evidence; Q17, only one of the two holding is imperfect); under permanent
# vegetation S/CEC 50-80 % and pH 5.5-6.5 (not under cultivation).
.rp_t_Aso <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  col <- .rp_colour(x, i)
  v <- col$value; c <- col$chroma
  gen <- if (is.na(v) || is.na(c)) NA else v < 4 && c < 4 && v + c < 6
  gri <- if (is.na(v) || is.na(c)) NA else v < 4.5 && c < 4 && !(v == 4 && c == 3)
  # the list of p. 13 is the test; the grisols reading of p. 193 is evidence
  # only (the review's D5)
  if (isFALSE(gen)) return(.rp_r(FALSE, note = sprintf("%s: not sombre by p. 13 (the grisols reading of p. 193 %s)",
                                                       .rp_colour_text(col), if (isTRUE(gri)) "holds" else "fails")))
  cv <- .rp_r(gen, missing = col$missing, tag = col$tag,
              note = sprintf("%s: the list of p. 13 holds, the grisols of p. 193 %s", .rp_colour_text(col),
                             if (isTRUE(gri)) "holds" else "fails"))
  oc <- .rp_tol(.rp_num(x, i, "oc_pct"), 1.2, "min", 0, "organic carbon (%)", "oc_pct")
  if (isFALSE(oc$passed)) return(oc)
  # good aeration (p. 13): no functional redox features and no reduced matrix
  if (isTRUE(.rp_redox(x, i)$functional) || isTRUE(.rp_reduced(x, i)$passed))
    return(.rp_r(FALSE, note = "redox features or a reduced matrix: not the well aerated Aso (p. 13)"))
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  cult <- isTRUE(.rp_ploughed(x, i, .rp_positions(x)$first %in% i)$passed)
  sat <- if (cult) .rp_r(TRUE, note = "under cultivation: S/CEC and pH not tested (LAso)")
         else .rp_all(.rp_bs_test(x, i, 80, "max"), .rp_bs_test(x, i, 50, "min"),
                      .rp_between(.rp_num(x, i, "ph_h2o"), 5.5, 6.5, .RP_TOL$ph, "pH in water", "ph_h2o"))
  if (isFALSE(sat$passed)) return(sat)
  st <- .rp_r(TRUE)
  if (is.na(.kidpr_structure(x, i)$type)) {
    st <- .rp_stand_in(x, i, "umbric_horizon", "the structure")
    if (is.na(st$passed)) st$passed <- TRUE
  }
  .rp_all(g, cv, oc, nc, sat, st)
}

# Aca (p. 12): generalised cold effervescence with CaCO3 over 2 %; organic
# carbon 1-8 %; hue 7.5YR to 2.5Y, moist value and chroma 4 or less (the
# value over 4 when CaCO3 is over 40 %); pH 7.0-8.7; biomacrostructured.
.rp_t_Aca <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  ca <- .rp_calcareous(x, i); if (isFALSE(ca$passed)) return(ca)
  oc <- .rp_between(.rp_num(x, i, "oc_pct"), 1, 8, 0, "organic carbon (%)", "oc_pct")
  if (isFALSE(oc$passed)) return(oc)
  col <- .rp_colour(x, i); cc <- .rp_num(x, i, "caco3_pct")
  hue <- .rp_hue_in(col, 17.5, 22.5)
  vok <- if (is.na(col$value)) NA else col$value <= 4 || isTRUE(cc > 40)
  cok <- if (is.na(col$chroma)) NA else col$chroma <= 4
  colr <- .rp_r(.kidpr_and(hue, vok, cok), missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(colr$passed)) return(colr)
  ph <- .rp_between(.rp_num(x, i, "ph_h2o"), 7.0, 8.7, .RP_TOL$ph, "pH in water", "ph_h2o")
  if (isFALSE(ph$passed)) return(ph)
  bm <- .rp_biomacro(x, i)
  .rp_all(g, ca, oc, colr, ph, bm)
}

# Ado (p. 12): carbonated, no or very weak cold effervescence, dolomitic.
.rp_t_Ado <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  c <- .rp_carbonates(x, i)
  if (isFALSE(c$carbonated)) return(.rp_r(FALSE))
  if (isTRUE(c$cold) && isTRUE(grepl("strong", c$eff))) return(.rp_r(FALSE, note = "a strong cold effervescence: calcite"))
  d <- .rp_dolomitic(x, i); if (isFALSE(d$passed)) return(d)
  .rp_all(g, d, .rp_biomacro(x, i))
}

# Aci, Amg (pp. 12-13): not carbonated; S/CEC over 80 %; Ca/Mg over 5 (Aci;
# 2-5 the calcimagnesian variant, imperfect) or under 2 (Amg); Aci organic
# carbon under 8 % and hue 5YR to 2.5Y; biostructured.
.rp_t_Aci <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  bs <- .rp_bs_test(x, i, 80, "min"); if (isFALSE(bs$passed)) return(bs)
  cm <- .rp_camg(x, i)
  camg <- if (is.na(cm)) .rp_r(NA, missing = c("ca_cmol", "mg_cmol"), note = "Ca/Mg not recorded")
          else if (cm > 5) .rp_r(TRUE, note = sprintf("Ca/Mg %s", format(signif(cm, 3))))
          else if (cm >= 2) .rp_r(TRUE, imperfect = TRUE, note = sprintf("Ca/Mg %s: the calcimagnesian variant (2-5), imperfect", format(signif(cm, 3))))
          else .rp_r(FALSE, note = sprintf("Ca/Mg %s: magnesic", format(signif(cm, 3))))
  if (isFALSE(camg$passed)) return(camg)
  oc <- .rp_oc_max(x, i, 8, .RP_TOL$oc8); if (isFALSE(oc$passed)) return(oc)
  col <- .rp_colour(x, i)
  hue <- .rp_r(.rp_hue_in(col, 15, 22.5), missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(hue$passed)) return(hue)
  .rp_all(g, nc, bs, camg, oc, hue, .rp_biomacro(x, i, meso = TRUE))
}

.rp_t_Amg <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  bs <- .rp_bs_test(x, i, 80, "min"); if (isFALSE(bs$passed)) return(bs)
  cm <- .rp_camg(x, i)
  camg <- if (is.na(cm)) .rp_r(NA, missing = c("ca_cmol", "mg_cmol")) else .rp_r(cm < 2, note = sprintf("Ca/Mg %s", format(signif(cm, 3))))
  .rp_all(g, nc, bs, camg)
}

# The andosol A horizons (pp. 77-79): the andic gate; Alu pH under 4.5,
# Alpy/Alox 0.5 or more, Siox under 0.6 %; And pH 4.5 or more, Alpy/Alox under
# 0.5 (default 15: under 0.5 silandic), Siox 0.6 % or more; both organic
# carbon over 3 % and under 25 %, moist value and chroma 3 or less. Avi: the
# vitric properties, clay 20 % or less, organic carbon 0.6-25 %.
.rp_andic_a <- function(x, i, kind) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  an <- .rp_andic(x, i); if (isFALSE(an$passed)) return(an)
  if (kind == "Avi") {
    if (identical(an$value, "andic")) return(.rp_r(FALSE, note = "andic, not vitric"))
    cl <- .rp_tol(.rp_clay(x, i), 20, "max", 0, "clay (%)", "clay_pct")
    oc <- .rp_between(.rp_num(x, i, "oc_pct"), 0.6, 25, 0, "organic carbon (%)", "oc_pct")
    return(.rp_all(g, an, cl, oc))
  }
  if (identical(an$value, "vitric")) return(.rp_r(FALSE, note = "vitric, not andic"))
  oc <- .rp_all(.rp_tol(.rp_num(x, i, "oc_pct"), 3, "min", 0, "organic carbon (%)", "oc_pct"),
                .rp_tol(.rp_num(x, i, "oc_pct"), 25, "max", 0, "organic carbon (%)", "oc_pct"))
  if (isFALSE(oc$passed)) return(oc)
  col <- .rp_colour(x, i)
  cv <- .rp_all(.rp_tol(col$value, 3, "max", 0, "moist value", "munsell_value_moist"),
                .rp_tol(col$chroma, 3, "max", 0, "moist chroma", "munsell_chroma_moist"))
  if (isFALSE(cv$passed)) return(cv)
  alp <- .rp_num(x, i, "al_pyro_pct"); alo <- .rp_num(x, i, "al_ox_pct")
  ratio <- if (!is.na(alp) && isTRUE(alo > 0)) alp / alo else NA_real_
  si <- .rp_num(x, i, "si_ox_pct"); ph <- .rp_num(x, i, "ph_h2o")
  chem <- if (kind == "Alu")
    .rp_all(.rp_tol(ph, 4.5 - 1e-9, "max", 0, "pH in water", "ph_h2o"),
            .rp_r(if (is.na(ratio)) NA else ratio >= 0.5, missing = c("al_pyro_pct", "al_ox_pct"),
                  note = if (!is.na(ratio)) sprintf("Alpy/Alox %s", format(signif(ratio, 3)))),
            .rp_r(if (is.na(si)) NA else si < 0.6, missing = "si_ox_pct"))
  else
    .rp_all(.rp_tol(ph, 4.5, "min", 0, "pH in water", "ph_h2o"),
            .rp_r(if (is.na(ratio)) NA else ratio < 0.5, missing = c("al_pyro_pct", "al_ox_pct"),
                  note = if (!is.na(ratio)) sprintf("Alpy/Alox %s", format(signif(ratio, 3)))),
            .rp_r(if (is.na(si)) NA else si >= 0.6, missing = "si_ox_pct"))
  .rp_all(g, an, oc, cv, chem)
}
.rp_t_Alu <- function(x, i) .rp_andic_a(x, i, "Alu")
.rp_t_And <- function(x, i) .rp_andic_a(x, i, "And")
.rp_t_Avi <- function(x, i) .rp_andic_a(x, i, "Avi")

# The vertic characters common to Av, SV and V (pp. 22-23, 318): clay 40 %
# or more (default 17).
.rp_vertic_clay <- function(x, i) .rp_clay_min(x, i, 40)
.rp_slickensides <- function(x, i) {
  s <- .rp_txt(x, i, "slickensides")
  if (is.na(s)) return(NA_integer_)
  if (grepl("^(absent|none|no)", s)) 0L else if (grepl("^few|^very few", s)) 1L else 2L
}
.rp_cracks <- function(x, i) {
  cw <- .rp_num(x, i, "cracks_width_cm"); pc <- .rp_num(x, i, "polygonal_cracks_spacing_cm")
  sec <- .rp_txt(x, i, "structure_type_secondary")
  if (isTRUE(cw >= 1) || !is.na(pc) || isTRUE(grepl("prism", sec))) TRUE
  else if (!is.na(cw)) FALSE else NA
}

# Av (p. 23): an A with the vertic characters.
.rp_t_Av <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  cl <- .rp_vertic_clay(x, i); if (isFALSE(cl$passed)) return(cl)
  sl <- .rp_slickensides(x, i); ck <- .rp_cracks(x, i)
  v <- if (isTRUE(sl >= 1) || isTRUE(ck)) .rp_r(TRUE, note = "slickensides or shrinkage cracks")
       else if (is.na(sl) && is.na(ck)) .rp_stand_in(x, i, "vertic_properties", "the vertic signs")
       else .rp_r(FALSE)
  if (is.na(v$passed)) v$passed <- FALSE
  r <- .rp_all(g, cl, v)
  if (isTRUE(r$passed)) { r$code <- "A"; r$features <- "v" }
  r
}

# Anod (p. 155): metallic nodules 30-60 % by weight (the volume, flagged).
.rp_t_Anod <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  nw <- .rp_num(x, i, "nodules_wt_pct")
  r <- if (!is.na(nw)) .rp_r(nw > 30 && nw < 60, note = sprintf("nodules %s %% by weight", format(nw)))
       else {
         nv <- sum(c(.rp_num(x, i, "ferric_nodules_pct"), .rp_num(x, i, "bauxitic_nodules_pct")), na.rm = TRUE)
         if (nv >= 30 && nv < 60) .rp_r(NA, missing = "nodules_wt_pct", tag = "weight_from_volume_band",
                                        note = sprintf("nodules %s %% by volume: undecided by weight", format(nv)))
         else .rp_r(FALSE)
       }
  r <- .rp_all(g, r)
  if (isTRUE(r$passed)) { r$code <- "A"; r$features <- "nod" }
  r
}

# Ae (p. 11): an A directly above an E.
.rp_t_Ae <- function(x, i) {
  g <- .rp_a_gate(x, i); if (isFALSE(g$passed)) return(g)
  nx <- .rp_next(x, i)
  if (is.na(nx)) return(.rp_r(FALSE))
  # not on a ploughed layer: p. 11 places Ae under forest or spontaneous
  # vegetation (the review's d2)
  if (isTRUE(.rp_ploughed(x, i, .rp_is_first(x, i))$passed)) return(.rp_r(FALSE, note = "a ploughed layer: not an Ae"))
  e <- .rp_e_raw(x, nx)
  .rp_all(g, .rp_r(e$passed, missing = e$missing, tag = e$tag, note = if (isTRUE(e$passed)) "directly above an E"))
}

.rp_t_A <- function(x, i) .rp_a_gate(x, i)

# Js (p. 25): a surface layer with little organic matter (carbon under
# 0.5 %), never biomacrostructured, a weak structure or none.
.rp_t_Js <- function(x, i) {
  if (!.rp_is_mineral(x, i) || !.rp_is_surface(x, i)) return(.rp_r(FALSE))
  oc <- .rp_tol(.rp_num(x, i, "oc_pct"), 0.5 - 1e-9, "max", 0, "organic carbon (%)", "oc_pct")
  if (isFALSE(oc$passed)) return(oc)
  gr <- .rp_grade(x, i); st <- .rp_structured(x, i)
  weak <- .rp_r(if (isFALSE(st$passed)) TRUE else if (!is.na(gr)) gr <= 1 else NA,
                missing = c("structure_grade", "structure_type"), note = st$note)
  .rp_all(oc, weak)
}


# --- step 7: subsurface horizons --------------------------------------------------------------

#' E on the data (pp. 14-15): under another horizon; lighter than the layer
#' below (moist value one more, or chroma one less) and depleted against it
#' (clay increase below, a BP-like layer below, or a bleached chroma of 2 or
#' less); less carbon than the layer above when both are recorded. With no
#' colour, soilKey's albic() stands in, flagged.
#' @noRd
.rp_e_raw <- function(x, i) {
  .rp_memo(x, sprintf("eraw_%d", i), function() {
    if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
    pv <- .rp_prev(x, i); nx <- .rp_next(x, i)
    if (is.na(pv) || is.na(nx)) return(.rp_r(FALSE))
    col <- .rp_colour(x, i); cb <- .rp_colour(x, nx)
    # bleached: moist value 5 or more and chroma 2 or less (the review's D4)
    bleached <- isTRUE(col$value >= 5 && col$chroma <= 2)
    ocv <- .rp_num(x, i, "oc_pct"); oca <- .rp_num(x, pv, "oc_pct")
    if (!is.na(ocv) && !is.na(oca) && .rp_is_mineral(x, pv) && ocv >= oca)
      return(.rp_r(FALSE, note = "no less carbon than the layer above"))
    # E is depleted: clay films common or many, or more clay than the layer
    # above, exclude it (the review's D4)
    if (isTRUE(.clay_films_class(.kidpr_v(x, i, "clay_films_amount")) >= 3L))
      return(.rp_r(FALSE, note = "clay films common or many: not an eluvial horizon"))
    ca <- .rp_clay(x, pv); ci <- .rp_clay(x, i)
    if (.rp_is_mineral(x, pv) && !is.na(ca) && !is.na(ci) && ci > ca)
      return(.rp_r(FALSE, note = "more clay than the layer above: not an eluvial horizon"))
    # above a rock or a coarse layer with no fine earth, only a bleached layer
    # is an E (the review's D3)
    rocky <- .rp_no_fine_earth(x, nx) || (length(x$hint_codes[[nx]]) && all(x$hint_codes[[nx]] %in% c("R", "M", "D"))) ||
      isTRUE(.rp_t_R(x, nx)$passed) || isTRUE(.rp_t_D(x, nx)$passed) || isTRUE(.rp_t_M(x, nx)$passed)
    if (rocky && !bleached)
      return(.rp_r(FALSE, note = "above a rock or a layer with no fine earth, and not bleached: not an E"))
    if (is.na(col$value) || is.na(cb$value)) {
      if (bleached) return(.rp_r(TRUE, tag = col$tag, note = sprintf("%s: bleached", .rp_colour_text(col))))
      r <- .rp_stand_in(x, i, "albic", "the colour")
      return(r)
    }
    # lighter: a higher value, or a lower chroma at an equal or higher value
    lighter <- col$value >= cb$value + 1 || isTRUE(col$chroma <= cb$chroma - 1 && col$value >= cb$value)
    if (!lighter) return(.rp_r(FALSE, note = sprintf("%s: not lighter than the layer below", .rp_colour_text(col))))
    cn <- .rp_clay(x, nx)
    clay_inc <- .rp_more_clay(cn, ci)
    bpb <- .rp_bp_raw(x, nx)$passed
    bp_below <- isTRUE(bpb)
    dep <- if (isTRUE(clay_inc) || bp_below || bleached) TRUE else if (is.na(clay_inc) || is.na(bpb)) NA else FALSE
    .rp_r(dep, missing = "clay_pct", tag = col$tag,
          note = c(sprintf("%s: lighter than the layer below", .rp_colour_text(col)),
                   if (isTRUE(clay_inc)) "more clay below", if (bp_below) "a podzolic B below",
                   if (bleached) "bleached (moist value 5 or more, chroma 2 or less)"))
  })
}

.rp_t_E <- function(x, i) {
  r <- .rp_e_raw(x, i)
  if (!isTRUE(r$passed)) return(r)
  col <- .rp_colour(x, i)
  f <- character(0)
  ug <- .rp_num(x, i, "uncoated_grains_pct")
  alb <- isTRUE(ug >= 50) || isTRUE(col$value >= 6 && col$chroma <= 2) || isTRUE(.kidpr_wrb_layer(x, "albic", i))
  if (alb) f <- "a"
  oc <- .rp_num(x, i, "oc_pct")
  if (isTRUE(col$value <= 4 && col$chroma <= 2) && isTRUE(oc >= 1)) f <- c(f, "h")
  r$features <- f
  r
}

#' BP on the data (p. 19): a moist hue of 7.5YR or redder (necessary), more
#' oxalate Al and Fe than the layer above (Alox + 1/2 Feox); with no oxalate,
#' soilKey's spodic() stands in, flagged; most of the carbon extractable by
#' pyrophosphate when c_pyro_pct is recorded.
#' @noRd
.rp_bp_raw <- function(x, i) {
  .rp_memo(x, sprintf("bpraw_%d", i), function() {
    if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
    pv <- .rp_prev(x, i)
    if (is.na(pv)) return(.rp_r(FALSE))
    col <- .rp_colour(x, i)
    hue <- .rp_hue_in(col, -10, 17.5)
    if (isFALSE(hue)) return(.rp_r(FALSE, note = sprintf("%s: yellower than 7.5YR", .rp_colour_text(col))))
    ox <- function(j) { a <- .rp_num(x, j, "al_ox_pct"); f <- .rp_num(x, j, "fe_ox_pct")
                        if (is.na(a) || is.na(f)) NA_real_ else a + f / 2 }
    oi <- ox(i); oa <- ox(pv)
    cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
    if (is.na(oi) && isTRUE(cf >= 3L)) return(.rp_r(FALSE, note = "clay coatings common or many: an illuvial clay, not a podzolic B"))
    acc <- if (!is.na(oi) && !is.na(oa)) .rp_r(oi > oa, note = sprintf("Alox + 1/2 Feox %s against %s above", format(oi), format(oa)))
           else .rp_stand_in(x, i, "spodic", "the oxalate Al and Fe")
    if (is.na(acc$passed)) {
      # spodic() undecided: a podzolic B only under a bleached layer or with
      # more carbon than the layer above (soilKey's reading)
      cp <- .rp_colour(x, pv)
      ocp <- .rp_num(x, pv, "oc_pct"); oci <- .rp_num(x, i, "oc_pct")
      if (!isTRUE(cp$chroma <= 2) && !isTRUE(oci > ocp))
        return(.rp_r(FALSE, note = "the oxalate Al and Fe not recorded; no bleached layer above and no illuvial carbon: not a podzolic B"))
    }
    if (isFALSE(acc$passed)) return(acc)
    cp <- .rp_num(x, i, "c_pyro_pct"); oc <- .rp_num(x, i, "oc_pct")
    py <- if (!is.na(cp) && isTRUE(oc > 0)) .rp_r(cp / oc > 0.5, note = sprintf("pyrophosphate carbon %s of %s %%", format(cp), format(oc)))
          else .rp_r(TRUE)
    .rp_all(.rp_r(hue, missing = col$missing, tag = col$tag, note = .rp_colour_text(col)), acc, py)
  })
}

.rp_t_BP <- function(x, i) {
  r <- .rp_bp_raw(x, i)
  if (!isTRUE(r$passed)) return(r)
  oc <- .rp_num(x, i, "oc_pct")
  nx <- .rp_next(x, i); pv <- .rp_prev(x, i)
  bp_n <- !is.na(nx) && isTRUE(.rp_bp_raw(x, nx)$passed)
  bp_p <- !is.na(pv) && isTRUE(.rp_bp_raw(x, pv)$passed)
  r$code <- "BP"
  if (bp_n && isTRUE(oc > .rp_num(x, nx, "oc_pct"))) r$code <- "BPh"
  else if (bp_p && isTRUE(oc < .rp_num(x, pv, "oc_pct"))) r$code <- "BPs"
  if (r$code != "BP") r$note <- c(r$note, sprintf("%s: %s carbon than the podzolic layer %s", r$code,
                                                   if (r$code == "BPh") "more" else "less", if (r$code == "BPh") "below" else "above"))
  r
}

#' BT on the data (pp. 17-18): more clay than the mineral layers above (or,
#' at the surface, than the layer below); 15 cm thick or more with the
#' adjacent layers that have the increase (or clay bands of 15 cm in all);
#' many clay coatings (clay_films_amount common or many); not recorded,
#' soilKey's argic() stands in, flagged, but not in a layer with no peds.
#' @noRd
.rp_bt_core <- function(x, i) {
  .rp_memo(x, sprintf("btcore_%d", i), function() {
    if (!.rp_is_mineral(x, i)) return(NA)
    ci <- .rp_clay(x, i)
    ref <- .rp_clay_above(x, i)
    if (is.na(ref) && is.na(.rp_prev(x, i))) {
      nx <- .rp_next(x, i)
      ref <- if (is.na(nx)) NA_real_ else .rp_clay(x, nx)
    }
    .rp_more_clay(ci, ref)
  })
}
.rp_bt_raw <- function(x, i) {
  .rp_memo(x, sprintf("btraw_%d", i), function() {
    core <- .rp_bt_core(x, i)
    if (isFALSE(core)) return(.rp_r(FALSE, note = "no more clay than the layers above"))
    inc <- .rp_r(core, missing = "clay_pct",
                 note = if (isTRUE(core)) sprintf("clay %s %% against %s %% above", format(.rp_clay(x, i)), format(.rp_clay_above(x, i))))
    lam <- .rp_num(x, i, "lamellae_cumulative_cm")
    th <- if (!is.na(lam)) .rp_r(lam > 15, note = sprintf("clay bands of %s cm in all", format(lam)))
          else {
            t <- .rp_run_thickness(x, i, function(j) .rp_bt_core(x, j))
            .rp_tol(t, 15, "min", 0, "thickness with the adjacent illuvial layers (cm)", "bottom_cm")
          }
    if (isFALSE(th$passed)) return(th)
    cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
    co <- if (!is.na(cf)) .rp_r(cf >= 3L, note = sprintf("clay films %s", .rp_txt(x, i, "clay_films_amount")))
          else if (isFALSE(.rp_structured(x, i)$passed)) .rp_r(FALSE, note = "clay films not recorded; no peds: no coatings")
          else .rp_stand_in(x, i, "argic", "the clay coatings")
    .rp_all(inc, th, co)
  })
}

.rp_t_BT <- function(x, i) {
  r <- .rp_bt_raw(x, i)
  if (!isTRUE(r$passed)) return(r)
  r$code <- "BT"
  # BTss (p. 18): at the contact of a carbonate rock, under a BT, darker,
  # more clay, twice the organic matter of the BT above
  nx <- .rp_next(x, i); pv <- .rp_prev(x, i)
  if (!is.na(nx) && !is.na(pv) && isTRUE(.rp_bt_raw(x, pv)$passed)) {
    rock <- .rp_hint_in(x, nx, c("R", "M", "D")) || isTRUE(.rp_num(x, nx, "caco3_pct") >= 40)
    calc <- isTRUE(.rp_num(x, nx, "caco3_pct") >= 40) || isTRUE(.rp_carbonates(x, nx)$cold)
    oc2 <- isTRUE(.rp_num(x, i, "oc_pct") >= 2 * .rp_num(x, pv, "oc_pct"))
    cl <- isTRUE(.rp_clay(x, i) > .rp_clay(x, pv))
    col <- .rp_colour(x, i)
    if (rock && calc && oc2 && cl && isTRUE(col$value <= 4)) {
      r$code <- "BT\u00df"
      r$note <- c(r$note, "at the contact of a carbonate rock, darker, more clay and twice the carbon of the BT above")
    }
  }
  r
}

# V (p. 23, 318): clay 40 % or more; many slickensides (common or more);
# not recorded, vertic_horizon() stands in.
.rp_t_V <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  cl <- .rp_vertic_clay(x, i); if (isFALSE(cl$passed)) return(cl)
  sl <- .rp_slickensides(x, i)
  s <- if (!is.na(sl)) .rp_r(sl >= 2L, note = sprintf("slickensides %s", .rp_txt(x, i, "slickensides")))
       else .rp_stand_in(x, i, "vertic_horizon", "the slickensides")
  if (is.na(sl) && is.na(s$passed)) s$passed <- FALSE
  .rp_all(cl, s)
}

# SV (p. 23): a subsurface vertic structure (cracks, a prismatic
# over-structure) with clay 40 % or more and no V; vertic_properties()
# stands in.
.rp_t_SV <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  cl <- .rp_vertic_clay(x, i); if (isFALSE(cl$passed)) return(cl)
  ck <- .rp_cracks(x, i)
  s <- if (!is.na(ck)) .rp_r(ck, note = if (ck) "shrinkage cracks or a prismatic over-structure")
       else .rp_stand_in(x, i, "vertic_properties", "the shrinkage cracks")
  if (is.na(ck) && is.na(s$passed)) s$passed <- FALSE
  .rp_all(cl, s)
}

# Snd, Slu (pp. 78, 80): the andic gate below the surface; organic carbon
# 0.6-3 %; Snd pH 5.0 or more, Alpy/Alox under 0.5, Siox 0.6 % or more; Slu
# pH under 5.0, Alpy/Alox 0.5 or more.
.rp_andic_s <- function(x, i, kind) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  an <- .rp_andic(x, i); if (isFALSE(an$passed)) return(an)
  if (identical(an$value, "vitric")) return(.rp_r(FALSE, note = "vitric, not andic"))
  oc <- .rp_between(.rp_num(x, i, "oc_pct"), 0.6, 3, 0, "organic carbon (%)", "oc_pct")
  if (isFALSE(oc$passed)) return(oc)
  alp <- .rp_num(x, i, "al_pyro_pct"); alo <- .rp_num(x, i, "al_ox_pct")
  ratio <- if (!is.na(alp) && isTRUE(alo > 0)) alp / alo else NA_real_
  ph <- .rp_num(x, i, "ph_h2o")
  chem <- if (kind == "Snd")
    .rp_all(.rp_tol(ph, 5.0, "min", .RP_TOL$ph, "pH in water", "ph_h2o"),
            .rp_r(if (is.na(ratio)) NA else ratio < 0.5, missing = c("al_pyro_pct", "al_ox_pct")),
            .rp_r(if (is.na(.rp_num(x, i, "si_ox_pct"))) NA else .rp_num(x, i, "si_ox_pct") >= 0.6, missing = "si_ox_pct"))
  else
    .rp_all(.rp_tol(ph, 5.0 - 1e-9, "max", .RP_TOL$ph, "pH in water", "ph_h2o"),
            .rp_r(if (is.na(ratio)) NA else ratio >= 0.5, missing = c("al_pyro_pct", "al_ox_pct")))
  .rp_all(an, oc, chem)
}
.rp_t_Snd <- function(x, i) .rp_andic_s(x, i, "Snd")
.rp_t_Slu <- function(x, i) .rp_andic_s(x, i, "Slu")

# Ki (molar SiO2/Al2O3 of the sulfuric attack).
.rp_ki <- function(x, i) compute_ki(.rp_num(x, i, "sio2_sulfuric_pct"), .rp_num(x, i, "al2o3_sulfuric_pct"))

# F (pp. 149-151, intertropical): chroma and value 2-8; hue 10R to 10YR;
# clay over 12 % (the mineralogical clay, read as clay_pct, flagged); Ki
# under 2.2; oxides under 60 % of the clay when recorded; CEC 5-16 cmol+/kg
# of clay (the table, Q17; per kg of soil under 16 +/- 1 as the fallback);
# pH 3.5-7. Without Ki, soilKey's ferralic() stands in.
.rp_t_F <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  ox <- .rp_num(x, i, "oxides_triacid_pct")
  if (isTRUE(ox > 60)) return(.rp_r(FALSE, note = "oxides over 60 % of the clay: oxidic"))
  col <- .rp_colour(x, i)
  colr <- .rp_r(.kidpr_and(.rp_hue_in(col, 10, 20),
                           if (is.na(col$value)) NA else col$value >= 2 && col$value <= 8,
                           if (is.na(col$chroma)) NA else col$chroma >= 2 && col$chroma <= 8),
                missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(colr$passed)) return(colr)
  cl <- .rp_tol(.rp_clay(x, i), 12, "min", 0, "clay (%)", "clay_pct")
  if (isFALSE(cl$passed)) return(cl)
  cc <- .rp_cec_clay(x, i)
  cec <- if (!is.na(cc)) .rp_r(cc >= 5 && cc <= 16, note = sprintf("CEC %s cmol+/kg of clay", format(round(cc, 1))))
         else .rp_tol(.rp_num(x, i, "cec_cmol"), 16, "max", .RP_TOL$cec, "CEC per kg of soil", "cec_cmol")
  if (isFALSE(cec$passed)) return(cec)
  ph <- .rp_between(.rp_num(x, i, "ph_h2o"), 3.5, 7, .RP_TOL$ph, "pH in water", "ph_h2o")
  if (isFALSE(ph$passed)) return(ph)
  ki <- .rp_ki(x, i)
  kr <- if (!is.na(ki)) .rp_r(ki < 2.2, note = sprintf("Ki %s", format(signif(ki, 3))))
        else .rp_stand_in(x, i, "ferralic", "Ki and the clay mineralogy")
  r <- .rp_all(colr, cl, cec, ph, kr, .rp_tropical(x))
  if (isTRUE(r$passed)) {
    hal <- .rp_num(x, i, "halloysite_pct")
    r$code <- if (isTRUE(hal >= 10 && hal <= 50)) "Fhy" else if (isTRUE(ox >= 30)) "Fox" else "F"
  }
  r
}

# OX (pp. 150-152, intertropical): oxides over 60 % of the clay; Ki under
# 0.5; CEC under 5 cmol+/kg of clay; pH in water 5.5-6.5 and not above the
# pH KCl. Without the oxides, ferralic() with the CEC of the clay and the
# delta pH stand in.
.rp_t_OX <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  ox <- .rp_num(x, i, "oxides_triacid_pct")
  cc <- .rp_cec_clay(x, i)
  if (isTRUE(cc >= 5)) return(.rp_r(FALSE, note = sprintf("CEC %s cmol+/kg of clay (5 or more)", format(round(cc, 1)))))
  phw <- .rp_num(x, i, "ph_h2o"); phk <- .rp_num(x, i, "ph_kcl")
  dph <- .rp_r(if (is.na(phw) || is.na(phk)) NA else phk >= phw, missing = c("ph_h2o", "ph_kcl"),
               note = if (!is.na(phw) && !is.na(phk)) sprintf("pH water %s, KCl %s", format(phw), format(phk)))
  if (isFALSE(dph$passed)) return(dph)
  o <- if (!is.na(ox)) .rp_r(ox > 60, note = sprintf("oxides %s %% of the clay", format(ox)))
       else {
         ki <- .rp_ki(x, i)
         if (!is.na(ki)) .rp_r(ki < 0.5, note = sprintf("Ki %s", format(signif(ki, 3))))
         else .rp_all(.rp_stand_in(x, i, "ferralic", "the oxides"), .rp_r(if (is.na(cc)) NA else TRUE, missing = c("cec_cmol", "clay_pct")))
       }
  ph <- .rp_between(phw, 5.5, 6.5, .RP_TOL$ph, "pH in water", "ph_h2o")
  .rp_all(o, dph, ph, .rp_tropical(x))
}

# RT (p. 152, intertropical): a reticulate pattern of contrasted colours
# (variegated_colors with two or more colours), or plinthite 15 % or more;
# plinthic() stands in for the pattern when only plinthite is recorded.
.rp_t_RT <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  vc <- .munsell_notations(.kidpr_v(x, i, "variegated_colors"))
  pl <- .rp_num(x, i, "plinthite_pct")
  r <- if (nrow(vc) >= 2L) .rp_r(TRUE, note = sprintf("variegated in %d colours", nrow(vc)))
       else if (isTRUE(pl >= 15)) .rp_stand_in(x, i, "plinthic", "the reticulate pattern")
       else .rp_r(FALSE)
  if (isFALSE(r$passed)) return(r)
  .rp_all(r, .rp_tropical(x))
}

# FE (pp. 170-171, intertropical): reddish (2.5YR to 7.5YR, value 4-7,
# chroma 6-8), organic carbon under 0.3 %, S/CEC under 50 %, sum of bases
# under 5 cmol+/kg, total Fe 12 % or less when recorded, not indurated,
# pH 6.0-6.5.
.rp_t_FE <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  col <- .rp_colour(x, i)
  colr <- .rp_r(.kidpr_and(.rp_hue_in(col, 12.5, 17.5),
                           if (is.na(col$value)) NA else col$value >= 4 && col$value <= 7,
                           if (is.na(col$chroma)) NA else col$chroma >= 6 && col$chroma <= 8),
                missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(colr$passed)) return(colr)
  oc <- .rp_tol(.rp_num(x, i, "oc_pct"), 0.3 - 1e-9, "max", 0, "organic carbon (%)", "oc_pct")
  if (isFALSE(oc$passed)) return(oc)
  bs <- .rp_bs_test(x, i, 50 - 1e-9, "max"); if (isFALSE(bs$passed)) return(bs)
  b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
  sb <- .rp_r(if (anyNA(b)) NA else sum(b) < 5, missing = "ca_cmol")
  ft <- .rp_num(x, i, "fe_total_pct")
  fe <- .rp_r(if (is.na(ft)) TRUE else ft <= 12, note = if (!is.na(ft)) sprintf("total Fe %s %%", format(ft)))
  hard <- .rp_r(!isTRUE(.rp_indurated(x, i)))
  ph <- .rp_between(.rp_num(x, i, "ph_h2o"), 6.0, 6.5, .RP_TOL$ph, "pH in water", "ph_h2o")
  .rp_all(colr, oc, bs, sb, fe, hard, ph, .rp_tropical(x))
}

# Sn (pp. 16-17, 150): clay 50-80 %; shiny pressure faces (pressure_faces
# common or many; not recorded, nitic_horizon() stands in); halloysite over
# 50 % of the clay, else Ki under 2.2 and Kr under 1.8 (map 3); clay films
# absent or few; CEC 8-30 cmol+/kg of soil.
.rp_t_Sn <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  cl <- .rp_between(.rp_clay(x, i), 50, 80, 0, "clay (%)", "clay_pct")
  if (isFALSE(cl$passed)) return(cl)
  cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
  if (isTRUE(cf >= 3L)) return(.rp_r(FALSE, note = "many clay coatings: not nitic"))
  pf <- .rp_txt(x, i, "pressure_faces")
  fa <- if (!is.na(pf)) .rp_r(pf %in% c("common", "many"), note = sprintf("pressure faces %s", pf))
        else .rp_stand_in(x, i, "nitic_horizon", "the shiny pressure faces")
  if (is.na(pf) && is.na(fa$passed)) fa$passed <- FALSE
  if (isFALSE(fa$passed)) return(fa)
  hal <- .rp_num(x, i, "halloysite_pct"); cm <- .rp_txt(x, i, "clay_mineralogy")
  ki <- .rp_ki(x, i)
  kr <- compute_kr(.rp_num(x, i, "sio2_sulfuric_pct"), .rp_num(x, i, "al2o3_sulfuric_pct"), .rp_num(x, i, "fe2o3_sulfuric_pct"))
  min <- if (!is.na(hal)) .rp_r(hal > 50, note = sprintf("halloysite %s %% of the clay", format(hal)))
         else if (!is.na(cm)) .rp_r(cm == "halloysite", note = sprintf("clay mineralogy %s", cm))
         else if (!is.na(ki) && !is.na(kr)) .rp_r(ki < 2.2 && kr < 1.8, tag = "ki_kr_for_halloysite",
                                                   note = sprintf("Ki %s, Kr %s (map 3)", format(signif(ki, 3)), format(signif(kr, 3))))
         else .rp_r(NA, missing = c("halloysite_pct", "clay_mineralogy", "sio2_sulfuric_pct"), note = "the clay mineralogy is not recorded")
  if (isFALSE(min$passed)) return(min)
  cec <- .rp_between(.rp_num(x, i, "cec_cmol"), 8, 30, .RP_TOL$cec, "CEC per kg of soil", "cec_cmol")
  .rp_all(cl, fa, min, cec)
}

# FS (pp. 179-180): matrix or ped faces 5YR or redder with chroma over 3.5;
# a well developed angular polyhedral structure; Fe free/Fe total over 0.50
# or FFE/Fe total over 0.20 when recorded (else the field route, map 2);
# kaolinite dominant excludes it (2:1 clays needed).
.rp_t_FS <- function(x, i) {
  if (!.rp_is_mineral(x, i) || .rp_is_first(x, i)) return(.rp_r(FALSE))
  col <- .rp_colour(x, i)
  colr <- .rp_r(.kidpr_and(.rp_hue_in(col, -10, 15), if (is.na(col$chroma)) NA else col$chroma > 3.5),
                missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(colr$passed)) return(colr)
  st <- .kidpr_structure(x, i); gr <- .field_grade_class(st$grade)
  stru <- .rp_r(if (is.na(st$type)) NA else grepl("angular|polyhed|block|cubic|prism", st$type) && !isTRUE(gr < 2L),
                missing = c("structure_type", "structure_grade"), note = if (!is.na(st$type)) sprintf("structure %s", st$type))
  if (isFALSE(stru$passed)) return(stru)
  cm <- .rp_txt(x, i, "clay_mineralogy")
  if (isTRUE(cm == "kaolinite")) return(.rp_r(FALSE, note = "kaolinite dominant: no 2:1 clays"))
  ft <- .rp_num(x, i, "fe_total_pct"); ffe <- .rp_num(x, i, "fe_ffe_pct"); fd <- .rp_num(x, i, "fe_dcb_pct")
  fe <- if (!is.na(ffe) && isTRUE(ft > 0)) .rp_r(ffe / ft > 0.2, note = sprintf("FFE/Fe total %s", format(signif(ffe / ft, 3))))
        else if (!is.na(fd) && isTRUE(ft > 0)) .rp_r(fd / ft > 0.5, note = sprintf("Fe free/Fe total %s", format(signif(fd / ft, 3))))
        else .rp_r(TRUE, note = "the iron release not recorded: the field route (map 2)")
  .rp_all(colr, stru, fe)
}

# The structural S horizons need a pedological structure.
.rp_s_gate <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  .rp_structured(x, i)
}

# Sp (p. 15): clay over 45 %, never calcareous, a prismatic or coarse
# polyhedral over-structure; undecided without the structure (map 4).
.rp_t_Sp <- function(x, i) {
  if (.rp_is_first(x, i)) return(.rp_r(FALSE))
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  cl <- .rp_clay(x, i)
  c1 <- .rp_r(if (is.na(cl)) NA else cl > 45, missing = "clay_pct", note = if (!is.na(cl)) sprintf("clay %s %%", format(cl)))
  if (isFALSE(c1$passed)) return(c1)
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  st <- .kidpr_structure(x, i)
  ov <- .rp_r(if (is.na(st$type) && is.na(st$secondary)) NA
              else isTRUE(grepl("prism", paste(st$type, st$secondary))) || isTRUE(grepl("coarse|very coarse", st$size) && grepl("block|polyhed|angular", st$type)),
              missing = "structure_type_secondary", note = "a prismatic or coarse polyhedral over-structure")
  .rp_all(g, c1, nc, ov)
}

# Sca (p. 15): generalised cold effervescence, CaCO3 2 % or more and less
# than the layer below when recorded; S/CEC over 95 % (a calcareous layer is
# read saturated when S/CEC is not recorded); organic carbon 1 % or less
# (more: Scah); hue 7.5YR to 2.5Y.
.rp_t_Sca <- function(x, i) {
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  ca <- .rp_calcareous(x, i); if (isFALSE(ca$passed)) return(ca)
  nx <- .rp_next(x, i)
  cc <- .rp_num(x, i, "caco3_pct"); cb <- if (is.na(nx)) NA else .rp_num(x, nx, "caco3_pct")
  less <- .rp_r(if (is.na(cc) || is.na(cb)) TRUE else cc < cb,
                note = if (!is.na(cc) && !is.na(cb)) sprintf("CaCO3 %s %% against %s %% below", format(cc), format(cb)))
  if (isFALSE(less$passed)) return(less)
  b <- .rp_bs(x, i)
  bs <- if (is.na(b$value)) .rp_r(TRUE, note = "a calcareous layer, read saturated") else .rp_bs_test(x, i, 95, "min")
  if (isFALSE(bs$passed)) return(bs)
  col <- .rp_colour(x, i)
  hue <- .rp_r(.rp_hue_in(col, 17.5, 22.5), missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  if (isFALSE(hue$passed)) return(hue)
  r <- .rp_all(g, ca, less, bs, hue)
  if (isTRUE(r$passed) && isTRUE(.rp_num(x, i, "oc_pct") > 1)) { r$features <- "h"; r$note <- c(r$note, "organic carbon over 1 %: Scah") }
  r
}

.rp_t_Sdo <- function(x, i) {
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  c <- .rp_carbonates(x, i)
  if (isFALSE(c$carbonated)) return(.rp_r(FALSE))
  if (isTRUE(c$cold) && isTRUE(grepl("strong", c$eff))) return(.rp_r(FALSE, note = "a strong cold effervescence: calcite"))
  .rp_all(g, .rp_dolomitic(x, i))
}

# Sci (p. 16): not carbonated, S/CEC over 80 %, Ca/Mg over 5 (2-5 the
# calcimagnesian variant, imperfect), pH over 6.5, hue 7.5YR to 2.5Y.
.rp_t_Sci <- function(x, i) {
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  bs <- .rp_bs_test(x, i, 80, "min"); if (isFALSE(bs$passed)) return(bs)
  cm <- .rp_camg(x, i)
  camg <- if (is.na(cm)) .rp_r(NA, missing = c("ca_cmol", "mg_cmol"))
          else if (cm > 5) .rp_r(TRUE, note = sprintf("Ca/Mg %s", format(signif(cm, 3))))
          else if (cm >= 2) .rp_r(TRUE, imperfect = TRUE, note = sprintf("Ca/Mg %s: the calcimagnesian variant, imperfect", format(signif(cm, 3))))
          else .rp_r(FALSE)
  if (isFALSE(camg$passed)) return(camg)
  ph <- .rp_tol(.rp_num(x, i, "ph_h2o"), 6.5 + 1e-9, "min", .RP_TOL$ph, "pH in water", "ph_h2o")
  if (isFALSE(ph$passed)) return(ph)
  col <- .rp_colour(x, i)
  hue <- .rp_r(.rp_hue_in(col, 17.5, 22.5), missing = col$missing, tag = col$tag, note = .rp_colour_text(col))
  .rp_all(g, nc, bs, camg, ph, hue)
}

.rp_t_Smg <- function(x, i) {
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  nc <- .rp_not_carbonated(x, i); if (isFALSE(nc$passed)) return(nc)
  bs <- .rp_bs_test(x, i, 80, "min"); if (isFALSE(bs$passed)) return(bs)
  cm <- .rp_camg(x, i)
  camg <- if (is.na(cm)) .rp_r(NA, missing = c("ca_cmol", "mg_cmol")) else .rp_r(cm < 2, note = sprintf("Ca/Mg %s", format(signif(cm, 3))))
  .rp_all(g, nc, bs, camg)
}

# Sal (p. 16): pH under 5.0; exchangeable Al 2 cmol+/kg or more (the lower
# end of "2 to 8"), 20 % or more of the CEC; Al/S over 2; S/CEC under 30 %.
.rp_t_Sal <- function(x, i) {
  g <- .rp_s_gate(x, i); if (isFALSE(g$passed)) return(g)
  ph <- .rp_tol(.rp_num(x, i, "ph_h2o"), 5.0 - 1e-9, "max", .RP_TOL$ph, "pH in water", "ph_h2o")
  if (isFALSE(ph$passed)) return(ph)
  al <- .rp_num(x, i, "al_kcl_cmol"); if (is.na(al)) al <- .rp_num(x, i, "al_cmol")
  a1 <- .rp_tol(al, 2, "min", 0, "exchangeable Al (cmol+/kg)", "al_cmol")
  if (isFALSE(a1$passed)) return(a1)
  cec <- .rp_num(x, i, "cec_cmol")
  a2 <- .rp_r(if (is.na(al) || !isTRUE(cec > 0)) NA else 100 * al / cec >= 20, missing = "cec_cmol")
  b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
  a3 <- .rp_r(if (is.na(al) || anyNA(b) || sum(b) <= 0) NA else al / sum(b) > 2, missing = "ca_cmol")
  bs <- .rp_bs_test(x, i, 30 - 1e-9, "max")
  .rp_all(g, ph, a1, a2, a3, bs)
}

# S haplique; a young Jp is no S (its weak structure follows the texture).
.rp_t_S <- function(x, i) {
  g <- .rp_s_gate(x, i)
  if (isTRUE(g$passed) && isTRUE(.rp_jp_raw(x, i)$passed))
    return(.rp_r(FALSE, note = "a young J horizon: its weak structure is not pedological"))
  g
}

# Jp (p. 25): under a Js, little organic matter, a weak structure or none.
.rp_t_Jp <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  .rp_jp_raw(x, i)
}

#' Jp on the data (p. 25): under a Js, or under an A or LA in a young deposit
#' (layer_origin fluvic, marine, aeolic, colluvic, tephric or solimovic: the
#' fluviosols, thalassosols, cryosols and arenosols of p. 25; the review's
#' D9); traces of carbon (under 0.3 %); a weak structure or none.
#' @noRd
.rp_jp_raw <- function(x, i) {
  .rp_memo(x, sprintf("jpraw_%d", i), function() {
    pv <- .rp_prev(x, i)
    if (is.na(pv) || !.rp_is_surface(x, pv) || .rp_is_surface(x, i)) return(.rp_r(FALSE))
    js <- isTRUE(.rp_t_Js(x, pv)$passed)
    lo <- .rp_txt(x, i, "layer_origin")
    young <- isTRUE(grepl("fluvi|marine|aeol|eolian|colluv|tephr|solimov|alluv", lo))
    if (!js && !young) return(.rp_r(FALSE))
    oc <- .rp_num(x, i, "oc_pct")
    if (isTRUE(oc >= 0.3)) return(.rp_r(FALSE, note = sprintf("organic carbon %s %%: more than traces", format(oc))))
    gr <- .rp_grade(x, i); st <- .rp_structured(x, i)
    .rp_r(if (isFALSE(st$passed)) TRUE else if (!is.na(gr)) gr <= 1 else NA,
          missing = c("structure_grade"),
          note = sprintf("under %s, a weak structure or none", if (js) "a Js" else sprintf("an A, in a %s deposit", lo)))
  })
}

# C (p. 25): no generalised pedological structure (massive, single grain,
# the rock's structure), or a recorded weathered rock (saprolite).
.rp_t_C <- function(x, i) {
  if (!.rp_is_mineral(x, i)) return(.rp_r(FALSE))
  st <- .rp_structured(x, i)
  if (!is.na(st$passed)) return(.rp_r(!st$passed, note = st$note))
  sp <- .rp_num(x, i, "saprolite_pct"); ws <- .rp_txt(x, i, "weathering_stage")
  if (isTRUE(sp > 0) || !is.na(ws)) return(.rp_r(TRUE, tag = "c_from_weathering", note = "a weathered rock recorded"))
  .rp_r(NA, missing = st$missing, note = st$note)
}


# --- the order of the tests ------------------------------------------------------------------

#' The fixed order of the tests (Q10): steps of codes, each with its test.
#' A test answers .rp_r(); its `code` field, when set, replaces the step's
#' name (Xgr for X, Gr for G, BTss for BT).
#' @noRd
.rp_steps <- function() {
  t <- function(code, f) list(code = code, f = f)
  list(
    layers = list(t("R", .rp_t_R), t("M", .rp_t_M), t("D", .rp_t_D), t("P", .rp_t_P),
                  t("TH", .rp_t_TH), t("Z", .rp_t_Z), t("Mli", .rp_t_Mli), t("Mt", .rp_t_Mt)),
    holorganic = list(t("H", .rp_t_H), t("O", .rp_t_O)),
    coarse = list(t("ND", .rp_t_ND), t("X", .rp_t_X)),
    hydromorphy = list(t("G", .rp_t_G)),
    accumulation = list(t("U", .rp_t_U), t("K", .rp_t_K), t("Si", .rp_t_Si), t("Fe", .rp_t_Fe),
                        t("Y", .rp_t_Y), t("OXm", .rp_t_OXh)),
    surface = list(t("An", .rp_t_An), t("Ach", .rp_t_Ach), t("Aso", .rp_t_Aso), t("Aca", .rp_t_Aca),
                   t("Ado", .rp_t_Ado), t("Aci", .rp_t_Aci), t("Amg", .rp_t_Amg), t("Alu", .rp_t_Alu),
                   t("And", .rp_t_And), t("Avi", .rp_t_Avi), t("Av", .rp_t_Av), t("Anod", .rp_t_Anod),
                   t("Ae", .rp_t_Ae), t("A", .rp_t_A), t("Js", .rp_t_Js)),
    subsurface = list(t("E", .rp_t_E), t("BP", .rp_t_BP), t("BT", .rp_t_BT), t("V", .rp_t_V),
                      t("SV", .rp_t_SV), t("Snd", .rp_t_Snd), t("Slu", .rp_t_Slu), t("F", .rp_t_F),
                      t("OX", .rp_t_OX), t("RT", .rp_t_RT), t("FE", .rp_t_FE), t("Sn", .rp_t_Sn),
                      t("FS", .rp_t_FS), t("Sp", .rp_t_Sp), t("Sca", .rp_t_Sca), t("Sdo", .rp_t_Sdo),
                      t("Sci", .rp_t_Sci), t("Smg", .rp_t_Smg), t("Sal", .rp_t_Sal), t("S", .rp_t_S),
                      t("Jp", .rp_t_Jp), t("C", .rp_t_C))
  )
}

# A test of the order on one layer, asked once and logged.
.rp_ask <- function(x, i, step, test) {
  key <- sprintf("t_%s_%s_%d", step, test$code, i)
  r <- x$cache[[key]]
  if (is.null(r)) {
    r <- tryCatch(suppressWarnings(test$f(x, i)), error = function(e)
      .rp_r(NA, note = sprintf("%s: %s", test$code, conditionMessage(e))))
    if (is.null(r$code) || is.na(r$code)) r$code <- if (isTRUE(r$passed)) test$code else r$code
    r$name <- test$code
    assign(key, r, envir = x$cache)
  }
  r
}


# --- the features (step 8) -----------------------------------------------------------------------

#' The features of a layer, independent of its code (pp. 3-5), each
#' three-valued; a feature is kept only on the hosts the book lists
#' (.rp_allowed_marks()).
#' @noRd
.rp_features <- function(x, i, code) {
  out <- list()
  add <- function(f, r) out[[f]] <<- r
  isG <- isTRUE(code %in% c("G", "Gr", "Go", "AG"))
  rx <- .rp_redox(x, i)
  if (!isG && !identical(code, "g"))
    add("g", .rp_r(rx$functional, missing = "redoximorphic_features_pct", note = rx$note, tag = rx$tag))
  oc <- .rp_num(x, i, "oc_pct")
  if (.rp_is_mineral(x, i) && !is.na(oc)) {
    w8 <- within_tolerance(oc, 8, "min", .RP_TOL$oc8); w30 <- within_tolerance(oc, 30, "max", .RP_TOL$oc30)
    add("ho", .rp_r(w8 != "fail" && w30 != "fail", imperfect = w8 == "imperfect" || w30 == "imperfect",
                    note = sprintf("organic carbon %s %%: hemiorganic", format(oc))))
  }
  if (!isTRUE(code %in% c("K", "Kc", "Km"))) {
    sc <- .rp_num(x, i, "secondary_carbonates_pct"); fm <- .rp_txt(x, i, "secondary_carbonates_form")
    add("k", .rp_r(isTRUE(sc > 0) || (!is.na(fm) && is.na(sc)), note = if (isTRUE(sc > 0) || !is.na(fm)) "secondary carbonates"))
  }
  cf <- .clay_films_class(.kidpr_v(x, i, "clay_films_amount"))
  add("t", .rp_r(isTRUE(cf >= 2L), note = if (isTRUE(cf >= 2L)) "clay films"))
  if (!isTRUE(code %in% c("V", "SV"))) {
    sl <- .rp_slickensides(x, i)
    add("v", .rp_r(isTRUE(sl >= 1L), note = if (isTRUE(sl >= 1L)) "slickensides: vertic signs"))
  }
  pt <- .rp_txt(x, i, "pan_type")
  add("x", .rp_r(isTRUE(grepl("fragipan", pt)) || "x" %in% x$des$hint_features[[i]], note = if (isTRUE(grepl("fragipan", pt))) "a fragipan"))
  if (!isTRUE(code %in% c("Y", "Ym", "Ys", "Yp"))) {
    gy <- .rp_num(x, i, "caso4_pct"); gv <- .rp_num(x, i, "gypsum_visible_pct")
    add("y", .rp_r(isTRUE(gy >= 5) || isTRUE(gv > 0), note = if (isTRUE(gy >= 5) || isTRUE(gv > 0)) "gypsum"))
  }
  df <- .rp_txt(x, i, "degradation_form"); tv <- .rp_num(x, i, "tongue_vol_pct"); tw <- .rp_num(x, i, "tongue_width_mm")
  d <- if (!is.na(df)) .rp_r(df %in% c("diffuse", "interdigitations", "tongues"), note = sprintf("degradation: %s", df))
       else if (!is.na(tv)) .rp_r(tv > 15 && !isTRUE(tw < 5), note = sprintf("tongues %s %% of the volume", format(tv)))
       else if (isTRUE(code %in% c("BT", "S"))) {
         w <- .kidpr_or(.kidpr_wrb_layer(x, "retic_properties", i), .kidpr_wrb_layer(x, "albeluvic_glossae", i))
         .rp_r(isTRUE(w), tag = if (isTRUE(w)) c("retic_properties", "albeluvic_glossae"),
               note = if (isTRUE(w)) "the degradation not recorded: soilKey's retic properties or albeluvic glossae stand in")
       } else .rp_r(FALSE)
  add("d", d)
  nod <- .rp_num(x, i, "ferric_nodules_pct") %||% NA
  mn <- .rp_num(x, i, "manganiferous_nodules_pct")
  # -cn on BT and FE, -j on S, Sci and BT: the hosts the book writes them on
  # (BTcn, FEcn; Sj, Scij, BTj; the review's D7)
  if (isTRUE(code %in% c("BT", "FE")))
    add("cn", .rp_r(isTRUE(sum(c(nod, mn), na.rm = TRUE) >= 15), note = if (isTRUE(sum(c(nod, mn), na.rm = TRUE) >= 15)) "many sesquioxide nodules"))
  col <- .rp_colour(x, i); st <- .kidpr_structure(x, i)
  if (isTRUE(code %in% c("S", "Sci", "BT"))) add("j", .rp_r(isTRUE(.rp_hue_in(col, 17.5, 22.5)) && isTRUE(col$chroma >= 6) &&
                   isTRUE(grepl("angular|polyhed", st$type) && !grepl("subangular", st$type)) &&
                   (isTRUE(mn > 0) || isTRUE(.rp_txt(x, i, "pressure_faces") %in% c("common", "many"))),
                 note = "bright yellow, angular, with Fe-Mn signs: xanthomorphic"))
  add("cry", .rp_r(isTRUE(.rp_txt(x, i, "cryoturbation") == "present"), note = "cryoturbation"))
  nw <- .rp_num(x, i, "nodules_wt_pct")
  if (!isTRUE(code %in% c("ND")))
    add("nod", .rp_r(isTRUE(nw > 30 && nw < 60), note = if (isTRUE(nw > 30 && nw < 60)) sprintf("nodules %s %% by weight", format(nw))))
  if (!isTRUE(code %in% c("Km", "Kc", "Sim", "Fem", "Femp", "OXm", "OXc", "Ym", "R")))
    add("c", .rp_r(isTRUE(.rp_indurated(x, i)), note = "indurated"))
  add("b", .rp_r("b" %in% x$des$hint_features[[i]], note = "buried, by its designation"))
  if (isTRUE(code %in% c("C", "M", "D", "R"))) {
    cb <- .rp_carbonates(x, i)
    add("ca", .rp_r(isTRUE(cb$carbonated), note = if (isTRUE(cb$carbonated)) "calcareous"))
  }
  if (isTRUE(code %in% c("G", "Gr", "Go"))) {
    ug <- .rp_num(x, i, "uncoated_grains_pct")
    add("a", .rp_r(isTRUE(ug >= 50) || isTRUE(col$value >= 7 && col$chroma <= 1), note = "bleached: albic"))
  }
  out
}


# --- one layer ------------------------------------------------------------------------------------

#' The order of tests on the data of one layer: the code, its variants, the
#' candidates, the features and overlays, the evidence. Memoised.
#' @noRd
.rp_data_layer <- function(x, i) {
  key <- sprintf("datalayer_%d", i)
  if (!is.null(x$cache[[key]])) return(x$cache[[key]])
  voc <- .rp_vocabulary()
  steps <- .rp_steps()
  ans <- list()
  code <- NA_character_; dec <- NULL; undecided <- FALSE; cands <- character(0); pending <- character(0)
  g_block <- FALSE; dec_step <- NA_character_; extra_notes <- character(0); extra_feats <- character(0)
  for (s in names(steps)) {
    rs <- lapply(steps[[s]], function(t) .rp_ask(x, i, s, t))
    ans[[s]] <- rs
    if (!is.na(code) || undecided) next
    v <- vapply(rs, function(r) r$passed, NA)
    lab <- vapply(seq_along(rs), function(k) rs[[k]]$code %||% steps[[s]][[k]]$code, "")
    lab[is.na(lab)] <- vapply(steps[[s]], `[[`, "", "code")[is.na(lab)]
    if (any(v %in% TRUE)) {
      k <- which(v %in% TRUE)[1L]
      nas <- which(is.na(v[seq_len(k - 1L)]))
      if (length(nas)) { undecided <- TRUE; cands <- c(lab[nas], lab[k]) }
      else if (g_block) { undecided <- TRUE; cands <- lab[k] }
      else { code <- lab[k]; dec <- rs[[k]]; dec_step <- s
             cands <- c(lab[-seq_len(k)][v[-seq_len(k)] %in% TRUE | is.na(v[-seq_len(k)])]) }
    } else if (any(is.na(v))) {
      nas <- which(is.na(v))
      if (length(nas) >= 2L) { undecided <- TRUE; cands <- lab[nas] }
      else pending <- c(pending, lab[nas])
      # G dominates every other character: an undecided G leaves the layer
      # undecided against the code read after it (soilKey's reading of p. 280)
      if (s == "hydromorphy") g_block <- TRUE
    }
  }
  sub_true <- function(names) {
    rs <- ans$subsurface
    k <- which(vapply(rs, function(r) isTRUE(r$passed) && r$name %in% names, NA))
    if (length(k)) rs[[k[1L]]] else NULL
  }
  # Fe only where no host of -cn holds, or with 40 % nodules or more: else the
  # host (BT, FE) with the feature cn (the review's D7)
  if (identical(code, "Fe") && !isTRUE(dec$value >= 40)) {
    h <- sub_true(c("BT", "FE"))
    if (!is.null(h)) {
      extra_notes <- c(dec$note, sprintf("the iron accumulation of a %s: %scn (the review's D7)", h$code, h$code))
      code <- h$code; dec <- h; dec_step <- "subsurface"; extra_feats <- "cn"
    }
  }
  # BT and FS both hold: FS with the clay illuviation -t, FSt (the review's D6)
  if (isTRUE(code %in% c("BT", "BT\u00df")) && !is.null(fs <- sub_true("FS"))) {
    extra_notes <- c(extra_notes, dec$note, "a fersiallitic horizon with clay illuviation: FSt (p. 180)")
    code <- "FS"; dec <- fs; extra_feats <- c(extra_feats, "t")
  }
  # a layer that continues the A run: when a subsurface code holds as well,
  # the layer is undecided between the two (the review's D2)
  if (identical(dec_step, "surface") && .rp_is_surface(x, i) && !.rp_is_first(x, i)) {
    rs <- ans$subsurface
    k <- which(vapply(rs, function(r) isTRUE(r$passed) && !r$name %in% c("C", "Jp"), NA))
    if (length(k)) {
      sc <- rs[[k[1L]]]$code %||% rs[[k[1L]]]$name
      undecided <- TRUE; cands <- c(code, sc, cands)
      extra_notes <- c(sprintf("the layer continues the A run and reads %s as well", sc))
      code <- NA_character_; dec <- NULL
    }
  }
  # the other codes that hold, in any step, are candidates too
  all_r <- unlist(ans, recursive = FALSE)
  holds <- unique(vapply(Filter(function(r) isTRUE(r$passed), all_r), function(r) r$code %||% r$name, ""))
  missing <- character(0); notes <- character(0); tags <- character(0); imperfect <- FALSE
  variants <- character(0); feats_code <- character(0)
  if (!is.na(code)) {
    notes <- c(extra_notes, dec$note); tags <- dec$tag; imperfect <- isTRUE(dec$imperfect)
    variants <- dec$variants %||% character(0); feats_code <- unique(c(dec$features %||% character(0), extra_feats))
    cands <- unique(c(pending, cands, setdiff(holds, code)))
  } else {
    cands <- unique(c(pending, cands))
    nr <- Filter(function(r) (r$code %||% r$name) %in% cands || r$name %in% cands, all_r)
    missing <- unique(unlist(lapply(nr, `[[`, "missing")))
    notes <- unique(c(extra_notes, unlist(lapply(nr, `[[`, "note"))))
    tags <- unique(unlist(lapply(nr, `[[`, "tag")))
  }
  passed <- if (!is.na(code)) TRUE else if (undecided || length(cands)) NA else FALSE
  # the designation of another notation narrows the candidates (Q9)
  hint_note <- character(0)
  hc <- x$hint_codes[[i]]; hf <- x$hint_fam[[i]]
  if (is.na(passed) && (length(hc) || length(hf))) {
    # a G candidate is never narrowed away: the FAO and CPCS g marks keep the
    # G family (the review's D10)
    keep <- cands[cands %in% hc | voc$info[cands, "family"] %in% hf | voc$info[cands, "family"] %in% "G"]
    if (length(keep) && length(keep) < length(cands)) {
      hint_note <- sprintf("the designation '%s' narrows the candidates %s to %s", .rp_des(x, i),
                           paste(cands, collapse = ", "), paste(keep, collapse = ", "))
      cands <- keep
    }
    if (length(cands) == 1L && !cands %in% c("H", "Hf", "Hm", "Hs", "Ha")) {
      code <- cands; passed <- TRUE; cands <- character(0); tags <- unique(c(tags, "designation_hint"))
      hint_note <- c(hint_note, sprintf("%s by the data and the designation", code))
      d1 <- Filter(function(r) identical(r$code %||% r$name, code) || identical(r$name, code), all_r)
      if (length(d1)) { variants <- d1[[1L]]$variants %||% character(0); feats_code <- d1[[1L]]$features %||% character(0) }
    }
  }
  # a loss on ignition that the carbon contradicts is flagged (the review's m3)
  og <- .rp_organic(x, i)
  if ("loi_oc_contradiction" %in% og$tag) { tags <- unique(c(tags, og$tag)); notes <- unique(c(notes, og$note)) }
  # features and overlays (step 8)
  fx <- .rp_features(x, i, code)
  if (!is.na(code) || !undecided) {
    # with no code, a functional redox feature makes the layer g
    if (is.na(code) && isFALSE(passed) && isTRUE(fx$g$passed)) {
      code <- "g"; passed <- TRUE; notes <- c(fx$g$note, "no other code: the redox horizon g"); fx$g <- NULL
    }
  }
  allowed <- .rp_allowed_marks(if (is.na(code) || !code %in% rownames(voc$info)) NA_character_ else code, voc)
  fp <- vapply(fx, function(r) r$passed, NA)
  feats <- unique(c(feats_code, names(fp)[fp %in% TRUE]))
  feats_na <- names(fp)[is.na(fp)]
  drop <- setdiff(feats, allowed)
  feats <- intersect(feats, allowed); feats_na <- intersect(feats_na, allowed)
  fnotes <- unlist(lapply(fx[names(fx) %in% feats], `[[`, "note"))
  ftags <- unlist(lapply(fx[names(fx) %in% c(feats, feats_na)], `[[`, "tag"))
  fmiss <- unlist(lapply(fx[names(fx) %in% feats_na], `[[`, "missing"))
  if (any(vapply(fx[names(fx) %in% feats], function(r) isTRUE(r$imperfect), NA))) imperfect <- TRUE
  pos <- .rp_positions(x)
  ov <- character(0); ov_na <- character(0); onotes <- character(0); otags <- character(0)
  pl <- .rp_ploughed(x, i, identical(pos$first, i))
  if (isTRUE(pl$passed) && !isTRUE(code %in% c("R", "M", "D", "P", "C", "L"))) { ov <- "L"; onotes <- c(onotes, pl$note); otags <- c(otags, pl$tag) }
  if (.rp_is_mineral(x, i) || isTRUE(code %in% c("H", "Hf", "Hm", "Hs"))) {
    sa <- .rp_salic(x, i); na <- .rp_sodic(x, i)
    if (isTRUE(sa$passed)) { ov <- c(ov, "Sa"); onotes <- c(onotes, sa$note) } else if (is.na(sa$passed)) ov_na <- c(ov_na, "Sa")
    if (isTRUE(na$passed)) { ov <- c(ov, "Na"); onotes <- c(onotes, na$note); otags <- c(otags, na$tag) } else if (is.na(na$passed)) ov_na <- c(ov_na, "Na")
    if (is.na(sa$passed)) fmiss <- c(fmiss, sa$missing)
    if (is.na(na$passed)) fmiss <- c(fmiss, na$missing)
  }
  if (isTRUE(code %in% c("Z", "Ztr", "Ztc", "ZO", "Zar")) && .rp_is_surface(x, i) && isTRUE(.rp_num(x, i, "oc_pct") >= 0.5)) {
    ov <- c(ov, "J"); onotes <- c(onotes, "a young horizon formed in the anthropic material (JZ)")
  }
  # a ploughed layer with no code of its own is an L
  if (is.na(code) && "L" %in% ov && !undecided) { code <- "L"; passed <- TRUE; ov <- setdiff(ov, "L") }
  all_tags <- unique(c(tags, ftags, otags))
  r <- list(code = code, passed = passed, variants = variants, candidates = if (isTRUE(passed)) setdiff(cands, code) else cands,
            features = feats, features_na = feats_na, overlay = ov, overlay_na = ov_na,
            imperfect = imperfect, missing = unique(c(missing, fmiss)),
            notes = unique(c(notes, fnotes, onotes)), hint_note = hint_note,
            dropped = drop, stand_ins = intersect(all_tags, .RP_STAND_INS),
            fallbacks = setdiff(all_tags, .RP_STAND_INS),
            answers = ans)
  r$lower_grade <- length(r$stand_ins) > 0L || any(r$fallbacks %in% .RP_LOWER_GRADE)
  assign(key, r, envir = x$cache)
  r
}

# The code of a layer written for display: the Roman numeral of a
# lithological discontinuity, the prefixes, the code, the features (II Scag,
# LAch, NaBTg).
.rp_compose <- function(code, overlay, features, lith = 1L) {
  if (is.na(code)) return(NA_character_)
  pre <- intersect(c("L", "Na", "Sa", "J", .RP_PI), overlay)
  s <- paste0(paste(pre, collapse = ""), code, paste(features, collapse = ""))
  if (isTRUE(lith > 1L)) paste(as.character(utils::as.roman(lith)), s) else s
}

.rp_evidence <- function(parts) {
  parts <- unique(parts[!is.na(parts) & nzchar(parts)])
  if (!length(parts)) NA_character_ else paste(parts, collapse = "; ")
}


# --- the profile --------------------------------------------------------------------------------

#' The reference horizons of a pedon by the Referentiel pedologique 2008
#'
#' The horizon identification of the namespace rp: every layer read as one of
#' the reference horizons, layers and materials of the RP. A layer written in
#' the RP notation keeps its code (route "notation"); the order of tests runs
#' on its data as well, and when it reaches the same code the route is
#' "both", otherwise its reading goes to the evidence. Any other layer is read
#' by the order of tests on the data (route "data"), the designation of
#' another notation narrowing the candidates.
#'
#' @param pedon A PedonRecord.
#' @param ... Not used.
#' @return The frame of identify_horizons(), one row per layer, with the RP
#'   columns `family` (the group of the code: A, S, BT, O ...), `composed`
#'   (the code written with its prefixes, features and Roman numeral),
#'   `variants` (the kinds the code may be refined to when the data do not
#'   say: H to Hf, Hm, Hs; O to OL, OF, OH; X to Xgr, Xc, Xp), `overlay_na`,
#'   `imperfect` (a threshold met only within the tolerance of p. 64),
#'   `data_code` (the code on the data), `stand_ins` (soilKey's WRB
#'   diagnostics that stood in for a field descriptor), `fallbacks` (the
#'   other substitutes: the volume for the weight, the moist colour from the
#'   dry one, a CEC by another method, the designation of another notation
#'   ...), `lower_grade` (a stand-in or a substitute of lower grade was
#'   used), `top_rp` and `bottom_rp` (depths from the top of the first layer
#'   that is not a litter OL, p. 64), `lith` (1, or the number of the
#'   lithological discontinuity) and `hint` (the RP codes the designation of
#'   another notation suggests).
#' @noRd
.rp_identify <- function(pedon, ...) {
  if (!inherits(pedon, "PedonRecord")) rlang::abort("'pedon' must be a PedonRecord.")
  fr <- .horizon_frame(pedon)
  n <- nrow(fr)
  chr <- rep(NA_character_, n); lst <- rep(list(character(0)), n)
  fr$family <- chr; fr$composed <- chr; fr$variants <- lst; fr$overlay_na <- lst
  fr$imperfect <- rep(FALSE, n); fr$data_code <- chr; fr$stand_ins <- lst; fr$fallbacks <- lst
  fr$lower_grade <- rep(FALSE, n); fr$top_rp <- rep(NA_real_, n); fr$bottom_rp <- rep(NA_real_, n)
  fr$lith <- rep(1L, n); fr$hint <- lst
  if (!n) return(fr)
  x <- .rp_profile(pedon)
  d <- x$des
  voc <- .rp_vocabulary()
  fam <- function(cd) if (is.na(cd) || !cd %in% rownames(voc$info)) NA_character_ else voc$info[cd, "family"]
  fr$top_rp <- x$rtop; fr$bottom_rp <- x$rbot
  fr$lith <- as.integer(d$lith)
  first_row <- x$ord[!x$litter[x$ord]][1L]
  for (i in x$ord) {
    k <- .rp_data_layer(x, i)
    fr$data_code[i] <- k$code
    fr$hint[[i]] <- x$hint_codes[[i]]
    dn <- if (identical(i, first_row)) x$datum_note else character(0)
    if (x$litter[i]) dn <- c(dn, "a litter OL: left out of the depths (p. 64)")
    if (x$rp && isTRUE(d$known[i])) {
      written <- d$code[i]
      trans <- isTRUE(d$transition[i]) || isTRUE(d$juxtaposed[i])
      fr$code[i] <- written
      fr$family[i] <- if (!is.na(d$family[i])) d$family[i] else fam(written)
      fr$overlay[[i]] <- d$overlay[[i]]
      fr$features[[i]] <- d$features[[i]]
      fr$transitional[i] <- trans
      fr$passed[i] <- if (trans || is.na(written)) FALSE else TRUE
      if (trans) fr$candidates[[i]] <- unique(stats::na.omit(vapply(d$parts[[i]], function(p) {
        r <- tryCatch(.rp_parse_one(p), error = function(e) NULL); if (is.null(r)) NA_character_ else r$code }, "")))
      fr$buried[i] <- if ("b" %in% d$features[[i]]) 1L else 0L
      fr$composed[i] <- if (!is.na(written)) .rp_compose(written, d$overlay[[i]], d$features[[i]], d$lith[i]) else d$written[i]
      comparable <- !is.na(written) && !trans
      agree <- comparable && identical(k$code, written)
      fr$route[i] <- if (agree) "both" else "notation"
      dis <- if (!comparable) NA_character_
             else if (agree) "the order of tests on the data reaches it too"
             else if (!is.na(k$code)) sprintf("the data read %s; the notation's %s is kept (default 7)", k$code, written)
             else if (length(k$candidates)) sprintf("the data leave it undecided (%s); the notation's %s is kept",
                                                  paste(k$candidates, collapse = ", "), written)
             else sprintf("the data read no reference horizon; the notation's %s is kept", written)
      fr$evidence[i] <- .rp_evidence(c(sprintf("the RP notation: %s", fr$designation[i]), dis, d$notes[[i]], dn))
    } else {
      fr$code[i] <- k$code
      fr$family[i] <- fam(k$code)
      fr$passed[i] <- k$passed
      fr$variants[[i]] <- k$variants
      fr$overlay[[i]] <- k$overlay
      fr$overlay_na[[i]] <- k$overlay_na
      fr$features[[i]] <- k$features
      fr$features_na[[i]] <- k$features_na
      fr$candidates[[i]] <- k$candidates
      fr$missing[[i]] <- k$missing
      fr$imperfect[i] <- isTRUE(k$imperfect)
      fr$stand_ins[[i]] <- k$stand_ins
      fr$fallbacks[[i]] <- k$fallbacks
      fr$lower_grade[i] <- isTRUE(k$lower_grade)
      fr$buried[i] <- if ("b" %in% k$features) 1L else 0L
      fr$composed[i] <- .rp_compose(k$code, k$overlay, k$features, fr$lith[i])
      fr$route[i] <- "data"
      head <- if (!is.na(k$code)) sprintf("[soilKey's order of tests] %s", k$code)
              else if (length(k$candidates)) sprintf("[soilKey's order of tests] undecided between %s", paste(k$candidates, collapse = ", "))
              else "[soilKey's order of tests] no reference horizon of the RP"
      hint_txt <- if (length(x$hint_codes[[i]]) && !length(k$hint_note))
        sprintf("the designation '%s' suggests %s (a hint, not read as an RP code)", fr$designation[i],
                paste(x$hint_codes[[i]], collapse = ", ")) else character(0)
      undec_txt <- if (!is.na(d$read_as_rp[i]) && is.na(d$rp %||% NA)) d$notes[[i]] else character(0)
      fr$evidence[i] <- .rp_evidence(c(head, k$notes, k$hint_note, hint_txt,
                                       if (length(k$stand_ins)) sprintf("soilKey's WRB stand-in, flagged: %s",
                                                                        paste0(k$stand_ins, "()", collapse = ", ")),
                                       if (k$imperfect) "a threshold met within the tolerance of p. 64 (imperfect)",
                                       undec_txt, dn))
    }
  }
  fr
}
