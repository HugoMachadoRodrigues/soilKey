# =============================================================================
# v0.9.371 -- the anthropic diagnostic horizons of the Chinese Soil Taxonomy
# (Cooperative Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil
# Taxonomy. Science Press, Beijing and New York, 2001; system id cst2001):
# the siltigic, cumulic, fimic and anthrostagnic epipedons (pp. 24-27) and
# the agric (with its phos-agric kind) and hydragric horizons (pp. 33-35).
# The machinery (context, runs, grades, stand-ins, the panel) is in
# R/diagnostics-cst-horizons.R.
#
# The book compares without numbers in places; Hugo's decision Q18
# (2026-11-04) gives the margins, recorded in thresholds.yaml: homogeneous =
# the same or neighbouring texture classes, colour within one unit of value
# and of chroma, CaCO3 within 20 g/kg; "higher" pH or base saturation = 0.5
# pH unit or 10 points; "much higher" Olsen P = twice.
# =============================================================================


# --- shared readings --------------------------------------------------------------

# Neighbouring USDA texture classes in the triangle (the siltigic and cumulic
# epipedons: neighbouring sub-horizons in classes that border on each other).
.CST_TEXTURE_NEIGHBOURS <- list(
  S = "LS", LS = c("S", "SL"), SL = c("LS", "L", "SCL", "SIL"),
  L = c("SL", "SIL", "CL", "SCL"), SIL = c("L", "SL", "SI", "SICL", "CL"), SI = "SIL",
  SCL = c("SL", "L", "CL", "SC"), CL = c("L", "SCL", "SICL", "C", "SIL", "SC"),
  SICL = c("SIL", "CL", "SIC"), SC = c("SCL", "CL", "C"), SIC = c("SICL", "C"),
  C = c("CL", "SC", "SIC"))

# Each layer homogeneous with the one above it (Q18): the same or a
# neighbouring texture class, the moist value or chroma within one unit,
# and, with `caco3`, CaCO3 within 20 g/kg.
.cst_homogeneous_pairs <- function(x, caco3 = TRUE) {
  t <- .cst_thr("horizons", "homogeneous")
  tex <- .cst_texture_class(x$h)
  col <- .cst_colours(x)
  up <- function(v) .cst_shift(v, -1L)
  tu <- up(tex)
  tex_ok <- ifelse(is.na(tex) | is.na(tu), NA,
                   tex == tu | mapply(function(a, b) b %in% .CST_TEXTURE_NEIGHBOURS[[a]], tex, tu))
  # value or chroma within one unit (Q18 as decided, Hugo, 2026-11-06)
  col_ok <- abs(col$vm - up(col$vm)) <= t$value_units | abs(col$c_any - up(col$c_any)) <= t$chroma_units
  out <- tex_ok & col_ok
  if (caco3) {
    cc <- .cst_v(x, "caco3_pct")
    out <- out & abs(cc - up(cc)) <= .cst_pct(t$caco3_gkg)
  }
  out
}

# Artefacts recorded in a layer (coal cinder, charcoal, bricks, tiles,
# porcelain): artefacts_pct and its kinds, or an anthropic material of
# artefacts. Not recorded: none.
.cst_artefacts <- function(x) {
  a <- cbind(.cst_v(x, "artefacts_pct"), .cst_v(x, "artefacts_industrial_pct"), .cst_v(x, "artefacts_urbic_pct"))
  am <- .cst_t(x, "anthropic_material")
  rec <- rowSums(!is.na(a)) > 0 | !is.na(am)
  has <- rowSums(a > 0, na.rm = TRUE) > 0 | (!is.na(am) & am %in% c("technological", "archaeological"))
  list(value = has, recorded = rec)
}

# The run of an anthropic epipedon from the mineral soil surface: the layers
# that continue the surface layer while `pair` holds (TRUE / FALSE / NA for
# each layer against the one above it), stopping at `stop`.
.cst_surface_run <- function(x, pair, stop = rep(FALSE, x$n)) {
  s <- x$first_min
  if (is.na(s)) return(NULL)
  p <- pair & !stop; p[s] <- !isTRUE(stop[s])
  r <- .cst_runs(x, p, start = s)
  lo <- which(!is.na(r$lo_id) & r$lo_id == r$lo_id[s])
  hi <- which(!is.na(r$hi_id) & r$hi_id == r$hi_id[s])
  list(s = s, lo = lo, hi = hi, T_lo = r$low[s], T_hi = r$high[s])
}

# Grade of a profile-level test on a run: full and evid for the layers of
# the run (sure layers when decided, the possible ones when not).
.cst_run_assign <- function(x, run, full_g, evid_g) {
  full <- rep(FALSE, x$n); evid <- rep(FALSE, x$n)
  if (is.null(run)) return(list(full = rep(NA, x$n), evid = rep(NA, x$n)))
  rows <- if (length(run$lo)) run$lo else run$hi
  full[if (is.na(full_g)) run$hi else rows] <- full_g
  evid[if (is.na(evid_g)) run$hi else rows] <- evid_g
  list(full = full, evid = evid)
}


# --- the siltigic and cumulic epipedons (pp. 24-25) ----------------------------------------------

# Where the run of a siltigic or cumulic epipedon stops (D6, Hugo,
# 2026-11-06): at a layer designated C or R; at an undesignated layer with
# under 3 g/kg organic carbon or without artefacts.
.cst_anthro_stop <- function(x) {
  oc <- .cst_v(x, "oc_pct")
  art <- .cst_artefacts(x)$value
  ifelse(x$kind %in% c("C", "R"), TRUE,
         ifelse(is.na(x$kind), (oc < .cst_pct(.cst_thr("horizons", "siltigic", "oc_base_min_gkg"))) %in% TRUE | !art,
                FALSE))
}

# Weighted organic carbon of the upper 50 cm of a run (its whole thickness
# when thinner), decreasing with depth and 3 g/kg or more at its base
# (siltigic only).
.cst_run_oc <- function(x, run, decreasing) {
  t <- .cst_thr("horizons", "siltigic")
  rows <- run$lo
  if (!length(rows)) return(NA)
  top <- x$msurf
  bot <- min(max(x$bot[rows]), top + t$oc_window_cm)
  w <- .cst_weighted(x$pedon, "oc_pct", top, bot)$value
  ok <- w >= .cst_pct(t$oc_mean_min_gkg)
  if (decreasing) {
    oc <- .cst_v(x, "oc_pct")[rows]
    dec <- if (length(oc) > 1L) all(diff(oc) <= 0) else TRUE
    ok <- .cst_and(ok, dec, oc[length(oc)] >= .cst_pct(t$oc_base_min_gkg))
  }
  ok
}

.cst_hz_siltigic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "siltigic")
  run <- .cst_surface_run(x, .cst_homogeneous_pairs(x, caco3 = TRUE), .cst_anthro_stop(x))
  if (is.null(run)) return(.cst_hz_ans(x, rep(NA, x$n), missing = c("designation", "oc_pct")))
  rows <- run$hi
  sv <- .cst_t(x, "silting_sieve_test"); mf <- .cst_t(x, "micromorph_features")
  sieve_l <- ifelse(!is.na(sv), sv == "positive", !is.na(mf) & grepl("layered_sorting", mf))
  sieve <- any(sieve_l[rows])
  art <- .cst_artefacts(x)
  arts <- any(art$value[rows])
  oc <- .cst_run_oc(x, run, decreasing = TRUE)
  items <- .cst_and(oc, sieve, arts)
  T_ok <- function(min) .cst_range_min(run$T_lo, run$T_hi, min)
  full_g <- .cst_and(T_ok(t$thick_min_cm), items)
  evid_g <- .cst_and(T_ok(t$evidence_thick_min_cm), items)
  a <- .cst_run_assign(x, run, full_g, evid_g)
  ans <- .cst_hz_ans(x, a$full, a$evid,
                     notes = c("the sieve test and the artefacts are observations: not recorded, absent",
                               "homogeneous: neighbouring texture classes, colour within 1 unit, CaCO3 within 20 g/kg (Q18)"),
                     missing = c(.cst_missing_in(x, rows, c("oc_pct", "munsell_value_moist", "munsell_chroma_moist",
                                                            "clay_pct", "caco3_pct"))))
  unrec <- !any(!is.na(sv[rows]) | !is.na(mf[rows])) || !any(art$recorded[rows])
  .cst_stand_in(x, ans, list(list(fn = "irragric")),
                eligible = seq_len(x$n) %in% rows & unrec & !(oc %in% FALSE))
}

.cst_hz_cumulic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "cumulic")
  run <- .cst_surface_run(x, .cst_homogeneous_pairs(x, caco3 = FALSE), .cst_anthro_stop(x))
  if (is.null(run)) return(.cst_hz_ans(x, rep(NA, x$n), missing = c("designation", "oc_pct")))
  rows <- run$hi
  mot <- as.character(.cst_col(x$h, "mottle_colors"))
  mud_l <- (!is.na(mot) & nzchar(trimws(mot))) | (.cst_v(x, "redoximorphic_features_pct") > 0) %in% TRUE |
    (.cst_l(x, "shelly_layer") %in% TRUE)
  earth_l <- (.cst_v(x, "foreign_fragments_pct") > 0) %in% TRUE |
    !is.na(.cst_t(x, "turbated_fragments"))
  mud <- any(mud_l[rows]); earth <- any(earth_l[rows])
  art <- .cst_artefacts(x)
  arts <- any(art$value[rows])
  oc <- .cst_run_oc(x, run, decreasing = FALSE)
  items <- .cst_and(oc, mud || earth, arts)
  T_ok <- function(min) .cst_range_min(run$T_lo, run$T_hi, min)
  full_g <- .cst_and(T_ok(t$thick_min_cm), items)
  evid_g <- .cst_and(T_ok(t$evidence_thick_min_cm), items)
  a <- .cst_run_assign(x, run, full_g, evid_g)
  kind <- rep(NA_character_, x$n)
  kind[rows] <- if (mud && earth) "mud-cumulic and earth-cumulic" else if (mud) "mud-cumulic"
                else if (earth) "earth-cumulic" else NA_character_
  ans <- .cst_hz_ans(x, a$full, a$evid, kind = kind,
                     notes = c("hydromorphic features, fragments of other horizons and artefacts are observations: not recorded, absent",
                               "the particle composition like that of the source soils is a statement of origin, not tested"),
                     missing = .cst_missing_in(x, rows, c("oc_pct", "munsell_value_moist", "munsell_chroma_moist", "clay_pct")))
  unrec <- !any(art$recorded[rows]) || !(mud || earth)
  .cst_stand_in(x, ans, list(list(fn = "terric")),
                eligible = seq_len(x$n) %in% rows & unrec & !(oc %in% FALSE))
}


# --- the fimic epipedon (pp. 25-26) ------------------------------------------------------

.cst_hz_fimic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "fimic")
  oc <- .cst_v(x, "oc_pct")
  # the epipedon: the A and AB layers from the mineral soil surface (without
  # designations, the layers with 4.5 g/kg of carbon or more; soilKey's
  # reading; Hugo, 2026-11-06)
  p <- ifelse(is.na(x$kind), oc >= .cst_pct(t$evidence_oc_min_gkg), x$kind %in% c("A", "AB"))
  run <- .cst_surface_run(x, p)
  if (is.null(run)) return(.cst_hz_ans(x, rep(NA, x$n), missing = c("designation", "oc_pct")))
  rows <- run$lo
  if (!length(rows)) rows <- run$hi
  top <- x$msurf; bot <- max(x$bot[rows])
  w_oc <- .cst_weighted(x$pedon, "oc_pct", top, bot)$value
  w_p <- .cst_weighted(x$pedon, "p_olsen_mg_kg", top, bot)$value
  worm <- any(.cst_v(x, "worm_holes_pct")[rows] >= t$worm_holes_min_pct, na.rm = TRUE)
  art <- .cst_artefacts(x)
  arts <- any(art$value[rows])
  # the three numbers: met, short (the evidence band) or failed
  band <- function(v, full, ev) ifelse(is.na(v), NA, ifelse(v >= full, "met", ifelse(v >= ev, "short", "fail")))
  T_band <- function(T) band(T, t$thick_min_cm, t$evidence_thick_min_cm)
  b <- c(T = T_band(run$T_lo), P = band(w_p, t$p_olsen_min_mg_kg, t$evidence_p_olsen_min_mg_kg),
         OC = band(w_oc, .cst_pct(t$oc_min_gkg), .cst_pct(t$evidence_oc_min_gkg)))
  if (run$T_hi != run$T_lo && !identical(T_band(run$T_hi), b[["T"]])) b[["T"]] <- NA
  other <- worm && arts
  full_g <- if (!other || any(b %in% c("short", "fail"))) FALSE else if (anyNA(b)) NA else TRUE
  short <- sum(b %in% "short")
  evid_g <- if (!other || any(b %in% "fail") || short > 1L) FALSE
            else if (anyNA(b)) (if (short + sum(is.na(b)) >= 1L && short <= 1L) NA else FALSE) else TRUE
  a <- .cst_run_assign(x, run, full_g, evid_g)
  ans <- .cst_hz_ans(x, a$full, a$evid,
                     notes = c("worm holes and artefacts are observations: not recorded, absent",
                               "Mehlich-3 P is not read for Olsen P (Q16)"),
                     missing = c(if (is.na(w_p)) "p_olsen_mg_kg", if (is.na(w_oc)) "oc_pct",
                                 if (anyNA(b[["T"]])) c("top_cm", "bottom_cm", "designation")))
  unrec <- all(is.na(.cst_v(x, "worm_holes_pct")[rows])) || !any(art$recorded[rows])
  .cst_stand_in(x, ans, list(list(fn = "hortic")),
                eligible = seq_len(x$n) %in% rows & (is.na(full_g) | (isFALSE(full_g) && unrec && !any(b %in% "fail"))))
}


# --- the anthrostagnic epipedon (pp. 26-27) ---------------------------------------------------

.cst_hz_anthrostagnic_epipedon <- function(x) {
  t <- .cst_thr("horizons", "anthrostagnic")
  s <- x$first_min
  cult <- .cst_cultivated(x)
  if (is.na(s)) return(.cst_hz_ans(x, rep(NA, x$n), missing = "designation",
                                    notes = "the cultivated layer and the plough pan are read from the designations"))
  if (isFALSE(cult[s])) return(.cst_hz_ans(x, rep(FALSE, x$n), notes = "no cultivated layer (Ap) at the surface"))
  # v0.9.373 (Hugo, 2026-11-06): a first mineral layer not known to be
  # cultivated (a plain A) is read with the core items first; one of them
  # failing rules the epipedon out, otherwise it stays undecided
  undecided_cult <- is.na(cult[s])
  run <- .cst_surface_run(x, if (undecided_cult) seq_len(x$n) == s else cult %in% TRUE)
  rows <- run$lo
  flags <- c("colour_not_submerged")
  # (2) the anthrostagnic regime; a recorded short paddy history gives the
  # evidence only
  reg <- moisture_regime_cst(x$pedon, "anthrostagnic")
  pyf <- .cst_site_num(x$pedon$site, "paddy_years_frac")
  short_hist <- !is.na(pyf) && pyf <= t$evidence_paddy_years_max
  # (3) puddled; not recorded, the paddy records, flagged
  pud <- .cst_l(x, "puddled")[s]
  puddled <- if (!is.na(pud)) pud else {
    pr <- .cst_paddy(x$pedon$site)$value
    if (!is.na(pr)) flags <- c(flags, "puddled_from_paddy_records")
    pr
  }
  # (4) the colour of the cultivated layer (the moist colour read for the
  # submerged one)
  col <- .cst_colours(x)
  colour <- col$vm[s] <= t$value_moist_max & col$c_any[s] <= t$chroma_moist_max & .cst_hue_ge(col, t$hue_rank_min)[s]
  # (5) rust mottles after draining
  mot <- as.character(.cst_col(x$h, "mottle_colors"))
  rf <- .cst_v(x, "redoximorphic_features_pct")
  mottles <- any((!is.na(mot[rows]) & nzchar(trimws(mot[rows]))) | (rf[rows] > 0) %in% TRUE)
  # (6) the plough pan: denser than the cultivated layer by 1.10 times
  bd <- .cst_v(x, "bulk_density_g_cm3")
  ratio <- if (length(rows) >= 2L) bd[max(rows)] / bd[s] else NA_real_
  pan <- if (length(rows) < 2L) FALSE else ratio >= t$bd_ratio_min
  T_ok <- .cst_range_min(run$T_lo, run$T_hi, t$thick_min_cm)
  core <- .cst_and(reg$passed, puddled, colour, mottles)
  # v0.9.376 (Hugo on the review, 2026-11-08, pp. 27, 54): with a short paddy
  # history recorded, the regime item of the evidence passes on the paddy
  # record whatever the saturation regime; the epipedon keeps the regime
  reg_ev <- if (short_hist && isTRUE(.cst_paddy(x$pedon$site)$value)) TRUE else reg$passed
  core_ev <- .cst_and(reg_ev, puddled, colour, mottles)
  if (undecided_cult) {
    if (isFALSE(core_ev)) return(.cst_hz_ans(x, rep(FALSE, x$n),
                                          notes = "the cultivated layer is not read from the designation, but an item of the core fails"))
    return(.cst_hz_ans(x, rep(NA, x$n), missing = "designation",
                       notes = "the cultivated layer and the plough pan are read from the designations"))
  }
  full_g <- .cst_and(core, T_ok, pan, !short_hist)
  evid_g <- core_ev
  a <- .cst_run_assign(x, run, full_g, evid_g)
  ans <- .cst_hz_ans(x, a$full, a$evid, flags = c(flags, reg$evidence$flags),
                     notes = c("rust mottles not recorded are absent", if (length(rows) < 2L) "no plough pan below the cultivated layer"),
                     missing = c(if (is.na(reg$passed)) reg$missing, if (is.na(puddled)) "puddled",
                                 if (is.na(colour)) c("munsell_value_moist", "munsell_chroma_moist", "munsell_hue_moist"),
                                 if (is.na(pan)) "bulk_density_g_cm3"))
  .cst_stand_in(x, ans, list(list(fn = "anthraquic")),
                eligible = seq_len(x$n) %in% run$hi & (is.na(a$full) | (a$full %in% FALSE & !mottles &
                  all(is.na(mot[rows]) & is.na(rf[rows])))))
}


# --- the agric and hydragric horizons (pp. 33-35) ------------------------------------------------

# Humus-clay films (the agric horizon item 2, and the evidence): their share
# of the faces (humus_clay_films_pct; not recorded, clay films common or many
# whose coating value is lower than the matrix, flagged) and thickness
# (humus_clay_films_mm; not recorded, undecided).
.cst_humus_films <- function(x) {
  pct <- .cst_v(x, "humus_clay_films_pct")
  am <- .cst_t(x, "clay_films_amount")
  cv <- .cst_v(x, "coatings_value_moist"); vm <- .cst_v(x, "munsell_value_moist")
  alt <- ifelse(!is.na(am) & !is.na(cv) & !is.na(vm) & cv < vm,
                ifelse(am %in% c("common", "many", "abundant"), 5, ifelse(am == "few", 3, 0)), NA_real_)
  flag <- ifelse(is.na(pct) & !is.na(alt), "humus_films_from_clay_films", NA_character_)
  pct <- ifelse(is.na(pct), alt, pct)
  list(pct = pct, mm = .cst_v(x, "humus_clay_films_mm"), flag = flag,
       recorded = !is.na(pct) | !is.na(.cst_v(x, "humus_clay_films_mm")))
}

.cst_hz_agric_horizon <- function(x) {
  t <- .cst_thr("horizons", "agric")
  cult <- .cst_cultivated(x)
  s <- x$first_min
  if (is.na(s))
    return(.cst_hz_ans(x, rep(NA, x$n), missing = "designation",
                       notes = "the cultivated layer is read from the designations"))
  last <- s
  if (isFALSE(cult[s])) return(.cst_hz_ans(x, rep(FALSE, x$n), notes = "no cultivated layer (Ap) at the surface"))
  # v0.9.373 (Hugo, 2026-11-06): a first mineral layer not known to be
  # cultivated is read as if it were; the horizon is then FALSE only where
  # the items fail, otherwise undecided
  undecided_cult <- is.na(cult[s])
  while (last < x$n && isTRUE(cult[last + 1L])) last <- last + 1L
  start <- last + 1L
  if (start > x$n) return(.cst_hz_ans(x, rep(FALSE, x$n), notes = "nothing described under the cultivated layer"))
  f <- .cst_humus_films(x)
  films <- ifelse(is.na(f$pct), ifelse(f$recorded, NA, FALSE), f$pct >= t$films_min_pct) & f$mm >= t$films_min_mm
  films[is.na(f$pct) & !f$recorded] <- FALSE
  col <- .cst_colours(x)
  below <- function(v) .cst_shift(v, 1L)
  # a neutral layer or a neutral layer below never meets the hue items (D11)
  chromic <- ifelse(col$neutral | below(col$neutral) %in% TRUE, FALSE,
                    below(col$rank) <= t$chromic_hue_rank_max & col$vm > below(col$vm) & col$c_any < below(col$c_any) &
                      (col$rank - below(col$rank)) >= 0 & (col$rank - below(col$rank)) <= t$hue_yellower_max_steps)
  ph <- .cst_v(x, "ph_h2o"); bs <- .cst_v(x, "bs_pct")
  acid <- below(ph) < t$acid_ph_max & ((ph - below(ph) >= t$ph_higher_units) | (bs - below(bs) >= t$bs_higher_points))
  po <- .cst_v(x, "p_olsen_mg_kg")
  phos <- po >= t$phos_p_olsen_min_mg_kg & po >= t$phos_much_higher_ratio * below(po)
  # an albic layer is no agric horizon (Hugo, 2026-11-06)
  alb <- .cst_hz(x, "albic_horizon")$full
  items <- (films | chromic | acid | phos) & !(alb %in% TRUE) & !(x$kind %in% c("C", "R", "O"))
  full <- .cst_run_grade(x, items, t$thick_min_cm, start = start)
  weak <- (f$pct >= t$evidence_films_min_pct) | (f$mm > 0)
  weak[is.na(weak) & !f$recorded] <- FALSE
  evid <- .cst_run_grade(x, items | (weak & !(alb %in% TRUE) & !(x$kind %in% c("C", "R", "O"))),
                         t$evidence_thick_min_cm, start = start)
  kind <- ifelse((full %in% TRUE | evid %in% TRUE) & phos %in% TRUE, "phos-agric", NA_character_)
  if (undecided_cult) {
    full <- ifelse(full %in% FALSE, FALSE, NA); evid <- ifelse(evid %in% FALSE, FALSE, NA)
    kind[] <- NA_character_
  }
  .cst_hz_ans(x, full, evid, kind = kind, flags = c(f$flag, col$hue_flag),
              notes = "chromic: the layer below 7.5YR or redder; acid: the layer below under pH 5.5; margins of Q18",
              missing = c(.cst_missing_in(x, which(is.na(full)), c("humus_clay_films_mm", "humus_clay_films_pct",
                                                                   "munsell_value_moist", "ph_h2o", "bs_pct", "p_olsen_mg_kg")),
                          if (undecided_cult && anyNA(full)) "designation"))
}

# The phos-agric horizon (p. 34, footnote): the agric horizon by its Olsen P
# (item 5).
.cst_hz_phos_agric_horizon <- function(x) {
  r <- .cst_hz(x, "agric_horizon")
  ph <- r$kind %in% "phos-agric"
  full <- ifelse(ph, r$full, ifelse(is.na(r$full), NA, FALSE))
  evid <- ifelse(ph, r$evid, ifelse(is.na(r$evid), NA, FALSE))
  r$full <- full; r$evid <- evid
  r
}

.cst_hz_hydragric_horizon <- function(x) {
  t <- .cst_thr("horizons", "hydragric")
  an <- .cst_hz(x, "anthrostagnic_epipedon")
  ep <- which(an$evid %in% TRUE)
  if (!length(ep)) {
    g <- if (all(an$evid %in% FALSE)) FALSE else NA
    return(.cst_hz_ans(x, rep(g, x$n), notes = "no anthrostagnic epipedon above", missing = if (is.na(g)) an$missing))
  }
  start <- max(ep) + 1L
  if (start > x$n) return(.cst_hz_ans(x, rep(FALSE, x$n), notes = "nothing described under the anthrostagnic epipedon"))
  col <- .cst_colours(x)
  mot <- as.character(.cst_col(x$h, "mottle_colors"))
  nod <- cbind(.cst_v(x, "redoximorphic_features_pct"), .cst_v(x, "ferric_nodules_pct"),
               .cst_v(x, "manganiferous_nodules_pct"), .cst_v(x, "fe_mn_nodules_pct"))
  redox <- (!is.na(mot) & nzchar(trimws(mot))) | rowSums(nod > 0, na.rm = TRUE) > 0
  grey <- .cst_hue_in(col, t$grey_hue_rank[1L], t$grey_hue_rank[2L], neutral = TRUE) &
    col$vm >= t$grey_value_moist[1L] & col$vm <= t$grey_value_moist[2L] & col$c_any <= t$grey_chroma_max
  i2 <- redox | (grey %in% TRUE)
  fe <- .cst_v(x, "fe_dcb_pct")
  i3 <- fe >= t$fe_dcb_ratio_min * fe[x$first_min]
  i23 <- i2 | i3
  # (4) grey humus-silt-clay films 0.5 mm; (5) prismatic or angular blocky
  # structure; not recorded, the WRB hydragric horizon stands in for them,
  # flagged (Hugo's reading 10)
  mm <- .cst_v(x, "humus_clay_films_mm")
  i4 <- mm >= t$films_min_mm
  ty <- .cst_t(x, "structure_type"); gr <- .cst_t(x, "structure_grade")
  i5 <- ifelse(is.na(ty), NA, grepl("prism|blocky|angular", ty) & (is.na(gr) | gr %in% c("moderate", "strong")))
  flags <- character(0)
  filled <- rep(FALSE, x$n)
  # items 4 and 5 not recorded: the WRB hydragric horizon fills them only on
  # the layers where it passes, flagged; elsewhere they stay undecided (D4)
  if (anyNA(i4[start:x$n]) || anyNA(i5[start:x$n])) {
    w <- tryCatch(hydragric(x$pedon), error = function(e) NULL)
    if (!is.null(w) && isTRUE(w$passed)) {
      wl <- seq_len(x$n) %in% match(w$layers, x$idx)
      filled <- wl & (is.na(i4) | is.na(i5))
      i4[wl & is.na(i4)] <- TRUE; i5[wl & is.na(i5)] <- TRUE
      if (any(filled)) flags <- c(flags, "stand_in:hydragric")
    }
  }
  p <- i23 & i4 & i5
  full <- .cst_run_grade(x, p, t$thick_min_cm, start = start)
  evid <- .cst_run_grade(x, p, t$evidence_thick_min_cm, start = start)
  si <- ifelse((full %in% TRUE | evid %in% TRUE) & filled, "hydragric", NA_character_)
  .cst_hz_ans(x, full, evid, flags = flags, stand_in = si,
              notes = "(1) and [(2) or (3)] and (4) and (5) (reading 10)",
              missing = .cst_missing_in(x, which(is.na(full)), c("humus_clay_films_mm", "structure_type", "fe_dcb_pct")))
}
