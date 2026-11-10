# =============================================================================
# v0.9.389 -- the key of the FAO-Unesco Legend of the Soil Map of the World
# (FAO-Unesco 1974, Vol. I Legend, Unesco Paris), pp. 43-53: the 26 major
# soil groups and their 106 soil units, registered as fao1974 (namespace
# fao74; classify_fao(edition = "1974") and its thin wrapper
# classify_fao1974(), in R/key-fao.R). The groups are in
# inst/rules/fao1974/key.yaml, the units in inst/rules/fao1974/units/ (one
# file per group, the Lithosols, Rankers and Rendzinas being single units).
#
# The gates read the diagnostic horizons (0.9.387) and properties (0.9.388)
# of R/diagnostics-fao74.R; the predicates below combine them where a gate
# reads more than one in one place (the H horizon of the Histosols, the clay
# and cracks of the Vertisols, "no diagnostic horizons other than", the
# material of the Andosols, the coarse texture and B features of the
# Arenosols, the calcareous material under the mollic A of the Rendzinas,
# the albic E over a slowly permeable horizon of the Planosols, the chroma
# of the mollic A to 15 cm, the 125 / 90 / 75 cm rule of the calcic and
# gypsic horizons, the tongues or nodules of the Podzoluvisols, the clay
# distribution of the Nitosols, the upper diagnostic B), and the unit gates
# that read a window, a B or a record the diagnostics do not read. No
# predicate of 1988 is redefined; the shared readers (.f88_*, .fao_*) run on
# the 1974 context (.f74_ctx()). The criteria are paraphrased; no text of
# the book is shipped. The numbers are those of
# inst/rules/fao1974/thresholds.yaml, the readings those of
# inst/rules/fao1974/readings.yaml.
#
# Conventions as for 1988 (R/key-fao.R): three values (a measurement not
# recorded leaves a criterion undecided, a field observation not recorded is
# absent); depths from the literal soil surface (Q9); the key read as printed,
# the definitions of pp. 32-41 not added (Q6); a diagnostic with no depth in
# the key read within 125 cm (Q10); of two diagnostic B horizons within 125
# cm the upper one decides at the key (74 p. 32, as decided for 1988); no
# residual group after the Cambisols (Q5).
# =============================================================================

# The 26 major soil groups in key order, with the key's page.
.F74_GROUPS <- data.frame(stringsAsFactors = FALSE,
  code = c("O", "I", "V", "J", "Z", "G", "T", "Q", "R", "U", "E", "P", "F", "W", "S", "M",
           "C", "K", "H", "D", "X", "Y", "N", "A", "L", "B"),
  name = c("Histosols", "Lithosols", "Vertisols", "Fluvisols", "Solonchaks", "Gleysols",
           "Andosols", "Arenosols", "Regosols", "Rankers", "Rendzinas", "Podzols",
           "Ferralsols", "Planosols", "Solonetz", "Greyzems", "Chernozems", "Kastanozems",
           "Phaeozems", "Podzoluvisols", "Xerosols", "Yermosols", "Nitosols", "Acrisols",
           "Luvisols", "Cambisols"),
  page = c("43", "43", "43", "44", "44", "44", "45", "45-46", "46", "46", "46", "47", "47",
           "48", "48", "48", "49", "49", "49", "50", "50", "50", "51", "51", "52", "52-53"))

# The groups that are single units (no unit list; the group is the unit).
.F74_SINGLE <- c("I", "U", "E")


# --- the result of a key-level predicate ---------------------------------------------

# As .f88_key_result(), for the 1974 key: `layers` in the context's depth
# order, mapped back to the rows of pedon$horizons.
.f74_key_result <- function(x, id, value, page, layers = integer(0), missing = character(0),
                            flags = character(0), notes = character(0), evidence = list()) {
  value <- if (length(value) == 1L) as.logical(value) else NA
  flags <- unique(as.character(flags[!is.na(flags) & nzchar(flags)]))
  if (x$n && all(is.na(x$top) | is.na(x$bot))) {
    value <- NA
    missing <- c("top_cm", "bottom_cm", missing)
  }
  layers <- sort(unique(as.integer(layers[!is.na(layers)])))
  DiagnosticResult$new(
    name = paste0(id, "_fao74"),
    passed = value,
    layers = x$idx[layers],
    evidence = c(evidence, list(edition = "1974", page = page, flags = flags,
                                stand_in = character(0), lower_grade = FALSE)),
    missing = if (is.na(value)) unique(as.character(missing)) else character(0),
    reference = sprintf("FAO-Unesco (1974), Soil Map of the World 1:5 000 000, Vol. I Legend, key, p. %s", page),
    notes = if (length(c(notes, flags))) paste(unique(c(notes, flags)), collapse = "; ") else NA_character_)
}

# The layers of a predicate's result that are rock or organic (H or O):
# not read where a gate reads the mineral soil.
.f74_ho <- function(x) !is.na(x$master) & grepl("^(H|O)", x$master)


# --- O Histosols (p. 43) --------------------------------------------------------------

# An H horizon 40 cm or more thick (60 cm when mainly sphagnum or moss,
# sphagnum_fibres_pct 75 or more, or with a bulk density below 0.1), from
# the surface down or cumulative within the upper 80 cm; thinner when it
# rests on rock, or on fragmental material (coarse_fragments_pct 90 or more,
# soilKey's number) with organic matter in its interstices: then every layer
# above it is H and more than 20 cm thick, the minimum of the histic H (the
# reading of 1988, Hugo 2026-11-09). The H horizon is the organic layer of 74
# p. 21, saturated (.f74_h_v()); a layer designated H whose organic matter
# leaves it undecided counts, flagged. A bulk density not measured is read as
# 0.1 or more, flagged.
histosols_gate_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "43"
  if (!x$n) return(.f74_key_result(x, "histosols_gate", FALSE, pg))
  org <- .f74_h_v(x)
  hd <- !is.na(x$master) & grepl("^H", x$master) & !x$rock
  use <- is.na(org) & hd
  org[use] <- TRUE
  sph <- .f88_num(x, "sphagnum_fibres_pct"); bd <- .f88_num(x, "bulk_density_g_cm3")
  light <- (!is.na(sph) & sph >= 75) | (!is.na(bd) & bd < 0.1)
  cf <- .f88_num(x, "coarse_fragments_pct")
  th <- pmax(x$bot - x$top, 0); th[is.na(th)] <- 0
  deepest <- suppressWarnings(max(x$bot, na.rm = TRUE)); if (!is.finite(deepest)) deepest <- 0
  on_rock <- any(x$rock)
  test <- function(o) {
    need <- if (any(light & o)) 60 else 40
    first <- which(!is.na(x$top))[1L]
    run <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, o))
    a <- length(run) > 0L && run[[1L]]$thickness >= need
    cum <- sum(pmax(pmin(x$bot, 80) - pmax(x$top, 0), 0)[o & !is.na(x$top) & !is.na(x$bot)])
    b <- cum >= need
    base <- which(x$rock | (!is.na(cf) & cf >= 90 & o))[1L]
    c <- !is.na(base) && base > 1L && isTRUE(x$top[base] > 0) && all(o[seq_len(base - 1L)]) &&
      sum(th[seq_len(base - 1L)]) > 20
    list(any = a || b || c, a = a, b = b, c = c, cum = cum, need = need)
  }
  sure <- test(org %in% TRUE)
  maybe <- test(!(org %in% FALSE))
  open <- !on_rock && deepest < 80 && !sure$any && (maybe$cum + (80 - deepest)) >= maybe$need
  value <- if (sure$any) TRUE else if (maybe$any || open) NA else FALSE
  lay <- which(org %in% TRUE)
  bd_flag <- isTRUE(value) && sure$need == 40 && !any(light[lay]) && any(is.na(bd[lay])) &&
    max(if (sure$a) sum(th[lay]) else 0, sure$cum) < 60
  .f74_key_result(x, "histosols_gate", value, pg,
                  layers = if (isTRUE(value)) lay else which(!(org %in% FALSE)),
                  missing = c("oc_pct", "clay_pct", "drainage_class", if (open) c("top_cm", "bottom_cm")),
                  flags = c(if (any(use[lay])) "h_horizon_from_designation", "om_from_oc_x1.724",
                            if (isTRUE(sure$c) && !sure$a && !sure$b) "h_horizon_on_rock_thinner",
                            if (bd_flag) "bulk_density_not_measured_read_0.1_or_more"),
                  evidence = list(h_cm_within_80 = sure$cum, thickness_needed_cm = sure$need,
                                  from_surface = sure$a, cumulative = sure$b, on_rock = sure$c))
}


# --- I Lithosols (p. 43) ----------------------------------------------------------------

# Continuous coherent hard rock (the R layer of 74 p. 22) whose top is within
# `max_top_cm` cm: the rock reader shared with 1988 (continuous_hard_rock_fao88():
# an R designation or bedrock not diggable; a cemented pan is not rock), the
# definition of the R layer being the same in both texts.
continuous_rock_fao74 <- function(pedon, max_top_cm = 10) {
  x <- .f74_ctx(pedon)
  pg <- "43"
  if (!x$n) return(.f74_key_result(x, "continuous_rock", FALSE, pg))
  r <- continuous_hard_rock_fao88(pedon, max_top_cm = max_top_cm)
  .f74_key_result(x, "continuous_rock", r$passed, pg, layers = .f88_ctx_rows(x, r),
                  missing = r$missing, flags = r$evidence$flags,
                  evidence = list(max_top_cm = max_top_cm))
}


# --- V Vertisols (p. 43) -----------------------------------------------------------------

# After mixing the upper 20 cm, 30 % clay or more in all horizons to 50 cm at
# least (a soil on rock above 50 cm fails); cracks 1 cm wide or more at a
# depth of 50 cm (cracks_width_cm with cracks_depth_cm reaching 50 cm, or
# recorded in a layer reaching 50 cm, as 1988), waived on an irrigated site,
# flagged; and gilgai (gilgai_fao74()), intersecting slickensides (any
# recorded, flagged) or wedge-shaped or parallelepiped aggregates in a layer
# between 25 and 100 cm. Cracks, slickensides, structure and gilgai are field
# observations: not recorded, absent.
vertisols_gate_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "43"
  if (!x$n) return(.f74_key_result(x, "vertisols_gate", FALSE, pg))
  clay <- .f88_num(x, "clay_pct")
  mix <- .fao_mixed(x$h, "clay_pct", 20)
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  shallow <- is.finite(rtop) && rtop < 50
  below <- .fao_in_window(x$h, .f88_cmp(clay, ">=", 30), 20, 50, "throughout")
  c1 <- if (shallow) FALSE else .fao_and(.f88_cmp(mix$value, ">=", 30), below$value)
  cw <- .f88_num(x, "cracks_width_cm"); cdp <- .f88_num(x, "cracks_depth_cm")
  rec <- !is.na(cw) | !is.na(cdp)
  wide <- !is.na(cw) & cw >= 1
  reach <- ifelse(!is.na(cdp), cdp >= 50, !is.na(x$bot) & x$bot >= 50)
  open <- (wide & is.na(cdp) & !reach) | (is.na(cw) & !is.na(cdp) & cdp >= 50)
  c2 <- if (any(wide & reach)) TRUE
        else if (!any(rec)) FALSE
        else if (any(open)) NA else FALSE
  irr <- isTRUE(.f88_site_lgl(x, "irrigated"))
  c2_flag <- character(0)
  if (!isTRUE(c2) && irr) { c2 <- TRUE; c2_flag <- "cracks_waived_irrigated" }
  if (!any(rec) && !irr) c2_flag <- "cracks_not_recorded_absent"
  sl <- .f88_values(slickensides_fao74(pedon), x$n)
  st <- .f88_chr(x, "structure_type")
  wedge <- !is.na(st) & grepl("wedge|parallelepiped|lenticular|cunei", st)
  win <- !is.na(x$top) & !is.na(x$bot) & x$top < 100 & x$bot > 25 & !x$rock
  gg <- gilgai_fao74(pedon)
  c3 <- isTRUE(gg$passed) || any((sl %in% TRUE | wedge) & win)
  value <- .fao_and(c1, c2, c3)
  .f74_key_result(x, "vertisols_gate", value, pg,
                  layers = which((sl %in% TRUE | wedge) & win),
                  missing = unique(c(mix$missing, below$missing, if (is.na(c2)) "cracks_depth_cm")),
                  flags = c(c2_flag, if (any(sl %in% TRUE & win)) "slickensides_read_as_intersecting",
                            gg$evidence$flags),
                  evidence = list(clay_mixed_20cm = mix$value, clay_30_to_50cm = c1, cracks = c2,
                                  gilgai = gg$passed, slickensides_wedges_or_gilgai = c3))
}


# --- "no diagnostic horizons other than ..." -----------------------------------------

# TRUE when none of the 13 diagnostic horizons of 1974 (pp. 23-27; the H
# horizon is a master horizon, not one of them) outside `allowed` is
# present, NA when one of them is undecided, FALSE when one is present; the
# horizon predicates leave out the horizons buried by 50 cm or more of new
# material (74 p. 32) and those starting below 125 cm. `allowed` holds
# horizon ids (.F74_HZ$id) and "a_horizon" (any A: mollic, umbric or ochric).
# "An H horizon" of the Fluvisols, Solonchaks and Gleysols gates is the
# histic H (an H horizon thick enough to be a Histosol has keyed out first).
only_horizons_fao74 <- function(pedon, allowed = character(0)) {
  x <- .f74_ctx(pedon)
  pg <- "32, 43-53"
  allowed <- as.character(unlist(allowed))
  if ("a_horizon" %in% allowed) allowed <- c(allowed, "mollic_a", "umbric_a", "ochric_a")
  others <- setdiff(setdiff(.F74_HZ$id, "h_horizon"), allowed)
  res <- lapply(others, function(id) get(paste0(id, "_fao74"), mode = "function")(pedon))
  names(res) <- others
  st <- vapply(res, function(r) { p <- r$passed; if (length(p) == 1L) as.logical(p) else NA }, NA)
  present <- names(st)[st %in% TRUE]
  undecided <- names(st)[is.na(st)]
  value <- if (length(present)) FALSE else if (length(undecided)) NA else TRUE
  lay <- unlist(lapply(res[present], function(r) .f88_ctx_rows(x, r)))
  .f74_key_result(x, "only_horizons", value, pg, layers = lay,
                  missing = unique(unlist(lapply(res[undecided], function(r) r$missing))),
                  notes = c(if (length(present)) sprintf("other diagnostic horizons present: %s", paste(present, collapse = ", ")),
                            if (length(undecided)) sprintf("undecided: %s", paste(undecided, collapse = ", "))),
                  evidence = list(allowed = allowed, present = present, undecided = undecided))
}


# --- T Andosols (p. 45) -----------------------------------------------------------------

# To 35 cm or more one or both of: (a) a bulk density below 0.85 with an
# exchange complex dominated by amorphous material (the per-layer reading of
# amorphous_complex_fao74(), which asks both, Q20); (b) 60 % or more vitric
# volcanic ash, cinders or other vitric pyroclastic material in the silt,
# sand and gravel (volcanic_glass_pct, else pyroclastic_fragments_pct, 60 or
# more). Every layer from the top of the mineral soil to `depth` cm below it
# (an O or H on top skipped, flagged), as the 35 cm of the Andosols of 1988
# (Hugo, 2026-11-09). As for the andic properties of 1988 (Hugo,
# 2026-11-09), volcanic glass not recorded is none and a layer left
# undecided is absent, flagged, unless the record names volcanic material
# (the parent material, a layer_origin, pyroclastic fragments).
andosol_material_fao74 <- function(pedon, depth = 35) {
  x <- .f74_ctx(pedon)
  pg <- "45"
  if (!x$n) return(.f74_key_result(x, "andosol_material", FALSE, pg))
  ho <- .f74_ho(x) | (.f74_organic(x)$value %in% TRUE)
  m <- which(!ho & !x$rock & !is.na(x$top))[1L]
  if (is.na(m)) return(.f74_key_result(x, "andosol_material", FALSE, pg, notes = "no mineral soil"))
  m0 <- x$top[m]
  a <- .f74_amorphous_v(x)
  gl <- .f88_num(x, "volcanic_glass_pct"); pf <- .f88_num(x, "pyroclastic_fragments_pct")
  volc_re <- "volcan|tephr|ash|pumic|cinder|scoria|lava|andesit|dacit|rhyolit|obsidian|tuff|ignimbrit|pyroclast|cinza"
  pm <- .f88_site(x, "parent_material")
  pm_volc <- !is.na(pm) && grepl(volc_re, .field_term(as.character(pm)))
  lo <- .f88_chr(x, "layer_origin")
  volc <- pm_volc | (!is.na(lo) & grepl(volc_re, lo)) | (!is.na(pf) & pf > 0)
  vit <- ifelse(!is.na(gl), gl >= 60, ifelse(!is.na(pf), pf >= 60, NA))
  vit[is.na(vit) & !volc] <- FALSE
  v <- .f88_or(a$value, vit)
  ab <- is.na(v) & !volc
  v[ab] <- FALSE
  v[x$rock] <- FALSE
  w <- .fao_in_window(x$h, v, m0, m0 + depth, "throughout", subset = !ho)
  .f74_key_result(x, "andosol_material", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) c(a$missing, "volcanic_glass_pct")),
                  flags = c(w$flags, .f88_flags(a$flags, w$layers %||% integer(0)),
                            if (any(ab)) "andosol_material_absent_no_volcanic_material",
                            if (m0 > 0) "andosol_depth_from_mineral_surface"),
                  evidence = list(mineral_top_cm = m0, depth = depth))
}


# --- Q Arenosols (pp. 45-46) -------------------------------------------------------------

# Coarse texture: every mineral layer (H and O not read) overlapping 0 to
# `to_cm` cm is in the coarse class of 74 p. 5 (clay below 18 % and sand
# above 65 %); a soil on rock above `to_cm` fails, as for 1988.
coarse_texture_fao74 <- function(pedon, to_cm = 100) {
  x <- .f74_ctx(pedon)
  pg <- "5, 45"
  if (!x$n) return(.f74_key_result(x, "coarse_texture", FALSE, pg))
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  if (is.finite(rtop) && rtop < to_cm)
    return(.f74_key_result(x, "coarse_texture", FALSE, pg, notes = sprintf("rock above %s cm", to_cm)))
  mineral <- !.f74_ho(x) & !(.f74_organic(x)$value %in% TRUE)
  cls <- .fao_texture_class(x$h, "1974")
  w <- .fao_in_window(x$h, ifelse(is.na(cls), NA, cls == "coarse"), 0, to_cm, "throughout",
                      subset = mineral)
  .f74_key_result(x, "coarse_texture", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) c("clay_pct", "sand_pct")),
                  flags = w$flags, notes = w$note, evidence = list(to_cm = to_cm))
}

# Lamellae of clay accumulation (Luvic Arenosols, within 125 cm, the
# definition, 74 p. 34): lamellae_cumulative_cm above 0, or a designation
# with lamellae (E&Bt, the lamellae suffix), flagged; not recorded, absent.
lamellae_fao74 <- function(pedon, max_top_cm = 125) {
  x <- .f74_ctx(pedon)
  pg <- "46"
  if (!x$n) return(.f74_key_result(x, "lamellae", FALSE, pg))
  lc <- .f88_num(x, "lamellae_cumulative_cm")
  ld <- !is.na(x$desg) & grepl("&B|lam", x$desg)
  ok <- !x$rock & !is.na(x$top) & x$top <= max_top_cm
  lam <- ok & ((!is.na(lc) & lc > 0) | ld)
  .f74_key_result(x, "lamellae", any(lam), pg, layers = which(lam),
                  flags = c(if (any(lam & ld & is.na(lc))) "lamellae_from_designation",
                            if (!any(lam) && all(is.na(lc))) "lamellae_not_recorded_absent"))
}

# The colouring or alteration of a cambic B right below the A (Cambic
# Arenosols, 74 p. 34): the layer right below the A (with no A at the
# surface, the first mineral layer) may hold a B (not C, not rock) and meets
# the alteration item of the cambic B of 1974 (more clay, a stronger chroma
# or a redder hue than the layer below, carbonates removed, or the colours
# of reduction), its texture not asked.
below_a_alteration_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "45-46"
  if (!x$n) return(.f74_key_result(x, "below_a_alteration", FALSE, pg))
  a <- .f88_a_run(x)
  k <- if (is.null(a)) which(!x$rock & !.f74_ho(x))[1L] else max(a$layers) + 1L
  if (is.na(k) || k > x$n || x$rock[k])
    return(.f74_key_result(x, "below_a_alteration", FALSE, pg, notes = "no layer below the A"))
  cr <- .f74_hz_cambic(x)$evidence$criteria
  alt <- if (is.data.frame(cr) && nrow(cr) == x$n) as.logical(cr$alteration[k]) else NA
  value <- .fao_and(.f88_b_layer(x)[k], alt)
  .f74_key_result(x, "below_a_alteration", value, pg, layers = k,
                  missing = if (is.na(value)) c("munsell_chroma_moist", "munsell_hue_moist", "clay_pct", "caco3_pct"),
                  evidence = list(layer_may_hold_b = .f88_b_layer(x)[k], alteration = alt))
}

# "Characteristics of argillic, cambic or oxic B horizons which do not
# qualify because of the textural requirements" (74 p. 45): the features the
# three units after the Albic Arenosols name, any of them: lamellae of clay
# accumulation within 125 cm (argillic), ferralic properties in the horizon
# right below the A (oxic), the colouring or alteration of a cambic B right
# below the A (cambic).
arenosol_b_features_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "45-46"
  if (!x$n) return(.f74_key_result(x, "arenosol_b_features", FALSE, pg))
  la <- lamellae_fao74(pedon)
  fe <- ferralic_properties_fao74(pedon, kind = "arenosol")
  al <- below_a_alteration_fao74(pedon)
  value <- .fao_or(la$passed, fe$passed, al$passed)
  .f74_key_result(x, "arenosol_b_features", value, pg,
                  layers = unlist(lapply(list(la, fe, al), function(r) if (isTRUE(r$passed)) .f88_ctx_rows(x, r))),
                  missing = unique(c(if (is.na(fe$passed)) fe$missing, if (is.na(al$passed)) al$missing)),
                  flags = c(la$evidence$flags, if (!isFALSE(fe$passed)) fe$evidence$flags),
                  evidence = list(lamellae = la$passed, ferralic = fe$passed, alteration = al$passed))
}


# --- U Rankers, B Cambisols: the thickness of the umbric A (p. 46, 52) ----------------

# The umbric A (umbric_a_fao74()) not more than `max` cm thick (the Rankers)
# or more than `gt` cm thick (the Cambisols without a cambic B): the
# thickness of its layers, from the top of the first to the base of the last.
umbric_thickness_fao74 <- function(pedon, max = NULL, gt = NULL) {
  x <- .f74_ctx(pedon)
  pg <- "46, 52"
  if (!x$n) return(.f74_key_result(x, "umbric_thickness", FALSE, pg))
  um <- umbric_a_fao74(pedon)
  L <- .f88_ctx_rows(x, um)
  if (isFALSE(um$passed) || !length(L))
    return(.f74_key_result(x, "umbric_thickness", if (is.na(um$passed)) NA else FALSE, pg,
                           missing = um$missing, notes = "no umbric A"))
  th <- max(x$bot[L]) - min(x$top[L])
  ok <- if (!is.null(max)) th <= max else th > gt
  value <- .fao_and(um$passed, ok)
  .f74_key_result(x, "umbric_thickness", value, pg, layers = L, missing = um$missing,
                  flags = um$evidence$flags,
                  evidence = list(thickness_cm = th, max = max, gt = gt))
}


# --- E Rendzinas (p. 46) ------------------------------------------------------------------

# A mollic A (mollic_a_fao74(); its waiver of the colour where it holds more
# than 40 % finely divided lime is the waiver the gate names) that contains,
# or lies directly on, calcareous material with a CaCO3 equivalent above 40
# %: a layer of the A or the layer right below it; a calcareous rock right
# below the A counts when its CaCO3 is not measured and limestone, chalk,
# marble or dolomite is recorded as its rock or parent material, flagged (the
# reading of the Rendzic Leptosols of 1988, Hugo 2026-11-09).
rendzinas_gate_fao74 <- function(pedon, cce_gt = 40) {
  x <- .f74_ctx(pedon)
  pg <- "46"
  if (!x$n) return(.f74_key_result(x, "rendzinas_gate", FALSE, pg))
  mo <- mollic_a_fao74(pedon)
  a <- .f88_a_run(x)
  if (isFALSE(mo$passed) || is.null(a))
    return(.f74_key_result(x, "rendzinas_gate", FALSE, pg, notes = "no mollic A"))
  k <- max(a$layers) + 1L
  zone <- c(a$layers, if (k <= x$n) k)
  cc <- .f88_caco3(x)
  cv <- .f88_cmp(cc$value, ">", cce_gt)
  rock_flag <- character(0)
  if (k <= x$n && x$rock[k] && is.na(cv[k])) {
    rt <- c(.f88_chr(x, "rock_type")[k], .f88_chr(x, "parent_material")[k],
            tolower(as.character(.f88_site(x, "parent_material"))), tolower(as.character(.f88_site(x, "rock_type"))))
    calc_rock <- any(!is.na(rt) & grepl("limestone|chalk|marble|dolomit|calcar|calc\u00e1r|marmore|m\u00e1rmore|giz", rt))
    cv[k] <- calc_rock
    if (calc_rock) rock_flag <- "calcareous_rock_from_record"
  }
  calc <- .f88_any(cv[zone])
  value <- .fao_and(mo$passed, calc)
  .f74_key_result(x, "rendzinas_gate", value, pg, layers = zone,
                  missing = c(if (is.na(calc)) "caco3_pct", if (is.na(mo$passed)) mo$missing),
                  flags = c(.f88_flags(cc$flags, zone), rock_flag, mo$evidence$flags),
                  evidence = list(mollic_a = mo$passed, calcareous_gt = calc, cce_gt = cce_gt))
}


# --- the upper diagnostic B (p. 32) -------------------------------------------------------

# Of the argillic, natric, cambic, spodic and oxic B of 1974 starting within
# 125 cm, the upper one decides (74 p. 32), read at the key as for 1988
# (Hugo, 2026-11-09): the gate of a group defined by one of them passes only
# when that B is the uppermost; FALSE when another B surely starts above it,
# NA when an undecided one does.
.F74_B_KINDS <- c("argillic_b", "natric_b", "cambic_b", "spodic_b", "oxic_b")
.f74_b_units <- function(x, kind) {
  r <- switch(kind,
              argillic_b = .f74_hz_argillic_natric(x, natric = FALSE),
              natric_b = .f74_hz_argillic_natric(x, natric = TRUE),
              cambic_b = .f74_hz_cambic(x),
              spodic_b = .f88_hz_spodic_b(x),
              oxic_b = .f74_hz_oxic(x))
  Filter(function(u) !is.na(u$top) && u$top <= 125, r$units)
}
upper_b_fao74 <- function(pedon, kind = .F74_B_KINDS) {
  kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  pg <- "32"
  if (!x$n) return(.f74_key_result(x, "upper_b", FALSE, pg))
  mine <- .f74_b_units(x, kind)
  if (!length(mine)) return(.f74_key_result(x, "upper_b", TRUE, pg, notes = sprintf("no %s", kind)))
  tops <- function(u, sure) vapply(Filter(function(z) identical(isTRUE(z$status), sure), u), function(z) z$top, 0)
  ts <- tops(mine, TRUE)
  t0 <- if (length(ts)) min(ts) else min(vapply(mine, function(z) z$top, 0))
  above_sure <- character(0); above_und <- character(0)
  for (k in setdiff(.F74_B_KINDS, kind)) {
    u <- .f74_b_units(x, k)
    if (any(tops(u, TRUE) < t0)) above_sure <- c(above_sure, k)
    else if (any(tops(u, FALSE) < t0)) above_und <- c(above_und, k)
  }
  value <- if (length(above_sure)) FALSE else if (length(above_und)) NA else TRUE
  .f74_key_result(x, "upper_b", value, pg,
                  missing = if (is.na(value)) "diagnostic_b_above",
                  notes = c(if (length(above_sure)) sprintf("%s lies above the %s and decides (p. 32)", paste(above_sure, collapse = ", "), kind),
                            if (length(above_und)) sprintf("undecided above the %s: %s", kind, paste(above_und, collapse = ", "))),
                  evidence = list(kind = kind, top_cm = t0, above = above_sure, above_undecided = above_und))
}

# The layers of the upper diagnostic B of a kind starting within `to_cm`,
# and a per-layer answer `fun(layers)` read on it: .f88_b_eval() on the B
# horizons of 1974 (the uppermost unit decides; when it is undecided and a
# sure B lies below it, the answer stands only where both agree; with no
# such B, the layers designated B, flagged).
.f74_b_eval <- function(x, kind = .F74_B_KINDS, to_cm = 125, fun) {
  kind <- match.arg(kind)
  clip <- function(L) L[!is.na(x$top[L]) & x$top[L] <= to_cm]
  u <- Filter(function(z) !is.na(z$top) && z$top <= to_cm, .f74_b_units(x, kind))
  if (!length(u)) {
    bd <- which(!is.na(x$master) & grepl("^B", x$master) & !x$rock & !is.na(x$top) & x$top <= to_cm)
    if (!length(bd)) return(list(value = FALSE, layers = integer(0), flags = character(0), none = TRUE))
    v <- fun(bd)
    return(list(value = if (isTRUE(v)) NA else v, layers = bd,
                flags = sprintf("b_designation_for_%s", kind), none = FALSE))
  }
  u <- u[order(vapply(u, function(z) z$top, 0))]
  up <- u[[1L]]
  L1 <- clip(up$layers)
  v1 <- fun(L1)
  if (isTRUE(up$status)) return(list(value = v1, layers = L1, flags = character(0), none = FALSE))
  sure <- Filter(function(z) isTRUE(z$status), u)
  if (!length(sure))
    return(list(value = if (isTRUE(v1)) NA else v1, layers = L1, flags = character(0), none = FALSE))
  L2 <- clip(sure[[1L]]$layers)
  v2 <- fun(L2)
  same <- identical(as.logical(v1), as.logical(v2))
  list(value = if (same) v2 else NA, layers = if (same) L2 else c(L1, L2),
       flags = if (!same) "undecided_b_above_decides", none = FALSE)
}


# --- W Planosols (p. 48) --------------------------------------------------------------------

# The slowly permeable horizon under an albic E (74 p. 48): the layer right
# below a run of albic E layers (albic_e_fao74()), its top within 125 cm,
# with an abrupt textural change at the contact (abrupt_textural_change_fao74(),
# the argillic or natric B the text names as its example: the change itself
# read, as for 1988), or a heavy clay (heavy_clay_fao74(): 60 % clay or more,
# soilKey's number, default 14, flagged), or a fragipan (suffix x) under an
# abrupt or clear boundary, flagged, or a measured ksat_mm_h below 4 under
# such a boundary, flagged (the 1988 reading). Returns the E runs and the
# slowly permeable layer of each, three-valued.
.f74_planosol <- function(x) {
  .f88_memo(x, "planosol74", function() {
    ae <- .f88_member(x, .f88_hz_albic_e(x)$units)
    ae[x$rock] <- FALSE
    ae[!is.na(x$top) & x$top > 125] <- FALSE
    ab <- .f74_abrupt_v(x)$value
    hc <- .f88_cmp(.f88_num(x, "clay_pct"), ">=", 60)
    sharp <- .fao_boundary_sharp(x$h)$value
    ks <- .f88_num(x, "ksat_mm_h")
    hy <- .f74_hydro_v(x)$value
    out <- list()
    for (r in .f88_run_list(x, !(ae %in% FALSE))) {
      k <- max(r$layers) + 1L
      if (k > x$n || x$rock[k] || is.na(x$top[k]) || x$top[k] > 125) next
      e_ok <- .f88_all(ae[r$layers])
      frag <- !is.na(x$desg[k]) && grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*x", x$desg[k]) && isTRUE(sharp[max(r$layers)])
      kslow <- !is.na(ks[k]) && ks[k] < 4 && isTRUE(sharp[max(r$layers)])
      slow <- .fao_or(ab[k], hc[k], frag, kslow)
      hyd <- .f88_any(hy[r$layers])
      out[[length(out) + 1L]] <- list(
        e = r$layers, k = k, e_ok = e_ok, slow = slow, hydro = hyd,
        status = .fao_and(e_ok, slow, hyd),
        flags = c(if (isTRUE(hc[k]) && !isTRUE(ab[k])) "heavy_clay_60pct_soilkey_number",
                  if (frag && !isTRUE(ab[k]) && !isTRUE(hc[k])) "fragipan_from_designation",
                  if (kslow && !isTRUE(ab[k]) && !isTRUE(hc[k]) && !frag) "ksat_below_4mm_h"))
    }
    out
  })
}

planosols_gate_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "48"
  if (!x$n) return(.f74_key_result(x, "planosols_gate", FALSE, pg))
  runs <- .f74_planosol(x)
  st <- vapply(runs, function(r) { s <- r$status; if (length(s) == 1L) as.logical(s) else NA }, NA)
  value <- if (any(st %in% TRUE)) TRUE else if (anyNA(st)) NA else FALSE
  pick <- if (isTRUE(value)) runs[[which(st %in% TRUE)[1L]]] else NULL
  lay <- if (!is.null(pick)) c(pick$e, pick$k) else unlist(lapply(runs[is.na(st)], function(r) c(r$e, r$k)))
  miss <- character(0)
  for (r in runs[is.na(st)]) {
    if (is.na(r$e_ok)) miss <- c(miss, "munsell_value_moist", "munsell_value_dry")
    if (is.na(r$slow)) miss <- c(miss, "clay_pct", "boundary_distinctness")
    if (is.na(r$hydro)) miss <- c(miss, .f74_hydro_v(x)$missing)
  }
  .f74_key_result(x, "planosols_gate", value, pg, layers = lay %||% integer(0),
                  missing = unique(miss), flags = if (!is.null(pick)) pick$flags,
                  evidence = list(slow_layer = if (!is.null(pick)) x$idx[pick$k] else NA_integer_))
}

# The slowly permeable horizon (the Solodic and Dystric Planosols, 74 p.
# 48): the layer of the Planosols gate below the E, with the adjoining
# layers below it of the same master horizon starting within `to_cm` (the
# reading of 1988, Hugo 2026-11-09). test "sodium": more than `gt` %
# exchangeable sodium in one of them (exchangeable_sodium_fao74()); test
# "bs": a base saturation (NH4OAc) below `lt` % in one of them.
slow_horizon_fao74 <- function(pedon, test = c("bs", "sodium"), lt = 50, gt = 6, to_cm = 125) {
  test <- match.arg(test)
  x <- .f74_ctx(pedon)
  pg <- "48"
  if (!x$n) return(.f74_key_result(x, "slow_horizon", FALSE, pg))
  g <- planosols_gate_fao74(pedon)
  runs <- .f74_planosol(x)
  st <- vapply(runs, function(r) { s <- r$status; if (length(s) == 1L) as.logical(s) else NA }, NA)
  cand <- runs[!(st %in% FALSE)]
  if (!length(cand))
    return(.f74_key_result(x, "slow_horizon", if (is.na(g$passed)) NA else FALSE, pg,
                           missing = g$missing, notes = "no slowly permeable horizon"))
  r <- if (any(st %in% TRUE)) runs[[which(st %in% TRUE)[1L]]] else cand[[1L]]
  k <- r$k
  m1 <- substr(x$master[k] %||% NA_character_, 1L, 1L)
  L <- k
  j <- k + 1L
  while (j <= x$n && !x$rock[j] && !is.na(m1) && !is.na(x$master[j]) &&
         substr(x$master[j], 1L, 1L) == m1 && !is.na(x$top[j]) && x$top[j] <= to_cm) {
    L <- c(L, j); j <- j + 1L
  }
  L <- L[!is.na(x$top[L]) & x$top[L] <= to_cm]
  if (test == "bs") {
    bs <- .f88_bs(x)
    v <- .f88_cmp(bs$value, "<", lt); fl <- .f88_flags(bs$flags, L); ms <- "bs_pct"
  } else {
    e <- .f88_esp(x)
    v <- .f88_cmpr(e$value, ">", gt); fl <- .f88_flags(e$flags, L); ms <- c("esp_pct", "na_cmol", "cec_cmol")
  }
  value <- .f88_any(v[L])
  if (isTRUE(value) && !isTRUE(g$passed)) value <- NA
  .f74_key_result(x, "slow_horizon", value, pg, layers = L,
                  missing = if (is.na(value)) c(ms, g$missing), flags = fl,
                  evidence = list(test = test, lt = lt, gt = gt))
}


# --- M, C, K: the chroma of the mollic A to 15 cm (pp. 48-49) -----------------------------

# A mollic A (mollic_a_fao74()) whose moist chroma is `max` or less in every
# layer (or more than `gt` in any layer) from its top to `depth` cm below it:
# the Greyzems, Chernozems and Kastanozems. The Kastanozems are the exact
# complement of the Chernozems, as Hugo decided for the same words of 1988
# (2026-11-09).
mollic_chroma_fao74 <- function(pedon, max = NULL, gt = NULL, depth = 15) {
  x <- .f74_ctx(pedon)
  pg <- "48-49"
  if (!x$n) return(.f74_key_result(x, "mollic_chroma", FALSE, pg))
  mo <- mollic_a_fao74(pedon)
  a <- .f88_a_run(x)
  if (isFALSE(mo$passed) || is.null(a))
    return(.f74_key_result(x, "mollic_chroma", FALSE, pg, notes = "no mollic A"))
  t0 <- x$top[a$first]
  cm <- .f88_colours(x)$cm
  ok <- if (!is.null(max)) .f88_cmp(cm, "<=", max) else .f88_cmp(cm, ">", gt)
  sel <- !x$rock & !is.na(x$top) & !is.na(x$bot) & x$bot > t0 & x$top < t0 + depth
  v <- ok[sel]
  ch <- if (!is.null(max)) {
    if (any(v %in% FALSE)) FALSE else if (length(v) && all(v %in% TRUE)) TRUE else NA
  } else {
    if (any(v %in% TRUE)) TRUE else if (length(v) && all(v %in% FALSE)) FALSE else NA
  }
  value <- .fao_and(mo$passed, ch)
  .f74_key_result(x, "mollic_chroma", value, pg, layers = which(sel),
                  missing = c(if (is.na(ch)) "munsell_chroma_moist", if (is.na(mo$passed)) mo$missing),
                  flags = mo$evidence$flags,
                  evidence = list(mollic_a = mo$passed, chroma = ch, max = max, gt = gt, depth = depth))
}


# --- the 125 / 90 / 75 cm rule (pp. 49-53) ---------------------------------------------------

# The depth of the rule by the texture class of the upper 30 cm (Q10). A
# function `f(depth)` returning TRUE / FALSE / NA is read at that depth; when
# the class is undecided, a pass at 75 cm passes, a failure at 125 cm fails,
# and anything else is undecided (the reading of high salinity, Hugo
# 2026-11-09).
.f74_by_texture <- function(x, f) {
  d <- .fao_depth_by_texture(x$h)
  if (!is.na(d$value)) return(list(value = f(d$value), depth = d$value, class = d$class, missing = character(0)))
  a75 <- f(75); a125 <- f(125)
  v <- if (isTRUE(a75)) TRUE else if (isFALSE(a125)) FALSE else NA
  list(value = v, depth = NA_real_, class = NA_character_,
       missing = if (is.na(v)) c("clay_pct", "sand_pct", d$missing) else character(0))
}

# One or more of `kinds` ("calcic", "gypsic", "soft_powdery_lime") with its
# upper limit within 125, 90 or 75 cm for a coarse, medium or fine weighted
# texture (the Chernozems, Kastanozems, Calcic Luvisols and Calcic
# Cambisols).
lime_by_texture_fao74 <- function(pedon, kinds = c("calcic", "gypsic", "soft_powdery_lime")) {
  kinds <- as.character(unlist(kinds))
  x <- .f74_ctx(pedon)
  pg <- "49-53"
  if (!x$n) return(.f74_key_result(x, "lime_by_texture", FALSE, pg))
  res <- list()
  f <- function(d) {
    rs <- lapply(kinds, function(k) switch(k,
      calcic = calcic_fao74(pedon, max_top_cm = d),
      gypsic = gypsic_fao74(pedon, max_top_cm = d),
      soft_powdery_lime = soft_powdery_lime_fao74(pedon, max_top_cm = d)))
    res[[as.character(d)]] <<- rs
    do.call(.fao_or, lapply(rs, function(r) r$passed))
  }
  b <- .f74_by_texture(x, f)
  rs <- res[[as.character(if (is.na(b$depth)) 125 else b$depth)]] %||% list()
  lay <- unlist(lapply(rs, function(r) if (isTRUE(r$passed)) .f88_ctx_rows(x, r)))
  .f74_key_result(x, "lime_by_texture", b$value, pg, layers = lay %||% integer(0),
                  missing = unique(c(b$missing, unlist(lapply(rs, function(r) if (is.na(r$passed)) r$missing)))),
                  flags = unique(unlist(lapply(rs, function(r) if (!isFALSE(r$passed)) r$evidence$flags))),
                  evidence = list(kinds = kinds, depth_cm = b$depth, texture_class = b$class))
}

# The Gypsic and Calcic Xerosols and Yermosols (key p. 50 and its footnote
# 1; Q26): a gypsic (or calcic) horizon within 125, 90 or 75 cm by texture;
# where both are present the one nearer the surface decides (the same top:
# the key's order, gypsic first).
aridic_lime_fao74 <- function(pedon, kind = c("gypsic", "calcic")) {
  kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  pg <- "50"
  if (!x$n) return(.f74_key_result(x, "aridic_lime", FALSE, pg))
  top_of <- function(r) { L <- .f88_ctx_rows(x, r); if (length(L)) min(x$top[L]) else NA_real_ }
  keep <- list()
  f <- function(d) {
    gy <- gypsic_fao74(pedon, max_top_cm = d); ca <- calcic_fao74(pedon, max_top_cm = d)
    keep[[as.character(d)]] <<- list(gy = gy, ca = ca)
    me <- if (kind == "gypsic") gy else ca
    ot <- if (kind == "gypsic") ca else gy
    if (isFALSE(me$passed)) return(FALSE)
    if (isFALSE(ot$passed)) return(me$passed)
    if (isTRUE(me$passed) && isTRUE(ot$passed)) {
      tm <- top_of(me); to <- top_of(ot)
      if (is.na(tm) || is.na(to)) return(NA)
      return(if (kind == "gypsic") tm <= to else tm < to)
    }
    NA
  }
  b <- .f74_by_texture(x, f)
  k <- keep[[as.character(if (is.na(b$depth)) 125 else b$depth)]]
  me <- if (kind == "gypsic") k$gy else k$ca
  .f74_key_result(x, "aridic_lime", b$value, pg,
                  layers = if (isTRUE(b$value)) .f88_ctx_rows(x, me) else integer(0),
                  missing = unique(c(b$missing, if (is.na(b$value)) c(k$gy$missing, k$ca$missing))),
                  flags = if (!isFALSE(b$value)) me$evidence$flags,
                  evidence = list(kind = kind, depth_cm = b$depth, texture_class = b$class,
                                  gypsic = k$gy$passed, calcic = k$ca$passed))
}


# --- D Podzoluvisols (p. 50) ------------------------------------------------------------------

# An argillic B whose upper boundary is irregular or broken from deep
# tonguing of the E into it (tonguing_fao74(kind = "albic")) or from discrete
# nodules of 2 cm or more, up to 30 cm, in diameter (nodule_size_max_cm) in
# its upper layer or the layer above, their enriched, redder exteriors taken
# as met, flagged (the reading of 1988).
podzoluvisols_gate_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "50"
  if (!x$n) return(.f74_key_result(x, "podzoluvisols_gate", FALSE, pg))
  ar <- argillic_b_fao74(pedon)
  tg <- tonguing_fao74(pedon, kind = "albic")
  ns <- .f88_num(x, "nodule_size_max_cm")
  L <- .f88_ctx_rows(x, ar)
  nod <- FALSE
  if (length(L)) {
    i <- min(L)
    zone <- c(if (i > 1L) i - 1L, i)
    nod <- any(!is.na(ns[zone]) & ns[zone] >= 2 & ns[zone] <= 30)
  }
  value <- .fao_and(ar$passed, .fao_or(tg$passed, nod))
  .f74_key_result(x, "podzoluvisols_gate", value, pg, layers = L,
                  missing = unique(c(ar$missing, tg$missing)),
                  flags = c(tg$evidence$flags, if (nod) "nodule_exteriors_taken_as_met"),
                  evidence = list(argillic_b = ar$passed, tonguing = tg$passed, nodules_2_to_30cm = nod))
}


# --- N Nitosols (p. 51) -----------------------------------------------------------------------

# An argillic B whose clay does not fall from its maximum by as much as 20 %
# within 150 cm (relative, default 13: no layer below the maximum and within
# 150 cm under 0.8 x the maximum; the layers from the top of the argillic B
# read, as 1988). The plinthite, vertic and ferric exclusions are in the key
# file.
nitosols_gate_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "51"
  if (!x$n) return(.f74_key_result(x, "nitosols_gate", FALSE, pg))
  ar <- argillic_b_fao74(pedon)
  L <- .f88_ctx_rows(x, ar)
  if (isFALSE(ar$passed) || !length(L))
    return(.f74_key_result(x, "nitosols_gate", if (is.na(ar$passed)) NA else FALSE, pg,
                           missing = ar$missing, notes = "no argillic B"))
  clay <- .f88_num(x, "clay_pct")
  w <- !x$rock & !is.na(x$top) & x$top < 150 & seq_len(x$n) >= min(L)
  cw <- clay[w]
  dec <- if (anyNA(cw) || !length(cw)) NA else {
    k <- which.max(cw)
    !any(cw[k:length(cw)] < 0.8 * cw[k])
  }
  rtop <- suppressWarnings(min(x$top[x$rock], na.rm = TRUE))
  deepest <- suppressWarnings(max(x$bot[!x$rock], na.rm = TRUE))
  open <- !(is.finite(rtop) && rtop <= 150) && is.finite(deepest) && deepest < 150
  if (isTRUE(dec) && open) dec <- NA
  value <- .fao_and(ar$passed, dec)
  .f74_key_result(x, "nitosols_gate", value, pg, layers = L,
                  missing = unique(c(ar$missing, if (is.na(dec)) c("clay_pct", "bottom_cm"))),
                  flags = c(ar$evidence$flags, if (open) "description_ends_above_150cm"),
                  evidence = list(argillic_b = ar$passed, clay_no_decrease_20pct = dec))
}


# --- unit gates: windows and B horizons ---------------------------------------------------------

# pH (H2O 1:5; ph_h2o_1to5, else ph_h2o, flagged, Q12) below `lt` over a
# window: "some" layer (the Dystric Histosols, "at least in some part of the
# soil between 20 and 50 cm") or "throughout".
ph_window_fao74 <- function(pedon, lt = 5.5, top = 20, bottom = 50, mode = c("some", "throughout")) {
  mode <- match.arg(mode)
  x <- .f74_ctx(pedon)
  pg <- "43"
  if (!x$n) return(.f74_key_result(x, "ph_window", FALSE, pg))
  ph <- .fao_ph(x$h, "1to5")
  w <- .fao_in_window(x$h, .f88_cmp(ph$value, "<", lt), top, bottom, mode)
  sel <- w$layers %||% integer(0)
  .f74_key_result(x, "ph_window", w$value, pg, layers = sel,
                  missing = c(w$missing, if (is.na(w$value)) ph$missing),
                  flags = c(w$flags, .f88_flags(ph$flags, sel)), notes = w$note,
                  evidence = list(lt = lt, window = c(top, bottom), mode = mode))
}

# A moist chroma below `lt` dominant in the soil matrix throughout `top`-
# `bottom` cm (the Pellic Vertisols: below 1.5 in the upper 30 cm); the
# matrix colour of each layer is its recorded moist colour.
chroma_window_fao74 <- function(pedon, lt = 1.5, top = 0, bottom = 30) {
  x <- .f74_ctx(pedon)
  pg <- "43"
  if (!x$n) return(.f74_key_result(x, "chroma_window", FALSE, pg))
  cm <- .f88_colours(x)$cm
  w <- .fao_in_window(x$h, .f88_cmp(cm, "<", lt), top, bottom, "throughout")
  .f74_key_result(x, "chroma_window", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) "munsell_chroma_moist"),
                  flags = w$flags, notes = w$note,
                  evidence = list(lt = lt, window = c(top, bottom)))
}

# Base saturation (NH4OAc) below `lt` % over a window: in "some" layer (the
# Dystric Fluvisols, Gleysols and Regosols of 1974, "at least in some part of
# the soil between 20 and 50 cm") or "throughout". H and O layers are not
# read, as in 1988.
bs_window_fao74 <- function(pedon, lt = 50, top = 20, bottom = 50, mode = c("some", "throughout")) {
  mode <- match.arg(mode)
  x <- .f74_ctx(pedon)
  pg <- "44-46"
  if (!x$n) return(.f74_key_result(x, "bs_window", FALSE, pg))
  bs <- .f88_bs(x)
  w <- .fao_in_window(x$h, .f88_cmp(bs$value, "<", lt), top, bottom, mode, subset = !.f74_ho(x))
  sel <- w$layers %||% integer(0)
  .f74_key_result(x, "bs_window", w$value, pg, layers = sel,
                  missing = c(w$missing, if (is.na(w$value)) "bs_pct"),
                  flags = c(w$flags, .f88_flags(bs$flags, sel)), notes = w$note,
                  evidence = list(lt = lt, window = c(top, bottom), mode = mode))
}

# Base saturation (NH4OAc) below `lt` % in at least a part of the upper B of
# `kind` starting within `to_cm` (.f74_b_eval()): the Acrisols gate and the
# Humic and Dystric units of the Ferralsols (oxic B, 100 cm), Podzoluvisols
# and Nitosols (argillic B, 125 cm) and Cambisols (cambic B, no depth: 125
# cm, Q10).
b_bs_below_fao74 <- function(pedon, kind = .F74_B_KINDS, lt = 50, to_cm = 125) {
  kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  pg <- "47-53"
  if (!x$n) return(.f74_key_result(x, "b_bs_below", FALSE, pg))
  bs <- .f88_bs(x)
  b <- .f74_b_eval(x, kind, to_cm, function(L) .f88_any(.f88_cmp(bs$value[L], "<", lt)))
  L <- b$layers
  if (b$none) return(.f74_key_result(x, "b_bs_below", FALSE, pg, notes = sprintf("no %s", kind)))
  .f74_key_result(x, "b_bs_below", b$value, pg, layers = L,
                  missing = if (is.na(b$value)) c("bs_pct", "diagnostic_b"),
                  flags = c(b$flags, .f88_flags(bs$flags, L)),
                  evidence = list(kind = kind, lt = lt, to_cm = to_cm, bs = bs$value[L]))
}

# A property in at least some part of a B starting within `to_cm`: the Acric
# CEC (acric_cec_fao74(): 1.5 me or less per 100 g clay by NH4Cl) in the oxic
# B (the Acric Ferralsols).
property_in_b_fao74 <- function(pedon, property = c("acric"), kind = .F74_B_KINDS, to_cm = 125) {
  property <- match.arg(property); kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  pg <- "47"
  if (!x$n) return(.f74_key_result(x, "property_in_b", FALSE, pg))
  r <- acric_cec_fao74(pedon)
  v <- .f88_values(r, x$n)
  b <- .f74_b_eval(x, kind, to_cm, function(L) .f88_any(v[L]))
  L <- b$layers
  if (b$none) return(.f74_key_result(x, "property_in_b", FALSE, pg, notes = sprintf("no %s", kind)))
  .f74_key_result(x, "property_in_b", b$value, pg, layers = L[v[L] %in% TRUE],
                  missing = if (is.na(b$value)) r$missing,
                  flags = c(b$flags, r$evidence$flags),
                  evidence = list(property = property, kind = kind, to_cm = to_cm))
}

# The rubbed colour of a B (Q24: munsell_*_rubbed, else the moist colour,
# flagged), in every layer of the upper B of `kind` starting within 125 cm
# (the B as a whole, as Hugo decided for 1988, 2026-11-09):
#   rhodic (Fr): hue redder than 5YR, moist value below 4, dry value no more
#     than one unit above the moist one (a dry value not recorded leaves it
#     undecided, flagged);
#   xanthic (Fx): hue 7.5YR or yellower, moist value 4 or more, moist chroma
#     5 or more;
#   chromic (Lc, Bc): hue 7.5YR with a chroma above 4, or a hue redder than
#     7.5YR.
b_colour_fao74 <- function(pedon, colour = c("rhodic", "xanthic", "chromic"), kind = .F74_B_KINDS) {
  colour <- match.arg(colour); kind <- match.arg(kind)
  x <- .f74_ctx(pedon)
  pg <- switch(colour, rhodic = "47", xanthic = "47", chromic = "52-53")
  if (!x$n) return(.f74_key_result(x, "b_colour", FALSE, pg))
  rb <- .fao_colour_rubbed(x$h)
  col <- .f88_colours(x)
  hue <- rb$hue; val <- rb$value; chr <- rb$chroma
  dry_flag <- character(0)
  v <- switch(colour,
    rhodic = {
      dry_flag <- "dry_value_not_recorded"
      .f88_and(.fao_hue_cmp(hue, "5YR", "redder"), .f88_cmp(val, "<", 4),
               .f88_cmp(col$vd - val, "<=", 1))
    },
    xanthic = .f88_and(.fao_hue_cmp(hue, "7.5YR", "as_yellow"), .f88_cmp(val, ">=", 4),
                       .f88_cmp(chr, ">=", 5)),
    chromic = {
      eq <- .fao_hue_rank(hue) == .fao_hue_rank("7.5YR")
      .f88_or(.f88_and(eq, .f88_cmp(chr, ">", 4)), .fao_hue_cmp(hue, "7.5YR", "redder"))
    })
  b <- .f74_b_eval(x, kind, 125, function(L) .f88_all(v[L]))
  L <- b$layers
  if (b$none) return(.f74_key_result(x, "b_colour", FALSE, pg, notes = sprintf("no %s", kind)))
  if (!any(is.na(col$vd[L]))) dry_flag <- character(0)
  .f74_key_result(x, "b_colour", b$value, pg, layers = L,
                  missing = if (is.na(b$value)) c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
                                                  if (colour == "rhodic") "munsell_value_dry"),
                  flags = c(b$flags, .f88_flags(rb$flags, L), if (colour == "rhodic") dry_flag),
                  evidence = list(colour = colour, kind = kind, hue = hue[L], value = val[L], chroma = chr[L]))
}


# --- unit gates: Andosols and Podzols ---------------------------------------------------------

# Ochric Andosols (To, p. 45): a smeary consistence (smeary_fao74() in a layer
# starting within `depth` cm), or a texture of silt loam or finer as the
# weighted average of the horizons within `depth` cm (USDA silt loam, silt,
# sandy clay loam, clay loam, silty clay loam, sandy clay, silty clay, clay,
# the classes Hugo set for 1988), or both. With no smeary record in those
# layers the smeary item is undecided (the decision of 1988 for the Vitric
# Andosols, Hugo 2026-11-09): the unit then passes on the texture alone.
smeary_or_fine_fao74 <- function(pedon, depth = 100) {
  x <- .f74_ctx(pedon)
  pg <- "45"
  if (!x$n) return(.f74_key_result(x, "smeary_or_fine", FALSE, pg))
  sm <- smeary_fao74(pedon, max_top_cm = depth)
  sc <- .fao_col(x$h, "smeary_consistence", "logical"); ti <- .f88_num(x, "thixotropic_index")
  rec <- any((!is.na(sc) | !is.na(ti)) & !is.na(x$top) & x$top <= depth)
  smeary <- if (isTRUE(sm$passed)) TRUE else if (rec) FALSE else NA
  hm <- x$h[!.f74_ho(x), , drop = FALSE]
  p <- .fao_psd(hm)
  cl <- .fao_weighted(hm, "clay_pct", 0, depth, values = p$clay)
  sa <- .fao_weighted(hm, "sand_pct", 0, depth, values = p$sand)
  cls <- if (is.na(cl$value) || is.na(sa$value)) NA_character_ else
    texture_class_from_pct(cl$value, 100 - cl$value - sa$value, sa$value)
  fine <- if (is.na(cls)) NA else cls %in% .F88_SILT_LOAM_OR_FINER
  value <- .fao_or(smeary, fine)
  .f74_key_result(x, "smeary_or_fine", value, pg,
                  missing = if (is.na(value)) unique(c("smeary_consistence", cl$missing, sa$missing, "clay_pct", "sand_pct")),
                  flags = c(sm$evidence$flags, cl$flags, sa$flags),
                  evidence = list(smeary = smeary, texture_class_0_100 = cls))
}

# Placic Podzols (Pp, p. 47): a thin iron pan (thin_iron_pan_fao74()) in or
# over the spodic B: its top at or above the base of the upper spodic B.
thin_pan_spodic_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "47"
  if (!x$n) return(.f74_key_result(x, "thin_pan_spodic", FALSE, pg))
  tp <- thin_iron_pan_fao74(pedon)
  v <- .f88_values(tp, x$n)
  u <- .f74_b_units(x, "spodic_b")
  if (!length(u)) return(.f74_key_result(x, "thin_pan_spodic", FALSE, pg, notes = "no spodic B"))
  base <- max(vapply(u, function(z) z$bottom, 0))
  sel <- !is.na(x$top) & x$top < base
  value <- .f88_any(v[sel])
  .f74_key_result(x, "thin_pan_spodic", value, pg, layers = which(sel & v %in% TRUE),
                  missing = if (is.na(value)) tp$missing, flags = tp$evidence$flags)
}

# Humic Podzols (Ph, p. 47): a layer of the spodic B with dispersed organic
# matter (organic carbon above 0, or an h in its designation) that lacks the
# free iron to turn redder on ignition (ignition_redder, else free iron below
# 0.5 % of the fine earth, footnote 23, flagged, Q23).
humic_podzol_fao74 <- function(pedon) {
  x <- .f74_ctx(pedon)
  pg <- "47"
  if (!x$n) return(.f74_key_result(x, "humic_podzol", FALSE, pg))
  oc <- .f88_num(x, "oc_pct")
  hs <- !is.na(x$desg) & grepl("^B[a-z]*h", x$desg)
  dom <- ifelse(!is.na(oc), oc > 0, ifelse(hs, TRUE, NA))
  ig <- .fao_ignition_redder(x$h)
  v <- .f88_and(dom, .f88_not(ig$value))
  b <- .f74_b_eval(x, "spodic_b", 125, function(L) .f88_any(v[L]))
  L <- b$layers
  if (b$none) return(.f74_key_result(x, "humic_podzol", FALSE, pg, notes = "no spodic B"))
  .f74_key_result(x, "humic_podzol", b$value, pg, layers = L[v[L] %in% TRUE],
                  missing = if (is.na(b$value)) c(ig$missing, "oc_pct"),
                  flags = c(b$flags, .f88_flags(ig$flags, L)))
}

# Ferric Podzols (Pf, p. 47): free iron / carbon (fe_dcb_pct / oc_pct) of
# `min` or more in every layer of the spodic B (both 0: no ratio, fails).
spodic_fe_oc_fao74 <- function(pedon, min = 6) {
  x <- .f74_ctx(pedon)
  pg <- "47"
  if (!x$n) return(.f74_key_result(x, "spodic_fe_oc", FALSE, pg))
  fe <- .f88_num(x, "fe_dcb_pct"); oc <- .f88_num(x, "oc_pct")
  r <- ifelse(is.na(fe) | is.na(oc), NA_real_,
              ifelse(oc > 0, fe / oc, ifelse(fe > 0, Inf, 0)))
  b <- .f74_b_eval(x, "spodic_b", 125, function(L) .f88_all(.f88_cmpr(r[L], ">=", min)))
  L <- b$layers
  if (b$none) return(.f74_key_result(x, "spodic_fe_oc", FALSE, pg, notes = "no spodic B"))
  .f74_key_result(x, "spodic_fe_oc", b$value, pg, layers = L,
                  missing = if (is.na(b$value)) c("fe_dcb_pct", "oc_pct"),
                  flags = b$flags, evidence = list(ratio = r[L], min = min))
}

# Leptic Podzols (Pl, p. 47): no albic E, or only a thin (`albic_max_cm` cm
# or less) and discontinuous one (boundary_topography broken or
# discontinuous; not recorded, continuous); and no layer of the spodic B
# visibly more enriched in carbon (an h in its designation, Bh or Bhs; not
# recorded, absent): the readings of the Cambic Podzols of 1988.
leptic_podzol_fao74 <- function(pedon, albic_max_cm = 2) {
  x <- .f74_ctx(pedon)
  pg <- "47"
  if (!x$n) return(.f74_key_result(x, "leptic_podzol", FALSE, pg))
  ae <- .f88_hz_albic_e(x)$units
  bt <- .f88_chr(x, "boundary_topography")
  ok_e <- vapply(ae, function(u) {
    thin <- u$thickness <= albic_max_cm
    disc <- any(!is.na(bt[u$layers]) & grepl("broken|discontin|interromp", bt[u$layers]))
    if (thin && disc) TRUE else if (isTRUE(u$status)) FALSE else NA
  }, NA)
  e_ok <- .f88_all(ok_e)
  u <- .f74_b_units(x, "spodic_b")
  L <- unlist(lapply(u, `[[`, "layers"))
  hs <- !is.na(x$desg) & grepl("^B[a-z]*h", x$desg)
  no_h <- !any(hs[L])
  value <- .fao_and(e_ok, no_h)
  .f74_key_result(x, "leptic_podzol", value, pg, layers = L %||% integer(0),
                  missing = if (is.na(value)) c("munsell_value_dry", "boundary_topography"),
                  evidence = list(albic_thin_discontinuous = e_ok, no_carbon_enriched_subhorizon = no_h))
}


# --- the classifier ------------------------------------------------------------------------------

.FAO74_NOT_CLASSIFIED <- "[Not classified in the FAO-Unesco Legend (1974)]"

# classify_fao(edition = "1974"): the 26 groups and, under each, its units
# (classify_hierarchical()); the Lithosols, Rankers and Rendzinas have no
# unit list, so the name stops at the group, which is the unit. No residual
# group after the Cambisols (Q5).
.classify_fao1974_impl <- function(pedon, rules = NULL, on_missing = "warn", gapfill = FALSE) {
  rules <- rules %||% load_rules("fao1974")
  res <- classify_hierarchical("fao1974", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  lv <- res$levels
  tr <- res$trace$major_groups %||% list()
  if (is.data.frame(lv) && nrow(lv)) {
    ent <- lapply(seq_len(nrow(lv)), function(i) {
      lst <- rules[[lv$level[i]]]
      if (i > 1L) lst <- lst[[lv$code[i - 1L]]]
      Find(function(z) identical(as.character(z$code), lv$code[i]), lst)
    })
    lv$name_pt <- vapply(ent, function(e) as.character(e$name_pt %||% NA_character_), "")
    lv$page <- vapply(ent, function(e) as.character(e$page %||% NA_character_), "")
    res$levels <- lv
    return(res)
  }
  st <- vapply(tr, function(e) { p <- e$passed; if (length(p) == 1L) as.logical(p) else NA }, NA)
  und <- names(st)[is.na(st)]
  nm <- .F74_GROUPS$name[match(und, .F74_GROUPS$code)]
  res$warnings <- res$warnings[!grepl("^No class of the major_groups key passed|^Provisional: [0-9]+ class", res$warnings)]
  if (length(und)) {
    res$name <- sprintf("[Undecided in the FAO-Unesco Legend (1974): %s, or not classified]",
                        paste(sprintf("%s %s", und, nm), collapse = ", "))
    res$warnings <- c(res$warnings, sprintf(paste(
      "No major soil group passed; the data leave %s undecided (the candidates).",
      "The key has no residual group after the Cambisols (Hugo's decision Q5): if they all fail,",
      "the soil is not classified in the 1974 legend."), paste(und, collapse = ", ")))
  } else {
    res$name <- .FAO74_NOT_CLASSIFIED
    res$warnings <- c(res$warnings, sprintf(paste(
      "Not classified: none of the %d gates of the 1974 key passed (%s), and the key has no",
      "residual group after the Cambisols (Hugo's decision Q5); see $trace for the test that",
      "failed in each gate."), length(st), paste(names(st), collapse = ", ")))
  }
  res
}


# --- examples ----------------------------------------------------------------------------------

# make_fao_example(code, edition = "1974"): a group or a unit of 1974.
.fao74_make_example <- function(code, nm = FALSE) {
  if (code %in% .F74_GROUPS$code) return(.fao74_example(code, nm))
  if (!code %in% .f74_unit_codes())
    rlang::abort(sprintf(paste("'%s' is not a code of the FAO-Unesco Legend of 1974: use a major",
                               "soil group (\"L\") or a soil unit (\"Lc\")."), code))
  if (nm && !code %in% .F74_UNIT_NEAR_MISS)
    rlang::abort(sprintf("The soil unit %s has no printed number in its gate, so no near miss.", code))
  .fao74_unit_example(code, nm)
}

# The 103 unit codes of the unit lists in the key's order, with the three
# single-unit groups (I, U, E) in their place: the 106 soil units.
.f74_unit_codes <- function() {
  u <- load_rules("fao1974")$units
  unlist(lapply(.F74_GROUPS$code, function(g) {
    if (g %in% .F74_SINGLE) return(g)
    vapply(u[[g]], function(e) as.character(e$code), "")
  }), use.names = FALSE)
}

# One synthetic profile per major soil group of 1974 (and, with `nm`, the
# near miss: the same profile with one printed number of its gate crossed),
# built from the key's numbers on the profiles of the 1988 examples where
# the concept holds (no profile of the book is copied). Every example
# records a udic moisture regime (the aridic regime of the Xerosols and
# Yermosols is decided) and, where it has an argillic B, its ECEC (the CEC by
# NH4Cl of the ferric properties, Q12).
.fao74_example <- function(code, nm = FALSE) {
  E <- .f88x_edit; S <- .f88x_set; A <- .f88x_append
  # a 1988 example, its ECEC (the CEC by NH4Cl of 1974, Q12) set to its CEC
  # where not given, the udic regime recorded
  b88 <- function(g, n = FALSE, ..., site = list()) {
    p <- .fao88_example(g, n)
    a <- list(...)
    if (!is.null(a$rows)) { p <- E(p, rows = a$rows); a$rows <- NULL }
    if (is.null(a$ecec_cmol)) a$ecec_cmol <- as.numeric(p$horizons$cec_cmol)
    do.call(E, c(list(p), a, list(site = utils::modifyList(list(soil_moisture_regime = "udic"), site))))
  }
  # a loamy argillic profile: A, Bt1, Bt2, BC, C (no E; clay falling below
  # the Bt so that it is no Nitosol), the CEC per kg clay and base
  # saturation set per group
  argillic <- function(bs = 60, bs_low = bs, cec = 30, ...) {
    p <- .fao88_example("LV", FALSE)
    p <- E(p, designation = c("A", "AB", "Bt1", "Bt2", "BC"), clay_pct = c(15, 18, 32, 34, 20),
           sand_pct = c(50, 47, 38, 38, 48), silt_pct = c(35, 35, 30, 28, 32),
           cec_cmol = c(8, 7, cec * 0.32, cec * 0.34, cec * 0.2),
           ecec_cmol = c(8, 7, cec * 0.32, cec * 0.34, cec * 0.2),
           bs_pct = c(bs, bs, bs, bs_low, bs_low), ph_h2o = if (min(bs, bs_low) < 50) 5.0 else 6.5,
           munsell_hue_moist = "10YR", munsell_value_moist = c(4, 4, 4, 4, 5),
           munsell_chroma_moist = c(3, 3, 4, 4, 4), munsell_value_dry = c(6, 6, 6, 6, 6),
           munsell_chroma_dry = c(3, 3, 4, 4, 4), boundary_distinctness = "gradual",
           site = list(soil_moisture_regime = "udic"))
    if (length(list(...))) p <- E(p, ...)
    p
  }
  out <- switch(code,
    O = b88("HS", nm),
    I = E(.fao88_example("LP", FALSE), rows = c(1, 3), top_cm = c(0, if (nm) 11 else 8),
          bottom_cm = c(if (nm) 11 else 8, 60), site = list(soil_moisture_regime = "udic")),
    V = b88("VR", FALSE, clay_pct = c(if (nm) 27 else 45, 50, 52, 48),
            sand_pct = c(if (nm) 43 else 25, 22, 20, 22), top_cm = c(0, 25, 60, 110),
            bottom_cm = c(25, 60, 110, 150)),
    J = b88("FL", nm),
    Z = b88("SC", FALSE, ec_dS_m = c(if (nm) 14 else 20, 12, 8)),
    G = b88("GL", nm),
    T = b88("AN", nm),
    Q = b88("AR", FALSE, clay_pct = c(4, 3, if (nm) 20 else 3), silt_pct = c(6, 5, if (nm) 20 else 4),
            sand_pct = c(90, 92, if (nm) 60 else 93)),
    # a BC whose CEC (1.5 cmol/kg, 8 me per 100 g clay) fails the 16 me of
    # the cambic B; the near miss has 9.6 (53 me) and is a Cambisol
    R = b88("RG", FALSE, designation = c("A", "BC", "C"), caco3_pct = 0, carbonate_soft_pct = 0,
            cec_cmol = c(10, if (nm) 9.6 else 1.5, 9), weatherable_minerals_pct = 0,
            muscovite_pct = 0, rock_structure_pct = 0, munsell_chroma_moist = c(3, 5, 4), ph_h2o = 6.5,
            structure_grade = c("weak", "weak", "structureless"),
            structure_type = c("granular", "subangular blocky", "massive")),
    U = b88("RG", FALSE, rows = c(1, 2, 3), top_cm = c(0, if (nm) 26 else 20, 60),
            bottom_cm = c(if (nm) 26 else 20, 60, 130), munsell_value_moist = c(3, 5, 5),
            munsell_chroma_moist = c(2, 4, 4), munsell_value_dry = c(5, 6, 6), oc_pct = c(3, 0.3, 0.2),
            bs_pct = c(35, 85, 85), caco3_pct = 0, carbonate_soft_pct = 0, ph_h2o = c(5.5, 6.5, 6.5),
            structure_grade = c("strong", "structureless", "structureless")),
    E = b88("RG", FALSE, rows = c(1, 2), designation = c("A", "Ck"), top_cm = c(0, 25),
            bottom_cm = c(25, 60), munsell_value_moist = c(3, 6), munsell_chroma_moist = c(2, 3),
            munsell_value_dry = c(5, 7), oc_pct = c(3, 0.4), cec_cmol = c(25, 9),
            caco3_pct = c(10, if (nm) 40 else 45), carbonate_soft_pct = 0, ph_h2o = c(7.6, 8.2),
            structure_grade = c("strong", "structureless")),
    P = b88("PZ", nm, ignition_redder = TRUE),
    F = b88("FR", nm, munsell_chroma_moist = c(3, 6, if (nm) 5 else 6, if (nm) 4 else 6)),
    W = b88("PL", nm),
    S = b88("SN", nm),
    M = b88("GR", nm, ecec_cmol = c(30, 26, 26, 22, 18)),
    C = b88("CH", FALSE, top_cm = c(0, 25, if (nm) 91 else 50, 120),
            bottom_cm = c(25, if (nm) 91 else 50, 120, 150)),
    K = b88("KS", FALSE, top_cm = c(0, 20, if (nm) 91 else 40, 120),
            bottom_cm = c(20, if (nm) 91 else 40, 120, 150)),
    H = b88("PH", FALSE, munsell_value_moist = c(if (nm) 4 else 2, if (nm) 4 else 3, 4, 5),
            munsell_value_dry = c(if (nm) 6 else 4, if (nm) 6 else 5, 6, 6)),
    D = b88("PD", nm, ecec_cmol = c(8, 6, 9.6, 10.2, 8.4)),
    X = b88("CL", FALSE, oc_pct = c(if (nm) 0.5 else 1.0, if (nm) 0.3 else 0.6, 0.2),
            site = list(soil_moisture_regime = "aridic")),
    Y = b88("CL", FALSE, oc_pct = c(if (nm) 1.0 else 0.3, if (nm) 0.6 else 0.1, 0.1),
            site = list(soil_moisture_regime = "aridic")),
    N = b88("NT", FALSE, clay_pct = c(40, 48, 60, 62, if (nm) 45 else 58),
            sand_pct = c(40, 34, 25, 24, if (nm) 39 else 26), ecec_cmol = c(12, 12, 15, 15, 14)),
    A = argillic(bs = if (nm) 55 else 30),
    L = argillic(bs = 60, bs_low = if (nm) 45 else 60),
    B = b88("CM", FALSE, top_cm = c(0, 20, if (nm) 24 else 60), bottom_cm = c(20, if (nm) 24 else 60, 130)))
  if (is.null(out)) rlang::abort(sprintf("No example for the major soil group '%s'.", code))
  out$site$id <- paste0("fao1974-", code, if (nm) "-near-miss")
  out
}

# The units whose gate has a printed number (a depth, a thickness, a
# percentage, a ratio, a Munsell limit), each with a near miss that crosses
# it (make_fao_example(code, edition = "1974", near_miss = TRUE)).
.F74_UNIT_NEAR_MISS <- c(
  "Ox", "Od", "Vp", "Jt", "Jc", "Jd", "Zg", "Gx", "Gp", "Gc", "Gd", "Qa", "Rx", "Rc", "Rd",
  "Pg", "Pf", "Pl", "Fp", "Fa", "Fr", "Fx", "Wx", "Ws", "Wd", "Sg", "Mg", "Hg", "Hc", "Dg",
  "Dd", "Xy", "Xk", "Yy", "Yk", "Nh", "Nd", "Ap", "Ag", "Ah", "Lp", "Lg", "Lk", "Lc", "Bx",
  "Bg", "Bk", "Bf", "Bd", "Bc")

# One synthetic profile per soil unit of 1974 (and, with `nm`, the near miss
# of a unit whose gate has a printed number), built on the example of its
# group. Each example keys to its own unit, not provisionally; each near miss
# crosses one printed number of its unit gate and keys to another unit.
.fao74_unit_example <- function(code, nm = FALSE) {
  g <- substr(code, 1L, 1L)
  b <- .fao74_example(g, FALSE)
  E <- .f88x_edit; S <- .f88x_set; A <- .f88x_append
  gl <- .f88x_gley
  perma <- function(p) A(p, if (nm) 201 else 200, 240, permafrost_temp_C = -1)
  deep <- function(p, X, ...) A(p, if (nm) X + 1 else X, X + 40, ...)
  deep2 <- function(p, X, ..., below = list()) {
    t0 <- if (nm) X + 1 else X
    q <- A(p, t0, t0 + 30, ...)
    do.call(A, c(list(q, t0 + 30, t0 + 60), below))
  }
  calcic_at <- function(p, X) deep2(p, X, designation = "Bk", caco3_pct = 25,
                                    below = list(designation = "C", caco3_pct = 10))
  gypsic_at <- function(p, X) deep2(p, X, designation = "By", caso4_pct = 20, ec_dS_m = 2.5,
                                    below = list(designation = "C", caso4_pct = 2, ec_dS_m = 2.2))
  gley_at <- function(p, X) A(p, if (nm) X + 1 else X, max(X + 41, 150), designation = "Cg",
                              munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1)
  plinth_at <- function(p, X) deep(p, X, designation = "Bv", plinthite_pct = 5)
  # the arid profiles of X and Y without carbonate, for the gypsic and calcic
  # horizons placed by the 125 / 90 / 75 cm rule (medium texture: 90 cm)
  arid0 <- function(p) E(p, designation = c("A", "Bw", "C"), caco3_pct = 0, ph_h2o = 7.2)
  out <- switch(code,
    # --- Histosols (H1 0-20, H2 20-45)
    Ox = perma(b),
    Od = E(b, top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 130), ph_h2o = c(5.0, if (nm) 5.5 else 5.4, 6.0)),
    Oe = E(b, ph_h2o = 6.0),
    # --- Vertisols
    Vp = E(b, munsell_chroma_moist = c(1, if (nm) 1.5 else 1, 2, 3)),
    Vc = b,
    # --- Fluvisols
    Jt = deep(b, 125, designation = "Cr", s_total_pct = 1.2, ph_h2o = 6),
    Jc = E(b, caco3_pct = c(0, if (nm) 2 else 5, 5, 5, 5), ph_h2o = 7.8),
    Jd = E(b, bs_pct = c(60, 60, if (nm) 50 else 40, 60, 60), ph_h2o = 5.5),
    Je = b,
    # --- Solonchaks (Az 0-20, Bw 20-60, C 60-130)
    Zg = gley_at(b, 50),
    Zt = E(b, clay_pct = c(42, 40, 38), sand_pct = c(18, 20, 22), silt_pct = c(40, 40, 40),
           polygonal_cracks_spacing_cm = c(20, NA, NA)),
    Zm = E(b, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 130), munsell_value_moist = c(3, 5, 6),
           munsell_chroma_moist = c(2, 5, 4), munsell_value_dry = c(5, 6, 7), oc_pct = c(2, 0.4, 0.2),
           structure_grade = c("strong", "moderate", "structureless")),
    Zo = b,
    # --- Gleysols (A 0-20, Bw 20-45, Cg 45-130)
    Gx = perma(b),
    Gp = plinth_at(b, 125),
    Gm = E(b, top_cm = c(0, 25, 45), bottom_cm = c(25, 45, 130), munsell_value_moist = c(3, 4, 5),
           munsell_chroma_moist = c(2, 5, 1), munsell_value_dry = c(5, 6, 6), oc_pct = c(3, 0.5, 0.3),
           structure_grade = c("strong", "moderate", "structureless")),
    Gh = E(b, top_cm = c(0, 25, 45), bottom_cm = c(25, 45, 130), munsell_value_moist = c(3, 4, 5),
           munsell_chroma_moist = c(2, 5, 1), munsell_value_dry = c(5, 6, 6), oc_pct = c(3, 0.5, 0.3),
           bs_pct = c(35, 60, 60), ph_h2o = 5.5, structure_grade = c("strong", "moderate", "structureless")),
    Gc = calcic_at(b, 125),
    Gd = E(b, bs_pct = c(60, if (nm) 50 else 40, 60), ph_h2o = 5.5),
    Ge = b,
    # --- Andosols (umbric A over a Bw and C)
    Tm = E(b, bs_pct = c(60, 55, 50), ph_h2o = 6.2),
    Th = b,
    To = , Tv = E(b, munsell_value_moist = c(4, 4, 4), munsell_chroma_moist = c(3, 4, 4),
                  munsell_value_dry = c(5, 5, 5), oc_pct = c(1.5, 1, 0.5),
                  smeary_consistence = code == "To",
                  clay_pct = if (code == "To") c(20, 22, 18) else c(8, 8, 6),
                  silt_pct = if (code == "To") c(55, 53, 57) else c(20, 20, 14),
                  sand_pct = if (code == "To") 25 else c(72, 72, 80)),
    # --- Arenosols (A 0-20, C1 20-80, C2 80-150); a dark A (value 3, dry
    # 4) is no albic material, its chroma 4 no mollic A
    Qa = if (nm) E(b, rows = c(1, 2, 2, 3), designation = c("A", "C1", "C2", "C3"),
                   top_cm = c(0, 20, 49, 80), bottom_cm = c(20, 49, 80, 150),
                   munsell_value_moist = c(4, 5, 3, 6), munsell_value_dry = c(5, 6, 4, 7),
                   munsell_chroma_moist = c(3, 4, 4, 4)) else b,
    Ql = , Qf = , Qc = {
      d <- E(b, munsell_value_moist = c(3, 5, 6), munsell_value_dry = c(4, 6, 7),
             munsell_chroma_moist = c(4, 4, 4))
      switch(code,
        Ql = E(d, lamellae_cumulative_cm = c(NA, NA, 10)),
        Qf = E(d, ecec_cmol = c(3, 0.5, 2)),
        Qc = E(d, designation = c("A", "Bw", "C"), munsell_chroma_moist = c(4, 5, 4),
               munsell_hue_moist = c("10YR", "7.5YR", "10YR")))
    },
    # --- Regosols (A 0-15, BC 15-60, C 60-130)
    Rx = perma(b),
    Rc = E(b, caco3_pct = c(0, if (nm) 2 else 5, 5), ph_h2o = 7.8),
    Rd = E(b, bs_pct = c(60, if (nm) 50 else 40, 60), ph_h2o = 5.5),
    Re = b,
    # --- Podzols (A 0-10, E 10-30, Bhs 30-45, Bs 45-80, C 80-130)
    Pp = S(b, 3, pan_type = "thin iron pan"),
    Pg = gley_at(b, 50),
    Ph = S(b, 3, ignition_redder = FALSE),
    Pf = E(b, fe_dcb_pct = c(0.2, 0.1, 3.0, if (nm) 2.2 else 2.4, 0.2), oc_pct = c(2, 0.4, 0.5, 0.4, 0.2),
           fe_pyro_pct = c(0.05, 0.02, 1.5, 1.2, 0.05)),
    Pl = E(b, designation = c("A", "E", "Bs1", "Bs2", "C"), top_cm = c(0, 10, if (nm) 13 else 12, 45, 80),
           bottom_cm = c(10, if (nm) 13 else 12, 45, 80, 130), boundary_topography = c(NA, "broken", NA, NA, NA)),
    Po = b,
    # --- Ferralsols (A 0-20, Bo1 20-60, Bo2 60-110, Bo3 110-180); `lo` has
    # too little carbon for the Humic unit and a rhodic B (2.5YR 3/6, dry 4)
    Fp = plinth_at(b, 125),
    Fh = b,
    Fa = , Fr = , Fx = , Fo = {
      lo <- E(b, oc_pct = c(1.0, 0.3, 0.2, 0.1), munsell_value_dry = c(4, 4, 4, 4))
      switch(code,
        Fa = E(lo, ecec_cmol = c(1, if (nm) c(0.9, 0.95, 0.95) else c(0.5, 0.5, 0.5))),
        Fr = E(lo, munsell_value_dry = c(4, if (nm) c(5, 5, 5) else c(4, 4, 4))),
        Fx = E(lo, munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "7.5YR"), munsell_value_moist = c(4, 5, 5, 5),
               munsell_chroma_moist = c(3, if (nm) c(4, 4, 4) else c(6, 6, 6)), munsell_value_dry = c(5, 6, 6, 6)),
        Fo = E(lo, munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "7.5YR"), munsell_value_moist = c(4, 5, 5, 5),
               munsell_chroma_moist = c(3, 4, 4, 4), munsell_value_dry = c(5, 6, 6, 6)))
    },
    # --- Planosols (A 0-15, Eg 15-40, Btg 40-90, C 90-140)
    Wx = perma(b),
    Ws = E(b, esp_pct = c(2, 3, if (nm) 6 else 8, 4)),
    Wm = E(b, top_cm = c(0, 26, 40, 90), bottom_cm = c(26, 40, 90, 140), munsell_value_moist = c(3, 6, 5, 5),
           munsell_chroma_moist = c(2, 1, 2, 3), munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(2.5, 0.4, 0.4, 0.2),
           cec_cmol = c(18, 5, 14, 10), ecec_cmol = c(18, 5, 14, 10),
           structure_grade = c("strong", "weak", "strong", "structureless")),
    Wh = E(b, top_cm = c(0, 26, 40, 90), bottom_cm = c(26, 40, 90, 140), munsell_value_moist = c(3, 6, 5, 5),
           munsell_chroma_moist = c(2, 1, 2, 3), munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(2.5, 0.4, 0.4, 0.2),
           cec_cmol = c(18, 5, 14, 10), ecec_cmol = c(18, 5, 14, 10), bs_pct = c(35, 55, 70, 75),
           ph_h2o = 5.5, structure_grade = c("strong", "weak", "strong", "structureless")),
    Wd = E(b, bs_pct = c(60, 55, if (nm) 50 else 40, 75)),
    We = b,
    # --- Solonetz (A 0-10, E 10-20, Btn 20-60, BCn 60-100, C 100-140)
    Sg = gley_at(b, 50),
    Sm = E(b, rows = c(1, 3, 4, 5), top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 140),
           munsell_value_moist = c(3, 4, 5, 5), munsell_chroma_moist = c(2, 3, 3, 4), munsell_value_dry = c(5, 6, 6, 6),
           oc_pct = c(2, 0.6, 0.3, 0.2), cec_cmol = c(20, 20, 18, 15), ecec_cmol = c(20, 20, 18, 15),
           clay_pct = c(20, 35, 30, 25), silt_pct = c(40, 35, 38, 40), sand_pct = c(40, 30, 32, 35),
           structure_grade = c("strong", "strong", "moderate", "structureless"),
           boundary_distinctness = c("abrupt", "clear", "clear", NA)),
    So = b,
    # --- Greyzems (Ah1 0-15, Ah2 15-35, Bt 35-80, BC 80-110, C 110-150)
    Mg = gley_at(b, 50),
    Mo = b,
    # --- Chernozems (Ah1 0-25, Ah2 25-50, Bk 50-120, Ck 120-150)
    Cl = , Kl = E(b, rows = c(1, 2, 3, 3, 4), designation = c("Ah1", "Ah2", "Bt", "Bk", "Ck"),
                  top_cm = c(0, 25, 50, 75, 110), bottom_cm = c(25, 50, 75, 110, 150),
                  clay_pct = c(22, 22, 34, 28, 22), silt_pct = c(55, 55, 46, 52, 58),
                  sand_pct = c(23, 23, 20, 20, 20), caco3_pct = c(0, 0, 0, 22, 12),
                  clay_films_amount = c(NA, NA, "common", NA, NA), cec_cmol = c(35, 30, 26, 20, 18),
                  ecec_cmol = c(35, 30, 26, 20, 18)),
    Cg = S(b, 3, tongue_vol_pct = 25, tongue_width_mm = 30, tongue_depth_cm = 20),
    Ck = , Kk = b,
    Ch = , Kh = E(b, caco3_pct = c(0, 1, 4, 3), carbonate_soft_pct = c(0, 0, 8, 2)),
    # --- Phaeozems (Ah1 0-20, Ah2 20-40, Bw 40-90, C 90-140)
    Hg = , Hl = {
      a <- E(b, designation = c("Ah1", "Ah2", "Bt", "C"), clay_pct = c(22, 22, 34, 22),
             sand_pct = c(33, 33, 23, 33), clay_films_amount = c(NA, NA, "common", NA))
      if (code == "Hg") gley_at(a, 50) else a
    },
    Hc = E(b, caco3_pct = c(0, if (nm) 2 else 5, 5, 3), ph_h2o = 7.6),
    Hh = b,
    # --- Podzoluvisols (A 0-15, E 15-35, Bt1 35-70, Bt2 70-110, BC 110-160)
    Dg = gl(b, if (nm) 4 else 3),
    Dd = E(b, bs_pct = c(60, 60, 60, if (nm) 50 else 40, 60), ph_h2o = 5.5),
    De = b,
    # --- Xerosols and Yermosols (A 0-20, Bk 20-70, Ck 70-130; medium
    # texture: the 90 cm of the rule)
    Xl = , Yl = E(b, designation = c("A", "Btk", "Ck"), clay_pct = c(15, 30, 22), silt_pct = c(45, 40, 43),
                  sand_pct = c(40, 30, 35), clay_films_amount = c(NA, "common", NA)),
    Xy = , Yy = gypsic_at(arid0(b), 90),
    Xk = , Yk = calcic_at(arid0(b), 90),
    Xh = , Yh = arid0(b),
    Yt = E(b, clay_pct = c(42, 40, 38), sand_pct = c(18, 20, 22), silt_pct = c(40, 40, 40),
           polygonal_cracks_spacing_cm = c(20, NA, NA), structure_type = c("platy", "subangular blocky", "massive")),
    # --- Nitosols (A 0-15, AB 15-30, Bt1 30-70, Bt2 70-120, Bt3 120-170)
    Nh = E(b, oc_pct = c(2, 1.2, if (nm) 0.4 else 0.6, 0.3, 0.2)),
    Nd = E(b, oc_pct = c(2, 1.2, 0.4, 0.3, 0.2), bs_pct = if (nm) 50 else c(40, 35, 30, 30, 30),
           ph_h2o = if (nm) 6 else 5.2),
    Ne = E(b, oc_pct = c(2, 1.2, 0.4, 0.3, 0.2), bs_pct = 60, ph_h2o = 6.2),
    # --- Acrisols and Luvisols (A 0-15, AB 15-35, Bt1 35-70, Bt2 70-110, BC 110-160)
    Ap = , Lp = plinth_at(b, 125),
    Ag = , Lg = gl(b, if (nm) 4 else 3),
    Ah = E(b, oc_pct = c(1.2, 0.5, if (nm) 0.8 else 1.0, 0.3, 0.2)),
    Af = , Lf = S(b, 4, redoximorphic_features_pct = 20, mottle_size_class = "coarse", mottle_colors = "2.5YR 4/8"),
    Ao = , Lo = b,
    # the argillic B under an albic E (the Luvisol of 1988), its BC with less
    # clay so that it is no Nitosol
    La = {
      a <- .fao88_example("LV", FALSE)
      E(a, ecec_cmol = as.numeric(a$horizons$cec_cmol), clay_pct = c(15, 14, 32, 34, 20),
        sand_pct = c(50, 50, 38, 38, 48), silt_pct = c(35, 36, 30, 28, 32),
        site = list(soil_moisture_regime = "udic"))
    },
    Lv = S(b, 3, cracks_width_cm = 2),
    Lk = deep2(b, 90, designation = "Bk", caco3_pct = 25, clay_pct = 20, sand_pct = 48, silt_pct = 32,
               clay_films_amount = NA_character_,
               below = list(designation = "C", caco3_pct = 10, clay_pct = 20, sand_pct = 48, silt_pct = 32,
                            clay_films_amount = NA_character_)),
    Lc = E(b, munsell_hue_moist = c("10YR", "10YR", if (nm) "7.5YR" else "5YR", "5YR", "5YR")),
    # --- Cambisols (A 0-20, Bw 20-60, C 60-130; Bw 7.5YR 4/6)
    Bx = perma(b),
    Bg = gley_at(b, 100),
    Bv = E(b, clay_pct = c(30, 35, 30), sand_pct = c(30, 25, 30), cracks_width_cm = c(NA, 2, NA)),
    Bk = calcic_at(b, 90),
    Bh = E(b, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 130), munsell_value_moist = c(3, 4, 5),
           munsell_chroma_moist = c(2, 6, 4), munsell_value_dry = c(5, 5, 6), oc_pct = c(2.5, 0.5, 0.2),
           bs_pct = c(35, 60, 75), cec_cmol = c(18, 12, 11), ecec_cmol = c(18, 12, 11), ph_h2o = c(5.5, 6, 6),
           structure_grade = c("strong", "moderate", "structureless")),
    Bf = E(b, ecec_cmol = c(12, if (nm) 5.3 else 5.0, 11)),
    Bd = E(b, bs_pct = c(70, if (nm) 50 else 40, 75), ph_h2o = 5.5),
    Bc = if (nm) E(b, munsell_chroma_moist = c(3, 4, 4)) else b,
    Be = E(b, munsell_hue_moist = "10YR", munsell_chroma_moist = c(3, 4, 4)),
    NULL)
  if (is.null(out)) rlang::abort(sprintf("No example for the soil unit '%s'.", code))
  out$site$id <- paste0("fao1974-", code, if (nm) "-near-miss")
  out
}
