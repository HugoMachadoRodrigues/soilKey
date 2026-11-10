# =============================================================================
# v0.9.385 -- the 153 soil units of the FAO-Unesco Revised Legend (FAO 1988,
# World Soil Resources Report 60; the text of the ISRIC reprint of 1997,
# Technical Paper 20), chapter X, pp. 75-89: the second level of the key,
# under the 28 major soil groupings of 0.9.384. The units are listed in
# inst/rules/fao1988/units/ (one file per grouping, in the key's order, the
# last unit of each the "Other ..." residual); most unit gates read the
# diagnostic horizons (0.9.382) and properties (0.9.383) directly from the
# YAML. The predicates below are those of the unit gates that read more
# than one diagnostic in one place, or a field record the diagnostics do not
# read: the drainage and fibres of the Histosols, the deep cultivation and
# man-made sediments of the Anthrosols, the rock within 10 cm and the
# calcareous material under the mollic A of the Leptosols, the base
# saturation of a window or of a B, the texture of the Vitric Andosols, the
# thick albic E, clay increase, colour under the A and cambic alteration of
# the Arenosols, the B of the Podzols (ignition, Fe/C ratio, thin albic E),
# the plinthite horizon of the Plinthosols, the geric and ferralic
# properties inside a B, the rubbed colours of the Rhodic, Xanthic and
# Chromic units, and the slowly permeable horizon of the Planosols.
#
# Conventions as in R/key-fao.R: three values (a measurement not recorded
# leaves a criterion undecided, a field observation not recorded is
# absent); depths from the literal soil surface (Q9); the key read as
# printed, the descriptions of chapter VI not added (Q6); the passages in
# italics of the 1997 text applied (Q4). The criteria are paraphrased; no
# text of the book is shipped. The numbers are those of
# inst/rules/fao1988/thresholds.yaml (unit_key), the readings those of
# inst/rules/fao1988/readings.yaml.
# =============================================================================


# --- shared ----------------------------------------------------------------------------------

# The layers of the upper diagnostic B of a kind (argic, natric, cambic,
# spodic or ferralic) starting within `to_cm`, sure units first (p. 42; the
# rule of .f88_argic_layers() for any B), with the unit's status; with no
# such B, the layers designated B starting within `to_cm`, flagged (plan
# section 4.2).
.f88_b_layers_kind <- function(x, kind = .F88_B_KINDS, to_cm = 125) {
  kind <- match.arg(kind)
  u <- Filter(function(z) !is.na(z$top) && z$top <= to_cm, .f88_b_units(x, kind))
  if (length(u)) {
    sure <- Filter(function(z) isTRUE(z$status), u)
    pick <- if (length(sure)) sure else u
    pick <- pick[[which.min(vapply(pick, function(z) z$top, 0))]]
    L <- pick$layers
    return(list(layers = L[!is.na(x$top[L]) & x$top[L] <= to_cm],
                status = if (isTRUE(pick$status)) TRUE else NA, flags = character(0)))
  }
  bd <- which(!is.na(x$master) & grepl("^B", x$master) & !x$rock & !is.na(x$top) & x$top <= to_cm)
  list(layers = bd, status = if (length(bd)) NA else FALSE,
       flags = if (length(bd)) sprintf("b_designation_for_%s", kind))
}

# A per-layer answer `fun(layers)` (TRUE / FALSE / NA) read on the upper
# diagnostic B of a kind starting within `to_cm` (p. 42: the upper B
# decides). The uppermost unit is read; when it is undecided and a sure B
# lies below it, the sure B is read too, and the answer stands only when
# both agree (else NA, the undecided B above deciding). An undecided B alone
# turns a TRUE into NA. With no such B, the layers designated B, flagged
# (plan section 4.2), the same way.
.f88_b_eval <- function(x, kind = .F88_B_KINDS, to_cm = 125, fun) {
  kind <- match.arg(kind)
  clip <- function(L) L[!is.na(x$top[L]) & x$top[L] <= to_cm]
  u <- Filter(function(z) !is.na(z$top) && z$top <= to_cm, .f88_b_units(x, kind))
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

# The drainage recorded at the site (drainage, then drainage_class) as the
# two ends of its class on the eight classes of soilKey's reader
# (.drainage_class(): 1 excessively ... 4 well, 5 moderately well, 6
# imperfectly or somewhat poorly, 7 poorly, 8 very poorly; English or
# Portuguese; a range "imperfectly to very poorly drained" gives 6 and 8).
# The codes E, SE, W, MW, I, SP, P, VP and "undrained" / "not drained"
# (very poorly) are read too. NA when not recorded or not read.
.f88_drainage_ends <- function(x) {
  d <- .f88_site(x, "drainage")
  if (is.na(d) || !nzchar(trimws(as.character(d)))) d <- .f88_site(x, "drainage_class")
  if (is.na(d) || !nzchar(trimws(as.character(d)))) return(c(drier = NA_integer_, wetter = NA_integer_))
  t <- trimws(as.character(d))
  codes <- c(E = "excessively drained", SE = "somewhat excessively drained", W = "well drained",
             MW = "moderately well drained", I = "imperfectly drained", SP = "somewhat poorly drained",
             P = "poorly drained", VP = "very poorly drained")
  if (toupper(t) %in% names(codes)) t <- codes[[toupper(t)]]
  if (grepl("^(un|not |non[- ]?|nao )drained|^undrain|^nao drenad", tolower(t))) t <- "very poorly drained"
  c(drier = .drainage_class(t, "drier"), wetter = .drainage_class(t, "wetter"))
}


# --- Histosols (p. 75) ---------------------------------------------------------------------------

# The drainage of the Folic and Fibric Histosols (Hugo, 2026-11-09): "well" (HSl, well drained and never saturated for more
# than a few days) is a site recorded well drained or better and not
# artificially drained; "very_poor" (HSf, very poor drainage or undrained)
# is a site recorded very poorly drained (or undrained) and not
# artificially drained. A range is read by both ends: undecided when it
# spans the limit. Not recorded: undecided (both units hinge on it).
drainage_fao88 <- function(pedon, class = c("well", "very_poor")) {
  class <- match.arg(class)
  x <- .f88_ctx(pedon)
  pg <- "75"
  if (!x$n) return(.f88_key_result(x, "drainage", FALSE, pg))
  e <- .f88_drainage_ends(x)
  art <- isTRUE(.f88_site_lgl(x, "artificially_drained"))
  # both ends of a range must meet the class; a range across it is undecided
  value <- if (art) FALSE else if (anyNA(e)) NA else if (class == "well") {
    if (e[["wetter"]] <= 4L) TRUE else if (e[["drier"]] >= 5L) FALSE else NA
  } else {
    if (e[["drier"]] == 8L) TRUE else if (e[["wetter"]] < 8L) FALSE else NA
  }
  dc <- if (anyNA(e)) NA_character_ else paste(unique(e), collapse = "-")
  .f88_key_result(x, "drainage", value, pg,
                  missing = if (is.na(value)) "drainage_class",
                  flags = if (art) "artificially_drained",
                  evidence = list(drainage = dc, class = class))
}

# Fibric material dominant to `depth` cm (HSf): every layer overlapping
# 0-`depth` cm is organic and fibric, read as the fibric material of Soil
# Taxonomy that the legend cites: rubbed fibre content (fiber_content_rubbed_pct)
# 40 % or more; else a von Post index of 4 or less, flagged; a rubbed fibre
# content below 40 %, or a von Post index of 5 or more, is not fibric; none
# recorded, undecided (soilKey's reading, accepted by Hugo on 2026-11-09). A mineral
# layer in the window fails.
fibric_fao88 <- function(pedon, depth = 35) {
  x <- .f88_ctx(pedon)
  pg <- "75"
  if (!x$n) return(.f88_key_result(x, "fibric", FALSE, pg))
  org <- .f88_organic_layers(x)$value
  fr <- .f88_num(x, "fiber_content_rubbed_pct"); vp <- .f88_num(x, "von_post_index")
  use_vp <- is.na(fr) & !is.na(vp)
  fib <- ifelse(!is.na(fr), fr >= 40, ifelse(use_vp, vp <= 4, NA))
  v <- .f88_and(org, fib)
  w <- .fao_in_window(x$h, v, 0, depth, "throughout")
  .f88_key_result(x, "fibric", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) c("fiber_content_rubbed_pct", "von_post_index")),
                  flags = c(w$flags, if (any(use_vp[w$layers %||% integer(0)])) "von_post_for_fibre_content"),
                  evidence = list(depth = depth))
}


# --- Anthrosols (pp. 75-76) ----------------------------------------------------------------------

# Aric (ATa): remnants of diagnostic horizons from deep cultivation, read on
# the site record as the Anthrosols gate reads deep cultivation
# (deep_cultivation_kind, or mechanical_mixing_depth_cm of 50 or more); not
# recorded, absent.
aric_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "75"
  if (!x$n) return(.f88_key_result(x, "aric", FALSE, pg))
  dk <- .f88_site(x, "deep_cultivation_kind")
  dk <- !is.na(dk) && nzchar(trimws(as.character(dk))) && !grepl("^(none|no)$", tolower(trimws(as.character(dk))))
  md <- suppressWarnings(as.numeric(.f88_site(x, "mechanical_mixing_depth_cm")))
  deep <- dk || (!is.na(md) && md >= 50)
  .f88_key_result(x, "aric", deep, pg,
                  flags = if (!deep) "deep_cultivation_not_recorded_absent")
}

# Cumulic (ATc): fine sediments more than `min_cm` thick from long irrigation
# or a man-made raising of the surface. A layer is such a sediment when its
# layer_origin names man-made sediments in the shared vocabulary of the
# anthropic record (.F88_MAN_SEDIMENT_RE: irrigation, irragric, terric,
# raised, man-made), or its silting_sieve_test is positive;
# the run of such layers from the surface must be more than `min_cm` thick
# and every layer of it sandy loam or finer (the number of ch. VI p. 61 for
# "fine"; soilKey's reading, accepted by Hugo on 2026-11-09). Not recorded: absent.
cumulic_fao88 <- function(pedon, min_cm = 50) {
  x <- .f88_ctx(pedon)
  pg <- "76"
  if (!x$n) return(.f88_key_result(x, "cumulic", FALSE, pg))
  lo <- .f88_chr(x, "layer_origin")
  st <- .f88_chr(x, "silting_sieve_test")
  sed <- (!is.na(lo) & grepl(.F88_MAN_SEDIMENT_RE, lo)) | (!is.na(st) & grepl("^pos", st))
  sed[x$rock] <- FALSE
  first <- which(!is.na(x$top))[1L]
  run <- Filter(function(r) r$layers[1L] == first, .f88_run_list(x, sed))
  if (!length(run) || run[[1L]]$thickness <= min_cm)
    return(.f88_key_result(x, "cumulic", FALSE, pg,
                           flags = if (!any(sed)) "man_made_sediments_not_recorded_absent"))
  L <- run[[1L]]$layers
  fine <- .f88_all(.fao_sandy_loam_or_finer(x$h)[L])
  .f88_key_result(x, "cumulic", fine, pg, layers = L,
                  missing = if (is.na(fine)) c("clay_pct", "silt_pct"),
                  evidence = list(thickness_cm = run[[1L]]$thickness))
}


# --- Leptosols (p. 76) ---------------------------------------------------------------------------

# Lithic (LPq): continuous hard rock (continuous_hard_rock_fao88()) or a
# continuous cemented layer (the cemented layers of the Leptosols gate)
# within `depth` cm.
lithic_fao88 <- function(pedon, depth = 10) {
  x <- .f88_ctx(pedon)
  pg <- "76"
  if (!x$n) return(.f88_key_result(x, "lithic", FALSE, pg))
  rock <- continuous_hard_rock_fao88(pedon, max_top_cm = depth)
  cem <- .f88_cemented(x)
  cemv <- which(cem$value & !x$rock & !is.na(x$top) & x$top <= depth)
  value <- .fao_or(rock$passed, length(cemv) > 0L)
  .f88_key_result(x, "lithic", value, pg, layers = c(.f88_ctx_rows(x, rock), cemv),
                  missing = rock$missing, flags = rock$evidence$flags,
                  evidence = list(depth = depth))
}

# Rendzic (LPk): a mollic A that contains, or lies directly on, calcareous
# material with a CaCO3 equivalent above `cce_gt` %: a layer of the A, or the
# layer right below it, a calcareous rock included.
rendzic_fao88 <- function(pedon, cce_gt = 40) {
  x <- .f88_ctx(pedon)
  pg <- "76"
  if (!x$n) return(.f88_key_result(x, "rendzic", FALSE, pg))
  mo <- .f88_status(.f88_hz_dark_a(x, umbric = FALSE))
  a <- .f88_a_run(x)
  if (isFALSE(mo) || is.null(a))
    return(.f88_key_result(x, "rendzic", FALSE, pg, notes = "no mollic A"))
  k <- max(a$layers) + 1L
  zone <- c(a$layers, if (k <= x$n) k)
  cc <- .f88_caco3(x)
  cv <- .f88_cmp(cc$value, ">", cce_gt)
  # a calcareous rock right below the A (Hugo, 2026-11-09): CaCO3 measured on
  # it, else limestone, chalk, marble or dolomite recorded as its rock or the
  # site's parent material, flagged
  rock_flag <- character(0)
  if (k <= x$n && x$rock[k] && is.na(cv[k])) {
    rt <- c(.f88_chr(x, "rock_type")[k], .f88_chr(x, "parent_material")[k],
            tolower(as.character(.f88_site(x, "parent_material"))), tolower(as.character(.f88_site(x, "rock_type"))))
    calc_rock <- any(!is.na(rt) & grepl("limestone|chalk|marble|dolomit|calcar|calc\u00e1r|marmore|m\u00e1rmore|giz", rt))
    # a rock type not recorded is absent (a field observation)
    cv[k] <- calc_rock
    if (calc_rock) rock_flag <- "calcareous_rock_from_record"
  }
  calc <- .f88_any(cv[zone])
  value <- .fao_and(mo, calc)
  .f88_key_result(x, "rendzic", value, pg, layers = zone,
                  missing = c(if (is.na(calc)) "caco3_pct", if (is.na(mo)) .f88_hz_dark_a(x, FALSE)$missing),
                  flags = c(.f88_flags(cc$flags, zone), rock_flag),
                  evidence = list(mollic_a = mo, calcareous_gt = calc, cce_gt = cce_gt))
}


# --- base saturation -------------------------------------------------------------------------------

# Base saturation (NH4OAc) below `lt` % over a window: "throughout"
# `top`-`bottom` cm (the Dystric Vertisols, Fluvisols, Gleysols and Regosols,
# "at least from 20 to 50 cm": every layer overlapping 20-50 cm) or in
# "some" layer of it (the Dystric Leptosols, no depth in the key: some part
# of the soil within 125 cm, ch. VI p. 45 and p. 42). H and O layers are not
# read, as for the Phaeozems.
bs_below_fao88 <- function(pedon, lt = 50, top = 20, bottom = 50, mode = c("throughout", "some")) {
  mode <- match.arg(mode)
  x <- .f88_ctx(pedon)
  pg <- "76-80"
  if (!x$n) return(.f88_key_result(x, "bs_below", FALSE, pg))
  bs <- .f88_bs(x)
  org <- !is.na(x$master) & grepl("^(H|O)", x$master)
  w <- .fao_in_window(x$h, .f88_cmp(bs$value, "<", lt), top, bottom, mode, subset = !org)
  sel <- w$layers %||% integer(0)
  .f88_key_result(x, "bs_below", w$value, pg, layers = sel,
                  missing = c(w$missing, if (is.na(w$value)) "bs_pct"),
                  flags = c(w$flags, .f88_flags(bs$flags, sel)),
                  notes = w$note, evidence = list(lt = lt, window = c(top, bottom), mode = mode))
}

# Base saturation (NH4OAc) below `lt` % in at least some part of a B starting
# within `to_cm`: the layers of the upper B of `kind` (.f88_b_layers_kind();
# the Humic Ferralsols, the B within 100 cm; the Humic Nitisols and Dystric
# Podzoluvisols, the argic B within 125 cm; the Humic and Dystric Cambisols,
# the cambic B).
b_bs_below_fao88 <- function(pedon, kind = .F88_B_KINDS, lt = 50, to_cm = 125) {
  kind <- match.arg(kind)
  x <- .f88_ctx(pedon)
  pg <- "82-89"
  if (!x$n) return(.f88_key_result(x, "b_bs_below", FALSE, pg))
  bs <- .f88_bs(x)
  b <- .f88_b_eval(x, kind, to_cm, function(L) .f88_any(.f88_cmp(bs$value[L], "<", lt)))
  L <- b$layers
  if (b$none) return(.f88_key_result(x, "b_bs_below", FALSE, pg, notes = sprintf("no %s", kind)))
  value <- b$value
  .f88_key_result(x, "b_bs_below", value, pg, layers = L,
                  missing = if (is.na(value)) c("bs_pct", "diagnostic_b"),
                  flags = c(b$flags, .f88_flags(bs$flags, L)),
                  evidence = list(kind = kind, lt = lt, to_cm = to_cm, bs = bs$value[L]))
}


# --- Andosols (p. 79) ---------------------------------------------------------------------------

# Vitric (ANz): lacking a smeary consistence, or lacking a texture of silt
# loam or finer as the weighted average of the horizons within `depth` cm, or
# both. Smeary: smeary_fao88() in a layer starting within `depth` cm; with
# no smeary record in those layers the smeary item is undecided, so the unit
# passes only on a texture coarser than silt loam (Hugo, 2026-11-09). The
# texture: the USDA class of
# the thickness-weighted clay and sand of the mineral layers over 0-`depth`
# cm; silt loam or finer is SIL, SI, SCL, CL, SICL, SC, SIC or C (S, LS,
# SL and L are coarser; Hugo, 2026-11-09).
.F88_SILT_LOAM_OR_FINER <- c("SIL", "SI", "SCL", "CL", "SICL", "SC", "SIC", "C")
vitric_fao88 <- function(pedon, depth = 100) {
  x <- .f88_ctx(pedon)
  pg <- "79"
  if (!x$n) return(.f88_key_result(x, "vitric", FALSE, pg))
  sm <- smeary_fao88(pedon, max_top_cm = depth)
  sc <- .fao_col(x$h, "smeary_consistence", "logical"); ti <- .f88_num(x, "thixotropic_index")
  rec <- any((!is.na(sc) | !is.na(ti)) & !is.na(x$top) & x$top <= depth)
  # not recorded: undecided, unless the texture alone decides (Hugo, 2026-11-09)
  no_smeary <- if (isTRUE(sm$passed)) FALSE else if (rec) TRUE else NA
  ho <- !is.na(x$master) & grepl("^(H|O)", x$master)
  hm <- x$h[!ho, , drop = FALSE]
  p <- .fao_psd(hm)
  cl <- .fao_weighted(hm, "clay_pct", 0, depth, values = p$clay)
  sa <- .fao_weighted(hm, "sand_pct", 0, depth, values = p$sand)
  cls <- if (is.na(cl$value) || is.na(sa$value)) NA_character_ else
    texture_class_from_pct(cl$value, 100 - cl$value - sa$value, sa$value)
  fine <- if (is.na(cls)) NA else cls %in% .F88_SILT_LOAM_OR_FINER
  value <- .fao_or(no_smeary, .f88_not(fine))
  .f88_key_result(x, "vitric", value, pg,
                  missing = if (is.na(value)) unique(c(sm$missing, cl$missing, sa$missing, "clay_pct", "sand_pct")),
                  flags = c(sm$evidence$flags, cl$flags, sa$flags),
                  evidence = list(smeary = sm$passed, texture_class_0_100 = cls,
                                  clay_weighted = cl$value, sand_weighted = sa$value))
}


# --- Arenosols (p. 80) -------------------------------------------------------------------------

# Albic (ARa): an albic E at least `min_cm` thick whose upper limit is within
# `max_top_cm` cm.
albic_e_thick_fao88 <- function(pedon, min_cm = 50, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "albic_e_thick", FALSE, pg))
  r <- .f88_hz_albic_e(x)
  u <- Filter(function(z) !is.na(z$top) && z$top <= max_top_cm, r$units)
  th <- vapply(u, function(z) z$thickness, 0)
  st <- vapply(u, function(z) isTRUE(z$status), NA)
  thick <- th >= min_cm
  value <- if (any(thick & st)) TRUE else if (any(thick)) NA else FALSE
  lay <- unlist(lapply(u[thick], `[[`, "layers"))
  .f88_key_result(x, "albic_e_thick", value, pg, layers = lay %||% integer(0),
                  missing = if (is.na(value)) r$missing,
                  flags = r$flags, evidence = list(thickness_cm = th, min_cm = min_cm))
}

# Luvic (ARl): a clay increase of `increase` points or more (default 10: per
# cent points), or lamellae of clay accumulation, within `max_top_cm` cm. The
# increase is that of a mineral layer starting within `max_top_cm` cm over a
# mineral layer above it (soilKey's reading, accepted by Hugo on 2026-11-09: no vertical
# distance is printed). Lamellae: lamellae_cumulative_cm above 0, or a
# designation with lamellae (E&Bt, the lamellae suffix), flagged; not
# recorded, absent.
clay_increase_or_lamellae_fao88 <- function(pedon, increase = 3, max_top_cm = 125) {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "clay_increase_or_lamellae", FALSE, pg))
  org <- .f88_organic_layers(x)$value
  ho <- (!is.na(x$master) & grepl("^(H|O)", x$master)) | (org %in% TRUE)
  ok <- !ho & !x$rock & !is.na(x$top)
  clay <- .f88_num(x, "clay_pct")
  inc <- rep(FALSE, x$n)
  for (i in which(ok & x$top <= max_top_cm)) {
    js <- which(ok & seq_len(x$n) < i)
    if (!length(js)) next
    inc[i] <- .f88_any(.f88_cmp(clay[i] - clay[js], ">=", increase))
  }
  lc <- .f88_num(x, "lamellae_cumulative_cm")
  ld <- !is.na(x$desg) & grepl("&B|lam", x$desg)
  lam <- ok & !is.na(x$top) & x$top <= max_top_cm & ((!is.na(lc) & lc > 0) | ld)
  value <- .fao_or(.f88_any(inc[ok & x$top <= max_top_cm]), any(lam))
  .f88_key_result(x, "clay_increase_or_lamellae", value, pg,
                  layers = which(inc %in% TRUE | lam),
                  missing = if (is.na(value)) "clay_pct",
                  flags = if (any(lam & ld & is.na(lc))) "lamellae_from_designation",
                  evidence = list(increase = increase, max_top_cm = max_top_cm))
}

# The colour of the layer right below the A (ARo): moist chroma of
# `chroma_min` or more, or a hue redder than `hue` (Q24: the moist colour).
# With no A at the surface, the first mineral layer is read.
below_a_colour_fao88 <- function(pedon, chroma_min = 5, hue = "10YR") {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "below_a_colour", FALSE, pg))
  a <- .f88_a_run(x)
  k <- if (is.null(a)) which(!x$rock & !(!is.na(x$master) & grepl("^(H|O)", x$master)))[1L] else max(a$layers) + 1L
  if (is.na(k) || k > x$n || x$rock[k])
    return(.f88_key_result(x, "below_a_colour", FALSE, pg, notes = "no layer below the A"))
  col <- .f88_colours(x)
  value <- .fao_or(.f88_cmp(col$cm[k], ">=", chroma_min), .fao_hue_cmp(col$hue[k], hue, "redder"))
  .f88_key_result(x, "below_a_colour", value, pg, layers = k,
                  missing = if (is.na(value)) c("munsell_chroma_moist", "munsell_hue_moist"),
                  evidence = list(chroma = col$cm[k], hue = col$hue[k]))
}

# Cambic (ARb): the colouring or alteration of a cambic B in the layer right
# below the A (ch. VI p. 47): item 5 of the cambic B (stronger chroma, redder
# hue or more clay than the layer below, carbonates removed, or structure
# without carbonates), read without the texture the Arenosols lack, on a
# layer that may hold a B (not C, not rock). With no A at the surface, the
# first mineral layer is read.
cambic_alteration_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "80"
  if (!x$n) return(.f88_key_result(x, "cambic_alteration", FALSE, pg))
  a <- .f88_a_run(x)
  k <- if (is.null(a)) which(!x$rock & !(!is.na(x$master) & grepl("^(H|O)", x$master)))[1L] else max(a$layers) + 1L
  if (is.na(k) || k > x$n || x$rock[k])
    return(.f88_key_result(x, "cambic_alteration", FALSE, pg, notes = "no layer below the A"))
  cr <- .f88_hz_cambic_b(x)$evidence$criteria
  alt <- if (is.data.frame(cr) && nrow(cr) == x$n) as.logical(cr$alteration[k]) else NA
  value <- .fao_and(.f88_b_layer(x)[k], alt)
  .f88_key_result(x, "cambic_alteration", value, pg, layers = k,
                  missing = if (is.na(value)) c("munsell_chroma_moist", "munsell_hue_moist", "clay_pct", "caco3_pct"),
                  evidence = list(layer_may_hold_b = .f88_b_layer(x)[k], alteration = alt))
}


# --- Podzols (p. 81) -----------------------------------------------------------------------------

# Carbic (PZc): a layer of the spodic B with dispersed organic matter
# (organic carbon above 0, or an h in its designation) that does not turn
# redder on ignition (.fao_ignition_redder(): ignition_redder, else free
# iron below 0.5 %, flagged).
carbic_fao88 <- function(pedon) {
  x <- .f88_ctx(pedon)
  pg <- "81"
  if (!x$n) return(.f88_key_result(x, "carbic", FALSE, pg))
  oc <- .f88_num(x, "oc_pct")
  hs <- !is.na(x$desg) & grepl("^B[a-z]*h", x$desg)
  dom <- ifelse(!is.na(oc), oc > 0, ifelse(hs, TRUE, NA))
  ig <- .fao_ignition_redder(x$h)
  v <- .f88_and(dom, .f88_not(ig$value))
  b <- .f88_b_eval(x, "spodic_b", 125, function(L) .f88_any(v[L]))
  L <- b$layers
  if (b$none) return(.f88_key_result(x, "carbic", FALSE, pg, notes = "no spodic B"))
  value <- b$value
  .f88_key_result(x, "carbic", value, pg, layers = L[v[L] %in% TRUE],
                  missing = if (is.na(value)) c(ig$missing, "oc_pct"),
                  flags = c(b$flags, .f88_flags(ig$flags, L)))
}

# Ferric (PZf): free iron / organic carbon (fe_dcb_pct / oc_pct) of `min` or
# more in every layer of the spodic B.
spodic_fe_oc_fao88 <- function(pedon, min = 6) {
  x <- .f88_ctx(pedon)
  pg <- "81"
  if (!x$n) return(.f88_key_result(x, "spodic_fe_oc", FALSE, pg))
  fe <- .f88_num(x, "fe_dcb_pct"); oc <- .f88_num(x, "oc_pct")
  # both recorded: oc 0 with iron is an infinite ratio, 0 / 0 no ratio (fails)
  r <- ifelse(is.na(fe) | is.na(oc), NA_real_,
              ifelse(oc > 0, fe / oc, ifelse(fe > 0, Inf, 0)))
  b <- .f88_b_eval(x, "spodic_b", 125, function(L) .f88_all(.f88_cmpr(r[L], ">=", min)))
  L <- b$layers
  if (b$none) return(.f88_key_result(x, "spodic_fe_oc", FALSE, pg, notes = "no spodic B"))
  value <- b$value
  .f88_key_result(x, "spodic_fe_oc", value, pg, layers = L,
                  missing = if (is.na(value)) c("fe_dcb_pct", "oc_pct"),
                  flags = b$flags, evidence = list(ratio = r[L], min = min))
}

# Cambic (PZb): no albic E, or only a thin (`albic_max_cm` cm or less) and
# discontinuous one (boundary_topography broken or discontinuous; not
# recorded, continuous: a field observation); and no layer of the spodic B
# visibly more enriched in organic carbon (an h in its designation, Bh or
# Bhs: the field record of that enrichment; not recorded, absent).
podzol_cambic_fao88 <- function(pedon, albic_max_cm = 2) {
  x <- .f88_ctx(pedon)
  pg <- "81"
  if (!x$n) return(.f88_key_result(x, "podzol_cambic", FALSE, pg))
  ae <- .f88_hz_albic_e(x)$units
  bt <- .f88_chr(x, "boundary_topography")
  ok_e <- vapply(ae, function(u) {
    thin <- u$thickness <= albic_max_cm
    disc <- any(!is.na(bt[u$layers]) & grepl("broken|discontin|interromp", bt[u$layers]))
    if (thin && disc) TRUE else if (isTRUE(u$status)) FALSE else NA
  }, NA)
  e_ok <- .f88_all(ok_e)
  b <- .f88_b_layers_kind(x, "spodic_b", 125)
  L <- b$layers
  hs <- !is.na(x$desg) & grepl("^B[a-z]*h", x$desg)
  no_h <- !any(hs[L])
  value <- .fao_and(e_ok, no_h)
  .f88_key_result(x, "podzol_cambic", value, pg, layers = L,
                  missing = if (is.na(value)) c("munsell_value_dry", "boundary_topography"),
                  evidence = list(albic_thin_discontinuous = e_ok, no_carbon_enriched_subhorizon = no_h))
}


# --- Plinthosols (pp. 81-82) ---------------------------------------------------------------------

# Dystric (PTd): base saturation (NH4OAc) below `lt` % throughout the upper
# `depth` cm of the plinthite horizon: the upper run of layers with 25 %
# plinthite or more, 15 cm thick or more, starting within 125 cm (the
# horizon of the Plinthosols gate); a horizon thinner than `depth` cm is read
# whole. The ochric A is asked in the units file.
plinthite_bs_fao88 <- function(pedon, lt = 50, depth = 50) {
  x <- .f88_ctx(pedon)
  pg <- "82"
  if (!x$n) return(.f88_key_result(x, "plinthite_bs", FALSE, pg))
  pl <- plinthite_fao88(pedon, min_pct = 25, min_thickness_cm = 15)
  pv <- .f88_values(pl, x$n)
  runs <- Filter(function(r) !is.na(r$top) && r$top <= 125, .f88_run_list(x, pv %in% TRUE))
  if (!length(runs))
    return(.f88_key_result(x, "plinthite_bs", if (any(is.na(pv))) NA else FALSE, pg,
                           missing = if (any(is.na(pv))) "plinthite_pct", notes = "no plinthite horizon"))
  r <- runs[[1L]]
  bot <- min(r$top + depth, r$bottom)
  sub <- seq_len(x$n) %in% r$layers
  bs <- .f88_bs(x)
  w <- .fao_in_window(x$h, .f88_cmp(bs$value, "<", lt), r$top, bot, "throughout", subset = sub)
  .f88_key_result(x, "plinthite_bs", w$value, pg, layers = w$layers %||% integer(0),
                  missing = c(w$missing, if (is.na(w$value)) "bs_pct"),
                  flags = c(w$flags, .f88_flags(bs$flags, r$layers)),
                  evidence = list(plinthite_top_cm = r$top, window = c(r$top, bot), lt = lt))
}


# --- inside a B ---------------------------------------------------------------------------------

# A property in at least some part of a B starting within `to_cm`: geric
# properties in the ferralic B (FRg), ferralic properties in the cambic B
# (CMo). The per-layer answers of the property (evidence$values) on the
# layers of the upper B of `kind`.
property_in_b_fao88 <- function(pedon, property = c("geric", "ferralic"), kind = .F88_B_KINDS,
                                to_cm = 125) {
  property <- match.arg(property); kind <- match.arg(kind)
  x <- .f88_ctx(pedon)
  pg <- if (property == "geric") "82" else "89"
  if (!x$n) return(.f88_key_result(x, "property_in_b", FALSE, pg))
  r <- if (property == "geric") geric_properties_fao88(pedon) else ferralic_properties_fao88(pedon)
  v <- .f88_values(r, x$n)
  b <- .f88_b_eval(x, kind, to_cm, function(L) .f88_any(v[L]))
  L <- b$layers
  if (b$none) return(.f88_key_result(x, "property_in_b", FALSE, pg, notes = sprintf("no %s", kind)))
  value <- b$value
  .f88_key_result(x, "property_in_b", value, pg, layers = L[v[L] %in% TRUE],
                  missing = if (is.na(value)) r$missing,
                  flags = c(b$flags, r$evidence$flags),
                  evidence = list(property = property, kind = kind, to_cm = to_cm))
}

# The rubbed colour of a B (Q24: munsell_*_rubbed, else the moist colour,
# flagged), in every layer of the upper B of `kind` starting within 125 cm
# (Hugo, 2026-11-09: "a red B" read as the B as a whole):
#   rhodic (FRr, NTr): hue redder than 5YR, moist value below 4, dry value
#     no more than one unit above the moist one (a dry value not recorded
#     leaves it undecided, flagged; Hugo, 2026-11-09);
#   xanthic (FRx): hue 7.5YR or yellower, moist value 4 or more, moist
#     chroma 5 or more;
#   chromic (LVx, CMx): hue 7.5YR with a chroma above 4, or a hue redder
#     than 7.5YR (the moist colour, Q24).
b_colour_fao88 <- function(pedon, colour = c("rhodic", "xanthic", "chromic"), kind = .F88_B_KINDS) {
  colour <- match.arg(colour); kind <- match.arg(kind)
  x <- .f88_ctx(pedon)
  pg <- switch(colour, rhodic = "82, 86", xanthic = "82", chromic = "88-89")
  if (!x$n) return(.f88_key_result(x, "b_colour", FALSE, pg))
  rb <- .fao_colour_rubbed(x$h)
  col <- .f88_colours(x)
  hue <- rb$hue; val <- rb$value; chr <- rb$chroma
  dry_flag <- character(0)
  v <- switch(colour,
    rhodic = {
      # a dry value not recorded leaves the item undecided (Hugo, 2026-11-09)
      dry <- col$vd
      dry_flag <- "dry_value_not_recorded"
      .f88_and(.fao_hue_cmp(hue, "5YR", "redder"), .f88_cmp(val, "<", 4),
               .f88_cmp(dry - val, "<=", 1))
    },
    xanthic = .f88_and(.fao_hue_cmp(hue, "7.5YR", "as_yellow"), .f88_cmp(val, ">=", 4),
                       .f88_cmp(chr, ">=", 5)),
    chromic = {
      eq <- .fao_hue_rank(hue) == .fao_hue_rank("7.5YR")
      .f88_or(.f88_and(eq, .f88_cmp(chr, ">", 4)), .fao_hue_cmp(hue, "7.5YR", "redder"))
    })
  b <- .f88_b_eval(x, kind, 125, function(L) .f88_all(v[L]))
  L <- b$layers
  if (b$none) return(.f88_key_result(x, "b_colour", FALSE, pg, notes = sprintf("no %s", kind)))
  value <- b$value
  if (!any(is.na(col$vd[L]))) dry_flag <- character(0)
  .f88_key_result(x, "b_colour", value, pg, layers = L,
                  missing = if (is.na(value)) c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist",
                                                if (colour == "rhodic") "munsell_value_dry"),
                  flags = c(b$flags, .f88_flags(rb$flags, L), dry_flag),
                  evidence = list(colour = colour, kind = kind, hue = hue[L], value = val[L], chroma = chr[L]))
}


# --- Planosols (pp. 82-83) -----------------------------------------------------------------------

# Dystric (PLd): base saturation (NH4OAc) below `lt` % in at least a part of
# the slowly permeable horizon within `to_cm` cm: the layer below the E that
# the Planosols gate found, with the adjoining layers below it of the same
# master horizon (B, C ...), starting within `to_cm` cm (soilKey's reading, accepted by Hugo on 2026-11-09).
slow_horizon_bs_fao88 <- function(pedon, lt = 50, to_cm = 125) {
  x <- .f88_ctx(pedon)
  pg <- "83"
  if (!x$n) return(.f88_key_result(x, "slow_horizon_bs", FALSE, pg))
  g <- planosols_gate_fao88(pedon)
  rows <- .f88_ctx_rows(x, g)
  if (!length(rows))
    return(.f88_key_result(x, "slow_horizon_bs", if (is.na(g$passed)) NA else FALSE, pg,
                           missing = g$missing, notes = "no slowly permeable horizon"))
  k <- max(rows)
  m1 <- substr(x$master[k] %||% NA_character_, 1L, 1L)
  L <- k
  j <- k + 1L
  while (j <= x$n && !x$rock[j] && !is.na(m1) && !is.na(x$master[j]) &&
         substr(x$master[j], 1L, 1L) == m1 && !is.na(x$top[j]) && x$top[j] <= to_cm) {
    L <- c(L, j); j <- j + 1L
  }
  L <- L[!is.na(x$top[L]) & x$top[L] <= to_cm]
  bs <- .f88_bs(x)
  value <- .f88_any(.f88_cmp(bs$value[L], "<", lt))
  if (isTRUE(value) && !isTRUE(g$passed)) value <- NA
  .f88_key_result(x, "slow_horizon_bs", value, pg, layers = L,
                  missing = if (is.na(value)) c("bs_pct", g$missing),
                  flags = .f88_flags(bs$flags, L), evidence = list(bs = bs$value[L], lt = lt))
}


# --- examples of the units ---------------------------------------------------------------------

# The units whose gate has a printed number (a depth, a thickness, a
# percentage, a ratio, a Munsell limit), each with a near miss that crosses
# it (make_fao_example(code, near_miss = TRUE)).
.F88_UNIT_NEAR_MISS <- c(
  "HSi", "HSt", "HSf", "ATc", "LPq", "LPi", "LPk", "LPd", "VRy", "VRk", "VRd", "FLt", "FLs", "FLc", "FLd",
  "SCi", "SCg", "SCy", "SCk", "SCn", "GLi", "GLt", "GLk", "GLd", "ANi", "ANg", "ANz", "ARg", "ARa",
  "ARc", "ARl", "ARo", "RGi", "RGy", "RGc", "RGd", "PZi", "PZg", "PZf", "PZb", "PTu", "PTd", "FRp",
  "FRg", "FRu", "FRr", "FRx", "PLi", "PLd", "SNg", "SNj", "SNy", "SNk", "GRg", "CHg", "PHg", "PHj",
  "PHc", "PDi", "PDg", "PDj", "PDd", "GYp", "CLp", "NTu", "NTr", "ALp", "ALg", "ALj", "ALu", "ACp",
  "ACg", "ACu", "LVg", "LVj", "LVk", "LVx", "LXp", "LXg", "LXj", "CMi", "CMg", "CMu", "CMc", "CMo",
  "CMd", "CMx")

# The 153 unit codes in the key's order (from the rule files).
.f88_unit_codes <- function() {
  u <- load_rules("fao1988")$units
  unlist(lapply(.F88_GROUPINGS$code, function(g) vapply(u[[g]], function(e) as.character(e$code), "")),
         use.names = FALSE)
}

# A pedon with its horizons edited: `rows` keeps (and orders) some layers;
# the named arguments set whole columns (one value or one per layer);
# `site` sets site fields.
.f88x_edit <- function(p, rows = NULL, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  v <- list(...)
  for (k in names(v)) h[[k]] <- if (length(v[[k]]) == 1L) rep(v[[k]], nrow(h)) else v[[k]]
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# The same, setting values in layer `i` only.
.f88x_set <- function(p, i, ..., site = list()) {
  h <- as.data.frame(p$horizons)
  v <- list(...)
  for (k in names(v)) {
    if (!k %in% names(h)) h[[k]] <- NA
    if (is.logical(h[[k]]) && !is.logical(v[[k]])) h[[k]] <- as.vector(h[[k]], class(v[[k]])[1L])
    h[[k]][i] <- v[[k]]
  }
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# A layer `top`-`bottom` cm added at the base of the profile: the layers
# starting at `top` or deeper are dropped, the last one kept is stretched to
# `top`, and the new layer copies it with the values given.
.f88x_append <- function(p, top, bottom, ...) {
  h <- as.data.frame(p$horizons)
  h <- h[order(h$top_cm), , drop = FALSE]
  h <- h[h$top_cm < top, , drop = FALSE]
  h$bottom_cm[nrow(h)] <- top
  new <- h[nrow(h), , drop = FALSE]
  new$top_cm <- top; new$bottom_cm <- bottom
  v <- list(...)
  for (k in names(v)) {
    if (!k %in% names(h)) { h[[k]] <- NA; new[[k]] <- NA }
    if (is.logical(new[[k]]) && !is.logical(v[[k]])) {
      h[[k]] <- as.vector(h[[k]], class(v[[k]])[1L]); new[[k]] <- as.vector(new[[k]], class(v[[k]])[1L])
    }
    new[[k]] <- v[[k]]
  }
  h <- rbind(h, new)
  rownames(h) <- NULL
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

# Gley colours (with the reduction they show) and stagnic colours of a layer.
.f88x_gley <- function(p, i) {
  .f88x_set(p, i, munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1,
            mottle_colors = NA_character_, redoximorphic_features_pct = 0)
}
.f88x_stag <- function(p, i) {
  .f88x_set(p, i, rh_value = 17, munsell_value_moist = 5, munsell_chroma_moist = 1,
            mottle_colors = "7.5YR 5/6", redoximorphic_features_pct = 10)
}

# One synthetic profile per soil unit (and, with `nm`, the near miss of a
# unit whose gate has a printed number), built on the example of its
# grouping. Each example keys to its own unit, not provisionally; each near
# miss crosses one printed number of its unit gate and keys to another unit.
.fao88_unit_example <- function(code, nm = FALSE) {
  g <- substr(code, 1L, 2L)
  b <- .fao88_example(g, FALSE)
  E <- .f88x_edit; S <- .f88x_set; A <- .f88x_append
  gl <- .f88x_gley; st <- .f88x_stag
  perma <- function(p) A(p, if (nm) 201 else 200, 240, permafrost_temp_C = -1)
  deep <- function(p, X, ...) A(p, if (nm) X + 1 else X, X + 40, ...)
  # a horizon at X cm (X + 1 in the near miss) over a C that it is compared
  # with (the calcic and gypsic horizons ask more carbonate or gypsum than a
  # deeper layer)
  deep2 <- function(p, X, ..., below = list()) {
    t0 <- if (nm) X + 1 else X
    q <- A(p, t0, t0 + 30, ...)
    do.call(A, c(list(q, t0 + 30, t0 + 60), below))
  }
  calcic_at <- function(p, X) deep2(p, X, designation = "Bk", caco3_pct = 25,
                                    below = list(designation = "C", caco3_pct = 10))
  gypsic_at <- function(p, X) deep2(p, X, designation = "By", caso4_pct = 20, ec_dS_m = 2.5,
                                    below = list(designation = "C", caso4_pct = 2, ec_dS_m = 2.2))
  # the decided form of each grouping's example where its own data leave a
  # unit undecided (sulfur, ignition, delta pH, fibre content)
  clean <- switch(g,
    HS = function(p) E(p, s_total_pct = 0.05, fiber_content_rubbed_pct = c(10, 10, NA)),
    FL = , GL = function(p) E(p, s_total_pct = 0.05),
    SC = function(p) E(p, mg_cmol = 2),
    PZ = function(p) E(p, ignition_redder = TRUE),
    FR = function(p) E(p, ph_kcl = 4.2, munsell_value_dry = c(4, 4, 4, 4)),
    NT = function(p) E(p, munsell_value_dry = 4),
    function(p) p)
  b <- clean(b)
  # the Luvisol-like examples (argic B under an E) without an albic E, its
  # boundary gradual so that no Planosol is read
  noalb <- function(p) E(p, munsell_value_moist = c(4, 4, 4, 4, 5), munsell_chroma_moist = c(3, 3, 4, 4, 4),
                         munsell_value_dry = c(6, 5, 6, 6, 6), munsell_chroma_dry = c(3, 3, 4, 4, 4))
  out <- switch(code,
    # --- Histosols
    HSi = perma(b),
    HSt = deep(b, 125, designation = "Cg2", s_total_pct = 1.2),
    HSl = E(b, site = list(drainage_class = "well drained")),
    HSf = E(b, fiber_content_rubbed_pct = c(60, if (nm) 20 else 60, NA)),
    HSs = b,
    # --- Anthrosols
    ATa = E(b, layer_origin = NA_character_, artefacts_pct = 0, site = list(deep_cultivation_kind = "trenching")),
    ATf = E(b, layer_origin = c("plaggen", "plaggen", NA, NA), artefacts_pct = 0),
    ATc = E(b, layer_origin = c("anthropic irrigation sediment", "anthropic irrigation sediment", NA, NA),
            artefacts_pct = 0, top_cm = c(0, 25, if (nm) 50 else 55, 100),
            bottom_cm = c(25, if (nm) 50 else 55, 100, 140)),
    ATu = b,
    # --- Leptosols
    LPq = E(b, rows = c(1, 3), top_cm = c(0, if (nm) 11 else 10), bottom_cm = c(if (nm) 11 else 10, 60)),
    # a deep Leptosol (less than 20 % fine earth to 75 cm), permafrost at 200 cm
    LPi = perma(E(b, rows = c(1, 2), top_cm = c(0, 15), bottom_cm = c(15, 200), coarse_fragments_pct = 85)),
    LPk = E(b, munsell_value_moist = c(3, 6, NA), munsell_chroma_moist = c(2, 3, NA), munsell_value_dry = c(5, 7, NA),
            oc_pct = c(3, 0.4, NA), caco3_pct = c(20, if (nm) 40 else 60, NA), ph_h2o = 8, cec_cmol = c(25, 9, NA),
            structure_grade = c("strong", "structureless", NA), top_cm = c(0, 20, 28), bottom_cm = c(20, 28, 60)),
    LPm = E(b, munsell_value_moist = c(3, 5, NA), munsell_chroma_moist = c(2, 4, NA), munsell_value_dry = c(5, 6, NA),
            oc_pct = c(3, 0.4, NA), cec_cmol = c(25, 9, NA), top_cm = c(0, 20, 28), bottom_cm = c(20, 28, 60),
            structure_grade = c("strong", "structureless", NA)),
    LPu = E(b, munsell_value_moist = c(3, 5, NA), munsell_chroma_moist = c(2, 4, NA), munsell_value_dry = c(5, 6, NA),
            oc_pct = c(3, 0.4, NA), cec_cmol = c(25, 9, NA), bs_pct = c(30, 40, NA), ph_h2o = 5,
            top_cm = c(0, 20, 28), bottom_cm = c(20, 28, 60), structure_grade = c("strong", "structureless", NA)),
    LPd = E(b, bs_pct = c(60, if (nm) 50 else 40, NA), ph_h2o = 5.5),
    LPe = b,
    # --- Vertisols
    VRy = gypsic_at(b, 125),
    VRk = calcic_at(b, 125),
    VRd = E(b, bs_pct = c(40, if (nm) 50 else 40, 40, 60), ph_h2o = 5.5),
    VRe = b,
    # --- Fluvisols
    FLt = deep(b, 125, designation = "Cr", s_total_pct = 1.2, ph_h2o = 6),
    FLs = E(b, ec_dS_m = c(if (nm) 15 else 16, 4, 4, 4, 4), ph_h2o = 7.5),
    FLm = E(b, top_cm = c(0, 25, 40, 70, 100), bottom_cm = c(25, 40, 70, 100, 140), munsell_value_moist = c(2, 5, 4, 5, 4),
            munsell_chroma_moist = c(2, 4, 3, 4, 3), munsell_value_dry = c(4, 6, 5, 6, 5), oc_pct = c(2.5, 0.3, 0.9, 0.15, 0.6),
            cec_cmol = c(25, 8, 16, 6, 14), structure_grade = c("strong", "structureless", "structureless", "structureless", "structureless")),
    FLc = E(b, caco3_pct = c(0, if (nm) 0 else 5, 5, 5, 5), ph_h2o = 7.8),
    FLu = E(b, top_cm = c(0, 25, 40, 70, 100), bottom_cm = c(25, 40, 70, 100, 140), munsell_value_moist = c(2, 5, 4, 5, 4),
            munsell_chroma_moist = c(2, 4, 3, 4, 3), munsell_value_dry = c(4, 6, 5, 6, 5), oc_pct = c(2.5, 0.3, 0.9, 0.15, 0.6),
            cec_cmol = c(25, 8, 16, 6, 14), bs_pct = c(35, 60, 60, 60, 60), ph_h2o = 5.5,
            structure_grade = c("strong", "structureless", "structureless", "structureless", "structureless")),
    FLd = E(b, bs_pct = c(60, 40, if (nm) 50 else 40, 60, 60), ph_h2o = 5.5),
    FLe = b,
    # --- Solonchaks
    SCi = perma(b),
    SCg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    SCm = E(b, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 130), munsell_value_moist = c(3, 5, 6),
            munsell_chroma_moist = c(2, 5, 4), munsell_value_dry = c(5, 6, 7), oc_pct = c(2, 0.4, 0.2),
            structure_grade = c("strong", "moderate", "structureless")),
    SCy = gypsic_at(b, 125),
    SCk = calcic_at(b, 125),
    SCn = E(b, esp_pct = c(5, if (nm) 14 else 20, 5)),
    SCh = b,
    # --- Gleysols
    GLi = perma(b),
    GLt = deep(b, 125, designation = "Cgr", s_total_pct = 1.2, ph_h2o = 6),
    GLa = S(b, 2, al_ox_pct = 3, fe_ox_pct = 1.5, bulk_density_g_cm3 = 0.7, phosphate_retention_pct = 92,
            volcanic_glass_pct = 10),
    GLm = E(b, munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 5, 1), munsell_value_dry = c(5, 6, 6),
            oc_pct = c(3, 0.5, 0.3), structure_grade = c("strong", "moderate", "structureless")),
    GLu = E(b, munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 5, 1), munsell_value_dry = c(5, 6, 6),
            oc_pct = c(3, 0.5, 0.3), bs_pct = c(35, 60, 60), ph_h2o = 5.5,
            structure_grade = c("strong", "moderate", "structureless")),
    GLk = calcic_at(b, 125),
    GLd = E(b, bs_pct = c(60, 40, if (nm) 50 else 40), ph_h2o = 5.5),
    GLe = b,
    # --- Andosols (smeary, silt loam: not Vitric)
    ANi = , ANg = , ANm = , ANu = , ANh = {
      a <- E(b, smeary_consistence = TRUE, clay_pct = 20, silt_pct = 55, sand_pct = 25)
      switch(code,
        ANi = perma(a),
        ANg = deep(a, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5,
                   munsell_chroma_moist = 1),
        ANm = E(a, bs_pct = c(60, 55, 50), ph_h2o = 6.2),
        ANu = a,
        ANh = E(a, munsell_value_moist = c(4, 4, 4), munsell_chroma_moist = c(3, 4, 4),
                munsell_value_dry = c(5, 5, 5), oc_pct = c(1.5, 1, 0.5)))
    },
    ANz = if (nm) E(b, smeary_consistence = TRUE, clay_pct = 20, silt_pct = 55, sand_pct = 25) else b,
    # --- Arenosols
    ARg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    ARa = E(b, rows = c(1, 2, 2, 3), designation = c("A", "E", "C1", "C2"),
            top_cm = c(0, 20, if (nm) 69 else 70, 80), bottom_cm = c(20, if (nm) 69 else 70, 80, 150),
            munsell_value_moist = c(4, 6, 5, 6), munsell_chroma_moist = c(3, 1, 4, 4),
            munsell_value_dry = c(5, 7, 6, 7), munsell_chroma_dry = c(3, 1, 4, 4)),
    ARc = E(b, top_cm = c(0, 20, 80), caco3_pct = c(0, if (nm) 2 else 5, 5), ph_h2o = 7.8),
    ARl = E(b, clay_pct = c(4, 3, if (nm) 5 else 6), sand_pct = c(90, 92, if (nm) 91 else 90)),
    ARo = S(b, 2, munsell_chroma_moist = if (nm) 4 else 5),
    ARb = E(b, designation = c("A", "Bw", "C"), munsell_chroma_moist = c(3, 5, 3), cec_cmol = c(3, 5, 2)),
    ARh = b,
    # --- Regosols
    RGi = perma(E(b, caco3_pct = 0, ph_h2o = 6.5)),
    RGu = E(b, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 130), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 4, 4),
            munsell_value_dry = c(5, 6, 6), oc_pct = c(2, 0.3, 0.2), bs_pct = c(35, 85, 85),
            caco3_pct = 0, ph_h2o = c(5.5, 6.5, 6.5), structure_grade = c("strong", "structureless", "structureless")),
    RGy = E(b, caso4_pct = c(0, if (nm) 4 else 6, 4), caco3_pct = 0, ph_h2o = 7, ec_dS_m = 2.2),
    RGc = E(b, caco3_pct = c(0, if (nm) 2 else 3, 4)),
    RGd = E(b, caco3_pct = 0, bs_pct = c(60, if (nm) 50 else 40, 60), ph_h2o = 5.5),
    RGe = E(b, caco3_pct = 0, ph_h2o = 6.5),
    # --- Podzols
    PZi = perma(b),
    PZg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    PZc = S(b, 3, ignition_redder = FALSE, rh_value = 25, ferricyanide_test = "negative", childs_test = "negative"),
    PZf = E(b, fe_dcb_pct = c(0.2, 0.1, 3.0, if (nm) 2.2 else 2.4, 0.2), oc_pct = c(2, 0.4, 0.5, 0.4, 0.2),
            fe_pyro_pct = c(0.05, 0.02, 1.5, 1.2, 0.05)),
    PZb = E(b, designation = c("A", "E", "Bs1", "Bs2", "C"), top_cm = c(0, 10, if (nm) 13 else 12, 45, 80),
            bottom_cm = c(10, if (nm) 13 else 12, 45, 80, 130), boundary_topography = c(NA, "broken", NA, NA, NA)),
    PZh = b,
    # --- Plinthosols
    PTa = E(b, rows = c(1, 1, 2, 3, 4), designation = c("A", "E", "Bv1", "Bv2", "C"), top_cm = c(0, 10, 20, 50, 100),
            bottom_cm = c(10, 20, 50, 100, 150), munsell_value_moist = c(4, 6, 5, 5, 5), munsell_chroma_moist = c(3, 1, 6, 6, 6),
            munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR", "5YR"), munsell_value_dry = c(5, 7, 6, 6, 6),
            munsell_chroma_dry = c(3, 1, 6, 6, 6), plinthite_pct = c(0, 0, 35, 40, 10), mottle_colors = c(NA, NA, "10R 4/8", "10R 4/8", NA),
            redoximorphic_features_pct = c(0, 0, 35, 40, 10)),
    PTu = E(b, top_cm = c(0, 26, 50, 100), bottom_cm = c(26, 50, 100, 150), munsell_value_moist = c(3, 5, 5, 5),
            munsell_chroma_moist = c(2, 6, 6, 6), munsell_value_dry = c(5, 6, 6, 6),
            oc_pct = c(if (nm) 3.3 else 4, 1.5, 0.3, 0.2),
            structure_grade = c("strong", "weak", "structureless", "structureless")),
    PTd = E(b, bs_pct = c(30, 25, if (nm) 50 else 20, 20)),
    PTe = E(b, bs_pct = c(60, 60, 60, 60), ph_h2o = 6),
    # --- Ferralsols
    FRp = deep(b, 125, designation = "Bov", plinthite_pct = 5),
    FRg = E(b, ca_cmol = c(1, 0.1, 0.1, 0.1), mg_cmol = c(0.4, 0.05, 0.05, 0.05),
            exch_acidity_kcl_cmol = c(1, if (nm) 0.75 else 0.6, if (nm) 0.75 else 0.6, if (nm) 0.75 else 0.6),
            k_cmol = c(0.1, 0.02, 0.02, 0.02), na_cmol = c(0.02, 0.01, 0.01, 0.01)),
    FRu = E(b, top_cm = c(0, 30, 60, 110), bottom_cm = c(30, 60, 110, 180), munsell_value_moist = c(2, 3, 3, 3),
            munsell_chroma_moist = c(2, 6, 6, 6), munsell_value_dry = c(4, 4, 4, 4),
            oc_pct = c(3.5, if (nm) 0.5 else 1.0, 0.3, 0.3),
            structure_grade = c("strong", "weak", "weak", "weak")),
    FRr = E(b, munsell_value_dry = c(4, if (nm) 5 else 4, 4, 4)),
    FRx = E(b, munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "7.5YR"), munsell_value_moist = c(4, 5, 5, 5),
            munsell_chroma_moist = c(3, if (nm) 4 else 6, 6, 6), munsell_value_dry = c(5, 6, 6, 6)),
    FRh = E(b, munsell_hue_moist = c("10YR", "5YR", "5YR", "5YR"), munsell_value_moist = c(4, 4, 4, 4),
            munsell_chroma_moist = c(3, 4, 4, 4), munsell_value_dry = c(5, 5, 5, 5)),
    # --- Planosols
    PLi = perma(b),
    PLm = E(b, top_cm = c(0, 26, 40, 90), bottom_cm = c(26, 40, 90, 140), munsell_value_moist = c(3, 6, 5, 5), munsell_chroma_moist = c(2, 1, 2, 3),
            munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(2.5, 0.4, 0.4, 0.2), cec_cmol = c(18, 5, 14, 10),
            structure_grade = c("strong", "weak", "strong", "structureless")),
    PLu = E(b, top_cm = c(0, 26, 40, 90), bottom_cm = c(26, 40, 90, 140), munsell_value_moist = c(3, 6, 5, 5), munsell_chroma_moist = c(2, 1, 2, 3),
            munsell_value_dry = c(5, 7, 6, 6), oc_pct = c(2.5, 0.4, 0.4, 0.2), cec_cmol = c(18, 5, 14, 10),
            bs_pct = c(35, 55, 70, 75), ph_h2o = 5.5, structure_grade = c("strong", "weak", "strong", "structureless")),
    PLd = E(b, bs_pct = c(60, 55, if (nm) 50 else 40, 75)),
    PLe = b,
    # --- Solonetz
    SNg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    SNj = st(b, if (nm) 4 else 2),
    SNm = E(b, rows = c(1, 3, 4, 5), top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 140),
            munsell_value_moist = c(3, 4, 5, 5), munsell_chroma_moist = c(2, 3, 3, 4), munsell_value_dry = c(5, 6, 6, 6),
            oc_pct = c(2, 0.6, 0.3, 0.2), cec_cmol = c(20, 20, 18, 15), clay_pct = c(20, 35, 30, 25),
            silt_pct = c(40, 35, 38, 40), sand_pct = c(40, 30, 32, 35), structure_grade = c("strong", "strong", "moderate", "structureless"),
            boundary_distinctness = c("abrupt", "clear", "clear", NA)),
    SNy = gypsic_at(b, 125),
    SNk = calcic_at(b, 125),
    SNh = b,
    # --- Greyzems
    GRg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    GRh = b,
    # --- Chernozems
    CHg = , CHl = {
      a <- E(b, rows = c(1, 2, 3, 4, 4), designation = c("Ah1", "Ah2", "Bt", "Bk", "Ck"),
             top_cm = c(0, 25, 50, 75, 110), bottom_cm = c(25, 50, 75, 110, 150),
             clay_pct = c(22, 22, 34, 28, 22), silt_pct = c(55, 55, 46, 52, 58), sand_pct = c(23, 23, 20, 20, 20),
             oc_pct = c(3, 1.8, 0.6, 0.4, 0.2), caco3_pct = c(0, 0, 0, 22, 12),
             clay_films_amount = c(NA, NA, "common", NA, NA), cec_cmol = c(35, 30, 26, 20, 18),
             munsell_value_moist = c(2, 3, 4, 5, 6), munsell_chroma_moist = c(1, 2, 3, 3, 3),
             munsell_value_dry = c(4, 5, 6, 7, 7), munsell_chroma_dry = c(2, 2, 3, 3, 3),
             structure_grade = c("strong", "moderate", "moderate", "weak", "structureless"),
             structure_type = c("granular", "granular", "subangular blocky", "subangular blocky", "massive"),
             ph_h2o = c(7.0, 7.2, 7.4, 8.0, 8.2))
      if (code == "CHg") deep(a, 100, designation = "Ckg", caco3_pct = 12, munsell_hue_moist = "5GY",
                              munsell_value_moist = 5, munsell_chroma_moist = 1) else a
    },
    CHw = S(b, 3, tongue_vol_pct = 25, tongue_width_mm = 30, tongue_depth_cm = 20),
    CHk = b,
    CHh = E(b, caco3_pct = c(0, 1, 4, 3), carbonate_soft_pct = c(0, 0, 8, 2)),
    # --- Kastanozems
    KSy = E(b, caso4_pct = c(0, 0, 20, 4), caco3_pct = c(0, 1, 3, 3), ec_dS_m = 2.5),
    KSl = E(b, rows = c(1, 2, 3, 4, 4), designation = c("Ah1", "Ah2", "Bt", "Bk", "Ck"),
            top_cm = c(0, 20, 40, 70, 110), bottom_cm = c(20, 40, 70, 110, 150),
            clay_pct = c(20, 20, 32, 24, 20), silt_pct = c(52, 52, 44, 52, 54), sand_pct = c(28, 28, 24, 24, 26),
            caco3_pct = c(0, 0, 0, 20, 12), clay_films_amount = c(NA, NA, "common", NA, NA),
            cec_cmol = c(25, 22, 20, 18, 15), oc_pct = c(1.8, 1.0, 0.5, 0.4, 0.2),
            structure_type = c("granular", "subangular blocky", "subangular blocky", "subangular blocky", "massive")),
    KSk = b,
    KSh = E(b, caco3_pct = c(0, 1, 4, 3), carbonate_soft_pct = c(0, 0, 8, 2)),
    # --- Phaeozems
    PHg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    PHj = E(st(b, 3), top_cm = c(0, 20, if (nm) 51 else 50, 90), bottom_cm = c(20, if (nm) 51 else 50, 90, 140)),
    PHl = E(b, designation = c("Ah1", "Ah2", "Bt", "C"), clay_pct = c(22, 22, 34, 28),
            clay_films_amount = c(NA, NA, "common", NA), sand_pct = c(33, 33, 23, 27)),
    PHc = E(b, caco3_pct = c(0, if (nm) 0 else 5, 5, 3), ph_h2o = 7.6),
    PHh = b,
    # --- Podzoluvisols
    PDi = perma(b),
    PDg = gl(b, if (nm) 5 else 4),
    PDj = st(b, if (nm) 4 else 3),
    PDd = E(b, bs_pct = c(60, 60, 60, if (nm) 50 else 40, 60), ph_h2o = 5.5),
    PDe = b,
    # --- Gypsisols
    GYp = E(b, rows = c(1, 2, 2, 3), designation = c("A", "By", "Bym", "C"),
            top_cm = c(0, 20, if (nm) 101 else 100, 130), bottom_cm = c(20, if (nm) 101 else 100, 130, 160),
            caso4_pct = c(2, 25, 40, 8), cementation_class = c(NA, NA, "strongly", NA)),
    GYk = E(b, rows = c(1, 2, 3, 3), designation = c("A", "By", "Bk", "C"),
            top_cm = c(0, 20, 70, 100), bottom_cm = c(20, 70, 100, 130), caco3_pct = c(3, 3, 25, 10),
            caso4_pct = c(2, 25, 8, 4)),
    GYl = E(b, designation = c("A", "Bty", "C"), clay_pct = c(15, 30, 22), silt_pct = c(40, 35, 38),
            sand_pct = c(45, 35, 40), clay_films_amount = c(NA, "common", NA)),
    GYh = b,
    # --- Calcisols
    CLp = E(b, rows = c(1, 2, 2, 3), designation = c("A", "Bk", "Bkm", "Ck"),
            top_cm = c(0, 20, if (nm) 101 else 100, 130), bottom_cm = c(20, if (nm) 101 else 100, 130, 160),
            caco3_pct = c(5, 25, 60, 15), cementation_class = c(NA, NA, "strongly", NA)),
    CLl = E(b, designation = c("A", "Btk", "Ck"), clay_pct = c(15, 30, 22), silt_pct = c(45, 40, 43),
            sand_pct = c(40, 30, 35), clay_films_amount = c(NA, "common", NA)),
    CLh = b,
    # --- Nitisols
    NTu = E(b, munsell_value_moist = c(2, 3, 3, 3, 3), munsell_chroma_moist = c(2, 2, 6, 6, 6),
            munsell_value_dry = c(4, 4, 4, 4, 4), oc_pct = c(3, 2, if (nm) 1.2 else 1.5, 0.4, 0.2),
            structure_grade = c("strong", "strong", "strong", "strong", "moderate")),
    NTr = E(b, munsell_value_dry = c(4, 4, if (nm) 5 else 4, 4, 4)),
    NTh = E(b, munsell_hue_moist = "5YR", munsell_value_moist = 4, munsell_value_dry = 5),
    # --- Alisols, Acrisols, Luvisols, Lixisols (argic B under an E)
    ALp = , ACp = , LXp = deep(noalb(b), 125, designation = "BCv", plinthite_pct = 5),
    ALg = , ACg = , LVg = , LXg = gl(noalb(b), if (nm) 5 else 4),
    ALj = , LVj = , LXj = st(noalb(b), if (nm) 4 else 3),
    ALu = E(noalb(b), oc_pct = c(5, if (nm) 1.5 else 2, 0.6, 0.3, 0.2)),
    ACu = E(noalb(b), top_cm = c(0, 30, 40, 70, 110), bottom_cm = c(30, 40, 70, 110, 160),
            munsell_value_moist = c(3, 4, 4, 4, 5), munsell_chroma_moist = c(2, 3, 4, 4, 4),
            munsell_value_dry = c(5, 5, 6, 6, 6), oc_pct = c(if (nm) 3.5 else 4, 1, 0.5, 0.3, 0.2),
            structure_grade = c("strong", "moderate", "moderate", "moderate", "moderate")),
    ALf = , ACf = , LVf = , LXf = S(noalb(b), 4, redoximorphic_features_pct = 20, mottle_size_class = "coarse",
                                   mottle_colors = "2.5YR 4/8"),
    ALh = , ACh = , LXh = , LVh = noalb(b),
    LVa = , LXa = b,
    LVv = S(noalb(b), 3, slickensides = "few"),
    LVk = calcic_at(noalb(b), 125),
    LVx = E(noalb(b), munsell_hue_moist = c("10YR", "10YR", if (nm) "7.5YR" else "5YR", "5YR", "5YR")),
    # --- Cambisols
    CMi = perma(b),
    CMg = deep(b, 100, designation = "Cg", munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1),
    CMv = E(b, clay_pct = c(30, 35, 30), sand_pct = c(30, 25, 30), slickensides = c(NA, "few", NA)),
    CMu = E(b, top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 130), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 6, 4),
            munsell_value_dry = c(5, 5, 6), oc_pct = c(2.5, 0.5, 0.2), bs_pct = c(60, if (nm) 50 else 40, 75),
            cec_cmol = c(18, 12, 11), ph_h2o = c(6, 5.5, 6), structure_grade = c("strong", "moderate", "structureless")),
    CMc = E(b, caco3_pct = c(0, if (nm) 2 else 5, 5), ph_h2o = 7.8),
    CMo = E(b, cec_cmol = c(12, if (nm) 5.3 else 5.0, 11)),
    CMd = E(b, bs_pct = c(70, if (nm) 50 else 40, 75), ph_h2o = 5.5),
    CMx = if (nm) E(b, munsell_chroma_moist = c(3, 4, 4)) else b,
    CMe = E(b, munsell_hue_moist = "10YR", munsell_chroma_moist = c(3, 4, 4)),
    NULL)
  if (is.null(out)) rlang::abort(sprintf("No example for the soil unit '%s'.", code))
  out$site$id <- paste0("fao1988-", code, if (nm) "-near-miss")
  out
}


# --- names -----------------------------------------------------------------------------------

# The unit codes of a printed name (case and surrounding space ignored): the
# name as printed in the key, or an alias the lists print (Gypsic Regosols,
# Q7). Several names give several codes; a name of no unit gives NA.
.f88_unit_lookup <- function(name) {
  u <- load_rules("fao1988")$units
  ents <- unlist(unname(u), recursive = FALSE)
  keys <- lapply(ents, function(e) tolower(trimws(c(e$name, unlist(e$aliases)))))
  codes <- vapply(ents, function(e) as.character(e$code), "")
  vapply(tolower(trimws(as.character(name))), function(n) {
    hit <- codes[vapply(keys, function(k) n %in% k, NA)]
    if (length(hit) == 1L) hit else NA_character_
  }, "", USE.NAMES = FALSE)
}
