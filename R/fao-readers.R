# =============================================================================
# v0.9.381 -- the shared readers of the FAO-Unesco soil legend, in its two
# editions: the Revised Legend (FAO 1988, World Soil Resources Report 60; the
# text of the ISRIC reprint of 1997, Technical Paper 20) and the Legend of the
# Soil Map of the World (FAO-Unesco 1974, Vol. I, Unesco Paris). Both are read
# by one module with an edition switch (Hugo's decision Q1 of 2026-11-08); the
# registry entries, the diagnostics and the keys follow in later parts.
#
# The legends write organic matter (1974) or organic carbon (1988), CEC per
# kg (1988) or per 100 g (1974) of clay by named methods, base saturation by
# NH4OAc, thickness-weighted means over a window, three texture classes of
# their own, the solum, buried horizons and a clay increase in three bands.
# The readers here turn the recorded data into those quantities,
# three-valued (a number, TRUE, FALSE, or NA when the data leave it open),
# and say what they read: the attributes that would decide an undecided
# answer (`missing`), the fallbacks they used (`flags`) and a note in
# soilKey's words (`note`). Every number is an argument whose default is the
# printed one; data-raw/fao_thresholds_1988.yaml and _1974.yaml give the
# numbers with their pages, and data-raw/fao_readings.yaml the readings.
#
# Hugo's decisions of 2026-11-08 read here:
#   Q9  depths from the literal soil surface: the top of the first recorded
#       horizon, H and O horizons included;
#   Q10 the 125 / 90 / 75 cm rule of 1974 by the texture class of the upper
#       30 cm; "coarse textured materials" = coarser than sandy loam;
#   Q11 organic matter = organic carbon x 1.724, flagged;
#   Q12 values as reported, flagged where the method is known to differ;
#       ph_h2o_1to1 and ph_h2o_1to5 read first; the NH4Cl CEC from cec_cmol
#       with cec_method nh4cl, else ecec_cmol, flagged; the ECEC of 1988 as
#       item 3 of the ferralic B (p. 31);
#   Q24 the moist colour read as rubbed, flagged;
#   default 18: the clay increase in three bands with its numbers as
#       arguments, equal to the usda branch of test_clay_increase_argic().
# =============================================================================


# --- answers, three values ----------------------------------------------------------

# A reader's answer: the value, the attributes that would decide it when it
# is (or, for a vector, one of its elements is) undecided, the fallbacks used
# and a note.
.fao_ans <- function(value = NA, missing = character(0), flags = character(0),
                     note = character(0), ...) {
  undecided <- is.atomic(value) && (length(value) == 0L || anyNA(value))
  c(list(value = value,
         flags = unique(as.character(flags[!is.na(flags)])),
         missing = if (undecided) unique(as.character(missing)) else character(0),
         note = unique(as.character(note[!is.na(note)]))),
    list(...))
}

# Three-valued AND / OR of plain logicals.
.fao_and <- function(...) {
  v <- c(...)
  if (any(v %in% FALSE)) FALSE else if (length(v) && all(v %in% TRUE)) TRUE else NA
}
.fao_or <- function(...) {
  v <- c(...)
  if (any(v %in% TRUE)) TRUE else if (length(v) && all(v %in% FALSE)) FALSE else NA
}

# The edition as "1988" or "1974" (88, 74, 1988, "fao88" ...).
.fao_edition <- function(edition) {
  e <- tolower(as.character(edition)[1L])
  e <- sub("^fao", "", e)
  if (e %in% c("1988", "88", "1990", "1997")) return("1988")
  if (e %in% c("1974", "74")) return("1974")
  rlang::abort(sprintf("Unknown edition of the FAO-Unesco legend: '%s' (use 1988 or 1974).",
                       as.character(edition)[1L]))
}

# A horizon column as a vector (NA where the column is absent).
.fao_col <- function(h, name, type = c("numeric", "character", "logical")) {
  type <- match.arg(type)
  n <- nrow(h)
  v <- if (name %in% names(h)) h[[name]] else NULL
  if (is.null(v)) {
    return(switch(type, numeric = rep(NA_real_, n), character = rep(NA_character_, n),
                  logical = rep(NA, n)))
  }
  switch(type,
         numeric = suppressWarnings(as.numeric(v)),
         character = as.character(v),
         logical = as.logical(v))
}


# --- depths and windows (Q9; plan section 4.2) --------------------------------------

# Depths from the soil surface (Q9): the top of the first recorded horizon,
# whatever its kind (an H or O horizon included), is 0. A profile written
# with its O horizon above 0 (O at -5 to 0) is read from -5.
.fao_depths <- function(h) {
  top <- .fao_col(h, "top_cm"); bot <- .fao_col(h, "bottom_cm")
  z <- if (all(is.na(top))) NA_real_ else min(top, na.rm = TRUE)
  list(top = top - z, bottom = bot - z)
}

# Continuous rock (Hugo, 2026-11-09): an R designation (R, 2R, R/Cr;
# .WRB_ROCK_DESIGNATION), or bedrock_pct of 90 or more together with a hard
# rupture resistance recorded (hard, very hard, extremely hard or rigid: the
# continuous hard rock that a spade cannot dig, 1988 p. 35). bedrock_pct
# alone is not rock. The key's own test of continuous hard rock follows with
# the diagnostics.
.fao_is_rock <- function(h) {
  d <- trimws(.fao_col(h, "designation", "character"))
  r <- !is.na(d) & grepl(.WRB_ROCK_DESIGNATION, d)
  b <- .fao_col(h, "bedrock_pct")
  rr <- tolower(trimws(.fao_col(h, "rupture_resistance", "character")))
  hard <- !is.na(rr) & rr %in% c("hard", "very hard", "extremely hard", "rigid")
  r | (!is.na(b) & b >= 90 & hard)
}

# The layers of a window [top, bottom) and whether the description covers
# it: every layer that overlaps the window ("at least between 20 and 50 cm"
# and "throughout" read on each of them; "in some part" on any). The first
# rock layer closes the window: the soil is read down to its top, and the
# window counts as described below it. The window is covered only when the
# clipped thicknesses of its soil layers fill it from its top to its base
# (or to the rock): a gap in the depths, a layer without depths or a
# description that stops above the base leaves it uncovered (`gap`), and
# layers that overlap fill more than the window (`overlap`).
.fao_window <- function(h, top, bottom) {
  d <- .fao_depths(h)
  rock <- .fao_is_rock(h)
  rtop <- d$top[rock & !is.na(d$top)]
  rock_top <- if (length(rtop)) min(rtop) else Inf
  on_rock <- rock_top < bottom
  eff <- min(bottom, rock_top)
  need <- max(eff - top, 0)
  ok <- !is.na(d$top) & !is.na(d$bottom) & !rock
  th <- rep(0, nrow(h))
  th[ok] <- pmax(pmin(d$bottom[ok], eff) - pmax(d$top[ok], top), 0)
  idx <- which(th > 0)
  tot <- sum(th)
  tol <- 1e-6
  overlap <- tot > need + tol
  gap <- tot < need - tol
  list(layers = idx, thickness = th[idx], covered = !gap && !overlap,
       gap = gap, overlap = overlap, on_rock = on_rock, soil_bottom = eff)
}

# What an uncovered window asks for.
.fao_window_missing <- function(w) {
  if (isTRUE(w$overlap)) {
    return(list(missing = c("top_cm", "bottom_cm"), flags = "overlapping_layers",
                note = "layers overlap in depth"))
  }
  list(missing = c("top_cm", "bottom_cm"), flags = character(0),
       note = "the depths leave part of the window undescribed")
}

# A per-layer test read over a window: "throughout" (every layer that
# overlaps it, and the description covers it) or "some" (one layer suffices).
# Three-valued: a layer not decided leaves "throughout" undecided unless
# another layer fails, and "some" undecided unless another passes. `subset`
# (logical, one per layer) restricts the reading to some layers, as in
# "throughout the B horizon to 125 cm" (the layers of the B whose top is
# above 125 cm). A window with no soil layer in it (rock above it) fails.
.fao_in_window <- function(h, test, top, bottom, mode = c("throughout", "some"),
                           subset = NULL) {
  mode <- match.arg(mode)
  w <- .fao_window(h, top, bottom)
  if (!is.null(subset)) w$layers <- w$layers[as.logical(subset)[w$layers] %in% TRUE]
  v <- as.logical(test)[w$layers]
  wm <- if (w$covered) list(missing = character(0), flags = character(0), note = character(0))
        else .fao_window_missing(w)
  if (mode == "throughout") {
    if (any(v %in% FALSE)) return(.fao_ans(FALSE, layers = w$layers[v %in% FALSE]))
    if (!length(v) && w$covered) return(.fao_ans(FALSE, layers = integer(0),
                                                 note = "no layer in the window"))
    if (length(v) && all(v %in% TRUE) && w$covered) return(.fao_ans(TRUE, layers = w$layers))
  } else {
    if (any(v %in% TRUE)) return(.fao_ans(TRUE, layers = w$layers[v %in% TRUE]))
    if (all(v %in% FALSE) && w$covered) return(.fao_ans(FALSE, layers = integer(0)))
  }
  .fao_ans(NA, missing = wm$missing, flags = wm$flags, note = wm$note,
           layers = w$layers[is.na(v)])
}

# "Within X cm of the surface": a layer whose upper boundary is at or above
# X cm passes (plan section 4.2, the same reading as soilKey's other
# systems). FALSE when every such layer fails and the description covers
# the profile, without gaps, to below X (a layer could start at X itself),
# or to rock at or above X.
.fao_within <- function(h, test, depth) {
  d <- .fao_depths(h)
  rock <- .fao_is_rock(h)
  idx <- which(!is.na(d$top) & d$top <= depth & !rock)
  v <- as.logical(test)[idx]
  if (any(v %in% TRUE)) return(.fao_ans(TRUE, layers = idx[v %in% TRUE]))
  w <- .fao_window(h, 0, depth + 1e-3)
  if (all(v %in% FALSE) && w$covered) return(.fao_ans(FALSE, layers = integer(0)))
  wm <- if (w$covered) list(missing = character(0), flags = character(0), note = character(0))
        else .fao_window_missing(w)
  .fao_ans(NA, missing = wm$missing, flags = wm$flags, note = wm$note,
           layers = idx[is.na(v)])
}


# --- weighted means and mixing (plan section 4.4) -------------------------------------

# The thickness-weighted mean of a column over [top, bottom), each layer
# clipped to the window. Undecided when a layer of the window has no value or
# the depths do not fill the window (a gap, a layer without depths, a
# description that stops above the base: missing top_cm / bottom_cm);
# overlapping layers are flagged overlapping_layers and undecided. Where the
# soil ends on rock above the base, `zero_below_rock = TRUE` counts the
# depth below the soil as zero (strongly humic, 1988 p. 40: the same 100 cm
# average for a soil 50 to 100 cm deep; for a soil shallower than
# `zero_from_cm` too, flagged, Hugo's decision of 2026-11-09) and
# `zero_below_rock = FALSE` averages the soil above the rock.
.fao_weighted <- function(h, var, top, bottom, zero_below_rock = FALSE,
                          values = NULL, zero_from_cm = 50) {
  x <- if (is.null(values)) .fao_col(h, var) else as.numeric(values)
  w <- .fao_window(h, top, bottom)
  if (!w$covered) {
    wm <- .fao_window_missing(w)
    return(.fao_ans(NA, missing = wm$missing, flags = wm$flags, note = wm$note))
  }
  i <- w$layers
  if (!length(i)) return(.fao_ans(NA, missing = var))
  if (anyNA(x[i])) return(.fao_ans(NA, missing = var, layers = i[is.na(x[i])]))
  th <- w$thickness
  soil <- sum(th)
  if (soil <= 0) return(.fao_ans(NA, missing = var))
  if (w$on_rock && soil < bottom - top) {
    if (isTRUE(zero_below_rock)) {
      shallow <- w$soil_bottom < zero_from_cm
      return(.fao_ans(sum(x[i] * th) / (bottom - top),
                      flags = if (shallow) "zero_below_rock_soil_shallower_than_50cm" else character(0),
                      note = "the depth below the soil counts as zero", layers = i))
    }
    return(.fao_ans(sum(x[i] * th) / soil,
                    note = "averaged over the soil above the rock", layers = i))
  }
  .fao_ans(sum(x[i] * th) / soil, layers = i)
}

# A surface mixed to a depth, as in ploughing (1988: 18 cm for the mollic A
# and the Vertisols, 25 cm for a thin histic H; 1974: 18 cm mollic, 20 cm
# Vertisols, 25 cm histic H): the thickness-weighted mean of the layers in
# 0-depth; a soil shallower than the depth over rock is averaged over itself.
.fao_mixed <- function(h, var, depth, values = NULL) {
  .fao_weighted(h, var, 0, depth, zero_below_rock = FALSE, values = values)
}


# --- organic matter (Q11) -------------------------------------------------------------

# Organic matter from organic carbon (the 1974 legend writes every limit in
# organic matter; soilKey records carbon): OC x 1.724, Van Bemmelen's factor
# of the US SCS methods the 1974 text cites (pp. 21, 24, 32). Always flagged
# as a conversion. Vectorised over `oc`.
.fao_om <- function(oc, edition = "1974", factor = 1.724) {
  edition <- .fao_edition(edition)
  .fao_ans(as.numeric(oc) * factor, flags = "om_from_oc_x1.724", missing = "oc_pct",
           note = sprintf("organic matter = organic carbon x %s (FAO %s)", factor, edition))
}


# --- the exchange complex (Q12) -----------------------------------------------------------

# Exchangeable bases (Ca + Mg + K + Na), cmol(+)/kg; NA where one is missing.
.fao_bases <- function(h) {
  .fao_col(h, "ca_cmol") + .fao_col(h, "mg_cmol") + .fao_col(h, "k_cmol") +
    .fao_col(h, "na_cmol")
}

# The CEC method of each layer in lower case; NA where not recorded.
.fao_cec_method <- function(h) {
  m <- tolower(trimws(.fao_col(h, "cec_method", "character")))
  m[!is.na(m) & !nzchar(m)] <- NA_character_
  m
}

# CEC per kg of clay (cmol(+)/kg clay = cmol(+)/kg soil x 100 / clay %), by
# the method the text names (default 1 of the questions: no deduction for
# organic matter):
#   "nh4oac": cec_cmol as reported; flagged where cec_method names another
#             method (the 1988 key and the 1974 cambic and oxic B);
#   "nh4cl":  cec_cmol where cec_method is nh4cl; else ecec_cmol, flagged
#             (the ferralic and ferric properties and the Acric Ferralsols of
#             1974, pp. 28, 47); else undecided.
# Per kg of soil with `per = "soil"` (the ferralic properties of 1988: less
# than 4 per kg soil, p. 35).
.fao_cec_clay <- function(h, method = c("nh4oac", "nh4cl"), per = c("clay", "soil")) {
  method <- match.arg(method); per <- match.arg(per)
  cec <- .fao_col(h, "cec_cmol"); ecec <- .fao_col(h, "ecec_cmol")
  clay <- .fao_col(h, "clay_pct"); m <- .fao_cec_method(h)
  n <- nrow(h)
  val <- rep(NA_real_, n); flag <- rep(NA_character_, n)
  if (method == "nh4oac") {
    val <- cec
    other <- !is.na(m) & !(m %in% c("acetate_ph7", "nh4oac"))
    flag[other & !is.na(cec)] <- paste0("cec_by_", m[other & !is.na(cec)])
  } else {
    own <- !is.na(m) & m == "nh4cl" & !is.na(cec)
    val[own] <- cec[own]
    use_ecec <- !own & !is.na(ecec)
    val[use_ecec] <- ecec[use_ecec]
    flag[use_ecec] <- "ecec_for_nh4cl_cec"
  }
  if (per == "clay") {
    val <- ifelse(!is.na(clay) & clay > 0, val * 100 / clay, NA_real_)
  }
  miss <- if (method == "nh4oac") c("cec_cmol", if (per == "clay") "clay_pct")
          else c("cec_cmol (cec_method nh4cl)", "ecec_cmol", if (per == "clay") "clay_pct")
  list(value = val, flags = flag, missing = miss)
}

# Effective CEC per kg of clay by edition:
#   1988 (ferralic B, p. 31 item 3): NH4OAc bases + 1 M KCl exchangeable
#        acidity (exch_acidity_kcl_cmol); else al_kcl_cmol, then al_cmol,
#        flagged; else ecec_cmol as recorded, flagged;
#   1974 (oxic B, p. 27): NH4OAc bases + KCl aluminium (al_kcl_cmol, else
#        al_cmol); else ecec_cmol as recorded, flagged.
# Appendix 5 of 1988 (p. 130) sums bases and Al; the definition at the point
# of use governs (Q12).
.fao_ecec_clay <- function(h, edition = "1988") {
  edition <- .fao_edition(edition)
  bases <- .fao_bases(h)
  acid <- .fao_col(h, "exch_acidity_kcl_cmol")
  alk <- .fao_col(h, "al_kcl_cmol"); al <- .fao_col(h, "al_cmol")
  ecec <- .fao_col(h, "ecec_cmol"); clay <- .fao_col(h, "clay_pct")
  n <- nrow(h)
  val <- rep(NA_real_, n); flag <- rep(NA_character_, n)
  if (edition == "1988") {
    a <- acid
    f <- rep(NA_character_, n)
    f[is.na(a) & !is.na(alk)] <- "al_kcl_for_kcl_acidity"
    a[is.na(a)] <- alk[is.na(a)]
    f[is.na(a) & !is.na(al)] <- "al_for_kcl_acidity"
    a[is.na(a)] <- al[is.na(a)]
  } else {
    a <- alk
    f <- rep(NA_character_, n)
    a[is.na(a)] <- al[is.na(a)]
  }
  own <- !is.na(bases) & !is.na(a)
  val[own] <- bases[own] + a[own]; flag[own] <- f[own]
  rec <- !own & !is.na(ecec)
  val[rec] <- ecec[rec]; flag[rec] <- "ecec_as_recorded"
  val <- ifelse(!is.na(clay) & clay > 0, val * 100 / clay, NA_real_)
  list(value = val, flags = flag,
       missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol",
                   if (edition == "1988") "exch_acidity_kcl_cmol" else "al_kcl_cmol",
                   "clay_pct"))
}

# Base saturation by NH4OAc (Q12): bs_pct as reported, flagged where
# cec_method names another method; where bs_pct is not recorded, the bases
# over the NH4OAc CEC, flagged as computed.
.fao_bs <- function(h) {
  bs <- .fao_col(h, "bs_pct"); m <- .fao_cec_method(h)
  cec <- .fao_col(h, "cec_cmol"); bases <- .fao_bases(h)
  n <- nrow(h)
  flag <- rep(NA_character_, n)
  other <- !is.na(m) & !(m %in% c("acetate_ph7", "nh4oac"))
  comp <- is.na(bs) & !is.na(bases) & !is.na(cec) & cec > 0
  bs[comp] <- pmin(100 * bases[comp] / cec[comp], 100)
  flag[comp] <- "bs_computed"
  o <- other & !is.na(bs)
  flag[o] <- ifelse(is.na(flag[o]), paste0("bs_by_cec_", m[o]),
                    paste(flag[o], paste0("bs_by_cec_", m[o]), sep = "; "))
  list(value = bs, flags = flag, missing = "bs_pct")
}

# Water pH at the ratio the text names (Q12): ph_h2o_1to1 or ph_h2o_1to5
# first; else ph_h2o (1:2.5 or as reported), flagged.
.fao_ph <- function(h, ratio = c("1to1", "1to5")) {
  ratio <- match.arg(ratio)
  own <- .fao_col(h, paste0("ph_h2o_", ratio))
  any_ <- .fao_col(h, "ph_h2o")
  flag <- ifelse(is.na(own) & !is.na(any_), paste0("ph_h2o_for_", ratio), NA_character_)
  list(value = ifelse(is.na(own), any_, own), flags = flag,
       missing = paste0("ph_h2o_", ratio))
}


# --- texture (Q10) ------------------------------------------------------------------------

# Silt and sand completed from the two others where one is missing.
.fao_psd <- function(h) {
  cl <- .fao_col(h, "clay_pct"); si <- .fao_col(h, "silt_pct"); sa <- .fao_col(h, "sand_pct")
  si <- ifelse(is.na(si) & !is.na(sa) & !is.na(cl), 100 - sa - cl, si)
  sa <- ifelse(is.na(sa) & !is.na(si) & !is.na(cl), 100 - si - cl, sa)
  list(clay = cl, silt = si, sand = sa)
}

# The three texture classes of the map symbol, by edition (vectorised over
# clay and sand, %):
#   1988 (Appendix 2, p. 107): coarse = clay < 15 and sand > 70; fine = clay
#        > 35; medium = the rest (sand up to 85 with clay 15 or more);
#   1974 (p. 5): coarse = clay < 18 and sand > 65; fine = clay > 35; medium
#        = the rest (sand up to 82 with clay 18 or more).
# Both texts leave clay of exactly 35 % and sand of exactly 70 (65) % with
# less clay unassigned; soilKey reads both as medium (data-raw/
# fao_readings.yaml). NA where clay or sand is missing, except clay > 35
# (fine) whatever the sand.
.fao_texture_class_pct <- function(clay, sand, edition = "1988") {
  edition <- .fao_edition(edition)
  cmax <- if (edition == "1988") 15 else 18
  smin <- if (edition == "1988") 70 else 65
  out <- ifelse(is.na(clay), NA_character_,
         ifelse(clay > 35, "fine",
         ifelse(is.na(sand), NA_character_,
         ifelse(clay < cmax & sand > smin, "coarse", "medium"))))
  as.character(out)
}

# The texture class of each horizon.
.fao_texture_class <- function(h, edition = "1988") {
  p <- .fao_psd(h)
  .fao_texture_class_pct(p$clay, p$sand, edition)
}

# The texture class of the upper 30 cm, from the thickness-weighted clay and
# sand of 0-30 cm (1988 p. 107; 1974 p. 5): the class of the map symbol and,
# in 1974, the class of the 125 / 90 / 75 cm rule (Q10).
#
# Under an organic surface (H or O horizons, or an undesignated layer of 20 %
# organic carbon or more) the 30 cm are measured from the top of the first
# mineral horizon, flagged texture_from_mineral_surface: the class of the map
# symbol describes the mineral soil (Hugo's decision of 2026-11-09).
.fao_texture_upper30 <- function(h, edition = "1988", depth = 30) {
  if (nrow(h)) {
    m <- .fao_master(h)
    oc <- .fao_col(h, "oc_pct")
    organic <- (!is.na(m) & grepl("^(O|H)", m)) | (is.na(m) & !is.na(oc) & oc >= 20)
    first_min <- which(!organic)[1L]
    flags <- character(0)
    if (is.na(first_min)) return(.fao_ans(NA, missing = "clay_pct", note = "no mineral horizon"))
    if (first_min > 1L) {
      h <- h[seq.int(first_min, nrow(h)), , drop = FALSE]
      flags <- "texture_from_mineral_surface"
    }
  } else flags <- character(0)
  p <- .fao_psd(h)
  cl <- .fao_weighted(h, "clay_pct", 0, depth, values = p$clay)
  sa <- .fao_weighted(h, "sand_pct", 0, depth, values = p$sand)
  cls <- .fao_texture_class_pct(cl$value, sa$value, edition)
  .fao_ans(cls, missing = c(cl$missing, sa$missing),
           flags = c(flags, cl$flags, sa$flags),
           clay = cl$value, sand = sa$value)
}

# The depth of the 1974 rule "within 125 cm for coarse, 90 cm for medium, 75
# cm for fine textures" (the calcic and gypsic horizons of C, K, H, L, B and
# high salinity, 1974 pp. 29, 49-53; the Xerosols and Yermosols, footnote 1
# of p. 50), by the class of the upper 30 cm (Q10).
.fao_depth_by_texture <- function(h, depths = c(coarse = 125, medium = 90, fine = 75)) {
  t <- .fao_texture_upper30(h, "1974")
  if (is.na(t$value)) return(.fao_ans(NA, missing = t$missing))
  .fao_ans(unname(depths[t$value]), class = t$value)
}

# USDA texture class of each horizon (texture_class_from_pct(): S, LS, SL,
# L, SIL, SI, SCL, CL, SICL, SC, SIC, C).
.fao_texture_usda <- function(h) {
  p <- .fao_psd(h)
  texture_class_from_pct(p$clay, p$silt, p$sand)
}

# "Sandy loam or finer" and its converse "coarser than sandy loam" (the
# argic, cambic and ferralic B of 1988, pp. 28-31; the Arenosols, p. 80; the
# coarse textured materials of the Gleysols, Q10), on the USDA boundary
# silt + 2 x clay >= 30 (is_sandy_loam_or_finer()). Vectorised; NA where the
# particle size is not recorded.
.fao_sandy_loam_or_finer <- function(h) {
  p <- .fao_psd(h)
  ifelse(is.na(p$silt) | is.na(p$clay), NA, p$silt + 2 * p$clay >= 30)
}
.fao_coarser_than_sandy_loam <- function(h) !.fao_sandy_loam_or_finer(h)

# The texture group of an argic B for tonguing (1988 p. 41; 1974 p. 31):
# "fine" (clay, silty clay, sandy clay), "moderately fine" (sandy clay loam,
# clay loam, silty clay loam) and "medium or coarser" (silt loam, loam,
# sandy loam and coarser, silt).
.fao_tonguing_texture <- function(h) {
  cls <- .fao_texture_usda(h)
  out <- ifelse(cls %in% c("C", "SIC", "SC"), "fine",
         ifelse(cls %in% c("SCL", "CL", "SICL"), "moderately fine",
         ifelse(cls %in% c("SIL", "L", "SL", "LS", "S", "SI"), "medium or coarser",
                NA_character_)))
  as.character(out)
}


# --- designations, the solum and buried horizons --------------------------------------

# The master letters of each designation, the material number taken off
# (.desg_master(): "2Bt1" is "Bt1"; the older notation A2 = E, B3 = BC).
.fao_master <- function(h) {
  d <- .fao_col(h, "designation", "character")
  m <- .desg_master(d)
  sub("^([A-Z]+(?:/[A-Z]+)?).*$", "\\1", m, perl = TRUE)
}

# The solum: the A, E and B horizons (AB, BA, EB, BE, BC, A/B, B/C ...; not
# CB, C, R, H or O). Its base is the base of the deepest such horizon
# (Hugo, 2026-11-08; data-raw/fao_readings.yaml; the mollic A of both
# editions compares its thickness with the solum, 1988 p. 26, 1974 p. 24),
# measured from the soil surface of Q9, so an O or H on top counts in it.
# A horizon without a designation below the last A, E or B and above the
# first C or R leaves it undecided. Where the description ends in the
# solum, the value is a lower bound (`open = TRUE`).
.fao_solum <- function(h) {
  d <- .fao_depths(h)
  m <- .fao_master(h)
  if (all(is.na(m))) return(.fao_ans(NA, missing = "designation"))
  sol <- !is.na(m) & grepl("^(A|E|B)", m) & !grepl("^(CB|C/B)", m)
  cr <- !is.na(m) & grepl("^(C|R)", m) & !grepl("^(C/B)", m) | .fao_is_rock(h)
  if (!any(sol)) return(.fao_ans(0, note = "no A, E or B horizon"))
  last <- max(which(sol))
  base <- d$bottom[last]
  # An undesignated layer above the last A, E or B does not matter; one
  # between it and the first C or R below it (or below it with no C or R
  # after) leaves the base undecided (Hugo's decision of 2026-11-09).
  first_cr <- which(cr & seq_along(m) > last)[1L]
  stop_at <- if (is.na(first_cr)) nrow(h) else first_cr - 1L
  if (stop_at > last && anyNA(m[seq.int(last + 1L, stop_at)])) {
    return(.fao_ans(NA, missing = "designation",
                    note = "a horizon without a designation below the last A, E or B"))
  }
  flags <- character(0)
  open <- last == nrow(h)
  .fao_ans(base, flags = flags, open = open,
           note = if (open) "the description ends in the solum: a lower bound" else character(0))
}

# Buried horizons (1988 p. 42; 1974 p. 32): horizons buried by 50 cm or more
# of new material are not diagnostic, and in 1988 by 35 cm or more when the
# new material has andic properties. A buried horizon is one whose
# designation has the suffix b (Ab, 2Btb); the new material is everything
# above the first of them. `mantle_andic` is the andic reading of the
# mantle (TRUE / FALSE / NA, from the andic properties of a later part).
# Returns per layer whether it is ignored: TRUE for the first buried horizon
# and everything below it once the mantle is thick enough, NA where the
# answer rests on an undecided andic mantle (1988, 35 to < 50 cm).
.fao_buried <- function(h, edition = "1988", mantle_andic = NA,
                        min_cm = 50, min_andic_cm = 35) {
  edition <- .fao_edition(edition)
  n <- nrow(h)
  out <- rep(FALSE, n)
  d <- .fao_depths(h)
  desg <- .desg_master(.fao_col(h, "designation", "character"))
  b <- !is.na(desg) & grepl("^[A-Z]+(/[A-Z]+)?[a-z0-9]*b", desg)
  if (!any(b)) return(.fao_ans(out, buried_from = NA_integer_))
  first <- min(which(b))
  mantle <- d$top[first]
  if (is.na(mantle)) return(.fao_ans(rep(NA, n), missing = "top_cm"))
  ign <- if (mantle >= min_cm) TRUE
         else if (edition == "1988" && mantle >= min_andic_cm) as.logical(mantle_andic)
         else FALSE
  out[seq.int(first, n)] <- ign
  .fao_ans(out, buried_from = first, mantle_cm = mantle,
           missing = if (is.na(ign)) "andic properties of the mantle" else character(0))
}


# --- the clay increase (default 18) ---------------------------------------------------------

# The clay increase of the argic B (1988 p. 28, item 3) and the argillic B
# (1974 p. 25), for pairs of an overlying coarser layer (`upper`) and the
# layer below (`lower`), % clay: below the first break, at least `steps[1]`
# points more; from the first to below the second break, a ratio of at
# least `steps[2]`; from the second break on, at least `steps[3]` points
# more. Vectorised; NA where either clay is missing. Equal to the usda
# branch of test_clay_increase_argic() (3 / 1.2 / 8, breaks 15 and 40; the
# exact 15 and 40 of 1974 read the same way, default 12).
.fao_clay_increase_pair <- function(upper, lower, steps = c(3, 1.2, 8),
                                    breaks = c(15, 40)) {
  ifelse(is.na(upper) | is.na(lower), NA,
  ifelse(upper < breaks[1], lower - upper >= steps[1],
  ifelse(upper < breaks[2], lower / upper >= steps[2],
         lower - upper >= steps[3])))
}

# The clay increase in a profile: each layer is compared with every layer
# above it whose base is at most `distance` cm above its top (the increase
# "reached within a vertical distance" of 30 cm with clay skins, 15 cm
# without, 1988 p. 28 item 4; 30 cm in 1974, p. 25). A pair across a
# lithological discontinuity (a different material number of the
# designation) is left undecided, not failed (plan section 4.2). Rock layers
# are never compared. Per layer: TRUE when a pair passes, FALSE when every
# pair was compared and failed, NA otherwise; `upper` names the layer of the
# first pair that passed.
.fao_clay_increase <- function(h, steps = c(3, 1.2, 8), breaks = c(15, 40),
                               distance = 30) {
  n <- nrow(h)
  d <- .fao_depths(h)
  clay <- .fao_col(h, "clay_pct")
  mat <- .desg_material_number(.fao_col(h, "designation", "character"))
  rock <- .fao_is_rock(h)
  res <- rep(NA, n); up <- rep(NA_integer_, n)
  if (n >= 1L) res[1L] <- FALSE
  for (i in seq_len(n)[-1L]) {
    if (rock[i]) { res[i] <- FALSE; next }
    js <- which(seq_len(n) < i & !rock & !is.na(d$bottom) & !is.na(d$top[i]) &
                  d$top[i] - d$bottom <= distance)
    if (!length(js)) { res[i] <- FALSE; next }
    p <- .fao_clay_increase_pair(clay[js], clay[i], steps, breaks)
    disc <- !is.na(mat[js]) & !is.na(mat[i]) & mat[js] != mat[i]
    p[disc] <- NA
    if (any(p %in% TRUE)) {
      res[i] <- TRUE; up[i] <- js[which(p %in% TRUE)[1L]]
    } else if (all(p %in% FALSE)) {
      res[i] <- FALSE
    }
  }
  .fao_ans(res, upper = up, missing = "clay_pct")
}


# --- colour (Q24) -------------------------------------------------------------------------

# A Munsell hue as a position from red to blue: 10RP 0, 5R 5, 10R 10, 2.5YR
# 12.5, 5YR 15, 7.5YR 17.5, 10YR 20, 2.5Y 22.5, 5Y 25, then GY, G, BG, B,
# PB, P from 30 on. Neutral (N) and unread hues are NA.
.fao_hue_rank <- function(hue) {
  base <- c(RP = -10, R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60,
            PB = 70, P = 80)
  vapply(as.character(hue), function(x) {
    p <- .munsell_hue_parts(x)
    if (is.na(p$family) || p$family == "N" || is.na(p$number)) return(NA_real_)
    unname(base[p$family]) + p$number
  }, numeric(1), USE.NAMES = FALSE)
}

# A hue compared with a reference hue: "redder" (strictly, "hues redder than
# 7.5YR" of the ferric properties, 1988 p. 35), "as_red" (as red as or
# redder: "5YR or redder", the albic E, p. 33), "yellower" and "as_yellow"
# ("2.5Y or more" of the jarosite mottles, p. 32). A neutral hue (N) is
# neither redder nor yellower than any hue (FALSE); a hue not recorded or
# not read is NA.
.fao_hue_cmp <- function(hue, ref, op = c("redder", "as_red", "yellower", "as_yellow")) {
  op <- match.arg(op)
  r <- .fao_hue_rank(hue); r0 <- .fao_hue_rank(ref)
  neutral <- vapply(as.character(hue), function(x) identical(.munsell_hue_parts(x)$family, "N"),
                    logical(1), USE.NAMES = FALSE)
  out <- switch(op, redder = r < r0, as_red = r <= r0, yellower = r > r0, as_yellow = r >= r0)
  out[neutral] <- FALSE
  out
}

# The rubbed moist colour (Q24): munsell_*_rubbed where recorded, else the
# moist colour read as rubbed, flagged per horizon. The limits written
# "less than 3.5" or "5.5" between Munsell chips are read as printed on the
# recorded number: a whole chip of 3 passes, a value recorded as 3.5 fails.
.fao_colour_rubbed <- function(h) {
  hr <- .fao_col(h, "munsell_hue_rubbed", "character")
  vr <- .fao_col(h, "munsell_value_rubbed"); cr <- .fao_col(h, "munsell_chroma_rubbed")
  hm <- .fao_col(h, "munsell_hue_moist", "character")
  vm <- .fao_col(h, "munsell_value_moist"); cm <- .fao_col(h, "munsell_chroma_moist")
  none <- is.na(hr) & is.na(vr) & is.na(cr)
  moist <- none & !(is.na(hm) & is.na(vm) & is.na(cm))
  list(hue = ifelse(none, hm, hr), value = ifelse(none, vm, vr),
       chroma = ifelse(none, cm, cr),
       flags = ifelse(moist, "moist_colour_for_rubbed", NA_character_),
       missing = "munsell_hue_rubbed")
}


# --- new attributes with their fallbacks (Q19) ------------------------------------------

# A field observation recorded as a logical column: TRUE / FALSE as recorded;
# not recorded, absent (FALSE) with a note (the standing rule: a field
# observation not recorded is absent).
.fao_field_obs <- function(h, name) {
  v <- .fao_col(h, name, "logical")
  list(value = ifelse(is.na(v), FALSE, v),
       flags = ifelse(is.na(v), paste0(name, "_not_recorded_absent"), NA_character_))
}

# Smeary consistence (1988 p. 39; 74 p. 31): smeary_consistence; else
# thixotropic_index of 50 or more, the limit soilKey's WRB Thixotropic reads,
# flagged; else absent (a field observation).
.fao_smeary <- function(h, thixotropic_min = 50) {
  v <- .fao_col(h, "smeary_consistence", "logical")
  ti <- .fao_col(h, "thixotropic_index")
  flag <- rep(NA_character_, nrow(h))
  use_ti <- is.na(v) & !is.na(ti)
  v[use_ti] <- ti[use_ti] >= thixotropic_min
  flag[use_ti] <- "thixotropic_index_for_smeary"
  flag[is.na(v)] <- "smeary_consistence_not_recorded_absent"
  v[is.na(v)] <- FALSE
  list(value = v, flags = flag)
}

# Turns redder on ignition (1988 PZc, p. 81; 74 Podzols, p. 47):
# ignition_redder; else free iron (fe_dcb_pct) below 0.5 % of the fine earth
# is FALSE, flagged (the footnote of the 1974 Humic Podzols, 74 p. 39; Q23);
# 0.5 % or more, or no iron recorded, undecided.
.fao_ignition_redder <- function(h, fe_min_pct = 0.5) {
  v <- .fao_col(h, "ignition_redder", "logical")
  fe <- .fao_col(h, "fe_dcb_pct")
  flag <- rep(NA_character_, nrow(h))
  use_fe <- is.na(v) & !is.na(fe)
  # Fe below the limit: does not turn redder; at or above it: undecided (the
  # footnote gives one direction only; Hugo's decision of 2026-11-09)
  low <- use_fe & fe < fe_min_pct
  v[low] <- FALSE
  flag[low] <- "free_iron_for_ignition"
  list(value = v, flags = flag, missing = c("ignition_redder", "fe_dcb_pct"))
}

# Rock structure, % by volume (the cambic B, 1988 p. 30, 74 p. 26; the
# ferralic B, p. 31; the oxic B, 74 p. 27): rock_structure_pct; else
# saprolite_pct, flagged; else a Cr or BC horizon has some rock structure of
# unknown amount (NA, flagged); a rock layer (.fao_is_rock()) is 100; else
# none (a field observation not recorded is absent), with a flag.
.fao_rock_structure <- function(h) {
  v <- .fao_col(h, "rock_structure_pct")
  sap <- .fao_col(h, "saprolite_pct")
  m <- .fao_master(h)
  d <- .desg_master(.fao_col(h, "designation", "character"))
  flag <- rep(NA_character_, nrow(h))
  use_sap <- is.na(v) & !is.na(sap)
  v[use_sap] <- sap[use_sap]; flag[use_sap] <- "saprolite_for_rock_structure"
  crbc <- is.na(v) & !is.na(d) & (grepl("^Cr", d) | grepl("^(BC|B/C)", m))
  rk <- .fao_is_rock(h) & is.na(v)
  v[rk] <- 100; flag[rk] <- NA_character_
  crbc <- crbc & !rk
  flag[crbc] <- "rock_structure_from_designation"
  none <- is.na(v) & !crbc
  v[none] <- 0; flag[none] <- "rock_structure_not_recorded_absent"
  list(value = v, flags = flag, missing = "rock_structure_pct")
}

# Palygorskite, % of the clay (the yermic phase of 1988, p. 70):
# palygorskite_pct; else a clay_mineralogy naming palygorskite counts as
# present (100, flagged: the dominant clay mineral); else absent (0).
.fao_palygorskite <- function(h) {
  v <- .fao_col(h, "palygorskite_pct")
  cm <- tolower(.fao_col(h, "clay_mineralogy", "character"))
  flag <- rep(NA_character_, nrow(h))
  named <- is.na(v) & !is.na(cm) & grepl("palygorsk|attapulg", cm)
  v[named] <- 100; flag[named] <- "palygorskite_from_clay_mineralogy"
  none <- is.na(v)
  v[none] <- 0; flag[none] <- "palygorskite_not_recorded_absent"
  list(value = v, flags = flag)
}
