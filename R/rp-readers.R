# =============================================================================
# v0.9.343 -- the readers of the reference horizons of the French Referentiel
# pedologique 2008 (AFES; Baize and Girard coord.; Quae, 2009) on the
# recorded data.
#
# The book defines its reference horizons in field and laboratory words:
# moist colours, carbon classes, effervescence, the saturation S/CEC by
# ammonium acetate at pH 7, coarse elements by weight of the whole soil,
# redox mottles at the voids or inside the peds, salts, the intertropical and
# andosol domains, depths counted from the top of the first layer that is not
# a litter OL. Each reader here turns one of them into the recorded data,
# three-valued (TRUE, FALSE, or NA when the data leave it open), and says what
# it read: the attributes that would decide it when undecided (`missing`), a
# note in soilKey's words (`note`), a tag when it used a fallback or a
# soilKey WRB diagnostic in place of a field descriptor (`tag`), and whether a
# threshold was met only within the tolerance of p. 64 (`imperfect`).
#
# Hugo's decisions of 2026-11-02 read here:
#   Q11 coarse elements by weight (coarse_fragments_wt_pct and the four
#     classes); else the volume converted with a rock density of 2.65 and the
#     recorded bulk density, flagged; without it, a volume at or above the
#     threshold is yes, under 35 % (for 60) or 20 % (for 40) no, between the
#     two undecided;
#   Q12 S/CEC with the CEC by ammonium acetate at pH 7; a CEC by another
#     method stands in, flagged, with a lower grade; Na of the sodic horizon
#     over the sum of the bases, esp_pct only where the bases are missing,
#     flagged;
#   Q13 redox features functional unless recorded relict (redox_relict);
#   Q17 the most specific text: g over 5 %, Gr over 90 %;
#   Q18 the intertropical domain: Koppen A, latitude between the tropics, or
#     declared (site$intertropical);
#   Q19 H against O by the designation, then water saturation over 182
#     days or a very poorly drained site; ash by loi_pct, else organic carbon
#     over 30 % (25-35 imperfect);
#   defaults 3 (a field observation not recorded is absent, a measurement
#     undecided), 8 (the depth datum), 10 (carbon classes), 11 (moist colour),
#     15 (the andosol gate), 17 (effervescence), 18 (Go against g).
# =============================================================================

# Tolerances of p. 64 used by the horizon tests (Hugo's default 9).
.RP_TOL <- list(oc8 = 2, oc30 = 5, ph = 0.2, bs = 5, cec = 1, d10 = 5, d20 = 5, d30 = 5)

# soilKey's WRB diagnostics that stand in for a field descriptor (default 6).
.RP_STAND_INS <- c("argic", "spodic", "calcic", "petrocalcic", "vertic_horizon",
                   "vertic_properties", "gleyic_properties", "stagnic_properties",
                   "reducing_conditions", "andic_properties", "vitric_properties",
                   "ferralic", "nitic_horizon", "histic_horizon", "folic", "chernic",
                   "mollic", "umbric_horizon", "albic", "thionic", "salic",
                   "natric_horizon", "gypsic", "petrogypsic", "duric_horizon",
                   "petroduric", "ferric", "petroplinthic", "plinthic", "pisoplinthic",
                   "retic_properties", "albeluvic_glossae", "continuous_rock",
                   "cryic_conditions", "technic_features", "fragic")

# Fallbacks that lower the grade of the reading as a stand-in does (Q12).
.RP_LOWER_GRADE <- c("cec_method_substitute", "esp_for_bases")


# --- answers ------------------------------------------------------------------

# A reader's answer.
.rp_r <- function(passed, missing = character(0), note = character(0),
                  tag = character(0), imperfect = FALSE, code = NULL,
                  variants = character(0), value = NULL) {
  p <- if (length(passed) == 1L) as.logical(passed) else NA
  list(passed = p, missing = if (is.na(p)) unique(as.character(missing)) else character(0),
       note = unique(as.character(note[!is.na(note)])), tag = unique(as.character(tag)),
       imperfect = isTRUE(p) && isTRUE(imperfect), code = code,
       variants = unique(as.character(variants)), value = value)
}

# Three-valued AND / OR of answers: the verdict, what counted for it, the
# attributes of the undecided parts, imperfect when a part that holds was.
.rp_all <- function(...) .rp_combine(list(...), "and")
.rp_any <- function(...) .rp_combine(list(...), "or")
.rp_combine <- function(rs, how) {
  rs <- Filter(Negate(is.null), rs)
  v <- vapply(rs, function(r) r$passed, NA)
  p <- if (how == "and") .kidpr_and(v) else .kidpr_or(v)
  key <- how != "and"
  use <- if (!is.na(p) && p == key) which(v %in% key) else seq_along(rs)
  pick <- function(f) unique(unlist(lapply(rs[use], `[[`, f)))
  imp <- if (isTRUE(p)) {
    if (how == "and") any(vapply(rs, function(r) isTRUE(r$imperfect), NA))
    else all(vapply(rs[v %in% TRUE], function(r) isTRUE(r$imperfect), NA))
  } else FALSE
  .rp_r(p, missing = unique(unlist(lapply(rs[is.na(v)], `[[`, "missing"))),
        note = pick("note"), tag = pick("tag"), imperfect = imp)
}

# A comparison with the tolerance of p. 64 as an answer: pass TRUE,
# imperfect TRUE flagged, fail FALSE, NA when the value is missing.
.rp_tol <- function(value, threshold, direction, tol = 0, what, attr) {
  w <- within_tolerance(value, threshold, direction, tol)
  if (is.na(w)) return(.rp_r(NA, missing = attr, note = sprintf("%s not recorded", what)))
  op <- if (direction == "min") ">=" else "<="
  .rp_r(w != "fail", imperfect = w == "imperfect",
        note = sprintf("%s %s (%s %s%s)%s", what, format(signif(value, 4)), op, format(threshold),
                       if (tol > 0) sprintf(" +/- %s", format(tol)) else "",
                       if (w == "imperfect") ": within the tolerance of p. 64" else ""))
}

# A value inside [lo, hi] (each end with its tolerance), as an answer.
.rp_between <- function(value, lo, hi, tol = 0, what, attr) {
  .rp_all(if (!is.na(lo)) .rp_tol(value, lo, "min", tol, what, attr),
          if (!is.na(hi)) .rp_tol(value, hi, "max", tol, what, attr))
}


# --- the profile ---------------------------------------------------------------

#' The profile as the RP horizon identification reads it
#'
#' An environment, as the Russian readers use (.kidpr_profile()), so that the
#' Russian accessors serve: the horizons, the site, the depth order, the
#' designations read in the RP notation (.rp_designations()), the hints of
#' other notations, the depth datum, and caches.
#' @noRd
.rp_profile <- function(pedon) {
  x <- new.env(parent = emptyenv())
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  x$pedon <- pedon
  x$h <- h
  x$n <- n
  x$site <- pedon$site %||% list()
  x$top <- if (n) suppressWarnings(as.numeric(h$top_cm)) else numeric(0)
  x$bot <- if (n) suppressWarnings(as.numeric(h$bottom_cm)) else numeric(0)
  x$thick <- ifelse(is.na(x$top) | is.na(x$bot), NA_real_, pmax(0, x$bot - x$top))
  x$ord <- if (n) order(ifelse(is.na(x$top), Inf, x$top), seq_len(n)) else integer(0)
  x$rank <- match(seq_len(n), x$ord)
  x$des <- .rp_designations(if (n) h$designation else character(0), x$site)
  x$rp <- isTRUE(x$des$rp)
  x$cache <- new.env(parent = emptyenv())
  x$log <- replicate(n, list(), simplify = FALSE)
  # the hints of other notations read as RP codes and families
  voc <- .rp_vocabulary()
  x$hint_codes <- lapply(seq_len(n), function(i) .rp_hint_codes(x$des$hint[[i]], voc))
  # OL, OF and OH written in a profile not read in the RP notation (all in
  # capitals, or undecided) are still the litter, fragmentation and humified
  # O horizons as hints (the review's d5, m5)
  for (i in seq_len(n)) {
    d <- .rp_des(x, i)
    if (!(x$rp && isTRUE(x$des$known[i])) && isTRUE(grepl("^O[LFH]([nvtrmf]|zo|noz)*[0-9]*$", d)))
      x$hint_codes[[i]] <- substr(d, 1L, 2L)
  }
  x$hint_fam <- lapply(x$hint_codes, function(cc) unique(stats::na.omit(voc$info[cc, "family"])))
  .rp_datum(x)
  x
}

# The codes the hints of a designation name ("NaBT" is BT, "Eg" is E).
.rp_hint_codes <- function(hint, voc) {
  if (!length(hint)) return(character(0))
  cc <- vapply(hint, function(s) {
    r <- tryCatch(.rp_parse_one(s), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$known) || is.na(r$code)) NA_character_ else r$code
  }, "")
  unique(stats::na.omit(cc))
}

# The designation of a layer as recorded (NA when none).
.rp_des <- function(x, i) {
  d <- if (is.null(x$h$designation)) NA else x$h$designation[i]
  d <- as.character(d)
  if (length(d) != 1L || is.na(d) || !nzchar(trimws(d))) NA_character_ else d
}

# Accessors (the Russian readers' ones: the profile has the same fields).
.rp_num <- function(x, i, name) .kidpr_num(x, i, name)
.rp_txt <- function(x, i, name) .kidpr_txt(x, i, name)
.rp_site <- function(x, name) .kidpr_site(x, name)
.rp_site_txt <- function(x, name) .kidpr_site_txt(x, name)
.rp_memo <- function(x, key, f) .kidpr_memo(x, key, f)
.rp_prev <- function(x, i) .kidpr_prev(x, i)
.rp_next <- function(x, i) .kidpr_next(x, i)
.rp_above <- function(x, i) .kidpr_above(x, i)
.rp_below <- function(x, i) .kidpr_below(x, i)
.rp_lgl <- function(x, i, name) {
  v <- .kidpr_v(x, i, name)
  if (length(v) != 1L || is.na(v)) return(NA)
  if (is.logical(v)) return(v)
  t <- tolower(trimws(as.character(v)))
  if (t %in% c("true", "yes", "y", "1", "present", "t")) TRUE
  else if (t %in% c("false", "no", "n", "0", "absent", "none", "f")) FALSE else NA
}
.rp_site_lgl <- function(x, name) {
  v <- .rp_site(x, name)
  if (length(v) != 1L || is.na(v)) return(NA)
  if (is.logical(v)) return(v)
  t <- tolower(trimws(as.character(v)))
  if (t %in% c("true", "yes", "y", "1")) TRUE else if (t %in% c("false", "no", "n", "0")) FALSE else NA
}

# Does the hint of a layer name one of these codes or families?
.rp_hint_in <- function(x, i, codes) {
  any(x$hint_codes[[i]] %in% codes) || any(x$hint_fam[[i]] %in% codes)
}


# --- the depth datum (default 8, p. 64) ------------------------------------------

# Depths count from the top of the first layer that is not a litter: only a
# layer designated OL (an OL of the RP notation, the FAO Oi, the CPCS A00)
# is left out; an organic layer with no designation counts, with a note.
.rp_is_litter <- function(x, i) {
  d <- x$des
  if (x$rp && isTRUE(d$known[i])) return(isTRUE(d$code[i] %in% c("OL", "OLn", "OLv", "OLt")))
  if (isTRUE(grepl("^OL[nvt]?[0-9]*$", .rp_des(x, i)))) return(TRUE)
  hc <- x$hint_codes[[i]]
  length(hc) > 0L && all(hc == "OL")
}

.rp_datum <- function(x) {
  n <- x$n
  x$litter <- vapply(seq_len(n), function(i) .rp_is_litter(x, i), NA)
  keep <- x$ord[!x$litter[x$ord] & !is.na(x$top[x$ord])]
  x$datum <- if (length(keep)) x$top[keep[1L]] else if (n && any(!is.na(x$top))) min(x$top, na.rm = TRUE) else 0
  x$rtop <- x$top - x$datum
  x$rbot <- x$bot - x$datum
  x$datum_note <- character(0)
  if (any(x$litter)) x$datum_note <- sprintf("depths from %s cm, the top of the first layer that is not a litter OL (p. 64)",
                                              format(x$datum))
  if (length(keep)) {
    i1 <- keep[1L]
    oc <- .rp_num(x, i1, "oc_pct")
    if (is.na(.rp_des(x, i1)) && isTRUE(oc >= 20))
      x$datum_note <- c(x$datum_note, "the top layer is organic with no designation: it counts in the depths (Hugo's default 8)")
  }
  invisible(x)
}


# --- colours (default 11) -----------------------------------------------------------

# The moist colour of a layer (the dry one less one value when only it is
# recorded, flagged): the Russian reader's colour.
.rp_colour <- function(x, i, state = "moist") .kidpr_colour(x, i, state)
.rp_colour_text <- function(col) .kidpr_colour_text(col)

# A Munsell hue as a position on the hue circle, red to blue: 10RP 0, 5R 5,
# 10R 10, 5YR 15, 7.5YR 17.5, 10YR 20, 2.5Y 22.5, 5Y 25, then GY, G, BG, B,
# PB from 30. Neutral and unread hues are NA.
.rp_hue_pos <- function(col) {
  f <- col$family; num <- col$number
  if (is.null(f) || is.na(f) || is.na(num) || f == "N") return(NA_real_)
  base <- c(RP = -10, R = 0, YR = 10, Y = 20, GY = 30, G = 40, BG = 50, B = 60, PB = 70, P = 80)
  if (!f %in% names(base)) return(NA_real_)
  unname(base[f]) + num
}
# The hue between two hues (inclusive), three-valued.
.rp_hue_in <- function(col, lo, hi) {
  if (!is.na(col$family) && col$family == "N") return(FALSE)
  p <- .rp_hue_pos(col)
  if (is.na(p)) NA else p >= lo - 1e-9 && p <= hi + 1e-9
}
# Gley hues (bluish, greenish): GY, G, BG, B, PB.
.rp_gley_hue <- function(col) isTRUE(col$family %in% c("GY", "G", "BG", "B", "PB"))

# noir: value + chroma 4 or less (4 is noir); sombre: over 4 to 6 (default 10).
.rp_dark_word <- function(col) {
  if (is.na(col$value) || is.na(col$chroma)) return(NA_character_)
  s <- col$value + col$chroma
  if (s <= 4) "noir" else if (s <= 6) "sombre" else "clair"
}


# --- carbon (default 10, p. 65, Annex 4) ------------------------------------------------

#' The carbon class of a layer: mineral (OC under 0.1 g/100 g), organo-mineral
#' (to 8 +/- 2), hemiorganic (to 30 +/- 5), holorganic (above); the bands
#' 6-10 and 25-35 are noted as imperfect boundaries.
#' @noRd
.rp_carbon_class <- function(oc) {
  if (is.na(oc)) return(NA_character_)
  if (oc < 0.1) "mineral" else if (oc < 8) "organo-mineral" else if (oc < 30) "hemiorganic" else "holorganic"
}

#' The humic words of a solum (default 10, p. 376): humique when the dark
#' layers (noir or sombre, moist value + chroma 6 or less) run 20 cm or more
#' from the base of the O horizons (the top of the first mineral layer);
#' clinohumique when they run 40 cm or more with organic carbon 0.6 % or more
#' below 40 cm. Never a horizon's word.
#' @noRd
.rp_humique <- function(x) {
  pos <- .rp_positions(x)
  if (is.na(pos$first)) return(list(humique = NA, clinohumique = NA, thickness = NA_real_))
  run <- 0; deep_oc <- logical(0); undecided <- FALSE
  for (i in x$ord[x$rank >= x$rank[pos$first]]) {
    if (pos$organic[i]) next
    w <- .rp_dark_word(.rp_colour(x, i))
    if (is.na(w)) { undecided <- TRUE; break }
    if (w == "clair") break
    run <- run + (x$thick[i] %||% 0)
    oc <- .rp_num(x, i, "oc_pct")
    if (isTRUE(x$rbot[i] > 40)) deep_oc <- c(deep_oc, if (is.na(oc)) NA else oc >= 0.6)
  }
  deep_oc <- if (!length(deep_oc)) FALSE else .kidpr_or(deep_oc)
  hum <- if (run >= 20) TRUE else if (undecided) NA else FALSE
  clino <- if (run >= 40) deep_oc else if (undecided) NA else FALSE
  list(humique = hum, clinohumique = clino, thickness = run)
}

# Holorganic (an O or an H, Q19): ash under 50 % by the loss on ignition
# (loi_pct 50 or more), else organic carbon 30 % or more (25-35 imperfect);
# with neither, a designation of an organic horizon in another notation, the
# fibre or von Post of a peat; a layer whose particle size is recorded is
# mineral; otherwise undecided.
.rp_organic <- function(x, i) {
  .rp_memo(x, sprintf("organic_%d", i), function() {
    loi <- .rp_num(x, i, "loi_pct")
    oc <- .rp_num(x, i, "oc_pct")
    if (!is.na(loi)) {
      # a carbon that contradicts the loss on ignition is flagged (the review's m3)
      contra <- (loi < 50 && isTRUE(oc >= 25)) || (loi >= 50 && isTRUE(oc < 17))
      return(.rp_r(loi >= 50, tag = if (contra) "loi_oc_contradiction",
                   note = c(sprintf("loss on ignition %s %% (ash %s %%): %s", format(loi), format(100 - loi),
                                    if (loi >= 50) "holorganic, ash under 50 %" else "ash 50 % or more, not holorganic"),
                            if (contra) sprintf("the organic carbon %s %% contradicts it: the loss on ignition decides, flagged", format(oc)))))
    }
    if (!is.na(oc)) return(.rp_tol(oc, 30, "min", .RP_TOL$oc30, "organic carbon (g/100 g)", "oc_pct"))
    if (length(x$hint_fam[[i]]) && !any(x$hint_fam[[i]] %in% c("O", "H")))
      return(.rp_r(FALSE, note = sprintf("no carbon recorded: the designation '%s' names a mineral horizon", .rp_des(x, i))))
    if (.rp_hint_in(x, i, c("O", "H")))
      return(.rp_r(TRUE, tag = "designation_hint",
                   note = sprintf("no carbon or loss on ignition: the designation '%s' names an organic horizon", .rp_des(x, i))))
    fib <- .rp_num(x, i, "fiber_content_rubbed_pct"); vp <- .rp_num(x, i, "von_post_index")
    if (!is.na(fib) || !is.na(vp))
      return(.rp_r(TRUE, tag = "peat_descriptors", note = "no carbon recorded: the fibre content or von Post degree of a peat"))
    mineral <- any(!is.na(c(.rp_num(x, i, "clay_pct"), .rp_num(x, i, "sand_pct"), .rp_num(x, i, "silt_pct")))) ||
      !is.na(.rp_txt(x, i, "field_texture"))
    if (mineral) return(.rp_r(FALSE, note = "no carbon recorded; the particle size is recorded: a mineral layer"))
    .rp_r(NA, missing = c("oc_pct", "loi_pct"), note = "organic or mineral: no carbon, loss on ignition or particle size recorded")
  })
}

# Water-saturated more than 6 months a year (H, Q19): water_saturation_days
# over 182, or a very poorly drained site or water above the surface; 182
# days or fewer, or a well to moderately well drained site, is aerated (O).
.rp_saturated <- function(x, i) {
  d <- .rp_num(x, i, "water_saturation_days")
  if (!is.na(d)) return(.rp_r(d > 182, note = sprintf("water saturation %s days a year: %s", format(d),
                                                      if (d > 182) "more than 6 months" else "6 months or less")))
  wt <- suppressWarnings(as.numeric(.rp_site(x, "water_table_cm_above_surface")))
  if (isTRUE(wt > 0) || isTRUE(.rp_site_lgl(x, "subaqueous")))
    return(.rp_r(TRUE, note = "water above the surface (site)"))
  dr <- .rp_site_txt(x, "drainage_class")
  if (!is.na(dr)) {
    if (grepl("very poor|muito mal", dr)) return(.rp_r(TRUE, note = sprintf("drainage: %s", dr)))
    if (grepl("^(well|moderately well|excessiv|somewhat excessiv|rapid|bem|moderadamente|fortemente|acentuadamente|excessivamente)", dr))
      return(.rp_r(FALSE, note = sprintf("drainage: %s (aerated)", dr)))
  }
  .rp_r(NA, missing = c("water_saturation_days", "drainage_class"),
        note = "the water saturation is not recorded")
}

# Organic carbon of a layer as an answer against a minimum or maximum.
.rp_oc_min <- function(x, i, t, tol = 0) .rp_tol(.rp_num(x, i, "oc_pct"), t, "min", tol, "organic carbon (%)", "oc_pct")
.rp_oc_max <- function(x, i, t, tol = 0) .rp_tol(.rp_num(x, i, "oc_pct"), t, "max", tol, "organic carbon (%)", "oc_pct")


# --- texture and structure --------------------------------------------------------------

.rp_clay <- function(x, i) .rp_num(x, i, "clay_pct")
.rp_clay_min <- function(x, i, t) .rp_tol(.rp_clay(x, i), t, "min", 0, "clay (%)", "clay_pct")

# A pedogenic structure recorded (granular, crumb, blocky, prismatic ...):
# TRUE; massive, single grain or the rock's structure: FALSE; not recorded:
# NA (the structure is the description itself, not a feature: S against C).
.rp_structured <- function(x, i) {
  st <- .kidpr_structure(x, i)
  g <- .field_grade_class(st$grade)
  if (isTRUE(grepl(.KIDPR_RAW_STRUCTURE, st$type)) || isTRUE(g == 0L))
    return(.rp_r(FALSE, note = sprintf("structure %s: no pedological structure", .rp_first(st$type, st$grade))))
  if (isTRUE(grepl(.KIDPR_PED_STRUCTURE, st$type)) || isTRUE(g >= 1L))
    return(.rp_r(TRUE, note = sprintf("structure %s", paste(stats::na.omit(c(st$grade, st$size, st$type)), collapse = " "))))
  .rp_r(NA, missing = c("structure_type", "structure_grade"), note = "the structure is not recorded")
}
.rp_first <- function(a, b) if (length(a) && !is.na(a)) a else b

# The grade of the structure (0 structureless to 3 strong; NA).
.rp_grade <- function(x, i) .field_grade_class(.kidpr_structure(x, i)$grade)

# A biomacrostructured A (p. 10): crumb or granular biogenic aggregates over
# 25 % of the volume, most over 3 mm; recorded as biogenic_aggregates_pct and
# biogenic_aggregate_size. Not recorded: a moderate or strong crumb or
# granular structure with worm holes or a mull, flagged; else undecided
# (Hugo's default 21, map 1; Q23). `meso` accepts a biomesostructure too.
.rp_biomacro <- function(x, i, meso = FALSE) {
  pct <- .rp_num(x, i, "biogenic_aggregates_pct")
  sz <- .rp_txt(x, i, "biogenic_aggregate_size")
  ok_size <- if (is.na(sz)) NA else if (meso) sz %in% c("macro", "meso") else sz == "macro"
  if (!is.na(pct)) {
    if (pct <= 25) return(.rp_r(FALSE, note = sprintf("biogenic aggregates %s %%: not biostructured", format(pct))))
    if (isFALSE(ok_size)) return(.rp_r(FALSE, note = sprintf("biogenic aggregates mostly %s: not biomacrostructured", sz)))
    return(.rp_r(if (is.na(ok_size)) NA else TRUE, missing = "biogenic_aggregate_size",
                 note = sprintf("biogenic aggregates %s %%%s", format(pct), if (is.na(sz)) "" else paste0(", ", sz))))
  }
  # not recorded (Hugo's decision d3, 2026-11-02): a crumb or granular
  # structure with a recorded biological activity (worm holes, burrows, a
  # mull) reads biomacrostructured, flagged; a polyhedral, massive or other
  # structure reads not biostructured; nothing recorded stays undecided
  st <- .kidpr_structure(x, i)
  if (is.na(st$type))
    return(.rp_r(NA, missing = c("biogenic_aggregates_pct", "structure_type"), note = "the biogenic aggregates and the structure are not recorded"))
  crumb <- grepl("crumb|granul|grumel", st$type)
  if (!crumb) return(.rp_r(FALSE, note = sprintf("structure %s: not biostructured (Hugo's decision d3)", st$type)))
  wh <- .rp_num(x, i, "worm_holes_pct")
  bt <- .rp_txt(x, i, "bioturbation_density")
  mull <- isTRUE(grepl("mull", .rp_site_txt(x, "humus_form")))
  act <- isTRUE(wh > 0) || isTRUE(!is.na(bt) && !grepl("^none", bt)) || mull
  if (act)
    return(.rp_r(TRUE, tag = "biomacro_from_structure",
                 note = sprintf("biogenic aggregates not recorded: a %s structure with %s, flagged (Hugo's decision d3)", st$type,
                                if (isTRUE(wh > 0)) "worm holes" else if (mull) "a mull" else "burrows")))
  .rp_r(NA, missing = c("biogenic_aggregates_pct", "worm_holes_pct"),
        note = sprintf("structure %s: biological activity not recorded, the biogenic aggregates undecided", st$type))
}


# --- the exchange complex (Q12) ------------------------------------------------------------

# The CEC method of a layer: as printed (acetate at pH 7), another method
# standing in (flagged, lower grade), or not recorded (read as reported).
.rp_cec_method <- function(x, i) {
  m <- .rp_txt(x, i, "cec_method")
  if (is.na(m)) return(list(tag = character(0), note = "CEC method not recorded: read as reported"))
  if (grepl("acetate", m)) return(list(tag = character(0), note = character(0)))
  list(tag = "cec_method_substitute",
       note = sprintf("CEC by %s stands in for the acetate at pH 7 (p. 50), flagged", m))
}

# S/CEC % (bs_pct, else the four bases over the CEC).
.rp_bs <- function(x, i) {
  .rp_memo(x, sprintf("bs_%d", i), function() {
    m <- .rp_cec_method(x, i)
    bs <- .rp_num(x, i, "bs_pct")
    if (is.na(bs)) {
      b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
      cec <- .rp_num(x, i, "cec_cmol")
      if (!anyNA(b) && isTRUE(cec > 0)) bs <- min(100, 100 * sum(b) / cec)
    }
    list(value = bs, tag = m$tag, note = m$note)
  })
}
.rp_bs_test <- function(x, i, t, direction) {
  b <- .rp_bs(x, i)
  r <- .rp_tol(b$value, t, direction, .RP_TOL$bs, "S/CEC (%)", c("bs_pct", "cec_cmol"))
  r$tag <- unique(c(r$tag, b$tag)); r$note <- unique(c(r$note, if (!is.na(b$value)) b$note))
  r
}

# Ca/Mg exchangeable.
.rp_camg <- function(x, i) {
  ca <- .rp_num(x, i, "ca_cmol"); mg <- .rp_num(x, i, "mg_cmol")
  if (is.na(ca) || is.na(mg) || mg <= 0) NA_real_ else ca / mg
}

# Exchangeable Na over the sum of the alkaline and alkaline-earth cations
# (the sodic horizon, p. 24); esp_pct where the bases are missing, flagged.
.rp_na_bases <- function(x, i) {
  b <- vapply(c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol"), function(a) .rp_num(x, i, a), 0)
  if (!anyNA(b) && sum(b) > 0) return(list(value = 100 * b[["na_cmol"]] / sum(b), tag = character(0),
                                           note = "Na over the sum of the bases"))
  esp <- .rp_num(x, i, "esp_pct")
  if (is.na(esp)) {
    na <- .rp_num(x, i, "na_cmol"); cec <- .rp_num(x, i, "cec_cmol")
    if (!is.na(na) && isTRUE(cec > 0)) esp <- 100 * na / cec
  }
  if (!is.na(esp)) return(list(value = esp, tag = "esp_for_bases",
                               note = "Na over the CEC (ESP) where the bases are missing, flagged (Q12)"))
  list(value = NA_real_, tag = character(0), note = character(0))
}

# CEC per kg of clay.
.rp_cec_clay <- function(x, i) {
  cec <- .rp_num(x, i, "cec_cmol"); cl <- .rp_clay(x, i)
  if (is.na(cec) || is.na(cl) || cl <= 0) NA_real_ else 100 * cec / cl
}


# --- carbonates (default 17) ------------------------------------------------------------------

#' The carbonates of a layer: carbonated (CaCO3 2 % or more in the fine
#' earth, or a cold or hot effervescence), the generalised cold effervescence
#' (effervescence_class with effervescence_extent continuous; with no extent,
#' strong or very strong; with neither, CaCO3 2 % or more leaves it open,
#' since a dolomite does not effervesce cold), and the dolomitic reading
#' (molar CaCO3/MgCO3 under 1.5; else a hot effervescence only).
#' @noRd
.rp_carbonates <- function(x, i) {
  .rp_memo(x, sprintf("carb_%d", i), function() {
    cc <- .rp_num(x, i, "caco3_pct")
    ec <- .rp_txt(x, i, "effervescence_class")
    ex <- .rp_txt(x, i, "effervescence_extent")
    hot <- .rp_lgl(x, i, "effervescence_hot")
    mg <- .rp_num(x, i, "mgco3_pct")
    eff_any <- if (!is.na(ec)) !grepl("^(none|no|absent)", ec) else NA
    # a slight (very weak) cold effervescence is not generalised: it leaves
    # the calcite open unless the molar ratio is measured
    cold <- if (!is.na(ex)) ex == "continuous" && !isTRUE(grepl("slight|very weak", ec))
            else if (!is.na(ec)) { if (!isTRUE(eff_any)) FALSE else if (grepl("strong", ec)) TRUE
                                   else if (grepl("slight|very weak", ec)) NA
                                   else if (isTRUE(cc >= 2)) TRUE else NA }
            else if (isTRUE(cc < 0.5)) FALSE else NA
    ph <- .rp_num(x, i, "ph_h2o")
    acid <- is.na(cc) && is.na(ec) && is.na(ex) && !isTRUE(hot) && isTRUE(ph < 6.5)
    carbonated <- if (isTRUE(cc >= 2) || isTRUE(cold) || isTRUE(hot)) TRUE
                  else if (!is.na(cc)) FALSE
                  else if (isFALSE(eff_any) && !isTRUE(hot)) FALSE
                  else if (acid) FALSE else NA
    if (acid && is.na(cold)) cold <- FALSE
    ratio <- if (!is.na(cc) && isTRUE(mg > 0)) (cc / 100.09) / (mg / 84.31) else NA_real_
    if (!is.na(ratio) && is.na(cold) && isTRUE(eff_any)) cold <- ratio >= 1.5
    dolo <- if (!is.na(ratio)) ratio < 1.5
            else if (isTRUE(hot) && (isFALSE(eff_any) || isTRUE(grepl("slight|very weak", ec)))) TRUE
            else if (isTRUE(cold) && isTRUE(grepl("strong", ec))) FALSE
            else NA
    tag <- if (is.na(ratio) && isTRUE(dolo)) "dolomite_from_hot_effervescence" else character(0)
    list(caco3 = cc, cold = cold, carbonated = carbonated, ratio = ratio, dolomitic = dolo,
         eff = ec, extent = ex, hot = hot, tag = tag,
         note = c(if (!is.na(cc)) sprintf("CaCO3 %s %%", format(cc)),
                  if (!is.na(ec)) sprintf("effervescence %s%s", ec, if (!is.na(ex)) paste0(", ", ex) else ""),
                  if (!is.na(ratio)) sprintf("molar CaCO3/MgCO3 %s", format(signif(ratio, 3))),
                  if (acid) sprintf("no carbonates recorded; pH %s: not carbonated", format(ph))))
  })
}

# Not carbonated in the fine earth (Aci, Amg, Sci, Smg, Sp).
.rp_not_carbonated <- function(x, i) {
  c <- .rp_carbonates(x, i)
  p <- if (is.na(c$carbonated)) NA else !c$carbonated
  .rp_r(p, missing = c("caco3_pct", "effervescence_class"), note = c$note)
}

# The generalised cold effervescence with CaCO3 2 % or more (Aca, Sca).
.rp_calcareous <- function(x, i) {
  c <- .rp_carbonates(x, i)
  p <- .kidpr_and(c$cold, if (is.na(c$caco3)) NA else c$caco3 >= 2)
  # a cold effervescence recorded without CaCO3: calcareous by the field test
  if (is.na(p) && isTRUE(c$cold) && is.na(c$caco3)) p <- TRUE
  # a dolomitic molar ratio makes it Ado or Sdo
  if (isTRUE(p) && isTRUE(c$dolomitic)) p <- FALSE
  .rp_r(p, missing = c("effervescence_class", "caco3_pct"), note = c$note, tag = c$tag)
}

# Dolomitic (Ado, Sdo).
.rp_dolomitic <- function(x, i) {
  c <- .rp_carbonates(x, i)
  p <- .kidpr_and(c$carbonated, c$dolomitic)
  .rp_r(p, missing = c("mgco3_pct", "effervescence_hot"), note = c$note, tag = c$tag)
}


# --- coarse elements (Q11) ------------------------------------------------------------------------

#' Coarse elements over a threshold by weight of the dry whole soil (p. 249):
#' coarse_fragments_wt_pct, else the sum of the four classes by weight; else
#' the volume converted with a rock density of 2.65 and the recorded bulk
#' density of the fine earth, flagged; without the bulk density, a volume at
#' or above the threshold reads yes (the weight is never smaller, but for
#' porous rocks), under `band` no, between the two undecided. Not recorded:
#' no coarse elements (a field observation).
#' @noRd
.rp_coarse_wt <- function(x, i, threshold = 60, band = if (threshold >= 60) 35 else 20) {
  w <- .rp_num(x, i, "coarse_fragments_wt_pct")
  if (is.na(w)) {
    cls <- vapply(c("gravel_wt_pct", "cobbles_wt_pct", "stones_wt_pct", "boulders_wt_pct"),
                  function(a) .rp_num(x, i, a), 0)
    if (any(!is.na(cls))) w <- sum(cls, na.rm = TRUE)
  }
  if (!is.na(w)) return(.rp_r(w > threshold, value = w,
                              note = sprintf("coarse elements %s %% by weight (> %s)", format(w), format(threshold))))
  v <- .rp_num(x, i, "coarse_fragments_pct")
  if (is.na(v)) return(.rp_r(FALSE, value = 0, note = "coarse elements not recorded: none (a field observation)"))
  bd <- .rp_num(x, i, "bulk_density_g_cm3")
  if (!is.na(bd) && bd > 0 && v < 100) {
    wc <- 100 * v * 2.65 / (v * 2.65 + (100 - v) * bd)
    return(.rp_r(wc > threshold, value = wc, tag = "weight_from_volume",
                 note = sprintf("coarse elements %s %% by volume, %s %% by weight with a rock density of 2.65 and the bulk density %s, flagged (Q11)",
                                format(v), format(round(wc, 1)), format(bd))))
  }
  if (v >= threshold) return(.rp_r(TRUE, value = v, tag = "weight_from_volume_band",
                                   note = sprintf("coarse elements %s %% by volume: at least %s %% by weight, flagged (Q11)", format(v), format(threshold))))
  if (v < band) return(.rp_r(FALSE, value = v, note = sprintf("coarse elements %s %% by volume: under %s %% by weight (Q11)", format(v), format(threshold))))
  .rp_r(NA, missing = c("coarse_fragments_wt_pct", "bulk_density_g_cm3"), value = v, tag = "weight_from_volume_band",
        note = sprintf("coarse elements %s %% by volume: between %s and %s, undecided by weight (Q11)", format(v), format(band), format(threshold)))
}

# The size classes of the peyric horizons (p. 24): Xgr over 50 % gravel,
# Xc and Xp over 50 % cobbles, stones and boulders, by the dominant one; X
# when neither. By weight; the gravel by volume (gravel_pct) as a fallback,
# flagged.
.rp_peyric_kind <- function(x, i) {
  gr <- .rp_num(x, i, "gravel_wt_pct"); cb <- .rp_num(x, i, "cobbles_wt_pct")
  stn <- .rp_num(x, i, "stones_wt_pct"); bl <- .rp_num(x, i, "boulders_wt_pct")
  tag <- character(0)
  if (is.na(gr) && is.na(cb) && is.na(stn) && is.na(bl)) {
    gv <- .rp_num(x, i, "gravel_pct")
    if (!is.na(gv) && gv > 50) return(list(code = "Xgr", tag = "gravel_volume",
                                            note = sprintf("gravel %s %% by volume: Xgr, flagged", format(gv))))
    return(list(code = "X", variants = c("Xgr", "Xc", "Xp"), tag = tag,
                note = "the coarse elements by size are not recorded: the kind of X is undecided"))
  }
  big <- sum(c(cb, stn, bl), na.rm = TRUE)
  if (isTRUE(gr > 50)) return(list(code = "Xgr", note = sprintf("gravel %s %% by weight", format(gr))))
  if (big > 50) {
    k <- if (isTRUE(cb >= sum(c(stn, bl), na.rm = TRUE))) "Xc" else "Xp"
    return(list(code = k, note = sprintf("cobbles, stones and boulders %s %% by weight", format(big))))
  }
  list(code = "X", note = "neither gravel nor the larger elements over 50 %")
}


# --- hydromorphy (Q13, Q17, default 18) -------------------------------------------------------

#' The redox features of a layer: their share (redoximorphic_features_pct;
#' over 5 % makes -g, p. 281; 2-5 % is only peu tachete), their location
#' (redox_segregations_location: voids, ped faces or skeletans for Go, inside
#' the peds or the matrix for g) and whether they are recorded relict
#' (redox_relict: not functional, Q13). Mottles not recorded are absent;
#' colours of mottles recorded with no share leave the share undecided, and
#' soilKey's stagnic properties stand in, flagged.
#' @noRd
.rp_redox <- function(x, i) {
  .rp_memo(x, sprintf("redox_%d", i), function() {
    pct <- .rp_num(x, i, "redoximorphic_features_pct")
    loc <- .rp_txt(x, i, "redox_segregations_location")
    mot <- .rp_txt(x, i, "mottle_colors")
    relict <- isTRUE(.rp_lgl(x, i, "redox_relict"))
    kind <- if (is.na(loc)) NA_character_ else if (grepl("void|face|skeletan|pore|root", loc)) "Go" else "g"
    tag <- character(0); note <- character(0)
    g <- if (!is.na(pct)) pct > 5
         else if (!is.na(mot) || !is.na(loc)) {
           w <- .kidpr_wrb_layer(x, "stagnic_properties", i)
           tag <- "stagnic_properties"
           note <- sprintf("mottles recorded with no share: soilKey's stagnic properties %s",
                           if (is.na(w)) "undecided" else if (w) "stand in (yes)" else "stand in (no)")
           w
         } else FALSE
    if (!is.na(pct)) note <- sprintf("redox features %s %%%s", format(pct),
                                     if (pct > 2 && pct <= 5) ": peu tachete only (2-5 %, p. 281)" else "")
    if (!is.na(loc)) note <- c(note, sprintf("segregations at %s", loc))
    if (relict && isTRUE(g)) note <- c(note, "recorded relict (paleoredoxique): not a functional -g (Q13)")
    list(pct = pct, kind = kind, mottles = !is.na(mot) || isTRUE(pct > 0), relict = relict,
         g = g, functional = if (relict) FALSE else g, tag = tag, note = note)
  })
}

# A sign of water excess in a layer or at its site: redox features, a
# positive dipyridyl test, water saturation, a poorly drained site, a water
# table at the layer.
.rp_wet_sign <- function(x, i) {
  rx <- .rp_redox(x, i)
  ct <- .rp_txt(x, i, "childs_test")
  d <- .rp_num(x, i, "water_saturation_days")
  dr <- .rp_site_txt(x, "drainage_class")
  gw <- suppressWarnings(as.numeric(.rp_site(x, "groundwater_depth_cm")))
  isTRUE(rx$mottles) || isTRUE(grepl("^(pos|\\+|yes|true)", ct)) || isTRUE(d > 30) ||
    isTRUE(grepl("poor|imperfect|mal drenado", dr)) || (!is.na(gw) && !is.na(x$bot[i]) && gw <= x$bot[i])
}

#' A reduced matrix (G, p. 22, 280): moist colour of a gley hue (GY, G, BG,
#' B, PB) or neutral, value 4 or more; or chroma 2 or less, value 4 or more,
#' in another hue, with a sign of water excess (else a pale E or a grey rock
#' would read reductic: soilKey's reading); a positive dipyridyl test.
#' @noRd
.rp_reduced <- function(x, i) {
  .rp_memo(x, sprintf("reduced_%d", i), function() {
    ct <- .rp_txt(x, i, "childs_test")
    if (isTRUE(grepl("^(pos|\\+|yes|true)", ct))) return(.rp_r(TRUE, note = "positive dipyridyl test"))
    col <- .rp_colour(x, i)
    v <- col$value; c <- col$chroma
    neutral <- isTRUE(col$family == "N")
    if (is.na(v) || (is.na(c) && !neutral)) {
      if (.rp_wet_sign(x, i)) return(.rp_r(NA, missing = col$missing, tag = col$tag,
                                           note = "a sign of water excess; the colour of the matrix is not recorded"))
      return(.rp_r(FALSE, note = "no colour of the matrix and no sign of water excess recorded"))
    }
    txt <- .rp_colour_text(col)
    if (v < 4 && !.rp_gley_hue(col))
      return(.rp_r(FALSE, tag = col$tag, note = sprintf("%s: too dark for a reductic matrix", txt)))
    if (.rp_gley_hue(col) || neutral)
      return(.rp_r(TRUE, tag = col$tag, note = c(sprintf("%s: a bluish, greenish or neutral matrix", txt), col$note)))
    if (c <= 2) {
      # outside the gley and neutral hues, a measured sign of water at the
      # layer: saturation days or a water table recorded at it (a positive
      # dipyridyl test is read above); never the drainage class alone (the
      # review's D1)
      d <- .rp_num(x, i, "water_saturation_days")
      gw <- suppressWarnings(as.numeric(.rp_site(x, "groundwater_depth_cm")))
      wt <- suppressWarnings(as.numeric(.rp_site(x, "water_table_cm_above_surface")))
      flooded <- isTRUE(wt > 0) || isTRUE(.rp_site_lgl(x, "subaqueous")) || isTRUE(.rp_site_lgl(x, "tidal_inundation"))
      at <- (isTRUE(d > 30)) || (!is.na(gw) && !is.na(x$bot[i]) && gw <= x$bot[i]) || flooded
      if (at) return(.rp_r(TRUE, tag = col$tag, note = c(sprintf("%s: a grey matrix with %s", txt,
                                                                 if (isTRUE(d > 30)) "water saturation recorded" else "the water table at the layer or above the surface"), col$note)))
      return(.rp_r(FALSE, tag = col$tag, note = sprintf("%s: grey, but no dipyridyl test, saturation or water table at the layer recorded", txt)))
    }
    .rp_r(FALSE, tag = col$tag, note = sprintf("%s: not a reductic matrix", txt))
  })
}


# --- salts and sodium (Q12, p. 24) -----------------------------------------------------------------

# A salic horizon (the overlay Sa): EC of the paste extract 15 dS/m at some
# time of the year in the neutral series (paste pH under 8.5), 8 dS/m in the
# alkaline one; ec_max_dS_m, else ec_dS_m; the pH of the paste, else the
# pH in water, decides the series; between 8 and 15 with no pH, undecided.
# No EC: the field signs of salts (halite, a salt crust, a salinised site)
# leave it undecided; none recorded, no salic horizon.
.rp_salic <- function(x, i) {
  ec <- .rp_num(x, i, "ec_max_dS_m")
  if (is.na(ec)) ec <- .rp_num(x, i, "ec_dS_m")
  if (is.na(ec)) {
    sign <- isTRUE(.rp_num(x, i, "halite_visible_pct") > 0) || !is.na(.rp_txt(x, i, "salt_crust_pattern")) ||
      isTRUE(.rp_site_lgl(x, "salinised"))
    if (sign) return(.rp_r(NA, missing = "ec_dS_m", note = "signs of salts; the EC is not recorded"))
    return(.rp_r(FALSE, note = "no EC and no sign of salts recorded"))
  }
  ph <- .rp_num(x, i, "ph_h2o")
  if (ec >= 15) return(.rp_r(TRUE, note = sprintf("EC %s dS/m (15 or more)", format(ec))))
  if (ec < 8) return(.rp_r(FALSE, note = sprintf("EC %s dS/m (under 8)", format(ec))))
  if (is.na(ph)) return(.rp_r(NA, missing = "ph_h2o", note = sprintf("EC %s dS/m: salic in the alkaline series only; the pH is not recorded", format(ec))))
  .rp_r(ph > 8.5, note = sprintf("EC %s dS/m with pH %s (%s series)", format(ec), format(ph), if (ph > 8.5) "alkaline" else "neutral"))
}

# A sodic horizon (the overlay Na): exchangeable Na 15 % or more of the sum
# of the bases (Q12), a degraded structure (columnar, prismatic, massive or
# coarse polyhedral; not recorded: absent) and little soluble salt (no
# salic horizon). Under 15 % with a degraded structure and Mg over Ca,
# undecided (high Mg may compensate, p. 24).
.rp_sodic <- function(x, i) {
  st <- .kidpr_structure(x, i)
  na_c <- .rp_num(x, i, "na_cmol")
  # a columnar, prismatic or coarse structure; a massive one only with 1
  # cmol+/kg of exchangeable Na or more (the review's D12)
  degraded <- isTRUE(grepl("column|prism|coarse", paste(st$type, st$size, st$secondary))) ||
    (isTRUE(grepl("massive", st$type)) && isTRUE(na_c >= 1))
  nb <- .rp_na_bases(x, i)
  if (!degraded) return(.rp_r(FALSE, note = if (is.na(st$type)) "no degraded structure recorded" else sprintf("structure %s: not degraded", st$type)))
  sal <- .rp_salic(x, i)
  if (isTRUE(sal$passed)) return(.rp_r(FALSE, note = "salts over the salic threshold: a salic, not a sodic horizon"))
  if (is.na(nb$value)) return(.rp_r(NA, missing = c("na_cmol", "ca_cmol", "mg_cmol", "k_cmol"),
                                    note = "a degraded structure; the exchangeable sodium is not recorded"))
  if (nb$value >= 15) return(.rp_r(TRUE, tag = nb$tag, note = c(sprintf("Na %s %% with a degraded structure", format(round(nb$value, 1))), nb$note)))
  camg <- .rp_camg(x, i)
  if (!is.na(camg) && camg < 1) return(.rp_r(NA, missing = "na_cmol", tag = nb$tag,
                                             note = sprintf("Na %s %% under 15 with Mg over Ca: undecided (p. 24)", format(round(nb$value, 1)))))
  .rp_r(FALSE, tag = nb$tag, note = sprintf("Na %s %% (under 15)", format(round(nb$value, 1))))
}


# --- the domain gates (Q18, default 15) ------------------------------------------------------------

#' The intertropical domain (F, OX, RT, ND, FE, OXc and OXm are specific to it,
#' p. 6): declared (site$intertropical), a Koppen climate of group A, or a
#' latitude between the tropics (23.44 degrees); a Koppen climate of another
#' group with a latitude outside them, or either alone, is outside; neither
#' recorded, undecided.
#' @noRd
.rp_tropical <- function(x) {
  .rp_memo(x, "tropical", function() {
    dec <- .rp_site_lgl(x, "intertropical")
    if (!is.na(dec)) return(.rp_r(dec, note = sprintf("declared %s the intertropical domain", if (dec) "in" else "outside")))
    kp <- .rp_site(x, "koppen_climate")
    kp <- if (length(kp) == 1L && !is.na(kp)) trimws(as.character(kp)) else NA_character_
    lat <- suppressWarnings(as.numeric(.rp_site(x, "lat")))
    if (isTRUE(grepl("^A", kp))) return(.rp_r(TRUE, note = sprintf("Koppen %s: the intertropical domain", kp)))
    if (!is.na(lat) && abs(lat) <= 23.44) return(.rp_r(TRUE, note = sprintf("latitude %s: between the tropics", format(lat))))
    if (!is.na(kp) || !is.na(lat))
      return(.rp_r(FALSE, note = sprintf("%s: outside the intertropical domain",
                                          paste(c(if (!is.na(kp)) paste("Koppen", kp), if (!is.na(lat)) paste("latitude", format(lat))), collapse = ", "))))
    .rp_r(NA, missing = c("koppen_climate", "lat"), note = "the climate and the latitude are not recorded")
  })
}

#' The andosol domain of a layer (Alu, And, Avi, Slu, Snd, default 15):
#' soilKey's andic or vitric properties hold in the layer (stand-ins for the
#' andosolic properties of pp. 76-79, flagged). With no measurement of the
#' andic set and no sign of volcanic material recorded, outside.
#' @noRd
.rp_andic <- function(x, i) {
  .rp_memo(x, sprintf("andic_%d", i), function() {
    meas <- c("al_ox_pct", "fe_ox_pct", "phosphate_retention_pct", "volcanic_glass_pct", "ph_naf", "naf_test")
    rec <- vapply(meas, function(a) !is.na(.kidpr_v(x, i, a)), NA)
    lo <- .rp_txt(x, i, "layer_origin"); pm <- .rp_site_txt(x, "parent_material")
    volcanic <- isTRUE(grepl("tephr|volcan|pyrocl|ash|pumic|cinder|basalt", paste(lo, pm)))
    alo <- .rp_num(x, i, "al_ox_pct"); feo <- .rp_num(x, i, "fe_ox_pct")
    if (!is.na(alo) && !is.na(feo) && alo + feo / 2 < 2 && !isTRUE(.rp_num(x, i, "volcanic_glass_pct") >= 5))
      return(.rp_r(FALSE, note = sprintf("Alox + 1/2 Feox %s %%: under 2 %%, outside the andosol domain", format(alo + feo / 2))))
    if (!any(rec) && !volcanic) return(.rp_r(FALSE, note = "no andic measurement and no volcanic material recorded"))
    a <- .kidpr_wrb_layer(x, "andic_properties", i)
    v <- .kidpr_wrb_layer(x, "vitric_properties", i)
    p <- .kidpr_or(a, v)
    # an oxalate measurement alone does not open the gate: volcanic material
    # recorded, or the andic or vitric properties holding (the review's D11)
    if (!isTRUE(p) && !volcanic) p <- FALSE
    .rp_r(p, missing = c("al_ox_pct", "fe_ox_pct", "phosphate_retention_pct", "bulk_density_g_cm3"),
          tag = c(if (!is.na(a)) "andic_properties", if (!is.na(v)) "vitric_properties"),
          value = if (isTRUE(a)) "andic" else if (isTRUE(v)) "vitric" else NA_character_,
          note = sprintf("soilKey's andic properties %s, vitric properties %s (stand-ins for the andosolic properties)",
                         if (is.na(a)) "undecided" else if (a) "yes" else "no",
                         if (is.na(v)) "undecided" else if (v) "yes" else "no"))
  })
}


# --- ploughing ---------------------------------------------------------------------------------------

# A ploughed layer (the overlay L, p. 14): the designation of another
# notation (Ap, an FAO p), the depth of a mechanical mixing recorded at the
# site, or a cultivated land use for the first mineral layer from the
# surface, flagged.
.rp_ploughed <- function(x, i, first_mineral) {
  if ("L" %in% x$hint_codes[[i]] || "L" %in% x$hint_fam[[i]])
    return(.rp_r(TRUE, tag = "designation_hint", note = sprintf("the designation '%s' is a ploughed horizon", .rp_des(x, i))))
  md <- suppressWarnings(as.numeric(.rp_site(x, "mechanical_mixing_depth_cm")))
  if (!is.na(md) && !is.na(x$rtop[i]))
    return(.rp_r(x$rtop[i] < md, note = sprintf("mechanical mixing to %s cm", format(md))))
  lu <- .rp_site_txt(x, "land_use")
  if (isTRUE(grepl("crop|arable|cultiv|tillage|plough|plow|vineyard|lavoura|agric", lu)) && isTRUE(first_mineral))
    return(.rp_r(TRUE, tag = "land_use", note = sprintf("land use %s: the first mineral layer is ploughed, flagged", lu)))
  .rp_r(FALSE)
}


# --- WRB stand-ins (default 6) -------------------------------------------------------------------------

# A soilKey WRB diagnostic standing in for a field descriptor of one layer,
# as an answer with its tag.
.rp_stand_in <- function(x, i, name, what) {
  w <- .kidpr_wrb_layer(x, name, i)
  .rp_r(w, missing = character(0), tag = name,
        note = sprintf("%s not recorded: soilKey's %s() %s", what, name,
                       if (is.na(w)) "undecided" else if (w) "stands in (the layer has it)" else "stands in (the layer has not)"))
}


# --- the hydromorphy of the profile (for the references) -----------------------------------------------

#' The hydromorphy of a profile from its horizon frame (.rp_identify()):
#' the top of the first functional g or -g, the top of the first G, their
#' combined thickness, from the depth datum. NA where none is read.
#' @noRd
.rp_hydromorphy <- function(fr) {
  isG <- !is.na(fr$code) & fr$code %in% c("G", "Gr", "Go", "AG")
  isg <- vapply(seq_len(nrow(fr)), function(k) "g" %in% fr$features[[k]] || identical(fr$code[k], "g"), NA)
  top <- if (!is.null(fr$top_rp)) fr$top_rp else fr$top_cm
  bot <- if (!is.null(fr$bottom_rp)) fr$bottom_rp else fr$bottom_cm
  first <- function(w) if (any(w & !is.na(top))) min(top[w & !is.na(top)]) else NA_real_
  th <- bot - top
  list(g_top = first(isg), G_top = first(isG),
       thickness = if (any((isG | isg) & !is.na(th))) sum(th[(isG | isg) & !is.na(th)]) else NA_real_)
}
