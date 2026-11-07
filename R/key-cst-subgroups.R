# =============================================================================
# v0.9.375 -- the subgroups of the Chinese Soil Taxonomy (Cooperative
# Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil Taxonomy.
# Science Press, Beijing and New York, 2001; the English edition, system id
# cst2001): the criteria the keys to subgroups (key 5.2) define where they
# use them. The keys are inst/rules/cst2001/subgroups/{A..G}.yaml (orders A
# to G in this part), keyed by group; most rows call a preset of
# inst/rules/cst2001/presets.yaml (expanded at load time, R/rule-presets.R)
# whose template calls one of the criteria below or a horizon,
# characteristic or regime of the earlier parts.
#
# Written here, three-valued like the rest (a measurement not recorded leaves
# the test undecided, a field observation not recorded is absent):
#   * a lithic (or paralithic) contact within or between depths, with the
#     coarse fragments alternative and the extent under the plane of the
#     later orders (the Lithic subgroups);
#   * a feature in a run of layers, in layers totalling a thickness, or
#     anywhere, in a depth window (Mottlic, Gleyic, Endogleyic, Parasalic,
#     Sodic, Sulfic, Paracalcic, Paragypsic ...);
#   * the mineral layers of the Histosols (Buric, Terric), the kinds of the
#     subsurface and bottom tiers (Hemic, Sapric) and a lithic contact within
#     the control section;
#   * the carbonate pattern of anthro-recalcification (Recalcaric), the
#     soluble salts of the Parasalic Siltigi-Orthic Anthrosols, base
#     saturation or pH in a window (Dystric) and in a share of it
#     (Eutrophic), hues in a run, a share or the upper part of the B
#     (Mottlic, Xanthic), a horizon exposed at the surface (Trunic), the dark
#     mixed surface (Dark) and an argic horizon with lime infiltrates.
# The readings where the text is silent are in
# inst/rules/cst2001/readings.yaml (part 0.9.375).
# =============================================================================

# The datum of a test: the mineral soil surface or the soil surface.
.cst_sg_datum <- function(x, datum) if (identical(datum, "mineral")) x$msurf else x$surf

# The per-layer reading of a feature for the subgroup keys: a horizon or a
# layer characteristic (.cst_layer_feature()), or "calcic_only", the calcic
# horizon without its hypercalcic layers (the key names the calcic horizon
# alone, Hugo, 2026-11-07).
.cst_sg_feature <- function(x, feature, grade = c("full", "at_least_evidence")) {
  grade <- match.arg(grade)
  if (identical(feature, "calcic_only")) {
    r <- .cst_calcic_only(x)
    return(list(full = if (grade == "full") r$full else r$evid, prov = rep(FALSE, x$n),
                missing = r$missing %||% character(0), flags = r$flags %||% character(0)))
  }
  .cst_layer_feature(x, feature, grade)
}

# A per-layer test p in the window [a, b): with `min_cm` 0, in any layer
# that reaches into the window; otherwise in one run of contiguous layers
# with min_cm or more inside the window, or (total) in layers totalling
# min_cm. Returns the answer, the layers and the thickness range.
.cst_sg_run <- function(x, p, a, b, min_cm, total = FALSE) {
  ov <- .cst_overlap(x, a, b); ov[is.na(ov)] <- 0
  # the window below the base of the description is undecided, unless the
  # description ends on rock or a contact (Hugo, 2026-11-07): its thickness
  # joins the upper bound only; a window with no depth printed is read on
  # the described profile
  gap <- .cst_sg_gap(x, a, b)
  if (min_cm <= 0) {
    inw <- ov > 0
    passed <- if (any(p[inw] %in% TRUE)) TRUE else if (any(is.na(p[inw])) || gap > 0) NA else FALSE
    return(list(passed = passed, rows = which(inw & p %in% TRUE), low = NA_real_, high = NA_real_, gap = gap))
  }
  if (total) {
    lo <- sum(ov[p %in% TRUE]); hi <- sum(ov[!(p %in% FALSE)])
    used <- which(p %in% TRUE & ov > 0)
  } else {
    best <- function(q) {
      v <- 0; rows <- integer(0); i <- 1L
      while (i <= x$n) {
        if (q[i]) {
          j <- i
          while (j < x$n && q[j + 1L]) j <- j + 1L
          s <- sum(ov[i:j])
          if (s > v) { v <- s; rows <- i:j }
          i <- j + 1L
        } else i <- i + 1L
      }
      list(v = v, rows = rows)
    }
    rl <- best(p %in% TRUE); rh <- best(!(p %in% FALSE))
    lo <- rl$v; hi <- rh$v
    used <- rl$rows[ov[rl$rows] > 0]
  }
  if (gap > 0) {
    # a run may go on below the description: the run that reaches its base
    last <- which.max(x$bot)
    tail_run <- 0
    if (!total) {
      i <- last
      while (length(i) && i >= 1L && !(p[i] %in% FALSE)) { tail_run <- tail_run + ov[i]; i <- i - 1L }
    }
    hi <- if (total) hi + gap else max(hi, tail_run + gap)
  }
  list(passed = .cst_at_least(lo, hi, min_cm), rows = used, low = lo, high = hi, gap = gap)
}

# The thickness of the window [a, b) below the base of the description, 0
# when the description ends on rock (an R layer) or a contact, or the window
# has no depth printed (Hugo, 2026-11-07).
.cst_sg_gap <- function(x, a, b) {
  if (!x$n || !is.finite(b)) return(0)
  deep <- suppressWarnings(max(x$bot, na.rm = TRUE))
  if (!is.finite(deep) || deep >= b) return(0)
  last <- which(x$bot == deep)[1L]
  if (x$kind[last] %in% "R" || !is.na(.cst_contact_cm(x$pedon))) return(0)
  b - max(a, deep)
}

# "Every horizon of the window meets the test" (Q23), the window cut by the
# base of the description: undecided below it, and a window that passes on
# the described part only passes provisionally (flag
# window_cut_at_description_base), as the shares do (.cst_share_cut()).
.cst_every_cut <- function(pedon, x, p, a, b) {
  ev <- .cst_window_every(pedon, p, a, b)
  passed <- ev$value
  flags <- character(0)
  deep <- suppressWarnings(max(x$bot, na.rm = TRUE))
  if (is.na(passed) && is.finite(deep) && deep > a && deep < b) {
    cut <- .cst_window_every(pedon, p, a, deep)$value
    if (isTRUE(cut)) {
      passed <- TRUE
      flags <- c("window_cut_at_description_base", "provisional")
    }
  }
  list(passed = passed, rows = ev$rows %||% integer(0), flags = flags)
}

# The window [a, b) stopped at a shallower lithic or paralithic contact.
.cst_sg_window <- function(pedon, a, b) {
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && ct < b) b <- ct
  c(a = a, b = b)
}


# --- the Lithic subgroups -------------------------------------------------------------------------

#' A lithic contact (kind "lithic") or a lithic or paralithic contact (kind
#' "lithic_or_paralithic") whose top lies within `top_max` cm of the mineral
#' soil surface or the soil surface, and at `top_min` cm or deeper when
#' given (F3.3.1: between 50 and 100 cm). The later orders add two
#' alternatives, read here too: `fragments` "more_than_half" or
#' "half_or_more": without a contact, 70 % or more by volume of fragments
#' larger than 2 mm (coarse_fragments_pct) in more than half (or half or
#' more) of the horizons within `top_max`, by thickness (Q22); and
#' `plane_extent`: the contact under more than half of the plane (the karst
#' groups), read from site$lithic_contact_extent_pct, a contact recorded
#' without its extent passing provisionally (default 22 of 2026-11-04).
#' @noRd
contact_sg_cst <- function(pedon, top_max, kind = c("lithic", "lithic_or_paralithic"),
                           datum = c("mineral", "surface"), top_min = NULL,
                           fragments = c("none", "more_than_half", "half_or_more"),
                           plane_extent = FALSE) {
  kind <- match.arg(kind); datum <- match.arg(datum); fragments <- match.arg(fragments)
  nm <- "contact_sg_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  rs <- list(lithic_contact_cst(pedon, top_max = top_max, datum = datum))
  if (kind == "lithic_or_paralithic") rs <- c(rs, list(paralithic_contact_cst(pedon, top_max = top_max, datum = datum)))
  rows <- unlist(lapply(rs, function(r) if (isTRUE(r$passed)) match(r$layers, x$idx) else integer(0)))
  if (!is.null(top_min)) rows <- rows[!is.na(x$top[rows]) & x$top[rows] - s >= top_min]
  ct <- if (length(rows)) TRUE else if (any(vapply(rs, function(r) is.na(r$passed), NA))) NA else FALSE
  flags <- character(0)
  if (isTRUE(ct) && isTRUE(plane_extent)) {
    ext <- .cst_site_num(pedon$site %||% list(), "lithic_contact_extent_pct")
    t <- .cst_thr("key", "subgroups", "lithic")
    if (is.na(ext)) flags <- c("contact_extent_not_recorded", "provisional")
    else if (ext <= t$plane_extent_more_than_pct) ct <- FALSE
  }
  passed <- ct
  if (!isTRUE(ct) && fragments != "none") {
    t <- .cst_thr("key", "subgroups", "lithic")
    cf <- .cst_v(x, "coarse_fragments_pct")
    p <- cf >= t$fragments_min_pct
    p[x$kind %in% "R"] <- FALSE
    sh <- .cst_share_cut(pedon, x, p, s, s + top_max, 0.5, strict = fragments == "more_than_half")
    if (isTRUE(sh$passed)) flags <- c(flags, sh$flags, "fragments_in_place_of_contact")
    passed <- .cst_or(ct, sh$passed)
  }
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = flags,
                  missing = c(unlist(lapply(rs, `[[`, "missing")), if (fragments != "none") "coarse_fragments_pct"),
                  notes = sprintf("%s contact %s%s cm of the %s%s", gsub("_", " ", kind),
                                  if (is.null(top_min)) "within " else sprintf("between %s and ", format(top_min)),
                                  format(top_max), if (datum == "mineral") "mineral soil surface" else "soil surface",
                                  if (fragments != "none") ", or the fragments alternative" else ""))
}


# --- a feature in a depth window ----------------------------------------------------------------

#' A feature (horizon or layer characteristic ids, or "calcic_only") in the
#' window [from_cm, to_cm) below the mineral soil surface or the soil
#' surface: with `min_cm` 0 in any layer reaching into the window ("redoxic
#' features between 50 and 100 cm"); otherwise in one run of contiguous
#' layers with min_cm or more inside the window ("some horizon 10 cm or more
#' thick", C5), or with `total` in layers totalling min_cm ("horizons with a
#' total thickness of 10 cm"). Several features are read each on its own
#' run, any of them passing (M4.2 of the groups). Rock carries no feature;
#' the gleyic and redoxic features are of a mineral matrix. `grade =
#' "at_least_evidence"` lets the full diagnostic pass where the key asks for
#' an evidence (Q19). A result resting on provisional layers (gley colours
#' without a test, Q11) is provisional.
#' @noRd
feature_run_cst <- function(pedon, features, min_cm = 10, from_cm = 0, to_cm = Inf, total = FALSE,
                            datum = c("mineral", "surface"), grade = c("full", "at_least_evidence")) {
  datum <- match.arg(datum); grade <- match.arg(grade)
  nm <- "feature_run_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  a <- s + from_cm; b <- s + to_cm
  one <- function(f) {
    r <- .cst_sg_feature(x, f, grade)
    p <- r$full
    p[x$kind %in% "R"] <- FALSE
    if (f %in% c("gleyic_features", "redoxic_features")) p[x$kind %in% "O"] <- FALSE
    rr <- .cst_sg_run(x, p, a, b, min_cm, total)
    rr$prov <- isTRUE(rr$passed) && length(rr$rows) > 0L && all(r$prov[rr$rows] %in% TRUE)
    rr$missing <- r$missing; rr$flags <- r$flags
    rr
  }
  rs <- lapply(as.character(features), one)
  ok <- vapply(rs, function(r) r$passed, NA)
  passed <- .cst_or(ok)
  win <- which(ok %in% TRUE)
  prov <- isTRUE(passed) && all(vapply(rs[win], `[[`, NA, "prov"))
  rows <- sort(unique(unlist(lapply(rs[win], `[[`, "rows"))))
  .cst_key_result(nm, passed, layers = x$idx[rows],
                  missing = unique(unlist(lapply(rs, `[[`, "missing"))),
                  flags = c(unlist(lapply(rs[win], `[[`, "flags")), if (prov) "provisional"),
                  evidence = list(features = as.character(features), window = c(top = a, bottom = b),
                                  thickness = lapply(rs, function(r) c(low = r$low, high = r$high)), total = total),
                  notes = sprintf("%s %s %s-%s cm of the %s", paste(gsub("_", " ", features), collapse = " or "),
                                  if (min_cm <= 0) "anywhere in" else if (total) sprintf("in layers totalling %s cm in", format(min_cm))
                                  else sprintf("in a horizon %s cm thick in", format(min_cm)),
                                  format(from_cm), format(to_cm), if (datum == "mineral") "mineral soil surface" else "soil surface"))
}


# --- the Histosols (A) --------------------------------------------------------------------------

# Organic (TRUE), mineral (FALSE) or undecided per layer; rock is neither
# (NA is never given to it: it is not a mineral layer of the soil).
.cst_sg_organic <- function(x) {
  o <- .cst_ch(x, "organic_soil_materials")$full
  o[x$kind %in% "R"] <- NA
  o
}

#' A mineral layer less than `max_cm` thick from the soil surface over the
#' organic soil materials (Buric, A1.2.1, A1.3.1, A2.2.1, A2.3.1, A2.4.1,
#' pp. 77-80; default 24 of 2026-11-04): the run of mineral layers that
#' starts at the soil surface.
#' @noRd
buric_cst <- function(pedon, max_cm = 40) {
  nm <- "buric_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "77-80"))
  o <- .cst_sg_organic(x)
  if (x$kind[1L] %in% "R" || isTRUE(o[1L])) return(.cst_key_result(nm, FALSE, page = "77-80",
                                                                  notes = "no mineral layer at the soil surface"))
  rr <- .cst_runs(x, .cst_not_v(o) & !(x$kind %in% "R"), start = 1L)
  lo <- rr$low[1L]; hi <- rr$high[1L]
  passed <- if (is.na(o[1L])) NA else if (hi < max_cm) TRUE else if (lo >= max_cm) FALSE else NA
  if (isTRUE(passed) && !any(o %in% TRUE)) passed <- NA
  .cst_key_result(nm, passed, layers = x$idx[which(rr$lo_id %in% rr$lo_id[1L])],
                  missing = if (is.na(passed)) c("oc_pct", "clay_pct"),
                  notes = sprintf("a mineral surface layer %s cm thick", paste(unique(c(lo, hi)), collapse = " to ")),
                  page = "77-80")
}

#' A mineral layer `min_cm` thick or more whose upper boundary lies within
#' the subsurface tier of the control section (Terric, A1.2.2 ..., pp.
#' 77-81; tiers p. 46): a run of mineral layers starting in the tier, its
#' whole thickness counted.
#' @noRd
terric_cst <- function(pedon, min_cm = 30) {
  nm <- "terric_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "77-81"))
  tr <- .cst_histosol_tiers(pedon)
  if (is.na(tr$value)) return(.cst_key_result(nm, NA, missing = tr$missing, page = "77-81"))
  sub <- tr$tiers$subsurface
  a <- x$surf + sub[["top"]]; b <- x$surf + sub[["bottom"]]
  o <- .cst_sg_organic(x)
  m <- .cst_not_v(o); m[x$kind %in% "R"] <- FALSE
  rr <- .cst_runs(x, m)
  start <- function(id_vec, i) i == 1L || is.na(id_vec[i - 1L]) || id_vec[i - 1L] != id_vec[i]
  sure <- may <- FALSE
  for (i in seq_len(x$n)) {
    inside <- !is.na(x$top[i]) && x$top[i] >= a && x$top[i] < b
    if (!inside) next
    if (!is.na(rr$lo_id[i]) && start(rr$lo_id, i) && rr$low[i] >= min_cm) sure <- TRUE
    if (!is.na(rr$hi_id[i]) && rr$high[i] >= min_cm) may <- TRUE
  }
  # the tier below the base of the description is undecided (Hugo,
  # 2026-11-07)
  if (!sure && .cst_sg_gap(x, a, b) > 0) may <- TRUE
  passed <- if (sure) TRUE else if (may) NA else FALSE
  .cst_key_result(nm, passed, missing = if (is.na(passed)) c("oc_pct", "clay_pct", "bottom_cm"),
                  evidence = list(subsurface_tier = c(top = a, bottom = b)),
                  notes = sprintf("a mineral layer %s cm thick starting in the subsurface tier (%s-%s cm)",
                                  format(min_cm), format(a), format(b)),
                  page = "77-81")
}

#' Layers of one kind of organic soil material totalling `min_cm` or more in
#' the subsurface and bottom tiers of the control section (Hemic: 25 cm;
#' Sapric: 12.5 cm; A1.2.4, A1.2.5, A1.3.3, A2.2.4, A2.2.5, A2.3.4, pp.
#' 77-80), the two tiers read together, as in the KST.
#' @noRd
histosol_tier_layers_cst <- function(pedon, kind = c("hemic", "sapric", "fibric"), min_cm) {
  kind <- match.arg(kind)
  nm <- "histosol_tier_layers_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "77-80"))
  tr <- .cst_histosol_tiers(pedon)
  if (is.na(tr$value)) return(.cst_key_result(nm, NA, missing = tr$missing, page = "77-80"))
  a <- x$surf + tr$tiers$subsurface[["top"]]; b <- x$surf + tr$tiers$bottom[["bottom"]]
  r <- .cst_ch(x, "organic_soil_materials")
  p <- ifelse(r$full %in% FALSE | x$kind %in% "R", FALSE, ifelse(is.na(r$full) | is.na(r$kind), NA, r$kind == kind))
  rr <- .cst_sg_run(x, p, a, b, min_cm, total = TRUE)
  .cst_key_result(nm, rr$passed, layers = x$idx[rr$rows],
                  missing = if (is.na(rr$passed)) c("fiber_content_rubbed_pct", "oc_pct"),
                  evidence = list(window = c(top = a, bottom = b), thickness = c(low = rr$low, high = rr$high)),
                  notes = sprintf("%s layers totalling %s cm in the subsurface and bottom tiers (%s-%s cm)",
                                  kind, paste(unique(c(rr$low, rr$high)), collapse = " to "), format(a), format(b)),
                  page = "77-80")
}

#' A lithic contact within the control section of the Histosols (A1.2.3,
#' A2.2.3, A2.3.3, A2.4.3, [A1.3.x], pp. 77-81): the control section ends at
#' a shallower contact (p. 45), so a contact at its base counts; with the
#' control section undecided (130 or 160 cm), a contact within 130 cm passes
#' and one deeper than 160 cm fails.
#' @noRd
histosol_lithic_cs_cst <- function(pedon) {
  nm <- "histosol_lithic_cs_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "77-81"))
  lc <- lithic_contact_cst(pedon)
  if (!isTRUE(lc$passed)) return(.cst_key_result(nm, lc$passed, missing = lc$missing, page = "77-81"))
  d <- lc$evidence$contact_cm
  tr <- .cst_histosol_tiers(pedon)
  t <- .cst_cthr("organic")
  passed <- if (!is.na(tr$value)) d <= tr$value + 1e-9
            else if (d <= t$control_cm) TRUE else if (d > t$control_moss_cm) FALSE else NA
  .cst_key_result(nm, passed, layers = lc$layers, missing = if (is.na(passed)) tr$missing,
                  notes = sprintf("lithic contact at %s cm; control section %s cm", format(d),
                                  if (is.na(tr$value)) "undecided" else format(tr$value)),
                  page = "77-81")
}


# --- the Anthrosols (B) -------------------------------------------------------------------------

#' Anthro-recalcification (Recalcaric, B1.1.4, B1.4.3, pp. 82-84; default 23
#' of 2026-11-04): calcium carbonate within `within` cm of the soil surface
#' whose maximum, over `surface_min_pct` % (45 g/kg), lies in the surface
#' horizon and which decreases from there downwards, read on the measured
#' CaCO3 equivalent of two layers or more within the depth; the human origin
#' is not tested (the pattern alone).
#' @noRd
recalcaric_cst <- function(pedon, within = 60, surface_min_pct = NULL, datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  nm <- "recalcaric_cst"
  t <- .cst_thr("key", "subgroups", "recalcaric")
  surface_min_pct <- surface_min_pct %||% .cst_pct(t$surface_min_gkg)
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "82-84"))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct"), page = "82-84"))
  ov <- .cst_overlap(x, s, s + within); ov[is.na(ov)] <- 0
  rows <- which(ov > 0 & !(x$kind %in% c("R", "O")))
  v <- .cst_v(x, "caco3_pct")[rows]
  if (!length(rows)) return(.cst_key_result(nm, FALSE, page = "82-84"))
  first <- v[1L]
  known <- v[!is.na(v)]
  passed <- if (!is.na(first) && first <= surface_min_pct) FALSE
            else if (length(known) >= 2L && any(diff(known) > 0)) FALSE
            else if (length(known) >= 2L && !is.na(first) && known[length(known)] >= first) FALSE
            else if (is.na(first) || length(known) < 2L || anyNA(v)) NA
            else TRUE
  .cst_key_result(nm, passed, layers = x$idx[rows],
                  missing = if (is.na(passed)) "caco3_pct",
                  notes = sprintf("CaCO3 %s %% from the surface horizon down to %s cm; the origin is not tested",
                                  paste(format(v), collapse = ", "), format(within)),
                  page = "82-84")
}

#' The soluble salts of the Parasalic Siltigi-Orthic Anthrosols (B2.2.2, p.
#' 86): 5-20 g/kg in arid regions, 1.5-10 g/kg elsewhere, in the cultivated
#' horizon (the layers designated Ap from the mineral soil surface) or, with
#' none, the first mineral layer (the upper part of the pedon). The region
#' is .cst_arid_region() (Q14); unknown, a content in both bands passes, one
#' in neither fails.
#' @noRd
soluble_salt_band_cst <- function(pedon) {
  nm <- "soluble_salt_band_cst"
  t <- .cst_thr("key", "subgroups", "parasalic_salts")
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$first_min)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 86))
  d <- trimws(as.character(.cst_col(x$h, "designation")))
  ap <- which(!is.na(x$top) & x$top >= x$msurf & grepl("^A[a-z]*p", d))
  rows <- if (length(ap)) ap else x$first_min
  v <- .cst_v(x, "soluble_salts_pct")[rows]
  ar <- .cst_arid_region(pedon)
  band <- function(r) v >= .cst_pct(r[1L]) & v <= .cst_pct(r[2L])
  inA <- band(t$arid_gkg); inO <- band(t$other_gkg)
  per <- if (isTRUE(ar$value)) inA else if (isFALSE(ar$value)) inO
         else ifelse(inA & inO, TRUE, ifelse(!inA & !inO, FALSE, NA))
  passed <- .cst_or(per)
  .cst_key_result(nm, passed, layers = x$idx[rows[per %in% TRUE]],
                  missing = c(if (anyNA(v)) "soluble_salts_pct", if (is.na(ar$value) && is.na(passed)) ar$missing),
                  notes = sprintf("soluble salts %s %% in %s; region %s", paste(format(v), collapse = ", "),
                                  if (length(ap)) "the cultivated horizon" else "the first mineral layer",
                                  if (isTRUE(ar$value)) "arid" else if (isFALSE(ar$value)) "not arid" else "unknown"),
                  page = 86)
}

#' Base saturation (NH4OAc) under 50 % or pH (water, Q16) under 5.5
#' (Dystric): `scope` "window" in every horizon of [from_cm, to_cm) (Q23;
#' the window stops at a shallower contact and is cut by the base of the
#' description as .cst_every_cut() reads it), or with `mode` "some" in a run
#' of `min_cm` there; `scope` "b" throughout the B within 125 cm or in some
#' subhorizon of it `min_cm` thick.
#' @noRd
dystric_cst <- function(pedon, from_cm = 0, to_cm = 125, mode = c("throughout", "some"),
                        scope = c("window", "b"), min_cm = 10, datum = c("mineral", "surface")) {
  mode <- match.arg(mode); scope <- match.arg(scope); datum <- match.arg(datum)
  nm <- "dystric_cst"
  if (scope == "b") {
    r <- if (mode == "throughout") b_throughout_cst(pedon, test = "acid_or")
         else b_subhorizon_cst(pedon, test = "acid_or", min_cm = min_cm)
    return(.cst_key_result(nm, r$passed, layers = r$layers, missing = r$missing,
                           flags = r$evidence$flags, notes = r$notes))
  }
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  w <- .cst_sg_window(pedon, s + from_cm, s + to_cm)
  if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE, notes = "a contact above the window"))
  p <- .cst_b_test(x, "acid_or")$p
  if (mode == "throughout") {
    ev <- .cst_every_cut(pedon, x, p, w[["a"]], w[["b"]])
    passed <- ev$passed; rows <- ev$rows; flags <- ev$flags
  } else {
    rr <- .cst_sg_run(x, p, w[["a"]], w[["b"]], min_cm)
    passed <- rr$passed; rows <- rr$rows; flags <- character(0)
  }
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = flags,
                  missing = if (is.na(passed)) c("bs_pct", "ph_h2o"),
                  notes = sprintf("base saturation under 50 %% or pH under 5.5 %s %s-%s cm",
                                  if (mode == "throughout") "in every horizon of" else sprintf("in a run of %s cm in", format(min_cm)),
                                  format(w[["a"]]), format(w[["b"]])))
}


# --- colours ------------------------------------------------------------------------------------

# The per-layer hue test: "yellower" (the hue or yellower; a neutral hue
# passes, Hugo, 2026-11-07), "redder" (the hue or redder) or "redder_than"
# (strictly redder); `state` "moist" (Q21: the dry hue where no moist one is
# recorded, flagged) or "both" (the dry and the moist hue, where the book
# writes it).
.cst_sg_hue <- function(x, hue, rule = c("yellower", "redder", "redder_than"), state = c("moist", "both")) {
  rule <- match.arg(rule); state <- match.arg(state)
  r <- .cst_hue_rank(hue)
  col <- .cst_colours(x)
  test <- function(cl) switch(rule,
    yellower = .cst_hue_ge(cl, r),
    redder = .cst_hue_le(cl, r),
    redder_than = .cst_hue_le(cl, r - 1e-9))
  p <- test(col)
  flags <- col$hue_flag
  if (state == "both") {
    hd <- toupper(gsub("[[:space:]]", "", as.character(.cst_col(x$h, "munsell_hue_dry"))))
    cd <- list(rank = .cst_hue(x$h, "dry")$rank, neutral = !is.na(hd) & grepl("^N", hd))
    hm <- toupper(gsub("[[:space:]]", "", as.character(.cst_col(x$h, "munsell_hue_moist"))))
    cm <- list(rank = .cst_hue_rank(.cst_col(x$h, "munsell_hue_moist")), neutral = !is.na(hm) & grepl("^N", hm))
    p <- .cst_and_v(test(cm), test(cd))
    flags <- rep(NA_character_, x$n)
  }
  p[x$kind %in% c("R", "O")] <- FALSE
  list(p = p, flags = flags)
}

# Vectorised three-valued AND of two vectors.
.cst_and_v <- function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE, ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA))

#' A hue in a run of layers `min_cm` thick (or anywhere, `min_cm` 0) in the
#' window [from_cm, to_cm) (E1.3.5 Mottlic: 7.5YR or yellower in some
#' horizon 10 cm thick within 100 cm, p. 95).
#' @noRd
hue_run_cst <- function(pedon, hue, rule = c("yellower", "redder", "redder_than"), min_cm = 10, from_cm = 0,
                        to_cm = 125, total = FALSE, datum = c("mineral", "surface"), state = c("moist", "both")) {
  rule <- match.arg(rule); datum <- match.arg(datum); state <- match.arg(state)
  nm <- "hue_run_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  s <- .cst_sg_datum(x, datum)
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  h <- .cst_sg_hue(x, hue, rule, state)
  rr <- .cst_sg_run(x, h$p, s + from_cm, s + to_cm, min_cm, total)
  .cst_key_result(nm, rr$passed, layers = x$idx[rr$rows], flags = h$flags[rr$rows],
                  missing = if (is.na(rr$passed)) "munsell_hue_moist",
                  notes = sprintf("hue %s %s in a horizon %s cm thick within %s-%s cm", hue, rule, format(min_cm),
                                  format(from_cm), format(to_cm)))
}

#' A hue in a share of the window (the Rhodic, Red, Brown subgroups of the
#' later orders; Q22 by thickness): `scope` "window" the horizons of the
#' mineral soil surface to `within` cm, stopped at a shallower contact, or
#' "b" the B horizon within 125 cm; `share` with `strict` for "more than".
#' @noRd
hue_share_cst <- function(pedon, hue, rule = c("yellower", "redder", "redder_than"), share = 0.5, strict = TRUE,
                          within = 125, scope = c("window", "b"), state = c("moist", "both")) {
  rule <- match.arg(rule); scope <- match.arg(scope); state <- match.arg(state)
  nm <- "hue_share_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE))
  h <- .cst_sg_hue(x, hue, rule, state)
  p <- h$p
  w <- .cst_sg_window(pedon, x$msurf, x$msurf + within)
  if (scope == "b") {
    B <- .cst_b_layers(x)
    if (!any(B$b %in% TRUE)) return(.cst_key_result(nm, if (any(is.na(B$b))) NA else FALSE, flags = B$flag,
                                                     notes = "no B horizon"))
    rows <- which(B$b %in% TRUE)
    w <- c(a = min(x$top[rows]), b = min(max(x$bot[rows]), w[["b"]]))
    p[!(B$b %in% TRUE)] <- FALSE
  }
  if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE))
  sh <- .cst_share_cut(pedon, x, p, w[["a"]], w[["b"]], share, strict)
  .cst_key_result(nm, sh$passed, layers = x$idx[which(p %in% TRUE)], flags = c(sh$flags, stats::na.omit(h$flags)),
                  missing = if (is.na(sh$passed)) "munsell_hue_moist",
                  evidence = list(share = sh$share, window = w),
                  notes = sprintf("hue %s %s in %s of %s-%s cm", hue, rule, paste(format(round(sh$share, 2)), collapse = " to "),
                                  format(w[["a"]]), format(w[["b"]])))
}

#' A hue in the upper part, `min_cm` thick or more, of the B horizon within
#' 125 cm (Xanthic, E1.3.4, p. 95): every layer of the first `min_cm` of the
#' B meets it (the B as .cst_b_layers() reads it).
#' @noRd
b_upper_hue_cst <- function(pedon, hue = "7.5YR", rule = c("yellower", "redder", "redder_than"), min_cm = 10) {
  rule <- match.arg(rule)
  nm <- "b_upper_hue_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 95))
  B <- .cst_b_layers(x)
  k <- which(!(B$b %in% FALSE))[1L]
  if (is.na(k)) return(.cst_key_result(nm, FALSE, flags = B$flag, notes = "no B horizon", page = 95))
  if (is.na(B$b[k])) return(.cst_key_result(nm, NA, missing = "designation", flags = B$flag, page = 95))
  h <- .cst_sg_hue(x, hue, rule)
  ev <- .cst_every_cut(pedon, x, h$p, x$top[k], x$top[k] + min_cm)
  .cst_key_result(nm, ev$passed, layers = x$idx[ev$rows], flags = c(B$flag, ev$flags, stats::na.omit(h$flags[ev$rows])),
                  missing = if (is.na(ev$passed)) "munsell_hue_moist",
                  notes = sprintf("hue %s %s in the upper %s cm of the B (from %s cm)", hue, rule, format(min_cm),
                                  format(x$top[k])),
                  page = 95)
}

#' The dark mixed surface of the Aridosols (Dark, G1.1.3 ..., pp. 101-108):
#' a moist value of 3.5 or less and a dry (crushed) value of 5.5 or less in
#' the upper 18 cm of the soil after mixing, the thickness-weighted means of
#' the layers there (p. 21).
#' @noRd
dark_surface_cst <- function(pedon) {
  nm <- "dark_surface_cst"
  t <- .cst_thr("key", "subgroups", "dark")
  m <- .cst_mixed_surface(pedon, "munsell_value_moist", depth = t$depth_cm, datum = "surface")
  d <- .cst_mixed_surface(pedon, "munsell_value_dry", depth = t$depth_cm, datum = "surface")
  pm <- if (is.na(m$value)) NA else m$value <= t$value_moist_max
  pd <- if (is.na(d$value)) NA else d$value <= t$value_dry_max
  passed <- .cst_and(pm, pd)
  .cst_key_result(nm, passed, missing = if (is.na(passed)) c(if (is.na(pm)) "munsell_value_moist", if (is.na(pd)) "munsell_value_dry"),
                  evidence = list(value_moist = m$value, value_dry = d$value),
                  notes = sprintf("mixed 0-%s cm: moist value %s, dry value %s", format(t$depth_cm),
                                  format(round(m$value, 2)), format(round(d$value, 2))),
                  page = "101-108")
}

#' An argic horizon with lime infiltrates within `top_max` cm (Argic
#' Calci-Cryic Aridosols, G1.1.2, p. 101; default 23 of 2026-11-04): an
#' argic horizon (the natro-argic horizon included, p. 37) whose upper
#' boundary lies within the depth, with CaCO3 measured over 0, secondary
#' carbonates or an effervescence recorded in one of its layers; CaCO3
#' measured 0 (or no effervescence) in all of them fails.
#' @noRd
argic_lime_cst <- function(pedon, top_max = 100, datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  nm <- "argic_lime_cst"
  ar <- argic_horizon_cst(pedon, top_max = top_max, datum = datum, natro = TRUE)
  if (!isTRUE(ar$passed)) return(.cst_key_result(nm, ar$passed, missing = ar$missing, page = 101))
  x <- .cst_ctx(pedon)
  rows <- match(ar$layers, x$idx)
  cc <- .cst_v(x, "caco3_pct")[rows]
  sc <- .cst_sec_carb(x)
  eff <- .cst_t(x, "effervescence_class")[rows]
  lime <- ifelse((cc > 0) %in% TRUE | (sc$value[rows] > 0) %in% TRUE | (!is.na(eff) & eff != "none"), TRUE,
                 ifelse((cc <= 0) %in% TRUE | eff %in% "none", FALSE, NA))
  passed <- .cst_or(lime)
  .cst_key_result(nm, passed, layers = x$idx[rows[lime %in% TRUE]], missing = if (is.na(passed)) "caco3_pct",
                  notes = "lime infiltrates: CaCO3, secondary carbonates or effervescence in the argic horizon",
                  page = 101)
}

#' The order's horizon exposed at the soil surface (Trunic; default 22 of
#' 2026-11-04, amended on the review, Hugo, 2026-11-07): the first mineral
#' layer belongs to it (`horizon` an id of .cst_horizon_table(),
#' "argic_or_claypan", or "b": a layer designated B or BC, or a transition
#' dominated by the B, BA or B/A). A first mineral layer designated A, AB,
#' AC or E fails, before anything else is read; for the ferralic horizon
#' (the Ferralosols) the horizon test is made on the first layer whatever its
#' designation. site$anthropic_truncation recorded TRUE passes only when the
#' designation of the first layer is not known (flagged).
#' @noRd
trunic_cst <- function(pedon, horizon = "b") {
  nm <- "trunic_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "94-99"))
  k <- x$first_min
  if (is.na(k)) return(.cst_key_result(nm, NA, missing = "designation", page = "94-99"))
  d <- toupper(trimws(as.character(.cst_col(x$h, "designation"))[k]))
  d <- sub("^([0-9]+|I[IVX]*)(?=[A-Z])", "", d, perl = TRUE)
  known <- !is.na(d) && nzchar(d)
  btrans <- known && grepl("^(BA|B/A)", d)
  kd <- x$kind[k]
  trunc <- isTRUE(.cst_site_lgl(pedon$site %||% list(), "anthropic_truncation"))
  flags <- character(0)
  if (horizon == "ferralic_horizon") {
    passed <- .cst_hz(x, "ferralic_horizon")$full[k]
  } else if (known && !btrans && kd %in% c("A", "AB", "AC", "E", "O")) {
    return(.cst_key_result(nm, FALSE, notes = "an A or E horizon at the mineral soil surface", page = "94-99"))
  } else if (horizon == "b") {
    passed <- if (btrans) TRUE else if (known && !is.na(kd)) kd %in% c("B", "BC") else NA
  } else {
    hz <- if (horizon == "argic_or_claypan") c("argic_horizon", "claypan") else horizon
    passed <- .cst_or(vapply(hz, function(h) .cst_hz(x, h)$full[k], NA))
  }
  if (!known && trunc && !isTRUE(passed)) {
    passed <- TRUE
    flags <- "trunic_from_anthropic_truncation"
  }
  .cst_key_result(nm, passed, layers = if (isTRUE(passed)) x$idx[k] else integer(0), flags = flags,
                  missing = if (is.na(passed)) "designation",
                  notes = c(sprintf("the first mineral layer (%s) as the %s", if (known) d else "no designation",
                                    gsub("_", " ", horizon)),
                            if (length(flags)) "the site records an anthropic truncation"),
                  page = "94-99")
}

#' Base saturation (NH4OAc) of `min_pct` % or more in a share of the
#' thickness of the horizons within `within` cm of the mineral soil surface
#' (Eutrophic, E1.3.7, p. 95: 35 % in 80 % or more; Q22), the window stopped
#' at a shallower contact and cut by the base of the description as the
#' shares are.
#' @noRd
bs_share_cst <- function(pedon, min_pct = 35, share = 0.8, strict = FALSE, within = 125) {
  nm <- "bs_share_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 95))
  p <- .cst_v(x, "bs_pct") >= min_pct
  p[x$kind %in% "R"] <- FALSE
  w <- .cst_sg_window(pedon, x$msurf, x$msurf + within)
  if (w[["b"]] <= w[["a"]]) return(.cst_key_result(nm, FALSE, page = 95))
  sh <- .cst_share_cut(pedon, x, p, w[["a"]], w[["b"]], share, strict)
  .cst_key_result(nm, sh$passed, layers = x$idx[which(p %in% TRUE)], flags = sh$flags,
                  missing = if (is.na(sh$passed)) "bs_pct",
                  evidence = list(share = sh$share, window = w),
                  notes = sprintf("base saturation %s %% or more in %s of %s-%s cm", format(min_pct),
                                  paste(format(round(sh$share, 2)), collapse = " to "), format(w[["a"]]), format(w[["b"]])),
                  page = 95)
}


# --- the examples of the subgroups ------------------------------------------------------------

# The profile of each subgroup example (orders A to G): a group, suborder or
# subgroup example (`base`) with some of its layers (`rows`, which may repeat
# one) and some columns replaced (`set`, whole columns, recycled) and some
# site fields (`site`), or a profile of its own (`h`, `site`). The subgroups
# that a group example reaches take it as it is (`base` alone).
.cst_subgroup_example_spec <- function(code) {
  # the Histosols: a fibric permagelic profile, a hemic one, fibric, hemic
  # and sapric orthic ones, all organic through the control section
  fib <- list(designation = c("Oi", "Oe1", "Oe2", "Cf"), top_cm = c(0, 25, 70, 140), bottom_cm = c(25, 70, 140, 170),
              oc_pct = c(42, 40, 38, 2), fiber_content_rubbed_pct = c(85, 80, 80, NA), sphagnum_fibres_pct = c(80, 80, 80, NA),
              bulk_density_g_cm3 = c(0.08, 0.09, 0.09, 1.3), water_saturation_days = 200, permafrost_temp_C = c(NA, NA, NA, -1),
              clay_pct = c(NA, NA, NA, 20), silt_pct = c(NA, NA, NA, 40), sand_pct = c(NA, NA, NA, 40))
  cold <- list(soil_temperature_regime = "gelic", soil_moisture_regime = "aquic", drainage_class = "very poorly drained")
  hem <- list(designation = c("Oe1", "Oe2", "Oe3"), top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 170),
              oc_pct = c(40, 38, 36), fiber_content_rubbed_pct = 45, sphagnum_fibres_pct = 0,
              bulk_density_g_cm3 = c(0.2, 0.22, 0.25), water_saturation_days = 250)
  perm <- list(soil_temperature_regime = "permagelic", soil_moisture_regime = "aquic", drainage_class = "very poorly drained")
  wet <- list(soil_temperature_regime = "mesic", soil_moisture_regime = "aquic", drainage_class = "very poorly drained")
  org <- function(fiber, f2 = fiber, f3 = f2) list(designation = c("Oe1", "Oe2", "Oe3"), top_cm = c(0, 30, 90),
                                                    bottom_cm = c(30, 90, 140), oc_pct = c(35, 32, 30),
                                                    fiber_content_rubbed_pct = c(fiber, f2, f3), sphagnum_fibres_pct = c(5, 0, 0),
                                                    bulk_density_g_cm3 = c(0.2, 0.22, 0.25), water_saturation_days = 250)
  buric <- function(h) {
    k <- length(h$top_cm)
    h <- lapply(h, function(v) { v <- rep_len(v, k); c(v[1L], v) })
    h$designation[1L] <- "A"; h$top_cm <- c(0, 20, h$top_cm[-(1:2)]); h$bottom_cm[1L] <- 20
    h$oc_pct[1L] <- 3; h$fiber_content_rubbed_pct[1L] <- NA; h$sphagnum_fibres_pct[1L] <- 0
    h$bulk_density_g_cm3[1L] <- 1.1
    for (k2 in c("clay_pct", "silt_pct", "sand_pct")) h[[k2]] <- c(switch(k2, clay_pct = 20, silt_pct = 45, sand_pct = 35),
                                                                     rep(NA, length(h$top_cm) - 1L))
    h
  }
  mineral_at <- function(h, top, bottom) {
    h$designation <- c(h$designation[1:2], "Cg", "Oe3"); h$top_cm <- c(0, 30, top, bottom)
    h$bottom_cm <- c(30, top, bottom, 140); h$oc_pct <- c(h$oc_pct[1:2], 1, h$oc_pct[3L])
    h$fiber_content_rubbed_pct <- c(h$fiber_content_rubbed_pct[1:2], NA, h$fiber_content_rubbed_pct[3L])
    h$sphagnum_fibres_pct <- c(h$sphagnum_fibres_pct[1:2], NA, 0)
    h$bulk_density_g_cm3 <- c(h$bulk_density_g_cm3[1:2], 1.4, h$bulk_density_g_cm3[3L])
    h$clay_pct <- c(NA, NA, 25, NA); h$silt_pct <- c(NA, NA, 45, NA); h$sand_pct <- c(NA, NA, 30, NA)
    h
  }
  rock_at <- function(h, top) {
    k <- which(h$top_cm < top)
    h <- lapply(h, function(v) c(rep_len(v, length(h$top_cm))[k], NA))
    h$designation[length(h$designation)] <- "R"
    h$top_cm[length(h$top_cm)] <- top; h$bottom_cm[length(h$bottom_cm) - 1L] <- top
    h$bottom_cm[length(h$bottom_cm)] <- top + 20
    h
  }
  salty <- function(h) { h$soluble_salts_pct <- c(0.5, 0.05, 0.05)[seq_along(h$top_cm)]; h$ph_h2o <- 6.5; h }
  hz <- function(h, site) list(h = h, site = site)
  switch(code,
  # --- A ---
  "A1.1.1" = list(base = "A1.1"),
  "A1.1.2" = hz(list(designation = c("Oi", "C"), top_cm = c(0, 45), bottom_cm = c(45, 120), oc_pct = c(40, 1),
                     plant_residues_pct = c(85, NA), water_saturation_days = c(5, NA), bulk_density_g_cm3 = c(0.12, 1.4),
                     fiber_content_rubbed_pct = c(80, NA), clay_pct = c(NA, 20), silt_pct = c(NA, 45), sand_pct = c(NA, 35)),
                list(soil_temperature_regime = "permagelic", soil_moisture_regime = "udic")),
  "A1.2.1" = hz(buric(fib), cold),
  "A1.2.2" = list(base = "A1.2"),
  "A1.2.3" = hz(rock_at(fib, 140), perm),
  "A1.2.4" = hz(utils::modifyList(fib, list(fiber_content_rubbed_pct = c(85, 80, 30, NA), sphagnum_fibres_pct = c(80, 80, 10, NA))), cold),
  "A1.2.5" = hz(utils::modifyList(fib, list(fiber_content_rubbed_pct = c(85, 80, 10, NA), sphagnum_fibres_pct = c(80, 80, 0, NA),
                                            designation = c("Oi", "Oe", "Oa", "Cf"))), cold),
  "A1.2.6" = hz(fib, cold),
  "A1.3.1" = hz(buric(hem), perm),
  "A1.3.2" = list(base = "A1.3"),
  "[A1.3.x]" = hz(rock_at(hem, 120), perm),
  "A1.3.3" = hz(utils::modifyList(hem, list(fiber_content_rubbed_pct = c(45, 45, 10), designation = c("Oe1", "Oe2", "Oa"))), perm),
  "A1.3.4" = hz(hem, perm),
  "A2.1.1" = list(base = "A2.1"),
  "A2.1.2" = list(base = "A1.1.2", site = list(soil_temperature_regime = "mesic")),
  "A2.2.1" = hz(buric(org(80)), wet),
  "A2.2.2" = hz(mineral_at(org(80), 50, 90), wet),
  "A2.2.3" = hz(rock_at(org(80), 110), wet),
  "A2.2.4" = hz(org(80, 30, 80), wet),
  "A2.2.5" = list(base = "A2.2"),
  "A2.2.6" = hz(org(80), wet),
  "A2.3.1" = hz(buric(org(30)), wet),
  "A2.3.2" = hz(mineral_at(org(30), 50, 90), wet),
  "A2.3.3" = hz(rock_at(org(30), 110), wet),
  "A2.3.4" = list(base = "A2.3"),
  "A2.3.5" = hz(salty(org(30)), wet),
  "A2.3.6" = hz(org(30), wet),
  "A2.4.1" = hz(buric(org(10)), wet),
  "A2.4.2" = hz(mineral_at(org(10), 50, 90), wet),
  "A2.4.3" = hz(rock_at(org(10), 110), wet),
  "A2.4.4" = hz(salty(org(10)), wet),
  "A2.4.5" = list(base = "A2.4"),
  # --- B ---
  # the Stagnic Anthrosols: the group examples with the sulfidic test made,
  # the dry values recorded and a C that is neither grey nor gleyed
  "B1.1.1" = list(base = "B1.1.7", set = list(clay_pct = c(45, 48, 50, 45), silt_pct = c(35, 32, 30, 35), sand_pct = 20,
                                              slickensides = c(NA, NA, "few", NA), cracks_width_cm = c(1, 1, 1, NA))),
  "B1.1.2" = list(base = "B1.1.7", set = list(sulfidic_s_pct = c(0, 0, 1.2, 0), incubation_ph = c(NA, NA, 3.5, NA))),
  "B1.1.3" = list(base = "B1.1.7", set = list(soluble_salts_pct = c(0.05, 0.05, 0.5, 0.05))),
  "B1.1.4" = list(base = "B1.1.7", set = list(caco3_pct = c(6, 5, 3, 1), ph_h2o = c(7.8, 7.9, 8.0, 8.0))),
  "B1.1.5" = list(base = "B1.1.7", set = list(munsell_chroma_moist = c(1, 2, 1, 1), munsell_value_moist = c(4, 4, 5, 4),
                                              munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"), mottle_colors = c("7.5YR 5/6", NA, NA, NA),
                                              childs_test = c("positive", "positive", "negative", "positive"))),
  "B1.1.6" = list(base = "B1.1", set = list(sulfidic_s_pct = 0, munsell_value_dry = c(5, 4, 6, 7))),
  "B1.1.7" = list(base = "B1.1.6", set = list(fe_dcb_pct = 1.2)),
  "B1.2.1" = list(base = "B1.2.4", set = list(clay_pct = c(45, 48, 50, 45), silt_pct = c(35, 32, 30, 35), sand_pct = 20,
                                              slickensides = c(NA, NA, "few", NA), cracks_width_cm = c(1, 1, 1, NA))),
  "B1.2.2" = list(base = "B1.2.4", set = list(designation = c("Ap1", "Ap2", "Bg", "E"), munsell_hue_moist = "10YR",
                                              munsell_value_moist = c(4, 4, 5, 6), munsell_chroma_moist = c(1, 2, 1, 2),
                                              munsell_value_dry = c(5, 4, 6, 7), childs_test = "negative")),
  "B1.2.3" = list(base = "B1.2.4", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"), munsell_value_moist = c(4, 4, 5, 4),
                                              munsell_chroma_moist = c(1, 2, 1, 1), munsell_value_dry = c(5, 4, 6, 5),
                                              childs_test = c("negative", "negative", "negative", "positive"))),
  "B1.2.4" = list(base = "B1.2", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 5, 5),
                                            munsell_chroma_moist = c(1, 2, 1, 4), munsell_value_dry = c(5, 4, 6, 6))),
  "B1.3.1" = list(base = "B1.3.4", set = list(clay_pct = c(45, 48, 50, 45), silt_pct = c(35, 32, 30, 35), sand_pct = 20,
                                              slickensides = c(NA, NA, "few", NA), cracks_width_cm = c(1, 1, 1, NA))),
  "B1.3.2" = list(base = "B1.3.4", set = list(designation = c("Ap1", "Ap2", "Bg", "E"), munsell_hue_moist = "10YR",
                                              munsell_value_moist = c(4, 4, 5, 6), munsell_chroma_moist = c(1, 2, 4, 2),
                                              munsell_value_dry = c(5, 4, 6, 7), childs_test = "negative")),
  "B1.3.3" = list(base = "B1.3.4", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"), munsell_value_moist = c(4, 4, 5, 4),
                                              munsell_chroma_moist = c(1, 2, 4, 1), munsell_value_dry = c(5, 4, 6, 5),
                                              childs_test = c("negative", "negative", "negative", "positive"))),
  "B1.3.4" = list(base = "B1.3", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 5, 5),
                                            munsell_chroma_moist = c(1, 2, 4, 4), munsell_value_dry = c(5, 4, 6, 6))),
  "B1.4.1" = list(base = "B1.4.6", set = list(clay_pct = c(45, 48, 50, 45), silt_pct = c(35, 32, 30, 35), sand_pct = 20,
                                              slickensides = c(NA, NA, "few", NA), cracks_width_cm = c(1, 1, 1, NA))),
  "B1.4.2" = list(base = "B1.4.6", set = list(soluble_salts_pct = c(0.05, 0.05, 0.5, 0.05))),
  "B1.4.3" = list(base = "B1.4.6", set = list(caco3_pct = c(6, 5, 3, 1), ph_h2o = c(7.8, 7.9, 8.0, 8.0))),
  "B1.4.4" = list(base = "B1.4.6", set = list(designation = c("Ap1", "Ap2", "Bg", "E"), munsell_hue_moist = "10YR",
                                              munsell_value_moist = c(4, 4, 5, 6), munsell_chroma_moist = c(1, 2, 4, 2),
                                              munsell_value_dry = c(5, 4, 6, 7), childs_test = "negative")),
  "B1.4.5" = list(base = "B1.4.6", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"), munsell_value_moist = c(4, 4, 5, 4),
                                              munsell_chroma_moist = c(1, 2, 4, 1), munsell_value_dry = c(5, 4, 6, 5),
                                              childs_test = c("negative", "negative", "negative", "positive"))),
  "B1.4.6" = list(base = "B1.4", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 5, 5),
                                            munsell_chroma_moist = c(1, 2, 4, 4), munsell_value_dry = c(5, 4, 6, 6))),
  # the Orthic Anthrosols
  "B2.1.1" = list(base = "B2.1", set = list(silting_sieve_test = c("positive", NA, NA, NA))),
  "B2.1.2" = list(base = "B2.1.3", set = list(caco3_pct = c(2, 2, 2, 2), ph_h2o = 7.9)),
  "B2.1.3" = list(base = "B2.1", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"), munsell_chroma_moist = c(2, 2, 3, 2)),
                  site = list(drainage_class = "moderately well drained")),
  "B2.1.4" = list(base = "B2.1", set = list(caco3_pct = c(2, 2, 2, 2), ph_h2o = 7.9)),
  "B2.1.5" = list(base = "B2.1", set = list(ph_h2o = c(6.5, 5.2, 5.0, 5.0), bs_pct = c(70, 40, 40, 40))),
  "B2.1.6" = list(base = "B2.1"),
  "B2.2.1" = list(base = "B2.2", site = list(soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 10)),
  "B2.2.2" = list(base = "B2.2", set = list(soluble_salts_pct = c(0.5, 0.3, 0.2, 0.05))),
  "B2.2.3" = list(base = "B2.2", set = list(worm_holes_pct = c(60, 55, 50, NA), artefacts_pct = c(2, 1, 1, NA),
                                            p_olsen_mg_kg = c(20, 20, 20, 5), oc_pct = c(0.8, 0.6, 0.5, 0.1))),
  "B2.2.4" = list(base = "B2.2", set = list(munsell_value_moist = c(4, 4, 4, 6), munsell_chroma_moist = c(2, 2, 2, 4),
                                            mottle_colors = c("7.5YR 5/6", "7.5YR 5/6", NA, NA)),
                  site = list(land_use = "paddy rice", paddy_years_frac = 0.4)),
  "B2.2.5" = list(base = "B2.2", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"), munsell_chroma_moist = c(3, 3, 3, 2)),
                  site = list(drainage_class = "moderately well drained")),
  "B2.2.6" = list(base = "B2.2"),
  "B2.3.1" = list(base = "B2.3", set = list(ph_h2o = c(6.0, 5.0, 5.0, 5.2), bs_pct = c(60, 40, 40, 45), caco3_pct = 0)),
  "B2.3.2" = list(base = "B2.3"),
  "B2.4.1" = list(base = "B2.4.5", set = list(soluble_salts_pct = c(0.05, 0.05, 0.05, 0.5))),
  "B2.4.2" = list(base = "B2.4.5", set = list(worm_holes_pct = c(60, 55, 50, NA), artefacts_pct = c(2, 1, 1, NA),
                                              p_olsen_mg_kg = c(20, 20, 20, 5))),
  "B2.4.3" = list(base = "B2.4.5", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"), munsell_chroma_moist = c(3, 3, 3, 2)),
                  site = list(drainage_class = "moderately well drained")),
  "B2.4.4" = list(base = "B2.4"),
  "B2.4.5" = list(base = "B2.4", set = list(caco3_pct = 0, ph_h2o = c(6.8, 6.8, 6.8, 7.0))),
  # --- C ---
  "C1.1.1" = list(base = "C1.1"),
  "C2.1.1" = list(base = "C2.1", site = list(soil_temperature_regime = "gelic")),
  "C2.1.2" = list(base = "C2.1"),
  # --- D ---
  "D1.1.1" = list(base = "D1.1"),
  "D1.2.1" = list(base = "D2.1", site = list(soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 10)),
  "D1.2.2" = list(base = "D1.2"),
  "D1.2.3" = list(base = "D1.2.4", set = list(designation = c("Ah", "Bw", "R"), top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 70))),
  "D1.2.4" = list(base = "D1.2", set = list(munsell_value_moist = c(4, 4, 5), munsell_chroma_moist = c(3, 3, 3),
                                            munsell_value_dry = c(6, 6, 7))),
  "D2.1.1" = list(base = "D2.1", set = list(designation = c("Ah", "C1", "R"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 60))),
  "D2.1.2" = list(base = "D2.1", set = list(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 110), munsell_value_moist = c(2, 4, 5),
                                            munsell_value_dry = c(4, 6, 7), oc_pct = c(3, 0.4, 0.2))),
  "D2.1.3" = list(base = "D2.1"),
  "D2.2.1" = list(base = "D2.2", set = list(designation = c("Ah", "C1", "R"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 60))),
  "D2.2.2" = list(base = "D2.2", set = list(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 110), munsell_value_moist = c(2, 4, 5),
                                            munsell_value_dry = c(4, 6, 7), oc_pct = c(3, 0.4, 0.2))),
  "D2.2.3" = list(base = "D2.2"),
  "D3.1.1" = list(base = "D3.1.4", set = list(designation = c("Ah", "Bw", "R"), top_cm = c(0, 35, 45), bottom_cm = c(35, 45, 70))),
  "D3.1.2" = list(base = "D3.1.4", set = list(ph_kcl = c(4.6, 3.8, 4.4), al_kcl_cmol = c(0.5, 6, 1), al_sat_pct = c(10, 70, 20))),
  "D3.1.3" = list(base = "D3.1.4", set = list(designation = c("Ah", "Bt", "C"), clay_pct = c(25, 40, 35), silt_pct = c(55, 45, 45),
                                              sand_pct = c(20, 15, 20), clay_films_strength = c(NA, "distinct", NA),
                                              clay_films_amount = c(NA, "common", NA))),
  "D3.1.4" = list(base = "D3.1", set = list(ph_kcl = c(4.6, 4.8, 5.0), al_kcl_cmol = c(0.5, 0.3, 0.2), al_sat_pct = c(10, 8, 5))),
  "D3.2.1" = list(base = "D3.2.4", set = list(designation = c("Ah", "Bw", "R"), top_cm = c(0, 35, 45), bottom_cm = c(35, 45, 70))),
  "D3.2.2" = list(base = "D3.2.4", set = list(ph_kcl = c(4.6, 3.8, 4.4), al_kcl_cmol = c(0.5, 6, 1), al_sat_pct = c(10, 70, 20))),
  "D3.2.3" = list(base = "D3.2.4", set = list(designation = c("Ah", "Bt", "C"), clay_pct = c(25, 40, 35), silt_pct = c(55, 45, 45),
                                              sand_pct = c(20, 15, 20), clay_films_strength = c(NA, "distinct", NA),
                                              clay_films_amount = c(NA, "common", NA))),
  "D3.2.4" = list(base = "D3.2", set = list(ph_kcl = c(4.6, 4.8, 5.0), al_kcl_cmol = c(0.5, 0.3, 0.2), al_sat_pct = c(10, 8, 5))),
  # --- E ---
  "E1.1.1" = list(base = "E1.1", rows = c(2L, 3L), set = list(top_cm = c(0, 65), bottom_cm = c(65, 185))),
  "E1.1.2" = list(base = "E1.1", set = list(oc_pct = c(3, 1.2, 0.6), humus_coatings = c(NA, "common", NA))),
  "E1.1.3" = list(base = "E1.1", set = list(designation = c("Ap", "Bo1", "Bo2"), munsell_hue_moist = c("7.5YR", "2.5YR", "2.5YR"),
                                            munsell_value_moist = c(4, 3, 3), munsell_chroma_moist = c(2, 6, 6),
                                            munsell_value_dry = c(5, 4, 4), mottle_colors = c("7.5YR 5/6", NA, NA)),
                  site = list(land_use = "paddy rice", paddy_years_frac = 0.4)),
  "E1.1.4" = list(base = "E1.1"),
  "E1.2.1" = list(base = "E1.2", rows = c(2L, 3L), set = list(top_cm = c(0, 65), bottom_cm = c(65, 185))),
  "E1.2.2" = list(base = "E1.2", set = list(oc_pct = c(3, 1.2, 0.6), humus_coatings = c(NA, "common", NA))),
  "E1.2.3" = list(base = "E1.2", set = list(designation = c("Ap", "Bo1", "Bo2"), munsell_chroma_moist = c(2, 6, 6),
                                            mottle_colors = c("7.5YR 5/6", NA, NA)),
                  site = list(land_use = "paddy rice", paddy_years_frac = 0.4)),
  "E1.2.4" = list(base = "E1.2"),
  "E1.3.1" = list(base = "E1.3", rows = c(2L, 3L), set = list(top_cm = c(0, 65), bottom_cm = c(65, 185))),
  "E1.3.2" = list(base = "E1.3", set = list(oc_pct = c(3, 1.2, 0.6), humus_coatings = c(NA, "common", NA))),
  "E1.3.3" = list(base = "E1.3", set = list(designation = c("Ap", "Bo1", "Bo2"), munsell_hue_moist = c("7.5YR", "5YR", "5YR"),
                                            munsell_chroma_moist = c(2, 6, 6), mottle_colors = c("7.5YR 5/6", NA, NA)),
                  site = list(land_use = "paddy rice", paddy_years_frac = 0.4)),
  "E1.3.4" = list(base = "E1.3", set = list(munsell_hue_moist = c("5YR", "7.5YR", "5YR"), top_cm = c(0, 15, 40),
                                            bottom_cm = c(15, 40, 200)), site = list(udic_perudic_border = TRUE)),
  "E1.3.5" = list(base = "E1.3", set = list(munsell_hue_moist = c("5YR", "7.5YR", "5YR"))),
  "E1.3.6" = list(base = "E1.3", set = list(plinthite_pct = c(NA, NA, 20), plinthite_hardens = c(NA, NA, TRUE))),
  "E1.3.7" = list(base = "E1.3", set = list(bs_pct = c(45, 40, 40))),
  "E1.3.8" = list(base = "E1.3"),
  # --- F ---
  "F1.1.1" = list(base = "F1.1", site = list(climatic_moisture_regime = "ustic", cracks_open_days = 120, irrigated = FALSE)),
  "F1.1.2" = list(base = "F1.1.3", set = list(shajiang_pct = c(NA, NA, 15, NA))),
  "F1.1.3" = list(base = "F1.1", site = list(climatic_moisture_regime = "udic")),
  "F1.2.1" = list(base = "F1.2.4", site = list(climatic_moisture_regime = "ustic", cracks_open_days = 120, irrigated = FALSE)),
  "F1.2.2" = list(base = "F1.2.4", set = list(childs_test = c("negative", "negative", "positive", "negative"),
                                              munsell_hue_moist = c("10YR", "10YR", "5Y", "10YR"), munsell_value_moist = c(3, 3, 5, 4),
                                              munsell_chroma_moist = c(2, 2, 1, 3))),
  "F1.2.3" = list(base = "F1.2.4", set = list(caco3_pct = 2, ph_h2o = c(7.9, 8.0, 8.1, 8.2))),
  "F1.2.4" = list(base = "F1.2", set = list(childs_test = "negative"), site = list(climatic_moisture_regime = "udic")),
  "F2.1.1" = list(base = "F2.1", site = list(cracks_open_days = 220)),
  "F2.1.2" = list(base = "F2.1", rows = 2:4, set = list(top_cm = c(0, 42, 82), bottom_cm = c(42, 82, 122))),
  "F2.1.3" = list(base = "F2.1"),
  "F2.2.1" = list(base = "F2.2.3", site = list(cracks_open_days = 220)),
  "F2.2.2" = list(base = "F2.2", site = list(cracks_open_days = 220)),
  "F2.2.3" = list(base = "F2.2", rows = 2:4, set = list(top_cm = c(0, 42, 82), bottom_cm = c(42, 82, 122))),
  "F2.2.4" = list(base = "F2.2"),
  "F3.1.1" = list(base = "F3.1", set = list(caco3_pct = c(0, 0, 8, 4), secondary_carbonates_pct = c(0, 0, 3, 1),
                                            ph_h2o = c(6.8, 7.0, 8.0, 8.0))),
  "F3.1.2" = list(base = "F3.1"),
  "F3.2.1" = list(base = "F3.2", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6", NA))),
  "F3.2.2" = list(base = "F3.2"),
  "F3.3.1" = list(base = "F3.3", set = list(designation = c("Ap", "Bss1", "Bss2", "R"), top_cm = c(0, 18, 45, 80),
                                            bottom_cm = c(18, 45, 80, 100))),
  "F3.3.2" = list(base = "F3.3"),
  # --- G ---
  "G1.1.1" = list(base = "G1.1", set = list(designation = c("Avk", "Bw", "Bk", "R"), top_cm = c(0, 8, 20, 45),
                                            bottom_cm = c(8, 20, 45, 70))),
  "G1.1.2" = list(base = "G1.1", set = list(designation = c("Avk", "Bt", "Bk", "C"), clay_pct = c(12, 26, 14, 14),
                                            sand_pct = c(54, 44, 52, 52), silt_pct = c(34, 30, 34, 34),
                                            clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA),
                                            na_cmol = 0.2, mg_cmol = 1)),
  "G1.1.3" = list(base = "G1.1", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7))),
  "G1.1.4" = list(base = "G1.1"),
  "G1.2.1" = list(base = "G1.2", set = list(designation = c("Avk", "Bw", "By", "R"), top_cm = c(0, 8, 20, 45),
                                            bottom_cm = c(8, 20, 45, 70))),
  "G1.2.2" = list(base = "G1.2"),
  "G1.3.1" = list(base = "G1.3", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7))),
  "G1.3.2" = list(base = "G1.3"),
  "G1.4.1" = list(base = "G1.4", set = list(designation = c("Av", "Bw1", "Bw2", "R"), top_cm = c(0, 8, 20, 45),
                                            bottom_cm = c(8, 20, 45, 70))),
  "G1.4.2" = list(base = "G1.4", set = list(polygonal_cracks_spacing_cm = c(20, NA, NA, NA), cracks_width_cm = c(1.5, NA, NA, NA),
                                            cracks_depth_cm = c(2, NA, NA, NA))),
  "G1.4.3" = list(base = "G1.4", set = list(caco3_pct = c(2, 2, 3, 8), secondary_carbonates_pct = c(0, 0, 0, 3))),
  "G1.4.4" = list(base = "G1.4", set = list(caso4_pct = c(0, 0, 3, 1))),
  "G1.4.5" = list(base = "G1.4", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7))),
  "G1.4.6" = list(base = "G1.4"),
  "G2.1.1" = list(base = "G2.1.9", set = list(designation = c("Avk", "Bw", "Bk", "R"), top_cm = c(0, 8, 20, 45),
                                              bottom_cm = c(8, 20, 45, 70))),
  "G2.1.2" = list(base = "G2.1.9", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"))),
  "G2.1.3" = list(base = "G2.1.9", set = list(cementation_class = c(NA, NA, "strongly", NA))),
  "G2.1.4" = list(base = "G2.1.9", set = list(caco3_pct = c(8, 10, 60, 9), secondary_carbonates_pct = c(0, 1, 55, 1))),
  "G2.1.5" = list(base = "G2.1.9", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.1.6" = list(base = "G2.1.9", set = list(designation = c("Avk", "Bt", "Bk", "C"), clay_pct = c(12, 26, 14, 14),
                                              sand_pct = c(54, 44, 52, 52), silt_pct = c(34, 30, 34, 34),
                                              clay_films_strength = c(NA, "distinct", NA, NA), clay_films_amount = c(NA, "common", NA, NA))),
  "G2.1.7" = list(base = "G2.1.9", set = list(caso4_pct = c(0, 0, 0, 12), gypsum_visible_pct = c(0, 0, 0, 3))),
  "G2.1.8" = list(base = "G2.1.9", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7))),
  "G2.1.9" = list(base = "G2.1", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "G2.2.1" = list(base = "G2.2.6", set = list(designation = c("Av", "Bw", "Bz", "R"), top_cm = c(0, 5, 25, 50),
                                              bottom_cm = c(5, 25, 50, 70))),
  "G2.2.2" = list(base = "G2.2.6", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.2.3" = list(base = "G2.2.4", set = list(caso4_pct = c(0.5, 1, 1, 12), gypsum_visible_pct = c(0, 0, 0, 3))),
  "G2.2.4" = list(base = "G2.2.6", set = list(soluble_salts_pct = c(0.1, 0.2, 25, 0.8),
                                              cementation_class = c(NA, NA, "strongly", NA))),
  "G2.2.5" = list(base = "G2.2.6", set = list(caso4_pct = c(0.5, 1, 1, 12), gypsum_visible_pct = c(0, 0, 0, 3))),
  "G2.2.6" = list(base = "G2.2", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "G2.3.1" = list(base = "G2.3.6", set = list(designation = c("Av", "Bw", "By", "R"), top_cm = c(0, 5, 25, 50),
                                              bottom_cm = c(5, 25, 50, 70))),
  "G2.3.2" = list(base = "G2.3.6", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"))),
  "G2.3.3" = list(base = "G2.3.6", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.3.4" = list(base = "G2.3.6", set = list(caso4_pct = c(0.5, 1, 55, 3))),
  "G2.3.5" = list(base = "G2.3", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "G2.3.6" = list(base = "G2.3", set = list(na_cmol = 0.2, mg_cmol = 1, clay_pct = 12, silt_pct = 32, sand_pct = 56)),
  "G2.4.1" = list(base = "G2.4.3", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.4.2" = list(base = "G2.4.3", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7))),
  "G2.4.3" = list(base = "G2.4", set = list(na_cmol = 0.2, mg_cmol = 1)),
  "G2.5.1" = list(base = "G2.5.8", set = list(designation = c("Av", "Bw1", "Bw2", "R"), top_cm = c(0, 5, 25, 45),
                                              bottom_cm = c(5, 25, 45, 70))),
  "G2.5.2" = list(base = "G2.5.8", set = list(mottle_colors = c(NA, NA, NA, "7.5YR 5/6"))),
  "G2.5.3" = list(base = "G2.5.4", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.5.4" = list(base = "G2.5.8", set = list(polygonal_cracks_spacing_cm = c(20, NA, NA, NA), cracks_width_cm = c(1.5, NA, NA, NA),
                                              cracks_depth_cm = c(2, NA, NA, NA))),
  "G2.5.5" = list(base = "G2.5.8", set = list(esp_pct = c(2, 35, 35, 10), na_cmol = c(0.2, 3, 3, 1))),
  "G2.5.6" = list(base = "G2.5.8", set = list(caso4_pct = c(0.5, 0.5, 3, 1))),
  "G2.5.7" = list(base = "G2.5.8", set = list(munsell_value_moist = c(3, 3, 6, 6), munsell_value_dry = c(5, 5, 7, 7),
                                              caco3_pct = c(5, 6, 9, 5))),
  "G2.5.8" = list(base = "G2.5", set = list(na_cmol = 0.2, mg_cmol = 1, caco3_pct = 5)),
  NULL)
}

.CST_SUBGROUP_EXAMPLE_CODES <- c(
  "A1.1.1", "A1.1.2", "A1.2.1", "A1.2.2", "A1.2.3", "A1.2.4", "A1.2.5", "A1.2.6", "A1.3.1", "A1.3.2", "[A1.3.x]",
  "A1.3.3", "A1.3.4", "A2.1.1", "A2.1.2", "A2.2.1", "A2.2.2", "A2.2.3", "A2.2.4", "A2.2.5", "A2.2.6", "A2.3.1",
  "A2.3.2", "A2.3.3", "A2.3.4", "A2.3.5", "A2.3.6", "A2.4.1", "A2.4.2", "A2.4.3", "A2.4.4", "A2.4.5",
  "B1.1.1", "B1.1.2", "B1.1.3", "B1.1.4", "B1.1.5", "B1.1.6", "B1.1.7", "B1.2.1", "B1.2.2", "B1.2.3", "B1.2.4",
  "B1.3.1", "B1.3.2", "B1.3.3", "B1.3.4", "B1.4.1", "B1.4.2", "B1.4.3", "B1.4.4", "B1.4.5", "B1.4.6",
  "B2.1.1", "B2.1.2", "B2.1.3", "B2.1.4", "B2.1.5", "B2.1.6", "B2.2.1", "B2.2.2", "B2.2.3", "B2.2.4", "B2.2.5",
  "B2.2.6", "B2.3.1", "B2.3.2", "B2.4.1", "B2.4.2", "B2.4.3", "B2.4.4", "B2.4.5",
  "C1.1.1", "C2.1.1", "C2.1.2",
  "D1.1.1", "D1.2.1", "D1.2.2", "D1.2.3", "D1.2.4", "D2.1.1", "D2.1.2", "D2.1.3", "D2.2.1", "D2.2.2", "D2.2.3",
  "D3.1.1", "D3.1.2", "D3.1.3", "D3.1.4", "D3.2.1", "D3.2.2", "D3.2.3", "D3.2.4",
  "E1.1.1", "E1.1.2", "E1.1.3", "E1.1.4", "E1.2.1", "E1.2.2", "E1.2.3", "E1.2.4",
  "E1.3.1", "E1.3.2", "E1.3.3", "E1.3.4", "E1.3.5", "E1.3.6", "E1.3.7", "E1.3.8",
  "F1.1.1", "F1.1.2", "F1.1.3", "F1.2.1", "F1.2.2", "F1.2.3", "F1.2.4", "F2.1.1", "F2.1.2", "F2.1.3",
  "F2.2.1", "F2.2.2", "F2.2.3", "F2.2.4", "F3.1.1", "F3.1.2", "F3.2.1", "F3.2.2", "F3.3.1", "F3.3.2",
  "G1.1.1", "G1.1.2", "G1.1.3", "G1.1.4", "G1.2.1", "G1.2.2", "G1.3.1", "G1.3.2",
  "G1.4.1", "G1.4.2", "G1.4.3", "G1.4.4", "G1.4.5", "G1.4.6",
  "G2.1.1", "G2.1.2", "G2.1.3", "G2.1.4", "G2.1.5", "G2.1.6", "G2.1.7", "G2.1.8", "G2.1.9",
  "G2.2.1", "G2.2.2", "G2.2.3", "G2.2.4", "G2.2.5", "G2.2.6", "G2.3.1", "G2.3.2", "G2.3.3", "G2.3.4", "G2.3.5", "G2.3.6",
  "G2.4.1", "G2.4.2", "G2.4.3", "G2.5.1", "G2.5.2", "G2.5.3", "G2.5.4", "G2.5.5", "G2.5.6", "G2.5.7", "G2.5.8")

# Build the example of a subgroup from its spec.
.cst_subgroup_example <- function(code) {
  sp <- .cst_subgroup_example_spec(code)
  if (is.null(sp)) rlang::abort(sprintf("No example for %s.", code))
  id <- paste0("cst2001-", code)
  if (!is.null(sp$h)) return(.cst_ex_build(sp$h, c(list(id = id), sp$site %||% list())))
  p <- if (sp$base %in% .CST_SUBGROUP_EXAMPLE_CODES) .cst_subgroup_example(sp$base)
       else make_cst2001_example(sp$base)
  h <- as.data.frame(p$horizons)
  if (!is.null(sp$rows)) h <- h[sp$rows, , drop = FALSE]
  n <- nrow(h)
  for (k in names(sp$set)) h[[k]] <- rep_len(sp$set[[k]], n)
  site <- p$site
  for (k in names(sp$site)) site[k] <- list(sp$site[[k]])
  site$id <- id
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
