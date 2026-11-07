# =============================================================================
# v0.9.374 -- the groups of the Chinese Soil Taxonomy (Cooperative Research
# Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil Taxonomy. Science
# Press, Beijing and New York, 2001; the English edition, system id cst2001):
# the criteria the keys to groups (key 5.2, pp. 75-176) define where they use
# them. The keys themselves are inst/rules/cst2001/groups/{A..N}.yaml, keyed
# by suborder; most rows call the horizons, characteristics and regimes of
# the earlier parts directly.
#
# Written here, three-valued like the rest (a measurement not recorded leaves
# the test undecided, a field observation not recorded is absent):
#   * the Foli- groups of the Histosols and the kind of organic soil
#     material that predominates in the surface tier (A);
#   * the iron-percoleached grey subhorizon and the iron accumulation of the
#     hydragric horizon (B1), the hydromorphic cumulic epipedon (B2.3);
#   * colours in a share of the horizons within 125 cm (E1.1, E1.2) and the
#     udic regime bordering on the perudic one;
#   * a horizon whose top lies between two depths (L2.2), the thickness of
#     the mollic epipedon (J2.3), an albic horizon above the claypan (L1.1,
#     L4.1), folic histic evidence (M2.1), shajiang (M2.2);
#   * the takyric surface of the Halosols (H1.1);
#   * a lithologic character in layers within a depth (J1.1) or in more than
#     half of the horizons (M5.2);
#   * a property in some subhorizon of the B 10 cm thick (K2.2, K3.2, K3.3)
#     or throughout the B (L3.2, L4.4, L4.5, M4.4-M4.5, M5.4, M5.6).
# The readings where the text is silent are in
# inst/rules/cst2001/readings.yaml (part 0.9.374).
# =============================================================================


# --- the Histosols (A1, A2) -------------------------------------------------------------------

#' The Foli- groups of the Histosols (A1.1, A2.1, pp. 76, 78): never
#' saturated except for a few days a year, folic soil materials, and a lithic
#' or paralithic contact within 100 cm of the soil surface or organic soil
#' materials with cindery, fragmental or pumiceous materials totalling 40 cm
#' within 50 cm (item b of the order). "A few days": under 30 days of
#' saturation recorded in the organic layers, or a drainage class of well
#' drained or better, flagged as a stand-in (Hugo, 2026-11-07); 30 days or
#' more fails; otherwise undecided. Folic
#' materials: some organic layer of the folic kind (plant residues three
#' fourths or more).
#' @noRd
foli_histosol_cst <- function(pedon) {
  nm <- "foli_histosol_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = "76, 78"))
  t <- .cst_thr("key", "groups", "foli")
  r <- .cst_ch(x, "organic_soil_materials")
  org <- which(r$full %in% TRUE & !(x$kind %in% "R"))
  sat <- .cst_v(x, "water_saturation_days")[org]
  flags <- character(0)
  dry <- if (length(sat) && any(sat >= t$saturated_days_max, na.rm = TRUE)) FALSE
         else if (length(sat) && !anyNA(sat)) TRUE
         else if (.cst_well_drained(pedon$site)) {
           # the drainage class stands in for the days of saturation, flagged
           # (Hugo, 2026-11-07)
           flags <- c("stand_in:drainage_class", "few_days_from_drainage_class"); TRUE
         } else NA
  if (isTRUE(.cst_site_lgl(pedon$site, "artificially_drained")) && !isFALSE(dry)) dry <- NA
  k <- r$kind
  fol <- if (any(r$full %in% TRUE & k %in% "folic")) TRUE
         else if (any(!(r$full %in% FALSE) & is.na(k))) NA else FALSE
  ct <- .cst_or(lithic_contact_cst(pedon, top_max = t$contact_within_cm)$passed,
                paralithic_contact_cst(pedon, top_max = t$contact_within_cm)$passed)
  hm <- histosol_materials_cst(pedon)
  b <- hm$evidence$items[["b"]]
  passed <- .cst_and(dry, fol, .cst_or(ct, b))
  .cst_key_result(nm, passed, layers = x$idx[which(r$full %in% TRUE & k %in% "folic")],
                  missing = c(if (is.na(dry)) "water_saturation_days", if (is.na(fol)) "plant_residues_pct"),
                  flags = if (isTRUE(passed)) flags,
                  evidence = list(items = c(few_days = dry, folic = fol, contact = ct, item_b = b),
                                  stand_in = if (isTRUE(passed) && length(flags)) "drainage_class" else character(0)),
                  notes = "a few days of saturation read as under 30 days, or a well-drained record",
                  page = "76, 78")
}

#' The kind of organic soil material that predominates (by thickness) in the
#' surface tier of the control section of the Histosols (A1.2, A1.3, A2.2 -
#' A2.4, pp. 76, 78; tiers p. 46); thin mineral layers in the tier are left
#' out of the comparison, as the key allows them.
#' @noRd
histosol_tier_kind_cst <- function(pedon, kind = c("fibric", "hemic", "sapric")) {
  kind <- match.arg(kind)
  nm <- "histosol_tier_kind_cst"
  tr <- .cst_histosol_tiers(pedon)
  if (is.na(tr$value)) return(.cst_key_result(nm, NA, missing = tr$missing, page = "76, 78"))
  dom <- tr$dominant[["surface"]]
  passed <- if (is.na(dom)) NA else identical(dom, kind)
  .cst_key_result(nm, passed, missing = if (is.na(dom)) c("fiber_content_rubbed_pct", "oc_pct"),
                  evidence = list(dominant = tr$dominant, tiers = tr$tiers, control_section_cm = tr$value),
                  notes = sprintf("surface tier %s-%s cm, predominantly %s", format(tr$tiers$surface[["top"]]),
                                  format(tr$tiers$surface[["bottom"]]), dom %||% "NA"),
                  page = "76, 78")
}


# --- the Anthrosols (B1, B2) --------------------------------------------------------------------

#' An iron-percoleached grey subhorizon immediately beneath the anthrostagnic
#' epipedon (B1.2, p. 82; the subhorizon of the hydragric horizon, item 2d,
#' p. 35): the layer whose top is the base of the epipedon, not an albic
#' horizon, with a hue of 10YR to 7.5Y, a moist value of 5-6 and a moist
#' chroma of 2 or less (the hydragric reader's grey). v0.9.375: with
#' `above_gleyic`, a layer with gleyic features lies below it (B1.1.5, p. 83).
#' @noRd
fe_percoleached_cst <- function(pedon, above_gleyic = FALSE) {
  nm <- "fe_percoleached_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 82))
  an <- .cst_hz(x, "anthrostagnic_epipedon")
  ep <- which(an$full %in% TRUE)
  if (!length(ep)) return(.cst_key_result(nm, if (any(is.na(an$full))) NA else FALSE,
                                          missing = an$missing, page = 82))
  k <- max(ep) + 1L
  if (k > x$n) return(.cst_key_result(nm, FALSE, notes = "nothing described beneath the epipedon", page = 82))
  t <- .cst_thr("horizons", "hydragric")
  col <- .cst_colours(x)
  grey <- .cst_hue_in(col, t$grey_hue_rank[1L], t$grey_hue_rank[2L], neutral = TRUE) &
    col$vm >= t$grey_value_moist[1L] & col$vm <= t$grey_value_moist[2L] & col$c_any <= t$grey_chroma_max
  alb <- .cst_hz(x, "albic_horizon")$full
  passed <- .cst_and(grey[k], .cst_not(alb[k]))
  # v0.9.375 (B1.1.5): between the epipedon and the horizon with gleyic
  # features, which lies below the grey subhorizon
  if (isTRUE(above_gleyic) && !isFALSE(passed)) {
    gl <- .cst_ch(x, "gleyic_features")$full
    below <- if (k < x$n) gl[(k + 1L):x$n] else logical(0)
    passed <- .cst_and(passed, if (any(below %in% TRUE)) TRUE else if (any(is.na(below))) NA else FALSE)
  }
  .cst_key_result(nm, passed, layers = if (isTRUE(passed)) x$idx[k] else integer(0),
                  missing = .cst_missing_in(x, k, c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist")),
                  flags = col$hue_flag[k], page = 82)
}

#' DCB iron of the hydragric horizon 1.5 times or more that of the
#' anthrostagnic epipedon (B1.3, p. 82): thickness-weighted means of
#' fe_dcb_pct over the layers of each.
#' @noRd
fe_accumulation_cst <- function(pedon, ratio = 1.5) {
  nm <- "fe_accumulation_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 82))
  an <- .cst_hz(x, "anthrostagnic_epipedon")$full
  hy <- .cst_hz(x, "hydragric_horizon")$full
  ea <- which(an %in% TRUE); hh <- which(hy %in% TRUE)
  if (!length(ea) || !length(hh))
    return(.cst_key_result(nm, if (anyNA(an) || anyNA(hy)) NA else FALSE, page = 82))
  fe <- .cst_v(x, "fe_dcb_pct")
  wm <- function(rows) if (anyNA(fe[rows]) || anyNA(x$thk[rows])) NA_real_ else sum(fe[rows] * x$thk[rows]) / sum(x$thk[rows])
  a <- wm(ea); b <- wm(hh)
  passed <- if (is.na(a) || is.na(b)) NA else b >= ratio * a - 1e-9
  .cst_key_result(nm, passed, layers = x$idx[hh], missing = if (is.na(passed)) "fe_dcb_pct",
                  evidence = list(fe_epipedon = a, fe_hydragric = b),
                  notes = sprintf("DCB Fe %s %% in the hydragric horizon against %s %% in the epipedon",
                                  format(round(b, 2)), format(round(a, 2))),
                  page = 82)
}

#' A cumulic epipedon with hydromorphic features (B2.3, p. 85): in its
#' layers, rust mottles or redox concentrations on a matrix of moist chroma
#' 2 or less, or shells (shelly_layer). Field observations: not recorded,
#' absent.
#' @noRd
mud_cumulic_cst <- function(pedon) {
  nm <- "mud_cumulic_cst"
  x <- .cst_ctx(pedon)
  cu <- cumulic_epipedon_cst(pedon)
  if (!isTRUE(cu$passed)) return(.cst_key_result(nm, cu$passed, missing = cu$missing, page = 85))
  rows <- match(cu$layers, x$idx)
  t <- .cst_thr("key", "groups", "mud_cumulic")
  mc <- as.character(.cst_col(x$h, "mottle_colors"))
  rust <- vapply(seq_len(x$n), function(i) {
    if (is.na(mc[i]) || !nzchar(trimws(mc[i]))) return(FALSE)
    m <- .munsell_notations(mc[i])
    nrow(m) > 0L && any(is.na(m$chroma) | m$chroma >= t$rust_chroma_min)
  }, NA) | (.cst_v(x, "redoximorphic_features_pct") > 0) %in% TRUE
  col <- .cst_colours(x)
  low <- col$c_any <= t$matrix_chroma_max
  shells <- .cst_l(x, "shelly_layer") %in% TRUE
  p <- (rust & low) | shells
  ok <- .cst_any_rows(p, rows)
  .cst_key_result(nm, ok, layers = x$idx[rows[p[rows] %in% TRUE]],
                  missing = if (is.na(ok)) "munsell_chroma_moist",
                  notes = "rust mottles, redox concentrations and shells not recorded are absent", page = 85)
}


# --- the Ferralosols (E1) -------------------------------------------------------------------------

#' Colours in half or more of the horizons within 125 cm of the mineral soil
#' surface (E1.1, E1.2, p. 93), by the thickness of the window (Q22): "rhodic"
#' a moist hue redder than 5YR, a moist value under 4 and a dry value no more
#' than one unit above the moist; "xanthic" a hue of 7.5YR or yellower, a
#' moist value of 4 or more and a moist chroma of 5 or more. The window ends
#' at a shallower contact; the depth below the description is undecided, and
#' a share that passes on the described part only passes provisionally.
#' @noRd
colour_share_cst <- function(pedon, rule = c("rhodic", "xanthic"), min = 0.5, within = 125) {
  rule <- match.arg(rule)
  nm <- "colour_share_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 93))
  t <- .cst_thr("key", "groups", "ferralosol_colours")
  col <- .cst_colours(x)
  p <- if (rule == "rhodic") {
    .cst_hue_le(col, t$rhodic_hue_rank_below - 1e-9) & col$vm < t$rhodic_value_moist_below &
      col$vd <= col$vm + t$rhodic_dry_above_moist_max
  } else {
    .cst_hue_ge(col, t$xanthic_hue_rank_min) & col$vm >= t$xanthic_value_moist_min &
      col$cm >= t$xanthic_chroma_moist_min
  }
  p[x$kind %in% c("R", "O")] <- FALSE
  a <- x$msurf; b <- a + within
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && ct < b) b <- ct
  if (b <= a) return(.cst_key_result(nm, FALSE, page = 93))
  sh <- .cst_share_cut(pedon, x, p, a, b, min)
  passed <- sh$passed
  rows <- which(p %in% TRUE & .cst_overlap(x, a, b) > 0)
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = c(col$hue_flag[rows], sh$flags),
                  missing = .cst_missing_in(x, which(is.na(p)), c("munsell_hue_moist", "munsell_value_moist",
                                                                  if (rule == "rhodic") "munsell_value_dry" else "munsell_chroma_moist")),
                  evidence = list(share = sh$share, window = c(top = a, bottom = b), rule = rule),
                  notes = c(sprintf("%s colours in %s of %s-%s cm", rule,
                                    paste(format(round(sh$share, 2)), collapse = " to "), format(a), format(b)),
                            if (sh$cut) "the depth not described is undecided"),
                  page = 93)
}

# A share of the thickness of the window [a, b) against a fraction, the
# window cut by the base of the description (D1 of the review, Hugo,
# 2026-11-07): read on the whole window, the thickness not described is
# undecided; when that leaves the share undecided, a share that passes on the
# described part only passes provisionally (flag
# window_cut_at_description_base), as the base saturation of the
# Isohumosols does; otherwise undecided.
.cst_share_cut <- function(pedon, x, p, a, b, fraction, strict = FALSE) {
  sh <- .cst_window_share(pedon, p, a, b)
  passed <- .cst_share_at_least(sh, fraction, strict)
  deep <- suppressWarnings(max(x$bot, na.rm = TRUE))
  flags <- character(0)
  if (is.na(passed) && is.finite(deep) && deep > a && deep < b) {
    cut <- .cst_share_at_least(.cst_window_share(pedon, p, a, deep), fraction, strict)
    if (isTRUE(cut)) {
      passed <- TRUE
      flags <- c("window_cut_at_description_base", "provisional")
    }
  }
  list(passed = passed, share = sh$value, flags = flags, cut = is.finite(deep) && deep < b)
}

#' A udic moisture regime bordering on the perudic one (E1.2, p. 93; Q8):
#' the reader of 0.9.370 as a key criterion.
#' @noRd
udic_perudic_border_cst <- function(pedon) {
  r <- .cst_udic_perudic_border(pedon)
  .cst_key_result("udic_perudic_border_cst", r$value, missing = r$missing, flags = r$flags,
                  notes = c(r$note, if (!is.null(r$source) && !is.na(r$source)) sprintf("read from %s", r$source)),
                  page = 93)
}


# --- horizons by depth and thickness ----------------------------------------------------------------

#' A diagnostic horizon whose upper boundary lies between `top_min` and
#' `top_max` cm below the mineral soil surface (L2.2: a calcic horizon with
#' its top between 50 and 125 cm, p. 137).
#' @noRd
horizon_top_between_cst <- function(pedon, horizon, top_min, top_max, datum = c("mineral", "surface")) {
  datum <- match.arg(datum)
  nm <- "horizon_top_between_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE))
  r <- if (horizon == "calcic_horizon") .cst_calcic_only(x) else .cst_hz(x, horizon)
  s <- if (datum == "mineral") x$msurf else x$surf
  if (is.na(s)) return(.cst_key_result(nm, NA, missing = c("designation", "oc_pct")))
  d <- x$top - s
  inside <- !is.na(d) & d >= top_min & d <= top_max
  # the upper boundary of a horizon: its first layer (a layer under another
  # layer of it continues the horizon; under an undecided one, it may)
  f <- r$full
  above <- c(FALSE, (f %in% TRUE)[-x$n])
  above_na <- c(FALSE, is.na(f)[-x$n])
  sure <- f %in% TRUE & !above & !above_na & inside
  may <- !(f %in% FALSE) & !above & inside
  passed <- if (any(sure)) TRUE else if (any(may)) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[which(sure)], missing = r$missing,
                  flags = r$flags,
                  notes = sprintf("%s with its upper boundary %s-%s cm below the %s", gsub("_", " ", horizon),
                                  format(top_min), format(top_max),
                                  if (datum == "mineral") "mineral soil surface" else "soil surface"))
}

# --- the calcic horizon alone --------------------------------------------------------------------

# The calcic horizon without its hypercalcic layers, per layer (the key
# names "a calcic horizon" alone in F1.1, F3.2, G1.1, J2.4 and L2.2: a
# hypercalcic horizon does not count, Hugo, 2026-11-07).
.cst_calcic_only <- function(x) {
  r <- .cst_hz(x, "calcic_horizon")
  hy <- .cst_hz(x, "hypercalcic_horizon")$full
  r$full <- ifelse(hy %in% TRUE, FALSE, ifelse(is.na(hy) & r$full %in% TRUE, NA, r$full))
  r$evid <- ifelse(r$full %in% TRUE, TRUE, r$evid)
  r
}

#' A calcic horizon that is not a hypercalcic horizon, with its upper
#' boundary within `top_max` cm (F1.1, F3.2, G1.1, J2.4, pp. 96-118; the key
#' names the calcic horizon alone).
#' @noRd
calcic_only_cst <- function(pedon, top_max = NULL, datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  x <- .cst_ctx(pedon)
  r <- if (x$n) .cst_calcic_only(x) else NULL
  res <- .cst_hz_result(pedon, "calcic_horizon", top_max = top_max, datum = datum, x = x, r = r)
  res$notes <- paste(stats::na.omit(c(res$notes, "a hypercalcic horizon does not count")), collapse = "; ")
  res
}

#' An epipedon (a run of layers from the surface) `min_cm` thick or more
#' (J2.3: a mollic epipedon 50 cm or more thick, p. 118).
#' @noRd
epipedon_thick_cst <- function(pedon, horizon = "mollic_epipedon", min_cm = 50) {
  nm <- "epipedon_thick_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 118))
  r <- .cst_hz(x, horizon)
  rr <- .cst_runs(x, r$full)
  lo <- if (length(rr$low)) max(rr$low, na.rm = TRUE) else 0
  hi <- if (length(rr$high)) max(rr$high, na.rm = TRUE) else 0
  # the epipedon read on its layer test (D2 of the review, Hugo,
  # 2026-11-07): a layer below the sure run that may still belong to it (its
  # test undecided) counts at the high end, so the thickness is not capped
  # at the sure run
  if (horizon %in% c("mollic_epipedon", "umbric_epipedon") && !is.na(x$first_min)) {
    L <- .cst_humic_layers(x)
    bs_ok <- if (horizon == "mollic_epipedon") L$bs >= .cst_thr("horizons", "mollic", "bs_min_pct")
             else L$bs < .cst_thr("horizons", "mollic", "bs_min_pct")
    lt <- L$dark & L$contrast & L$oc_ok & bs_ok & (if (horizon == "mollic_epipedon") L$st_ok else TRUE)
    may <- ifelse(r$full %in% TRUE, TRUE, ifelse(lt %in% FALSE, FALSE, NA))
    mr <- .cst_runs(x, may, start = x$first_min)
    if (any(r$full %in% TRUE) || any(is.na(r$full))) hi <- max(hi, mr$high[x$first_min], na.rm = TRUE)
  }
  passed <- .cst_at_least(lo, hi, min_cm)
  .cst_key_result(nm, passed, layers = x$idx[which(r$full %in% TRUE)], missing = r$missing,
                  evidence = list(thickness = c(low = lo, high = hi)),
                  notes = sprintf("%s %s cm thick", gsub("_", " ", horizon), paste(unique(c(lo, hi)), collapse = " to ")),
                  page = 118)
}

#' An albic horizon above the claypan (L1.1, L4.1, pp. 134, 141): an albic
#' layer whose top lies above the top of the first claypan layer.
#' @noRd
albic_above_claypan_cst <- function(pedon) {
  nm <- "albic_above_claypan_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "134, 141"))
  cp <- .cst_hz(x, "claypan")$full
  al <- .cst_hz(x, "albic_horizon")$full
  k <- which(cp %in% TRUE)[1L]
  if (is.na(k)) {
    return(.cst_key_result(nm, if (any(is.na(cp)) && any(!(al %in% FALSE))) NA else FALSE,
                           missing = .cst_hz(x, "claypan")$missing, page = "134, 141"))
  }
  above <- which(!is.na(x$top) & x$top < x$top[k])
  passed <- if (any(al[above] %in% TRUE)) TRUE else if (any(is.na(al[above]))) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[above[al[above] %in% TRUE]],
                  missing = .cst_hz(x, "albic_horizon")$missing, page = "134, 141")
}

#' Folic histic evidence (M2.1, p. 151): histic evidence or a histic
#' epipedon (Q19) of the littery kind (litter, folic materials).
#' @noRd
histic_littery_cst <- function(pedon) {
  nm <- "histic_littery_cst"
  r <- histic_epipedon_cst(pedon, grade = "at_least_evidence")
  if (!isTRUE(r$passed)) return(.cst_key_result(nm, r$passed, missing = r$missing, page = 151))
  k <- r$evidence$kind
  passed <- if (all(is.na(k))) NA else any(grepl("^littery", k))
  .cst_key_result(nm, passed, layers = r$layers, missing = if (is.na(passed)) "water_saturation_days",
                  notes = sprintf("histic %s of the %s kind", r$evidence$grade %||% "NA", paste(k, collapse = ", ")),
                  page = 151)
}

#' Shajiang, irregular calcareous nodules or concretions, 10 % or more by
#' volume in some horizon 10 cm or more thick within 125 cm of the mineral
#' soil surface (M2.2, p. 151): shajiang_pct; not recorded, hard carbonate
#' concretions stand in, flagged (carbonate_hard_pct, or
#' secondary_carbonates_pct with a hard form); none recorded, absent. v0.9.375:
#' `from_cm` starts the window deeper and `total` asks for horizons totalling
#' min_cm (F1.1.2, p. 97).
#' @noRd
shajiang_cst <- function(pedon, min_pct = 10, min_cm = 10, within = 125, from_cm = 0, total = FALSE) {
  nm <- "shajiang_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$msurf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 151))
  sj <- .cst_v(x, "shajiang_pct")
  hard <- .cst_v(x, "carbonate_hard_pct")
  form <- .cst_t(x, "secondary_carbonates_form")
  sec <- ifelse(!is.na(form) & grepl("hard|concretion|nodul", form), .cst_v(x, "secondary_carbonates_pct"), NA_real_)
  fb <- ifelse(!is.na(hard), hard, sec)
  use_fb <- is.na(sj) & !is.na(fb)
  v <- ifelse(is.na(sj), fb, sj)
  p <- (v >= min_pct) %in% TRUE
  p[x$kind %in% c("O", "R")] <- FALSE
  a <- x$msurf + from_cm; b <- x$msurf + within
  ov <- .cst_overlap(x, a, b); ov[is.na(ov)] <- 0
  best <- 0; rows <- integer(0); i <- 1L
  if (isTRUE(total)) {
    # v0.9.375: horizons totalling min_cm (F1.1.2, between 50 and 100 cm)
    rows <- which(p & ov > 0); best <- sum(ov[rows])
  } else while (i <= x$n) {
    if (p[i]) {
      j <- i; while (j < x$n && p[j + 1L]) j <- j + 1L
      s <- sum(ov[i:j]); if (s > best) { best <- s; rows <- i:j }
      i <- j + 1L
    } else i <- i + 1L
  }
  passed <- best >= min_cm
  .cst_key_result(nm, passed, layers = x$idx[rows],
                  flags = if (passed && any(use_fb[rows])) "shajiang_from_hard_carbonates",
                  notes = "shajiang not recorded is absent (hard carbonate concretions stand in, flagged)",
                  page = 151)
}


# --- the Halosols (H1) ------------------------------------------------------------------------------

#' The takyric surface of the Halosols (H1.1, p. 108): polygonal cracks 1 cm
#' or more wide on the soil surface, through a thickness of 1-4 cm. The
#' depth of the cracks: cracks_depth_cm (the first horizon or the site);
#' not recorded, the thickness of the surface crust stands in, flagged
#' (site$surface_crust_thickness_cm, else the top horizon when it has
#' vesicular pores) (Hugo, 2026-11-07); the cracks: polygonal cracks
#' recorded (polygonal_cracks_spacing_cm) with cracks_width_cm of 1 cm or
#' more. Field observations: not recorded, absent.
#' @noRd
takyric_surface_cst <- function(pedon) {
  nm <- "takyric_surface_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = 108))
  t <- .cst_thr("key", "groups", "takyric")
  site <- pedon$site %||% list()
  pos1 <- function(name) { v <- .cst_v(x, name)[1L]; if (is.na(v)) .cst_site_num(site, name) else v }
  flags <- character(0)
  d <- pos1("cracks_depth_cm")
  if (is.na(d)) {
    d <- .cst_site_num(site, "surface_crust_thickness_cm")
    ves <- .cst_t(x, "vesicular_pores")
    if (is.na(d) && !is.na(ves[1L]) && ves[1L] %in% c("few", "common", "many")) d <- x$thk[1L]
    if (!is.na(d)) flags <- c("stand_in:crust_thickness", "crack_depth_from_crust_thickness")
  }
  deep <- !is.na(d) && d >= t$crust_cm[1L] && d <= t$crust_cm[2L]
  sp <- pos1("polygonal_cracks_spacing_cm"); cw <- pos1("cracks_width_cm")
  cracks <- !is.na(sp) && sp > 0 && !is.na(cw) && cw >= t$crack_width_min_cm
  passed <- deep && cracks
  .cst_key_result(nm, passed, flags = if (passed) flags,
                  evidence = list(crack_depth_cm = d, crack_width_cm = cw,
                                  stand_in = if (passed && length(flags)) "crust_thickness" else character(0)),
                  notes = "the cracks and their depth are field observations: not recorded, absent (the crust thickness stands in for the depth, flagged)",
                  page = 108)
}


# --- lithologic characters ---------------------------------------------------------------------------

#' A lithologic character in some horizon whose upper boundary lies within
#' `top_max` cm of the mineral soil surface (J1.1: coral sands within 100
#' cm, p. 117). A character read for the whole profile (no layer named)
#' counts as present at the surface.
#' @noRd
lc_within_cst <- function(pedon, kind, top_max) {
  nm <- "lc_within_cst"
  r <- lithologic_characters_cst(pedon, kind)
  if (!isTRUE(r$passed)) return(.cst_key_result(nm, r$passed, missing = r$missing, page = 117))
  x <- .cst_ctx(pedon)
  pos <- match(r$layers, x$idx)
  if (!length(pos)) return(.cst_key_result(nm, TRUE, notes = "read for the whole profile", page = 117))
  s <- if (is.na(x$msurf)) x$surf else x$msurf
  ok <- any(x$top[pos] - s <= top_max, na.rm = TRUE)
  .cst_key_result(nm, ok, layers = r$layers[which(x$top[pos] - s <= top_max)],
                  notes = sprintf("%s within %s cm of the mineral soil surface", kind, format(top_max)), page = 117)
}

#' Carbonate rocks or coral sands in more than half of the horizons within
#' 100 cm of the soil surface or above a shallower lithic contact (M5.2,
#' p. 161), by thickness (Q22): the lithologic character of carbonate rocks
#' is read for the profile (when it holds, every horizon of the window
#' carries it); that of coral sands layer by layer.
#' @noRd
lc_carbonate_share_cst <- function(pedon, more_than = 0.5, within = 100) {
  nm <- "lc_carbonate_share_cst"
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_key_result(nm, if (x$n) NA else FALSE, page = 161))
  carb <- lithologic_characters_cst(pedon, "carbonate_rocks")
  if (isTRUE(carb$passed))
    return(.cst_key_result(nm, TRUE, layers = carb$layers, notes = "carbonate rocks: the whole window", page = 161))
  co <- x$memo[["lc:coral_sands"]] %||% .cst_lc_coral_sands(x)
  p <- seq_len(x$n) %in% co$rows
  if (is.na(co$value)) p[!p] <- NA
  a <- x$surf; b <- a + within
  lc <- lithic_contact_cst(pedon)
  if (isTRUE(lc$passed)) {
    cb <- lc$evidence$contact_cm
    if (!is.null(cb) && !is.na(cb) && x$surf + cb < b) b <- x$surf + cb
  }
  sh <- if (b <= a) list(passed = FALSE, flags = character(0)) else .cst_share_cut(pedon, x, p, a, b, more_than, strict = TRUE)
  passed <- .cst_or(carb$passed, sh$passed)
  .cst_key_result(nm, passed, layers = x$idx[which(p %in% TRUE)], missing = c(carb$missing, co$missing),
                  flags = if (isTRUE(passed) && !isTRUE(carb$passed)) sh$flags,
                  evidence = list(window = c(top = a, bottom = b)),
                  notes = "carbonate rocks or coral sands in more than half of the window, by thickness", page = 161)
}


# --- the B horizon -------------------------------------------------------------------------------------

# A per-layer test of the B horizon keys: TRUE / FALSE / NA by layer, with
# the attributes that decide it.
.cst_b_test <- function(x, test) {
  t <- .cst_thr("key", "groups", "b_horizon")
  switch(test,
    allitic = list(p = .cst_ch(x, "allitic_property")$full,
                   missing = .cst_ch(x, "allitic_property")$missing),
    hiweatheri = {
      cec <- .cst_clay_cec(x$h)$value; ecec <- .cst_clay_ecec(x$h)$value
      al <- .cst_ch(x, "allitic_property")$full
      list(p = cec < t$hiweatheri_cec_clay_below & ecec < t$hiweatheri_ecec_clay_below & al,
           missing = c("cec_cmol", "clay_pct", "ca_cmol", "al_kcl_cmol", .cst_ch(x, "allitic_property")$missing))
    },
    alic = list(p = .cst_ch(x, "alic_property")$evid,
                missing = .cst_ch(x, "alic_property")$missing),
    acid_and = list(p = .cst_v(x, "ph_h2o") < t$acid_ph_below & .cst_v(x, "bs_pct") < t$acid_bs_below,
                    missing = c("ph_h2o", "bs_pct")),
    acid_or = {
      ph <- .cst_v(x, "ph_h2o") < t$acid_ph_below; bs <- .cst_v(x, "bs_pct") < t$acid_bs_below
      list(p = ifelse(ph %in% TRUE | bs %in% TRUE, TRUE, ifelse(ph %in% FALSE & bs %in% FALSE, FALSE, NA)),
           missing = c("ph_h2o", "bs_pct"))
    },
    rlang::abort(sprintf("Unknown B horizon test '%s'.", test)))
}

#' A property in some subhorizon(s) of the B horizon 10 cm or more thick
#' within 125 cm (K2.2, K3.2, K3.3, pp. 125, 128): a run of contiguous B
#' layers that meet `test` ("allitic"; "hiweatheri": a CEC7 under 16 and an
#' ECEC under 12 cmol(+)/kg clay and the allitic property in the same layers;
#' v0.9.375, for the subgroups: "alic", the alic property or alic evidence,
#' D3.1.2; "acid_or", base saturation under 50 % or pH under 5.5).
#' @noRd
b_subhorizon_cst <- function(pedon, test = c("allitic", "hiweatheri", "alic", "acid_or"), min_cm = 10) {
  test <- match.arg(test)
  nm <- "b_subhorizon_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "125, 128"))
  B <- .cst_b_layers(x)
  bt <- .cst_b_test(x, test)
  p <- ifelse(B$b %in% FALSE, FALSE, ifelse(is.na(B$b), ifelse(bt$p %in% FALSE, FALSE, NA), bt$p))
  g <- .cst_run_grade(x, p, min_cm)
  passed <- if (any(g %in% TRUE)) TRUE else if (any(is.na(g))) NA else FALSE
  .cst_key_result(nm, passed, layers = x$idx[which(g %in% TRUE)],
                  missing = c(bt$missing, if (any(is.na(B$b))) "designation"), flags = B$flag,
                  notes = sprintf("%s in a run of B layers %s cm thick within 125 cm", test, format(min_cm)),
                  page = "125, 128")
}

#' A property throughout the B horizon within 125 cm (L3.2, L4.4, L4.5,
#' M4.4, M4.5, M5.4, M5.6, pp. 139-162; Q23): every B layer meets `test`
#' ("alic": the alic property or alic evidence; "acid_and": pH under 5.5 and
#' base saturation under 50 %; "acid_or": base saturation under 50 % or pH
#' under 5.5). A layer that may be of the B counts only when it meets the
#' test, as for the ferric property.
#' @noRd
b_throughout_cst <- function(pedon, test = c("alic", "acid_and", "acid_or")) {
  test <- match.arg(test)
  nm <- "b_throughout_cst"
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_key_result(nm, FALSE, page = "139-162"))
  B <- .cst_b_layers(x)
  bt <- .cst_b_test(x, test)
  inB <- B$b %in% TRUE; maybe <- is.na(B$b)
  if (!any(inB) && !any(maybe))
    return(.cst_key_result(nm, FALSE, flags = B$flag, notes = "no B horizon", page = "139-162"))
  passed <- if (any(inB)) .cst_and(c(bt$p[inB], ifelse(bt$p[maybe] %in% TRUE, TRUE, NA))) else NA
  rows <- which(inB)
  .cst_key_result(nm, passed, layers = x$idx[rows], flags = B$flag,
                  missing = c(.cst_missing_in(x, rows[is.na(bt$p[rows])], bt$missing), if (any(maybe)) "designation"),
                  notes = sprintf("%s throughout the B within 125 cm", test), page = "139-162")
}


# --- the examples of the groups -----------------------------------------------------------------------

# The profile of each group example: a suborder example (`base`) with some
# columns replaced (`set`, whole columns, recycled) and some site fields
# (`site`), or a profile of its own (`h`, `site`). The groups that a suborder
# example reaches take it as it is (`base` alone).
.cst_group_example_spec <- function(code) {
  lime <- list(parent_material = "limestone")
  switch(code,
  # --- A ---
  "A1.1" = list(h = list(designation = c("Oi", "R"), top_cm = c(0, 30), bottom_cm = c(30, 60),
                         oc_pct = c(40, NA), plant_residues_pct = c(85, NA), water_saturation_days = c(5, NA),
                         bulk_density_g_cm3 = c(0.12, NA), fiber_content_rubbed_pct = c(80, NA)),
                site = list(soil_temperature_regime = "permagelic", soil_moisture_regime = "udic")),
  "A1.2" = list(base = "A1", set = list(fiber_content_rubbed_pct = c(85, 80, NA))),
  "A1.3" = list(base = "A1"),
  "[A1.4]" = list(base = "A1", set = list(fiber_content_rubbed_pct = c(10, 10, NA), sphagnum_fibres_pct = c(0, 0, NA),
                                          bulk_density_g_cm3 = c(0.2, 0.25, 1.3))),
  "A2.1" = list(h = list(designation = c("Oi", "R"), top_cm = c(0, 30), bottom_cm = c(30, 60),
                         oc_pct = c(40, NA), plant_residues_pct = c(85, NA), water_saturation_days = c(5, NA),
                         bulk_density_g_cm3 = c(0.12, NA), fiber_content_rubbed_pct = c(80, NA)),
                site = list(soil_temperature_regime = "mesic", soil_moisture_regime = "udic")),
  "A2.2" = list(base = "A2", set = list(fiber_content_rubbed_pct = c(80, 10, NA))),
  "A2.3" = list(base = "A2"),
  "A2.4" = list(base = "A2", set = list(fiber_content_rubbed_pct = c(10, 10, NA))),
  "[A2.5]" = list(base = "A2", set = list(plant_residues_pct = c(85, NA, NA))),
  # --- B ---
  "B1.1" = list(base = "B1", set = list(childs_test = c("positive", "positive", "negative", NA))),
  "B1.2" = list(base = "B1", set = list(munsell_chroma_moist = c(1, 2, 1, 2), munsell_value_moist = c(4, 4, 5, 6),
                                        munsell_value_dry = c(5, 5, 6, 7), fe_dcb_pct = c(1.2, 1.2, 1.0, 1.2))),
  "B1.3" = list(base = "B1"),
  "B1.4" = list(base = "B1", set = list(fe_dcb_pct = c(1.2, 1.2, 1.5, 1.2))),
  "B2.1" = list(h = list(designation = c("Ap1", "Ap2", "Bw", "C"), top_cm = c(0, 15, 30, 45), bottom_cm = c(15, 30, 45, 120),
                         clay_pct = 20, silt_pct = 55, sand_pct = 25, oc_pct = c(1.4, 1.1, 0.5, 0.2),
                         p_olsen_mg_kg = c(70, 60, 40, 10), worm_holes_pct = c(60, 55, NA, NA), artefacts_pct = c(2, 1, 0, 0),
                         munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(2, 2, 3, 4),
                         munsell_value_dry = c(5, 5, 6, 7), structure_type = c("granular", "granular", "blocky", "massive"),
                         ph_h2o = 7.0, bs_pct = 85, cec_cmol = 14),
                site = list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", land_use = "vegetable garden")),
  "B2.2" = list(base = "B2", set = list(silting_sieve_test = c("positive", NA, NA, NA), oc_pct = c(0.8, 0.6, 0.5, 0.1),
                                        foreign_fragments_pct = NA)),
  "B2.3" = list(base = "B2", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA, NA), munsell_chroma_moist = c(2, 2, 2, 4),
                                        foreign_fragments_pct = NA)),
  "B2.4" = list(base = "B2"),
  # --- C ---
  "C1.1" = list(base = "C1"),
  "C2.1" = list(base = "C2"),
  # --- D ---
  "D1.1" = list(base = "D1", site = list(soil_temperature_regime = "gelic")),
  "D1.2" = list(base = "D1"),
  "D2.1" = list(base = "D2"),
  "D2.2" = list(base = "D2", site = list(soil_moisture_regime = "udic")),
  "D3.1" = list(base = "D3", set = list(humus_coatings = c(NA, "common", NA))),
  "D3.2" = list(base = "D3"),
  # --- E ---
  "E1.1" = list(base = "E1"),
  "E1.2" = list(base = "E1", set = list(munsell_hue_moist = "7.5YR", munsell_value_moist = c(4, 5, 5),
                                        munsell_chroma_moist = c(4, 6, 6), munsell_value_dry = c(5, 6, 6)),
                site = list(udic_perudic_border = TRUE)),
  "E1.3" = list(base = "E1", set = list(munsell_hue_moist = "5YR", munsell_value_moist = c(4, 4, 4),
                                        munsell_chroma_moist = c(4, 6, 6), munsell_value_dry = c(5, 5, 5)),
                site = list(udic_perudic_border = FALSE)),
  # --- F ---
  "F1.1" = list(base = "F1", set = list(caco3_pct = c(1, 2, 18, 8), secondary_carbonates_pct = c(0, 0, 8, 2),
                                        ph_h2o = c(7.6, 7.9, 8.2, 8.3), designation = c("Ap", "Bss1", "Bkss", "BC"))),
  "F1.2" = list(base = "F1"),
  "F2.1" = list(base = "F2"),
  "F2.2" = list(base = "F2", set = list(caco3_pct = 0, secondary_carbonates_pct = 0, ph_h2o = c(7.2, 7.4, 7.5, 7.6),
                                        designation = c("Ap", "Bss1", "Bss2", "BC"))),
  "F3.1" = list(base = "F3", set = list(humus_coatings = c(NA, "common", "common", NA))),
  "F3.2" = list(base = "F3", set = list(caco3_pct = c(1, 2, 18, 8), secondary_carbonates_pct = c(0, 0, 8, 2),
                                        ph_h2o = c(7.6, 7.9, 8.2, 8.3), designation = c("Ap", "Bss1", "Bkss", "BC"))),
  "F3.3" = list(base = "F3"),
  # --- G ---
  "G1.1" = list(base = "G1"),
  "G1.2" = list(base = "G1", set = list(designation = c("Avk", "Bw", "By", "C"), caco3_pct = c(5, 6, 6, 5),
                                        secondary_carbonates_pct = 0, caso4_pct = c(0.5, 1, 12, 3),
                                        gypsum_visible_pct = c(0, 0, 3, 0))),
  "G1.3" = list(base = "G1", set = list(designation = c("Av", "Bw", "Bt", "C"), clay_pct = c(12, 14, 26, 18),
                                        sand_pct = c(54, 52, 44, 50), silt_pct = c(34, 34, 30, 32),
                                        caco3_pct = c(2, 2, 3, 3), secondary_carbonates_pct = 0,
                                        clay_films_strength = c(NA, NA, "distinct", NA),
                                        clay_films_amount = c(NA, NA, "common", NA), na_cmol = 0.2, mg_cmol = 1)),
  "G1.4" = list(base = "G1", set = list(designation = c("Av", "Bw1", "Bw2", "C"), caco3_pct = c(2, 2, 3, 3),
                                        secondary_carbonates_pct = 0, structure_type = c("platy", "blocky", "blocky", "massive"),
                                        munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"),
                                        munsell_chroma_moist = c(3, 5, 5, 3))),
  "G2.1" = list(base = "G1", site = list(soil_temperature_regime = "mesic", mean_summer_soil_temp_C = NA)),
  "G2.2" = list(base = "G2", set = list(designation = c("Av", "Bw", "Bz", "C"), caso4_pct = c(0.5, 1, 1, 1),
                                        gypsum_visible_pct = 0, soluble_salts_pct = c(0.1, 0.2, 2.5, 0.8))),
  "G2.3" = list(base = "G2"),
  "G2.4" = list(base = "G2", set = list(designation = c("Av", "Bw", "Bt", "C"), clay_pct = c(10, 12, 24, 16),
                                        sand_pct = c(60, 54, 46, 56), silt_pct = c(30, 34, 30, 28), caso4_pct = 0.5,
                                        gypsum_visible_pct = 0, clay_films_strength = c(NA, NA, "distinct", NA),
                                        clay_films_amount = c(NA, NA, "common", NA))),
  "G2.5" = list(base = "G2", set = list(designation = c("Av", "Bw1", "Bw2", "C"), caso4_pct = 0.5, gypsum_visible_pct = 0,
                                        clay_pct = c(12, 13, 13, 12), sand_pct = c(58, 53, 54, 60), silt_pct = c(30, 34, 33, 28),
                                        munsell_hue_moist = c("7.5YR", "7.5YR", "7.5YR", "10YR"),
                                        munsell_chroma_moist = c(4, 5, 5, 3), structure_type = c("platy", "blocky", "blocky", "massive"))),
  # --- H ---
  "H1.1" = list(base = "H1", set = list(polygonal_cracks_spacing_cm = c(20, NA, NA, NA), cracks_width_cm = c(1.5, NA, NA, NA),
                                        cracks_depth_cm = c(2, NA, NA, NA)),
                site = list(soil_moisture_regime = "aridic", surface_crust_thickness_cm = 2, irrigated = FALSE)),
  "H1.2" = list(base = "H1", set = list(mottle_colors = c(NA, "7.5YR 5/6", "7.5YR 5/6", NA))),
  "H1.3" = list(base = "H1"),
  "H2.1" = list(base = "H2"),
  "H2.2" = list(base = "H2", site = list(soil_moisture_regime = "ustic")),
  # --- I ---
  "I1.1" = list(base = "I1", set = list(bottom_cm = c(25, 50, 90, 120), top_cm = c(0, 25, 50, 90))),
  "I1.2" = list(base = "I1"),
  "I2.1" = list(h = list(designation = c("Oa", "Eg", "Btg", "C"), top_cm = c(0, 25, 45, 85), bottom_cm = c(25, 45, 85, 125),
                         oc_pct = c(25, 0.5, 0.4, 0.2), clay_pct = c(NA, 18, 38, 30), silt_pct = c(NA, 57, 42, 45),
                         sand_pct = c(NA, 25, 20, 25), water_saturation_days = c(100, 100, 60, 20),
                         fiber_content_rubbed_pct = c(10, NA, NA, NA), bulk_density_g_cm3 = c(0.3, 1.4, 1.5, 1.5),
                         munsell_hue_moist = "10YR", munsell_value_moist = c(2, 6, 5, 5), munsell_chroma_moist = c(1, 1, 2, 4),
                         munsell_value_dry = c(3, 7, 6, 6), childs_test = c(NA, "positive", "positive", NA),
                         mottle_colors = c(NA, NA, "7.5YR 5/6", NA), ph_h2o = 5.8, bs_pct = 55, cec_cmol = c(60, 8, 20, 15),
                         structure_type = c(NA, "platy", "prismatic", "massive")),
                site = list(soil_moisture_regime = "stagnic", soil_temperature_regime = "mesic", drainage_class = "poorly drained")),
  "I2.2" = list(base = "I2"),
  "I3.1" = list(base = "I3", set = list(designation = c("Oa", "Bg", "Cg"), oc_pct = c(25, 0.8, 0.4), clay_pct = c(NA, 30, 25),
                                        silt_pct = c(NA, 48, 50), sand_pct = c(NA, 22, 25), water_saturation_days = c(100, 150, 200),
                                        fiber_content_rubbed_pct = c(10, NA, NA), bulk_density_g_cm3 = c(0.3, 1.4, 1.5),
                                        structure_type = c(NA, "massive", "massive"))),
  "I3.2" = list(base = "I3"),
  "I3.3" = list(base = "I3", set = list(oc_pct = c(0.6, 0.5, 0.3), munsell_value_moist = c(4, 5, 5),
                                        munsell_value_dry = c(6, 6, 6))),
  # --- J ---
  "J1.1" = list(base = "J1"),
  "J1.2" = list(base = "J1", set = list(p_total_pct = c(0.1, 0.08, 0.06, 0.05))),
  "J2.1" = list(base = "J2", site = list(soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 10)),
  "J2.2" = list(base = "J2", set = list(foreign_fragments_pct = c(5, NA, NA, NA), artefacts_pct = c(1, 0, 0, 0),
                                        munsell_value_moist = c(2, 4, 5, 6), munsell_chroma_moist = c(2, 4, 3, 3))),
  "J2.3" = list(base = "J2"),
  "J2.4" = list(base = "J2", set = list(top_cm = c(0, 35, 60, 110), bottom_cm = c(35, 60, 110, 180),
                                        munsell_value_moist = c(2, 4, 5, 6), munsell_value_dry = c(4, 6, 7, 7))),
  "J2.5" = list(base = "J2", set = list(top_cm = c(0, 35, 60, 110), bottom_cm = c(35, 60, 110, 180),
                                        designation = c("Ah", "AB", "Bw", "C"), caco3_pct = c(0.5, 1, 2, 3),
                                        munsell_value_moist = c(2, 4, 5, 6), munsell_value_dry = c(4, 6, 7, 7),
                                        secondary_carbonates_pct = 0)),
  "J3.1" = list(base = "J3", set = list(designation = c("Ah", "Bw", "Bg", "C"), clay_pct = c(22, 24, 25, 22),
                                        clay_films_strength = NA, clay_films_amount = NA,
                                        mottle_colors = c(NA, "7.5YR 5/6", NA, NA), munsell_hue_moist = c("10YR", "10YR", "2.5Y", "10YR"),
                                        munsell_value_moist = c(2, 4, 5, 5), munsell_chroma_moist = c(2, 3, 2, 4),
                                        childs_test = c(NA, "negative", "positive", NA), top_cm = c(0, 35, 60, 130),
                                        bottom_cm = c(35, 60, 130, 180)),
                site = list(soil_moisture_regime = "stagnic", climatic_moisture_regime = "udic", drainage_class = "imperfectly drained")),
  "J3.2" = list(base = "J3"),
  "J3.3" = list(base = "J3", set = list(designation = c("Ah", "Bw", "BC", "C"), clay_pct = c(22, 24, 24, 22),
                                        clay_films_strength = NA, clay_films_amount = NA)),
  # --- K ---
  "K1.1" = list(base = "K1"),
  "K1.2" = list(base = "K1", set = list(clay_pct = c(48, 50, 50), silt_pct = c(27, 25, 25))),
  "K2.1" = list(base = "K2", set = list(designation = c("A", "Bw", "R"), bs_pct = c(60, 55, NA), ph_h2o = c(6.0, 5.8, NA),
                                        top_cm = c(0, 15, 110), bottom_cm = c(15, 110, 130)), site = lime),
  "K2.2" = list(base = "K2"),
  "K2.3" = list(base = "K2", set = list(sio2_al2o3_clay = c(NA, 2.4, 2.4), sio2_al2o3_naoh = c(NA, 1.5, 1.5),
                                        sio2_sulfuric_pct = c(NA, 20, 20), al2o3_sulfuric_pct = c(NA, 15, 15))),
  "K3.1" = list(base = "K3", set = list(designation = c("A", "Bt", "R"), top_cm = c(0, 20, 110), bottom_cm = c(20, 110, 130),
                                        bs_pct = c(60, 55, NA), ph_h2o = c(6.0, 5.8, NA)), site = lime),
  "K3.2" = list(h = list(designation = c("A", "Bt1", "Bt2", "Bt3"), top_cm = c(0, 20, 60, 80), bottom_cm = c(20, 60, 80, 130),
                         clay_pct = c(20, 38, 40, 40), sand_pct = c(45, 30, 30, 30), silt_pct = c(35, 32, 30, 30),
                         oc_pct = c(1.5, 0.5, 0.3, 0.3), cec_cmol = c(8, 8.5, 5.6, 9), ca_cmol = c(1.5, 1.5, 1.0, 1.5),
                         mg_cmol = c(0.8, 0.8, 0.5, 0.8), k_cmol = 0.2, na_cmol = 0.1, al_kcl_cmol = c(1, 1, 2.5, 1),
                         bs_pct = 30, ph_h2o = 5.0, fe_dcb_pct = c(1.5, 2.2, 2.4, 2.3), fe_total_pct = c(3, 4, 4, 4),
                         weatherable_minerals_pct = 15, sio2_al2o3_clay = c(NA, 2.4, 1.8, 2.4), sio2_al2o3_naoh = c(NA, 1.5, 0.8, 1.5),
                         sio2_sulfuric_pct = c(NA, 20, 15, 20), al2o3_sulfuric_pct = c(NA, 15, 17, 15),
                         clay_films_strength = c(NA, "distinct", "distinct", "distinct"),
                         clay_films_amount = c(NA, "common", "common", "common"),
                         munsell_hue_moist = "5YR", munsell_value_moist = 4, munsell_chroma_moist = c(4, 6, 6, 6),
                         munsell_value_dry = 5, structure_type = c("granular", "subangular blocky", "subangular blocky", "subangular blocky")),
                site = list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic")),
  "K3.3" = list(base = "K3", set = list(sio2_al2o3_clay = c(NA, 1.8, 1.8), sio2_al2o3_naoh = c(NA, 0.8, 0.8))),
  "K3.4" = list(base = "K3"),
  "K3.5" = list(base = "K3", set = list(clay_pct = c(36, 38, 40), sand_pct = c(30, 30, 30), silt_pct = c(34, 32, 30),
                                        clay_films_strength = NA, clay_films_amount = NA, designation = c("A", "Bw1", "Bw2"))),
  # --- L ---
  "L1.1" = list(base = "L1", set = list(clay_pct = c(18, 12, 34, 24), sand_pct = c(27, 30, 21, 28), silt_pct = c(55, 58, 45, 48),
                                        structure_type = c("granular", "platy", "prismatic", "massive"),
                                        micromorph_features = c(NA, NA, "clay_stripes", NA))),
  "L1.2" = list(base = "L1", set = list(designation = c("Ah", "Bt1", "Bt2", "C"), top_cm = c(0, 25, 45, 80),
                                        bottom_cm = c(25, 45, 80, 120), munsell_value_moist = c(2, 4, 4, 5),
                                        munsell_chroma_moist = c(2, 4, 4, 4),
                                        munsell_value_dry = c(4, 6, 6, 6), clay_pct = c(18, 28, 30, 24),
                                        silt_pct = c(55, 47, 45, 48), sand_pct = c(27, 25, 25, 28), bs_pct = c(70, 70, 70, 75),
                                        clay_films_strength = c(NA, "distinct", "distinct", NA),
                                        clay_films_amount = c(NA, "common", "common", NA),
                                        structure_type = c("granular", "blocky", "blocky", "massive"))),
  "L1.3" = list(base = "L1"),
  "L2.1" = list(base = "L2", set = list(designation = c("Ap", "Bt", "Bk", "R"), top_cm = c(0, 20, 70, 110),
                                        bottom_cm = c(20, 70, 110, 140)), site = lime),
  "L2.2" = list(base = "L2"),
  "L2.3" = list(base = "L2", set = list(designation = c("Ap", "Bt1", "Bt2", "C"), caco3_pct = 0.5, secondary_carbonates_pct = 0,
                                        ph_h2o = 7.2, munsell_hue_moist = c("5YR", "5YR", "5YR", "7.5YR"))),
  "L2.4" = list(base = "L2", set = list(designation = c("Ap", "Bt1", "Bt2", "C"), caco3_pct = 0.5, secondary_carbonates_pct = 0,
                                        ph_h2o = 7.2, fe_dcb_pct = c(0.7, 0.7, 0.7, 0.7), fe_total_pct = 3)),
  "L3.1" = list(base = "L3", set = list(designation = c("A", "Bt1", "Bt2", "R"), bs_pct = c(60, 55, 55, NA),
                                        ph_h2o = c(6.0, 5.8, 5.8, NA), al_kcl_cmol = c(0.2, 0.2, 0.2, NA)), site = lime),
  "L3.2" = list(base = "L3"),
  "L3.3" = list(base = "L3", set = list(ph_kcl = 4.5, al_kcl_cmol = c(1, 1, 1, 1))),
  "L4.1" = list(base = "L4", set = list(designation = c("A", "E", "Bt", "C"), clay_pct = c(20, 14, 36, 30),
                                        silt_pct = c(45, 50, 38, 42), sand_pct = c(35, 36, 26, 28),
                                        munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(4, 6, 5, 5),
                                        munsell_chroma_moist = c(3, 2, 6, 6), munsell_value_dry = c(5, 7, 6, 6),
                                        structure_type = c("granular", "platy", "prismatic", "massive"),
                                        micromorph_features = c(NA, NA, "clay_stripes", NA))),
  "L4.2" = list(base = "L4", set = list(designation = c("A", "Bt1", "Bt2", "R")), site = lime),
  "L4.3" = list(base = "L4", set = list(clay_pct = c(16, 36, 36, 30), silt_pct = c(49, 38, 38, 42), sand_pct = c(35, 26, 26, 28),
                                        structure_type = c("granular", "prismatic", "blocky", "massive"),
                                        micromorph_features = c(NA, "clay_stripes", NA, NA),
                                        munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"))),
  "L4.4" = list(base = "L3", site = list(soil_moisture_regime = "udic")),
  "L4.5" = list(base = "L4", set = list(ph_h2o = c(5.0, 5.0, 5.1, 5.2), bs_pct = c(30, 30, 35, 40),
                                        munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"))),
  "L4.6" = list(base = "L4"),
  "L4.7" = list(base = "L4", set = list(munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"),
                                        fe_dcb_pct = c(0.8, 0.9, 0.9, 0.8))),
  # --- M ---
  "M1.1" = list(base = "M1", set = list(designation = c("Ah", "Bw", "Cf"), permafrost_temp_C = c(NA, NA, -1)),
                site = list(frost_regime = NA)),
  "M1.2" = list(base = "M1", set = list(n_total_pct = c(0.5, 0.2, 0.1), mottle_colors = c(NA, "7.5YR 5/6", NA)),
                site = list(soil_moisture_regime = "aquic", climatic_moisture_regime = "udic",
                            drainage_class = "somewhat poorly drained")),
  "M1.3" = list(base = "M1", set = list(n_total_pct = c(0.9, 0.2, 0.1))),
  "M1.4" = list(base = "M1", set = list(designation = c("Ah", "Bw", "C"), top_cm = c(0, 25, 50), bottom_cm = c(25, 50, 110),
                                        root_mat = NA, root_mat_pct = NA, oc_pct = c(3, 1.0, 0.4), n_total_pct = c(0.25, 0.1, 0.05),
                                        bulk_density_g_cm3 = c(1.1, 1.3, 1.4))),
  "M1.5" = list(base = "M1", set = list(designation = c("Ah", "Bw", "C"), top_cm = c(0, 25, 50), bottom_cm = c(25, 50, 110),
                                        root_mat = NA, root_mat_pct = NA, oc_pct = c(3, 1.0, 0.4), n_total_pct = c(0.25, 0.1, 0.05),
                                        bulk_density_g_cm3 = c(1.1, 1.3, 1.4), bs_pct = 30)),
  "M1.6" = list(base = "M1"),
  "M2.1" = list(h = list(designation = c("Oi", "Ap", "Bw", "Bg", "C"), top_cm = c(0, 8, 28, 68, 108),
                         bottom_cm = c(8, 28, 68, 108, 138), oc_pct = c(30, 1.5, 0.5, 0.3, 0.2),
                         plant_residues_pct = c(85, NA, NA, NA, NA), water_saturation_days = c(10, 10, 20, 60, 90),
                         clay_pct = c(NA, 20, 22, 20, 18), silt_pct = c(NA, 50, 50, 52, 52), sand_pct = c(NA, 30, 28, 28, 30),
                         mottle_colors = c(NA, NA, "7.5YR 5/6", "7.5YR 5/8", NA), childs_test = c(NA, NA, "negative", "negative", NA),
                         munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(2, 4, 4, 5, 5),
                         munsell_chroma_moist = c(1, 3, 4, 3, 3), munsell_value_dry = c(3, 6, 6, 6, 7),
                         structure_type = c(NA, "granular", "blocky", "blocky", "massive"),
                         structure_grade = c(NA, "moderate", "moderate", "weak", "structureless"), ph_h2o = 6.8, bs_pct = 80,
                         cec_cmol = 14),
                site = list(soil_moisture_regime = "aquic", soil_temperature_regime = "mesic", drainage_class = "somewhat poorly drained")),
  "M2.2" = list(base = "M2", set = list(shajiang_pct = c(NA, 15, 5, NA))),
  "M2.3" = list(base = "M2", set = list(munsell_value_moist = c(2, 4, 5, 5), munsell_chroma_moist = c(2, 4, 3, 3),
                                        munsell_value_dry = c(4, 6, 6, 7), oc_pct = c(2.5, 0.5, 0.3, 0.2),
                                        top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 130))),
  "M2.4" = list(base = "M2"),
  "M3.1" = list(base = "M3", set = list(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120), oc_pct = c(1.0, 0.4, 0.2),
                                        silting_sieve_test = c("positive", NA, NA), artefacts_pct = c(1, 0, 0),
                                        munsell_value_moist = c(4, 6, 5), munsell_value_dry = c(6, 7, 7))),
  "M3.2" = list(base = "M3", set = list(munsell_hue_moist = c("10YR", "5YR", "10YR"))),
  "M3.3" = list(base = "M3", set = list(mottle_colors = c(NA, NA, "7.5YR 5/6"))),
  "M3.4" = list(base = "M3", set = list(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120), oc_pct = c(2.0, 0.5, 0.2),
                                        munsell_value_moist = c(2, 5, 5), munsell_chroma_moist = c(2, 6, 3),
                                        munsell_value_dry = c(4, 6, 7))),
  "M3.5" = list(base = "M3"),
  "M4.1" = list(base = "M4", site = list(soil_temperature_regime = "frigid", mean_summer_soil_temp_C = 17)),
  "M4.2" = list(base = "M4", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA)),
                site = list(soil_moisture_regime = "stagnic", climatic_moisture_regime = "perudic",
                            drainage_class = "imperfectly drained")),
  "M4.3" = list(base = "M4", set = list(designation = c("A", "Bw", "R"), bs_pct = c(70, 65, NA), ph_h2o = c(6.5, 6.3, NA),
                                        top_cm = c(0, 15, 90), bottom_cm = c(15, 90, 120)), site = lime),
  "M4.4" = list(base = "M4", set = list(ph_kcl = 3.8, al_kcl_cmol = 4)),
  "M4.5" = list(base = "M4"),
  "M4.6" = list(base = "M4", set = list(ph_h2o = 5.8, bs_pct = c(60, 55, 60), al_kcl_cmol = 0.3)),
  "M5.1" = list(base = "M5", site = list(soil_temperature_regime = "frigid", mean_summer_soil_temp_C = 17)),
  "M5.2" = list(base = "M5", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", NA), bs_pct = c(90, 90, 95, NA),
                                        ph_h2o = c(7.5, 7.6, 7.8, NA)), site = lime),
  "M5.3" = list(base = "M5"),
  "M5.4" = list(base = "M5", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", NA), ph_kcl = c(4.0, 3.8, 3.9, NA),
                                        al_kcl_cmol = c(3, 4, 3, NA), bs_pct = c(30, 20, 25, NA), ph_h2o = c(4.8, 4.6, 4.8, NA)),
                site = list(parent_material = "shale")),
  "M5.5" = list(base = "M5", set = list(munsell_hue_moist = c("5YR", "5YR", "5YR", NA), ph_kcl = 4.8, al_kcl_cmol = 0.2,
                                        al_sat_pct = 5), site = list(parent_material = "shale")),
  "M5.6" = list(base = "M5", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", NA), ph_h2o = c(5.0, 5.0, 5.2, NA),
                                        bs_pct = c(30, 30, 35, NA), ph_kcl = 4.5, al_kcl_cmol = 0.2, al_sat_pct = 5,
                                        fe_dcb_pct = 0.8, fe_total_pct = 3), site = list(parent_material = "shale")),
  "M5.7" = list(base = "M5", set = list(munsell_hue_moist = c("10YR", "10YR", "10YR", NA), ph_kcl = 5.2, al_kcl_cmol = 0.2,
                                        al_sat_pct = 5, fe_dcb_pct = 0.8, fe_total_pct = 3), site = list(parent_material = "shale")),
  # --- N ---
  "N1.1" = list(base = "N1"),
  "N1.2" = list(base = "N1", set = list(designation = c("Ap", "C1", "C2"), foreign_fragments_pct = NA,
                                        stratification_pattern = c(NA, "continuous", "continuous")),
                site = list(silting_rate_cm_yr = 1, silting_kind = "irrigation", land_use = "irrigated cropland")),
  "N2.1" = list(base = "N2", site = list(soil_moisture_regime = "udic", soil_temperature_regime = "cryic",
                                         mean_summer_soil_temp_C = 8, freeze_thaw_days_pct = 75)),
  "N2.2" = list(base = "N2", set = list(mottle_colors = c("7.5YR 5/6", NA)),
                site = list(soil_moisture_regime = "aquic", climatic_moisture_regime = "udic", drainage_class = "poorly drained")),
  "N2.3" = list(base = "N2"),
  "N2.4" = list(base = "N2", site = list(soil_moisture_regime = "ustic")),
  "N2.5" = list(base = "N2", site = list(soil_moisture_regime = "udic")),
  "N3.1" = list(base = "N3", site = list(soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 8,
                                         freeze_thaw_days_pct = 75)),
  "N3.2" = list(base = "N3", set = list(mottle_colors = c(NA, "7.5YR 5/6", NA, NA)),
                site = list(soil_moisture_regime = "aquic", climatic_moisture_regime = "ustic", drainage_class = "poorly drained")),
  "N3.3" = list(base = "N3", site = list(soil_moisture_regime = "aridic", irrigated = FALSE)),
  "N3.4" = list(base = "N3"),
  "N3.5" = list(base = "N3", site = list(soil_moisture_regime = "udic")),
  "N4.1" = list(base = "N4"),
  "N4.2" = list(h = list(designation = c("A", "C"), top_cm = c(0, 15), bottom_cm = c(15, 120),
                         clay_pct = 22, silt_pct = 45, sand_pct = 33, oc_pct = c(0.8, 0.2),
                         munsell_hue_moist = "10RP", munsell_value_moist = 4, munsell_chroma_moist = 3, munsell_value_dry = 5,
                         rock_fragment_max_cm = 2, coarse_fragments_pct = c(5, 20), structure_type = c("granular", "massive"),
                         structure_grade = c("weak", "structureless"), ph_h2o = 6.5, bs_pct = 70, cec_cmol = 15),
                site = list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", parent_material = "purplish shale",
                            flood_frequency = "none")),
  "N4.3" = list(h = list(designation = c("A", "C"), top_cm = c(0, 15), bottom_cm = c(15, 120),
                         clay_pct = 20, silt_pct = 35, sand_pct = 45, oc_pct = c(0.8, 0.2),
                         munsell_hue_moist = "10R", munsell_value_moist = 4, munsell_chroma_moist = 6, munsell_value_dry = 5,
                         structure_type = c("granular", "massive"), structure_grade = c("weak", "structureless"),
                         ph_h2o = 6.0, bs_pct = 60, cec_cmol = 10),
                site = list(soil_moisture_regime = "udic", soil_temperature_regime = "thermic", parent_material = "red sandstone",
                            flood_frequency = "none")),
  "N4.4" = list(h = list(designation = c("A", "C"), top_cm = c(0, 15), bottom_cm = c(15, 100),
                         clay_pct = 12, silt_pct = 30, sand_pct = 58, oc_pct = c(0.8, 0.2),
                         munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5), munsell_chroma_moist = 3,
                         munsell_value_dry = c(5, 6), munsell_chroma_dry = 3, structure_type = c("granular", "massive"),
                         structure_grade = c("weak", "structureless"), ph_h2o = 6.5, bs_pct = 70, cec_cmol = 8),
                site = list(soil_moisture_regime = "udic", soil_temperature_regime = "cryic", mean_summer_soil_temp_C = 8,
                            freeze_thaw_days_pct = 75, parent_material = "granite residuum", flood_frequency = "none",
                            frost_regime = "seasonal")),
  "N4.5" = list(base = "N4.4", site = list(soil_moisture_regime = "aridic", soil_temperature_regime = "mesic",
                                           mean_summer_soil_temp_C = NA, freeze_thaw_days_pct = NA, irrigated = FALSE)),
  "N4.6" = list(base = "N4.4", site = list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic",
                                           mean_summer_soil_temp_C = NA, freeze_thaw_days_pct = NA)),
  "N4.7" = list(base = "N4.4", site = list(soil_moisture_regime = "udic", soil_temperature_regime = "mesic",
                                           mean_summer_soil_temp_C = NA, freeze_thaw_days_pct = NA)),
  NULL)
}

.CST_GROUP_EXAMPLE_CODES <- c(
  "A1.1", "A1.2", "A1.3", "[A1.4]", "A2.1", "A2.2", "A2.3", "A2.4", "[A2.5]",
  "B1.1", "B1.2", "B1.3", "B1.4", "B2.1", "B2.2", "B2.3", "B2.4", "C1.1", "C2.1",
  "D1.1", "D1.2", "D2.1", "D2.2", "D3.1", "D3.2", "E1.1", "E1.2", "E1.3",
  "F1.1", "F1.2", "F2.1", "F2.2", "F3.1", "F3.2", "F3.3",
  "G1.1", "G1.2", "G1.3", "G1.4", "G2.1", "G2.2", "G2.3", "G2.4", "G2.5",
  "H1.1", "H1.2", "H1.3", "H2.1", "H2.2", "I1.1", "I1.2", "I2.1", "I2.2", "I3.1", "I3.2", "I3.3",
  "J1.1", "J1.2", "J2.1", "J2.2", "J2.3", "J2.4", "J2.5", "J3.1", "J3.2", "J3.3",
  "K1.1", "K1.2", "K2.1", "K2.2", "K2.3", "K3.1", "K3.2", "K3.3", "K3.4", "K3.5",
  "L1.1", "L1.2", "L1.3", "L2.1", "L2.2", "L2.3", "L2.4", "L3.1", "L3.2", "L3.3",
  "L4.1", "L4.2", "L4.3", "L4.4", "L4.5", "L4.6", "L4.7",
  "M1.1", "M1.2", "M1.3", "M1.4", "M1.5", "M1.6", "M2.1", "M2.2", "M2.3", "M2.4",
  "M3.1", "M3.2", "M3.3", "M3.4", "M3.5", "M4.1", "M4.2", "M4.3", "M4.4", "M4.5", "M4.6",
  "M5.1", "M5.2", "M5.3", "M5.4", "M5.5", "M5.6", "M5.7",
  "N1.1", "N1.2", "N2.1", "N2.2", "N2.3", "N2.4", "N2.5", "N3.1", "N3.2", "N3.3", "N3.4", "N3.5",
  "N4.1", "N4.2", "N4.3", "N4.4", "N4.5", "N4.6", "N4.7")

# Build the example of a group from its spec.
.cst_group_example <- function(code) {
  sp <- .cst_group_example_spec(code)
  if (is.null(sp)) rlang::abort(sprintf("No example for %s.", code))
  id <- paste0("cst2001-", code)
  if (!is.null(sp$h)) return(.cst_ex_build(sp$h, c(list(id = id), sp$site %||% list())))
  p <- if (sp$base %in% .CST_GROUP_EXAMPLE_CODES) .cst_group_example(sp$base) else make_cst2001_example(sp$base)
  h <- as.data.frame(p$horizons)
  n <- nrow(h)
  for (k in names(sp$set)) h[[k]] <- rep_len(sp$set[[k]], n)
  site <- p$site
  for (k in names(sp$site)) site[[k]] <- sp$site[[k]]
  site$id <- id
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}
