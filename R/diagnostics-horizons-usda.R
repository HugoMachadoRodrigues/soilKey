# ================================================================
# USDA Soil Taxonomy -- the argillic, kandic and oxic horizons
# (Keys to Soil Taxonomy, 13th ed., 2022, Ch. 3, pp. 20-28) and the
# readings they share with the Alfisol, Ultisol and Oxisol keys.
#
# v0.9.420 (audit section 2.4, Hugo, 2026-11-20): the three horizons
# follow the KST text. Until v0.9.419 the argillic horizon was the WRB
# argic horizon (its texture test, and the WRB thresholds when no clay
# films were recorded) and the oxic horizon the WRB ferralic horizon.
# ================================================================


# ---- Shared readings ----------------------------------------------------

# Tri-state OR: TRUE if any is TRUE, FALSE if all are FALSE, else NA.
.usda_or3 <- function(...) {
  x <- c(...)
  if (any(x %in% TRUE)) TRUE
  else if (length(x) && all(x %in% FALSE)) FALSE
  else NA
}

# KST 13th ed., endnote 1 (p. 70): a value is rounded to the precision of
# the limit it is compared with, a 5 to the even digit, before it is compared
# (audit reading L24). R's round() rounds half to even.
.kst_round <- function(x, digits = 0) round(x, digits)

# The clay increase of the KST (argillic, p. 21; kandic, p. 26; oxic, p. 28)
# from the overlying clay `over` to `here`: 3 % (absolute) or more below 15 %
# clay, 1.2 times (20 % relative) from 15 to 40 %, 8 % (absolute) above. At 40
# both rules ask for 48 %. TRUE / FALSE, NA when either clay is missing.
.kst_clay_increase <- function(over, here) {
  if (is.na(over) || is.na(here)) return(NA)
  if (over < 15) .kst_round(here - over) >= 3
  else if (over < 40) .kst_round(here / over, 1) >= 1.2
  else .kst_round(here - over) >= 8
}

# Water retained at 1500 kPa measured by mass, layer by layer (audit reading
# L25): a value the record converts from a volume (ISIS gives it in
# cm3/100 cm3 and its reader divides by the bulk density) is not used for the
# apparent clay of the KST.
.w1500_by_mass <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  w <- h$water_content_1500kpa %||% rep(NA_real_, n)
  ok <- !is.na(w)
  pv <- pedon$provenance
  if (any(ok) && !is.null(pv) && nrow(pv) > 0L && !is.null(pv$notes)) {
    vol <- pv$attribute == "water_content_1500kpa" &
      grepl("by volume|cm3/100 ?cm3|/ bulk density", pv$notes, ignore.case = TRUE)
    i <- unique(pv$horizon_idx[vol])
    ok[i[i >= 1L & i <= n]] <- FALSE
  }
  ok
}

# The apparent clay of the KST: the measured clay or `factor` x (water at
# 1500 kPa - organic carbon), whichever is higher, at most 100 (factor 3 for
# the oxic horizon, p. 28; 2.5 for the kandic horizon, p. 26, and the Pale-
# great groups). The estimate is used only where the water is measured by
# mass (L25) and the organic carbon is recorded.
.apparent_clay_usda <- function(pedon, factor = 2.5) {
  h <- pedon$horizons
  n <- nrow(h)
  w  <- h$water_content_1500kpa %||% rep(NA_real_, n)
  oc <- h$oc_pct %||% rep(NA_real_, n)
  est <- ifelse(.w1500_by_mass(pedon) & !is.na(oc), factor * (w - oc), NA_real_)
  out <- pmax(h$clay_pct, est, na.rm = TRUE)
  pmin(100, out)
}

# The material number of a designation, Arabic ("2Bt") or Roman ("IIBt");
# 1 when none is written, NA without a designation.
.usda_material_number <- function(d) {
  d <- trimws(as.character(d))
  out <- .desg_material_number(d)
  rx <- "^(VII|VI|IV|V|III|II)(?=[ABCEORLMWH])"
  has_r <- !is.na(d) & grepl(rx, d, perl = TRUE)
  if (any(has_r)) {
    rom <- c(II = 2L, III = 3L, IV = 4L, V = 5L, VI = 6L, VII = 7L)
    r <- regmatches(d[has_r], regexpr(rx, d[has_r], perl = TRUE))
    out[has_r] <- unname(rom[r])
  }
  out
}

# Sandy or sandy-skeletal particle-size class for one layer (h sorted), from
# the laboratory texture (sand or loamy sand: silt + 2 clay < 30) or, where
# neither clay nor sand is measured, the field texture class. TRUE / FALSE /
# NA.
.usda_sandy_layer <- function(h, i) {
  cl <- h$clay_pct[i]; si <- h$silt_pct[i]; sa <- h$sand_pct[i]
  if (is.na(si) && !is.na(sa) && !is.na(cl)) si <- 100 - sa - cl
  if (!is.na(cl) && !is.na(si)) return(si + 2 * cl < 30)
  if (!is.na(cl) && cl >= 15) return(FALSE)
  fb <- .field_texture_fallback(h, i)
  if (!is.null(fb)) return(!fb$sandy_loam_or_finer)
  NA
}

# The abrupt textural change (KST 13th ed., p. 31) between an overlying layer
# with clay `over` and an underlying one with clay `here`, across a boundary
# of distinctness `bd`: at least 8 % clay below, and within 7.5 cm the clay
# doubles (overlying clay < 20 %) or rises by 20 % (absolute). An FAO
# boundary that is abrupt (< 2 cm) or clear (2-5 cm) is within 7.5 cm; a
# gradual or diffuse one is not; an unrecorded boundary leaves it undecided
# (audit reading L23: an "abrupt" boundary counts only with this clay rule).
.abrupt_textural_change_usda <- function(over, here, bd) {
  if (is.na(over) || is.na(here)) return(NA)
  if (.kst_round(here) < 8) return(FALSE)
  clay_ok <- if (over < 20) .kst_round(here / max(over, 1e-9), 1) >= 2 else .kst_round(here - over) >= 20
  if (!clay_ok) return(FALSE)
  bd <- tolower(as.character(bd))
  if (is.na(bd) || !nzchar(bd)) return(NA)
  if (grepl("abrupt|clear|sharp|very abrupt", bd)) return(TRUE)
  if (grepl("gradual|diffuse", bd)) return(FALSE)
  NA
}

# The top of the shallowest densic, lithic, paralithic (and, with
# `petroferric`, petroferric) contact the record shows: continuous rock, an
# R or Cr layer, a petroferric contact or a petroplinthic layer. NA when none.
.kst_contact_top_cm <- function(pedon, petroferric = TRUE) {
  h <- pedon$horizons
  tops <- numeric(0)
  rk <- continuous_rock(pedon)
  if (isTRUE(rk$passed)) tops <- c(tops, h$top_cm[rk$layers])
  d <- .desg_master(h$designation %||% rep(NA_character_, nrow(h)), site = pedon$site)
  rc <- which(!is.na(d) & grepl("^(R|Cr)(?![a-qs-z])", d, perl = TRUE))
  tops <- c(tops, h$top_cm[rc])
  if (petroferric) {
    pf <- petroferric_contact_usda(pedon, max_top_cm = Inf)
    if (isTRUE(pf$passed)) tops <- c(tops, h$top_cm[pf$layers])
    pp <- petroplinthic(pedon)
    if (isTRUE(pp$passed)) tops <- c(tops, h$top_cm[pp$layers])
  }
  tops <- tops[!is.na(tops)]
  if (length(tops)) min(tops) else NA_real_
}


# ---- Oxic horizon (KST 13th ed., p. 28) ---------------------------------

#' Oxic horizon (USDA Soil Taxonomy)
#'
#' v0.9.420: the oxic horizon of Keys to Soil Taxonomy (13th ed., 2022, Ch. 3,
#' p. 28), in place of the WRB ferralic horizon it delegated to. A subsurface
#' horizon of adjoining layers, 30 cm or more thick, in which every layer:
#' \itemize{
#'   \item has a texture class of sandy loam or finer;
#'   \item has an apparent CEC of 16 cmol(+) or less per kg clay (NH4OAc,
#'     pH 7), the clay being the measured clay or 3 x (water at 1500 kPa -
#'     organic carbon), whichever is higher, the estimate only where the
#'     water is measured by mass (audit reading L25);
#'   \item has less than 10\% weatherable minerals in the 0.05-0.2 mm fraction
#'     (\code{weatherable_minerals_pct}; never recorded in most data, and
#'     then treated as met, as the evidence says: audit reading L21);
#'   \item has rock structure in less than 5\% of its volume
#'     (\code{rock_structure_pct}, a field observation: absent when not
#'     recorded);
#'   \item has no andic soil properties (a layer that
#'     \code{andic_soil_properties_usda()} finds andic is not oxic);
#' }
#' and whose upper boundary has a clay increase from the horizon above of
#' less than 3\% (absolute), 20\% (relative) or 8\% (absolute) (by the clay of
#' the horizon above), unless the boundary is diffuse (15 cm or more). The
#' layer at the mineral soil surface is not part of it. Values are rounded to
#' the precision of the limit (endnote 1, p. 70).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Ignored (kept for the earlier signature).
#' @return A \code{\link{DiagnosticResult}} (\code{name = "oxic_usda"}).
#' @references Soil Survey Staff (2022). \emph{Keys to Soil Taxonomy}, 13th
#'   edition. USDA-NRCS. Chapter 3, oxic horizon (p. 28).
#' @export
oxic_usda <- function(pedon, ...) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "oxic_usda", list(), .oxic_usda_uncached(pedon = pedon))
}

# The computation of oxic_usda().
.oxic_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch. 3, oxic horizon (p. 28)"
  h0 <- pedon$horizons
  if (is.null(h0) || nrow(h0) == 0L)
    return(DiagnosticResult$new(name = "oxic_usda", passed = FALSE,
             evidence = list(reason = "no layers"), reference = ref))
  h <- as.data.frame(h0)
  o <- order(h$top_cm); n <- length(o); idx <- seq_len(nrow(h))[o]
  hs <- h[o, , drop = FALSE]
  col <- function(x) { v <- hs[[x]]; if (is.null(v)) rep(NA_real_, n) else v }
  top <- hs$top_cm; bot <- hs$bottom_cm; clay <- hs$clay_pct
  ms <- .q_mineral_surface_cm(h0); if (is.na(ms)) ms <- min(top, na.rm = TRUE)
  app <- .apparent_clay_usda(pedon, factor = 3)[o]
  cec <- col("cec_cmol")
  # texture: sandy loam or finer (lab texture, else the field class)
  tex <- vapply(seq_len(n), function(i) {
    sa <- hs$sand_pct[i]; si <- hs$silt_pct[i]; cl <- clay[i]
    if (is.na(si) && !is.na(sa) && !is.na(cl)) si <- 100 - sa - cl
    if (is.na(sa) && !is.na(si) && !is.na(cl)) sa <- 100 - si - cl
    r <- is_sandy_loam_or_finer(sa, si, cl)
    if (is.na(r) && !is.na(cl) && cl >= 15) r <- TRUE
    if (is.na(r)) { fb <- .field_texture_fallback(hs, i); if (!is.null(fb)) r <- fb$sandy_loam_or_finer }
    r
  }, NA)
  ratio <- ifelse(!is.na(cec) & !is.na(app) & app > 0, 100 * cec / app, NA_real_)
  cec_ok <- ifelse(is.na(ratio), NA, .kst_round(ratio) <= 16)
  wm <- col("weatherable_minerals_pct")
  wm_ok <- ifelse(is.na(wm), TRUE, .kst_round(wm) < 10)
  rs <- col("rock_structure_pct")
  rs_ok <- ifelse(is.na(rs), TRUE, .kst_round(rs) < 5)
  an <- andic_soil_properties_usda(pedon)
  andic <- rep(FALSE, n)
  if (isTRUE(an$passed)) andic <- idx %in% an$layers
  ok <- vapply(seq_len(n), function(i) .and3(tex[i], cec_ok[i], wm_ok[i], rs_ok[i], !andic[i]), NA)
  # a subsurface horizon: not the layer at the mineral soil surface
  sub <- !is.na(top) & top > ms
  bdist <- tolower(as.character(hs$boundary_distinctness %||% rep(NA_character_, n)))
  # criterion 5: the clay increase at the upper boundary
  c5 <- vapply(seq_len(n), function(s) {
    if (s == 1L || is.na(top[s - 1L]) || top[s - 1L] < ms) return(TRUE)
    inc <- .kst_clay_increase(clay[s - 1L], clay[s])
    if (is.na(inc)) return(NA)
    if (!inc) return(TRUE)
    if (is.na(bdist[s - 1L]) || !nzchar(bdist[s - 1L])) return(NA)
    grepl("diffuse", bdist[s - 1L])
  }, NA)
  miss <- character(0); cand <- numeric(0)
  for (s in which(sub)) {
    if (isFALSE(ok[s]) || isFALSE(c5[s])) next
    run <- s; j <- s + 1L
    while (j <= n && !isFALSE(ok[j]) && !is.na(top[j]) && !is.na(bot[j - 1L]) &&
           abs(top[j] - bot[j - 1L]) < 1e-6) { run <- c(run, j); j <- j + 1L }
    yes <- run[cumprod(ok[run] %in% TRUE) == 1L]
    thk_yes <- sum(bot[yes] - top[yes], na.rm = TRUE)
    thk_max <- sum(bot[run] - top[run], na.rm = TRUE)
    open_bottom <- anyNA(bot[run])
    if (isTRUE(ok[s]) && isTRUE(c5[s]) && thk_yes >= 30 - 1e-6) {
      return(DiagnosticResult$new(name = "oxic_usda", passed = TRUE, layers = idx[yes],
        evidence = list(upper_boundary_cm = top[s], thickness_cm = thk_yes,
                        cec_per_apparent_clay = ratio[yes],
                        weatherable_minerals = if (all(is.na(wm[yes])))
                          "not recorded: treated as met (audit reading L21)" else wm[yes],
                        apparent_clay_from_water = any(app[yes] > clay[yes] %||% 0, na.rm = TRUE)),
        missing = character(0), reference = ref))
    }
    if (thk_max >= 30 - 1e-6 || open_bottom) {
      cand <- c(cand, top[s])
      if (is.na(c5[s])) miss <- c(miss, "boundary_distinctness")
      bad <- run[is.na(ok[run])]
      if (length(bad)) {
        if (anyNA(cec_ok[bad])) miss <- c(miss, "cec_cmol", "clay_pct")
        if (anyNA(tex[bad])) miss <- c(miss, "sand_pct", "silt_pct")
      }
      if (open_bottom) miss <- c(miss, "bottom_cm")
    }
  }
  passed <- if (length(cand)) NA else FALSE
  DiagnosticResult$new(name = "oxic_usda", passed = passed, layers = integer(0),
    evidence = list(reason = "no subsurface horizon meets the oxic criteria",
                    candidate_tops_cm = unique(cand)),
    missing = unique(miss), reference = ref)
}


# ---- Argillic horizon (KST 13th ed., pp. 20-21) -------------------------

#' Argillic horizon (USDA Soil Taxonomy)
#'
#' v0.9.420: the argillic horizon of Keys to Soil Taxonomy (13th ed., 2022,
#' Ch. 3, pp. 20-21), as its own function (audit item G1). Until v0.9.419 it
#' was the WRB argic horizon: with its texture test (sandy loam or finer),
#' with the WRB thresholds (6 / 1.4 / 20) when no clay films were recorded,
#' always with a clay increase, and with clay films anywhere in the profile.
#' Now a subsurface horizon of adjoining layers (not the layer at the mineral
#' soil surface, nor an O, A, E, C or R horizon) whose upper layer:
#' \itemize{
#'   \item has more clay than the eluvial horizon above within a vertical
#'     distance of 30 cm: 3\% (absolute) more if any part of it has less than
#'     15\% clay, 1.2 times from 15 to 40\%, 8\% (absolute) more from 40\%
#'     (values rounded to the precision of the limit, endnote 1, p. 70); this
#'     item does not apply under a plow layer (Ap) directly above, nor across
#'     a lithologic discontinuity (a change of the material number of the
#'     designations) (item 2);
#' }
#' and that, with the layers below it that keep the clay increase or show
#' illuviation:
#' \itemize{
#'   \item is at least 7.5 cm thick, or one-tenth of the thickness of all
#'     overlying horizons if more; 15 cm if sandy; 15 cm of lamellae if made
#'     of lamellae (item 1.a);
#'   \item shows evidence of clay illuviation in the horizon itself (item
#'     1.b): clay films, oriented clay bridging sand grains
#'     (\code{clay_bridges}), more than 1\% oriented clay in thin sections, a
#'     designation with the suffix t, or a surveyor's "Argillic horizon"
#'     (NASIS). With none of these recorded the horizon is undecided (audit
#'     reading L22): the thin sections and the fine-clay ratio are
#'     measurements that are not recorded.
#' }
#' There is no texture test and no fluvic exclusion: neither is in the KST.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Ignored (kept for the earlier signature).
#' @return A \code{\link{DiagnosticResult}} (\code{name = "argillic_usda"}).
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed.,
#'   Ch. 3, argillic horizon (pp. 20-21).
#' @noRd
argillic_usda <- function(pedon, ...) {
  .memo(pedon, "argillic_usda", list(), .argillic_usda_uncached(pedon = pedon))
}

#' @rdname argillic_usda
#' @noRd
argillic_horizon_usda <- function(pedon) argillic_usda(pedon)

# The computation of argillic_usda().
.argillic_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch. 3, argillic horizon (pp. 20-21)"
  out <- function(passed, layers = integer(0), evidence = list(), missing = character(0))
    DiagnosticResult$new(name = "argillic_usda", passed = passed, layers = layers,
                         evidence = evidence, missing = unique(missing), reference = ref)
  h0 <- pedon$horizons
  if (is.null(h0) || nrow(h0) < 2L)
    return(out(FALSE, evidence = list(reason = "fewer than two layers")))
  h <- as.data.frame(h0)
  o <- order(h$top_cm); n <- length(o); idx <- seq_len(nrow(h))[o]
  hs <- h[o, , drop = FALSE]
  col <- function(x, na = NA_real_) { v <- hs[[x]]; if (is.null(v)) rep(na, n) else v }
  top <- hs$top_cm; bot <- hs$bottom_cm; clay <- hs$clay_pct
  desg <- as.character(col("designation", NA_character_))
  master <- .desg_master(desg, site = pedon$site)
  ms <- .q_mineral_surface_cm(h0); if (is.na(ms)) ms <- min(top, na.rm = TRUE)
  surf <- min(top, na.rm = TRUE)
  mineral <- !is.na(top) & top >= ms
  first_min <- which(mineral)[1]
  eligible <- mineral & seq_len(n) != first_min &
    !(!is.na(master) & grepl("^[OAEHCR]", master))
  # evidence of clay illuviation, layer by layer (item 1.b)
  films <- .clay_films_class(col("clay_films_amount", NA_character_))
  bridges <- col("clay_bridges", NA)
  ts <- suppressWarnings(pmax(col("clay_coatings_thin_section_pct"),
                              col("micromorph_argillans_pct"), na.rm = TRUE))
  t_suf <- !is.na(master) & grepl("^[ABCEORW]+[a-z]*t", master)
  feats <- pedon$site$nasis_diagnostic_features
  nasis <- !is.null(feats) && length(feats) > 0L &&
    any(!is.na(feats) & grepl("argillic", feats, ignore.case = TRUE))
  ev_pos <- (!is.na(films) & films >= 1L) | (bridges %in% TRUE) |
    (!is.na(ts) & ts > 1) | t_suf
  # Reading N5 (Hugo, 2026-11-21): clay films recorded as none, in a layer
  # not designated t and with no clay bridges or thin-section argillans
  # recorded, decide "no evidence of illuviation": the describer looked and
  # saw none. Thin sections and COLE, measurements, could still show it, but
  # are read only where recorded.
  ev_neg <- (!is.na(films) & films == 0L) & !(bridges %in% TRUE) & !t_suf &
    (is.na(ts) | ts <= 1)
  evid <- ifelse(ev_pos, TRUE, ifelse(ev_neg, FALSE, NA))
  # item 2 waived under a plow layer or across a lithologic discontinuity
  mat <- .usda_material_number(desg)
  plow <- !is.na(master) & grepl("^A[a-z]*p", master)
  inc_at <- function(s) {
    if (s > 1L && plow[s - 1L])
      return(list(inc = TRUE, ref = NA_real_, waiver = "plow layer directly above"))
    if (s > 1L && !is.na(mat[s]) && !is.na(mat[s - 1L]) && mat[s] != mat[s - 1L])
      return(list(inc = TRUE, ref = NA_real_, waiver = "lithologic discontinuity directly above"))
    el <- which(seq_len(n) < s & mineral & !is.na(bot) & bot > top[s] - 30)
    if (!length(el)) return(list(inc = FALSE, ref = NA_real_, waiver = NA_character_))
    if (is.na(clay[s]) || all(is.na(clay[el])))
      return(list(inc = NA, ref = NA_real_, waiver = NA_character_))
    mn <- min(clay[el], na.rm = TRUE)
    r <- .kst_clay_increase(mn, clay[s])
    if (!r && anyNA(clay[el])) r <- NA
    list(inc = r, ref = mn, waiver = NA_character_)
  }
  lam <- col("lamellae_cumulative_cm")
  miss <- character(0); cand <- numeric(0)
  for (s in which(eligible)) {
    ic <- inc_at(s)
    if (isFALSE(ic$inc)) next
    run <- s; j <- s + 1L
    while (j <= n && eligible[j] && !is.na(top[j]) && !is.na(bot[j - 1L]) &&
           abs(top[j] - bot[j - 1L]) < 1e-6) {
      keep <- isTRUE(evid[j]) ||
        (if (!is.na(ic$ref)) isTRUE(.kst_clay_increase(ic$ref, clay[j]))
         else !is.na(clay[j]) && !is.na(clay[s]) && clay[j] >= clay[s])
      if (!keep) break
      run <- c(run, j); j <- j + 1L
    }
    thk_l <- bot[run] - top[run]
    thick <- sum(thk_l, na.rm = TRUE)
    sandy <- vapply(run, function(i) .usda_sandy_layer(hs, i), NA)
    need_loamy <- max(7.5, 0.1 * (top[s] - surf))
    need <- if (all(sandy %in% TRUE)) 15 else if (any(sandy %in% FALSE)) need_loamy else NA_real_
    if (all(!is.na(lam[run]))) { thick <- sum(lam[run]); need <- 15 }
    thick_ok <- if (!is.na(need)) {
      if (thick >= need - 1e-6) TRUE else if (anyNA(thk_l)) NA else FALSE
    } else {
      if (thick >= max(15, need_loamy) - 1e-6) TRUE
      else if (thick < min(15, need_loamy) - 1e-6 && !anyNA(thk_l)) FALSE else NA
    }
    ev <- if (nasis || any(evid[run] %in% TRUE)) TRUE
          else if (all(evid[run] %in% FALSE)) FALSE else NA
    ok <- .and3(ic$inc, thick_ok, ev)
    if (isTRUE(ok)) {
      return(out(TRUE, idx[run], evidence = list(
        upper_boundary_cm = top[s], eluvial_clay_pct = ic$ref, waiver = ic$waiver,
        thickness_cm = thick, required_thickness_cm = need,
        illuviation = if (nasis) "NASIS argillic horizon" else
          paste(unique(c(if (any(films[run] >= 1L, na.rm = TRUE)) "clay films",
                         if (any(bridges[run] %in% TRUE)) "clay bridges",
                         if (any(ts[run] > 1, na.rm = TRUE)) "thin sections",
                         if (any(t_suf[run])) "designation t")), collapse = ", "))))
    }
    if (is.na(ok)) {
      cand <- c(cand, top[s])
      if (is.na(ic$inc)) miss <- c(miss, "clay_pct")
      if (is.na(thick_ok)) miss <- c(miss, if (anyNA(thk_l)) "bottom_cm" else c("sand_pct", "silt_pct"))
      if (is.na(ev)) miss <- c(miss, "clay_films_amount")
    }
  }
  out(if (length(cand)) NA else FALSE,
      evidence = list(reason = "no subsurface horizon meets the argillic criteria",
                      candidate_tops_cm = unique(cand),
                      note = if ("clay_films_amount" %in% miss)
                        "no evidence of clay illuviation recorded in the horizon (audit reading L22)"),
      missing = miss)
}


#' Test for clay-illuviation evidence (KST 13ed Ch 3 p 4)
#'
#' KST 13ed argillic horizon requires "evidence of illuvial accumulation
#' of clay" alongside the clay-increase rule. Acceptable evidence:
#' \itemize{
#'   \item oriented clays bridging sand grains in >= 1\% of the horizon;
#'   \item clay films lining pores or coating ped faces;
#'   \item lamellae more than 5 mm thick.
#' }
#'
#' This test reads three complementary slots, in order of evidence strength:
#' \enumerate{
#'   \item \code{pedon$site$nasis_diagnostic_features} -- the NASIS
#'         \code{pediagfeatures.featkind} vector. The surveyor's
#'         explicit "Argillic horizon" entry directly confirms
#'         clay-illuviation evidence (~13 500 entries in the 2021
#'         NASIS snapshot). Strongest evidence.
#'   \item \code{pedon$horizons$clay_films_amount} -- per-horizon
#'         clay-film abundance derived from NASIS \code{phpvsf}.
#'         Values: \code{"few"}, \code{"common"}, \code{"many"},
#'         \code{"continuous"}. Direct measurement.
#'   \item \code{pedon$horizons$designation} containing a 't'
#'         master suffix (e.g. \code{Bt}, \code{Btk}, \code{Btx},
#'         \code{Bt1}, \code{2Bt}). v0.9.28: the pedologist who
#'         wrote that designation explicitly identified the
#'         horizon as clay-illuvial -- per KST 13ed Ch 18, the 't'
#'         suffix means "accumulation of silicate clay" -- so it
#'         counts as positive evidence even when NASIS records are
#'         absent. This unlocks the KST 13ed argillic thresholds
#'         for the ~47 % of KSSL profiles that lack NASIS
#'         pediagfeatures and phpvsf records.
#' }
#'
#' Any of the three sources counts as positive evidence (logical OR).
#' \code{passed = NA} when none is populated AND no horizon designation
#' field is present at all (lab-only loaders without horizon
#' descriptions). \code{passed = FALSE} when designations exist but
#' none has a 't' suffix and NASIS slots are empty.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13th
#'   ed., Ch. 3, argillic horizon (clay-illuviation criteria, p. 4);
#'   Ch. 18, master horizon symbols (\code{t}: silicate-clay
#'   accumulation, p. 332).
#' @export
argillic_clay_films_test <- function(pedon) {
  feats <- pedon$site$nasis_diagnostic_features
  has_pediag <- !is.null(feats) && length(feats) > 0L &&
                  any(!is.na(feats) &
                        grepl("argillic", feats, ignore.case = TRUE))

  h <- pedon$horizons
  cf_col <- h$clay_films_amount
  has_films_any <- !is.null(cf_col) &&
                     any(!is.na(cf_col) & nzchar(cf_col))

  # v0.9.28: designation-based proxy. KST 13ed Ch 18 (master horizon
  # symbols) defines the suffix 't' as "an accumulation of silicate
  # clay that has either formed in the horizon and is subsequently
  # translocated within it, or has been moved into the horizon by
  # illuviation". A pedologist who wrote 'Bt' / 'Btk' / 'Btx' / etc.
  # in the field designation is making exactly the clay-illuviation
  # claim that the KST 13ed argillic test requires. We treat that as
  # positive evidence equivalent to a phpvsf clay-films record. The
  # match is strict: the suffix must be a master horizon AND have 't'
  # AS A SUFFIX OF THE MASTER (not just any 't' in the string -- "BAt"
  # is fine, "Btx" is fine, but "test" is not, and a hypothetical
  # made-up "BTest" would be rejected because we anchor to the master
  # horizon letters [ABCEORW]).
  des_col <- h$designation
  has_t_designation <- FALSE
  designation_layers <- integer(0)
  if (!is.null(des_col) && length(des_col) > 0L) {
    # Match: optional digit prefix + ONE OR MORE master letters
    # [ABCEORW] (transitional horizons like AB / BA / BC have multiple
    # master letters) + a 't' suffix possibly followed by other
    # suffixes/digits. The 't' must be a lowercase suffix letter,
    # NOT a master horizon letter.
    # Examples that match: Bt, Bt1, Btk, Btx, 2Bt, Btss, BAt, ABt, B't.
    # Examples that do NOT match: A, Bw, Bk, BC, C, R, O, Bg, Bs, Bh,
    #                              "test" (no master letters).
    t_pat <- "^[0-9']*[ABCEORW]+[a-z]*t[a-z0-9']*$"
    designation_layers <- which(!is.na(des_col) & nzchar(des_col) &
                                  grepl(t_pat, des_col, ignore.case = FALSE))
    has_t_designation <- length(designation_layers) > 0L
  }

  has_evidence <- isTRUE(has_pediag) ||
                    isTRUE(has_films_any) ||
                    isTRUE(has_t_designation)

  designation_absent <- is.null(des_col) || length(des_col) == 0L ||
                          all(is.na(des_col) | !nzchar(des_col))
  nasis_absent <- (is.null(feats) || length(feats) == 0L) &&
                    (is.null(cf_col) || all(is.na(cf_col)))
  evidence_absent <- nasis_absent && designation_absent

  passed <- if (has_evidence) TRUE
            else if (evidence_absent) NA
            else FALSE

  films_layers <- if (!is.null(cf_col))
                      which(!is.na(cf_col) & nzchar(cf_col))
                    else integer(0)

  # Identify which evidence source produced the TRUE: useful for
  # downstream provenance / debugging.
  evidence_source <- if (isTRUE(has_pediag)) "nasis_pediagfeatures"
                     else if (isTRUE(has_films_any)) "nasis_phpvsf"
                     else if (isTRUE(has_t_designation)) "designation_t_suffix"
                     else NA_character_

  DiagnosticResult$new(
    name = "argillic_clay_films_test",
    passed = passed,
    layers = unique(c(films_layers, designation_layers)),
    evidence = list(
      pediagfeatures_argillic_flag = has_pediag,
      horizons_with_clay_films     = length(films_layers),
      films_summary                = if (length(films_layers) > 0L)
                                          unique(cf_col[films_layers])
                                       else character(0),
      horizons_with_t_designation  = length(designation_layers),
      t_designations               = if (length(designation_layers) > 0L)
                                          des_col[designation_layers]
                                       else character(0),
      evidence_source              = evidence_source
    ),
    missing = if (evidence_absent)
                c("nasis_diagnostic_features", "clay_films_amount", "designation")
              else character(0),
    reference = paste0("Soil Survey Staff (2022), Keys to Soil Taxonomy ",
                         "13th ed., Ch. 3, argillic horizon (clay illuviation, p 4); ",
                         "Ch. 18, master horizon symbols (t suffix, p 332)")
  )
}


# ---- Kandic horizon (KST 13th ed., pp. 26-27) ---------------------------

#' Kandic horizon (USDA, KST 13ed Ch 3, pp. 26-27)
#'
#' v0.9.309: the criteria of Keys to Soil Taxonomy (13th ed., 2022). Until
#' v0.9.308 soilKey took the WRB argic horizon plus a CEC of 16 or less per kg
#' clay. The kandic horizon:
#' \itemize{
#'   \item has its upper boundary where the clay increases with depth within
#'     15 cm by 3\% (absolute) or more if the overlying horizon has < 15\%
#'     clay, 20\% (relative) if 15 to 40\%, 8\% (absolute) if > 40\%; at a
#'     depth within 100 cm if the overlying horizon has 20\% clay or more,
#'     within 125 cm otherwise, or between 100 and 200 cm if the upper 100 cm
#'     is sandy throughout;
#'   \item lies under a coarser surface horizon at least 18 cm thick after
#'     mixing, or 5 cm if the textural transition is abrupt and there is no
#'     densic, lithic, paralithic or petroferric contact within 50 cm;
#'   \item is 30 cm thick or more (15 cm over a densic, lithic, paralithic
#'     or petroferric contact within 50 cm);
#'   \item is loamy very fine sand or finer;
#'   \item has an apparent CEC of 16 cmol(+) or less per kg clay (NH4OAc,
#'     pH 7) in 50\% or more of its thickness between the clay increase and
#'     100 cm below it; the clay is the measured clay or 2.5 x (water at 1500
#'     kPa - organic carbon), whichever is higher, at most 100;
#'   \item has no fine stratification (organic carbon decreasing regularly).
#' }
#' Clay films are not required.
#'
#' v0.9.420 (audit reading L23, Hugo, 2026-11-20): the surface horizon is
#' mixed to 18 cm: a layer that starts above 18 cm is compared, below 18 cm,
#' with the weighted clay of the upper 18 cm; the 5 cm alternative needs an
#' abrupt textural change (KST p. 31), not only an FAO "abrupt" boundary; the
#' kandic horizon is a subsurface horizon (not the layer at the mineral soil
#' surface). The apparent clay uses the water at 1500 kPa only where it is
#' measured by mass (L25), and values are rounded to the precision of the
#' limit (L24, endnote 1, p. 70).
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
kandic_horizon_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "kandic_horizon_usda", list(),
        .kandic_horizon_usda_uncached(pedon = pedon))
}

# The computation of kandic_horizon_usda().
.kandic_horizon_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13ed, Ch. 3, kandic horizon (pp. 26-27)"
  h <- pedon$horizons
  if (is.null(h) || nrow(h) < 2L)
    return(DiagnosticResult$new(name = "kandic_horizon_usda", passed = FALSE,
             layers = integer(0), evidence = list(reason = "fewer than two layers"),
             missing = character(0), reference = ref))
  h <- as.data.frame(h)
  o <- order(h$top_cm); idx <- seq_len(nrow(h))[o]
  top <- h$top_cm[o]; bot <- h$bottom_cm[o]; clay <- h$clay_pct[o]
  ms <- .q_mineral_surface_cm(pedon$horizons); if (is.na(ms)) ms <- 0
  oc <- if (!is.null(h$oc_pct)) h$oc_pct[o] else rep(NA_real_, length(o))
  # the apparent clay of the CEC criterion (water by mass only, L25)
  clay_app <- .apparent_clay_usda(pedon, factor = 2.5)[o]
  cec <- h$cec_cmol[o]
  sand <- if (!is.null(h$sand_pct)) h$sand_pct[o] else rep(NA_real_, length(o))
  sandy <- !is.na(sand) & !is.na(clay) & sand >= 70 & clay < 15        # loamy fine sand or coarser
  # the contact that shortens the thickness and the CEC window (the reading
  # before v0.9.315: the WRB cemented layers added then are not KST contacts)
  contact <- tryCatch(.barrier_top_cm(pedon, other_cemented = FALSE),
                      error = function(e) NA_real_)
  u100 <- which(!is.na(top) & top < ms + 100)
  upper100_sandy <- length(u100) > 0L && all(sandy[u100])
  miss <- character(0); cand_tops <- numeric(0)
  bdist <- if (!is.null(h$boundary_distinctness)) tolower(as.character(h$boundary_distinctness[o])) else rep(NA_character_, length(o))
  undecided <- function(at, what) { miss <<- c(miss, what); cand_tops <<- c(cand_tops, at) }
  # the clay of the upper 18 cm after mixing, when the record covers them
  covered18 <- sum(pmax(0, pmin(bot, ms + 18) - pmax(top, ms))[!is.na(clay)], na.rm = TRUE)
  clay18 <- if (covered18 >= 18 - 1e-6) .q_weighted_mean(clay, top, bot, ms, ms + 18) else NA_real_
  # One candidate: layer k compared with clay `over`, its upper boundary at
  # `kt`. TRUE (returns the result), FALSE, or NA (recorded as undecided).
  try_k <- function(k, over, kt, how) {
    # no kandic horizon starts below 125 cm unless the upper 100 cm are sandy
    if (!upper100_sandy && kt > ms + 125) return(NULL)
    inc <- .kst_clay_increase(over, clay[k])
    if (is.na(inc)) { undecided(kt, "clay_pct"); return(NULL) }
    if (!inc) return(NULL)
    depth_ok <- if (upper100_sandy) kt >= ms + 100 && kt <= ms + 200
                else if (over >= 20) kt <= ms + 100 else kt <= ms + 125
    if (!depth_ok) return(NULL)
    # the run of layers from k down that keep the increase (no drop below the overlying clay)
    run <- k
    j <- k + 1L
    # (a layer with no recorded lower boundary ends the run: what follows it
    # cannot be shown to be contiguous)
    while (j <= length(o) && !is.na(clay[j]) && clay[j] >= over && !sandy[j] &&
           !is.na(top[j]) && isTRUE(abs(top[j] - bot[run[length(run)]]) < 1e-6)) { run <- c(run, j); j <- j + 1L }
    # KST 13 p 26, item 3: "Has a thickness of either: a. 30 cm or more; or
    # b. 15 cm or more" over a contact within 50 cm. A layer of the run with
    # no recorded lower boundary has an unknown thickness: the known layers
    # can meet the minimum, otherwise the thickness is undecided.
    thk <- bot[run] - c(kt, top[run[-1L]])
    thick <- sum(thk, na.rm = TRUE)
    min_thk <- if (!is.na(contact) && contact <= ms + 50) 15 else 30
    if (thick < min_thk) {
      if (anyNA(thk)) undecided(kt, "bottom_cm")
      return(NULL)
    }
    if (any(sandy[run])) return(NULL)
    # CEC <= 16 per kg apparent clay in >= 50% of the thickness from the increase to 100 cm below
    lo <- kt; hi <- kt + 100
    if (!is.na(contact)) hi <- min(hi, contact)
    ov <- pmax(0, pmin(bot, hi) - pmax(top, lo)); ov[is.na(ov)] <- 0
    win <- which(ov > 0)
    ratio <- .kst_round(100 * cec[win] / clay_app[win])
    ok_cm <- sum(ov[win][!is.na(ratio) & ratio <= 16]); bad_cm <- sum(ov[win][!is.na(ratio) & ratio > 16])
    na_cm <- sum(ov[win][is.na(ratio)]) + max(0, (hi - lo) - sum(ov[win]))
    half <- (hi - lo) / 2
    cec_ok <- if (ok_cm >= half - 1e-6) TRUE else if (ok_cm + na_cm < half - 1e-6) FALSE else NA
    if (is.na(cec_ok)) { undecided(kt, "cec_cmol"); return(NULL) }
    if (!cec_ok) return(NULL)
    # no fine stratification: organic carbon does not rise again with depth
    oc_run <- oc[seq_len(max(run))]
    strat <- any(diff(oc_run[!is.na(oc_run)]) > 0.3)
    if (isTRUE(strat)) return(NULL)
    DiagnosticResult$new(name = "kandic_horizon_usda", passed = TRUE,
      layers = idx[run],
      evidence = list(upper_boundary_cm = kt, overlying_clay_pct = over,
                      surface_horizon = how,
                      thickness_cm = thick, cec_low_cm = ok_cm, window = c(lo, hi)),
      missing = character(0), reference = ref)
  }
  for (k in seq_along(o)[-1]) {
    # a subsurface horizon under a mineral horizon (L23): not the layer at
    # the mineral soil surface, and not compared across organic layers; no
    # kandic horizon starts below 200 cm
    if (is.na(top[k]) || top[k] <= ms || is.na(top[k - 1]) || top[k - 1] < ms) next
    if (top[k] > ms + 200) next
    # the increase is reached within a vertical distance of 15 cm: not across
    # a diffuse boundary (> 15 cm)
    if (!is.na(bdist[k - 1]) && grepl("diffuse", bdist[k - 1])) next
    if (top[k] - ms >= 18) {
      r <- try_k(k, clay[k - 1], top[k], "18 cm or more")
      if (!is.null(r)) return(r)
      next
    }
    # the surface horizon above is thinner than 18 cm: 5 cm or more with an
    # abrupt textural change and no contact within 50 cm, or else the upper
    # 18 cm mixed
    atc <- if (top[k] - ms < 5 || (!is.na(contact) && contact <= ms + 50)) FALSE
           else .abrupt_textural_change_usda(clay[k - 1], clay[k], bdist[k - 1])
    if (isTRUE(atc)) {
      r <- try_k(k, clay[k - 1], top[k], "abrupt textural change")
      if (!is.null(r)) return(r)
      next
    }
    n_before <- length(cand_tops)
    if (!is.na(bot[k]) && bot[k] > ms + 18) {
      if (is.na(clay18)) undecided(ms + 18, "clay_pct")
      else {
        r <- try_k(k, clay18, ms + 18, "upper 18 cm after mixing")
        if (!is.null(r)) { r$evidence$after_mixing <- TRUE; return(r) }
      }
    }
    if (is.na(atc) && length(cand_tops) == n_before) {
      # the short route might hold if the boundary were known
      if (isTRUE(.kst_clay_increase(clay[k - 1], clay[k]))) undecided(top[k], "boundary_distinctness")
    }
  }
  passed <- if (length(miss)) NA else FALSE
  DiagnosticResult$new(name = "kandic_horizon_usda", passed = passed,
    layers = integer(0), evidence = list(reason = "no layer meets the kandic criteria",
                                         candidate_tops_cm = unique(cand_tops)),
    missing = unique(miss), reference = ref)
}
