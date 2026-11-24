# =============================================================================
# v0.9.431 -- USDA family: the control section for particle-size classes and
# the particle-size classes with their substitutes (Soil Survey Staff 2022,
# Keys to Soil Taxonomy 13th ed., ch. 17, pp. 355-362).
#
# Three-valued throughout (TRUE / FALSE / NA). A field observation not
# recorded is absent (root-limiting layers, coarse fragments, artifacts); a
# measurement not recorded leaves the test undecided; an undecided class is
# value NULL with the missing columns in $missing and a reason in $evidence.
# The readings F5-F11 and F18 are those of the author (USDA audit, wave 5,
# 2026-11-23); see NEWS.md, 0.9.431.
# =============================================================================

.PS_REF_USDA <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17, pp. 355-362"

# Substitute classes (p. 356, pp. 358-359): named by the thickest part when
# the control section is not strongly contrasting (p. 358). Fragmental is a
# particle-size class, not a substitute.
.PS_SUBSTITUTES_USDA <- c(
  "pumiceous", "cindery", "ashy", "ashy-pumiceous", "ashy-skeletal",
  "medial", "medial-pumiceous", "medial-skeletal", "hydrous",
  "hydrous-pumiceous", "hydrous-skeletal", "diatomaceous",
  "gypseous-skeletal", "coarse-gypseous", "fine-gypseous")

# The closed list of strongly contrasting particle-size classes (pp. 361-362),
# as data: the classes the upper and the lower part may have ("|" joins the
# members of a combined name, p. 361), the family name, and the condition
# attached to the pair (none; clay25: an absolute difference of 25 % clay or
# more in the fine earth of the two parts; gyp15: 15 % gypsum or more;
# fe35: 35 % (absolute) more fine earth by volume in the upper part; fs50_up /
# fs50_low: the loamy part (upper / lower) has less than 50 % by weight fine
# sand or coarser; ash_med: water at 1500 kPa on dried samples 10 % or less
# in the ashy part and 15 % or more in the medial part; med_hyd: water at
# 1500 kPa on undried samples 75 % or less in the medial part).
.PS_CONTRAST_PAIRS_USDA <- local({
  p <- function(n, up, low, name, cond = "none")
    data.frame(n = n, upper = up, lower = low, name = name, cond = cond,
               stringsAsFactors = FALSE)
  SS <- "sandy|sandy-skeletal"
  rbind(
    p(1,  "ashy", "clayey", "ashy over clayey"),
    p(2,  "ashy", "clayey-skeletal", "ashy over clayey-skeletal"),
    p(3,  "ashy", "loamy", "ashy over loamy"),
    p(4,  "ashy", "loamy-skeletal", "ashy over loamy-skeletal"),
    p(5,  "ashy", "medial", "ashy over medial", "ash_med"),
    p(6,  "ashy", "medial-skeletal", "ashy over medial-skeletal"),
    p(7,  "ashy", "pumiceous|cindery", "ashy over pumiceous or cindery"),
    p(8,  "ashy", SS, "ashy over sandy or sandy-skeletal"),
    p(9,  "ashy-skeletal", "clayey", "ashy-skeletal over clayey"),
    p(10, "ashy-skeletal", "fragmental|cindery", "ashy-skeletal over fragmental or cindery", "fe35"),
    p(11, "ashy-skeletal", "loamy-skeletal", "ashy-skeletal over loamy-skeletal"),
    p(12, "ashy-skeletal", SS, "ashy-skeletal over sandy or sandy-skeletal"),
    p(13, "cindery", "loamy", "cindery over loamy"),
    p(14, "cindery", "medial", "cindery over medial"),
    p(15, "cindery", "medial-skeletal", "cindery over medial-skeletal"),
    p(16, "clayey", "coarse-gypseous", "clayey over coarse-gypseous"),
    p(17, "clayey", "fine-gypseous", "clayey over fine-gypseous", "gyp15"),
    p(18, "clayey", "fragmental", "clayey over fragmental"),
    p(19, "clayey", "gypseous-skeletal", "clayey over gypseous-skeletal"),
    p(20, "clayey", "loamy", "clayey over loamy", "clay25"),
    p(21, "clayey", "loamy-skeletal", "clayey over loamy-skeletal", "clay25"),
    p(22, "clayey", SS, "clayey over sandy or sandy-skeletal"),
    p(23, "clayey-skeletal", SS, "clayey-skeletal over sandy or sandy-skeletal"),
    p(24, "coarse-loamy", "clayey", "coarse-loamy over clayey"),
    p(25, "coarse-loamy", "fragmental", "coarse-loamy over fragmental"),
    p(26, "coarse-loamy", SS, "coarse-loamy over sandy or sandy-skeletal", "fs50_up"),
    p(27, "coarse-silty", "clayey", "coarse-silty over clayey"),
    p(28, "coarse-silty", SS, "coarse-silty over sandy or sandy-skeletal"),
    p(29, "fine-loamy", "clayey", "fine-loamy over clayey", "clay25"),
    p(30, "fine-loamy", "fragmental", "fine-loamy over fragmental"),
    p(31, "fine-loamy", SS, "fine-loamy over sandy or sandy-skeletal"),
    p(32, "fine-silty", "clayey", "fine-silty over clayey", "clay25"),
    p(33, "fine-silty", "fragmental", "fine-silty over fragmental"),
    p(34, "fine-silty", SS, "fine-silty over sandy or sandy-skeletal"),
    p(35, "hydrous", "clayey", "hydrous over clayey"),
    p(36, "hydrous", "clayey-skeletal", "hydrous over clayey-skeletal"),
    p(37, "hydrous", "fragmental", "hydrous over fragmental"),
    p(38, "hydrous", "loamy", "hydrous over loamy"),
    p(39, "hydrous", "loamy-skeletal", "hydrous over loamy-skeletal"),
    p(40, "hydrous", SS, "hydrous over sandy or sandy-skeletal"),
    p(41, "loamy", "ashy|ashy-pumiceous", "loamy over ashy or ashy-pumiceous"),
    p(42, "loamy", "coarse-gypseous", "loamy over coarse-gypseous", "gyp15"),
    p(43, "loamy", "fine-gypseous", "loamy over fine-gypseous", "gyp15"),
    p(44, "loamy", "pumiceous|cindery", "loamy over pumiceous or cindery"),
    p(45, "loamy", SS, "loamy over sandy or sandy-skeletal", "fs50_up"),
    p(46, "loamy-skeletal", "cindery", "loamy-skeletal over cindery", "fe35"),
    p(47, "loamy-skeletal", "clayey", "loamy-skeletal over clayey", "clay25"),
    p(48, "loamy-skeletal", "fragmental", "loamy-skeletal over fragmental", "fe35"),
    p(49, "loamy-skeletal", "gypseous-skeletal", "loamy-skeletal over gypseous-skeletal", "gyp15"),
    p(50, "loamy-skeletal", SS, "loamy-skeletal over sandy or sandy-skeletal", "fs50_up"),
    p(51, "medial", "ashy", "medial over ashy", "ash_med"),
    p(52, "medial", "ashy-pumiceous|ashy-skeletal", "medial over ashy-pumiceous or ashy-skeletal", "ash_med"),
    p(53, "medial", "clayey", "medial over clayey"),
    p(54, "medial", "clayey-skeletal", "medial over clayey-skeletal"),
    p(55, "medial", "fragmental", "medial over fragmental"),
    p(56, "medial", "hydrous", "medial over hydrous", "med_hyd"),
    p(57, "medial", "loamy", "medial over loamy"),
    p(58, "medial", "loamy-skeletal", "medial over loamy-skeletal"),
    p(59, "medial", "pumiceous|cindery", "medial over pumiceous or cindery"),
    p(60, "medial", SS, "medial over sandy or sandy-skeletal"),
    p(61, "medial-skeletal", "fragmental|cindery", "medial-skeletal over fragmental or cindery", "fe35"),
    p(62, "medial-skeletal", "loamy-skeletal", "medial-skeletal over loamy-skeletal"),
    p(63, "medial-skeletal", SS, "medial-skeletal over sandy or sandy-skeletal"),
    p(64, "pumiceous|ashy-pumiceous", "loamy", "pumiceous or ashy-pumiceous over loamy"),
    p(65, "pumiceous|ashy-pumiceous", "loamy-skeletal", "pumiceous or ashy-pumiceous over loamy-skeletal"),
    p(66, "pumiceous|ashy-pumiceous", "medial", "pumiceous or ashy-pumiceous over medial"),
    p(67, "pumiceous|ashy-pumiceous", "medial-skeletal", "pumiceous or ashy-pumiceous over medial-skeletal"),
    p(68, "pumiceous|ashy-pumiceous", SS, "pumiceous or ashy-pumiceous over sandy or sandy-skeletal"),
    p(69, "sandy", "clayey", "sandy over clayey"),
    p(70, "sandy", "loamy", "sandy over loamy", "fs50_low"),
    p(71, "sandy-skeletal", "loamy", "sandy-skeletal over loamy", "fs50_low")
  )
})


# ---- small three-valued helpers ---------------------------------------------

.ps_and3 <- function(...) {
  x <- c(...)
  if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
}
.ps_or3 <- function(...) {
  x <- c(...)
  if (any(x %in% TRUE)) TRUE else if (all(x %in% FALSE)) FALSE else NA
}
.ps_num <- function(h, col) suppressWarnings(as.numeric(.usda_col(h, col, NA_real_)))
.ps_txt <- function(h, col) tolower(trimws(as.character(.usda_col(h, col, NA_character_))))


#' Depth-weighted average of a horizon property over a depth window (USDA)
#'
#' Each horizon is clipped to the window \code{[lo, hi]} and weighted by the
#' thickness it has inside it (KST 13th ed., p. 373, endnote 6). The parts
#' with a measured value must cover \code{min_cover} (default half, reading
#' F18) of the window; otherwise the average is undecided (NA).
#'
#' @param top,bottom Horizon depths (cm).
#' @param x Values of the property, one per horizon.
#' @param lo,hi The window (cm).
#' @param w Optional weights in place of the clipped thicknesses.
#' @param min_cover Share of the window the measured parts must cover.
#' @return A list: \code{value} (NA when undecided) and \code{cover} (share).
#' @noRd
.wavg_clip_usda <- function(top, bottom, x, lo, hi, w = NULL, min_cover = 0.5) {
  if (is.null(w)) {
    w <- pmax(pmin(bottom, hi) - pmax(top, lo), 0)
    w[is.na(w)] <- 0
  }
  tot <- sum(w)
  if (tot <= 0) return(list(value = NA_real_, cover = 0))
  have <- !is.na(x) & w > 0
  cov <- sum(w[have])
  if (cov <= 0 || cov < min_cover * tot - 1e-9)
    return(list(value = NA_real_, cover = cov / tot))
  list(value = sum(x[have] * w[have]) / cov, cover = cov / tot)
}


# ---- taxon context -------------------------------------------------------

# What the particle-size key needs to know of the taxa assigned above the
# family: order code, great group and subgroup names.
.ps_context_usda <- function(order_code = NULL, great_group = NULL, subgroup = NULL) {
  nm <- function(x) {
    if (is.list(x)) x <- x$name %||% x$code
    x <- as.character(x %||% "")
    if (!length(x) || is.na(x[1L])) "" else x[1L]
  }
  ord <- toupper(nm(order_code)); gg <- tolower(nm(great_group)); sg <- nm(subgroup)
  has <- function(w) grepl(paste0("\\b", w, "\\b"), sg)
  list(
    order = ord, great_group = gg, subgroup = sg,
    vertisol = identical(ord, "VE"),
    andisol = identical(ord, "AD"),
    four = ord %in% c("AF", "UT", "AS", "MO"),
    oxisol = identical(ord, "OX"),
    histosol = identical(ord, "HI"),
    histel = grepl("istels$", gg),
    terric = has("Terric"),
    wassent = grepl("wassents$", gg),
    # p. 356: no class for these taxa (they are sandy by definition)
    psamm_taxon = grepl("psamments$", gg) ||
      gg %in% c("psammaquents", "psammowassents", "psammoturbels", "psammorthels"),
    psammentic = has("Psammentic"),
    lithic = has("Lithic"),
    arenic = has("Arenic"),
    grossarenic = has("Grossarenic"),
    lamellic = has("Lamellic"),
    volcanic_taxon = identical(ord, "AD") || grepl("Andic|Vitrandic|Vitric|Aquandic", sg)
  )
}


# ---- root-limiting layers (p. 357) ------------------------------------------

#' Root-limiting layers of the family key (USDA)
#'
#' The tops of the root-limiting layers of the family key (KST 13th ed., p.
#' 357): duripan, fragipan, petrocalcic, petrogypsic and placic horizons,
#' continuous ortstein, and densic, lithic, manufactured-layer, paralithic and
#' petroferric contacts. A field observation: a layer not recorded is absent.
#' A layer with the suffix m (cemented in 90 percent or more) counts whatever
#' its cementing agent; a geomembrane counts as a manufactured layer.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A list: \code{top} (cm, Inf when none), \code{kind}, and
#'   \code{fragipan_top} (cm, Inf when none).
#' @noRd
.root_limiting_usda <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(list(top = Inf, kind = NA_character_, fragipan_top = Inf))
  tops <- c(); kinds <- c()
  add <- function(t, k) {
    t <- t[!is.na(t)]
    if (length(t)) { tops <<- c(tops, min(t)); kinds <<- c(kinds, k) }
  }
  dr <- function(f) tryCatch(f, error = function(e) NULL)
  fr <- dr(fragipan_usda(pedon, max_top_cm = Inf))
  fp_top <- Inf
  if (isTRUE(fr$passed) && length(fr$layers)) {
    fp_top <- min(h$top_cm[fr$layers], na.rm = TRUE)
    add(fp_top, "fragipan")
  }
  d <- .usda_desig(h$designation %||% rep(NA_character_, n))
  fx <- which(!is.na(d$suffix) & grepl("x", d$suffix) & !is.na(d$master) & grepl("B|C|E", d$master))
  if (length(fx) && !is.finite(fp_top)) {
    # a recorded fragipan (suffix x) counts as the field record of one
    fp_top <- min(h$top_cm[fx], na.rm = TRUE)
    add(fp_top, "fragipan")
  }
  for (nm in c("duripan", "petrocalcic", "petrogypsic", "placic")) {
    r <- switch(nm,
      duripan = dr(duripan_usda(pedon, max_top_cm = Inf)),
      petrocalcic = dr(petrocalcic_subgroup_usda(pedon, max_top_cm = Inf)),
      petrogypsic = dr(petrogypsic_horizon_usda(pedon, max_top_cm = Inf)),
      placic = dr(placic_horizon_usda(pedon, max_top_cm = Inf)))
    if (isTRUE(r$passed) && length(r$layers)) add(h$top_cm[r$layers], nm)
  }
  # v0.9.433 (reading F15): ortstein is root-limiting only when continuous
  # (p. 369: 90 percent or more cemented and laterally continuous); a
  # cemented spodic layer whose lateral continuity is not recorded is not.
  pan <- .ps_txt(h, "pan_type")
  spod_m <- (!is.na(d$master) & grepl("B", d$master) & grepl("[hs]", d$suffix %||% "")) |
    (!is.na(pan) & pan == "ortstein")
  cont <- .ps_txt(h, "horizon_continuity")
  m <- which(!is.na(d$suffix) & grepl("m", d$suffix) &
               (!spod_m | (!is.na(cont) & cont == "continuous")))
  if (length(m)) add(h$top_cm[m], "cemented layer (suffix m)")
  add(dr(.kst_contact_top_cm(pedon)), "lithic, paralithic or petroferric contact")
  add(dr(.usda_contact_top_cm(h)), "densic, lithic or paralithic contact")
  lc <- dr(lithic_contact_usda(pedon, max_top_cm = Inf))
  if (isTRUE(lc$passed) && length(lc$layers)) add(h$top_cm[lc$layers], "lithic contact")
  gm <- as.logical(.usda_col(h, "geomembrane_present", NA))
  if (any(gm %in% TRUE)) add(h$top_cm[gm %in% TRUE], "manufactured layer")
  if (!length(tops)) return(list(top = Inf, kind = NA_character_, fragipan_top = fp_top))
  k <- which.min(tops)
  list(top = tops[k], kind = kinds[k], fragipan_top = fp_top)
}

# The argillic, kandic or natric horizon (and, with `kinds`, only some of
# them): passed (TRUE / FALSE / NA), its upper and lower boundaries (cm).
.ps_clay_horizon_usda <- function(pedon, kinds = c("argillic", "kandic", "natric")) {
  h <- pedon$horizons
  res <- lapply(kinds, function(k) tryCatch(switch(k,
    argillic = argillic_usda(pedon),
    kandic = kandic_horizon_usda(pedon),
    natric = natric_horizon_usda(pedon)), error = function(e) NULL))
  pass <- vapply(res, function(r) if (is.null(r)) NA else as.logical(r$passed)[1L], NA)
  lay <- unique(unlist(lapply(res[pass %in% TRUE], function(r) r$layers)))
  lay <- lay[!is.na(lay)]
  passed <- if (any(pass %in% TRUE) && length(lay)) TRUE
            else if (all(pass %in% FALSE)) FALSE else if (any(pass %in% TRUE)) TRUE else NA
  top <- if (length(lay)) min(h$top_cm[lay], na.rm = TRUE) else NA_real_
  bot <- if (length(lay)) max(h$bottom_cm[lay], na.rm = TRUE) else NA_real_
  miss <- unique(unlist(lapply(res[is.na(pass)], function(r) r$missing)))
  list(passed = passed, top = top, bottom = bot, kinds = kinds[pass %in% TRUE],
       missing = if (is.na(passed)) miss else character(0))
}


#' Control section for particle-size classes and their substitutes (USDA)
#'
#' The key to the control section for particle-size classes and their
#' substitutes in mineral soils of the Keys to Soil Taxonomy (13th ed., 2022,
#' ch. 17, pp. 357-358), cases A to G: a root-limiting layer within 36 cm
#' (A); Andisols (B); Alfisols, Ultisols and the Aridisols and Mollisols with
#' an argillic, kandic or natric horizon starting within 100 cm and ending at
#' 25 cm or deeper, or in Arenic or Grossarenic subgroups (C, items 1 to 4);
#' the same orders with that horizon at 100 cm or deeper or in Lamellic
#' subgroups (D); an argillic or natric horizon ending above 25 cm (E);
#' Wassents (F); and all other mineral soils, from the Ap horizon or 25 cm to
#' 100 cm or a root-limiting layer (G). Depths are from the mineral soil
#' surface (or the top of organic material with andic soil properties, when
#' shallower).
#'
#' Root-limiting layers (p. 357) are field observations: one not recorded is
#' absent. When a test that decides the case is undecided, the bounds are NA
#' and the missing columns are listed.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code USDA order code (e.g. \code{"AF"}), or NULL.
#' @param great_group Great group name (e.g. \code{"Hapludalfs"}), or NULL.
#' @param subgroup Subgroup name (e.g. \code{"Arenic Hapludalfs"}), or NULL.
#' @return A list: \code{top}, \code{bottom} (cm), \code{case} (A to G, with
#'   the item of case C, e.g. \code{"C4"}), \code{evidence} and
#'   \code{missing}.
#' @references Soil Survey Staff (2022). \emph{Keys to Soil Taxonomy}, 13th
#'   ed. USDA-NRCS. Chapter 17, pp. 357-358.
#' @examples
#' p <- make_luvisol_canonical()
#' family_control_section_usda(p, order_code = "AF",
#'                             great_group = "Hapludalfs",
#'                             subgroup = "Typic Hapludalfs")[c("top", "bottom", "case")]
#' @export
family_control_section_usda <- function(pedon, order_code = NULL,
                                        great_group = NULL, subgroup = NULL) {
  ctx <- .ps_context_usda(order_code, great_group, subgroup)
  .ps_control_section_usda(pedon, ctx)
}

.ps_control_section_usda <- function(pedon, ctx, layers = NULL) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17, pp. 357-358"
  out <- function(top, bottom, case, ev = list(), missing = character(0)) {
    if (!is.na(top) && !is.na(bottom) && bottom < top) bottom <- top
    list(top = top, bottom = bottom, case = case,
         evidence = c(base_ev, ev, list(reference = ref)), missing = unique(missing))
  }
  undecided <- function(case, why, missing)
    out(NA_real_, NA_real_, case, list(reason = why), missing)
  if (is.null(h) || !nrow(h)) {
    base_ev <- list()
    return(undecided(NA_character_, "no layers", "horizons"))
  }
  ms <- .usda_mineral_surface(pedon)
  at <- tryCatch(.andic_top_usda(pedon)$top, error = function(e) ms)
  start <- min(ms, at, na.rm = TRUE)
  rl <- .root_limiting_usda(pedon)
  rlt <- rl$top
  ap <- .sg_ie_ap_bottom(h)
  base_ev <- list(mineral_surface_cm = ms, start_cm = start,
                  root_limiting_top_cm = if (is.finite(rlt)) rlt else NA_real_,
                  root_limiting_kind = rl$kind,
                  ap_bottom_cm = ap)
  upper25 <- max(c(ms + 25, ap), na.rm = TRUE)
  low100 <- min(ms + 100, rlt)

  # A. thin soil over a root-limiting layer
  if (rlt <= start + 36) return(out(start, rlt, "A"))
  # B. Andisols
  if (ctx$andisol) return(out(start, min(start + 100, rlt), "B"))

  # C and D: Alfisols, Ultisols, Aridisols and Mollisols
  arg <- NULL
  if (ctx$four) {
    arg <- .ps_clay_horizon_usda(pedon)
    in_c <- if (ctx$arenic || ctx$grossarenic) TRUE
            else if (ctx$lamellic) FALSE
            else if (isTRUE(arg$passed))
              (arg$top < ms + 100) && (arg$bottom >= ms + 25)
            else if (isFALSE(arg$passed)) FALSE else NA
    if (is.na(in_c))
      return(undecided("C", "argillic, kandic or natric horizon undecided (case C)",
                       c(arg$missing, "clay_pct")))
    if (isTRUE(in_c)) {
      if (!isTRUE(arg$passed))
        return(undecided("C", "Arenic or Grossarenic subgroup with no argillic, kandic or natric horizon identified",
                         c(arg$missing, "clay_pct")))
      ev_c <- list(clay_horizon = arg$kinds, clay_horizon_top_cm = arg$top,
                   clay_horizon_bottom_cm = arg$bottom)
      # C1: a strongly contrasting class within or below the horizon and
      # within 100 cm
      if (is.null(layers)) layers <- .ps_layers_usda(pedon, ctx)
      con <- .ps_contrasts_usda(layers, arg$top, ms + 100, ctx,
                                gen_soil = .ps_generalized_soil_usda(pedon, ctx, rl)$value,
                                window_mode = "c1", start = start, rlt = rlt)
      c1 <- if (any(con$passed %in% TRUE)) TRUE else if (any(is.na(con$passed))) NA else FALSE
      if (is.na(c1))
        return(undecided("C1", "strongly contrasting classes below the clay-enriched horizon undecided",
                         unique(c(con$missing, "boundary_distinctness"))))
      if (isTRUE(c1))
        return(out(arg$top, min(max(arg$top + 50, ms + 100), rlt), "C1",
                   c(ev_c, list(contrast = con$name[con$passed %in% TRUE][1L]))))
      ft <- rl$fragipan_top
      if (is.finite(ft) && arg$top >= ft)
        return(out(ms + 25, ft, "C2", ev_c))
      if (is.finite(ft) && ft < arg$top + 50)
        return(out(arg$top, ft, "C3", ev_c))
      bot <- if (arg$bottom - arg$top <= 50) arg$bottom else arg$top + 50
      return(out(arg$top, min(bot, rlt), "C4", ev_c))
    }
    in_d <- ctx$lamellic || (isTRUE(arg$passed) && arg$top >= ms + 100)
    if (in_d && !(ctx$arenic || ctx$grossarenic))
      return(out(upper25, low100, "D"))
  }

  # E. an argillic or natric horizon with its lower boundary above 25 cm
  an <- .ps_clay_horizon_usda(pedon, kinds = c("argillic", "natric"))
  in_e <- if (isTRUE(an$passed)) an$bottom < ms + 25
          else if (isFALSE(an$passed)) FALSE
          else {
            # undecided horizon: only a B layer ending above 25 cm could make it
            d <- .usda_desig(h$designation)
            cand <- !is.na(h$bottom_cm) & h$bottom_cm < ms + 25 & h$top_cm >= ms &
              (is.na(d$master) | grepl("B", d$master))
            if (any(cand)) NA else FALSE
          }
  if (is.na(in_e))
    return(undecided("E", "argillic or natric horizon near the surface undecided (case E)",
                     c(an$missing, "clay_pct")))
  if (isTRUE(in_e))
    return(out(an$top, low100, "E", list(clay_horizon_top_cm = an$top,
                                           clay_horizon_bottom_cm = an$bottom)))
  # F. Wassents
  if (ctx$wassent) return(out(ms, low100, "F"))
  # G. all other mineral soils
  out(upper25, low100, "G")
}


# ---- per-layer readings ---------------------------------------------------

# Width (cm) of a boundary from its distinctness: abrupt < 2, clear 2-5,
# gradual 5-15, diffuse > 15; not recorded: anything.
.ps_bd_width <- function(bd) {
  bd <- tolower(trimws(as.character(bd)))
  if (is.na(bd) || !nzchar(bd)) return(c(0, Inf))
  if (grepl("abrupt|sharp", bd)) return(c(0, 2))
  if (grepl("clear", bd)) return(c(2, 5))
  if (grepl("gradual", bd)) return(c(5, 15))
  if (grepl("diffuse", bd)) return(c(15, Inf))
  c(0, Inf)
}

# Particles 0.1-2 mm, % of the fine earth: exact with very_fine_sand_pct
# (sand - very fine sand), else bounds (reading F6): at least the coarse sand
# (0.2-2 mm) or sand - fine sand (0.05-0.2 mm), at most the whole sand.
.ps_sand01 <- function(sand, vfs, cs, fs) {
  if (is.na(sand)) return(c(NA_real_, NA_real_))
  if (!is.na(vfs)) { v <- max(sand - vfs, 0); return(c(v, v)) }
  lo <- 0
  if (!is.na(cs)) lo <- max(lo, cs)
  if (!is.na(fs)) lo <- max(lo, sand - fs)
  c(min(lo, sand), sand)
}

# Texture S or LS by the triangle: silt + 2 clay < 30 (reading F7: no
# clay-and-sand shortcut). From the laboratory fractions, else the field
# texture class where clay and sand are not measured.
.ps_sandy_tex <- function(clay, silt, sand, tc_class) {
  if (is.na(silt) && !is.na(sand) && !is.na(clay)) silt <- 100 - sand - clay
  if (!is.na(clay) && !is.na(silt)) return(silt + 2 * clay < 30)
  if (!is.na(clay) && clay >= 15) return(FALSE)
  if (is.na(clay) && is.na(sand) && !is.na(tc_class))
    return(tc_class %in% c("S", "COS", "MS", "FS", "VFS", "US", "LS", "LCOS", "LFS", "LVFS"))
  NA
}

# Reading F7: is the sand grade other than very fine? Very fine sand is
# treated as silt unless the texture is loamy fine sand or coarser (p. 355),
# so a sand or loamy sand is sandy only when very fine sand cannot be its
# grade (50 % or more very fine sand).
.ps_grade_ok <- function(sand, vfs, fs, cs, tc_class, grade) {
  if (!is.na(vfs)) return(vfs < 50)
  if (!is.na(tc_class)) {
    if (tc_class %in% c("COS", "MS", "FS", "LS", "LCOS", "LFS")) return(TRUE)
    if (tc_class %in% c("VFS", "LVFS")) return(FALSE)
  }
  if (!is.na(grade)) {
    if (grepl("very", grade)) return(FALSE)
    if (grepl("fine|medium|coarse", grade)) return(TRUE)
  }
  if (!is.na(fs) && fs < 50) return(TRUE)
  if (!is.na(cs) && !is.na(sand) && sand - cs < 50) return(TRUE)
  NA
}

# Particles 0.1-75 mm, % by weight of the fraction < 75 mm (reading F6), as
# bounds: gravel and cobbles by weight (gravel_wt_pct + cobbles_wt_pct, % of
# the whole soil) plus the 0.1-2 mm sand of the fine earth, the stones and
# boulders by weight set aside. Coarse fragments not recorded are absent
# (F8); fragments recorded by volume only leave the weight open.
.ps_p0175 <- function(s01, gw, cw, sw, bw, frag) {
  if (any(is.na(s01))) return(c(NA_real_, NA_real_))
  wts <- c(gw, cw)
  S <- sum(c(sw, bw), na.rm = TRUE)
  if (all(is.na(wts))) {
    G <- if (frag > 0) c(0, 100 - S) else c(0, 0)
  } else {
    g <- sum(wts, na.rm = TRUE); G <- c(g, g)
  }
  f <- function(Gx, sx) {
    den <- 100 - S
    if (den <= 0) return(NA_real_)
    100 * (Gx + sx * max(100 - Gx - S, 0) / 100) / den
  }
  c(f(G[1], s01[1]), f(G[2], s01[2]))
}

# Per-layer table of the particle-size key, sorted by depth.
.ps_layers_usda <- function(pedon, ctx) {
  h0 <- pedon$horizons
  n <- nrow(h0)
  o <- order(h0$top_cm)
  h <- as.data.frame(h0)[o, , drop = FALSE]
  num <- function(c) .ps_num(h, c); txt <- function(c) .ps_txt(h, c)
  clay <- num("clay_pct"); sand <- num("sand_pct"); silt <- num("silt_pct")
  silt <- ifelse(is.na(silt) & !is.na(sand) & !is.na(clay), 100 - sand - clay, silt)
  vfs <- num("very_fine_sand_pct"); fs <- num("fine_sand_pct"); cs <- num("coarse_sand_pct")
  frag <- num("coarse_fragments_pct"); frag[is.na(frag)] <- 0   # F8
  art <- num("artefacts_pct"); acp <- as.logical(.usda_col(h, "artefacts_cohesive_persistent", NA))
  frag <- pmin(100, frag + ifelse(acp %in% TRUE & !is.na(art), art, 0))
  tcl <- vapply(as.character(.usda_col(h, "texture_class_usda", NA_character_)), function(x)
    if (is.na(x)) NA_character_ else as.character(.texture_class_reading(x)$class), "", USE.NAMES = FALSE)
  grade <- txt("sand_grade")
  w15d <- num("water_content_1500kpa"); w15u <- num("water_content_1500kpa_undried")
  oc <- num("oc_pct"); bd <- num("bulk_density_g_cm3")
  glass <- num("volcanic_glass_pct"); p002 <- num("particles_002_2mm_pct")
  pyro <- num("pyroclastic_fragments_pct"); teph <- num("tephra_wt_pct")
  pum <- num("pumice_fragments_share_pct"); ll <- num("liquid_limit_pct")
  g20 <- num("gypsum_lt20mm_pct"); caso4 <- num("caso4_pct"); gvis <- num("gypsum_visible_pct")
  gw <- num("gravel_wt_pct"); cw <- num("cobbles_wt_pct")
  sw <- num("stones_wt_pct"); bw <- num("boulders_wt_pct")
  d <- .usda_desig(h$designation %||% rep(NA_character_, n))
  orig <- paste(txt("layer_origin"), txt("rock_origin"))
  # andic soil properties per layer; a volcanic record anywhere in the
  # profile (or a volcanic taxon) keeps an undecided one undecided, else an
  # undecided andic status or glass count is read as absent.
  ast <- tryCatch(andic_soil_properties_usda(pedon)$evidence$layer_status,
                  error = function(e) rep(NA, n))
  if (length(ast) != n) ast <- rep(NA, n)
  ast <- ast[o]
  volcanic <- ctx$volcanic_taxon || any(ast %in% TRUE) || any(glass > 0, na.rm = TRUE) ||
    any(pyro > 0, na.rm = TRUE) || any(teph > 0, na.rm = TRUE) ||
    any(grepl("volcan|tephra|pumice|cinder|ash|pyroclast", orig))
  ast[is.na(ast) & !volcanic] <- FALSE
  rows <- lapply(seq_len(n), function(i) {
    s01 <- .ps_sand01(sand[i], vfs[i], cs[i], fs[i])
    p <- .ps_p0175(s01, gw[i], cw[i], sw[i], bw[i], frag[i])
    sub <- .ps_substitute_layer(
      fe = 100 - frag[i], frag = frag[i], andic = ast[i], w15d = w15d[i], w15u = w15u[i],
      glass = glass[i], p002 = p002[i], pyro = pyro[i], teph = teph[i], pum = pum[i],
      volcanic = volcanic, limnic = (!is.na(d$master[i]) && d$master[i] == "L") ||
        grepl("limnic|diatom|coprogen|marl", orig[i]),
      clay = clay[i], bd = bd[i], ll = ll[i],
      g20 = g20[i], caso4 = caso4[i],
      gyps_hint = (!is.na(gvis[i]) && gvis[i] > 0) ||
        (!is.na(d$suffix[i]) && grepl("y", d$suffix[i])),
      s01 = s01)
    data.frame(row = o[i], top = h$top_cm[i], bottom = h$bottom_cm[i],
               bd = as.character(.usda_col(h, "boundary_distinctness", NA_character_))[i],
               clay = clay[i], silt = silt[i], sand = sand[i], vfs = vfs[i],
               frag = frag[i], s01_lo = s01[1], s01_hi = s01[2],
               p_lo = p[1], p_hi = p[2],
               sandy_tex = .ps_sandy_tex(clay[i], silt[i], sand[i], tcl[i]),
               grade_ok = .ps_grade_ok(sand[i], vfs[i], fs[i], cs[i], tcl[i], grade[i]),
               w15d = w15d[i], w15u = w15u[i], oc = oc[i],
               w15_mass = TRUE, gyps = sub$gyps,
               sub_state = sub$state, sub = sub$class,
               sub_missing = paste(sub$missing, collapse = ","),
               stringsAsFactors = FALSE)
  })
  L <- do.call(rbind, rows)
  if (is.null(L)) return(L)
  wm <- tryCatch(.w1500_by_mass(pedon), error = function(e) rep(TRUE, n))
  L$w15_mass <- wm[o]
  L
}

# Key A and B of the particle-size key for one layer (pp. 358-359): the
# substitute class (or fragmental is left to the weighted average), with
# state TRUE (a substitute), FALSE (none) or NA (undecided).
.ps_substitute_layer <- function(fe, frag, andic, w15d, w15u, glass, p002, pyro,
                                 teph, pum, volcanic, limnic, clay, bd, ll,
                                 g20, caso4, gyps_hint, s01) {
  res <- function(state, class = NA_character_, missing = character(0), gyps = NA_real_)
    list(state = state, class = class, missing = missing, gyps = gyps)
  # pumice share of the fragments >= 2 mm: two-thirds or more (by volume)
  pum_23 <- if (!is.na(pum)) pum >= 200 / 3
            else if (is.na(pyro) || pyro == 0) FALSE else NA
  if (fe < 10) {
    # A1 / A2: more than 60 % by weight volcanic ash, cinders, lapilli and
    # pumice in the whole soil
    teph60 <- if (!is.na(teph)) teph > 60
              else if ((is.na(pyro) || pyro == 0) && !volcanic) FALSE else NA
    if (isTRUE(teph60)) {
      if (is.na(pum_23)) return(res(NA, missing = "pumice_fragments_share_pct"))
      return(res(TRUE, if (pum_23) "pumiceous" else "cindery"))
    }
    if (is.na(teph60)) return(res(NA, missing = "tephra_wt_pct"))
    return(res(FALSE))   # A3, fragmental: left to the weighted average
  }
  skel <- function(base) {
    if (frag >= 35) {
      if (is.na(pum_23)) return(res(NA, missing = "pumice_fragments_share_pct"))
      return(res(TRUE, paste0(base, if (pum_23) "-pumiceous" else "-skeletal")))
    }
    res(TRUE, base)
  }
  # B1: ashy
  b1a <- .ps_and3(andic, if (is.na(w15u)) NA else w15u < 30, if (is.na(w15d)) NA else w15d < 12)
  glass_ok <- if (!is.na(glass)) glass >= 30 else if (!volcanic) FALSE else NA
  p_ok <- if (!is.na(p002)) p002 >= 30 else if (!volcanic) FALSE else NA
  b1b <- .ps_and3(!andic, p_ok, glass_ok)
  b1 <- .ps_or3(b1a, b1b)
  if (isTRUE(b1)) return(skel("ashy"))
  if (is.na(b1))
    return(res(NA, missing = c(if (is.na(andic)) "andic soil properties",
                               if (is.na(w15u)) "water_content_1500kpa_undried",
                               if (is.na(w15d)) "water_content_1500kpa",
                               if (is.na(glass)) "volcanic_glass_pct",
                               if (is.na(p002)) "particles_002_2mm_pct")))
  # B2 / B3: medial, hydrous
  if (isTRUE(andic)) {
    if (is.na(w15u)) return(res(NA, missing = "water_content_1500kpa_undried"))
    return(skel(if (w15u < 100) "medial" else "hydrous"))
  }
  if (is.na(andic)) return(res(NA, missing = "andic soil properties"))
  # B4: diatomaceous
  if (isTRUE(limnic)) {
    b4 <- .ps_and3(if (is.na(w15d) || is.na(clay) || clay <= 0) NA else w15d / clay > 0.6,
                   if (is.na(bd)) NA else bd < 0.80, if (is.na(ll)) NA else ll > 100)
    if (isTRUE(b4)) return(res(TRUE, "diatomaceous"))
    if (is.na(b4)) return(res(NA, missing = c("water_content_1500kpa", "bulk_density_g_cm3", "liquid_limit_pct")))
  }
  # B5: gypsum, 40 % or more by weight in the fraction < 20 mm
  gy <- if (!is.na(g20)) g20
        else if (!is.na(caso4) && (frag == 0 || !gyps_hint)) caso4
        else if (gyps_hint) NA_real_ else 0
  if (is.na(gy)) return(res(NA, missing = "gypsum_lt20mm_pct"))
  if (gy >= 40) {
    if (frag >= 35) return(res(TRUE, "gypseous-skeletal", gyps = gy))
    cg <- if (any(is.na(s01))) NA else if (s01[1] >= 50) TRUE else if (s01[2] < 50) FALSE else NA
    if (is.na(cg)) return(res(NA, missing = c("sand_pct", "very_fine_sand_pct"), gyps = gy))
    return(res(TRUE, if (cg) "coarse-gypseous" else "fine-gypseous", gyps = gy))
  }
  res(FALSE, gyps = gy)
}


# ---- the class of a material (key C, p. 360) -------------------------------

# Weighted material of a set of layers L (weights w, the thickness inside the
# window): clay (with the correction of p. 358 when `clay_fix`), sand, silt,
# fragments, 0.1-75 mm bounds, the sandy texture and the sand grade.
.ps_material <- function(L, w, clay_fix = FALSE) {
  av <- function(x) .wavg_clip_usda(NULL, NULL, x, 0, 0, w = w)$value
  clay <- L$clay
  if (isTRUE(clay_fix)) {
    est <- ifelse(!is.na(L$w15d) & L$w15_mass & !is.na(L$oc), pmax(2.5 * (L$w15d - L$oc), 0), NA)
    clay <- ifelse(is.na(est), clay, est)
  }
  m <- list(clay = av(clay), sand = av(L$sand), silt = av(L$silt),
            frag = av(L$frag), p_lo = av(L$p_lo), p_hi = av(L$p_hi),
            s01_lo = av(L$s01_lo), s01_hi = av(L$s01_hi), vfs = av(L$vfs))
  st <- .ps_sandy_tex(m$clay, m$silt, m$sand, NA_character_)
  if (is.na(st) && is.na(m$clay)) {
    lt <- L$sandy_tex[w > 0]
    st <- if (length(lt) && all(lt %in% TRUE)) TRUE else if (length(lt) && all(lt %in% FALSE)) FALSE else NA
  }
  m$sandy_tex <- st
  g <- L$grade_ok[w > 0]
  m$grade_ok <- if (!is.na(m$vfs)) m$vfs < 50
                else if (length(g) && all(g %in% TRUE)) TRUE
                else if (length(g) && all(g %in% FALSE)) FALSE else NA
  m
}

# Key C (p. 360) for a material m: the class, or NA with the missing columns.
# `generalized`: items 5 and 10 apply (shallow family, Lithic, Arenic or
# Grossarenic subgroup, or a part of a strongly contrasting class).
.ps_class_c <- function(m, ctx, generalized = FALSE) {
  r <- function(cl, miss = character(0), why = NULL)
    list(class = cl, missing = miss, reason = why)
  clayey <- if (is.na(m$clay)) NA else if (ctx$vertisol) m$clay > 30 else m$clay >= 35
  sandy <- .ps_and3(m$sandy_tex, m$grade_ok)
  sandy_miss <- c(if (is.na(m$sand)) "sand_pct",
                  if (is.na(m$clay)) "clay_pct",
                  if (isTRUE(m$sandy_tex) && is.na(m$grade_ok))
                    c("very_fine_sand_pct", "texture_class_usda"))
  if (!is.na(m$frag) && m$frag > 90) return(r("fragmental"))
  # 1-3: skeletal
  if (!is.na(m$frag) && m$frag >= 35) {
    if (isTRUE(sandy)) return(r("sandy-skeletal"))
    if (is.na(sandy)) return(r(NA_character_, sandy_miss, "sandy texture or sand grade undecided (F5, F7)"))
    if (is.na(m$clay)) return(r(NA_character_, "clay_pct", "clay undecided"))
    return(r(if (m$clay < 35) "loamy-skeletal" else "clayey-skeletal"))
  }
  # 4: sandy
  if (isTRUE(sandy)) return(r("sandy"))
  if (is.na(sandy)) return(r(NA_character_, sandy_miss, "sandy texture or sand grade undecided (F5, F7)"))
  if (is.na(clayey)) return(r(NA_character_, "clay_pct", "clay undecided"))
  # 5: loamy (generalized; Vertisols excluded)
  if (!clayey && generalized && !ctx$vertisol) return(r("loamy"))
  # 6-9: coarse-loamy, fine-loamy, coarse-silty, fine-silty
  low18 <- m$clay < 18
  if (!clayey && (low18 || !ctx$vertisol)) {
    loamy <- if (is.na(m$p_lo) || is.na(m$p_hi)) NA
             else if (m$p_lo >= 15) TRUE else if (m$p_hi < 15) FALSE else NA
    if (is.na(loamy))
      return(r(NA_character_, c(if (is.na(m$sand)) "sand_pct", "very_fine_sand_pct", "coarse_sand_pct",
                                if (!is.na(m$frag) && m$frag > 0) c("gravel_wt_pct", "cobbles_wt_pct")),
               "loamy versus silty undecided (F5, F6)"))
    return(r(paste0(if (low18) "coarse-" else "fine-", if (loamy) "loamy" else "silty")))
  }
  # 10: clayey (generalized)
  if (clayey && generalized) return(r("clayey"))
  # 11-12: fine, very-fine
  r(if (m$clay < 60) "fine" else "very-fine")
}

# The generalized class of a layer for the parts of a contrasting pair:
# sandy, sandy-skeletal, loamy, loamy-skeletal, clayey, clayey-skeletal,
# fragmental, or a substitute.
.ps_layer_part_class <- function(L, i, ctx) {
  if (isTRUE(L$sub_state[i])) return(list(class = L$sub[i], missing = character(0)))
  if (is.na(L$sub_state[i]))
    return(list(class = NA_character_, missing = strsplit(L$sub_missing[i], ",")[[1]]))
  m <- .ps_material(L[i, , drop = FALSE], 1)
  ctx2 <- ctx; ctx2$vertisol <- FALSE
  cl <- .ps_class_c(m, ctx2, generalized = TRUE)
  if (!is.na(cl$class) && ctx$vertisol && cl$class %in% c("loamy", "clayey")) {
    cl$class <- if (!is.na(m$clay) && m$clay > 30) "clayey" else "loamy"
  }
  if (!is.na(cl$class) && cl$class %in% c("fine", "very-fine")) cl$class <- "clayey"
  list(class = cl$class, missing = cl$missing)
}


# ---- generalized-class soils ------------------------------------------------

# Shallow family (p. 369: less than 50 cm to a root-limiting layer, 100 cm
# for Oxisols, not in a Lithic subgroup; families with a fragipan take no
# depth class) or a Lithic, Arenic or Grossarenic subgroup: items 5 and 10.
.ps_generalized_soil_usda <- function(pedon, ctx, rl = .root_limiting_usda(pedon)) {
  if (ctx$lithic || ctx$arenic || ctx$grossarenic)
    return(list(value = TRUE, why = "Lithic, Arenic or Grossarenic subgroup"))
  ms <- .usda_mineral_surface(pedon)
  lim <- if (ctx$oxisol) 100 else 50
  shallow <- is.finite(rl$top) && rl$top < ms + lim && !is.finite(rl$fragipan_top)
  list(value = shallow, why = if (shallow) "shallow family" else NULL)
}


# ---- strongly contrasting classes (pp. 361-362) -----------------------------

# Runs of adjacent layers with the same part class, within the mineral soil
# above the root-limiting layer.
.ps_runs <- function(L, ctx, start, rlt) {
  keep <- which(!is.na(L$top) & !is.na(L$bottom) & L$bottom > start & L$top < rlt)
  if (!length(keep)) return(list())
  pc <- lapply(keep, function(i) .ps_layer_part_class(L, i, ctx))
  cls <- vapply(pc, function(x) x$class, "")
  runs <- list()
  for (j in seq_along(keep)) {
    i <- keep[j]
    t <- max(L$top[i], start); b <- min(L$bottom[i], rlt)
    if (length(runs) && !is.na(cls[j]) && identical(runs[[length(runs)]]$class, cls[j])) {
      k <- length(runs)
      runs[[k]]$rows <- c(runs[[k]]$rows, i); runs[[k]]$bottom <- b
    } else {
      runs[[length(runs) + 1L]] <- list(class = cls[j], rows = i, top = t, bottom = b,
                                        missing = pc[[j]]$missing)
    }
  }
  runs
}

# Names a part may take in a pair (p. 357: clayey for every part with 35 %
# clay or more; loamy for a lower part, for a pair with a substitute, and in
# the generalized-class soils; the narrow loamy and silty classes otherwise).
# Where the narrow class is undecided, every class it may be is returned.
.ps_part_names <- function(run, L, lo, hi, ctx, position, partner_sub, gen_soil) {
  cl <- run$class
  if (is.na(cl)) return(NA_character_)
  if (cl != "loamy") return(cl)
  if (position == "lower" || partner_sub || isTRUE(gen_soil)) return("loamy")
  w <- .ps_run_w(run, L, lo, hi)
  m <- .ps_material(L[run$rows, , drop = FALSE], w)
  nc <- .ps_class_c(m, ctx, generalized = FALSE)$class
  if (is.na(nc)) {
    nc <- if (is.na(m$clay)) c("coarse-loamy", "coarse-silty", "fine-loamy", "fine-silty")
          else if (m$clay < 18) c("coarse-loamy", "coarse-silty")
          else if (ctx$vertisol) "clayey" else c("fine-loamy", "fine-silty")
  }
  ifelse(nc %in% c("fine", "very-fine"), "clayey", nc)
}

.ps_run_w <- function(run, L, lo, hi) {
  t <- L$top[run$rows]; b <- L$bottom[run$rows]
  w <- pmax(pmin(b, hi) - pmax(t, lo), 0)
  if (sum(w) <= 0) w <- pmax(pmin(b, run$bottom) - pmax(t, run$top), 0)
  w
}

# Condition attached to a pair; TRUE / FALSE / NA.
.ps_pair_cond <- function(cond, up, low, L, lo, hi, ctx) {
  if (cond == "none") return(TRUE)
  mu <- .ps_material(L[up$rows, , drop = FALSE], .ps_run_w(up, L, lo, hi), clay_fix = FALSE)
  ml <- .ps_material(L[low$rows, , drop = FALSE], .ps_run_w(low, L, lo, hi), clay_fix = FALSE)
  avg <- function(run, col) .wavg_clip_usda(NULL, NULL, L[[col]][run$rows], 0, 0,
                                            w = .ps_run_w(run, L, lo, hi))$value
  lt <- function(a, b) if (is.na(a) || is.na(b)) NA else a < b
  switch(cond,
    clay25 = if (is.na(mu$clay) || is.na(ml$clay)) NA else abs(mu$clay - ml$clay) >= 25,
    gyp15 = { a <- avg(up, "gyps"); b <- avg(low, "gyps")
              if (is.na(a) || is.na(b)) NA else abs(a - b) >= 15 },
    fe35 = (100 - mu$frag) - (100 - ml$frag) >= 35,
    fs50_up = if (is.na(mu$s01_hi)) NA else if (mu$s01_hi < 50) TRUE else if (mu$s01_lo >= 50) FALSE else NA,
    fs50_low = if (is.na(ml$s01_hi)) NA else if (ml$s01_hi < 50) TRUE else if (ml$s01_lo >= 50) FALSE else NA,
    ash_med = {
      a <- avg(if (grepl("^ashy", up$class)) up else low, "w15d")
      m <- avg(if (grepl("^medial", up$class)) up else low, "w15d")
      if (is.na(a) || is.na(m)) NA else a <= 10 && m >= 15 },
    med_hyd = { m <- avg(up, "w15u"); if (is.na(m)) NA else m <= 75 },
    NA)
}

# The contrasting pairs whose boundary lies inside (lo, hi): a list with
# passed (TRUE / FALSE / NA per candidate), name, strength and missing.
# window_mode "cs": both parts in the control section, a substitute part
# 10 cm or more inside it; "c1": the search of case C1.
.ps_contrasts_usda <- function(L, lo, hi, ctx, gen_soil = FALSE, window_mode = "cs",
                               start = NULL, rlt = Inf) {
  empty <- list(passed = logical(0), name = character(0), strength = numeric(0),
                missing = character(0), parts = list())
  if (is.null(L) || !nrow(L) || is.na(lo) || is.na(hi) || hi <= lo) return(empty)
  if (is.null(start)) start <- min(L$top, na.rm = TRUE)
  runs <- .ps_runs(L, ctx, start, rlt)
  nr <- length(runs)
  if (nr < 2L) return(empty)
  thk <- vapply(runs, function(r) r$bottom - r$top, 0)
  passed <- c(); nm <- c(); strength <- c(); miss <- c(); parts <- list()
  # v0.9.432: the two parts of each candidate, for the mineralogy of each
  # named part (KST 13th ed., p. 364)
  part_of <- function(up, low) list(
    upper = list(top = up$top, bottom = up$bottom, class = up$class),
    lower = list(top = low$top, bottom = low$bottom, class = low$class))
  P <- .PS_CONTRAST_PAIRS_USDA
  for (a in seq_len(nr - 1L)) {
    for (gap in 0:1) {
      b <- a + 1L + gap
      if (b > nr) next
      mid <- if (gap == 1L) runs[[a + 1L]] else NULL
      if (gap == 1L && thk[a + 1L] >= 12.5) next
      up <- runs[[a]]; low <- runs[[b]]
      # the boundary (or the transition zone) inside the window
      if (!(up$bottom > lo && low$top < hi)) next
      if (up$bottom <= lo || low$top >= hi) next
      if (thk[a] < 12.5 || thk[b] < 12.5) next
      if (!is.na(up$class) && !is.na(low$class) && identical(up$class, low$class)) next
      if (is.na(up$class) || is.na(low$class)) {
        passed <- c(passed, NA); nm <- c(nm, NA_character_); strength <- c(strength, NA)
        miss <- c(miss, up$missing, low$missing); parts <- c(parts, list(part_of(up, low)))
        next
      }
      ov_up <- min(up$bottom, hi) - max(up$top, lo)
      ov_low <- min(low$bottom, hi) - max(low$top, lo)
      if (window_mode == "cs") {
        if (up$class %in% .PS_SUBSTITUTES_USDA && ov_up < 10) next
        if (low$class %in% .PS_SUBSTITUTES_USDA && ov_low < 10) next
      }
      su <- up$class %in% .PS_SUBSTITUTES_USDA; sl <- low$class %in% .PS_SUBSTITUTES_USDA
      un <- .ps_part_names(up, L, lo, hi, ctx, "upper", sl, gen_soil)
      ln <- .ps_part_names(low, L, lo, hi, ctx, "lower", su, gen_soil)
      up_sets <- strsplit(P$upper, "|", fixed = TRUE)
      low_sets <- strsplit(P$lower, "|", fixed = TRUE)
      oks <- c(); nms <- c(); mm <- c()
      for (u1 in un) for (l1 in ln) {
        hit <- which(vapply(up_sets, function(s) u1 %in% s, NA) &
                     vapply(low_sets, function(s) l1 %in% s, NA))
        if (!length(hit)) { oks <- c(oks, FALSE); nms <- c(nms, NA_character_); next }
        pr <- P[hit[1L], ]
        cnd <- .ps_pair_cond(pr$cond, up, low, L, lo, hi, ctx)
        # transition zone less than 12.5 cm (reading F9)
        tz <- .ps_bd_width(L$bd[up$rows[length(up$rows)]])
        if (!is.null(mid))
          tz <- tz + .ps_bd_width(L$bd[mid$rows[length(mid$rows)]]) + (mid$bottom - mid$top)
        trans <- if (tz[2] < 12.5) TRUE else if (tz[1] >= 12.5) FALSE else NA
        ok1 <- .ps_and3(cnd, trans)
        if (is.na(ok1)) mm <- c(mm, if (is.na(trans)) "boundary_distinctness",
                                if (is.na(cnd)) switch(pr$cond, clay25 = "clay_pct",
                                  gyp15 = "gypsum_lt20mm_pct", fs50_up = "very_fine_sand_pct",
                                  fs50_low = "very_fine_sand_pct", ash_med = "water_content_1500kpa",
                                  med_hyd = "water_content_1500kpa_undried", character(0)))
        oks <- c(oks, ok1); nms <- c(nms, pr$name)
      }
      if (all(oks %in% FALSE)) next
      ok <- if (all(oks %in% TRUE) && length(unique(nms)) == 1L) TRUE else NA
      if (is.na(ok)) miss <- c(miss, mm, if (length(un) > 1L) c("sand_pct", "very_fine_sand_pct"))
      mu <- .ps_material(L[up$rows, , drop = FALSE], .ps_run_w(up, L, lo, hi))
      ml <- .ps_material(L[low$rows, , drop = FALSE], .ps_run_w(low, L, lo, hi))
      st <- if (!is.na(mu$clay) && !is.na(ml$clay)) abs(mu$clay - ml$clay)
            else abs((100 - mu$frag) - (100 - ml$frag))
      passed <- c(passed, ok); nm <- c(nm, if (isTRUE(ok)) nms[1L] else NA_character_)
      strength <- c(strength, st); parts <- c(parts, list(part_of(up, low)))
    }
  }
  list(passed = as.logical(passed), name = as.character(nm), strength = as.numeric(strength),
       missing = unique(miss), parts = parts)
}


# ---- the family class ---------------------------------------------------------

#' USDA family: particle-size class or substitute (KST 13th ed., Ch. 17)
#'
#' The control section of \code{family_control_section_usda()} (pp. 357-358)
#' and the key to the particle-size and substitute classes (pp. 358-360):
#' strongly contrasting classes from the closed list of pp. 361-362 ("X over
#' Y", with ", aniso" when more than one pair is present, named for the pair
#' with the largest difference in clay); otherwise the substitute of the
#' thickest part when one applies (p. 358); otherwise the class of the
#' weighted-average material of the control section. Not named for
#' Psamments, Psammaquents, Psammowassents, Psammoturbels, Psammorthels and
#' the Psammentic subgroups that are sandy (p. 356), nor for Histosols and
#' Histels outside the Terric subgroups.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family
#'   (order code, great group and subgroup names), or NULL.
#' @return A \code{\link{FamilyAttribute}} (\code{value} is the class or
#'   NULL).
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch.
#'   17, pp. 355-362.
#' @noRd
family_particle_size_usda <- function(pedon, order_code = NULL,
                                      great_group = NULL, subgroup = NULL) {
  ctx <- .ps_context_usda(order_code, great_group, subgroup)
  ref <- .PS_REF_USDA
  fa <- function(value, ev, missing = character(0))
    FamilyAttribute$new(name = "particle_size", value = value, evidence = ev,
                        missing = unique(missing), reference = ref)
  # Organic soils: particle-size classes only for Terric subgroups (p. 370)
  if (ctx$histosol || ctx$histel) {
    if (!ctx$terric) return(fa(NULL, list(reason = "not named in this taxon")))
    return(fa(NULL, list(reason = paste("the particle-size classes of Terric Histosols and",
                                         "Histels (KST 13th ed., p. 370) are not keyed in this version"))))
  }
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons"))
  L <- .ps_layers_usda(pedon, ctx)
  cs <- .ps_control_section_usda(pedon, ctx, layers = L)
  ev <- list(control_section = list(top = cs$top, bottom = cs$bottom, case = cs$case))
  if (is.na(cs$top) || is.na(cs$bottom))
    return(fa(NULL, c(ev, list(reason = paste("control section undecided:",
                                               cs$evidence$reason %||% ""))), cs$missing))
  lo <- cs$top; hi <- cs$bottom
  if (hi <= lo) return(fa(NULL, c(ev, list(reason = "control section has no thickness"))))
  rl <- .root_limiting_usda(pedon)
  gen <- .ps_generalized_soil_usda(pedon, ctx, rl)
  ev$generalized <- gen$why
  w <- pmax(pmin(L$bottom, hi) - pmax(L$top, lo), 0); w[is.na(w)] <- 0
  inside <- w > 0
  ev$layers_in_control_section <- L$row[inside]

  value <- NULL; missing <- character(0); reason <- NULL; sub_undecided <- FALSE
  # 1. strongly contrasting classes
  con <- .ps_contrasts_usda(L, lo, hi, ctx, gen_soil = gen$value, window_mode = "cs",
                            start = cs$evidence$start_cm %||% lo, rlt = rl$top)
  yes <- which(con$passed %in% TRUE); may <- which(is.na(con$passed))
  if (length(may)) {
    reason <- "strongly contrasting classes undecided"
    missing <- c(con$missing, "boundary_distinctness")
    ev$contrast_candidates <- con$name
  } else if (length(yes)) {
    k <- yes[which.max(ifelse(is.na(con$strength[yes]), -1, con$strength[yes]))]
    value <- con$name[k]
    if (length(yes) > 1L) value <- paste0(value, ", aniso")
    ev$contrasts <- con$name[yes]
    # v0.9.432: the named parts (read by the mineralogy classes, p. 364)
    ev$contrast_parts <- con$parts[[k]]
  } else {
    # 2. substitutes: the thickest part (cumulative)
    ss <- L$sub_state
    if (any(ss[inside] %in% TRUE) || any(is.na(ss[inside]))) {
      grp <- ifelse(ss %in% TRUE, L$sub, ifelse(is.na(ss), NA, "<particle-size>"))
      tg <- tapply(w[inside & !is.na(grp)], grp[inside & !is.na(grp)], sum)
      na_thk <- sum(w[inside & is.na(grp)])
      tg <- sort(tg, decreasing = TRUE)
      lead <- names(tg)[1L]
      second <- if (length(tg) > 1L) tg[2L] else 0
      if (na_thk > 0 && !(tg[1L] > second + na_thk)) {
        reason <- "substitute class undecided"
        missing <- unique(unlist(strsplit(L$sub_missing[inside & is.na(ss)], ",")))
        sub_undecided <- TRUE
      } else if (!identical(lead, "<particle-size>")) {
        value <- lead
        ev$thickest_part <- as.list(tg)
      } else {
        keep <- inside & ss %in% FALSE
        m <- .ps_material(L[keep, , drop = FALSE], w[keep], clay_fix = .ps_clay_fix(L[keep, , drop = FALSE], w[keep], ev))
        cl <- .ps_class_c(m, ctx, generalized = gen$value)
        value <- if (is.na(cl$class)) NULL else cl$class
        missing <- cl$missing; reason <- cl$reason
        ev$thickest_part <- as.list(tg)
      }
    } else {
      # 3. the weighted average of the control section
      fix <- .ps_clay_fix(L[inside, , drop = FALSE], w[inside], ev)
      m <- .ps_material(L[inside, , drop = FALSE], w[inside], clay_fix = fix)
      ev$weighted <- m[c("clay", "sand", "silt", "frag", "p_lo", "p_hi")]
      ev$clay_corrected <- fix
      cl <- .ps_class_c(m, ctx, generalized = gen$value)
      value <- if (is.na(cl$class)) NULL else cl$class
      missing <- cl$missing; reason <- cl$reason
    }
  }
  w15_note <- if (!any(!is.na(L$w15d[inside])))
    "water at 1500 kPa not measured: the measured clay is used (F11)" else NULL
  ev$note <- w15_note

  # Taxa with no particle-size class name (p. 356)
  ashy <- !is.null(value) && grepl("^ashy", value)
  # v0.9.432: the class the key reached, also where the taxon does not name
  # it (read by the mineralogy key to pick its section, p. 364)
  ev$class_reached <- value
  if (ctx$psamm_taxon && !ashy && !sub_undecided)
    return(fa(NULL, c(ev, list(reason = "not named in this taxon"))))
  if (ctx$psammentic && identical(value, "sandy"))
    return(fa(NULL, c(ev, list(reason = "not named in this taxon"))))
  if (is.null(value))
    return(fa(NULL, c(ev, list(reason = reason %||% "undecided")), missing))
  fa(value, ev)
}

# The clay of p. 358 estimated from the water at 1500 kPa (2.5 x (water -
# organic carbon)) when the ratio water / measured clay is 0.25 or less or
# 0.6 or more in half or more of the part. Water not measured: the measured
# clay (reading F11).
.ps_clay_fix <- function(L, w, ev = NULL) {
  if (!nrow(L)) return(FALSE)
  r <- ifelse(!is.na(L$w15d) & L$w15_mass & !is.na(L$clay) & L$clay > 0, L$w15d / L$clay, NA)
  bad <- !is.na(r) & (r <= 0.25 | r >= 0.6)
  sum(w[bad]) >= 0.5 * sum(w) && sum(w) > 0
}
