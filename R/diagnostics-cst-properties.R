# =============================================================================
# v0.9.372 -- the diagnostic characteristics of the Chinese Soil Taxonomy
# (Cooperative Research Group on Chinese Soil Taxonomy, ISSAS. Chinese Soil
# Taxonomy. Science Press, Beijing and New York, 2001; the English edition,
# system id cst2001), read on the recorded data.
#
# The book defines 25 diagnostic characteristics (pp. 43-64). The two
# regimes (moisture, temperature) are read by R/cst-regimes.R
# (moisture_regime_cst(), temperature_regime_cst()); the other 23 are read
# here, three-valued (TRUE / FALSE / NA), with the layers, the attributes
# that would decide an undecided answer, the fallbacks used (flags) and,
# where the book gives them, the weaker evidence grade (vertic, gleyic, alic,
# phosphic and sodic evidence, p. 19) or the kinds (the five organic soil
# materials, the seven lithologic characters).
#
# Two scopes:
#   * layer characteristics (organic soil materials, anthroturbic layer,
#     gleyic and redoxic features, permafrost layer, n value, andic,
#     allitic, alic, phosphic and sodic properties, sulfidic materials):
#     .cst_ch_<id>(x) gives each layer a grade on the context of the
#     horizons (R/diagnostics-cst-horizons.R), and <id>_cst(pedon, grade,
#     top_max) reads it as the horizons are read; they join the horizon
#     panel (.cst_identify());
#   * profile characteristics (lithologic characters, contacts,
#     anthro-silting materials, vertic and frost-thawic features, isohumic,
#     humic, ferric and calcaric properties, base saturation): one answer for
#     the profile, with the layers that carry it.
#
# Conventions as for the horizons (inst/rules/cst2001/readings.yaml): a field
# observation not recorded is absent, a measurement not recorded leaves its
# item undecided; a WRB or USDA diagnostic of soilKey stands in only where a
# CST descriptor is missing, flagged with a lower grade; the numbers are in
# thresholds.yaml (characteristics).
# =============================================================================

.CST_REF_CH <- "Chinese Soil Taxonomy (2001), 3.3 Diagnostic characteristics, pp. 43-64"


# --- the table of the 25 characteristics ----------------------------------------------

# id, the book's name, page, whether the book gives it an evidence grade, the
# scope (layer or profile) and the reader of the key.
.cst_characteristic_table <- function() {
  if (!is.null(.cst_cache$ch_table)) return(.cst_cache$ch_table)
  t <- data.frame(stringsAsFactors = FALSE,
    id = c("organic_soil_materials", "lithologic_characters", "lithic_contact", "paralithic_contact",
           "anthro_silting_materials", "vertic_features", "anthroturbic_layer", "moisture_regimes",
           "gleyic_features", "redoxic_features", "temperature_regimes", "permafrost_layer",
           "frost_thawic_features", "n_value", "isohumic_property", "humic_property",
           "andic_property", "ferric_property", "allitic_property", "alic_property",
           "phosphic_property", "sodic_property", "calcaric_property", "base_saturation",
           "sulfidic_materials"),
    name = c("Organic soil materials", "Lithologic characters", "Lithic contact", "Paralithic contact",
             "Anthro-silting materials", "Vertic features", "Anthroturbic layer", "Soil moisture regimes",
             "Gleyic features", "Redoxic features", "Soil temperature regimes", "Permafrost layer",
             "Frost-thawic features", "n value", "Isohumic property", "Humic property",
             "Andic property", "Ferric property", "Allitic property", "Alic property",
             "Phosphic property", "Sodic property", "Calcaric property", "Base saturation",
             "Sulfidic materials"),
    page = c("43-46", "46-48", "48", "48", "48-49", "49-50", "50-51", "51-55", "55-56", "56", "56-57",
             "57", "57", "58", "58-59", "59-60", "60-61", "61-62", "62", "62-63", "63", "63", "63",
             "64", "64"),
    evidence = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    scope = c("layer", "profile", "profile", "profile", "profile", "profile", "layer", "profile",
              "layer", "layer", "profile", "layer", "profile", "layer", "profile", "profile",
              "layer", "profile", "layer", "layer", "layer", "layer", "profile", "profile", "layer"))
  t$fun <- paste0(t$id, "_cst")
  t$fun[t$id == "moisture_regimes"] <- "moisture_regime_cst"
  t$fun[t$id == "temperature_regimes"] <- "temperature_regime_cst"
  .cst_cache$ch_table <- t
  t
}

# A number of thresholds.yaml, characteristics section.
.cst_cthr <- function(...) .cst_thr("characteristics", ...)

# Run a layer characteristic once per context.
.cst_ch <- function(x, id) {
  key <- paste0("ch:", id)
  if (!is.null(x$memo[[key]])) return(x$memo[[key]])
  fn <- get(paste0(".cst_ch_", id), envir = asNamespace("soilKey"), mode = "function")
  r <- if (!x$n) .cst_hz_ans(x, logical(0)) else fn(x)
  if (is.null(r$prov)) r$prov <- rep(FALSE, x$n)
  if (is.null(r$lower)) r$lower <- rep(FALSE, x$n)
  x$memo[[key]] <- r
  r
}

# The DiagnosticResult of a layer characteristic, read as a horizon is
# (.cst_hz_result()); a result resting on a provisional layer is marked
# provisional.
.cst_ch_result <- function(pedon, id, grade = c("full", "evidence", "at_least_evidence"),
                           top_max = NULL, datum = c("surface", "mineral"), r = NULL) {
  grade <- match.arg(grade); datum <- match.arg(datum)
  x <- .cst_ctx(pedon)
  r <- r %||% .cst_ch(x, id)
  res <- .cst_hz_result(pedon, id, grade = grade, top_max = top_max, datum = datum, x = x, r = r)
  pos <- match(res$layers, x$idx)
  # a lower grade without a stand-in function (a number from another
  # system, a field class): r$lower
  if (isTRUE(res$passed) && any(r$lower[pos] %in% TRUE)) res$evidence$lower_grade <- TRUE
  prov <- isTRUE(res$passed) && any(r$prov[pos] %in% TRUE)
  res$evidence$provisional <- prov
  if (prov) {
    res$evidence$flags <- unique(c(res$evidence$flags, "provisional"))
    res$notes <- paste(stats::na.omit(c(res$notes, "provisional")), collapse = "; ")
  }
  res
}

# The DiagnosticResult of a profile characteristic.
.cst_ch_prof <- function(id, passed, evid = NULL, kind = NA_character_, layers = integer(0),
                         missing = character(0), flags = character(0), notes = character(0),
                         grade_asked = "full", stand_in = character(0), provisional = FALSE,
                         extra = list()) {
  t <- .cst_characteristic_table()
  row <- t[t$id == id, , drop = FALSE]
  Fp <- passed
  Ep <- if (is.null(evid)) Fp else .cst_or(Fp, evid)
  g <- if (isTRUE(Fp)) "full" else if (isFALSE(Fp) && isTRUE(Ep)) "evidence"
       else if (isFALSE(Fp) && isFALSE(Ep)) "none" else NA_character_
  out <- switch(grade_asked,
    full = Fp,
    at_least_evidence = Ep,
    evidence = if (isTRUE(Fp)) FALSE else if (isFALSE(Ep)) FALSE
               else if (isFALSE(Fp) && isTRUE(Ep)) TRUE else NA)
  flags <- unique(as.character(stats::na.omit(flags)))
  prov <- isTRUE(out) && isTRUE(provisional)
  if (prov) flags <- unique(c(flags, "provisional"))
  ev <- c(extra, list(grade = g, grade_asked = grade_asked,
                      kind = if (length(kind) && !all(is.na(kind))) kind else NA_character_,
                      stand_in = stand_in, lower_grade = length(stand_in) > 0L,
                      provisional = prov, flags = flags,
                      has_evidence_grade = isTRUE(row$evidence[1L]), page = row$page[1L]))
  nt <- unique(as.character(stats::na.omit(c(notes, flags))))
  DiagnosticResult$new(
    name = paste0(id, "_cst"), passed = out, layers = as.integer(layers), evidence = ev,
    missing = if (is.na(out)) unique(as.character(missing)) else character(0),
    reference = sprintf("Chinese Soil Taxonomy (2001), %s, p. %s", row$name[1L], row$page[1L]),
    notes = if (length(nt)) paste(nt, collapse = "; ") else NA_character_)
}

# Frozen designations (Cf, Cff, a frozen suffix), read after the numeral of a
# lithological discontinuity is taken off (2Cf, IICff).
.cst_frozen <- function(pedon, d) {
  d <- sub("^([0-9]+|[IVX]+)(?=[A-Z])", "", trimws(as.character(d)), perl = TRUE)
  !is.na(d) & grepl(.frozen_designation_pattern(pedon$site), d)
}

# Layers within `depth` cm of the soil surface (their top).
.cst_within <- function(x, depth) !is.na(x$top) & !is.na(x$surf) & x$top < x$surf + depth

# Three-valued "some layer" and "every layer" of a per-layer test over rows.
.cst_any_rows <- function(v, rows) if (!length(rows)) FALSE else .cst_or(v[rows])
.cst_all_rows <- function(v, rows) if (!length(rows)) NA else .cst_and(v[rows])

# The B horizon of the profile (pp. 59-62; the "whole B"), per layer TRUE /
# FALSE / NA: the layers designated B or BC whose top lies within 125 cm; with
# no designation recorded, the argic, LAC-ferric, cambic or ferralic horizon
# and the layers below it within 125 cm, flagged b_from_diagnostics (cn_plan
# 6, K2 N19).
.cst_b_layers <- function(x) {
  lim <- .cst_cthr("humic", "b_within_cm")
  within <- .cst_within(x, lim)
  if (any(!is.na(x$kind))) {
    b <- ifelse(is.na(x$kind), NA, x$kind %in% c("B", "BC"))
    return(list(b = b & within, flag = character(0)))
  }
  hz <- c("argic_horizon", "lac_ferric_horizon", "cambic_horizon", "ferralic_horizon")
  full <- Reduce(`|`, lapply(hz, function(h) .cst_hz(x, h)$full))
  k <- which(full %in% TRUE)
  if (length(k)) {
    b <- seq_len(x$n) >= k[1L]
    return(list(b = b & within, flag = "b_from_diagnostics"))
  }
  if (any(is.na(full))) return(list(b = rep(NA, x$n) & within, flag = "b_from_diagnostics"))
  list(b = rep(FALSE, x$n), flag = "b_from_diagnostics")
}


# --- 1. organic soil materials (pp. 43-46) ----------------------------------------------

# The value and chroma of a pyrophosphate colour ("7/2", "10YR 7/2").
.cst_pyro_colour <- function(v) {
  v <- as.character(v)
  m <- regmatches(v, regexec("([0-9.]+)\\s*/\\s*([0-9.]+)\\s*$", v))
  val <- vapply(m, function(z) if (length(z) == 3L) as.numeric(z[2L]) else NA_real_, 0)
  chr <- vapply(m, function(z) if (length(z) == 3L) as.numeric(z[3L]) else NA_real_, 0)
  list(value = val, chroma = chr,
       text = ifelse(is.na(val), NA_character_, sprintf("%g/%g", val, chr)))
}

# A von Post index (H1-H10, or the number).
.cst_von_post <- function(v) suppressWarnings(as.numeric(sub("^[Hh]\\s*", "", as.character(v))))

.cst_ch_organic_soil_materials <- function(x) {
  t <- .cst_cthr("organic")
  th <- .cst_thr("horizons", "histic")
  oc <- .cst_v(x, "oc_pct"); clay <- .cst_v(x, "clay_pct")
  # the carbon of the organic epipedon: peat and mud (saturated a month or
  # more) by the clay formula, litter 200 g/kg (pp. 21-22)
  peat_oc <- .cst_oc_organic(oc, clay, th$peaty_oc_base_pct, th$peaty_oc_clay_slope, th$peaty_oc_cap_pct)
  litter_oc <- oc >= th$littery_oc_min_pct
  sat <- .cst_v(x, "water_saturation_days")
  sat[is.na(sat)] <- .cst_sat_days(x)
  drained <- isTRUE(.cst_site_lgl(x$pedon$site, "artificially_drained"))
  satd <- if (drained) rep(TRUE, x$n) else sat >= th$saturated_days_min
  org <- litter_oc | (peat_oc & satd)
  # mattic soil materials: woven roots 10 % in the mattic epipedon (its
  # evidence or the full one; the epipedon has no carbon limit, p. 22)
  roots <- .cst_v(x, "root_mat_pct")
  mattic <- .cst_hz(x, "mattic_epipedon")$evid %in% TRUE & roots >= t$mattic_roots_min_pct
  mattic[is.na(mattic)] <- FALSE
  org[mattic] <- TRUE
  # the kinds
  fib <- .cst_v(x, "fiber_content_rubbed_pct")
  py <- .cst_pyro_colour(.cst_col(x$h, "pyrophosphate_color"))
  light <- !is.na(py$text) & py$text %in% t$fibric_colours
  vp <- .cst_von_post(.cst_col(x$h, "von_post_index"))
  by_fibre <- ifelse(fib >= t$fibric_fibre_min_pct | (fib >= t$fibric_colour_fibre_min_pct & light), "fibric",
                ifelse(fib < t$sapric_fibre_max_pct, "sapric", "hemic"))
  by_vp <- ifelse(is.na(vp), NA_character_,
                  ifelse(vp <= t$von_post_fibric_max, "fibric", ifelse(vp <= t$von_post_hemic_max, "hemic", "sapric")))
  use_vp <- is.na(by_fibre) & !is.na(by_vp)
  kind <- ifelse(is.na(by_fibre), by_vp, by_fibre)
  folic <- .cst_v(x, "plant_residues_pct") >= t$folic_litter_min_pct
  kind <- ifelse(folic %in% TRUE, "folic", kind)
  kind <- ifelse(mattic, "mattic", kind)
  kind[org %in% FALSE] <- NA_character_
  lflags <- lapply(seq_len(x$n), function(i) {
    c(if (use_vp[i] && !(folic[i] %in% TRUE) && !mattic[i]) "kind_from_von_post",
      if (!is.na(kind[i]) && kind[i] == "sapric" && !is.na(py$value[i]) && py$value[i] - py$chroma[i] >= 4)
        "possibly_limnic_material")
  })
  flags <- unlist(lflags[org %in% TRUE])
  rows <- which(is.na(org))
  .cst_hz_ans(x, org, kind = kind, flags = flags, lflags = lflags,
              notes = c("peat and mud by the carbon of the organic epipedon when saturated a month or more, litter by 200 g/kg",
                        if (any(use_vp & org %in% TRUE)) "the von Post index stands in for the rubbed fibre content, flagged"),
              missing = c(.cst_missing_in(x, rows, c("oc_pct", "clay_pct", "water_saturation_days")),
                          if (any(org %in% TRUE & is.na(kind))) c("fiber_content_rubbed_pct", "von_post_index")))
}

#' Organic soil materials of the Chinese Soil Taxonomy (2001)
#'
#' The layers of organic soil materials (pp. 43-45): peat and mud saturated a
#' month or more with the carbon of the organic epipedon, litter with 200
#' g/kg, and the mattic materials of the mattic epipedon; `kind` one of
#' fibric, hemic, sapric, folic or mattic, or several (any of them).
#' @noRd
organic_soil_materials_cst <- function(pedon, kind = NULL, top_max = NULL,
                                       datum = c("surface", "mineral")) {
  datum <- match.arg(datum)
  x <- .cst_ctx(pedon)
  r <- .cst_ch(x, "organic_soil_materials")
  if (!is.null(kind) && x$n) {
    k <- r$kind
    r$full <- ifelse(r$full %in% FALSE, FALSE,
                ifelse(!is.na(k), r$full & k %in% kind, NA))
    r$evid <- r$full
  }
  res <- .cst_ch_result(pedon, "organic_soil_materials", top_max = top_max, datum = datum, r = r)
  res$evidence$kind_asked <- kind %||% NA_character_
  res
}

#' The control section of the Histosols and its tiers (pp. 45-46): 130 cm,
#' 160 cm when the upper 60 cm hold three-fourths moss fibres or a bulk
#' density under 0.1; ended by a lithic or paralithic contact or 25 cm below
#' the top of permafrost. Tiers: surface 0-30 (0-60 with the moss), subsurface
#' 60 cm, bottom 40 cm, each cut at the end of the control section; with the
#' kind of organic soil material that is thickest in each (NA while a layer
#' of it is undecided).
#' @noRd
.cst_histosol_tiers <- function(pedon) {
  t <- .cst_cthr("organic")
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_ans(NA, missing = c("top_cm", "bottom_cm")))
  s <- x$surf
  sph <- .cst_v(x, "sphagnum_fibres_pct"); bd <- .cst_v(x, "bulk_density_g_cm3")
  moss <- sph >= t$moss_min_pct | bd < t$bd_low_max
  wide <- .cst_window_every(pedon, moss, s, s + t$moss_window_cm)$value
  flags <- character(0)
  ctl <- if (isTRUE(wide)) t$control_moss_cm else if (isFALSE(wide)) t$control_cm else NA_real_
  ct <- .cst_contact_cm(pedon)
  pf <- which((.cst_v(x, "permafrost_temp_C") <= 0) %in% TRUE | .cst_frozen(pedon, .cst_col(x$h, "designation")))
  ends <- c(if (!is.na(ct)) ct - s, if (length(pf)) x$top[pf[1L]] - s + t$permafrost_below_cm)
  if (length(ends)) {
    e <- min(ends)
    if (!is.na(ctl) && e < ctl) ctl <- e
    if (is.na(ctl) && e <= t$control_cm) ctl <- e
  }
  if (is.na(ctl)) return(.cst_ans(NA, missing = c("sphagnum_fibres_pct", "bulk_density_g_cm3")))
  # the 60 cm surface tier (p. 46): fibric materials of three-fourths moss
  # fibres in every layer of 0-60 cm, or a bulk density under 0.1; a hemic or
  # sapric layer there gives the 30 cm tier
  r <- .cst_ch(x, "organic_soil_materials")
  fib_moss <- ifelse(is.na(r$kind), NA, r$kind == "fibric") & sph >= t$moss_min_pct
  tier60 <- .cst_window_every(pedon, fib_moss | bd < t$bd_low_max, s, s + t$moss_window_cm)$value
  if (is.na(tier60)) return(.cst_ans(NA, missing = c("fiber_content_rubbed_pct", "sphagnum_fibres_pct", "bulk_density_g_cm3")))
  st <- if (tier60) t$surface_tier_moss_cm else t$surface_tier_cm
  cut <- function(a, b) c(top = min(a, ctl), bottom = min(b, ctl))
  tiers <- list(surface = cut(0, st), subsurface = cut(st, st + t$subsurface_tier_cm),
                bottom = cut(st + t$subsurface_tier_cm, ctl))
  dominant <- vapply(tiers, function(z) {
    a <- s + z[["top"]]; b <- s + z[["bottom"]]
    if (b <= a) return(NA_character_)
    part <- pmax(0, pmin(x$bot, b) - pmax(x$top, a))
    rows <- which(part > 0 & !(r$full %in% FALSE))
    if (!length(rows) || any(is.na(r$kind[rows]))) return(NA_character_)
    th <- tapply(part[rows], r$kind[rows], sum)
    names(th)[which.max(th)]
  }, "")
  .cst_ans(ctl, tiers = tiers, dominant = dominant, flags = flags,
           note = sprintf("control section %s cm", format(ctl)))
}


# --- 2. lithologic characters (pp. 46-48) ------------------------------------------------

# One kind's answer.
.cst_lc_ans <- function(value, rows = integer(0), flags = character(0), missing = character(0),
                        note = character(0), provisional = FALSE, sub = NA_character_) {
  list(value = value, rows = rows, flags = flags, missing = if (is.na(value)) missing else character(0),
       note = note, provisional = isTRUE(value) && provisional, sub = sub)
}

# The carbonate or coral words of the site's parent material.
.cst_pm_has <- function(pedon, pat) {
  pm <- .cst_site_txt(pedon$site, "parent_material")
  if (is.na(pm)) NA else grepl(pat, pm)
}

.cst_lc_alluvial <- function(x) {
  t <- .cst_cthr("lithologic")
  s <- x$surf
  # a. distinct stratification within 50 cm: stratification_pattern, or a
  # lithological discontinuity (a numeral prefix) between layers there
  sp <- .cst_t(x, "stratification_pattern")
  in50 <- .cst_within(x, t$alluvial_stratification_within_cm)
  strat <- in50 & !is.na(sp) & sp != "none"
  d <- trimws(as.character(.cst_col(x$h, "designation")))
  disc <- in50 & grepl("^([2-9]|I[IVX]*)[A-Z]", d)
  flags <- character(0)
  a <- any(strat) || any(disc)
  if (!any(strat) && any(disc)) flags <- c(flags, "stratification_from_discontinuity")
  stand <- character(0)
  if (!a) {
    fm <- tryCatch(fluvic_material(x$pedon), error = function(e) NULL)
    if (!is.null(fm) && isTRUE(fm$passed)) { a <- TRUE; stand <- "fluvic_material" }
  }
  # b. OC 2 g/kg at 125 cm, or decreasing irregularly from 25 to 125 cm (or
  # a contact)
  oc <- .cst_v(x, "oc_pct")
  ct <- .cst_contact_cm(x$pedon)
  d125 <- s + t$alluvial_oc_at_depth_cm
  at <- which(!is.na(x$top) & !is.na(x$bot) & x$top <= d125 & x$bot > d125 & !(x$kind %in% "R"))
  b1 <- if (length(at)) oc[at[1L]] >= t$alluvial_oc_min_pct
        else if (!is.na(ct) && ct <= d125) FALSE else NA
  lo <- if (!is.na(ct) && ct < d125) ct else d125
  rows <- which(!is.na(x$top) & x$bot > s + t$alluvial_irregular_from_cm & x$top < lo & !(x$kind %in% "R"))
  v <- oc[rows]
  inc <- if (length(v) > 1L) diff(v) > 0 else logical(0)
  b2 <- if (any(inc %in% TRUE)) TRUE else if (length(v) > 1L && !anyNA(v)) FALSE else NA
  b <- .cst_or(b1, b2)
  # still flooded at regular intervals (default 20): frequent or annual
  # floods confirm; none or rare fail; occasional undecided; not recorded,
  # provisional
  ff <- .cst_site_txt(x$pedon$site, "flood_frequency")
  fl <- if (is.na(ff)) TRUE else if (ff %in% c("frequent", "annual")) TRUE
        else if (ff %in% c("none", "rare")) FALSE else NA
  prov <- is.na(ff)
  val <- .cst_and(a, b, fl)
  .cst_lc_ans(val, rows = which(strat | disc | seq_len(x$n) %in% rows),
              flags = c(flags, if (length(stand)) sprintf("stand_in:%s", stand), if (prov) "flooding_not_recorded"),
              missing = c(if (is.na(b)) c("oc_pct", "bottom_cm"), if (is.na(fl)) "flood_frequency"),
              provisional = prov)
}

# Structure of the sandy character: single grain (or structureless), or
# blocky and very friable or loose moist.
.cst_single_grain <- function(x) {
  ty <- .cst_t(x, "structure_type"); gr <- .cst_t(x, "structure_grade")
  cons <- .cst_t(x, "consistence_moist")
  sg <- (!is.na(ty) & grepl("single", ty)) | (is.na(ty) & !is.na(gr) & gr == "structureless")
  bl <- !is.na(ty) & grepl("blocky", ty) & !is.na(cons) & cons %in% c("very friable", "loose")
  ifelse(sg | bl, TRUE, ifelse(is.na(ty) & is.na(gr), NA, FALSE))
}

.cst_lc_sandy <- function(x) {
  t <- .cst_cthr("lithologic")
  s <- x$surf
  ct <- .cst_contact_cm(x$pedon)
  bot <- s + t$sandy_window_cm
  if (!is.na(ct) && ct < bot) bot <- ct
  tex <- .cst_texture_class(x$h)
  sp <- .cst_t(x, "stratification_pattern")
  p <- (tex %in% t$sandy_classes | ifelse(is.na(tex), NA, FALSE)) & .cst_single_grain(x) &
    !(!is.na(sp) & sp != "none")
  p[x$kind %in% "R"] <- TRUE
  w <- .cst_window_every(x$pedon, p, s, bot)
  # the carbon as the weighted mean of the window (Hugo, 2026-11-06)
  oc <- .cst_weighted(x$pedon, "oc_pct", s, bot)$value <= t$sandy_oc_max_pct
  .cst_lc_ans(.cst_and(w$value, oc), rows = w$rows,
              missing = .cst_missing_in(x, w$rows, c("sand_pct", "clay_pct", "structure_type", "oc_pct")))
}

.cst_lc_loess <- function(x) {
  t <- .cst_cthr("lithologic")
  col <- .cst_colours(x)
  silt <- .cst_v(x, "silt_pct"); fs <- .cst_v(x, "fine_sand_pct")
  sf <- ifelse(is.na(fs), ifelse(silt > t$loess_silt_fine_sand_min_pct, TRUE, NA),
               silt + fs > t$loess_silt_fine_sand_min_pct)
  p <- .cst_hue_ge(col, t$loess_hue_rank_min) & !col$neutral & col$vd >= t$loess_value_dry_min &
    col$cd >= t$loess_chroma_dry_min & .cst_v(x, "caco3_pct") >= t$loess_caco3_min_pct & sf &
    .cst_within(x, t$within_cm)
  # b. homogeneous: neighbouring texture classes with the loess layers
  # above and below
  tex <- .cst_texture_class(x$h)
  nb <- function(a, b) ifelse(is.na(a) | is.na(b), NA, a == b | mapply(function(u, v) v %in% .CST_TEXTURE_NEIGHBOURS[[u]], a, b))
  up <- .cst_shift(tex, -1L); dn <- .cst_shift(tex, 1L)
  pu <- .cst_shift(p, -1L); pd <- .cst_shift(p, 1L)
  hom <- ifelse(pu %in% TRUE, nb(tex, up), TRUE) & ifelse(pd %in% TRUE, nb(tex, dn), TRUE)
  p <- p & hom
  p[x$kind %in% "R"] <- FALSE   # rock carries no soil colour (D1)
  rows <- which(p %in% TRUE)
  .cst_lc_ans(.cst_any_rows(p, seq_len(x$n)), rows = rows,
              flags = c(col$hue_flag[rows], if (any(is.na(fs[rows]))) "silt_without_fine_sand"),
              missing = .cst_missing_in(x, which(is.na(p)), c("munsell_hue_moist", "munsell_value_dry",
                                                              "munsell_chroma_dry", "caco3_pct", "silt_pct", "fine_sand_pct")))
}

# Value and chroma, moist, the dry ones where no moist one is recorded,
# flagged (as Q21 reads the hue).
.cst_vc_moist_first <- function(x) {
  col <- .cst_colours(x)
  v <- ifelse(is.na(col$vm), col$vd, col$vm); c <- ifelse(is.na(col$cm), col$cd, col$cm)
  list(col = col, v = v, c = c,
       flag = ifelse((is.na(col$vm) & !is.na(col$vd)) | (is.na(col$cm) & !is.na(col$cd)) | !is.na(col$hue_flag),
                     "colour_dry_for_moist", NA_character_))
}

.cst_lc_purplish <- function(x) {
  t <- .cst_cthr("lithologic")
  col <- .cst_colours(x)
  hue <- .cst_hue_in(col, t$purplish_hue_rank[1L], t$purplish_hue_rank[2L])
  mx <- .cst_v(x, "rock_fragment_max_cm"); cf <- .cst_v(x, "coarse_fragments_pct")
  fr <- ifelse(!is.na(mx), mx < t$purplish_fragment_max_cm, ifelse(!is.na(cf) & cf == 0, TRUE, NA))
  # the purplish layers within 125 cm, and the fragments under 4 cm in every
  # one of them (Hugo, 2026-11-06)
  pl <- hue & .cst_within(x, t$within_cm)
  pl[x$kind %in% "R"] <- FALSE   # rock carries no soil colour (D1)
  rows <- which(pl %in% TRUE)
  val <- .cst_and(.cst_any_rows(pl, seq_len(x$n)), if (length(rows)) .cst_and(fr[rows]) else TRUE)
  .cst_lc_ans(val, rows = rows,
              flags = c(col$hue_flag[rows], if (any(is.na(mx[rows]))) "no_coarse_fragments_read_as_small"),
              missing = .cst_missing_in(x, which(is.na(pl) | (pl %in% TRUE & is.na(fr))),
                                        c("munsell_hue_moist", "rock_fragment_max_cm")))
}

.cst_lc_red_beds <- function(x) {
  t <- .cst_cthr("lithologic")
  m <- .cst_vc_moist_first(x)
  col <- m$col
  a1 <- .cst_hue_in(col, t$red_hue_rank_a[1L], t$red_hue_rank_a[2L]) & m$v >= t$red_value_a[1L] &
    m$v <= t$red_value_a[2L] & m$c >= t$red_chroma_a[1L] & m$c <= t$red_chroma_a[2L]
  a2 <- .cst_hue_in(col, t$red_hue_rank_b[1L], t$red_hue_rank_b[2L]) & m$v >= t$red_value_b[1L] &
    m$v <= t$red_value_b[2L] & m$c >= t$red_chroma_b_min
  p <- (a1 | a2) & .cst_within(x, t$within_cm)
  p[x$kind %in% "R"] <- FALSE   # rock carries no soil colour (D1)
  # northern red earths (Hugo, 2026-11-06): a parent material recorded as red
  # earth or red clay, with (b) calcareous, carbonate nodules, base saturated
  # or salic evidence in a layer within 125 cm; no hue limit
  eff <- .cst_t(x, "effervescence_class")
  nr <- (.cst_v(x, "caco3_pct") >= t$northern_calcareous_min_pct) %in% TRUE |
    (!is.na(eff) & eff != "none") | (.cst_v(x, "carbonate_hard_pct") > 0) %in% TRUE |
    (.cst_t(x, "secondary_carbonates_form") %in% c("soft concretions", "hard concretions")) |
    (.cst_v(x, "bs_pct") >= t$northern_base_saturated_min_pct) %in% TRUE |
    .cst_hz(x, "salic_horizon")$evid %in% TRUE
  red_pm <- isTRUE(.cst_pm_has(x$pedon, "red earth|red clay"))
  ne <- red_pm & nr & .cst_within(x, t$within_cm) & !(x$kind %in% "R")
  a_val <- .cst_any_rows(p, seq_len(x$n))
  rows <- which(p %in% TRUE | ne)
  sub <- if (any(ne)) "northern red earths" else if (isTRUE(a_val)) "red beds" else NA_character_
  .cst_lc_ans(.cst_or(a_val, any(ne)), rows = rows, sub = sub,
              flags = unique(stats::na.omit(m$flag[rows])),
              missing = .cst_missing_in(x, which(is.na(p)), c("munsell_hue_moist", "munsell_value_moist",
                                                              "munsell_chroma_moist")))
}

.cst_lc_carbonate_rocks <- function(x) {
  t <- .cst_cthr("lithologic")
  # carbonate ROCK nouns only (D2): not a calcareous sediment, not karst
  carb <- .cst_pm_has(x$pedon, "limestone|dolomit|marl|chalk|travertine|tufa|carbonate rock")
  d <- .desg_master(as.character(.cst_col(x$h, "designation")))
  lith <- x$kind %in% "R" & !grepl("^C", d) & .cst_within(x, t$within_cm)
  cf <- .cst_v(x, "coarse_fragments_pct")
  frag <- (cf > 0) %in% TRUE & .cst_within(x, t$within_cm)
  # the rock not named (no parent material recorded): a contact or rock
  # fragments leave it undecided, nothing rock-like is absent (Hugo,
  # 2026-11-06)
  rocky <- any(lith) || any(frag)
  ab <- if (is.na(carb)) (if (rocky) NA else FALSE) else carb && rocky
  ct <- .cst_contact_cm(x$pedon)
  bot <- x$surf + t$within_cm
  if (!is.na(ct) && ct < bot) bot <- ct
  ok <- .cst_v(x, "bs_pct") >= t$carbonate_bs_min_pct & .cst_v(x, "ph_h2o") >= t$carbonate_ph_min
  ok[x$kind %in% "R"] <- TRUE
  w <- .cst_window_every(x$pedon, ok, x$surf, bot)
  val <- .cst_and(ab, w$value)
  .cst_lc_ans(val, rows = c(which(lith | frag), w$rows),
              note = if (is.na(carb)) "the parent material is not recorded: the rock is not named",
              flags = if (any(!is.na(.cst_v(x, "ph_h2o")))) "ph_ratio_substitute",
              missing = .cst_missing_in(x, w$rows, c("bs_pct", "ph_h2o")))
}

.cst_lc_coral_sands <- function(x) {
  t <- .cst_cthr("lithologic")
  coral <- .cst_pm_has(x$pedon, "coral|shell")
  shelly <- .cst_l(x, "shelly_layer") %in% TRUE
  src <- shelly | isTRUE(coral)
  sand <- .cst_v(x, "sand_pct")
  sand <- ifelse(is.na(sand), 100 - .cst_v(x, "silt_pct") - .cst_v(x, "clay_pct"), sand)
  p <- src & sand >= t$coral_sand_min_pct & .cst_v(x, "caco3_pct") >= t$coral_caco3_min_pct &
    .cst_within(x, t$within_cm)
  rows <- which(p %in% TRUE)
  .cst_lc_ans(.cst_any_rows(p, seq_len(x$n)), rows = rows,
              missing = .cst_missing_in(x, which(is.na(p)), c("sand_pct", "caco3_pct")))
}

.CST_LC_KINDS <- c("alluvial", "sandy", "loess", "purplish", "red_beds", "carbonate_rocks", "coral_sands")

#' Lithologic characters of the Chinese Soil Taxonomy (2001)
#'
#' The seven lithologic characters within 125 cm (pp. 46-48): alluvial
#' deposits, sandy deposits, loess, purplish sandstones and shales, red beds
#' and northern red earths, carbonate rocks, coral sands. `kind` restricts
#' the test to some of them (default all); `evidence$kind` names the kinds
#' met.
#' @noRd
lithologic_characters_cst <- function(pedon, kind = NULL) {
  kinds <- if (is.null(kind)) .CST_LC_KINDS else match.arg(kind, .CST_LC_KINDS, several.ok = TRUE)
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) {
    return(.cst_ch_prof("lithologic_characters", if (x$n) NA else FALSE, missing = c("top_cm", "bottom_cm")))
  }
  res <- lapply(kinds, function(k) {
    key <- paste0("lc:", k)
    if (is.null(x$memo[[key]])) x$memo[[key]] <- get(paste0(".cst_lc_", k), envir = asNamespace("soilKey"))(x)
    x$memo[[key]]
  })
  names(res) <- kinds
  vals <- vapply(res, function(r) as.logical(r$value), NA)
  passed <- .cst_or(vals)
  met <- kinds[vals %in% TRUE]
  rows <- unique(unlist(lapply(res[met], `[[`, "rows")))
  sub <- unname(unlist(lapply(res[met], `[[`, "sub"))); sub <- sub[!is.na(sub)]
  .cst_ch_prof("lithologic_characters", passed,
               kind = if (length(met)) met else NA_character_,
               layers = x$idx[sort(rows)],
               flags = unlist(lapply(res[if (length(met)) met else kinds], `[[`, "flags")),
               notes = unlist(lapply(res, `[[`, "note")),
               missing = unlist(lapply(res[is.na(vals)], `[[`, "missing")),
               provisional = isTRUE(passed) && all(vapply(res[met], `[[`, NA, "provisional")),
               extra = list(kinds = vals, red_beds_kind = if (length(sub)) sub else NA_character_,
                            kinds_asked = kinds))
}

lc_alluvial_cst        <- function(pedon) lithologic_characters_cst(pedon, "alluvial")
lc_sandy_cst           <- function(pedon) lithologic_characters_cst(pedon, "sandy")
lc_loess_cst           <- function(pedon) lithologic_characters_cst(pedon, "loess")
lc_purplish_cst        <- function(pedon) lithologic_characters_cst(pedon, "purplish")
lc_red_beds_cst        <- function(pedon) lithologic_characters_cst(pedon, "red_beds")
lc_carbonate_rocks_cst <- function(pedon) lithologic_characters_cst(pedon, "carbonate_rocks")
lc_coral_sands_cst     <- function(pedon) lithologic_characters_cst(pedon, "coral_sands")


# --- 3-4. lithic and paralithic contacts (p. 48) ---------------------------------------------

# The contact by its designation: R (lithic), Cr read as a paralithic contact
# (.cst_cr_contact()). With no designation recorded, a description that ends
# above the depth asked leaves it undecided; otherwise not recorded, absent.
.cst_contact_result <- function(pedon, id, top_max) {
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_ch_prof(id, FALSE))
  d <- .desg_master(as.character(.cst_col(x$h, "designation")))
  cr <- grepl("^C", d)
  hit <- x$kind %in% "R" & (if (id == "lithic_contact") !cr else cr)
  # the upper boundary at the depth asked counts (as top_max of the horizons)
  if (!is.null(top_max)) hit <- hit & !is.na(x$top) & x$top - x$surf <= top_max
  if (any(hit)) {
    return(.cst_ch_prof(id, TRUE, layers = x$idx[which(hit)],
                        extra = list(contact_cm = x$top[which(hit)[1L]] - x$surf)))
  }
  lim <- if (is.null(top_max)) Inf else x$surf + top_max
  if (all(is.na(x$kind)) && max(x$bot, na.rm = TRUE) < lim) {
    return(.cst_ch_prof(id, NA, missing = "designation",
                        notes = "no designation recorded and the description ends above the depth asked"))
  }
  .cst_ch_prof(id, FALSE, notes = if (id == "paralithic_contact")
    "a Cr is a paralithic contact only with a contact recorded or an FAO, WRB or USDA notation declared (Q2 of 2026-11-05)")
}

#' Lithic and paralithic contacts of the Chinese Soil Taxonomy (2001), p. 48;
#' `top_max` the depth of the soil surface within which the contact lies.
#' @noRd
lithic_contact_cst <- function(pedon, top_max = NULL) .cst_contact_result(pedon, "lithic_contact", top_max)
#' @noRd
paralithic_contact_cst <- function(pedon, top_max = NULL) .cst_contact_result(pedon, "paralithic_contact", top_max)


# --- 5. anthro-silting materials (pp. 48-49) -------------------------------------------------

#' Anthro-silting materials (pp. 48-49): (1) the silting rate (0.5 cm a year
#' by irrigation, 10 cm by interception), and (2) stratification or (3) the
#' sieve test. soilKey reads (2) and (3) as alternatives: the sieve test is
#' the test of the layers whose stratification tillage erased.
#' @noRd
anthro_silting_materials_cst <- function(pedon) {
  t <- .cst_cthr("anthro_silting")
  x <- .cst_ctx(pedon)
  site <- pedon$site %||% list()
  rate <- .cst_site_num(site, "silting_rate_cm_yr")
  kind <- .cst_site_txt(site, "silting_kind")
  i1 <- if (is.na(rate)) NA
        else if (rate >= t$interception_rate_min_cm_yr) TRUE
        else if (rate < t$irrigation_rate_min_cm_yr) FALSE
        else if (is.na(kind)) NA else kind == "irrigation"
  stand <- character(0)
  if (is.na(i1) && (is.na(kind) || kind == "irrigation")) {
    ir <- tryCatch(irragric(pedon), error = function(e) NULL)
    if (!is.null(ir) && isTRUE(ir$passed)) { i1 <- TRUE; stand <- "irragric" }
  }
  sp <- .cst_t(x, "stratification_pattern")
  sv <- .cst_t(x, "silting_sieve_test"); mf <- .cst_t(x, "micromorph_features")
  strat <- !is.na(sp) & sp != "none"
  sieve <- (!is.na(sv) & sv == "positive") | (!is.na(mf) & grepl("layered_sorting", mf))
  i23 <- any(strat | sieve)
  passed <- .cst_and(i1, i23)
  .cst_ch_prof("anthro_silting_materials", passed, kind = if (isTRUE(passed)) kind else NA_character_,
               layers = x$idx[which(strat | sieve)], stand_in = stand,
               flags = if (length(stand)) sprintf("stand_in:%s", stand),
               missing = c("silting_rate_cm_yr", if (is.na(kind)) "silting_kind"),
               notes = "stratification (2) or the sieve test (3) read as alternatives (soilKey's reading)")
}


# --- 6. vertic features and vertic evidence (pp. 49-50) --------------------------------------

# Runs of layers with a per-layer field feature: TRUE when a run at least
# `min` cm thick has its top within `within` cm of the soil surface.
.cst_feature_run <- function(x, p, min, within) {
  if (!x$n) return(FALSE)
  r <- .cst_runs(x, p)
  ok <- FALSE
  for (b in unique(stats::na.omit(r$lo_id))) {
    k <- which(r$lo_id == b)
    if (isTRUE(x$top[k[1L]] < x$surf + within) && isTRUE(r$low[k[1L]] >= min)) ok <- TRUE
  }
  ok
}

#' Vertic features (pp. 49-50): (1) clay and (2) cracks and [(3)
#' slickensides or (4) self-swallowing]; vertic evidence [(1) or (2)] and (3)
#' and (4) of its own list (default 19). grade as for the horizons.
#' @noRd
vertic_features_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence")) {
  grade <- match.arg(grade)
  t <- .cst_cthr("vertic")
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_ch_prof("vertic_features", if (x$n) NA else FALSE,
                                                grade_asked = grade, missing = c("top_cm", "bottom_cm")))
  s <- x$surf
  clay <- .cst_v(x, "clay_pct")
  ct <- .cst_contact_cm(pedon)
  notes <- character(0); flags <- character(0)
  # (1) the cultivated layers (or the upper 18 cm), then every layer to 50 cm
  cult <- .cst_cultivated(x)
  if (isTRUE(cult[1L])) {
    k <- 1L
    while (k < x$n && isTRUE(cult[k + 1L])) k <- k + 1L
    base <- x$bot[k]
    w <- .cst_weighted(pedon, "clay_pct", x$top[1L], base)
  } else {
    base <- s + t$surface_cm
    w <- .cst_weighted(pedon, "clay_pct", s, base)
  }
  if (!is.na(ct) && ct < s + t$window_cm) {
    i1 <- FALSE
    notes <- c(notes, "a lithic or paralithic contact within 50 cm: the clay of item 1 cannot be met")
  } else {
    sub <- if (isTRUE(base >= s + t$window_cm)) TRUE
           else .cst_window_every(pedon, clay >= t$clay_min_pct, base, s + t$window_cm)$value
    i1 <- .cst_and(w$value >= t$clay_min_pct, sub)
  }
  # (2) cracks of 0.5 cm in a run of 25 cm with its top within 50 cm (a
  # field observation: not recorded, absent; on a ploughed or irrigated soil,
  # whose cracks may stay closed, undecided)
  wd <- .cst_v(x, "cracks_width_cm")
  crk <- (wd >= t$crack_width_min_cm) %in% TRUE
  cdep <- .cst_v(x, "cracks_depth_cm"); ctop <- .cst_v(x, "cracks_top_cm")
  zone <- function(within, min) {
    by_run <- .cst_feature_run(x, crk, min, within)
    k <- which(crk & !is.na(cdep))
    by_depth <- length(k) > 0L && any(vapply(k, function(i) {
      top <- if (is.na(ctop[i])) 0 else ctop[i]
      top < within && cdep[i] - top >= min
    }, NA))
    by_run || by_depth
  }
  cracks_rec <- any(!is.na(wd))
  ploughed <- isTRUE(cult[1L]) || isTRUE(.cst_irrigated(pedon$site %||% list())$value)
  i2 <- zone(t$window_cm, t$crack_run_min_cm)
  if (!i2 && !cracks_rec && ploughed) {
    i2 <- NA
    notes <- c(notes, "cracks not recorded on a ploughed or irrigated soil: item 2 undecided")
  }
  # (3) intersecting slickensides (common or more) in a run of 25 cm with its
  # top within 100 cm; (4) humic infillings between the surface layer and
  # 100 cm
  sl <- .cst_t(x, "slickensides")
  slick <- !is.na(sl) & sl %in% c("common", "many", "continuous")
  i3 <- .cst_feature_run(x, slick, t$slickenside_run_min_cm, t$slickenside_top_within_cm)
  hi <- .cst_l(x, "humic_infillings")
  i4 <- any(hi %in% TRUE & seq_len(x$n) > 1L & .cst_within(x, t$infillings_within_cm))
  full <- .cst_and(i1, i2, .cst_or(i3, i4))
  # the evidence
  win_bot <- s + t$evidence_window_cm
  if (!is.na(ct) && ct < win_bot) win_bot <- ct
  part <- pmax(0, pmin(x$bot, win_bot) - pmax(x$top, s))
  cl_ok <- clay >= t$clay_min_pct
  e1 <- .cst_range_min(sum(part[cl_ok %in% TRUE]), sum(part[!(cl_ok %in% FALSE)]), t$evidence_clay_total_cm)
  if (is.na(e1) && sum(part[!(cl_ok %in% FALSE)]) == 0) e1 <- FALSE
  cole <- .cst_v(x, "cole_value")
  rows <- which(part > 0)
  le <- if (length(rows) && !anyNA(cole[rows])) sum(part[rows] * cole[rows]) else NA_real_
  e2 <- le >= t$evidence_le_min_cm
  e3 <- zone(t$evidence_window_cm, t$crack_run_min_cm)
  if (!e3 && !cracks_rec && ploughed) e3 <- NA
  slick_any <- !is.na(sl) & sl %in% c("few", "common", "many", "continuous")
  e4 <- .cst_feature_run(x, slick_any, t$evidence_slickenside_run_min_cm, t$slickenside_top_within_cm)
  evid <- .cst_and(.cst_or(e1, e2), e3, e4)
  lay <- which(crk | slick_any | hi %in% TRUE)
  .cst_ch_prof("vertic_features", full, evid = evid, grade_asked = grade,
               layers = x$idx[lay], notes = notes, flags = flags,
               missing = c(if (is.na(i1) || is.na(e1)) "clay_pct", if (is.na(e2)) "cole_value",
                           if (is.na(i2)) "cracks_width_cm"),
               extra = list(items = c(clay = i1, cracks = i2, slickensides = i3, self_swallowing = i4),
                            evidence_items = c(clay = e1, le = e2, cracks = e3, slickensides = e4),
                            le_cm = le))
}


# --- 7. anthroturbic layer (pp. 50-51) -------------------------------------------------------

.cst_ch_anthroturbic_layer <- function(x) {
  t <- .cst_cthr("anthroturbic")
  ff <- .cst_v(x, "foreign_fragments_pct")
  win <- !is.na(x$top) & !is.na(x$bot) & x$bot > x$surf + t$window_cm[1L] & x$top < x$surf + t$window_cm[2L]
  full <- (ff >= t$fragments_min_pct) %in% TRUE & win
  .cst_hz_ans(x, full, notes = "fragments of former horizons not recorded are absent; the levelling or terracing itself is not tested")
}

#' Anthroturbic layer (pp. 50-51): 3 % or more fragments of former horizons
#' in a layer between 25 and 100 cm.
#' @noRd
anthroturbic_layer_cst <- function(pedon, top_max = NULL) .cst_ch_result(pedon, "anthroturbic_layer", top_max = top_max)


# --- 9. gleyic features and gleyic evidence (pp. 55-56) --------------------------------------

# The reduced colours of item (1), per layer, on the moist colour (the dry
# hue where no moist one is recorded, flagged, Q21).
.cst_gley_colour <- function(x) {
  t <- .cst_cthr("gleyic")
  col <- .cst_colours(x)
  rk <- col$rank; v <- col$vm; c <- col$cm
  a <- col$neutral | rk > t$hue_greener_than_rank
  b <- rk > t$hue_5y_rank[1L] & rk <= t$hue_5y_rank[2L] & v >= t$value_5y_min & c <= t$chroma_5y_max
  cc <- rk > t$hue_25y_rank[1L] & rk <= t$hue_25y_rank[2L] & v >= t$value_25y_min & c <= t$chroma_25y_max
  d <- rk >= t$hue_yr_rank[1L] & rk <= t$hue_yr_rank[2L] & v >= t$value_yr[1L] & v <= t$value_yr[2L] &
    c <= t$chroma_yr_max
  e <- rk < t$hue_yr_rank[1L] & v >= t$value_red[1L] & v <= t$value_red[2L] & c <= t$chroma_red_max
  out <- a | (!col$neutral & (b | cc | d | e))
  out[col$neutral] <- TRUE
  list(value = out, flag = col$hue_flag)
}

.cst_ch_gleyic_features <- function(x) {
  t <- .cst_cthr("gleyic")
  gc <- .cst_gley_colour(x)
  rm <- .cst_v(x, "reduced_matrix_pct")
  # the share: reduced_matrix_pct, or the matrix colour read as 100 % (Q20)
  share_full <- ifelse(is.na(rm), gc$value, rm > t$matrix_share_min_pct)
  share_ev <- ifelse(is.na(rm), FALSE, rm >= t$evidence_share_min_pct & rm <= t$matrix_share_min_pct)
  ft <- .cst_t(x, "ferricyanide_test"); ct <- .cst_t(x, "childs_test")
  pos <- (ft %in% "positive") | (ct %in% "positive")
  neg <- ((ft %in% "negative") | (ct %in% "negative")) & !pos
  rh <- .cst_v(x, "eh_mv") / t$rh_eh_divisor + 2 * .cst_v(x, "ph_h2o")
  # Fe2+ shown: a positive test or rH 19 or less; a measured rH over 19 or a
  # negative test with no Eh says no; neither recorded: undecided, read as
  # met provisionally (Q11)
  reduc <- ifelse(pos | (rh <= t$rh_max) %in% TRUE, TRUE,
                  ifelse(!is.na(rh), FALSE, ifelse(neg, FALSE, NA)))
  prov <- is.na(reduc)
  red_ok <- ifelse(prov, TRUE, reduc)
  # by colour alone not in an E or albic layer, unless redox features or a
  # saturation regime are recorded (Hugo, 2026-11-06)
  pale <- x$kind %in% "E" | .cst_hz(x, "albic_horizon")$full %in% TRUE
  mc <- as.character(.cst_col(x$h, "mottle_colors"))
  redox <- (!is.na(mc) & nzchar(trimws(mc))) | (.cst_v(x, "redoximorphic_features_pct") > 0) %in% TRUE |
    (.cst_v(x, "ferric_nodules_pct") > 0) %in% TRUE | (.cst_v(x, "manganiferous_nodules_pct") > 0) %in% TRUE
  sat <- .cst_moisture_regime(x$pedon)$saturation
  sat_rec <- all(sat$candidates %in% .CST_SATURATION)
  red_ok[prov & pale & !redox & !sat_rec] <- FALSE
  full <- share_full & red_ok
  rh_ev <- (rh > t$rh_max & rh <= t$evidence_rh_max) %in% TRUE
  evid <- (share_ev & red_ok) | (share_full & rh_ev)
  flags <- c(if (any(is.na(rm) & !(full %in% FALSE))) "reduced_matrix_from_matrix_colour",
             if (any(prov & full %in% TRUE)) "gleyic_without_test_provisional",
             gc$flag[full %in% TRUE])
  lflags <- lapply(seq_len(x$n), function(i) c(if (is.na(rm[i])) "reduced_matrix_from_matrix_colour",
                                                if (prov[i]) "gleyic_without_test_provisional"))
  rows <- which(is.na(full) | is.na(evid))
  ans <- .cst_hz_ans(x, full, evid, flags = flags, lflags = lflags,
                     notes = "gley colours without a ferrous-iron test or Eh give the gleyic features provisionally (Q11)",
                     missing = .cst_missing_in(x, rows, c("munsell_hue_moist", "munsell_value_moist",
                                                          "munsell_chroma_moist", "reduced_matrix_pct")))
  ans$prov <- prov & (full %in% TRUE | evid %in% TRUE)
  ans$rh <- rh
  ans
}

#' Gleyic features and gleyic evidence (pp. 55-56), per layer; grade and
#' top_max as for the horizons. A result resting on gley colours without a
#' ferrous-iron test or Eh is provisional (Q11).
#' @noRd
gleyic_features_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence"), top_max = NULL,
                                datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "gleyic_features", grade = match.arg(grade), top_max = top_max, datum = match.arg(datum))
}


# --- 10. redoxic features (p. 56) --------------------------------------------------------------

.cst_ch_redoxic_features <- function(x) {
  t <- .cst_cthr("redoxic")
  # (1) rust mottles: a mottle colour of chroma 3 or more (or a mottle not
  # read as a colour), or redox concentrations recorded
  mc <- as.character(.cst_col(x$h, "mottle_colors"))
  has_mot <- !is.na(mc) & nzchar(trimws(mc))
  rust <- vapply(seq_len(x$n), function(i) {
    if (!has_mot[i]) return(FALSE)
    m <- .munsell_notations(mc[i])
    if (!nrow(m)) return(FALSE)
    any(is.na(m$chroma) | m$chroma >= t$rust_chroma_min)
  }, NA)
  i1 <- rust | (.cst_v(x, "redoximorphic_features_pct") > 0) %in% TRUE
  # (2) iron-manganese nodules, concretions, spots or an iron pan
  pan <- .cst_t(x, "pan_type")
  i2 <- (.cst_v(x, "ferric_nodules_pct") > 0) %in% TRUE | (.cst_v(x, "manganiferous_nodules_pct") > 0) %in% TRUE |
    (.cst_v(x, "fe_mn_nodules_pct") > 0) %in% TRUE | (!is.na(pan) & grepl("iron", pan))
  # (4) a reduced matrix under 30 %, and some (default 15)
  rm <- .cst_v(x, "reduced_matrix_pct")
  i4 <- (rm > 0 & rm < t$reduced_matrix_max_pct) %in% TRUE
  # (3) no mottles and a moist chroma of 2 or less; 1 or less where the
  # layers above and below are dark (chroma 2) (default 15). Only where a
  # saturation regime is read (the definition's seasonal saturation); a
  # recorded drainage class well drained or better says no; otherwise
  # undecided (Hugo, 2026-11-06).
  cm <- .cst_v(x, "munsell_chroma_moist")
  dark_nb <- vapply(seq_len(x$n), function(i) {
    nb <- c(if (i > 1L) cm[i - 1L], if (i < x$n) cm[i + 1L])
    length(nb) > 0L && !anyNA(nb) && all(nb <= t$chroma_max)
  }, NA)
  lim <- ifelse(dark_nb, t$chroma_dark_max, t$chroma_max)
  sat <- .cst_moisture_regime(x$pedon)$saturation
  dc <- .cst_site_txt(x$pedon$site, "drainage_class")
  drained <- !is.na(dc) && grepl("^(excessively|somewhat excessively|rapidly|well) drained|^(excessivamente|fortemente|acentuadamente|bem) drenado", dc)
  sat_ok <- if (all(sat$candidates %in% .CST_SATURATION)) TRUE else if (drained) FALSE else NA
  i3 <- !has_mot & cm <= lim & sat_ok
  full <- i1 | i2 | i4 | i3
  rows <- which(is.na(full))
  .cst_hz_ans(x, full,
              notes = c("rust mottles, Fe-Mn nodules and a reduced matrix not recorded are absent",
                        "a low chroma alone counts only under a saturation regime (Hugo, 2026-11-06)"),
              missing = c(.cst_missing_in(x, rows, "munsell_chroma_moist"),
                          if (length(rows)) c(sat$missing, "drainage_class")))
}

#' Redoxic features (p. 56), per layer.
#' @noRd
redoxic_features_cst <- function(pedon, top_max = NULL, datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "redoxic_features", top_max = top_max, datum = match.arg(datum))
}


# --- 12. permafrost layer (p. 57) ----------------------------------------------------------------

.cst_ch_permafrost_layer <- function(x) {
  pt <- .cst_v(x, "permafrost_temp_C")
  fz <- .cst_frozen(x$pedon, .cst_col(x$h, "designation"))
  tr <- .cst_temperature_regime(x$pedon)
  warm <- all(tr$candidates %in% c("frigid", "mesic", "thermic", "hyperthermic"))
  # frozen ground is a field observation: a described layer that is not
  # recorded frozen is no permafrost (Hugo, 2026-11-06); permafrost_layer_cst()
  # leaves the test undecided when the description stops above the depth
  # asked
  full <- (pt <= 0) %in% TRUE | fz
  .cst_hz_ans(x, full,
              notes = c(if (warm && !any(full)) sprintf("no permafrost under the %s regime",
                                                        paste(tr$candidates, collapse = " or ")),
                        "permafrost by permafrost_temp_C at or below 0 C or a frozen designation"))
}

#' Permafrost layer (p. 57): a layer perennially at or below 0 C; top_max the
#' depth within which its top lies.
#' @noRd
permafrost_layer_cst <- function(pedon, top_max = NULL) {
  res <- .cst_ch_result(pedon, "permafrost_layer", top_max = top_max)
  if (isFALSE(res$passed)) {
    x <- .cst_ctx(pedon)
    fr <- .cst_site_txt(pedon$site, "frost_regime")
    no_pf <- !is.na(fr) && fr %in% c("none", "seasonal", "long-seasonal")
    tr <- .cst_temperature_regime(pedon)
    warm <- all(tr$candidates %in% c("frigid", "mesic", "thermic", "hyperthermic"))
    deep <- if (!x$n) -Inf else suppressWarnings(max(x$bot, na.rm = TRUE))
    lim <- if (is.null(top_max)) -Inf else x$surf + top_max
    # the description stops above the depth asked and nothing rules
    # permafrost out: undecided
    if (x$n && !no_pf && !warm && (is.na(x$surf) || deep < lim)) {
      res$passed <- NA
      res$missing <- c("bottom_cm", "permafrost_temp_C", "frost_regime", "soil_temperature_regime")
      res$evidence$grade <- NA_character_
    }
  }
  if (is.na(res$passed)) {
    si <- tryCatch(permafrost_within_usda(pedon, max_top_cm = top_max %||% 200), error = function(e) NULL)
    if (!is.null(si) && isTRUE(si$passed)) {
      res$passed <- TRUE
      res$layers <- si$layers
      res$missing <- character(0)
      res$evidence$grade <- "full"
      res$evidence$stand_in <- "permafrost_within_usda"
      res$evidence$lower_grade <- TRUE
      res$evidence$flags <- unique(c(res$evidence$flags, "stand_in:permafrost_within_usda"))
    }
  }
  res
}


# --- 13. frost-thawic features (p. 57) ---------------------------------------------------------

#' Frost-thawic features (p. 57): one or more of (1) cryoturbation, (2) a
#' scaly (lenticular) structure in an A or B, (3) the thin-section items,
#' (4) diurnal freeze-thaw on 70 % of the days.
#' @noRd
frost_thawic_features_cst <- function(pedon) {
  t <- .cst_cthr("frost_thawic")
  x <- .cst_ctx(pedon)
  site <- pedon$site %||% list()
  cr <- .cst_t(x, "cryoturbation")
  pg <- .cst_site_txt(site, "patterned_ground")
  i1 <- any(!is.na(cr) & cr == "present") || (!is.na(pg) && !grepl("^none$", pg))
  ty <- .cst_t(x, "structure_type")
  # in an A or B only (p. 57); a layer with no designation, undecided
  ab <- ifelse(is.na(x$kind), NA, x$kind %in% c("A", "AB", "B", "BC"))
  sc <- !is.na(ty) & grepl("lentic|scal", ty)
  scaly <- sc & ab %in% TRUE
  i2 <- if (any(scaly)) TRUE else if (any(sc & is.na(ab))) NA else FALSE
  mf <- .cst_t(x, "micromorph_features")
  mm <- !is.na(mf) & grepl("freeze_thaw_aggregates|vermiform_voids", mf) &
    grepl("fibrous_oriented_clay|layered_sorting|grain_clusters", mf)
  i3 <- any(mm)
  ft <- .cst_site_num(site, "freeze_thaw_days_pct")
  note <- character(0)
  i4 <- ft >= t$freeze_thaw_days_min_pct
  if (is.na(i4)) {
    tr <- .cst_temperature_regime(pedon)
    if (all(tr$candidates %in% c("mesic", "thermic", "hyperthermic"))) {
      i4 <- FALSE
      note <- sprintf("no diurnal freeze-thaw on 70 %% of the days under the %s regime (soilKey's reading)",
                      paste(tr$candidates, collapse = " or "))
    }
  }
  passed <- .cst_or(i1, i2, i3, i4)
  .cst_ch_prof("frost_thawic_features", passed, layers = x$idx[which(scaly | mm | (!is.na(cr) & cr == "present"))],
               notes = c(note, "cryoturbation, scaly structure and the thin-section items not recorded are absent"),
               missing = c("freeze_thaw_days_pct", "soil_temperature_regime"),
               extra = list(items = c(cryoturbation = i1, scaly = i2, thin_section = i3, freeze_thaw = i4)))
}


# --- 14. n value (p. 58) ---------------------------------------------------------------------------

.cst_n_ranges <- function(x) {
  n <- .cst_v(x, "n_value")
  fc <- .cst_t(x, "fluidity_class")
  t <- .cst_cthr("n_value")
  lo <- ifelse(!is.na(n), n, ifelse(fc %in% "non-fluid", 0, ifelse(fc %in% "slightly fluid", t$critical,
          ifelse(fc %in% c("moderately fluid", "very fluid"), t$flows_easily_min, NA_real_))))
  hi <- ifelse(!is.na(n), n, ifelse(fc %in% "non-fluid", t$critical - 1e-9, ifelse(fc %in% "slightly fluid", t$flows_easily_min - 1e-9,
          ifelse(fc %in% c("moderately fluid", "very fluid"), Inf, NA_real_))))
  list(lo = lo, hi = hi, by_class = is.na(n) & !is.na(lo))
}

.cst_ch_n_value <- function(x, min = .cst_cthr("n_value", "critical")) {
  r <- .cst_n_ranges(x)
  full <- ifelse(r$lo >= min, TRUE, ifelse(r$hi < min, FALSE, NA))
  ans <- .cst_hz_ans(x, full, flags = if (any(r$by_class & !is.na(full))) "n_value_from_fluidity_class",
              missing = .cst_missing_in(x, which(is.na(full)), c("n_value", "fluidity_class")))
  ans$lower <- r$by_class & full %in% TRUE
  ans
}

#' n value (p. 58): the layers whose n value is `min` (0.7, the critical
#' value) or more; the field fluidity class stands in, flagged (slightly
#' fluid 0.7-1.0, moderately or very fluid 1 or more).
#' @noRd
n_value_cst <- function(pedon, min = .cst_cthr("n_value", "critical"), top_max = NULL) {
  x <- .cst_ctx(pedon)
  r <- if (x$n) .cst_ch_n_value(x, min) else .cst_hz_ans(x, logical(0))
  r$prov <- rep(FALSE, x$n)
  if (is.null(r$lower)) r$lower <- rep(FALSE, x$n)
  res <- .cst_ch_result(pedon, "n_value", top_max = top_max, r = r)
  res$evidence$min <- min
  res
}


# --- 15. isohumic property (pp. 58-59) --------------------------------------------------------------

#' Isohumic property (pp. 58-59): Rh, the storage of organic matter within
#' 20 cm over that within 100 cm (scaled on a contact at 50-100 cm), 0.4 or
#' less (the printed "4" read as 0.4, Hugo's decision Q9), and no organic
#' (histic) evidence in the upper pedon. Storages use the book's default
#' bulk densities where none was measured, flagged (Q10).
#' @noRd
isohumic_property_cst <- function(pedon) {
  t <- .cst_cthr("isohumic")
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_ch_prof("isohumic_property", FALSE))
  # the depths scaled to the contact (the total thickness of the horizons)
  # when a lithic or paralithic contact lies at 50-100 cm (Hugo,
  # 2026-11-06); the solum stays for the humic storage only
  d <- c(upper = .cst_thr("storage", "isohumic_upper_cm"), lower = .cst_thr("storage", "isohumic_lower_cm"))
  ct <- .cst_contact_cm(pedon)
  if (!is.na(ct) && !is.na(x$surf)) {
    dd <- ct - x$surf
    if (dd >= .cst_thr("storage", "contact_scaled_from_cm") && dd < d[["lower"]])
      d <- c(upper = d[["upper"]] * dd / d[["lower"]], lower = dd)
  }
  s20 <- .cst_storage(pedon, to = d[["upper"]], what = "om")
  s100 <- .cst_storage(pedon, to = d[["lower"]], what = "om")
  flags <- unique(c(s20$flags, s100$flags))
  rh <- c(low = NA_real_, high = NA_real_)
  r1 <- NA
  if (length(s20$value) == 2L && length(s100$value) == 2L && !anyNA(c(s20$value, s100$value))) {
    a_lo <- s20$value[["low"]]; a_hi <- s20$value[["high"]]
    b_lo <- max(0, s100$value[["low"]] - a_lo); b_hi <- max(0, s100$value[["high"]] - a_hi)
    if (a_hi + b_hi <= 0) {
      r1 <- FALSE
    } else {
      rh <- c(low = a_lo / (a_lo + b_hi), high = if (a_hi + b_lo > 0) a_hi / (a_hi + b_lo) else 0)
      r1 <- if (rh[["high"]] <= t$rh_max) TRUE else if (rh[["low"]] > t$rh_max) FALSE else NA
    }
  }
  # (2) no organic evidence 5-20 cm thick (read as the histic epipedon, at
  # least its evidence, default 16)
  hi <- .cst_hz_result(pedon, "histic_epipedon", grade = "at_least_evidence", x = x)$passed
  r2 <- !hi
  passed <- .cst_and(r1, r2)
  .cst_ch_prof("isohumic_property", passed, flags = flags,
               layers = x$idx[s100$rows %||% integer(0)],
               notes = c(sprintf("Rh = %s (limit %s; the printed %s read as %s, Q9)",
                                 if (anyNA(rh)) "undecided" else paste(format(round(rh, 3)), collapse = "-"),
                                 t$rh_max, t$rh_printed, t$rh_max), s20$note, s100$note),
               missing = unique(c(s20$missing, s100$missing, if (is.na(r2)) "oc_pct")),
               extra = list(rh = rh, storage_upper = s20$value, storage_lower = s100$value, depths = d,
                            bd_default_rows = unique(c(s20$bd_default_rows, s100$bd_default_rows))))
}


# --- 16. humic property (pp. 59-60) ------------------------------------------------------------------

#' Humic property (pp. 59-60): (1) humus high in the A and decreasing, with
#' humus coatings (or humic infillings) in the B, and (2) organic carbon
#' storage within 100 cm of 12 kg/m2 or more (the solum on a contact at 50-100
#' cm; default bulk densities flagged, Q10).
#' @noRd
humic_property_cst <- function(pedon) {
  t <- .cst_cthr("humic")
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_ch_prof("humic_property", FALSE))
  B <- .cst_b_layers(x)
  hc <- .cst_t(x, "humus_coatings")
  inf <- .cst_l(x, "humic_infillings")
  cv <- .cst_v(x, "coatings_value_moist")
  flags <- B$flag
  coat <- ifelse(!is.na(hc), hc %in% c("few", "common", "many"), FALSE)
  fb <- is.na(hc) & (cv <= t$coating_value_max) %in% TRUE
  coat <- coat | fb | inf %in% TRUE
  if (any(fb & B$b %in% TRUE & !(inf %in% TRUE))) flags <- c(flags, "humus_coatings_from_coating_value")
  i1b <- if (any(coat & B$b %in% TRUE)) TRUE else if (any(coat & is.na(B$b))) NA else FALSE
  # humus highest in the A (the first mineral layer) among the mineral
  # layers within 100 cm (soilKey's reading of "gradually decreasing")
  oc <- .cst_v(x, "oc_pct")
  s <- x$first_min
  rows <- which(.cst_within(x, t$window_cm) & seq_len(x$n) > (s %||% x$n) & !(x$kind %in% c("O", "R")))
  i1a <- if (is.na(s)) NA else if (!length(rows)) TRUE else .cst_and(oc[s] >= oc[rows])
  st <- .cst_storage(pedon, to = t$window_cm, what = "oc")
  i2 <- .cst_storage_at_least(st, t$oc_storage_min_kg_m2)
  passed <- .cst_and(i1a, i1b, i2)
  .cst_ch_prof("humic_property", passed, flags = c(flags, st$flags),
               layers = x$idx[which(coat & B$b %in% TRUE)],
               notes = c(st$note, "humus coatings not recorded are absent (dark coatings or humic infillings stand in, flagged)"),
               missing = unique(c(st$missing, if (is.na(i1a)) "oc_pct", if (is.na(i1b)) "designation")),
               extra = list(oc_storage = st$value, items = c(decreasing = i1a, coatings = i1b, storage = i2)))
}


# --- 17. andic property (pp. 60-61) ------------------------------------------------------------------

.cst_ch_andic_property <- function(x) {
  t <- .cst_cthr("andic")
  alfe <- .cst_v(x, "al_ox_pct") + 0.5 * .cst_v(x, "fe_ox_pct")
  bd <- .cst_v(x, "bulk_density_g_cm3")
  pr <- .cst_v(x, "phosphate_retention_pct")
  gl <- .cst_v(x, "volcanic_glass_pct")
  pt <- .cst_v(x, "particles_002_2mm_pct")
  p1 <- alfe >= t$alfe_min_pct & bd <= t$bd_max & pr >= t$p_retention_min_pct
  a0 <- t$glass_c_from; a1 <- t$glass_c_to
  line <- a0[[2L]] + (alfe - a0[[1L]]) * (a1[[2L]] - a0[[2L]]) / (a1[[1L]] - a0[[1L]])
  g <- (alfe >= t$glass_a_alfe_min_pct & gl >= t$glass_a_min_pct) | (alfe >= t$alfe_min_pct & gl >= t$glass_b_min_pct) |
    (alfe >= a0[[1L]] & alfe < a1[[1L]] & gl >= line)
  p2 <- pr >= t$glass_path_p_retention_min_pct & pt >= t$glass_path_particles_min_pct & g
  full <- .cst_v(x, "oc_pct") < t$oc_max_pct & (p1 | p2)
  ans <- .cst_hz_ans(x, full,
                     notes = "the 60 % pyroclastic preamble read as the concept, not a test (default 16)",
                     missing = .cst_missing_in(x, which(is.na(full)),
                                               c("al_ox_pct", "fe_ox_pct", "bulk_density_g_cm3", "phosphate_retention_pct",
                                                 "volcanic_glass_pct", "particles_002_2mm_pct", "oc_pct")))
  .cst_stand_in(x, ans, list(list(fn = "andic_soil_properties_usda")), eligible = is.na(full))
}

#' Andic property (pp. 60-61, KST 1996), per layer.
#' @noRd
andic_property_cst <- function(pedon, top_max = NULL, datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "andic_property", top_max = top_max, datum = match.arg(datum))
}


# --- 18. ferric property (pp. 61-62) -------------------------------------------------------------------

#' Ferric property (pp. 61-62): the matrix 5YR or redder, or DCB iron 14 g/kg
#' or more, or free iron 40 % or more of the total, in every layer of the B
#' (Q23).
#' @noRd
ferric_property_cst <- function(pedon) {
  t <- .cst_cthr("ferric")
  x <- .cst_ctx(pedon)
  if (!x$n) return(.cst_ch_prof("ferric_property", FALSE))
  col <- .cst_colours(x)
  p <- .cst_hue_le(col, t$hue_rank_max) | .cst_v(x, "fe_dcb_pct") >= t$fe_dcb_min_pct |
    .cst_fe_free_total(x$h) >= t$free_total_min
  B <- .cst_b_layers(x)
  inB <- B$b %in% TRUE; maybe <- is.na(B$b)
  if (!any(inB) && !any(maybe)) {
    return(.cst_ch_prof("ferric_property", FALSE, flags = B$flag, notes = "no B horizon"))
  }
  # a layer that may be of the B counts only when it meets the test
  passed <- if (any(inB)) .cst_and(c(p[inB], ifelse(p[maybe] %in% TRUE, TRUE, NA))) else NA
  rows <- which(inB)
  .cst_ch_prof("ferric_property", passed, layers = x$idx[rows],
               flags = c(B$flag, col$hue_flag[rows]),
               missing = c(.cst_missing_in(x, rows, c("munsell_hue_moist", "fe_dcb_pct", "fe_total_pct")),
                           if (any(maybe)) "designation"))
}


# --- 19. allitic property (p. 62) ----------------------------------------------------------------------

.cst_ch_allitic_property <- function(x) {
  t <- .cst_cthr("allitic")
  r1 <- .cst_v(x, "sio2_al2o3_clay") <= t$sio2_al2o3_max
  r2 <- .cst_v(x, "sio2_al2o3_naoh") <= t$naoh_ratio_max
  # item 1 is the clay ratio (alkali fusion) or the fine-earth ratio (acid
  # digestion), read here as the Ki of the sulfuric attack, flagged (Q16, D7)
  ki <- compute_ki(.cst_v(x, "sio2_sulfuric_pct"), .cst_v(x, "al2o3_sulfuric_pct"))
  rk <- ki <= t$sio2_al2o3_max
  use_ki <- !is.na(ki) & !(r1 %in% TRUE) & !(r2 %in% TRUE)
  full <- r1 | rk | r2
  .cst_hz_ans(x, full, flags = if (any(use_ki & !is.na(full))) "ki_sulfuric_substitute",
              lflags = lapply(use_ki, function(u) if (u) "ki_sulfuric_substitute" else character(0)),
              notes = "the exclusion of the Ferralosols is the key's (not read here)",
              missing = .cst_missing_in(x, which(is.na(full)), c("sio2_al2o3_clay", "sio2_al2o3_naoh",
                                                                 "sio2_sulfuric_pct", "al2o3_sulfuric_pct")))
}

#' Allitic property (p. 62), per layer.
#' @noRd
allitic_property_cst <- function(pedon, top_max = NULL, datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "allitic_property", top_max = top_max, datum = match.arg(datum))
}


# --- 20. alic property and alic evidence (pp. 62-63) -----------------------------------------------------

.cst_ch_alic_property <- function(x) {
  t <- .cst_cthr("alic")
  cec <- .cst_clay_cec(x$h)
  clay <- .cst_v(x, "clay_pct")
  al <- .cst_v(x, "al_kcl_cmol"); al <- ifelse(is.na(al), .cst_v(x, "al_cmol"), al)
  al_clay <- ifelse(!is.na(clay) & clay > 0, al * 100 / clay, NA_real_)
  cecs <- .cst_v(x, "cec_cmol")
  al_share <- ifelse(!is.na(cecs) & cecs > 0, al / cecs, NA_real_)
  ecec <- .cst_clay_ecec(x$h)$value * clay / 100
  alsat <- .cst_v(x, "al_sat_pct")
  alsat <- ifelse(is.na(alsat) & !is.na(ecec) & ecec > 0, al / ecec * 100, alsat)
  silt <- .cst_v(x, "silt_pct")
  silt <- ifelse(is.na(silt), 100 - .cst_v(x, "sand_pct") - clay, silt)
  trb <- .cst_v(x, "trb_cmol"); trbc <- .cst_v(x, "trb_clay_cmol")
  i1 <- cec$value >= t$cec_clay_min
  # the TRB path where both TRB are recorded; otherwise the silt/clay ratio
  # decides (Q20)
  sc <- silt / clay <= t$silt_clay_max
  i2 <- ifelse(is.na(trb) | is.na(trbc) | !(trb > 0), sc, (trbc / trb >= t$trb_clay_share_min) | sc)
  i3 <- .cst_v(x, "ph_kcl") <= t$ph_kcl_max
  i4a <- al_clay >= t$al_clay_min; i4b <- al_share >= t$al_cec_share_min
  i5 <- alsat >= t$al_sat_min_pct
  full <- i1 & i2 & i3 & i4a & i4b & i5
  m <- cbind(i3, i5, i4a, i4b)
  nT <- rowSums(matrix(m %in% TRUE, nrow = x$n))
  nN <- rowSums(matrix(is.na(m), nrow = x$n))
  two <- ifelse(nT >= t$evidence_items_min, TRUE, ifelse(nT + nN < t$evidence_items_min, FALSE, NA))
  evid <- i1 & two
  .cst_hz_ans(x, full, evid, flags = stats::na.omit(cec$flag),
              notes = "the exclusion of the Ferralosols and Ferrosols is the key's (not read here)",
              missing = .cst_missing_in(x, which(is.na(full) | is.na(evid)),
                                        c("cec_cmol", "clay_pct", "ph_kcl", "al_kcl_cmol", "al_sat_pct", "silt_pct")))
}

#' Alic property and alic evidence (pp. 62-63), per layer; grade and top_max
#' as for the horizons.
#' @noRd
alic_property_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence"), top_max = NULL,
                              datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "alic_property", grade = match.arg(grade), top_max = top_max, datum = match.arg(datum))
}


# --- 21. phosphic property and phosphic evidence (p. 63) -------------------------------------------------

.cst_ch_phosphic_property <- function(x) {
  t <- .cst_cthr("phosphic")
  p <- .cst_v(x, "p_total_pct"); ca <- .cst_v(x, "caco3_pct")
  # (2) total P decreasing with depth while CaCO3 increases, over the layers
  # from the first to the last with P recorded
  rec <- which(!is.na(p))
  pattern <- if (length(rec) < 2L) NA else {
    rows <- seq(min(rec), max(rec))
    pv <- p[rows]; cv <- ca[rows]
    if (anyNA(pv) || anyNA(cv)) NA
    # P higher and CaCO3 lower at the top than at the base, with no
    # reversal of over 10 % between layers (Hugo, 2026-11-06)
    else all(pv[-1L] <= (1 + t$reversal_max_rel) * pv[-length(pv)]) && all(cv[-1L] >= (1 - t$reversal_max_rel) * cv[-length(cv)]) &&
      pv[1L] > pv[length(pv)] && cv[length(cv)] > cv[1L]
  }
  full <- p >= t$p_total_min_pct & pattern
  evid <- p >= t$evidence_p_total_min_pct & p < t$p_total_min_pct
  .cst_hz_ans(x, full, evid,
              notes = c("the upper figure of 130 g/kg is what the content may reach, not a limit (soilKey's reading)",
                        sprintf("P decreasing and CaCO3 increasing with depth: %s", format(pattern))),
              missing = c(.cst_missing_in(x, which(is.na(full) | is.na(evid)), c("p_total_pct", "caco3_pct"))))
}

#' Phosphic property and phosphic evidence (p. 63), per layer.
#' @noRd
phosphic_property_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence"), top_max = NULL) {
  .cst_ch_result(pedon, "phosphic_property", grade = match.arg(grade), top_max = top_max)
}


# --- 22. sodic property and sodic evidence (p. 63) --------------------------------------------------------

.cst_ch_sodic_property <- function(x) {
  t <- .cst_cthr("sodic")
  esp <- .cst_esp(x)
  na <- .cst_v(x, "na_cmol"); mg <- .cst_v(x, "mg_cmol"); cec <- .cst_v(x, "cec_cmol")
  share <- ifelse(!is.na(cec) & cec > 0, (na + mg) / cec * 100, NA_real_)
  full <- (esp >= t$esp_min & na >= t$na_min_cmol) | share >= t$na_mg_share_min
  evid <- esp >= t$evidence_esp_min & esp < t$evidence_esp_max
  .cst_hz_ans(x, full, evid,
              notes = "ESP 30 with 2 cmol(+)/kg Na, or Na + Mg half the CEC; evidence ESP 5-29 (read 5 to under 30)",
              missing = .cst_missing_in(x, which(is.na(full) | is.na(evid)), c("esp_pct", "na_cmol", "mg_cmol", "cec_cmol")))
}

#' Sodic property and sodic evidence (p. 63), per layer.
#' @noRd
sodic_property_cst <- function(pedon, grade = c("full", "evidence", "at_least_evidence"), top_max = NULL,
                               datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "sodic_property", grade = match.arg(grade), top_max = top_max, datum = match.arg(datum))
}


# --- 23. calcaric property (p. 63) --------------------------------------------------------------------------

#' Calcaric property (p. 63): CaCO3 10 g/kg or more, or effervescence, within
#' 50 cm (`top_max`), with no sub-horizon there holding more than 20 g/kg
#' over a neighbour (that would be calcic evidence).
#' @noRd
calcaric_property_cst <- function(pedon, top_max = .cst_cthr("calcaric", "within_cm")) {
  t <- .cst_cthr("calcaric")
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_ch_prof("calcaric_property", if (x$n) NA else FALSE,
                                                missing = c("top_cm", "bottom_cm")))
  ca <- .cst_v(x, "caco3_pct")
  eff <- .cst_t(x, "effervescence_class")
  win <- which(.cst_within(x, top_max) & !(x$kind %in% "R"))
  ph <- .cst_v(x, "ph_h2o")
  # measured CaCO3 decides; without it effervescence (flagged); without
  # either, pH(H2O) under 6.5 is non-calcaric (Hugo, 2026-11-06)
  by_eff <- is.na(ca) & !is.na(eff)
  p <- ifelse(!is.na(ca), (ca >= t$caco3_min_pct) | (!is.na(eff) & eff != "none"),
              ifelse(by_eff, eff != "none", ifelse((ph < t$non_calcaric_ph_max) %in% TRUE, FALSE, NA)))
  # the excess over a neighbour, at most 20 g/kg, on measured CaCO3 only
  rock <- x$kind %in% "R"
  ex <- vapply(seq_len(x$n), function(i) {
    if (is.na(ca[i])) return(TRUE)
    nb <- c(if (i > 1L && !rock[i - 1L]) ca[i - 1L], if (i < x$n && !rock[i + 1L]) ca[i + 1L])
    nb <- nb[!is.na(nb)]
    if (!length(nb)) return(TRUE)
    all(ca[i] - nb <= t$excess_max_pct)
  }, NA)
  calc <- .cst_any_rows(p, win)
  excess_ok <- .cst_all_rows(ex, win)
  passed <- .cst_and(calc, excess_ok)
  .cst_ch_prof("calcaric_property", passed, layers = x$idx[win[p[win] %in% TRUE]],
               flags = if (any(by_eff[win] & p[win] %in% TRUE)) "calcaric_by_effervescence",
               notes = c(if (isFALSE(excess_ok)) "a sub-horizon holds more than 20 g/kg CaCO3 over a neighbour (calcic evidence)",
                         "the 20 g/kg increment read on measured CaCO3 only"),
               missing = c(.cst_missing_in(x, win, "caco3_pct"), "effervescence_class", "ph_h2o"),
               extra = list(window_cm = top_max))
}


# --- 24. base saturation (p. 64) ----------------------------------------------------------------------------

#' Base saturation (p. 64, NH4OAc): eutric 50 % or more, dystric under 50 %;
#' in the Ferralosols and Ferrosols eutrophic 35 % or more, dystrophic under
#' 35 %. The key gives the window: [top, bottom) cm below the soil surface,
#' every layer touching it (Q23) or, with every = FALSE, some layer.
#' @noRd
base_saturation_cst <- function(pedon, class = c("eutric", "dystric", "eutrophic", "dystrophic"),
                                top = 0, bottom = 125, every = TRUE) {
  class <- match.arg(class)
  t <- .cst_cthr("base_saturation")
  x <- .cst_ctx(pedon)
  if (!x$n || is.na(x$surf)) return(.cst_ch_prof("base_saturation", if (x$n) NA else FALSE,
                                                missing = c("top_cm", "bottom_cm")))
  bs <- .cst_v(x, "bs_pct")
  p <- switch(class,
    eutric = bs >= t$eutric_min_pct, dystric = bs < t$eutric_min_pct,
    eutrophic = bs >= t$eutrophic_min_pct, dystrophic = bs < t$eutrophic_min_pct)
  s <- x$surf
  if (every) {
    pp <- p; pp[x$kind %in% "R"] <- TRUE
    w <- .cst_window_every(pedon, pp, s + top, s + bottom)
    passed <- w$value; rows <- w$rows
  } else {
    w <- .cst_window(pedon, s + top, s + bottom)
    rows <- w$rows[!(x$kind[w$rows] %in% "R")]
    passed <- .cst_any_rows(p, rows)
    if (isFALSE(passed) && w$gap > 1e-9) passed <- NA
  }
  .cst_ch_prof("base_saturation", passed, kind = class, layers = x$idx[rows[p[rows] %in% TRUE]],
               missing = .cst_missing_in(x, rows, "bs_pct"),
               extra = list(class = class, window = c(top = top, bottom = bottom), every = every))
}


# --- 25. sulfidic materials (p. 64) -------------------------------------------------------------------------

.cst_ch_sulfidic_materials <- function(x) {
  t <- .cst_cthr("sulfidic")
  ss <- .cst_v(x, "sulfidic_s_pct")
  inc <- .cst_v(x, "incubation_ph")
  s_ok <- ss >= t$sulfidic_s_min_pct
  i_ok <- inc < t$incubation_ph_max
  full <- ifelse(i_ok %in% TRUE, TRUE, s_ok)
  ans <- .cst_hz_ans(x, full,
                     flags = if (any(full %in% TRUE)) "no_number_in_the_book",
                     notes = "the book gives no number: the WRB 2022 sulfidic S (0.01 %) or an incubation pH under 4 stand in, with a lower grade",
                     missing = .cst_missing_in(x, which(is.na(full)), c("sulfidic_s_pct", "incubation_ph")))
  # no number in the book: always a lower grade
  ans$lower <- full %in% TRUE
  .cst_stand_in(x, ans, list(list(fn = "hypersulfidic_material"), list(fn = "hyposulfidic_material")),
                eligible = is.na(full))
}

#' Sulfidic materials (p. 64), per layer; no number in the book, so every
#' passing layer carries a stand-in (lower grade).
#' @noRd
sulfidic_materials_cst <- function(pedon, top_max = NULL, datum = c("surface", "mineral")) {
  .cst_ch_result(pedon, "sulfidic_materials", top_max = top_max, datum = match.arg(datum))
}
