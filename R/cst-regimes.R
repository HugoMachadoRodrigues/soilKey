# =============================================================================
# v0.9.370 -- the soil moisture and temperature regimes of the Chinese Soil
# Taxonomy (2001, English edition; pp. 51-57).
#
# The regimes decide most suborders (moisture: B1, E1, F, I2, J2, K, L, M, N;
# temperature: A1, D1, G1, L1, M1, N). Profiles rarely record them, and the
# book gives one estimator only, Penman's aridity (p. 52). The readers here
# give the regime with its source, so the trace says why:
#
#   moisture, on two axes (Hugo's decisions Q5, Q12 and D1 of 2026-11-05):
#     the climatic axis (aridic, ustic, udic, perudic) from a recorded
#     climatic soil_moisture_regime, else climatic_moisture_regime, else
#     the aridity index (annual, and the highest month for perudic against
#     udic); the saturation axis (stagnic, anthrostagnic, aquic, peraquic)
#     from a recorded saturation soil_moisture_regime, else the site markers,
#     flagged (paddy records anthrostagnic; tidal flooding or water above the
#     surface peraquic; perched water stagnic; a water table within 100 cm
#     with redox features aquic or peraquic). Each test reads its own axis;
#     a value on one axis says nothing of the other;
#   temperature (Q6, Q7, default 8, D3 of 2026-11-05): the recorded
#     soil_temperature_regime ("iso" read off, pergelic read as gelic,
#     permagelic when permafrost starts within 50 cm); else the mean annual
#     soil temperature (mean_annual_soil_temp_C, else the permafrost_temp_C
#     of the horizon at 50 cm or at a shallower contact); else permafrost
#     within 50 cm (permagelic); else the mean annual air temperature + 1 C,
#     flagged; cryic against frigid by the
#     summer soil temperature only (with the O horizon and summer saturation),
#     otherwise undecided between them.
#
# A regime that the data leave between several values is kept as a set of
# candidates: a test passes when every candidate is one it asks for, fails
# when none is, and is undecided otherwise. The gelic regime includes the
# permagelic one and the aquic regime the peraquic one; "or colder" is an
# argument (Q7).
# =============================================================================

.CST_REF_SMR <- "Chinese Soil Taxonomy (2001), 3.3.8 Soil moisture regimes, pp. 51-55"
.CST_REF_STR <- "Chinese Soil Taxonomy (2001), 3.3.11 Soil temperature regimes, pp. 56-57"


# --- Penman's aridity (p. 52) -----------------------------------------------------

# The annual and monthly aridity of the book's footnote: D = ET / P, ET =
# f x Eo, f 0.6 from November to February, 0.8 from May to August, 0.7 in the
# other months. `evaporation` and `precipitation` are twelve monthly totals,
# January first, in the same unit. NA when a month is missing; a month with
# no precipitation has an infinite monthly aridity.
.cst_penman_aridity <- function(evaporation, precipitation) {
  ev <- suppressWarnings(as.numeric(evaporation))
  pr <- suppressWarnings(as.numeric(precipitation))
  if (length(ev) != 12L || length(pr) != 12L) {
    rlang::abort("Penman's aridity needs twelve monthly values of evaporation and precipitation.")
  }
  pen <- .cst_thr("moisture_regimes", "penman")
  f <- rep(pen$factor_other_months, 12L)
  f[c(11L, 12L, 1L, 2L)] <- pen$factor_nov_feb
  f[5:8] <- pen$factor_may_aug
  if (anyNA(ev) || anyNA(pr)) {
    return(list(annual = NA_real_, monthly = rep(NA_real_, 12L), max_month = NA_real_))
  }
  et <- f * ev
  monthly <- ifelse(pr > 0, et / pr, Inf)
  list(annual = sum(et) / sum(pr), monthly = monthly, max_month = max(monthly))
}


# --- moisture -------------------------------------------------------------------------

# Redox features recorded in a horizon whose top lies above `depth` (the
# aquic marker of Q5): mottles, redoximorphic features, iron or manganese
# nodules.
.cst_redox_above <- function(pedon, depth) {
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h)) return(NA)
  rows <- which(!is.na(h$top_cm) & h$top_cm < depth)
  if (!length(rows)) return(NA)
  rd <- .cst_num(h, "redoximorphic_features_pct")[rows]
  fn <- .cst_num(h, "ferric_nodules_pct")[rows]
  mn <- .cst_num(h, "manganiferous_nodules_pct")[rows]
  mc <- trimws(as.character(.cst_col(h, "mottle_colors")[rows]))
  any(rd > 0, fn > 0, mn > 0, !is.na(mc) & nzchar(mc), na.rm = TRUE)
}

.CST_CLIMATIC <- c("aridic", "ustic", "udic", "perudic")
.CST_SATURATION <- c("stagnic", "anthrostagnic", "aquic", "peraquic")

# One axis of the moisture regime: list(regime (NA when the data leave
# several), candidates, source, flags, missing, note). On the saturation
# axis "none" is a candidate: no saturation regime.
.cst_axis <- function(cand, source = NA_character_, flags = character(0),
                      missing = character(0), note = character(0)) {
  list(regime = if (length(cand) == 1L) cand else NA_character_, candidates = cand,
       source = source, flags = flags,
       missing = if (length(cand) == 1L) character(0) else missing, note = note)
}

# The moisture regime of the profile on its two axes (D1 of 2026-11-05):
# list(climatic = <axis>, saturation = <axis>, note).
.cst_moisture_regime <- function(pedon) {
  site <- pedon$site %||% list()
  note <- character(0)
  rec <- .cst_site_txt(site, "soil_moisture_regime")
  if (!is.na(rec) && rec == "torric") {
    rec <- "aridic"
    note <- "torric read as aridic"
  }
  if (!is.na(rec) && !rec %in% c(.CST_CLIMATIC, .CST_SATURATION)) {
    note <- if (rec == "xeric") "the xeric regime has no counterpart in the CST; read from the other records"
            else sprintf("the recorded regime '%s' is not one of the CST; read from the other records", rec)
    rec <- NA_character_
  }
  list(climatic = .cst_climatic_axis(site, rec),
       saturation = .cst_saturation_axis(pedon, rec), note = note)
}

# The climatic axis: a recorded climatic regime; else the regional regime
# (climatic_moisture_regime); else Penman's aridity (Q5, Q14): over 3 aridic,
# 1 to 3 ustic, under 1 udic or perudic by the highest month.
.cst_climatic_axis <- function(site, rec) {
  if (!is.na(rec) && rec %in% .CST_CLIMATIC) return(.cst_axis(rec, "recorded"))
  cr <- .cst_site_txt(site, "climatic_moisture_regime")
  if (!is.na(cr) && cr == "torric") cr <- "aridic"
  if (!is.na(cr) && cr %in% .CST_CLIMATIC) {
    return(.cst_axis(cr, "climatic_moisture_regime", "regime_from_climatic_moisture_regime"))
  }
  ai <- .cst_site_num(site, "aridity_index")
  if (!is.na(ai)) {
    ar <- .cst_thr("moisture_regimes", "aridic", "aridity_min_exclusive")
    ud <- .cst_thr("moisture_regimes", "udic", "aridity_max_exclusive")
    fl <- "regime_from_aridity_index"
    if (ai > ar) return(.cst_axis("aridic", "aridity_index", fl))
    if (ai >= ud) return(.cst_axis("ustic", "aridity_index", fl))
    mm <- .cst_site_num(site, "aridity_index_max_month")
    if (is.na(mm)) {
      return(.cst_axis(c("udic", "perudic"), "aridity_index", fl, missing = "aridity_index_max_month"))
    }
    pm <- .cst_thr("moisture_regimes", "perudic", "every_month_aridity_max_exclusive")
    return(.cst_axis(if (mm < pm) "perudic" else "udic", "aridity_index", fl))
  }
  .cst_axis(.CST_CLIMATIC, missing = c("soil_moisture_regime", "climatic_moisture_regime", "aridity_index"))
}

# The saturation axis: a recorded saturation regime; else the site markers,
# flagged: paddy records (Q12) anthrostagnic; tidal flooding (tidal_zone
# intertidal or subtidal, tidal_inundation) or water standing above the
# surface (water_table_cm_above_surface over 0) peraquic (p. 55); perched
# water stagnic; a water table within 100 cm (100 included) with redox
# features recorded above that depth aquic or peraquic (D2 of 2026-11-05).
# Otherwise undecided ("none" among the candidates).
.cst_saturation_axis <- function(pedon, rec) {
  site <- pedon$site %||% list()
  if (!is.na(rec) && rec %in% .CST_SATURATION) return(.cst_axis(rec, "recorded"))
  pad <- .cst_paddy(site)
  # v0.9.376 (Hugo on the review, 2026-11-08): a short paddy history (half
  # of the years or fewer) does not give the anthrostagnic regime
  pyf <- .cst_site_num(site, "paddy_years_frac")
  short <- !is.na(pyf) && pyf <= .cst_thr("horizons", "anthrostagnic", "evidence_paddy_years_max")
  if (isTRUE(pad$value) && !short) {
    return(.cst_axis("anthrostagnic", "site_marker", paste0("anthrostagnic_from_", pad$source)))
  }
  tz <- .cst_site_txt(site, "tidal_zone")
  ti <- .cst_site_lgl(site, "tidal_inundation")
  wa <- .cst_site_num(site, "water_table_cm_above_surface")
  if ((!is.na(tz) && tz %in% c("intertidal", "subtidal")) || isTRUE(ti) || (!is.na(wa) && wa > 0)) {
    return(.cst_axis("peraquic", "site_marker", "peraquic_from_tidal_or_surface_water"))
  }
  gk <- .cst_site_txt(site, "groundwater_kind")
  if (!is.na(gk) && gk == "perched") {
    return(.cst_axis("stagnic", "site_marker", "stagnic_from_perched_water"))
  }
  gd <- .cst_site_num(site, "groundwater_depth_cm")
  lim <- .cst_thr("moisture_regimes", "marker_groundwater_within_cm")
  if (!is.na(gd) && gd <= lim && isTRUE(.cst_redox_above(pedon, lim))) {
    return(.cst_axis(c("aquic", "peraquic"), "site_marker", "aquic_from_water_table",
                     missing = "water_table_cm_above_surface"))
  }
  # v0.9.373: a recorded drainage class of well drained or better rules the
  # saturation regimes out (as for the low chroma of the redoxic features,
  # 0.9.372); soilKey's reading, for Hugo to confirm
  if (.cst_well_drained(site)) {
    return(.cst_axis("none", "drainage_class", "no_saturation_regime_from_drainage_class"))
  }
  .cst_axis(c(.CST_SATURATION, "none"),
            missing = c("soil_moisture_regime", "groundwater_depth_cm", "groundwater_kind", "drainage_class"))
}

# A recorded drainage class of well drained or better (English or
# Portuguese words).
.cst_well_drained <- function(site) {
  dc <- .cst_site_txt(site, "drainage_class")
  !is.na(dc) && grepl(paste0("^(excessively|somewhat excessively|rapidly|well) drained|",
                             "^(excessivamente|fortemente|acentuadamente|bem) drenado"), dc)
}

# Candidates against the regimes asked for: TRUE when every candidate is
# asked for, FALSE when none is, NA otherwise.
.cst_cand_pass <- function(cand, targets) {
  if (all(cand %in% targets)) TRUE else if (!any(cand %in% targets)) FALSE else NA
}

#' Soil moisture regime of the Chinese Soil Taxonomy (2001)
#'
#' Whether the soil moisture regime of a profile is one of \code{regimes}
#' (pp. 51-55), read on two axes: the climatic one (aridic, ustic, udic,
#' perudic) from a recorded climatic \code{site$soil_moisture_regime} (torric
#' read as aridic), \code{climatic_moisture_regime} or Penman's aridity
#' (\code{aridity_index}, \code{aridity_index_max_month}); the saturation one
#' (stagnic, anthrostagnic, aquic, peraquic) from a recorded saturation regime
#' or the site markers, flagged. Each regime asked for is read on its own
#' axis; a test across both passes when either does. The aquic regime
#' includes the peraquic one. \code{evidence$source} names what decided it.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param regimes Character vector of the regimes that pass.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Cooperative Research Group on Chinese Soil Taxonomy (2001).
#'   Chinese Soil Taxonomy. Science Press, Beijing and New York, pp. 51-55.
#' @noRd
moisture_regime_cst <- function(pedon, regimes) {
  mr <- .cst_moisture_regime(pedon)
  targets <- tolower(as.character(regimes))
  targets[targets == "torric"] <- "aridic"
  if ("aquic" %in% targets) targets <- union(targets, "peraquic")
  axes <- list()
  if (any(targets %in% .CST_CLIMATIC)) axes$climatic <- mr$climatic
  if (any(targets %in% .CST_SATURATION)) axes$saturation <- mr$saturation
  res <- vapply(axes, function(ax) .cst_cand_pass(ax$candidates, targets), NA)
  passed <- if (!length(res)) FALSE else .cst_or(res)
  one <- if (length(axes) == 1L) axes[[1L]] else NULL
  flags <- unlist(lapply(axes, `[[`, "flags"), use.names = FALSE)
  notes <- c(flags, mr$note)
  DiagnosticResult$new(
    name = "moisture_regime_cst", passed = passed, layers = integer(0),
    evidence = list(regime = if (!is.null(one)) one$regime else NA_character_,
                    candidates = if (!is.null(one)) one$candidates else unlist(lapply(axes, `[[`, "candidates"), use.names = FALSE),
                    source = if (!is.null(one)) one$source else vapply(axes, `[[`, "", "source"),
                    flags = flags, regimes = targets,
                    climatic = mr$climatic, saturation = mr$saturation),
    missing = if (is.na(passed)) unique(unlist(lapply(axes, `[[`, "missing"), use.names = FALSE)) else character(0),
    reference = .CST_REF_SMR,
    notes = if (length(notes)) paste(notes, collapse = "; ") else NA_character_)
}

# A udic regime bordering on the perudic one (the Xanthi- and Xanthic taxa;
# Hugo's decision Q8), read on the climatic axis: a regime that is neither
# udic nor perudic fails (udic_perudic_border TRUE against it is noted as a
# conflict); a perudic regime passes (recorded, or from the aridity index,
# flagged); otherwise site$udic_perudic_border; otherwise undecided. A
# saturation regime says nothing of it.
.cst_udic_perudic_border <- function(pedon) {
  b <- .cst_site_lgl(pedon$site, "udic_perudic_border")
  cl <- .cst_moisture_regime(pedon)$climatic
  if (!any(cl$candidates %in% c("udic", "perudic"))) {
    return(.cst_ans(FALSE, source = cl$source, flags = cl$flags,
                    note = if (isTRUE(b)) sprintf("udic_perudic_border recorded against the %s regime", cl$regime)))
  }
  if (identical(cl$regime, "perudic")) return(.cst_ans(TRUE, source = cl$source, flags = cl$flags))
  if (!is.na(b)) return(.cst_ans(b, source = "udic_perudic_border"))
  .cst_ans(NA, missing = unique(c("udic_perudic_border", cl$missing)), source = cl$source)
}


# --- temperature ----------------------------------------------------------------------

# Permafrost starting within `depth` cm of the soil surface (default 8): a
# permafrost temperature at or below 0 C, or a frozen designation, in a
# horizon whose top lies at or above that depth. FALSE when the frost regime
# recorded has no permafrost, or a horizon reaching that depth is recorded
# above 0 C; otherwise undecided.
.cst_permafrost_within <- function(pedon, depth = .cst_thr("temperature_regimes", "permafrost_within_cm")) {
  h <- .cst_ordered(pedon)
  fr <- .cst_site_txt(pedon$site, "frost_regime")
  if (!is.null(h) && nrow(h)) {
    s <- .cst_surface_cm(pedon, "surface")
    lim <- (if (is.na(s)) 0 else s) + depth
    rows <- which(!is.na(h$top_cm) & h$top_cm <= lim)
    pt <- .cst_num(h, "permafrost_temp_C")[rows]
    fz <- grepl(.frozen_designation_pattern(pedon$site), as.character(h$designation[rows]))
    if (any(pt <= 0, na.rm = TRUE) || any(fz)) return(TRUE)
    at <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm <= lim & h$bottom_cm > lim)
    if (length(at) && any(.cst_num(h, "permafrost_temp_C")[at] > 0, na.rm = TRUE)) return(FALSE)
  }
  if (!is.na(fr) && fr %in% c("none", "seasonal", "long-seasonal")) return(FALSE)
  NA
}

# Cryic against frigid for a mean annual soil temperature between 0 and 8 C
# (p. 57): the summer soil temperature under the limit of the case (15 C, or
# 8 C with an O horizon; 13 C and 6 C when saturated in summer). Unknown
# saturation or O horizon decide only where all their limits agree. Organic
# soils have other criteria the data do not give: undecided.
.cst_cryic_or_frigid <- function(pedon, organic = FALSE) {
  if (isTRUE(organic)) {
    return(list(cand = c("cryic", "frigid"), missing = "soil_temperature_regime",
                note = "the cryic regime of organic soils is not read from the data"))
  }
  site <- pedon$site %||% list()
  summer <- .cst_site_num(site, "mean_summer_soil_temp_C")
  if (is.na(summer)) {
    return(list(cand = c("cryic", "frigid"),
                missing = c("soil_temperature_regime", "mean_summer_soil_temp_C")))
  }
  sat <- .cst_site_lgl(site, "summer_saturated")
  h <- .cst_ordered(pedon)
  first <- if (!is.null(h) && nrow(h)) .cst_horizon_kind(h$designation[1L]) else NA_character_
  o <- if (is.na(first)) NA else first == "O"
  lims <- .cst_thr("temperature_regimes", "cryic", "summer_max_exclusive_C")
  cases <- expand.grid(sat = if (is.na(sat)) c(FALSE, TRUE) else sat,
                       o = if (is.na(o)) c(FALSE, TRUE) else o)
  lim <- mapply(function(s, oo) {
    key <- paste0(if (s) "saturated" else "not_saturated", if (oo) "_with_o" else "_no_o")
    lims[[key]]
  }, cases$sat, cases$o)
  if (summer < min(lim)) return(list(cand = "cryic", missing = character(0)))
  if (summer >= max(lim)) return(list(cand = "frigid", missing = character(0)))
  list(cand = c("cryic", "frigid"),
       missing = c(if (is.na(sat)) "summer_saturated", if (is.na(o)) "designation"))
}

# The permafrost_temp_C of the horizon at the control depth (50 cm, or the
# horizon above a shallower contact), read as the mean annual soil
# temperature (D3 of 2026-11-05).
.cst_soil_temp_at_control <- function(pedon) {
  h <- .cst_ordered(pedon)
  if (is.null(h) || !nrow(h)) return(NA_real_)
  s <- .cst_surface_cm(pedon, "surface")
  d <- (if (is.na(s)) 0 else s) + .cst_thr("temperature_regimes", "depth_cm")
  ct <- .cst_contact_cm(pedon)
  pt <- .cst_num(h, "permafrost_temp_C")
  if (!is.na(ct) && ct < d) {
    k <- which(!is.na(h$bottom_cm) & h$bottom_cm == ct)
  } else {
    k <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$top_cm <= d & h$bottom_cm > d)
  }
  if (length(k) && !is.na(pt[k[1L]])) pt[k[1L]] else NA_real_
}

# The regime of a mean annual soil temperature (cryic or frigid between 0
# and 8 C by the summer).
.cst_regime_of_mast <- function(pedon, mast, organic, pf) {
  if (mast <= .cst_thr("temperature_regimes", "gelic_mast_max_C")) {
    return(list(cand = if (isFALSE(pf)) "gelic" else c("gelic", "permagelic"),
                missing = "permafrost_temp_C"))
  }
  if (mast < .cst_thr("temperature_regimes", "frigid_mast_max_exclusive_C")) {
    return(.cst_cryic_or_frigid(pedon, organic))
  }
  r <- if (mast < .cst_thr("temperature_regimes", "thermic_mast_C")[1L]) "mesic"
       else if (mast < .cst_thr("temperature_regimes", "hyperthermic_mast_min_C")) "thermic"
       else "hyperthermic"
  list(cand = r, missing = character(0))
}

# The temperature regime of the profile with its source: list(regime (NA when
# the data leave several), candidates, source, flags, missing, note, mast).
.cst_temperature_regime <- function(pedon, organic = FALSE) {
  site <- pedon$site %||% list()
  vals <- .cst_thr("temperature_regimes", "values")
  out <- function(cand, source, flags = character(0), missing = character(0),
                  note = character(0), mast = NA_real_) {
    list(regime = if (length(cand) == 1L) cand else NA_character_, candidates = cand,
         source = source, flags = flags,
         missing = if (length(cand) == 1L) character(0) else missing,
         note = note, mast = mast)
  }
  pf <- .cst_permafrost_within(pedon)
  note <- character(0)
  rec <- .cst_site_txt(site, "soil_temperature_regime")
  if (!is.na(rec)) {
    r <- sub("^iso", "", rec)
    if (r == "pergelic") {
      r <- "gelic"
      note <- "pergelic read as gelic (p. 56)"
    } else if (r != rec) {
      note <- sprintf("%s read as %s (the CST has no iso regimes)", rec, r)
    }
    if (r %in% vals) {
      if (r == "gelic" && isTRUE(pf)) {
        return(out("permagelic", "recorded", "permagelic_from_permafrost", note = note))
      }
      if (isTRUE(pf) && r != "permagelic") {
        note <- c(note, sprintf("permafrost within 50 cm under a recorded %s regime; the recorded regime is kept", r))
      }
      return(out(r, "recorded", note = note))
    }
    note <- sprintf("the recorded regime '%s' is not one of the CST; read from the other records", rec)
  }
  mast <- .cst_site_num(site, "mean_annual_soil_temp_C")
  source <- "mean_annual_soil_temp_C"
  if (is.na(mast)) {
    mast <- .cst_soil_temp_at_control(pedon)
    source <- "permafrost_temp_C"
  }
  if (!is.na(mast)) {
    rm <- .cst_regime_of_mast(pedon, mast, organic, pf)
    if (isTRUE(pf)) {
      if (mast <= .cst_thr("temperature_regimes", "permagelic_max_C")) {
        return(out("permagelic", source, "permagelic_from_permafrost", note = note, mast = mast))
      }
      return(out(unique(c("permagelic", rm$cand)), source, "permafrost_soil_temperature_conflict",
                 missing = "soil_temperature_regime",
                 note = c(note, sprintf("permafrost within 50 cm against a mean annual soil temperature of %s C", format(mast))),
                 mast = mast))
    }
    return(out(rm$cand, source, missing = rm$missing, note = c(note, rm$note), mast = mast))
  }
  if (isTRUE(pf)) {
    return(out("permagelic", "permafrost", "permagelic_from_permafrost", note = note))
  }
  air <- .cst_site_num(site, "mean_annual_air_temp_C")
  if (is.na(air)) {
    return(out(vals, NA_character_,
               missing = c("soil_temperature_regime", "mean_annual_soil_temp_C",
                           "mean_annual_air_temp_C"), note = note))
  }
  mast <- air + .cst_thr("temperature_regimes", "air_to_soil_offset_C")
  rm <- .cst_regime_of_mast(pedon, mast, organic, pf)
  out(rm$cand, "mean_annual_air_temp_C", "mast_from_air_temperature",
      missing = rm$missing, note = c(note, rm$note), mast = mast)
}

#' Soil temperature regime of the Chinese Soil Taxonomy (2001)
#'
#' Whether the soil temperature regime of a profile is one of
#' \code{regimes} (permagelic, gelic, cryic, frigid, mesic, thermic,
#' hyperthermic; pp. 56-57). The regime is the recorded
#' \code{site$soil_temperature_regime} ("iso" read off, pergelic read as
#' gelic, permagelic when permafrost starts within 50 cm); else
#' \code{mean_annual_soil_temp_C}; else \code{mean_annual_air_temp_C} + 1 C,
#' flagged. Cryic against frigid needs \code{mean_summer_soil_temp_C}. The
#' gelic regime includes the permagelic one; \code{or_colder} adds the
#' regimes colder than those asked for (the key's "cryic or colder").
#' \code{evidence$source} names what decided it.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param regimes Character vector of the regimes that pass.
#' @param or_colder Logical; the colder regimes pass too.
#' @param organic Logical; the profile is an organic soil (its cryic regime
#'   is not read from the data).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Cooperative Research Group on Chinese Soil Taxonomy (2001).
#'   Chinese Soil Taxonomy. Science Press, Beijing and New York, pp. 56-57.
#' @noRd
temperature_regime_cst <- function(pedon, regimes, or_colder = FALSE, organic = FALSE) {
  tr <- .cst_temperature_regime(pedon, organic = organic)
  ord <- .cst_thr("temperature_regimes", "coldness_order")
  targets <- tolower(as.character(regimes))
  if (isTRUE(or_colder)) {
    k <- suppressWarnings(max(match(targets, ord), na.rm = TRUE))
    if (is.finite(k)) targets <- union(targets, ord[seq_len(k)])
  }
  if ("gelic" %in% targets) targets <- union(targets, "permagelic")
  cand <- tr$candidates
  passed <- if (all(cand %in% targets)) TRUE else if (!any(cand %in% targets)) FALSE else NA
  DiagnosticResult$new(
    name = "temperature_regime_cst", passed = passed, layers = integer(0),
    evidence = list(regime = tr$regime, candidates = cand, source = tr$source,
                    flags = tr$flags, mast = tr$mast, regimes = targets),
    missing = if (is.na(passed)) tr$missing else character(0),
    reference = .CST_REF_STR,
    notes = if (length(c(tr$flags, tr$note))) paste(c(tr$flags, tr$note), collapse = "; ") else NA_character_)
}
