# =============================================================================
# v0.9.331 -- the 35 sub-classes of the French soil classification (CPCS
# 1967). The sub-classes rest where possible on the pedoclimate (pp. 8-9):
# a climatic sub-class needs its climate and the morphology of one of its
# groups; with no climate recorded the soil falls to the class's
# non-climatic or temperate sub-class, with a note in the trace (Hugo,
# 2026-11-01). The climate is read from site$koppen_climate, else from the
# USDA soil temperature and moisture regimes, else from the latitude (the
# tropical rule). Three-valued. Names carry the _cpcs suffix.
# =============================================================================

# ---- the climate -------------------------------------------------------------------

.cpcs_koppen <- function(site) {
  k <- trimws(as.character((site %||% list())$koppen_climate %||% NA)[1L])
  if (is.na(k) || !nzchar(k) || toupper(k) == "NA") NA_character_ else k
}

.cpcs_regimes <- function(site) {
  f <- function(v) { v <- tolower(trimws(as.character(v %||% NA)[1L])); if (is.na(v) || !nzchar(v) || v == "na") NA_character_ else v }
  list(t = f(site$soil_temperature_regime), m = f(site$soil_moisture_regime))
}

.cpcs_lat <- function(site) suppressWarnings(as.numeric(as.character(site$lat %||% NA)[1L]))

# Brazil, which soilKey already reads as tropical for the WRB ferralic
# horizon and the SiBCS B latossolico, when no latitude is recorded.
.cpcs_brazil <- function(site) {
  ct <- toupper(trimws(as.character(site$country %||% NA)[1L]))
  !is.na(ct) && ct %in% c("BR", "BRA", "BRAZIL", "BRASIL")
}

# One climate kind: TRUE, FALSE, or NA. The Koppen code decides when it is
# recorded; otherwise the USDA regimes; otherwise the latitude where the kind
# has a latitude rule. With no climate at all the result is `none`: FALSE
# for a class with a non-climatic or temperate default sub-class, NA for the
# isohumic soils, whose four sub-classes are all climatic.
.cpcs_climate <- function(x, kind, none = FALSE) {
  s <- x$site; k <- .cpcs_koppen(s); r <- .cpcs_regimes(s); lat <- .cpcs_lat(s)
  kp <- function(re) grepl(re, k, perl = TRUE)
  tre <- function(re) !is.na(r$t) && grepl(re, r$t)
  mre <- function(re) !is.na(r$m) && grepl(re, r$m)
  if (!is.na(k)) {
    return(switch(kind,
      cold_desert = kp("^E"),
      hot_desert = kp("^BW"),
      dry = kp("^B|^Cs"),
      not_arid = !kp("^BW"),
      tropical = kp("^A"),
      boreal = kp("^D[fsw][cd]"),
      continental = kp("^D[fsw][ab]"),
      cold = kp("^E|^D[fsw][cd]"),
      vi1 = kp("^Cfa|^Dfa"),
      vi2 = kp("^D|^BSk") && !kp("^Dfa"),
      vi3 = kp("^Cs"),
      vi4 = kp("^BSh|^Aw|^As"),
      NA))
  }
  have_t <- !is.na(r$t); have_m <- !is.na(r$m)
  if (have_t || have_m) {
    out <- switch(kind,
      cold_desert = if (tre("cryic|pergelic|gelic")) TRUE else if (have_t) FALSE else none,
      hot_desert = if (mre("aridic|torric")) TRUE else if (have_m) FALSE else none,
      dry = if (mre("aridic|torric|xeric|ustic")) TRUE else if (have_m) FALSE else none,
      not_arid = if (have_m) !mre("aridic|torric") else TRUE,
      tropical = if (tre("isothermic|isohyperthermic|^hyperthermic")) TRUE else
        if (!is.na(lat)) abs(lat) <= 23.5 else if (.cpcs_brazil(s)) TRUE else if (have_t) FALSE else none,
      boreal = if (tre("cryic")) TRUE else if (have_t) FALSE else none,
      continental = none,
      cold = if (tre("cryic|gelic|pergelic")) TRUE else if (have_t) FALSE else none,
      vi1 = if (mre("udic")) TRUE else if (have_m) FALSE else NA,
      vi2 = if (tre("frigid|cryic") && !mre("udic")) TRUE else if (have_t && (have_m || !tre("frigid|cryic"))) FALSE else NA,
      vi3 = if (mre("xeric")) TRUE else if (have_m) FALSE else NA,
      vi4 = if (mre("ustic|aridic|torric") && tre("thermic|hyperthermic|isothermic|isohyperthermic") && !tre("mesic|frigid|cryic"))
        TRUE else if (have_t && have_m) FALSE else NA,
      NA)
    return(out)
  }
  if (kind == "tropical" && !is.na(lat)) return(abs(lat) <= 23.5)
  if (kind == "tropical" && .cpcs_brazil(s)) return(TRUE)
  if (kind == "not_arid") return(TRUE)
  if (kind %in% c("vi1", "vi2", "vi3", "vi4")) return(NA)
  none
}

# The climate a sub-class test read, for the trace.
.cpcs_climate_source <- function(x) {
  s <- x$site
  if (!is.na(.cpcs_koppen(s))) return(sprintf("Koppen %s", .cpcs_koppen(s)))
  r <- .cpcs_regimes(s)
  if (!is.na(r$t) || !is.na(r$m)) return(sprintf("regimes %s / %s", r$t %||% "-", r$m %||% "-"))
  if (!is.na(.cpcs_lat(s))) return(sprintf("latitude %s", .cpcs_lat(s)))
  "no climate recorded: the default sub-class"
}

.cpcs_sub_result <- function(name, passed, x, evidence = list(), where = "") {
  evidence$climate <- .cpcs_climate_source(x)
  .cpcs_result(name, passed, integer(0), evidence, character(0), where)
}

# ---- morphology shared by the sub-classes -----------------------------------------

# A textural B: a designated B with t, else soilKey's argic horizon.
.cpcs_textural_b <- function(x, pedon) {
  rows <- which(!(x$organic %in% TRUE))
  if (any(x$role[rows] %in% "B" & x$des$t[rows] %in% TRUE)) return(TRUE)
  if (all(is.na(x$role[rows]) | x$role[rows] %in% "other")) return(tryCatch(argic(pedon)$passed, error = function(e) NA))
  FALSE
}

# ---- I --------------------------------------------------------------------------

#' CPCS 1967 sub-class gates (internal)
#'
#' The tests of the 35 sub-classes of the French soil classification
#' (CPCS 1967), read by \code{\link{classify_cpcs1967}} in the order of
#' \code{inst/rules/cpcs1967/subclasses/}. Each returns a
#' \code{\link{DiagnosticResult}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @keywords internal
#' @name cpcs1967_subclasses
NULL

#' @rdname cpcs1967_subclasses
#' @export
cold_desert_raw_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  p <- .cpcs_or(.cpcs_permafrost(x), .cpcs_climate(x, "cold_desert"))
  .cpcs_sub_result("cold_desert_raw_soils_cpcs", p, x, list(permafrost = .cpcs_permafrost(x)), "I-2, p. 18")
}

#' @rdname cpcs1967_subclasses
#' @export
hot_desert_raw_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("hot_desert_raw_soils_cpcs", .cpcs_climate(x, "hot_desert"), x, where = "I-3, p. 19")
}

# ---- II -------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
permafrost_weakly_developed_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("permafrost_weakly_developed_soils_cpcs", .cpcs_permafrost(x), x, where = "II-1, p. 22")
}

#' @rdname cpcs1967_subclasses
#' @export
humic_weakly_developed_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # high organic matter in the A (5 % OM or more; the text gives no figure,
  # Hugo, 2026-11-01), pH under 6.8, not an arid climate (p. 23)
  a <- .cpcs_a_rows(x)
  om <- if (length(a)) x$om[a] else NA_real_
  om_ok <- if (!length(a)) NA else .cpcs_any(ifelse(is.na(om), NA, om >= 5))
  ph <- .cpcs_num(x$h, "ph_h2o")[a]
  ph_ok <- if (!length(a)) NA else .cpcs_all(ifelse(is.na(ph), NA, ph < 6.8))
  p <- .cpcs_and(om_ok, ph_ok, .cpcs_climate(x, "not_arid"))
  .cpcs_sub_result("humic_weakly_developed_soils_cpcs", p, x,
                   list(om_5_in_A = om_ok, ph_under_6_8 = ph_ok), "II-2, p. 23")
}

#' @rdname cpcs1967_subclasses
#' @export
xeric_weakly_developed_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # a dry climate, poor in OM (under 2 % in the A), no calcareous
  # accumulation horizon (p. 24; Hugo, 2026-11-01)
  a <- .cpcs_a_rows(x)
  om <- if (length(a)) x$om[a] else NA_real_
  poor <- if (!length(a)) NA else .cpcs_all(ifelse(is.na(om), NA, om < 2))
  below <- setdiff(which(!(x$organic %in% TRUE)), a)
  cc <- .cpcs_num(x$h, "caco3_pct")
  accum <- any(x$des$ca[below] %in% TRUE) || isTRUE(tryCatch(calcic(pedon)$passed, error = function(e) NA))
  p <- .cpcs_and(.cpcs_climate(x, "dry"), poor, !accum)
  .cpcs_sub_result("xeric_weakly_developed_soils_cpcs", p, x,
                   list(om_under_2_in_A = poor, calcareous_accumulation = accum), "II-3, p. 24")
}

# ---- III ------------------------------------------------------------------------

.CPCS_FLAT_RE <- "depression|plain|basin|valley floor|bottom|flat|playa|pan\\b|cuvette|plaine|bas-fond|bassin|d\u00e9pression"
.CPCS_SLOPE_RE <- "hill|slope|ridge|summit|shoulder|backslope|footslope|mountain|pente|versant|colline|butte"

#' @rdname cpcs1967_subclasses
#' @export
vertisols_reduced_external_drainage_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  s <- x$site
  # external drainage nil or reduced (p. 27): a flat or low landform, or a
  # slope under 2 %; a slope of 2 % or more or a sloping landform is not;
  # nothing recorded is undecided (Hugo, 2026-11-01)
  sl <- suppressWarnings(as.numeric(as.character(s$slope_pct %||% NA)[1L]))
  lf <- tolower(paste(as.character(unlist(c(s$landform, s$landform_element))), collapse = " "))
  p <- if (!is.na(sl)) sl < 2 else if (grepl(.CPCS_FLAT_RE, lf)) TRUE else if (grepl(.CPCS_SLOPE_RE, lf)) FALSE else NA
  .cpcs_sub_result("vertisols_reduced_external_drainage_cpcs", p, x, list(slope_pct = sl, landform = lf), "III-1, p. 28")
}

# ---- IV -------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
cold_country_andosols_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # the cold countries: OM above 10 % in the A over 25 cm or more (p. 33;
  # Hugo, 2026-11-01)
  rows <- .cpcs_a_rows(x)
  th <- sum(vapply(rows, function(i) if (!is.na(x$om[i]) && x$om[i] > 10 && !is.na(x$dbot[i])) max(0, x$dbot[i] - x$dtop[i]) else 0, 0), na.rm = TRUE)
  p <- if (th >= 25) TRUE else if (length(rows) && any(is.na(x$om[rows]))) NA else FALSE
  .cpcs_sub_result("cold_country_andosols_cpcs", p, x, list(cm_above_10_pct_om = th), "IV-1, p. 33")
}

# ---- V --------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
gypseous_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # gypsum in the A: 5 % or more, or visible gypsum, or a Cs of the notation
  # where gypsum is not measured (p. 38; Hugo, 2026-11-01)
  a <- .cpcs_a_rows(x)
  g <- .cpcs_num(x$h, "caso4_pct")[a]; vis <- .cpcs_num(x$h, "gypsum_visible_pct")[a]
  v <- ifelse(!is.na(g), g >= 5, ifelse(!is.na(vis), vis > 0, ifelse(x$des$cs[a] %in% TRUE, TRUE, NA)))
  p <- if (!length(a)) NA else if (any(v %in% TRUE)) TRUE else if (all(is.na(g)) && all(is.na(vis))) FALSE else .cpcs_any(v)
  .cpcs_sub_result("gypseous_soils_cpcs", p, x, where = "V-3, p. 38")
}

#' @rdname cpcs1967_subclasses
#' @export
carbonated_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # carbonate in the fine earth of the A: 1 % or more, or, with no value,
  # an effervescence moderate or stronger (p. 36; Hugo, 2026-11-01)
  a <- .cpcs_a_rows(x)
  cc <- .cpcs_num(x$h, "caco3_pct")[a]
  ef <- .scsj_txt(x$h, "effervescence_class")[a]
  efv <- ifelse(is.na(ef), NA, grepl("moderate|strong|violent|forte|moyenne", ef) & !grepl("^(none|no|not|non)", ef))
  v <- ifelse(!is.na(cc), cc >= 1, ifelse(!is.na(efv), efv, ifelse(x$des$ca[a] %in% TRUE, TRUE, NA)))
  p <- if (!length(a)) NA else .cpcs_any(v)
  .cpcs_sub_result("carbonated_soils_cpcs", p, x, where = "V-1, p. 36")
}

# ---- VI -------------------------------------------------------------------------

# The VI-1 property test (p. 40): the upper horizons partly desaturated
# (S/T 50 to under 80 %), 80 % or more at depth.
.cpcs_vi1_props <- function(x) {
  bs <- .cpcs_bs(x)
  a <- .cpcs_a_rows(x); b <- .cpcs_b_rows(x)
  if (!length(a) || !length(b)) return(NA)
  up <- .cpcs_all(ifelse(is.na(bs[a]), NA, bs[a] >= 50 & bs[a] < 80))
  deep <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs >= 80), b)
  .cpcs_and(up, deep)
}

#' @rdname cpcs1967_subclasses
#' @export
isohumic_humid_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  p <- .cpcs_or(.cpcs_climate(x, "vi1", none = NA), .cpcs_vi1_props(x))
  if (!is.na(.cpcs_koppen(x$site)) && isFALSE(.cpcs_climate(x, "vi1"))) p <- FALSE
  .cpcs_sub_result("isohumic_humid_cpcs", p, x, list(desaturated_A = .cpcs_vi1_props(x)), "VI-1, p. 40")
}

#' @rdname cpcs1967_subclasses
#' @export
isohumic_very_cold_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("isohumic_very_cold_cpcs", .cpcs_climate(x, "vi2", none = NA), x, where = "VI-2, p. 41")
}

#' @rdname cpcs1967_subclasses
#' @export
isohumic_cool_rainy_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("isohumic_cool_rainy_cpcs", .cpcs_climate(x, "vi3", none = NA), x, where = "VI-3, p. 42")
}

#' @rdname cpcs1967_subclasses
#' @export
isohumic_hot_rainy_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("isohumic_hot_rainy_cpcs", .cpcs_climate(x, "vi4", none = NA), x, where = "VI-4, p. 43")
}

# ---- VII ------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
brunified_tropical_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # a tropical climate and the tropical eutrophic brown soils of VII/41: a
  # B or (B) whose larger part has base saturation of 50 % or more (p. 48)
  bs <- .cpcs_bs(x)
  eu <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs >= 50))
  p <- .cpcs_and(.cpcs_climate(x, "tropical"), ty$b_any, eu)
  .cpcs_sub_result("brunified_tropical_cpcs", p, x, list(eutrophic_B = eu), "VII-4, p. 48")
}

#' @rdname cpcs1967_subclasses
#' @export
brunified_boreal_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # a boreal climate and the boreal leached soils of VII/31: an A2 over a
  # textural B (p. 48)
  p <- .cpcs_and(.cpcs_climate(x, "boreal"), ty$e, .cpcs_textural_b(x, pedon))
  .cpcs_sub_result("brunified_boreal_cpcs", p, x, list(A2 = ty$e), "VII-3, p. 48")
}

#' @rdname cpcs1967_subclasses
#' @export
brunified_continental_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  ty <- .cpcs_horizon_types(x, pedon)
  # a continental temperate climate (Koppen only: the USDA regimes do not
  # tell it from the humid one) and the grey forest or sod-podzolic soils of
  # VII/21-22: an A2 over a textural B (pp. 47-48)
  p <- .cpcs_and(.cpcs_climate(x, "continental"), ty$e, .cpcs_textural_b(x, pedon))
  .cpcs_sub_result("brunified_continental_cpcs", p, x, list(A2 = ty$e), "VII-2, p. 47")
}

# ---- VIII -----------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
podzolised_hydromorphic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # hydromorphy over the whole profile: a hydromorphic layer in the A2 or the
  # top of the B, or a gley above the B (p. 50; slight signs below the B stay
  # in the other sub-classes)
  hyd <- .cpcs_hydromorphic_rows(x)
  rows <- x$ord[!(x$organic[x$ord] %in% TRUE)]
  b <- .cpcs_b_rows(x)
  b1 <- if (length(b)) b[which.min(x$dtop[b])] else integer(0)
  top_b <- if (length(b1)) x$dtop[b1] else Inf
  e <- rows[x$role[rows] %in% "E"]
  above <- rows[!is.na(x$dtop[rows]) & x$dtop[rows] < top_b & x$role[rows] %in% c("G", "E")]
  cand <- unique(c(e, b1, above))
  p <- if (!length(cand)) FALSE else .cpcs_any(hyd[cand])
  .cpcs_sub_result("podzolised_hydromorphic_cpcs", p, x, where = "VIII-3, p. 50")
}

#' @rdname cpcs1967_subclasses
#' @export
podzols_cold_climate_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("podzols_cold_climate_cpcs", .cpcs_climate(x, "cold"), x, where = "VIII-2, p. 53")
}

# ---- IX -------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
ferruginous_tropical_soils_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  # base saturation in the B under 65 % (p. 57); without it, the yellow B of
  # the ferruginous soils (10YR or 7.5YR) against the red one (10R to 5YR)
  bs <- .cpcs_bs(x)
  v <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs < 65))
  if (is.na(v)) {
    hn <- .scsj_hue_num(as.character(.scsj_col(x$h, "munsell_hue_moist", NA_character_)))
    v <- .cpcs_larger_part_b(x, ifelse(is.na(hn), NA, hn > 15))
  }
  .cpcs_sub_result("ferruginous_tropical_soils_cpcs", v, x, where = "IX-1, pp. 57-58")
}

# ---- X --------------------------------------------------------------------------

# The base saturation of the (B), in the larger part of its first 100 cm,
# in a band; without it, the sum of the bases S (Hugo, 2026-11-01).
.cpcs_x_band <- function(x, lo, hi, s_test) {
  bs <- .cpcs_bs(x)
  v <- .cpcs_larger_part_b(x, ifelse(is.na(bs), NA, bs >= lo & bs < hi))
  if (!is.na(v)) return(v)
  h <- x$h
  z <- function(v) ifelse(is.na(v), 0, v)
  s <- .cpcs_num(h, "ca_cmol") + .cpcs_num(h, "mg_cmol") + z(.cpcs_num(h, "k_cmol")) + z(.cpcs_num(h, "na_cmol"))
  .cpcs_larger_part_b(x, ifelse(is.na(s), NA, s_test(s)))
}

#' @rdname cpcs1967_subclasses
#' @export
ferrallitic_weakly_desaturated_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("ferrallitic_weakly_desaturated_cpcs",
                   .cpcs_x_band(x, 40, Inf, function(s) ifelse(s > 3, TRUE, ifelse(s < 1, FALSE, NA))), x,
                   where = "X-1, pp. 65, 67")
}

#' @rdname cpcs1967_subclasses
#' @export
ferrallitic_moderately_desaturated_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("ferrallitic_moderately_desaturated_cpcs",
                   .cpcs_x_band(x, 20, 40, function(s) ifelse(s > 3 | s < 1, FALSE, NA)), x,
                   where = "X-2, pp. 64-65, 68")
}

#' @rdname cpcs1967_subclasses
#' @export
ferrallitic_strongly_desaturated_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("ferrallitic_strongly_desaturated_cpcs",
                   .cpcs_x_band(x, -Inf, 20, function(s) ifelse(s < 1, TRUE, ifelse(s > 3, FALSE, NA))), x,
                   where = "X-3, pp. 64, 69")
}

# ---- XI -------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
hydromorphic_organic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  .cpcs_sub_result("hydromorphic_organic_cpcs", .cpcs_peat_route(x), x, where = "XI-1, p. 73")
}

#' @rdname cpcs1967_subclasses
#' @export
hydromorphic_moderately_organic_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  r <- hydromorphic_soils_cpcs(pedon)
  .cpcs_sub_result("hydromorphic_moderately_organic_cpcs", r$evidence$anmoor_over_gley %||% NA, x, where = "XI-2, p. 74")
}

# ---- XII ------------------------------------------------------------------------

#' @rdname cpcs1967_subclasses
#' @export
sodic_degraded_structure_cpcs <- function(pedon) {
  x <- .cpcs_profile(pedon)
  r <- sodic_soils_cpcs(pedon)
  .cpcs_sub_result("sodic_degraded_structure_cpcs", r$evidence$na_above_10_degraded %||% NA, x, where = "XII-2, p. 78")
}
