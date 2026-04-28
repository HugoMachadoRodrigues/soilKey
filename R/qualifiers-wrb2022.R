# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5, pp 127-156).
#
# v0.9 implements ~50 core qualifiers covering the most common usage
# across the 32 RSGs. The remaining ~150 sub-qualifiers (Hyper-, Hypo-,
# Proto-, Endo-, Epi-, Bathy-, Thapto-, Supra-, Ano-, Panto-, Poly-,
# Amphi-, Kato- variants and the Novic combinations) are scheduled for
# v0.9.1.
#
# Each qualifier function returns a DiagnosticResult: passed = TRUE if
# the qualifier applies to the pedon at the canonical reference depth
# (typically <= 100 cm from the mineral soil surface, per Ch 5 marker
# (2)). The qualifier system in classify_wrb2022() filters the per-RSG
# applicable list (Ch 4 tables) and formats the result as
#   "<Principal>(s) <RSG> (<Supplementary>(s))"
# per the rules of Ch 6, p 154.
# ============================================================================


# Helper: returns the layer indices that start within max_top_cm of the
# mineral soil surface. Used as a depth gate for almost every
# qualifier.
.in_upper <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
}

# Helper: build a DiagnosticResult for a thin "presence" qualifier.
.q_presence <- function(name, base_diag, max_top_cm = 100, pedon = NULL) {
  passed <- isTRUE(base_diag$passed) &&
              (is.null(pedon) ||
                 length(intersect(base_diag$layers,
                                      .in_upper(pedon, max_top_cm))) > 0L)
  layers <- if (passed) base_diag$layers else integer(0)
  DiagnosticResult$new(
    name = name, passed = passed, layers = layers,
    evidence = list(base = base_diag),
    missing = base_diag$missing %||% character(0),
    reference = "WRB (2022) Ch 5"
  )
}


# ---------- HORIZON-BASED PRINCIPAL QUALIFIERS ------------------------------

#' Albic qualifier (ab): albic horizon <= 100 cm.
#' @export
qual_albic       <- function(pedon) .q_presence("Albic",       albic(pedon),       100, pedon)

#' Andic qualifier (an): andic OR vitric properties combined >= 30 cm.
#' v0.9 simplification: passes if andic_properties or vitric_properties
#' passes within 100 cm.
#' @export
qual_andic       <- function(pedon) {
  ap <- andic_properties(pedon); vp <- vitric_properties(pedon)
  passed <- (isTRUE(ap$passed) || isTRUE(vp$passed)) &&
              length(intersect(union(ap$layers, vp$layers),
                                   .in_upper(pedon, 100))) > 0L
  DiagnosticResult$new(
    name = "Andic", passed = passed,
    layers = union(ap$layers, vp$layers),
    evidence = list(andic = ap, vitric = vp),
    missing = unique(c(ap$missing, vp$missing)),
    reference = "WRB (2022) Ch 5, Andic"
  )
}

#' Anthric qualifier (ak): anthric properties.
#' @export
qual_anthric     <- function(pedon) .q_presence("Anthric",     anthric_horizons(pedon), 100, pedon)

#' Calcic qualifier (cc): calcic horizon <= 100 cm.
#' @export
qual_calcic      <- function(pedon) .q_presence("Calcic",      calcic(pedon),      100, pedon)

#' Cambic qualifier (cm): cambic horizon <= 50 cm.
#' @export
qual_cambic      <- function(pedon) .q_presence("Cambic",      cambic(pedon),      50,  pedon)

#' Cryic qualifier (cy): cryic horizon <= 100 cm.
#' @export
qual_cryic       <- function(pedon) .q_presence("Cryic",       cryic_conditions(pedon), 100, pedon)

#' Duric qualifier (du): duric horizon <= 100 cm.
#' @export
qual_duric       <- function(pedon) .q_presence("Duric",       duric_horizon(pedon),    100, pedon)

#' Ferralic qualifier (fl): ferralic horizon <= 150 cm.
#' @export
qual_ferralic    <- function(pedon) .q_presence("Ferralic",    ferralic(pedon),    150, pedon)

#' Ferric qualifier (fr): ferric horizon <= 100 cm.
#' @export
qual_ferric      <- function(pedon) .q_presence("Ferric",      ferric(pedon),      100, pedon)

#' Fluvic qualifier (fv): fluvic material >= 25 cm thick starting <= 75 cm.
#' @export
qual_fluvic      <- function(pedon) .q_presence("Fluvic",      fluvic_material(pedon),  75,  pedon)

#' Folic qualifier (fo): folic horizon at the soil surface. v0.9
#' delegates to histic_horizon with surface-only filter.
#' @export
qual_folic       <- function(pedon) {
  h <- histic_horizon(pedon)
  surface <- length(h$layers) > 0L &&
               any(pedon$horizons$top_cm[h$layers] <= 5, na.rm = TRUE)
  passed <- isTRUE(h$passed) && surface
  DiagnosticResult$new(
    name = "Folic", passed = passed,
    layers = if (passed) h$layers else integer(0),
    evidence = list(histic = h),
    missing = h$missing,
    reference = "WRB (2022) Ch 5, Folic"
  )
}

#' Gleyic qualifier (gl): gleyic properties throughout a layer >= 25 cm
#' starting <= 75 cm + reducing conditions.
#' @export
qual_gleyic      <- function(pedon) .q_presence("Gleyic",      gleyic_properties(pedon), 75, pedon)

#' Gypsic qualifier (gy): gypsic horizon <= 100 cm.
#' @export
qual_gypsic      <- function(pedon) .q_presence("Gypsic",      gypsic(pedon),      100, pedon)

#' Histic qualifier (hi): histic horizon at or near the surface.
#' @export
qual_histic      <- function(pedon) .q_presence("Histic",      histic_horizon(pedon),   100, pedon)

#' Leptic qualifier (le): continuous rock <= 100 cm.
#' @export
qual_leptic      <- function(pedon) .q_presence("Leptic",      continuous_rock(pedon),  100, pedon)

#' Mollic qualifier (mo): mollic horizon.
#' @export
qual_mollic      <- function(pedon) .q_presence("Mollic",      mollic(pedon),      100, pedon)

#' Natric qualifier (na): natric horizon <= 100 cm.
#' @export
qual_natric      <- function(pedon) .q_presence("Natric",      natric_horizon(pedon),   100, pedon)

#' Nitic qualifier (ni): nitic horizon <= 100 cm.
#' @export
qual_nitic       <- function(pedon) .q_presence("Nitic",       nitic_horizon(pedon),    100, pedon)

#' Petrocalcic qualifier (pc): petrocalcic horizon <= 100 cm.
#' @export
qual_petrocalcic <- function(pedon) .q_presence("Petrocalcic", petrocalcic(pedon),      100, pedon)

#' Petroduric qualifier (pd): petroduric horizon <= 100 cm.
#' @export
qual_petroduric  <- function(pedon) .q_presence("Petroduric",  petroduric(pedon),       100, pedon)

#' Petrogypsic qualifier (pg): petrogypsic horizon <= 100 cm.
#' @export
qual_petrogypsic <- function(pedon) .q_presence("Petrogypsic", petrogypsic(pedon),      100, pedon)

#' Petroplinthic qualifier (pp): petroplinthic horizon <= 100 cm.
#' @export
qual_petroplinthic <- function(pedon) .q_presence("Petroplinthic", petroplinthic(pedon), 100, pedon)

#' Plinthic qualifier (pl): plinthic horizon <= 100 cm.
#' @export
qual_plinthic    <- function(pedon) .q_presence("Plinthic",    plinthic(pedon),    100, pedon)

#' Retic qualifier (rt): retic properties <= 100 cm.
#' @export
qual_retic       <- function(pedon) .q_presence("Retic",       retic_properties(pedon), 100, pedon)

#' Salic qualifier (sz): salic horizon <= 100 cm.
#' @export
qual_salic       <- function(pedon) .q_presence("Salic",       salic(pedon),       100, pedon)

#' Spodic qualifier (sd): spodic horizon <= 200 cm.
#' @export
qual_spodic      <- function(pedon) .q_presence("Spodic",      spodic(pedon),      200, pedon)

#' Stagnic qualifier (st): stagnic properties <= 75 cm.
#' @export
qual_stagnic     <- function(pedon) .q_presence("Stagnic",     stagnic_properties(pedon), 75, pedon)

#' Umbric qualifier (um): umbric horizon.
#' @export
qual_umbric      <- function(pedon) .q_presence("Umbric",      umbric_horizon(pedon),   100, pedon)

#' Vertic qualifier (vr): vertic horizon <= 100 cm.
#' @export
qual_vertic      <- function(pedon) .q_presence("Vertic",      vertic_horizon(pedon),   100, pedon)

#' Vitric qualifier (vi): vitric properties >= 30 cm within 100 cm.
#' @export
qual_vitric      <- function(pedon) .q_presence("Vitric",      vitric_properties(pedon), 100, pedon)


# ---------- CHEMISTRY-BASED PRINCIPAL QUALIFIERS ----------------------------

#' Acric qualifier (ac): argic horizon + low CEC + high Al.
#' v0.9: argic + CEC < 24 cmolc/kg clay + exch Al > Ca+Mg+K+Na.
#' @export
qual_acric <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Acric", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing, reference = "WRB (2022) Ch 5"))
  ac <- acrisol(pedon)
  passed <- isTRUE(ac$passed)
  DiagnosticResult$new(name = "Acric", passed = passed,
    layers = ac$layers, evidence = list(acrisol = ac),
    missing = ac$missing, reference = "WRB (2022) Ch 5, Acric")
}

#' Alic qualifier (al): argic + high CEC + high Al saturation.
#' @export
qual_alic <- function(pedon) {
  al <- alisol(pedon)
  DiagnosticResult$new(name = "Alic", passed = al$passed,
    layers = al$layers, evidence = list(alisol = al),
    missing = al$missing, reference = "WRB (2022) Ch 5, Alic")
}

#' Lixic qualifier (lx): argic + low CEC, low Al.
#' @export
qual_lixic <- function(pedon) {
  lx <- lixisol(pedon)
  DiagnosticResult$new(name = "Lixic", passed = lx$passed,
    layers = lx$layers, evidence = list(lixisol = lx),
    missing = lx$missing, reference = "WRB (2022) Ch 5, Lixic")
}

#' Luvic qualifier (lv): argic + high CEC, low Al saturation.
#' @export
qual_luvic <- function(pedon) {
  lv <- luvisol(pedon)
  DiagnosticResult$new(name = "Luvic", passed = lv$passed,
    layers = lv$layers, evidence = list(luvisol = lv),
    missing = lv$missing, reference = "WRB (2022) Ch 5, Luvic")
}

#' Dystric qualifier (dy): low base saturation throughout. v0.9: BS <
#' 50% from 20 to 100 cm in mineral material.
#' @export
qual_dystric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 20 & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Dystric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Dystric"))
  bs <- h$bs_pct[layers]
  passed <- length(bs) > 0L && all(!is.na(bs) & bs < 50)
  DiagnosticResult$new(name = "Dystric", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(bs_values = bs),
    missing = if (any(is.na(bs))) "bs_pct" else character(0),
    reference = "WRB (2022) Ch 5, Dystric")
}

#' Eutric qualifier (eu): high base saturation. v0.9: BS >= 50%
#' throughout 20-100 cm.
#' @export
qual_eutric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 20 & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Eutric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Eutric"))
  bs <- h$bs_pct[layers]
  passed <- length(bs) > 0L && all(!is.na(bs) & bs >= 50)
  DiagnosticResult$new(name = "Eutric", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(bs_values = bs),
    missing = if (any(is.na(bs))) "bs_pct" else character(0),
    reference = "WRB (2022) Ch 5, Eutric")
}

#' Magnesic qualifier (mg): exchangeable Ca/Mg < 1 in upper 100 cm.
#' @export
qual_magnesic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Magnesic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("ca_cmol", "mg_cmol"),
            reference = "WRB (2022) Ch 5, Magnesic"))
  ca <- h$ca_cmol[layers]; mg <- h$mg_cmol[layers]
  ratios <- ca / mg
  passed <- any(!is.na(ratios) & ratios < 1)
  DiagnosticResult$new(name = "Magnesic", passed = passed,
    layers = layers[which(!is.na(ratios) & ratios < 1)],
    evidence = list(ca_mg_ratios = ratios),
    missing = if (any(is.na(ratios))) c("ca_cmol", "mg_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Magnesic")
}

#' Sodic qualifier (so): ESP >= 6% (incl. SAR-derived).
#' @export
qual_sodic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Sodic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Sodic"))
  na_pct <- vapply(layers, function(i) {
    if (is.na(h$na_cmol[i]) || is.na(h$cec_cmol[i]) || h$cec_cmol[i] <= 0)
      NA_real_
    else h$na_cmol[i] / h$cec_cmol[i] * 100
  }, numeric(1))
  passed <- any(!is.na(na_pct) & na_pct >= 6)
  DiagnosticResult$new(name = "Sodic", passed = passed,
    layers = layers[which(!is.na(na_pct) & na_pct >= 6)],
    evidence = list(esp = na_pct),
    missing = if (any(is.na(na_pct))) c("na_cmol", "cec_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Sodic")
}


# ---------- COLOUR-BASED PRINCIPAL QUALIFIERS -------------------------------

#' Rhodic qualifier (ro): hue redder than 5YR + value < 4 + dry no
#' more than 1 unit higher than moist (in upper subsoil 25-150 cm).
#' @export
qual_rhodic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 150)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Rhodic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Rhodic"))
  hues <- h$munsell_hue_moist[layers]
  vals <- h$munsell_value_moist[layers]
  ok <- vapply(seq_along(layers), function(i) {
    hu <- hues[i]; v <- vals[i]
    if (is.na(hu) || is.na(v)) return(FALSE)
    grepl("^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b", hu, ignore.case = TRUE) &&
      v < 4
  }, logical(1))
  passed <- any(ok)
  DiagnosticResult$new(name = "Rhodic", passed = passed,
    layers = layers[ok],
    evidence = list(hues = hues, values = vals),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Rhodic")
}

#' Chromic qualifier (cr): hue redder than 7.5YR + chroma > 4 (in upper
#' subsoil 25-150 cm).
#' @export
qual_chromic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 150)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Chromic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Chromic"))
  hues <- h$munsell_hue_moist[layers]
  chrs <- h$munsell_chroma_moist[layers]
  ok <- vapply(seq_along(layers), function(i) {
    hu <- hues[i]; c <- chrs[i]
    if (is.na(hu) || is.na(c)) return(FALSE)
    grepl("^(5YR|2\\.5YR|10R|7\\.5R|5R|2\\.5R)\\b", hu, ignore.case = TRUE) && c > 4
  }, logical(1))
  passed <- any(ok)
  DiagnosticResult$new(name = "Chromic", passed = passed,
    layers = layers[ok],
    evidence = list(hues = hues, chromas = chrs),
    missing = if (all(is.na(hues))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Chromic")
}

#' Xanthic qualifier (xa): ferralic + hue 7.5YR or yellower + value >=
#' 4 + chroma >= 5.
#' @export
qual_xanthic <- function(pedon) {
  fr <- ferralic(pedon)
  if (!isTRUE(fr$passed))
    return(DiagnosticResult$new(name = "Xanthic", passed = FALSE,
            layers = integer(0), evidence = list(ferralic = fr),
            missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic"))
  h <- pedon$horizons
  layers <- fr$layers
  hues <- h$munsell_hue_moist[layers]
  vals <- h$munsell_value_moist[layers]
  chrs <- h$munsell_chroma_moist[layers]
  ok <- vapply(seq_along(layers), function(i) {
    hu <- hues[i]; v <- vals[i]; c <- chrs[i]
    if (is.na(hu) || is.na(v) || is.na(c)) return(FALSE)
    grepl("^(7\\.5YR|10YR|2\\.5Y|5Y)\\b", hu, ignore.case = TRUE) &&
      v >= 4 && c >= 5
  }, logical(1))
  passed <- any(ok)
  DiagnosticResult$new(name = "Xanthic", passed = passed,
    layers = layers[ok],
    evidence = list(ferralic = fr, hues = hues),
    missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic")
}


# ---------- TEXTURE / SKELETIC QUALIFIERS ----------------------------------

#' Arenic qualifier (ar): texture sand or loamy sand >= 30 cm in <= 100 cm.
#' @export
qual_arenic <- function(pedon) .q_presence("Arenic", arenic_texture(pedon), 100, pedon)

#' Clayic qualifier (ce): clay >= 60% texture for a layer >= 30 cm in
#' the upper 100 cm.
#' @export
qual_clayic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 &
                    !is.na(h$clay_pct) & h$clay_pct >= 60)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Clayic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Clayic")
}

#' Loamic qualifier (lo): loam-class texture >= 30 cm in the upper 100 cm.
#' @export
qual_loamic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 &
                    !is.na(h$clay_pct) & !is.na(h$silt_pct) & !is.na(h$sand_pct) &
                    h$clay_pct >= 8 & h$clay_pct < 40 &
                    h$silt_pct >= 15)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Loamic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Loamic")
}

#' Siltic qualifier (sl): silt or silt-loam texture >= 30 cm in the upper
#' 100 cm.
#' @export
qual_siltic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 &
                    !is.na(h$clay_pct) & !is.na(h$silt_pct) & !is.na(h$sand_pct) &
                    h$clay_pct < 35 & h$silt_pct >= 50)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Siltic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Siltic")
}

#' Skeletic qualifier (sk): coarse fragments >= 40% averaged over 100 cm.
#' @export
qual_skeletic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Skeletic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "coarse_fragments_pct",
            reference = "WRB (2022) Ch 5, Skeletic"))
  cf <- h$coarse_fragments_pct[layers]
  passed <- any(!is.na(cf) & cf >= 40)
  DiagnosticResult$new(name = "Skeletic", passed = passed,
    layers = layers[which(!is.na(cf) & cf >= 40)],
    evidence = list(coarse_fragments = cf),
    missing = if (any(is.na(cf))) "coarse_fragments_pct" else character(0),
    reference = "WRB (2022) Ch 5, Skeletic")
}


# ---------- ORGANIC / HUMUS QUALIFIERS --------------------------------------

#' Humic qualifier (hu): >= 1% SOC in upper 50 cm (weighted average).
#' @export
qual_humic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Humic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "oc_pct", reference = "WRB (2022) Ch 5, Humic"))
  oc <- h$oc_pct[layers]
  thk <- h$bottom_cm[layers] - h$top_cm[layers]
  if (all(is.na(oc)))
    return(DiagnosticResult$new(name = "Humic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "oc_pct", reference = "WRB (2022) Ch 5, Humic"))
  weighted <- sum(oc * thk, na.rm = TRUE) / sum(thk, na.rm = TRUE)
  passed <- !is.na(weighted) && weighted >= 1
  DiagnosticResult$new(name = "Humic", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(oc_weighted = weighted),
    missing = if (any(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Humic")
}

#' Ochric qualifier (oh): SOC >= 0.2% upper 10 cm + no mollic/umbric.
#' @export
qual_ochric <- function(pedon) {
  has_mollic <- isTRUE(mollic(pedon)$passed)
  has_umbric <- isTRUE(umbric_horizon(pedon)$passed)
  if (has_mollic || has_umbric)
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0),
            evidence = list(mollic = has_mollic, umbric = has_umbric),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Ochric"))
  h <- pedon$horizons
  surface <- which(!is.na(h$top_cm) & h$top_cm <= 10)
  if (length(surface) == 0L)
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Ochric"))
  oc <- h$oc_pct[surface]
  passed <- any(!is.na(oc) & oc >= 0.2)
  DiagnosticResult$new(name = "Ochric", passed = passed,
    layers = surface[which(!is.na(oc) & oc >= 0.2)],
    evidence = list(oc = oc),
    missing = if (all(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Ochric")
}


# ---------- GENERIC CATCH-ALL ----------------------------------------------

#' Haplic qualifier (ha): no other principal qualifier of the RSG
#' applies. Always passes; the qualifier resolution machinery uses it
#' as the default when no other qualifier matched.
#' @export
qual_haplic <- function(pedon) {
  DiagnosticResult$new(name = "Haplic", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Haplic (default)")
}


# ---------- v0.9 RESOLUTION ENGINE ------------------------------------------

#' Resolve WRB qualifiers for an assigned RSG
#'
#' Given a pedon and the RSG code (e.g. "FR" for Ferralsols), returns a
#' list of applied qualifiers per the WRB 2022 Ch 4 RSG-specific
#' principal-qualifier list (read from
#' \code{inst/rules/wrb2022/qualifiers.yaml}). Principal qualifiers
#' that pass are returned in the canonical Ch 4 order; if none pass,
#' \code{Haplic} is returned.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code Two-letter RSG code (e.g. "FR", "PZ").
#' @param rules Optional pre-loaded WRB 2022 rules.
#' @return List with \code{principal} (character vector of qualifier
#'         names in canonical order) and \code{trace} (per-qualifier
#'         passed/missing).
#' @export
# v0.9.2 family-suppression table.
# When several qualifiers from the same family pass for the same RSG,
# WRB convention is to print only the most-specific one. Each entry is
# ordered from most-specific (kept) to least-specific (suppressed).
.wrb_qualifier_families <- list(
  salinity   = c("Hypersalic",  "Salic",        "Hyposalic"),
  sodicity   = c("Hypersodic",  "Sodic",        "Hyposodic"),
  calcic     = c("Hypercalcic", "Calcic",       "Hypocalcic",  "Protocalcic"),
  gypsic     = c("Hypergypsic", "Gypsic",       "Hypogypsic",  "Protogypsic"),
  vertic     = c("Vertic",      "Protovertic"),
  albic      = c("Hyperalbic",  "Albic"),
  skeletic   = c("Hyperskeletic", "Skeletic"),
  eutric     = c("Hypereutric", "Eutric"),
  dystric    = c("Hyperdystric", "Dystric"),
  alic       = c("Hyperalic",   "Alic")
)

# Drop suppressed siblings within each family while preserving the
# original YAML order of the surviving names.
.suppress_qualifier_siblings <- function(matched) {
  if (length(matched) <= 1L) return(matched)
  drop <- character(0)
  for (family in .wrb_qualifier_families) {
    in_match <- intersect(family, matched)
    if (length(in_match) > 1L) {
      keeper <- in_match[which.min(match(in_match, family))]
      drop <- c(drop, setdiff(in_match, keeper))
    }
  }
  setdiff(matched, drop)
}

resolve_wrb_qualifiers <- function(pedon, rsg_code, rules = NULL) {
  rules <- rules %||% load_rules("wrb2022")
  qfile <- system.file("rules/wrb2022/qualifiers.yaml",
                          package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  if (!file.exists(qfile)) {
    return(list(principal = character(0), trace = list(),
                  note = "qualifiers.yaml not found"))
  }
  qrules <- yaml::read_yaml(qfile)
  per_rsg <- qrules$rsg_qualifiers[[rsg_code]]
  if (is.null(per_rsg)) {
    return(list(principal = character(0), trace = list(),
                  note = sprintf("No qualifiers defined for RSG %s",
                                  rsg_code)))
  }

  trace <- list()
  matched <- character(0)
  for (qname in per_rsg$principal) {
    # v0.9.2.B: detect Ano-/Epi-/Endo-/Bathy-/Panto- specifier prefixes
    # and dispatch to the specifier handler instead of the bare lookup.
    spec <- .detect_specifier(qname)
    if (!is.null(spec)) {
      res <- tryCatch(
        .apply_specifier(pedon, spec$prefix, spec$base,
                          spec$min_top_cm, spec$max_top_cm),
        error = function(e) NULL)
      if (is.null(res)) {
        trace[[qname]] <- list(passed = NA,
                                  note = "specifier dispatch threw error")
        next
      }
      trace[[qname]] <- list(passed = res$passed,
                                missing = res$missing %||% character(0),
                                specifier = spec$prefix,
                                base = spec$base)
      if (isTRUE(res$passed)) matched <- c(matched, qname)
      next
    }

    fn_name <- paste0("qual_", tolower(qname))
    fn <- tryCatch(get(fn_name, envir = asNamespace("soilKey")),
                     error = function(e) NULL)
    if (is.null(fn)) {
      trace[[qname]] <- list(passed = NA,
                                note = "function not implemented in v0.9")
      next
    }
    res <- tryCatch(fn(pedon), error = function(e) NULL)
    if (is.null(res)) {
      trace[[qname]] <- list(passed = NA, note = "diagnostic threw error")
      next
    }
    trace[[qname]] <- list(passed = res$passed,
                              missing = res$missing %||% character(0))
    if (isTRUE(res$passed)) matched <- c(matched, qname)
  }
  matched <- .suppress_qualifier_siblings(matched)
  if (length(matched) == 0L) matched <- "Haplic"
  list(principal = matched, trace = trace)
}


#' Format a WRB 2022 soil name with qualifiers
#'
#' @param rsg_name Full RSG name (e.g. "Ferralsols").
#' @param principal Character vector of principal-qualifier names.
#' @param supplementary Character vector of supplementary-qualifier
#'        names (default empty in v0.9).
#' @return Formatted string per Ch 6 p 154 ("Rhodic Ferralsol (Clayic,
#'         Humic, Dystric)").
#' @export
format_wrb_name <- function(rsg_name, principal = character(0),
                              supplementary = character(0)) {
  rsg_singular <- sub("s$", "", rsg_name)
  prefix <- if (length(principal) > 0L)
              paste(principal, collapse = " ") else ""
  base <- paste(prefix, rsg_singular)
  base <- trimws(base)
  if (length(supplementary) > 0L) {
    suffix <- paste0(" (", paste(supplementary, collapse = ", "), ")")
    base <- paste0(base, suffix)
  }
  base
}
