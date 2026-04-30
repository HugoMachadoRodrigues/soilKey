# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers Bloco B (v0.9.1).
#
# Adds the principal qualifiers required to fully wire the canonical
# Ch 4 lists for the next 5 RSGs of the key:
#
#   SN  Solonetz     VR  Vertisols    SC  Solonchaks
#   GL  Gleysols     AN  Andosols
#
# Bloco B's signature qualifiers are the salinity / clay-rich /
# wet / volcanic family. Three structural Vertisol surface qualifiers
# (Mazic, Grumic, Pellic) and seven Andosol active-component / depth /
# moisture qualifiers (Aluandic, Silandic, Hydric, Melanic, Acroxic,
# Pachic, Eutrosilic) are introduced here. The remaining additions
# are thin delegations to v0.3.x diagnostics that v0.9 had not yet
# wrapped (Chernic, Pisoplinthic, Abruptic, Aceric).
# ============================================================================


# ---------- THIN DELEGATIONS TO v0.3.x DIAGNOSTICS --------------------------

#' Chernic qualifier (ch): chernic horizon (intensely worm-mixed mollic-like)
#' within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_chernic     <- function(pedon) .q_presence("Chernic",     chernic(pedon),     100, pedon)

#' Pisoplinthic qualifier (px): pisoplinthic horizon within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pisoplinthic <- function(pedon) .q_presence("Pisoplinthic", pisoplinthic(pedon), 100, pedon)

#' Abruptic qualifier (ap): abrupt textural difference within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_abruptic    <- function(pedon) .q_presence("Abruptic",    abrupt_textural_difference(pedon), 100, pedon)


# ---------- ACID-SULFATE QUALIFIER ------------------------------------------

#' Aceric qualifier (ae): pH (1:1 H2O) <= 5 in some layer within the
#' upper 50 cm. Used for sub-aerially exposed acid-sulfate soils
#' (Solonchaks, Gleysols on former tidal flats). v0.9.1: numeric pH gate
#' only; v0.9.2 adds the cross-check against \code{thionic} / sulfidic
#' material to disambiguate from naturally acidic Histosols.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_aceric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Aceric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "ph_h2o",
            reference = "WRB (2022) Ch 5, Aceric"))
  ph <- h$ph_h2o[layers]
  ok <- !is.na(ph) & ph <= 5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Aceric", passed = passed,
    layers = layers[ok],
    evidence = list(ph_h2o = ph),
    missing = if (all(is.na(ph))) "ph_h2o" else character(0),
    reference = "WRB (2022) Ch 5, Aceric"
  )
}


# ---------- VERTISOL SURFACE-STRUCTURE QUALIFIERS ---------------------------
# These three are mutually exclusive in the strict WRB sense (the
# surface horizon has exactly one structure type). v0.9.1 returns each
# independently; for the canonical Ch 4 prefix the first to pass wins
# via the YAML-order resolver.

# Helper: indices of mineral layers reaching the surface (top_cm <= 5).
.surface_layer <- function(pedon) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm <= 5)
}

#' Mazic qualifier (mz): structureless / massive surface horizon
#' (Vertisol). Diagnostic of slaked, crusted Vertisol surfaces.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_mazic <- function(pedon) {
  h  <- pedon$horizons
  sl <- .surface_layer(pedon)
  if (length(sl) == 0L)
    return(DiagnosticResult$new(name = "Mazic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Mazic"))
  grade <- h$structure_grade[sl]
  type  <- h$structure_type[sl]
  ok <- (!is.na(grade) & grade %in% c("structureless", "massive")) |
          (!is.na(type) & grepl("massive", type, ignore.case = TRUE))
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Mazic", passed = passed,
    layers = sl[ok],
    evidence = list(structure_grade = grade, structure_type = type),
    missing = if (all(is.na(grade)) && all(is.na(type)))
                c("structure_grade", "structure_type") else character(0),
    reference = "WRB (2022) Ch 5, Mazic"
  )
}

#' Grumic qualifier (gr): strong fine granular surface horizon
#' (self-mulching Vertisol).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_grumic <- function(pedon) {
  h  <- pedon$horizons
  sl <- .surface_layer(pedon)
  if (length(sl) == 0L)
    return(DiagnosticResult$new(name = "Grumic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Grumic"))
  grade <- h$structure_grade[sl]
  type  <- h$structure_type[sl]
  size  <- h$structure_size[sl]
  ok <- !is.na(grade) & grade %in% c("strong", "moderate") &
          !is.na(type)  & grepl("granular", type, ignore.case = TRUE) &
          (is.na(size)  | size %in% c("very fine", "fine", "medium"))
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Grumic", passed = passed,
    layers = sl[ok],
    evidence = list(structure_grade = grade, structure_type = type,
                    structure_size = size),
    missing = if (all(is.na(grade)) && all(is.na(type)))
                c("structure_grade", "structure_type") else character(0),
    reference = "WRB (2022) Ch 5, Grumic"
  )
}

#' Pellic qualifier (pe): in the upper 30 cm, Munsell value <= 4 moist
#' AND chroma <= 2 moist. Diagnostic of "black" (dark) Vertisols.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pellic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm < 30)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Pellic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "munsell_value_moist",
            reference = "WRB (2022) Ch 5, Pellic"))
  vals <- h$munsell_value_moist[layers]
  chrs <- h$munsell_chroma_moist[layers]
  ok <- !is.na(vals) & !is.na(chrs) & vals <= 4 & chrs <= 2
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Pellic", passed = passed,
    layers = layers[ok],
    evidence = list(values = vals, chromas = chrs),
    missing = if (all(is.na(vals)) && all(is.na(chrs)))
                c("munsell_value_moist", "munsell_chroma_moist") else character(0),
    reference = "WRB (2022) Ch 5, Pellic"
  )
}


# ---------- ANDOSOL ACTIVE-COMPONENT QUALIFIERS -----------------------------
# Aluandic / Silandic split the andic active component by Al vs Si
# dominance. WRB 2022 uses the molar ratio Al / (Al + 0.5 * Si);
# Aluandic when Al / (Al + 0.5 * Si) >= 0.5 (Al-dominant) and Silandic
# otherwise. With Al = 26.98 g/mol and Si = 28.09 g/mol, the molar
# ratio reduces to mass ratio with a < 1% correction; v0.9.1 uses the
# mass-ratio expression directly. Both qualifiers require andic
# properties to pass first.

.al_si_dominance <- function(pedon) {
  ap <- andic_properties(pedon)
  if (!isTRUE(ap$passed))
    return(list(passed = FALSE, layers = integer(0), andic = ap,
                ratio = numeric(0)))
  h  <- pedon$horizons
  ly <- intersect(ap$layers, .in_upper(pedon, 100))
  al <- h$al_ox_pct[ly]
  si <- h$si_ox_pct[ly]
  # Mass ratio Al / (Al + 0.5 Si). >= 0.5 => Al-dominant => Aluandic.
  ratio <- al / (al + 0.5 * si)
  list(passed = TRUE, layers = ly, andic = ap,
       ratio = ratio, al = al, si = si)
}

#' Aluandic qualifier (aa): andic properties + Al-dominant active
#' component (Al / (Al + 0.5 Si) >= 0.5 in mass).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_aluandic <- function(pedon) {
  d <- .al_si_dominance(pedon)
  if (!isTRUE(d$passed))
    return(DiagnosticResult$new(name = "Aluandic", passed = FALSE,
            layers = integer(0), evidence = list(andic = d$andic),
            missing = d$andic$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Aluandic"))
  ok <- !is.na(d$ratio) & d$ratio >= 0.5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Aluandic", passed = passed,
    layers = d$layers[ok],
    evidence = list(andic = d$andic, ratio = d$ratio,
                    al_ox = d$al, si_ox = d$si),
    missing = if (all(is.na(d$ratio))) c("al_ox_pct", "si_ox_pct") else character(0),
    reference = "WRB (2022) Ch 5, Aluandic"
  )
}

#' Silandic qualifier (sn): andic properties + Si-dominant active
#' component (Al / (Al + 0.5 Si) < 0.5 in mass; allophane-rich).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_silandic <- function(pedon) {
  d <- .al_si_dominance(pedon)
  if (!isTRUE(d$passed))
    return(DiagnosticResult$new(name = "Silandic", passed = FALSE,
            layers = integer(0), evidence = list(andic = d$andic),
            missing = d$andic$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Silandic"))
  ok <- !is.na(d$ratio) & d$ratio < 0.5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Silandic", passed = passed,
    layers = d$layers[ok],
    evidence = list(andic = d$andic, ratio = d$ratio,
                    al_ox = d$al, si_ox = d$si),
    missing = if (all(is.na(d$ratio))) c("al_ox_pct", "si_ox_pct") else character(0),
    reference = "WRB (2022) Ch 5, Silandic"
  )
}


# ---------- ANDOSOL MOISTURE / DARKNESS / EXCHANGE QUALIFIERS ---------------

#' Hydric qualifier (hy): water content at 1500 kPa >= 100\% (undried
#' fine earth, WRB 2022). v0.9.1 accepts the air-dried equivalent
#' (>= 70\%) when the lab protocol pre-dries; the result is flagged as
#' "potentially over-permissive" via the \code{notes} field when the
#' value falls in the 70-100\% band.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hydric <- function(pedon) {
  ap <- andic_properties(pedon)
  if (!isTRUE(ap$passed))
    return(DiagnosticResult$new(name = "Hydric", passed = FALSE,
            layers = integer(0), evidence = list(andic = ap),
            missing = ap$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hydric"))
  h  <- pedon$horizons
  ly <- intersect(ap$layers, .in_upper(pedon, 100))
  w  <- h$water_content_1500kpa[ly]
  ok <- !is.na(w) & w >= 70
  passed <- any(ok)
  air_dried_band <- any(!is.na(w) & w >= 70 & w < 100)
  DiagnosticResult$new(
    name = "Hydric", passed = passed,
    layers = ly[ok],
    evidence = list(andic = ap, water_content_1500kpa = w),
    missing = if (all(is.na(w))) "water_content_1500kpa" else character(0),
    reference = "WRB (2022) Ch 5, Hydric",
    notes = if (passed && air_dried_band)
              "v0.9.1: 70-100% accepted as air-dried equivalent of WRB's >=100% undried"
            else NA_character_
  )
}

#' Melanic qualifier (me): andic + dark high-OC surface horizon.
#' v0.9.1: thickness >= 30 cm within upper 50 cm, OC weighted >= 6\%,
#' Munsell value <= 2 and chroma <= 2 (moist). Melanic Index >= 1.7
#' (the canonical UV-OD ratio) is deferred to v0.9.2.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_melanic <- function(pedon) {
  ap <- andic_properties(pedon)
  if (!isTRUE(ap$passed))
    return(DiagnosticResult$new(name = "Melanic", passed = FALSE,
            layers = integer(0), evidence = list(andic = ap),
            missing = ap$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Melanic"))
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  vals <- h$munsell_value_moist[layers]
  chrs <- h$munsell_chroma_moist[layers]
  oc   <- h$oc_pct[layers]
  thk  <- pmax(0, h$bottom_cm[layers] - h$top_cm[layers])
  ok <- !is.na(vals) & !is.na(chrs) & !is.na(oc) &
          vals <= 2 & chrs <= 2 & oc >= 6
  ok_layers <- layers[ok]
  thickness <- if (length(ok_layers) > 0L)
                 sum(h$bottom_cm[ok_layers] - h$top_cm[ok_layers],
                     na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(
    name = "Melanic", passed = passed,
    layers = if (passed) ok_layers else integer(0),
    evidence = list(andic = ap, oc_pct = oc, values = vals,
                    chromas = chrs, thickness_cm = thickness),
    missing = if (all(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Melanic",
    notes = "v0.9.1: Melanic Index (UV-OD) deferred to v0.9.2"
  )
}

#' Acroxic qualifier (ax): andic + extremely low effective exchange
#' complex (Ca + Mg + K + Na exch + 1 N KCl Al-exch <= 2 cmol+/kg fine
#' earth) in some layer of the andic part within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_acroxic <- function(pedon) {
  ap <- andic_properties(pedon)
  if (!isTRUE(ap$passed))
    return(DiagnosticResult$new(name = "Acroxic", passed = FALSE,
            layers = integer(0), evidence = list(andic = ap),
            missing = ap$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Acroxic"))
  h  <- pedon$horizons
  ly <- intersect(ap$layers, .in_upper(pedon, 100))
  ca <- h$ca_cmol[ly]; mg <- h$mg_cmol[ly]
  k  <- h$k_cmol[ly];  na <- h$na_cmol[ly]
  al <- h$al_kcl_cmol[ly]
  if (all(is.na(al))) al <- h$al_cmol[ly]   # fallback
  ecec <- ca + mg + k + na + ifelse(is.na(al), 0, al)
  ok <- !is.na(ecec) & ecec <= 2
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Acroxic", passed = passed,
    layers = ly[ok],
    evidence = list(andic = ap, ecec_proxy = ecec),
    missing = c(
      if (all(is.na(ca))) "ca_cmol" else character(0),
      if (all(is.na(mg))) "mg_cmol" else character(0),
      if (all(is.na(k)))  "k_cmol"  else character(0),
      if (all(is.na(na))) "na_cmol" else character(0),
      if (all(is.na(al))) "al_kcl_cmol" else character(0)
    ),
    reference = "WRB (2022) Ch 5, Acroxic"
  )
}

#' Pachic qualifier (pc): mollic OR umbric horizon >= 50 cm thick.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pachic <- function(pedon) {
  mo <- mollic(pedon)
  um <- umbric_horizon(pedon)
  h  <- pedon$horizons
  thick <- function(d) {
    if (!isTRUE(d$passed)) return(0)
    ly <- d$layers
    sum(h$bottom_cm[ly] - h$top_cm[ly], na.rm = TRUE)
  }
  t_mo <- thick(mo); t_um <- thick(um)
  passed <- (t_mo >= 50) || (t_um >= 50)
  layers <- if (t_mo >= 50) mo$layers
            else if (t_um >= 50) um$layers
            else integer(0)
  DiagnosticResult$new(
    name = "Pachic", passed = passed,
    layers = layers,
    evidence = list(mollic = mo, umbric = um,
                    thickness_mollic_cm = t_mo,
                    thickness_umbric_cm = t_um),
    missing = unique(c(mo$missing, um$missing)),
    reference = "WRB (2022) Ch 5, Pachic"
  )
}

#' Eutrosilic qualifier (es): silandic + base saturation >= 50\% in some
#' layer of the silandic part within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_eutrosilic <- function(pedon) {
  si <- qual_silandic(pedon)
  if (!isTRUE(si$passed))
    return(DiagnosticResult$new(name = "Eutrosilic", passed = FALSE,
            layers = integer(0), evidence = list(silandic = si),
            missing = si$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Eutrosilic"))
  h  <- pedon$horizons
  ly <- si$layers
  bs <- h$bs_pct[ly]
  ok <- !is.na(bs) & bs >= 50
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Eutrosilic", passed = passed,
    layers = ly[ok],
    evidence = list(silandic = si, bs_pct = bs),
    missing = if (all(is.na(bs))) "bs_pct" else character(0),
    reference = "WRB (2022) Ch 5, Eutrosilic"
  )
}
