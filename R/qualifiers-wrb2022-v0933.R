# =============================================================================
# v0.9.33 -- WRB 2022 qualifier closure (Ch 5 + Ch 6)
#
# Closes the 7 qualifiers identified by the v0.9.33 audit as referenced in
# qualifiers.yaml but lacking a backing function. After this release, 139/139
# unique qualifier names across the 32 RSGs have a backing qual_* function
# (100 % structural coverage).
#
# Each qualifier defined here cross-references the WRB 2022 Ch 5 (page-precise
# definition) and the RSG(s) where it is referenced as principal in Ch 4.
#
#   Endocalcic   (cam) -- depth-conditional calcic
#   Endogleyic   (eng) -- depth-conditional gleyic
#   Endostagnic  (ens) -- depth-conditional stagnic
#   Floatic      (fc)  -- specific gravity < 1 (organic floats on water)
#   Ombric       (om)  -- rain-fed Histosol (no extraneous water input)
#   Rheic        (rh)  -- water-fed Histosol (groundwater / surface water)
#   Toxic        (tx)  -- toxic levels of organic / inorganic constituents
# =============================================================================


# Helper: layer indices where the horizon FALLS WITHIN [min_top, max_top] cm
# of the mineral soil surface. The "Endo-" prefix in WRB 2022 is depth-
# conditional: the diagnostic must appear between 50 and 100 cm of the
# mineral surface (not within the upper 50). Used by Endocalcic /
# Endogleyic / Endostagnic.
.in_lower_subsoil <- function(pedon, min_top_cm = 50, max_top_cm = 100) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm >= min_top_cm & h$top_cm <= max_top_cm)
}


# Helper: an Endo- qualifier built on a diagnostic. WRB 2022 Ch 2.3.1, rule 2
# (a qualifier referring to a layer): "Endo-: the layer starts >= 50 cm from
# the (mineral) soil surface; and no such layer occurs < 50 cm" -- with the
# example "Endocalcic: the calcic horizon starts >= 50 and <= 100 cm".
# v0.9.217: the helper took any diagnostic layer starting in 50-100 cm, so a
# calcic or gleyic layer from 30 cm that went on below 50 cm also counted.
# It now reads where the uppermost diagnostic layer starts.
.q_endo_presence <- function(name, base_diag, pedon,
                                min_top_cm = 50, max_top_cm = 100) {
  ref <- sprintf("WRB (2022) Ch 5 and Ch 2.3.1, %s", name)
  if (!isTRUE(base_diag$passed) || !length(base_diag$layers))
    return(DiagnosticResult$new(
      name = name, passed = if (is.na(base_diag$passed)) NA else FALSE,
      layers = integer(0), evidence = list(base = base_diag),
      missing = base_diag$missing %||% character(0), reference = ref))
  top <- suppressWarnings(min(pedon$horizons$top_cm[base_diag$layers], na.rm = TRUE))
  passed <- is.finite(top) && top >= min_top_cm && top <= max_top_cm
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (passed) base_diag$layers else integer(0),
    evidence = list(base = base_diag, top_cm = top,
                      min_top_cm = min_top_cm, max_top_cm = max_top_cm),
    missing = base_diag$missing %||% character(0),
    reference = ref
  )
}


# ---- Endo-* qualifiers (subsoil-conditional, 50-100 cm) -------------------

#' Endocalcic qualifier (cam): calcic horizon between 50 and 100 cm.
#'
#' WRB 2022 Ch 5 (depth-conditional supplementary form of Calcic).
#' Referenced in Chernozems Ch 4. The diagnostic is the same as
#' Calcic; the difference is the depth band -- Endocalcic requires
#' the calcic horizon to start at >= 50 cm (deep, subsoil) rather
#' than within the upper 50 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_endocalcic <- function(pedon)
  .q_endo_presence("Endocalcic", calcic(pedon), pedon, 50, 100)

#' Endogleyic qualifier (eng): gleyic conditions between 50 and 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_endogleyic <- function(pedon)
  .q_endo_presence("Endogleyic", gleyic_properties(pedon), pedon, 50, 100)

#' Endostagnic qualifier (ens): stagnic conditions between 50 and 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_endostagnic <- function(pedon)
  .q_endo_presence("Endostagnic", stagnic_properties(pedon), pedon, 50, 100)


# ---- Histosol-specific qualifiers -----------------------------------------

#' Floatic qualifier (ft): organic material floating on water.
#'
#' WRB 2022 Ch 5 (p. 134): having organic material floating on water (in
#' Histosols only). Read from the record: \code{site$floating_organic_material}
#' (TRUE / FALSE), or, without it, a floating mat or mire named in
#' \code{site$landform} ("floating mat", "floating bog", "Schwingmoor",
#' "quaking bog"). Not recorded, it is absent (a field observation).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_floatic <- function(pedon) {
  # v0.9.414 (Hugo, 2026-11-19): only floating organic material recorded at
  # the site makes it. Until v0.9.413 any organic layer with a bulk density
  # <= 0.4 within 100 cm was taken to float, which named most peats Floatic
  # (the canonical Histosol among them).
  ref <- "WRB (2022) Ch 5, Floatic (p. 134)"
  s <- pedon$site %||% list()
  rec <- suppressWarnings(as.logical(s$floating_organic_material %||% NA)[1L])
  lf <- as.character(s$landform %||% NA)[1L]
  text <- !is.na(lf) &&
    grepl("float|schwingmoor|quaking\\s+(bog|mire|fen)", lf, ignore.case = TRUE)
  floating <- if (!is.na(rec)) rec else text
  om <- organic_material(pedon)
  passed <- if (!floating) FALSE else om$passed
  DiagnosticResult$new(
    name = "Floatic", passed = passed,
    layers = if (isTRUE(passed)) om$layers else integer(0),
    evidence = list(floating_organic_material = rec, landform = lf,
                    organic = om),
    missing = if (is.na(passed)) om$missing %||% "oc_pct" else character(0),
    reference = ref)
}


#' Toxic qualifier (tx): toxic concentration of organic or inorganic constituents.
#'
#' WRB 2022 Ch 5 / Ch 4 Histosols + Cryosols + Technosols (variable pages):
#' substances at concentrations toxic to plant roots. Practical proxy:
#' very low pH (<= 3.5, sulfuric / hyperacidic) OR very high electrical
#' conductivity (>= 16 dS/m, equivalent to Salic) OR specific contamination
#' fields (heavy metals, hydrocarbons) which the soilKey schema does not
#' yet model. v0.9.33 v0 implementation uses pH <= 3.5 OR EC >= 16 dS/m.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_toxic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5, Toxic: "having in some layer, within 50 cm of the
  # soil surface, toxic concentrations of organic or inorganic substances other
  # than ions of Al, Fe, Na, Ca and Mg, or having radioactivity dangerous to
  # humans", the limit values being "the task of governments". The function
  # used pH <= 3.5 or EC >= 16 dS/m within 100 cm, i.e. acidity and salts --
  # exactly the ions the definition leaves out -- so every saline Solonchak was
  # Toxic. It now needs a recorded contamination (contamination_type) in a layer
  # starting within 50 cm, and is NA when none was recorded.
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 50)
  ct <- h$contamination_type %||% rep(NA_character_, nrow(h))
  ct <- ct[cand]
  if (!length(cand) || all(is.na(ct) | !nzchar(trimws(ct))))
    return(DiagnosticResult$new(
      name = "Toxic", passed = NA, layers = integer(0),
      evidence = list(reason = "no contamination recorded within 50 cm"),
      missing = "contamination_type",
      reference = "WRB (2022) Ch 5, Toxic"))
  hit <- !is.na(ct) & nzchar(trimws(ct)) &
         !grepl("^(none|no|absent|nenhum|ausente)$", trimws(tolower(ct)))
  passing <- cand[hit]
  DiagnosticResult$new(
    name = "Toxic", passed = length(passing) > 0L, layers = passing,
    evidence = list(contamination_type = ct, depth_window_cm = c(0, 50)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Toxic")
}


# v0.9.414 (Hugo, 2026-11-19): the water that saturates the upper part of the
# histic horizon, from the record: "rain" (an ombrotrophic, rain-fed mire:
# raised or blanket bog), "ground" (groundwater or flowing water: a fen) or NA
# (not recorded, or the records disagree). Read in this order:
# site$mire_water_supply; the peat_moor_type of the layers of the upper 20 cm
# or upper half of the histic horizon, whichever is shallower (high-moor peat,
# the peat of raised bogs, as rain; low-moor peat, of fens, as groundwater;
# transitional undecided); a mire named in site landform, vegetation or
# land_use ("raised bog", "blanket bog", "ombrotrophic" against "fen",
# "minerotrophic", "groundwater-fed").
.wrb_mire_water_supply <- function(pedon, histic_layers = integer(0)) {
  s <- pedon$site %||% list()
  one <- function(x) {
    x <- unique(x[!is.na(x)])
    if (length(x) == 1L) x else NA_character_
  }
  rec <- tolower(trimws(as.character(s$mire_water_supply %||% NA)[1L]))
  if (!is.na(rec) && nzchar(rec)) {
    if (grepl("^(rain|ombro)", rec)) return("rain")
    if (grepl("^(ground|flowing|minero)", rec)) return("ground")
  }
  h <- pedon$horizons
  pm <- h$peat_moor_type
  if (length(histic_layers) && !is.null(pm)) {
    top <- min(h$top_cm[histic_layers]); bot <- max(h$bottom_cm[histic_layers])
    upper <- top + min(20, (bot - top) / 2)
    ly <- histic_layers[h$top_cm[histic_layers] < upper]
    v <- tolower(as.character(pm[ly]))
    v <- ifelse(grepl("high", v), "rain", ifelse(grepl("low", v), "ground",
           ifelse(is.na(v), NA_character_, "undecided")))
    if (length(v) && !anyNA(v) && !any(v == "undecided")) {
      r <- one(v)
      if (!is.na(r)) return(r)
    }
  }
  txt <- vapply(c("landform", "vegetation", "land_use"), function(f)
    as.character(s[[f]] %||% NA)[1L], character(1))
  txt <- txt[!is.na(txt)]
  rain <- any(grepl("raised bog|blanket bog|ombrotroph|ombrogen|hochmoor",
                    txt, ignore.case = TRUE))
  ground <- any(grepl("\\bfens?\\b|minerotroph|groundwater[- ]fed|niedermoor|spring mire|soligen|rheotroph",
                      txt, ignore.case = TRUE))
  if (rain && !ground) return("rain")
  if (ground && !rain) return("ground")
  NA_character_
}

# Ombric and Rheic: a histic horizon (WRB 2022 Ch 3.1.15, p. 48, with its
# saturation criterion) and the recorded water supply of its upper part.
.mire_water_state <- function(pedon) {
  hist <- histic_horizon(pedon, saturation = TRUE)
  src <- if (isTRUE(hist$passed)) .wrb_mire_water_supply(pedon, hist$layers)
         else NA_character_
  list(histic = hist, supply = src)
}
.mire_water_result <- function(name, passed, w, ref) {
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) w$histic$layers else integer(0),
    evidence = list(histic = w$histic, water_supply = w$supply),
    missing = if (!is.na(passed)) character(0)
              else if (!isTRUE(w$histic$passed)) w$histic$missing %||% character(0)
              else c("mire_water_supply", "peat_moor_type"),
    reference = ref)
}

#' Ombric qualifier (om): a histic horizon saturated with rainwater.
#'
#' WRB 2022 Ch 5 (p. 142): having a histic horizon, the upper >= 20 cm or at
#' least the upper half of which, whichever is shallower, are saturated
#' predominantly with rainwater (in Histosols only). The histic horizon is
#' read with its saturation criterion (saturated >= 30 days or drained); the
#' water supply from the record (\code{.wrb_mire_water_supply()}). Without a
#' record it is undecided.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_ombric <- function(pedon) {
  # v0.9.414 (Hugo, 2026-11-19): until v0.9.413 a pH (water) <= 4.5 in an
  # organic layer stood for rainwater; the definition reads the water, not
  # the pH, and needs a histic horizon (which a folic Histosol has not).
  w <- .mire_water_state(pedon)
  passed <- if (!isTRUE(w$histic$passed)) w$histic$passed
            else if (is.na(w$supply)) NA else w$supply == "rain"
  .mire_water_result("Ombric", passed, w, "WRB (2022) Ch 5, Ombric (p. 142)")
}


#' Rheic qualifier (rh): a histic horizon reached by groundwater or flowing
#' water.
#'
#' WRB 2022 Ch 5 (p. 145): having a histic horizon, in which groundwater or
#' flowing water ascends to < 20 cm of the soil surface or reaches the histic
#' horizon's upper half, whichever is shallower (in Histosols only). Read as
#' \code{qual_ombric()}, with the other water supply.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
qual_rheic <- function(pedon) {
  # v0.9.414 (Hugo, 2026-11-19): until v0.9.413 a pH (water) > 4.5 or
  # carbonates in an organic layer stood for groundwater.
  w <- .mire_water_state(pedon)
  passed <- if (!isTRUE(w$histic$passed)) w$histic$passed
            else if (is.na(w$supply)) NA else w$supply == "ground"
  .mire_water_result("Rheic", passed, w, "WRB (2022) Ch 5, Rheic (p. 145)")
}
