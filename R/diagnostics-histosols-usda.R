# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Helpers for Histosols Suborder/GG/SG keys -- Chapter 10, pp 199-205
# =============================================================
#
# Histosols are organic soils that are NOT Gelisols (i.e. no
# permafrost within 100 cm). The 5 Suborders are distinguished by:
#   Folists  -- saturated < 30 days/yr (well drained)
#   Wassists -- water table >= 2 cm above surface for 21+ hr/day
#   Fibrists -- fibric soil materials dominate (low decomposition)
#   Saprists -- sapric soil materials dominate (high decomposition)
#   Hemists  -- hemic (intermediate)
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 10.
# =============================================================


# ---- Histosols Order qualifier --------------------------------------

#' Histosols Order qualifier (USDA, KST 13ed, Ch 2, p 7)
#'
#' Organic soils not meeting the Gelisols requirements (no permafrost
#' within 100 cm). The KST defines Histosols as soils with organic
#' soil materials that meet specific thickness/depth criteria
#' (Ch 2, pp 7-9; see also Ch 3 organic soil materials).
#'
#' Implementation: pass when cumulative organic-layer thickness
#' (designation H or O) within 0-100 cm >= 40 cm AND no permafrost
#' within 100 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 2, pp 7-9.
#' @export
histosol_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  org <- which(!is.na(h$designation) & grepl("^[OH]", h$designation) &
                   !is.na(h$top_cm) & h$top_cm < 100)
  thk_org <- if (length(org) > 0L)
                sum(pmax(pmin(h$bottom_cm[org], 100) -
                              pmax(h$top_cm[org], 0), 0), na.rm = TRUE)
              else 0
  pf <- permafrost_within_usda(pedon, max_top_cm = 100)
  passed <- thk_org >= 40 && !isTRUE(pf$passed)
  DiagnosticResult$new(
    name = "histosol_qualifying_usda", passed = passed,
    layers = if (passed) org else integer(0),
    evidence = list(organic_thickness_cm = thk_org,
                      permafrost_present = isTRUE(pf$passed)),
    missing = pf$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 2, pp 7-9"
  )
}


# ---- Wassists Suborder qualifier ------------------------------------

#' Wassists Suborder qualifier (KST 13ed, Ch 10, p 203)
#'
#' Histosols having a "field-observable water table 2 cm or more
#' above the soil surface for more than 21 hours of each day in
#' all years."  Diagnostic for the Wassists suborder.
#'
#' Implementation: pass when \code{site$water_table_cm_above_surface}
#' is provided and >= 2 (positive = above surface).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
wassist_qualifying_usda <- function(pedon) {
  wt <- pedon$site$water_table_cm_above_surface %||% NA_real_
  passed <- !is.na(wt) && wt >= 2
  DiagnosticResult$new(
    name = "wassist_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(water_table_cm_above_surface = wt),
    missing = if (is.na(wt)) "site$water_table_cm_above_surface"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
  )
}


# ---- Folists Suborder qualifier (well-drained organic) --------------

#' Folists Suborder qualifier (KST 13ed, Ch 10, p 200)
#'
#' Histosols saturated for less than 30 days per year (and not
#' artificially drained). Implementation: pass when there is no
#' aquic conditions and no glei designation in the upper 50 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
folist_qualifying_usda <- function(pedon) {
  aq <- aquic_conditions_usda(pedon, max_top_cm = 50)
  passed <- !isTRUE(aq$passed)
  DiagnosticResult$new(
    name = "folist_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(aquic_present = isTRUE(aq$passed)),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 200"
  )
}


# ---- Soil Moisture / Temperature Regimes ----------------------------

#' Soil moisture regime helper (USDA, KST 13ed Ch 3, pp 50-52)
#'
#' Returns TRUE when \code{pedon$site$soil_moisture_regime} matches
#' \code{target}. Climatic data is required; in v0.8.x the regime
#' is read directly from site metadata (a v0.9 helper will derive it
#' from monthly precipitation+ETP).
#'
#' Recognized targets (KST 13ed Ch 3): "aquic", "aridic", "torric",
#' "udic", "perudic", "ustic", "xeric".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param target Character, one of the recognized regimes.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
soil_moisture_regime_usda <- function(pedon,
                                          target = c("aquic", "aridic",
                                                       "torric", "udic",
                                                       "perudic", "ustic",
                                                       "xeric")) {
  target <- match.arg(target)
  smr <- pedon$site$soil_moisture_regime %||% NA_character_
  # Aridic and torric are synonyms in KST 13ed.
  if (target == "aridic") {
    passed <- !is.na(smr) && tolower(smr) %in% c("aridic", "torric")
  } else if (target == "torric") {
    passed <- !is.na(smr) && tolower(smr) %in% c("aridic", "torric")
  } else {
    passed <- !is.na(smr) && tolower(smr) == target
  }
  DiagnosticResult$new(
    name = paste0("smr_", target, "_usda"), passed = passed,
    layers = integer(0),
    evidence = list(soil_moisture_regime = smr, target = target),
    missing = if (is.na(smr)) "site$soil_moisture_regime"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 50-52"
  )
}


#' Soil temperature regime helper (USDA, KST 13ed Ch 3, pp 53-58)
#'
#' Returns TRUE when \code{pedon$site$soil_temperature_regime}
#' matches \code{target}. Temperature regimes:
#' \itemize{
#'   \item "gelic":     MAST < 0 C (and permafrost present)
#'   \item "cryic":     MAST 0-8 C, summer < 15 C
#'   \item "frigid":    MAST < 8 C, summer >= 15 C
#'   \item "mesic":     MAST 8-15 C
#'   \item "thermic":   MAST 15-22 C
#'   \item "hyperthermic": MAST >= 22 C
#'   \item Plus iso- variants (low summer-winter difference)
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param target Character, one of the recognized regimes.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
soil_temperature_regime_usda <- function(pedon,
                                              target = c("gelic", "cryic",
                                                          "frigid", "mesic",
                                                          "thermic",
                                                          "hyperthermic",
                                                          "isofrigid",
                                                          "isomesic",
                                                          "isothermic",
                                                          "isohyperthermic")) {
  target <- match.arg(target)
  str <- pedon$site$soil_temperature_regime %||% NA_character_
  passed <- !is.na(str) && tolower(str) == target
  DiagnosticResult$new(
    name = paste0("str_", target, "_usda"), passed = passed,
    layers = integer(0),
    evidence = list(soil_temperature_regime = str, target = target),
    missing = if (is.na(str)) "site$soil_temperature_regime"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 53-58"
  )
}


# ---- Per-regime convenience helpers (called from YAML) --------------

#' @export
str_cryic_usda  <- function(pedon) soil_temperature_regime_usda(pedon, "cryic")
#' @export
smr_aridic_usda <- function(pedon) soil_moisture_regime_usda(pedon, "aridic")
#' @export
smr_torric_usda <- function(pedon) soil_moisture_regime_usda(pedon, "torric")
#' @export
smr_ustic_usda  <- function(pedon) soil_moisture_regime_usda(pedon, "ustic")
#' @export
smr_xeric_usda  <- function(pedon) soil_moisture_regime_usda(pedon, "xeric")
#' @export
smr_udic_usda   <- function(pedon) soil_moisture_regime_usda(pedon, "udic")


# ---- Sulfidic / Sulfuric helpers ------------------------------------

#' Sulfidic materials helper (USDA, KST 13ed Ch 3, p 49)
#'
#' Pass when sulfidic materials (soft, dark, sulfide-rich) are
#' present within \code{max_top_cm}. Proxy: sulfidic_s_pct >= 0.75
#' AND in a layer >= 15 cm thick.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @param min_thickness_cm Default 15.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
sulfidic_materials_usda <- function(pedon, max_top_cm = 100,
                                         min_thickness_cm = 15) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  miss <- character(0)
  if (all(is.na(h$sulfidic_s_pct[cand]))) miss <- "sulfidic_s_pct"
  passing <- cand[!is.na(h$sulfidic_s_pct[cand]) &
                      h$sulfidic_s_pct[cand] >= 0.75]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "sulfidic_materials_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk, threshold = min_thickness_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 49"
  )
}


#' Sulfic Subgroup helper (Haplowassists)
#' Pass when sulfidic materials within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
sulfic_subgroup_usda <- function(pedon) {
  res <- sulfidic_materials_usda(pedon, max_top_cm = 100)
  res$name <- "sulfic_subgroup_usda"
  res
}


# ---- Halic / Frasic Subgroup helpers --------------------------------

#' Halic Subgroup helper (Haplosaprists)
#'
#' Pass when EC >= 30 dS/m through a 30+ cm layer for 6+ months
#' (KST 13ed, Ch 10). v0.8 proxy: any layer with ec_dS_m >= 30.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ec Default 30.
#' @param min_thickness_cm Default 30.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
halic_subgroup_usda <- function(pedon, min_ec = 30,
                                     min_thickness_cm = 30) {
  h <- pedon$horizons
  miss <- if (all(is.na(h$ec_dS_m))) "ec_dS_m" else character(0)
  passing <- which(!is.na(h$ec_dS_m) & h$ec_dS_m >= min_ec)
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "halic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Frasiwassists Subgroup helper (Wassists)
#'
#' Pass when ec_dS_m < 0.6 (1:5 soil:water) in all horizons within
#' 100 cm. KST 13ed, Ch 10, p 203.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_ec Default 0.6.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
frasic_qualifying_usda <- function(pedon, max_ec = 0.6,
                                        max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "frasic_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no candidate layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
    ))
  }
  ecs <- h$ec_dS_m[cand]
  miss <- if (all(is.na(ecs))) "ec_dS_m" else character(0)
  # All layers with EC must satisfy < 0.6
  passed <- !all(is.na(ecs)) && all(is.na(ecs) | ecs < max_ec)
  DiagnosticResult$new(
    name = "frasic_qualifying_usda", passed = passed, layers = cand,
    evidence = list(ec_layers = ecs, threshold = max_ec),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
  )
}


# ---- Hydric / Humilluvic Subgroup helpers ---------------------------

#' Hydric Subgroup helper (Histosols Cryofibrists / Sphagnofibrists /
#' etc.)
#'
#' Pass when there is a "layer of water" within the control section.
#' Detected via designation containing "W" (water layer) or
#' \code{layer_origin == "water"}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 130.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
hydric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  h <- pedon$horizons
  cand <- which((!is.na(h$designation) &
                    grepl("^W(?![efr])", h$designation, perl = TRUE)) |
                   (!is.na(h$layer_origin) &
                      tolower(h$layer_origin) == "water"))
  cand <- cand[!is.na(h$top_cm[cand]) & h$top_cm[cand] < max_top_cm]
  passed <- length(cand) > 0L
  DiagnosticResult$new(
    name = "hydric_subgroup_usda", passed = passed, layers = cand,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Humilluvic Subgroup helper (Luvihemists)
#'
#' Pass when a horizon >= 2 cm thick has humilluvic material (humus
#' translocated from above) >= 50\% volume. v0.8 deferred (no
#' specific column). Refinement to use a humilluvic_pct column or
#' a designation marker is planned.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
humilluvic_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "humilluvic_subgroup_usda", passed = FALSE,
    layers = integer(0),
    evidence = list(reason = "humilluvic_pct column not in schema; deferred to v0.9"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 202"
  )
}


# ---- Predominance helpers (Saprists vs Hemists vs Fibrists) ---------

# Helper: cumulative thickness of layers passing a per-layer test
.cum_layer_thickness <- function(h, layers, max_top_cm = Inf) {
  if (length(layers) == 0L) return(0)
  ok <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_top_cm
  layers <- layers[ok]
  if (length(layers) == 0L) return(0)
  sum(pmax(h$bottom_cm[layers] - h$top_cm[layers], 0), na.rm = TRUE)
}


#' Sapric_predominant_usda: Saprists Suborder qualifier
#' Pass when thickness of sapric > thickness of fibric+hemic in 0-130 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
sapric_predominant_usda <- function(pedon) {
  h <- pedon$horizons
  s <- saprico(pedon)
  he <- hemico(pedon)
  fi <- fibrico(pedon)
  thk_s <- .cum_layer_thickness(h, s$layers, max_top_cm = 130)
  thk_h <- .cum_layer_thickness(h, he$layers, max_top_cm = 130)
  thk_f <- .cum_layer_thickness(h, fi$layers, max_top_cm = 130)
  passed <- thk_s > thk_h && thk_s > thk_f
  DiagnosticResult$new(
    name = "sapric_predominant_usda", passed = passed,
    layers = s$layers,
    evidence = list(sapric_cm = thk_s, hemic_cm = thk_h,
                      fibric_cm = thk_f),
    missing = unique(c(s$missing %||% character(0),
                          he$missing %||% character(0),
                          fi$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 202"
  )
}


#' Fibric_predominant_usda: Fibrists Suborder qualifier
#' @export
fibric_predominant_usda <- function(pedon) {
  h <- pedon$horizons
  s <- saprico(pedon); he <- hemico(pedon); fi <- fibrico(pedon)
  thk_s <- .cum_layer_thickness(h, s$layers, max_top_cm = 130)
  thk_h <- .cum_layer_thickness(h, he$layers, max_top_cm = 130)
  thk_f <- .cum_layer_thickness(h, fi$layers, max_top_cm = 130)
  passed <- thk_f > thk_s && thk_f > thk_h
  DiagnosticResult$new(
    name = "fibric_predominant_usda", passed = passed,
    layers = fi$layers,
    evidence = list(sapric_cm = thk_s, hemic_cm = thk_h,
                      fibric_cm = thk_f),
    missing = unique(c(s$missing %||% character(0),
                          he$missing %||% character(0),
                          fi$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 199"
  )
}


# ---- Subgroup-level fibric/hemic/sapric helpers (tier rules) -------

#' Fibric Subgroup helper (Haplohemists / Haplowassists / Sulfiwassists)
#' Pass when fibric layers cumulative thickness >= 25 cm in control
#' section below surface tier.
#' @export
fibric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  fi <- fibrico(pedon)
  thk <- .cum_layer_thickness(pedon$horizons, fi$layers,
                                  max_top_cm = max_top_cm)
  passed <- thk >= 25
  DiagnosticResult$new(
    name = "fibric_subgroup_usda", passed = passed,
    layers = if (passed) fi$layers else integer(0),
    evidence = list(thickness_cm = thk, threshold = 25),
    missing = fi$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Hemic Subgroup helper
#' @export
hemic_subgroup_usda <- function(pedon, max_top_cm = 130) {
  he <- hemico(pedon)
  thk <- .cum_layer_thickness(pedon$horizons, he$layers,
                                  max_top_cm = max_top_cm)
  passed <- thk >= 25
  DiagnosticResult$new(
    name = "hemic_subgroup_usda", passed = passed,
    layers = if (passed) he$layers else integer(0),
    evidence = list(thickness_cm = thk, threshold = 25),
    missing = he$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Sapric Subgroup helper (Sphagnofibrists)
#' @export
sapric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  sa <- saprico(pedon)
  thk <- .cum_layer_thickness(pedon$horizons, sa$layers,
                                  max_top_cm = max_top_cm)
  passed <- thk >= 25
  DiagnosticResult$new(
    name = "sapric_subgroup_usda", passed = passed,
    layers = if (passed) sa$layers else integer(0),
    evidence = list(thickness_cm = thk, threshold = 25),
    missing = sa$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}
