# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Oxisols helpers -- Chapter 13, pp 295-310
# =============================================================
#
# Oxisols are highly weathered tropical/subtropical soils with an
# oxic horizon (low CEC, low weatherable minerals, advanced kaolinite/
# Fe-oxide stage). 5 Suborders distinguished by SMR + aquic.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 13.
# =============================================================


#' Oxic horizon (USDA, KST 13ed, Ch 3)
#' Delegates to WRB \code{ferralic}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
oxic_horizon_usda <- function(pedon) {
  res <- oxic_usda(pedon)  # already exists, delegates to ferralic
  res$name <- "oxic_horizon_usda"
  res
}


#' Petroferric contact helper (USDA, KST 13ed Ch 3, p 48)
#'
#' Ironstone-like layer with >50\% Fe oxides, indurated. v0.8 proxy:
#' \code{cementation_class} in \{strongly, indurated\} AND
#' \code{plinthite_pct >= 50} (Fe-rich) AND \code{coarse_fragments_pct >= 50}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 125.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
petroferric_contact_usda <- function(pedon, max_top_cm = 125) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cem <- h$cementation_class[cand]
  pp <- h$plinthite_pct[cand]
  cf <- h$coarse_fragments_pct[cand]
  passing <- cand[!is.na(cem) & tolower(cem) %in%
                                  c("strongly", "indurated") &
                      ((!is.na(pp) & pp >= 50) |
                         (!is.na(cf) & cf >= 50))]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "petroferric_contact_usda", passed = passed,
    layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 48"
  )
}


#' Anionic Subgroup helper (Oxisols)
#'
#' Pass when delta pH (KCl - water) is 0 or net positive in a 18+ cm
#' layer within 125 cm. Indicates exchange complex dominated by
#' positive-charge minerals (Fe/Al oxides).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
anionic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  miss <- character(0)
  if (all(is.na(h$ph_kcl)) || all(is.na(h$ph_h2o))) {
    miss <- c("ph_kcl", "ph_h2o")
    return(DiagnosticResult$new(
      name = "anionic_subgroup_usda", passed = NA, layers = integer(0),
      evidence = list(reason = "pH data missing"),
      missing = miss,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
    ))
  }
  delta_ph <- h$ph_kcl - h$ph_h2o
  cand <- which(!is.na(delta_ph) & delta_ph >= 0 &
                  !is.na(h$top_cm) & h$top_cm < 125)
  thk <- if (length(cand) > 0L)
           sum(pmax(h$bottom_cm[cand] - h$top_cm[cand], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 18
  DiagnosticResult$new(
    name = "anionic_subgroup_usda", passed = passed, layers = cand,
    evidence = list(delta_ph = delta_ph, thickness_cm = thk),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Rhodic Subgroup helper (Oxisols, Mollisols, etc.)
#' Pass when 50\%+ colors have hue <= 2.5YR AND value <= 3 in
#' B horizons 25-125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
rhodic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 125)
  hu <- h$munsell_hue_moist[cand]
  vm <- h$munsell_value_moist[cand]
  red <- !is.na(hu) & grepl("^2\\.5YR|^10R|^7\\.5R|^5R", hu)
  dark <- !is.na(vm) & vm <= 3
  passing <- cand[red & dark]
  passed <- length(passing) > 0L &&
              sum(red & dark, na.rm = TRUE) >= 0.5 * length(cand)
  DiagnosticResult$new(
    name = "rhodic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(n_red_dark = length(passing), n_total = length(cand)),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Xanthic Subgroup helper (Oxisols)
#' Pass when 50\%+ colors have hue >= 7.5YR AND value >= 6 in B horizons.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
xanthic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 125)
  hu <- h$munsell_hue_moist[cand]
  vm <- h$munsell_value_moist[cand]
  yellow <- !is.na(hu) & grepl("^7\\.5YR|^10YR|^2\\.5Y|^5Y", hu)
  light  <- !is.na(vm) & vm >= 6
  passing <- cand[yellow & light]
  passed <- length(passing) > 0L &&
              sum(yellow & light, na.rm = TRUE) >= 0.5 * length(cand)
  DiagnosticResult$new(
    name = "xanthic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(n_yellow_light = length(passing), n_total = length(cand)),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Sombric Subgroup helper (Oxisols Sombri-)
#' Pass when sombric horizon (humus illuviation in tropics) is
#' present. v0.8: detects via 'sombric' designation OR a B horizon
#' with V<=4 + V<=4 + chroma<=2 + OC>1 in 50-150 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
sombric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 50 & h$top_cm < 150)
  vm <- h$munsell_value_moist[cand]
  cm <- h$munsell_chroma_moist[cand]
  oc <- h$oc_pct[cand]
  passing <- cand[!is.na(vm) & vm <= 4 & !is.na(cm) & cm <= 2 &
                      !is.na(oc) & oc > 1]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "sombric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(note = "v0.8 proxy: dark, low-chroma, OC-rich subsurface"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 60"
  )
}


#' Humic-Oxisol Subgroup helper
#' Pass when cumulative organic carbon mass is >= 16 kg/m2 between
#' surface and 100 cm (computed as SUM(OC\% * bulk_density * dz)).
#' v0.8 proxy: uses default bulk_density 1.0 g/cm3 if unavailable.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
humic_oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "humic_oxisol_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no candidate layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
    ))
  }
  oc_mass <- 0
  for (i in cand) {
    oc <- h$oc_pct[i]
    bd <- h$bulk_density_g_cm3[i] %||% 1.0
    if (is.na(bd)) bd <- 1.0
    if (is.na(oc)) next
    top <- max(h$top_cm[i], 0)
    bot <- min(h$bottom_cm[i], 100)
    dz <- pmax(bot - top, 0)
    # OC% * BD g/cm3 * dz cm = (OC/100 * BD * dz) g/cm2 = same g/m2 / 100 ; *10 = kg/m2
    oc_mass <- oc_mass + (oc / 100) * bd * dz * 10
  }
  passed <- oc_mass >= 16
  DiagnosticResult$new(
    name = "humic_oxisol_usda", passed = passed, layers = cand,
    evidence = list(oc_mass_kg_m2 = oc_mass),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Plinthic Subgroup helper (Oxisols)
#' Pass when plinthite >= 5\% in any horizon within 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
plinthic_subgroup_usda <- function(pedon, max_top_cm = 125) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  pp <- h$plinthite_pct[cand]
  miss <- if (all(is.na(pp))) "plinthite_pct" else character(0)
  passing <- cand[!is.na(pp) & pp >= 5]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "plinthic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13 / Ch. 3"
  )
}


#' Aeric Subgroup (for Oxisols Aquox) -- chroma-3 below epipedon
#' Already defined for Aquods; here we add Oxisol-specific variant
#' (any 10+ cm horizon below A with chroma >= 3 in 50\%+ peds).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aeric_oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm > 0 & h$top_cm < 100)
  chr <- h$munsell_chroma_moist[cand]
  passing <- cand[!is.na(chr) & chr >= 3]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 10
  DiagnosticResult$new(
    name = "aeric_oxisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Acric Oxisol Suborder helper (Acroperox/Acrudox/Acrustox/Acraquox)
#' Pass when oxic or kandic horizon has ECEC < 1.5 cmol/kg clay AND
#' pH (KCl) >= 5.0.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
acric_oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 150)
  miss <- character(0)
  if (all(is.na(h$ecec_cmol[cand]))) miss <- c(miss, "ecec_cmol")
  if (all(is.na(h$clay_pct[cand]))) miss <- c(miss, "clay_pct")
  if (all(is.na(h$ph_kcl[cand]))) miss <- c(miss, "ph_kcl")
  passing <- integer(0)
  for (i in cand) {
    ec <- h$ecec_cmol[i]; cl <- h$clay_pct[i]; pk <- h$ph_kcl[i]
    if (is.na(ec) || is.na(cl) || is.na(pk) || cl == 0) next
    ec_per_clay <- ec * 100 / cl
    if (ec_per_clay < 1.5 && pk >= 5.0) {
      passing <- c(passing, i)
    }
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "acric_oxisol_usda", passed = passed, layers = passing,
    evidence = list(threshold_ecec_clay = 1.5),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Kandic Suborder helper for Oxisols (Kandiperox/Kandiudox/Kandiustox)
#' Delegates to kandic_horizon_usda.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
kandic_oxisol_usda <- function(pedon) {
  res <- kandic_horizon_usda(pedon)
  res$name <- "kandic_oxisol_usda"
  res
}


#' Eutric Oxisol Suborder helper (Eutroperox/Eutrudox/etc.)
#' Pass when BS (NH4OAc) >= 35\% in all layers within 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
eutric_oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 125)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  passed <- !all(is.na(bs)) && all(is.na(bs) | bs >= 35)
  DiagnosticResult$new(
    name = "eutric_oxisol_usda", passed = passed, layers = cand,
    evidence = list(bs_layers = bs, threshold = 35),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}


#' Plinthaquox qualifying helper (Aquox: continuous plinthite phase)
#' Pass when plinthite >= 50\% in some 10+ cm layer (continuous phase
#' proxy).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
plinthaquox_qualifying_usda <- function(pedon, max_top_cm = 125) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  pp <- h$plinthite_pct[cand]
  passing <- cand[!is.na(pp) & pp >= 50]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 10
  DiagnosticResult$new(
    name = "plinthaquox_qualifying_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = if (all(is.na(pp))) "plinthite_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
  )
}
