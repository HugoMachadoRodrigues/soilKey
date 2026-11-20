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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


#' Sombric horizon (USDA Soil Taxonomy, KST 13th ed., pp. 30-31)
#'
#' A subsurface horizon of illuvial humus formed under free drainage, read
#' by its six required characteristics, three-valued, layer by layer
#' against the horizon directly above: (1) organic carbon 0.6 \% or more
#' and at least 0.2 points more than the horizon above; (2) chroma of 4 or
#' less and a value of 4 or less moist or 5 or less dry; (3) value or
#' chroma at least 1 unit darker than the horizon above; (4) 15 cm or more;
#' (5) pH 5.0 or less; (6) Al plus half Fe (ammonium oxalate) under 0.5 \%
#' (0.4 \% with 5 \% or more volcanic glass). Only B horizons (or
#' undesignated layers) below the surface layer qualify; a buried (b) or
#' gleyed (g) horizon does not (no free drainage, no illuviation).
#' v0.9.419: up to v0.9.418 a dark, low-chroma layer with more than 1 \%
#' organic carbon at 50 to 150 cm, buried and gleyed layers included
#' (audit I12).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Upper boundary within this depth below the mineral soil
#'   surface (cm).
#' @noRd
sombric_horizon_usda <- function(pedon, max_top_cm = 150) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, sombric horizon (pp. 30-31)"
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(DiagnosticResult$new(name = "sombric_horizon_usda", passed = FALSE, reference = ref))
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  ord <- order(h$top_cm)
  d <- .usda_desig(h$designation)
  cand <- !is.na(h$top_cm) & h$top_cm > ms & h$top_cm < ms + max_top_cm &
    (is.na(d$master) | d$master == "" | grepl("B", d$master)) &
    !(!is.na(d$suffix) & grepl("[bg]", d$suffix))
  oc <- h$oc_pct; vm <- h$munsell_value_moist; vd <- h$munsell_value_dry
  cm <- h$munsell_chroma_moist; ph <- h$ph_h2o
  alfe <- .usda_col(h, "al_ox_pct", NA_real_) + .usda_col(h, "fe_ox_pct", NA_real_) / 2
  glass <- .usda_col(h, "volcanic_glass_pct", NA_real_)
  pos <- integer(n); pos[ord] <- seq_len(n)
  lay <- vapply(seq_len(n), function(i) {
    if (!cand[i] || pos[i] == 1L) return(FALSE)
    j <- ord[pos[i] - 1L]
    c1 <- if (is.na(oc[i])) NA else if (oc[i] < 0.6) FALSE
          else if (is.na(oc[j])) NA else oc[i] >= oc[j] + 0.2
    c2 <- .and3v(if (is.na(cm[i])) NA else cm[i] <= 4,
                 .usda_any3(c(if (is.na(vm[i])) NA else vm[i] <= 4,
                              if (is.na(vd[i])) NA else vd[i] <= 5)))
    c3 <- .usda_any3(c(if (is.na(vm[i]) || is.na(vm[j])) NA else vm[i] <= vm[j] - 1,
                       if (is.na(cm[i]) || is.na(cm[j])) NA else cm[i] <= cm[j] - 1))
    c5 <- if (is.na(ph[i])) NA else ph[i] <= 5.0
    lim <- if (!is.na(glass[i]) && glass[i] >= 5) 0.4 else 0.5
    c6 <- if (is.na(alfe[i])) NA else alfe[i] < lim
    .usda_all3(c(c1, c2, c3, c5, c6))
  }, NA)
  yes <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(lay %in% TRUE, ord))
  may <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(!(lay %in% FALSE), ord))
  passed <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  DiagnosticResult$new(
    name = "sombric_horizon_usda", passed = passed,
    layers = if (isTRUE(passed)) sort(unique(unlist(yes)))
             else if (is.na(passed)) sort(unique(unlist(may))) else integer(0),
    evidence = list(layer = lay, max_top_cm = max_top_cm),
    missing = if (is.na(passed)) c("oc_pct", "munsell_value_moist", "munsell_chroma_moist",
                                   "ph_h2o", "al_ox_pct", "fe_ox_pct") else character(0),
    reference = ref)
}


#' Sombric Subgroup / great-group helper (Sombri-: a sombric horizon within
#' 150 cm of the mineral soil surface)
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Depth (cm below the mineral soil surface).
#' @noRd
sombric_subgroup_usda <- function(pedon, max_top_cm = 150) {
  res <- sombric_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "sombric_subgroup_usda"
  res
}


#' Humic-Oxisol Subgroup helper
#' Pass when cumulative organic carbon mass is >= 16 kg/m2 between
#' surface and 100 cm (computed as SUM(OC\% * bulk_density * dz)).
#' v0.8 proxy: uses default bulk_density 1.0 g/cm3 if unavailable.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
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
#' @noRd
aeric_oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  # KST 13ed Ch. 13: the chroma-3 horizon must lie DIRECTLY BELOW an epipedon
  # (10+ cm thick). Exclude surface epipedon horizons (A*/O* designations) so a
  # high-chroma A/AB layer cannot satisfy the test from within the epipedon.
  d <- h$designation
  is_epi <- !is.na(d) & grepl("^A|^O", d)
  cand <- which(!is.na(h$top_cm) & h$top_cm > 0 & h$top_cm < 100 & !is_epi)
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
#' @noRd
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
#' @noRd
kandic_oxisol_usda <- function(pedon) {
  res <- kandic_horizon_usda(pedon)
  res$name <- "kandic_oxisol_usda"
  res
}


#' Eutric Oxisol Suborder helper (Eutroperox/Eutrudox/etc.)
#' Pass when BS (NH4OAc) >= 35\% in all layers within 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
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
