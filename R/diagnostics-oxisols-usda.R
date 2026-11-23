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
#' \code{oxic_usda()} under this name (v0.9.420: the KST oxic horizon, no
#' longer the WRB ferralic horizon).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
oxic_horizon_usda <- function(pedon) {
  res <- oxic_usda(pedon)
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
#'
#' v0.9.420: \code{kst} names the reading of the KST (13th ed.); \code{NULL}
#' keeps the earlier one, which the Alfisol, Ultisol and Entisol subgroup
#' lists still use.
#' \describe{
#'   \item{\code{"oxisol"}}{Rhodic subgroups of the Oxisols (pp. 296-310,
#'     audit item O3): in all horizons between 25 and 125 cm, more than 50\%
#'     colours with hue 2.5YR or redder and value, moist, 3 or less.}
#'   \item{\code{"argillic"}}{Rhodudalfs, Rhodustalfs (pp. 86-87, 98; item G3):
#'     in all subhorizons of the upper 100 cm of the argillic horizon (all of
#'     it if thinner), hue 2.5YR or redder, value, moist, 3 or less and a dry
#'     value no more than 1 unit higher than the moist value.}
#'   \item{\code{"argillic_kandic"}}{Rhodoxeralfs (p. 109): the same in the
#'     argillic or kandic horizon.}
#'   \item{\code{"ultisol"}}{Rhodudults, Rhodustults (pp. 329, 337): the same
#'     in the argillic horizon, and an epipedon with value, moist, of 3 or
#'     less throughout.}
#' }
#' The matrix colour of each layer stands for its "more than 50 percent
#' colors". A dry colour not recorded leaves the test undecided (audit reading
#' L26); so does a profile that ends above 125 cm (Oxisols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kst The reading, see above.
#' @noRd
rhodic_subgroup_usda <- function(pedon, kst = NULL) {
  h <- pedon$horizons
  if (is.null(kst)) {
    cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 125)
    hu <- h$munsell_hue_moist[cand]
    vm <- h$munsell_value_moist[cand]
    red <- !is.na(hu) & grepl("^2\\.5YR|^10R|^7\\.5R|^5R", hu)
    dark <- !is.na(vm) & vm <= 3
    passing <- cand[red & dark]
    passed <- length(passing) > 0L &&
                sum(red & dark, na.rm = TRUE) >= 0.5 * length(cand)
    return(DiagnosticResult$new(
      name = "rhodic_subgroup_usda", passed = passed, layers = passing,
      evidence = list(n_red_dark = length(passing), n_total = length(cand)),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 13"
    ))
  }
  kst <- match.arg(kst, c("oxisol", "argillic", "argillic_kandic", "ultisol"))
  ref <- "Soil Survey Staff (2022), KST 13th ed., Rhod- great groups and Rhodic subgroups"
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  res <- function(passed, layers = integer(0), miss = character(0), ev = list())
    DiagnosticResult$new(name = "rhodic_subgroup_usda", passed = passed,
      layers = if (isTRUE(passed)) layers else integer(0),
      evidence = c(list(kst = kst), ev),
      missing = if (is.na(passed)) unique(miss) else character(0), reference = ref)
  red <- .hue_redder_than(h$munsell_hue_moist, "2.5YR")
  vm <- h$munsell_value_moist; vd <- h$munsell_value_dry
  if (kst == "oxisol") {
    L <- which(!is.na(h$top_cm) & h$top_cm < ms + 125 &
                 (is.na(h$bottom_cm) | h$bottom_cm > ms + 25))
    ok <- ifelse(red[L] %in% FALSE | (!is.na(vm[L]) & vm[L] > 3), FALSE,
                 ifelse(is.na(red[L]) | is.na(vm[L]), NA, TRUE))
    deepest <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
    contact <- .kst_contact_top_cm(pedon, petroferric = TRUE)
    short <- (!is.finite(deepest) || deepest < ms + 125) && (is.na(contact) || contact > deepest)
    p <- if (!length(L) || any(ok %in% FALSE)) FALSE
         else if (all(ok %in% TRUE) && !short) TRUE else NA
    return(res(p, L, c("munsell_hue_moist", "munsell_value_moist", if (short) "bottom_cm")))
  }
  hz <- argillic_usda(pedon)
  if (kst == "argillic_kandic" && !isTRUE(hz$passed)) {
    ka <- kandic_horizon_usda(pedon)
    if (isTRUE(ka$passed)) hz <- ka else hz$passed <- .usda_or3(hz$passed, ka$passed)
  }
  if (!isTRUE(hz$passed)) return(res(hz$passed, miss = hz$missing))
  t0 <- min(h$top_cm[hz$layers])
  L <- hz$layers[h$top_cm[hz$layers] < t0 + 100]
  ok <- ifelse(red[L] %in% FALSE | (!is.na(vm[L]) & vm[L] > 3) |
                 (!is.na(vm[L]) & !is.na(vd[L]) & vd[L] - vm[L] > 1), FALSE,
               ifelse(is.na(red[L]) | is.na(vm[L]) | is.na(vd[L]), NA, TRUE))
  p <- .and3(ok)
  if (kst == "ultisol" && !isFALSE(p)) {
    m <- .desg_master(as.character(h$designation), site = pedon$site)
    epi <- which(!is.na(h$top_cm) & h$top_cm >= ms & h$top_cm < t0 & !is.na(m) & grepl("^A", m))
    if (!length(epi)) epi <- which(!is.na(h$top_cm) & h$top_cm >= ms & h$top_cm < t0)[1L]
    e_ok <- if (!length(epi) || is.na(epi[1L])) NA
            else .and3(ifelse(is.na(vm[epi]), NA, vm[epi] <= 3))
    p <- .and3(p, e_ok)
  }
  res(p, L, c("munsell_hue_moist", "munsell_value_moist", "munsell_value_dry"),
      ev = list(horizon = hz$name))
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
#' v0.9.420 (audit item O4): a missing bulk density is no longer 1.0 g/cm3;
#' the test is three-valued (\code{.oc_mass_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humic_oxisol_usda <- function(pedon) {
  m <- .oc_mass_usda(pedon, depth = 100, threshold = 16)
  DiagnosticResult$new(
    name = "humic_oxisol_usda", passed = m$passed, layers = integer(0),
    evidence = list(oc_mass_kg_m2 = m$known_kg_m2, oc_mass_upper_kg_m2 = m$upper_kg_m2,
                    threshold = 16),
    missing = m$missing,
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 13, Humic subgroups"
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
#'
#' v0.9.420 (audit item O5). Great groups (KST 13th ed., pp. 295-305): in one
#' or more subhorizons of an oxic or kandic horizon within 150 cm of the
#' mineral soil surface, an apparent ECEC of less than 1.50 cmol(+) per kg
#' clay and a pH (1N KCl) of 5.0 or more. The ECEC is \code{ecec_cmol}, or
#' the NH4OAc bases plus KCl Al; the clay is the apparent clay of the oxic
#' horizon (3 x the water at 1500 kPa by mass, if higher). With
#' \code{subgroup = TRUE}, the Acrudoxic, Acrustoxic and Acraquoxic subgroups
#' of the Kandi- and Kanhapl- great groups (p. 331): an ECEC of 1.5 cmol(+)
#' or less per kg clay in one or more horizons within 150 cm. Until
#' v0.9.419 any layer within 150 cm was read, with the measured clay.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param subgroup The subgroup reading.
#' @noRd
acric_oxisol_usda <- function(pedon, subgroup = FALSE) {
  h <- pedon$horizons
  n <- nrow(h)
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  col <- function(x) h[[x]] %||% rep(NA_real_, n)
  bases <- col("ca_cmol") + col("mg_cmol") + col("k_cmol") + col("na_cmol")
  al <- ifelse(is.na(col("al_kcl_cmol")), col("al_cmol"), col("al_kcl_cmol"))
  ecec <- ifelse(is.na(col("ecec_cmol")), bases + al, col("ecec_cmol"))
  within <- which(!is.na(h$top_cm) & h$top_cm < ms + 150)
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 13 (pp. 295-305) and Ch. 15 (p. 331)"
  if (isTRUE(subgroup)) {
    r <- 100 * ecec[within] / h$clay_pct[within]
    ok <- ifelse(is.na(r), NA, .kst_round(r, 1) <= 1.5)
    passed <- .usda_or3(ok)
    return(DiagnosticResult$new(name = "acric_oxisol_usda", passed = passed,
      layers = within[ok %in% TRUE], evidence = list(ecec_per_kg_clay = r, threshold = 1.5),
      missing = if (is.na(passed)) c("ecec_cmol", "clay_pct") else character(0), reference = ref))
  }
  ox <- oxic_usda(pedon); ka <- kandic_horizon_usda(pedon)
  L <- intersect(unique(c(if (isTRUE(ox$passed)) ox$layers, if (isTRUE(ka$passed)) ka$layers)), within)
  hor <- .usda_or3(ox$passed, ka$passed)
  app <- .apparent_clay_usda(pedon, factor = 3)
  pk <- col("ph_kcl")
  r <- 100 * ecec / app
  ok <- ifelse(!is.na(r) & .kst_round(r, 2) >= 1.5, FALSE,
               ifelse(!is.na(pk) & .kst_round(pk, 1) < 5, FALSE,
                      ifelse(is.na(r) | is.na(pk), NA, TRUE)))
  passed <- if (length(L)) .usda_or3(ok[L])
            else if (isFALSE(hor)) FALSE
            else if (any(ok[within] %in% TRUE)) NA else FALSE
  DiagnosticResult$new(
    name = "acric_oxisol_usda", passed = passed,
    layers = L[ok[L] %in% TRUE],
    evidence = list(threshold_ecec_clay = 1.5, apparent_ecec = r[L], ph_kcl = pk[L]),
    missing = if (is.na(passed)) unique(c("ecec_cmol", "ph_kcl", ox$missing, ka$missing)) else character(0),
    reference = ref
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
