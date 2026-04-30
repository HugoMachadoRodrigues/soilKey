# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Andisols helpers -- Chapter 6, pp 117-136
# =============================================================
#
# Andisols are mineral soils with andic soil properties dominant
# in the upper part of the profile. They form predominantly from
# weathered volcanic ash and other tephra, but also from materials
# weathered from non-volcanic sources high in active Al + Fe.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 6.
# =============================================================


# ---- Andic soil properties (full KST 13ed Ch 3, p 32) ---------------

#' Andic soil properties (USDA, KST 13ed Ch 3, p 32)
#'
#' Soil materials with one or both of the following:
#' \itemize{
#'   \item bulk_density <= 0.90 g/cm3 AND
#'         Al + 0.5*Fe (oxalate) >= 2.0\% AND
#'         phosphate_retention >= 85\%; OR
#'   \item Al + 0.5*Fe (oxalate) >= 0.4\% AND
#'         phosphate_retention >= 25\% AND
#'         volcanic_glass_pct varying with the texture-class proxy
#'         (deferred -- requires fine-earth fraction analysis).
#' }
#'
#' Implementation (v0.8.6): primary "humic-andic" branch (bd <= 0.9 +
#' Al+0.5Fe >= 2 + Pret >= 85). The vitric-andic branch (lower Al+Fe
#' but high glass content) is partially captured.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 32.
#' @export
andic_soil_properties_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "andic_soil_properties_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 32"
    ))
  }
  passing <- integer(0); miss <- character(0)
  for (i in seq_len(nrow(h))) {
    bd  <- h$bulk_density_g_cm3[i]
    al  <- h$al_ox_pct[i]
    fe  <- h$fe_ox_pct[i]
    pr  <- h$phosphate_retention_pct[i]
    vg  <- h$volcanic_glass_pct[i]
    alfe <- if (!is.na(al) && !is.na(fe)) al + 0.5 * fe else NA_real_
    # Primary branch (humic-andic)
    primary <- !is.na(bd) && bd <= 0.90 &&
                 !is.na(alfe) && alfe >= 2.0 &&
                 !is.na(pr) && pr >= 85
    # Vitric-andic branch
    vitric <- !is.na(alfe) && alfe >= 0.4 &&
                !is.na(pr) && pr >= 25 &&
                !is.na(vg) && vg >= 5
    if (isTRUE(primary) || isTRUE(vitric)) passing <- c(passing, i)
    if (is.na(bd))  miss <- c(miss, "bulk_density_g_cm3")
    if (is.na(al))  miss <- c(miss, "al_ox_pct")
    if (is.na(fe))  miss <- c(miss, "fe_ox_pct")
    if (is.na(pr))  miss <- c(miss, "phosphate_retention_pct")
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "andic_soil_properties_usda", passed = passed,
    layers = passing,
    evidence = list(passing_layers = passing),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 32"
  )
}


# ---- Andisol Order qualifier ----------------------------------------

#' Andisol Order qualifier (USDA, KST 13ed Ch 3, p 7)
#'
#' Andisols have andic soil properties in 60\%+ of the thickness
#' between the surface and either:
#' \itemize{
#'   \item a depth of 60 cm; or
#'   \item a densic, lithic, or paralithic contact, a duripan, or
#'         a petrocalcic horizon (whichever is shallower).
#' }
#' v0.8.6 implementation: pass when total thickness of layers with
#' andic_soil_properties is >= 0.6 * (depth from surface to 60 cm).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117.
#' @export
andisol_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  asp <- andic_soil_properties_usda(pedon)
  passing <- intersect(asp$layers,
                          which(!is.na(h$top_cm) & h$top_cm < 60))
  thk_andic <- if (length(passing) > 0L)
                  sum(pmax(pmin(h$bottom_cm[passing], 60) -
                                pmax(h$top_cm[passing], 0), 0),
                        na.rm = TRUE)
                else 0
  passed <- thk_andic >= 36   # 60% of 60 cm
  DiagnosticResult$new(
    name = "andisol_qualifying_usda", passed = passed,
    layers = if (passed) passing else integer(0),
    evidence = list(andic_thickness_cm = thk_andic,
                      threshold_cm = 36),
    missing = asp$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117"
  )
}


# ---- Aquand Suborder qualifier --------------------------------------

#' Aquands Suborder qualifier (Cap 6, p 117)
#' Pass when histic OR aquic conditions in 40-50 cm with redox
#' features. Simplified: histic OR aquic_conditions(max_top=50).
#' @export
aquand_qualifying_usda <- function(pedon) {
  hi <- histic_epipedon_usda(pedon)
  aq <- aquic_conditions_usda(pedon, max_top_cm = 50)
  passed <- isTRUE(hi$passed) || isTRUE(aq$passed)
  DiagnosticResult$new(
    name = "aquand_qualifying_usda", passed = passed,
    layers = c(hi$layers, aq$layers),
    evidence = list(histic = hi, aquic = aq),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117"
  )
}


# ---- Vitrand Suborder qualifier -------------------------------------

#' Vitrands qualifier (Cap 6, pp 117-118)
#' Pass when 1500 kPa water retention < 15\% (air-dried) and
#' < 30\% (undried) throughout 60\%+ of the thickness. v0.8 proxy:
#' uses water_content_1500kpa < 15\%.
#' @export
vitrand_qualifying_usda <- function(pedon, max_top_cm = 60) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  wr <- h$water_content_1500kpa[cand]
  miss <- if (all(is.na(wr))) "water_content_1500kpa" else character(0)
  passing <- cand[!is.na(wr) & wr < 15]
  thk_pass <- if (length(passing) > 0L)
                sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                      na.rm = TRUE)
              else 0
  thk_total <- if (length(cand) > 0L)
                 sum(pmax(h$bottom_cm[cand] - h$top_cm[cand], 0),
                       na.rm = TRUE)
               else 0
  passed <- thk_total > 0 && thk_pass >= 0.6 * thk_total
  DiagnosticResult$new(
    name = "vitrand_qualifying_usda", passed = passed,
    layers = passing,
    evidence = list(thk_pass_cm = thk_pass, thk_total_cm = thk_total),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117-118"
  )
}


# ---- Hydric / Hydrudands / Hydrocryands ----------------------------

#' Hydric (Andisols): 1500 kPa water retention >= 70\% on undried
#' samples throughout a 35+ cm layer within 100 cm.
#' @export
hydric_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  wr <- h$water_content_1500kpa[cand]
  miss <- if (all(is.na(wr))) "water_content_1500kpa" else character(0)
  passing <- cand[!is.na(wr) & wr >= 70]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 35
  DiagnosticResult$new(
    name = "hydric_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 118"
  )
}


# ---- Melanic / Fulvic Andisols --------------------------------------

#' Melanic Andisols: melanic_epipedon present.
#' @export
melanic_andisol_usda <- function(pedon) {
  res <- melanic_epipedon_usda(pedon)
  res$name <- "melanic_andisol_usda"
  res
}


#' Fulvic Andisols: similar to melanic but with melanic_index > 1.70
#' (more humic acid). v0.8: detected via OC >= 6 in cumulative 30 cm
#' but WITHOUT melanic_epipedon (since melanic requires index <= 1.70).
#' @export
fulvic_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 30)
  oc <- h$oc_pct[cand]
  passing <- cand[!is.na(oc) & oc >= 6]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  has_melanic <- isTRUE(melanic_epipedon_usda(pedon)$passed)
  passed <- thk >= 30 && !has_melanic
  DiagnosticResult$new(
    name = "fulvic_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk, has_melanic = has_melanic),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 122"
  )
}


# ---- Acric / Acraquoxic / Acrudoxic Andisols ------------------------

#' Acric Subgroup helper (Andisols Acrudoxic / Acraquoxic /
#' Acrustoxic / etc.)
#'
#' Pass when the sum of extractable bases (NH4OAc) plus 1N KCl-Al
#' is < 2.0 cmol(+)/kg in fine earth, in a 30+ cm layer between
#' 25 and 100 cm. v0.8 proxy: ECEC <= 2.0 in B horizons.
#' @export
acric_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 100)
  ec  <- h$ecec_cmol[cand]
  miss <- if (all(is.na(ec))) "ecec_cmol" else character(0)
  passing <- cand[!is.na(ec) & ec < 2.0]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 30
  DiagnosticResult$new(
    name = "acric_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Alic Subgroup (high KCl-extractable Al) ------------------------

#' Alic Subgroup helper (Andisols)
#' Pass when al_kcl_cmol > 2.0 in a 10+ cm layer between 25 and 50 cm.
#' @export
alic_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 50)
  al <- h$al_kcl_cmol[cand]
  miss <- if (all(is.na(al))) "al_kcl_cmol" else character(0)
  passing <- cand[!is.na(al) & al > 2.0]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 10
  DiagnosticResult$new(
    name = "alic_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Pachic Subgroup helper (thick mollic/umbric, 50+ cm) -----------

#' Pachic Subgroup helper (Andisols, Mollisols)
#' Pass when mollic OR umbric epipedon is >= 50 cm thick.
#' @export
pachic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  layers <- unique(c(mo$layers, um$layers))
  thk <- if (length(layers) > 0L)
           sum(pmax(h$bottom_cm[layers] - h$top_cm[layers], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 50
  DiagnosticResult$new(
    name = "pachic_subgroup_usda", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(thickness_cm = thk, mollic = mo, umbric = um),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6 / Ch. 12"
  )
}


# ---- Thaptic Subgroup helper (buried mollic-like 25-100 cm) ---------

#' Thaptic Subgroup helper (Andisols)
#' Pass when, between 25 and 100 cm, a 10+ cm layer with OC > 3.0\%
#' and mollic colors exists, underlying lighter horizons.
#' @export
thaptic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 100)
  oc <- h$oc_pct[cand]
  vm <- h$munsell_value_moist[cand]
  passing <- cand[!is.na(oc) & oc > 3 & !is.na(vm) & vm <= 3]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 10
  DiagnosticResult$new(
    name = "thaptic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Eutric Subgroup helper -----------------------------------------

#' Eutric Subgroup helper (Andisols)
#' Pass when base_saturation (sum-of-cations) >= 50\% in some part.
#' @export
eutric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  bs <- h$bs_pct
  passed <- any(!is.na(bs) & bs >= 50)
  DiagnosticResult$new(
    name = "eutric_subgroup_usda", passed = passed,
    layers = which(!is.na(bs) & bs >= 50),
    evidence = list(bs_pct = bs),
    missing = if (all(is.na(bs))) "bs_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Vitric Subgroup helper (proxy via volcanic_glass_pct) ----------

#' Vitric Subgroup helper (Andisols)
#' Pass when volcanic_glass_pct >= 30 in a 25+ cm layer within 100 cm.
#' @export
vitric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  vg <- h$volcanic_glass_pct[cand]
  miss <- if (all(is.na(vg))) "volcanic_glass_pct" else character(0)
  passing <- cand[!is.na(vg) & vg >= 30]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 25
  DiagnosticResult$new(
    name = "vitric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Spodic Andisols Subgroup helper --------------------------------

#' Spodic-Andisols Subgroup helper
#' Pass when albic horizon overlies a cambic OR spodic horizon,
#' OR when a spodic horizon is present in 50%+ of the pedon.
#' @export
spodic_andisol_usda <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  al <- albic_horizon_usda(pedon)
  passed <- isTRUE(sp$passed) ||
              (isTRUE(al$passed) &&
                 isTRUE(cambic(pedon)$passed))
  DiagnosticResult$new(
    name = "spodic_andisol_usda", passed = passed,
    layers = c(sp$layers, al$layers),
    evidence = list(spodic = sp, albic = al),
    missing = unique(c(sp$missing %||% character(0),
                          al$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Humic Andisols Subgroup helper ---------------------------------

#' Humic Andisols Subgroup helper
#' Pass when mollic OR umbric epipedon present.
#' @export
humic_andisol_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  passed <- isTRUE(mo$passed) || isTRUE(um$passed)
  DiagnosticResult$new(
    name = "humic_andisol_usda", passed = passed,
    layers = unique(c(mo$layers, um$layers)),
    evidence = list(mollic = mo, umbric = um),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}
