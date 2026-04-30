# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Helpers for Gelisols Subgroup-level distinctions (Cap 9)
# =============================================================
#
# This file collects helpers used in the Gelisols Subgroup keys
# (Histels / Turbels / Orthels great groups). Each helper maps to
# a Subgroup adjective: "Lithic", "Glacic", "Sphagnic", "Terric",
# "Limnic", "Thapto-Humic", "Fluvaquentic", "Fluventic", "Andic",
# "Vitrandic", "Vertic", "Aquic", "Folistic", "Cumulic", "Spodic",
# "Sulfuric", "Ruptic-Histic", "Psammentic", "Nitric".
#
# All helpers return DiagnosticResult.
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 9, pp 189-198.
# =============================================================


# ---- Histels Suborder qualifier (KST 13ed, Ch 9, p 189) -------------

#' Histels Suborder qualifier (USDA, KST 13ed)
#'
#' Pass when a Gelisol has organic soil materials that:
#' \itemize{
#'   \item Total >= 40 cm cumulative thickness within 0-50 cm; OR
#'   \item Comprise >= 80\% (by volume) of 0-50 cm.
#' }
#' KST 13ed, Ch 9, p 189 (item AA in Key to Suborders).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness_cm Default 40.
#' @param max_top_cm Default 50.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 9, p 189.
#' @export
histel_qualifying_usda <- function(pedon,
                                       min_thickness_cm = 40,
                                       max_top_cm = 50) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "histel_qualifying_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 189"
    ))
  }
  # Organic layers: designation starting with O or H
  org <- which(!is.na(h$designation) &
                   grepl("^[OH]", h$designation) &
                   !is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(org) == 0L) {
    return(DiagnosticResult$new(
      name = "histel_qualifying_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no organic layers in upper 50 cm"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 189"
    ))
  }
  # Cumulative thickness, clamped to 0-50
  tops <- pmax(h$top_cm[org], 0)
  bots <- pmin(h$bottom_cm[org], max_top_cm)
  thk <- sum(pmax(bots - tops, 0), na.rm = TRUE)
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "histel_qualifying_usda", passed = passed,
    layers = if (passed) org else integer(0),
    evidence = list(organic_thickness_cm = thk,
                      threshold_cm = min_thickness_cm,
                      max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 189"
  )
}


# ---- "Lithic" -- lithic contact within X cm -------------------------

#' Lithic contact within X cm of the surface (USDA Subgroup helper)
#'
#' Pass when a horizon designation matches an R contact within
#' \code{max_top_cm}. Default 50 cm (Subgroup-level depth bound).
#' For Gelisols organic soil materials (Folistels), the depth is
#' 50 cm; for Fibristels/Hemistels/Sapristels and other Gelisols,
#' it is 100 cm (KST 13ed, p 46).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 50 cm.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45;
#'   Ch. 9 various Subgroups.
#' @export
lithic_contact_usda <- function(pedon, max_top_cm = 50) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "lithic_contact_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
    ))
  }
  layers <- which(!is.na(h$designation) &
                    grepl("^R(?![/a-z])|^2R|^3R", h$designation, perl = TRUE) &
                    !is.na(h$top_cm) & h$top_cm <= max_top_cm)
  passed <- length(layers) > 0L
  DiagnosticResult$new(
    name = "lithic_contact_usda", passed = passed, layers = layers,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
  )
}


# ---- "Sphagnic" -- Sphagnum-rich fibric organic material -------------

#' Sphagnic Subgroup helper (Histels Fibristels)
#'
#' Pass when 75 percent or more of the fibric soil materials are
#' derived from Sphagnum to a depth of 50 cm or to a contact,
#' whichever is shallower (KST 13ed, p 190).
#'
#' Implementation uses \code{fiber_content_rubbed_pct >= 75} as
#' a proxy. A more specific Sphagnum-fraction column is deferred.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 50.
#' @param min_sphagnum_pct Default 75.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190.
#' @export
sphagnic_usda <- function(pedon, max_top_cm = 50, min_sphagnum_pct = 75) {
  h <- pedon$horizons
  org <- which(!is.na(h$designation) & grepl("^[OH]", h$designation) &
                 !is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(org) == 0L) {
    return(DiagnosticResult$new(
      name = "sphagnic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no organic layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190"
    ))
  }
  fc <- h$fiber_content_rubbed_pct[org]
  miss <- if (all(is.na(fc))) "fiber_content_rubbed_pct" else character(0)
  passing <- org[!is.na(fc) & fc >= min_sphagnum_pct]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "sphagnic_usda", passed = passed, layers = passing,
    evidence = list(fiber_content_rubbed_pct = fc,
                      threshold = min_sphagnum_pct,
                      max_top_cm = max_top_cm,
                      note = "v0.8: uses fiber_content_rubbed_pct as Sphagnum proxy"),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190"
  )
}


# ---- "Terric" -- mineral material >= 30 cm thick within 100 cm -------

#' Terric Subgroup helper (Histels)
#'
#' Pass when a layer of mineral soil material 30 cm or more thick
#' occurs within 100 cm of the soil surface (KST 13ed, p 190).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness_cm Default 30.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 9.
#' @export
terric_usda <- function(pedon, min_thickness_cm = 30, max_top_cm = 100) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "terric_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  # Mineral layers (not O/H) that start within 100 cm
  mineral <- which(!is.na(h$designation) &
                       !grepl("^[OH]", h$designation) &
                       !is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(mineral) == 0L) {
    return(DiagnosticResult$new(
      name = "terric_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no mineral layer within 100 cm"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  # Cumulative thickness of mineral layers within 100 cm
  thk <- pmax(h$bottom_cm[mineral] - h$top_cm[mineral], 0)
  thk_total <- sum(thk[!is.na(thk)])
  passed <- thk_total >= min_thickness_cm
  DiagnosticResult$new(
    name = "terric_usda", passed = passed,
    layers = if (passed) mineral else integer(0),
    evidence = list(mineral_thickness_cm = thk_total,
                      threshold = min_thickness_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190"
  )
}


# ---- "Limnic" -- limnic layers (sedimentary peat / diatomaceous /
#                  marl) within control section ------------------------

#' Limnic Subgroup helper (Histels)
#'
#' Pass when one or more limnic layers (coprogenous earth /
#' diatomaceous earth / marl) with cumulative thickness >= 5 cm
#' occur within the control section (KST 13ed, p 190).
#'
#' Implementation: detects designation containing 'L' (KST notation
#' for limnic) OR \code{layer_origin == "lacustrine"}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness_cm Default 5.
#' @param max_top_cm Default 130 cm (control section).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45;
#'   Ch. 9 Hemistels / Sapristels.
#' @export
limnic_usda <- function(pedon, min_thickness_cm = 5, max_top_cm = 130) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "limnic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  cand <- which(((!is.na(h$designation) &
                     grepl("^L|/L|nLn", h$designation)) |
                    (!is.na(h$layer_origin) &
                       tolower(h$layer_origin) %in%
                         c("lacustrine", "limnic"))) &
                   !is.na(h$top_cm) & h$top_cm < max_top_cm)
  thk_total <- 0
  if (length(cand) > 0L) {
    thk_total <- sum(pmax(h$bottom_cm[cand] - h$top_cm[cand], 0),
                        na.rm = TRUE)
  }
  passed <- thk_total >= min_thickness_cm
  DiagnosticResult$new(
    name = "limnic_usda", passed = passed,
    layers = if (passed) cand else integer(0),
    evidence = list(candidate_layers = cand,
                      thickness_cm = thk_total,
                      threshold_cm = min_thickness_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190"
  )
}


# ---- "Thapto-Humic" -- buried mollic/umbric/melanic/histic ------------

#' Thapto-Humic Subgroup helper
#'
#' Pass when a buried layer meets criteria for histic, mollic,
#' umbric, or melanic epipedon within 200 cm of the soil surface,
#' OR buried O and dark-colored A horizons (V <= 3 moist, combined
#' thickness >= 20 cm, OC >= 1 percent Holocene-age) within 200 cm
#' (KST 13ed, p 189-191).
#'
#' Implementation detects buried horizons via designation containing
#' 'b' (KST notation for buried) AND dark color (V <= 3) within 200 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 200.
#' @param min_thickness_cm Default 20.
#' @param min_oc_pct Default 1.0.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 9 various.
#' @export
thapto_humic_usda <- function(pedon,
                                  max_top_cm = 200,
                                  min_thickness_cm = 20,
                                  min_oc_pct = 1) {
  h <- pedon$horizons
  buried <- which(!is.na(h$designation) & grepl("b", h$designation) &
                    !is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(buried) == 0L) {
    return(DiagnosticResult$new(
      name = "thapto_humic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no buried horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  # Subset: dark colors AND OC >= 1.0
  passing <- integer(0)
  for (i in buried) {
    vm <- h$munsell_value_moist[i]
    oc <- h$oc_pct[i]
    if (!is.na(vm) && vm <= 3 && !is.na(oc) && oc >= min_oc_pct) {
      passing <- c(passing, i)
    }
  }
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "thapto_humic_usda", passed = passed,
    layers = if (passed) passing else integer(0),
    evidence = list(buried_layers = buried,
                      qualifying_layers = passing,
                      cumulative_thickness_cm = thk),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Fluvaquentic" / "Fluventic" -- irregular OC decrease ------------

#' Fluvaquentic Subgroup helper (irregular OC decrease + aquic)
#'
#' Pass when:
#' \itemize{
#'   \item Irregular decrease in organic carbon between 25 cm and
#'         125 cm (or to a densic/lithic/paralithic contact); AND
#'   \item Aquic conditions in some horizon within 75 cm
#'         (\code{aquic_conditions_usda(pedon, max_top_cm = 75)}).
#' }
#'
#' Implementation: tests whether OC values are non-monotonic (some
#' upward variation) within 25-125 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 9.
#' @export
fluvaquentic_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "fluvaquentic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  # Layers in 25-125 cm
  band <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 125)
  oc_band <- h$oc_pct[band]
  irregular <- FALSE
  if (length(oc_band) >= 2L) {
    diffs <- diff(oc_band)
    irregular <- any(!is.na(diffs) & diffs > 0)  # any upward jump
  }
  aq <- aquic_conditions_usda(pedon, max_top_cm = 75)
  passed <- isTRUE(irregular) && isTRUE(aq$passed)
  DiagnosticResult$new(
    name = "fluvaquentic_usda", passed = passed,
    layers = if (passed) band else integer(0),
    evidence = list(oc_band = oc_band, irregular = irregular,
                      aquic = aq$passed),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


#' Fluventic Subgroup helper (irregular OC decrease, NO aquic req.)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
fluventic_usda <- function(pedon) {
  h <- pedon$horizons
  band <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 125)
  oc_band <- h$oc_pct[band]
  irregular <- FALSE
  if (length(oc_band) >= 2L) {
    diffs <- diff(oc_band)
    irregular <- any(!is.na(diffs) & diffs > 0)
  }
  DiagnosticResult$new(
    name = "fluventic_usda", passed = isTRUE(irregular),
    layers = if (isTRUE(irregular)) band else integer(0),
    evidence = list(oc_band = oc_band, irregular = irregular),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Andic" / "Vitrandic" ------------------------------------------

#' Andic Subgroup helper (USDA, KST 13ed)
#'
#' Pass when, throughout one or more horizons with total thickness
#' >= 18 cm within 75 cm of the surface:
#' \itemize{
#'   \item bulk_density_g_cm3 <= 1.0 (at 33 kPa); AND
#'   \item Al + 0.5 * Fe (oxalate-extractable) > 1.0 percent.
#' }
#' KST 13ed, p 117 (Andisols core, applies to subgroup criteria too).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
andic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 75)
  passing <- integer(0)
  miss <- character(0)
  for (i in cand) {
    bd <- h$bulk_density_g_cm3[i]
    al <- h$al_ox_pct[i]
    fe <- h$fe_ox_pct[i]
    if (is.na(bd)) miss <- c(miss, "bulk_density_g_cm3")
    if (is.na(al)) miss <- c(miss, "al_ox_pct")
    if (is.na(fe)) miss <- c(miss, "fe_ox_pct")
    if (!is.na(bd) && bd <= 1.0 && !is.na(al) && !is.na(fe) &&
          (al + 0.5 * fe) > 1.0) {
      passing <- c(passing, i)
    }
  }
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 18
  DiagnosticResult$new(
    name = "andic_subgroup_usda", passed = passed,
    layers = if (passed) passing else integer(0),
    evidence = list(passing_layers = passing, thickness_cm = thk),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6 / Ch. 9"
  )
}


#' Vitrandic Subgroup helper (USDA, KST 13ed)
#'
#' Pass when, throughout one or more horizons with total thickness
#' >= 18 cm within 75 cm of the surface, BOTH:
#' \itemize{
#'   \item More than 35\% (volume) particles >= 2 mm of which
#'         > 66\% are cinders/pumice; OR fine-earth has >= 30\%
#'         particles 0.02-2 mm AND >= 5\% volcanic glass (in 0.02-2 mm); AND
#'   \item (Al + 0.5 * Fe) * 60 + volcanic_glass_pct >= 30.
#' }
#' KST 13ed, Ch 9 various.
#'
#' Implementation simplified to the volcanic-glass branch:
#' volcanic_glass_pct >= 5 AND (Al + 0.5 * Fe) * 60 + volcanic_glass_pct >= 30.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
vitrandic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 75)
  passing <- integer(0)
  miss <- character(0)
  for (i in cand) {
    al <- h$al_ox_pct[i]
    fe <- h$fe_ox_pct[i]
    vg <- h$volcanic_glass_pct[i]
    cf <- h$coarse_fragments_pct[i]
    if (is.na(vg)) miss <- c(miss, "volcanic_glass_pct")
    branch_a <- !is.na(cf) && cf > 35  # cinders/pumice branch (proxy)
    branch_b <- !is.na(vg) && vg >= 5
    score <- if (!is.na(al) && !is.na(fe) && !is.na(vg))
               (al + 0.5 * fe) * 60 + vg
             else NA_real_
    if (isTRUE(branch_a || branch_b) && !is.na(score) && score >= 30) {
      passing <- c(passing, i)
    }
  }
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 18
  DiagnosticResult$new(
    name = "vitrandic_subgroup_usda", passed = passed,
    layers = if (passed) passing else integer(0),
    evidence = list(passing_layers = passing, thickness_cm = thk),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6 / Ch. 9"
  )
}


# ---- "Vertic" -- shrink-swell features ------------------------------

#' Vertic Subgroup helper (USDA, KST 13ed)
#'
#' Pass when EITHER:
#' \itemize{
#'   \item Cracks within 125 cm of the mineral soil surface that
#'         are >= 5 mm wide through a thickness >= 30 cm AND
#'         slickensides or wedge-shaped peds in a layer >= 15 cm
#'         thick within 125 cm; OR
#'   \item Linear extensibility (LE) >= 6.0 cm between surface and
#'         100 cm (or to a densic/lithic/paralithic contact).
#' }
#'
#' Implementation: tests cracks_width_cm >= 0.5 AND cracks_depth_cm
#' >= 30 AND slickensides present, OR sum(thickness * cole_value)
#' >= 6 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
vertic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  miss <- character(0)
  # Branch 1: cracks
  cw <- h$cracks_width_cm
  cd <- h$cracks_depth_cm
  ss <- h$slickensides
  cracks_ok <- any(!is.na(cw) & cw >= 0.5 & !is.na(cd) & cd >= 30) &&
                 any(!is.na(ss) & tolower(ss) %in%
                       c("few", "common", "many", "continuous"))
  # Branch 2: LE >= 6
  cole <- h$cole_value
  thk <- pmax(h$bottom_cm - h$top_cm, 0)
  in_100 <- !is.na(h$top_cm) & h$top_cm < 100
  le_total <- sum(thk[in_100] * cole[in_100], na.rm = TRUE)
  le_ok <- le_total >= 6
  passed <- isTRUE(cracks_ok) || isTRUE(le_ok)
  DiagnosticResult$new(
    name = "vertic_subgroup_usda", passed = passed,
    layers = which(in_100),
    evidence = list(cracks_ok = cracks_ok, le_total_cm = le_total),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9 various"
  )
}


# ---- "Aquic" Subgroup helper (within 100 cm) ------------------------

#' Aquic Subgroup helper (within 100 cm of mineral soil surface)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
aquic_subgroup_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 100)
  res$name <- "aquic_subgroup_usda"
  res
}


# ---- "Folistic" Subgroup helper -------------------------------------

#' Folistic Subgroup helper (folistic_epipedon present)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
folistic_subgroup_usda <- function(pedon) {
  res <- folistic_epipedon_usda(pedon)
  res$name <- "folistic_subgroup_usda"
  res
}


# ---- "Cumulic" Subgroup helper --------------------------------------

#' Cumulic Subgroup helper (Mollorthels / Umbrorthels)
#'
#' Pass when:
#' \itemize{
#'   \item Mollic or umbric epipedon >= 40 cm thick with texture
#'         finer than loamy fine sand; AND
#'   \item Slope < 25 percent.
#' }
#'
#' Slope is taken from \code{site$slope_pct} when available; if NA,
#' assumed to satisfy (TRUE).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
cumulic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  ms_top <- .mineral_soil_surface_cm(h)
  if (is.na(ms_top)) ms_top <- 0
  cand <- which(!is.na(h$designation) & grepl("^A", h$designation) &
                  !is.na(h$top_cm) & h$top_cm <= ms_top + 5)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "cumulic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no surface A horizon"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  thk_a <- sum(pmax(h$bottom_cm[cand] - h$top_cm[cand], 0), na.rm = TRUE)
  # Texture finer than LFS: clay > 15 OR sand <= 70
  textures_ok <- vapply(cand, function(i) {
    cl <- h$clay_pct[i]; sd <- h$sand_pct[i]
    isTRUE(.is_finer_than_loamy_fine_sand(cl, h$silt_pct[i], sd))
  }, logical(1))
  texture_ok <- any(textures_ok)
  slope <- pedon$site$slope_pct %||% NA_real_
  slope_ok <- is.na(slope) || slope < 25
  passed <- thk_a >= 40 && isTRUE(texture_ok) && isTRUE(slope_ok)
  DiagnosticResult$new(
    name = "cumulic_subgroup_usda", passed = passed,
    layers = if (passed) cand else integer(0),
    evidence = list(a_thickness_cm = thk_a,
                      texture_ok = texture_ok,
                      slope_pct = slope),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Spodic" Subgroup helper (Psammorthels / Psammoturbels) -------

#' Spodic Subgroup helper for Psammorthels/Psammoturbels
#'
#' Pass when a horizon >= 5 cm thick has any of:
#' \itemize{
#'   \item In >= 25\% of pedon, extremely weakly coherent or more
#'         coherent due to pedogenic cementation by OM and Al
#'         (with or without Fe); OR
#'   \item Al + 0.5 * Fe (oxalate) >= 0.25, and half that or less
#'         in an overlying horizon; OR
#'   \item ODOE >= 0.12, and value half as high or lower in an
#'         overlying horizon.
#' }
#'
#' Implementation simplified to: any horizon with
#' (al_ox_pct + 0.5 * fe_ox_pct) >= 0.25 with an overlying layer
#' having <= half that value.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
spodic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) < 2L) {
    return(DiagnosticResult$new(
      name = "spodic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "fewer than 2 horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  ord <- order(h$top_cm, na.last = TRUE)
  passing <- integer(0); miss <- character(0)
  for (k in seq_along(ord)) {
    if (k == 1L) next
    i <- ord[k]
    al <- h$al_ox_pct[i]; fe <- h$fe_ox_pct[i]
    if (is.na(al)) miss <- c(miss, "al_ox_pct")
    if (is.na(fe)) miss <- c(miss, "fe_ox_pct")
    val <- if (!is.na(al) && !is.na(fe)) al + 0.5 * fe else NA_real_
    if (is.na(val) || val < 0.25) next
    above <- ord[k - 1L]
    al_a <- h$al_ox_pct[above]; fe_a <- h$fe_ox_pct[above]
    val_a <- if (!is.na(al_a) && !is.na(fe_a))
               al_a + 0.5 * fe_a
             else NA_real_
    if (!is.na(val_a) && val_a <= val / 2) {
      passing <- c(passing, i)
    }
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "spodic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(threshold_al_fe = 0.25),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Sulfuric" / "Salic" / "Gypsic" / "Calcic" / "Petrogypsic" /
#       "Argillic" / "Natric" Subgroup helpers ------------------------

#' Sulfuric horizon helper (USDA, KST 13ed Ch 3)
#'
#' Pass when sulfidic_s_pct present in any horizon within
#' \code{max_top_cm} (proxy: KST sulfuric horizon requires pH < 4.0
#' OR sulfidic materials AND certain mottle colors; this v0.8 uses
#' sulfidic_s_pct >= 0.75 as proxy).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @param min_s_pct Default 0.75.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
sulfuric_horizon_usda <- function(pedon, max_top_cm = 100,
                                       min_s_pct = 0.75) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  miss <- character(0)
  if (all(is.na(h$sulfidic_s_pct[cand]))) miss <- "sulfidic_s_pct"
  passing <- cand[!is.na(h$sulfidic_s_pct[cand]) &
                      h$sulfidic_s_pct[cand] >= min_s_pct]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "sulfuric_horizon_usda", passed = passed, layers = passing,
    evidence = list(threshold_pct = min_s_pct, max_top_cm = max_top_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3"
  )
}


#' Petrogypsic horizon helper (USDA)
#'
#' Pass when a horizon has \code{cementation_class} in
#' \{strongly, indurated\} AND \code{caso4_pct >= 5} within
#' \code{max_top_cm}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
petrogypsic_horizon_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  miss <- character(0)
  cem <- h$cementation_class[cand]
  cas <- h$caso4_pct[cand]
  if (all(is.na(cem))) miss <- c(miss, "cementation_class")
  if (all(is.na(cas))) miss <- c(miss, "caso4_pct")
  passing <- cand[!is.na(cem) & tolower(cem) %in%
                                  c("strongly", "indurated") &
                      !is.na(cas) & cas >= 5]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "petrogypsic_horizon_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 39"
  )
}


#' Nitric Subgroup helper (Anhyturbels / Anhyorthels)
#'
#' Pass when a horizon >= 15 cm thick has nitrate concentration
#' >= 118 mmol(-)/L AND (thickness * concentration) >= 3500.
#' (Nitrate is not in the schema; v0.8 returns NA with missing flag.)
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
nitric_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "nitric_subgroup_usda", passed = NA, layers = integer(0),
    evidence = list(reason = "no nitrate column in schema (v0.8 deferred)"),
    missing = "nitrate_mmol_l",
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9, p 192"
  )
}


# ---- "Ruptic-Histic" / "Ruptic" -------------------------------------

#' Ruptic-Histic Subgroup helper
#'
#' Pass when surface organic soil materials are discontinuous OR
#' change in thickness fourfold or more within a pedon. v0.8
#' approximation: returns FALSE -- requires multi-pedon transect data
#' not in the single-pedon schema. Refinement deferred.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
ruptic_histic_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "ruptic_histic_subgroup_usda", passed = FALSE,
    layers = integer(0),
    evidence = list(reason = "Requires multi-pedon transect data; deferred to v0.9"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


#' Ruptic Subgroup helper (Histoturbels / Historthels)
#'
#' Pass when more than 40\% (volume) organic soil materials from
#' surface to 50 cm in 75\% or LESS of the pedon. v0.8 also
#' deferred -- requires multi-pedon data.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
ruptic_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "ruptic_subgroup_usda", passed = FALSE, layers = integer(0),
    evidence = list(reason = "Requires multi-pedon data; deferred to v0.9"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Psammentic" Subgroup helper -----------------------------------

#' Psammentic Subgroup helper (Aquorthels)
#'
#' Pass when, in particle-size control section: < 35\% rock
#' fragments AND texture class loamy fine sand or coarser in all
#' layers.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
psammentic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "psammentic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no candidate layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
    ))
  }
  cf <- h$coarse_fragments_pct[cand]
  cf_ok <- all(is.na(cf) | cf < 35)
  texture_ok <- all(vapply(cand, function(i) {
    cl <- h$clay_pct[i]; sd <- h$sand_pct[i]
    if (is.na(cl) || is.na(sd)) return(NA)
    !isTRUE(.is_finer_than_loamy_fine_sand(cl, h$silt_pct[i], sd))
  }, logical(1)), na.rm = TRUE)
  passed <- isTRUE(cf_ok) && isTRUE(texture_ok)
  DiagnosticResult$new(
    name = "psammentic_subgroup_usda", passed = passed,
    layers = if (passed) cand else integer(0),
    evidence = list(cf_ok = cf_ok, texture_ok = texture_ok),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- Argillic horizon helper (delegating to WRB argic for now) -----

#' Argillic horizon helper (USDA, KST 13ed Ch 3)
#'
#' Wrapper around argillic_usda that simply re-exports the
#' DiagnosticResult with a max-depth check (default 100 cm for
#' Argiorthels Subgroup keys).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100 cm.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
argillic_within_usda <- function(pedon, max_top_cm = 100) {
  res <- argillic_usda(pedon)
  if (isTRUE(res$passed) && length(res$layers) > 0L) {
    h <- pedon$horizons
    in_depth <- !is.na(h$top_cm[res$layers]) &
                  h$top_cm[res$layers] < max_top_cm
    res$layers <- res$layers[in_depth]
    res$passed <- length(res$layers) > 0L
  }
  res$name <- "argillic_within_usda"
  res
}


# ---- Natric horizon helper (delegating to WRB natric_horizon) -------

#' Natric horizon helper (USDA, KST 13ed Ch 3)
#'
#' Pass when natric_horizon (WRB natric: argillic + ESP > 15) is
#' present.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
natric_horizon_usda <- function(pedon) {
  res <- natric_horizon(pedon)
  res$name <- "natric_horizon_usda"
  res
}


# ---- Salic / Gypsic / Calcic helpers (delegating WRB) ---------------

#' Salic horizon (USDA, delegates to WRB salic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
salic_horizon_usda <- function(pedon, max_top_cm = 100) {
  res <- salic(pedon)
  res$name <- "salic_horizon_usda"
  if (isTRUE(res$passed) && length(res$layers) > 0L) {
    h <- pedon$horizons
    res$layers <- res$layers[!is.na(h$top_cm[res$layers]) &
                                  h$top_cm[res$layers] < max_top_cm]
    res$passed <- length(res$layers) > 0L
  }
  res
}

#' Gypsic horizon (USDA, delegates to WRB gypsic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
gypsic_horizon_usda <- function(pedon, max_top_cm = 100) {
  res <- gypsic(pedon)
  res$name <- "gypsic_horizon_usda"
  if (isTRUE(res$passed) && length(res$layers) > 0L) {
    h <- pedon$horizons
    res$layers <- res$layers[!is.na(h$top_cm[res$layers]) &
                                  h$top_cm[res$layers] < max_top_cm]
    res$passed <- length(res$layers) > 0L
  }
  res
}

#' Calcic horizon (USDA, delegates to WRB calcic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
calcic_horizon_usda <- function(pedon, max_top_cm = 100) {
  res <- calcic(pedon)
  res$name <- "calcic_horizon_usda"
  if (isTRUE(res$passed) && length(res$layers) > 0L) {
    h <- pedon$horizons
    res$layers <- res$layers[!is.na(h$top_cm[res$layers]) &
                                  h$top_cm[res$layers] < max_top_cm]
    res$passed <- length(res$layers) > 0L
  }
  res
}
