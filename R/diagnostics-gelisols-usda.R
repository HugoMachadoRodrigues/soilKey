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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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


#' Fluventic Subgroup helper (organic carbon of the Fluvents)
#'
#' 0.2 \% or more organic carbon at 125 cm below the mineral soil surface,
#' or an irregular decrease of organic carbon between 25 cm and 125 cm (or a
#' shallower densic, lithic or paralithic contact): a rise of more than 0.05
#' percentage points from one layer to the next one down (audit L15; KST 13th
#' ed., p. 165). v0.9.419: up to v0.9.418 only the layers whose top lay
#' between 25 and 125 cm were read (a layer straddling 25 cm was left out),
#' any rise counted, and a missing value was read as no rise; it is now
#' three-valued. The slope and human-transported-material conditions some
#' Fluventic subgroups add are read by the subgroup lists (a later wave).
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
fluventic_usda <- function(pedon) {
  oc <- .usda_fluvent_oc(pedon)
  passed <- .usda_any3(c(oc$at_125, oc$irregular))
  DiagnosticResult$new(
    name = "fluventic_usda", passed = passed,
    layers = if (isTRUE(passed)) oc$layers else integer(0),
    evidence = list(oc_125 = oc$at_125, irregular = oc$irregular),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, p. 165 (Fluvents, item LD.3)"
  )
}


# ---- "Andic" / "Vitrandic" ------------------------------------------

# Per-layer answers (TRUE / FALSE / NA) of the Andic and Vitrandic subgroup
# criteria of the mineral orders (v0.9.417; KST 13th ed., e.g. Andic,
# Vitrandic and Aquandic Dystrudepts, p. 225):
#   andic: a fine earth with a bulk density at 33 kPa of 1.0 g/cm3 or less and
#     Al + 1/2 Fe (oxalate) more than 1.0 %; where the bulk density is not
#     measured a pH NaF of 9.5 or more in a carbonate-free layer stands in
#     for it (reading L2);
#   vitrandic: more than 35 % (by volume) fragments of 2 mm or more, of which
#     more than 66 % are cinders, pumice and pumicelike fragments
#     (pyroclastic_fragments_pct; a fragment kind not recorded is absent), or
#     a fine earth with 30 % or more 0.02 to 2.0 mm (particles_002_2mm_pct;
#     otherwise sand >= 30 % passes and sand + silt < 30 % fails) with 5 % or
#     more volcanic glass and 60 (Al + 1/2 Fe) + glass >= 30.
.andic_subgroup_layers_usda <- function(h) {
  n <- nrow(h)
  col <- function(x) h[[x]] %||% rep(NA_real_, n)
  bd <- col("bulk_density_g_cm3"); al <- col("al_ox_pct"); fe <- col("fe_ox_pct")
  naf <- col("ph_naf"); caco3 <- col("caco3_pct"); vg <- col("volcanic_glass_pct")
  cf <- col("coarse_fragments_pct"); py <- col("pyroclastic_fragments_pct")
  sand <- col("sand_pct"); silt <- col("silt_pct"); p002 <- col("particles_002_2mm_pct")
  alfe <- al + fe / 2
  alfe_gt1 <- ifelse(!is.na(alfe), alfe > 1.0, ifelse(!is.na(al) & al > 1.0, TRUE, NA))
  naf_proxy <- is.na(bd) & !is.na(naf) & naf >= 9.5 & !(!is.na(caco3) & caco3 > 0)
  andic <- ifelse(naf_proxy, TRUE, bd <= 1.0) & alfe_gt1
  cinders <- !is.na(cf) & cf > 35 & !is.na(py) & py > 0.66 * cf
  f30 <- ifelse(!is.na(p002), p002 >= 30,
           ifelse(!is.na(sand) & sand >= 30, TRUE,
             ifelse(!is.na(sand) & !is.na(silt) & sand + silt < 30, FALSE, NA)))
  score <- ifelse(!is.na(alfe) & 60 * alfe >= 30, TRUE, 60 * alfe + vg >= 30)
  vitr <- cinders | (f30 & (vg >= 5) & score)
  miss_a <- unique(c(if (any(is.na(andic) & is.na(bd) & !naf_proxy)) "bulk_density_g_cm3",
                     if (any(is.na(andic) & is.na(al))) "al_ox_pct",
                     if (any(is.na(andic) & is.na(fe))) "fe_ox_pct"))
  miss_v <- unique(c(if (any(is.na(vitr) & is.na(vg))) "volcanic_glass_pct",
                     if (any(is.na(vitr) & is.na(f30))) "particles_002_2mm_pct",
                     if (any(is.na(vitr) & is.na(al))) "al_ox_pct"))
  list(andic = andic, vitrandic = vitr, naf = which(naf_proxy),
       missing_andic = miss_a, missing_vitrandic = miss_v)
}

# 18 cm or more of layers with `ok` within 75 cm of the mineral soil surface:
# TRUE / FALSE / NA, with the layers.
.andic_subgroup_18_usda <- function(h, ok) {
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (!is.na(h$designation) & grepl("^[0-9]*[OHL]", h$designation))
  mt <- h$top_cm[!org & !is.na(h$top_cm)]
  ms <- if (length(mt)) min(mt) else 0
  ov <- pmax(pmin(h$bottom_cm, ms + 75) - pmax(h$top_cm, ms), 0)
  ov[is.na(ov)] <- 0
  yes <- sum(ov[ok %in% TRUE]); may <- sum(ov[is.na(ok)])
  list(passed = if (yes >= 18) TRUE else if (yes + may < 18) FALSE else NA,
       layers = which(ok %in% TRUE & ov > 0), thickness_cm = yes,
       undecided_cm = may, mineral_surface_cm = ms)
}

#' Andic Subgroup helper (USDA, KST 13th ed., e.g. p. 225)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface, a fine earth with a bulk density
#' (33 kPa) of 1.0 g/cm3 or less and Al + 1/2 Fe (oxalate) of more than 1.0\%.
#' v0.9.417: the horizons are clipped to 75 cm below the mineral soil surface
#' (up to v0.9.416 a horizon starting above 75 cm counted whole); a value not
#' recorded leaves the test undecided (it failed before); where the bulk
#' density is not measured, a pH NaF of 9.5 or more stands in for it (named
#' in \code{notes}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
andic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 225 (e.g. Andic Dystrudepts)"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(name = "andic_subgroup_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  L <- .andic_subgroup_layers_usda(h)
  r <- .andic_subgroup_18_usda(h, L$andic)
  naf_used <- isTRUE(r$passed) && length(intersect(L$naf, r$layers)) > 0L
  DiagnosticResult$new(
    name = "andic_subgroup_usda", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = list(passing_layers = r$layers, thickness_cm = r$thickness_cm,
                    undecided_cm = r$undecided_cm),
    missing = if (is.na(r$passed)) L$missing_andic else character(0),
    reference = ref,
    notes = if (naf_used) "pH NaF >= 9.5 read in place of the bulk density, not measured (reading L2)"
            else NA_character_
  )
}


#' Vitrandic Subgroup helper (USDA, KST 13th ed., e.g. p. 225)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface, one or both of: more than 35\%
#' (by volume) fragments of 2 mm or more, of which more than 66\% are cinders,
#' pumice and pumicelike fragments; or a fine earth with 30\% or more
#' particles 0.02 to 2.0 mm, with 5\% or more volcanic glass in that fraction
#' and 60 (Al + 1/2 Fe) + glass >= 30. v0.9.417: the cinder branch needs the
#' cinders (\code{pyroclastic_fragments_pct}; up to v0.9.416 any coarse
#' fragments over 35\%), the 60 (Al + 1/2 Fe) + glass test belongs to the
#' fine-earth branch only, the horizons are clipped to 75 cm and the answer
#' is three-valued (volcanic glass not measured leaves it undecided).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
vitrandic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 225 (e.g. Vitrandic Dystrudepts)"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(name = "vitrandic_subgroup_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  L <- .andic_subgroup_layers_usda(h)
  r <- .andic_subgroup_18_usda(h, L$vitrandic)
  DiagnosticResult$new(
    name = "vitrandic_subgroup_usda", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = list(passing_layers = r$layers, thickness_cm = r$thickness_cm,
                    undecided_cm = r$undecided_cm),
    missing = if (is.na(r$passed)) L$missing_vitrandic else character(0),
    reference = ref
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
#' @noRd
vertic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  miss <- character(0)
  # Branch 1: cracks >= 5 mm wide through >= 30 cm thickness, WITHIN 125 cm of
  # the surface, plus slickensides / wedge-shaped peds. The "within 125 cm"
  # depth is enforced per layer only when cracks_top_cm is measured; absent =>
  # prior behaviour (v0.9.128).
  cw <- h$cracks_width_cm
  cd <- h$cracks_depth_cm
  ss <- h$slickensides
  ct_col <- h[["cracks_top_cm"]]
  ct <- if (is.null(ct_col)) rep(NA_real_, nrow(h)) else ct_col
  cracks_ok <- any(!is.na(cw) & cw >= 0.5 & !is.na(cd) & cd >= 30 &
                     (is.na(ct) | ct < 125)) &&
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
#' @noRd
aquic_subgroup_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 100)
  res$name <- "aquic_subgroup_usda"
  res
}


# ---- "Folistic" Subgroup helper -------------------------------------

#' Folistic Subgroup helper (folistic_epipedon present)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
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
#' @noRd
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


# ---- Gelic materials (KST 13th ed., p. 44) --------------------------

#' Gelic materials (USDA, KST 13th ed., p. 44)
#'
#' v0.9.418: mineral or organic soil materials with evidence of
#' cryoturbation (frost churning) or ice segregation in the active layer or
#' the upper part of the permafrost. Read as a layer within
#' \code{max_top_cm} of the soil surface with cryoturbation
#' (\code{cryoturbation_usda()}: the designation jj or @, or an irregular or
#' broken boundary) or visible segregated ice (\code{ice_pct} above 0).
#' Field observations: not recorded, absent. With permafrost within 200 cm
#' it makes a Gelisol (p. 67, item A.2).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
gelic_materials_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cr <- cryoturbation_usda(pedon)
  ice <- suppressWarnings(as.numeric(h$ice_pct %||% rep(NA, nrow(h))))
  k <- union(cr$layers, which(!is.na(ice) & ice > 0))
  k <- k[!is.na(h$top_cm[k]) & h$top_cm[k] <= max_top_cm]
  DiagnosticResult$new(
    name = "gelic_materials_usda", passed = length(k) > 0L, layers = sort(k),
    evidence = list(cryoturbation = cr$layers, max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 44")
}


# ---- "Spodic" Subgroup helper (Psamments, Inceptisols, Gelisols) ----

#' Spodic subgroups (KST 13th ed., pp. 175, 184, 195, 197, 222, 226, 234)
#'
#' A horizon 5 cm or more thick that has one or more of: (1) in 25 percent or
#' more of each pedon, pedogenic cementation (extremely weakly coherent or
#' more) by organic matter and aluminum, with or without iron; (2) Al + 1/2
#' Fe (oxalate) of 0.25 percent or more and half that or less in an
#' overlying horizon; (3) an ODOE of 0.12 or more and half that or less in an
#' overlying horizon. In the Psamments and Psammaquents (pp. 175, 184;
#' \code{psamments = TRUE}) the horizon is also below any Ap horizon and 18
#' cm or more below the mineral soil surface.
#'
#' v0.9.418: all three items (up to v0.9.417 only item 2, against the layer
#' directly above, with no thickness or depth). Item 1 reads a recorded
#' cementation (any class, reading L6) in a layer with the suffix h or s
#' (illuvial humus or sesquioxides), in 25 percent or more of the pedon when
#' its share is recorded; items 2 and 3 compare with any overlying mineral
#' layer. An oxalate extract or ODOE not recorded leaves the item undecided
#' (reading L5).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param psamments \code{TRUE} for the depth rule of the Psamments.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
spodic_subgroup_usda <- function(pedon, psamments = FALSE) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 184 (Spodic Udipsamments) and p. 234 (Spodic Dystrudepts)"
  h <- pedon$horizons
  n <- nrow(h)
  if (n == 0L) {
    return(DiagnosticResult$new(name = "spodic_subgroup_usda", passed = FALSE,
                                layers = integer(0), evidence = list(),
                                missing = character(0), reference = ref))
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  d <- .desg_master(h$designation, site = pedon$site)
  d0 <- ifelse(is.na(d), "", d)
  suffix <- sub("^[A-Z]+", "", d0)
  ord <- order(h$top_cm)
  pos <- integer(n); pos[ord] <- seq_len(n)
  mineral <- !(org %in% TRUE)
  share <- .usda_cemented_share(pedon)
  cem <- .usda_cemented(h) & grepl("h|s", suffix) & (is.na(share) | share >= 25)
  alfe <- num(h$al_ox_pct) + 0.5 * num(h$fe_ox_pct)
  odoe <- num(h$odoe %||% rep(NA, n))
  halved <- function(val, i, min_val) {
    above <- ord[seq_len(pos[i] - 1L)]
    above <- above[mineral[above]]
    if (!length(above)) return(FALSE)
    if (is.na(val[i])) return(NA)
    if (val[i] < min_val) return(FALSE)
    .any3u(val[above] <= val[i] / 2)
  }
  ox <- vapply(seq_len(n), function(i) halved(alfe, i, 0.25), NA)
  od <- vapply(seq_len(n), function(i) halved(odoe, i, 0.12), NA)
  ok <- cem | ox | od
  thk <- num(h$bottom_cm) - num(h$top_cm)
  elig <- mineral & !is.na(thk) & thk >= 5
  if (isTRUE(psamments)) {
    ap <- grepl("^Ap", d0)
    min_top <- max(c(ms + 18, h$bottom_cm[ap]), na.rm = TRUE)
    elig <- elig & h$top_cm >= min_top - 1e-6
  }
  ok[!elig] <- FALSE
  passed <- .any3u(ok)
  DiagnosticResult$new(
    name = "spodic_subgroup_usda", passed = passed, layers = which(ok %in% TRUE),
    evidence = list(cemented = cem, oxalate = ox, odoe = od, psamments = psamments),
    missing = if (is.na(passed)) unique(c(if (any(is.na(ox) & elig)) c("al_ox_pct", "fe_ox_pct"),
                                          if (any(is.na(od) & elig)) "odoe")) else character(0),
    reference = ref)
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
ruptic_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "ruptic_subgroup_usda", passed = FALSE, layers = integer(0),
    evidence = list(reason = "Requires multi-pedon data; deferred to v0.9"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 9"
  )
}


# ---- "Psammentic" Subgroup helper -----------------------------------

#' Psammentic Subgroup helper
#'
#' Less than 35 \% rock fragments and a texture class of loamy fine sand or
#' coarser in all layers of the particle-size control section (KST 13th ed.,
#' p. 165). v0.9.419: three-valued, on the layers of the particle-size
#' control section; up to v0.9.418 \code{all(..., na.rm = TRUE)} made it pass
#' when no layer had a texture recorded (audit F12).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
psammentic_subgroup_usda <- function(pedon) {
  r <- .usda_psamment_texture(pedon)
  DiagnosticResult$new(
    name = "psammentic_subgroup_usda", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = list(lfs_or_coarser = r$sandy, rock_fragments = r$rock_fragments),
    missing = if (is.na(r$passed)) c("clay_pct", "sand_pct", "texture_class_usda") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 8, p. 165 (particle-size control section, loamy fine sand or coarser)"
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
#' @noRd
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
#' @noRd
natric_horizon_usda <- function(pedon) {
  res <- .natric_horizon_impl(pedon)   # v0.9.310: the reading before the WRB 2022 criteria 3-4
  res$name <- "natric_horizon_usda"
  res
}


# ---- Salic / Gypsic / Calcic helpers (delegating WRB) ---------------

#' Salic horizon (USDA, delegates to WRB salic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
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

#' Gypsic horizon (USDA Soil Taxonomy, KST 13th ed., p. 25)
#'
#' A horizon 15 cm or more thick, not cemented by gypsum (or cemented in
#' parts less than 5 mm thick), with 5 \% or more gypsum and 1 \% or more
#' (by volume) visible secondary gypsum, and a product of thickness (cm)
#' and gypsum (\%) of 150 or more. v0.9.419: up to v0.9.418 the WRB gypsic
#' horizon. Visible secondary gypsum is a field observation:
#' \code{gypsum_visible_pct}, or a y (gypsum accumulation) in the
#' designation; not recorded, absent, so the horizon is then not present
#' whatever the laboratory gypsum. Gypsum not measured where visible gypsum
#' is recorded leaves it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Upper boundary within this depth (cm).
#' @noRd
gypsic_horizon_usda <- function(pedon, max_top_cm = 100) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, gypsic horizon (p. 25)"
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(DiagnosticResult$new(name = "gypsic_horizon_usda", passed = FALSE, reference = ref))
  d <- .usda_desig(h$designation)
  gv <- .usda_col(h, "gypsum_visible_pct", NA_real_)
  vis <- ifelse(!is.na(gv), gv >= 1, !is.na(d$suffix) & grepl("y", d$suffix))
  gy <- .usda_col(h, "caso4_pct", NA_real_)
  cem <- tolower(as.character(.usda_col(h, "cementation_class", NA_character_)))
  cemented <- (!is.na(cem) & cem %in% c("weakly", "moderately", "strongly", "indurated")) |
              (!is.na(d$suffix) & grepl("y", d$suffix) & grepl("m", d$suffix))
  lay <- ifelse(!vis | cemented, FALSE, ifelse(is.na(gy), NA, gy >= 5))
  ord <- order(h$top_cm)
  prod_ok <- function(r) sum(pmax(h$bottom_cm[r] - h$top_cm[r], 0) * gy[r], na.rm = TRUE) >= 150
  runs_y <- Filter(function(r) .usda_run_thickness(h, r) >= 15 && prod_ok(r) &&
                     min(h$top_cm[r]) < max_top_cm, .usda_runs(lay %in% TRUE, ord))
  runs_m <- Filter(function(r) .usda_run_thickness(h, r) >= 15 && (anyNA(gy[r]) || prod_ok(r)) &&
                     min(h$top_cm[r]) < max_top_cm,
                   .usda_runs(!(lay %in% FALSE), ord))
  passed <- if (length(runs_y)) TRUE else if (length(runs_m)) NA else FALSE
  DiagnosticResult$new(
    name = "gypsic_horizon_usda", passed = passed,
    layers = if (isTRUE(passed)) sort(unique(unlist(runs_y)))
             else if (is.na(passed)) sort(unique(unlist(runs_m))) else integer(0),
    evidence = list(visible_gypsum = vis, gypsum_5 = lay, max_top_cm = max_top_cm),
    missing = if (is.na(passed)) "caso4_pct" else character(0),
    reference = ref)
}

#' Calcic horizon (USDA, delegates to WRB calcic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
calcic_horizon_usda <- function(pedon, max_top_cm = 100) {
  res <- .calcic_impl(pedon)   # v0.9.314: the reading before the WRB 2022 text (USDA)
  res$name <- "calcic_horizon_usda"
  if (isTRUE(res$passed) && length(res$layers) > 0L) {
    h <- pedon$horizons
    res$layers <- res$layers[!is.na(h$top_cm[res$layers]) &
                                  h$top_cm[res$layers] < max_top_cm]
    res$passed <- length(res$layers) > 0L
  }
  res
}
