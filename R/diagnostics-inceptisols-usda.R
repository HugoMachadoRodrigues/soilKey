# =============================================================
# USDA Soil Taxonomy 13ed -- Inceptisols helpers (Cap 11, pp 207-246)
# =============================================================
#
# Inceptisols are weakly developed soils with a cambic horizon (or
# cambic + a few other mild diagnostic features). 6 Suborders.
# =============================================================


#' Inceptisol Order qualifier
#' Pass when a cambic horizon is present (no argillic, no spodic,
#' no mollic, etc. -- enforced by prior order exclusion).
#' @param pedon A \code{\link{PedonRecord}}.
#' @keywords internal
#' @export
inceptisol_qualifying_usda <- function(pedon) {
  cb <- cambic(pedon)
  res <- DiagnosticResult$new(
    name = "inceptisol_qualifying_usda", passed = isTRUE(cb$passed),
    layers = cb$layers,
    evidence = list(cambic = cb),
    missing = cb$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11, p 207"
  )
  res
}


#' Aquept Suborder qualifier
#' @param pedon A \code{\link{PedonRecord}}.
#' @keywords internal
#' @export
aquept_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquept_qualifying_usda"
  res
}


#' Halic helper for Halaquepts
#' Pass when EC >= 8 dS/m within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @keywords internal
#' @export
halaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  ec <- h$ec_dS_m[cand]
  miss <- if (all(is.na(ec))) "ec_dS_m" else character(0)
  passing <- cand[!is.na(ec) & ec >= 8]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "halaquept_qualifying_usda", passed = passed, layers = passing,
    evidence = list(threshold_dS_m = 8),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Densiaquept qualifying (densic contact within 100 cm)
#' @param pedon A \code{\link{PedonRecord}}.
#' @keywords internal
#' @export
densiaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100 &
                  !is.na(h$rupture_resistance) &
                  tolower(h$rupture_resistance) %in%
                    c("very firm", "extremely firm"))
  passed <- length(cand) > 0L
  DiagnosticResult$new(
    name = "densiaquept_qualifying_usda", passed = passed, layers = cand,
    evidence = list(note = "v0.8 proxy: very firm/extremely firm rupture-resistance"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 39"
  )
}


#' Eutric Inceptisol Suborder helper (Eutrudepts)
#' Pass when BS (NH4OAc) >= 60\% in some part of upper 75 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @keywords internal
#' @export
eutric_inceptisol_usda <- function(pedon, min_bs = 60) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 75)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  passed <- any(!is.na(bs) & bs >= min_bs)
  DiagnosticResult$new(
    name = "eutric_inceptisol_usda", passed = passed,
    layers = cand[!is.na(bs) & bs >= min_bs],
    evidence = list(threshold = min_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Humic Inceptisol \emph{great-group} helper (Hum*)
#'
#' Keys the \emph{Hum-} great groups of the Inceptisols (Humudepts
#' KFE, Humustepts KEC, Humixerepts KDC, Humicryepts KCA, Humigelepts
#' KBA, Humaquepts KAI): "Other [suborder] that have an umbric or
#' mollic epipedon" (Humaquepts also admits histic / melanic). This is
#' the correct, epipedon-based differentia at the great-group level
#' and must not be confused with the \emph{Humic subgroup} modifier of
#' the udept / ustept / xerept great groups, which is a pure
#' dark-colour-VALUE test handled by \code{\link{dark_color_value_usda}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @keywords internal
#' @export
humic_inceptisol_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  passed <- isTRUE(mo$passed) || isTRUE(um$passed)
  DiagnosticResult$new(
    name = "humic_inceptisol_usda", passed = passed,
    layers = unique(c(mo$layers, um$layers)),
    evidence = list(mollic = mo, umbric = um),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Dark colour value (USDA "Humic" subgroup colour differentia)
#'
#' Tests the colour-value criterion that defines the \emph{Humic}
#' subgroups of the udept / ustept / xerept great groups in KST 13ed,
#' e.g. Humic Dystrudepts (KFGY), Humic Eutrudepts (KFFV), Humic
#' Dystrustepts (KDDK), Humic Dystroxerepts (KEEN) and Humic
#' Haploxerepts (KEFP): "a colour value, moist, of 3 or less and a
#' colour value, dry, of 5 or less (crushed and smoothed sample)
#' either throughout the upper 18 cm of the mineral soil (unmixed) or
#' between the mineral soil surface and a depth of 18 cm after mixing."
#'
#' This is a pure dark-colour-VALUE screen and is \emph{not} an
#' epipedon test. Unlike \code{\link{humic_inceptisol_usda}} (which
#' keys the \emph{Hum-} great groups via a mollic or umbric epipedon)
#' it does not test chroma, base saturation, organic carbon, thickness
#' or structure. Re-using \code{humic_inceptisol_usda} for these
#' subgroups under-matches dark colour-only soils and could mis-match
#' epipedon soils that lack the dark colour.
#'
#' The aquept "Humic" subgroups behave differently and are \emph{not}
#' represented by this helper: Humic Fragiaquepts (KADB) / Densiaquepts
#' (KAEB) are histic-mollic-umbric epipedon-defined and Humic
#' Gelaquepts (KAFF) / Cryaquepts (KAGL) are mollic-umbric
#' epipedon-defined (all keep \code{humic_inceptisol_usda}); Humic
#' Endoaquepts (KAKJ) / Epiaquepts (KAJG) combine a 15 cm colour
#' window with a base-saturation < 50 percent clause.
#'
#' Implementation notes: this is a value-ONLY test, so it deliberately
#' does not go through \code{\link{test_mollic_color}} (whose moist
#' path is gated on chroma being present and would otherwise ignore a
#' recorded moist value when chroma is missing). It reuses the same
#' moist/dry correspondence convention (value moist approx value dry
#' minus 1) and the OC-darkness fallback used elsewhere. The "unmixed,
#' throughout" path is implemented (every colour-evaluable mineral
#' layer in the window must qualify); the "after mixing" depth-weighted
#' path is a deferred refinement.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_value_moist Maximum moist colour value (default 3).
#' @param max_value_dry Maximum dry colour value (default 5).
#' @param depth_cm Depth of the mineral-soil colour window in cm
#'        (default 18; the Endoaquepts / Epiaquepts variant uses 15).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 11.
#' @keywords internal
#' @export
dark_color_value_usda <- function(pedon, max_value_moist = 3,
                                     max_value_dry = 5, depth_cm = 18) {
  h <- pedon$horizons
  empty <- function(reason, missing = character(0)) {
    DiagnosticResult$new(
      name = "dark_color_value_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = reason, depth_cm = depth_cm),
      missing = missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
    )
  }
  if (nrow(h) == 0L) return(empty("empty horizons"))

  ms_top <- .mineral_soil_surface_cm(h)
  if (is.na(ms_top)) ms_top <- 0
  upper <- ms_top + depth_cm
  # Mineral layers intersecting the [mineral surface, mineral surface +
  # depth_cm] colour window. Only explicit organic (O/H) layers are
  # excluded; an unlabelled layer is treated as mineral.
  cand <- which((is.na(h$designation) | !grepl("^[OH]", h$designation)) &
                  !is.na(h$top_cm) & h$top_cm < upper &
                  (is.na(h$bottom_cm) | h$bottom_cm > ms_top))
  if (length(cand) == 0L) {
    return(empty("no mineral layer in colour window", "designation"))
  }

  # Per-layer value-only evaluation. A layer is "evaluable" when it has
  # a moist OR dry Munsell value, or (fallback) measured OC; otherwise
  # it is unevaluated and contributes to `missing`.
  evaluated <- integer(0); passing <- integer(0); miss <- character(0)
  for (i in cand) {
    vm <- h$munsell_value_moist[i]
    vd <- h$munsell_value_dry[i]
    oc <- if ("oc_pct" %in% names(h)) h$oc_pct[i] else NA_real_
    if (!is.na(vm) || !is.na(vd)) {
      # Estimate the missing member via value moist ~ value dry - 1.
      vm_eff <- if (!is.na(vm)) vm else vd - 1
      vd_eff <- if (!is.na(vd)) vd else vm + 1
      ok <- (vm_eff <= max_value_moist) && (vd_eff <= max_value_dry)
      evaluated <- c(evaluated, i)
      if (ok) passing <- c(passing, i)
    } else if (!is.na(oc) && oc >= 1.5) {
      # No Munsell -> infer dark colour from high OC (same convention
      # as test_mollic_color path 3).
      evaluated <- c(evaluated, i)
      passing   <- c(passing, i)
    } else {
      miss <- c(miss, "munsell_value_moist", "munsell_value_dry")
    }
  }

  # "throughout the upper depth_cm" -> every evaluable layer in the
  # window must qualify (>= 1 evaluable). If none are evaluable the
  # result is NA (missing data).
  if (length(evaluated) == 0L) {
    return(DiagnosticResult$new(
      name = "dark_color_value_usda", passed = NA, layers = integer(0),
      evidence = list(candidate_layers = cand, depth_cm = depth_cm,
                        reason = "no colour or OC evidence in window"),
      missing = unique(miss),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
    ))
  }
  passed <- setequal(evaluated, passing)
  DiagnosticResult$new(
    name = "dark_color_value_usda", passed = passed,
    layers = if (isTRUE(passed)) passing else integer(0),
    evidence = list(candidate_layers = cand, evaluated = evaluated,
                      passing = passing, depth_cm = depth_cm,
                      max_value_moist = max_value_moist,
                      max_value_dry = max_value_dry),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}
