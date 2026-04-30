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
#' @export
aquept_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquept_qualifying_usda"
  res
}


#' Halic helper for Halaquepts
#' Pass when EC >= 8 dS/m within 100 cm.
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
#' Pass when BS (NH4OAc) >= 60% in some part of upper 75 cm.
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


#' Humic Inceptisol Suborder helper (Hum*)
#' Pass when umbric or mollic epipedon present + thick (>= 25 cm).
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
