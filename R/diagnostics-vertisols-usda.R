# =============================================================
# USDA Soil Taxonomy 13ed -- Vertisols helpers (Cap 16, pp 343-354)
# =============================================================
#
# Vertisols are clay-rich soils with shrink-swell features (cracks,
# slickensides, gilgai). 6 Suborders by SMR + temperature.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 16.
# =============================================================


#' Vertisol Order qualifier (USDA, KST 13ed, Ch 2 / Ch 3 vertic horizon)
#' Pass when a vertic horizon (clay >= 30, cracks, slickensides, LE)
#' is present. Delegates to WRB \code{vertic_horizon}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
vertisol_qualifying_usda <- function(pedon) {
  res <- vertic_horizon(pedon)
  res$name <- "vertisol_qualifying_usda"
  res
}


# ---- Aquerts Suborder qualifier ------------------------------------

#' Aquerts qualifier (Vertisols with aquic conditions)
#' Pass when aquic_conditions within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aquert_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquert_qualifying_usda"
  res
}


# ---- Salic horizon helper for Salaquerts ---------------------------

#' Salic Subgroup helper
#' Wraps salic_horizon_usda. Used for Salaquerts/Salitorrerts/etc.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
salic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- salic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "salic_subgroup_usda"
  res
}


# ---- Natric horizon (delegating) -----------------------------------

#' Natric Subgroup helper for Natraquerts.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
natric_subgroup_usda <- function(pedon) {
  res <- natric_horizon(pedon)
  res$name <- "natric_subgroup_usda"
  res
}


# ---- Calcic Subgroup helper ----------------------------------------

#' Calcic Subgroup helper -- delegates to calcic_horizon_usda within
#' \code{max_top_cm}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
calcic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- calcic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "calcic_subgroup_usda"
  res
}


# ---- Gypsic Subgroup helper ----------------------------------------

#' Gypsic Subgroup helper -- delegates to gypsic_horizon_usda.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
gypsic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- gypsic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "gypsic_subgroup_usda"
  res
}


# ---- Dystric Subgroup helper (low BS) -------------------------------

#' Dystric Subgroup helper (Vertisols Dystr*)
#' Pass when BS (NH4OAc) < 50\% in some part of the upper 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
dystric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  passing <- cand[!is.na(bs) & bs < 50]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "dystric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(bs_layers = bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 16"
  )
}
