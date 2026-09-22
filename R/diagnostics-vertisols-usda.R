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
#' @noRd
vertisol_qualifying_usda <- function(pedon) {
  res <- vertic_horizon(pedon)
  res$name <- "vertisol_qualifying_usda"
  res
}


# ---- Aquerts Suborder qualifier ------------------------------------

#' Aquerts qualifier (Vertisols with aquic conditions)
#' Pass when aquic_conditions within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
salic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- salic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "salic_subgroup_usda"
  res
}


# ---- Natric horizon (delegating) -----------------------------------

#' Natric Subgroup helper for Natraquerts.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
calcic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- calcic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "calcic_subgroup_usda"
  res
}


# ---- Gypsic Subgroup helper ----------------------------------------

#' Gypsic Subgroup helper -- delegates to gypsic_horizon_usda.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
gypsic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- gypsic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "gypsic_subgroup_usda"
  res
}


# ---- Dystric Subgroup helper (low BS) -------------------------------

#' Dystric Subgroup helper (Vertisols Dystr*)
#' Pass when BS (NH4OAc) < 50\% in some part of the upper 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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


# ---- Chromic Subgroup helper (Vertisols) ---------------------------

#' Chromic Subgroup helper (Vertisols, KST 13ed Ch 16)
#'
#' The Vertisol \emph{Chromic} subgroups are the \dQuote{not dark} ones: per
#' KST 13ed, those that have, in one or more horizons within
#' \code{max_top_cm} of the mineral soil surface, 50 percent or more of the
#' colours with a moist value of 4 or more, a dry value of 6 or more, or
#' (when \code{use_chroma}) a moist chroma of 3 or more. This is a
#' value/chroma test, \strong{not} the red-hue \code{chromic} qualifier of
#' WRB. The Aquerts great groups (Dur-/Dystr-/Endo-/Epiaquerts) drop the
#' chroma clause -- gleyed material is low-chroma by definition -- so they
#' are wired with \code{use_chroma = FALSE}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param use_chroma Logical; include the moist-chroma >= 3 clause
#'        (default \code{TRUE}). Set \code{FALSE} for Aquerts.
#' @param max_top_cm Upper-depth window in cm (default 30).
#' @noRd
chromic_subgroup_usda <- function(pedon, use_chroma = TRUE, max_top_cm = 30) {
  h    <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  vm <- h$munsell_value_moist[cand]
  vd <- h$munsell_value_dry[cand]
  cm <- h$munsell_chroma_moist[cand]
  light <- (!is.na(vm) & vm >= 4) | (!is.na(vd) & vd >= 6)
  if (isTRUE(use_chroma)) light <- light | (!is.na(cm) & cm >= 3)
  passing <- cand[light]
  passed  <- length(passing) > 0L &&
               sum(light, na.rm = TRUE) >= 0.5 * length(cand)
  DiagnosticResult$new(
    name = "chromic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(n_light = length(passing), n_total = length(cand),
                    use_chroma = use_chroma),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 16"
  )
}


# ---- Leptic Subgroup helper (Vertisols) ----------------------------

#' Leptic Subgroup helper (Vertisols, KST 13ed Ch 16)
#'
#' The Vertisol \emph{Leptic} subgroups have a densic, lithic, or paralithic
#' contact, a duripan, or a petrocalcic horizon within \code{max_top_cm} of
#' the surface. Detection is deliberately conservative (under-fire before
#' over-fire): a root-restricting designation (\code{R}/\code{Cr}/\code{Cd}),
#' a duripan (\code{duripan_pct > 0} or strongly/indurated cementation), or a
#' petrocalcic horizon (strongly/indurated cementation with CaCO3 >= 15\%).
#' This is the USDA \dQuote{shallow contact} sense of \emph{leptic}, distinct
#' from both the WRB coarse-fragment \code{leptic} and the gypsic/soluble-salt
#' \emph{Leptic} of certain Gypsids and Natr- great groups.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Depth window in cm (default 100).
#' @noRd
leptic_vertic_usda <- function(pedon, max_top_cm = 100) {
  h    <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
  des  <- h$designation[cand]
  contact <- !is.na(des) &
    grepl("^R(?![/a-z])|^2R|^3R|^Cr|^2Cr|^3Cr|^Cd|^2Cd", des, perl = TRUE)
  dp  <- h$duripan_pct[cand]
  dur <- !is.na(dp) & dp > 0
  cem <- h$cementation_class[cand]
  cas <- h$caco3_pct[cand]
  petro <- !is.na(cem) & tolower(cem) %in% c("strongly", "indurated") &
             !is.na(cas) & cas >= 15
  hit <- contact | dur | petro
  passing <- cand[hit]
  passed  <- length(passing) > 0L
  DiagnosticResult$new(
    name = "leptic_vertic_usda", passed = passed, layers = passing,
    evidence = list(n_contact = sum(contact), n_duripan = sum(dur),
                    n_petrocalcic = sum(petro)),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 16"
  )
}
