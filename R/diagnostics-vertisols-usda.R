# =============================================================
# USDA Soil Taxonomy 13ed -- Vertisols helpers (Cap 16, pp 343-354)
# =============================================================
#
# Vertisols are clay-rich soils with shrink-swell features (cracks,
# slickensides, gilgai). 6 Suborders: the crack patterns of the KST are read
# from the recorded regimes (p. 343 note; L32).
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 16.
# =============================================================


# Clay of the fine earth of a layer as a range: measured, or the limits of
# the field texture class (USDA texture triangle) where clay is not
# measured, or 0 to 100.
.VE_CLAY_RANGE <- list(S = c(0, 10), COS = c(0, 10), MS = c(0, 10), FS = c(0, 10),
  VFS = c(0, 10), US = c(0, 10), LS = c(0, 15), LCOS = c(0, 15), LFS = c(0, 15),
  LVFS = c(0, 15), SL = c(0, 20), COSL = c(0, 20), FSL = c(0, 20), VFSL = c(0, 20),
  L = c(7, 27), SIL = c(0, 27), SI = c(0, 12), SCL = c(20, 35), CL = c(27, 40),
  SICL = c(27, 40), SC = c(35, 55), SIC = c(40, 60), C = c(40, 100))
.ve_clay_range <- function(h) {
  clay <- .mo_col(h, "clay_pct")
  tc <- h[["texture_class_usda"]]
  k <- if (is.null(tc)) rep(NA_character_, nrow(h)) else .texture_class_norm(tc)
  lapply(seq_len(nrow(h)), function(i) {
    if (!is.na(clay[i])) return(c(clay[i], clay[i]))
    if (!is.na(k[i]) && !is.null(.VE_CLAY_RANGE[[k[i]]])) return(.VE_CLAY_RANGE[[k[i]]])
    c(0, 100)
  })
}

# Slickensides recorded in a layer (a field observation: not recorded is
# absent).
.ve_slickensides <- function(h) {
  sl <- .field_term(.mo_col(h, "slickensides"))
  !is.na(sl) & !sl %in% c("absent", "none", "no", "0", "nil")
}

# Thickness of the thickest run of depth-adjoining layers among `sel` within
# [from, to].
.ve_run <- function(h, sel, from, to) {
  best <- 0; cur <- 0; last <- NA_real_
  for (i in order(h$top_cm)) {
    if (!isTRUE(sel[i])) { cur <- 0; last <- NA_real_; next }
    t <- max(h$top_cm[i], from); b <- min(h$bottom_cm[i], to)
    if (b <= t) next
    cur <- if (!is.na(last) && t <= last + 0.01) cur + (b - t) else b - t
    last <- b; best <- max(best, cur)
  }
  best
}

#' Vertisol order qualifier (KST 13th ed., p. 69, item F)
#'
#' The three items of the order key, three-valued (v0.9.421):
#' \enumerate{
#'   \item A layer 25 cm or more thick (adjoining horizons), within 100 cm of
#'         the mineral soil surface, with slickensides or wedge-shaped peds.
#'         Slickensides not recorded are absent; a structure not recorded is
#'         undecided.
#'   \item A weighted average of 30 \% or more clay between the mineral soil
#'         surface and 18 cm or in an Ap horizon, whichever is thicker, and
#'         30 \% or more clay in every horizon between 18 cm and 50 cm (or a
#'         densic, lithic or paralithic contact, a duripan or a petrocalcic
#'         horizon if shallower): a horizon that reaches into that depth
#'         range at all is one of them (L31). The field texture class bounds
#'         the clay where it is not measured.
#'   \item Cracks that open and close periodically: cracks 0.5 cm or more
#'         wide recorded within 100 cm; where no crack is recorded, the
#'         slickensides of item 1 are taken as evidence of them, and
#'         \code{notes} says so (L30). Cracks not recorded are absent.
#' }
#' The linear extensibility path of the earlier reading is not part of the
#' order key (it defines the Vertic subgroups).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vertisol_qualifying_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 69, item F"
  H <- pedon$horizons
  ok <- which(!is.na(H$top_cm) & !is.na(H$bottom_cm) & H$bottom_cm > H$top_cm)
  ok <- ok[order(H$top_cm[ok])]
  h <- H[ok, , drop = FALSE]
  if (!nrow(h)) return(DiagnosticResult$new(name = "vertisol_qualifying_usda", passed = FALSE,
    evidence = list(reason = "no horizons"), reference = ref))
  org <- .mo_is_organic(h)
  contact <- .mo_is_contact(h)
  ms <- if (any(!org)) h$top_cm[which(!org)[1]] else h$top_cm[1]
  # 1. slickensides or wedge-shaped peds
  slick <- .ve_slickensides(h)
  st <- .field_term(.mo_col(h, "structure_type"))
  st2 <- .field_term(.mo_col(h, "structure_type_secondary"))
  wedge <- ifelse(!is.na(st2) & grepl("wedge|cuneiform", st2), TRUE,
                  ifelse(is.na(st), NA, grepl("wedge|cuneiform", st)))
  feat <- ifelse(slick, TRUE, wedge)
  feat[org | contact] <- FALSE
  run_yes <- .ve_run(h, feat %in% TRUE, ms, ms + 100)
  run_may <- .ve_run(h, !feat %in% FALSE, ms, ms + 100)
  c1 <- if (run_yes >= 25) TRUE else if (run_may < 25) FALSE else NA
  # 2. clay
  cr <- .ve_clay_range(h)
  dsg <- as.character(.mo_col(h, "designation"))
  ap <- which(!is.na(dsg) & grepl("^Ap", dsg) & !org)
  sb <- max(ms + 18, if (length(ap)) max(h$bottom_cm[ap]) else -Inf)
  s1 <- which(!org & !contact & h$bottom_cm > ms & h$top_cm < sb)
  w <- pmin(h$bottom_cm[s1], sb) - pmax(h$top_cm[s1], ms)
  lo <- sum(vapply(cr[s1], `[`, 0, 1) * w) / sum(w)
  hi <- sum(vapply(cr[s1], `[`, 0, 2) * w) / sum(w)
  c2a <- if (!length(s1)) FALSE else .mo_ge(c(lo, hi), 30)
  ct <- .mo_contact_top(h)
  lb <- min(ms + 50, if (is.na(ct)) Inf else ct)
  s2 <- which(!org & !contact & h$bottom_cm > ms + 18 & h$top_cm < lb)
  c2b <- .mo_and(vapply(s2, function(i) .mo_ge(cr[[i]], 30), NA))
  c2 <- .mo_and(c2a, c2b)
  # 3. cracks
  in100 <- which(!org & h$top_cm < ms + 100)
  cw <- .mo_col(h, "cracks_width_cm")[in100]
  crack_note <- NA_character_
  c3 <- if (any(!is.na(cw) & cw >= 0.5)) TRUE
        else if (any(!is.na(cw))) FALSE
        else if (any(slick[in100])) {
          crack_note <- "cracks not recorded: the slickensides recorded are taken as evidence of cracks that open and close (L30)"
          TRUE
        } else FALSE
  passed <- .mo_and(c1, c2, c3)
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(c1)) miss <- c(miss, "structure_type")
    if (is.na(c2)) miss <- c(miss, "clay_pct")
  }
  DiagnosticResult$new(
    name = "vertisol_qualifying_usda", passed = passed,
    layers = ok[which(feat %in% TRUE & h$top_cm < ms + 100)],
    evidence = list(item1_slickensides_or_wedges = c1, run_cm = run_yes,
                    run_possible_cm = run_may, item2_clay = c2, clay_0_18 = c2a,
                    clay_18_50 = c2b, clay_surface_depth_cm = sb - ms,
                    item3_cracks = c3, crack_evidence = crack_note),
    missing = miss,
    notes = if (isTRUE(passed)) crack_note else NA_character_,
    reference = ref)
}


# ---- Aquerts (KST 13th ed., p. 343, item FA) -------------------------------

#' Aquerts qualifier (KST 13th ed., p. 343, item FA)
#'
#' In one or more horizons within 50 cm of the mineral soil surface, aquic
#' conditions (redox features, a g designation or a reduced matrix recorded)
#' and a matrix chroma of 2 or less with redox concentrations, or of 1 or
#' less. The alpha,alpha-dipyridyl reaction is not recorded in soilKey.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquert_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  cand <- which(!is.na(h$top_cm) & h$top_cm < ms + 50 & !.mo_is_organic(h))
  dsg <- as.character(.mo_col(h, "designation"))
  red <- .mo_col(h, "reduced_matrix_pct")
  lay <- vapply(cand, function(i) {
    rc <- .mo_redox_conc(h, i)
    aquic <- rc || (!is.na(dsg[i]) && grepl("g", dsg[i])) || (!is.na(red[i]) && red[i] > 0)
    if (!aquic) return(FALSE)
    cm <- h$munsell_chroma_moist[i]
    if (is.na(cm)) return(NA)
    cm <= 1 || (cm <= 2 && rc)
  }, NA)
  passed <- .mo_or(lay)
  DiagnosticResult$new(
    name = "aquert_qualifying_usda", passed = passed,
    layers = cand[lay %in% TRUE],
    evidence = list(layers_tested = cand, layer_result = lay),
    missing = if (is.na(passed)) "munsell_chroma_moist" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 343, item FA")
}


# ---- Xererts, Cryerts (KST 13th ed., p. 343; L32) --------------------------

#' Xererts qualifier (KST 13th ed., p. 343, item FC)
#'
#' A thermic, mesic or frigid soil temperature regime and the xeric crack
#' pattern; the recorded xeric moisture regime stands in for the crack
#' pattern (KST note to the key, p. 343; L32). Undecided where a regime is
#' not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
xerert_qualifying_usda <- function(pedon) {
  t <- soil_temperature_regime_usda(pedon, c("thermic", "mesic", "frigid"))
  m <- smr_xeric_usda(pedon)
  passed <- .mo_and(t$passed, m$passed)
  DiagnosticResult$new(
    name = "xerert_qualifying_usda", passed = passed, layers = integer(0),
    evidence = list(temperature = t$passed, xeric = m$passed),
    missing = if (is.na(passed)) unique(c(t$missing, m$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 343, item FC")
}

#' Humicryerts (KST 13th ed., p. 347): 10 kg/m2 or more organic carbon
#' between the mineral soil surface and 50 cm (organic carbon x bulk density
#' x thickness x fine earth).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humicryert_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  L <- which(!is.na(h$top_cm) & h$bottom_cm > ms & h$top_cm < ms + 50 & !.mo_is_organic(h))
  thk <- pmin(h$bottom_cm[L], ms + 50) - pmax(h$top_cm[L], ms)
  oc <- h$oc_pct[L]; bd <- .mo_col(h, "bulk_density_g_cm3")[L]
  cf <- .mo_col(h, "coarse_fragments_pct")[L]; cf[is.na(cf)] <- 0
  f <- thk * (1 - cf / 100) / 10
  lo <- sum(ifelse(is.na(oc), 0, oc) * ifelse(is.na(bd), 0.3, bd) * f)
  hi <- sum(ifelse(is.na(oc), 100, oc) * ifelse(is.na(bd), 2.0, bd) * f)
  passed <- if (lo >= 10) TRUE else if (hi < 10) FALSE else NA
  DiagnosticResult$new(
    name = "humicryert_usda", passed = passed, layers = L,
    evidence = list(oc_kg_m2 = c(lo, hi)),
    missing = if (is.na(passed)) c("oc_pct", "bulk_density_g_cm3") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 347")
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
  res <- .natric_horizon_impl(pedon)   # v0.9.310: the reading before the WRB 2022 criteria 3-4
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


# ---- Dystr- Vertisols (KST 13th ed., pp. 344, 349, 350) --------------------

#' Dystraquerts, Dystruderts and Dystrusterts (KST 13th ed., pp. 344, 349,
#' 350)
#'
#' Throughout one or more horizons with a total thickness of 25 cm or more
#' within 50 cm of the mineral soil surface, both an electrical conductivity
#' (saturation extract) below 4.0 dS/m and a pH of 4.5 or less in 0.01 M
#' CaCl2 (5.0 or less in water, read where the CaCl2 pH is not measured).
#' Not the base saturation: the KST does not
#' use it here. Three-valued: undecided where the EC or the pH that could
#' decide it is not measured.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
dystric_vertisol_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .mineral_soil_surface_cm(h); if (is.na(ms)) ms <- 0
  L <- which(!is.na(h$top_cm) & h$bottom_cm > ms & h$top_cm < ms + 50 & !.mo_is_organic(h))
  ec <- .mo_col(h, "ec_dS_m")[L]; pc <- .mo_col(h, "ph_cacl2")[L]
  pw <- .mo_col(h, "ph_h2o_1to1")[L]; pw0 <- .mo_col(h, "ph_h2o")[L]
  pw[is.na(pw)] <- pw0[is.na(pw)]
  ok <- vapply(seq_along(L), function(k)
    .mo_and(if (is.na(ec[k])) NA else ec[k] < 4,
            if (!is.na(pc[k])) pc[k] <= 4.5 else if (!is.na(pw[k])) pw[k] <= 5.0 else NA), NA)
  thk <- pmin(h$bottom_cm[L], ms + 50) - pmax(h$top_cm[L], ms)
  passed <- if (sum(thk[ok %in% TRUE]) >= 25) TRUE
            else if (sum(thk[!ok %in% FALSE]) < 25) FALSE else NA
  DiagnosticResult$new(
    name = "dystric_vertisol_usda", passed = passed, layers = L[ok %in% TRUE],
    evidence = list(thickness_cm = sum(thk[ok %in% TRUE]),
                    thickness_possible_cm = sum(thk[!ok %in% FALSE])),
    missing = if (is.na(passed)) unique(c(if (anyNA(ec)) "ec_dS_m",
                                          if (anyNA(pc) && anyNA(pw)) "ph_cacl2")) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., pp. 344, 349, 350")
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
