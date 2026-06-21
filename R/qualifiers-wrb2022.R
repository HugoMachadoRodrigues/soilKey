# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5, pp 127-156).
#
# v0.9 implements ~50 core qualifiers covering the most common usage
# across the 32 RSGs. The remaining ~150 sub-qualifiers (Hyper-, Hypo-,
# Proto-, Endo-, Epi-, Bathy-, Thapto-, Supra-, Ano-, Panto-, Poly-,
# Amphi-, Kato- variants and the Novic combinations) are scheduled for
# v0.9.1.
#
# Each qualifier function returns a DiagnosticResult: passed = TRUE if
# the qualifier applies to the pedon at the canonical reference depth
# (typically <= 100 cm from the mineral soil surface, per Ch 5 marker
# (2)). The qualifier system in classify_wrb2022() filters the per-RSG
# applicable list (Ch 4 tables) and formats the result as
#   "<Principal>(s) <RSG> (<Supplementary>(s))"
# per the rules of Ch 6, p 154.
# ============================================================================


# Helper: returns the layer indices that start within max_top_cm of the
# mineral soil surface. Used as a depth gate for almost every
# qualifier.
.in_upper <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
}

# Helper: build a DiagnosticResult for a thin "presence" qualifier.
.q_presence <- function(name, base_diag, max_top_cm = 100, pedon = NULL) {
  passed <- isTRUE(base_diag$passed) &&
              (is.null(pedon) ||
                 length(intersect(base_diag$layers,
                                      .in_upper(pedon, max_top_cm))) > 0L)
  layers <- if (passed) base_diag$layers else integer(0)
  DiagnosticResult$new(
    name = name, passed = passed, layers = layers,
    evidence = list(base = base_diag),
    missing = base_diag$missing %||% character(0),
    reference = "WRB (2022) Ch 5"
  )
}


# ---------- HORIZON-BASED PRINCIPAL QUALIFIERS ------------------------------

#' Albic qualifier (ab): albic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_albic       <- function(pedon) .q_presence("Albic",       albic(pedon),       100, pedon)

#' Andic qualifier (an): andic OR vitric properties combined >= 30 cm.
#' v0.9 simplification: passes if andic_properties or vitric_properties
#' passes within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_andic       <- function(pedon) {
  ap <- andic_properties(pedon); vp <- vitric_properties(pedon)
  passed <- (isTRUE(ap$passed) || isTRUE(vp$passed)) &&
              length(intersect(union(ap$layers, vp$layers),
                                   .in_upper(pedon, 100))) > 0L
  DiagnosticResult$new(
    name = "Andic", passed = passed,
    layers = union(ap$layers, vp$layers),
    evidence = list(andic = ap, vitric = vp),
    missing = unique(c(ap$missing, vp$missing)),
    reference = "WRB (2022) Ch 5, Andic"
  )
}

#' Anthric qualifier (ak): anthric properties.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_anthric     <- function(pedon) .q_presence("Anthric",     anthric_horizons(pedon), 100, pedon)

# ---- v0.9.113: thin presence wrappers over existing diagnostics ------------
# Each wraps a diagnostic already implemented in
# diagnostics-{horizons,materials}-wrb-v033.R; wired per RSG in
# inst/rules/wrb2022/qualifiers.yaml. Verified byte-identical on the 44
# canonical fixtures (the diagnostics that fire do so only on RSGs whose
# Ch 4 applicable list does not list the qualifier).

#' Aeolic qualifier (ae): aeolic (wind-sorted) material <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_aeolic      <- function(pedon) .q_presence("Aeolic",      aeolic_material(pedon), 100, pedon)

#' Fragic qualifier (fg): fragic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_fragic      <- function(pedon) .q_presence("Fragic",      fragic(pedon),      100, pedon)

#' Limonic qualifier (lm): limonic (bog-iron) horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_limonic     <- function(pedon) .q_presence("Limonic",     limonic(pedon),     100, pedon)

#' Tsitelic qualifier (ts): tsitelic (red, low-activity) horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_tsitelic    <- function(pedon) .q_presence("Tsitelic",    tsitelic(pedon),    100, pedon)

#' Calcic qualifier (cc): calcic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_calcic      <- function(pedon) .q_presence("Calcic",      calcic(pedon),      100, pedon)

#' Cambic qualifier (cm): cambic horizon <= 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cambic      <- function(pedon) .q_presence("Cambic",      cambic(pedon),      50,  pedon)

#' Cryic qualifier (cy): cryic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cryic       <- function(pedon) .q_presence("Cryic",       cryic_conditions(pedon), 100, pedon)

#' Duric qualifier (du): duric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_duric       <- function(pedon) .q_presence("Duric",       duric_horizon(pedon),    100, pedon)

#' Ferralic qualifier (fl): ferralic horizon <= 150 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ferralic    <- function(pedon) .q_presence("Ferralic",    ferralic(pedon),    150, pedon)

#' Ferric qualifier (fr): ferric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ferric      <- function(pedon) .q_presence("Ferric",      ferric(pedon),      100, pedon)

#' Fluvic qualifier (fv): fluvic material >= 25 cm thick starting <= 75 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_fluvic      <- function(pedon) .q_presence("Fluvic",      fluvic_material(pedon),  75,  pedon)

#' Folic qualifier (fo): folic horizon at the soil surface. v0.9
#' delegates to histic_horizon with surface-only filter.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_folic       <- function(pedon) {
  h <- histic_horizon(pedon)
  surface <- length(h$layers) > 0L &&
               any(pedon$horizons$top_cm[h$layers] <= 5, na.rm = TRUE)
  passed <- isTRUE(h$passed) && surface
  DiagnosticResult$new(
    name = "Folic", passed = passed,
    layers = if (passed) h$layers else integer(0),
    evidence = list(histic = h),
    missing = h$missing,
    reference = "WRB (2022) Ch 5, Folic"
  )
}

#' Gleyic qualifier (gl): gleyic properties throughout a layer >= 25 cm
#' starting <= 75 cm + reducing conditions.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gleyic      <- function(pedon) .q_presence("Gleyic",      gleyic_properties(pedon), 75, pedon)

#' Gypsic qualifier (gy): gypsic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gypsic      <- function(pedon) .q_presence("Gypsic",      gypsic(pedon),      100, pedon)

#' Histic qualifier (hi): histic horizon at or near the surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_histic      <- function(pedon) .q_presence("Histic",      histic_horizon(pedon),   100, pedon)

#' Leptic qualifier (le): continuous rock <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_leptic      <- function(pedon) .q_presence("Leptic",      continuous_rock(pedon),  100, pedon)

#' Mollic qualifier (mo): mollic horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mollic      <- function(pedon) .q_presence("Mollic",      mollic(pedon),      100, pedon)

#' Natric qualifier (na): natric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_natric      <- function(pedon) .q_presence("Natric",      natric_horizon(pedon),   100, pedon)

#' Nitic qualifier (ni): nitic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_nitic       <- function(pedon) .q_presence("Nitic",       nitic_horizon(pedon),    100, pedon)

#' Petrocalcic qualifier (pc): petrocalcic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petrocalcic <- function(pedon) .q_presence("Petrocalcic", petrocalcic(pedon),      100, pedon)

#' Petroduric qualifier (pd): petroduric horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petroduric  <- function(pedon) .q_presence("Petroduric",  petroduric(pedon),       100, pedon)

#' Petrogypsic qualifier (pg): petrogypsic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petrogypsic <- function(pedon) .q_presence("Petrogypsic", petrogypsic(pedon),      100, pedon)

#' Petroplinthic qualifier (pp): petroplinthic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petroplinthic <- function(pedon) .q_presence("Petroplinthic", petroplinthic(pedon), 100, pedon)

#' Plinthic qualifier (pl): plinthic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_plinthic    <- function(pedon) .q_presence("Plinthic",    plinthic(pedon),    100, pedon)

#' Retic qualifier (rt): retic properties <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_retic       <- function(pedon) .q_presence("Retic",       retic_properties(pedon), 100, pedon)

#' Salic qualifier (sz): salic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_salic       <- function(pedon) .q_presence("Salic",       salic(pedon),       100, pedon)

#' Spodic qualifier (sd): spodic horizon <= 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_spodic      <- function(pedon) .q_presence("Spodic",      spodic(pedon),      200, pedon)

#' Stagnic qualifier (st): stagnic properties <= 75 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_stagnic     <- function(pedon) .q_presence("Stagnic",     stagnic_properties(pedon), 75, pedon)

#' Umbric qualifier (um): umbric horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_umbric      <- function(pedon) .q_presence("Umbric",      umbric_horizon(pedon),   100, pedon)

#' Vertic qualifier (vr): vertic horizon <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_vertic      <- function(pedon) .q_presence("Vertic",      vertic_horizon(pedon),   100, pedon)

#' Vitric qualifier (vi): vitric properties >= 30 cm within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_vitric      <- function(pedon) .q_presence("Vitric",      vitric_properties(pedon), 100, pedon)


# ---------- CHEMISTRY-BASED PRINCIPAL QUALIFIERS ----------------------------

#' Acric qualifier (ac): argic horizon + low CEC + high Al.
#' v0.9: argic + CEC < 24 cmolc/kg clay + exch Al > Ca+Mg+K+Na.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_acric <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Acric", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing, reference = "WRB (2022) Ch 5"))
  ac <- acrisol(pedon)
  passed <- isTRUE(ac$passed)
  DiagnosticResult$new(name = "Acric", passed = passed,
    layers = ac$layers, evidence = list(acrisol = ac),
    missing = ac$missing, reference = "WRB (2022) Ch 5, Acric")
}

#' Alic qualifier (al): argic + high CEC + high Al saturation.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_alic <- function(pedon) {
  al <- alisol(pedon)
  DiagnosticResult$new(name = "Alic", passed = al$passed,
    layers = al$layers, evidence = list(alisol = al),
    missing = al$missing, reference = "WRB (2022) Ch 5, Alic")
}

#' Lixic qualifier (lx): argic + low CEC, low Al.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_lixic <- function(pedon) {
  lx <- lixisol(pedon)
  DiagnosticResult$new(name = "Lixic", passed = lx$passed,
    layers = lx$layers, evidence = list(lixisol = lx),
    missing = lx$missing, reference = "WRB (2022) Ch 5, Lixic")
}

#' Luvic qualifier (lv): argic + high CEC, low Al saturation.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_luvic <- function(pedon) {
  lv <- luvisol(pedon)
  DiagnosticResult$new(name = "Luvic", passed = lv$passed,
    layers = lv$layers, evidence = list(luvisol = lv),
    missing = lv$missing, reference = "WRB (2022) Ch 5, Luvic")
}

#' WRB 2022 exchangeable-acidity status over a depth window.
#'
#' WRB 2022 (Ch 5) defines Dystric/Eutric (and the Hyper-/Ortho- variants) by
#' \strong{exchangeable Al vs exchangeable bases}, NOT by base saturation:
#' a layer is "dystric-side" when exch. Al \eqn{>} \code{factor} times exch.
#' (Ca+Mg+K+Na), and "eutric-side" when exch. bases \eqn{\ge} \code{factor}
#' times exch. Al (\code{factor = 1} for Dystric/Eutric, \code{4} for the Hyper-
#' variants). Mineral layers use \code{al_sat_pct} (primary) or \code{al_cmol}
#' against the summed base cations; organic layers (\code{oc_pct >= 20}) use the
#' WRB Histosol pH branch (pH_water cutoffs 5.5, or 4.5/6.5 when \code{factor >=
#' 4}). Per the user's "strict" policy there is NO base-saturation fallback: a
#' layer with neither Al datum nor (for organic layers) pH is undeterminable and
#' counts as NA thickness.
#'
#' Returns thickness-weighted fractions of the candidate layers (those
#' overlapping \code{[dmin, dmax]}) that are dystric-side / eutric-side / NA,
#' plus the qualifying layer indices.
#' @noRd
.wrb_acidity_fracs <- function(pedon, dmin = 20, dmax = 100, factor = 1) {
  h <- pedon$horizons
  empty <- list(dystric = 0, eutric = 0, mid = 0, na = 1, layers_d = integer(0),
                layers_e = integer(0), total = 0)
  if (is.null(h) || nrow(h) == 0L) return(empty)
  ov_top <- pmax(h$top_cm, dmin)
  ov_bot <- pmin(h$bottom_cm, dmax)
  thk <- pmax(ov_bot - ov_top, 0)
  cand <- which(!is.na(thk) & thk > 0)
  if (length(cand) == 0L) return(empty)
  total <- sum(thk[cand])
  d_thk <- 0; e_thk <- 0; mid_thk <- 0; na_thk <- 0
  layers_d <- integer(0); layers_e <- integer(0)
  for (i in cand) {
    oc <- h$oc_pct[i]
    # status: TRUE=strong dystric-side, FALSE=strong eutric-side,
    #         "mid"=data present but neither (only for factor>1), NA=data absent.
    status <- NA
    if (!is.na(oc) && oc >= 20) {
      # organic material -> WRB Histosol pH branch
      ph <- h$ph_h2o[i]
      d_cut <- if (factor >= 4) 4.5 else 5.5
      e_cut <- if (factor >= 4) 6.5 else 5.5
      if (!is.na(ph)) {
        if (ph < d_cut) status <- TRUE
        else if (ph >= e_cut) status <- FALSE
        else status <- "mid"
      }
    } else {
      # mineral material -> exchangeable Al vs bases
      alc <- h$al_cmol[i]
      bcmol <- c(h$ca_cmol[i], h$mg_cmol[i], h$k_cmol[i], h$na_cmol[i])
      als <- h$al_sat_pct[i]
      if (!is.na(alc) && any(!is.na(bcmol))) {
        b <- sum(bcmol, na.rm = TRUE)
        if (alc > factor * b) status <- TRUE
        else if (b >= factor * alc) status <- FALSE
        else status <- "mid"
      } else if (!is.na(als)) {
        d_thr <- 100 * factor / (factor + 1)  # al_sat > this <=> Al > factor*bases
        e_thr <- 100 / (factor + 1)           # al_sat <= this <=> bases >= factor*Al
        if (als > d_thr) status <- TRUE
        else if (als <= e_thr) status <- FALSE
        else status <- "mid"
      }
    }
    if (isTRUE(status)) { d_thk <- d_thk + thk[i]; layers_d <- c(layers_d, i) }
    else if (isFALSE(status)) { e_thk <- e_thk + thk[i]; layers_e <- c(layers_e, i) }
    else if (identical(status, "mid")) mid_thk <- mid_thk + thk[i]
    else na_thk <- na_thk + thk[i]
  }
  list(dystric = d_thk / total, eutric = e_thk / total, mid = mid_thk / total,
       na = na_thk / total, layers_d = layers_d, layers_e = layers_e,
       total = total)
}

# Build the Dystric/Eutric result for a depth window from the fractions.
# side = "dystric" (Al-dom in >= half) or "eutric" (base-dom in > half).
.wrb_base_status_result <- function(pedon, name, side, dmin = 20, dmax = 100) {
  f <- .wrb_acidity_fracs(pedon, dmin, dmax, factor = 1)
  miss <- c("al_sat_pct", "al_cmol")
  if (f$total == 0)
    return(DiagnosticResult$new(name = name, passed = NA, layers = integer(0),
      evidence = list(reason = "no layers in depth window"),
      missing = "top_cm", reference = paste0("WRB (2022) Ch 5, ", name)))
  if (side == "dystric") {
    passed <- if (f$dystric >= 0.5) TRUE
              else if (f$dystric + f$na < 0.5) FALSE else NA
    layers <- f$layers_d
  } else {
    passed <- if (f$eutric > 0.5) TRUE
              else if (f$eutric + f$na <= 0.5) FALSE else NA
    layers <- f$layers_e
  }
  DiagnosticResult$new(name = name, passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(dystric_frac = f$dystric, eutric_frac = f$eutric,
                    na_frac = f$na),
    missing = if (is.na(passed)) miss else character(0),
    reference = paste0("WRB (2022) Ch 5, ", name))
}

# Build a Hyper- result: factor-1 dominance THROUGHOUT + factor-4 in major part.
.wrb_hyper_status_result <- function(pedon, name, side, dmin = 20, dmax = 100) {
  f1 <- .wrb_acidity_fracs(pedon, dmin, dmax, factor = 1)
  f4 <- .wrb_acidity_fracs(pedon, dmin, dmax, factor = 4)
  miss <- c("al_sat_pct", "al_cmol")
  if (f1$total == 0)
    return(DiagnosticResult$new(name = name, passed = NA, layers = integer(0),
      evidence = list(reason = "no layers in depth window"),
      missing = "top_cm", reference = paste0("WRB (2022) Ch 5, ", name)))
  if (side == "dystric") {
    through <- f1$dystric            # 1 means all layers dystric-side
    other   <- f1$eutric
    major4  <- f4$dystric
    na4     <- f4$na
    layers  <- f1$layers_d
  } else {
    through <- f1$eutric
    other   <- f1$dystric
    major4  <- f4$eutric
    na4     <- f4$na
    layers  <- f1$layers_e
  }
  passed <- if (other > 0) FALSE                         # a contrary layer
            else if (f1$na > 0) NA                       # cannot confirm "throughout"
            else if (major4 > 0.5) TRUE                  # throughout + strong major part
            else if (major4 + na4 <= 0.5) FALSE
            else NA
  DiagnosticResult$new(name = name, passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(through_frac = through, contrary_frac = other,
                    strong_major_frac = major4),
    missing = if (is.na(passed)) miss else character(0),
    reference = paste0("WRB (2022) Ch 5, ", name))
}

#' Dystric qualifier (dy), WRB 2022 Ch 5.
#'
#' Exchangeable Al \eqn{>} exchangeable bases (Ca+Mg+K+Na) in half or more of
#' the combined thickness of mineral layers between 20 and 100 cm (organic
#' layers use the Histosol pH_water < 5.5 branch). Uses \code{al_sat_pct} or
#' \code{al_cmol} vs the base cations; no base-saturation fallback (strict).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_dystric <- function(pedon) {
  .wrb_base_status_result(pedon, "Dystric", "dystric", 20, 100)
}

#' Eutric qualifier (eu), WRB 2022 Ch 5.
#'
#' Exchangeable bases (Ca+Mg+K+Na) \eqn{\ge} exchangeable Al in the major part
#' of the combined thickness of mineral layers between 20 and 100 cm (organic
#' layers use the Histosol pH_water \eqn{\ge} 5.5 branch). Strict: no
#' base-saturation fallback.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_eutric <- function(pedon) {
  .wrb_base_status_result(pedon, "Eutric", "eutric", 20, 100)
}

#' Magnesic qualifier (mg): exchangeable Ca/Mg < 1 in upper 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_magnesic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Magnesic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("ca_cmol", "mg_cmol"),
            reference = "WRB (2022) Ch 5, Magnesic"))
  ca <- h$ca_cmol[layers]; mg <- h$mg_cmol[layers]
  ratios <- ca / mg
  # WRB 2022 Ch 5 Magnesic: exch. Ca/Mg < 1 in a layer >= 30 cm thick within
  # 100 cm (the >= 30 cm combined thickness was missing).
   qly <- layers[which(!is.na(ratios) & ratios < 1)]
  thk <- if (length(qly) > 0L)
    sum(h$bottom_cm[qly] - h$top_cm[qly], na.rm = TRUE) else 0
  passed <- thk >= 30
  DiagnosticResult$new(name = "Magnesic", passed = passed,
    layers = if (passed) qly else integer(0),
    evidence = list(ca_mg_ratios = ratios, thickness_cm = thk),
    missing = if (any(is.na(ratios))) c("ca_cmol", "mg_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Magnesic")
}

#' Sodic qualifier (so): ESP >= 6\% (incl. SAR-derived).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_sodic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Sodic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "top_cm",
            reference = "WRB (2022) Ch 5, Sodic"))
  # WRB 2022 Ch 5 Sodic: >= 15% (Na+Mg) AND >= 6% Na on the exchange complex
  # (the v0.9 code checked only the >= 6% Na clause).
  na_pct <- vapply(layers, function(i) {
    if (is.na(h$na_cmol[i]) || is.na(h$cec_cmol[i]) || h$cec_cmol[i] <= 0)
      NA_real_
    else h$na_cmol[i] / h$cec_cmol[i] * 100
  }, numeric(1))
  namg_pct <- vapply(layers, function(i) {
    if (is.na(h$na_cmol[i]) || is.na(h$mg_cmol[i]) ||
          is.na(h$cec_cmol[i]) || h$cec_cmol[i] <= 0) NA_real_
    else (h$na_cmol[i] + h$mg_cmol[i]) / h$cec_cmol[i] * 100
  }, numeric(1))
  ok <- !is.na(na_pct) & na_pct >= 6 & !is.na(namg_pct) & namg_pct >= 15
  passed <- any(ok)
  DiagnosticResult$new(name = "Sodic", passed = passed,
    layers = layers[which(ok)],
    evidence = list(esp = na_pct, na_mg_pct = namg_pct),
    missing = if (any(is.na(na_pct)) || any(is.na(namg_pct)))
                c("na_cmol", "mg_cmol", "cec_cmol") else character(0),
    reference = "WRB (2022) Ch 5, Sodic")
}


# ---------- COLOUR-BASED PRINCIPAL QUALIFIERS -------------------------------

#' Rhodic qualifier (ro): hue redder than 5YR + value < 4 + dry no
#' more than 1 unit higher than moist (in upper subsoil 25-150 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_rhodic <- function(pedon) {
  # WRB 2022 Ch 5: a layer, >= 30 cm thick, between 25 and 150 cm, that shows
  # evidence of soil formation (cambic criterion 3) and has hue redder than
  # 5YR, value < 4 (moist) and a dry value not more than one unit higher than
  # the moist value.
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 150)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Rhodic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Rhodic"))
  hu <- .munsell_hue_units(h$munsell_hue_moist[cand])  # < 12.5 == redder than 5YR
  vm <- h$munsell_value_moist[cand]
  vd <- h$munsell_value_dry[cand]
  # value-dry clause applied only where dry colour is recorded (refine-present).
  colour_ok <- !is.na(hu) & hu < 12.5 & !is.na(vm) & vm < 4 &
    (is.na(vd) | vd <= vm + 1)
  colour_layers <- cand[colour_ok]
  sf <- test_cambic_soil_formation(h, candidate_layers = colour_layers)
  qual_layers <- intersect(colour_layers, sf$layers)
  thickness <- if (length(qual_layers) > 0L)
    sum(h$bottom_cm[qual_layers] - h$top_cm[qual_layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Rhodic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(thickness_cm = thickness),
    missing = if (all(is.na(hu))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Rhodic")
}

#' Chromic qualifier (cr): hue redder than 7.5YR + chroma > 4 (in upper
#' subsoil 25-150 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_chromic <- function(pedon) {
  # WRB 2022 Ch 5: a layer, >= 30 cm thick, between 25 and 150 cm, that shows
  # evidence of soil formation (cambic criterion 3) and has hue redder than
  # 7.5YR and chroma > 4 (moist), and does NOT meet the Rhodic qualifier.
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm <= 150)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Chromic", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Chromic"))
  hu  <- .munsell_hue_units(h$munsell_hue_moist[cand])  # < 15 == redder than 7.5YR
  chr <- h$munsell_chroma_moist[cand]
  colour_ok <- !is.na(hu) & hu < 15 & !is.na(chr) & chr > 4
  colour_layers <- cand[colour_ok]
  sf <- test_cambic_soil_formation(h, candidate_layers = colour_layers)
  qual_layers <- intersect(colour_layers, sf$layers)
  thickness <- if (length(qual_layers) > 0L)
    sum(h$bottom_cm[qual_layers] - h$top_cm[qual_layers], na.rm = TRUE) else 0
  not_rhodic <- !isTRUE(qual_rhodic(pedon)$passed)
  passed <- thickness >= 30 && not_rhodic
  DiagnosticResult$new(name = "Chromic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(thickness_cm = thickness, not_rhodic = not_rhodic),
    missing = if (all(is.na(hu))) "munsell_hue_moist" else character(0),
    reference = "WRB (2022) Ch 5, Chromic")
}

#' Xanthic qualifier (xa): ferralic + hue 7.5YR or yellower + value >=
#' 4 + chroma >= 5.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_xanthic <- function(pedon) {
  # WRB 2022 Ch 5: a ferralic horizon with a subhorizon >= 30 cm thick (within
  # 75 cm of the ferralic top) that has hue 7.5YR or yellower, value >= 4 and
  # chroma >= 5 (moist).
  fr <- ferralic(pedon)
  if (!isTRUE(fr$passed))
    return(DiagnosticResult$new(name = "Xanthic", passed = FALSE,
            layers = integer(0), evidence = list(ferralic = fr),
            missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic"))
  h <- pedon$horizons
  layers <- fr$layers
  hu  <- .munsell_hue_units(h$munsell_hue_moist[layers])  # >= 15 == 7.5YR or yellower
  v   <- h$munsell_value_moist[layers]
  chr <- h$munsell_chroma_moist[layers]
  ok <- !is.na(hu) & hu >= 15 & !is.na(v) & v >= 4 & !is.na(chr) & chr >= 5
  qual_layers <- layers[ok]
  thickness <- if (length(qual_layers) > 0L)
    sum(h$bottom_cm[qual_layers] - h$top_cm[qual_layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Xanthic", passed = passed,
    layers = if (passed) qual_layers else integer(0),
    evidence = list(ferralic = fr, thickness_cm = thickness),
    missing = fr$missing, reference = "WRB (2022) Ch 5, Xanthic")
}


# ---------- TEXTURE / SKELETIC QUALIFIERS ----------------------------------

#' Arenic qualifier (ar): texture sand or loamy sand >= 30 cm in <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_arenic <- function(pedon) .q_presence("Arenic", arenic_texture(pedon), 100, pedon)

#' Clayic qualifier (ce), WRB 2022 Ch 5.
#'
#' A texture class of \strong{clay, sandy clay or silty clay} in one or more
#' layers with a combined thickness of >= 30 cm within 100 cm of the mineral
#' soil surface. Those three classes correspond to clay >= 40\% (clay / silty
#' clay) or clay >= 35\% with sand >= 45\% (sandy clay) -- NOT the v0.9 proxy of
#' clay >= 60\%, which under-fired (fixed v0.9.130).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_clayic <- function(pedon) {
  h <- pedon$horizons
  clay <- h$clay_pct; sand <- h$sand_pct
  is_clayey <- !is.na(clay) &
    (clay >= 40 | (clay >= 35 & !is.na(sand) & sand >= 45))
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 & is_clayey)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Clayic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Clayic")
}

#' Loamic qualifier (lo): loam-class texture >= 30 cm in the upper 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_loamic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 &
                    !is.na(h$clay_pct) & !is.na(h$silt_pct) & !is.na(h$sand_pct) &
                    h$clay_pct >= 8 & h$clay_pct < 40 &
                    h$silt_pct >= 15)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Loamic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Loamic")
}

#' Siltic qualifier (sl): silt or silt-loam texture >= 30 cm in the upper
#' 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_siltic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100 &
                    !is.na(h$clay_pct) & !is.na(h$silt_pct) & !is.na(h$sand_pct) &
                    h$clay_pct < 35 & h$silt_pct >= 50)
  thickness <- if (length(layers) > 0L)
    sum(h$bottom_cm[layers] - h$top_cm[layers], na.rm = TRUE) else 0
  passed <- thickness >= 30
  DiagnosticResult$new(name = "Siltic", passed = passed,
    layers = layers, evidence = list(thickness_cm = thickness),
    missing = character(0), reference = "WRB (2022) Ch 5, Siltic")
}

#' Skeletic qualifier (sk): coarse fragments >= 40\% averaged over 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_skeletic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Skeletic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "coarse_fragments_pct",
            reference = "WRB (2022) Ch 5, Skeletic"))
  cf <- h$coarse_fragments_pct[layers]
  passed <- any(!is.na(cf) & cf >= 40)
  DiagnosticResult$new(name = "Skeletic", passed = passed,
    layers = layers[which(!is.na(cf) & cf >= 40)],
    evidence = list(coarse_fragments = cf),
    missing = if (any(is.na(cf))) "coarse_fragments_pct" else character(0),
    reference = "WRB (2022) Ch 5, Skeletic")
}


# ---------- ORGANIC / HUMUS QUALIFIERS --------------------------------------

#' Humic qualifier (hu): >= 1\% SOC in upper 50 cm (weighted average).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_humic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 50)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Humic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "oc_pct", reference = "WRB (2022) Ch 5, Humic"))
  oc <- h$oc_pct[layers]
  thk <- h$bottom_cm[layers] - h$top_cm[layers]
  if (all(is.na(oc)))
    return(DiagnosticResult$new(name = "Humic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "oc_pct", reference = "WRB (2022) Ch 5, Humic"))
  weighted <- sum(oc * thk, na.rm = TRUE) / sum(thk, na.rm = TRUE)
  passed <- !is.na(weighted) && weighted >= 1
  DiagnosticResult$new(name = "Humic", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(oc_weighted = weighted),
    missing = if (any(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Humic")
}

#' Ochric qualifier (oh): SOC >= 0.2\% upper 10 cm + no mollic/umbric.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ochric <- function(pedon) {
  has_mollic <- isTRUE(mollic(pedon)$passed)
  has_umbric <- isTRUE(umbric_horizon(pedon)$passed)
  if (has_mollic || has_umbric)
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0),
            evidence = list(mollic = has_mollic, umbric = has_umbric),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Ochric"))
  h <- pedon$horizons
  surface <- which(!is.na(h$top_cm) & h$top_cm <= 10)
  if (length(surface) == 0L)
    return(DiagnosticResult$new(name = "Ochric", passed = FALSE,
            layers = integer(0), evidence = list(),
            missing = character(0),
            reference = "WRB (2022) Ch 5, Ochric"))
  oc <- h$oc_pct[surface]
  passed <- any(!is.na(oc) & oc >= 0.2)
  DiagnosticResult$new(name = "Ochric", passed = passed,
    layers = surface[which(!is.na(oc) & oc >= 0.2)],
    evidence = list(oc = oc),
    missing = if (all(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Ochric")
}


# ---------- GENERIC CATCH-ALL ----------------------------------------------

#' Haplic qualifier (ha): no other principal qualifier of the RSG
#' applies. Always passes; the qualifier resolution machinery uses it
#' as the default when no other qualifier matched.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_haplic <- function(pedon) {
  DiagnosticResult$new(name = "Haplic", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Haplic (default)")
}


# ---------- v0.9 RESOLUTION ENGINE ------------------------------------------

# v0.9.2 family-suppression table.
# When several qualifiers from the same family pass for the same RSG,
# WRB convention is to print only the most-specific one. Each entry is
# ordered from most-specific (kept) to least-specific (suppressed).
# (Internal data; not exported. The full @export-tagged
# documentation for the resolve_wrb_qualifiers() function lives on
# the function definition itself, further down this file.)
.wrb_qualifier_families <- list(
  salinity   = c("Hypersalic",  "Salic",        "Hyposalic"),
  sodicity   = c("Hypersodic",  "Sodic",        "Hyposodic"),
  calcic     = c("Hypercalcic", "Calcic",       "Hypocalcic",  "Protocalcic"),
  gypsic     = c("Hypergypsic", "Gypsic",       "Hypogypsic",  "Protogypsic"),
  vertic     = c("Vertic",      "Protovertic"),
  albic      = c("Hyperalbic",  "Albic"),
  skeletic   = c("Hyperskeletic", "Skeletic"),
  eutric     = c("Hypereutric", "Eutric"),
  dystric    = c("Hyperdystric", "Dystric"),
  alic       = c("Hyperalic",   "Alic")
)

# Drop suppressed siblings within each family while preserving the
# original YAML order of the surviving names.
.suppress_qualifier_siblings <- function(matched) {
  if (length(matched) <= 1L) return(matched)
  drop <- character(0)
  for (family in .wrb_qualifier_families) {
    in_match <- intersect(family, matched)
    if (length(in_match) > 1L) {
      keeper <- in_match[which.min(match(in_match, family))]
      drop <- c(drop, setdiff(in_match, keeper))
    }
  }
  setdiff(matched, drop)
}

# Internal: evaluate a single qualifier name against a pedon. Returns
# a list(passed, layers, trace_entry). Used for both principal and
# supplementary slots.
.evaluate_qualifier <- function(pedon, qname) {
  spec <- .detect_specifier(qname)
  if (!is.null(spec)) {
    res <- tryCatch(
      .apply_specifier(pedon, spec$prefix, spec$base, spec$spec),
      error = function(e) NULL)
    if (is.null(res)) {
      return(list(passed = NA,
                  trace_entry = list(passed = NA,
                                     note = "specifier dispatch threw error")))
    }
    return(list(
      passed = res$passed,
      layers = res$layers %||% integer(0),
      trace_entry = list(passed = res$passed,
                         missing = res$missing %||% character(0),
                         specifier = spec$prefix,
                         base = spec$base)
    ))
  }
  fn_name <- paste0("qual_", tolower(qname))
  fn <- tryCatch(get(fn_name, envir = asNamespace("soilKey")),
                   error = function(e) NULL)
  if (is.null(fn)) {
    return(list(passed = NA,
                trace_entry = list(passed = NA,
                                   note = "function not implemented in v0.9")))
  }
  res <- tryCatch(fn(pedon), error = function(e) NULL)
  if (is.null(res)) {
    return(list(passed = NA,
                trace_entry = list(passed = NA,
                                   note = "diagnostic threw error")))
  }
  list(passed = res$passed,
       layers = res$layers %||% integer(0),
       trace_entry = list(passed = res$passed,
                          missing = res$missing %||% character(0)))
}

#' Resolve WRB 2022 qualifiers for a Reference Soil Group
#'
#' Walks the YAML qualifier list for a given RSG code and tests every
#' principal / supplementary qualifier against the pedon. Returns the
#' resolved canonical name pieces (principal + supplementary) plus a
#' per-qualifier trace.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code Two-letter RSG code (e.g. \code{"FR"} for Ferralsols).
#' @param rules Optional pre-loaded rules list (saves I/O when many
#'        RSGs are tested).
#' @param specifiers If \code{TRUE}, auto-attach WRB Ch 5 depth specifiers
#'        (Epi-/Endo-/Bathy-/Amphi-/Panto-/Kato-) to depth-anchored
#'        qualifiers based on the feature's actual depth. Default
#'        \code{FALSE} leaves names byte-identical to earlier versions.
#' @return A list with \code{principal} (character vector),
#'         \code{supplementary} (character vector), \code{trace}, and
#'         \code{trace_supplementary}.
#' @export
resolve_wrb_qualifiers <- function(pedon, rsg_code, rules = NULL,
                                   specifiers = FALSE) {
  rules <- rules %||% load_rules("wrb2022")
  qfile <- system.file("rules/wrb2022/qualifiers.yaml",
                          package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  if (!file.exists(qfile)) {
    return(list(principal = character(0), supplementary = character(0),
                  trace = list(), note = "qualifiers.yaml not found"))
  }
  qrules <- yaml::read_yaml(qfile)
  per_rsg <- qrules$rsg_qualifiers[[rsg_code]]
  if (is.null(per_rsg)) {
    return(list(principal = character(0), supplementary = character(0),
                  trace = list(),
                  note = sprintf("No qualifiers defined for RSG %s",
                                  rsg_code)))
  }

  trace_principal     <- list()
  trace_supplementary <- list()
  matched_principal     <- character(0)
  matched_supplementary <- character(0)
  layers_principal      <- list()   # qualifier name -> feature layers
  layers_supplementary  <- list()

  for (qname in per_rsg$principal %||% character(0)) {
    ev <- .evaluate_qualifier(pedon, qname)
    trace_principal[[qname]] <- ev$trace_entry
    if (isTRUE(ev$passed)) {
      matched_principal <- c(matched_principal, qname)
      layers_principal[[qname]] <- ev$layers %||% integer(0)
    }
  }
  for (qname in per_rsg$supplementary %||% character(0)) {
    ev <- .evaluate_qualifier(pedon, qname)
    trace_supplementary[[qname]] <- ev$trace_entry
    if (isTRUE(ev$passed)) {
      matched_supplementary <- c(matched_supplementary, qname)
      layers_supplementary[[qname]] <- ev$layers %||% integer(0)
    }
  }

  matched_principal <- .suppress_qualifier_siblings(matched_principal)
  if (length(matched_principal) == 0L) matched_principal <- "Haplic"
  # Apply family suppression to supplementary too -- the same logic
  # (only the most-specific sibling survives) keeps parenthesised
  # tags concise.
  matched_supplementary <- .suppress_qualifier_siblings(matched_supplementary)

  # v0.9.105: opt-in auto-attachment of WRB Ch 5 depth specifiers. Applied
  # AFTER sibling suppression (which keys on bare family names) so it never
  # interferes with it -- the specifier is just a prefix on the surviving
  # name. specifiers = FALSE leaves the canonical names byte-identical.
  if (isTRUE(specifiers)) {
    matched_principal <-
      .apply_depth_specifiers(pedon, matched_principal, layers_principal)
    matched_supplementary <-
      .apply_depth_specifiers(pedon, matched_supplementary, layers_supplementary)
  }

  list(principal     = matched_principal,
       supplementary = matched_supplementary,
       trace         = trace_principal,
       trace_supplementary = trace_supplementary)
}


#' Format a WRB 2022 soil name with qualifiers
#'
#' @param rsg_name Full RSG name (e.g. "Ferralsols").
#' @param principal Character vector of principal-qualifier names.
#' @param supplementary Character vector of supplementary-qualifier
#'        names (default empty in v0.9).
#' @return Formatted string per Ch 6 p 154 ("Rhodic Ferralsol (Clayic,
#'         Humic, Dystric)").
#' @export
format_wrb_name <- function(rsg_name, principal = character(0),
                              supplementary = character(0)) {
  rsg_singular <- sub("s$", "", rsg_name)
  prefix <- if (length(principal) > 0L)
              paste(principal, collapse = " ") else ""
  base <- paste(prefix, rsg_singular)
  base <- trimws(base)
  if (length(supplementary) > 0L) {
    suffix <- paste0(" (", paste(supplementary, collapse = ", "), ")")
    base <- paste0(base, suffix)
  }
  base
}
