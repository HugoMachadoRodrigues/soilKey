# ============================================================================
# WRB 2022 (4th ed.) -- Qualifiers Bloco C (v0.9.1).
#
# Adds the principal qualifiers required to fully wire the canonical
# Ch 4 lists for the next 6 RSGs of the key:
#
#   PZ  Podzols      PT  Plinthosols   PL  Planosols
#   ST  Stagnosols   NT  Nitisols      FR  Ferralsols
#
# This is the bloco brasileiro / tropical: Latossolos and Argissolos
# (Brazilian SiBCS) live here as Ferralsols / Nitisols / Acrisols /
# Lixisols. The signature additions are the Podzol spodic family
# (Carbic / Rustic / Hyperspodic / Ortsteinic / Placic / Densic),
# the very-low-CEC family of highly weathered tropical soils
# (Geric / Vetic / Posic / Hyperdystric / Hypereutric / Hyperalic),
# and the dark-illuvial sombric / hyperalbic special qualifiers.
# ============================================================================


# ---------- PODZOL SPODIC FAMILY --------------------------------------------

#' Hyperspodic qualifier (hp): spodic horizon with very strong active
#' Al + Fe accumulation (Al_ox + 0.5 * Fe_ox >= 1.5\%) -- twice the
#' minimum spodic threshold per WRB Ch 3.1. v0.9.1 also requires
#' p-retention >= 85\% in the same layers when available.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hyperspodic <- function(pedon) {
  sp <- spodic(pedon)
  if (!isTRUE(sp$passed))
    return(DiagnosticResult$new(name = "Hyperspodic", passed = FALSE,
            layers = integer(0), evidence = list(spodic = sp),
            missing = sp$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hyperspodic"))
  h <- pedon$horizons
  ly <- intersect(sp$layers, .in_upper(pedon, 200))
  al <- h$al_ox_pct[ly]; fe <- h$fe_ox_pct[ly]
  active <- al + 0.5 * fe
  ok <- !is.na(active) & active >= 1.5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hyperspodic", passed = passed,
    layers = ly[ok],
    evidence = list(spodic = sp, al_ox = al, fe_ox = fe,
                    al_plus_half_fe = active),
    missing = if (all(is.na(active))) c("al_ox_pct", "fe_ox_pct") else character(0),
    reference = "WRB (2022) Ch 5, Hyperspodic"
  )
}

#' Carbic qualifier (cb): spodic horizon dominated by humus illuviation.
#' v0.9.1: spodic + OC >= 6\% in some spodic layer (the WRB threshold for
#' Carbic / "humus-Podzol" expression).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_carbic <- function(pedon) {
  sp <- spodic(pedon)
  if (!isTRUE(sp$passed))
    return(DiagnosticResult$new(name = "Carbic", passed = FALSE,
            layers = integer(0), evidence = list(spodic = sp),
            missing = sp$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Carbic"))
  h <- pedon$horizons
  oc <- h$oc_pct[sp$layers]
  ok <- !is.na(oc) & oc >= 6
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Carbic", passed = passed,
    layers = sp$layers[ok],
    evidence = list(spodic = sp, oc_pct = oc),
    missing = if (all(is.na(oc))) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Carbic"
  )
}

#' Rustic qualifier (rs): iron-dominated spodic illuviation. v0.9.1:
#' spodic + OC < 1\% AND active iron (Fe_ox) >= 0.5\% in the same spodic
#' layer (humus-poor, Fe-rich ortstein / Bs).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_rustic <- function(pedon) {
  sp <- spodic(pedon)
  if (!isTRUE(sp$passed))
    return(DiagnosticResult$new(name = "Rustic", passed = FALSE,
            layers = integer(0), evidence = list(spodic = sp),
            missing = sp$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Rustic"))
  h <- pedon$horizons
  oc <- h$oc_pct[sp$layers]
  fe <- h$fe_ox_pct[sp$layers]
  ok <- !is.na(oc) & !is.na(fe) & oc < 1 & fe >= 0.5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Rustic", passed = passed,
    layers = sp$layers[ok],
    evidence = list(spodic = sp, oc_pct = oc, fe_ox = fe),
    missing = if (all(is.na(oc)) || all(is.na(fe)))
                c("oc_pct", "fe_ox_pct") else character(0),
    reference = "WRB (2022) Ch 5, Rustic"
  )
}

#' Ortsteinic qualifier (os): cemented spodic horizon. v0.9.1:
#' spodic horizon + cementation_class strongly OR indurated.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ortsteinic <- function(pedon) {
  sp <- spodic(pedon)
  if (!isTRUE(sp$passed))
    return(DiagnosticResult$new(name = "Ortsteinic", passed = FALSE,
            layers = integer(0), evidence = list(spodic = sp),
            missing = sp$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Ortsteinic"))
  h <- pedon$horizons
  ce <- h$cementation_class[sp$layers]
  ok <- !is.na(ce) & ce %in% c("strongly", "indurated")
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Ortsteinic", passed = passed,
    layers = sp$layers[ok],
    evidence = list(spodic = sp, cementation_class = ce),
    missing = if (all(is.na(ce))) "cementation_class" else character(0),
    reference = "WRB (2022) Ch 5, Ortsteinic"
  )
}

#' Placic qualifier (pi): thin (<= 25 mm = 2.5 cm) cemented Fe pan,
#' typically inside or just above a spodic horizon. v0.9.1: a layer
#' with cementation_class strongly or indurated AND thickness <= 2.5 cm,
#' anywhere in the upper 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_placic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  ce <- h$cementation_class[layers]
  thk <- pmax(0, h$bottom_cm[layers] - h$top_cm[layers])
  # WRB 2022 Ch 5 Placic: layer >= 0.1 and < 2.5 cm thick, Fe-cemented at
  # AT LEAST weakly (was restricted to strongly/indurated).
  ok <- !is.na(ce) & ce %in% c("weakly", "moderately", "strongly", "indurated") &
          !is.na(thk) & thk >= 0.1 & thk < 2.5
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Placic", passed = passed,
    layers = layers[ok],
    evidence = list(cementation_class = ce, thickness_cm = thk),
    missing = if (all(is.na(ce))) "cementation_class" else character(0),
    reference = "WRB (2022) Ch 5, Placic"
  )
}

#' Densic qualifier (dn): bulk density >= 1.8 g/cm3 in some root-
#' restricting layer within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_densic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Densic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "bulk_density_g_cm3",
            reference = "WRB (2022) Ch 5, Densic"))
  bd <- h$bulk_density_g_cm3[layers]
  ok <- !is.na(bd) & bd >= 1.8
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Densic", passed = passed,
    layers = layers[ok],
    evidence = list(bulk_density = bd),
    missing = if (all(is.na(bd))) "bulk_density_g_cm3" else character(0),
    reference = "WRB (2022) Ch 5, Densic"
  )
}


# ---------- ELUVIAL EXTREMES ------------------------------------------------

#' Hyperalbic qualifier (ha): albic horizon thicker than 100 cm in a
#' \emph{contiguous} run (extremely deep eluvial bleaching, common in
#' giant Podzols of tropical white-sand systems and the deepest
#' Stagnosol / Planosol profiles). Non-contiguous albic layers
#' separated by an illuvial Bs / Bt do NOT count toward the threshold.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hyperalbic <- function(pedon) {
  ab <- albic(pedon)
  if (!isTRUE(ab$passed))
    return(DiagnosticResult$new(name = "Hyperalbic", passed = FALSE,
            layers = integer(0), evidence = list(albic = ab),
            missing = ab$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hyperalbic"))
  h <- pedon$horizons
  # Albic must be eluvial -- exclude any layer that is simultaneously
  # argic (Bt, illuvial clay accumulation) or spodic (Bs / Bh,
  # illuvial Al-Fe-OC accumulation), and require the candidate to
  # carry positive eluvial evidence (designation \\code{E*} OR strict
  # claric Munsell: value moist >= 6 AND chroma moist <= 2). Without
  # these guards the v0.3.3 albic Munsell test over-accepts pale loess
  # parent material (BC / C horizons) as if they were eluvial.
  arg <- argic(pedon)
  sp  <- spodic(pedon)
  excl <- union(arg$layers %||% integer(0), sp$layers %||% integer(0))
  candidates <- setdiff(ab$layers, excl)
  if (length(candidates) > 0L) {
    desg  <- h$designation[candidates]
    vals  <- h$munsell_value_moist[candidates]
    chrs  <- h$munsell_chroma_moist[candidates]
    eluvial_evidence <- (!is.na(desg) & grepl("^E", desg, ignore.case = TRUE)) |
                          (!is.na(vals) & !is.na(chrs) & vals >= 6 & chrs <= 2)
    candidates <- candidates[eluvial_evidence]
  }
  if (length(candidates) == 0L)
    return(DiagnosticResult$new(name = "Hyperalbic", passed = FALSE,
            layers = integer(0),
            evidence = list(albic = ab, argic = arg, spodic = sp),
            missing = ab$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hyperalbic"))
  ord <- order(h$top_cm[candidates])
  ly  <- candidates[ord]
  # Walk the ordered albic layers, accumulating contiguous thickness.
  best <- 0; best_run <- integer(0)
  cur  <- 0; cur_run  <- integer(0)
  prev_bot <- NA_real_
  for (i in seq_along(ly)) {
    top <- h$top_cm[ly[i]]; bot <- h$bottom_cm[ly[i]]
    if (is.na(top) || is.na(bot)) next
    if (!is.na(prev_bot) && abs(top - prev_bot) < 1e-6) {
      cur <- cur + (bot - top); cur_run <- c(cur_run, ly[i])
    } else {
      cur <- bot - top; cur_run <- ly[i]
    }
    if (cur > best) { best <- cur; best_run <- cur_run }
    prev_bot <- bot
  }
  passed <- best >= 100
  DiagnosticResult$new(
    name = "Hyperalbic", passed = passed,
    layers = if (passed) best_run else integer(0),
    evidence = list(albic = ab, max_contiguous_thickness_cm = best),
    missing = ab$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Hyperalbic"
  )
}


# ---------- VERY-LOW-CEC FAMILY (HIGHLY WEATHERED TROPICAL SOILS) ----------

#' Geric qualifier (gr): a layer within 100 cm with exchangeable bases plus
#' exchangeable Al < 6 cmolc per kg clay.
#'
#' WRB 2022 Ch 5 (p. 135): "having within 100 cm of the mineral soil surface a
#' layer that has a sum of exchangeable bases (by 1 M NH4OAc, pH 7) plus
#' exchangeable Al (by 1 M KCl, unbuffered) of < 6 cmolc kg-1 clay" (v0.9.132
#' replaced the 1.5 cmolc per kg fine earth and the delta-pH branch). v0.9.316:
#' an exchangeable Al or base that is not recorded is no longer read as 0
#' (ISRIC's BR001 and BR002 have no Al where the bases alone are 5-6 per kg
#' clay); the layer is then undecided unless the recorded part alone reaches
#' 6. The Annex 2 pH substitute (Ch 9.13) is not used: Geric does not carry
#' that note. Depth is counted from the mineral soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_geric <- function(pedon) {
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Geric (p. 135)"
  miss <- c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "al_kcl_cmol", "clay_pct")
  ms <- .q_mineral_surface_cm(h)
  layers <- if (is.na(ms)) integer(0)
            else which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                         h$bottom_cm > ms & h$top_cm < ms + 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Geric", passed = NA,
            layers = integer(0), evidence = list(), missing = miss,
            reference = ref))
  bases <- cbind(h$ca_cmol[layers], h$mg_cmol[layers],
                 h$k_cmol[layers], h$na_cmol[layers])
  al <- ifelse(is.na(h$al_kcl_cmol[layers]), h$al_cmol[layers], h$al_kcl_cmol[layers])
  clay <- h$clay_pct[layers]
  complete <- rowSums(is.na(bases)) == 0L & !is.na(al)
  recorded <- rowSums(bases, na.rm = TRUE) + ifelse(is.na(al), 0, al)
  per_clay <- ifelse(!is.na(clay) & clay > 0, recorded / clay * 100, NA_real_)
  status <- ifelse(!is.na(clay) & clay <= 0, FALSE,
            ifelse(is.na(per_clay), NA,
            ifelse(per_clay >= 6, FALSE,
            ifelse(complete, TRUE, NA))))
  passed <- if (any(status %in% TRUE)) TRUE else if (anyNA(status)) NA else FALSE
  DiagnosticResult$new(
    name = "Geric", passed = passed,
    layers = if (isTRUE(passed)) layers[status %in% TRUE] else integer(0),
    evidence = list(bases_plus_al_per_kg_clay = per_clay, complete = complete,
                    layer_status = status),
    missing = if (is.na(passed)) miss else character(0),
    reference = ref
  )
}

#' Vetic qualifier (vt): CEC (1 N NH4OAc, pH 7) by clay does not exceed
#' 6 cmol+/kg clay in some layer at <= 100 cm. Stronger than the
#' ferralic-CEC threshold (< 16 cmol+/kg clay).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_vetic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Vetic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("cec_cmol", "clay_pct"),
            reference = "WRB (2022) Ch 5, Vetic"))
  cec <- h$cec_cmol[layers]
  clay <- h$clay_pct[layers]
  cec_per_clay <- cec / pmax(clay, 1) * 100
  ok <- !is.na(cec_per_clay) & cec_per_clay <= 6 &
          !is.na(clay) & clay >= 8
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Vetic", passed = passed,
    layers = layers[ok],
    evidence = list(cec = cec, clay_pct = clay,
                    cec_per_kg_clay = cec_per_clay),
    missing = if (all(is.na(cec_per_clay))) c("cec_cmol", "clay_pct") else character(0),
    reference = "WRB (2022) Ch 5, Vetic"
  )
}

#' Posic qualifier (po): net positive permanent charge (pH_KCl > pH_H2O)
#' in some layer at <= 100 cm. Diagnostic of the most weathered
#' Ferralsols where free Fe / Al oxides dominate the surface charge.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_posic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Posic", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("ph_h2o", "ph_kcl"),
            reference = "WRB (2022) Ch 5, Posic"))
  ph_h <- h$ph_h2o[layers]; ph_k <- h$ph_kcl[layers]
  delta <- ph_k - ph_h
  ok <- !is.na(delta) & delta > 0
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Posic", passed = passed,
    layers = layers[ok],
    evidence = list(delta_ph = delta, ph_h2o = ph_h, ph_kcl = ph_k),
    missing = if (all(is.na(delta))) c("ph_h2o", "ph_kcl") else character(0),
    reference = "WRB (2022) Ch 5, Posic"
  )
}


# ---------- BASE-SATURATION EXTREMES ----------------------------------------

#' Hyperdystric qualifier (yd), WRB 2022 Ch 5.
#'
#' Exchangeable Al > exchangeable bases THROUGHOUT 20-100 cm AND, in the major
#' part, exch. Al > 4 times the bases (Al-saturation > 80\%). Stronger than
#' Dystric. Strict exchangeable-Al criterion; no base-saturation fallback.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hyperdystric <- function(pedon) {
  .wrb_hyper_status_result(pedon, "Hyperdystric", "dystric", 20, 100)
}

#' Hypereutric qualifier (ye), WRB 2022 Ch 5.
#'
#' Exchangeable bases >= exchangeable Al THROUGHOUT 20-100 cm AND, in the major
#' part, bases >= 4 times Al (Al-saturation <= 20\%). Stronger than Eutric.
#' Strict; no base-saturation fallback.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hypereutric <- function(pedon) {
  .wrb_hyper_status_result(pedon, "Hypereutric", "eutric", 20, 100)
}

#' Hyperalic qualifier (yl): argic horizon with Al saturation >= 50\% in
#' some layer of the argic part within 100 cm. Stronger version of Alic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hyperalic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(DiagnosticResult$new(name = "Hyperalic", passed = FALSE,
            layers = integer(0), evidence = list(argic = arg),
            missing = arg$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Hyperalic"))
  h <- pedon$horizons
  ly <- intersect(arg$layers, .in_upper(pedon, 100))
  als <- h$al_sat_pct[ly]
  ok <- !is.na(als) & als >= 50
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Hyperalic", passed = passed,
    layers = ly[ok],
    evidence = list(argic = arg, al_sat_pct = als),
    missing = if (all(is.na(als))) "al_sat_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hyperalic"
  )
}


# ---------- DARK-ILLUVIAL SOMBRIC -------------------------------------------

#' Sombric qualifier (sm): sombric horizon (humus-illuviated layer at
#' depth) within 200 cm. WRB excludes layers that simultaneously meet
#' spodic or ferralic criteria from being Sombric -- those have
#' specific qualifiers of their own. v0.9.1 enforces both exclusions.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_sombric <- function(pedon) {
  so <- sombric(pedon)
  if (!isTRUE(so$passed))
    return(DiagnosticResult$new(name = "Sombric", passed = FALSE,
            layers = integer(0), evidence = list(sombric = so),
            missing = so$missing %||% character(0),
            reference = "WRB (2022) Ch 5, Sombric"))
  sp <- spodic(pedon)
  fr <- ferralic(pedon)
  exclude <- union(sp$layers %||% integer(0), fr$layers %||% integer(0))
  ly <- intersect(so$layers, .in_upper(pedon, 200))
  ly <- setdiff(ly, exclude)
  passed <- length(ly) > 0L
  DiagnosticResult$new(
    name = "Sombric", passed = passed,
    layers = ly,
    evidence = list(sombric = so, spodic = sp, ferralic = fr),
    missing = so$missing %||% character(0),
    reference = "WRB (2022) Ch 5, Sombric",
    notes = "v0.9.1: excludes layers that simultaneously meet spodic or ferralic"
  )
}
