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
#' Al + Fe accumulation (Al_ox + 0.5 * Fe_ox >= 1.5%) -- twice the
#' minimum spodic threshold per WRB Ch 3.1. v0.9.1 also requires
#' p-retention >= 85% in the same layers when available.
#' @export
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
#' v0.9.1: spodic + OC >= 6% in some spodic layer (the WRB threshold for
#' Carbic / "humus-Podzol" expression).
#' @export
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
#' spodic + OC < 1% AND active iron (Fe_ox) >= 0.5% in the same spodic
#' layer (humus-poor, Fe-rich ortstein / Bs).
#' @export
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
#' @export
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
#' @export
qual_placic <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  ce <- h$cementation_class[layers]
  thk <- pmax(0, h$bottom_cm[layers] - h$top_cm[layers])
  ok <- !is.na(ce) & ce %in% c("strongly", "indurated") &
          !is.na(thk) & thk <= 2.5
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
#' @export
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
#' @export
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

#' Geric qualifier (gr): in some layer at <= 100 cm, the effective
#' exchange complex (sum of bases + 1 N KCl Al-exchangeable) does not
#' exceed 1.5 cmol+/kg fine earth, OR the soil shows net positive charge
#' (delta pH = pH_KCl - pH_H2O > 0). The "or" path makes Geric / Posic
#' overlap by design (per WRB Ch 5).
#' @export
qual_geric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Geric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol",
                          "al_kcl_cmol", "ph_h2o", "ph_kcl"),
            reference = "WRB (2022) Ch 5, Geric"))
  ca <- h$ca_cmol[layers]; mg <- h$mg_cmol[layers]
  k  <- h$k_cmol[layers];  na <- h$na_cmol[layers]
  al <- h$al_kcl_cmol[layers]
  if (all(is.na(al))) al <- h$al_cmol[layers]
  ecec <- ca + mg + k + na + ifelse(is.na(al), 0, al)
  ok_ecec <- !is.na(ecec) & ecec <= 1.5
  ph_h <- h$ph_h2o[layers]; ph_k <- h$ph_kcl[layers]
  delta_ph <- ph_k - ph_h
  ok_dph <- !is.na(delta_ph) & delta_ph > 0
  ok <- ok_ecec | ok_dph
  passed <- any(ok)
  DiagnosticResult$new(
    name = "Geric", passed = passed,
    layers = layers[ok],
    evidence = list(ecec_proxy = ecec, delta_ph = delta_ph),
    missing = if (all(is.na(ecec)) && all(is.na(delta_ph)))
                c("cec components", "ph_kcl", "ph_h2o") else character(0),
    reference = "WRB (2022) Ch 5, Geric"
  )
}

#' Vetic qualifier (vt): CEC (1 N NH4OAc, pH 7) by clay does not exceed
#' 6 cmol+/kg clay in some layer at <= 100 cm. Stronger than the
#' ferralic-CEC threshold (<= 16 cmol+/kg clay).
#' @export
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
#' @export
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

#' Hyperdystric qualifier (yd): base saturation < 5% throughout the
#' upper 100 cm (mineral soil layers only). Stronger than Dystric (BS
#' < 50%).
#' @export
qual_hyperdystric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 20 & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Hyperdystric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "bs_pct",
            reference = "WRB (2022) Ch 5, Hyperdystric"))
  bs <- h$bs_pct[layers]
  passed <- length(bs) > 0L && all(!is.na(bs) & bs < 5)
  DiagnosticResult$new(
    name = "Hyperdystric", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(bs_pct = bs),
    missing = if (any(is.na(bs))) "bs_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hyperdystric"
  )
}

#' Hypereutric qualifier (ye): base saturation >= 80% throughout the
#' upper 100 cm. Stronger than Eutric (BS >= 50%).
#' @export
qual_hypereutric <- function(pedon) {
  h <- pedon$horizons
  layers <- which(!is.na(h$top_cm) & h$top_cm >= 20 & h$top_cm <= 100)
  if (length(layers) == 0L)
    return(DiagnosticResult$new(name = "Hypereutric", passed = NA,
            layers = integer(0), evidence = list(),
            missing = "bs_pct",
            reference = "WRB (2022) Ch 5, Hypereutric"))
  bs <- h$bs_pct[layers]
  passed <- length(bs) > 0L && all(!is.na(bs) & bs >= 80)
  DiagnosticResult$new(
    name = "Hypereutric", passed = passed,
    layers = if (passed) layers else integer(0),
    evidence = list(bs_pct = bs),
    missing = if (any(is.na(bs))) "bs_pct" else character(0),
    reference = "WRB (2022) Ch 5, Hypereutric"
  )
}

#' Hyperalic qualifier (yl): argic horizon with Al saturation >= 50% in
#' some layer of the argic part within 100 cm. Stronger version of Alic.
#' @export
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
#' @export
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
