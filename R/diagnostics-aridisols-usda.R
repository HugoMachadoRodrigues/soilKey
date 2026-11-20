# =============================================================
# USDA Soil Taxonomy 13ed -- Aridisols helpers (Cap 7, pp 137-164)
# =============================================================
#
# Aridisols form in arid (or seasonally arid) climates and have one
# or more diagnostic subsurface horizons or characteristics
# (argillic, natric, calcic, gypsic, salic, duripan, etc.).
# 7 Suborders distinguished by the dominant subsurface feature.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 7.
# =============================================================


# One TRUE / FALSE / NA value from a DiagnosticResult (NA when the result
# carries no single logical).
.p3v <- function(r) {
  v <- r$passed
  if (length(v) != 1L || !is.logical(v)) NA else v
}

# Three-valued OR over any number of values.
.any3v_all <- function(...) Reduce(.any3v, list(...), FALSE)


#' Aridisol Order qualifier (USDA, KST 13ed, Key to Soil Orders item G)
#'
#' Follows item G of the Key to Soil Orders (KST 13 p 69) point by point,
#' three-valued (TRUE / FALSE / NA):
#' \itemize{
#'   \item G.1: (a) an aridic soil moisture regime, read only from
#'     \code{site$soil_moisture_regime} (v0.9.315); and (b) an ochric or
#'     anthropic epipedon; and either (c) within 100 cm a cambic horizon
#'     reaching 25 cm or more, a cryic regime and a cambic horizon, a
#'     calcic, gypsic, petrocalcic, petrogypsic or salic horizon or a
#'     duripan, or (d) an argillic or natric horizon;
#'   \item G.2: a salic horizon with saturation within 100 cm for a month
#'     or more, a moisture control section dry at some time, and no
#'     sulfuric horizon within 150 cm of the mineral soil surface.
#' }
#' A missing regime or an undecided criterion leaves the order undecided
#' (NA) and the key goes on to the next order.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aridisol_qualifying_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "aridisol_qualifying_usda", list(),
        .aridisol_qualifying_usda_uncached(pedon = pedon))
}

# The computation of aridisol_qualifying_usda().
.aridisol_qualifying_usda_uncached <- function(pedon) {
  smr <- smr_aridic_usda(pedon)
  # Key to Soil Orders, item G.1.a (KST 13 p 69): "An aridic soil moisture
  # regime". KST 13 p 48 defines it by the moisture control section in
  # normal years: "Dry in all parts for more than half of the cumulative
  # days per year" above 5 C at 50 cm, and moist for "less than 90
  # consecutive days" above 8 C. It is a property of the climate of the
  # soil, estimated "from climatic data" (p 47, the Newhall model), not
  # of the horizons. So a site that records no regime leaves the test
  # undecided (NA). Up to v0.9.314 a missing regime was read as aridic
  # whenever the surface (top <= 5 cm) organic carbon was below 1 % or
  # not recorded: a proxy the text does not give, which sent humid
  # Brazilian Latosols without a recorded regime to the Aridisols.
  aridic <- if (length(smr$missing) > 0L) NA else isTRUE(smr$passed)
  # G.1.b (p 69): "An ochric or anthropic epipedon".
  epi <- .aridisol_epipedon_usda(pedon)
  # G.1.c (p 69): "One or more of the following within 100 cm of the soil
  # surface". Up to v0.9.314 soilKey searched to 200 cm and also accepted
  # a kandic, a sulfuric and the WRB duric horizon, which item G does not
  # list, and any cambic horizon, without the 25 cm of the text.
  item_c <- .aridisol_item_c_usda(pedon)
  # G.1.d (p 69): "An argillic or natric horizon", at any depth.
  arg <- argillic_usda(pedon)
  nat <- .natric_horizon_impl(pedon)   # v0.9.310: USDA reading
  item_d <- .any3v(.p3v(arg), .p3v(nat))
  g1 <- .and3v(.and3v(aridic, .p3v(epi)), .any3v(.p3v(item_c), item_d))
  # G.2 (p 69): the salic path, which needs no aridic regime.
  g2 <- .aridisol_item_g2_usda(pedon)
  passed <- .any3v(g1, .p3v(g2))
  DiagnosticResult$new(
    name = "aridisol_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(aridic = smr, smr_pass = aridic, epipedon = epi,
                    item_c = item_c, argillic = arg, natric = nat,
                    item_d = item_d, item_g1 = g1, item_g2 = g2,
                    diag_present = .any3v(.p3v(item_c), item_d)),
    # the attributes that leave the order undecided, when it is
    missing = if (!is.na(passed)) character(0) else unique(c(
      smr$missing, if (is.na(.p3v(epi))) epi$missing,
      if (is.na(.p3v(item_c))) item_c$missing,
      if (is.na(item_d)) c(arg$missing, nat$missing),
      if (is.na(.p3v(g2))) g2$missing)),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 4, Key to Soil Orders, item G (p 69)"
  )
}


#' Ochric or anthropic epipedon for the Aridisols (KST 13 item G.1.b)
#'
#' KST 13 p 16: the ochric epipedon "fails to meet the definitions for any
#' of the other seven epipedons". Read three-valued: an epipedon at the
#' mineral soil surface (an A or E horizon; KST 13 p 16 includes "eluvial
#' horizons that are at or near the soil surface") that is not a histic,
#' folistic, melanic, mollic or umbric epipedon. Any of these present: FALSE;
#' one undecided and none present: NA; no recorded designation: NA. The
#' anthropic epipedon is accepted by item G.1.b as well (v0.9.419:
#' \code{anthropic_epipedon_usda()}), and a plaggen epipedon is one of the
#' others (\code{plaggen_epipedon_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
.aridisol_epipedon_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13ed, Ch. 3, ochric epipedon (p 16); Ch. 4, item G.1.b (p 69)"
  h <- pedon$horizons
  if (is.null(h) || nrow(h) == 0L)
    return(DiagnosticResult$new(name = "aridisol_epipedon_usda", passed = FALSE,
             layers = integer(0), evidence = list(reason = "empty horizons"),
             missing = character(0), reference = ref))
  ms <- .mineral_soil_surface_cm(h)
  if (is.na(ms)) ms <- 0
  surf <- which(!is.na(h$top_cm) & h$top_cm <= ms + 5)
  des <- h$designation[surf]
  has_epi <- if (length(surf) == 0L || all(is.na(des))) NA
             else any(grepl("^[AE]", des[!is.na(des)]))
  others <- list(
    plaggen  = plaggen_epipedon_usda(pedon),
    histic   = histic_epipedon_usda(pedon),
    folistic = folistic_epipedon_usda(pedon),
    melanic  = melanic_epipedon_usda(pedon),
    mollic   = mollic_epipedon_usda(pedon),
    umbric   = umbric_epipedon_usda(pedon)
  )
  # The mollic and umbric epipedons need their colour, organic carbon, base
  # saturation and structure in the same layers. Their helpers return NA when
  # any part is undecided, even when another required part fails in every
  # candidate layer (no dark colour, too little carbon): read such a result
  # as FALSE, since no layer can then meet all the requirements.
  decisive <- c("color", "organic_carbon", "base_saturation",
                "base_saturation_low", "structure")
  ov <- vapply(others, function(r) {
    v <- .p3v(r)
    if (is.na(v) && is.list(r$evidence) &&
        any(vapply(r$evidence[intersect(decisive, names(r$evidence))],
                   function(t) is.list(t) && isFALSE(t$passed), logical(1))))
      v <- FALSE
    v
  }, logical(1))
  not_other <- if (any(ov %in% TRUE)) FALSE else if (anyNA(ov)) NA else TRUE
  # v0.9.419: the anthropic epipedon (anthropic_epipedon_usda(), KST 13 p.
  # 13) passes item G.1.b as well; the plaggen one does not.
  anth <- .p3v(anthropic_epipedon_usda(pedon))
  passed <- .any3v(isTRUE(anth), .and3v(has_epi, not_other))
  miss <- character(0)
  if (is.na(has_epi)) miss <- c(miss, "designation")
  if (is.na(not_other))
    miss <- c(miss, unlist(lapply(others[is.na(ov)], function(r) r$missing)))
  DiagnosticResult$new(
    name = "aridisol_epipedon_usda", passed = passed,
    layers = if (isTRUE(passed)) surf[grepl("^[AE]", des) & !is.na(des)] else integer(0),
    evidence = list(epipedon_at_surface = has_epi, other_epipedons = ov),
    missing = unique(miss), reference = ref)
}


#' Item G.1.c of the Key to Soil Orders (KST 13 p 69), three-valued
#'
#' One or more within 100 cm of the soil surface: a cambic horizon with a
#' lower depth of 25 cm or more; a cryic soil temperature regime and a
#' cambic horizon; a calcic, gypsic, petrocalcic, petrogypsic or salic
#' horizon; or a duripan. The anhydritic horizon of the list is not read:
#' soilKey records no anhydrite.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Depth of the item, 100 cm.
#' @noRd
.aridisol_item_c_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  # Cambic horizon within 100 cm, and whether it reaches 25 cm: "a cambic
  # horizon with a lower depth of 25 cm or more" (p 69).
  # v0.9.419: the USDA cambic horizon (cambic_horizon_usda(), KST 13 pp.
  # 22-23), no longer the WRB reading; depths from the soil surface.
  cb <- cambic_horizon_usda(pedon)
  cambic <- .cambic_within_usda(pedon, max_top_cm = max_top_cm, min_bottom_cm = 0,
                                mineral = FALSE)
  cambic_25 <- .cambic_within_usda(pedon, max_top_cm = max_top_cm, min_bottom_cm = 25,
                                   mineral = FALSE)
  # "a cryic soil temperature regime and a cambic horizon" (p 69).
  st <- str_cryic_usda(pedon)
  cryic <- if (length(st$missing) > 0L) NA else isTRUE(st$passed)
  cryic_cambic <- .and3v(cryic, cambic)
  # "an anhydritic, calcic, gypsic, petrocalcic, petrogypsic, or salic
  # horizon; or a duripan" (p 69).
  hz <- list(
    calcic      = calcic_horizon_usda(pedon, max_top_cm = max_top_cm),
    gypsic      = gypsic_horizon_usda(pedon, max_top_cm = max_top_cm),
    petrocalcic = petrocalcic_subgroup_usda(pedon, max_top_cm = max_top_cm),
    petrogypsic = petrogypsic_horizon_usda(pedon, max_top_cm = max_top_cm),
    salic       = salic_horizon_usda(pedon, max_top_cm = max_top_cm),
    duripan     = duripan_usda(pedon, max_top_cm = max_top_cm)
  )
  hv <- vapply(hz, .p3v, logical(1))
  passed <- do.call(.any3v_all, c(list(cambic_25, cryic_cambic), as.list(unname(hv))))
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(cambic_25)) miss <- c(miss, cb$missing)
    if (is.na(cryic_cambic)) miss <- c(miss, st$missing)
    miss <- c(miss, unlist(lapply(hz[is.na(hv)], function(r) r$missing)))
  }
  DiagnosticResult$new(
    name = "aridisol_item_c_usda", passed = passed, layers = integer(0),
    evidence = c(list(cambic = cambic, cambic_lower_depth_25 = cambic_25,
                      cryic_and_cambic = cryic_cambic), as.list(hv)),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 4, item G.1.c (p 69)")
}


#' Item G.2 of the Key to Soil Orders (KST 13 p 69), three-valued
#'
#' A salic horizon; and (a) saturation with water in one or more layers
#' within 100 cm for 1 month or more in a normal year, read from
#' \code{water_saturation_days} (30 days or more); and (b) a moisture
#' control section dry in some or all parts at some time in normal years,
#' TRUE for a recorded aridic, torric, ustic or xeric regime (each is
#' defined by dry days, KST 13 pp 48-49) and undecided otherwise; and (c)
#' no sulfuric horizon within 150 cm of the mineral soil surface: TRUE when
#' every layer starting there has a pH of 4.0 or more (the sulfuric
#' horizon has "a pH value ... of 3.5 or less or less than 4.0" with
#' sulfides, KST 13 p 51), FALSE when the sulfuric horizon is found.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
.aridisol_item_g2_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13ed, Ch. 4, item G.2 (p 69)"
  # "Have a salic horizon" (no depth is given in G.2).
  sal <- salic_horizon_usda(pedon, max_top_cm = Inf)
  salic <- .p3v(sal)
  # (a) "Saturation with water in one or more layers within 100 cm of the
  # soil surface for 1 month or more during a normal year".
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  sd <- if (!is.null(h$water_saturation_days)) h$water_saturation_days[cand] else rep(NA_real_, length(cand))
  sat <- if (any(sd >= 30, na.rm = TRUE)) TRUE
         else if (length(cand) > 0L && !anyNA(sd)) FALSE else NA
  # (b) "A moisture control section that is dry in some or all parts at
  # some time during normal years".
  smr <- .usda_smr_value(as.character(pedon$site$soil_moisture_regime %||% NA_character_)[1L])
  dry <- if (!is.na(smr) && smr %in% c("aridic", "torric", "ustic", "xeric")) TRUE else NA
  # (c) "No sulfuric horizon within 150 cm of the mineral soil surface".
  ms <- .mineral_soil_surface_cm(h)
  if (is.na(ms)) ms <- 0
  c150 <- which(!is.na(h$top_cm) & h$top_cm < ms + 150 & h$top_cm >= ms)
  ph <- h$ph_h2o[c150]
  no_sulfuric <- if (length(c150) > 0L && !anyNA(ph) && all(ph >= 4.0)) TRUE
                 else if (isTRUE(.p3v(sulfuric_horizon_usda(pedon, max_top_cm = ms + 150)))) FALSE
                 else NA
  passed <- Reduce(.and3v, list(salic, sat, dry, no_sulfuric), TRUE)
  miss <- character(0)
  if (is.na(passed)) {
    if (is.na(salic)) miss <- c(miss, sal$missing)
    if (is.na(sat)) miss <- c(miss, "water_saturation_days")
    if (is.na(dry)) miss <- c(miss, "site$soil_moisture_regime")
    if (is.na(no_sulfuric)) miss <- c(miss, "ph_h2o")
  }
  DiagnosticResult$new(
    name = "aridisol_item_g2_usda", passed = passed, layers = integer(0),
    evidence = list(salic = salic, saturation_1_month = sat,
                    dry_at_some_time = dry, no_sulfuric_150 = no_sulfuric),
    missing = unique(miss), reference = ref)
}


#' Petrocalcic horizon (USDA Soil Taxonomy, KST 13th ed., pp. 28-29)
#'
#' A horizon pedogenically cemented or indurated by carbonates (with or
#' without other agents), whose lateral continuity limits the roots, 10 cm
#' or more thick (1 cm for a laminar cap directly on bedrock). v0.9.419: up
#' to v0.9.418 any strongly cemented or indurated layer with 15 \% CaCO3,
#' with no reading of the cementing agent (audit I10). Read layer by layer:
#' cemented is a recorded cementation class (weakly cemented or more: the
#' KST "cemented" begins at extremely weakly coherent), an m in the
#' designation or a recorded calcrete pan (\code{pan_type}); carbonates as
#' the agent are a k in the designation, a calcrete pan, or 15 \% or more
#' CaCO3. A cemented layer with q and no k is silica-cemented (a duripan),
#' not petrocalcic; a cemented layer with no record of the agent is
#' undecided. Cementation not recorded: not cemented (a field
#' observation). Used by the Petrocalcic subgroups and the Petro- great
#' groups, by item K.1.b and the cambic exclusions.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Upper boundary within this depth (cm).
#' @noRd
petrocalcic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 3, petrocalcic horizon (pp. 28-29)"
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(DiagnosticResult$new(name = "petrocalcic_subgroup_usda", passed = FALSE, reference = ref))
  d <- .usda_desig(h$designation)
  sx <- ifelse(is.na(d$suffix), "", d$suffix)
  cem <- tolower(as.character(.usda_col(h, "cementation_class", NA_character_)))
  pan <- tolower(as.character(.usda_col(h, "pan_type", NA_character_)))
  calcrete <- !is.na(pan) & grepl("calcrete", pan)
  cas <- .usda_col(h, "caco3_pct", NA_real_)
  cemented <- (!is.na(cem) & cem %in% c("weakly", "moderately", "strongly", "indurated")) |
              grepl("m", sx) | calcrete
  k <- grepl("k", sx)
  silica_only <- grepl("q", sx) & !k
  carb <- ifelse(k | calcrete, TRUE, ifelse(silica_only, FALSE,
          ifelse(is.na(cas), NA, cas >= 15)))
  lay <- ifelse(cemented, carb, FALSE)
  ord <- order(h$top_cm)
  rock <- !is.na(d$master) & d$master == "R"
  pos <- integer(n); pos[ord] <- seq_len(n)
  thick_ok <- function(r) {
    t <- .usda_run_thickness(h, r)
    last <- r[length(r)]
    on_rock <- pos[last] < n && rock[ord[pos[last] + 1L]]
    t >= 10 || (on_rock && t >= 1)
  }
  keep <- function(runs) Filter(function(r) thick_ok(r) && min(h$top_cm[r]) < max_top_cm, runs)
  yes <- keep(.usda_runs(lay %in% TRUE, ord))
  may <- keep(.usda_runs(!(lay %in% FALSE), ord))
  passed <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  DiagnosticResult$new(
    name = "petrocalcic_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) sort(unique(unlist(yes)))
             else if (is.na(passed)) sort(unique(unlist(may))) else integer(0),
    evidence = list(max_top_cm = max_top_cm, cemented = cemented, carbonate_agent = carb),
    missing = if (is.na(passed)) "caco3_pct" else character(0),
    reference = ref
  )
}


#' Petrogypsic Subgroup helper -- delegate to petrogypsic_horizon_usda
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
petrogypsic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- petrogypsic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "petrogypsic_subgroup_usda"
  res
}


#' Sodic Subgroup helper -- delegate to natric_horizon (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sodic_subgroup_usda <- function(pedon) {
  res <- natric_horizon_usda(pedon)
  res$name <- "sodic_subgroup_usda"
  res
}


#' Petronodic Subgroup helper (Aridisols)
#' Pass when 5\%+ rock fragments cemented by carbonates within 100 cm.
#' v0.8 proxy: caco3_pct >= 15 AND coarse_fragments_pct >= 5.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
petronodic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cas <- h$caco3_pct[cand]
  cf  <- h$coarse_fragments_pct[cand]
  passing <- cand[!is.na(cas) & cas >= 15 &
                      !is.na(cf) & cf >= 5]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "petronodic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
  )
}


#' Argic Aridisol helper -- argillic-or-kandic in Argids/Cryids/etc.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
argic_aridisol_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "argic_aridisol_usda"
  res
}


#' Paleargid qualifying helper
#' Pass when argillic horizon has continuous clay films AND
#' clay >> 35\% in upper 10 cm (proxy for old, well-developed argillic).
#' v0.8 proxy: argillic + clay_pct >= 35 in upper 30 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
paleargid_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  arg <- argillic_within_usda(pedon, max_top_cm = 100)
  if (!isTRUE(arg$passed) || length(arg$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "paleargid_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no argillic"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
    ))
  }
  upper_arg <- arg$layers[order(h$top_cm[arg$layers])][1]
  cl <- h$clay_pct[upper_arg]
  passed <- !is.na(cl) && cl >= 35
  DiagnosticResult$new(
    name = "paleargid_qualifying_usda", passed = passed,
    layers = if (passed) upper_arg else integer(0),
    evidence = list(clay_upper_argillic_pct = cl),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
  )
}


#' Vertic Aridisols helper -- delegates to vertic_subgroup_usda
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vertic_aridisol_usda <- function(pedon) {
  res <- vertic_subgroup_usda(pedon)
  res$name <- "vertic_aridisol_usda"
  res
}
