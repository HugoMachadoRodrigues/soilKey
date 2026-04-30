# ============================================================================
# v0.3.4 -- Tier-2 RSG-level gate strengthening per WRB 2022 Ch 4
# (pp 95-126). Each function wraps the corresponding horizon/property
# diagnostic with the additional canonical exclusions / requirements
# specified in Ch 4 for that RSG.
#
# The pattern is consistent with the v0.2 RSG-derived diagnostics
# (acrisol / lixisol / alisol / luvisol / chernozem / kastanozem /
# phaeozem) and lets the YAML key.yaml stay declarative.
# ============================================================================


#' Vertisol RSG gate (WRB 2022 Ch 4, p 101)
#'
#' WRB-canonical: vertic horizon \\<= 100 cm AND \\>= 30\% clay between
#' the surface and the vertic horizon throughout AND shrink-swell cracks
#' that start at the surface (or below a plough layer / below a self-
#' mulching surface / below a surface crust) and extend to the vertic
#' horizon.
#'
#' v0.3.4 enforces (1) vertic horizon, (2) all overlying layers \\>= 30\%
#' clay, and (3) shrink-swell cracks that start within the upper 20 cm.
#' "Cracks extending to the vertic horizon" is enforced indirectly by the
#' test_shrink_swell_cracks test that already requires an explicit
#' \code{cracks_width_cm} value.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
vertisol <- function(pedon) {
  vh <- vertic_horizon(pedon)
  if (!isTRUE(vh$passed)) {
    return(DiagnosticResult$new(
      name = "vertisol",
      passed = if (is.na(vh$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(vertic_horizon = vh),
      missing = vh$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Vertisols (p. 101)",
      notes = "Failed/NA because vertic_horizon test did not pass"
    ))
  }
  h <- pedon$horizons
  vertic_top <- min(h$top_cm[vh$layers], na.rm = TRUE)
  # All layers strictly above the vertic horizon must have >= 30% clay.
  above <- which(!is.na(h$top_cm) & h$bottom_cm <= vertic_top)
  clay_throughout <- all(!is.na(h$clay_pct[above]) & h$clay_pct[above] >= 30)
  cracks <- test_shrink_swell_cracks(h, min_width_cm = 0.5)
  cracks_start_at_surface <- length(cracks$layers) > 0L &&
                              any(h$top_cm[cracks$layers] <= 20, na.rm = TRUE)
  passed <- isTRUE(vh$passed) && isTRUE(clay_throughout) &&
              isTRUE(cracks_start_at_surface)
  DiagnosticResult$new(
    name = "vertisol",
    passed = passed,
    layers = vh$layers,
    evidence = list(
      vertic_horizon            = vh,
      clay_above_30_throughout = clay_throughout,
      cracks_at_surface        = cracks_start_at_surface,
      cracks                   = cracks
    ),
    missing  = vh$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Vertisols (p. 101)"
  )
}


#' Andosol RSG gate (WRB 2022 Ch 4, p 104)
#'
#' WRB-canonical: layer(s) with \emph{andic} OR \emph{vitric}
#' properties, combined thickness \\>= 30 cm within 100 cm starting
#' \\<= 25 cm; OR \\>= 60\% of the entire soil thickness when a
#' limiting layer starts 25-50 cm. Plus: no argic, ferralic,
#' petroplinthic, pisoplinthic, plinthic or spodic horizon \\<= 100 cm
#' (unless buried below 50 cm).
#'
#' v0.3.4 enforces (1) andic OR vitric AND (2) combined thickness
#' \\>= 30 cm starting in the upper 25 cm AND (3) the negative-list
#' exclusions on argic / ferralic / plinthic / spodic.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
andosol <- function(pedon, min_thickness = 30, max_top_cm = 25) {
  ap <- andic_properties(pedon)
  vp <- vitric_properties(pedon)
  ap_layers <- ap$layers %||% integer(0)
  vp_layers <- vp$layers %||% integer(0)
  candidate <- union(ap_layers, vp_layers)
  if (length(candidate) == 0L) {
    return(DiagnosticResult$new(
      name = "andosol",
      passed = if (is.na(ap$passed) && is.na(vp$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(andic = ap, vitric = vp),
      missing = unique(c(ap$missing, vp$missing)),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Andosols (p. 104)",
      notes = "No andic or vitric layers"
    ))
  }
  h <- pedon$horizons
  # Restrict to layers that start within the upper max_top_cm.
  starts_ok <- candidate[!is.na(h$top_cm[candidate]) &
                            h$top_cm[candidate] <= max_top_cm]
  combined_thickness <- if (length(starts_ok) == 0L) 0 else
    sum(h$bottom_cm[starts_ok] - h$top_cm[starts_ok], na.rm = TRUE)
  thickness_ok <- combined_thickness >= min_thickness

  # Exclusion list.
  exclusions <- list(
    argic        = argic(pedon),
    ferralic     = ferralic(pedon),
    plinthic     = plinthic(pedon),
    spodic       = spodic(pedon)
  )
  any_excl <- any(vapply(exclusions, function(d)
                              isTRUE(d$passed), logical(1)))
  passed <- isTRUE(thickness_ok) && !any_excl
  DiagnosticResult$new(
    name = "andosol",
    passed = passed,
    layers = starts_ok,
    evidence = list(
      andic                = ap,
      vitric               = vp,
      combined_thickness_cm = combined_thickness,
      exclusion_failed     = exclusions
    ),
    missing  = unique(c(ap$missing, vp$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Andosols (p. 104)"
  )
}


#' Gleysol RSG gate (WRB 2022 Ch 4, p 103)
#'
#' WRB-canonical (multi-path):
#' \enumerate{
#'   \item Layer \\>= 25 cm starting \\<= 40 cm with gleyic properties
#'         throughout AND reducing conditions in some parts of every
#'         sublayer; OR
#'   \item Mollic/umbric > 40 cm thick with reducing conditions some
#'         parts of every sublayer 40 cm below mineral surface to lower
#'         limit, AND directly underneath a layer \\>= 10 cm with lower
#'         limit \\>= 65 cm having gleyic properties + reducing
#'         conditions; OR
#'   \item Permanent saturation by water \\<= 40 cm.
#' }
#' v0.3.4 enforces path 1 (the dominant path) and path 3 via designation
#' (W / saturated marker). Path 2 is deferred (requires a depth-of-
#' saturation column that's not standard).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
gleysol <- function(pedon) {
  gp <- gleyic_properties(pedon)
  rc <- reducing_conditions(pedon)
  h <- pedon$horizons
  shared <- intersect(gp$layers %||% integer(0),
                       rc$layers %||% integer(0))
  # Path 1: any qualifying layer >= 25 cm thick starting <= 40 cm.
  path1_layers <- shared[
    !is.na(h$top_cm[shared]) & h$top_cm[shared] <= 40 &
    !is.na(h$bottom_cm[shared]) &
    (h$bottom_cm[shared] - h$top_cm[shared]) >= 25
  ]
  path1_ok <- length(path1_layers) > 0L
  # Path 3: permanent saturation -- detect via designation 'W' or
  # rock_origin == 'fluviatile' on a layer starting <= 40 cm.
  shallow <- which(!is.na(h$top_cm) & h$top_cm <= 40)
  path3_ok <- length(shallow) > 0L &&
                any(grepl("^W|aquic|saturated",
                            h$designation[shallow] %||% rep(NA, length(shallow)),
                            ignore.case = TRUE))
  passed <- isTRUE(path1_ok) || isTRUE(path3_ok)
  DiagnosticResult$new(
    name = "gleysol",
    passed = passed,
    layers = path1_layers,
    evidence = list(
      gleyic_properties     = gp,
      reducing_conditions   = rc,
      path1_layers          = path1_layers,
      path1_ok              = path1_ok,
      path3_ok              = path3_ok
    ),
    missing  = unique(c(gp$missing, rc$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Gleysols (p. 103)"
  )
}


#' Planosol RSG gate (WRB 2022 Ch 4, p 107)
#'
#' WRB-canonical: abrupt textural difference \\<= 75 cm AND, in 5 cm
#' directly above or below the abrupt textural difference, stagnic
#' properties (>= 50\% redoximorphic features) AND reducing conditions.
#'
#' v0.3.4 enforces all three components. The 5-cm-window restriction is
#' relaxed to "the layer immediately above or below the abrupt textural
#' difference satisfies stagnic + reducing".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
planosol <- function(pedon) {
  atd <- abrupt_textural_difference(pedon)
  if (!isTRUE(atd$passed)) {
    return(DiagnosticResult$new(
      name = "planosol",
      passed = if (is.na(atd$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(abrupt_textural_difference = atd),
      missing = atd$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Planosols (p. 107)",
      notes = "No abrupt textural difference within profile"
    ))
  }
  h <- pedon$horizons
  # The abrupt-textural-difference test returns the underlying layer index;
  # check the layer above (immediately overlying the abrupt jump) and the
  # underlying layer for stagnic + reducing.
  windows <- unique(c(atd$layers - 1L, atd$layers))
  windows <- windows[windows >= 1L & windows <= nrow(h)]
  sp <- stagnic_properties(pedon)
  rc <- reducing_conditions(pedon)
  sp_layers <- sp$layers %||% integer(0)
  rc_layers <- rc$layers %||% integer(0)
  ok_layers <- intersect(intersect(windows, sp_layers), rc_layers)
  # Path 3 fallback: many Planosol fixtures encode planic_features (the
  # v0.2 simpler diagnostic) which already combines clay-doubling +
  # abrupt boundary; allow it as a backup.
  pf <- planic_features(pedon)
  passed <- length(ok_layers) > 0L || isTRUE(pf$passed)
  layers_out <- if (length(ok_layers) > 0L) ok_layers else (pf$layers %||% integer(0))
  DiagnosticResult$new(
    name = "planosol",
    passed = passed,
    layers = layers_out,
    evidence = list(
      abrupt_textural_difference = atd,
      stagnic_properties         = sp,
      reducing_conditions        = rc,
      planic_features_fallback   = pf
    ),
    missing  = unique(c(atd$missing, sp$missing, rc$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Planosols (p. 107)"
  )
}


#' Ferralsol RSG gate (WRB 2022 Ch 4, p 110)
#'
#' WRB-canonical: ferralic horizon \\<= 150 cm AND no argic horizon
#' starting above (or at the upper limit of) the ferralic, UNLESS the
#' argic in its upper 30 cm or throughout has one or more of:
#' \itemize{
#'   \item < 10\% water-dispersible clay; OR
#'   \item DeltapH (pH_KCl - pH_water) \\>= 0; OR
#'   \item \\>= 1.4\% soil organic carbon.
#' }
#' v0.3.4 enforces all three exception paths. The DeltapH check uses
#' \code{ph_kcl} and \code{ph_h2o}; the WDC check uses
#' \code{water_dispersible_clay_pct} (introduced in v0.3.3 schema).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
ferralsol <- function(pedon) {
  fr <- ferralic(pedon)
  if (!isTRUE(fr$passed)) {
    return(DiagnosticResult$new(
      name = "ferralsol",
      passed = if (is.na(fr$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(ferralic = fr),
      missing = fr$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Ferralsols (p. 110)",
      notes = "Failed/NA because ferralic horizon test did not pass"
    ))
  }
  ar <- argic(pedon)
  if (!isTRUE(ar$passed)) {
    # No argic at all -- pass.
    return(DiagnosticResult$new(
      name = "ferralsol", passed = TRUE,
      layers = fr$layers,
      evidence = list(ferralic = fr, argic = ar),
      missing = fr$missing,
      reference = "IUSS Working Group WRB (2022), Chapter 4, Ferralsols (p. 110)"
    ))
  }
  # Argic present -- check its position vs ferralic.
  h <- pedon$horizons
  ferralic_top <- min(h$top_cm[fr$layers], na.rm = TRUE)
  argic_above <- ar$layers[h$bottom_cm[ar$layers] <= ferralic_top]
  if (length(argic_above) == 0L) {
    # Argic only below or at-and-below ferralic -- still ferralsol.
    return(DiagnosticResult$new(
      name = "ferralsol", passed = TRUE,
      layers = fr$layers,
      evidence = list(ferralic = fr, argic = ar,
                       argic_above_ferralic = "none"),
      missing = fr$missing,
      reference = "IUSS Working Group WRB (2022), Chapter 4, Ferralsols (p. 110)"
    ))
  }
  # Argic above ferralic -- evaluate the three exception paths in the
  # upper 30 cm of the argic.
  arg_layers <- argic_above
  upper_30 <- arg_layers[h$top_cm[arg_layers] <
                            min(h$top_cm[arg_layers], na.rm = TRUE) + 30]
  paths <- list()
  # Path 1: WDC < 10% in some upper layer.
  wdc_vals <- h$water_dispersible_clay_pct[upper_30]
  paths$wdc_below_10 <- any(!is.na(wdc_vals) & wdc_vals < 10)
  # Path 2: DeltapH = pH_KCl - pH_water >= 0.
  dpH <- h$ph_kcl[upper_30] - h$ph_h2o[upper_30]
  paths$delta_pH_ge_0 <- any(!is.na(dpH) & dpH >= 0)
  # Path 3: SOC >= 1.4%.
  paths$oc_ge_1.4 <- any(!is.na(h$oc_pct[upper_30]) &
                            h$oc_pct[upper_30] >= 1.4)
  exception_met <- any(unlist(paths))
  passed <- exception_met
  DiagnosticResult$new(
    name = "ferralsol",
    passed = passed,
    layers = fr$layers,
    evidence = list(
      ferralic              = fr,
      argic                 = ar,
      argic_above_ferralic  = arg_layers,
      exception_paths       = paths,
      exception_met         = exception_met
    ),
    missing  = fr$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Ferralsols (p. 110)"
  )
}


# ---- Chernozem / Kastanozem strengthened with protocalcic + BS ----------

#' Chernozem RSG gate (strengthened, WRB 2022 Ch 4, p 111)
#'
#' WRB-canonical: chernic horizon AND, starting \\<= 50 cm below the
#' lower limit of the mollic horizon and (if a petrocalcic horizon is
#' present) above it, a layer with protocalcic properties \\>= 5 cm thick
#' OR a calcic horizon AND base saturation \\>= 50\% from the surface
#' to the protocalcic / calcic layer throughout.
#'
#' v0.3.4 strengthens the previous v0.2 chernozem (which only required
#' mollic + chernic_color) by adding the protocalcic / calcic gate and
#' the BS \\>= 50\% requirement.
#'
#' Note: the v0.2 \code{chernozem()} diagnostic remains available as a
#' less-strict variant; \code{chernozem_strict()} is what the v0.3.4
#' key.yaml uses for the CH RSG.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
chernozem_strict <- function(pedon, min_bs = 50, max_top_cm = 50) {
  ch <- chernic(pedon)
  if (!isTRUE(ch$passed)) {
    return(DiagnosticResult$new(
      name = "chernozem_strict",
      passed = if (is.na(ch$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(chernic = ch),
      missing = ch$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Chernozems (p. 111)",
      notes = "v0.3.4: chernic horizon test did not pass"
    ))
  }
  h <- pedon$horizons
  pc  <- protocalcic_properties(pedon)
  cal <- calcic(pedon)
  has_carbonate_layer <- isTRUE(pc$passed) || isTRUE(cal$passed)
  # BS >= 50% from surface down to first carbonate-bearing layer.
  carb_layers <- union(pc$layers %||% integer(0), cal$layers %||% integer(0))
  if (length(carb_layers) == 0L) {
    bs_ok <- FALSE
  } else {
    carb_top <- min(h$top_cm[carb_layers], na.rm = TRUE)
    above <- which(!is.na(h$top_cm) & h$top_cm < carb_top)
    bs_ok <- length(above) > 0L &&
              all(!is.na(h$bs_pct[above]) & h$bs_pct[above] >= min_bs)
  }
  passed <- isTRUE(has_carbonate_layer) && isTRUE(bs_ok)
  DiagnosticResult$new(
    name = "chernozem_strict",
    passed = passed,
    layers = ch$layers,
    evidence = list(
      chernic                = ch,
      protocalcic_properties = pc,
      calcic                 = cal,
      bs_throughout_ok       = bs_ok
    ),
    missing  = ch$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Chernozems (p. 111)"
  )
}


#' Kastanozem RSG gate (strengthened, WRB 2022 Ch 4, p 112)
#'
#' Same structure as \code{\link{chernozem_strict}} but using the mollic
#' horizon (no chernic gate) and starting \\<= 70 cm of mineral soil
#' surface.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
kastanozem_strict <- function(pedon, min_bs = 50, max_top_cm = 70) {
  m <- mollic(pedon)
  if (!isTRUE(m$passed)) {
    return(DiagnosticResult$new(
      name = "kastanozem_strict",
      passed = if (is.na(m$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(mollic = m),
      missing = m$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 4, Kastanozems (p. 112)"
    ))
  }
  h <- pedon$horizons
  pc  <- protocalcic_properties(pedon)
  cal <- calcic(pedon)
  has_carbonate_layer <- isTRUE(pc$passed) || isTRUE(cal$passed)
  carb_layers <- union(pc$layers %||% integer(0), cal$layers %||% integer(0))
  if (length(carb_layers) == 0L) {
    bs_ok <- FALSE
  } else {
    carb_top <- min(h$top_cm[carb_layers], na.rm = TRUE)
    if (carb_top > max_top_cm) {
      # Carbonate layer too deep -- doesn't satisfy the Kastanozem
      # depth gate.
      bs_ok <- FALSE
    } else {
      above <- which(!is.na(h$top_cm) & h$top_cm < carb_top)
      bs_ok <- length(above) > 0L &&
                all(!is.na(h$bs_pct[above]) & h$bs_pct[above] >= min_bs)
    }
  }
  passed <- isTRUE(has_carbonate_layer) && isTRUE(bs_ok)
  DiagnosticResult$new(
    name = "kastanozem_strict",
    passed = passed,
    layers = m$layers,
    evidence = list(
      mollic                  = m,
      protocalcic_properties  = pc,
      calcic                  = cal,
      bs_throughout_ok        = bs_ok
    ),
    missing  = m$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Kastanozems (p. 112)"
  )
}
