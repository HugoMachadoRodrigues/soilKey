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
#' WRB 2022 Ch 4 (p. 101), Vertisols: other soils having
#' \enumerate{
#'   \item a vertic horizon starting \\<= 100 cm from the mineral soil
#'         surface;
#'   \item \\>= 30\% clay between the mineral soil surface and the vertic
#'         horizon throughout;
#'   \item shrink-swell cracks that start (a) at the mineral soil surface,
#'         (b) at the base of a plough layer, (c) directly below a
#'         self-mulching surface (a layer with strong granular, or strong
#'         angular or subangular blocky, structure of aggregates \\<= 1 cm)
#'         or (d) directly below a surface crust, and extend to the vertic
#'         horizon.
#' }
#' Since v0.9.311 each item is TRUE, FALSE or NA (not recorded), and the gate
#' is NA when an item is undecided and none fails. Only a vertic horizon that
#' can start within 100 cm decides item 1, so unrecorded layers below it do
#' not leave the gate undecided. Item 3 reads the cracks of each layer as
#' \code{\link{vertic_horizon}} does (\code{cracks_width_cm}, or slickensides
#' or an \code{ss} designation as evidence of them): every layer between the
#' start and the vertic horizon must have them, and a layer whose cracks are
#' not recorded leaves the item undecided. Until v0.9.310 item 3 asked for a
#' layer with cracks starting within 20 cm and read missing cracks as absent.
#'
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the overlying-clay threshold is raised from
#' 30\% to 35\%, tightening the gate against marginally clayey profiles
#' that satisfy the vertic horizon but sit close to the Vertisol cut-off.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} applies the Tier-3 strengthened threshold.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
vertisol <- function(pedon, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  min_clay <- if (strict) 35L else 30L
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Vertisols (p. 101)"
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0

  # 1. a vertic horizon starting <= 100 cm from the mineral soil surface
  vh <- .vertic_horizon_wrb(pedon, max_top_cm = ms + 100)
  c1 <- vh$passed
  if (isFALSE(c1))
    return(DiagnosticResult$new(
      name = "vertisol", passed = FALSE, layers = integer(0),
      evidence = list(vertic_horizon = vh, mineral_surface_cm = ms),
      missing = character(0), reference = ref,
      notes = "No vertic horizon starting <= 100 cm from the mineral soil surface"))
  # the horizon's top; while it is undecided, the shallowest top it could
  # have, where items 2 and 3 can still fail
  top <- if (isTRUE(c1)) min(h$top_cm[vh$layers], na.rm = TRUE)
         else vh$evidence$possible_top_cm

  # 2. >= 30 % clay throughout between the mineral soil surface and the
  # vertic horizon (35 % in strict mode)
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm)
  above <- which(ok & h$top_cm < top & h$bottom_cm > ms)
  c2 <- if (!length(above)) TRUE
        else .and3(ifelse(is.na(h$clay_pct[above]), NA,
                          h$clay_pct[above] >= min_clay))

  # 3. shrink-swell cracks that start at the surface (or below a plough
  # layer or a self-mulching surface) and extend to the vertic horizon
  cracks <- .vertisol_cracks_start(h, ms = ms, top = top)
  # v0.9.77 -- when vertic_horizon fired via the v0.9.72 v-suffix
  # designation inference OR the v0.9.76 chroma+clay inference (both
  # opt-in), the cracks item is allowed to be missing because the
  # inference paths themselves require strong morphological evidence.
  inferred_path_fired <- isTRUE(vh$evidence$designation_inference$passed) ||
                            isTRUE(vh$evidence$chroma_clay_inference$passed)
  c3 <- if (inferred_path_fired) TRUE else cracks$passed

  passed <- .and3(c1, c2, c3)
  missing <- if (is.na(passed)) unique(c(
    if (is.na(c1)) vh$missing,
    if (is.na(c2)) "clay_pct",
    if (is.na(c3)) "cracks_width_cm")) else character(0)
  DiagnosticResult$new(
    name = "vertisol",
    passed = passed,
    layers = if (isTRUE(passed)) vh$layers else integer(0),
    evidence = list(
      vertic_horizon                = vh,
      mineral_surface_cm            = ms,
      vertic_top_cm                 = top,
      clay_above_30_throughout      = c2,
      clay_threshold_pct            = min_clay,
      strict_mode                   = strict,
      cracks_at_surface             = c3,
      cracks                        = cracks,
      morphological_inference_fired = inferred_path_fired
    ),
    missing  = missing,
    reference = ref
  )
}


# Item 3 of the Vertisols key (WRB 2022 Ch 4, p. 101): shrink-swell cracks
# that start (a) at the mineral soil surface, (b) at the base of a plough
# layer, (c) directly below a self-mulching surface, or (d) directly below a
# surface crust, and extend to the vertic horizon starting at `top`. A start
# is met when every layer between it and the vertic horizon has cracks
# (.vertic_crack_status()). A surface crust is a few cm at most, inside the
# surface layer of a description, so (d) is read as (a). TRUE, FALSE or NA.
.vertisol_cracks_start <- function(h, ms, top, min_width_cm = 0.5) {
  cr <- .vertic_crack_status(h, min_width_cm = min_width_cm)
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm)
  span <- function(from) {
    idx <- which(ok & h$top_cm < top & h$bottom_cm > from)
    if (!length(idx)) TRUE else .and3(cr[idx])
  }
  # the layers from the mineral soil surface downwards, in depth order
  surf <- which(ok & h$bottom_cm > ms)
  surf <- surf[order(h$top_cm[surf])]
  # base of the run of surface layers where `cond` is TRUE (NA if none)
  run_base <- function(cond) {
    base <- NA_real_; edge <- ms
    for (i in surf) {
      if (h$top_cm[i] > edge || !isTRUE(cond[i])) break
      edge <- max(edge, h$bottom_cm[i]); base <- edge
    }
    base
  }
  dsg <- as.character(h$designation)
  # (b) a plough layer: Ap layers at the mineral soil surface
  plough_base <- run_base(!is.na(dsg) & grepl("^[0-9]*A[a-z]*p", dsg))
  at_plough <- if (is.na(plough_base)) FALSE else span(plough_base)
  # (c) a self-mulching surface: strong granular structure, or strong angular
  # or subangular blocky structure, of aggregates <= 1 cm. The size is read
  # by the FAO classes of each type: granular up to coarse (5-10 mm), blocky
  # up to fine (5-10 mm).
  grade <- .field_grade_class(h$structure_grade)
  stype <- .field_term(h$structure_type)
  size  <- .structure_size_class(h$structure_size)
  gran  <- !is.na(stype) & grepl("granul", stype)
  blky  <- !is.na(stype) & grepl("blocky|blocos", stype)
  small <- ifelse(is.na(stype) | (!gran & !blky), NA,
                  ifelse(is.na(size), NA, ifelse(gran, size <= 4, size <= 2)))
  mulch <- .and3v(.and3v(ifelse(is.na(grade), NA, grade >= 3),
                         ifelse(is.na(stype), NA, gran | blky)), small)
  mulch_base <- run_base(mulch)
  below_mulch <- if (!is.na(mulch_base)) span(mulch_base)
                 else if (length(surf) && is.na(mulch[surf[1L]]))
                   .and3(NA, span(h$bottom_cm[surf[1L]]))
                 else FALSE
  # (a), and (d) read as (a)
  at_surface <- span(ms)
  list(passed = .any3(c(at_surface, at_plough, below_mulch)),
       starts = list(mineral_surface = at_surface,
                     plough_layer_base = at_plough,
                     below_self_mulching_surface = below_mulch),
       plough_base_cm = plough_base, self_mulching_base_cm = mulch_base,
       crack_status = cr)
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
#' @section v0.9.85 buried-exclusion fix:
#' WRB 2022 Ch 4 p 104 specifies the Andosol exclusion list (argic /
#' ferralic / petroplinthic / pisoplinthic / plinthic / spodic) as
#' "<= 100 cm \emph{unless buried below 50 cm}". The earlier
#' implementation excluded an Andosol whenever any of those
#' diagnostics passed anywhere in the profile, including on layers
#' starting deeper than 50 cm -- which mis-fires on AfSP Andosol
#' references like \code{CM W3_0047}, where an argic layer at
#' 56-72 cm wrongly excluded the andic surface stack. v0.9.85
#' restricts the exclusion check to layers starting <= 50 cm:
#' a buried argic / ferralic / plinthic / spodic at deeper levels no
#' longer disqualifies the surface andic stack from Andosol.
#'
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the v0.9.85 buried-exclusion tolerance is
#' switched off: \emph{any} argic / ferralic / plinthic / spodic horizon
#' anywhere in the profile excludes the Andosol, regardless of depth.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param buried_below_cm Numeric: layers of the exclusion
#'        diagnostics whose top_cm \\>= this depth are treated as
#'        buried and do NOT exclude the Andosol (default 50, per WRB
#'        2022 Ch 4 p 104).
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} disables the buried-exclusion tolerance.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
andosol <- function(pedon, min_thickness = 30, max_top_cm = 25,
                       buried_below_cm = 50, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  if (strict) buried_below_cm <- Inf
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

  # v0.9.85: exclusion-list refinement. The diagnostic is only
  # disqualifying if it ALSO has at least one layer starting in
  # the upper `buried_below_cm` (default 50 cm). When all of its
  # passing layers lie deeper than 50 cm the diagnostic is
  # treated as "buried" and does NOT exclude the Andosol, per
  # WRB 2022 Ch 4 p 104.
  exclusions <- list(
    argic        = argic(pedon),
    ferralic     = ferralic(pedon),
    plinthic     = plinthic(pedon),
    spodic       = spodic(pedon)
  )
  exclusion_buried <- vapply(exclusions, function(d) {
    if (!isTRUE(d$passed) || length(d$layers) == 0L) return(FALSE)
    tops <- h$top_cm[d$layers]
    if (all(is.na(tops))) return(FALSE)
    # Buried: every passing layer starts >= buried_below_cm.
    all(!is.na(tops) & tops >= buried_below_cm)
  }, logical(1))
  exclusion_active <- vapply(exclusions, function(d) isTRUE(d$passed),
                                logical(1)) & !exclusion_buried
  any_excl <- any(exclusion_active)
  passed <- isTRUE(thickness_ok) && !any_excl
  DiagnosticResult$new(
    name = "andosol",
    passed = passed,
    layers = starts_ok,
    evidence = list(
      andic                = ap,
      vitric               = vp,
      combined_thickness_cm = combined_thickness,
      strict_mode          = strict,
      exclusion_failed     = exclusions,
      exclusion_buried     = as.list(exclusion_buried),
      exclusion_active     = as.list(exclusion_active)
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
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the path-1 gleyic+reducing layer must start
#' within the upper 25 cm (instead of 40 cm), and the path-3
#' designation-only fallback (a \dQuote{W} / aquic marker) is disabled:
#' strict mode requires measured gleyic and reducing evidence.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} tightens path 1 and disables path 3.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
gleysol <- function(pedon, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  path1_max_top <- if (strict) 25 else 40
  gp <- gleyic_properties(pedon)
  rc <- reducing_conditions(pedon)
  h <- pedon$horizons
  shared <- intersect(gp$layers %||% integer(0),
                       rc$layers %||% integer(0))
  # Path 1: any qualifying layer >= 25 cm thick starting <= path1_max_top.
  path1_layers <- shared[
    !is.na(h$top_cm[shared]) & h$top_cm[shared] <= path1_max_top &
    !is.na(h$bottom_cm[shared]) &
    (h$bottom_cm[shared] - h$top_cm[shared]) >= 25
  ]
  path1_ok <- length(path1_layers) > 0L
  # Path 3: permanent saturation -- detect via designation 'W' or
  # rock_origin == 'fluviatile' on a layer starting <= 40 cm. Disabled
  # in strict mode (designation-only evidence is not accepted).
  shallow <- which(!is.na(h$top_cm) & h$top_cm <= 40)
  path3_ok <- !strict && length(shallow) > 0L &&
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
      path3_ok              = path3_ok,
      strict_mode           = strict
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
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the \code{planic_features} fallback path is
#' disabled. Strict mode requires the canonical evidence -- an abrupt
#' textural difference \emph{plus} measured stagnic and reducing
#' conditions in the bracketing layer -- and will not accept the
#' simpler clay-doubling proxy on its own.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} disables the planic-features fallback.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
planosol <- function(pedon, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
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
  fallback_ok <- !strict && isTRUE(pf$passed)
  passed <- length(ok_layers) > 0L || isTRUE(fallback_ok)
  layers_out <- if (length(ok_layers) > 0L) ok_layers else (pf$layers %||% integer(0))
  DiagnosticResult$new(
    name = "planosol",
    passed = passed,
    layers = layers_out,
    evidence = list(
      abrupt_textural_difference = atd,
      stagnic_properties         = sp,
      reducing_conditions        = rc,
      planic_features_fallback   = pf,
      fallback_used              = isTRUE(fallback_ok) && length(ok_layers) == 0L,
      strict_mode                = strict
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
#' @section Tier-3 strict mode (v0.9.98):
#' When an argic horizon sits above the ferralic, the default gate
#' keeps the profile as a Ferralsol if \emph{any one} of the three
#' exception paths (WDC \\< 10\%, DeltapH \\>= 0, SOC \\>= 1.4\%) holds.
#' With \code{strict = TRUE} the gate requires \emph{at least two} of
#' the three -- a single weak indicator no longer rescues a profile
#' with a translocated-clay argic from being keyed out of Ferralsols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} requires two of the three argic exception paths.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
ferralsol <- function(pedon, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  fr <- ferralic(pedon)
  if (!isTRUE(fr$passed)) {
    return(DiagnosticResult$new(
      name = "ferralsol",
      passed = if (is.na(fr$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(ferralic = fr, strict_mode = strict),
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
      evidence = list(ferralic = fr, argic = ar, strict_mode = strict),
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
                       argic_above_ferralic = "none",
                       strict_mode = strict),
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
  n_paths <- sum(unlist(paths))
  min_paths <- if (strict) 2L else 1L
  exception_met <- n_paths >= min_paths
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
      exception_paths_met   = n_paths,
      exception_paths_required = min_paths,
      strict_mode           = strict,
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
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the base-saturation floor above the
#' carbonate-bearing layer is raised from 50\% to 80\%, in line with
#' the very high base status expected of a textbook Chernozem.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} raises the base-saturation floor to 80\%.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
chernozem_strict <- function(pedon, min_bs = 50, max_top_cm = 50,
                             strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  if (strict) min_bs <- max(min_bs, 80)
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
      bs_throughout_ok       = bs_ok,
      bs_threshold_pct       = min_bs,
      strict_mode            = strict
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
#' @section Tier-3 strict mode (v0.9.98):
#' With \code{strict = TRUE} the base-saturation floor above the
#' carbonate-bearing layer is raised from 50\% to 75\%. The 70 cm
#' carbonate-depth window is left unchanged.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param strict Logical or \code{NULL}. When \code{NULL} (default) it
#'        resolves via \code{getOption("soilKey.rsg_strict", FALSE)}.
#'        \code{TRUE} raises the base-saturation floor to 75\%.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
kastanozem_strict <- function(pedon, min_bs = 50, max_top_cm = 70,
                              strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  if (strict) min_bs <- max(min_bs, 75)
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
      bs_throughout_ok        = bs_ok,
      bs_threshold_pct        = min_bs,
      strict_mode             = strict
    ),
    missing  = m$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Kastanozems (p. 112)"
  )
}
