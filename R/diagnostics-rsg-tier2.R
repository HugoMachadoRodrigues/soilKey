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
#' @section v0.9.313 (WRB 2022 Ch 4, p 104, as worded):
#' The combined thickness is that of every andic or vitric layer within
#' 100 cm of the soil surface (clipped at 100 cm), provided the uppermost
#' starts \\<= 25 cm; until v0.9.312 only layers starting \\<= 25 cm were
#' added up. Item 1b applies: where a limiting layer starts \\> 25 and
#' \\<= 50 cm, andic or vitric layers covering \\>= 60\% of the soil above
#' it qualify. Petroplinthic and pisoplinthic horizons join the
#' exclusions. An argic horizon read from a clay increase alone (no clay
#' films recorded) whose layers all have andic properties does not
#' exclude (Hugo, N4: the clay of andic material does not disperse fully).
#' Layers whose andic or vitric status is not recorded leave the gate
#' undecided (\code{NA}) where they could change it.
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
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Andosols (p. 104)"
  ap <- andic_properties(pedon)
  vp <- vitric_properties(pedon)
  h <- pedon$horizons
  # Per layer: andic or vitric (TRUE), neither (FALSE), not known (NA). The
  # layers andic_properties() and vitric_properties() pass are TRUE.
  st_an <- ap$evidence$layer_status %||% rep(NA, nrow(h))
  st_vi <- .vitric_layer_status(h)
  status <- ifelse(st_an %in% TRUE | st_vi %in% TRUE, TRUE,
                   ifelse(st_an %in% FALSE & st_vi %in% FALSE, FALSE, NA))
  status[union(ap$layers %||% integer(0), vp$layers %||% integer(0))] <- TRUE
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm)
  if (!any(status %in% TRUE) && all(status[ok] %in% FALSE)) {
    return(DiagnosticResult$new(
      name = "andosol", passed = FALSE, layers = integer(0),
      evidence = list(andic = ap, vitric = vp),
      missing = character(0), reference = ref,
      notes = "No andic or vitric layers"
    ))
  }

  # v0.9.313: WRB 2022 Ch 4, Andosols (p 104), item 1: "one or more layers
  # with andic or vitric properties with a combined thickness of: a. >= 30 cm,
  # within 100 cm of the soil surface and starting <= 25 cm from the soil
  # surface; or b. >= 60% of the entire thickness of the soil, if a limiting
  # layer starts > 25 and <= 50 cm from the soil surface". The thickness is
  # summed over all such layers within 100 cm (until v0.9.312 only layers
  # starting <= 25 cm were added up), clipped at 100 cm. The limiting layer
  # (Ch 2.1, rule 8, p 23) is .barrier_top_cm(): continuous rock, a petric
  # horizon or technic hard material.
  surface <- if (any(ok)) min(h$top_cm[ok]) else 0
  top <- h$top_cm - surface; bot <- h$bottom_cm - surface
  thick_in <- function(sel, to) sum(pmax(0, pmin(bot[sel], to) - pmax(top[sel], 0)))
  yes <- ok & status %in% TRUE
  may <- ok & !(status %in% FALSE)
  start_yes <- any(yes & top <= max_top_cm & top < 100)
  start_may <- any(may & top <= max_top_cm & top < 100)
  thk_yes <- thick_in(yes, 100); thk_may <- thick_in(may, 100)
  item1a <- if (start_yes && thk_yes >= min_thickness) TRUE
            else if (!start_may || thk_may < min_thickness) FALSE
            else NA
  lim <- .barrier_top_cm(pedon) - surface
  item1b <- FALSE
  if (!is.na(lim) && lim > 25 && lim <= 50) {
    need <- 0.6 * lim
    item1b <- if (thick_in(yes, lim) >= need) TRUE
              else if (thick_in(may, lim) < need) FALSE else NA
  }
  thickness_ok <- if (isTRUE(item1a) || isTRUE(item1b)) TRUE
                  else if (isFALSE(item1a) && isFALSE(item1b)) FALSE
                  else NA
  andic_layers <- which(yes & top < 100)

  # Item 2: "no argic, ferralic, petroplinthic, pisoplinthic, plinthic or
  # spodic horizon starting <= 100 cm of the soil surface, unless buried
  # deeper than 50 cm from the mineral soil surface". v0.9.85: a diagnostic
  # whose layers all start >= `buried_below_cm` is read as buried. v0.9.313:
  # petroplinthic and pisoplinthic added; an argic horizon read from a clay
  # increase alone (no clay films recorded) whose layers all have andic
  # properties does not exclude (Hugo, N4).
  exclusions <- list(
    argic         = argic(pedon),
    ferralic      = ferralic(pedon),
    petroplinthic = petroplinthic(pedon),
    pisoplinthic  = pisoplinthic(pedon),
    plinthic      = plinthic(pedon),
    spodic        = spodic(pedon)
  )
  argic_andic <- FALSE
  arg <- exclusions$argic
  if (isTRUE(arg$passed) && length(arg$layers)) {
    films <- .clay_films_class(h$clay_films_amount %||% rep(NA_character_, nrow(h)))
    no_films <- all(is.na(films[arg$layers]) | films[arg$layers] == 0L)
    argic_andic <- no_films && all(st_an[arg$layers] %in% TRUE)
  }
  exclusion_buried <- vapply(exclusions, function(d) {
    if (!isTRUE(d$passed) || length(d$layers) == 0L) return(FALSE)
    tops <- h$top_cm[d$layers]
    if (all(is.na(tops))) return(FALSE)
    # Buried: every passing layer starts >= buried_below_cm.
    all(!is.na(tops) & tops >= buried_below_cm)
  }, logical(1))
  exclusion_active <- vapply(exclusions, function(d) isTRUE(d$passed),
                                logical(1)) & !exclusion_buried
  exclusion_active[["argic"]] <- exclusion_active[["argic"]] && !argic_andic
  any_excl <- any(exclusion_active)
  passed <- if (any_excl) FALSE else thickness_ok
  missing <- if (is.na(passed)) unique(c(ap$missing, vp$missing)) else character(0)
  DiagnosticResult$new(
    name = "andosol",
    passed = passed,
    layers = andic_layers,
    evidence = list(
      andic                 = ap,
      vitric                = vp,
      layer_status          = status,
      combined_thickness_cm = thk_yes,
      possible_thickness_cm = thk_may,
      item1a                = item1a,
      item1b                = item1b,
      limiting_layer_top_cm = if (is.na(lim)) NA_real_ else lim,
      strict_mode           = strict,
      exclusion_failed      = exclusions,
      exclusion_buried      = as.list(exclusion_buried),
      argic_of_andic_layers = argic_andic,
      exclusion_active      = as.list(exclusion_active)
    ),
    missing  = missing,
    reference = ref
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
#' Path 1 reads the layers of \code{\link{gleyic_properties}} (since
#' v0.9.312: the matrix colour, the layer underneath and the site's
#' groundwater, without the old decay-with-depth test) and of
#' \code{\link{reducing_conditions}}; since v0.9.312 the 25 cm may be made
#' of adjoining sublayers (Bg1 over Bg2), counted from the mineral soil
#' surface, and a layer whose redox reading is undecided leaves the gate
#' \code{NA} when it could complete the 25 cm. Path 3 is read from the
#' designation (W / saturated marker). Path 2 is not implemented.
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
  h <- pedon$horizons
  gp <- gleyic_properties(pedon, max_top_cm = Inf)
  rc <- reducing_conditions(pedon)
  z  <- gp$evidence$redox_layers
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  # Path 1 (Ch 4, p. 103): one layer, >= 25 cm thick, starting <= 40 cm from
  # the mineral soil surface, gleyic throughout with reducing conditions in
  # every sublayer: adjoining sublayers count together (v0.9.312).
  gl <- z$gleyic
  gl[gp$layers] <- TRUE                  # opt-in designation inference
  ok <- .and3v(gl, z$reducing)
  run <- .adjoining_runs(h, ok, min_cm = 25, max_top_cm = ms + path1_max_top)
  path1_ok <- run$passed
  path1_layers <- if (isTRUE(path1_ok)) run$layers else integer(0)
  # Path 3: permanent saturation -- detect via designation 'W' or
  # rock_origin == 'fluviatile' on a layer starting <= 40 cm. Disabled
  # in strict mode (designation-only evidence is not accepted).
  shallow <- which(!is.na(h$top_cm) & h$top_cm <= 40)
  path3_ok <- !strict && length(shallow) > 0L &&
                any(grepl("^W|aquic|saturated",
                            h$designation[shallow] %||% rep(NA, length(shallow)),
                            ignore.case = TRUE))
  passed <- if (isTRUE(path1_ok) || isTRUE(path3_ok)) TRUE
            else if (is.na(path1_ok)) NA else FALSE
  DiagnosticResult$new(
    name = "gleysol",
    passed = passed,
    layers = path1_layers,
    evidence = list(
      gleyic_properties     = gp,
      reducing_conditions   = rc,
      path1_layers          = path1_layers,
      path1_undecided       = if (is.na(path1_ok)) run$layers else integer(0),
      path1_ok              = path1_ok,
      path3_ok              = path3_ok,
      strict_mode           = strict
    ),
    missing  = if (isTRUE(passed)) character(0) else unique(c(gp$missing, rc$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Gleysols (p. 103)"
  )
}


#' Plinthosol RSG gate (WRB 2022 Ch 4, p 106)
#'
#' WRB 2022: a plinthic, pisoplinthic or petroplinthic horizon starting
#' \\<= 100 cm from the mineral soil surface. The horizons are those of
#' \code{\link{plinthic}}, \code{\link{pisoplinthic}} and
#' \code{\link{petroplinthic}}; a horizon that cannot be decided and could
#' start within 100 cm leaves the gate \code{NA}. Until v0.9.311 the gate
#' was the plinthic horizon alone, at any depth.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
plinthosol <- function(pedon) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  d <- list(plinthic = plinthic(pedon), pisoplinthic = pisoplinthic(pedon),
            petroplinthic = petroplinthic(pedon))
  start <- function(r) {
    lay <- if (isTRUE(r$passed)) r$layers else r$evidence$undecided_layers %||% integer(0)
    if (length(lay)) min(h$top_cm[lay], na.rm = TRUE) else NA_real_
  }
  tops <- vapply(d, start, 0)
  sure <- vapply(d, function(r) isTRUE(r$passed), NA) & !is.na(tops) & tops <= ms + 100
  maybe <- vapply(d, function(r) is.na(r$passed), NA) &
    (is.na(tops) | tops <= ms + 100)
  passed <- if (any(sure)) TRUE else if (any(maybe)) NA else FALSE
  DiagnosticResult$new(
    name = "plinthosol",
    passed = passed,
    layers = if (isTRUE(passed)) sort(unique(unlist(lapply(d[sure], `[[`, "layers")))) else integer(0),
    evidence = c(d, list(starts_cm = tops, mineral_surface_cm = ms)),
    missing = if (isTRUE(passed)) character(0) else unique(unlist(lapply(d[maybe], `[[`, "missing"))),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Plinthosols (p. 106)"
  )
}


#' Planosol RSG gate (WRB 2022 Ch 4, p 107)
#'
#' WRB 2022: an abrupt textural difference \\<= 75 cm from the mineral soil
#' surface and, within 5 cm directly above or below it, (1) stagnic
#' properties in which the area of reductimorphic plus oximorphic features
#' is \\>= 50\% and (2) reducing conditions in some parts of the volume with
#' the reductimorphic features.
#'
#' The two layers that meet at the abrupt textural difference stand for the
#' 5 cm above and below it. Stagnic properties are those of
#' \code{\link{stagnic_properties}}; a claric layer directly above the
#' abrupt textural difference is stagnic (Ch 3.2.14, criterion 4) and its
#' whole area counts as reductimorphic. Since v0.9.312 the abrupt textural
#' difference has its transition criterion (\code{boundary_distinctness}),
#' the depth limit of 75 cm is applied, and the v0.2 \code{planic_features}
#' fallback (clay doubling at an abrupt boundary, with no redox evidence),
#' which is not in WRB 2022, is gone.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param strict Logical or \code{NULL}; recorded in the evidence. Since
#'        v0.9.312 it changes nothing (the fallback it disabled is gone).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
planosol <- function(pedon, strict = NULL) {
  if (is.null(strict)) strict <- isTRUE(getOption("soilKey.rsg_strict", FALSE))
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Planosols (p. 107)"
  h <- pedon$horizons
  atd <- abrupt_textural_difference(pedon)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  pairs <- Filter(function(p) !isFALSE(p$passed) && !is.na(p$top_cm) &&
                    p$top_cm - ms <= 75,
                  atd$evidence$layer_pairs %||% list())
  if (!length(pairs)) {
    return(DiagnosticResult$new(
      name = "planosol",
      passed = if (is.na(atd$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(abrupt_textural_difference = atd, strict_mode = strict),
      missing = atd$missing %||% character(0),
      reference = ref,
      notes = "No abrupt textural difference within 75 cm of the mineral soil surface"
    ))
  }
  z <- .wrb_redox_layers(pedon)
  # one of the two layers at the abrupt textural difference: stagnic, with
  # >= 50% reductimorphic plus oximorphic features, and reducing conditions
  side_ok <- function(k) .and3(z$stagnic[k], z$area[k] >= 50, z$reducing[k])
  verdict <- vapply(pairs, function(p) {
    sides <- vapply(p$between, side_ok, NA)
    near <- if (any(sides %in% TRUE)) TRUE else if (any(is.na(sides))) NA else FALSE
    .and3(p$passed, near)
  }, NA)
  hit <- which(verdict %in% TRUE)
  passed <- if (length(hit)) TRUE else if (anyNA(verdict)) NA else FALSE
  layers <- if (length(hit)) sort(unique(unlist(lapply(pairs[hit], `[[`, "between"))))
            else integer(0)
  DiagnosticResult$new(
    name = "planosol",
    passed = passed,
    layers = layers,
    evidence = list(
      abrupt_textural_difference = atd,
      pairs_within_75_cm         = pairs,
      redox_layers               = z,
      strict_mode                = strict
    ),
    missing  = if (isTRUE(passed)) character(0)
               else unique(c(atd$missing, .redox_missing(z, unique(unlist(lapply(pairs, `[[`, "between")))))),
    reference = ref
  )
}


#' Stagnosol RSG gate (WRB 2022 Ch 4, p 108)
#'
#' WRB 2022: (1) stagnic properties in which the area of reductimorphic plus
#' oximorphic features is \\>= one third (weighted average) of the area from
#' the mineral soil surface to 60 cm or to continuous rock, whichever is
#' shallower; and (2) reducing conditions for some time in some parts of the
#' volume with the reductimorphic features within that depth.
#'
#' The layers and their areas are those of the redox reading behind
#' \code{\link{stagnic_properties}}: a mottled stagnic layer counts with its
#' mottles, or wholly when its matrix is grey (moist value 4 or more, chroma
#' 2 or less), a claric layer of criteria 4-5 wholly. Undecided layers (and a
#' part of the 60 cm not described) count towards what the area could be, so
#' that the gate is \code{NA} when they could reach one third. Until v0.9.311
#' the gate was \code{stagnic_properties()}: one layer with 5\% redox features
#' that decayed with depth.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
stagnosol <- function(pedon) {
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Stagnosols (p. 108)"
  h <- pedon$horizons
  z <- .wrb_redox_layers(pedon)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- ms + 60
  rock_top <- suppressWarnings(min(h$top_cm[z$rock], na.rm = TRUE))
  if (is.finite(rock_top)) lim <- min(lim, rock_top)
  span <- lim - ms
  if (span <= 0) {
    return(DiagnosticResult$new(name = "stagnosol", passed = FALSE,
      layers = integer(0), evidence = list(redox_layers = z), missing = character(0),
      reference = ref, notes = "Continuous rock at the mineral soil surface"))
  }
  w <- pmax(0, pmin(h$bottom_cm, lim) - pmax(h$top_cm, ms))
  w[is.na(w)] <- 0
  low  <- ifelse(z$stagnic %in% TRUE, z$area, 0)
  high <- ifelse(z$stagnic %in% FALSE, 0, z$area)
  uncovered <- max(0, span - sum(w))
  denom <- max(span, sum(w))
  frac_low  <- sum(w * low) / 100 / denom
  frac_high <- (sum(w * high) / 100 + uncovered) / denom
  area_ok <- if (frac_low >= 1/3) TRUE else if (frac_high >= 1/3) NA else FALSE
  inside <- w > 0
  red_ok <- .and3v(z$stagnic, z$reducing)[inside]
  reducing_ok <- if (any(red_ok %in% TRUE)) TRUE
                 else if (anyNA(red_ok) || uncovered > 0) NA else FALSE
  passed <- .and3(area_ok, reducing_ok)
  DiagnosticResult$new(
    name = "stagnosol",
    passed = passed,
    layers = if (isTRUE(passed)) which(inside & z$stagnic %in% TRUE) else integer(0),
    evidence = list(redox_layers = z, depth_cm = c(from = ms, to = lim),
                    area_fraction = c(low = frac_low, high = frac_high),
                    area_ok = area_ok, reducing_ok = reducing_ok),
    missing = if (isTRUE(passed)) character(0) else .redox_missing(z, which(inside & is.na(z$stagnic))),
    reference = ref
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
#' OR a calcic horizon AND base saturation \\>= 50\% from the mineral
#' soil surface to the protocalcic / calcic layer throughout. Protocalcic
#' properties are visible secondary carbonates
#' (\code{\link{protocalcic_properties}}); where BS is not recorded it is
#' read from pH (WRB 2022 Annex 2, Table 9.2).
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
#' @param min_bs Minimum base saturation \% above the carbonate layer
#'        (default 50).
#' @param max_top_cm Maximum depth of the carbonate layer below the lower
#'        limit of the mollic horizon, cm (default 50).
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
  # v0.9.309: WRB 2022 Ch 4, Chernozems (p. 111), criteria 2-3: the layer
  # with protocalcic properties (>= 5 cm) or the calcic horizon starts <= 50
  # cm below the lower limit of the mollic horizon (which may extend below
  # the chernic, footnote 4) and above any petrocalcic horizon; BS >= 50%
  # from the mineral soil surface to it, throughout (pH where BS is not
  # recorded, footnote 5 and Annex 2 Table 9.2). A carbonate layer at the
  # mineral soil surface leaves nothing above it to test.
  h <- pedon$horizons
  mol <- mollic(pedon)
  mol_stack <- if (isTRUE(mol$passed))
                 mol$evidence$thickness$details$layers %||% integer(0)
               else integer(0)
  mollic_bottom <- max(h$bottom_cm[c(ch$layers, mol_stack)], na.rm = TRUE)
  g <- .wrb_carbonate_bs_gate(pedon, max_top_cm = mollic_bottom + max_top_cm,
                              min_bs = min_bs)
  DiagnosticResult$new(
    name = "chernozem_strict",
    passed = g$passed,
    layers = ch$layers,
    evidence = list(
      chernic                = ch,
      mollic_lower_limit_cm  = mollic_bottom,
      protocalcic_properties = g$protocalcic,
      calcic                 = g$calcic,
      carbonate_layers       = g$candidates,
      carbonate_layer        = g$layer,
      bs_throughout_ok       = g$bs_above$passed %||% NA,
      bs_above               = g$bs_above,
      petrocalcic_top_cm     = g$petrocalcic_top_cm,
      bs_threshold_pct       = min_bs,
      strict_mode            = strict
    ),
    missing  = g$missing,
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
#' @param min_bs Minimum base saturation \% above the carbonate layer
#'        (default 50).
#' @param max_top_cm Maximum depth of the carbonate layer below the mineral
#'        soil surface, cm (default 70).
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
  # v0.9.309: WRB 2022 Ch 4, Kastanozems (p. 112), criteria 2-3: the layer
  # with protocalcic properties (>= 5 cm) or the calcic horizon starts <= 70
  # cm from the mineral soil surface and above any petrocalcic horizon; BS
  # >= 50% from the mineral soil surface to it, throughout (pH where BS is
  # not recorded, footnote 6 and Annex 2 Table 9.2). A carbonate layer at the
  # mineral soil surface leaves nothing above it to test.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  g <- .wrb_carbonate_bs_gate(pedon, max_top_cm = ms + max_top_cm,
                              min_bs = min_bs)
  DiagnosticResult$new(
    name = "kastanozem_strict",
    passed = g$passed,
    layers = m$layers,
    evidence = list(
      mollic                  = m,
      protocalcic_properties  = g$protocalcic,
      calcic                  = g$calcic,
      carbonate_layers        = g$candidates,
      carbonate_layer         = g$layer,
      bs_throughout_ok        = g$bs_above$passed %||% NA,
      bs_above                = g$bs_above,
      petrocalcic_top_cm      = g$petrocalcic_top_cm,
      bs_threshold_pct        = min_bs,
      strict_mode             = strict
    ),
    missing  = g$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 4, Kastanozems (p. 112)"
  )
}
