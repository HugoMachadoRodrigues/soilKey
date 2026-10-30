# ============================================================================
# v0.3.3 -- WRB 2022 Ch 3.1 horizon diagnostics not previously implemented.
#
# Each function returns a DiagnosticResult and follows the same pattern as
# the v0.1-v0.3 horizons (R/diagnostics-horizons-wrb.R). Limitations
# specific to v0.3.3 are noted inline; most stem from the canonical
# horizon schema not yet carrying the full mineralogical / micromorpho-
# logical attributes that WRB defines in some criteria.
# ============================================================================


# ---- albic horizon (WRB 2022 Ch 3.1) ---------------------------------------

#' Albic horizon (WRB 2022)
#'
#' A bleached eluvial horizon -- claric material that has lost iron oxides
#' and/or organic matter due to clay migration, podzolization, or redox
#' under stagnant water. Diagnostic for parts of Podzols, Retisols and
#' Planosols qualifiers.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 1, per WRB
#'        2022). The albic horizon has no canonical thickness gate; we
#'        keep a token min so that fully-NA layers don't pass.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details Sub-tests:
#' \itemize{
#'   \item \code{test_claric_munsell} -- Munsell criteria of claric
#'         material (Ch 3.3.4).
#' }
#' Designation pattern \code{E} or \code{Eg} also serves as positive
#' evidence when Munsell columns are missing (proxy path).
#'
#' @references IUSS Working Group WRB (2022), Ch 3.1 -- Albic horizon.
#' @export
albic <- function(pedon, min_thickness = 1) {
  h <- pedon$horizons
  tests <- list()
  tests$claric <- test_claric_munsell(h)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$claric$layers)
  # Designation proxy as fallback.
  desg <- test_pattern_match(h, "designation", "^E[bg]?$|^Eg")
  if (!isTRUE(tests$claric$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "albic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Albic horizon",
    notes     = "v0.3.3: claric Munsell + thickness; designation E* as fallback"
  )
}


# ---- ferric horizon -------------------------------------------------------

#' Ferric horizon (WRB 2022)
#'
#' v0.9.309: the criteria of WRB 2022 Ch 3.1.11 (p. 45). The horizon consists
#' of mineral material, is >= 15 cm thick and is not part of a petroplinthic,
#' pisoplinthic or plinthic horizon, and has, in one or more subhorizons,
#' (a) >= 15\% of its area as coarse (> 20 mm) masses inside aggregates that
#' are black or have a hue redder than 7.5YR and a chroma >= 5, moist, or
#' (b) >= 5\% concretions or nodules, at least weakly cemented, reddish or
#' blackish, > 2 mm (\code{ferric_nodules_pct}). Until v0.9.308 soilKey read
#' it from Fe-dithionite >= 5\%, which is not a criterion of the text: a red
#' Ferralsol with 6\% Fe-dithionite and no nodules became Ferric (ISRIC's
#' KE009). soilKey records no size for mottles, so criterion (a) is met only
#' by nodules; mottles of the right colours leave it undecided, and a layer
#' with no mottles and no nodules recorded as such does not meet it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 15).
#' @param min_nodules_pct Minimum nodules, \% (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.11, Ferric horizon.
#' @export
ferric <- function(pedon, min_thickness = 15, min_nodules_pct = 5) {
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  col <- function(v, na) if (!is.null(h[[v]])) h[[v]] else rep(na, n)
  nod <- col("ferric_nodules_pct", NA_real_)
  mot <- col("redoximorphic_features_pct", NA_real_)
  mcol <- col("mottle_colors", NA_character_)
  oc <- col("oc_pct", NA_real_)
  # a mottle colour that could be a ferric mass: black, or hue redder than
  # 7.5YR (10R to 5YR, the R hues) with chroma >= 5
  red_or_black <- function(x) {
    if (is.na(x) || !nzchar(x)) return(NA)
    parts <- trimws(strsplit(x, ";")[[1]])
    any(vapply(parts, function(m) {
      if (grepl("^N", m)) return(TRUE)
      hue <- sub(" .*", "", m); vc <- sub("^[^ ]+ ", "", m)
      chroma <- suppressWarnings(as.numeric(sub(".*/", "", vc)))
      value <- suppressWarnings(as.numeric(sub("/.*", "", vc)))
      if (!is.na(value) && value <= 2.5) return(TRUE)
      redder <- grepl("R$", hue) || grepl("^(2\\.5|5)YR$", hue)
      redder && !is.na(chroma) && chroma >= 5
    }, NA))
  }
  status <- rep(NA, n)
  for (i in seq_len(n)) {
    if (!is.na(oc[i]) && oc[i] >= 20) { status[i] <- FALSE; next }   # not mineral material
    if (!is.na(nod[i]) && nod[i] >= min_nodules_pct) { status[i] <- TRUE; next }
    masses_possible <- if (is.na(mot[i])) NA
                       else if (mot[i] <= 0) FALSE
                       else { rb <- red_or_black(mcol[i]); if (isFALSE(rb)) FALSE else NA }
    nod_no <- !is.na(nod[i]) && nod[i] < min_nodules_pct
    status[i] <- if (nod_no && isFALSE(masses_possible)) FALSE else NA
  }
  # not part of a petroplinthic, pisoplinthic or plinthic horizon
  pl <- unique(unlist(lapply(list(tryCatch(plinthic(pedon), error = function(e) NULL),
                                  tryCatch(pisoplinthic(pedon), error = function(e) NULL),
                                  tryCatch(petroplinthic(pedon), error = function(e) NULL)),
                             function(d) if (!is.null(d) && isTRUE(d$passed)) d$layers else integer(0))))
  status[pl] <- FALSE
  yes <- which(status %in% TRUE); maybe <- which(is.na(status))
  thick <- function(idx) {
    if (!length(idx)) return(0)
    idx <- idx[order(h$top_cm[idx])]; best <- 0; run <- 0; last <- NA_real_
    for (i in idx) {
      if (!is.na(last) && !is.na(h$top_cm[i]) && abs(h$top_cm[i] - last) < 1e-6) run <- run + (h$bottom_cm[i] - h$top_cm[i])
      else run <- h$bottom_cm[i] - h$top_cm[i]
      last <- h$bottom_cm[i]; best <- max(best, run, na.rm = TRUE)
    }
    best
  }
  t_yes <- thick(yes); t_any <- thick(sort(c(yes, maybe)))
  passed <- if (t_yes >= min_thickness) TRUE else if (t_any >= min_thickness) NA else FALSE
  miss <- if (is.na(passed)) c("ferric_nodules_pct", "redoximorphic_features_pct") else character(0)
  DiagnosticResult$new(
    name      = "ferric",
    passed    = passed,
    layers    = if (isTRUE(passed)) yes else integer(0),
    evidence  = list(layer_status = status, thickness_cm = t_yes,
                     thickness_possible_cm = t_any),
    missing   = miss,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.11, Ferric horizon (p. 45)"
  )
}


# ---- petric variants (cemented horizons) -----------------------------------

#' Petrocalcic horizon (WRB 2022)
#'
#' A continuously cemented variant of the calcic horizon. Same chemistry
#' (CaCO3 \\>= 15\%) plus moderate-or-greater cementation in at least
#' 50\% of the layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_caco3_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
petrocalcic <- function(pedon, min_thickness = 10, min_caco3_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$caco3      <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$caco3$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "petrocalcic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petrocalcic horizon"
  )
}

#' Petroduric horizon (WRB 2022): cemented duric.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_duripan_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
petroduric <- function(pedon, min_thickness = 10, min_duripan_pct = 10) {
  h <- pedon$horizons
  tests <- list()
  tests$duripan    <- test_duripan_concentration(h, min_pct = min_duripan_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$duripan$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petroduric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petroduric horizon"
  )
}

#' Petrogypsic horizon (WRB 2022): cemented gypsic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_gypsum_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
petrogypsic <- function(pedon, min_thickness = 10, min_gypsum_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$gypsum     <- test_caso4_concentration(h, min_pct = min_gypsum_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$gypsum$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petrogypsic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petrogypsic horizon"
  )
}

#' Per-layer reading of the petroplinthic horizon (WRB 2022 3.1.27, p. 59):
#' Fe-cemented (criteria 1-3: an iron pan, pan_type ferricrete or ironpan,
#' or plinthite \\>= \code{min_plinthite_pct}) and at least strongly cemented
#' (criterion 4), \\>= \code{min_thickness} cm of adjoining layers
#' (criterion 7). \code{layers_true} are the layers of a sure horizon.
#' @noRd
.petroplinthic_run <- function(h, min_thickness = 10, min_plinthite_pct = 15) {
  n <- nrow(h)
  pan <- tolower(as.character(h$pan_type %||% rep(NA_character_, n)))
  pl  <- suppressWarnings(as.numeric(h$plinthite_pct %||% rep(NA_real_, n)))
  cem <- tolower(as.character(h$cementation_class %||% rep(NA_character_, n)))
  fe  <- ifelse(pan %in% c("ferricrete", "ironpan"), TRUE, pl >= min_plinthite_pct)
  strong <- ifelse(is.na(cem), NA, cem %in% c("strongly", "indurated"))
  ok <- .and3v(fe, strong)
  run <- .adjoining_runs(h, ok, min_cm = min_thickness)
  c(run, list(layers_true = if (isTRUE(run$passed)) run$layers else integer(0),
              per_layer = ok))
}

#' Per-layer reading of the pisoplinthic horizon (WRB 2022 3.1.28, p. 60):
#' \\>= \code{min_nodules_pct} \% of the volume Fe/Mn concretions or nodules
#' over 2 mm and at least moderately cemented (\code{ferric_nodules_pct}),
#' not part of a petroplinthic horizon, \\>= \code{min_thickness} cm of
#' adjoining layers.
#' @noRd
.pisoplinthic_run <- function(h, min_thickness = 15, min_nodules_pct = 40) {
  n <- nrow(h)
  nod <- suppressWarnings(as.numeric(h$ferric_nodules_pct %||% rep(NA_real_, n)))
  ok <- nod >= min_nodules_pct
  ok[.petroplinthic_run(h)$layers_true] <- FALSE
  run <- .adjoining_runs(h, ok, min_cm = min_thickness)
  c(run, list(layers_true = if (isTRUE(run$passed)) run$layers else integer(0),
              per_layer = ok))
}

#' Petroplinthic horizon (WRB 2022)
#'
#' Continuous or fractured layer cemented by Fe (WRB 2022 Ch 3.1.27, p. 59).
#' Since v0.9.312 a layer qualifies when its Fe cement is recorded (an iron
#' pan, \code{pan_type} ferricrete or ironpan, or plinthite \\>=
#' \code{min_plinthite_pct}) and its cementation class is at least strongly
#' cemented (criterion 4; until v0.9.311 moderately was enough), over
#' \code{min_thickness} cm of adjoining layers (criterion 7). A layer whose
#' Fe cement or cementation is not recorded is undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_plinthite_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
petroplinthic <- function(pedon, min_thickness = 10, min_plinthite_pct = 15) {
  h <- pedon$horizons
  r <- .petroplinthic_run(h, min_thickness, min_plinthite_pct)
  DiagnosticResult$new(
    name = "petroplinthic", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = list(per_layer = r$per_layer,
                    undecided_layers = if (is.na(r$passed)) r$layers else integer(0)),
    missing = if (isTRUE(r$passed)) character(0)
              else c("pan_type", "plinthite_pct", "cementation_class"),
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.27, Petroplinthic horizon (p. 59)"
  )
}

#' Pisoplinthic horizon (WRB 2022)
#'
#' WRB 2022 Ch 3.1.28 (p. 60): \\>= 40\% of the volume (whole soil)
#' yellowish, reddish or blackish concretions or nodules over 2 mm, at least
#' moderately cemented, not part of a petroplinthic horizon, \\>= 15 cm
#' thick. Since v0.9.312 the concretions are read from
#' \code{ferric_nodules_pct} (Fe/Mn nodules or concretions; the ISIS reader
#' stores the hard ones over 2 mm), adjoining sublayers counting together; a
#' layer where they are not recorded is undecided. Until v0.9.311 the test
#' was plinthite \\>= 15\% with a "pisolitic" structure, which is not the
#' WRB 2022 definition.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_plinthite_pct Not used since v0.9.312; kept so that existing
#'        calls still work.
#' @param min_nodules_pct Minimum volume \% of concretions and nodules
#'        (default 40).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
pisoplinthic <- function(pedon, min_thickness = 15, min_plinthite_pct = 15,
                         min_nodules_pct = 40) {
  h <- pedon$horizons
  r <- .pisoplinthic_run(h, min_thickness, min_nodules_pct)
  DiagnosticResult$new(
    name = "pisoplinthic", passed = r$passed,
    layers = if (isTRUE(r$passed)) r$layers else integer(0),
    evidence = list(per_layer = r$per_layer,
                    undecided_layers = if (is.na(r$passed)) r$layers else integer(0)),
    missing = if (isTRUE(r$passed)) character(0) else "ferric_nodules_pct",
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.28, Pisoplinthic horizon (p. 60)"
  )
}

# ---- vertic horizon (the horizon, not the property) -----------------------

#' Vertic horizon (WRB 2022 Ch 3.1.40)
#'
#' A clay-rich subsurface horizon with slickensides or wedge-shaped
#' aggregates, formed by shrinking and swelling. Used by Vertisols.
#'
#' Since v0.9.311 the default reading (\code{reading = "wrb2022"}) follows
#' the diagnostic criteria of WRB 2022 Ch 3.1.40 (p. 70). Mineral material
#' with:
#' \enumerate{
#'   \item \\>= 30\% clay;
#'   \item wedge-shaped aggregates (a \code{structure_type} that names them)
#'         or slickensides on \\>= 10\% of the aggregate surfaces
#'         (\code{slickensides} "common" or more);
#'   \item shrink-swell cracks: \code{cracks_width_cm} \\>= 0.5, or, where
#'         the width is not recorded, slickensides "common" or more or an
#'         \code{ss} designation as evidence of them;
#'   \item a thickness of \\>= 25 cm, of adjoining layers that meet 1 to 3
#'         taken together.
#' }
#' Each criterion is read per layer as met, not met or unrecorded. The
#' horizon is \code{NA} only when layers that might meet them (a criterion
#' unrecorded) could make up the thickness. Organic layers and continuous
#' rock are not part of it. WRB gives the COLE (\\>= 0.06) as additional
#' information only, so this reading does not use \code{cole_value}.
#'
#' \code{reading = "usda"} keeps the reading that the USDA Vertisols and the
#' SiBCS horizonte vertico are built on, as it was before v0.9.311:
#' slickensides "common" or more and cracks \\>= \code{min_crack_width_cm}
#' in one layer of \code{min_thickness}, OR the KST 13 linear-extensibility
#' alternative: summed \code{cole_value * thickness} over the upper
#' \code{le_max_depth_cm} \\>= \code{min_le_cm} (KST 13ed Ch 16, p. 343).
#'
#' @section v0.9.72 designation morphological inference (opt-in):
#' Field-described Brazilian Vertissolos profiles (e.g.\ the Embrapa
#' Redape curated dataset) encode vertic morphology via a \code{v}
#' master-letter modifier in the horizon designation (\code{Bv},
#' \code{Bvk1}, \code{Cv}, \code{Cvz}) without recording
#' \code{slickensides} class or \code{shrink_swell_cracks_cm} as
#' numeric inputs. With
#' \code{options(soilKey.vertic_designation_inference = TRUE)} the
#' function accepts a layer as vertic when the criteria of the reading
#' fail or are NA AND the layer has \code{clay_pct >=
#' min_clay} AND its designation matches a \code{v} master-letter
#' modifier. Default is \code{FALSE}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Numeric threshold or option (see Details).
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_le_cm Minimum LE sum (cm) for the COLE-based path of
#'        \code{reading = "usda"} (default 6, per KST 13ed Ch 16).
#' @param le_max_depth_cm Depth window (cm) for the COLE-based path of
#'        \code{reading = "usda"} (default 100).
#' @param min_crack_width_cm Minimum shrink-swell crack width (cm).
#'        Defaults to 0.5 (WRB/USDA); the SiBCS
#'        \code{horizonte_vertico} wrapper passes 1.0 per Embrapa
#'        (2018) Cap 2 p.73.
#' @param reading \code{"wrb2022"} (default): the criteria of WRB 2022 Ch
#'        3.1.40. \code{"usda"}: the reading of the USDA Vertisols and the
#'        SiBCS horizonte vertico, with the KST 13 COLE path (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
vertic_horizon <- function(pedon, min_clay = 30, min_thickness = 25,
                              min_le_cm = 6, le_max_depth_cm = 100,
                              min_crack_width_cm = 0.5,
                              reading = c("wrb2022", "usda")) {
  reading <- match.arg(reading)
  if (identical(reading, "usda"))
    return(.vertic_horizon_usda(pedon, min_clay = min_clay,
                                min_thickness = min_thickness,
                                min_le_cm = min_le_cm,
                                le_max_depth_cm = le_max_depth_cm,
                                min_crack_width_cm = min_crack_width_cm))
  .vertic_horizon_wrb(pedon, min_clay = min_clay,
                      min_thickness = min_thickness,
                      min_crack_width_cm = min_crack_width_cm)
}


#' The vertic horizon of WRB 2022 Ch 3.1.40 (p. 70); see vertic_horizon().
#' `max_top_cm` keeps only a horizon that starts at or above that depth: the
#' Vertisols gate asks for one starting <= 100 cm from the mineral soil
#' surface (Ch 4, p. 101), so layers below it cannot leave the gate undecided.
#' @noRd
.vertic_horizon_wrb <- function(pedon, min_clay = 30, min_thickness = 25,
                                min_crack_width_cm = 0.5, max_top_cm = Inf) {
  h <- pedon$horizons
  st <- .vertic_layer_status(h, min_clay = min_clay,
                             min_crack_width_cm = min_crack_width_cm)
  # Criterion 4: >= 25 cm, of the adjoining layers that meet 1 to 3 (a
  # horizon is often described in sub-horizons). TRUE when the layers known
  # to meet them reach it, NA when only the layers that might do so could.
  yes <- .vertic_runs(h, which(st$status %in% TRUE), min_thickness, max_top_cm)
  may <- .vertic_runs(h, which(!st$status %in% FALSE), min_thickness, max_top_cm)
  canonical <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  undecided <- intersect(as.integer(unlist(may)), which(is.na(st$status)))
  missing <- if (is.na(canonical)) unique(c(
    if (anyNA(st$clay[undecided])) "clay_pct",
    if (anyNA(st$features[undecided])) c("slickensides", "structure_type"),
    if (anyNA(st$cracks[undecided])) "cracks_width_cm")) else character(0)

  inf <- .vertic_inference_paths(pedon, h, min_clay = min_clay,
                                 min_thickness = min_thickness,
                                 main_passed = isTRUE(canonical))
  for (k in c("designation", "chroma_clay")) {
    p <- inf[[k]]
    if (isTRUE(p$passed) && is.finite(max_top_cm) &&
          !any(h$top_cm[p$layers] <= max_top_cm, na.rm = TRUE))
      inf[[k]]$passed <- FALSE
  }

  passed <- if (isTRUE(canonical) || isTRUE(inf$designation$passed) ||
                  isTRUE(inf$chroma_clay$passed)) TRUE
            else if (is.na(canonical)) NA else FALSE
  layers <- if (isTRUE(canonical)) sort(as.integer(unlist(yes)))
            else if (isTRUE(inf$designation$passed)) inf$designation$layers
            else if (isTRUE(inf$chroma_clay$passed)) inf$chroma_clay$layers
            else integer(0)
  run_info <- function(r) list(layers = r, top = min(h$top_cm[r]),
                               bottom = max(h$bottom_cm[r]),
                               thickness = max(h$bottom_cm[r]) - min(h$top_cm[r]))
  DiagnosticResult$new(
    name     = "vertic_horizon",
    passed   = passed,
    layers   = layers,
    evidence = list(
      clay_pct          = st$clay,
      slickensides      = st$slickensides,
      wedge_shaped      = st$wedge,
      cracks            = st$cracks,
      layer_status      = st$status,
      thickness         = list(runs = lapply(yes, run_info),
                               possible_runs = lapply(may, run_info),
                               min_thickness = min_thickness,
                               max_top_cm = max_top_cm),
      possible_top_cm   = if (length(may))
                            min(h$top_cm[as.integer(unlist(may))]) else NA_real_,
      designation_inference = inf$designation,
      chroma_clay_inference = inf$chroma_clay),
    missing  = missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3.1.40, Vertic horizon (p. 70)",
                      if (length(inf$inferred_layers) > 0L ||
                            length(inf$chroma_inferred) > 0L)
                        "[v0.9.72/76 morphological inference]" else "")
  )
}


# Per layer, the criteria of the vertic horizon of WRB 2022 Ch 3.1.40 (p. 70),
# each TRUE, FALSE or NA (unrecorded):
#   1. >= 30 % clay;
#   2. wedge-shaped aggregates or slickensides on >= 10 % of the aggregate
#      surfaces ("common" or more). The 20 % by volume of the wedge-shaped
#      aggregates is taken as met where they are the structure recorded;
#   3. shrink-swell cracks (.vertic_crack_status()).
# The horizon consists of mineral material: organic layers (SOC >= 20 %, an O
# or H designation) and continuous rock are not part of it.
.vertic_layer_status <- function(h, min_clay = 30, min_crack_width_cm = 0.5) {
  col <- function(n, na) if (is.null(h[[n]])) rep(na, nrow(h)) else h[[n]]
  dsg   <- as.character(col("designation", NA_character_))
  clay  <- col("clay_pct", NA_real_)
  sl    <- col("slickensides", NA_character_)
  stype <- .field_term(col("structure_type", NA_character_))
  oc    <- col("oc_pct", NA_real_)
  clay_ok <- ifelse(is.na(clay), NA, clay >= min_clay)
  slick   <- ifelse(is.na(sl), NA,
                    .amount_at_least(sl, c("common", "many", "continuous")))
  wedge   <- ifelse(is.na(stype), NA, grepl("wedge|cuneiform", stype))
  features <- .any3v(wedge, slick)
  cracks  <- .vertic_crack_status(h, min_width_cm = min_crack_width_cm)
  status  <- .and3v(.and3v(clay_ok, features), cracks)
  not_mineral <- (!is.na(oc) & oc >= 20) |
                 (!is.na(dsg) & grepl("^[0-9]*[OH]", dsg)) |
                 (!is.na(dsg) & grepl(.WRB_ROCK_DESIGNATION, dsg))
  status[not_mineral] <- FALSE
  list(status = as.logical(status), clay = as.logical(clay_ok),
       slickensides = as.logical(slick), wedge = as.logical(wedge),
       features = as.logical(features), cracks = as.logical(cracks))
}


# Shrink-swell cracks in a layer (WRB 2022 Ch 3.2.12, p. 80: >= 0.5 cm wide
# when the soil is dry), as test_shrink_swell_cracks() reads them: TRUE or
# FALSE by the recorded width; where it is not recorded, TRUE on slickensides
# "common" or more or an ss designation, as evidence of them; NA otherwise.
.vertic_crack_status <- function(h, min_width_cm = 0.5) {
  col <- function(n, na) if (is.null(h[[n]])) rep(na, nrow(h)) else h[[n]]
  w   <- col("cracks_width_cm", NA_real_)
  dsg <- as.character(col("designation", NA_character_))
  sl  <- col("slickensides", NA_character_)
  proxy <- (!is.na(dsg) & grepl("ss|Vss", dsg)) |
           .amount_at_least(sl, c("common", "many", "continuous"))
  as.logical(ifelse(!is.na(w), w >= min_width_cm, ifelse(proxy, TRUE, NA)))
}


# Runs of depth-adjoining layers among `idx` (the top of one at or above the
# deepest bottom of the run so far) that are >= `min_thickness` thick and
# start at or above `max_top_cm`. A list of integer vectors.
.vertic_runs <- function(h, idx, min_thickness, max_top_cm = Inf) {
  idx <- idx[!is.na(h$top_cm[idx]) & !is.na(h$bottom_cm[idx])]
  if (!length(idx)) return(list())
  idx <- idx[order(h$top_cm[idx])]
  runs <- list(); cur <- idx[1L]; end <- h$bottom_cm[idx[1L]]
  for (i in idx[-1L]) {
    if (h$top_cm[i] <= end) {
      cur <- c(cur, i); end <- max(end, h$bottom_cm[i])
    } else {
      runs[[length(runs) + 1L]] <- cur; cur <- i; end <- h$bottom_cm[i]
    }
  }
  runs[[length(runs) + 1L]] <- cur
  Filter(function(r) max(h$bottom_cm[r]) - min(h$top_cm[r]) >= min_thickness &&
                       min(h$top_cm[r]) <= max_top_cm, runs)
}


#' The vertic reading kept for the USDA Vertisols and the SiBCS horizonte
#' vertico (vertic_horizon(reading = "usda")), as it was before v0.9.311.
#' @noRd
.vertic_horizon_usda <- function(pedon, min_clay = 30, min_thickness = 25,
                                 min_le_cm = 6, le_max_depth_cm = 100,
                                 min_crack_width_cm = 0.5) {
  h <- pedon$horizons

  # Path 1: clay >= min_clay + slickensides "common"+ + cracks >=
  # min_crack_width_cm + thickness, in one layer.
  # min_crack_width_cm defaults to 0.5 (WRB/USDA); the SiBCS vertico wrapper
  # (horizonte_vertico) passes 1.0, per Embrapa 2018 Cap 2 p.73 ("fendas ...
  # com pelo menos 1 cm de largura").
  tests <- list()
  tests$clay         <- test_clay_above(h, min_pct = min_clay)
  tests$slickensides <- test_slickensides_present(h,
                                                     levels = c("common", "many",
                                                                "continuous"),
                                                     candidate_layers = tests$clay$layers)
  tests$cracks       <- test_shrink_swell_cracks(h, min_width_cm = min_crack_width_cm,
                                                    candidate_layers = tests$slickensides$layers)
  shared <- intersect(tests$slickensides$layers, tests$cracks$layers)
  tests$thickness    <- test_minimum_thickness(h, min_cm = min_thickness,
                                                  candidate_layers = shared)
  agg_canonical <- aggregate_subtests(tests)

  # Path 2 (v0.9.19): KST 13ed Ch 16 (Vertisols, p 343) accepts
  # linear extensibility (LE) sum >= 6 cm between the surface and
  # 100 cm as an alternative to slickensides + cracks. LE = sum of
  # (cole_value * thickness) over each layer in the upper 100 cm.
  # Used by USDA Vertisols for shrink-swell soils where field morphology
  # was not recorded but COLE was measured (typical of KSSL profiles).
  cole_path <- list()
  if ("cole_value" %in% names(h)) {
    cl <- which(!is.na(h$top_cm) & h$top_cm < le_max_depth_cm)
    if (length(cl) > 0L) {
      thk    <- pmax(h$bottom_cm[cl] - h$top_cm[cl], 0)
      cole_v <- h$cole_value[cl]
      le     <- sum(thk * cole_v, na.rm = TRUE)
      clay_layers <- intersect(cl, tests$clay$layers)
      cole_path <- list(
        passed = if (any(!is.na(cole_v)) && le >= min_le_cm &&
                       length(clay_layers) > 0L) TRUE
                 else if (all(is.na(cole_v))) NA
                 else FALSE,
        layers = clay_layers,
        details = list(le_total_cm = le, threshold_cm = min_le_cm,
                        depth_window_cm = le_max_depth_cm)
      )
    }
  }

  inf <- .vertic_inference_paths(pedon, h, min_clay = min_clay,
                                 min_thickness = min_thickness,
                                 main_passed = isTRUE(agg_canonical$passed) ||
                                                 isTRUE(cole_path$passed))
  v_suffix_path    <- inf$designation
  chroma_clay_path <- inf$chroma_clay

  passed <- isTRUE(agg_canonical$passed) || isTRUE(cole_path$passed) ||
              isTRUE(v_suffix_path$passed) || isTRUE(chroma_clay_path$passed)
  layers <- if (isTRUE(agg_canonical$passed)) agg_canonical$layers
            else if (isTRUE(cole_path$passed)) cole_path$layers
            else if (isTRUE(v_suffix_path$passed)) v_suffix_path$layers
            else if (isTRUE(chroma_clay_path$passed)) chroma_clay_path$layers
            else integer(0)
  missing <- unique(c(agg_canonical$missing,
                       if (length(cole_path) == 0L) "cole_value"))
  if (is.null(missing)) missing <- character(0)

  DiagnosticResult$new(
    name     = "vertic_horizon",
    passed   = if (passed) TRUE
                else if (is.na(agg_canonical$passed) ||
                           (length(cole_path) > 0L && is.na(cole_path$passed))) NA
                else FALSE,
    layers   = layers,
    evidence = c(tests, list(cole_le_path = cole_path,
                                designation_inference = v_suffix_path,
                                chroma_clay_inference = chroma_clay_path)),
    missing  = missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3.1, Vertic horizon;",
                       "KST 13ed Ch 16 LE alternative",
                       if (length(inf$inferred_layers) > 0L ||
                             length(inf$chroma_inferred) > 0L)
                         "[v0.9.72/76 morphological inference]" else "")
  )
}


# The opt-in morphological inference paths of the vertic horizon (both
# readings), tried when `main_passed`, the reading's own criteria, is not
# TRUE. Returns list(designation, chroma_clay, inferred_layers,
# chroma_inferred).
.vertic_inference_paths <- function(pedon, h, min_clay, min_thickness,
                                    main_passed) {
  # v0.9.72 -- designation morphological inference (opt-in).
  # Field-described Brazilian Vertissolos profiles (e.g. the Embrapa
  # Redape curated dataset) encode vertic morphology via a 'v' master-
  # letter modifier in the horizon designation (Bv, Bvk1, Cv, Cvz)
  # without recording slickensides_class or shrink_swell_cracks_cm.
  # This path passes when:
  #   (1) the reading's own criteria fail or are NA, AND
  #   (2) at least one layer has clay >= min_clay AND its designation
  #       matches a 'v' master-letter modifier ([A-Z][A-Za-z]*v).
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.vertic_designation_inference", default = FALSE))
  v_suffix_path <- list(passed = NA, layers = integer(0), source = "off")
  inferred_layers <- integer(0)
  if (designation_inference_enabled && !isTRUE(main_passed)) {
    desig <- if (!is.null(h$designation)) as.character(h$designation)
              else rep(NA_character_, nrow(h))
    has_v_suffix <- !is.na(desig) & grepl("[A-Z][a-z0-9]*v", desig)
    clay_ok      <- !is.na(h$clay_pct) & h$clay_pct >= min_clay
    inferred_mask <- has_v_suffix & clay_ok
    inferred_layers <- which(inferred_mask)
    if (length(inferred_layers) > 0L) {
      # Apply thickness on the contiguous chunk of v-suffixed layers
      thk <- pmax(h$bottom_cm[inferred_layers] - h$top_cm[inferred_layers], 0,
                    na.rm = TRUE)
      if (sum(thk, na.rm = TRUE) >= min_thickness) {
        v_suffix_path <- list(
          passed = TRUE, layers = inferred_layers,
          source = "designation_v_suffix",
          details = list(matched_designations = desig[inferred_mask],
                            total_thickness_cm   = sum(thk, na.rm = TRUE),
                            min_thickness        = min_thickness,
                            min_clay             = min_clay)
        )
      } else {
        v_suffix_path <- list(
          passed = FALSE, layers = inferred_layers,
          source = "designation_v_suffix_thin",
          details = list(matched_designations = desig[inferred_mask],
                            total_thickness_cm   = sum(thk, na.rm = TRUE),
                            min_thickness        = min_thickness)
        )
        inferred_layers <- integer(0)
      }
    }
  }

  # v0.9.76 -- high-clay + low-chroma + carbonate path (opt-in).
  # Field-described USDA Aquerts / Torrerts profiles (KSSL/NASIS)
  # often have very high clay (50-70%) and low Munsell chroma (<= 2)
  # throughout, plus carbonate accumulation (Bk* designation), but
  # neither v-suffix nor recorded slickensides because the lab/NASIS
  # tables don't preserve the field-observed shrink-swell features.
  # This path corroborates vertic morphology from these proxies.
  chroma_clay_path <- list(passed = NA, layers = integer(0), source = "off")
  chroma_inferred <- integer(0)
  chroma_clay_enabled <- isTRUE(
    getOption("soilKey.vertic_chroma_clay_inference", default = FALSE))
  # v0.9.79: mollic-priority intergrade resolution. The chroma+clay
  # PROXY path (v0.9.76 morphological inference) commonly fires on
  # Mollisol Phaeozems / Kastanozems whose surface stack qualifies as
  # mollic AND whose subsoil happens to have high clay + low chroma
  # (typical of dark, well-drained Mollisols). Per WRB 2022 the
  # vertic horizon requires shrink-swell evidence that the chroma+clay
  # proxy alone does not confirm, so when mollic also fires we
  # decline the chroma+clay path and let the WRB key cascade through
  # to the Mollisol section. Canonical (slickensides+cracks) and
  # COLE paths are unaffected -- they win on real Vertisols.
  mollic_competing <- if (chroma_clay_enabled) {
    isTRUE(tryCatch(mollic(pedon)$passed, error = function(e) NULL))
  } else FALSE
  if (chroma_clay_enabled && !isTRUE(main_passed) &&
        !isTRUE(v_suffix_path$passed) &&
        !isTRUE(mollic_competing)) {
    chroma <- if (!is.null(h$munsell_chroma_moist)) h$munsell_chroma_moist
              else rep(NA_real_, nrow(h))
    desig <- if (!is.null(h$designation)) as.character(h$designation)
              else rep(NA_character_, nrow(h))
    high_clay_mask <- !is.na(h$clay_pct) & h$clay_pct >= 50  # very high clay
    low_chroma_mask <- !is.na(chroma) & chroma <= 2
    # Subsoil B horizon (with k for carbonate or g for gleyic)
    subsoil_B <- !is.na(desig) & grepl("^[0-9]?B", desig) &
                   !is.na(h$top_cm) & h$top_cm >= 20
    chroma_inferred_mask <- high_clay_mask & low_chroma_mask & subsoil_B
    chroma_inferred <- which(chroma_inferred_mask)
    if (length(chroma_inferred) > 0L) {
      thk <- pmax(h$bottom_cm[chroma_inferred] - h$top_cm[chroma_inferred],
                    0, na.rm = TRUE)
      if (sum(thk, na.rm = TRUE) >= min_thickness) {
        chroma_clay_path <- list(
          passed = TRUE, layers = chroma_inferred,
          source = "high_clay_low_chroma_subsoil",
          details = list(min_clay_in_path = 50,
                            max_chroma       = 2,
                            total_thickness_cm = sum(thk, na.rm = TRUE),
                            matched_designations = desig[chroma_inferred_mask])
        )
      } else {
        chroma_clay_path <- list(
          passed = FALSE, layers = chroma_inferred,
          source = "high_clay_low_chroma_thin",
          details = list(total_thickness_cm = sum(thk, na.rm = TRUE),
                            min_thickness      = min_thickness)
        )
        chroma_inferred <- integer(0)
      }
    }
  }
  list(designation = v_suffix_path, chroma_clay = chroma_clay_path,
       inferred_layers = inferred_layers, chroma_inferred = chroma_inferred)
}


# ---- thionic / fragic / sombric / chernic --------------------------------

#' Thionic horizon (WRB 2022): post-oxidation acid sulfate horizon.
#' Requires sulfidic_s_pct >= 0.01 AND pH(H2O) <= 4.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_pH Numeric threshold or option (see Details).
#' @param min_sulfidic_s Numeric threshold or option (see Details).
#' @param adjoining \code{FALSE} (default): the thickness is asked of each
#'   layer. \code{TRUE}: of adjoining layers taken together, as the SiBCS
#'   horizonte sulfurico reads it since v0.9.246.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
thionic <- function(pedon, min_thickness = 15, max_pH = 4,
                       min_sulfidic_s = 0.01, adjoining = FALSE) {
  h <- pedon$horizons
  tests <- list()
  tests$pH       <- test_ph_below(h, max_ph = max_pH)
  tests$sulfidic <- test_numeric_above(h, "sulfidic_s_pct",
                                          threshold = min_sulfidic_s,
                                          candidate_layers = tests$pH$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$sulfidic$layers,
                                               adjoining = adjoining)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "thionic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Thionic horizon"
  )
}

#' Fragic horizon (WRB 2022): a high-bulk-density horizon with restricted
#' rooting. v0.3.3: detects via bulk_density_g_cm3 >= 1.65 AND structure
#' grade massive/very firm OR designation pattern \code{x}/\code{Bx}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_bd Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
fragic <- function(pedon, min_thickness = 15, min_bd = 1.65) {
  h <- pedon$horizons
  tests <- list()
  tests$bulk_density <- test_numeric_above(h, "bulk_density_g_cm3",
                                              threshold = min_bd)
  tests$designation_or_struct <- test_pattern_match(
    h, "structure_grade", "massive|very firm",
    candidate_layers = tests$bulk_density$layers
  )
  if (!isTRUE(tests$designation_or_struct$passed)) {
    desg <- test_pattern_match(h, "designation", "x|Bx|Btx",
                                  candidate_layers = tests$bulk_density$layers)
    if (isTRUE(desg$passed)) tests$designation_or_struct <- desg
  }
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$bulk_density$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "fragic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Fragic horizon",
    notes = "v0.3.3 simplification: BD + structure or designation 'x' as proxy"
  )
}

#' Sombric horizon (WRB 2022): subsurface accumulation of humus that
#' qualified neither as spodic nor as a true mollic-like horizon
#' (low-base-saturation cool tropical highlands). v0.3.3 detects via
#' designation pattern + OC criteria (BS < 50, OC > 0.6, depth > 25 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_oc Numeric threshold or option (see Details).
#' @param max_bs Numeric threshold or option (see Details).
#' @param min_top_cm Numeric threshold or option (see Details).
#' @param min_oc_increase Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
sombric <- function(pedon, min_thickness = 15, min_oc = 0.6,
                       max_bs = 50, min_top_cm = 25,
                       min_oc_increase = 0.1) {
  h <- pedon$horizons
  tests <- list()
  # v0.9.1 fix (Bloco C): the v0.3.3 implementation passed
  # `min_top_cm = ...` to `test_top_at_or_above`, whose only argument is
  # `max_top_cm`; the call errored out and sombric() was effectively
  # never callable. The intent of the test is "layer starts AT OR BELOW
  # min_top_cm" (sombric is a subsurface humus-illuviated horizon), so
  # we anchor the candidate layers via a direct depth filter.
  deep_layers <- which(!is.na(h$top_cm) & h$top_cm >= min_top_cm)
  tests$top <- .subtest_result(
    passed  = if (length(deep_layers) > 0L) TRUE
              else if (any(is.na(h$top_cm))) NA else FALSE,
    layers  = deep_layers,
    missing = if (any(is.na(h$top_cm))) "top_cm" else character(0),
    details = list(min_top_cm = min_top_cm)
  )
  tests$oc          <- test_oc_above(h, min_pct = min_oc,
                                        candidate_layers = tests$top$layers)
  tests$bs          <- test_bs_below(h, max_pct = max_bs,
                                        candidate_layers = tests$oc$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = tests$bs$layers)

  # v0.9.2.C tightening: sombric is a HUMUS-ILLUVIATED subsurface
  # horizon, so it must accumulate organic carbon relative to the
  # layer immediately above it. Without this signal a typical Ferralsol
  # / Acrisol BA / Bw, where OC drops monotonically with depth, would
  # spuriously match the bare OC + BS + thickness simplification.
  ord <- order(h$top_cm, na.last = NA)
  cand <- intersect(tests$thickness$layers, ord)
  illuvial_layers <- integer(0)
  for (i in cand) {
    pos <- which(ord == i)
    if (length(pos) == 0L || pos == 1L) next
    upper_oc <- h$oc_pct[ord[pos - 1L]]
    here_oc  <- h$oc_pct[i]
    if (!is.na(upper_oc) && !is.na(here_oc) &&
        here_oc >= upper_oc + min_oc_increase) {
      illuvial_layers <- c(illuvial_layers, i)
    }
  }
  tests$illuvial_signal <- .subtest_result(
    passed  = if (length(illuvial_layers) > 0L) TRUE
              else if (length(cand) == 0L) FALSE
              else FALSE,
    layers  = illuvial_layers,
    missing = if (any(is.na(h$oc_pct))) "oc_pct" else character(0),
    details = list(min_oc_increase = min_oc_increase)
  )

  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "sombric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Sombric horizon",
    notes = "v0.9.2.C: requires OC accumulation vs. immediately overlying layer (humus-illuviation signal)"
  )
}

#' Chernic horizon (WRB 2022 3.1.6)
#'
#' A thick, well-structured, very dark surface horizon with a high base
#' saturation (WRB 2022 3.1.6, pp. 40-41). Every layer of the horizon, from
#' the mineral soil surface down, must have: (1) \\>= 50\% fine earth and no
#' mulmic material; (2) granular structure, subangular blocky structure of
#' average size \\<= 2 cm (very fine, fine or medium), or cloddy /
#' agricultural structure; (3) \\>= 1\% SOC; (4) moist value \\<= 3, dry
#' value \\<= 5 and moist chroma \\<= 2, or the carbonate-rich (15-40\%
#' CaCO3, \\>= 1.5\% SOC) or the \\>= 40\% CaCO3 / loamy-sand (value \\<= 5,
#' \\>= 2.5\% SOC) alternatives; (5) \\>= 1\% more SOC than a parent-material
#' layer of moist value \\<= 4, where one is recorded; (6) BS \\>= 50\%.
#' The layers together must be (7) \\>= 30 cm thick. Worm holes belong to the
#' Vermic qualifier, not to this horizon.
#'
#' A missing dry value is taken as the moist value plus one, as in the mollic
#' colour test. A missing BS is read from pH (Annex 2, Table 9.2).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 30).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
chernic <- function(pedon, min_thickness = 30) {
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.6, Chernic horizon (pp. 40-41)"
  ms <- .q_mineral_surface_cm(h)
  if (nrow(h) == 0L || is.na(ms)) {
    return(DiagnosticResult$new(name = "chernic", passed = NA, layers = integer(0),
                                evidence = list(), missing = c("top_cm", "bottom_cm"),
                                reference = ref))
  }
  parent <- .chernic_parent_oc(h)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= ms - 1e-6]
  yes <- integer(0); maybe <- integer(0); undecided <- FALSE
  last_bottom <- NA_real_; rows <- list(); missing <- character(0)
  for (i in ord) {
    # a surface horizon: contiguous layers from the mineral soil surface
    if (!length(maybe)) {
      if (h$top_cm[i] > ms + 1) break
    } else if (is.na(last_bottom) || abs(h$top_cm[i] - last_bottom) > 1) {
      break
    }
    s <- .chernic_layer(h, i, parent)
    rows[[length(rows) + 1L]] <- c(layer = i, s$criteria)
    if (isFALSE(s$passed)) break
    maybe <- c(maybe, i)
    last_bottom <- h$bottom_cm[i]
    if (isTRUE(s$passed) && !undecided) {
      yes <- c(yes, i)
    } else {
      undecided <- TRUE
      missing <- c(missing, s$missing)
    }
  }
  thk <- function(L) sum(pmax(h$bottom_cm[L] - h$top_cm[L], 0), na.rm = TRUE)
  # 7. a thickness of >= 30 cm
  passed <- if (thk(yes) >= min_thickness) TRUE
            else if (thk(maybe) >= min_thickness) NA
            else FALSE
  DiagnosticResult$new(
    name = "chernic",
    passed = passed,
    layers = if (isTRUE(passed)) yes else integer(0),
    evidence = list(
      layers = if (length(rows)) as.data.frame(do.call(rbind, rows)) else NULL,
      thickness_cm = thk(yes), possible_thickness_cm = thk(maybe),
      parent_material = parent, mineral_surface_cm = ms),
    missing = if (is.na(passed)) unique(missing) else character(0),
    reference = ref
  )
}

# The parent-material layer of criterion 5 of the chernic horizon (3.1.6,
# p. 41): a C layer with a recorded moist value <= 4, the shallowest one. NULL
# when none is recorded, and the criterion is then not applied (Ch 2.1 rule 5:
# an if-clause that is not true is ignored).
.chernic_parent_oc <- function(h) {
  d <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
  vm <- h$munsell_value_moist
  c_layer <- which(!is.na(d) & grepl("^[0-9]*C", d) & !is.na(vm) & vm <= 4)
  if (!length(c_layer)) return(NULL)
  i <- c_layer[order(h$top_cm[c_layer])][1]
  list(layer = i, oc_pct = h$oc_pct[i])
}

# Criteria 1-6 of the chernic horizon (3.1.6, pp. 40-41) in layer i: each
# subhorizon must meet them (Ch 2.1 rule 7). passed is TRUE / FALSE / NA.
.chernic_layer <- function(h, i, parent = NULL) {
  num <- function(x) suppressWarnings(as.numeric(.wrb_hcell(h, x, i)))
  le <- function(x, t) if (is.na(x)) NA else x <= t
  ge <- function(x, t) if (is.na(x)) NA else x >= t
  vm <- num("munsell_value_moist"); cm <- num("munsell_chroma_moist")
  vd <- num("munsell_value_dry");  oc <- num("oc_pct")
  ca <- num("caco3_pct");          cf <- num("coarse_fragments_pct")
  if (is.na(vd) && !is.na(vm)) vd <- vm + 1   # as test_mollic_color does
  structure <- .chernic_structure(.wrb_hcell(h, "structure_type", i),
                                  .wrb_hcell(h, "structure_size", i))
  # loamy sand or coarser (4c)
  coarse <- {
    f <- is_sandy_loam_or_finer(num("sand_pct"), num("silt_pct"), num("clay_pct"))
    if (is.na(f)) NA else !f
  }
  # 1. >= 50% fine earth, and no mulmic material (3.3.12, p. 90: >= 8% SOC,
  # single grain or blocky <= 2 cm, chroma <= 2; a granular layer is not)
  granular <- !is.na(structure$type) && structure$type == "granular"
  not_mulmic <- .any3(c(le(oc, 8 - 1e-9), if (!is.na(cm)) cm > 2 else NA, granular))
  fine_earth <- .and3(le(cf, 50), not_mulmic)
  # 4. colour: a, b or c
  col_a <- .and3(le(vm, 3), le(vd, 5), le(cm, 2))
  col_b <- .and3(ge(ca, 15), if (is.na(ca)) NA else ca < 40, le(vm, 3), le(cm, 2), ge(oc, 1.5))
  col_c <- .and3(.any3(c(ge(ca, 40), coarse)), le(vm, 5), le(cm, 2), ge(oc, 2.5))
  colour <- .any3(c(col_a, col_b, col_c))
  # 5. >= 1% (absolute) more SOC than the parent material, where recorded
  parent_ok <- if (is.null(parent) || identical(parent$layer, i)) TRUE
               else if (is.na(oc) || is.na(parent$oc_pct)) NA
               else oc >= parent$oc_pct + 1
  # 6. base saturation >= 50% (Annex 2, Table 9.2 where BS is not recorded)
  bs <- .wrb_bs_status(h, i, 50)$passed
  crit <- c(fine_earth = fine_earth, structure = structure$passed,
            organic_carbon = ge(oc, 1), colour = colour,
            parent_material = parent_ok, base_saturation = bs)
  miss <- list(fine_earth = "coarse_fragments_pct",
               structure = c("structure_type", "structure_size"),
               organic_carbon = "oc_pct",
               colour = c(if (is.na(vm)) "munsell_value_moist",
                          if (is.na(cm)) "munsell_chroma_moist",
                          if (!is.na(vm) && !is.na(cm)) "caco3_pct"),
               parent_material = "oc_pct",
               base_saturation = c("bs_pct", "ph_h2o"))
  list(passed = .and3(crit), criteria = crit,
       missing = unique(unlist(miss[names(crit)[is.na(crit)]])))
}

# Criterion 2 of the chernic horizon (3.1.6, p. 40): granular, subangular
# blocky of average size <= 2 cm (very fine, fine or medium), or cloddy /
# agricultural structure, single or in combination in >= 90% of the volume.
# A combination with another type leaves the share unknown (NA).
.chernic_structure <- function(type, size) {
  t <- tolower(trimws(as.character(type)))
  if (length(t) != 1L || is.na(t) || !nzchar(t)) return(list(passed = NA, type = NA_character_))
  sab <- grepl("subangular", t)
  t2 <- gsub("subangular", "", t)
  good <- grepl("granular|crumb|clod", t2)
  bad <- grepl("angular|prism|column|plat|massive|single|wedge|lentic|rock|strat", t2)
  unknown <- grepl("blocky", t2) && !sab && !grepl("angular", t2)
  parts <- c(if (good) TRUE, if (bad) FALSE, if (unknown) NA)
  if (sab) {
    s <- tolower(trimws(as.character(size)))
    k <- if (length(s) != 1L || is.na(s) || !nzchar(s)) NA_integer_
         else .structure_size_class(trimws(strsplit(s, " to | and | a |-|/")[[1]]))
    parts <- c(parts, if (!length(k) || anyNA(k)) NA else if (all(k <= 3L)) TRUE
                      else if (all(k >= 4L)) FALSE else NA)
  }
  passed <- if (!length(parts)) NA
            else if (all(parts %in% TRUE)) TRUE
            else if (all(parts %in% FALSE)) FALSE
            else NA
  list(passed = passed,
       type = if (good && !sab && !bad && !unknown) "granular" else t)
}


# ---- anthropogenic family (anthraquic, hydragric, hortic, irragric, ----
# ---- plaggic, pretic, terric) -------------------------------------------

# Common helper: detect the anthropogenic horizons via designation pattern
# OR via structured agronomic indicators (P_mehlich-3, structure).

#' Anthraquic horizon (WRB 2022): puddled-rice / paddy plough layer.
#' v0.3.3 detects via designation pattern \code{Apl|Ap|Hh}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
anthraquic <- function(pedon, min_thickness = 20, max_top_cm = 50) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "Apl|Apg|Ahg|Hh")
  tests$top         <- test_starts_within(h, max_top_cm = max_top_cm,
                                            candidate_layers = tests$designation$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = tests$top$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "anthraquic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Anthraquic horizon"
  )
}

#' Hydragric horizon (WRB 2022): subsoil hydric horizon under anthraquic.
#' v0.3.3 detects via designation pattern \code{Bg|Brg} immediately below
#' an anthraquic-like topsoil.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
hydragric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  # v0.9.217: WRB 2022 Ch 3.1, Hydragric horizon, criterion 1: it "is
  # overlain by an anthraquic horizon". Until now any Bg/Brg/Bdg horizon of
  # 20 cm passed, so every gleyed Gleysol or Stagnosol B was "hydragric" and
  # the soil was named Hydragric. Only layers below an anthraquic horizon count.
  aq <- anthraquic(pedon)
  aq_bottom <- if (isTRUE(aq$passed) && length(aq$layers))
                 max(h$bottom_cm[aq$layers], na.rm = TRUE) else NA_real_
  below <- if (is.finite(aq_bottom))
             which(!is.na(h$top_cm) & h$top_cm >= aq_bottom - 1e-6) else integer(0)
  tests$under_anthraquic <- .subtest_result(
    passed = if (isTRUE(aq$passed)) length(below) > 0L else aq$passed,
    layers = below, missing = aq$missing %||% character(0))
  tests$designation <- test_pattern_match(h, "designation", "^Bg|^Brg|^Bdg")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = intersect(
                                                  tests$designation$layers, below))
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "hydragric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Hydragric horizon"
  )
}

#' Hortic horizon (WRB 2022): garden / kitchen-midden topsoil. Diagnostic
#' criteria: thickness \\>= 20 cm, dark colour (mollic-like), high P
#' (Mehlich-3 P >= 100 mg/kg or P2O5_1pct_citric >= 175 mg/kg), high SOC.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_oc Numeric threshold or option (see Details).
#' @param min_p_mehlich3 Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
hortic <- function(pedon, min_thickness = 20, min_oc = 1.0,
                      min_p_mehlich3 = 100) {
  h <- pedon$horizons
  tests <- list()
  tests$top    <- test_starts_within(h, max_top_cm = 5)
  tests$oc     <- test_oc_above(h, min_pct = min_oc,
                                   candidate_layers = tests$top$layers)
  tests$p      <- test_numeric_above(h, "p_mehlich3_mg_kg",
                                        threshold = min_p_mehlich3,
                                        candidate_layers = tests$oc$layers)
  tests$thick  <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = tests$p$layers)
  desg <- test_pattern_match(h, "designation", "^Ah?h|^Ap[ht]")
  if (!isTRUE(tests$p$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "hortic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Hortic horizon"
  )
}

#' Irragric horizon (WRB 2022): topsoil thickened by irrigation deposits.
#'
#' v0.9.317: the criteria of WRB 2022 Ch 3.1.18 (p. 50-51), tested on every
#' stack of contiguous layers from the mineral soil surface (a surface
#' horizon of mineral material), each against the layer directly below it:
#' \enumerate{
#'   \item soil aggregate structure or cloddy structure in \eqn{\ge} 90\% by
#'         volume;
#'   \item a clay content \eqn{\ge} 10\% (relative) and \eqn{\ge} 3\%
#'         (absolute) higher than that of the layer directly buried; the fine
#'         clay alternative is not recorded, so a smaller increase leaves it
#'         undecided;
#'   \item differences between subhorizons of < 20\% (relative) or < 4\%
#'         (absolute) in silt, clay and carbonates (also in the medium, fine and
#'         very fine sand, which soilKey does not record: undecided for more
#'         than one layer unless a recorded difference fails);
#'   \item \eqn{\ge} 0.3\% SOC in each layer and \eqn{\ge} 0.5\% on a weighted
#'         average;
#'   \item \eqn{\ge} 25\% traces of soil animal activity (\code{worm_holes_pct},
#'         weighted);
#'   \item evidence that the land surface has been raised: soilKey records no
#'         such evidence, so this criterion is undecided;
#'   \item \eqn{\ge} 20 cm thick (\code{min_thickness}).
#' }
#' The horizon is therefore never established from the data: \code{FALSE}
#' when every stack fails a criterion, \code{NA} otherwise. Until v0.9.316 any
#' Ap or Au of 20 cm passed.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
irragric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.18, Irragric horizon (p. 50-51)"
  ms <- if (n) .q_mineral_surface_cm(h) else NA_real_
  if (!n || is.na(ms))
    return(DiagnosticResult$new(name = "irragric", passed = NA, layers = integer(0),
                                evidence = list(), missing = c("top_cm", "bottom_cm"),
                                reference = ref))
  col <- function(v) suppressWarnings(as.numeric(h[[v]] %||% rep(NA_real_, n)))
  clay <- col("clay_pct"); silt <- col("silt_pct"); caco3 <- col("caco3_pct")
  oc <- col("oc_pct"); worm <- col("worm_holes_pct")
  thick <- h$bottom_cm - h$top_cm
  kind <- .wrb_structure_kind(h$structure_type %||% rep(NA_character_, n),
                              h$structure_grade %||% rep(NA_character_, n))
  aggregate <- ifelse(is.na(kind), NA, kind != "none")
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & !is.na(h$bottom_cm[ord]) & h$top_cm[ord] >= ms - 1e-6]
  run <- integer(0)
  for (i in ord) {
    if (!length(run)) {
      if (abs(h$top_cm[i] - ms) > 1e-6) break
    } else if (abs(h$top_cm[i] - h$bottom_cm[run[length(run)]]) > 1) break
    run <- c(run, i)
  }
  wavg <- function(x, L) if (anyNA(x[L])) NA_real_ else sum(x[L] * thick[L]) / sum(thick[L])
  # differences between subhorizons: < 20% relative or < 4% absolute
  small <- function(x, L) {
    if (length(L) < 2L) return(TRUE)
    d <- vapply(seq_len(length(L) - 1L), function(k) {
      a <- x[L[k]]; b <- x[L[k + 1L]]
      if (is.na(a) || is.na(b)) return(NA)
      abs(a - b) < 4 || (max(a, b) > 0 && abs(a - b) / max(a, b) < 0.2)
    }, logical(1))
    if (any(d %in% FALSE)) FALSE else if (all(d %in% TRUE)) TRUE else NA
  }
  stacks <- lapply(seq_along(run), function(k) {
    L <- run[seq_len(k)]
    below <- ord[match(L[k], ord) + 1L]
    c1 <- .wrb_share_by_volume(h, L, aggregate[L], 0.9)
    c2 <- if (is.na(below) || is.na(clay[below]) || anyNA(clay[L])) NA
          else if (all(clay[L] >= 1.1 * clay[below] & clay[L] - clay[below] >= 3)) TRUE
          else NA
    c3 <- .and3(small(silt, L), small(clay, L), small(caco3, L),
                if (length(L) > 1L) NA else TRUE)
    c4 <- .and3(if (any(oc[L] < 0.3, na.rm = TRUE)) FALSE else if (anyNA(oc[L])) NA else TRUE,
                { a <- wavg(oc, L); if (is.na(a)) NA else a >= 0.5 })
    c5 <- { a <- wavg(worm, L); if (is.na(a)) NA else a >= 25 }
    c6 <- NA
    c7 <- sum(thick[L]) >= min_thickness
    data.frame(layers = paste(L, collapse = " "), bottom_cm = h$bottom_cm[L[k]],
               buried_layer = below, structure = c1, clay_increase = c2,
               uniform = c3, organic_carbon = c4, animal_traces = c5,
               raised_surface = c6, thickness = c7,
               status = .and3(c1, c2, c3, c4, c5, c6, c7), stringsAsFactors = FALSE)
  })
  crit <- if (length(stacks)) do.call(rbind, stacks) else NULL
  st <- if (is.null(crit)) logical(0) else crit$status
  passed <- if (any(st %in% TRUE)) TRUE else if (length(st) && all(st %in% FALSE)) FALSE else NA
  layers <- if (isTRUE(passed))
    as.integer(strsplit(crit$layers[which(st %in% TRUE)[1]], " ")[[1]]) else integer(0)
  und <- if (is.null(crit)) crit else crit[is.na(st), , drop = FALSE]
  miss <- if (!is.na(passed) || is.null(und)) character(0) else unique(c(
    if (anyNA(und$structure)) "structure_type",
    if (anyNA(und$clay_increase)) "clay_pct",
    if (anyNA(und$organic_carbon)) "oc_pct",
    if (anyNA(und$animal_traces)) "worm_holes_pct"))
  DiagnosticResult$new(
    name = "irragric", passed = passed, layers = layers,
    evidence = list(criteria = crit),
    missing = miss, reference = ref,
    notes = paste("WRB 2022 Ch 3.1.18 criterion 6 (evidence that the land",
                  "surface has been raised) is not recorded by soilKey"))
}

#' Plaggic horizon (WRB 2022): sod-derived topsoil >= 20 cm with low BD
#' AND independent evidence of human input.
#'
#' v0.9.2.C tightening: the v0.3.3 implementation accepted ANY thick,
#' low-BD, OC-rich A horizon, which over-fired across natural mollic /
#' umbric / chernic surfaces. The diagnostic now requires, in addition
#' to the OC + BD + thickness baseline, at least one independent
#' anthropogenic-input marker:
#' \itemize{
#'   \item \code{p_mehlich3_mg_kg >= 50} (sustained sod / manure
#'         additions concentrate Mehlich-3 P in the topsoil), OR
#'   \item \code{artefacts_pct > 0} (any human artefact volume fraction
#'         is sufficient as a presence signal), OR
#'   \item designation pattern \code{Apl} / \code{Aplg} / \code{Apk}
#'         / explicit "plagg".
#' }
#' Without one of those markers the diagnostic returns FALSE even when
#' OC + BD + thickness pass. This mirrors the v0.9.1 \code{qual_plaggic}
#' gate but enforces the rule at the diagnostic level so any caller
#' (SiBCS, USDA, future modules) inherits the protection.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param max_bd Numeric threshold or option (see Details).
#' @param min_oc Numeric threshold or option (see Details).
#' @param min_p_mehlich3 Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
plaggic <- function(pedon, min_thickness = 20, max_bd = 1.5,
                       min_oc = 0.6, min_p_mehlich3 = 100) {
  # WRB 2022 Ch 3.1.29 criterion 2b: >= 100 mg/kg P (Mehlich-3) in the upper
  # 20 cm (corrected from 50).
  h <- pedon$horizons
  tests <- list()
  tests$oc          <- test_oc_above(h, min_pct = min_oc)
  tests$bd          <- test_bulk_density_below(h, max_g_cm3 = max_bd,
                                                  candidate_layers = tests$oc$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$bd$layers)
  desg <- test_pattern_match(h, "designation", "^Ap[lp]")
  if (!isTRUE(tests$thickness$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }

  # v0.9.2.C anthropogenic-evidence gate.
  candidate <- if (!is.null(tests$designation_proxy))
                 union(tests$thickness$layers, tests$designation_proxy$layers)
               else tests$thickness$layers
  candidate <- candidate %||% integer(0)
  if (length(candidate) > 0L) {
    p   <- h$p_mehlich3_mg_kg[candidate]
    art <- h$artefacts_pct[candidate]
    dsg <- h$designation[candidate]
    ev_p   <- !is.na(p)   & p   >= min_p_mehlich3
    ev_art <- !is.na(art) & art >  0
    ev_dsg <- !is.na(dsg) & grepl("^Apl|^Aplg|plagg|^Apk|^Aphu", dsg,
                                    ignore.case = TRUE)
    has_ev <- ev_p | ev_art | ev_dsg
    tests$anthropic_evidence <- .subtest_result(
      passed  = if (any(has_ev)) TRUE
                else if (all(is.na(p)) && all(is.na(art)) &&
                         all(is.na(dsg))) NA
                else FALSE,
      layers  = candidate[has_ev],
      missing = if (all(is.na(p)) && all(is.na(art)) && all(is.na(dsg)))
                  c("p_mehlich3_mg_kg", "artefacts_pct", "designation")
                else character(0),
      details = list(p = p, artefacts_pct = art, designation = dsg)
    )
  } else {
    tests$anthropic_evidence <- .subtest_result(
      passed = FALSE, layers = integer(0), missing = character(0),
      details = list()
    )
  }

  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "plaggic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Plaggic horizon",
    notes = "v0.9.2.C: anthropic-evidence gate (P / artefacts / Apl-family designation)"
  )
}

#' Pretic horizon (WRB 2022): "Amazonian Dark Earth" (terra preta de
#' indio) horizon -- thick anthropogenic surface with high P, SOC, and
#' incorporated charcoal / pottery.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_oc Numeric threshold or option (see Details).
#' @param min_p_mehlich3 Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
pretic <- function(pedon, min_thickness = 20, min_oc = 1.5,
                      min_p_mehlich3 = 30) {
  h <- pedon$horizons
  tests <- list()
  tests$oc    <- test_oc_above(h, min_pct = min_oc)
  tests$p     <- test_numeric_above(h, "p_mehlich3_mg_kg",
                                       threshold = min_p_mehlich3,
                                       candidate_layers = tests$oc$layers)
  tests$thick <- test_minimum_thickness(h, min_cm = min_thickness,
                                           candidate_layers = tests$p$layers)
  desg <- test_pattern_match(h, "designation", "^A[pu]?p|^Au")
  if (!isTRUE(tests$thick$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "pretic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Pretic horizon"
  )
}

#' Terric horizon (WRB 2022): topsoil thickened by long-term application
#' of mineral material (sediment / sand additions). v0.3.3: thickness >=
#' 20 cm + designation Au / Apc.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
terric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "^Au|^Apc")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$designation$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "terric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Terric horizon"
  )
}


# ---- Ch 3.1 final four (v0.3.5): tsitelic, panpaic, limonic, protovertic ----

#' Tsitelic horizon (WRB 2022 Ch 3.1)
#'
#' From Georgian \emph{tsiteli} = red. A red colour-defined horizon
#' formed on weathered basalt or similar Fe-rich parent material in
#' Caucasian / Mediterranean settings. Used by the Cambisols key
#' (Ch 4 p 123, criterion 4) and by the Tsitelic qualifier.
#'
#' Diagnostic criteria (v0.3.5 simplification):
#' \itemize{
#'   \item Munsell hue \\<= 2.5YR (i.e. 2.5YR, 10R, 7.5R, 5R, 2.5R)
#'         AND value \\<= 4 (moist) AND chroma \\>= 4 (moist);
#'   \item evidence of soil formation (cambic-style criterion 3)
#'         proxied by clay \\>= 8\% AND \code{structure_grade} not
#'         "single grain" / "massive";
#'   \item thickness \\>= 10 cm.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
tsitelic <- function(pedon, min_thickness = 10) {
  h <- pedon$horizons
  tests <- list()
  tests$hue <- test_pattern_match(
    h, "munsell_hue_moist",
    "^(2\\.5YR|10R|7\\.5R|5R|2\\.5R)"
  )
  ok_value <- which(!is.na(h$munsell_value_moist) &
                      h$munsell_value_moist <= 4)
  ok_chroma <- which(!is.na(h$munsell_chroma_moist) &
                       h$munsell_chroma_moist >= 4)
  red_layers <- intersect(intersect(tests$hue$layers, ok_value), ok_chroma)
  tests$red_munsell <- .subtest_result(
    passed = if (length(red_layers) > 0L) TRUE
             else if (all(is.na(h$munsell_hue_moist))) NA
             else FALSE,
    layers = red_layers,
    missing = if (any(is.na(h$munsell_hue_moist)))
                "munsell_hue_moist" else character(0),
    details = list()
  )
  ok_clay <- which(!is.na(h$clay_pct) & h$clay_pct >= 8)
  ok_struct <- which(is.na(h$structure_grade) |
                       !grepl("single grain|massive",
                                h$structure_grade,
                                ignore.case = TRUE))
  formed_layers <- intersect(ok_clay, ok_struct)
  tests$formed <- .subtest_result(
    passed = if (length(formed_layers) > 0L) TRUE
             else if (all(is.na(h$clay_pct))) NA
             else FALSE,
    layers = formed_layers,
    missing = if (any(is.na(h$clay_pct))) "clay_pct" else character(0),
    details = list()
  )
  shared <- intersect(tests$red_munsell$layers, tests$formed$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = shared)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "tsitelic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Tsitelic horizon",
    notes = "v0.3.5: Munsell red hue + value/chroma + clay >= 8% + non-massive structure"
  )
}


#' Panpaic horizon (WRB 2022 Ch 3.1)
#'
#' From Quechua \emph{p'anpay} = "to bury". A buried diagnostic horizon
#' (any horizon whose original surface was subsequently overlain by
#' younger material). Used by the Panpaic qualifier and by the Cambisols
#' / Anthrosols branches.
#'
#' Since v0.9.220 the four criteria of WRB 2022 Ch 3.1.23 on a buried
#' surface horizon (an A designation with a \code{b} suffix, or below a
#' lithic discontinuity, \code{2A}): \eqn{\ge} 0.2\% SOC; SOC
#' \eqn{\ge} 25\% (relative) and \eqn{\ge} 0.2\% (absolute) higher
#' than in the overlying layer; a lithic discontinuity at its upper limit;
#' \eqn{\ge} 5 cm thick. Until v0.9.219 any designation containing a
#' \code{b} passed, \code{AB} included.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
panpaic <- function(pedon) {
  # v0.9.220: WRB 2022 Ch 3.1.23, "a buried surface horizon consisting of
  # mineral material and has: 1. >= 0.2% soil organic carbon; and 2. a content
  # of soil organic carbon >= 25% (relative) and >= 0.2% (absolute) higher than
  # in the overlying layer; and 3. a lithic discontinuity at its upper limit;
  # and 4. a thickness of >= 5 cm". A buried surface horizon is an A
  # designation with a "b" suffix or below a lithic discontinuity (2A). Until
  # v0.9.219 any designation with a "b" anywhere passed, AB included (the
  # pattern was matched ignoring case), with none of the four criteria.
  h <- pedon$horizons
  n <- nrow(h)
  d <- as.character(h$designation %||% rep(NA_character_, n))
  num <- .desg_material_number(d)
  buried_a <- !is.na(d) & grepl("^[0-9]*A", d) &
    (grepl("^[0-9]*A[a-z0-9]*b[0-9]*$", d) | (!is.na(num) & num >= 2))
  ld <- tryCatch(lithic_discontinuity(pedon), error = function(e) NULL)
  ord <- order(h$top_cm)
  status <- rep(FALSE, n)
  for (k in seq_along(ord)) {
    i <- ord[k]
    if (!buried_a[i] || k == 1L) next
    j <- ord[k - 1L]
    thk <- h$bottom_cm[i] - h$top_cm[i]
    disc <- if ((i %in% (ld$layers %||% integer(0))) ||
                (!is.na(num[i]) && !is.na(num[j]) && num[i] != num[j])) TRUE
            else if (is.null(ld) || is.na(ld$passed)) NA else FALSE
    oc_i <- h$oc_pct[i]; oc_j <- h$oc_pct[j]
    mineral <- if (is.na(oc_i)) NA else oc_i < 20
    soc <- if (is.na(oc_i) || is.na(oc_j)) NA
           else oc_i >= 0.2 && oc_i - oc_j >= 0.2 && oc_i >= 1.25 * oc_j
    status[i] <- .and3(mineral, soc, disc, if (is.na(thk)) NA else thk >= 5)
  }
  passed <- if (any(status %in% TRUE)) TRUE
            else if (anyNA(status)) NA
            else if (all(is.na(d))) NA
            else FALSE
  DiagnosticResult$new(
    name = "panpaic", passed = passed, layers = which(status %in% TRUE),
    evidence = list(buried_surface_horizon = which(buried_a), status = status,
                    lithic_discontinuity = ld),
    missing = if (is.na(passed)) c("designation", "oc_pct", "top_cm", "bottom_cm")
              else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.23, Panpaic horizon"
  )
}


#' Limonic horizon (WRB 2022 Ch 3.1)
#'
#' From Greek \emph{leimon} = meadow. A subaqueous / wet-meadow horizon
#' showing accumulation of secondary Fe/Mn (oxi)hydroxides from
#' fluctuating redox cycles. Distinct from \emph{limnic material}
#' (Ch 3.3.10), which is the parent material; the limonic horizon is
#' the \emph{soil} horizon derived from such material plus subsequent
#' pedogenesis.
#'
#' v0.3.5 detection: redoximorphic_features_pct \\>= 5 AND designation
#' pattern \code{Bm} / \code{Bjm} / \code{m} as proxy for past meadow
#' wetness.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_redox_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
limonic <- function(pedon, min_thickness = 5, min_redox_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$redox <- test_numeric_above(h, "redoximorphic_features_pct",
                                       threshold = min_redox_pct)
  tests$pattern <- test_pattern_match(h, "designation",
                                          "^Bm|^Bjm|^Bjg|^Bgm",
                                          candidate_layers = tests$redox$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$pattern$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "limonic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Limonic horizon",
    notes = "v0.3.5: redox >=5% + meadow-pattern designation (Bm / Bjm)"
  )
}


#' Protovertic horizon (WRB 2022 Ch 3.1)
#'
#' A weakly developed vertic horizon -- the swelling/shrinking
#' machinery is present but does not reach the full vertic intensity
#' (cracks too narrow, or slickensides only "few", or thickness too
#' small). Used by the Protovertic qualifier; relevant for soils that
#' would be Vertisols if the cracks/slickensides were a notch
#' stronger.
#'
#' v0.3.5 detection: clay \\>= 30\% AND any positive vertic evidence
#' (slickensides at \\>= "few" OR cracks_width_cm \\>= 0.2 OR a
#' wedge/lenticular structure_type) AND thickness \\>= 15 cm. The
#' positive cases that pass the strict \code{\link{vertic_horizon}}
#' test are explicitly excluded so the two diagnostics partition the
#' vertic-spectrum cleanly.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Numeric threshold or option (see Details).
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
protovertic <- function(pedon, min_clay = 30, min_thickness = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$clay <- test_clay_above(h, min_pct = min_clay)
  tests$weak_evidence <- test_slickensides_present(
    h,
    levels = c("few", "common", "many", "continuous"),
    candidate_layers = tests$clay$layers
  )
  if (!isTRUE(tests$weak_evidence$passed)) {
    cracks <- test_shrink_swell_cracks(h, min_width_cm = 0.2,
                                          candidate_layers = tests$clay$layers)
    if (isTRUE(cracks$passed)) tests$weak_evidence <- cracks
  }
  if (!isTRUE(tests$weak_evidence$passed)) {
    wedge <- test_pattern_match(h, "structure_type",
                                   "wedge|lenticular",
                                   candidate_layers = tests$clay$layers)
    if (isTRUE(wedge$passed)) tests$weak_evidence <- wedge
  }
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$weak_evidence$layers)
  # Mutual exclusion with the strict vertic horizon.
  vh <- vertic_horizon(pedon)
  vh_layers <- vh$layers %||% integer(0)
  agg <- aggregate_subtests(tests)
  layers_out <- setdiff(agg$layers, vh_layers)
  passed <- if (length(layers_out) > 0L) TRUE
            else if (is.na(agg$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "protovertic", passed = passed, layers = layers_out,
    evidence = c(tests, list(strict_vertic_excluded = vh_layers)),
    missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Protovertic horizon",
    notes = "v0.3.5: clay + weak vertic evidence + thickness; strict-vertic layers excluded"
  )
}
