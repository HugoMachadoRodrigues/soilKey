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

#' Petroplinthic horizon (WRB 2022): cemented plinthic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_plinthite_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
petroplinthic <- function(pedon, min_thickness = 10, min_plinthite_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$plinthite  <- test_plinthite_concentration(h, min_pct = min_plinthite_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$plinthite$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petroplinthic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petroplinthic horizon"
  )
}

#' Pisoplinthic horizon (WRB 2022): pisolitic plinthic. v0.3.3 detects via
#' designation pattern \code{Bspl} / \code{Bvpi} or via plinthite \\>= 15\%
#' AND structure_type containing 'pisol'.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @param min_plinthite_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
pisoplinthic <- function(pedon, min_thickness = 15, min_plinthite_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$plinthite  <- test_plinthite_concentration(h, min_pct = min_plinthite_pct)
  tests$pisolitic  <- test_pattern_match(h, "structure_type", "pisol",
                                            candidate_layers = tests$plinthite$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$pisolitic$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "pisoplinthic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Pisoplinthic horizon"
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

#' Chernic horizon (WRB 2022): the cherozemic-style mollic with very high
#' biological activity (worm holes, casts, coprolites). v0.3.3:
#' delegates to mollic + worm_holes_pct >= 50 (proxy for "biological
#' homogenization").
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_worm_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
chernic <- function(pedon, min_worm_pct = 50) {
  m <- mollic(pedon)
  if (!isTRUE(m$passed)) {
    return(DiagnosticResult$new(
      name = "chernic",
      passed = if (is.na(m$passed)) NA else FALSE,
      layers = integer(0), evidence = list(mollic = m),
      missing = m$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3.1, Chernic horizon",
      notes = "Failed/NA because mollic horizon test did not pass"
    ))
  }
  h <- pedon$horizons
  worms <- test_numeric_above(h, "worm_holes_pct",
                                 threshold = min_worm_pct,
                                 candidate_layers = m$layers)
  passed <- if (is.na(worms$passed)) NA else
            (isTRUE(m$passed) && isTRUE(worms$passed))
  DiagnosticResult$new(
    name = "chernic",
    passed = passed,
    layers = if (isTRUE(passed)) intersect(m$layers, worms$layers)
             else integer(0),
    evidence = list(mollic = m, worm_holes = worms),
    missing  = unique(c(m$missing, worms$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Chernic horizon",
    notes = "Mollic + worm_holes_pct >= 50 (biological-homogenization proxy)"
  )
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
#' v0.3.3: thickness >= 20 cm + sediment-derived structure proxied via
#' designation \code{Apk|Apg|Au}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
irragric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "^A[pu]k?|^Ahu")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$designation$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "irragric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Irragric horizon"
  )
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
