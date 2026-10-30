# =============================================================================
# v0.9.314 -- WRB 2022 readings for the weakly developed soils and the soils of
# carbonate and gypsum accumulation: the cambic horizon (Ch 3.1.5), the calcic
# horizon (Ch 3.1.4), fluvic material (Ch 3.3.6) and the Ch 4 entries of the
# Gypsisols, Calcisols, Cambisols, Fluvisols and Arenosols. The USDA and SiBCS
# keys keep the readings they had (.cambic_impl(), .calcic_impl(),
# .fluvic_material_impl()).
# =============================================================================

# Mineral layers: not organic (SOC >= 20 %, or an O or H designation), as
# .q_mineral_surface_cm() reads them.
.wrb_mineral_layer <- function(h) {
  n <- nrow(h)
  d <- as.character(h$designation %||% rep(NA_character_, n))
  oc <- h$oc_pct %||% rep(NA_real_, n)
  !((!is.na(oc) & oc >= 20) | (!is.na(d) & grepl("^[0-9]*[OH]", d)))
}

# Depth runs of the layers `idx`, each a list(top, bottom, layers): the
# adjoining layers taken together, as a horizon described in sub-horizons.
.wrb_runs <- function(h, idx) .q_layer_runs(h, idx)

# TRUE if any is TRUE, NA if none is TRUE and one is NA, else FALSE.
.wrb_any3 <- function(x) {
  x <- unlist(x)
  if (any(x %in% TRUE)) TRUE else if (anyNA(x)) NA else FALSE
}

# Texture class sandy loam or finer per layer (WRB 2022 Annex 1 texture
# triangle: silt + 2 x clay >= 30), decided from what is recorded: silt and
# clay; clay >= 15 alone; or the silt that sand and clay leave.
.wrb_sandy_loam_or_finer <- function(h, idx = seq_len(nrow(h))) {
  vapply(idx, function(i) {
    sa <- h$sand_pct[i]; si <- h$silt_pct[i]; cl <- h$clay_pct[i]
    if (is.na(si) && !is.na(sa) && !is.na(cl)) si <- 100 - sa - cl
    if (!is.na(si) && !is.na(cl)) si + 2 * cl >= 30
    else if (!is.na(cl) && cl >= 15) TRUE
    else NA
  }, NA)
}


# ---- Cambic horizon (WRB 2022 Ch 3.1.5, p. 39-40) ----------------------------

# The horizons and layers a cambic horizon does not form part of (criterion 4,
# p. 40), by the soilKey functions that test them. The plough layer is an Ap
# and is set aside with the A horizons (.wrb_not_subsurface()). The tsitelic
# horizon is left out: tsitelic() reads only a red hue, value and chroma and
# >= 8% clay, not the Fe-ox / Fe-dith ratio that tells it from the cambic
# horizon (Ch 3.1.5, p. 40), so it would set aside every red cambic horizon.
.WRB_CAMBIC_NOT_PART_OF <- c(
  albic = "albic", anthraquic = "anthraquic", argic = "argic",
  calcic = "calcic", duric = "duric_horizon", ferralic = "ferralic",
  fragic = "fragic", gypsic = "gypsic", hortic = "hortic",
  hydragric = "hydragric", irragric = "irragric", limonic = "limonic",
  mollic = "mollic", natric = "natric_horizon", nitic = "nitic_horizon",
  petrocalcic = "petrocalcic", petroduric = "petroduric",
  petrogypsic = "petrogypsic", petroplinthic = "petroplinthic",
  pisoplinthic = "pisoplinthic", plaggic = "plaggic", plinthic = "plinthic",
  pretic = "pretic", salic = "salic", sombric = "sombric", spodic = "spodic",
  umbric = "umbric_horizon", terric = "terric",
  vertic = "vertic_horizon", andic = "andic_properties")

#' Which of the layers `idx` form part of a horizon of criterion 4 of the
#' cambic horizon. A horizon whose test is undecided (NA) sets no layer aside.
#' Returns list(excluded = logical per layer, by = horizon names per layer).
#' @noRd
.wrb_cambic_part_of_other <- function(pedon, idx,
                                      horizons = .WRB_CAMBIC_NOT_PART_OF) {
  n <- nrow(pedon$horizons)
  excluded <- rep(FALSE, n); by <- rep("", n)
  ns <- asNamespace("soilKey")
  for (nm in names(horizons)) {
    if (!any(!excluded[idx])) break
    f <- get(horizons[[nm]], envir = ns, mode = "function")
    r <- tryCatch(f(pedon), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$passed)) next
    lay <- intersect(as.integer(r$layers), idx)
    if (nm == "albic") lay <- .wrb_albic_layers(pedon, lay)
    lay <- lay[!excluded[lay]]
    excluded[lay] <- TRUE; by[lay] <- nm
  }
  list(excluded = excluded, by = by)
}

#' The layers of claric material among `idx` that are an albic horizon by
#' criterion 2 of WRB 2022 Ch 3.1.1 (p. 34): they overlie an argic, natric,
#' plinthic or spodic horizon, or form part of a layer with stagnic
#' properties. albic() tests the claric colours and the thickness only.
#' @noRd
.wrb_albic_layers <- function(pedon, idx) {
  if (!length(idx)) return(integer(0))
  h <- pedon$horizons
  layers_of <- function(f) {
    r <- tryCatch(f(pedon), error = function(e) NULL)
    if (!is.null(r) && isTRUE(r$passed)) as.integer(r$layers) else integer(0)
  }
  under <- unique(c(layers_of(argic), layers_of(natric_horizon),
                    layers_of(plinthic), layers_of(spodic)))
  stag <- layers_of(stagnic_properties)
  deepest <- if (length(under)) max(h$top_cm[under], na.rm = TRUE) else -Inf
  idx[(!is.na(h$bottom_cm[idx]) & h$bottom_cm[idx] <= deepest + 1e-6) | idx %in% stag]
}

#' The cambic horizon as WRB 2022 Ch 3.1.5 words it (p. 39-40), for the WRB
#' key and qualifiers (cambic()):
#' \itemize{
#'   \item a subsurface horizon (General description) of mineral material:
#'         the layer at the mineral soil surface and the A horizons are not
#'         part of it, as Hugo read "subsurface" for the argic family
#'         (.wrb_not_subsurface()); until v0.9.314 the layers starting above
#'         5 cm were left out instead;
#'   \item criterion 1, texture sandy loam or finer;
#'   \item criterion 2, soil aggregate structure (as before v0.9.314);
#'   \item criterion 4, layer by layer: a layer that forms part of one of the
#'         horizons listed (argic, ferralic, calcic, mollic, ...) or of a layer
#'         with andic properties is not cambic; until v0.9.314 an argic or
#'         ferralic horizon anywhere set the whole profile aside;
#'   \item criterion 5, a thickness of >= 15 cm, of the horizon: of the
#'         adjoining layers that meet criteria 1, 2 and 4.
#' }
#' Criterion 3 (evidence of soil formation) is not tested (Hugo, N9).
#' `min_top_cm`, when given, is asked of the layers as well. Each layer is
#' TRUE, FALSE or undecided on criteria 1 and 2; the result is undecided when
#' the undecided layers could make the 15 cm.
#' @noRd
.cambic_wrb2022 <- function(pedon, min_thickness = 15, min_top_cm = NULL) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.5, Cambic horizon (p. 39-40)"
  d <- as.character(h$designation %||% rep(NA_character_, n))
  sub <- !.wrb_not_subsurface(h) & .wrb_mineral_layer(h) &
         !(!is.na(d) & grepl(.WRB_ROCK_DESIGNATION, d)) &
         !is.na(h$top_cm) & !is.na(h$bottom_cm)
  if (!is.null(min_top_cm)) sub <- sub & h$top_cm >= min_top_cm
  subsurface <- which(sub)
  tests <- list()
  tests$subsurface <- .subtest_result(
    passed  = length(subsurface) > 0L, layers = subsurface,
    details = list(rule = "mineral, not the layer at the mineral soil surface, not an A horizon",
                   min_top_cm = min_top_cm))
  # Criterion 1: sandy loam or finer.
  tx <- .wrb_sandy_loam_or_finer(h, subsurface)
  tests$texture <- .subtest_result(
    passed  = if (any(tx %in% TRUE)) TRUE else if (anyNA(tx)) NA else FALSE,
    layers  = subsurface[tx %in% TRUE],
    missing = if (anyNA(tx)) c("sand_pct", "silt_pct", "clay_pct") else character(0),
    details = list(layers = subsurface, sandy_loam_or_finer = tx))
  # Criterion 2, as before v0.9.314: weak, moderate or strong structure that is
  # not massive or single grain. With no grade recorded: an aggregate type
  # recorded (blocky, granular, prismatic, ...), or a Bw, Bg, Bk, Bj, Bv, Bz or
  # Bx designation (v0.9.19); otherwise undecided.
  grade <- tolower(as.character(h$structure_grade[subsurface]))
  type  <- tolower(as.character(h$structure_type[subsurface]))
  nostr <- !is.na(type) & grepl("^massive$|^single grain$", type)
  st <- ifelse(!is.na(grade), grade %in% c("weak", "moderate", "strong") & !nostr,
               ifelse(nostr, FALSE,
                      ifelse(!is.na(type) |
                               (!is.na(d[subsurface]) &
                                  grepl("^B[wgkjvzx]|^B[wgkjvzx][0-9]", d[subsurface])), TRUE, NA)))
  tests$structure_development <- .subtest_result(
    passed  = if (any(st %in% TRUE)) TRUE else if (anyNA(st)) NA else FALSE,
    layers  = subsurface[st %in% TRUE],
    missing = if (anyNA(st)) "structure_grade" else character(0),
    details = list(layers = subsurface, grade = grade, type = type, structure = st))
  c12 <- ifelse(tx %in% FALSE | st %in% FALSE, FALSE,
                ifelse(tx %in% TRUE & st %in% TRUE, TRUE, NA))
  # Criterion 4, on the layers not ruled out by 1 and 2.
  cand <- subsurface[c12 %in% c(TRUE, NA)]
  part <- if (length(cand)) .wrb_cambic_part_of_other(pedon, cand)
          else list(excluded = rep(FALSE, n), by = rep("", n))
  out <- cand[part$excluded[cand]]
  status <- c12
  status[subsurface %in% out] <- FALSE
  yes <- subsurface[status %in% TRUE]
  maybe <- subsurface[is.na(status)]
  tests$not_part_of_other_horizon <- .subtest_result(
    passed  = length(yes) > 0L, layers = yes,
    details = list(excluded_layers = out, part_of = part$by[out]),
    notes   = if (length(out)) paste0("Layer ", out, " forms part of the ",
                                      part$by[out], " horizon", collapse = "; ")
              else NA_character_)
  # Criterion 5: the 15 cm of the adjoining layers.
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = yes, adjoining = TRUE)
  passed <- if (isTRUE(tests$thickness$passed)) TRUE
            else if (length(maybe) &&
                     isTRUE(test_minimum_thickness(h, min_cm = min_thickness,
                                                   candidate_layers = union(yes, maybe),
                                                   adjoining = TRUE)$passed)) NA
            else FALSE
  miss <- if (is.na(passed)) unique(c(tests$texture$missing, tests$structure_development$missing))
          else character(0)
  DiagnosticResult$new(name = "cambic", passed = passed,
                       layers = if (isTRUE(passed)) tests$thickness$layers else integer(0),
                       evidence = tests, missing = miss, reference = ref)
}

#' Cambisols RSG gate (WRB 2022 Ch 4, p. 122). Other soils having:
#' \enumerate{
#'   \item a cambic horizon starting <= 50 cm from the mineral soil surface
#'         and having its lower limit >= 25 cm from it; or
#'   \item an anthraquic, hydragric, irragric, plaggic, pretic or terric
#'         horizon; or
#'   \item a fragic, thionic or vertic horizon starting <= 100 cm from the
#'         mineral soil surface; or
#'   \item a tsitelic horizon with a texture class of sandy loam or finer,
#'         starting <= 50 cm from the mineral soil surface; or
#'   \item layers with andic or vitric properties with a combined thickness of
#'         >= 15 cm within 100 cm of the soil surface.
#' }
#' v0.9.314; until then the key asked for a cambic horizon at any depth.
#' Alternatives 2 and 4 are not tested: soilKey's tests of those horizons
#' read the designation or the colour alone (irragric() passes any Ap of 20
#' cm, terric() any Au, tsitelic() any red layer with >= 8\% clay), so they
#' would make a Cambisol of every Regosol with an Ap or a red layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
cambisol <- function(pedon) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  top0 <- suppressWarnings(min(h$top_cm, na.rm = TRUE))
  if (!is.finite(top0)) top0 <- 0
  starts_by <- function(r, depth) {
    if (!isTRUE(r$passed) || !length(r$layers)) return(r$passed %||% NA)
    any(h$top_cm[r$layers] - ms <= depth, na.rm = TRUE)
  }
  # 1. the cambic horizon by its depth
  cb <- cambic(pedon)
  runs <- if (isTRUE(cb$passed)) .wrb_runs(h, cb$layers) else list()
  ok1 <- Filter(function(r) r$top - ms <= 50 && r$bottom - ms >= 25, runs)
  alt <- list()
  alt$cambic <- if (length(ok1)) TRUE else if (isTRUE(cb$passed)) FALSE else cb$passed
  # 3. fragic, thionic or vertic starting <= 100 cm
  fr <- tryCatch(fragic(pedon), error = function(e) NULL)
  th <- tryCatch(thionic(pedon), error = function(e) NULL)
  vt <- tryCatch(vertic_horizon(pedon), error = function(e) NULL)
  alt$fragic_thionic_vertic <- .wrb_any3(lapply(list(fr, th, vt), function(r)
    if (is.null(r)) NA else starts_by(r, 100)))
  # 5. andic or vitric properties, >= 15 cm combined within 100 cm of the soil
  # surface
  an <- tryCatch(andic_properties(pedon), error = function(e) NULL)
  vi <- tryCatch(vitric_properties(pedon), error = function(e) NULL)
  av <- unique(c(if (isTRUE(an$passed)) an$layers, if (isTRUE(vi$passed)) vi$layers))
  av_cm <- sum(vapply(.wrb_runs(h, av), function(r)
    max(0, min(r$bottom, top0 + 100) - max(r$top, top0)), 0))
  alt$andic_vitric <- if (av_cm >= 15) TRUE
                      else if (is.na(an$passed %||% NA) || is.na(vi$passed %||% NA)) NA
                      else FALSE
  passed <- .wrb_any3(alt)
  layers <- if (isTRUE(alt$cambic)) unlist(lapply(ok1, `[[`, "layers")) else integer(0)
  DiagnosticResult$new(
    name = "cambisol", passed = passed, layers = as.integer(layers),
    evidence = list(alternatives = alt, cambic = cb, mineral_surface_cm = ms),
    missing = if (is.na(passed)) cb$missing %||% character(0) else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Cambisols (p. 122)")
}


# ---- Calcic horizon (WRB 2022 Ch 3.1.4, p. 37-38) ----------------------------

#' Criterion 2.b of the calcic horizon, per layer: "a calcium carbonate
#' equivalent of >= 5% higher (absolute ...) than that of an underlying layer
#' and no lithic discontinuity between the two layers" (WRB 2022 Ch 3.1.4,
#' p. 38). The underlying layers are those of the same material, read from the
#' designations' material numbers (2C under C is another material). TRUE when
#' one of them has >= 5% less CaCO3; FALSE when all of them are measured and
#' none has, or when the layer directly below is another material; NA when
#' nothing below the layer is described or measured. No other exemption: the
#' highly calcareous substrate of test_caco3_enrichment() (an underlying layer
#' with >= 40% CaCO3) is not in the text.
#' @noRd
.wrb_caco3_2b <- function(h, idx, min_delta = 5) {
  n <- nrow(h)
  mat <- .desg_material_number(h$designation %||% rep(NA_character_, n))
  ord <- order(h$top_cm, na.last = NA)
  vapply(idx, function(i) {
    if (is.na(h$caco3_pct[i]) || is.na(h$bottom_cm[i])) return(NA)
    below <- ord[h$top_cm[ord] >= h$bottom_cm[i] - 1e-6]
    if (!length(below)) return(NA)
    # stop at the first change of material (a lithic discontinuity)
    same <- integer(0)
    for (j in below) {
      if (!is.na(mat[i]) && !is.na(mat[j]) && mat[j] != mat[i]) break
      same <- c(same, j)
    }
    if (!length(same)) return(FALSE)
    v <- h$caco3_pct[same]
    if (any(h$caco3_pct[i] - v[!is.na(v)] >= min_delta)) TRUE
    else if (anyNA(v)) NA else FALSE
  }, NA)
}

#' The calcic horizon as WRB 2022 Ch 3.1.4 words it (p. 37-38), for the WRB key
#' and qualifiers (calcic()). Per layer:
#' \enumerate{
#'   \item a calcium carbonate equivalent of >= 15\% (\code{caco3_pct});
#'   \item a. protocalcic properties: secondary carbonates on >= 5\% of the
#'         exposed area (\code{secondary_carbonates_pct}, WRB 2022 Ch 3.2.8);
#'         or b. >= 5\% more CaCO3 than an underlying layer, with no lithic
#'         discontinuity between (.wrb_caco3_2b());
#'   \item not part of a petrocalcic horizon (\code{\link{petrocalcic}});
#'   \item a thickness of >= 15 cm, of the adjoining layers that meet 1-3.
#' }
#' A layer whose criterion 2 is undecided (secondary carbonates not recorded,
#' and the CaCO3 below it not measured or not lower) is undecided: carbonates
#' that are not recorded stay unknown (Hugo, 2026-10-29). Until v0.9.314 such a
#' layer was taken as calcic, and a layer over a layer with >= 40\% CaCO3
#' passed criterion 2.b.
#' @noRd
.calcic_wrb2022 <- function(pedon, min_thickness = 15, min_caco3_pct = 15) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.4, Calcic horizon (p. 37-38)"
  tests <- list()
  tests$caco3 <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  c1 <- tests$caco3$layers
  sc <- h$secondary_carbonates_pct %||% rep(NA_real_, n)
  c2a <- ifelse(is.na(sc[c1]), NA, sc[c1] >= 5)
  c2b <- .wrb_caco3_2b(h, c1)
  c2 <- ifelse(c2a %in% TRUE | c2b %in% TRUE, TRUE,
               ifelse(c2a %in% FALSE & c2b %in% FALSE, FALSE, NA))
  # Hugo (2026-10-29): with no secondary carbonates recorded, a layer with
  # CaCO3 >= 15% keeps meeting criterion 2 by the laboratory, as before the
  # 2022 reading; recorded secondary carbonates still decide it.
  c2[is.na(c2) & is.na(sc[c1])] <- TRUE
  tests$secondary <- .subtest_result(
    passed  = if (any(c2 %in% TRUE)) TRUE else if (anyNA(c2)) NA else FALSE,
    layers  = c1[c2 %in% TRUE],
    missing = if (anyNA(c2)) c("secondary_carbonates_pct", "caco3_pct") else character(0),
    details = list(layers = c1, protocalcic = c2a, enrichment = c2b,
                   undecided = c1[is.na(c2)]))
  pc <- petrocalcic(pedon)
  petro <- if (isTRUE(pc$passed)) pc$layers else integer(0)
  c3 <- setdiff(tests$secondary$layers, petro)
  tests$not_petrocalcic <- .subtest_result(
    passed = length(c3) > 0L, layers = c3,
    details = list(petrocalcic_layers = petro))
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = c3,
                                            adjoining = TRUE)
  # Undecided: no CaCO3 measured, or layers whose criterion 2 is undecided
  # that would make a horizon of >= 15 cm with the layers that pass.
  und <- setdiff(c1[is.na(c2)], petro)
  passed <- if (isTRUE(tests$thickness$passed)) TRUE
            else if (is.na(tests$caco3$passed)) NA
            else if (length(und) &&
                     isTRUE(test_minimum_thickness(h, min_cm = min_thickness,
                                                   candidate_layers = union(c3, und),
                                                   adjoining = TRUE)$passed)) NA
            else FALSE
  DiagnosticResult$new(name = "calcic", passed = passed,
                       layers = if (isTRUE(passed)) tests$thickness$layers else integer(0),
                       evidence = tests,
                       missing = if (is.na(passed)) unique(c(tests$caco3$missing, tests$secondary$missing))
                                 else character(0),
                       reference = ref)
}

# Does a layer contain secondary carbonates (TRUE / FALSE / NA)? Recorded
# secondary carbonates (secondary_carbonates_pct > 0, or 0 for none), or the
# suffix k of the designation ("Accumulation of secondary carbonates", WRB
# 2022 Ch 10.2, Table 10.2, p. 227). A k does not say how much, so it is not
# read as protocalcic properties (>= 5% of the exposed area) in calcic().
.wrb_has_secondary_carbonates <- function(h, idx) {
  n <- nrow(h)
  sc <- h$secondary_carbonates_pct %||% rep(NA_real_, n)
  d <- .desg_master(h$designation %||% rep(NA_character_, n))
  k <- !is.na(d) & grepl("^[A-Z]+[a-z]*k", d)
  vapply(idx, function(i) if (k[i] || (!is.na(sc[i]) && sc[i] > 0)) TRUE
                          else if (!is.na(sc[i])) FALSE else NA, NA)
}

# The same for secondary gypsum: visible gypsum of pedogenic origin
# (gypsum_visible_pct) or the suffix y ("Accumulation of secondary gypsum",
# WRB 2022 Ch 10.2, Table 10.2, p. 228).
.wrb_has_secondary_gypsum <- function(h, idx) {
  n <- nrow(h)
  gv <- h$gypsum_visible_pct %||% rep(NA_real_, n)
  d <- .desg_master(h$designation %||% rep(NA_character_, n))
  y <- !is.na(d) & grepl("^[A-Z]+[a-z]*y", d)
  vapply(idx, function(i) if (y[i] || (!is.na(gv[i]) && gv[i] > 0)) TRUE
                          else if (!is.na(gv[i])) FALSE else NA, NA)
}

#' The Gypsisols and Calcisols gates (WRB 2022 Ch 4, p. 116-117): a horizon of
#' accumulation (`main` or `petro`) starting <= 100 cm from the mineral soil
#' surface, and no argic horizon starting above or at its upper limit, unless
#' the argic horizon contains the secondary accumulation throughout
#' (`contains`, per layer TRUE / FALSE / NA).
#' @noRd
.wrb_accumulation_gate <- function(pedon, main, petro, contains, name, ref) {
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lay <- unique(c(if (isTRUE(main$passed)) main$layers,
                  if (isTRUE(petro$passed)) petro$layers))
  lay <- lay[!is.na(h$top_cm[lay]) & h$top_cm[lay] - ms <= 100]
  ev <- list(horizon = main, petro = petro, mineral_surface_cm = ms)
  if (!length(lay)) {
    # Undecided when the horizon is undecided and nothing else passed.
    deep <- isTRUE(main$passed) || isTRUE(petro$passed)
    passed <- if (deep) FALSE else if (is.na(main$passed) || is.na(petro$passed)) NA else FALSE
    return(DiagnosticResult$new(name = name, passed = passed, layers = integer(0),
                                evidence = ev,
                                missing = if (is.na(passed)) unique(c(main$missing, petro$missing)) else character(0),
                                reference = ref))
  }
  top <- min(h$top_cm[lay])
  arg <- argic(pedon)
  ev$argic <- arg
  passed <- TRUE
  if (isTRUE(arg$passed) && length(arg$layers) &&
      min(h$top_cm[arg$layers], na.rm = TRUE) <= top + 1e-6) {
    th <- contains(h, arg$layers)
    ev$argic_contains_throughout <- th
    passed <- if (all(th %in% TRUE)) TRUE else if (any(th %in% FALSE)) FALSE else NA
  } else if (is.na(arg$passed)) {
    # An undecided argic horizon could start above: the gate is undecided.
    passed <- NA
  }
  miss <- if (!is.na(passed)) character(0)
          else if (is.na(arg$passed)) arg$missing %||% "argic"
          else c("secondary_carbonates_pct", "designation")
  DiagnosticResult$new(name = name, passed = passed,
                       layers = if (isTRUE(passed)) as.integer(lay) else integer(0),
                       evidence = ev, missing = miss, reference = ref)
}

#' Gypsisols RSG gate (WRB 2022 Ch 4, p. 116): "1. a gypsic or petrogypsic
#' horizon starting <= 100 cm from the mineral soil surface; and 2. no argic
#' horizon starting above or at the upper limit of the gypsic or petrogypsic
#' horizon, unless the argic horizon contains secondary gypsum or secondary
#' carbonates, throughout". v0.9.314; until then the key asked for a gypsic
#' horizon alone.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
gypsisol <- function(pedon) {
  contains <- function(h, idx) {
    g <- .wrb_has_secondary_gypsum(h, idx); k <- .wrb_has_secondary_carbonates(h, idx)
    ifelse(g %in% TRUE | k %in% TRUE, TRUE, ifelse(g %in% FALSE & k %in% FALSE, FALSE, NA))
  }
  .wrb_accumulation_gate(pedon, gypsic(pedon), petrogypsic(pedon), contains,
                         "gypsisol", "IUSS Working Group WRB (2022), Chapter 4, Gypsisols (p. 116)")
}

#' Calcisols RSG gate (WRB 2022 Ch 4, p. 117): "1. a calcic or petrocalcic
#' horizon starting <= 100 cm from the mineral soil surface; and 2. no argic
#' horizon starting above or at the upper limit of the calcic or petrocalcic
#' horizon unless the argic horizon contains secondary carbonates,
#' throughout". v0.9.314; until then the key asked for a calcic horizon alone.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
calcisol <- function(pedon) {
  .wrb_accumulation_gate(pedon, calcic(pedon), petrocalcic(pedon),
                         .wrb_has_secondary_carbonates,
                         "calcisol", "IUSS Working Group WRB (2022), Chapter 4, Calcisols (p. 117)")
}


# ---- Fluvic material (WRB 2022 Ch 3.3.6, p. 87) ------------------------------

# The recorded origins that are fluviatile, marine or lacustrine (rock_origin,
# layer_origin), as .q_fluvic_layers() reads them.
.WRB_WATER_ORIGIN <- "fluv|marine|lacustr|alluv"

#' Criterion 1 of fluvic material per layer: "is of fluviatile, marine or
#' lacustrine origin". TRUE when the origin recorded for the layer
#' (rock_origin, layer_origin) is one of them, FALSE when an origin recorded
#' for it is another, NA when none is recorded. A layer with no origin of its
#' own takes the parent material of the site when that names one of them
#' ("Holocene alluvium", "Unconsolidated fluvial rock"); a parent material
#' that names none is free text and is not read as another origin.
#' @noRd
.wrb_water_origin <- function(h, site = list()) {
  n <- nrow(h)
  ro <- tolower(as.character(h$rock_origin %||% rep(NA_character_, n)))
  lo <- tolower(as.character(h$layer_origin %||% rep(NA_character_, n)))
  rec <- !is.na(ro) | !is.na(lo)
  other <- (!is.na(ro) & !grepl(.WRB_WATER_ORIGIN, ro)) |
           (!is.na(lo) & !grepl(.WRB_WATER_ORIGIN, lo))
  out <- ifelse(!rec, NA, !other)
  pm <- tolower(paste(as.character(unlist(site$parent_material %||% NA)), collapse = " "))
  if (grepl(.WRB_WATER_ORIGIN, pm)) out[is.na(out)] <- TRUE
  out
}

#' Fluvic material as WRB 2022 Ch 3.3.6 words it (p. 87), layer by layer, in
#' the mineral layers starting above `max_top_cm`:
#' \enumerate{
#'   \item fluviatile, marine or lacustrine origin (.wrb_water_origin(): the
#'         layer's rock_origin or layer_origin, else the site's parent
#'         material);
#'   \item strata, one or both of: a. obvious, read from the texture (Field
#'         identification: "variation in texture"): an interior layer where
#'         the clay changes by \code{min_clay_swing} or more (as before
#'         v0.9.314), or the sand by \code{min_sand_swing} or more, in one
#'         direction above and in the other below; b. two or more layers with
#'         >= 0.2\% SOC and >= 25\%
#'         (relative) and >= 0.2\% (absolute) more SOC than the directly
#'         overlying layer, not part of a natric or spodic horizon (nor a Bh
#'         or Bs horizon, as before);
#'   \item a single grain, massive, platy or weak subangular blocky structure,
#'         or a granular or subangular blocky one in a layer of 2.b.
#' }
#' Until v0.9.314 2.a and 2.b were both required, the origin and the structure
#' were not read, and every layer above 100 cm was returned. Undecided parts
#' (origin, clay, SOC or structure not recorded) make the result NA.
#' @noRd
.fluvic_material_wrb2022 <- function(pedon, max_top_cm = 100, min_clay_swing = 8,
                                     min_sand_swing = 15) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.3.6, Fluvic material (p. 87)"
  d <- as.character(h$designation %||% rep(NA_character_, n))
  rock <- !is.na(d) & grepl(.WRB_ROCK_DESIGNATION, d)
  cl <- which(.wrb_mineral_layer(h) & !rock & !is.na(h$top_cm) & h$top_cm < max_top_cm)
  cl <- cl[order(h$top_cm[cl])]
  res <- function(passed, layers = integer(0), missing = character(0), ...)
    DiagnosticResult$new(name = "fluvic_material", passed = passed,
                         layers = as.integer(layers), evidence = list(...),
                         missing = missing, reference = ref)
  if (length(cl) < 2L) return(res(FALSE, notes = "fewer than 2 mineral layers"))
  # 1. origin
  c1 <- .wrb_water_origin(h, pedon$site)
  # 2.a obvious strata: a reversal of the clay or of the sand content
  reversal <- function(x, swing) {
    dd <- diff(x[!is.na(x)])
    length(dd) >= 2L && any(abs(dd[-length(dd)]) >= swing & abs(dd[-1L]) >= swing &
                              sign(dd[-length(dd)]) * sign(dd[-1L]) < 0)
  }
  rev_a <- reversal(h$clay_pct[cl], min_clay_swing) ||
           reversal(h$sand_pct[cl], min_sand_swing)
  c2a <- if (rev_a) TRUE else if (anyNA(h$clay_pct[cl])) NA else FALSE
  # 2.b SOC higher than in the directly overlying layer
  nat <- tryCatch(natric_horizon(pedon), error = function(e) NULL)
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  ns_layers <- c(if (isTRUE(nat$passed)) nat$layers, if (isTRUE(spo$passed)) spo$layers)
  bhs <- !is.na(d) & grepl("^[0-9]*B[a-z]*[hs]", d)
  oc <- h$oc_pct
  b_layer <- vapply(cl, function(i) {
    if (i %in% ns_layers || bhs[i]) return(FALSE)
    j <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[i]) < 1e-6)
    if (!length(j)) return(FALSE)
    a <- oc[i]; b <- oc[j[1L]]
    if (is.na(a) || is.na(b)) return(NA)
    a >= 0.2 && a >= 1.25 * b && a - b >= 0.2
  }, NA)
  nb <- sum(b_layer %in% TRUE)
  c2b <- if (nb >= 2L) TRUE else if (nb + sum(is.na(b_layer)) >= 2L) NA else FALSE
  c2 <- .wrb_any3(list(c2a, c2b))
  # 3. structure, per layer
  grade <- tolower(as.character(h$structure_grade[cl]))
  type  <- tolower(as.character(h$structure_type[cl]))
  grade[grade %in% "na"] <- NA; type[type %in% "na"] <- NA
  loose <- grepl("single grain|massive|platy|structureless", paste(grade, type)) |
           (grade %in% "weak" & grepl("subangular", type))
  gran_sab <- !is.na(type) & grepl("granular|subangular", type)
  c3 <- ifelse(loose, TRUE,
               ifelse(gran_sab & b_layer %in% TRUE, TRUE,
                      ifelse(is.na(grade) & is.na(type), NA, FALSE)))
  # the layers: origin not ruled out, structure of criterion 3
  per <- ifelse(c1[cl] %in% FALSE | c3 %in% FALSE, FALSE,
                ifelse(c1[cl] %in% TRUE & c3 %in% TRUE, TRUE, NA))
  passed <- if (isFALSE(c2) || !any(per %in% c(TRUE, NA))) FALSE
            else if (isTRUE(c2) && any(per %in% TRUE)) TRUE
            else NA
  miss <- if (is.na(passed))
            unique(c(if (anyNA(c1[cl])) c("rock_origin", "layer_origin"),
                     if (is.na(c2)) c("clay_pct", "oc_pct"),
                     if (anyNA(c3)) c("structure_grade", "structure_type")))
          else character(0)
  res(passed, layers = if (isTRUE(passed)) cl[per %in% TRUE] else integer(0),
      missing = miss,
      origin = stats::setNames(c1[cl], cl), strata_obvious = c2a,
      strata_soc = c2b, soc_layers = cl[b_layer %in% TRUE],
      structure = stats::setNames(c3, cl), undecided_layers = cl[is.na(per)])
}

#' Fluvisols RSG gate (WRB 2022 Ch 4, p. 123): "fluvic material: 1. >= 25 cm
#' thick and starting <= 25 cm from the mineral soil surface; or 2. from the
#' lower limit of a plough layer, <= 40 cm thick, to a depth of >= 50 cm from
#' the mineral soil surface". v0.9.314; until then the key asked for fluvic
#' material at any depth.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
fluvisol <- function(pedon) {
  fm <- fluvic_material(pedon)
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Fluvisols (p. 123)"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  if (!isTRUE(fm$passed))
    return(DiagnosticResult$new(name = "fluvisol", passed = fm$passed,
                                layers = integer(0), evidence = list(fluvic = fm),
                                missing = fm$missing %||% character(0), reference = ref))
  runs <- .wrb_runs(h, fm$layers)
  ok1 <- Filter(function(r) r$top - ms <= 25 && r$bottom - max(r$top, ms) >= 25, runs)
  # 2. below a plough layer (an Ap from the mineral soil surface) <= 40 cm thick
  d <- .desg_master(h$designation %||% rep(NA_character_, nrow(h)))
  ap <- Filter(function(r) r$top <= ms + 1e-6,
               .wrb_runs(h, which(!is.na(d) & grepl("^Ap", d))))
  ap_bot <- if (length(ap)) ap[[1L]]$bottom else NA_real_
  ok2 <- if (!is.na(ap_bot) && ap_bot - ms <= 40)
           Filter(function(r) abs(r$top - ap_bot) < 1e-6 && r$bottom - ms >= 50, runs)
         else list()
  ok <- c(ok1, ok2)
  passed <- if (length(ok)) TRUE
            else if (length(fm$evidence$undecided_layers)) NA else FALSE
  DiagnosticResult$new(name = "fluvisol", passed = passed,
                       layers = if (isTRUE(passed)) unique(unlist(lapply(ok, `[[`, "layers"))) else integer(0),
                       evidence = list(fluvic = fm, mineral_surface_cm = ms,
                                       plough_layer_bottom_cm = ap_bot),
                       missing = if (is.na(passed)) fm$missing %||% character(0) else character(0),
                       reference = ref)
}


# ---- Arenosols (WRB 2022 Ch 4, p. 124) ---------------------------------------

#' The texture test of the Arenosols, as WRB 2022 Ch 4 words it (p. 124):
#' "having within 100 cm of the mineral soil surface: 1. a weighted average
#' texture class of loamy sand or sand; and 2. layers of finer texture, if
#' present, with a combined thickness of < 15 cm; and 3. layers with >= 40\%
#' (by volume, related to the whole soil) coarse fragments, if present, with a
#' combined thickness of < 15 cm". Loamy sand or sand is silt + 2 x clay < 30
#' (Annex 1 texture triangle). The 100 cm are those of the mineral layers from
#' the mineral soil surface down to continuous rock; a part whose texture is
#' not recorded, or that is not described, is unknown, and the result is
#' undecided when the unknown part could change it. Criterion 3 reads the
#' layers whose coarse fragments are recorded ("if present"): a layer with no
#' record of them does not make the result undecided (a reading for Hugo).
#' Until v0.9.314 every layer above 100 cm had to be recorded and coarser
#' than sandy loam, and coarse fragments were not read.
#' @noRd
test_arenosol_texture <- function(h, max_depth_cm = 100) {
  n <- nrow(h)
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) return(.subtest_result(passed = NA, missing = c("top_cm", "bottom_cm")))
  d <- as.character(h$designation %||% rep(NA_character_, n))
  rock <- !is.na(d) & grepl(.WRB_ROCK_DESIGNATION, d)
  rock_top <- suppressWarnings(min(h$top_cm[rock], na.rm = TRUE))
  lo <- ms
  hi <- min(ms + max_depth_cm, if (is.finite(rock_top)) rock_top else Inf)
  if (hi <= lo)
    return(.subtest_result(passed = FALSE, notes = "continuous rock at the mineral soil surface"))
  w <- pmax(pmin(h$bottom_cm, hi) - pmax(h$top_cm, lo), 0)
  w[is.na(w) | !.wrb_mineral_layer(h) | rock] <- 0
  silt <- ifelse(is.na(h$silt_pct), 100 - h$sand_pct - h$clay_pct, h$silt_pct)
  s2c <- silt + 2 * h$clay_pct
  known <- w > 0 & !is.na(s2c)
  fine <- known & s2c >= 30
  cf <- h$coarse_fragments_pct
  stony <- w > 0 & !is.na(cf) & cf >= 40
  covered <- sum(vapply(.wrb_runs(h, which(w > 0)), function(r)
    max(0, min(r$bottom, hi) - max(r$top, lo)), 0))
  # Hugo (2026-10-29): a profile described above 100 cm is decided on the part
  # that is described; depths not described do not keep it undecided
  undescribed <- 0
  unk_tex <- sum(w[w > 0 & is.na(s2c)]) + undescribed
  unk_cf  <- sum(w[w > 0 & is.na(cf)]) + undescribed
  if (!any(known))
    return(.subtest_result(passed = NA, missing = c("sand_pct", "silt_pct", "clay_pct")))
  wa <- sum(s2c[known] * w[known]) / sum(w[known])
  fine_cm <- sum(w[fine]); stony_cm <- sum(w[stony])
  details <- list(window_cm = c(lo, hi), weighted_silt_2clay = wa, finer_cm = fine_cm,
                  coarse_fragment_cm = stony_cm, unknown_texture_cm = unk_tex,
                  unknown_coarse_fragments_cm = unk_cf)
  if (wa >= 30 || fine_cm >= 15 || stony_cm >= 15)
    return(.subtest_result(passed = FALSE, details = details))
  # The unknown part: finer or stony layers of >= 15 cm in all, or enough finer
  # material to lift the weighted average to sandy loam.
  max_wa <- (sum(s2c[known] * w[known]) + 200 * unk_tex) / (sum(w[known]) + unk_tex)
  und <- fine_cm + unk_tex >= 15 || max_wa >= 30
  .subtest_result(passed = if (und) NA else TRUE,
                  layers = which(known & !fine),
                  missing = if (und) c("sand_pct", "silt_pct", "clay_pct") else character(0),
                  details = details)
}
