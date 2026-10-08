# =============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5) MISSING in v0.9.62 -- v0.9.63 batch.
#
# This file implements the Tier-1 missing qualifiers identified by the
# v0.9.62 audit (`audit_wrb_canonical_v0962_2026-05-08.md`):
#
#   - 33 missing principal qualifiers (PQs)
#   - 68 missing supplementary qualifiers (SQs)
#
# v0.9.63 ships a batch of ~20 PQs + SQs that map cleanly to existing
# soilKey horizon attributes or to existing diagnostics. The remaining
# qualifiers (composite -- multiple existing primitives -- and
# new-primitive ones) are tracked for v0.9.64+.
#
# Each function returns a `DiagnosticResult` per the established
# `qual_<Name>` convention from `R/qualifiers-wrb2022.R`.
#
# References: IUSS Working Group WRB (2022). World Reference Base for
# Soil Resources, 4th edition. Chapter 5 (qualifiers).
# =============================================================================


# ---------- Helpers (private) ------------------------------------------------

#' Test "X within depth d cm" given an existing diagnostic
#'
#' Many WRB sub-qualifiers (Endo-, Bathy-, Hyper-, Pano-, Ortho-,
#' Ano-, etc.) are depth-bounded modifiers of an existing principal
#' qualifier or diagnostic horizon. This helper tests whether the
#' base diagnostic fires AND has any of its passing layers in the
#' given depth window.
#'
#' @noRd
.q_within_depth <- function(name, base_diag,
                                pedon, top_cm, bottom_cm) {
  if (!isTRUE(base_diag$passed)) {
    return(DiagnosticResult$new(
      name = name, passed = FALSE, layers = integer(0),
      evidence = list(base = base_diag,
                        depth_window = c(top_cm, bottom_cm)),
      missing = base_diag$missing %||% character(0),
      reference = sprintf("WRB (2022) Ch 5, %s", name)
    ))
  }
  h <- pedon$horizons
  in_window <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                       h$bottom_cm > top_cm & h$top_cm < bottom_cm)
  ok_layers <- intersect(base_diag$layers, in_window)
  passed <- length(ok_layers) > 0L
  DiagnosticResult$new(
    name = name, passed = passed, layers = ok_layers,
    evidence = list(base = base_diag,
                      depth_window = c(top_cm, bottom_cm),
                      n_layers_in_window = length(ok_layers)),
    missing = if (length(ok_layers) == 0L && length(base_diag$missing) > 0L)
                base_diag$missing else character(0),
    reference = sprintf("WRB (2022) Ch 5, %s", name)
  )
}


#' Volume-weighted mean of a horizon attribute over a depth window
#' @noRd
.q_weighted_mean <- function(values, top, bottom,
                                window_top = 0, window_bottom = 100) {
  ok <- !is.na(values) & !is.na(top) & !is.na(bottom) & bottom > top
  if (!any(ok)) return(NA_real_)
  values <- values[ok]; top <- top[ok]; bottom <- bottom[ok]
  overlap <- pmax(0, pmin(bottom, window_bottom) - pmax(top, window_top))
  if (sum(overlap) == 0) return(NA_real_)
  sum(values * overlap) / sum(overlap)
}


# ============================================================================
# PRINCIPAL QUALIFIERS (PQ) -- v0.9.63 batch
# ============================================================================


# v0.9.217: a WRB "layer, >= x cm thick" may be made of several described
# horizons, so the qualifying horizons are merged into contiguous runs before
# the thickness is measured. Used by the Chapter 5 rewrites in this file.

# Contiguous runs of the horizons `idx`, in depth order; each run is a list of
# top, bottom and layers.
.q_layer_runs <- function(h, idx) {
  idx <- idx[!is.na(h$top_cm[idx]) & !is.na(h$bottom_cm[idx])]
  if (!length(idx)) return(list())
  idx <- idx[order(h$top_cm[idx])]
  runs <- list()
  cur <- idx[1L]; bot <- h$bottom_cm[idx[1L]]
  for (i in idx[-1L]) {
    if (h$top_cm[i] <= bot + 1e-6) {
      cur <- c(cur, i); bot <- max(bot, h$bottom_cm[i])
    } else {
      runs[[length(runs) + 1L]] <- list(top = min(h$top_cm[cur]), bottom = bot,
                                        layers = cur)
      cur <- i; bot <- h$bottom_cm[i]
    }
  }
  runs[[length(runs) + 1L]] <- list(top = min(h$top_cm[cur]), bottom = bot,
                                    layers = cur)
  runs
}

# Runs of `idx` that are >= min_thk cm thick and start <= max_top cm.
.q_thick_runs <- function(h, idx, min_thk, max_top = Inf) {
  Filter(function(r) r$bottom - r$top >= min_thk && r$top <= max_top,
         .q_layer_runs(h, idx))
}

# Ch 2.3.1: "Subqualifiers related to depth requirements are only used if the
# relevant soil characteristics are reported until >= 100 cm of the (mineral)
# soil surface or to a limiting layer, whichever is shallower."
.q_reported_to_100 <- function(pedon, lim = .barrier_top_cm(pedon)) {
  depth <- if (is.na(lim)) 100 else min(100, lim)
  isTRUE(suppressWarnings(max(pedon$horizons$bottom_cm, na.rm = TRUE)) >= depth)
}


#' Coarsic qualifier (cs): < 20\% fine earth averaged over 0-75 cm
#'
#' WRB 2022 Ch 5: < 20\% (by volume) fine earth plus dead plant residues,
#' averaged over 75 cm from the soil surface or to a limiting layer starting
#' > 25 cm, whichever is shallower.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_coarsic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Coarsic: "having < 20% (by volume, related to the
  # whole soil) fine earth plus dead plant residues of any size, averaged over
  # a depth of 75 cm from the soil surface or to a limiting layer starting
  # > 25 cm from the soil surface, whichever is shallower". Was >= 70% coarse
  # fragments over 0-100 cm. Only guaranteed readings now: coarse fragments
  # > 80% leave < 20% for fine earth (TRUE); coarse fragments plus artefacts
  # <= 80% leave >= 20% (FALSE; unrecorded artefacts count as none). In between
  # the fine-earth share is unknown (NA), as it is when a limiting layer starts
  # <= 25 cm: the average then runs through it, where fine earth is undefined.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Coarsic"
  lim <- .barrier_top_cm(pedon)
  if (!is.na(lim) && lim <= 25) {
    return(DiagnosticResult$new(
      name = "Coarsic", passed = NA, layers = integer(0),
      evidence = list(limiting_layer_top_cm = lim,
                      reason = "limiting layer starts <= 25 cm; the 0-75 cm average runs through it"),
      missing = "fine earth share below the limiting layer", reference = ref))
  }
  wbot <- if (is.na(lim)) 75 else min(75, lim)
  cf <- h$coarse_fragments_pct
  ov <- pmax(0, pmin(h$bottom_cm, wbot) - pmax(h$top_cm, 0))
  ov[is.na(ov)] <- 0
  covered <- if (is.null(cf)) 0 else sum(ov[!is.na(cf)])
  if (covered < wbot) {
    return(DiagnosticResult$new(
      name = "Coarsic", passed = NA, layers = integer(0),
      evidence = list(window_cm = c(0, wbot), covered_cm = covered),
      missing = "coarse_fragments_pct", reference = ref))
  }
  art <- h$artefacts_pct %||% rep(NA_real_, nrow(h))
  art[is.na(art)] <- 0
  mean_cf  <- .q_weighted_mean(cf, h$top_cm, h$bottom_cm, 0, wbot)
  mean_cfa <- .q_weighted_mean(cf + art, h$top_cm, h$bottom_cm, 0, wbot)
  passed <- if (mean_cf > 80) TRUE else if (mean_cfa <= 80) FALSE else NA
  DiagnosticResult$new(
    name = "Coarsic", passed = passed,
    layers = if (isTRUE(passed)) which(ov > 0) else integer(0),
    evidence = list(window_cm = c(0, wbot), mean_coarse_fragments_pct = mean_cf,
                    mean_coarse_plus_artefacts_pct = mean_cfa,
                    limiting_layer_top_cm = lim),
    missing = if (is.na(passed)) "fine earth share (artefacts > 2 mm, interstices)"
              else character(0),
    reference = ref
  )
}


#' Fractic qualifier (fc): broken-up petrocalcic or petrogypsic horizon
#'
#' WRB 2022 Ch 5: a layer >= 10 cm thick, starting <= 100 cm, of the remnants
#' of a broken-up petrocalcic or petrogypsic horizon.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_fractic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Fractic: "having a layer, >= 10 cm thick and
  # starting <= 100 cm from the mineral soil surface, consisting of a broken-up
  # petrocalcic or petrogypsic horizon, the remnants of which: occupy >= 40%
  # (by volume, related to the whole soil), and have an average horizontal
  # length of < 10 cm and/or occupy < 80%". The old code passed on any
  # shrink-swell crack (cracks_width_cm / cracks_depth_cm), which is not a
  # broken-up cemented horizon. The schema holds neither the volume nor the
  # size of such remnants (coarse_fragments_pct does not include them), so the
  # result is NA.
  DiagnosticResult$new(
    name = "Fractic", passed = NA, layers = integer(0),
    evidence = list(reason = "no field for remnants of a broken-up petrocalcic or petrogypsic horizon"),
    missing = c("broken_petro_remnants_pct", "broken_petro_remnants_length_cm"),
    reference = "WRB (2022) Ch 5, Fractic"
  )
}


#' Gibbsic qualifier (gi): >= 25\% gibbsite in the clay fraction
#'
#' WRB 2022 Ch 5: a layer >= 30 cm thick, starting <= 100 cm, with >= 25\%
#' gibbsite in the clay fraction.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gibbsic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Gibbsic: "having a layer, >= 30 cm thick and
  # starting <= 100 cm from the mineral soil surface, containing >= 25%
  # gibbsite in the clay fraction". The old code read Al2O3 from sulfuric
  # attack >= 25% (averaged over 0-100 cm) as gibbsite. That attack also
  # dissolves the Al of kaolinite, so most clayey Ferralsols would have passed.
  # Gibbsite needs a mineralogical measurement (XRD, thermal analysis) the
  # horizon schema does not hold: NA.
  DiagnosticResult$new(
    name = "Gibbsic", passed = NA, layers = integer(0),
    evidence = list(reason = "no gibbsite content in the horizon schema; Al2O3 by sulfuric attack is not gibbsite"),
    missing = "gibbsite_clay_fraction_pct",
    reference = "WRB (2022) Ch 5, Gibbsic"
  )
}


#' Ferritic qualifier (fe): a layer with >= 10\% Fedith
#'
#' WRB 2022 Ch 5: a layer >= 30 cm thick, starting <= 100 cm, with >= 10\%
#' Fedith, outside plinthic, pisoplinthic and petroplinthic horizons.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_ferritic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Ferritic: "having a layer, >= 30 cm thick and
  # starting <= 100 cm from the mineral soil surface, with >= 10% Fedith and
  # not forming part of a petroplinthic, pisoplinthic or plinthic horizon".
  # Was a 0-100 cm weighted mean of fe_dcb_pct >= 18 (an Fe2O3 figure), with
  # no thickness and no plinthic exclusion. Now every horizon of a >= 30 cm
  # run has fe_dcb_pct (Fedith, % Fe) >= 10, outside those three horizons.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Ferritic"
  fe <- h$fe_dcb_pct
  if (is.null(fe) || all(is.na(fe))) {
    return(DiagnosticResult$new(
      name = "Ferritic", passed = NA, layers = integer(0),
      evidence = list(reason = "no fe_dcb_pct data"),
      missing = "fe_dcb_pct", reference = ref))
  }
  pl <- list(plinthic = plinthic(pedon), pisoplinthic = pisoplinthic(pedon),
             petroplinthic = petroplinthic(pedon))
  excl <- unique(unlist(lapply(pl, function(d)
    if (isTRUE(d$passed)) d$layers else integer(0))))
  pl_unknown <- any(vapply(pl, function(d) is.na(d$passed), logical(1)))
  hit   <- setdiff(which(!is.na(fe) & fe >= 10), excl)
  maybe <- setdiff(which(is.na(fe) | fe >= 10), excl)
  runs <- .q_thick_runs(h, hit, 30, 100)
  passed <- if (length(runs) && !pl_unknown) TRUE
            else if (length(runs) || length(.q_thick_runs(h, maybe, 30, 100))) NA
            else FALSE
  DiagnosticResult$new(
    name = "Ferritic", passed = passed,
    layers = if (isTRUE(passed)) unlist(lapply(runs, `[[`, "layers")) else integer(0),
    evidence = list(fe_dcb_pct = fe, threshold = 10, min_thickness_cm = 30,
                    plinthic_layers_excluded = excl),
    missing = if (!is.na(passed)) character(0)
              else if (length(runs)) unique(unlist(lapply(pl, function(d) d$missing)))
              else "fe_dcb_pct",
    reference = ref
  )
}


#' Greyzemic qualifier (gz): uncoated grains in the lower mollic horizon
#'
#' WRB 2022 Ch 5: uncoated sand and/or coarse silt grains on aggregate
#' surfaces in the lower half of a mollic horizon.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_greyzemic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Greyzemic: "having uncoated sand and/or coarse
  # silt grains on soil aggregate surfaces in the lower half of a mollic
  # horizon". The old code looked for a thin bleached layer above the mollic
  # horizon, which is not the criterion. The schema records no observation of
  # uncoated grains on ped faces: FALSE without a mollic horizon, NA with one.
  h <- pedon$horizons
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(DiagnosticResult$new(
      name = "Greyzemic", passed = mol$passed, layers = integer(0),
      evidence = list(mollic = mol),
      missing = mol$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Greyzemic"
    ))
  }
  mtop <- min(h$top_cm[mol$layers], na.rm = TRUE)
  mbot <- max(h$bottom_cm[mol$layers], na.rm = TRUE)
  DiagnosticResult$new(
    name = "Greyzemic", passed = NA, layers = integer(0),
    evidence = list(mollic = mol, lower_half_cm = c((mtop + mbot) / 2, mbot)),
    missing = "uncoated_grains_on_ped_faces",
    reference = "WRB (2022) Ch 5, Greyzemic"
  )
}


# ---- v0.9.217 helpers for the Chapter 5 rewrites below ---------------------

# Per-layer organic material (WRB 2022 Ch 3.3.13) as TRUE / FALSE / NA: from
# organic_material() where oc_pct is known; without it, an O or H master
# horizon, which consists of organic material by definition.
.q_organic_layers <- function(pedon) {
  h <- pedon$horizons
  om <- organic_material(pedon)$layers %||% integer(0)
  out <- rep(NA, nrow(h))
  known <- !is.na(h$oc_pct)
  out[known] <- which(known) %in% om
  d <- h$designation
  out[!known & !is.na(d) & grepl("^[0-9]*[OH]", d)] <- TRUE
  out
}

# The layer starting at depth `d` (directly below it) and the layer ending at
# `d` (directly above it), or NA.
.q_layer_at <- function(h, d) {
  i <- which(!is.na(h$top_cm) & abs(h$top_cm - d) < 1e-6)
  if (length(i)) i[1L] else NA_integer_
}
.q_layer_ending <- function(h, d) {
  i <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - d) < 1e-6)
  if (length(i)) i[1L] else NA_integer_
}

# Organic material starting at the soil surface (Muusic, Rockic, Mawic):
# status (TRUE, else the FALSE / NA of the uppermost layer), its layers, its
# lower limit and the layer directly below it.
.q_organic_from_surface <- function(pedon) {
  h <- pedon$horizons
  org <- .q_organic_layers(pedon)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord])]
  ly <- integer(0)
  for (i in ord) if (isTRUE(org[i])) ly <- c(ly, i) else break
  if (!length(ly))
    return(list(status = if (length(ord)) org[ord[1L]] else NA,
                layers = integer(0), bottom = NA_real_, below = NA_integer_))
  bottom <- max(h$bottom_cm[ly])
  list(status = TRUE, layers = ly, bottom = bottom,
       below = .q_layer_at(h, bottom))
}

# Technic hard material (WRB 2022 Ch 3.3.18) with positive evidence. A strongly
# cemented layer may be a natural petrocalcic or petroduric horizon: it counts
# only when recorded as >= 95% artefacts (technic hard material is artefacts,
# Ch 3.3.2, and continuous); otherwise it is `unsure` (technic_hard_material()
# itself no longer reads cementation since v0.9.220). A designation counts when it
# names asphalt, concrete or cement (not a geomembrane), and so does
# technic_hardmaterial_pct >= 95. `status`: TRUE found, FALSE none, NA unsure.
.q_technic_hard <- function(pedon) {
  h <- pedon$horizons
  th <- technic_hard_material(pedon)
  d <- h$designation
  named <- which(!is.na(d) & grepl("asph|asfalt|concret|cement", d, ignore.case = TRUE))
  cem <- tryCatch(test_cemented(h, min_class = "strongly")$layers,
                  error = function(e) integer(0)) %||% integer(0)
  art <- h$artefacts_pct
  pct <- h$technic_hardmaterial_pct %||% rep(NA_real_, nrow(h))
  layers <- sort(unique(c(named, cem[!is.na(art[cem]) & art[cem] >= 95],
                          which(!is.na(pct) & pct >= 95))))
  unsure <- setdiff(cem, layers)
  status <- if (length(layers)) TRUE
            else if (length(unsure) || is.na(th$passed)) NA else FALSE
  list(layers = layers, unsure = unsure, status = status, diagnostic = th)
}

# Continuous rock or technic hard material in layer `i`: TRUE / FALSE / NA.
# Both are read from the designation (continuous_rock(), .q_technic_hard()),
# so a designated layer that is neither is FALSE.
.q_hard_layer <- function(pedon, i, rock = continuous_rock(pedon),
                          thm = .q_technic_hard(pedon)) {
  if (is.na(i)) return(NA)
  if (i %in% c(rock$layers, thm$layers)) return(TRUE)
  if (i %in% thm$unsure || is.na(pedon$horizons$designation[i])) return(NA)
  FALSE
}

# Ice in layer `i` (WRB 2022 Annex 10.1, master horizon I: ">= 75% ice (by
# volume, related to the whole soil), permanent"): ice_pct >= 75 or an I (or
# Wf) designation; FALSE when ice_pct < 75 or another master horizon is given.
.q_ice_layer <- function(h, i) {
  if (is.na(i)) return(NA)
  d <- h$designation[i]
  if (!is.na(d) && grepl("^[0-9]*(I([a-z0-9]|$)|Wf)", d)) return(TRUE)
  ice <- (h$ice_pct %||% rep(NA_real_, nrow(h)))[i]
  if (!is.na(ice)) return(ice >= 75)
  if (!is.na(d)) FALSE else NA
}

# Gleyic properties, diagnostic criterion 1 (WRB 2022 Ch 3.2.6), per layer as
# TRUE / FALSE / NA: ">= 95% (by exposed area) reductimorphic features" with
# hue N, 10Y, GY, G, BG, B or PB, or hue 2.5Y or 5Y and chroma <= 2 (moist).
# The matrix colour is the dominant one, so a matrix outside these colours
# rules the criterion out. A matrix inside them meets it when <= 5%
# redoximorphic features are recorded; with more, those features (Fe/Mn
# concentrations in a reduced matrix) cover > 5% and the criterion fails.
.q_gleyic_crit1 <- function(h) {
  hue <- toupper(gsub("\\s+", "", as.character(h$munsell_hue_moist)))
  chroma <- h$munsell_chroma_moist
  redox <- h$redoximorphic_features_pct
  yel <- !is.na(hue) & grepl("^(2\\.5|5)Y$", hue)
  red <- (!is.na(hue) & grepl("^(N.*|10Y|[0-9.]*(GY|G|BG|B|PB))$", hue)) |
         (yel & !is.na(chroma) & chroma <= 2)
  out <- rep(NA, nrow(h))
  out[!is.na(hue) & !red & (!yel | !is.na(chroma))] <- FALSE
  out[red & !is.na(redox)] <- redox[red & !is.na(redox)] <= 5
  out
}

# Thickness from layer `from` downwards over a per-layer status: TRUE once the
# consecutive TRUE layers reach `need` cm, FALSE at a FALSE layer before that,
# NA at an unknown layer or where the data end.
.q_thick_from <- function(h, status, from, need) {
  start <- h$top_cm[from]
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= start]
  ly <- integer(0)
  for (i in ord) {
    if (!isTRUE(status[i]))
      return(list(passed = if (is.na(status[i])) NA else FALSE, layers = ly))
    ly <- c(ly, i)
    if (isTRUE(h$bottom_cm[i] - start >= need))
      return(list(passed = TRUE, layers = ly))
  }
  list(passed = NA, layers = ly)
}

# Fluvic layers for the Fluvic subqualifiers, by the rule qual_anofluvic()
# applies: fluvic_material() marks every horizon starting above 100 cm, so
# horizons whose recorded origin (rock_origin, layer_origin) is not
# fluviatile, marine or lacustrine are removed, and so is everything from a
# limiting layer down.
.q_fluvic_layers <- function(pedon, fm = fluvic_material(pedon)) {
  h <- pedon$horizons
  pat <- "fluv|marine|lacustr|alluv"
  ro <- tolower(as.character(h$rock_origin))
  lo <- tolower(as.character(h$layer_origin))
  not_fluvic <- (!is.na(ro) & !grepl(pat, ro)) | (!is.na(lo) & !grepl(pat, lo))
  fl <- fm$layers[!not_fluvic[fm$layers]]
  lim <- .barrier_top_cm(pedon)
  if (!is.na(lim)) fl <- fl[!is.na(h$top_cm[fl]) & h$top_cm[fl] < lim]
  fl
}


#' Profundihumic qualifier (dh): >= 1.4\% SOC to 100 cm, >= 1\% throughout
#'
#' WRB 2022 Ch 5 (subqualifier of Humic): "having to a depth of 100 cm from
#' the mineral soil surface >= 1.4\% soil organic carbon as a weighted average
#' and >= 1\% soil organic carbon throughout."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_profundihumic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Profundihumic: "having to a depth of 100 cm from
  # the mineral soil surface >= 1.4% soil organic carbon as a weighted average
  # and >= 1% soil organic carbon throughout". Adds the ">= 1% throughout"
  # criterion, measures from the mineral soil surface and needs OC over the
  # whole 100 cm (a limiting layer inside it holds no SOC and fails it). The
  # weighted mean used to be taken over whatever layers had data.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  oc <- h$oc_pct
  ov <- pmax(0, pmin(h$bottom_cm, ms + 100) - pmax(h$top_cm, ms))
  win <- which(!is.na(ov) & ov > 0)
  lim <- .barrier_top_cm(pedon)
  lim_in <- !is.na(lim) && lim < ms + 100
  if (lim_in) win <- win[h$top_cm[win] < lim]
  low <- win[!is.na(oc[win]) & oc[win] < 1]
  full <- !lim_in && sum(ov[win]) >= 100 - 1e-6 && !anyNA(oc[win])
  wmean <- .q_weighted_mean(oc[win], h$top_cm[win], h$bottom_cm[win],
                            ms, ms + 100)
  passed <- if (length(low) || lim_in) FALSE
            else if (!full) NA
            else wmean >= 1.4
  DiagnosticResult$new(
    name = "Profundihumic", passed = passed,
    layers = if (isTRUE(passed)) win else integer(0),
    evidence = list(weighted_mean_oc_pct = wmean, layers_below_1pct = low,
                    mineral_surface_cm = ms, limiting_layer_top_cm = lim,
                    threshold_mean = 1.4, threshold_min = 1),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = "WRB (2022) Ch 5, Profundihumic"
  )
}


#' Wapnic qualifier (wa): calcic horizon within organic material
#'
#' WRB 2022 Ch 5: "having a calcic horizon within organic material, starting
#' <= 100 cm from the soil surface."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_wapnic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Wapnic: "having a calcic horizon within organic
  # material, starting <= 100 cm from the soil surface". Reads calcic() and
  # organic material: the calcic horizon is within organic material when its
  # layers are organic material, or when organic material lies directly above
  # and directly below it (e.g. lake marl inside peat). Until v0.9.217 any
  # layer with >= 80% CaCO3 passed, with no calcic horizon and no organic
  # material.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Wapnic"
  cal <- calcic(pedon)
  if (!isTRUE(cal$passed))
    return(DiagnosticResult$new(
      name = "Wapnic", passed = cal$passed, layers = integer(0),
      evidence = list(calcic = cal),
      missing = cal$missing %||% character(0), reference = ref))
  org <- .q_organic_layers(pedon)
  surface <- min(h$top_cm, na.rm = TRUE)
  runs <- Filter(function(r) r$top <= 100, .q_layer_runs(h, cal$layers))
  within <- vapply(runs, function(r) {
    up <- if (r$top <= surface) FALSE else org[.q_layer_ending(h, r$top)]
    dn <- org[.q_layer_at(h, r$bottom)]
    all(org[r$layers]) | (up & dn)
  }, logical(1))
  passed <- if (any(within %in% TRUE)) TRUE
            else if (anyNA(within)) NA else FALSE
  DiagnosticResult$new(
    name = "Wapnic", passed = passed,
    layers = if (isTRUE(passed))
               unlist(lapply(runs[within %in% TRUE], `[[`, "layers"))
             else integer(0),
    evidence = list(calcic = cal, calcic_within_organic = within),
    missing = if (is.na(passed)) "oc_pct" else character(0),
    reference = ref
  )
}


#' Mawic qualifier (mw): coarse fragments filled with organic material
#'
#' WRB 2022 Ch 5 (Histosols): a layer of coarse fragments that, with any
#' overlying organic material, starts at the soil surface and is >= 40 cm
#' thick (>= 10 cm over continuous rock or technic hard material), its
#' interstices mostly filled with organic material.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mawic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Mawic: "having a layer of coarse fragments that,
  # together with overlying organic material, if present, starts at the soil
  # surface and has a thickness of >= 10 cm if overlying continuous rock or
  # technic hard material; or >= 40 cm; and the major part of the interstices
  # between the coarse fragments is filled with organic material and the
  # remaining interstices, if present, are void". soilKey does not record how
  # much of the space between coarse fragments is filled or void, so Mawic is
  # FALSE where the data rule it out (no organic material from the surface,
  # < 50% coarse fragments throughout it, too thin) and NA otherwise. Until
  # v0.9.217 it tested moss fibres, a criterion of the Histosols, not of Mawic.
  h <- pedon$horizons
  os <- .q_organic_from_surface(pedon)
  res <- function(passed, ...) DiagnosticResult$new(
    name = "Mawic", passed = passed, layers = integer(0),
    evidence = list(organic_from_surface = os, ...),
    missing = if (is.na(passed))
                c(if (!isTRUE(os$status)) "oc_pct",
                  "interstices between coarse fragments filled with organic material (% of their volume; not in the schema)")
              else character(0),
    reference = "WRB (2022) Ch 5, Mawic")
  if (!isTRUE(os$status)) return(res(os$status))
  cf <- h$coarse_fragments_pct[os$layers]
  stony <- if (any(!is.na(cf) & cf >= 50)) TRUE else if (anyNA(cf)) NA else FALSE
  thick <- os$bottom - min(h$top_cm[os$layers])
  thick_ok <- if (thick >= 40) TRUE else if (thick < 10) FALSE
              else .q_hard_layer(pedon, os$below)
  passed <- if (isFALSE(stony) || isFALSE(thick_ok)) FALSE else NA
  res(passed, coarse_fragments_pct = cf, thickness_cm = thick)
}


#' Muusic qualifier (mu): organic material from the surface directly over ice
#'
#' WRB 2022 Ch 5 (Histosols): "having organic material starting at the soil
#' surface that directly overlies ice."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_muusic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Muusic: "having organic material starting at the
  # soil surface that directly overlies ice". The layer directly below the
  # organic material must be ice: ice_pct >= 75 or an I master horizon (WRB
  # Annex 10.1). Until v0.9.217 it tested >= 75% rubbed fibres, which has
  # nothing to do with ice.
  h <- pedon$horizons
  os <- .q_organic_from_surface(pedon)
  ice <- if (isTRUE(os$status)) .q_ice_layer(h, os$below) else NA
  passed <- if (!isTRUE(os$status)) os$status else ice
  DiagnosticResult$new(
    name = "Muusic", passed = passed,
    layers = if (isTRUE(passed)) c(os$layers, os$below) else integer(0),
    evidence = list(organic_layers = os$layers, organic_bottom_cm = os$bottom,
                    layer_below = os$below, ice_below = ice),
    missing = if (is.na(passed))
                c(if (!isTRUE(os$status)) "oc_pct", "ice_pct")
              else character(0),
    reference = "WRB (2022) Ch 5, Muusic"
  )
}


#' Murshic qualifier (mh): drained histic horizon with aggregates or cracks
#'
#' WRB 2022 Ch 5 (Histosols): a drained histic horizon, >= 20 cm thick, from
#' the soil surface or below < 40 cm of mulmic (or aerated organic) material,
#' with bulk density >= 0.2 kg dm-3 and moderate to strong granular or blocky
#' structure, or cracks.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_murshic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Murshic: "having a drained histic horizon, >= 20 cm
  # thick, and starting at the soil surface, or directly below a layer,
  # < 40 cm thick, consisting of mulmic material, or directly below a layer,
  # < 40 cm thick, consisting of organic material that is saturated with water
  # for < 30 consecutive days in most years and is not drained, and having a
  # bulk density of >= 0.2 kg dm-3 and one or both of the following: moderate
  # to strong granular structure or moderate to strong angular or subangular
  # blocky structure, or cracks". Reads histic_horizon(), qual_drainic() (the
  # site's record of artificial drainage), mulmic_material(), the bulk density
  # of every layer of the horizon, structure and cracks. Whether an overlying
  # organic layer is drained is not held per layer, so that start is NA. Until
  # v0.9.217 it passed on rubbed fibres < 17% or von Post >= 7 above 50 cm.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Murshic"
  hist <- histic_horizon(pedon)
  if (!isTRUE(hist$passed))
    return(DiagnosticResult$new(
      name = "Murshic", passed = hist$passed, layers = integer(0),
      evidence = list(histic = hist),
      missing = hist$missing %||% character(0), reference = ref))
  run <- .q_layer_runs(h, hist$layers)[[1L]]
  ly <- run$layers
  surface <- min(h$top_cm, na.rm = TRUE)
  above <- which(!is.na(h$bottom_cm) & h$bottom_cm <= run$top)
  mul <- mulmic_material(pedon)$layers %||% integer(0)
  start_ok <- if (run$top <= surface) TRUE
              else if (run$top - surface >= 40) FALSE
              else if (all(above %in% mul)) TRUE
              else NA
  dr <- qual_drainic(pedon)
  drained <- if (isTRUE(dr$passed)) TRUE else if (length(dr$missing)) NA else FALSE
  bd <- h$bulk_density_g_cm3[ly]
  bd_ok <- if (any(!is.na(bd) & bd < 0.2)) FALSE else if (anyNA(bd)) NA else TRUE
  grade <- tolower(h$structure_grade[ly]); type <- tolower(h$structure_type[ly])
  aggregated <- !is.na(grade) & grepl("moderate|strong", grade) &
                !is.na(type) & grepl("granular|blocky", type)
  cw <- h$cracks_width_cm[ly]; cd <- h$cracks_depth_cm[ly]
  cracks <- (!is.na(cw) & cw > 0) | (!is.na(cd) & cd > 0)
  no_cracks <- (!is.na(cw) & cw == 0) | (!is.na(cd) & cd == 0)
  form_ok <- if (any(aggregated | cracks)) TRUE
             else if (!anyNA(grade) && !anyNA(type) && all(no_cracks)) FALSE
             else NA
  parts <- c(drained = drained, thickness = run$bottom - run$top >= 20,
             start = start_ok, bulk_density = bd_ok,
             structure_or_cracks = form_ok)
  need <- list(drained = "site$drainage_class",
               thickness = character(0),
               start = "drainage of the overlying organic layer (not in the schema)",
               bulk_density = "bulk_density_g_cm3",
               structure_or_cracks = c("structure_grade", "structure_type",
                                       "cracks_width_cm"))
  passed <- if (any(parts %in% FALSE)) FALSE else if (anyNA(parts)) NA else TRUE
  DiagnosticResult$new(
    name = "Murshic", passed = passed,
    layers = if (isTRUE(passed)) ly else integer(0),
    evidence = list(histic = hist, histic_top_cm = run$top,
                    histic_thickness_cm = run$bottom - run$top,
                    criteria = parts, bulk_density_g_cm3 = bd),
    missing = if (is.na(passed)) unlist(need[names(parts)[is.na(parts)]],
                                        use.names = FALSE)
              else character(0),
    reference = ref
  )
}


#' Rockic qualifier (rk): organic material from the surface directly over rock
#'
#' WRB 2022 Ch 5 (Histosols): "having organic material starting at the soil
#' surface that directly overlies continuous rock or technic hard material."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_rockic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Rockic: "having organic material starting at the
  # soil surface that directly overlies continuous rock or technic hard
  # material". The layer directly below the organic material must be
  # continuous rock (continuous_rock()) or technic hard material. Until
  # v0.9.217 it asked for rock within 25 cm plus >= 50% coarse fragments
  # above 50 cm, with no organic material at all.
  os <- .q_organic_from_surface(pedon)
  hard <- if (isTRUE(os$status)) .q_hard_layer(pedon, os$below) else NA
  passed <- if (!isTRUE(os$status)) os$status else hard
  DiagnosticResult$new(
    name = "Rockic", passed = passed,
    layers = if (isTRUE(passed)) c(os$layers, os$below) else integer(0),
    evidence = list(organic_layers = os$layers, organic_bottom_cm = os$bottom,
                    layer_below = os$below, hard_below = hard),
    missing = if (is.na(passed))
                c(if (!isTRUE(os$status)) "oc_pct", "designation")
              else character(0),
    reference = "WRB (2022) Ch 5, Rockic"
  )
}


#' Thyric qualifier (th): technic hard material starting > 5 and <= 100 cm
#'
#' WRB 2022 Ch 5: "having technic hard material starting within > 5 and
#' <= 100 cm from the soil surface."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_thyric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Thyric: "having technic hard material starting
  # within > 5 and <= 100 cm from the soil surface". Reads technic hard
  # material (.q_technic_hard()); a cemented layer of unknown origin in that
  # depth gives NA. Until v0.9.217 it passed on >= 20% industrial artefacts
  # with >= 5% organic carbon, which is not technic hard material.
  h <- pedon$horizons
  thm <- .q_technic_hard(pedon)
  in_win <- function(r) r$top > 5 && r$top <= 100
  hits <- Filter(in_win, .q_layer_runs(h, thm$layers))
  maybe <- Filter(in_win, .q_layer_runs(h, union(thm$layers, thm$unsure)))
  passed <- if (length(hits)) TRUE
            else if (length(maybe) || is.na(thm$diagnostic$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "Thyric", passed = passed,
    layers = if (isTRUE(passed)) unlist(lapply(hits, `[[`, "layers"))
             else integer(0),
    evidence = list(technic_hard_layers = thm$layers,
                    cemented_unknown_origin = thm$unsure,
                    starts_cm = vapply(hits, `[[`, numeric(1), "top")),
    missing = if (is.na(passed)) c("designation", "artefacts_pct")
              else character(0),
    reference = "WRB (2022) Ch 5, Thyric"
  )
}


#' Anthromollic qualifier (am): mollic horizon with anthric properties
#'
#' WRB 2022 Ch 5: a mollic horizon and anthric properties (Ch 3.2.4).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_anthromollic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Anthromollic: "having a mollic horizon and anthric
  # properties". The old code required an Anthrosol surface horizon (hortic,
  # plaggic, ...: anthric_horizons()) over a spodic horizon. Anthric properties
  # (Ch 3.2.4) need (2) evidence of human disturbance, of which the schema
  # holds only "(d) >= 430 mg kg-1 P in the Mehlich-3 extract in the upper
  # 20 cm" (p_mehlich3_mg_kg; the ploughing, mixing and lime-lump paths are not
  # recorded), and (3) "< 5% ... animal pores, coprolites or other traces of
  # soil animal activity" in the lowermost 5 cm of the mollic horizon or in
  # 5 cm below the plough layer (worm_holes_pct; bioturbation_density "none").
  # Without the P evidence the result is NA, as (2a-c) cannot be ruled out.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Anthromollic"
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(DiagnosticResult$new(
      name = "Anthromollic", passed = mol$passed, layers = integer(0),
      evidence = list(mollic = mol),
      missing = mol$missing %||% character(0), reference = ref))
  }
  # (2d) P (Mehlich-3) >= 430 mg/kg throughout the upper 20 cm.
  top20 <- which(!is.na(h$top_cm) & h$top_cm < 20)
  p <- h$p_mehlich3_mg_kg[top20]
  human <- if (length(top20) && all(!is.na(p) & p >= 430)) TRUE else NA
  # (3) few traces of animal activity: TRUE / FALSE / NA for a set of layers.
  low_traces <- function(ly) {
    if (!length(ly)) return(NA)
    w <- h$worm_holes_pct[ly]
    b <- tolower(trimws(as.character(h$bioturbation_density[ly])))
    ok <- (!is.na(w) & w < 5) | (!is.na(b) & b == "none")
    if (all(ok)) TRUE else if (any(!is.na(w) & w >= 5)) FALSE else NA
  }
  ml <- mol$layers
  mbot <- max(h$bottom_cm[ml], na.rm = TRUE)
  zone_a <- ml[!is.na(h$bottom_cm[ml]) & h$bottom_cm[ml] > mbot - 5]
  ap <- which(!is.na(h$designation) & grepl("^Ap", h$designation))
  zone_b <- if (length(ap)) {
    pb <- max(h$bottom_cm[ap], na.rm = TRUE)
    which(!is.na(h$top_cm) & h$top_cm < pb + 5 & h$bottom_cm > pb)
  } else integer(0)
  a <- low_traces(zone_a)
  b <- if (length(ap)) low_traces(zone_b)
       else if (any(!is.na(h$designation))) FALSE else NA
  few_animals <- if (isTRUE(a) || isTRUE(b)) TRUE
                 else if (isFALSE(a) && isFALSE(b)) FALSE else NA
  passed <- if (isFALSE(few_animals)) FALSE
            else if (isTRUE(human) && isTRUE(few_animals)) TRUE else NA
  DiagnosticResult$new(
    name = "Anthromollic", passed = passed,
    layers = if (isTRUE(passed)) ml else integer(0),
    evidence = list(mollic = mol, p_mehlich3_upper20 = p,
                    animal_traces_low_mollic = a, animal_traces_below_plough = b),
    missing = if (!is.na(passed)) character(0)
              else c(if (is.na(human)) c("p_mehlich3_mg_kg",
                                         "ploughing / mixing / lime-lump evidence"),
                     if (is.na(few_animals)) "worm_holes_pct"),
    reference = ref
  )
}


# v0.9.217: Endo- form of Calcaric and Dolomitic. Chapter 5: the material "in a
# layer, >= 30 cm thick and within 100 cm of the mineral soil surface" (or in
# the major part above a limiting layer starting < 60 cm), with "no
# subqualifier if a limiting layer starts < 60 cm"; Chapter 2.3.1 Endo- for a
# layer: "the layer starts >= 50 cm from the (mineral) soil surface; and no
# such layer occurs < 50 cm". So: a run of the material with >= 30 cm inside
# 0-100 cm, starting >= 50 cm, and no such run starting above 50 cm. Horizons
# whose `col` is missing may hide a shallower run (NA). `exclude` holds
# diagnostics that must not start <= 100 cm (calcic, petrocalcic).
.q_endo_material <- function(pedon, name, mat, col = "caco3_pct",
                             exclude = list()) {
  h <- pedon$horizons
  ref <- paste0("WRB (2022) Ch 5, ", name)
  res <- function(passed, layers = integer(0), missing = character(0), ...)
    DiagnosticResult$new(name = name, passed = passed, layers = layers,
                         evidence = list(material = mat, ...),
                         missing = missing, reference = ref)
  if (!isTRUE(mat$passed)) return(res(mat$passed, missing = mat$missing %||% character(0)))
  lim <- .barrier_top_cm(pedon)
  if (!is.na(lim) && lim < 60) return(res(FALSE, limiting_layer_top_cm = lim))
  thk100 <- function(r) min(r$bottom, 100) - r$top
  runs <- Filter(function(r) thk100(r) >= 30, .q_layer_runs(h, mat$layers))
  shallow <- Filter(function(r) r$top < 50, runs)
  deep <- Filter(function(r) r$top >= 50, runs)
  if (length(shallow) || !length(deep)) return(res(FALSE, runs = runs))
  unk <- which(is.na(h[[col]]))
  maybe <- Filter(function(r) r$top < 50 && thk100(r) >= 30,
                  .q_layer_runs(h, union(mat$layers, unk)))
  if (length(maybe)) return(res(NA, missing = col, runs = runs))
  for (d in exclude) {
    top <- if (isTRUE(d$passed)) suppressWarnings(min(h$top_cm[d$layers], na.rm = TRUE)) else Inf
    if (is.finite(top) && top <= 100) return(res(FALSE, excluded_by = d$name))
  }
  for (d in exclude) {
    if (is.na(d$passed))
      return(res(NA, missing = if (length(d$missing)) d$missing else d$name))
  }
  if (!.q_reported_to_100(pedon, lim))
    return(res(NA, missing = "horizons to 100 cm", runs = deep))
  res(TRUE, layers = unlist(lapply(deep, `[[`, "layers")), runs = deep)
}


#' Endocalcaric qualifier: calcaric material only from 50 cm down
#'
#' WRB 2022 Ch 5 Calcaric with the Endo- specifier (Ch 2.3.1).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endocalcaric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Calcaric: "having calcaric material in a layer,
  # >= 30 cm thick and within 100 cm of the mineral soil surface ...; and not
  # having a calcic or a petrocalcic horizon starting <= 100 cm"; Endo-: "the
  # layer starts >= 50 cm ...; and no such layer occurs < 50 cm". The old code
  # passed on any horizon with >= 2% CaCO3 overlapping 50-200 cm, with no
  # thickness, no shallow-layer check and no calcic exclusion.
  .q_endo_material(pedon, "Endocalcaric", calcaric_material(pedon),
                   exclude = list(calcic(pedon), petrocalcic(pedon)))
}


#' Endodolomitic qualifier: dolomitic material only from 50 cm down
#'
#' WRB 2022 Ch 5 Dolomitic with the Endo- specifier (Ch 2.3.1).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endodolomitic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Dolomitic: "having dolomitic material in a layer,
  # >= 30 cm thick and within 100 cm of the mineral soil surface ... (2; no
  # subqualifier if a limiting layer starts < 60 cm)"; Endo-: "the layer
  # starts >= 50 cm ...; and no such layer occurs < 50 cm". The old code
  # passed on any dolomitic horizon overlapping 50-200 cm.
  .q_endo_material(pedon, "Endodolomitic", dolomitic_material(pedon))
}


#' Anofluvic qualifier: fluvic material from the surface to 50-100 cm
#'
#' WRB 2022 Ch 5 Fluvic with the Ano- specifier (Ch 2.3.1).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_anofluvic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Fluvic: "having fluvic material, >= 25 cm thick
  # and starting <= 75 cm from the mineral soil surface"; Ano- (Ch 2.3.1):
  # "the layer starts at the (mineral) soil surface and has its lower limit
  # > 50 and < 100 cm of the (mineral) soil surface; and no such layer occurs
  # between 99 and 100 cm ... or directly above a limiting layer". The old code
  # tested fluvic material anywhere in 50-200 cm (an Endo- reading). The
  # fluvic layers are those of fluvic_material(), less horizons whose recorded
  # origin (rock_origin, layer_origin) is not fluviatile, marine or lacustrine
  # and anything from the limiting layer down. fluvic_material() marks every
  # horizon starting above 100 cm, so the lower limit is found only where an
  # origin record ends the fluvic layer; a profile that stops above 100 cm
  # gives NA.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Anofluvic"
  res <- function(passed, layers = integer(0), missing = character(0), ...)
    DiagnosticResult$new(name = "Anofluvic", passed = passed, layers = layers,
                         evidence = list(...), missing = missing, reference = ref)
  fm <- fluvic_material(pedon)
  if (!isTRUE(fm$passed))
    return(res(fm$passed, missing = fm$missing %||% character(0), fluvic = fm))
  pat <- "fluv|marine|lacustr|alluv"
  ro <- tolower(as.character(h$rock_origin))
  lo <- tolower(as.character(h$layer_origin))
  not_fluvic <- (!is.na(ro) & !grepl(pat, ro)) | (!is.na(lo) & !grepl(pat, lo))
  lim <- .barrier_top_cm(pedon)
  fl <- fm$layers[!not_fluvic[fm$layers]]
  if (!is.na(lim)) fl <- fl[!is.na(h$top_cm[fl]) & h$top_cm[fl] < lim]
  runs <- .q_layer_runs(h, fl)
  surface <- min(h$top_cm, na.rm = TRUE)
  if (!length(runs) || runs[[1L]]$top > surface)
    return(res(FALSE, fluvic = fm, reason = "no fluvic layer from the surface"))
  bot <- runs[[1L]]$bottom
  at_99 <- any(vapply(runs, function(r) r$top < 100 && r$bottom > 99, logical(1)))
  if (bot <= 50 || bot >= 100 || at_99 || (!is.na(lim) && lim <= bot))
    return(res(FALSE, fluvic = fm, lower_limit_cm = bot, limiting_layer_top_cm = lim))
  if (!.q_reported_to_100(pedon, lim))
    return(res(NA, missing = "horizons to 100 cm", fluvic = fm, lower_limit_cm = bot))
  res(TRUE, layers = runs[[1L]]$layers, fluvic = fm, lower_limit_cm = bot)
}


#' Pantofluvic qualifier: fluvic material from the mineral soil surface to
#' >= 100 cm (or to a limiting layer starting > 50 cm)
#'
#' WRB 2022 Ch 5 Fluvic with the Panto- specifier (Ch 2.3.1).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_pantofluvic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Fluvic: "having fluvic material, >= 25 cm thick
  # and starting <= 75 cm from the mineral soil surface"; Panto- (Ch 2.3.1):
  # "the layer starts at the (mineral) soil surface and has its lower limit
  # >= 100 cm of the (mineral) soil surface or at a limiting layer starting
  # > 50 cm from the (mineral) soil surface". Fluvic layers as in
  # qual_anofluvic() (.q_fluvic_layers()), depths from the mineral soil
  # surface; a profile not reported to 100 cm or a limiting layer is NA. Until
  # v0.9.217 it passed whenever fluvic_material() did (that diagnostic marks
  # every horizon above 100 cm), at any described depth, and a missing
  # fluvic_material() result was FALSE.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Pantofluvic"
  fm <- fluvic_material(pedon)
  if (!isTRUE(fm$passed))
    return(DiagnosticResult$new(
      name = "Pantofluvic", passed = fm$passed, layers = integer(0),
      evidence = list(fluvic = fm),
      missing = fm$missing %||% character(0), reference = ref))
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- .barrier_top_cm(pedon)
  first <- Filter(function(r) r$top <= ms + 1e-6 && r$bottom > ms,
                  .q_layer_runs(h, .q_fluvic_layers(pedon, fm)))
  bot <- if (length(first)) first[[1L]]$bottom else NA_real_
  passed <- if (is.na(bot)) FALSE
            else if (bot >= ms + 100) TRUE
            else if (!is.na(lim) && lim > ms + 50 && bot >= lim) TRUE
            else if (!.q_reported_to_100(pedon, lim)) NA
            else FALSE
  DiagnosticResult$new(
    name = "Pantofluvic", passed = passed,
    layers = if (isTRUE(passed)) first[[1L]]$layers else integer(0),
    evidence = list(fluvic = fm, mineral_surface_cm = ms,
                    fluvic_lower_limit_cm = bot, limiting_layer_top_cm = lim),
    missing = if (is.na(passed)) "horizons to 100 cm" else character(0),
    reference = ref
  )
}


#' Orthofluvic qualifier (of): fluvic material from the mineral soil surface
#'
#' WRB 2022 Ch 5 (subqualifier of Fluvic): fluvic material from the mineral
#' soil surface to >= 5 cm, and >= 25 cm thick starting <= 25 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_orthofluvic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Orthofluvic: "having fluvic material: from the
  # mineral soil surface to a depth of >= 5 cm, and >= 25 cm thick and
  # starting <= 25 cm from the mineral soil surface". Fluvic layers as in
  # qual_anofluvic() (.q_fluvic_layers()). Until v0.9.217 it passed when a
  # fluvic layer reached the 50-100 cm window, a definition WRB does not give.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Orthofluvic"
  fm <- fluvic_material(pedon)
  if (!isTRUE(fm$passed))
    return(DiagnosticResult$new(
      name = "Orthofluvic", passed = fm$passed, layers = integer(0),
      evidence = list(fluvic = fm),
      missing = fm$missing %||% character(0), reference = ref))
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  runs <- .q_layer_runs(h, .q_fluvic_layers(pedon, fm))
  first <- Filter(function(r) r$top <= ms + 1e-6 && r$bottom > ms, runs)
  from_surface <- length(first) > 0L && first[[1L]]$bottom >= ms + 5
  thick <- Filter(function(r) max(r$top, ms) <= ms + 25 &&
                    r$bottom - max(r$top, ms) >= 25, runs)
  data_end <- suppressWarnings(max(h$bottom_cm, na.rm = TRUE))
  passed <- if (from_surface && length(thick)) TRUE
            else if (!length(first)) FALSE
            else if (first[[1L]]$bottom >= data_end) NA
            else FALSE
  DiagnosticResult$new(
    name = "Orthofluvic", passed = passed,
    layers = if (isTRUE(passed)) unique(c(first[[1L]]$layers,
                                          unlist(lapply(thick, `[[`, "layers"))))
             else integer(0),
    evidence = list(fluvic = fm, mineral_surface_cm = ms,
                    fluvic_from_surface_to_cm = if (length(first)) first[[1L]]$bottom else NA),
    missing = if (is.na(passed)) "horizons below the fluvic layer" else character(0),
    reference = ref
  )
}


#' Oxyaquic qualifier (oa): saturated layer without gleyic or stagnic
#' properties
#'
#' WRB 2022 Ch 5: a layer >= 25 cm thick, starting <= 75 cm, saturated with
#' water for >= 20 consecutive days, and no gleyic or stagnic properties
#' within 100 cm of the mineral soil surface.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_oxyaquic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Oxyaquic: "having a layer, >= 25 cm thick and
  # starting <= 75 cm from the mineral soil surface, that is saturated with
  # water during a period of >= 20 consecutive days; and not having gleyic
  # properties and not having stagnic properties in any layer within 100 cm of
  # the mineral soil surface". Gleyic and stagnic properties come from
  # gleyic_properties() and stagnic_properties() over 100 cm, plus criterion 1
  # of the gleyic properties (.q_gleyic_crit1()); their absence must be shown
  # for every layer. Saturation comes from water_saturation_days, a count of
  # days per year: < 20 rules a 20-day period out. Over a run of layers, the
  # days on which any of them is unsaturated are at most the sum of their
  # unsaturated days (366 - days, leap years included); if that is <= 16, the
  # >= 349 days on which the whole run is saturated fall in <= 17 periods, one
  # of which lasts >= 21 days. Anything between is NA. Until v0.9.217 it
  # passed on >= 5% redox features without a
  # gleyic hue, i.e. on the features of gleyic or stagnic properties, so it
  # fired on the example Gleysol.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Oxyaquic"
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  gl <- gleyic_properties(pedon, max_top_cm = ms + 100)
  st <- stagnic_properties(pedon, max_top_cm = ms + 100)
  win <- which(!is.na(h$top_cm) & h$top_cm < ms + 100 &
               !is.na(h$bottom_cm) & h$bottom_cm > ms)
  c1 <- .q_gleyic_crit1(h)[win]
  wet <- isTRUE(gl$passed) || isTRUE(st$passed) || any(c1 %in% TRUE)
  dry <- isFALSE(gl$passed) && !length(gl$missing) && isFALSE(st$passed) &&
         length(win) > 0L && all(c1 %in% FALSE)
  wsd <- suppressWarnings(as.numeric(h$water_saturation_days))
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & !is.na(h$bottom_cm[ord])]
  sat_layers <- integer(0)
  for (k in seq_along(ord)) {
    s <- ord[k]
    if (h$bottom_cm[s] <= ms || h$top_cm[s] > ms + 75) next
    start <- max(h$top_cm[s], ms); dry_days <- 0; ly <- integer(0)
    for (j in ord[k:length(ord)]) {
      if (length(ly) && h$top_cm[j] > max(h$bottom_cm[ly]) + 1e-6) break
      if (is.na(wsd[j])) break
      dry_days <- dry_days + max(0, 366 - wsd[j]); ly <- c(ly, j)
      if (dry_days > 16) break
      if (h$bottom_cm[j] - start >= 25) { sat_layers <- ly; break }
    }
    if (length(sat_layers)) break
  }
  may <- which(is.na(wsd) | wsd >= 20)
  could <- .q_layers_thickness(h, may, win_top = ms, contiguous = TRUE,
                               max_start = ms + 75) >= 25
  sat <- if (length(sat_layers)) TRUE else if (!could) FALSE else NA
  passed <- if (wet || isFALSE(sat)) FALSE
            else if (isTRUE(sat) && dry) TRUE
            else NA
  DiagnosticResult$new(
    name = "Oxyaquic", passed = passed,
    layers = if (isTRUE(passed)) sat_layers else integer(0),
    evidence = list(gleyic = gl, stagnic = st, gleyic_criterion1 = c1,
                    water_saturation_days = wsd, saturated_layers = sat_layers),
    missing = if (is.na(passed))
                unique(c(if (is.na(sat)) "water_saturation_days",
                         if (!dry) c(gl$missing, st$missing,
                                     if (anyNA(c1)) c("munsell_hue_moist",
                                                      "munsell_chroma_moist",
                                                      "redoximorphic_features_pct"))))
              else character(0),
    reference = ref
  )
}


#' Oxygleyic qualifier (oy): no reduced layer within 100 cm (Gleysols)
#'
#' WRB 2022 Ch 5: no layer meeting diagnostic criterion 1 of the gleyic
#' properties within 100 cm of the mineral soil surface.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_oxygleyic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Oxygleyic: "not having, within 100 cm of the
  # mineral soil surface, a layer that meets diagnostic criterion 1 of the
  # gleyic properties (in Gleysols only)"; criterion 1 is ">= 95% (by exposed
  # area) reductimorphic features" of hue N, 10Y, GY, G, BG, B or PB, or 2.5Y
  # or 5Y with chroma <= 2. Every layer down to 100 cm (or a limiting layer)
  # must be shown not to meet it (.q_gleyic_crit1()). Until v0.9.217 it passed
  # on gleyic properties plus >= 10% redox features above 50 cm, without
  # looking for a reduced layer.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- .barrier_top_cm(pedon)
  bot <- if (!is.na(lim) && lim < ms + 100) lim else ms + 100
  ov <- pmax(0, pmin(h$bottom_cm, bot) - pmax(h$top_cm, ms))
  win <- which(!is.na(ov) & ov > 0)
  c1 <- .q_gleyic_crit1(h)[win]
  reach <- sum(ov[win]) >= bot - ms - 1e-6
  passed <- if (any(c1 %in% TRUE)) FALSE
            else if (length(win) && all(c1 %in% FALSE) && reach) TRUE
            else NA
  DiagnosticResult$new(
    name = "Oxygleyic", passed = passed,
    layers = if (isTRUE(passed)) win else integer(0),
    evidence = list(gleyic_criterion1 = c1, layers = win,
                    mineral_surface_cm = ms, depth_cm = bot),
    missing = if (is.na(passed))
                c(if (anyNA(c1)) c("munsell_hue_moist", "munsell_chroma_moist",
                                   "redoximorphic_features_pct"),
                  if (!reach) "horizons to 100 cm")
              else character(0),
    reference = "WRB (2022) Ch 5, Oxygleyic"
  )
}


#' Reductaquic qualifier (ra): saturated, reduced layer above a cryic horizon
#'
#' WRB 2022 Ch 5 (Cryosols): a layer >= 25 cm thick, starting <= 75 cm,
#' above a cryic horizon, saturated during the thaw and with reducing
#' conditions at some time of the year.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_reductaquic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Reductaquic: "having above a cryic horizon a layer,
  # >= 25 cm thick and starting <= 75 cm from the soil surface, that is
  # saturated with water during the thawing period and that has at some time
  # of the year reducing conditions (in Cryosols only)". Reads cryic_horizon()
  # and reducing_conditions(). soilKey holds no record of saturation during
  # the thaw, so the qualifier is FALSE where the cryic horizon or a reduced
  # layer above it is missing, and NA otherwise. Until v0.9.217 it passed on a
  # gleyic hue with chroma <= 1 below 50 cm, with no cryic horizon.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Reductaquic"
  cry <- cryic_horizon(pedon)
  if (!isTRUE(cry$passed))
    return(DiagnosticResult$new(
      name = "Reductaquic", passed = cry$passed, layers = integer(0),
      evidence = list(cryic = cry),
      missing = cry$missing %||% character(0), reference = ref))
  ctop <- min(h$top_cm[cry$layers], na.rm = TRUE)
  rc <- reducing_conditions(pedon)
  evaluated <- suppressWarnings(as.integer(names(rc$evidence$redox$details)))
  status <- rep(FALSE, nrow(h))
  above <- which(!is.na(h$top_cm) & h$top_cm < ctop)
  status[above] <- ifelse(above %in% rc$layers, TRUE,
                          ifelse(above %in% evaluated, FALSE, NA))
  red <- .q_thickness_rule(h, status, 25, win_bot = ctop, contiguous = TRUE,
                           max_start = 75)
  passed <- if (isFALSE(red$passed)) FALSE else NA
  DiagnosticResult$new(
    name = "Reductaquic", passed = passed, layers = integer(0),
    evidence = list(cryic = cry, cryic_top_cm = ctop,
                    reducing_conditions = rc, reduced_layer = red),
    missing = if (is.na(passed))
                c(if (is.na(red$passed)) "redoximorphic_features_pct",
                  "saturation during the thawing period (not in the schema)")
              else character(0),
    reference = ref
  )
}


#' Reductigleyic qualifier (ry): no oximorphic layer from 40 cm down
#' (Gleysols)
#'
#' WRB 2022 Ch 5: no layer meeting diagnostic criterion 2 of the gleyic
#' properties >= 40 cm from the mineral soil surface.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_reductigleyic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Reductigleyic: "not having, >= 40 cm from the
  # mineral soil surface, a layer that meets diagnostic criterion 2 of the
  # gleyic properties (in Gleysols only)"; criterion 2 is "a layer with > 5%
  # ... oximorphic features" on biopore walls and aggregate surfaces. A layer
  # with <= 5% redoximorphic features cannot meet it; a layer with more that
  # gleyic_properties() accepts (redox features that do not fade with depth,
  # i.e. not the stagnic pattern) is taken to meet it. Every described layer
  # reaching below 40 cm (above a limiting layer) must be shown free of it.
  # Until v0.9.217 it passed when gleyic layers filled >= 25 cm of the upper
  # 50 cm, which says nothing about criterion 2.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- .barrier_top_cm(pedon)
  deep <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > ms + 40)
  if (!is.na(lim)) deep <- deep[h$top_cm[deep] < lim]
  gl <- gleyic_properties(pedon,
                          max_top_cm = suppressWarnings(max(h$bottom_cm, na.rm = TRUE)))
  gl_layers <- if (isTRUE(gl$passed)) gl$layers else integer(0)
  redox <- h$redoximorphic_features_pct
  c2 <- vapply(deep, function(i) {
    if (is.na(redox[i])) NA
    else if (redox[i] <= 5) FALSE
    else if (i %in% gl_layers) TRUE
    else NA
  }, logical(1))
  passed <- if (any(c2 %in% TRUE)) FALSE
            else if (length(deep) && all(c2 %in% FALSE)) TRUE
            else NA
  DiagnosticResult$new(
    name = "Reductigleyic", passed = passed,
    layers = if (isTRUE(passed)) deep else integer(0),
    evidence = list(gleyic = gl, layers_below_40cm = deep,
                    gleyic_criterion2 = c2, mineral_surface_cm = ms),
    missing = if (is.na(passed))
                c(if (anyNA(redox[deep]) || !length(deep)) "redoximorphic_features_pct",
                  if (any(!is.na(redox[deep]) & redox[deep] > 5 & is.na(c2)))
                    "whether the > 5% redox features are gleyic oximorphic features (criterion 2)")
              else character(0),
    reference = "WRB (2022) Ch 5, Reductigleyic"
  )
}


#' Transportic qualifier (tn): soil material moved in by humans from
#' outside the immediate vicinity
#'
#' WRB 2022 Ch 5: a layer at the soil surface (or below a recent organic
#' surface horizon), >= 20 cm thick (or >= 50\% of the soil above a limiting
#' layer starting <= 40 cm), with < 10\% artefacts, moved from elsewhere by
#' intentional human activity.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_transportic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Transportic: "having at the soil surface or below
  # a recently formed organic surface horizon a layer, >= 20 cm thick, or with
  # a thickness of >= 50% of the entire soil if a limiting layer starts
  # <= 40 cm from the soil surface, with soil material containing, if any,
  # < 10% (by volume, related to the whole soil) artefacts; and that has been
  # moved from a source area outside the immediate vicinity by intentional
  # human activity". Human transport is read only from a layer_origin that
  # says so (transported, dredged, imported); "fill" (may be moved within the
  # site: Relocatic), "spoil" (artefacts) and "antrop" no longer count. The
  # layer must start below any organic surface layers, reach the thickness
  # and have artefacts_pct < 10. Until v0.9.217 any matching layer starting
  # above 100 cm passed, at any depth or thickness.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Transportic"
  pat <- "transport|dredg|import"
  origin <- h$layer_origin
  art <- h$artefacts_pct
  moved <- !is.na(origin) & grepl(pat, origin, ignore.case = TRUE)
  status <- ifelse(moved & !is.na(art) & art < 10, TRUE,
                   ifelse((!is.na(origin) & !moved) | (!is.na(art) & art >= 10),
                          FALSE, NA))
  org <- .q_organic_layers(pedon)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord])]
  k <- 1L
  while (k <= length(ord) && isTRUE(org[ord[k]])) k <- k + 1L
  lim <- .barrier_top_cm(pedon)
  need <- if (!is.na(lim) && lim <= 40) 0.5 * lim else 20
  w <- if (k <= length(ord)) .q_thick_from(h, status, ord[k], need)
       else list(passed = FALSE, layers = integer(0))
  DiagnosticResult$new(
    name = "Transportic", passed = w$passed,
    layers = if (isTRUE(w$passed)) w$layers else integer(0),
    evidence = list(pattern = pat, layer_status = status,
                    thickness_needed_cm = need, limiting_layer_top_cm = lim),
    missing = if (is.na(w$passed)) c("layer_origin", "artefacts_pct")
              else character(0),
    reference = ref
  )
}


#' Relocatic qualifier (rc): remodelled in situ to >= 100 cm
#'
#' WRB 2022 Ch 5: remodelled in situ or within the immediate vicinity by
#' human activity to a depth of >= 100 cm, with no diagnostic horizon formed
#' since (except a mollic or umbric horizon).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_relocatic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Relocatic: "being remodelled in situ or within the
  # immediate vicinity by human activity to a depth of >= 100 cm (e.g. by deep
  # ploughing, refilling soil pits or levelling land) and no formation of
  # diagnostic horizons after remodelling, throughout, except a mollic or
  # umbric horizon". Every layer from the soil surface to >= 100 cm must carry
  # a remodelling layer_origin (relocated, remodelled, deep ploughing, cut and
  # fill, refilled, land levelling); a B or E horizon, or a missing
  # designation, in that depth may be a horizon formed after remodelling, and
  # gives NA. Until v0.9.217 one matching layer anywhere above 100 cm passed,
  # and "terraced" and "aterrado" (land fill, i.e. Transportic) counted.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Relocatic"
  pat <- "relocat|remodel|deep.?plou?gh|deep.?plow|rigol|cut.?(and.?)?fill|refill|land.?levell?"
  origin <- h$layer_origin
  status <- ifelse(is.na(origin), NA, grepl(pat, origin, ignore.case = TRUE))
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord])]
  w <- if (length(ord)) .q_thick_from(h, status, ord[1L], 100)
       else list(passed = NA, layers = integer(0))
  d <- h$designation[w$layers]
  new_horizon <- isTRUE(w$passed) && (anyNA(d) || any(grepl("^[0-9]*[BE]", d)))
  passed <- if (new_horizon) NA else w$passed
  DiagnosticResult$new(
    name = "Relocatic", passed = passed,
    layers = if (isTRUE(passed)) w$layers else integer(0),
    evidence = list(pattern = pat, remodelled_layers = w$layers,
                    designations = d),
    missing = if (is.na(passed))
                if (new_horizon) "designation (no horizon formed after remodelling)"
                else "layer_origin"
              else character(0),
    reference = ref
  )
}


#' Isolatic qualifier (il): soil isolated above a barrier (roofs, pots)
#'
#' WRB 2022 Ch 5: fine-earth soil material above technic hard material, a
#' geomembrane or a continuous layer of artefacts starting <= 100 cm, without
#' contact to other fine-earth soil material.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_isolatic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Isolatic: "having, above technic hard material,
  # above a geomembrane or above a continuous layer of artefacts starting
  # <= 100 cm from the soil surface, soil material containing fine earth
  # without any contact to other soil material containing fine earth (e.g.
  # soils on roofs or in pots)". soilKey holds the barriers but not whether
  # the soil above them touches other soil, so the qualifier is FALSE when
  # the data show no barrier within 100 cm (no technic hard material, no
  # geomembrane, every layer < 95% artefacts) and NA otherwise. Until v0.9.217
  # it passed on 5-50% urbic or industrial artefacts, which says nothing about
  # isolation.
  h <- pedon$horizons
  up <- which(!is.na(h$top_cm) & h$top_cm <= 100)
  thm <- .q_technic_hard(pedon)
  geo <- h$geomembrane_present %||% rep(NA, nrow(h))
  art <- h$artefacts_pct
  barrier <- length(intersect(thm$layers, up)) > 0L || any(geo[up] %in% TRUE)
  none <- !barrier && !is.na(thm$status) &&
          !length(intersect(thm$unsure, up)) &&
          all(geo[up] %in% FALSE) && all(!is.na(art[up]) & art[up] < 95)
  passed <- if (none) FALSE else NA
  DiagnosticResult$new(
    name = "Isolatic", passed = passed, layers = integer(0),
    evidence = list(barrier_within_100cm = barrier,
                    technic_hard_layers = thm$layers,
                    geomembrane_present = geo[up], artefacts_pct = art[up]),
    missing = if (is.na(passed))
                c(if (!barrier) c("geomembrane_present", "artefacts_pct"),
                  "contact of the soil with other soil material (not in the schema)")
              else character(0),
    reference = "WRB (2022) Ch 5, Isolatic"
  )
}


# ============================================================================
# SUPPLEMENTARY QUALIFIERS (SQ) -- v0.9.63 batch
# ============================================================================
#
# SQs are typically modifiers (Endo-, Epi-, Bathy-, Hyper-, Hypo-,
# Proto-) of an existing diagnostic. The patterns are stereotyped:
#
#   Endo-X     : X applies only at depth >= 50 cm
#   Epi-X      : X applies only in upper 50 cm
#   Bathy-X    : X applies very deep (100-200 cm)
#   Hyper-X    : X with extreme intensity (chemistry > threshold + N)
#   Hypo-X     : X with weak intensity
#   Proto-X    : early-stage / borderline X
# =============================================================================


# v0.9.217: Epi- and Endo- forms of Dystric and Eutric (Ch 2.3.1, rule 3).
# Epi-: "the characteristic is present in the major part (or half or more of
# the part) between the specified upper limit and 50 cm ... and is absent in
# the major part (or half or more of the part) between the specified upper
# limit and 100 cm ... or ... a limiting layer starting > 50 cm"; Endo-: present
# between 50 and 100 cm (or the limiting layer), absent over the same 20-100 cm.
# "These additional subqualifiers are only allowed together with the
# predominant qualifier" (Epidystric Eutric, Epieutric Dystric), so "absent"
# over 20-100 cm means the opposite qualifier holds there: Eutric for
# Epi/Endodystric, Dystric for Epi/Endoeutric. Until v0.9.217 only the
# presence part was tested, so a soil Dystric throughout was also Epidystric.
.wrb_epi_endo_status <- function(pedon, name, side, part) {
  h <- pedon$horizons
  ref <- paste0("WRB (2022) Ch 5 and Ch 2.3.1, ", name)
  lim <- .barrier_top_cm(pedon)
  if (!is.na(lim) && lim <= 50)
    return(DiagnosticResult$new(name = name, passed = FALSE, layers = integer(0),
      evidence = list(limiting_layer_top_cm = lim,
                      reason = "limiting layer <= 50 cm: no separate upper and lower part"),
      missing = character(0), reference = ref))
  bot <- if (is.na(lim)) 100 else min(100, lim)
  win <- if (identical(part, "epi")) c(20, 50) else c(50, bot)
  fp <- .wrb_acidity_fracs(pedon, win[1], win[2], factor = 1)
  ff <- .wrb_acidity_fracs(pedon, 20, bot, factor = 1)
  dys <- function(f) if (f$total == 0) NA else if (f$dystric >= 0.5) TRUE
                     else if (f$dystric + f$na < 0.5) FALSE else NA
  eut <- function(f) if (f$total == 0) NA else if (f$eutric > 0.5) TRUE
                     else if (f$eutric + f$na <= 0.5) FALSE else NA
  present  <- if (side == "dystric") dys(fp) else eut(fp)
  opposite <- if (side == "dystric") eut(ff) else dys(ff)
  reported <- isTRUE(suppressWarnings(max(h$bottom_cm, na.rm = TRUE)) >= bot)
  passed <- if (isFALSE(present) || isFALSE(opposite)) FALSE
            else if (isTRUE(present) && isTRUE(opposite) && reported) TRUE
            else NA
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) (if (side == "dystric") fp$layers_d else fp$layers_e)
             else integer(0),
    evidence = list(part_cm = win, whole_cm = c(20, bot),
                    part_dystric_frac = fp$dystric, part_eutric_frac = fp$eutric,
                    whole_dystric_frac = ff$dystric, whole_eutric_frac = ff$eutric),
    missing = if (!is.na(passed)) character(0)
              else if (!reported) "horizons to 100 cm"
              else c("al_cmol", "al_sat_pct"),
    reference = ref)
}


#' Endodystric qualifier: Dystric only between 50 and 100 cm
#'
#' WRB 2022 Ch 5 Dystric with the Endo- specifier (Ch 2.3.1, rule 3).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endodystric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Dystric: "exchangeable Al > exchangeable
  # (Ca+Mg+K+Na) in half or more of their combined thickness" (20-100 cm);
  # Endo-: present between 50 and 100 cm and absent over 20-100 cm (where the
  # soil is Eutric). Was the 50-100 cm presence test alone.
  .wrb_epi_endo_status(pedon, "Endodystric", "dystric", "endo")
}


#' Epidystric qualifier: Dystric only between 20 and 50 cm
#'
#' WRB 2022 Ch 5 Dystric with the Epi- specifier (Ch 2.3.1, rule 3).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_epidystric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Dystric (exchangeable Al > bases in half or more
  # of 20-100 cm); Epi-: present between 20 and 50 cm and absent over
  # 20-100 cm (where the soil is Eutric). Was the 20-50 cm presence test alone.
  .wrb_epi_endo_status(pedon, "Epidystric", "dystric", "epi")
}


#' Endoeutric supplementary qualifier: Eutric only between 50 and 100 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endoeutric <- function(pedon) {
  # v0.9.217: same rule as Endodystric, the other way round (WRB 2022 Ch 5
  # Eutric with the Endo- specifier of Ch 2.3.1): Eutric between 50 and 100 cm
  # and Dystric over 20-100 cm. Was the 50-100 cm presence test alone.
  .wrb_epi_endo_status(pedon, "Endoeutric", "eutric", "endo")
}


#' Epieutric qualifier: Eutric only between 20 and 50 cm
#'
#' WRB 2022 Ch 5 Eutric with the Epi- specifier (Ch 2.3.1, rule 3).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_epieutric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Eutric: "exchangeable (Ca+Mg+K+Na) >= exchangeable
  # Al in the major part of their combined thickness" (20-100 cm); Epi-:
  # present between 20 and 50 cm and absent over 20-100 cm (where the soil is
  # Dystric). Was the 20-50 cm presence test alone, which every soil with a
  # base-rich upper subsoil passed.
  .wrb_epi_endo_status(pedon, "Epieutric", "eutric", "epi")
}


#' Endoabruptic qualifier: abrupt textural difference at > 50-100 cm only
#'
#' WRB 2022 Ch 5 Abruptic with the Endo- specifier (Ch 2.3.1, rule 1).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endoabruptic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Abruptic: "having an abrupt textural difference
  # within 100 cm of the mineral soil surface (1)"; Endo- for a point of depth
  # (Ch 2.3.1, rule 1): "present somewhere > 50 cm ... and is absent <= 50 cm".
  # The old code accepted a difference on any horizon overlapping 50-200 cm and
  # did not check the upper 50 cm. The depth of a difference is the top of the
  # finer-textured horizon (the layer abrupt_textural_difference() returns).
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Endoabruptic"
  atd <- abrupt_textural_difference(pedon)
  at <- h$top_cm[atd$layers]
  deep    <- atd$layers[!is.na(at) & at > 50 & at <= 100]
  shallow <- atd$layers[!is.na(at) & at <= 50]
  # Horizon boundaries that could not be tested (clay or depths missing).
  tested <- suppressWarnings(as.integer(names(atd$evidence$layer_pairs)))
  bound <- which(seq_len(nrow(h)) >= 2L & !is.na(h$top_cm) & h$top_cm >= 10)
  untested <- setdiff(bound, tested)
  untested_sh <- untested[h$top_cm[untested] <= 50]
  untested_dp <- untested[h$top_cm[untested] > 50 & h$top_cm[untested] <= 100]
  reported <- .q_reported_to_100(pedon)
  passed <- if (length(shallow)) FALSE
            else if (length(deep)) (if (length(untested_sh) || !reported) NA else TRUE)
            else if (length(untested_dp) || is.na(atd$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "Endoabruptic", passed = passed,
    layers = if (isTRUE(passed)) deep else integer(0),
    evidence = list(abrupt_textural_difference = atd,
                    depths_cm = at, untested_boundaries = untested),
    missing = if (!is.na(passed)) character(0)
              else if (length(deep) && !length(untested_sh)) "horizons to 100 cm"
              else "clay_pct",
    reference = ref)
}


#' Endoleptic qualifier: continuous rock starting > 50 and <= 100 cm
#'
#' WRB 2022 Ch 5 Leptic with the Endo- specifier (Ch 2.3.1, rule 1).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endoleptic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Leptic: "having continuous rock starting <= 100 cm
  # from the soil surface (1: Epi- and Endo- only)"; Endo- for a point of
  # depth: present > 50 cm and absent <= 50 cm, so the rock starts > 50 and
  # <= 100 cm. The old code took leptic_features() layers starting >= 50 cm,
  # which also counted rock at exactly 50 cm. Now continuous_rock(), the
  # diagnostic qual_leptic() uses.
  h <- pedon$horizons
  rk <- continuous_rock(pedon)
  if (!isTRUE(rk$passed)) {
    return(DiagnosticResult$new(
      name = "Endoleptic", passed = rk$passed, layers = integer(0),
      evidence = list(continuous_rock = rk),
      missing = rk$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Endoleptic"))
  }
  top <- suppressWarnings(min(h$top_cm[rk$layers], na.rm = TRUE))
  passed <- is.finite(top) && top > 50 && top <= 100
  DiagnosticResult$new(
    name = "Endoleptic", passed = passed,
    layers = if (passed) rk$layers else integer(0),
    evidence = list(continuous_rock = rk, rock_top_cm = top),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Endoleptic"
  )
}


#' Endothionic qualifier: thionic horizon starting >= 50 and <= 100 cm
#'
#' WRB 2022 Ch 5 Thionic with the Endo- specifier (Ch 2.3.1, rule 2).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endothionic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Thionic: "having a thionic horizon starting
  # <= 100 cm from the soil surface (2)"; Endo- for a layer: "the layer starts
  # >= 50 cm ...; and no such layer occurs < 50 cm". The old code read the
  # SiBCS carater tionico and accepted any depth down to 200 cm. Now the WRB
  # thionic horizon, thionic(); horizons above 50 cm whose pH or sulfidic S is
  # missing may hide a shallower thionic horizon (NA).
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Endothionic"
  th <- thionic(pedon)
  if (!isTRUE(th$passed)) {
    return(DiagnosticResult$new(
      name = "Endothionic", passed = th$passed, layers = integer(0),
      evidence = list(thionic = th),
      missing = th$missing %||% character(0), reference = ref))
  }
  top <- suppressWarnings(min(h$top_cm[th$layers], na.rm = TRUE))
  up <- which(!is.na(h$top_cm) & h$top_cm < 50)
  ph <- h$ph_h2o[up]; s <- h$sulfidic_s_pct[up]
  unknown <- up[is.na(ph) | (ph <= 4 & is.na(s))]
  reported <- .q_reported_to_100(pedon)
  passed <- if (!is.finite(top) || top < 50 || top > 100) FALSE
            else if (length(unknown) || !reported) NA else TRUE
  DiagnosticResult$new(
    name = "Endothionic", passed = passed,
    layers = if (isTRUE(passed)) th$layers else integer(0),
    evidence = list(thionic = th, thionic_top_cm = top),
    missing = if (!is.na(passed)) character(0)
              else if (length(unknown)) c("ph_h2o", "sulfidic_s_pct")
              else "horizons to 100 cm",
    reference = ref)
}


#' Hypernatric supplementary qualifier (jn): natric horizon with ESP >= 15
#' throughout its upper 40 cm
#'
#' WRB 2022 Ch 5 (subqualifier of Natric): "having a natric horizon with an
#' exchangeable Na percentage (ESP) of >= 15 throughout the entire natric
#' horizon or within its upper 40 cm, whichever is thinner."
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hypernatric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Hypernatric: "having a natric horizon with an
  # exchangeable Na percentage (ESP) of >= 15 throughout the entire natric
  # horizon or within its upper 40 cm, whichever is thinner", the natric
  # horizon "starting <= 100 cm from the mineral soil surface" (Natric). The
  # horizon is natric_horizon() together with the argic layers it lies in;
  # ESP (na_cmol / cec_cmol) must be >= 15 in every layer of its upper 40 cm.
  # Until v0.9.217 it passed on ESP >= 70 in any layer above 100 cm, with no
  # natric horizon.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Hypernatric"
  nat <- natric_horizon(pedon)
  if (!isTRUE(nat$passed))
    return(DiagnosticResult$new(
      name = "Hypernatric", passed = nat$passed, layers = integer(0),
      evidence = list(natric = nat),
      missing = nat$missing %||% character(0), reference = ref))
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  body <- union(nat$layers, nat$evidence$argic$layers %||% integer(0))
  first <- nat$layers[which.min(h$top_cm[nat$layers])]
  run <- Filter(function(r) first %in% r$layers, .q_layer_runs(h, body))[[1L]]
  bot <- min(run$bottom, run$top + 40)
  upper <- run$layers[h$top_cm[run$layers] < bot]
  esp <- vapply(upper, function(i) compute_esp(h$na_cmol[i], h$cec_cmol[i]),
                numeric(1))
  passed <- if (run$top > ms + 100 || any(!is.na(esp) & esp < 15)) FALSE
            else if (anyNA(esp)) NA
            else TRUE
  DiagnosticResult$new(
    name = "Hypernatric", passed = passed,
    layers = if (isTRUE(passed)) upper else integer(0),
    evidence = list(natric = nat, natric_top_cm = run$top,
                    upper_part_cm = c(run$top, bot), esp_pct = esp),
    missing = if (is.na(passed)) c("na_cmol", "cec_cmol") else character(0),
    reference = ref
  )
}


#' Sulfatic supplementary qualifier (su): sulfate-dominated salic horizon
#'
#' WRB 2022 Ch 5 (Solonchaks): a salic horizon whose 1:1 soil solution has
#' [SO4] > 2*[HCO3] > 2*[Cl].
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_sulfatic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Sulfatic: "having a salic horizon with a soil
  # solution (1:1 in water) with [SO42-] > 2*[HCO3-] > 2*[Cl-] (in Solonchaks
  # only)". Reads salic(); soilKey holds no anion composition of the soil
  # solution, so a soil with a salic horizon is NA and one without is FALSE.
  # Until v0.9.217 it read an so4_pct column that the schema does not have and
  # tested >= 5% sulfate.
  sal <- salic(pedon)
  passed <- if (isTRUE(sal$passed)) NA else sal$passed
  DiagnosticResult$new(
    name = "Sulfatic", passed = passed, layers = integer(0),
    evidence = list(salic = sal),
    missing = if (is.na(passed))
                c(if (is.na(sal$passed)) sal$missing %||% character(0),
                  "SO4, HCO3 and Cl of the 1:1 soil solution (not in the schema)")
              else character(0),
    reference = "WRB (2022) Ch 5, Sulfatic"
  )
}


#' Carbonic qualifier (cx): >= 5\% organic carbon belonging to artefacts
#'
#' WRB 2022 Ch 5: a layer >= 10 cm thick, starting <= 100 cm, with >= 5\%
#' organic carbon that belongs to artefacts.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_carbonic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Carbonic: "having a layer, >= 10 cm thick and
  # starting <= 100 cm from the soil surface, with >= 5% organic carbon that
  # belongs to artefacts". The old code counted any organic carbon >= 5%, so
  # every peat or humus-rich topsoil was Carbonic. The share of organic carbon
  # held in artefacts is not in the schema. A horizon is ruled out when its
  # total organic carbon is < 5% or it has no artefacts (artefacts_pct 0):
  # FALSE when no 10-cm layer is left, NA otherwise.
  h <- pedon$horizons
  oc  <- h$oc_pct %||% rep(NA_real_, nrow(h))
  art <- h$artefacts_pct %||% rep(NA_real_, nrow(h))
  ruled_out <- (!is.na(oc) & oc < 5) | (!is.na(art) & art <= 0)
  open <- .q_thick_runs(h, which(!ruled_out), 10, 100)
  passed <- if (length(open)) NA else FALSE
  DiagnosticResult$new(
    name = "Carbonic", passed = passed, layers = integer(0),
    evidence = list(oc_pct = oc, artefacts_pct = art, threshold_oc_pct = 5,
                    open_layers = unlist(lapply(open, `[[`, "layers"))),
    missing = if (is.na(passed)) "organic carbon in artefacts" else character(0),
    reference = "WRB (2022) Ch 5, Carbonic"
  )
}


#' Carbonatic qualifier (cn): bicarbonate-dominated salic horizon
#'
#' WRB 2022 Ch 5: a salic horizon whose 1:1 soil solution has pH >= 8.5 and
#' [HCO3-] > [SO4 2-] > 2*[Cl-] (in Solonchaks only).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_carbonatic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Carbonatic: "having a salic horizon with a soil
  # solution (1:1 in water) with a pH of >= 8.5 and [HCO3-] > [SO42-] >
  # 2*[Cl-] (in Solonchaks only)". The old code tested >= 50% CaCO3, a
  # different property. The schema holds no anion concentrations of the soil
  # solution: FALSE without a salic horizon, NA with one.
  sal <- salic(pedon)
  if (!isTRUE(sal$passed)) {
    return(DiagnosticResult$new(
      name = "Carbonatic", passed = sal$passed, layers = integer(0),
      evidence = list(salic = sal),
      missing = sal$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Carbonatic"))
  }
  DiagnosticResult$new(
    name = "Carbonatic", passed = NA, layers = integer(0),
    evidence = list(salic = sal, ph_h2o = pedon$horizons$ph_h2o[sal$layers]),
    missing = c("hco3_soil_solution", "so4_soil_solution", "cl_soil_solution"),
    reference = "WRB (2022) Ch 5, Carbonatic"
  )
}


#' Hydrophobic qualifier (hf): water-repellent soil surface
#'
#' WRB 2022 Ch 5: water stands on the dry soil surface for >= 60 seconds (in
#' Arenosols only).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_hydrophobic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Hydrophobic: "water-repellent, i.e. water stands
  # on a dry soil surface for >= 60 seconds (in Arenosols only)". The old code
  # searched the yermic vesicular_pores field for the words "water repellent",
  # which records neither the test nor its duration. The schema has no water
  # drop penetration time: NA.
  DiagnosticResult$new(
    name = "Hydrophobic", passed = NA, layers = integer(0),
    evidence = list(reason = "no water drop penetration time on the dry soil surface"),
    missing = "water_drop_penetration_time_s",
    reference = "WRB (2022) Ch 5, Hydrophobic"
  )
}


#' Pyric supplementary qualifier (py): >= 5\% visible black carbon in
#' >= 10 cm within 100 cm
#'
#' WRB 2022 Ch 5: layers with a combined thickness of >= 10 cm within 100 cm
#' with >= 5\% (by exposed area) visible black carbon, not part of a pretic
#' horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_pyric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Pyric: "having within 100 cm of the soil surface
  # one or more layers with a combined thickness of >= 10 cm with >= 5% (by
  # exposed area, related to the fine earth plus black carbon of any size)
  # visible black carbon and not forming part of a pretic horizon". soilKey
  # holds no amount of visible black carbon (charcoal), so the result is NA.
  # Until v0.9.217 it passed on any layer_origin or designation mentioning
  # burning or charcoal ("burn", "charcoal", "fogo", ...), with no amount, no
  # thickness and no depth limit.
  DiagnosticResult$new(
    name = "Pyric", passed = NA, layers = integer(0),
    evidence = list(reason = "visible black carbon is not in the schema"),
    missing = "visible black carbon, % of exposed area (not in the schema)",
    reference = "WRB (2022) Ch 5, Pyric"
  )
}


#' Lignic supplementary qualifier (lg): >= 25\% intact wood within 50 cm
#'
#' WRB 2022 Ch 5: intact wood fragments making up >= 25\% of the soil volume
#' (related to the fine earth plus all dead plant residues) within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_lignic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Lignic: "having inclusions of intact wood
  # fragments that make up >= 25% of the soil volume (related to the fine
  # earth plus all dead plant residues), within 50 cm from the soil surface".
  # Read as the share of wood in the soil volume of the upper 50 cm (or down
  # to a limiting layer), from woody_fragments_pct. That column counts wood
  # >= 2 cm related to the whole soil, so it can only understate the share:
  # >= 25 (unmeasured layers counted as 0) proves the qualifier. For FALSE
  # every layer must be measured, the share is related to the fine earth plus
  # residues (divided by 1 - coarse fragments) and must stay < 25 (finer wood
  # is still not counted). Until v0.9.217 one layer with >= 25% passed at any
  # depth, and so did any layer_origin mentioning wood.
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Lignic"
  wf <- h$woody_fragments_pct %||% rep(NA_real_, nrow(h))
  lim <- .barrier_top_cm(pedon)
  depth <- if (!is.na(lim) && lim < 50) lim else 50
  ov <- pmax(0, pmin(h$bottom_cm, depth) - pmax(h$top_cm, 0))
  win <- which(!is.na(ov) & ov > 0)
  if (depth <= 0 || !length(win) || all(is.na(wf[win])))
    return(DiagnosticResult$new(
      name = "Lignic", passed = NA, layers = integer(0),
      evidence = list(reason = "no woody_fragments_pct within 50 cm"),
      missing = "woody_fragments_pct", reference = ref))
  known <- win[!is.na(wf[win])]
  share_min <- sum(wf[known] * ov[known]) / depth
  rel <- wf[win] / (1 - h$coarse_fragments_pct[win] / 100)
  full <- sum(ov[win]) >= depth - 1e-6 && !anyNA(rel)
  share_rel <- if (full) sum(rel * ov[win]) / depth else NA_real_
  passed <- if (share_min >= 25) TRUE
            else if (full && share_rel < 25) FALSE
            else NA
  DiagnosticResult$new(
    name = "Lignic", passed = passed,
    layers = if (isTRUE(passed)) known[wf[known] > 0] else integer(0),
    evidence = list(wood_share_whole_soil_pct = share_min,
                    wood_share_fine_earth_pct = share_rel, depth_cm = depth,
                    threshold = 25),
    missing = if (is.na(passed)) c("woody_fragments_pct", "coarse_fragments_pct")
              else character(0),
    reference = ref
  )
}


#' Bathyspodic qualifier: spodic horizon starting > 200 cm
#'
#' WRB 2022 Ch 5 Spodic with the Bathy- specifier (Ch 2.3.1, rule 6; Ch 4,
#' Arenosols footnote).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_bathyspodic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Spodic: "having a spodic horizon starting
  # <= 200 cm from the mineral soil surface"; Bathy- "extends to a greater
  # depth than specified for the qualifier" (Ch 2.3.1), and the Arenosol key
  # (Ch 4, footnote 11) gives "Bathyspodic (> 200 cm)". The old code took a
  # spodic horizon in 100-200 cm, which is plain Spodic. Now the shallowest
  # spodic horizon starts > 200 cm; horizons above 200 cm without oxalate
  # Al and Fe may hide a shallower one (NA).
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Bathyspodic"
  sp <- spodic(pedon)
  if (!isTRUE(sp$passed)) {
    return(DiagnosticResult$new(
      name = "Bathyspodic", passed = sp$passed, layers = integer(0),
      evidence = list(spodic = sp),
      missing = sp$missing %||% character(0), reference = ref))
  }
  top <- suppressWarnings(min(h$top_cm[sp$layers], na.rm = TRUE))
  up <- which(!is.na(h$top_cm) & h$top_cm <= 200)
  unknown <- up[is.na(h$al_ox_pct[up]) | is.na(h$fe_ox_pct[up])]
  passed <- if (!is.finite(top) || top <= 200) FALSE
            else if (length(unknown)) NA else TRUE
  DiagnosticResult$new(
    name = "Bathyspodic", passed = passed,
    layers = if (isTRUE(passed)) sp$layers else integer(0),
    evidence = list(spodic = sp, spodic_top_cm = top),
    missing = if (is.na(passed)) c("al_ox_pct", "fe_ox_pct") else character(0),
    reference = ref)
}


#' Cohesic qualifier (co): cohesic horizon starting <= 150 cm
#'
#' WRB 2022 Ch 5: a cohesic horizon (Ch 3.1.7) starting <= 150 cm from the
#' mineral soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cohesic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Cohesic: "having a cohesic horizon starting
  # <= 150 cm from the mineral soil surface (2)". The old code matched "very
  # firm" moist consistence or a consistence_dry column the schema does not
  # have (cohesic horizons are "friable or firm when moist"). Now the cohesic
  # horizon diagnostic, cohesic() (Ch 3.1.7: < 0.5% SOC, >= 15% clay, CEC
  # < 24 cmolc/kg clay, massive or weak subangular blocky, not cemented, at
  # least hard when dry, >= 10 cm).
  h <- pedon$horizons
  coh <- cohesic(pedon)
  ly <- coh$layers[!is.na(h$top_cm[coh$layers]) & h$top_cm[coh$layers] <= 150]
  passed <- if (isTRUE(coh$passed)) length(ly) > 0L else coh$passed
  DiagnosticResult$new(
    name = "Cohesic", passed = passed,
    layers = if (isTRUE(passed)) ly else integer(0),
    evidence = list(cohesic = coh),
    missing = if (is.na(passed)) coh$missing %||% character(0) else character(0),
    reference = "WRB (2022) Ch 5, Cohesic"
  )
}


#' Inclinic supplementary qualifier (ic): wet layer on a slope with
#' subsurface water flow
#'
#' WRB 2022 Ch 5: slope >= 5\% and a layer >= 25 cm thick, starting <= 75 cm,
#' with gleyic or stagnic properties and a subsurface water flow for some
#' time during the year.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_inclinic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Inclinic: "having a slope inclination of >= 5%,
  # and a layer, >= 25 cm thick and starting <= 75 cm from the mineral soil
  # surface, with gleyic or stagnic properties and a subsurface water flow for
  # some time during the year". Reads site$slope_pct, gleyic_properties() and
  # stagnic_properties(). soilKey holds no record of subsurface water flow,
  # so the qualifier is FALSE where the slope or the wet layer rules it out
  # and NA otherwise. Until v0.9.217 a slope >= 10% (or a hilly relief class)
  # was enough.
  h <- pedon$horizons
  slope <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA_real_))[1]
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  gl <- gleyic_properties(pedon, max_top_cm = ms + 100)
  st <- stagnic_properties(pedon, max_top_cm = ms + 100)
  wet <- union(if (isTRUE(gl$passed)) gl$layers else integer(0),
               if (isTRUE(st$passed)) st$layers else integer(0))
  layer_ok <- if (length(.q_thick_runs(h, wet, 25, max_top = ms + 75))) TRUE
              else if (is.na(gl$passed) || is.na(st$passed)) NA
              else FALSE
  slope_ok <- if (is.na(slope)) NA else slope >= 5
  passed <- if (isFALSE(slope_ok) || isFALSE(layer_ok)) FALSE else NA
  DiagnosticResult$new(
    name = "Inclinic", passed = passed, layers = integer(0),
    evidence = list(slope_pct = slope, gleyic = gl, stagnic = st,
                    wet_layers = wet),
    missing = if (is.na(passed))
                c(if (is.na(slope)) "site$slope_pct",
                  if (is.na(layer_ok)) "redoximorphic_features_pct",
                  "subsurface water flow (not in the schema)")
              else character(0),
    reference = "WRB (2022) Ch 5, Inclinic"
  )
}


#' Gelic qualifier (ge): permafrost within 200 cm, outside the Cryosol case
#'
#' WRB 2022 Ch 5: a layer below 0 degC for >= 2 years starting <= 200 cm, no
#' cryic horizon starting <= 100 cm, and no cryic horizon starting <= 200 cm
#' with cryogenic alteration within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gelic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Gelic: "having a layer with a soil temperature of
  # < 0 degC for >= 2 consecutive years, starting <= 200 cm from the soil
  # surface, and not having a cryic horizon starting <= 100 cm from the soil
  # surface, and not having a cryic horizon starting <= 200 cm from the soil
  # surface with evidence of cryogenic alteration in some layer within 100 cm".
  # The old code returned the cryic test at <= 100 cm, i.e. the Cryosol case
  # that Gelic excludes. Frozen layer: permafrost_temp_C < 0 or a frozen (f)
  # designation; cryic horizon: cryic_horizon(); cryogenic alteration:
  # cryoturbation_usda() (jj/@ designations, broken or involuted boundaries).
  h <- pedon$horizons
  ref <- "WRB (2022) Ch 5, Gelic"
  res <- function(passed, layers = integer(0), missing = character(0), ...)
    DiagnosticResult$new(name = "Gelic", passed = passed, layers = layers,
                         evidence = list(...), missing = missing, reference = ref)
  top <- h$top_cm
  t <- h$permafrost_temp_C %||% rep(NA_real_, nrow(h))
  cy200 <- cryic_horizon(pedon, max_top_cm = 200)
  fdes <- cy200$evidence$designation$frozen_designation$layers %||% integer(0)
  frozen <- sort(union(which(!is.na(t) & t < 0 & !is.na(top) & top <= 200),
                       fdes[!is.na(top[fdes]) & top[fdes] <= 200]))
  if (!length(frozen)) {
    up <- which(!is.na(top) & top <= 200)
    warm <- length(up) > 0L && all(!is.na(t[up]) & t[up] >= 0) &&
              isTRUE(max(h$bottom_cm, na.rm = TRUE) >= 200)
    return(res(if (warm) FALSE else NA,
               missing = if (warm) character(0) else "permafrost_temp_C",
               reason = "no layer below 0 degC found within 200 cm"))
  }
  # Not a cryic horizon starting <= 100 cm (the Cryosol case). Where
  # temperatures are missing above 100 cm, horizons described without the
  # frozen suffix, in a profile that uses it deeper down, count as not frozen.
  cy100 <- cryic_horizon(pedon)
  if (isTRUE(cy100$passed))
    return(res(FALSE, cryic_top_cm = min(top[cy100$layers], na.rm = TRUE)))
  up100 <- which(!is.na(top) & top <= 100)
  if (is.na(cy100$passed) &&
        !(length(fdes) && all(!is.na(h$designation[up100]))))
    return(res(NA, missing = if (length(cy100$missing)) cy100$missing
                             else "permafrost_temp_C",
               frozen_layers = frozen))
  # A cryic horizon starting > 100 and <= 200 cm: Gelic only without evidence
  # of cryogenic alteration within 100 cm.
  if (isTRUE(cy200$passed) && length(cy200$layers)) {
    ct <- cryoturbation_usda(pedon)
    alt <- ct$layers[!is.na(top[ct$layers]) & top[ct$layers] < 100]
    if (length(alt)) return(res(FALSE, cryogenic_alteration_layers = alt))
    up <- which(!is.na(top) & top < 100)
    seen <- any(!is.na(h$designation[up]) | !is.na(h$boundary_topography[up]))
    if (!seen)
      return(res(NA, missing = c("designation", "boundary_topography"),
                 frozen_layers = frozen))
  }
  res(TRUE, layers = frozen, frozen_layers = frozen,
      cryic_top_cm = suppressWarnings(min(top[cy200$layers], na.rm = TRUE)))
}
