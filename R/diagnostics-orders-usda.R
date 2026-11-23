# ============================================================================
# USDA Soil Taxonomy 13th Ed. (2022) -- Order-level diagnostics (Cap 4
# Order Key, pp 65-72). 12 orders in canonical key order:
#   A. Gelisols    -- gelic conditions / permafrost
#   B. Histosols   -- organic materials >= 40 cm
#   C. Spodosols   -- spodic horizon
#   D. Andisols    -- andic soil properties >= 60% of thickness
#   E. Oxisols     -- oxic horizon + kandic + 40% clay (already wired
#                       via oxic_usda() in diagnostics-horizons-usda.R)
#   F. Vertisols   -- slickensides + cracks open/close
#   G. Aridisols   -- aridic moisture regime + ochric/anthropic
#   H. Ultisols    -- argillic/kandic + base saturation < 35%
#   I. Mollisols   -- mollic epipedon + base saturation >= 50%
#   J. Alfisols    -- argillic/kandic/natric horizon (no base saturation test)
#   K. Inceptisols -- cambic / various subsurface diagnostics
#   L. Entisols    -- catch-all
#
# Each order's gate must enforce: (1) the order's diagnostic horizon /
# property; (2) the exclusion of all orders that come earlier in the
# key. This ensures the chave is mutually exclusive.
# ============================================================================


# v0.9.420: the base saturation of the Ultisols and Alfisols is read at the
# depth of the KST in .ultisol_usda_uncached() (R/diagnostics-ultisols-usda.R
# has the readings); the mean over the WRB argic layers and its inference from
# Al saturation or pH (not in the KST; audit items U2, G5) are gone.


# ---- A. Gelisols (Cap 9, p 189) -------------------------------------------

#' Gelisols (USDA, KST 13th ed., p. 67, key to soil orders, item A)
#'
#' v0.9.418: soils that have (1) permafrost within 100 cm of the soil
#' surface, or (2) gelic materials within 100 cm of the soil surface and
#' permafrost within 200 cm. Permafrost is read by
#' \code{permafrost_within_usda()} (a layer at or below 0 C, or a frozen
#' designation), gelic materials by \code{gelic_materials_usda()}. Up to
#' v0.9.417 the WRB cryic conditions within 100 cm decided and item 2 was
#' not read.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
gelisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "gelisol_usda", list(),
        .gelisol_usda_uncached(pedon = pedon))
}

# The computation of gelisol_usda().
.gelisol_usda_uncached <- function(pedon) {
  pf100 <- permafrost_within_usda(pedon, max_top_cm = 100)
  pf200 <- permafrost_within_usda(pedon, max_top_cm = 200)
  gel <- gelic_materials_usda(pedon, max_top_cm = 100)
  item2 <- isTRUE(gel$passed) && isTRUE(pf200$passed)
  passed <- isTRUE(pf100$passed) || item2
  DiagnosticResult$new(
    name = "gelisol_usda", passed = passed,
    layers = if (isTRUE(pf100$passed)) pf100$layers else if (item2) union(gel$layers, pf200$layers) else integer(0),
    evidence = list(permafrost_100 = pf100, permafrost_200 = pf200,
                    gelic_materials_100 = gel),
    missing = if (passed) character(0) else pf100$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., p. 67 (key to soil orders, item A)"
  )
}


# ---- B. Histosols (Cap 10, p 199) -----------------------------------------

#' Histosols (USDA, KST 13th ed., pp. 67-68, key to soil orders, item B)
#'
#' Reads \code{histosol_qualifying_usda()} (organic soil materials by items
#' a to d, no andic soil properties in 60 percent of the upper 60 cm) and
#' excludes the Gelisols.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
histosol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "histosol_usda", list(),
        .histosol_usda_uncached(pedon = pedon))
}

# The computation of histosol_usda().
.histosol_usda_uncached <- function(pedon) {
  hi <- histosol_qualifying_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- Histic / Folistic / Hemic / Sapric
  # / Fibric surveyor identification flips an NA histosol_qualifying
  # to TRUE.
  hi <- .apply_nasis_tiebreaker(hi, pedon,
                                 pattern       = "^Histic epipedon$|^Folistic epipedon$|^Hemic|^Sapric|^Fibric|^Limnic|^Coprogenous",
                                 feature_label = "Histic / Folistic / Hemic / Sapric / Fibric materials")
  ge <- gelisol_usda(pedon)
  passed <- if (isTRUE(ge$passed)) FALSE else hi$passed
  DiagnosticResult$new(
    name = "histosol_usda", passed = passed,
    layers = hi$layers, evidence = list(histosol_qualifying = hi,
                                          gelisol_excluded = ge),
    missing = hi$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., pp. 67-68 (key to soil orders, item B)"
  )
}


# ---- C. Spodosols (Cap 14, p 311) -----------------------------------------

#' Spodosols (USDA, KST 13th ed., p. 68, key to soil orders, item C)
#'
#' v0.9.418: the key as worded. Soils that have no plaggen epipedon and no
#' argillic or kandic horizon above a spodic horizon, and that have one or
#' more of:
#' \enumerate{
#'   \item a spodic horizon, an albic horizon, and a cryic or gelic
#'     temperature regime;
#'   \item an Ap horizon with 85 percent or more spodic materials;
#'   \item a spodic horizon with all of: (a) 10 cm thick or more, an
#'     overlying Ap, cementation in 50 percent of the pedon, a texture finer
#'     than loamy fine sand with a frigid regime, or a cryic or gelic regime;
#'     (b) an upper boundary less than 50 cm below the mineral soil surface,
#'     or less than 200 cm if some horizon between the mineral soil surface
#'     and the spodic horizon is loamy fine sand or coarser; (c) a lower
#'     boundary 25 cm or more below the mineral soil surface (or at a duripan,
#'     fragipan or contact), or at any depth under the frigid or the cryic or
#'     gelic conditions of item a; and (d) a directly overlying albic horizon,
#'     or no andic soil properties in 60 percent of the upper 60 cm (C.3.d(2)).
#' }
#' The spodic horizon is \code{spodic_horizon_usda()}; an undecided layer of
#' it is tried both ways, and the order is undecided (NA) when it decides.
#' Item C.3.d(2) is read from \code{andisol_qualifying_usda()}; where that
#' is undecided, by a bound: no andic soil properties where Al + 1/2 Fe
#' (oxalate) is under 0.4 percent (KST p. 32, both branches need 0.4 or
#' more) or organic carbon is 25 percent or more, undecided otherwise
#' (reading L7). Up to v0.9.417 the WRB
#' spodic horizon alone decided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
spodosol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "spodosol_usda", list(),
        .spodosol_usda_uncached(pedon = pedon))
}

# Order key item C.3.d (KST 13th ed., p. 68; v0.9.417, audit A5): a spodic
# horizon qualifies only with either (1) a directly overlying albic horizon
# or (2) no andic soil properties in 60 % or more of the thickness of the
# Andisol window (andisol_qualifying_usda()). Applied to a passing Spodosol
# result `res`: it fails when the window has andic soil properties and no
# albic horizon lies directly on the spodic horizon, and is undecided when
# that is undecided (reading L7). Paths C.1 and C.2 are not separated here.
.spodosol_no_andic_usda <- function(pedon, res) {
  if (!isTRUE(res$passed)) return(res)
  h <- pedon$horizons
  ab <- albic_horizon_usda(pedon)
  sp_top <- suppressWarnings(min(h$top_cm[res$layers], na.rm = TRUE))
  direct <- isTRUE(ab$passed) && length(ab$layers) > 0L &&
            any(!is.na(h$bottom_cm[ab$layers]) & h$bottom_cm[ab$layers] == sp_top)
  if (direct) return(res)
  aq <- andisol_qualifying_usda(pedon)
  d <- !aq$passed
  res$passed <- res$passed & d
  res$evidence$no_andic_c3d <- aq
  if (is.na(res$passed)) res$missing <- unique(c(res$missing, aq$missing))
  res
}

# C.3.d(2) where the Andisol test is undecided: TRUE when the layers that
# cannot have andic soil properties (Al + 1/2 Fe under 0.4 %, or 25 % organic
# carbon or more) make 60 % or more of the upper 60 cm of the mineral soil (or
# of the soil above a contact, duripan or petrocalcic horizon), NA otherwise.
.spodosol_no_andic_bound <- function(pedon, ms) {
  h <- pedon$horizons
  d <- .desg_master(h$designation, site = pedon$site)
  stop_k <- which(!is.na(d) & (grepl("^(R|Cr|Cd)", d) | grepl("qm|km", d)) & h$top_cm >= ms)
  bot <- min(c(ms + 60, h$top_cm[stop_k]))
  if (bot <= ms) return(NA)
  alfe <- suppressWarnings(as.numeric(h$al_ox_pct) + 0.5 * as.numeric(h$fe_ox_pct))
  oc <- suppressWarnings(as.numeric(h$oc_pct))
  not_andic <- (!is.na(alfe) & alfe < 0.4) | (!is.na(oc) & oc >= 25)
  w <- pmax(pmin(h$bottom_cm, bot) - pmax(h$top_cm, ms), 0)
  if (sum(w[not_andic]) >= 0.6 * (bot - ms) - 1e-9) TRUE else NA
}

# The computation of spodosol_usda().
.spodosol_usda_uncached <- function(pedon) {
  ref <- "USDA Soil Survey Staff (2022), KST 13th ed., p. 68 (key to soil orders, item C)"
  h <- pedon$horizons
  sp <- spodic_horizon_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- "Spodic horizon" or "Spodic materials"
  # surveyor identification turns the undecided layers into spodic ones.
  nasis <- .has_nasis_feature(pedon, "^Spodic horizon$|^Spodic materials$|^Ortstein$")
  sure_layers <- sp$layers
  maybe_layers <- sp$evidence$maybe_layers %||% integer(0)
  if (isTRUE(nasis) && is.na(sp$passed)) {
    sure_layers <- sort(union(sure_layers, maybe_layers)); maybe_layers <- integer(0)
  }
  org <- sp$evidence$organic %||% .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  albic <- sp$evidence$albic_horizon
  d <- .desg_master(h$designation, site = pedon$site)
  d0 <- ifelse(is.na(d), "", d)
  ord <- order(h$top_cm)
  cold <- soil_temperature_regime_usda(pedon, c("cryic", "gelic"))$passed
  frigid <- soil_temperature_regime_usda(pedon, "frigid")$passed
  sandy <- .usda_lfs_or_coarser(h)
  cem <- .usda_cemented(h)
  share <- .usda_cemented_share(pedon)
  cem50 <- cem & (is.na(share) | share >= 50)
  ak <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  ak_layers <- if (isTRUE(ak$passed)) ak$layers else integer(0)
  # the pans and contacts that may end the spodic horizon (item c(1))
  stop_k <- which(!is.na(d) & (grepl("^(R|Cr|Cd)", d) | grepl("qm", d)))
  frag <- fragipan_usda(pedon)
  stop_k <- union(stop_k, if (isTRUE(frag$passed)) frag$layers else integer(0))
  # C.3.d(2): no andic soil properties in the Andisol window
  # (andisol_qualifying_usda(), v0.9.417); where that is undecided, the
  # bound of .spodosol_no_andic_bound() may still decide it (reading L7).
  aq_c3d <- andisol_qualifying_usda(pedon)$passed
  no_andic <- if (isFALSE(aq_c3d)) TRUE else if (isTRUE(aq_c3d)) FALSE
              else .spodosol_no_andic_bound(pedon, ms)
  # item C.3 for one spodic horizon (a run of adjoining layers)
  eval_run <- function(run) {
    run <- run[order(h$top_cm[run])]
    top <- h$top_cm[run[1L]]; bot <- max(h$bottom_cm[run])
    thk <- sum(h$bottom_cm[run] - h$top_cm[run])
    above <- ord[h$top_cm[ord] < top & !(org[ord] %in% TRUE)]
    finer <- .all3u(!sandy[run])
    a <- (thk >= 10) | any(grepl("^Ap", d0[above])) | any(cem50[run]) |
         (finer & frigid) | cold
    between <- above[h$top_cm[above] >= ms - 1e-6]
    b <- if (top - ms < 50) TRUE
         else if (top - ms < 200) .any3u(sandy[between]) else FALSE
    stops <- h$top_cm[stop_k]
    c1 <- (bot - ms >= 25) || any(abs(stops - bot) < 1e-6)
    cc <- c1 | (finer & frigid) | cold
    d1 <- if (length(above)) albic[above[length(above)]] else FALSE
    dd <- d1 | no_andic
    excl <- any(h$top_cm[ak_layers] < top)
    !excl & a & b & cc & dd
  }
  eval_set <- function(L) {
    if (!length(L)) return(FALSE)
    L <- L[order(h$top_cm[L])]
    brk <- c(TRUE, abs(h$top_cm[L][-1] - h$bottom_cm[L][-length(L)]) > 1e-6)
    runs <- split(L, cumsum(brk))
    .any3u(vapply(runs, eval_run, NA))
  }
  item3_sure <- eval_set(sure_layers)
  item3_best <- eval_set(sort(union(sure_layers, maybe_layers)))
  item3 <- if (isTRUE(item3_sure)) TRUE else if (isFALSE(item3_best)) FALSE else NA
  has_sp <- if (length(sure_layers)) TRUE else if (length(maybe_layers)) NA else FALSE
  item1 <- has_sp & .any3u(albic) & cold
  spm <- sp$evidence$spodic_materials %||% rep(FALSE, nrow(h))
  item2 <- .any3u(spm[grepl("^Ap", d0)])
  plaggen <- .usda_plaggen_surface(pedon, min_cm = 50)
  spodic_ok <- !plaggen & (item1 | item2 | item3)
  ex <- isTRUE(gelisol_usda(pedon)$passed) || isTRUE(histosol_usda(pedon)$passed)
  passed <- if (ex) FALSE else spodic_ok
  miss <- character(0)
  if (is.na(passed)) {
    miss <- sp$missing
    if (is.na(cold) || is.na(frigid)) miss <- c(miss, "site$soil_temperature_regime")
    if (is.na(no_andic)) miss <- c(miss, "al_ox_pct", "fe_ox_pct", "phosphate_retention_pct", "bulk_density_g_cm3")
  }
  DiagnosticResult$new(
    name = "spodosol_usda", passed = passed,
    layers = sure_layers,
    evidence = list(spodic = sp, items = c(C1 = item1, C2 = item2, C3 = item3),
                    plaggen = plaggen, no_andic_60 = no_andic,
                    argillic_or_kandic_layers = ak_layers, prior_order = ex),
    missing = unique(miss), reference = ref)
}


# ---- D. Andisols (Cap 6, p 117) -------------------------------------------

#' Andisols (USDA Cap 6): andic soil properties >= 60\% of thickness.
#'
#' Order key item D (KST 13th ed., p. 68), through
#' \code{andisol_qualifying_usda()}. v0.9.417: NA when the andic window is
#' undecided (up to v0.9.416 FALSE).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
andisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "andisol_usda", list(),
        .andisol_usda_uncached(pedon = pedon))
}

# The computation of andisol_usda().
.andisol_usda_uncached <- function(pedon) {
  # Refined v0.8.6: uses andisol_qualifying_usda which enforces the
  # 60% / 60 cm rule (KST 13ed, Ch 6 p 117) instead of just any
  # andic-property layer.
  aq <- andisol_qualifying_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- "Andic soil properties" or "Volcanic
  # glass" surveyor identification flips an NA andisol_qualifying to
  # TRUE.
  aq <- .apply_nasis_tiebreaker(aq, pedon,
                                 pattern       = "^Andic soil properties$|^Vitric|^Volcanic glass$",
                                 feature_label = "Andic soil properties / Volcanic glass")
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed)
  # v0.9.417: three-valued (an undecided andic window is NA, not FALSE)
  passed <- if (ex) FALSE else aq$passed
  DiagnosticResult$new(
    name = "andisol_usda", passed = passed,
    layers = aq$layers,
    evidence = list(andisol_qualifying = aq, prior_order = ex),
    missing = if (is.na(passed)) aq$missing else character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., p. 68 (order key item D)",
    notes = aq$notes %||% NA_character_
  )
}


# ---- E. Oxisols already wired via oxic_usda() ------------------------------

#' Oxisols (USDA, KST 13ed Ch 8, key to soil orders, item E, pp. 68-69)
#'
#' v0.9.309: the text of the key. "Other soils that have either: 1. an oxic
#' horizon within 150 cm of the mineral soil surface and no kandic horizon
#' within that depth; or 2. 40 percent or more (by weight) clay in the
#' fine-earth fraction between the mineral soil surface and a depth of 18 cm
#' (after mixing) and a kandic horizon that has the weatherable-mineral
#' properties of an oxic horizon and has its upper boundary within 100 cm of
#' the mineral soil surface." Since v0.9.17 soilKey had used instead "no
#' argillic horizon starting above the oxic", which turned clayey Oxisols with
#' a kandic horizon into Mollisols, Ultisols or Alfisols (ISRIC's KE009, a
#' Rhodic Eutrustox, came out a Typic Paleustoll). The weatherable minerals of
#' the kandic horizon of item 2 are rarely recorded, and are then treated as
#' meeting the oxic limit, as in the oxic horizon (v0.9.420, audit reading
#' L21). v0.9.420: the clay of the upper 18 cm is rounded to the precision of
#' the limit before it is compared with 40\% (KST endnote 1, p. 70; audit
#' reading L24), so 39.9\% is 40\%.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
oxisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "oxisol_usda", list(),
        .oxisol_usda_uncached(pedon = pedon))
}

# The computation of oxisol_usda().
.oxisol_usda_uncached <- function(pedon) {
  h <- pedon$horizons
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed)
  ox <- oxic_usda(pedon)
  ka <- kandic_horizon_usda(pedon)
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  ox_top <- if (isTRUE(ox$passed) && length(ox$layers)) min(h$top_cm[ox$layers], na.rm = TRUE) else NA_real_
  ka_top <- if (isTRUE(ka$passed) && length(ka$layers)) min(h$top_cm[ka$layers], na.rm = TRUE) else NA_real_
  # The two items, for a kandic horizon present (starting at `kt`) or absent;
  # an undecided kandic horizon decides nothing when both answers agree.
  ox150 <- if (isTRUE(ox$passed)) !is.na(ox_top) && ox_top <= ms + 150 else if (is.na(ox$passed)) NA else FALSE
  clay18 <- .q_weighted_mean(h$clay_pct, h$top_cm, h$bottom_cm, ms, ms + 18)
  covered <- sum(pmax(0, pmin(h$bottom_cm, ms + 18) - pmax(h$top_cm, ms))[!is.na(h$clay_pct)], na.rm = TRUE)
  clay_ok <- if (is.na(clay18) || covered < 18 - 1e-6) NA else .kst_round(clay18) >= 40
  and3 <- function(a, b) if (isFALSE(a) || isFALSE(b)) FALSE else if (isTRUE(a) && isTRUE(b)) TRUE else NA
  or3 <- function(a, b) if (isTRUE(a) || isTRUE(b)) TRUE else if (isFALSE(a) && isFALSE(b)) FALSE else NA
  items <- function(has_kandic, kt) {
    r1 <- and3(ox150, if (has_kandic) !(kt <= ms + 150) else TRUE)   # oxic, no kandic within 150 cm
    r2 <- and3(clay_ok, if (has_kandic) kt <= ms + 100 else FALSE)   # clayey surface, kandic within 100 cm
    list(r1 = r1, r2 = r2, any = or3(r1, r2))
  }
  if (isTRUE(ka$passed)) {
    it <- items(TRUE, ka_top); route <- it$any
  } else if (isFALSE(ka$passed)) {
    it <- items(FALSE, NA); route <- it$any
  } else {
    # undecided: a kandic horizon might start at any of the undecided
    # boundaries, or be absent; decide when every case gives the same answer
    kts <- ka$evidence$candidate_tops_cm %||% numeric(0)
    cases <- c(list(items(FALSE, NA)), lapply(kts, function(kt) items(TRUE, kt)))
    anys <- vapply(cases, function(x) if (is.na(x$any)) NA else x$any, NA)
    route <- if (!length(kts)) NA else if (all(anys %in% TRUE)) TRUE
             else if (all(anys %in% FALSE)) FALSE else NA
    it <- list(r1 = NA, r2 = NA)
  }
  r1 <- it$r1; r2 <- it$r2
  passed <- if (ex) FALSE else route
  DiagnosticResult$new(
    name = "oxisol_usda", passed = passed,
    layers = if (isTRUE(r1)) ox$layers else if (isTRUE(r2)) ka$layers else integer(0),
    evidence = list(oxic = ox, kandic = ka, prior_order = ex,
                    item1_oxic_no_kandic = r1, item2_clayey_surface_kandic = r2,
                    clay_0_18_pct = clay18),
    missing = if (is.na(passed)) unique(c(ox$missing, ka$missing, if (is.na(clay_ok)) "clay_pct")) else character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 8, key to soil orders, E (pp. 68-69)"
  )
}


# ---- F. Vertisols (Cap 16, p 343) ------------------------------------------

#' Vertisols (USDA Cap 16): slickensides + cracks.
#' Delegates to vertic_horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vertisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "vertisol_usda", list(),
        .vertisol_usda_uncached(pedon = pedon))
}

# The computation of vertisol_usda().
.vertisol_usda_uncached <- function(pedon) {
  # v0.9.311: the USDA reading (slickensides and cracks, or the KST 13 LE
  # path); vertic_horizon() itself now follows WRB 2022 Ch 3.1.40.
  vh <- vertic_horizon(pedon, reading = "usda")
  # v0.9.21: NASIS tie-breaker -- Slickensides surveyor
  # identification flips an NA vertic_horizon to TRUE (when no COLE
  # or cracks measurement is available).
  vh <- .apply_nasis_tiebreaker(vh, pedon,
                                 pattern       = "^Slickensides$|^Vertic|^Gilgai$",
                                 feature_label = "Slickensides / Vertic features / Gilgai")
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed) ||
          isTRUE(oxisol_usda(pedon)$passed)
  passed <- isTRUE(vh$passed) && !ex
  DiagnosticResult$new(
    name = "vertisol_usda", passed = passed,
    layers = vh$layers, evidence = list(vertic = vh, prior_order = ex),
    missing = vh$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 16 Vertisols (p 343)"
  )
}


# ---- G. Aridisols (Cap 7, p 137) -------------------------------------------

#' Aridisols (USDA Cap 7): aridic moisture regime + ochric/anthropic +
#' subsurface diagnostic. The aridic regime is read from
#' \code{site$soil_moisture_regime}; when it is not recorded the order
#' is undecided (NA) and the key goes on (v0.9.315: no aridity proxy).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aridisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "aridisol_usda", list(),
        .aridisol_usda_uncached(pedon = pedon))
}

# The computation of aridisol_usda().
.aridisol_usda_uncached <- function(pedon) {
  # Refined v0.8.9: uses aridisol_qualifying_usda which enforces
  # aridic SMR + a diagnostic subsurface horizon (KST 13ed Ch 7
  # p 137). v0.9.315: no fallback to aridity proxies when the regime
  # is not recorded (KST 13 p 69, item G.1.a, needs the regime itself);
  # the result is then NA, kept here instead of collapsed to FALSE.
  aq <- aridisol_qualifying_usda(pedon)
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed) ||
          isTRUE(oxisol_usda(pedon)$passed) ||
          isTRUE(vertisol_usda(pedon)$passed)
  passed <- if (ex) FALSE else aq$passed
  DiagnosticResult$new(
    name = "aridisol_usda", passed = passed,
    layers = aq$layers,
    evidence = list(aridisol_qualifying = aq, prior_order = ex),
    missing = aq$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 7 Aridisols (p 137)"
  )
}


# ---- H. Ultisols (Cap 15, p 321) -------------------------------------------

#' Ultisols (USDA, KST 13th ed., key to soil orders, item H, p. 69)
#'
#' v0.9.420 (audit items U1, U2, G5): the text of the key. Other soils that
#' have either:
#' \enumerate{
#'   \item an argillic or kandic horizon (the USDA horizons,
#'     \code{argillic_usda()} and \code{kandic_horizon_usda()}), no fragipan,
#'     and a base saturation (by sum of cations) of less than 35\% at the
#'     shallowest of 125 cm below the upper boundary of the argillic or
#'     kandic horizon, 180 cm below the mineral soil surface, or a densic,
#'     lithic, paralithic or petroferric contact; when the epipedon is sandy
#'     throughout (coarse sand to loamy fine sand), at 125 cm below the upper
#'     boundary (no deeper than 200 cm) or 180 cm, whichever is deeper, or at
#'     a contact if shallower; or
#'   \item a fragipan, an argillic or kandic horizon or clay films 1 mm or more
#'     thick in it, and a base saturation of less than 35\% at the shallowest
#'     of 75 cm below the top of the fragipan, 200 cm, or a contact.
#' }
#' Until v0.9.419 the key took the WRB argic horizon, the mean base
#' saturation of its layers, and, with no base saturation, a low one inferred
#' from Al saturation or pH (not in the KST). The base saturation by sum of
#' cations is computed where the acidity at pH 8.2 (or the CEC at pH 8.2) is
#' recorded; elsewhere the NH4OAc base saturation stands in for it, and the
#' evidence says so (audit reading L18: it is usually higher, so less than 35
#' by NH4OAc is less than 35 by sum of cations). A profile that ends above the
#' depth takes the base saturation of its deepest sampled horizon, recorded
#' as an inference that lowers the evidence grade to C (reading L19). Values
#' are rounded to the precision of the limit (L24).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ultisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "ultisol_usda", list(),
        .ultisol_usda_uncached(pedon = pedon))
}

# The computation of ultisol_usda().
.ultisol_usda_uncached <- function(pedon) {
  h <- pedon$horizons
  ar <- argillic_usda(pedon)
  # v0.9.21: NASIS tie-breaker for an undecided argillic horizon.
  ar <- .apply_nasis_tiebreaker(ar, pedon,
                                 pattern       = "^Argillic horizon$|^Kandic horizon$",
                                 feature_label = "Argillic / Kandic horizon")
  ka <- kandic_horizon_usda(pedon)
  hor <- .usda_or3(ar$passed, ka$passed)
  # the upper boundaries the base saturation depth is measured from: those of
  # the horizons present, or, while undecided, every candidate boundary
  top_of <- function(r) if (isTRUE(r$passed) && length(r$layers)) min(h$top_cm[r$layers], na.rm = TRUE) else numeric(0)
  tops <- c(top_of(ar), top_of(ka))
  tops <- if (length(tops)) min(tops)
          else unique(c(ar$evidence$candidate_tops_cm, ka$evidence$candidate_tops_cm))
  fr <- fragipan_usda(pedon, max_top_cm = Inf)
  item1 <- .ultisol_bs_item(pedon, tops, fragipan = FALSE)
  item2 <- if (isTRUE(fr$passed)) .ultisol_bs_item(pedon, min(h$top_cm[fr$layers], na.rm = TRUE), fragipan = TRUE)
           else list(low = FALSE)
  films_frag <- if (isTRUE(fr$passed)) {
    th <- h$clay_films_thickness_mm %||% rep(NA_real_, nrow(h))
    any(!is.na(th[fr$layers]) & th[fr$layers] >= 1)
  } else FALSE
  r1 <- .and3(hor, item1$low)                                  # no fragipan
  r2 <- .and3(.usda_or3(hor, films_frag), item2$low)           # a fragipan
  route <- if (isTRUE(fr$passed)) r2 else if (isFALSE(fr$passed)) r1
           else if (identical(r1, r2)) r1 else NA
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed)
  ))
  passed <- if (ex) FALSE else route
  used <- if (isTRUE(fr$passed)) item2 else item1
  ev <- list(argillic = ar, kandic = ka, fragipan = fr,
             base_saturation = used$bs, bs_depth_cm = used$depth,
             bs_source = used$source, bs_low = used$low, prior_order = ex)
  if (isTRUE(passed) && isTRUE(used$inferred))
    ev$inference <- sprintf(paste0(
      "The profile ends above %g cm, the depth at which the KST reads the base ",
      "saturation; the deepest sampled horizon (%g%%) stands in for it (audit ",
      "reading L19)."), used$depth, used$bs)
  if (isTRUE(passed) && identical(used$source, "NH4OAc"))
    ev$note <- paste0("Base saturation by NH4OAc stands in for the sum of ",
                      "cations, which is not recorded (audit reading L18).")
  miss <- if (is.na(passed)) unique(c(if (is.na(hor)) c(ar$missing, ka$missing),
                                      if (is.na(used$low)) "bs_pct")) else character(0)
  DiagnosticResult$new(
    name = "ultisol_usda", passed = passed,
    layers = unique(c(if (isTRUE(ar$passed)) ar$layers, if (isTRUE(ka$passed)) ka$layers)),
    evidence = ev, missing = miss,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 4, key to soil orders, H (p. 69)"
  )
}


# ---- I. Mollisols (Cap 12, p 247) ------------------------------------------

#' Mollisols (USDA Cap 12): mollic epipedon + base saturation >= 50\%.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
mollisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "mollisol_usda", list(),
        .mollisol_usda_uncached(pedon = pedon))
}

# The computation of mollisol_usda().
.mollisol_usda_uncached <- function(pedon) {
  # v0.9.10: switched the mollic gate from the WRB `mollic()` (a v0.2
  # scaffold leftover) to the USDA-native `mollic_epipedon_usda()`,
  # which carries the artefact-rich exclusion in v0.9.10 and the
  # full KST 13ed Ch. 3 thickness / colour / OC / BS / structure
  # contract. This is the fix that prevents Technosol fixtures
  # (artefacts >= 20 % in the surface A) from being routed to
  # Hapludolls.
  m <- mollic_epipedon_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- when the canonical lab + morphology
  # gate returns NA (insufficient data), the surveyor's
  # pediagfeatures.featkind = "Mollic epipedon" identification flips
  # the result to TRUE. Does NOT override TRUE / FALSE.
  m <- .apply_nasis_tiebreaker(m, pedon,
                                pattern       = "^Mollic epipedon$",
                                feature_label = "Mollic epipedon")
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed)
  ))
  passed <- isTRUE(m$passed) && !ex
  DiagnosticResult$new(
    name = "mollisol_usda", passed = passed,
    layers = m$layers, evidence = list(mollic = m, prior_order = ex),
    missing = m$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 12 Mollisols (p 247)"
  )
}


# ---- J. Alfisols (Cap 5, p 73) ---------------------------------------------

#' Alfisols (USDA, KST 13th ed., key to soil orders, item J, p. 70)
#'
#' v0.9.420 (audit item U3): "Other soils that do not have a plaggen epipedon
#' and that have either an argillic, kandic, or natric horizon, or a fragipan
#' that has clay films 1 mm or more thick in some part." There is no base
#' saturation test: the soils with less than 35\% are Ultisols, keyed out
#' before. Until v0.9.419 the gate asked for the argillic horizon only and a
#' mean base saturation of 35\% or more. soilKey has no test of the plaggen
#' epipedon, so that exclusion is not applied.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alfisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "alfisol_usda", list(),
        .alfisol_usda_uncached(pedon = pedon))
}

# The computation of alfisol_usda().
.alfisol_usda_uncached <- function(pedon) {
  h <- pedon$horizons
  ar <- argillic_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- argillic / kandic / natric horizon
  # surveyor identification flips an NA argillic_usda to TRUE.
  ar <- .apply_nasis_tiebreaker(ar, pedon,
                                 pattern       = "^Argillic horizon$|^Kandic horizon$|^Natric horizon$",
                                 feature_label = "Argillic / Kandic / Natric horizon")
  ka <- kandic_horizon_usda(pedon)
  na <- natric_horizon_usda(pedon)
  fr <- fragipan_usda(pedon, max_top_cm = Inf)
  films_frag <- if (isTRUE(fr$passed)) {
    th <- h$clay_films_thickness_mm %||% rep(NA_real_, nrow(h))
    any(!is.na(th[fr$layers]) & th[fr$layers] >= 1)
  } else FALSE
  hor <- .usda_or3(ar$passed, ka$passed, na$passed, films_frag)
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed),
    isTRUE(mollisol_usda(pedon)$passed)
  ))
  passed <- if (ex) FALSE else hor
  DiagnosticResult$new(
    name = "alfisol_usda", passed = passed,
    layers = unique(c(if (isTRUE(ar$passed)) ar$layers, if (isTRUE(ka$passed)) ka$layers,
                      if (isTRUE(na$passed)) na$layers)),
    evidence = list(argillic = ar, kandic = ka, natric = na,
                    fragipan_with_clay_films = films_frag, prior_order = ex),
    missing = if (is.na(passed)) unique(c(ar$missing, ka$missing, na$missing)) else character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 4, key to soil orders, J (p. 70)"
  )
}


# ---- K. Inceptisols (Cap 11, p 207) ----------------------------------------

#' Inceptisols (USDA, KST 13th ed., Key to Soil Orders, item K, p. 70)
#'
#' v0.9.419: item K in full (\code{inceptisol_qualifying_usda()}): K.1 (a) a
#' cambic horizon within 100 cm with its lower boundary at 25 cm or more,
#' (b) a calcic, petrocalcic, gypsic, petrogypsic or placic horizon or a
#' duripan within 100 cm, (c) a fragipan or an oxic, sombric or spodic
#' horizon within 200 cm, (d) a sulfuric horizon within 150 cm, (e) a cryic
#' or gelic regime and a cambic horizon; or K.2 (the epipedon, salic and
#' sodium path), and not the Wassent condition. The cambic horizon is the
#' USDA one (\code{cambic_horizon_usda()}). Up to v0.9.418 only a cambic
#' horizon (the WRB reading) counted, and an undecided one was read as
#' absent. Three-valued: undecided, the key goes on to the Entisols and the
#' name is graded C.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
inceptisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "inceptisol_usda", list(),
        .inceptisol_usda_uncached(pedon = pedon))
}

# The computation of inceptisol_usda().
.inceptisol_usda_uncached <- function(pedon) {
  k <- inceptisol_qualifying_usda(pedon)
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed),
    isTRUE(mollisol_usda(pedon)$passed),
    isTRUE(alfisol_usda(pedon)$passed)
  ))
  passed <- if (ex) FALSE else k$passed
  DiagnosticResult$new(
    name = "inceptisol_usda", passed = passed,
    layers = k$layers,
    evidence = list(item_k = k, cambic = k$evidence$cambic, prior_order = ex),
    missing = if (is.na(passed)) k$missing else character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 4, Key to Soil Orders, item K (p 70)"
  )
}


# ---- L. Entisols (Cap 8, p 165) - catch-all -----------------------------

#' Entisols (USDA Cap 8): catch-all for soils that don't match any
#' other Order. Always passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
entisol_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "entisol_usda", list(),
        .entisol_usda_uncached(pedon = pedon))
}

# The computation of entisol_usda().
.entisol_usda_uncached <- function(pedon) {
  DiagnosticResult$new(
    name = "entisol_usda", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 8 Entisols (p 165) -- catch-all"
  )
}
