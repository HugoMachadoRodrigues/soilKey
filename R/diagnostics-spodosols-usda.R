# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Helpers for Spodosols Suborder/GG/SG keys -- Chapter 14, pp 311-320
# =============================================================
#
# Spodosols are soils with a spodic horizon (illuvial accumulation of
# organic matter, Al, and/or Fe). The 5 Suborders are:
#   Aquods   -- aquic conditions within 50 cm
#   Gelods   -- gelic STR (very cold)
#   Cryods   -- cryic STR (cold)
#   Humods   -- 6%+ OC in spodic horizon
#   Orthods  -- catch-all
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 14.
# =============================================================


# ---- Spodosols Order qualifier --------------------------------------

#' Spodosols Order qualifier (USDA, KST 13ed)
#'
#' Pass when the profile has a spodic horizon (illuvial Fe/Al/OM
#' accumulation). Implementation delegates to the WRB \code{spodic}
#' diagnostic.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 64-67.
#' @export
spodic_horizon_usda <- function(pedon) {
  res <- spodic(pedon)
  res$name <- "spodic_horizon_usda"
  res
}


# ---- Albic horizon helper -------------------------------------------

#' Albic horizon (USDA, KST 13ed Ch 3)
#'
#' Pass when an albic horizon (light-colored, eluvial; chroma <= 2,
#' value >= 4) >= 1 cm thick is present. Delegates to WRB albic
#' diagnostic.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
albic_horizon_usda <- function(pedon) {
  res <- albic(pedon)
  res$name <- "albic_horizon_usda"
  res
}


# ---- Placic horizon helper ------------------------------------------

#' Placic horizon (USDA, KST 13ed Ch 3, pp 47-48)
#'
#' Pass when a thin (1-25 mm) Fe/Mn-cemented horizon is present.
#' Detected via designation containing 'm' (cemented) AND
#' \code{cementation_class} in \{strongly, indurated\} AND thickness
#' between 1 mm and 25 mm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
placic_horizon_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm &
                  !is.na(h$designation) &
                  grepl("m", h$designation) &
                  !is.na(h$cementation_class) &
                  tolower(h$cementation_class) %in%
                    c("strongly", "indurated"))
  thk <- if (length(cand) > 0L) h$bottom_cm[cand] - h$top_cm[cand]
         else numeric(0)
  # placic is THIN: 0.1-2.5 cm (1-25 mm)
  passing <- cand[!is.na(thk) & thk >= 0.1 & thk <= 2.5]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "placic_horizon_usda", passed = passed, layers = passing,
    evidence = list(thicknesses_cm = thk, max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 47-48"
  )
}


# ---- Fragipan helper -------------------------------------------------

#' Fragipan (USDA, KST 13ed Ch 3, p 38)
#'
#' Pass when a horizon has fragic soil properties:
#' \itemize{
#'   \item rupture_resistance class >= "firm" (firm, very firm,
#'         extremely firm); AND
#'   \item brittle manner of failure (proxy: not in schema; v0.8
#'         uses rupture-resistance as primary indicator).
#' }
#' Plus thickness >= 15 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
fragipan_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  rr <- h$rupture_resistance[cand]
  miss <- if (all(is.na(rr))) "rupture_resistance" else character(0)
  firm_classes <- c("firm", "very firm", "extremely firm")
  passing <- cand[!is.na(rr) & tolower(rr) %in% firm_classes]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 15
  DiagnosticResult$new(
    name = "fragipan_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk, max_top_cm = max_top_cm,
                      note = "v0.8: brittleness inferred from rupture_resistance"),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 38"
  )
}


# ---- Duripan helper --------------------------------------------------

#' Duripan (USDA, KST 13ed Ch 3, pp 36-37)
#'
#' Silica-cemented horizon, very strongly resistant. Detected via
#' \code{cementation_class == "indurated"} AND \code{duripan_pct
#' >= 50}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
duripan_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  miss <- character(0)
  cem <- h$cementation_class[cand]
  dp  <- h$duripan_pct[cand]
  if (all(is.na(cem))) miss <- c(miss, "cementation_class")
  if (all(is.na(dp))) miss <- c(miss, "duripan_pct")
  passing <- cand[!is.na(cem) & tolower(cem) == "indurated" &
                      !is.na(dp) & dp >= 50]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "duripan_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 36-37"
  )
}


# ---- Duric Subgroup helper (Aquods/Cryods) --------------------------

#' Duric Subgroup helper (USDA Spodosols)
#'
#' Pass when a pedogenically cemented horizon (extremely weakly
#' coherent or stronger) is present in 90\%+ of the pedon within
#' 100 cm. v0.8 proxy: any horizon with cementation_class >= "weakly".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
duric_subgroup_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cem <- h$cementation_class[cand]
  miss <- if (all(is.na(cem))) "cementation_class" else character(0)
  cemented <- c("weakly", "moderately", "strongly", "indurated")
  passing <- cand[!is.na(cem) & tolower(cem) %in% cemented]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "duric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Kandic horizon helper ------------------------------------------

#' Kandic horizon (USDA, KST 13ed Ch 3, p 45)
#'
#' Subsurface horizon with low-activity clays (CEC <= 16 cmol/kg
#' clay, ECEC <= 12) and clay increase. Implementation:
#' delegates to argic with additional CEC/clay check.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
kandic_horizon_usda <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed)) {
    return(DiagnosticResult$new(
      name = "kandic_horizon_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no argic / clay-increase horizon"),
      missing = arg$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
    ))
  }
  h <- pedon$horizons
  cand <- arg$layers
  passing <- integer(0); miss <- character(0)
  for (i in cand) {
    cec <- h$cec_cmol[i]
    clay <- h$clay_pct[i]
    if (is.na(cec)) miss <- c(miss, "cec_cmol")
    if (is.na(clay)) miss <- c(miss, "clay_pct")
    if (!is.na(cec) && !is.na(clay) && clay > 0) {
      cec_per_clay <- cec * 100 / clay
      if (cec_per_clay <= 16) passing <- c(passing, i)
    }
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "kandic_horizon_usda", passed = passed, layers = passing,
    evidence = list(argic = arg),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
  )
}


# ---- Argic / Kandic combined helper for Spodosols ----------------

#' Argillic-or-Kandic helper (USDA, used in Spodosols Subgroups)
#'
#' Pass when EITHER an argillic OR a kandic horizon is present
#' within \code{max_top_cm}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 200.
#' @param min_bs Optional minimum BS for "Alfic" subgroups.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
argillic_or_kandic_usda <- function(pedon, max_top_cm = 200,
                                         min_bs = NULL) {
  arg <- argillic_within_usda(pedon, max_top_cm = max_top_cm)
  ka  <- kandic_horizon_usda(pedon)
  passed <- isTRUE(arg$passed) || isTRUE(ka$passed)
  if (passed && !is.null(min_bs)) {
    h <- pedon$horizons
    layers <- unique(c(arg$layers, ka$layers))
    bs <- h$bs_pct[layers]
    bs_ok <- any(!is.na(bs) & bs >= min_bs)
    passed <- isTRUE(bs_ok)
    if (!passed) {
      return(DiagnosticResult$new(
        name = "argillic_or_kandic_usda", passed = FALSE,
        layers = integer(0),
        evidence = list(argillic = arg, kandic = ka,
                          min_bs = min_bs),
        missing = character(0),
        reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
      ))
    }
  }
  DiagnosticResult$new(
    name = "argillic_or_kandic_usda", passed = passed,
    layers = if (passed) unique(c(arg$layers, ka$layers))
             else integer(0),
    evidence = list(argillic = arg, kandic = ka, min_bs = min_bs),
    missing = unique(c(arg$missing %||% character(0),
                          ka$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Alfic Subgroup helper (Spodosols): argillic or kandic with BS >= 35\%
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
alfic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200, min_bs = 35)
  res$name <- "alfic_subgroup_usda"
  res
}


#' Ultic Subgroup helper: argillic or kandic (any BS).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
ultic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "ultic_subgroup_usda"
  res
}


#' Argic Subgroup helper (Endoaquods/Fragiaquods): argillic or kandic.
#' Synonym of ultic at this level. Re-exported for naming clarity.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
argic_subgroup_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "argic_subgroup_usda"
  res
}


# ---- Arenic / Grossarenic Subgroup helpers --------------------------

#' Arenic / Grossarenic Subgroup helper (Spodosols)
#'
#' Pass when texture class (fine-earth fraction) is sandy
#' throughout from the surface to the top of the spodic horizon
#' AND the spodic top depth falls in
#' \code{[min_spodic_top, max_spodic_top]}.
#'
#' Standard cuts:
#' - "Arenic":      75-125 cm
#' - "Grossarenic": 125+ cm (use min_spodic_top=125, max=Inf)
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_spodic_top Default 75.
#' @param max_spodic_top Default 125.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
arenic_subgroup_usda <- function(pedon,
                                      min_spodic_top = 75,
                                      max_spodic_top = 125) {
  sp <- spodic_horizon_usda(pedon)
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "arenic_subgroup_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(spodic = sp,
                        reason = "no spodic horizon"),
      missing = sp$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  h <- pedon$horizons
  spodic_top <- min(h$top_cm[sp$layers], na.rm = TRUE)
  depth_ok <- spodic_top >= min_spodic_top &&
                spodic_top <= max_spodic_top
  # All horizons above spodic must be sandy
  above <- which(!is.na(h$top_cm) & h$top_cm < spodic_top)
  texture_ok <- TRUE
  for (i in above) {
    cl <- h$clay_pct[i]; sd <- h$sand_pct[i]
    if (is.na(cl) || is.na(sd)) next
    is_sandy <- !isTRUE(.is_finer_than_loamy_fine_sand(cl, h$silt_pct[i],
                                                          sd))
    if (!is_sandy) {
      texture_ok <- FALSE
      break
    }
  }
  passed <- isTRUE(depth_ok) && isTRUE(texture_ok)
  DiagnosticResult$new(
    name = "arenic_subgroup_usda", passed = passed,
    layers = if (passed) above else integer(0),
    evidence = list(spodic_top_cm = spodic_top,
                      depth_ok = depth_ok,
                      texture_ok = texture_ok,
                      min_spodic_top = min_spodic_top,
                      max_spodic_top = max_spodic_top),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Grossarenic Subgroup helper: sandy throughout, spodic >= 125 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
grossarenic_subgroup_usda <- function(pedon) {
  res <- arenic_subgroup_usda(pedon, min_spodic_top = 125,
                                  max_spodic_top = Inf)
  res$name <- "grossarenic_subgroup_usda"
  res
}


# ---- Entic / Aeric Subgroup helpers --------------------------------

#' Entic Subgroup helper (Spodosols)
#'
#' Pass when the spodic horizon is "weakly developed":
#' \itemize{
#'   \item Less than 1.2\% organic carbon in the upper 10 cm of
#'         spodic; OR
#'   \item Spodic horizon < 10 cm thick.
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
entic_subgroup_usda <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  h <- pedon$horizons
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "entic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic horizon"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  # Thickness of spodic horizon
  thk <- sum(pmax(h$bottom_cm[sp$layers] - h$top_cm[sp$layers], 0),
                na.rm = TRUE)
  # OC in upper 10 cm of spodic
  ord <- order(h$top_cm[sp$layers])
  upper <- sp$layers[ord][1]
  oc_upper <- h$oc_pct[upper]
  thin <- thk < 10
  oc_low <- !is.na(oc_upper) && oc_upper < 1.2
  passed <- isTRUE(thin) || isTRUE(oc_low)
  DiagnosticResult$new(
    name = "entic_subgroup_usda", passed = passed,
    layers = sp$layers,
    evidence = list(spodic_thickness_cm = thk,
                      oc_upper_pct = oc_upper,
                      thin = thin, oc_low = oc_low),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Aeric Subgroup helper (Aquods)
#' Pass when ochric epipedon is present (vs. histic/umbric/etc).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aeric_subgroup_usda <- function(pedon) {
  res <- ochric_epipedon_usda(pedon)
  res$name <- "aeric_subgroup_usda"
  res
}


#' Histic Subgroup helper (in Spodosols, Aquods)
#' Pass when histic_epipedon_usda passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
histic_subgroup_usda <- function(pedon) {
  res <- histic_epipedon_usda(pedon)
  res$name <- "histic_subgroup_usda"
  res
}


#' Umbric Subgroup helper (in Spodosols)
#' Pass when umbric_epipedon_usda passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
umbric_subgroup_usda <- function(pedon) {
  res <- umbric_epipedon_usda(pedon)
  res$name <- "umbric_subgroup_usda"
  res
}


# ---- Oxyaquic Subgroup helper ---------------------------------------

#' Oxyaquic Subgroup helper (Spodosols, Mollisols, etc.)
#'
#' Pass when the soil is saturated with water in one or more layers
#' within 100 cm of the mineral soil surface for either or both:
#' \itemize{
#'   \item 20+ consecutive days; OR
#'   \item 30+ cumulative days.
#' }
#'
#' v0.8 proxy: pass when redoximorphic features OR low chroma in any
#' layer within 100 cm (subset of full aquic conditions).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
oxyaquic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "oxyaquic_subgroup_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no candidate layers in upper 100 cm"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  rdx <- h$redoximorphic_features_pct[cand]
  chr <- h$munsell_chroma_moist[cand]
  des <- h$designation[cand]
  # v0.9.24 tightening: KST 13ed Ch 14 (p 311) defines the Oxyaquic
  # subgroup as a profile with saturation 20-30 days plus redox
  # features but NOT meeting full aquic conditions. The pre-v0.9.24
  # logic fired on `redox >= 2` OR `chroma <= 2` ALONE (a single
  # disjunctive low-evidence trigger), which produced large numbers
  # of false-positive Oxyaquic predictions on KSSL Typic-reference
  # profiles. The canonical interpretation requires BOTH evidence
  # of saturation (low chroma, redox features, or 'g' designation
  # suffix) AND a redoximorphic indicator that does not yet meet
  # the stronger aquic-conditions threshold. We now require either:
  #   (a) measured redox features >= 2 % AND chroma <= 4 in the
  #       matrix (presence of redox + non-bright matrix), OR
  #   (b) a 'g' suffix in the designation AND chroma <= 3 (gleyed
  #       designation evidence with somewhat low chroma).
  redox_ok    <- !is.na(rdx) & rdx >= 2
  chroma_low  <- !is.na(chr) & chr <= 4
  glei_des    <- !is.na(des) & grepl("g", des, ignore.case = FALSE)
  chroma_v3   <- !is.na(chr) & chr <= 3
  passing <- cand[(redox_ok & chroma_low) |
                      (glei_des & chroma_v3)]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "oxyaquic_subgroup_usda", passed = passed,
    layers = passing,
    evidence = list(note = "v0.9.24: requires redox_ok + chroma<=4 OR glei_designation + chroma<=3"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14 (Oxyaquic subgroup)"
  )
}


# ---- Compound Subgroup helpers (combinations) -----------------------

#' Aquandic Subgroup helper (Spodosols / others)
#' Aquic + Andic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aquandic_subgroup_usda <- function(pedon) {
  aq <- aquic_subgroup_usda(pedon)
  an <- andic_subgroup_usda(pedon)
  passed <- isTRUE(aq$passed) && isTRUE(an$passed)
  DiagnosticResult$new(
    name = "aquandic_subgroup_usda", passed = passed,
    layers = if (passed) union(aq$layers, an$layers) else integer(0),
    evidence = list(aquic = aq, andic = an),
    missing = unique(c(aq$missing %||% character(0),
                          an$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Gelic Materials / Turbic helper --------------------------------

#' Turbic Subgroup helper (Gelods)
#' Pass when gelic materials are present within 200 cm.
#' Implementation: cryoturbation + permafrost within 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
turbic_subgroup_usda <- function(pedon) {
  cr <- cryoturbation_usda(pedon)
  pf <- permafrost_within_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(cr$passed) || isTRUE(pf$passed)
  DiagnosticResult$new(
    name = "turbic_subgroup_usda", passed = passed,
    layers = c(cr$layers, pf$layers),
    evidence = list(cryoturbation = cr, permafrost = pf),
    missing = pf$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Gelic STR helper -----------------------------------------------

#' Gelic soil temperature regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
str_gelic_usda <- function(pedon) {
  pf <- permafrost_within_usda(pedon, max_top_cm = 100)
  res <- DiagnosticResult$new(
    name = "str_gelic_usda", passed = isTRUE(pf$passed),
    layers = pf$layers, evidence = list(permafrost = pf),
    missing = pf$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 53"
  )
  res
}


# ---- Lamellic Subgroup helper ---------------------------------------

#' Lamellic Subgroup helper (Spodosols Haplorthods)
#'
#' Pass when 2+ lamellae (clay-rich bands < 7.5 cm thick) are
#' present below the spodic horizon. v0.8: detected via designation
#' containing "&" (lamella notation in KST) OR multiple thin
#' clay-bumps in clay_pct.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
lamellic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  des_lam <- which(!is.na(h$designation) &
                       grepl("&", h$designation, fixed = TRUE))
  passed <- length(des_lam) >= 2L
  DiagnosticResult$new(
    name = "lamellic_subgroup_usda", passed = passed,
    layers = des_lam,
    evidence = list(note = "v0.8: detected via '&' in designation"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 35-36"
  )
}


# ---- Folistic Subgroup helper (already defined; re-exported) -------
# folistic_subgroup_usda is in diagnostics-gelisols-usda.R


# ---- Andic Subgroup helper (already in diagnostics-gelisols-usda.R)
# andic_subgroup_usda is reused.

# ---- Aquic Subgroup helper (already defined) -----------------------
# aquic_subgroup_usda is reused.


# ---- Aluminum-rich Spodic check (Alaquods, Alorthods) -------------

#' Aluminum-rich spodic helper (Alaquods, Alorthods, KST Ch 14)
#'
#' Pass when the spodic horizon has < 0.10\% Fe (oxalate) in 75\%+
#' of layers, OR Al >= 3 * Fe in 75\%+ of layers (Alaquods only).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
al_rich_spodic_usda <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "al_rich_spodic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic"),
      missing = sp$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  h <- pedon$horizons
  layers <- sp$layers
  fe <- h$fe_ox_pct[layers]
  al <- h$al_ox_pct[layers]
  miss <- character(0)
  if (all(is.na(fe))) miss <- c(miss, "fe_ox_pct")
  if (all(is.na(al))) miss <- c(miss, "al_ox_pct")
  cond_fe_low <- !is.na(fe) & fe < 0.10
  cond_al_dom <- !is.na(al) & !is.na(fe) & al >= 3 * fe
  pass_count <- sum(cond_fe_low | cond_al_dom, na.rm = TRUE)
  passed <- pass_count >= 0.75 * length(layers)
  DiagnosticResult$new(
    name = "al_rich_spodic_usda", passed = passed, layers = layers,
    evidence = list(fe_pct = fe, al_pct = al,
                      pass_count = pass_count,
                      total_layers = length(layers)),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


# ---- Humic-spodic check (Humods, Humicryods) ------------------------

#' Humic-spodic Suborder/GG check (>= 6\% OC in 10+ cm of spodic)
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
humic_spodic_usda <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  if (!isTRUE(sp$passed) || length(sp$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "humic_spodic_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no spodic"),
      missing = sp$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
    ))
  }
  h <- pedon$horizons
  layers <- sp$layers
  oc <- h$oc_pct[layers]
  thk <- pmax(h$bottom_cm[layers] - h$top_cm[layers], 0)
  passing <- layers[!is.na(oc) & oc >= 6 & thk >= 10]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "humic_spodic_usda", passed = passed, layers = passing,
    evidence = list(oc_pct = oc, thicknesses_cm = thk),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 14"
  )
}


#' Humic Subgroup helper (Humic Duricryods / Humic Placocryods)
#' Pass when spodic horizon has >= 6\% OC in 10+ cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
humic_subgroup_usda <- function(pedon) {
  res <- humic_spodic_usda(pedon)
  res$name <- "humic_subgroup_usda"
  res
}


# ---- Episaturation / Endosaturation ---------------------------------

#' Episaturation helper (USDA, KST 13ed Ch 3, p 41)
#' Pass when aquic conditions PLUS perched water (saturation type
#' "episaturation").
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
episaturation_usda <- function(pedon) {
  aq <- aquic_conditions_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(aq$passed) &&
              identical(aq$evidence$saturation_type, "episaturation")
  DiagnosticResult$new(
    name = "episaturation_usda", passed = passed,
    layers = aq$layers,
    evidence = list(aquic = aq),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 41"
  )
}
