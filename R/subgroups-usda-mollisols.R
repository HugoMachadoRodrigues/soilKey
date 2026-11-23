# =============================================================
# USDA Soil Taxonomy 13ed -- Mollisol subgroups (Ch 12, pp 247-294)
# =============================================================
#
# v0.9.424: the tests of the subgroup keys of the Mollisols, as the KST 13th
# edition words them for each great group (chapter 12, pp. 248-294), used by
# inst/rules/usda/subgroups/mollisols.yaml. Each is three-valued: TRUE or
# FALSE where the recorded data decide it, NA with the missing data listed
# where they do not. Field observations not recorded (redox features,
# cracks, slickensides, worm holes, visible salts, buried horizons, tongues
# of albic material, cementation) are absent; measurements (organic carbon,
# COLE, base saturation, CEC), colour, structure and texture not recorded
# are undecided. The functions are prefixed sg_mollisol_ and read the
# Mollisol wording; the helpers shared with other orders are not changed.
# =============================================================


.sgm_out <- function(name, passed, layers = integer(0), evidence = list(),
                     missing = character(0), ref, notes = NA_character_) {
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) unique(as.integer(layers)) else integer(0),
    evidence = evidence,
    missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
    reference = ref, notes = notes)
}

.sgm_ref <- function(pages) sprintf("Soil Survey Staff (2022), KST 13th ed., %s", pages)

.sgm_ms <- function(h) {
  ms <- .mineral_soil_surface_cm(h)
  if (is.na(ms)) 0 else ms
}

# Mineral layers that overlap [from, to) (cm from the soil surface).
.sgm_window <- function(h, from, to) {
  which(!is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > from &
          h$top_cm < to & !.mo_is_organic(h))
}

# Thickness of layers `idx` clipped to [from, to).
.sgm_clip <- function(h, idx, from = -Inf, to = Inf) {
  if (!length(idx)) return(0)
  sum(pmax(pmin(h$bottom_cm[idx], to) - pmax(h$top_cm[idx], from), 0), na.rm = TRUE)
}

# Three-valued "thickness of the layers with `ok` is at least t": TRUE /
# FALSE / NA from the layers that are TRUE and those that are undecided.
.sgm_thick3 <- function(h, ok, idx, t, from = -Inf, to = Inf) {
  yes <- .sgm_clip(h, idx[ok %in% TRUE], from, to)
  may <- .sgm_clip(h, idx[is.na(ok)], from, to)
  if (yes >= t) TRUE else if (yes + may < t) FALSE else NA
}

.sgm_suffix <- function(h, i, letters) {
  d <- .usda_desig(h$designation[i])$suffix
  !is.na(d) & grepl(paste0("[", letters, "]"), d)
}


# ---- Aquic conditions and redox features (KST pp. 41-44) --------------------

# Evidence of aquic conditions in a layer (reading N10): redox features
# recorded, a g in the designation or a reduced matrix. A field observation:
# not recorded, absent.
.sgm_aquic_layer <- function(h, i) {
  red <- .mo_col(h, "reduced_matrix_pct")[i]
  isTRUE(.mo_redox_conc(h, i)) || isTRUE(.sgm_suffix(h, i, "g")) ||
    (!is.na(red) && red > 0)
}

# Redox depletions with chroma of 2 or less and aquic conditions in a layer:
# a g designation or a reduced matrix (a depleted, low-chroma matrix); a
# mottle colour recorded with chroma 2 or less; or redox features in a
# matrix of chroma 2 or less. Redox features recorded without their colours
# in a matrix of higher chroma: undecided.
.sgm_depletion <- function(h, i) {
  if (!.sgm_aquic_layer(h, i)) return(FALSE)
  red <- .mo_col(h, "reduced_matrix_pct")[i]
  if (isTRUE(.sgm_suffix(h, i, "g")) || (!is.na(red) && red > 0)) return(TRUE)
  mc <- .munsell_notations(as.character(.mo_col(h, "mottle_colors")[i]))
  if (nrow(mc) && any(mc$chroma <= 2, na.rm = TRUE)) return(TRUE)
  cm <- h$munsell_chroma_moist[i]
  if (!is.na(cm) && cm <= 2) return(TRUE)
  if (nrow(mc) && !anyNA(mc$chroma) && !is.na(cm)) return(FALSE)
  NA
}

# The aquic colour test of the Aquic subgroups of the Udolls (e.g. Aquic
# Argiudolls, KST p. 260): aquic conditions within 40 cm in horizons with
# redoximorphic features, or within 75 cm in horizons 15 cm or more thick
# (in total) with aquic conditions and (1) moist value 4 or more and redox
# depletions of chroma 2 or less, (2) hue 10YR or redder and chroma 2 or
# less, or (3) hue 2.5Y or yellower and chroma 3 or less.
.sgm_aquic_udoll <- function(h, ms) {
  w40 <- .sgm_window(h, ms, ms + 40)
  if (any(vapply(w40, function(i) .sgm_aquic_layer(h, i), NA))) return(TRUE)
  w75 <- .sgm_window(h, ms, ms + 75)
  hr <- .mo_hue_rank(h$munsell_hue_moist)
  ok <- vapply(w75, function(i) {
    if (!.sgm_aquic_layer(h, i)) return(FALSE)
    vm <- h$munsell_value_moist[i]; cm <- h$munsell_chroma_moist[i]
    a <- .mo_and(if (is.na(vm)) NA else vm >= 4, .sgm_depletion(h, i))
    b <- if (is.na(hr[i]) || is.na(cm)) NA else hr[i] >= 1 && hr[i] <= 5 && cm <= 2
    c <- if (is.na(hr[i]) || is.na(cm)) NA else hr[i] >= 6 && cm <= 3
    if (!is.na(hr[i]) && hr[i] %in% c(0, 8)) b <- TRUE
    .mo_or(a, b, c)
  }, NA)
  .sgm_thick3(h, ok, w75, 15, ms, ms + 75)
}

#' Aquic subgroups of the Mollisols (KST 13th ed., ch. 12)
#'
#' The aquic tests of the Mollisol subgroup keys, which differ by great
#' group:
#' \describe{
#'   \item{udoll}{Aquic Argiudolls and Hapludolls (pp. 260, 263): aquic
#'     conditions within 40 cm with redoximorphic features, or within 75 cm
#'     in horizons 15 cm or more thick with the colours of the key.}
#'   \item{depletions}{redox depletions with chroma of 2 or less and aquic
#'     conditions in a horizon within \code{max_cm} (75 cm in the Paleudolls,
#'     Calciudolls and Xerolls; 100 cm in the Ustolls, Argicryolls and
#'     Palecryolls).}
#'   \item{concentrations}{redox concentrations and aquic conditions within
#'     \code{max_cm} (Calciustolls, Calcixerolls, 75 cm).}
#'   \item{distinct}{distinct or prominent redox concentrations and aquic
#'     conditions within 100 cm (Haplocryolls, Haplogelolls).}
#'   \item{natrustoll}{Aquic Natrustolls (p. 280): between 50 and 100 cm,
#'     aquic conditions and 50 \% or more chroma 1 or less with hue 2.5Y or
#'     yellower, or chroma 2 or less with redox concentrations, or chroma 2
#'     or less with a higher ESP in the upper 25 cm than below.}
#'   \item{above_duripan}{Aquic Durixerolls (p. 287): redox depletions with
#'     chroma 2 or less and aquic conditions above the duripan.}
#' }
#' Aquic conditions are read from the redox features, a g designation or a
#' reduced matrix (reading N10); redox features are field observations (not
#' recorded, absent); a redox feature whose colour is not recorded leaves a
#' depletion undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param form One of the names above.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_mollisol_aquic <- function(pedon, form = c("depletions", "udoll", "concentrations",
                                              "distinct", "natrustoll", "above_duripan"),
                              max_cm = 75) {
  form <- match.arg(form)
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  ref <- .sgm_ref("ch. 12, pp. 248-294 (Aquic subgroups)")
  miss <- c("mottle_colors", "munsell_chroma_moist")
  v <- switch(form,
    udoll = { miss <- c(miss, "munsell_hue_moist", "munsell_value_moist"); .sgm_aquic_udoll(h, ms) },
    depletions = .usda_any3(vapply(.sgm_window(h, ms, ms + max_cm), function(i) .sgm_depletion(h, i), NA)),
    concentrations = .usda_any3(vapply(.sgm_window(h, ms, ms + max_cm),
                                       function(i) isTRUE(.mo_redox_conc(h, i)), NA)),
    distinct = { miss <- "mottle_contrast"
      .usda_any3(vapply(.sgm_window(h, ms, ms + max_cm),
                        function(i) .mo_redox_conc(h, i, contrast = TRUE), NA)) },
    natrustoll = {
      esp <- .usda_esp(h)
      up <- .sgm_window(h, ms, ms + 25)
      hr <- .mo_hue_rank(h$munsell_hue_moist)
      miss <- c(miss, "munsell_hue_moist", "esp_pct")
      .usda_any3(vapply(.sgm_window(h, ms + 50, ms + 100), function(i) {
        if (!.sgm_aquic_layer(h, i)) return(FALSE)
        cm <- h$munsell_chroma_moist[i]
        if (is.na(cm)) return(NA)
        a <- if (is.na(hr[i])) NA else cm <= 1 && hr[i] >= 6
        b <- cm <= 2 && isTRUE(.mo_redox_conc(h, i))
        eu <- suppressWarnings(max(esp[up], na.rm = TRUE))
        c <- if (cm > 2) FALSE else if (!is.finite(eu) || is.na(esp[i])) NA else eu > esp[i]
        .mo_or(a, b, c)
      }, NA))
    },
    above_duripan = {
      du <- duripan_usda(pedon, max_top_cm = Inf)
      dt <- if (isTRUE(du$passed) && length(du$layers)) min(h$top_cm[du$layers]) else Inf
      .usda_any3(vapply(.sgm_window(h, ms, dt), function(i) .sgm_depletion(h, i), NA))
    })
  .sgm_out("sg_mollisol_aquic", v, evidence = list(form = form, max_cm = max_cm),
           missing = miss, ref = ref)
}


# ---- Vertic properties (e.g. Vertic Argiudolls, KST p. 260) -----------------

#' Vertic subgroups of the Mollisols (KST 13th ed., ch. 12)
#'
#' One or both of: (1) cracks within 125 cm of the mineral soil surface, 5
#' mm or more wide through a thickness of 30 cm or more, and slickensides or
#' wedge-shaped peds in a layer 15 cm or more thick whose upper boundary is
#' within 125 cm; (2) a linear extensibility of 6.0 cm or more between the
#' mineral soil surface and 100 cm or a densic, lithic or paralithic contact.
#' With \code{duripan = TRUE} (Vertic Durixerolls and Duraquolls, pp. 250,
#' 287) the cracks, the slickensides and the linear extensibility are read
#' between the surface and the top of the duripan.
#'
#' Cracks and slickensides are field observations (not recorded, absent);
#' where no crack is recorded the slickensides are taken as evidence of
#' cracks (reading L30, as for the Vertisol order), and \code{notes} says
#' so. A structure not recorded leaves the wedge-shaped peds undecided only
#' where cracks are recorded. The linear extensibility is the sum of COLE x
#' thickness, a measurement: COLE not measured leaves item 2 undecided unless
#' the layers measured already reach 6 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param duripan Read above the duripan.
#' @noRd
sg_mollisol_vertic <- function(pedon, duripan = FALSE) {
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, e.g. p. 260 (Vertic Argiudolls), p. 287 (Vertic Durixerolls)")
  if (!nrow(h)) return(.sgm_out("sg_mollisol_vertic", FALSE, ref = ref))
  ms <- .sgm_ms(h)
  lim <- ms + 125
  le_to <- ms + 100
  ct <- .usda_contact_top_cm(h)
  if (!is.na(ct)) le_to <- min(le_to, ct)
  if (duripan) {
    du <- duripan_usda(pedon, max_top_cm = Inf)
    dt <- if (isTRUE(du$passed) && length(du$layers)) min(h$top_cm[du$layers]) else NA
    if (!is.na(dt)) { lim <- dt; le_to <- min(le_to, dt) }
  }
  ord <- order(h$top_cm)
  w <- .sgm_window(h, ms, lim)
  slick <- .ve_slickensides(h)
  st <- .field_term(.mo_col(h, "structure_type"))
  st2 <- .field_term(.mo_col(h, "structure_type_secondary"))
  wedge <- ifelse(!is.na(st2) & grepl("wedge|cuneiform|lenticular", st2), TRUE,
                  ifelse(is.na(st), NA, grepl("wedge|cuneiform|lenticular", st)))
  # cracks
  cw <- .mo_col(h, "cracks_width_cm"); cd <- .mo_col(h, "cracks_depth_cm")
  note <- NA_character_
  crk_rec <- w[!is.na(cw[w])]
  cracks <- if (length(crk_rec)) {
    .usda_any3(vapply(crk_rec, function(i)
      if (cw[i] < 0.5) FALSE else if (is.na(cd[i])) NA else cd[i] >= 30, NA))
  } else if (any(slick[w])) {
    note <- "cracks not recorded: the slickensides recorded are taken as evidence of cracks (L30)"
    TRUE
  } else FALSE
  feat <- if (length(crk_rec)) ifelse(slick, TRUE, wedge) else slick
  inside <- seq_len(nrow(h)) %in% .sgm_window(h, ms, Inf)
  runs_y <- Filter(function(r) .usda_run_thickness(h, r) >= 15 && min(h$top_cm[r]) < lim,
                   .usda_runs(feat %in% TRUE & inside, ord))
  runs_m <- Filter(function(r) .usda_run_thickness(h, r) >= 15 && min(h$top_cm[r]) < lim,
                   .usda_runs(!(feat %in% FALSE) & inside, ord))
  layer15 <- if (length(runs_y)) TRUE else if (length(runs_m)) NA else FALSE
  i1 <- .mo_and(cracks, layer15)
  # linear extensibility
  lw <- .sgm_window(h, ms, le_to)
  cole <- .mo_col(h, "cole_value")
  thk <- pmax(pmin(h$bottom_cm[lw], le_to) - pmax(h$top_cm[lw], ms), 0)
  le_known <- sum((thk * cole[lw])[!is.na(cole[lw])])
  i2 <- if (le_known >= 6) TRUE else if (!length(lw) || !anyNA(cole[lw])) FALSE else NA
  passed <- .mo_or(i1, i2)
  miss <- c(if (is.na(i2)) "cole_value", if (is.na(i1) && is.na(cracks)) "cracks_depth_cm",
            if (is.na(i1) && is.na(layer15)) "structure_type")
  .sgm_out("sg_mollisol_vertic", passed,
           layers = unique(unlist(runs_y)),
           evidence = list(cracks = cracks, slickensides_or_wedges_15cm = layer15,
                           linear_extensibility_cm = le_known, item1 = i1, item2 = i2,
                           above_duripan = duripan),
           missing = miss, ref = ref, notes = if (isTRUE(i1) && !isTRUE(i2)) note else NA_character_)
}


# ---- Thickness of the mollic epipedon (Pachic, Cumulic, Haplic, Entic) ------

.sgm_mollic <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  e <- mo$evidence
  list(mo = mo, passed = mo$passed,
       known = if (isTRUE(mo$passed)) e$thickness_cm %||% 0 else 0,
       possible = if (isFALSE(mo$passed)) 0 else e$thickness_possible_cm %||% 0,
       upper = e$upper_cm %||% .sgm_ms(pedon$horizons),
       lower = e$lower_possible_cm %||% NA_real_,
       coarse = e$texture_lfs_or_coarser %||% NA)
}

.sgm_thick_ge <- function(m, t) {
  if (isFALSE(m$passed)) return(FALSE)
  if (isTRUE(m$passed) && m$known >= t - 1e-6) TRUE
  else if (m$possible < t - 1e-6) FALSE else NA
}

#' Thickness of the mollic epipedon (Pachic, Cumulic and related subgroups)
#'
#' A mollic epipedon (\code{mollic_epipedon_usda()}, KST pp. 14-16)
#' \code{min_cm} or more thick, with, where \code{finer = TRUE}, a texture
#' class finer than loamy fine sand (as the Pachic subgroups read it,
#' \code{pachic_subgroup_usda()}), or, with \code{finer_half = TRUE}, 50 \% or
#' more of its thickness finer than loamy fine sand (Cumulic Hapludolls,
#' p. 264). \code{frigid_cm}: the thinner limit that holds in a frigid
#' temperature regime ("40 cm or more thick in a frigid soil temperature
#' regime; or 50 cm or more thick", e.g. Pachic Argiudolls, p. 261).
#' \code{cumulic_frigid}: the frigid alternative of the Cumulic Haplustolls
#' and Hapludolls (pp. 264, 276): a frigid regime and a mollic epipedon 40 cm
#' or more thick, less than half of it sandy, no densic or paralithic
#' contact and no sandy layer between 40 and 50 cm. Undecided where the
#' epipedon may or may not reach the thickness, the texture is not recorded,
#' or the regime is needed and not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm Thickness (cm).
#' @param finer,finer_half,frigid_cm,cumulic_frigid See above.
#' @noRd
sg_mollisol_mollic <- function(pedon, min_cm = 50, finer = TRUE, finer_half = FALSE,
                               frigid_cm = NULL, cumulic_frigid = FALSE) {
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 14-16; ch. 12, e.g. pp. 261, 264, 276 (Pachic, Cumulic)")
  m <- .sgm_mollic(pedon)
  lfs <- .mo_lfs_or_coarser(h)
  ep <- if (!isFALSE(m$passed) && !is.na(m$lower))
    .sgm_window(h, m$upper, m$lower) else integer(0)
  share_finer <- function() {
    if (!length(ep)) return(if (isFALSE(m$passed)) FALSE else NA)
    tot <- .sgm_clip(h, ep, m$upper, m$lower)
    .sgm_thick3(h, !lfs[ep], ep, tot / 2, m$upper, m$lower)
  }
  tex <- if (finer_half) share_finer()
         else if (finer) .mo_not(m$coarse) else TRUE
  main <- .mo_and(.sgm_thick_ge(m, min_cm), tex)
  alt <- FALSE; fr <- NULL
  if (!is.null(frigid_cm) || cumulic_frigid) {
    fr <- soil_temperature_regime_usda(pedon, "frigid")
    if (cumulic_frigid) {
      sandy_lt_half <- if (!length(ep)) NA else {
        tot <- .sgm_clip(h, ep, m$upper, m$lower)
        .mo_not(.sgm_thick3(h, lfs[ep], ep, tot / 2, m$upper, m$lower))
      }
      d <- .usda_desig(h$designation)
      dp <- any(!is.na(d$master) & ((d$master == "C" & grepl("[rd]", d$suffix)) |
                                      grepl("^Cr", d$core)))
      l45 <- .sgm_window(h, m$upper + 40, m$upper + 50)
      no_sandy45 <- .mo_not(.usda_any3(lfs[l45]))
      alt <- .mo_and(fr$passed, .sgm_thick_ge(m, 40), sandy_lt_half, !dp, no_sandy45)
    } else {
      alt <- .mo_and(fr$passed, .sgm_thick_ge(m, frigid_cm), tex)
    }
  }
  passed <- .mo_or(main, alt)
  miss <- c(m$mo$missing, if (is.na(tex)) c("clay_pct", "sand_pct"),
            if (!is.null(fr) && is.na(fr$passed)) fr$missing)
  .sgm_out("sg_mollisol_mollic", passed, layers = ep,
           evidence = list(thickness_cm = m$known, thickness_possible_cm = m$possible,
                           min_cm = min_cm, texture = tex, frigid_alternative = alt),
           missing = miss, ref = ref)
}


# ---- Fluventic, Fluvaquentic, Cumulic: slope, HTM and organic carbon ---------

#' Organic carbon and setting of the Fluventic and Cumulic subgroups
#'
#' A slope of less than 25 \% (not recorded: less than 25 \%, reading N7), less
#' than 50 cm of human-transported material at the surface (field records;
#' not recorded, absent), and an irregular decrease in organic carbon between
#' 25 and 125 cm (or a densic, lithic or paralithic contact): a rise of more
#' than 0.05 points from one layer to the next one down (reading L15), or,
#' by \code{oc}: "at125", 0.3 \% or more organic carbon at 125 cm below the
#' mineral soil surface (e.g. Fluventic Hapludolls, p. 264); "all125", 0.3
#' \% or more in all horizons within 125 cm (e.g. Fluvaquentic Endoaquolls,
#' p. 252); "irregular", the irregular decrease only (Cumulic Haplocryolls,
#' p. 256). \code{slope_htm = FALSE} drops the slope and the
#' human-transported material (Cumulic Haplogelolls, p. 257). The Holocene
#' age of the carbon is not tested.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param oc,slope_htm See above.
#' @noRd
sg_mollisol_fluventic <- function(pedon, oc = c("at125", "all125", "irregular"),
                                  slope_htm = TRUE) {
  oc <- match.arg(oc)
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, e.g. pp. 252, 256, 264 (Fluventic, Fluvaquentic, Cumulic)")
  ms <- .sgm_ms(h)
  slope <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA)[1])
  s_ok <- if (!slope_htm) TRUE else is.na(slope) || slope < 25
  htm_ok <- if (!slope_htm) TRUE else .usda_htm_surface_cm(pedon) < 50
  fo <- .usda_fluvent_oc(pedon)
  rl <- .usda_contact_top_cm(h)
  occ <- h$oc_pct
  part <- switch(oc,
    irregular = FALSE,
    at125 = {
      at <- which(!is.na(h$top_cm) & h$top_cm <= ms + 125 & h$bottom_cm > ms + 125)
      if (!is.na(rl) && rl <= ms + 125) FALSE
      else if (!length(at) || is.na(occ[at[1]])) NA else occ[at[1]] >= 0.3
    },
    all125 = {
      end <- if (!is.na(rl) && rl < ms + 125) rl else ms + 125
      w <- .sgm_window(h, ms, end)
      v <- vapply(w, function(i) if (is.na(occ[i])) NA else occ[i] >= 0.3, NA)
      deep <- .mo_described_to(h)
      v <- c(v, if (is.na(rl) || rl >= ms + 125) { if (is.na(deep) || deep < ms + 125) NA })
      if (!length(v)) FALSE else .usda_all3(v)
    })
  passed <- .mo_and(s_ok, htm_ok, .mo_or(part, fo$irregular))
  .sgm_out("sg_mollisol_fluventic", passed, layers = fo$layers,
           evidence = list(slope_pct = slope, slope_ok = s_ok, htm_ok = htm_ok,
                           oc_criterion = oc, oc_part = part, irregular = fo$irregular),
           missing = "oc_pct", ref = ref)
}


# ---- Buried horizons: Thapto-Humic, Thapto-Histic --------------------------

.sgm_buried <- function(h) {
  d <- .usda_desig(h$designation)
  !is.na(d$suffix) & grepl("b", d$suffix)
}

#' Thapto-Humic subgroups of the Mollisols (e.g. Thapto-Humic Hapludolls,
#' KST 13th ed., p. 264)
#'
#' Within 200 cm of the soil surface, a buried layer that meets the criteria
#' of a histic, mollic, umbric or melanic epipedon, or buried O and dark A
#' horizons (moist value 3 or less) 20 cm or more thick in total with 1.0 \%
#' or more organic carbon. Buried horizons are read from the b of the
#' designation (a field observation: not recorded, absent). The buried
#' epipedon is read from its colour and carbon (organic soil material 20 cm
#' or more thick; or 18 cm or more of moist value and chroma 3 or less with
#' 0.6 \% or more organic carbon); the colour or carbon not recorded leaves
#' it undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_thapto_humic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, e.g. p. 264 (Thapto-Humic Hapludolls)")
  b <- which(.sgm_buried(h) & !is.na(h$top_cm) & h$top_cm < 200)
  if (!length(b)) return(.sgm_out("sg_mollisol_thapto_humic", FALSE,
                                  evidence = list(reason = "no buried horizon recorded"), ref = ref))
  d <- .usda_desig(h$designation)
  org <- (!is.na(d$master) & grepl("^[OH]", d$master)) | (!is.na(h$oc_pct) & h$oc_pct >= 12)
  vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist; oc <- h$oc_pct
  dark_oa <- ifelse(org[b], ifelse(is.na(oc[b]), TRUE, oc[b] >= 1),
                    .mo_and_v(vm[b] <= 3, oc[b] >= 1))
  i2 <- .sgm_thick3(h, dark_oa, b, 20)
  epi <- ifelse(org[b], TRUE, .mo_and_v(vm[b] <= 3, cm[b] <= 3, oc[b] >= 0.6))
  i1 <- .mo_or(.sgm_thick3(h, org[b], b, 20), .sgm_thick3(h, epi, b, 18))
  passed <- .mo_or(i1, i2)
  .sgm_out("sg_mollisol_thapto_humic", passed, layers = b,
           evidence = list(buried_layers = b, buried_epipedon = i1, dark_o_a_20cm = i2),
           missing = c("munsell_value_moist", "oc_pct"), ref = ref)
}

# Element-wise three-valued AND of logical vectors.
.mo_and_v <- function(...) {
  x <- list(...)
  out <- Reduce(function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE,
                                      ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA)), x)
  as.logical(out)
}

#' Thapto-Histic subgroups of the Aquolls (e.g. Thapto-Histic Cryaquolls,
#' KST 13th ed., p. 250)
#'
#' A buried layer of organic soil materials 40 cm or more thick whose upper
#' boundary is within 200 cm of the mineral soil surface: buried (b) layers
#' with an O or H designation or 12 \% or more organic carbon (a measured
#' organic carbon under 12 \% prevails, reading L10). Not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_thapto_histic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  d <- .usda_desig(h$designation)
  oc <- h$oc_pct
  org <- ifelse(!is.na(oc), oc >= 12, !is.na(d$master) & grepl("^[OH]", d$master))
  b <- .sgm_buried(h) & org & !is.na(h$top_cm) & h$top_cm >= ms
  runs <- Filter(function(r) .usda_run_thickness(h, r) >= 40 && min(h$top_cm[r]) < ms + 200,
                 .usda_runs(b, order(h$top_cm)))
  .sgm_out("sg_mollisol_thapto_histic", length(runs) > 0L, layers = unlist(runs),
           evidence = list(buried_organic = which(b)),
           ref = .sgm_ref("ch. 12, e.g. p. 250 (Thapto-Histic Cryaquolls)"))
}


# ---- Duric (durinodes or brittle horizon) -----------------------------------

#' Duric subgroups of the Mollisols (e.g. Duric Argixerolls, p. 286)
#'
#' A horizon 15 cm or more thick within 100 cm of the mineral soil surface
#' with 20 \% or more (by volume) durinodes, or brittle with at least a firm
#' rupture-resistance class when moist (in the Ustolls, brittle with some
#' opal coats). soilKey has no record of durinodes or brittleness: a layer
#' with silica accumulation (q in the designation, not cemented) 15 cm or
#' more thick leaves the test undecided, since the share of durinodes is
#' not recorded; nothing recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_duric <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  w <- .sgm_window(h, ms, ms + 100)
  q <- seq_len(nrow(h)) %in% w & .sgm_suffix(h, seq_len(nrow(h)), "q") &
       !.sgm_suffix(h, seq_len(nrow(h)), "m")
  runs <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(q, order(h$top_cm)))
  passed <- if (length(runs)) NA else FALSE
  .sgm_out("sg_mollisol_duric", passed,
           evidence = list(silica_layers = which(q)),
           missing = "durinodes_pct", ref = .sgm_ref("ch. 12, e.g. p. 286 (Duric Argixerolls)"),
           notes = if (is.na(passed)) "silica accumulation (q) recorded without the share of durinodes" else NA_character_)
}


# ---- Calcic horizon and secondary carbonates --------------------------------

# Possible particle-size classes of the control section, among "sandy",
# "clayey" and "other", by the weighted clay (or the clay range of the field
# texture class) and the texture of its layers.
.sgm_psc <- function(pedon) {
  h <- pedon$horizons
  L <- .usda_pscs_layers(h)
  if (!length(L)) return(c("sandy", "clayey", "other"))
  lfs <- .usda_texture_grades(h)$lfs_or_coarser[L]
  sandy <- .usda_all3(lfs)
  cr <- .ve_clay_range(h)[L]
  ms <- .sgm_ms(h)
  w <- pmax(pmin(h$bottom_cm[L], ms + 100) - pmax(h$top_cm[L], ms + 25), 0)
  if (sum(w) <= 0) w <- pmax(h$bottom_cm[L] - h$top_cm[L], 0)
  lo <- sum(vapply(cr, `[`, 0, 1) * w) / sum(w); hi <- sum(vapply(cr, `[`, 0, 2) * w) / sum(w)
  clayey <- .mo_ge(c(lo, hi), 35)
  if (isTRUE(sandy)) return("sandy")
  c(if (is.na(sandy)) "sandy", if (!isFALSE(clayey)) "clayey",
    if (!isTRUE(clayey)) "other")
}

# Identifiable secondary carbonates in a layer (a field observation): a k in
# the designation, secondary carbonates or soft carbonate masses recorded.
.sgm_secondary_carb <- function(h) {
  sc <- .mo_col(h, "secondary_carbonates_pct"); cs <- .mo_col(h, "carbonate_soft_pct")
  .sgm_suffix(h, seq_len(nrow(h)), "k") | (!is.na(sc) & sc > 0) | (!is.na(cs) & cs > 0)
}

#' Calcic subgroups of the Mollisols (KST 13th ed., ch. 12)
#'
#' \describe{
#'   \item{within}{a calcic horizon within \code{max_cm} of the mineral soil
#'     surface (e.g. Calcic Argiudolls, p. 262).}
#'   \item{psc_secondary}{a calcic horizon or identifiable secondary
#'     carbonates within 150 cm (sandy or sandy-skeletal particle-size
#'     class), 90 cm (clayey, clayey-skeletal, fine or very-fine) or 110 cm
#'     (any other) (e.g. Calcic Argixerolls, p. 287).}
#'   \item{psc_horizon}{a calcic horizon within 100, 50 or 60 cm by the same
#'     classes (Calcic Paleustolls, p. 283).}
#'   \item{mollic}{a calcic horizon within or directly below the mollic
#'     epipedon (Calcic Cryaquolls, p. 250).}
#'   \item{above_duripan}{a calcic horizon above the duripan (Calcic
#'     Duricryolls, p. 255).}
#'   \item{paleudoll}{a calcic horizon within 100 cm and, in all parts above
#'     it after mixing the upper 18 cm, free carbonates or a texture of
#'     loamy fine sand or coarser (Calcic Paleudolls, p. 267).}
#' }
#' The particle-size class is that of the control section, by the weighted
#' clay; where it is undecided every possible class is tried and the test
#' holds only where they agree.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @param max_cm Depth for "within".
#' @noRd
sg_mollisol_calcic <- function(pedon, mode = c("within", "psc_secondary", "psc_horizon",
                                               "mollic", "above_duripan", "paleudoll"),
                               max_cm = 100) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  ref <- .sgm_ref("ch. 12, pp. 250, 255, 262, 267, 283, 287 (Calcic subgroups)")
  cal <- calcic_horizon_usda(pedon, max_top_cm = Inf)
  cv <- .usda_pv(cal)
  ctop <- if (isTRUE(cv) && length(cal$layers)) min(h$top_cm[cal$layers]) else NA_real_
  # top known (yes) and the shallowest possible top (may)
  at_depth <- function(lim, extra = numeric(0)) {
    yes <- min(c(ctop, extra, Inf), na.rm = TRUE)
    if (yes < lim) return(TRUE)
    if (is.na(cv)) return(NA)
    FALSE
  }
  miss <- cal$missing
  v <- switch(mode,
    within = at_depth(ms + max_cm),
    psc_secondary = , psc_horizon = {
      lims <- if (mode == "psc_secondary") c(sandy = 150, clayey = 90, other = 110)
              else c(sandy = 100, clayey = 50, other = 60)
      sec <- if (mode == "psc_secondary") h$top_cm[.sgm_secondary_carb(h) & !is.na(h$top_cm)] else numeric(0)
      cls <- .sgm_psc(pedon)
      miss <- c(miss, if (length(cls) > 1L) "clay_pct")
      r <- vapply(cls, function(k) at_depth(ms + lims[[k]], sec), NA)
      if (all(r %in% TRUE)) TRUE else if (all(r %in% FALSE)) FALSE else NA
    },
    mollic = {
      m <- .sgm_mollic(pedon)
      if (is.na(m$lower)) { if (isTRUE(cv)) NA else cv }
      else at_depth(m$lower + 0.5)
    },
    above_duripan = {
      du <- duripan_usda(pedon, max_top_cm = Inf)
      dt <- if (isTRUE(du$passed) && length(du$layers)) min(h$top_cm[du$layers]) else Inf
      at_depth(dt)
    },
    paleudoll = {
      w100 <- at_depth(ms + 100)
      if (!isTRUE(w100)) w100 else {
        above <- .sgm_window(h, ms, ctop)
        lfs <- .mo_lfs_or_coarser(h)
        fc <- function(i) .mo_free_carbonates(h, i)
        mix <- above[h$top_cm[above] < ms + 18]; rest <- setdiff(above, mix)
        mix_ok <- if (length(mix)) .mo_or(.mo_or(vapply(mix, fc, NA)), .mo_and(lfs[mix])) else TRUE
        rest_ok <- .mo_and(vapply(rest, function(i) .mo_or(fc(i), lfs[i]), NA))
        miss <- c(miss, "caco3_pct", "effervescence_class")
        .mo_and(mix_ok, rest_ok)
      }
    })
  .sgm_out("sg_mollisol_calcic", v, layers = cal$layers,
           evidence = list(mode = mode, calcic = cv, calcic_top_cm = ctop),
           missing = miss, ref = ref)
}


# ---- Alfic, Ultic --------------------------------------------------------------

#' Alfic subgroups of the Mollisols (e.g. Alfic Argiudolls, KST p. 262)
#'
#' An argillic horizon with, above it, an albic horizon or a horizon whose
#' colour value is too high for a mollic epipedon (moist value more than 3
#' or dry value more than 5) and whose chroma is too high for an albic
#' horizon; or (with \code{glossic = TRUE}) a glossic horizon,
#' interfingering of albic materials into the upper part of the argillic
#' horizon, or skeletans of clean silt and sand covering 50 \% or more of
#' the ped faces in its upper 5 cm. The horizons above the argillic are
#' read below the mixed upper part of the mollic epipedon (its lower
#' boundary, or 18 cm). Tongues, interfingering and skeletans are field
#' observations (not recorded, absent); a colour not recorded leaves a
#' horizon undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param glossic Include item 2.
#' @noRd
sg_mollisol_alfic <- function(pedon, glossic = TRUE) {
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, e.g. pp. 262, 268 (Alfic subgroups)")
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  av <- .usda_pv(arg)
  if (!isTRUE(av)) return(.sgm_out("sg_mollisol_alfic", if (is.na(av)) NA else FALSE,
                                   missing = arg$missing, ref = ref))
  ms <- .sgm_ms(h)
  at <- min(h$top_cm[arg$layers])
  m <- .sgm_mollic(pedon)
  from <- if (isTRUE(m$passed)) m$upper + m$known else ms + 18
  above <- which(!is.na(h$top_cm) & h$top_cm >= from - 0.01 & h$top_cm < at & !.mo_is_organic(h))
  vd <- .mo_dry_value(h)$value
  d <- .usda_desig(h$designation)
  light <- vapply(above, function(i) {
    if (!is.na(d$master[i]) && grepl("^E", d$master[i])) return(TRUE)
    .mo_or(if (is.na(h$munsell_value_moist[i])) NA else h$munsell_value_moist[i] > 3,
           if (is.na(vd[i])) NA else vd[i] > 5)
  }, NA)
  i1 <- .usda_any3(light)
  if (!length(above)) i1 <- FALSE
  i2 <- FALSE
  if (glossic) {
    tg <- .mo_col(h, "tongue_depth_cm"); am <- .mo_col(h, "albic_materials_pct")
    sk <- .mo_col(h, "skeletans_pct")
    A <- arg$layers
    up <- A[which.min(h$top_cm[A])]
    gl <- !is.na(d$master) & grepl("E/B|B/E|E&B|B&E|E/|/E", d$master)
    i2 <- any(gl) || any(!is.na(tg[A]) & tg[A] > 0) || any(!is.na(am[A]) & am[A] > 0) ||
          (!is.na(sk[up]) && sk[up] >= 50)
  }
  passed <- .mo_or(i1, i2)
  .sgm_out("sg_mollisol_alfic", passed, layers = above,
           evidence = list(light_horizon_above = i1, glossic = i2, argillic_top_cm = at),
           missing = c("munsell_value_moist", "munsell_value_dry"), ref = ref)
}

#' Ultic subgroups of the Mollisols (e.g. Ultic Argixerolls, KST p. 287)
#'
#' A base saturation (by sum of cations) of 75 \% or less in one or more
#' horizons: \describe{
#'   \item{upper75}{between an Ap horizon or 25 cm, whichever is deeper,
#'     and 75 cm or a densic, lithic or paralithic contact, whichever is
#'     shallower;}
#'   \item{lithic}{between the mineral soil surface or an Ap horizon,
#'     whichever is deeper, and the lithic contact (Lithic Ultic, p. 285);}
#'   \item{argillic50}{within the argillic horizon if 50 cm or less thick,
#'     or its upper 50 cm (Ultic Palexerolls, p. 294).}
#' }
#' Where the base saturation by sum of cations is not measured, the NH4OAc
#' value stands in (reading L18). Not measured: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param zone One of the names above.
#' @noRd
sg_mollisol_ultic <- function(pedon, zone = c("upper75", "lithic", "argillic50")) {
  zone <- match.arg(zone)
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, pp. 285, 287, 291, 294 (Ultic subgroups)")
  ms <- .sgm_ms(h)
  d <- .usda_desig(h$designation)
  ap <- which(!is.na(d$master) & d$master == "A" & !is.na(d$suffix) & grepl("p", d$suffix))
  apb <- if (length(ap)) max(h$bottom_cm[ap], na.rm = TRUE) else -Inf
  ct <- .usda_contact_top_cm(h)
  rng <- switch(zone,
    upper75 = c(max(ms + 25, apb), min(ms + 75, if (is.na(ct)) Inf else ct)),
    lithic = c(max(ms, apb), if (is.na(ct)) ms + 50 else ct),
    argillic50 = {
      arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
      if (!isTRUE(.usda_pv(arg))) return(.sgm_out("sg_mollisol_ultic", .usda_pv(arg),
                                                  missing = arg$missing, ref = ref))
      at <- min(h$top_cm[arg$layers]); c(at, at + 50)
    })
  w <- .sgm_window(h, rng[1], rng[2])
  bs <- .bs_sum_cations_usda(pedon)
  v <- if (!length(w) || rng[2] <= rng[1]) FALSE
       else .usda_any3(vapply(w, function(i) if (is.na(bs$bs[i])) NA else bs$bs[i] <= 75, NA))
  src <- unique(stats::na.omit(bs$source[w]))
  .sgm_out("sg_mollisol_ultic", v, layers = w,
           evidence = list(zone = zone, depth_cm = rng, source = src),
           missing = "bs_pct", ref = ref,
           notes = if (!is.na(v) && "NH4OAc" %in% src)
             "base saturation by NH4OAc read for the sum of cations (L18)" else NA_character_)
}


# ---- Clay increase at the argillic or natric horizon ---------------------------

# Pairs (layer above, layer below) at the upper boundary of the horizon
# `H` and within it, with the clay increase and the boundary distinctness.
.sgm_clay_steps <- function(h, H) {
  ord <- order(h$top_cm)
  out <- list()
  for (k in seq_along(ord)[-1]) {
    i <- ord[k]; p <- ord[k - 1]
    if (!i %in% H) next
    if (is.na(h$bottom_cm[p]) || abs(h$bottom_cm[p] - h$top_cm[i]) > 0.5) next
    out[[length(out) + 1L]] <- list(over = p, here = i, upper = !p %in% H)
  }
  out
}

# Boundary distinctness: "within" 7.5 cm (abrupt or clear), "within 2.5 cm"
# (abrupt, very abrupt), or not; NA where not recorded.
.sgm_bd_within <- function(bd, cm = 7.5) {
  bd <- .field_term(bd)
  if (is.na(bd) || !nzchar(bd)) return(NA)
  if (cm >= 7.5) {
    if (grepl("abrupt|clear|sharp", bd)) return(TRUE)
    if (grepl("gradual|diffuse", bd)) return(FALSE)
  } else {
    if (grepl("abrupt|sharp", bd)) return(TRUE)
    if (grepl("clear|gradual|diffuse", bd)) return(FALSE)
  }
  NA
}

#' Abruptic subgroups of the Mollisols (e.g. Abruptic Argiudolls, p. 262)
#'
#' An argillic (or, with \code{horizon = "natric"}, natric) horizon with a
#' clay increase of 20 \% or more (absolute) within a vertical distance of
#' 7.5 cm, within the horizon or at its upper boundary, or (with
#' \code{alt15 = TRUE}, the Palexerollic and Argiduridic Durixerolls, p.
#' 287) of 15 \% or more within 2.5 cm; or an abrupt textural change
#' between the eluvial horizon and the upper boundary of the horizon
#' (\code{.abrupt_textural_change_usda()}). \code{change_only = TRUE} reads
#' only the abrupt textural change (Argiaquic Argialbolls, p. 248). The
#' vertical distance is read from the boundary distinctness of the upper
#' layer (abrupt or clear: within 7.5 cm; abrupt: within 2.5 cm); not
#' recorded, undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon,alt15,change_only See above.
#' @noRd
sg_mollisol_abruptic <- function(pedon, horizon = c("argillic", "natric"), alt15 = FALSE,
                                 change_only = FALSE) {
  horizon <- match.arg(horizon)
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, pp. 248, 262, 287 (Abruptic subgroups)")
  r <- tryCatch(if (horizon == "argillic") argillic_usda(pedon) else natric_horizon_usda(pedon),
                error = function(e) NULL)
  rv <- .usda_pv(r)
  if (!isTRUE(rv)) return(.sgm_out("sg_mollisol_abruptic", rv, missing = r$missing, ref = ref))
  bd <- .mo_col(h, "boundary_distinctness")
  cl <- h$clay_pct
  steps <- .sgm_clay_steps(h, r$layers)
  v <- vapply(steps, function(s) {
    over <- cl[s$over]; here <- cl[s$here]
    atc <- if (s$upper) .abrupt_textural_change_usda(over, here, bd[s$over]) else FALSE
    if (change_only) return(atc)
    inc <- if (is.na(over) || is.na(here)) NA else here - over
    a <- .mo_and(if (is.na(inc)) NA else inc >= 20, .sgm_bd_within(bd[s$over], 7.5))
    b <- if (alt15) .mo_and(if (is.na(inc)) NA else inc >= 15, .sgm_bd_within(bd[s$over], 2.5)) else FALSE
    .mo_or(a, b, atc)
  }, NA)
  passed <- if (!length(v)) FALSE else .usda_any3(v)
  .sgm_out("sg_mollisol_abruptic", passed, layers = r$layers,
           evidence = list(horizon = horizon, steps = v),
           missing = c("clay_pct", "boundary_distinctness"), ref = ref)
}

#' Haplic Palexerolls (KST 13th ed., p. 294)
#'
#' An argillic horizon with less than 35 \% clay in its upper part, or at
#' its upper boundary a clay increase both less than 20 \% (absolute) within
#' 7.5 cm and less than 15 \% within 2.5 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_haplic_palexeroll <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgm_ref("p. 294 (Haplic Palexerolls)")
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  av <- .usda_pv(arg)
  if (!isTRUE(av)) return(.sgm_out("sg_mollisol_haplic_palexeroll", av, missing = arg$missing, ref = ref))
  A <- arg$layers; up <- A[which.min(h$top_cm[A])]
  c35 <- if (is.na(h$clay_pct[up])) NA else h$clay_pct[up] < 35
  bd <- .mo_col(h, "boundary_distinctness")
  st <- Filter(function(s) s$upper, .sgm_clay_steps(h, A))
  inc_small <- if (!length(st)) TRUE else {
    s <- st[[1]]
    inc <- h$clay_pct[s$here] - h$clay_pct[s$over]
    a <- .mo_and(if (is.na(inc)) NA else inc >= 20, .sgm_bd_within(bd[s$over], 7.5))
    b <- .mo_and(if (is.na(inc)) NA else inc >= 15, .sgm_bd_within(bd[s$over], 2.5))
    .mo_not(.mo_or(a, b))
  }
  passed <- .mo_or(c35, inc_small)
  .sgm_out("sg_mollisol_haplic_palexeroll", passed, layers = up,
           evidence = list(clay_upper_lt35 = c35, small_increase = inc_small),
           missing = c("clay_pct", "boundary_distinctness"), ref = ref)
}


# ---- Natric-horizon subgroups: Glossic, Leptic ------------------------------

#' Glossic subgroups of the Natr- Mollisols (e.g. Glossic Natrudolls, p. 266)
#'
#' A glossic horizon or interfingering of albic materials into the natric
#' horizon: E/B, B/E or E&B designations, tongues (\code{tongue_depth_cm}) or
#' albic materials recorded in the natric horizon. Field observations: not
#' recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_glossic <- function(pedon) {
  h <- pedon$horizons
  d <- .usda_desig(h$designation)
  nat <- tryCatch(natric_horizon_usda(pedon), error = function(e) NULL)
  N <- if (isTRUE(.usda_pv(nat))) nat$layers else integer(0)
  tg <- .mo_col(h, "tongue_depth_cm"); am <- .mo_col(h, "albic_materials_pct")
  gl <- which(!is.na(d$master) & grepl("E/B|B/E|E&B|B&E", d$master))
  hit <- c(gl, N[(!is.na(tg[N]) & tg[N] > 0) | (!is.na(am[N]) & am[N] > 0)])
  .sgm_out("sg_mollisol_glossic", length(hit) > 0L, layers = hit,
           ref = .sgm_ref("ch. 12, pp. 253, 266, 281 (Glossic subgroups)"))
}

#' Leptic subgroups (e.g. Leptic Natrudolls, p. 266)
#'
#' Visible crystals of gypsum or more soluble salts within 40 cm of the
#' mineral soil surface: \code{gypsum_visible_pct} or
#' \code{halite_visible_pct} above 0, or a y or z in the designation. Field
#' observations: not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_leptic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  w <- .sgm_window(h, ms, ms + 40)
  gv <- .mo_col(h, "gypsum_visible_pct"); hv <- .mo_col(h, "halite_visible_pct")
  hit <- w[(!is.na(gv[w]) & gv[w] > 0) | (!is.na(hv[w]) & hv[w] > 0) | .sgm_suffix(h, w, "yz")]
  .sgm_out("sg_mollisol_leptic", length(hit) > 0L, layers = hit,
           ref = .sgm_ref("ch. 12, pp. 249, 266, 280 (Leptic subgroups)"))
}


# ---- Sandy textures: Arenic, Grossarenic, Psammentic ---------------------------

#' Arenic and Grossarenic subgroups of the Mollisols (pp. 249, 262)
#'
#' A texture class (fine earth) of coarse sand, sand, fine sand, loamy
#' coarse sand, loamy sand or loamy fine sand throughout the layer from the
#' mineral soil surface to the top of an argillic horizon at a depth between
#' \code{min_cm} and \code{max_cm} (Arenic Argiaquolls 50-100 cm; Arenic
#' Argiudolls 50 cm or more; Grossarenic Argiaquolls 100 cm or more). The
#' Mollisol wording: no spodic horizon is involved (audit R5). Texture not
#' recorded: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Depth of the top of the argillic horizon.
#' @noRd
sg_mollisol_arenic <- function(pedon, min_cm = 50, max_cm = Inf) {
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, pp. 249, 262 (Arenic, Grossarenic)")
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  av <- .usda_pv(arg)
  if (!isTRUE(av)) return(.sgm_out("sg_mollisol_arenic", av, missing = arg$missing, ref = ref))
  ms <- .sgm_ms(h)
  at <- min(h$top_cm[arg$layers]) - ms
  if (at < min_cm || at > max_cm) return(.sgm_out("sg_mollisol_arenic", FALSE,
                                                  evidence = list(argillic_top_cm = at), ref = ref))
  w <- .sgm_window(h, ms, ms + at)
  s <- .usda_texture_grades(h)$lfs_or_coarser[w]
  passed <- .usda_all3(s)
  .sgm_out("sg_mollisol_arenic", passed, layers = w, evidence = list(argillic_top_cm = at),
           missing = c("clay_pct", "sand_pct", "texture_class_usda"), ref = ref)
}

#' Psammentic subgroups of the Mollisols
#'
#' \describe{
#'   \item{profile}{a sandy particle-size class in all horizons within 100
#'     cm of the mineral soil surface (Psammentic and Torripsammentic
#'     Haploxerolls, pp. 291-292);}
#'   \item{argillic}{a sandy particle-size class throughout the upper 75 cm
#'     of the argillic horizon, or all of it if thinner (Psammentic
#'     Argiudolls, p. 262).}
#' }
#' Sandy: loamy fine sand or coarser with less than 35 \% rock fragments
#' (rock fragments not recorded: absent).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @noRd
sg_mollisol_psammentic <- function(pedon, mode = c("profile", "argillic")) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ref <- .sgm_ref("ch. 12, pp. 262, 291-292 (Psammentic subgroups)")
  ms <- .sgm_ms(h)
  w <- if (mode == "profile") {
    ct <- .usda_contact_top_cm(h)
    .sgm_window(h, ms, min(ms + 100, if (is.na(ct)) Inf else ct))
  } else {
    arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
    if (!isTRUE(.usda_pv(arg))) return(.sgm_out("sg_mollisol_psammentic", .usda_pv(arg),
                                               missing = arg$missing, ref = ref))
    at <- min(h$top_cm[arg$layers])
    intersect(.sgm_window(h, at, at + 75), arg$layers)
  }
  lfs <- .usda_texture_grades(h)$lfs_or_coarser
  rf <- .mo_col(h, "coarse_fragments_pct")
  s <- ifelse(!is.na(rf[w]) & rf[w] >= 35, FALSE, lfs[w])
  passed <- if (!length(w)) FALSE else .usda_all3(s)
  .sgm_out("sg_mollisol_psammentic", passed, layers = w,
           missing = c("clay_pct", "sand_pct", "texture_class_usda"), ref = ref)
}

#' Lamellic Argiudolls (KST 13th ed., p. 262)
#'
#' An argillic horizon made of lamellae, alone or with thicker parts each
#' under an eluvial horizon: read as an argillic horizon with lamellae
#' recorded in it (an & or "lamella" in the designation). A field
#' observation: not recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_lamellic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgm_ref("p. 262 (Lamellic Argiudolls)")
  arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
  av <- .usda_pv(arg)
  if (!isTRUE(av)) return(.sgm_out("sg_mollisol_lamellic", av, missing = arg$missing, ref = ref))
  d <- as.character(h$designation)
  lam <- which(!is.na(d) & grepl("&|lamel", d, ignore.case = TRUE))
  hit <- union(intersect(lam, arg$layers), lam[grepl("Bt", d[lam])])
  .sgm_out("sg_mollisol_lamellic", length(hit) > 0L, layers = hit, ref = ref)
}


# ---- Oxic, Torroxic -------------------------------------------------------------

#' Oxic subgroups of the Mollisols
#'
#' A CEC (NH4OAc, pH 7) of less than 24 cmol(+)/kg clay in 50 \% or more of:
#' \describe{
#'   \item{argillic}{the argillic horizon, or its upper 100 cm (Oxic
#'     Argiudolls, p. 262);}
#'   \item{apparent}{the soil volume between 25 cm and 100 cm or a densic,
#'     lithic or paralithic contact, with the apparent clay of the key: where
#'     (1500 kPa water - organic carbon) / clay is 0.6 or more, the clay is
#'     the larger of the measured clay and 3 x (1500 kPa water - organic
#'     carbon), at most 100 (Oxic and Torroxic Haplustolls, p. 274).}
#' }
#' CEC or clay not measured: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @noRd
sg_mollisol_oxic <- function(pedon, mode = c("argillic", "apparent")) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 262, 274 (Oxic, Torroxic)")
  ms <- .sgm_ms(h)
  clay <- h$clay_pct
  if (mode == "apparent") {
    wc <- .mo_col(h, "water_content_1500kpa"); oc <- h$oc_pct
    x <- wc - ifelse(is.na(oc), 0, oc)
    ratio <- x / clay
    clay <- ifelse(!is.na(ratio) & ratio >= 0.6, pmin(100, pmax(clay, 3 * x)), clay)
    ct <- .usda_contact_top_cm(h)
    rng <- c(ms + 25, min(ms + 100, if (is.na(ct)) Inf else ct))
    w <- .sgm_window(h, rng[1], rng[2])
  } else {
    arg <- tryCatch(argillic_usda(pedon), error = function(e) NULL)
    if (!isTRUE(.usda_pv(arg))) return(.sgm_out("sg_mollisol_oxic", .usda_pv(arg),
                                               missing = arg$missing, ref = ref))
    at <- min(h$top_cm[arg$layers])
    rng <- c(at, at + 100)
    w <- intersect(.sgm_window(h, rng[1], rng[2]), arg$layers)
  }
  cec <- h$cec_cmol
  low <- ifelse(is.na(cec) | is.na(clay) | clay <= 0, NA, 100 * cec / clay < 24)[w]
  tot <- .sgm_clip(h, w, rng[1], rng[2])
  passed <- if (!length(w)) FALSE else .sgm_thick3(h, low, w, tot / 2, rng[1], rng[2])
  .sgm_out("sg_mollisol_oxic", passed, layers = w,
           evidence = list(mode = mode, depth_cm = rng),
           missing = c("cec_cmol", "clay_pct"), ref = ref)
}


# ---- Entic subgroups -------------------------------------------------------------

# The Entic test of the Haplustolls, Hapludolls and Haploxerolls (pp. 265,
# 278, 292): no cambic horizon and no part of the mollic epipedon below 25
# cm that meets the cambic requirements except colour (15 cm or more of
# very fine sand or finer with soil structure); or free carbonates
# throughout the cambic horizon or in all parts of the mollic epipedon below
# 25 cm.
.sgm_entic_cambic <- function(pedon) {
  h <- pedon$horizons
  m <- .sgm_mollic(pedon)
  cam <- tryCatch(cambic_horizon_usda(pedon), error = function(e) NULL)
  cv <- .usda_pv(cam)
  from <- m$upper + 25
  to <- if (is.na(m$lower)) from else m$lower
  w <- .sgm_window(h, from, to)
  vfs <- .usda_texture_grades(h)$vfs_or_finer
  st <- .field_term(.mo_col(h, "structure_type")); gr <- .field_term(.mo_col(h, "structure_grade"))
  structured <- ifelse(!is.na(st), !grepl("massive|single grain|structureless|rock", st),
                       ifelse(!is.na(gr), !grepl("structureless|massive|single", gr), NA))
  meets <- .mo_and_v(vfs[w], structured[w])
  mc <- if (!length(w)) FALSE else .sgm_thick3(h, meets, w, 15, from, to)
  c1 <- .mo_and(.mo_not(cv), .mo_not(mc))
  fcw <- if (isTRUE(cv)) cam$layers else w
  c2 <- if (!length(fcw)) FALSE else .mo_and(vapply(fcw, function(i) .mo_free_carbonates(h, i), NA))
  list(passed = .mo_or(c1, c2), cambic = cv, mollic_meets_cambic = mc, carbonates = c2,
       missing = c(if (is.na(cv)) cam$missing, if (is.na(mc)) "structure_type",
                   if (is.na(c2)) "caco3_pct"))
}

#' Entic subgroups of the Mollisols
#'
#' \describe{
#'   \item{cambic}{Entic Haplustolls, Hapludolls and Haploxerolls (pp. 265,
#'     278, 292): no cambic horizon and no part of the mollic epipedon below
#'     25 cm that meets the cambic requirements except colour (15 cm or more
#'     of very fine sand or finer with soil structure); or free carbonates
#'     throughout the cambic horizon or the mollic epipedon below 25 cm.}
#'   \item{carbonates}{Entic Paleustolls (p. 283): free carbonates
#'     throughout after the upper 18 cm are mixed.}
#'   \item{haprendoll}{Entic Haprendolls (p. 258): a dry colour value of 6 or
#'     more in the upper 18 cm of the mollic epipedon after mixing, or in an
#'     Ap horizon 18 cm or more thick.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @noRd
sg_mollisol_entic <- function(pedon, mode = c("cambic", "carbonates", "haprendoll")) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 258, 265, 278, 283, 292 (Entic subgroups)")
  ms <- .sgm_ms(h)
  miss <- character(0)
  v <- switch(mode,
    cambic = { r <- .sgm_entic_cambic(pedon); miss <- r$missing; r$passed },
    carbonates = {
      w <- .sgm_window(h, ms, Inf)
      ct <- .usda_contact_top_cm(h)
      if (!is.na(ct)) w <- w[h$top_cm[w] < ct]
      fc <- vapply(w, function(i) .mo_free_carbonates(h, i), NA)
      mix <- h$top_cm[w] < ms + 18
      miss <- c("caco3_pct", "effervescence_class")
      if (!length(w)) FALSE else .mo_and(if (any(mix)) .usda_any3(fc[mix]) else TRUE,
                                          .usda_all3(fc[!mix]))
    },
    haprendoll = {
      vd <- .mo_dry_value(h)$value
      w <- .sgm_window(h, ms, ms + 18)
      wt <- pmin(h$bottom_cm[w], ms + 18) - pmax(h$top_cm[w], ms)
      mixed <- .mo_ge(.mo_wrange(vd[w], wt, 0, 10), 6)
      d <- .usda_desig(h$designation)
      ap <- which(!is.na(d$master) & d$master == "A" & grepl("p", d$suffix))
      apv <- if (length(ap) && .usda_run_thickness(h, ap) >= 18)
        .usda_all3(vapply(ap, function(i) if (is.na(vd[i])) NA else vd[i] >= 6, NA)) else FALSE
      miss <- "munsell_value_dry"
      .mo_or(mixed, apv)
    })
  .sgm_out("sg_mollisol_entic", v, evidence = list(mode = mode), missing = miss, ref = ref)
}


# ---- Vermic subgroups -------------------------------------------------------------

#' Vermic subgroups of the Mollisols
#'
#' A mollic epipedon with 50 \% or more (by volume, a thickness-weighted
#' mean) wormholes, wormcasts or filled animal burrows below any Ap horizon
#' (\code{mode = "any"}, Vermic Calcixerolls, p. 286); with granular
#' structure in the mollic epipedon (\code{"xeroll"}, Vermic Haploxerolls, p.
#' 292); or (\code{"hapludoll"}, Vermic Hapludolls, p. 265) a mollic epipedon
#' 60 cm or more thick, finer than loamy fine sand, with 50 \% or more worm
#' holes below an Ap horizon or 18 cm, and the Entic condition of the
#' Hapludolls. Worm holes are a field observation (not recorded, absent).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @noRd
sg_mollisol_vermic <- function(pedon, mode = c("any", "xeroll", "hapludoll")) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 265, 286, 292 (Vermic subgroups)")
  m <- .sgm_mollic(pedon)
  if (isFALSE(m$passed)) return(.sgm_out("sg_mollisol_vermic", FALSE, ref = ref))
  d <- .usda_desig(h$designation)
  ap <- which(!is.na(d$master) & d$master == "A" & grepl("p", d$suffix))
  from <- max(m$upper + if (mode == "hapludoll") 18 else 0,
              if (length(ap)) max(h$bottom_cm[ap]) else -Inf)
  to <- if (is.na(m$lower)) m$upper else m$lower
  w <- .sgm_window(h, from, to)
  wh <- .mo_col(h, "worm_holes_pct")
  wl <- pmin(h$bottom_cm[w], to) - pmax(h$top_cm[w], from)
  worms <- length(w) > 0L && sum(ifelse(is.na(wh[w]), 0, wh[w]) * wl) / sum(wl) >= 50
  if (!worms) return(.sgm_out("sg_mollisol_vermic", FALSE, ref = ref))
  extra <- switch(mode,
    any = TRUE,
    xeroll = {
      ep <- .sgm_window(h, m$upper, to)
      st <- .field_term(.mo_col(h, "structure_type"))[ep]
      if (any(!is.na(st) & grepl("granular|crumb", st))) TRUE else if (anyNA(st)) NA else FALSE
    },
    hapludoll = .mo_and(.usda_pv(sg_mollisol_mollic(pedon, 60, finer = TRUE)),
                        .sgm_entic_cambic(pedon)$passed))
  passed <- .mo_and(m$passed, extra)
  .sgm_out("sg_mollisol_vermic", passed, layers = w, evidence = list(mode = mode),
           missing = c(m$mo$missing, "structure_type"), ref = ref)
}


# ---- Moisture intergrades ------------------------------------------------------

#' Moisture-regime intergrades of the Mollisol subgroups
#'
#' The subgroups named for a drier or moister neighbour of the suborder's
#' own regime, read from the recorded regime (\code{site$soil_moisture_regime})
#' and, where recorded, a subdivision of it (\code{site$soil_moisture_subclass},
#' e.g. "aridic ustic", "udic ustic", "typic xeric", "dry xeric"); the
#' regime is never inferred (audit P2, P3).
#' \describe{
#'   \item{aridic}{the Aridic, Torr- and -idic subgroups of the Ustolls and
#'     Xerolls (e.g. Aridic Haplustolls, p. 278; Aridic Haploxerolls, p.
#'     292): an aridic regime, or the dryness bordering it. TRUE where aridic
#'     (torric) is recorded or a subdivision names it (aridic, dry); FALSE
#'     where a udic or perudic regime is recorded or a subdivision is typic
#'     or udic; undecided where only the suborder's own ustic or xeric regime
#'     is recorded, which does not say how dry it is.}
#'   \item{udic}{the Udic, Udertic and Udorthentic Ustolls (e.g. Udic
#'     Haplustolls, p. 278): the moister side, by the same reading.}
#'   \item{ustic_cryoll}{Ustic Cryolls (e.g. p. 255): dry in some part of the
#'     moisture control section for 45 days or more; an ustic or aridic
#'     regime recorded passes; udic, perudic, aquic or xeric fails (the xeric
#'     Cryolls are the next subgroup).}
#'   \item{xeric_summer}{the Xeric and Xerertic Argialbolls (p. 248): dry in
#'     all parts for 45 or more consecutive days in the 120 days after the
#'     summer solstice; a xeric or aridic regime passes, any other fails.}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind One of the names above.
#' @noRd
sg_mollisol_moisture <- function(pedon, kind = c("aridic", "udic", "ustic_cryoll", "xeric_summer")) {
  kind <- match.arg(kind)
  ref <- .sgm_ref("ch. 3, pp. 47-49; ch. 12 (moisture intergrades)")
  raw <- pedon$site$soil_moisture_regime %||% NA_character_
  blank <- .usda_regime_blank(raw)
  smr <- if (blank) NA_character_ else trimws(.usda_smr_value(as.character(raw)[1]))
  sub <- tolower(as.character(pedon$site$soil_moisture_subclass %||% NA_character_)[1])
  sub_kind <- if (is.na(sub) || !nzchar(trimws(sub))) NA_character_
              else if (grepl("aridic|\\bdry\\b", sub)) "aridic"
              else if (grepl("udic|\\bwet\\b", sub)) "udic"
              else if (grepl("typic", sub)) "typic" else NA_character_
  dry <- c("aridic", "torric")
  v <- switch(kind,
    aridic = if (!is.na(sub_kind)) sub_kind == "aridic"
             else if (blank) NA else if (smr %in% dry) TRUE
             else if (smr %in% c("udic", "perudic", "aquic")) FALSE else NA,
    udic = if (!is.na(sub_kind)) sub_kind == "udic"
           else if (blank) NA else if (smr %in% c("udic", "perudic")) TRUE
           else if (smr %in% c(dry, "aquic")) FALSE else NA,
    ustic_cryoll = if (blank) NA else smr %in% c("ustic", dry),
    xeric_summer = if (blank) NA else smr %in% c("xeric", dry))
  .sgm_out("sg_mollisol_moisture", v,
           evidence = list(kind = kind, soil_moisture_regime = raw, subclass = sub),
           missing = if (blank) "site$soil_moisture_regime" else "site$soil_moisture_subclass",
           ref = ref)
}


# ---- Duripan subgroups (Durixerolls, Durustolls) --------------------------------

#' Duripan readings of the Durixerolls and Durustolls subgroups
#'
#' \describe{
#'   \item{weak}{a duripan that is neither very strongly coherent nor
#'     indurated in any subhorizon (the Haplic, Argidic, Cambidic and
#'     Paleargidic Durixerolls, p. 288; "strongly coherent or less coherent
#'     in all subhorizons", Haplic Durustolls, p. 273): every duripan layer
#'     with a recorded cementation class other than very strongly cemented
#'     or indurated. A duripan whose cementation class is not recorded:
#'     undecided.}
#'   \item{natric_above}{a natric horizon above the duripan (Natric
#'     Durustolls, p. 273).}
#' }
#' @param pedon A \code{\link{PedonRecord}}.
#' @param mode One of the names above.
#' @noRd
sg_mollisol_duripan <- function(pedon, mode = c("weak", "natric_above")) {
  mode <- match.arg(mode)
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 273, 287-288 (Durixerolls, Durustolls)")
  du <- duripan_usda(pedon, max_top_cm = Inf)
  dv <- .usda_pv(du)
  if (!isTRUE(dv)) return(.sgm_out("sg_mollisol_duripan", dv, missing = du$missing, ref = ref))
  D <- du$layers
  v <- switch(mode,
    weak = {
      cem <- .field_term(.mo_col(h, "cementation_class"))[D]
      .usda_all3(ifelse(is.na(cem), NA, !grepl("indurated|very strong", cem)))
    },
    natric_above = {
      nat <- tryCatch(natric_horizon_usda(pedon), error = function(e) NULL)
      nv <- .usda_pv(nat)
      if (isTRUE(nv)) any(h$top_cm[nat$layers] < min(h$top_cm[D])) else nv
    })
  .sgm_out("sg_mollisol_duripan", v, layers = D, evidence = list(mode = mode),
           missing = "cementation_class", ref = ref)
}


# ---- Single-great-group subgroups ------------------------------------------------

#' Aeric Calciaquolls (KST 13th ed., p. 250)
#'
#' 50 \% or more chroma of 3 or more in one or more horizons within 75 cm
#' (the matrix colour), or directly below the mollic epipedon hue 2.5Y or
#' yellower with chroma 3 or more, hue 10YR or redder with chroma 2 or
#' more, or hue 2.5Y or yellower with chroma 2 or more and no distinct or
#' prominent redox concentrations. Colour not recorded: undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_aeric <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  cm <- h$munsell_chroma_moist
  w <- .sgm_window(h, ms, ms + 75)
  a <- .usda_any3(vapply(w, function(i) if (is.na(cm[i])) NA else cm[i] >= 3, NA))
  geo <- .mo_geometry(pedon)
  nb <- geo$below
  b <- if (is.na(nb)) FALSE else {
    hr <- .mo_hue_rank(h$munsell_hue_moist[nb]); c <- cm[nb]
    if (is.na(hr) || is.na(c)) NA else {
      rc <- .mo_redox_conc(h, nb, contrast = TRUE)
      .mo_or(hr >= 6 && c >= 3, hr >= 1 && hr <= 5 && c >= 2,
             .mo_and(hr >= 6 && c >= 2, .mo_not(rc)))
    }
  }
  .sgm_out("sg_mollisol_aeric", .mo_or(a, b), evidence = list(chroma3_75cm = a, below_mollic = b),
           missing = c("munsell_chroma_moist", "munsell_hue_moist"),
           ref = .sgm_ref("p. 250 (Aeric Calciaquolls)"))
}

#' Ruptic-Lithic Haplustolls (KST 13th ed., p. 273)
#'
#' A lithic contact within 50 cm in part of each pedon and below 50 cm in
#' other parts: a lithic contact within 50 cm whose upper boundary is
#' recorded as broken or irregular (the topography of the boundary above it,
#' or the continuity of the R layer). Field observations: not recorded,
#' absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_ruptic_lithic <- function(pedon) {
  h <- pedon$horizons
  li <- lithic_contact_usda(pedon, max_top_cm = 50)
  hit <- FALSE
  if (isTRUE(li$passed)) {
    r <- li$layers[1]
    above <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[r]) < 0.5)
    bt <- .field_term(.mo_col(h, "boundary_topography"))[above]
    hc <- .field_term(.mo_col(h, "horizon_continuity"))[r]
    hit <- any(!is.na(bt) & grepl("broken|irregular|discontinuous", bt)) ||
           (!is.na(hc) && grepl("broken|discontinuous", hc))
  }
  .sgm_out("sg_mollisol_ruptic_lithic", hit, layers = li$layers,
           ref = .sgm_ref("p. 273 (Ruptic-Lithic Haplustolls)"))
}

#' Petrocalcic horizon formed in human-transported material (Anthropic
#' Petrocalcic Calciudolls, KST 13th ed., p. 262): a petrocalcic horizon
#' within 100 cm whose layers are human-transported material (field
#' records; not recorded, absent).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_htm_petrocalcic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgm_ms(h)
  pc <- petrocalcic_subgroup_usda(pedon, max_top_cm = ms + 100)
  pv <- .usda_pv(pc)
  htm <- .usda_htm_layers(pedon)
  v <- if (isTRUE(pv)) any(htm[pc$layers] %in% TRUE) else if (is.na(pv)) {
    if (any(htm)) NA else FALSE } else FALSE
  .sgm_out("sg_mollisol_htm_petrocalcic", v, layers = pc$layers, missing = pc$missing,
           ref = .sgm_ref("p. 262 (Anthropic Petrocalcic Calciudolls)"))
}

#' Anthraquic Haplustolls (KST 13th ed., p. 276): anthraquic conditions, read
#' from a recorded anthraquic (anthrostagnic) water regime, or a puddled
#' layer with redox features within 100 cm. Field observations: not
#' recorded, absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_anthraquic <- function(pedon) {
  h <- pedon$horizons
  raw <- tolower(as.character(pedon$site$soil_moisture_regime %||% NA)[1])
  wr <- .field_term(.mo_col(h, "water_regime_pattern"))
  pud <- .mo_col(h, "puddled")
  ms <- .sgm_ms(h)
  w <- .sgm_window(h, ms, ms + 100)
  redox <- any(vapply(w, function(i) .sgm_aquic_layer(h, i), NA))
  hit <- (!is.na(raw) && grepl("anthraquic|anthrostagnic", raw)) ||
         any(!is.na(wr) & grepl("anthraquic|anthrostagnic|paddy", wr)) ||
         (any(as.logical(pud) %in% TRUE) && redox)
  .sgm_out("sg_mollisol_anthraquic", hit, ref = .sgm_ref("p. 276 (Anthraquic Haplustolls)"))
}

#' Aquandic subgroups of the Aquolls and Argialbolls (e.g. Aquandic
#' Endoaquolls, KST 13th ed., p. 251)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm, one or more of: the andic criteria (bulk density 1.0 or less
#' and Al + 1/2 Fe more than 1.0), more than 35 \% cinders, or the volcanic
#' glass criteria of the Vitrandic subgroups. Unlike
#' \code{aquandic_subgroup_usda()}, no aquic test: these are already Aquolls
#' or Albolls. Read layer by layer as \code{andic_subgroup_usda()} and
#' \code{vitrandic_subgroup_usda()} do.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_mollisol_aquandic <- function(pedon) {
  h <- pedon$horizons
  ref <- .sgm_ref("pp. 248-252 (Aquandic subgroups)")
  if (!nrow(h)) return(.sgm_out("sg_mollisol_aquandic", FALSE, ref = ref))
  L <- .andic_subgroup_layers_usda(h)
  ok <- .mo_or_v(L$andic, L$vitrandic)
  r <- .andic_subgroup_18_usda(h, ok)
  .sgm_out("sg_mollisol_aquandic", r$passed, layers = r$layers,
           evidence = list(thickness_cm = r$thickness_cm, undecided_cm = r$undecided_cm),
           missing = c(L$missing_andic, L$missing_vitrandic), ref = ref)
}

.mo_or_v <- function(a, b) {
  as.logical(ifelse(a %in% TRUE | b %in% TRUE, TRUE,
                    ifelse(a %in% FALSE & b %in% FALSE, FALSE, NA)))
}
