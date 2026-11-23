# ================================================================
# USDA Soil Taxonomy -- the subgroups of the Ultisols and Oxisols
# (Keys to Soil Taxonomy, 13th ed., 2022: Oxisols, Ch. 13, pp. 295-309;
# Ultisols, Ch. 15, pp. 321-341).
#
# v0.9.423 (audit items R3 to R6, O7, F16, F17, G7; Hugo, 2026-11-22): the
# subgroup lists of inst/rules/usda/subgroups/{ultisols,oxisols}.yaml are the
# lists of the KST, in its order, and their tests read the criteria of each
# subgroup. The tests here are new; the helpers that other orders share are
# called, never changed.
#
# Three-valued like the rest of the key: TRUE or FALSE where the data
# recorded decide it, NA (with the missing data listed) where they do not.
# A field observation not recorded is absent (redox features, plinthite,
# slickensides, cracks, lamellae, fragic properties, a discontinuous contact,
# skeletans); a measurement not recorded is undecided (colour, texture, the
# linear extensibility, the base saturation, the CEC, the oxalate
# extractions, the bulk density, the days of saturation, a regime).
# ================================================================


# ---- Shared readings ------------------------------------------------------

.SGUX_OX <- "Soil Survey Staff (2022), KST 13th ed., Oxisols, Ch. 13"
.SGUX_UL <- "Soil Survey Staff (2022), KST 13th ed., Ultisols, Ch. 15"

# A DiagnosticResult; the missing data are listed only when undecided.
.sgux_res <- function(name, passed, layers = integer(0), missing = character(0),
                      evidence = list(), reference = .SGUX_UL) {
  passed <- if (length(passed) != 1L) NA else as.logical(passed)
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) as.integer(unique(layers)) else integer(0),
    evidence = evidence,
    missing = if (is.na(passed)) unique(as.character(missing)) else character(0),
    reference = reference)
}

.sgux_ms <- function(pedon) {
  ms <- .q_mineral_surface_cm(pedon$horizons)
  if (is.na(ms)) 0 else ms
}

# Layers overlapping [lo, hi) cm (absolute depths).
.sgux_win <- function(h, lo, hi) {
  which(!is.na(h$top_cm) & h$top_cm < hi & (is.na(h$bottom_cm) | h$bottom_cm > lo))
}

# The deepest depth described (cm), -Inf with none.
.sgux_bottom <- function(h) suppressWarnings(max(c(h$bottom_cm, h$top_cm), na.rm = TRUE))

# Does the record reach `depth` (cm below the mineral soil surface), or stop
# at a contact above it?
.sgux_reaches <- function(pedon, depth) {
  h <- pedon$horizons
  if (.sgux_bottom(h) >= depth - 1e-6) return(TRUE)
  ct <- .kst_contact_top_cm(pedon, petroferric = TRUE)
  !is.na(ct) && ct <= .sgux_bottom(h) + 1e-6
}

.sgux_master <- function(pedon) {
  h <- pedon$horizons
  .desg_master(as.character(h$designation %||% rep(NA_character_, nrow(h))), site = pedon$site)
}

# The epipedon read as the A horizons at the mineral soil surface (with an O
# above them); without an A designation, the layer at the mineral soil
# surface. Indices in depth order.
.sgux_epipedon_layers <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  m <- .sgux_master(pedon)
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= ms - 1e-6]
  out <- integer(0)
  for (i in ord) {
    if (!is.na(m[i]) && grepl("^A", m[i])) out <- c(out, i) else break
  }
  if (!length(out) && length(ord)) out <- ord[1L]
  out
}

# Munsell notations recorded as mottle colours of layer i (a data frame with
# value and chroma; no rows when none).
.sgux_mottles <- function(h, i) {
  mc <- .usda_col(h, "mottle_colors", NA_character_)[i]
  if (is.na(mc) || !nzchar(trimws(mc))) return(.munsell_notations(NA_character_))
  .munsell_notations(as.character(mc))
}

# Any mottles or redox features recorded in layer i (a field observation:
# not recorded is absent).
.sgux_features <- function(h, i) {
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)[i]
  mp <- .usda_col(h, "mottles_present", NA)[i]
  mc <- .usda_col(h, "mottle_colors", NA_character_)[i]
  nod <- .usda_col(h, "fe_mn_nodules_pct", NA_real_)[i]
  (!is.na(rdx) && rdx > 0) || isTRUE(as.logical(mp)) ||
    (!is.na(mc) && nzchar(trimws(mc))) || (!is.na(nod) && nod > 0)
}

# Redox concentrations recorded in layer i: mottles or Fe-Mn nodules, unless
# every mottle colour recorded is a depletion colour (value 4 or more,
# chroma 2 or less) and no nodules are recorded. TRUE / FALSE.
.sgux_conc <- function(h, i) {
  if (!.sgux_features(h, i)) return(FALSE)
  nod <- .usda_col(h, "fe_mn_nodules_pct", NA_real_)[i]
  if (!is.na(nod) && nod > 0) return(TRUE)
  mot <- .sgux_mottles(h, i)
  mot <- mot[!is.na(mot$value) & !is.na(mot$chroma), , drop = FALSE]
  if (nrow(mot) && all(mot$value >= 4 & mot$chroma <= 2)) return(FALSE)
  TRUE
}

# Redox depletions with a moist value of `vmin` or more (any value when NA)
# and chroma of `cmax` or less in layer i, as evidence of aquic conditions:
# a mottle colour so recorded, or such a matrix colour where redox features
# or a g designation show that the matrix is depleted (KST 13th ed., pp.
# 41-44: depletions "on faces of peds or in the matrix"). Redox features are
# field observations (not recorded, absent); the matrix colour is not
# (not recorded, undecided). TRUE / FALSE / NA.
.sgux_depl <- function(h, i, vmin = 4, cmax = 2) {
  vok <- function(v) if (is.na(vmin)) rep(TRUE, length(v)) else v >= vmin
  mot <- .sgux_mottles(h, i)
  mot <- mot[!is.na(mot$value) & !is.na(mot$chroma), , drop = FALSE]
  if (nrow(mot) && any(vok(mot$value) & mot$chroma <= cmax)) return(TRUE)
  d <- .usda_desig(h$designation[i])
  gley <- !is.na(d$suffix) && grepl("g", d$suffix)
  if (!.sgux_features(h, i) && !gley) return(FALSE)
  vm <- h$munsell_value_moist[i]; cm <- h$munsell_chroma_moist[i]
  if (is.na(cm) || (!is.na(vmin) && is.na(vm))) return(NA)
  if (cm <= cmax && vok(vm)) return(TRUE)
  # mottles recorded without a readable colour could be depletions
  rdx <- .usda_col(h, "redoximorphic_features_pct", NA_real_)[i]
  if (!is.na(rdx) && rdx > 0 && nrow(mot) == 0L) return(NA)
  FALSE
}

# The redox test of a layer: depletions, with redox concentrations where
# `concentrations`; or, `either`, concentrations or depletions (Aquic
# Hapludults, p. 330).
.sgux_redox_layer <- function(h, i, concentrations = FALSE, vmin = 4, cmax = 2,
                              either = FALSE) {
  dp <- .sgux_depl(h, i, vmin = vmin, cmax = cmax)
  if (either) return(.usda_or3(.sgux_conc(h, i), dp))
  if (concentrations) .and3(dp, .sgux_conc(h, i)) else dp
}

# The argillic or kandic horizon a subgroup names: "argillic", "kandic" or
# "either" (the argillic, or the kandic where the argillic is not present).
.sgux_horizon <- function(pedon, horizon = c("argillic", "kandic", "either")) {
  horizon <- match.arg(horizon)
  if (horizon == "argillic") return(argillic_usda(pedon))
  if (horizon == "kandic") return(kandic_horizon_usda(pedon))
  .pale_horizon_usda(pedon, kandic = TRUE)
}

# Is the horizon's upper boundary known? Its top (cm), NA when the horizon is
# not present.
.sgux_horizon_top <- function(pedon, hz) {
  if (!isTRUE(hz$passed) || !length(hz$layers)) return(NA_real_)
  min(pedon$horizons$top_cm[hz$layers], na.rm = TRUE)
}


# ---- Contacts ----------------------------------------------------------------

# Top (cm) of the shallowest lithic contact (an R layer, continuous rock) or
# petroferric contact (an indurated ironstone layer, a petroplinthic layer)
# recorded; NA when none. Field observations.
.sgux_contact_top <- function(pedon, kind = c("lithic", "petroferric")) {
  kind <- match.arg(kind)
  h <- pedon$horizons
  tops <- numeric(0)
  if (kind == "lithic") {
    d <- .usda_desig(h$designation)
    tops <- c(tops, h$top_cm[!is.na(d$master) & d$master == "R"])
    rk <- .usda_try(continuous_rock(pedon))
    if (isTRUE(.usda_pv(rk))) {
      L <- rk$layers[!is.na(d$master[rk$layers]) & !grepl("^Cr", d$core[rk$layers])]
      tops <- c(tops, h$top_cm[L])
    }
  } else {
    pf <- .usda_try(petroferric_contact_usda(pedon, max_top_cm = Inf))
    if (isTRUE(.usda_pv(pf))) tops <- c(tops, h$top_cm[pf$layers])
    pp <- .usda_try(petroplinthic(pedon))
    if (isTRUE(.usda_pv(pp))) tops <- c(tops, h$top_cm[pp$layers])
  }
  tops <- tops[!is.na(tops)]
  if (length(tops)) min(tops) else NA_real_
}

.sgux_contact <- function(pedon, kind, max_cm) {
  t <- .sgux_contact_top(pedon, kind)
  !is.na(t) && t < .sgux_ms(pedon) + max_cm + 1e-6
}


# ---- Oxisols (KST 13th ed., pp. 295-309) -------------------------------------

# Redox depletions with a moist value of 4 or more and chroma of 2 or less,
# and aquic conditions, in one or more horizons within 125 cm of the mineral
# soil surface (the Aquic subgroups of the Oxisols). TRUE / FALSE / NA.
.sgux_oxisol_aquic <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  L <- .sgux_win(h, ms, ms + 125)
  ok <- vapply(L, function(i) .sgux_depl(h, i), NA)
  list(passed = .usda_or3(ok), layers = L[ok %in% TRUE])
}

#' Aquic subgroups of the Oxisols (KST 13th ed., pp. 296-309)
#'
#' Within 125 cm of the mineral soil surface, redox depletions with a moist
#' value of 4 or more and chroma of 2 or less and also aquic conditions
#' (Aquic Acroperox, Aquic Hapludox ...). With \code{contact}, a lithic or
#' petroferric contact within 125 cm as well (Aquic Lithic, Aquic
#' Petroferric). Redox features are field observations (absent when not
#' recorded); a depleted matrix needs its colour.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param contact \code{NULL}, \code{"lithic"} or \code{"petroferric"}.
#' @noRd
sg_oxisol_aquic_usda <- function(pedon, contact = NULL) {
  aq <- .sgux_oxisol_aquic(pedon)
  p <- aq$passed
  if (!is.null(contact)) p <- .and3(.sgux_contact(pedon, contact, 125), p)
  .sgux_res("sg_oxisol_aquic_usda", p, aq$layers,
            c("munsell_value_moist", "munsell_chroma_moist", "mottle_colors"),
            list(contact = contact), .SGUX_OX)
}

#' Lithic and Petroferric subgroups (Oxisols: within 125 cm, pp. 296-309;
#' Ultisols: lithic within 50 cm, petroferric within 100 cm in the
#' Haplustults, pp. 326-341). A contact is a field observation: an R layer or
#' continuous rock (lithic), an indurated ironstone or petroplinthic layer
#' (petroferric); absent when not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind \code{"lithic"} or \code{"petroferric"}.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_ultox_contact_usda <- function(pedon, kind = "lithic", max_cm = 125) {
  t <- .sgux_contact_top(pedon, kind)
  .sgux_res("sg_ultox_contact_usda", .sgux_contact(pedon, kind, max_cm),
            which(abs(pedon$horizons$top_cm - t) < 1e-6),
            evidence = list(kind = kind, top_cm = t, max_cm = max_cm),
            reference = "Soil Survey Staff (2022), KST 13th ed., Lithic and Petroferric subgroups (pp. 296-341)")
}

#' Plinthic subgroups (Oxisols, Ultisols)
#'
#' 5 percent or more (by volume) plinthite in one or more horizons within
#' \code{max_cm} of the mineral soil surface (125 cm in the Oxisols, 150 cm in
#' the Ultisols; 5 to 50 percent in the Plinthic Umbraquults, p. 325).
#' Plinthite is a field observation: absent when not recorded. With
#' \code{aquic} the Oxisol aquic test as well (Plinthaquic Oxisols), or the
#' Ultisol one (\code{"ultisol75"}: depletions with redox concentrations
#' within 75 cm; \code{"ultisol40"}: depletions of chroma 2 or less within
#' 40 cm, Plinthaquic Fragiudults).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @param max_pct Upper limit of the plinthite (50 in the Umbraquults).
#' @param aquic \code{NULL}, \code{"oxisol"}, \code{"ultisol75"} or \code{"ultisol40"}.
#' @noRd
sg_ultox_plinthic_usda <- function(pedon, max_cm = 150, max_pct = 100, aquic = NULL) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  L <- .sgux_win(h, ms, ms + max_cm)
  pp <- .usda_col(h, "plinthite_pct", NA_real_)[L]
  hit <- L[!is.na(pp) & pp >= 5 & pp <= max_pct]
  p <- length(hit) > 0L
  miss <- character(0)
  if (!is.null(aquic) && p) {
    a <- switch(aquic,
      oxisol = .sgux_oxisol_aquic(pedon)$passed,
      ultisol75 = sg_ultisol_aquic_usda(pedon, zone = "75", concentrations = TRUE)$passed,
      ultisol40 = sg_ultisol_aquic_usda(pedon, zone = "40", concentrations = FALSE, vmin = NA)$passed)
    p <- .and3(p, a)
    miss <- c("munsell_value_moist", "munsell_chroma_moist", "mottle_colors")
  }
  .sgux_res("sg_ultox_plinthic_usda", p, hit, miss,
            list(max_cm = max_cm, max_pct = max_pct, aquic = aquic),
            "Soil Survey Staff (2022), KST 13th ed., Plinthic subgroups (pp. 295-341)")
}

#' Aeric subgroups of the Aquox (KST 13th ed., pp. 295-296)
#'
#' Directly below an epipedon, a horizon 10 cm or more thick with 50 percent
#' or more chroma of 3 or more (the moist matrix colour). The epipedon is read
#' as the A horizons at the mineral soil surface. A chroma not recorded
#' leaves the test undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_oxisol_aeric_usda <- function(pedon) {
  h <- pedon$horizons
  epi <- .sgux_epipedon_layers(pedon)
  ref <- "Soil Survey Staff (2022), KST 13th ed., Aeric Aquox (pp. 295-296)"
  if (!length(epi)) return(.sgux_res("sg_oxisol_aeric_usda", NA, missing = "designation", reference = ref))
  z <- max(h$bottom_cm[epi])
  if (is.na(z)) return(.sgux_res("sg_oxisol_aeric_usda", NA, missing = "bottom_cm", reference = ref))
  ord <- order(h$top_cm)
  below <- ord[!is.na(h$top_cm[ord]) & h$top_cm[ord] >= z - 1e-6]
  thk <- 0; p <- FALSE; L <- integer(0)
  for (i in below) {
    ch <- h$munsell_chroma_moist[i]
    if (is.na(ch)) { p <- NA; break }
    if (ch < 3) break
    L <- c(L, i)
    thk <- thk + max(0, h$bottom_cm[i] - h$top_cm[i], na.rm = TRUE)
    if (thk >= 10) { p <- TRUE; break }
  }
  .sgux_res("sg_oxisol_aeric_usda", p, L, "munsell_chroma_moist",
            list(epipedon_bottom_cm = z, thickness_cm = thk), ref)
}

#' Xanthic subgroups of the Oxisols (KST 13th ed., pp. 297-309)
#'
#' 50 percent or more hue of 7.5YR or yellower and a moist value of 6 or more
#' at a depth between 25 and 125 cm of the mineral soil surface, read as 50
#' percent or more of the thickness of the layers between those depths (the
#' moist matrix colour of each layer). TRUE when the share already reached
#' is half, FALSE when even the layers without a colour could not make it
#' half, NA otherwise.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_oxisol_xanthic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  lo <- ms + 25; hi <- ms + 125
  L <- .sgux_win(h, lo, hi)
  ov <- pmax(0, pmin(h$bottom_cm[L], hi) - pmax(h$top_cm[L], lo)); ov[is.na(ov)] <- 0
  hr <- .usda_hue_rank(h$munsell_hue_moist[L])
  neutral <- !is.na(h$munsell_hue_moist[L]) & grepl("^N", toupper(trimws(h$munsell_hue_moist[L])))
  vm <- h$munsell_value_moist[L]
  ok <- ifelse(neutral | (!is.na(hr) & hr < 17.5) | (!is.na(vm) & vm < 6), FALSE,
               ifelse(is.na(hr) | is.na(vm), NA, TRUE))
  tot <- hi - lo
  yes <- sum(ov[ok %in% TRUE]); may <- yes + sum(ov[is.na(ok)]) + max(0, tot - sum(ov))
  p <- if (!length(L)) NA else if (yes >= tot / 2) TRUE else if (may < tot / 2) FALSE else NA
  .sgux_res("sg_oxisol_xanthic_usda", p, L[ok %in% TRUE],
            c("munsell_hue_moist", "munsell_value_moist", if (sum(ov) < tot) "bottom_cm"),
            list(share_cm = yes, window_cm = tot), .SGUX_OX)
}

#' Anionic subgroups of the Oxisols (KST 13th ed., pp. 296-305)
#'
#' A delta pH (KCl pH minus 1:1 water pH) of 0 or more (0 or net positive
#' charge) in a layer 18 cm or more thick (adjoining layers) within 125 cm of
#' the mineral soil surface. With \code{aquic}, the Oxisol aquic test as well
#' (Anionic Aquic). A pH not recorded leaves a layer undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param aquic Logical.
#' @noRd
sg_oxisol_anionic_usda <- function(pedon, aquic = FALSE) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  hi <- ms + 125
  pw <- .usda_col(h, "ph_h2o_1to1", NA_real_)
  pw <- ifelse(is.na(pw), h$ph_h2o, pw)
  d <- .kst_round(h$ph_kcl - pw, 1)
  win <- !is.na(h$top_cm) & h$top_cm < hi & (is.na(h$bottom_cm) | h$bottom_cm > ms)
  lay <- ifelse(!win, FALSE, ifelse(is.na(d), NA, d >= 0))
  ord <- order(h$top_cm)
  thick <- function(r) sum(pmax(0, pmin(h$bottom_cm[r], hi) - pmax(h$top_cm[r], ms)), na.rm = TRUE)
  yes <- Filter(function(r) thick(r) >= 18, .usda_runs(lay %in% TRUE, ord))
  may <- Filter(function(r) thick(r) >= 18, .usda_runs(!(lay %in% FALSE), ord))
  p <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  if (isTRUE(aquic)) p <- .and3(p, .sgux_oxisol_aquic(pedon)$passed)
  .sgux_res("sg_oxisol_anionic_usda", p, unlist(yes),
            c("ph_kcl", "ph_h2o", if (isTRUE(aquic)) c("munsell_chroma_moist", "mottle_colors")),
            list(delta_ph = d, aquic = aquic), .SGUX_OX)
}

#' Eutric subgroups of the Acrudox and Acrustox (KST 13th ed., pp. 301, 306)
#'
#' A base saturation (NH4OAc) of 35 percent or more in all horizons within
#' 125 cm of the mineral soil surface. FALSE where one layer recorded has
#' less; undecided where a layer has none recorded or the profile ends above
#' 125 cm without a contact.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_oxisol_eutric_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  L <- .sgux_win(h, ms, ms + 125)
  bs <- h$bs_pct[L]
  ok <- ifelse(is.na(bs), NA, .kst_round(bs) >= 35)
  p <- .usda_all3(ok)
  if (!length(L)) p <- NA
  if (isTRUE(p) && !.sgux_reaches(pedon, ms + 125)) p <- NA
  .sgux_res("sg_oxisol_eutric_usda", p, L, c("bs_pct", "bottom_cm"),
            list(bs_pct = bs), .SGUX_OX)
}

#' Inceptic and Aqueptic subgroups of the Oxisols (KST 13th ed., pp. 298-308)
#'
#' An oxic horizon whose lower boundary is within 125 cm of the mineral soil
#' surface: TRUE where a layer or a contact below the oxic horizon is
#' recorded above 125 cm, FALSE where the oxic horizon reaches 125 cm or no
#' oxic horizon is present, NA where the profile ends inside the oxic horizon
#' above 125 cm. With \code{humic} the 16 kg/m2 of organic carbon as well
#' (Humic Inceptic); with \code{aquic} the Oxisol aquic test (Aqueptic
#' Haplustox).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param humic,aquic Logical.
#' @noRd
sg_oxisol_inceptic_usda <- function(pedon, humic = FALSE, aquic = FALSE) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  ox <- oxic_usda(pedon)
  p <- if (isTRUE(ox$passed) && length(ox$layers)) {
    b <- max(h$bottom_cm[ox$layers])
    if (is.na(b)) NA
    else if (b >= ms + 125 - 1e-6) FALSE
    else if (.sgux_bottom(h) > b + 1e-6) TRUE
    else {
      ct <- .kst_contact_top_cm(pedon, petroferric = TRUE)
      if (!is.na(ct) && abs(ct - b) < 1e-6) TRUE else NA
    }
  } else ox$passed
  miss <- c(if (is.na(ox$passed)) ox$missing, "bottom_cm")
  if (isTRUE(humic)) {
    hu <- humic_oxisol_usda(pedon)
    p <- .and3(p, hu$passed); miss <- c(miss, hu$missing)
  }
  if (isTRUE(aquic)) {
    p <- .and3(p, .sgux_oxisol_aquic(pedon)$passed)
    miss <- c(miss, "munsell_chroma_moist", "mottle_colors")
  }
  .sgux_res("sg_oxisol_inceptic_usda", p, ox$layers, miss,
            list(humic = humic, aquic = aquic), .SGUX_OX)
}

#' Humic Rhodic and Humic Xanthic subgroups of the Oxisols (KST 13th ed.,
#' pp. 297-309): 16 kg/m2 or more organic carbon to 100 cm
#' (\code{humic_oxisol_usda()}) and the Rhodic (\code{rhodic_subgroup_usda(kst
#' = "oxisol")}) or Xanthic (\code{sg_oxisol_xanthic_usda()}) colours.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param colour \code{"rhodic"} or \code{"xanthic"}.
#' @noRd
sg_oxisol_humic_colour_usda <- function(pedon, colour = c("rhodic", "xanthic")) {
  colour <- match.arg(colour)
  hu <- humic_oxisol_usda(pedon)
  co <- if (colour == "rhodic") rhodic_subgroup_usda(pedon, kst = "oxisol") else sg_oxisol_xanthic_usda(pedon)
  .sgux_res("sg_oxisol_humic_colour_usda", .and3(hu$passed, co$passed), co$layers,
            c(hu$missing, co$missing), list(colour = colour), .SGUX_OX)
}

#' Kandiudalfic and Kandiustalfic subgroups of the Eutr- Oxisols (KST 13th
#' ed., pp. 297, 302, 306): a kandic horizon within 150 cm of the mineral soil
#' surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_oxisol_kandic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  ka <- kandic_horizon_usda(pedon)
  p <- if (isTRUE(ka$passed)) any(h$top_cm[ka$layers] < ms + 150) else ka$passed
  .sgux_res("sg_oxisol_kandic_usda", p, ka$layers, ka$missing, list(), .SGUX_OX)
}


# ---- Ultisols (KST 13th ed., pp. 321-341) ------------------------------------

#' Aquic subgroups of the Ultisols (KST 13th ed., pp. 326-341)
#'
#' Redox depletions with a moist value of 4 or more (\code{vmin}; NA for "any
#' value") and chroma of 2 or less, accompanied by redox concentrations where
#' \code{concentrations}, and aquic conditions, in the zone the great group
#' names:
#' \describe{
#'   \item{\code{"arg25"}}{subhorizons within the upper 25 cm of the argillic
#'     or kandic horizon (Humults, Xerults);}
#'   \item{\code{"75"}}{layers within 75 cm of the mineral soil surface
#'     (Kandi-, Kanhapl-, Pale- Udults and Ustults);}
#'   \item{\code{"75_uncoated"}}{the same, or, if the chroma throughout the
#'     upper 75 cm results from uncoated sand grains, the upper 12.5 cm of the
#'     argillic or kandic horizon (Arenic Plinthaquic and Aquic Arenic
#'     Kandiudults and Paleudults);}
#'   \item{\code{"arg12_75"}}{layers within both the upper 12.5 cm of the
#'     argillic horizon and 75 cm (Aquic Haplustults);}
#'   \item{\code{"100"}}{layers within 100 cm, with redox concentrations or
#'     depletions (\code{either}; Aquic Hapludults);}
#'   \item{\code{"40"}}{layers within 40 cm (Plinthaquic and Glossaquic
#'     Fragiudults);}
#'   \item{\code{"fragipan_arg25"}}{subhorizons above the fragipan and within
#'     the upper 25 cm of the argillic or kandic horizon (Aquic Fragiudults).}
#' }
#' Redox features are field observations (absent when not recorded); a
#' depleted matrix needs its colour; aquic conditions are read from the
#' redox features themselves.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param zone See above.
#' @param concentrations Logical.
#' @param vmin Minimum moist value of the depletions (NA: any).
#' @param either Logical: concentrations or depletions.
#' @param horizon The horizon of the zones that name one.
#' @noRd
sg_ultisol_aquic_usda <- function(pedon, zone = "75", concentrations = TRUE, vmin = 4,
                             either = FALSE, horizon = "either") {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  lay <- function(L) vapply(L, function(i) .sgux_redox_layer(h, i, concentrations, vmin, 2, either), NA)
  any_feature <- any(vapply(seq_len(nrow(h)), function(i) .sgux_features(h, i) ||
                             isTRUE(grepl("g", .usda_desig(h$designation[i])$suffix)), NA))
  hz <- NULL
  if (zone %in% c("75", "100", "40")) {
    L <- .sgux_win(h, ms, ms + as.numeric(zone))
    p <- .usda_or3(lay(L))
  } else {
    hz <- .sgux_horizon(pedon, horizon)
    t0 <- .sgux_horizon_top(pedon, hz)
    if (zone == "75_uncoated") {
      L <- .sgux_win(h, ms, ms + 75)
      ug <- .usda_col(h, "uncoated_grains_pct", NA_real_)[L]
      unc <- length(L) && all(!is.na(ug) & ug > 0)
      p1 <- .usda_or3(lay(L))
      p2 <- if (!unc) FALSE
            else if (is.na(t0)) (if (isFALSE(hz$passed)) FALSE else NA)
            else .usda_or3(lay(.sgux_win(h, t0, t0 + 12.5)))
      p <- .usda_or3(p1, p2)
    } else if (is.na(t0)) {
      p <- if (isFALSE(hz$passed) || !any_feature) FALSE else NA
      L <- integer(0)
    } else {
      L <- switch(zone,
        arg25 = .sgux_win(h, t0, t0 + 25),
        arg12_75 = .sgux_win(h, t0, min(t0 + 12.5, ms + 75)),
        fragipan_arg25 = {
          fp <- fragipan_usda(pedon, max_top_cm = Inf)
          ftop <- if (isTRUE(fp$passed) && length(fp$layers)) min(h$top_cm[fp$layers]) else Inf
          .sgux_win(h, t0, min(t0 + 25, ftop))
        })
      p <- .usda_or3(lay(L))
    }
  }
  if (!any_feature) p <- FALSE
  .sgux_res("sg_ultisol_aquic_usda", p, L,
            c("munsell_value_moist", "munsell_chroma_moist", "mottle_colors",
              if (!is.null(hz) && is.na(hz$passed)) hz$missing),
            list(zone = zone, concentrations = concentrations, either = either), .SGUX_UL)
}

#' Vertic subgroups of the Ultisols (KST 13th ed., pp. 322-334)
#'
#' One or both of: (1) cracks within 125 cm 5 mm or more wide through 30 cm
#' or more and slickensides or wedge-shaped peds in a layer 15 cm or more
#' thick whose upper boundary is within 125 cm; (2) a linear extensibility of
#' 6.0 cm or more between the mineral soil surface and 100 cm or a densic,
#' lithic or paralithic contact. Slickensides, wedge-shaped peds and cracks
#' are field observations (absent when not recorded; where no cracks are
#' recorded at all the slickensides stand for them, as in the Vertisol order
#' since v0.9.421, reading L30). The linear extensibility (sum of COLE x
#' thickness) is a measurement: not recorded, undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_vertic_usda <- function(pedon)
  .usda_vertic_field_reading(.sg_ultisol_vertic_usda_record(pedon), pedon)

# The test as the record gives it; the wrapper above reads a COLE not
# recorded from the field record (R/subgroups-usda-vertic-field.R).
.sg_ultisol_vertic_usda_record <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  ord <- order(h$top_cm)
  ss <- tolower(trimws(as.character(.usda_col(h, "slickensides", NA_character_))))
  st <- tolower(as.character(.usda_col(h, "structure_type", NA_character_)))
  sl <- (!is.na(ss) & nzchar(ss) & !ss %in% c("absent", "none", "no", "0", "false")) |
        (!is.na(st) & grepl("wedge", st))
  sl <- sl & !is.na(h$top_cm) & h$top_cm < ms + 125
  runs <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(sl, ord))
  cw <- .usda_col(h, "cracks_width_cm", NA_real_); cd <- .usda_col(h, "cracks_depth_cm", NA_real_)
  ct <- .usda_col(h, "cracks_top_cm", NA_real_)
  cr_rec <- any(!is.na(cw) | !is.na(cd))
  cracks <- any(!is.na(cw) & cw >= 0.5 & !is.na(cd) & cd >= 30 & (is.na(ct) | ct < ms + 125))
  b1 <- length(runs) > 0L && (cracks || !cr_rec)
  ctc <- .kst_contact_top_cm(pedon, petroferric = FALSE)
  hi <- min(ms + 100, if (is.na(ctc)) Inf else ctc)
  L <- .sgux_win(h, ms, hi)
  dz <- pmax(0, pmin(h$bottom_cm[L], hi) - pmax(h$top_cm[L], ms)); dz[is.na(dz)] <- 0
  cole <- .usda_col(h, "cole_value", NA_real_)[L]
  le <- sum((cole * dz)[!is.na(cole)])
  b2 <- if (.kst_round(le, 1) >= 6) TRUE
        else if (anyNA(cole[dz > 0]) || !length(L) || !.sgux_reaches(pedon, hi)) NA else FALSE
  p <- .usda_or3(b1, b2)
  .sgux_res("sg_ultisol_vertic_usda", p, unlist(runs), c("cole_value"),
            list(slickensides_cm = sum(vapply(runs, function(r) .usda_run_thickness(h, r), 0)),
                 linear_extensibility_cm = le),
            "Soil Survey Staff (2022), KST 13th ed., Vertic subgroups of the Ultisols (pp. 322-334)")
}

#' Arenic and Grossarenic subgroups of the Ultisols (KST 13th ed., pp.
#' 322-341)
#'
#' A texture class (fine earth) of coarse sand, sand, fine sand, loamy coarse
#' sand, loamy sand or loamy fine sand throughout a layer extending from the
#' mineral soil surface to the top of the argillic (or kandic, or either)
#' horizon, at a depth from \code{min_cm} to \code{max_cm} (Arenic 50 to 100,
#' Grossarenic 100 or more, "50 cm or more" in some great groups). The
#' Ultisol definition (audit F16), not the Spodosol one. The texture is the
#' laboratory texture or, where it is not measured, the field texture class
#' (\code{.usda_texture_grades()}); not recorded, undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon \code{"argillic"}, \code{"kandic"} or \code{"either"}.
#' @param min_cm,max_cm Depth range of the top of the horizon.
#' @noRd
sg_ultisol_arenic_usda <- function(pedon, horizon = "argillic", min_cm = 50, max_cm = 100) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  g <- .usda_texture_grades(h)$lfs_or_coarser
  hz <- .sgux_horizon(pedon, horizon)
  t0 <- .sgux_horizon_top(pedon, hz)
  sandy_to <- function(z) {
    L <- .sgux_win(h, ms, z)
    if (!length(L)) return(NA)
    .usda_all3(g[L])
  }
  p <- if (!is.na(t0)) {
    d <- t0 - ms
    if (d < min_cm - 1e-6 || d > max_cm + 1e-6) FALSE else sandy_to(t0)
  } else if (isFALSE(hz$passed)) FALSE
  else if (isFALSE(sandy_to(ms + min_cm))) FALSE else NA
  .sgux_res("sg_ultisol_arenic_usda", p, .sgux_win(h, ms, if (is.na(t0)) ms else t0),
            c("clay_pct", "sand_pct", "texture_class_usda", if (is.na(hz$passed)) hz$missing),
            list(horizon = horizon, top_cm = t0, min_cm = min_cm, max_cm = max_cm),
            "Soil Survey Staff (2022), KST 13th ed., Arenic and Grossarenic Ultisols (pp. 322-341)")
}

#' Aeric subgroups of the Aquults (KST 13th ed., pp. 322-324)
#'
#' 50 percent or more chroma of 3 or more (the moist matrix colour) in one or
#' more horizons between the A or Ap horizon or 25 cm, whichever is deeper,
#' and 75 cm (\code{to = "75"}) or the fragipan (\code{to = "fragipan"},
#' Aeric Fragiaquults). A chroma not recorded leaves the test undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param to \code{"75"} or \code{"fragipan"}.
#' @noRd
sg_ultisol_aeric_usda <- function(pedon, to = "75") {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  epi <- .sgux_epipedon_layers(pedon)
  m <- .sgux_master(pedon)
  aa <- epi[!is.na(m[epi]) & grepl("^A", m[epi])]
  lo <- max(ms + 25, if (length(aa)) max(h$bottom_cm[aa], na.rm = TRUE) else -Inf)
  hi <- if (to == "fragipan") {
    fp <- fragipan_usda(pedon, max_top_cm = Inf)
    if (isTRUE(fp$passed) && length(fp$layers)) min(h$top_cm[fp$layers]) else ms + 100
  } else ms + 75
  L <- .sgux_win(h, lo, hi)
  ch <- h$munsell_chroma_moist[L]
  p <- if (!length(L)) (if (.sgux_bottom(h) > lo) FALSE else NA)
       else .usda_or3(ifelse(is.na(ch), NA, ch >= 3))
  .sgux_res("sg_ultisol_aeric_usda", p, L[!is.na(ch) & ch >= 3], c("munsell_chroma_moist", "bottom_cm"),
            list(from_cm = lo, to_cm = hi),
            "Soil Survey Staff (2022), KST 13th ed., Aeric Aquults (pp. 322-324)")
}

#' Umbric subgroups of the Aquults (KST 13th ed., pp. 323-325): a mollic or
#' umbric epipedon. With \code{aeric}, the Aeric test as well (Aeric Umbric
#' Kanhaplaquults).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param aeric Logical.
#' @noRd
sg_ultisol_umbric_usda <- function(pedon, aeric = FALSE) {
  mo <- .usda_try(mollic_epipedon_usda(pedon)); um <- .usda_try(umbric_epipedon_usda(pedon))
  p <- .usda_or3(.usda_pv(mo), .usda_pv(um))
  miss <- c(mo$missing, um$missing)
  if (isTRUE(aeric)) {
    ae <- sg_ultisol_aeric_usda(pedon)
    p <- .and3(p, ae$passed); miss <- c(miss, ae$missing)
  }
  .sgux_res("sg_ultisol_umbric_usda", p, c(if (isTRUE(.usda_pv(mo))) mo$layers, if (isTRUE(.usda_pv(um))) um$layers),
            miss, list(aeric = aeric),
            "Soil Survey Staff (2022), KST 13th ed., Umbric Aquults (pp. 323-325)")
}

#' Kandic and Kanhaplic subgroups (KST 13th ed., pp. 322, 325, 337)
#'
#' \code{"kandic"}: a kandic horizon (Kandic Albaquults). \code{"cec"}: a CEC
#' (NH4OAc, pH 7) of less than 24 cmol(+)/kg clay in 50 percent or more (by
#' thickness) of the argillic horizon, or of its upper 100 cm (Kanhaplic
#' Haplustults). \code{"kandic_or_cec"}: either (Kandic Plinthaquults).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param what See above.
#' @noRd
sg_ultisol_kandic_usda <- function(pedon, what = "kandic") {
  h <- pedon$horizons
  ka <- if (what != "cec") kandic_horizon_usda(pedon)
  cec <- if (what != "kandic") {
    ar <- argillic_usda(pedon)
    if (isTRUE(ar$passed) && length(ar$layers)) {
      t0 <- min(h$top_cm[ar$layers])
      L <- ar$layers
      dz <- pmax(0, pmin(h$bottom_cm[L], t0 + 100) - h$top_cm[L]); dz[is.na(dz)] <- 0
      r <- 100 * h$cec_cmol[L] / h$clay_pct[L]
      ok <- ifelse(is.na(r), NA, .kst_round(r) < 24)
      tot <- sum(dz)
      yes <- sum(dz[ok %in% TRUE]); no <- sum(dz[ok %in% FALSE])
      if (tot <= 0) NA else if (yes >= tot / 2) TRUE else if (no > tot / 2) FALSE else NA
    } else ar$passed
  }
  p <- switch(what, kandic = ka$passed, cec = cec, .usda_or3(ka$passed, cec))
  .sgux_res("sg_ultisol_kandic_usda", p, ka$layers, c(ka$missing, if (what != "kandic") c("cec_cmol", "clay_pct")),
            list(what = what, cec_test = cec), .SGUX_UL)
}

#' Fragic subgroups of the Ultisols (KST 13th ed., pp. 322-336)
#'
#' Fragic soil properties in 30 percent or more of the volume of a layer 15
#' cm or more thick whose upper boundary is within 100 cm, or in 60 percent
#' or more of a layer 15 cm or more thick. Fragic properties are a field
#' observation: a horizon designated x (Btx, Bx, Ex), or a fragipan recorded
#' by the surveyor (NASIS), counts as fragic throughout; not recorded,
#' absent. With \code{aquic}, depletions with redox concentrations within 75
#' cm as well (Fragiaquic); with \code{aeric}, the Aeric test (Aeric Fragic
#' Epiaquults).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param aquic,aeric Logical.
#' @noRd
sg_ultisol_fragic_usda <- function(pedon, aquic = FALSE, aeric = FALSE) {
  h <- pedon$horizons
  d <- .usda_desig(h$designation)
  x <- !is.na(d$suffix) & grepl("x", d$suffix)
  runs <- Filter(function(r) .usda_run_thickness(h, r) >= 15, .usda_runs(x, order(h$top_cm)))
  p <- length(runs) > 0L || isTRUE(.has_nasis_feature(pedon, "fragipan"))
  miss <- character(0)
  if (isTRUE(aquic)) {
    a <- sg_ultisol_aquic_usda(pedon, zone = "75", concentrations = TRUE)
    p <- .and3(p, a$passed); miss <- c(miss, a$missing)
  }
  if (isTRUE(aeric)) {
    a <- sg_ultisol_aeric_usda(pedon)
    p <- .and3(p, a$passed); miss <- c(miss, a$missing)
  }
  .sgux_res("sg_ultisol_fragic_usda", p, unlist(runs), miss, list(aquic = aquic, aeric = aeric),
            "Soil Survey Staff (2022), KST 13th ed., Fragic subgroups of the Ultisols (pp. 322-336)")
}

#' Andic and Aquandic subgroups of the Ultisols (KST 13th ed., pp. 324-341)
#'
#' Throughout one or more horizons with a total thickness of 18 cm or more
#' within 75 cm of the mineral soil surface: the andic criterion (bulk density
#' 1.0 g/cm3 or less at 33 kPa and Al + 1/2 Fe by oxalate over 1.0 percent),
#' or, with \code{vitrandic}, also the cinders and volcanic glass criteria
#' (Aquandic Kanhaplaquults and Kandiudults). With \code{aquic} the aquic zone
#' of \code{sg_ultisol_aquic_usda()} as well (\code{"arg25"}: Aquandic Humults and
#' Palexerults; \code{"75"}: Aquandic Kandiudults). The layers are those of
#' \code{andic_subgroup_usda()}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param vitrandic Logical.
#' @param aquic \code{NULL} or a zone of \code{sg_ultisol_aquic_usda()}.
#' @noRd
sg_ultisol_andic_usda <- function(pedon, vitrandic = FALSE, aquic = NULL) {
  h <- pedon$horizons
  if (!nrow(h)) return(.sgux_res("sg_ultisol_andic_usda", FALSE))
  L <- .andic_subgroup_layers_usda(h)
  ok <- if (isTRUE(vitrandic)) L$andic | L$vitrandic else L$andic
  an <- .andic_subgroup_18_usda(h, ok)
  p <- an$passed
  miss <- c(L$missing_andic, if (isTRUE(vitrandic)) L$missing_vitrandic)
  if (!is.null(aquic)) {
    a <- sg_ultisol_aquic_usda(pedon, zone = aquic, concentrations = TRUE)
    p <- .and3(p, a$passed); miss <- c(miss, a$missing)
  }
  .sgux_res("sg_ultisol_andic_usda", p, an$layers, miss,
            list(vitrandic = vitrandic, aquic = aquic, thickness_cm = an$thickness_cm),
            "Soil Survey Staff (2022), KST 13th ed., Andic and Aquandic Ultisols (pp. 324-341)")
}

#' Ombroaquic subgroups of the Ultisols (KST 13th ed., pp. 326-339)
#'
#' (1) In one or more horizons within 75 cm of the mineral soil surface,
#' redox concentrations, a moist value of 4 or more and a hue of 10YR or
#' yellower that becomes redder with increasing depth within 100 cm; and (2)
#' saturation within 100 cm for 20 consecutive or 30 cumulative days
#' (\code{oxyaquic_subgroup_usda()}). Redox concentrations are a field
#' observation: absent when not recorded, which decides the test. With
#' \code{andic}, the andic criterion as well (Andic Ombroaquic Kandihumults).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param andic Logical.
#' @noRd
sg_ultisol_ombroaquic_usda <- function(pedon, andic = FALSE) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  L <- .sgux_win(h, ms, ms + 75)
  hr <- .usda_hue_rank(h$munsell_hue_moist)
  deep <- .sgux_win(h, ms, ms + 100)
  c1 <- .usda_or3(vapply(L, function(i) {
    if (!.sgux_conc(h, i)) return(FALSE)
    vm <- h$munsell_value_moist[i]
    if (is.na(vm) || is.na(hr[i])) return(NA)
    if (vm < 4 || hr[i] < 20) return(FALSE)
    below <- deep[!is.na(h$top_cm[deep]) & h$top_cm[deep] > h$top_cm[i]]
    .usda_or3(ifelse(is.na(hr[below]), NA, hr[below] < hr[i]))
  }, NA))
  ox <- oxyaquic_subgroup_usda(pedon, max_depth_cm = 100)
  p <- .and3(c1, ox$passed)
  miss <- c("munsell_hue_moist", "munsell_value_moist", ox$missing)
  if (isTRUE(andic)) {
    an <- sg_ultisol_andic_usda(pedon)
    p <- .and3(p, an$passed); miss <- c(miss, an$missing)
  }
  .sgux_res("sg_ultisol_ombroaquic_usda", p, L, miss, list(redox_colour = c1, saturation = ox$passed),
            "Soil Survey Staff (2022), KST 13th ed., Ombroaquic subgroups (pp. 326-339)")
}

#' Andic subgroups combined with a moisture regime (KST 13th ed., pp. 326-339)
#'
#' The andic criterion and the soil moisture regime: \code{"ustic"} (Ustandic
#' Kandihumults, Kanhaplohumults) or the udic side of the ustic regime
#' (\code{"udic"}, Udandic Kandiustults and Kanhaplustults, see
#' \code{sg_ultisol_ustic_intergrade_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param regime \code{"ustic"} or \code{"udic"}.
#' @noRd
sg_ultisol_andic_regime_usda <- function(pedon, regime = "ustic") {
  an <- sg_ultisol_andic_usda(pedon)
  rg <- if (regime == "ustic") smr_ustic_usda(pedon) else sg_ultisol_ustic_intergrade_usda(pedon, "udic")
  .sgux_res("sg_ultisol_andic_regime_usda", .and3(an$passed, rg$passed), an$layers,
            c(an$missing, rg$missing), list(regime = regime), .SGUX_UL)
}

#' Udic and Aridic subgroups of the Kandiustults and Kanhaplustults (KST 13th
#' ed., pp. 338-340)
#'
#' The ustic regime on its udic side (dry in some part for 135 or fewer
#' cumulative days, or under 120 days in iso regimes) or on its aridic side
#' (dry for more than four-tenths of the cumulative days ...). soilKey records
#' the regime as a class; the side of the ustic regime is read from
#' \code{site$soil_moisture_subclass} ("udic", "aridic", "typic"; NASIS
#' writes "udic ustic", "aridic ustic", "typic ustic"). Not recorded: the
#' regime is a measurement of the soil climate, and the test is undecided
#' (audit P2).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param toward \code{"udic"} or \code{"aridic"}.
#' @noRd
sg_ultisol_ustic_intergrade_usda <- function(pedon, toward = "udic") {
  s <- tolower(as.character(pedon$site$soil_moisture_subclass %||% NA_character_)[1L])
  p <- if (is.na(s) || !nzchar(trimws(s))) NA
       else if (grepl(toward, s)) TRUE
       else if (grepl("udic|aridic|torric|typic|xeric", s)) FALSE else NA
  .sgux_res("sg_ultisol_ustic_intergrade_usda", p, missing = "site$soil_moisture_subclass",
            evidence = list(subclass = s, toward = toward),
            reference = "Soil Survey Staff (2022), KST 13th ed., Udic and Aridic Kandiustults and Kanhaplustults (pp. 338-340)")
}

#' Rhodic subgroups of the Ultisols (KST 13th ed., pp. 332-339)
#'
#' In all subhorizons of the upper \code{upper_cm} of the horizon (kandic or
#' argillic), or all of it if thinner, more than 50 percent colours with hue
#' of 2.5YR or redder, a moist value of 3 or less and a dry value no more
#' than 1 unit higher than the moist value (upper 75 cm: Kandiudults,
#' Kandiustults, Paleudults; upper 50 cm: Kanhapludults, Kanhaplustults). The
#' matrix colour stands for the "more than 50 percent"; a dry value not
#' recorded leaves the test undecided (audit reading L26). With \code{arenic}
#' the Arenic test (50 to 100 cm) as well (Arenic Rhodic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon \code{"kandic"} or \code{"argillic"}.
#' @param upper_cm Thickness of the upper part read.
#' @param arenic Logical.
#' @noRd
sg_ultisol_rhodic_usda <- function(pedon, horizon = "kandic", upper_cm = 75, arenic = FALSE) {
  h <- pedon$horizons
  hz <- .sgux_horizon(pedon, horizon)
  red <- .hue_redder_than(h$munsell_hue_moist, "2.5YR")
  vm <- h$munsell_value_moist; vd <- h$munsell_value_dry
  p <- if (isTRUE(hz$passed) && length(hz$layers)) {
    t0 <- min(h$top_cm[hz$layers])
    L <- hz$layers[h$top_cm[hz$layers] < t0 + upper_cm]
    ok <- ifelse(red[L] %in% FALSE | (!is.na(vm[L]) & vm[L] > 3) |
                   (!is.na(vm[L]) & !is.na(vd[L]) & vd[L] - vm[L] > 1), FALSE,
                 ifelse(is.na(red[L]) | is.na(vm[L]) | is.na(vd[L]), NA, TRUE))
    .and3(ok)
  } else hz$passed
  miss <- c("munsell_hue_moist", "munsell_value_moist", "munsell_value_dry", if (is.na(hz$passed)) hz$missing)
  if (isTRUE(arenic)) {
    ar <- sg_ultisol_arenic_usda(pedon, horizon = horizon, min_cm = 50, max_cm = 100)
    p <- .and3(ar$passed, p); miss <- c(miss, ar$missing)
  }
  .sgux_res("sg_ultisol_rhodic_usda", p, hz$layers, miss,
            list(horizon = horizon, upper_cm = upper_cm, arenic = arenic), .SGUX_UL)
}

#' Psammentic subgroups of the Ultisols (KST 13th ed., pp. 331-341)
#'
#' A sandy particle-size class (loamy fine sand or coarser, not very fine
#' sand, under 35 percent rock fragments) throughout the upper 75 cm of the
#' argillic (or, \code{horizon = "either"}, argillic or kandic) horizon, or
#' all of it if thinner. Texture not recorded, undecided; rock fragments not
#' recorded leave a sandy layer undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon \code{"argillic"} or \code{"either"}.
#' @noRd
sg_ultisol_psammentic_usda <- function(pedon, horizon = "argillic") {
  h <- pedon$horizons
  hz <- .sgux_horizon(pedon, horizon)
  g <- .usda_texture_grades(h)$lfs_or_coarser
  cf <- h$coarse_fragments_pct
  p <- if (isTRUE(hz$passed) && length(hz$layers)) {
    t0 <- min(h$top_cm[hz$layers])
    L <- hz$layers[h$top_cm[hz$layers] < t0 + 75]
    ok <- ifelse(g[L] %in% FALSE | (!is.na(cf[L]) & cf[L] >= 35), FALSE,
                 ifelse(is.na(g[L]) | is.na(cf[L]), NA, TRUE))
    .and3(ok)
  } else hz$passed
  .sgux_res("sg_ultisol_psammentic_usda", p, hz$layers,
            c("clay_pct", "sand_pct", "texture_class_usda", "coarse_fragments_pct",
              if (is.na(hz$passed)) hz$missing),
            list(horizon = horizon), .SGUX_UL)
}

#' Lamellic subgroups of the Ultisols (KST 13th ed., pp. 330-340)
#'
#' An argillic (or argillic or kandic) horizon that consists entirely of
#' lamellae, or of two or more lamellae with other parts, or of thicker parts
#' under two or more lamellae (5 cm or more together). Lamellae are a field
#' observation (a designation with "&", such as E&Bt, or
#' \code{lamellae_cumulative_cm}): read as TRUE where every layer of the
#' horizon carries lamellae, or two or more layers within or above it carry
#' them; absent when not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param horizon \code{"argillic"} or \code{"either"}.
#' @noRd
sg_ultisol_lamellic_usda <- function(pedon, horizon = "argillic") {
  h <- pedon$horizons
  lam <- (!is.na(h$designation) & grepl("&", h$designation, fixed = TRUE)) |
         (!is.na(.usda_col(h, "lamellae_cumulative_cm", NA_real_)) &
            .usda_col(h, "lamellae_cumulative_cm", NA_real_) > 0)
  if (!any(lam)) return(.sgux_res("sg_ultisol_lamellic_usda", FALSE, reference = .SGUX_UL))
  hz <- .sgux_horizon(pedon, horizon)
  p <- if (isTRUE(hz$passed) && length(hz$layers)) {
    b <- max(h$bottom_cm[hz$layers], na.rm = TRUE)
    all(lam[hz$layers]) || sum(lam & !is.na(h$top_cm) & h$top_cm < b) >= 2L
  } else hz$passed
  .sgux_res("sg_ultisol_lamellic_usda", p, which(lam), hz$missing, list(horizon = horizon), .SGUX_UL)
}

#' Inceptic Hapludults (KST 13th ed., p. 331): no densic, lithic or
#' paralithic contact within 50 cm, and an argillic horizon 25 cm or less
#' thick. The thickness is that of the argillic layers; where the argillic
#' horizon reaches the bottom of the profile it may be thicker, and a
#' thickness of 25 cm or less is then undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_inceptic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  ct <- .kst_contact_top_cm(pedon, petroferric = FALSE)
  if (!is.na(ct) && ct < ms + 50)
    return(.sgux_res("sg_ultisol_inceptic_usda", FALSE, reference = .SGUX_UL))
  ar <- argillic_usda(pedon)
  p <- if (isTRUE(ar$passed) && length(ar$layers)) {
    thk <- sum(pmax(0, h$bottom_cm[ar$layers] - h$top_cm[ar$layers]), na.rm = TRUE)
    open <- max(h$bottom_cm[ar$layers], na.rm = TRUE) >= .sgux_bottom(h) - 1e-6 &&
      (is.na(ct) || ct > max(h$bottom_cm[ar$layers], na.rm = TRUE) + 1e-6)
    if (thk > 25) FALSE else if (open) NA else TRUE
  } else ar$passed
  .sgux_res("sg_ultisol_inceptic_usda", p, ar$layers, c("bottom_cm", ar$missing), list(), .SGUX_UL)
}

#' Humic Hapludults and Fragiudults (KST 13th ed., pp. 330-331)
#'
#' A moist value of 3 or less and a dry value of 5 or less (crushed and
#' smoothed) in an Ap horizon 18 cm or more thick, or in the upper 18 cm
#' after mixing (thickness-weighted). A dry value not recorded is read as the
#' moist value + 1 only where the moist value is 2 or less (as the mollic
#' epipedon since v0.9.421, reading L27); otherwise it leaves the test
#' undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_humic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  vm <- h$munsell_value_moist
  vd <- ifelse(is.na(h$munsell_value_dry) & !is.na(vm) & vm <= 2, vm + 1, h$munsell_value_dry)
  test <- function(L, lo, hi) {
    if (!length(L)) return(FALSE)
    w <- pmax(0, pmin(h$bottom_cm[L], hi) - pmax(h$top_cm[L], lo)); w[is.na(w)] <- 0
    if (sum(w) <= 0) return(NA)
    mvm <- if (anyNA(vm[L][w > 0])) NA else sum(vm[L] * w) / sum(w)
    mvd <- if (anyNA(vd[L][w > 0])) NA else sum(vd[L] * w) / sum(w)
    .and3(if (is.na(mvm)) NA else .kst_round(mvm) <= 3, if (is.na(mvd)) NA else .kst_round(mvd) <= 5)
  }
  d <- .usda_desig(h$designation)
  ap <- which(!is.na(d$core) & grepl("^Ap", d$core) & !is.na(h$top_cm) & h$top_cm >= ms - 1e-6)
  apt <- sum(pmax(0, h$bottom_cm[ap] - h$top_cm[ap]), na.rm = TRUE)
  p1 <- if (apt >= 18) test(ap, ms, ms + 1000) else FALSE
  p2 <- if (.sgux_bottom(h) < ms + 18) NA else test(.sgux_win(h, ms, ms + 18), ms, ms + 18)
  .sgux_res("sg_ultisol_humic_usda", .usda_or3(p1, p2), c(ap, .sgux_win(h, ms, ms + 18)),
            c("munsell_value_moist", "munsell_value_dry"), list(ap_cm = apt), .SGUX_UL)
}

#' Glossic and Glossaquic Fragiudults (KST 13th ed., p. 329)
#'
#' One or more of: (1) a glossic horizon above the fragipan
#' (\code{glossic_subgroup_usda()}); (2) above the fragipan, no argillic or
#' kandic horizon with clay films on both vertical and horizontal ped faces
#' (TRUE where no such horizon lies above it; the faces are never recorded,
#' so an argillic or kandic horizon above the fragipan with clay films
#' leaves it undecided, and one with clay films recorded as none passes);
#' (3) between that horizon and the fragipan, a horizon with 50 percent or
#' more chroma of 3 or less and 3 percent (absolute) less clay than both.
#' With \code{aquic}, depletions of chroma 2 or less within 40 cm as well
#' (Glossaquic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param aquic Logical.
#' @noRd
sg_ultisol_glossic_usda <- function(pedon, aquic = FALSE) {
  h <- pedon$horizons
  fp <- fragipan_usda(pedon, max_top_cm = Inf)
  if (!isTRUE(fp$passed) || !length(fp$layers))
    return(.sgux_res("sg_ultisol_glossic_usda", if (isFALSE(fp$passed)) FALSE else NA,
                     missing = fp$missing, reference = .SGUX_UL))
  ftop <- min(h$top_cm[fp$layers])
  gl <- glossic_subgroup_usda(pedon)
  c1 <- if (isTRUE(gl$passed)) {
    al <- gl$evidence$albic$layers
    length(al) > 0L && min(h$top_cm[al]) < ftop
  } else gl$passed
  hz <- .sgux_horizon(pedon, "either")
  above <- if (isTRUE(hz$passed)) hz$layers[h$top_cm[hz$layers] < ftop] else integer(0)
  cf <- tolower(as.character(.usda_col(h, "clay_films_amount", NA_character_)))
  c2 <- if (isTRUE(hz$passed) && !length(above)) TRUE
        else if (isTRUE(hz$passed) && all(!is.na(cf[above]) & cf[above] %in% c("none", "0", "absent"))) TRUE
        else NA
  c3 <- if (length(above)) {
    b <- max(h$bottom_cm[above], na.rm = TRUE)
    mid <- which(!is.na(h$top_cm) & h$top_cm >= b - 1e-6 & h$bottom_cm <= ftop + 1e-6)
    cl_a <- suppressWarnings(max(h$clay_pct[above], na.rm = TRUE))
    cl_f <- h$clay_pct[fp$layers[which.min(h$top_cm[fp$layers])]]
    .usda_or3(vapply(mid, function(i) {
      ch <- h$munsell_chroma_moist[i]; cl <- h$clay_pct[i]
      if ((!is.na(ch) && ch > 3)) return(FALSE)
      if (is.na(cl) || !is.finite(cl_a) || is.na(cl_f)) return(NA)
      dc <- .kst_round(min(cl_a, cl_f) - cl) >= 3
      if (!dc) FALSE else if (is.na(ch)) NA else TRUE
    }, NA))
  } else FALSE
  p <- .usda_or3(c1, c2, c3)
  miss <- c("clay_films_amount", "clay_pct", "munsell_chroma_moist")
  if (isTRUE(aquic)) {
    a <- sg_ultisol_aquic_usda(pedon, zone = "40", concentrations = FALSE, vmin = NA)
    p <- .and3(p, a$passed); miss <- c(miss, a$missing)
  }
  .sgux_res("sg_ultisol_glossic_usda", p, fp$layers, miss,
            list(glossic = c1, no_clay_films = c2, depleted_layer = c3, aquic = aquic), .SGUX_UL)
}

#' Spodic Paleudults (KST 13th ed., p. 334)
#'
#' A horizon 5 cm or more thick, below an Ap horizon or at 18 cm or more
#' (whichever is deeper), with one or more of: (1) 25 percent or more
#' cemented by organic matter and aluminium (a field observation: a Bhsm or
#' Bsm designation; absent when not recorded); (2) Al + 1/2 Fe (oxalate) of
#' 0.25 percent or more, and half that or less in an overlying horizon; (3)
#' an ODOE of 0.12 or more, and half that or less in an overlying horizon.
#' The extractions are measurements: not recorded, undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_spodic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  d <- .usda_desig(h$designation)
  ap <- which(!is.na(d$core) & grepl("^Ap", d$core))
  z <- max(ms + 18, if (length(ap)) max(h$bottom_cm[ap], na.rm = TRUE) else -Inf)
  alfe <- .usda_col(h, "al_ox_pct", NA_real_) + .usda_col(h, "fe_ox_pct", NA_real_) / 2
  odoe <- .usda_col(h, "odoe", NA_real_)
  cand <- which(!is.na(h$top_cm) & h$top_cm >= z - 1e-6 & (h$bottom_cm - h$top_cm) >= 5)
  rel <- function(x, i, lim) {
    if (is.na(x[i])) return(NA)
    if (.kst_round(x[i], 2) < lim) return(FALSE)
    up <- which(!is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[i] + 1e-6 & h$top_cm >= ms - 1e-6)
    if (!length(up)) return(FALSE)
    .usda_or3(ifelse(is.na(x[up]), NA, x[up] <= x[i] / 2))
  }
  ok <- vapply(cand, function(i) {
    c1 <- !is.na(d$suffix[i]) && grepl("m", d$suffix[i]) && grepl("[hs]", d$suffix[i]) &&
      !is.na(d$master[i]) && grepl("B", d$master[i])
    .usda_or3(c1, rel(alfe, i, 0.25), rel(odoe, i, 0.12))
  }, NA)
  p <- .usda_or3(ok)
  .sgux_res("sg_ultisol_spodic_usda", p, cand[ok %in% TRUE], c("al_ox_pct", "fe_ox_pct", "odoe"),
            list(from_cm = z), .SGUX_UL)
}

#' Anthraquic Paleudults (KST 13th ed., p. 335): anthraquic conditions
#' (flood irrigation, a puddled layer). A field observation: the WRB
#' anthraquic horizon (\code{anthraquic()}, designations Apl, Apg ...) or a
#' site land use of paddy rice; absent when not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_anthraquic_usda <- function(pedon) {
  aq <- .usda_try(anthraquic(pedon))
  lu <- tolower(paste(unlist(pedon$site[c("land_use", "landuse", "vegetation")]), collapse = " "))
  p <- isTRUE(.usda_pv(aq)) || grepl("paddy|rice", lu)
  .sgux_res("sg_ultisol_anthraquic_usda", p, if (isTRUE(.usda_pv(aq))) aq$layers, reference = .SGUX_UL)
}

#' Lithic-Ruptic-Entic Hapludults and Lithic Ruptic-Inceptic Haploxerults
#' (KST 13th ed., pp. 330, 340-341)
#'
#' A discontinuous lithic contact within 50 cm in each pedon, or (Entic) a
#' discontinuous argillic horizon interrupted by ledges of bedrock; the
#' Inceptic variant needs a lithic contact within 50 cm and a discontinuous
#' argillic or kandic horizon. Discontinuity is a field observation of the
#' pedon (\code{horizon_continuity} of "broken", "discontinuous" or
#' "interrupted"); absent when not recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param variant \code{"entic"} or \code{"inceptic"}.
#' @noRd
sg_ultisol_lithic_ruptic_usda <- function(pedon, variant = "entic") {
  h <- pedon$horizons
  ms <- .sgux_ms(pedon)
  cont <- tolower(as.character(.usda_col(h, "horizon_continuity", NA_character_)))
  broken <- !is.na(cont) & grepl("broken|discontinu|interrupt", cont)
  lt <- .sgux_contact_top(pedon, "lithic")
  lith <- !is.na(lt) && lt < ms + 50 + 1e-6
  dlith <- lith && any(broken & !is.na(h$top_cm) & abs(h$top_cm - lt) < 1e-6)
  hz <- .sgux_horizon(pedon, if (variant == "entic") "argillic" else "either")
  darg <- isTRUE(hz$passed) && any(broken[hz$layers])
  p <- if (variant == "entic") dlith || darg else lith && darg
  .sgux_res("sg_ultisol_lithic_ruptic_usda", p, which(broken), evidence = list(variant = variant),
            reference = .SGUX_UL)
}

#' Haplic Plinthustults (KST 13th ed., p. 340): a densic, lithic, paralithic
#' or petroferric contact within 150 cm, or within 150 cm a clay decrease of
#' 20 percent or more (relative) from the maximum without 5 percent skeletans
#' and a clay increase of 3 percent below: the negation of the Pale- core
#' test (\code{.pale_core_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_ultisol_haplic_usda <- function(pedon) {
  core <- .pale_core_usda(pedon, petroferric = TRUE)
  .sgux_res("sg_ultisol_haplic_usda", .usda_not3(core$passed), missing = core$missing %||% "clay_pct",
            evidence = list(core = core$reason), reference = .SGUX_UL)
}
