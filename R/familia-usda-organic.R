# =============================================================================
# v0.9.434 -- USDA family of organic soils (Histosols and Histels): the
# particle-size classes of the Terric subgroups, the mineralogy classes
# (ferrihumic; coprogenous, diatomaceous and marly limnic materials; the key
# of mineral soils for the Terric subgroups), the reaction classes (kalkic,
# euic, dysic) and the soil depth classes of Histosols (micro, shallow)
# (Soil Survey Staff 2022, Keys to Soil Taxonomy 13th ed., ch. 17, pp.
# 370-372). The temperature classes of organic soils are those of the
# mineral soils and the Gelisols (R/familia-usda-classes.R), and so is the
# depth class of the Histels (p. 369).
#
# Three-valued throughout (TRUE / FALSE / NA). A field observation not
# recorded is absent (limnic layers and their kind, ferrihumic soil
# material, root-limiting layers, coarse fragments); a measurement not
# recorded leaves the class undecided: value NULL, the missing columns in
# $missing, the reason in $evidence and $evidence$undecided = TRUE. A class
# the taxon does not use is value NULL with nothing missing and the reason
# "not used ...". Readings F16 to F18 of the author (USDA audit, wave 5,
# 2026-11-23) and the further readings F39 to F46 (NEWS.md, 0.9.434).
#
# The functions are reached through family_particle_size_usda(),
# family_mineralogy_usda(), family_reaction_usda() and
# family_depth_class_usda(), which hand organic soils over to them; the
# mineral-soil families do not change.
# =============================================================================

.ORG_REF_USDA <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
.org_ref_usda <- function(pages) paste0(.ORG_REF_USDA, ", ", pages)

# The taxon facts the organic classes read.
.org_context_usda <- function(ctx) {
  gg <- ctx$great_group; sg <- ctx$subgroup
  has <- function(w) grepl(paste0("\\b", w, "\\b"), sg)
  ctx$organic <- isTRUE(ctx$histosol) || isTRUE(ctx$histel)
  ctx$folist <- grepl("folists$", gg)
  ctx$sphagnofibrist <- identical(gg, "sphagnofibrists")
  ctx$sphagnic <- has("Sphagnic")
  ctx
}

# The control section of Histosols and Histels (KST 13th ed., p. 41) and its
# tiers, as the order and subgroup keys read it.
.org_tiers_usda <- function(pedon) {
  .histosol_tiers_usda(pedon, .usda_organic_status(pedon))
}


# ---- the control section of the Terric subgroups (p. 370) -------------------

#' Control section for the particle-size and mineralogy classes of Terric
#' Histosols and Histels (USDA, KST 13th ed., pp. 370-371)
#'
#' The upper 30 cm of the mineral layer that makes the soil Terric, or the
#' part of that layer within the control section of Histosols and Histels
#' (p. 41), whichever is thicker. The mineral layer is the shallowest run of
#' adjoining layers of mineral soil material that meets the Terric test of
#' the taxon: in Histosols 30 cm or more thick with its top within the
#' control section, below the surface tier; in Histels 30 cm or more within
#' 100 cm of the soil surface (as \code{sg_histosols_terric_usda()} and
#' \code{sg_gelisols_terric_usda()} read it). A layer of unknown kind
#' (organic or mineral undecided) that could move the top or the base of the
#' section leaves it undecided (reading F45).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ctx The taxon context (\code{.ps_context_usda()}).
#' @return A list: \code{top}, \code{bottom} (cm, NA when undecided),
#'   \code{missing}, \code{reason}.
#' @noRd
.org_terric_section_usda <- function(pedon, ctx) {
  h <- pedon$horizons
  t <- .org_tiers_usda(pedon)
  r <- .sg_shg_mineral_runs(pedon)
  top0 <- .sg_shg_soil_top(h)
  ok <- if (isTRUE(ctx$histel)) {
    function(run) .thk_in(h, run, top0, top0 + 100) >= 30
  } else {
    function(run) {
      top <- min(h$top_cm[run])
      top >= t$surface[2] - 1e-6 && top < t$bottom && .usda_run_thickness(h, run) >= 30
    }
  }
  sec <- function(run) {
    a <- min(h$top_cm[run]); b <- max(h$bottom_cm[run])
    hi <- min(max(a + 30, min(b, t$bottom)), b)
    c(a, hi)
  }
  first <- function(runs) {
    runs <- Filter(ok, runs)
    if (!length(runs)) return(NULL)
    runs[[which.min(vapply(runs, function(x) min(h$top_cm[x]), 0))]]
  }
  y <- first(r$yes); m <- first(r$may)
  undec <- list(top = NA_real_, bottom = NA_real_, missing = c("oc_pct", "designation"))
  if (is.null(y)) {
    return(c(undec, list(reason = if (is.null(m)) "no mineral layer of the Terric subgroups"
                                  else "the mineral layer of the Terric subgroups is undecided")))
  }
  sy <- sec(y); sm <- sec(m)
  if (any(abs(sy - sm) > 1e-6))
    return(c(undec, list(reason = paste("layers of unknown kind (organic or mineral) could",
                                        "move the bounds of the mineral layer"))))
  list(top = sy[1L], bottom = sy[2L], missing = character(0), reason = NULL,
       layers = y, histosol_control_section_cm = t$bottom)
}


# ---- particle-size classes (p. 370) -----------------------------------------

#' USDA family: particle-size class of organic soils (KST 13th ed., p. 370)
#'
#' Only the Terric subgroups of Histosols and Histels, over the control
#' section of \code{.org_terric_section_usda()}, by weighted average:
#' fragmental (fine earth less than 10 percent of the volume: coarse
#' fragments and cohesive, persistent artifacts more than 90 percent),
#' "sandy or sandy-skeletal" (a sand or loamy sand texture, not very fine
#' sand), loamy-skeletal (35 percent or more fragments, less than 35 percent
#' clay), clayey-skeletal (35 percent or more fragments), clayey (35 percent
#' or more clay) and loamy. No strongly contrasting classes. The clay
#' estimated from the water at 1500 kPa and the reading of the sandy
#' textures are those of the mineral key (readings F5, F7, F8, F11).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ctx The taxon context (\code{.ps_context_usda()}).
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_particle_size_organic_usda <- function(pedon, ctx) {
  ref <- .org_ref_usda("p. 370")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("particle_size", value, ev, missing, ref, undecided)
  if (!isTRUE(ctx$terric))
    return(fa(NULL, list(reason = "not named in this taxon")))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  cs <- .org_terric_section_usda(pedon, ctx)
  ev <- list(control_section = list(top = cs$top, bottom = cs$bottom, case = "organic Terric"))
  if (is.na(cs$top) || is.na(cs$bottom))
    return(fa(NULL, c(ev, list(reason = paste("control section undecided:", cs$reason))),
              cs$missing, TRUE))
  L <- .ps_layers_usda(pedon, ctx)
  w <- pmax(pmin(L$bottom, cs$bottom) - pmax(L$top, cs$top), 0); w[is.na(w)] <- 0
  inside <- w > 0
  ev$layers_in_control_section <- L$row[inside]
  fix <- .ps_clay_fix(L[inside, , drop = FALSE], w[inside])
  m <- .ps_material(L[inside, , drop = FALSE], w[inside], clay_fix = fix)
  ev$weighted <- m[c("clay", "sand", "silt", "frag")]
  ev$clay_corrected <- fix
  r <- .org_ps_class_usda(m)
  ev$class_reached <- r$class
  if (is.na(r$class))
    return(fa(NULL, c(ev, list(reason = r$reason)), r$missing, TRUE))
  fa(r$class, ev)
}

# The key of p. 370 for a weighted material m: the class, or NA with the
# missing columns.
.org_ps_class_usda <- function(m) {
  r <- function(cl, miss = character(0), why = NULL) list(class = cl, missing = miss, reason = why)
  if (!is.na(m$frag) && m$frag > 90) return(r("fragmental"))
  sandy <- .ps_and3(m$sandy_tex, m$grade_ok)
  if (isTRUE(sandy)) return(r("sandy or sandy-skeletal"))
  if (is.na(sandy))
    return(r(NA_character_, c(if (is.na(m$sand)) "sand_pct", if (is.na(m$clay)) "clay_pct",
                              if (isTRUE(m$sandy_tex) && is.na(m$grade_ok))
                                c("very_fine_sand_pct", "texture_class_usda")),
             "sandy texture or sand grade undecided (F5, F7)"))
  if (is.na(m$clay)) return(r(NA_character_, "clay_pct", "clay undecided"))
  skel <- !is.na(m$frag) && m$frag >= 35
  if (skel) return(r(if (m$clay < 35) "loamy-skeletal" else "clayey-skeletal"))
  r(if (m$clay >= 35) "clayey" else "loamy")
}


# ---- mineralogy classes (pp. 371-372) ---------------------------------------

# Layers of ferrihumic soil material (bog iron, p. 371): a recorded
# identification (ferrihumic_material TRUE). A field observation: not
# recorded, absent (reading F40).
.org_ferrihumic_layers <- function(h) {
  x <- as.logical(.usda_col(h, "ferrihumic_material", NA))
  x %in% TRUE
}

# The kind of each limnic layer (KST 13th ed., ch. 3): "coprogenous",
# "diatomaceous", "marl", NA when the layer is limnic and its kind is not
# recorded, "" when the layer is not limnic. Read from the designation (the
# master L with the suffix co, di or ma) or a layer origin naming the
# material. A field observation: a layer not recorded as limnic is not.
.org_limnic_kind <- function(h) {
  lim <- .sg_shg_limnic_layers(h)
  d <- .usda_desig(h$designation %||% rep(NA_character_, nrow(h)))
  sfx <- ifelse(!is.na(d$master) & d$master == "L", d$suffix %||% "", "")
  sfx[is.na(sfx)] <- ""
  lo <- tolower(as.character(.usda_col(h, "layer_origin", NA_character_)))
  lo[is.na(lo)] <- ""
  kind <- ifelse(grepl("^co", sfx) | grepl("coprogen", lo), "coprogenous",
          ifelse(grepl("^di", sfx) | grepl("diatom", lo), "diatomaceous",
          ifelse(grepl("^ma", sfx) | grepl("marl", lo), "marl", NA_character_)))
  ifelse(lim, kind, "")
}

#' USDA family: mineralogy class of organic soils (KST 13th ed., pp. 371-372)
#'
#' The key A to D of p. 371: (A) ferrihumic, for Histosols other than
#' Folists, Sphagnofibrists and the Sphagnic subgroups (Wassists included,
#' reading F39), with ferrihumic soil material within the control section of
#' Histosols; (B) other Histosols (Folists included) with limnic materials 5
#' cm or more thick within that control section, by kind in the order
#' coprogenous, diatomaceous, marly (the thickness of each kind summed, as
#' the Limnic subgroups read it; a limnic layer of unrecorded kind leaves
#' the class undecided, reading F41); (C) the Terric subgroups (of
#' Histels, reading F17, and of Histosols): the key of mineral soils (pp.
#' 364-366) over the control section of the particle-size classes, the
#' section chosen by the class of p. 370; (D) all others: no class.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ctx The taxon context (\code{.ps_context_usda()}).
#' @param particle_size The particle-size \code{FamilyAttribute} of the
#'   organic soil (computed when NULL).
#' @param suggest Opt-in stand-ins for an undecided class of key C (readings
#'   F3, F4), as for mineral soils.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_mineralogy_organic_usda <- function(pedon, ctx, particle_size = NULL,
                                           suggest = FALSE) {
  ctx <- .org_context_usda(ctx)
  ref <- .org_ref_usda("pp. 371-372")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("mineralogy", value, ev, missing, ref, undecided)
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  t <- .org_tiers_usda(pedon)
  inside <- !is.na(h$top_cm) & !is.na(h$bottom_cm) & h$bottom_cm > 0 & h$top_cm < t$bottom
  ev <- list(histosol_control_section = c(0, t$bottom))
  # A. ferrihumic
  if (isTRUE(ctx$histosol) && !ctx$folist && !ctx$sphagnofibrist && !ctx$sphagnic) {
    fh <- which(.org_ferrihumic_layers(h) & inside)
    if (length(fh)) return(fa("ferrihumic", c(ev, list(key = "A", layers = fh))))
  }
  # B. limnic materials, 5 cm or more thick, by kind
  if (isTRUE(ctx$histosol)) {
    kind <- .org_limnic_kind(h)
    unk <- .thk_in(h, which(inside & is.na(kind)), 0, t$bottom)
    ev$limnic_kind_unknown_cm <- unk
    for (k in c("coprogenous", "diatomaceous", "marl")) {
      thk <- .thk_in(h, which(inside & kind %in% k), 0, t$bottom)
      ev[[paste0(k, "_cm")]] <- thk
      if (thk >= 5) {
        cls <- if (k == "marl") "marly" else k
        return(fa(cls, c(ev, list(key = "B"))))
      }
      if (thk + unk >= 5)
        return(fa(NULL, c(ev, list(key = "B", reason = paste(
          "limnic materials of unrecorded kind could make", k, "material 5 cm thick"))),
          c("designation", "layer_origin"), TRUE))
    }
  }
  # C. Terric subgroups: the key of mineral soils
  if (!isTRUE(ctx$terric))
    return(fa(NULL, c(ev, list(reason = "not used: no class of the key applies (p. 372, D)"))))
  ps <- particle_size %||% family_particle_size_organic_usda(pedon, ctx)
  cs <- ps$evidence$control_section
  if (is.null(cs) || is.na(cs$top) || is.na(cs$bottom))
    return(fa(NULL, c(ev, list(key = "C", reason = "control section undecided")),
              ps$missing, TRUE))
  cls <- ps$value %||% ps$evidence$class_reached
  wm <- ps$evidence$weighted
  clayey <- if (!is.null(cls) && !is.na(cls)) cls %in% c("clayey", "clayey-skeletal")
            else if (!is.null(wm$frag) && !is.na(wm$frag) && wm$frag > 90) FALSE
            else if (is.null(wm$clay) || is.na(wm$clay)) NA else wm$clay >= 35
  # the mineral key's own name for the fragmental class (section C, p. 365)
  mcls <- if (identical(cls, "fragmental")) "fragmental" else NA_character_
  M <- .min_layers_usda(pedon)
  U <- .min_unit_usda(M, cs$top, cs$bottom)
  ev$key <- "C"; ev$control_section <- cs; ev$particle_size <- ps$value
  if (is.null(U))
    return(fa(NULL, c(ev, list(reason = "no mineral layer in the control section")),
              "horizons", TRUE))
  # no substitute class in organic soils (p. 370): section B is not reached
  # (reading F42)
  r <- .min_unit_class(U, FALSE, mcls, FALSE, clayey)
  ev$sections <- r$section; ev$tested <- list(whole = r$tested)
  if (isTRUE(r$state)) return(fa(r$class, ev))
  ev$reason <- "mineralogy class undecided: an earlier class of the section could not be ruled out"
  if (isTRUE(suggest)) {
    s <- .min_suggest_usda(pedon, U, r$section)
    if (!is.null(s$value)) {
      ev$suggestion <- TRUE
      ev$suggestion_evidence <- s$evidence
      return(FamilyAttribute$new(name = "mineralogy", value = s$value, evidence = ev,
                                 missing = unique(r$missing), reference = ref))
    }
  }
  fa(NULL, ev, r$missing, TRUE)
}


# ---- reaction classes (p. 372) ----------------------------------------------

#' USDA family: reaction class of organic soils (KST 13th ed., p. 372)
#'
#' Used in all Histosols and Histels. (A) kalkic: Histosols that effervesce
#' with dilute HCl throughout the surface tier (effervescence as the
#' calcareous class reads it, reading F12); (B) euic: a pH in 0.01 M CaCl2
#' of 4.5 or more in one or more layers of organic soil material within the
#' control section of Histosols (the recorded \code{ph_cacl2}, reading F16;
#' the KST asks for undried samples); (C) dysic: all others. Dysic needs the
#' pH of every organic layer of the control section, and a part of it with no
#' layer described (or a layer of unknown kind with a pH of 4.5 or more, or
#' none) leaves it undecided (readings F18, F46).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ctx The taxon context (\code{.ps_context_usda()}).
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_reaction_organic_usda <- function(pedon, ctx) {
  ref <- .org_ref_usda("p. 372")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("reaction", value, ev, missing, ref, undecided)
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  t <- .org_tiers_usda(pedon)
  H <- .fc_layers_usda(pedon)
  org <- .usda_organic_status(pedon)[order(h$top_cm)]
  ev <- list(surface_tier = t$surface, histosol_control_section = c(0, t$bottom),
             ph_note = paste("pH in 0.01 M CaCl2 read as recorded; the KST asks for",
                             "undried samples (reading F16)"))
  # A. kalkic (Histosols only)
  kalkic <- FALSE
  if (isTRUE(ctx$histosol)) {
    eff <- .fc_effervescent_usda(H)
    kalkic <- .fc_all_parts3(H$top_cm, H$bottom_cm, eff, t$surface[1], t$surface[2])
    ev$kalkic <- kalkic
    if (isTRUE(kalkic)) return(fa("kalkic", ev))
  }
  # B. euic: one organic layer with pH (CaCl2) 4.5 or more
  ph <- .ps_num(H, "ph_cacl2")
  w <- pmax(pmin(H$bottom_cm, t$bottom) - pmax(H$top_cm, 0), 0); w[is.na(w)] <- 0
  inside <- w > 0
  hi <- !is.na(ph) & ph >= 4.5
  euic_yes <- any(inside & org %in% TRUE & hi)
  # a layer that may be organic and may reach 4.5; or a part not described
  may <- any(inside & !(org %in% FALSE) & (hi | is.na(ph)))
  gap <- sum(w[inside]) < t$bottom - 1e-6
  ev$euic <- if (euic_yes) TRUE else if (may || gap) NA else FALSE
  ev$control_section_described <- !gap
  if (euic_yes) {
    if (is.na(kalkic))
      return(fa(NULL, c(ev, list(reason = "kalkic class undecided")),
                c("effervescence_class", "caco3_pct"), TRUE))
    return(fa("euic", ev))
  }
  if (is.na(kalkic) || may || gap) {
    miss <- c(if (is.na(kalkic)) c("effervescence_class", "caco3_pct"),
              if (may) "ph_cacl2",
              if (any(inside & is.na(org))) "oc_pct",
              if (gap) "horizons")
    why <- if (is.na(kalkic)) "kalkic class undecided"
           else if (gap) "part of the control section of Histosols not described"
           else "euic or dysic undecided"
    return(fa(NULL, c(ev, list(reason = why)), miss, TRUE))
  }
  fa("dysic", ev)
}


# ---- soil depth classes of Histosols (p. 372) -------------------------------

#' USDA family: soil depth class of Histosols (KST 13th ed., p. 372)
#'
#' From the soil surface to the shallowest root-limiting layer (duripan;
#' petrocalcic, petrogypsic or placic horizon; continuous ortstein; densic,
#' lithic, manufactured-layer, paralithic or petroferric contact: those of
#' \code{.root_limiting_usda()} without the fragipan) or layer of mineral
#' soil material with a pumiceous, cindery or fragmental substitute class
#' (fine earth less than 10 percent of the volume): micro when less than 18
#' cm; shallow from 18 to less than 50 cm, except in Folists (reading F43);
#' otherwise no class. Used in all subgroups of Histosols, Lithic ones too.
#' Field observations: a layer not recorded is absent (reading F15).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ctx The taxon context (\code{.ps_context_usda()}).
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_depth_class_organic_usda <- function(pedon, ctx) {
  ctx <- .org_context_usda(ctx)
  ref <- .org_ref_usda("p. 372")
  fa <- function(value, ev) .fc_attr_usda("depth_class", value, ev, character(0), ref)
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers")))
  rl <- .root_limiting_usda(pedon, fragipan = FALSE)
  L <- .ps_layers_usda(pedon, ctx)
  org <- .usda_organic_status(pedon)[L$row]
  fr <- which(!(org %in% TRUE) & !is.na(L$frag) & L$frag > 90 & !is.na(L$top))
  fr_top <- if (length(fr)) min(L$top[fr]) else Inf
  top <- min(rl$top, fr_top)
  surf <- .sg_shg_soil_top(h)
  if (!is.finite(top))
    return(fa(NULL, list(reason = "no root-limiting layer or fragmental, cindery or pumiceous layer recorded")))
  depth <- top - surf
  ev <- list(depth_cm = depth,
             limit_kind = if (rl$top <= fr_top) rl$kind else "pumiceous, cindery or fragmental substitute class",
             from = "soil surface")
  if (depth < 18) return(fa("micro", ev))
  if (depth < 50) {
    if (ctx$folist) return(fa(NULL, c(ev, list(reason = "not used: the shallow class in Folists"))))
    return(fa("shallow", ev))
  }
  fa(NULL, c(ev, list(reason = "root-limiting layer at 50 cm or deeper")))
}
