# =============================================================================
# v0.9.433 -- USDA family: cation-exchange activity, calcareous and reaction,
# soil temperature, soil depth, rupture-resistance, coatings on sands and
# permanent cracks classes (Soil Survey Staff 2022, Keys to Soil Taxonomy
# 13th ed., ch. 17, pp. 355 and 366-370).
#
# Three-valued throughout (TRUE / FALSE / NA). A field observation not
# recorded is absent; a measurement not recorded leaves the class undecided:
# value NULL, the missing columns in $missing, the reason in $evidence and
# $evidence$undecided = TRUE. A class omitted by rule (the taxon does not
# use it) is value NULL with nothing missing and the reason "not used ...".
# The readings F12-F15, F18 and F20 are those of the author (USDA audit,
# wave 5, 2026-11-23); see NEWS.md, 0.9.433. Organic soils (Histosols and
# Histels, pp. 370-372) are keyed only where these pages name them (the
# depth class of Histels, the temperature classes).
# =============================================================================

.FC_REF_USDA <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
.fc_ref_usda <- function(pages) paste0(.FC_REF_USDA, ", ", pages)

# The taxon facts these classes read, on top of the particle-size context.
.fc_context_usda <- function(order_code = NULL, great_group = NULL, subgroup = NULL) {
  ctx <- .ps_context_usda(order_code, great_group, subgroup)
  gg <- ctx$great_group; sg <- ctx$subgroup
  has <- function(w) grepl(paste0("\\b", w, "\\b"), sg)
  ctx$gelisol <- identical(ctx$order, "GE")
  # Gelisols and the Gelic suborders and great groups (Gelands, Gelepts,
  # Gelolls, Gelods and their great groups; Gelaquands, Gelaquepts,
  # Gelaquents, Gelorthents, Gelifluvents ...)
  ctx$gelic_taxon <- ctx$gelisol || grepl("gel", gg)
  # cryic suborders, great groups and subgroups (frigid implied, p. 368)
  ctx$cry_taxon <- grepl("cry", gg) || has("Cryic")
  ctx$entisol <- identical(ctx$order, "EN")
  ctx$aquand <- grepl("aquands$", gg)
  ctx$aquept <- grepl("aquepts$", gg)
  ctx$aquoll <- grepl("aquolls$", gg)
  ctx$spodosol <- identical(ctx$order, "SP")
  ctx$kandi <- ctx$order %in% c("AF", "UT") &&
    (grepl("^(kandi|kanhap)", gg) || has("Kandic") || has("Kanhaplic"))
  ctx$folistel <- grepl("folistels$", gg)
  ctx
}

# A FamilyAttribute of these classes.
.fc_attr_usda <- function(name, value, ev, missing = character(0), ref,
                          undecided = FALSE) {
  if (isTRUE(undecided)) ev$undecided <- TRUE
  FamilyAttribute$new(name = name, value = value, evidence = ev,
                      missing = unique(as.character(missing)), reference = ref)
}

#' Is a USDA family term undecided? (reading F20)
#'
#' Value NULL with missing columns, or flagged undecided. A term omitted by
#' rule (not used in the taxon) is not undecided.
#' @noRd
.family_term_undecided_usda <- function(a) {
  !is.null(a) && inherits(a, "FamilyAttribute") && is.null(a$value) &&
    (length(a$missing) > 0L || isTRUE(a$evidence$undecided))
}

# Per-layer table sorted by depth with the columns these classes read.
.fc_layers_usda <- function(pedon) {
  h <- as.data.frame(pedon$horizons)
  h <- h[order(h$top_cm), , drop = FALSE]
  h
}

# Three-valued "in all parts" of a window [lo, hi]: TRUE when the layers
# with a TRUE state cover the whole window, FALSE when a layer inside it is
# FALSE, NA otherwise (reading F18: a part with no data leaves it undecided).
.fc_all_parts3 <- function(top, bottom, state, lo, hi) {
  if (is.na(lo) || is.na(hi) || hi <= lo) return(NA)
  w <- pmax(pmin(bottom, hi) - pmax(top, lo), 0); w[is.na(w)] <- 0
  inside <- w > 0
  if (any(state[inside] %in% FALSE)) return(FALSE)
  if (sum(w[inside & state %in% TRUE]) >= (hi - lo) - 1e-6) return(TRUE)
  NA
}


# ---- cation-exchange activity classes (pp. 366-367) -------------------------

# Particle-size classes that use a cation-exchange activity class (p. 366),
# ranked by clay (the more clayey part names a contrasting family, p. 367).
.CEC_PS_RANK_USDA <- c("very-fine" = 5, "fine" = 4, "clayey" = 4,
                       "clayey-skeletal" = 4, "fine-loamy" = 3, "fine-silty" = 3,
                       "loamy" = 2, "loamy-skeletal" = 2, "coarse-loamy" = 1,
                       "coarse-silty" = 1)

# The ratio CEC (NH4OAc pH 7) / silicate clay over [lo, hi] (p. 366): the
# weighted CEC over the weighted clay of the layers that record both, with
# the clay estimated as 2.5 x (1500 kPa water - organic carbon) when the
# ratio water / measured clay is 0.25 or less or 0.6 or more in half or more
# of the part (as the particle-size classes, reading F11), and the clay-size
# carbonate subtracted (reading F13: zero where the layer has no carbonate,
# caco3_clay_pct where it has, else undecided). The layers read must cover
# half the part (reading F18).
.cec_ratio_usda <- function(pedon, L, lo, hi) {
  h <- as.data.frame(pedon$horizons)
  w <- pmax(pmin(L$bottom, hi) - pmax(L$top, lo), 0); w[is.na(w)] <- 0
  inside <- w > 0
  if (!any(inside)) return(list(value = NA_real_, missing = "horizons", cover = 0))
  fix <- .ps_clay_fix(L[inside, , drop = FALSE], w[inside])
  clay <- L$clay
  if (isTRUE(fix)) {
    est <- ifelse(!is.na(L$w15d) & L$w15_mass & !is.na(L$oc), pmax(2.5 * (L$w15d - L$oc), 0), NA)
    clay <- ifelse(is.na(est), clay, est)
  }
  hh <- h[L$row, , drop = FALSE]
  cec <- .ps_num(hh, "cec_cmol")
  ca <- .mo_caco3_known(hh)
  cc <- .ps_num(hh, "caco3_clay_pct")
  carb <- ifelse(!is.na(cc), cc, ifelse(!is.na(ca) & ca == 0, 0, NA_real_))
  sil <- clay - carb
  have <- inside & !is.na(cec) & !is.na(sil) & sil > 0
  tot <- sum(w[inside]); cov <- sum(w[have])
  ev <- list(clay_estimated = isTRUE(fix), cover = cov / tot)
  if (cov <= 0 || cov < 0.5 * tot - 1e-9) {
    lack <- inside & !have
    miss <- c(if (any(is.na(cec[lack]))) "cec_cmol",
              if (any(is.na(clay[lack]))) "clay_pct",
              if (any(is.na(carb[lack]) & !is.na(ca[lack]) & ca[lack] > 0)) "caco3_clay_pct",
              if (any(is.na(carb[lack]) & is.na(ca[lack]))) c("caco3_pct", "caco3_clay_pct"))
    return(c(list(value = NA_real_, missing = unique(miss)), ev))
  }
  c(list(value = sum(cec[have] * w[have]) / sum(sil[have] * w[have]),
         missing = character(0),
         cec_cmol = sum(cec[have] * w[have]) / cov,
         silicate_clay_pct = sum(sil[have] * w[have]) / cov), ev)
}

#' USDA family: cation-exchange activity class (KST 13th ed., pp. 366-367)
#'
#' Used only with the mixed or siliceous mineralogy classes of the clayey,
#' clayey-skeletal, coarse-loamy, coarse-silty, fine, fine-loamy, fine-silty,
#' loamy, loamy-skeletal and very-fine particle-size classes; never in
#' Histosols, Histels, Oxisols, the kandi and kanhap great groups and
#' subgroups of Alfisols and Ultisols, Psamments, the psamm great groups,
#' Psammentic subgroups, or with any other particle-size, substitute or
#' mineralogy class. The ratio CEC (NH4OAc pH 7) / silicate clay over the
#' particle-size control section: superactive 0.60 or more, active 0.40 to
#' 0.60, semiactive 0.24 to 0.40, subactive below 0.24. In a strongly
#' contrasting family the part that uses a class names it, the more clayey
#' one when both do.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @param particle_size,mineralogy The particle-size and mineralogy
#'   \code{FamilyAttribute}s (computed when NULL).
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_cec_activity_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                     subgroup = NULL, particle_size = NULL,
                                     mineralogy = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("pp. 366-367")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("cec_activity", value, ev, missing, ref, undecided)
  not_used <- function(why, ev = list()) fa(NULL, c(list(reason = paste("not used:", why)), ev))
  if (ctx$histosol || ctx$histel) return(not_used("Histosols and Histels"))
  if (ctx$oxisol) return(not_used("Oxisols"))
  if (ctx$kandi) return(not_used("kandi and kanhap great groups and subgroups"))
  if (ctx$psamm_taxon) return(not_used("Psamments and psamm great groups"))
  if (ctx$psammentic) return(not_used("Psammentic subgroups"))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  ps <- particle_size %||% family_particle_size_usda(pedon, order_code, great_group, subgroup)
  if (is.null(ps$value)) {
    if (.family_term_undecided_usda(ps))
      return(fa(NULL, list(reason = "particle-size class undecided"), ps$missing, TRUE))
    return(not_used("no particle-size class"))
  }
  mn <- mineralogy %||% family_mineralogy_usda(pedon, order_code, great_group, subgroup,
                                               particle_size = ps)
  cs <- ps$evidence$control_section
  psv <- sub(", aniso$", "", ps$value)
  parts <- ps$evidence$contrast_parts
  # the units: the whole control section, or the two named parts
  if (!is.null(parts) && grepl(" over ", psv)) {
    nms <- strsplit(psv, " over ", fixed = TRUE)[[1L]]
    units <- list(
      upper = list(name = nms[1L], lo = max(parts$upper$top, cs$top),
                   hi = min(parts$upper$bottom, cs$bottom)),
      lower = list(name = nms[2L], lo = max(parts$lower$top, cs$top),
                   hi = min(parts$lower$bottom, cs$bottom)))
  } else {
    units <- list(whole = list(name = psv, lo = cs$top, hi = cs$bottom))
  }
  min_of <- function(u) {
    mp <- mn$evidence$parts
    if (!is.null(mn$value) && !is.null(mp) && !is.null(mp[[u]])) return(mp[[u]]$class)
    if (!is.null(mn$value)) return(mn$value)
    NA_character_
  }
  min_und <- is.null(mn$value) && .family_term_undecided_usda(mn)
  qual <- vapply(names(units), function(u) {
    ps_ok <- units[[u]]$name %in% names(.CEC_PS_RANK_USDA)
    if (!ps_ok) return(FALSE)
    m <- min_of(u)
    if (is.na(m)) return(if (min_und) NA else FALSE)
    m %in% c("mixed", "siliceous")
  }, NA)
  ev <- list(control_section = cs, particle_size = ps$value,
             mineralogy = mn$value, units = lapply(units, function(u) u[c("name", "lo", "hi")]))
  if (all(qual %in% FALSE))
    return(not_used(paste0("particle-size class '", ps$value, "' with mineralogy class '",
                           mn$value %||% "none", "'"), ev))
  L <- .ps_layers_usda(pedon, ctx)
  cand <- names(units)[!(qual %in% FALSE)]
  if (length(cand) > 1L) {
    rk <- .CEC_PS_RANK_USDA[vapply(units[cand], function(u) u$name, "")]
    if (rk[1L] == rk[2L]) {
      cl <- vapply(cand, function(u) .cec_ratio_usda(pedon, L, units[[u]]$lo,
                                                     units[[u]]$hi)$silicate_clay_pct %||% NA_real_, 0)
      pick <- if (any(is.na(cl))) NA_character_ else cand[which.max(cl)]
    } else pick <- cand[which.max(rk)]
    if (is.na(pick))
      return(fa(NULL, c(ev, list(reason = "the more clayey part of the contrasting family is undecided")),
                c("clay_pct", "cec_cmol"), TRUE))
  } else pick <- cand
  ev$unit <- pick
  if (is.na(qual[[pick]]))
    return(fa(NULL, c(ev, list(reason = "mineralogy class undecided (mixed or siliceous?)")),
              mn$missing, TRUE))
  r <- .cec_ratio_usda(pedon, L, units[[pick]]$lo, units[[pick]]$hi)
  ev$ratio <- r[setdiff(names(r), "missing")]
  if (is.na(r$value))
    return(fa(NULL, c(ev, list(reason = "CEC / silicate clay ratio undecided")), r$missing, TRUE))
  value <- if (r$value >= 0.60) "superactive"
           else if (r$value >= 0.40) "active"
           else if (r$value >= 0.24) "semiactive"
           else "subactive"
  ev$cec_to_clay_ratio <- r$value
  fa(value, ev)
}


# ---- calcareous and reaction classes (pp. 367-368) --------------------------

# Effervescence of the fine earth in each layer (reading F12): recorded, as
# recorded ("none" or a local extent is FALSE: not in all parts); not
# recorded, CaCO3 above 0 is TRUE and 0 FALSE; neither, undecided. A layer
# with pH (CaCl2) below 5.0 cannot hold carbonates (further reading).
.fc_effervescent_usda <- function(h) {
  ef <- .ps_txt(h, "effervescence_class")
  ex <- .ps_txt(h, "effervescence_extent")
  cc <- .ps_num(h, "caco3_pct")
  ph <- .ps_num(h, "ph_cacl2")
  ef_no <- !is.na(ef) & nzchar(ef) & (ef %in% c("none", "no", "noncalcareous", "non-calcareous") |
                                        grepl("^non", ef))
  ef_yes <- !is.na(ef) & nzchar(ef) & !ef_no
  out <- ifelse(ef_yes, TRUE, ifelse(ef_no, FALSE,
           ifelse(!is.na(cc), cc > 0, NA)))
  out[!is.na(ex) & ex %in% c("none", "local")] <- FALSE
  out[is.na(out) & !is.na(ph) & ph < 5.0] <- FALSE
  as.logical(out)
}

# The anhydritic, carbonatic, gypsic or hypergypsic mineralogy (exclusion 7
# of the calcareous class, 5 of acid and nonacid, p. 368): TRUE / FALSE / NA.
.fc_min_excluded_usda <- function(mn) {
  set <- c("anhydritic", "carbonatic", "gypsic", "hypergypsic")
  if (is.null(mn)) return(NA)
  if (!is.null(mn$value)) {
    parts <- strsplit(mn$value, " over ", fixed = TRUE)[[1L]]
    return(any(parts %in% set))
  }
  if (!.family_term_undecided_usda(mn)) return(FALSE)
  tested <- mn$evidence$tested
  if (!length(tested)) return(NA)
  poss <- vapply(tested, function(t) {
    if (!length(t)) return(NA)
    k <- intersect(set, names(t))
    if (!length(k)) return(FALSE)
    any(!(unlist(t[k]) %in% FALSE))
  }, NA)
  if (any(poss %in% TRUE) || any(is.na(poss))) NA else FALSE
}

#' USDA family: calcareous and reaction classes (KST 13th ed., pp. 367-368)
#'
#' The key A to D: allic (Oxisols with a layer 30 cm or more thick in the
#' particle-size control section holding more than 2 cmol(+) KCl-extractable
#' Al per kg), calcareous (effervescence with cold dilute HCl in all parts
#' of its control section), acid (pH in 0.01 M CaCl2 below 5.0 throughout the
#' control section) and nonacid. Calcareous is used in Entisols, Gelisols,
#' Aquands, Aquepts, Aquolls and the Gelic suborders and great groups;
#' acid and nonacid in the same taxa without the Aquolls; allic only in
#' Oxisols; each with the exclusions of p. 368. Control sections (p. 368):
#' calcareous 25 to 50 cm from the mineral soil surface (Gelisols other than
#' Histels, Gelic taxa and Wassents: the surface to 25 cm or a root-limiting
#' layer; a root-limiting layer within 25 cm: the 2.5 cm above it; within 26
#' to 50 cm: 25 cm to the layer); acid, nonacid and allic: the particle-size
#' control section (Gelisols and Gelic taxa: the surface to 25 cm or a
#' root-limiting layer).
#'
#' @inheritParams family_cec_activity_usda
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_reaction_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                 subgroup = NULL, particle_size = NULL,
                                 mineralogy = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("pp. 367-368")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("reaction", value, ev, missing, ref, undecided)
  not_used <- function(why, ev = list()) fa(NULL, c(list(reason = paste("not used:", why)), ev))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  H <- .fc_layers_usda(pedon)
  gg <- ctx$great_group
  ps <- NULL
  get_ps <- function() {
    if (is.null(ps)) ps <<- particle_size %||%
      family_particle_size_usda(pedon, order_code, great_group, subgroup)
    ps
  }
  ps_cs <- function() {
    cs <- get_ps()$evidence$control_section
    if (is.null(cs)) cs <- family_control_section_usda(pedon, order_code, great_group, subgroup)
    cs
  }

  # A. allic: Oxisols only
  if (ctx$oxisol) {
    cs <- ps_cs()
    if (is.na(cs$top) || is.na(cs$bottom))
      return(fa(NULL, list(reason = "control section undecided"),
                get_ps()$missing %||% cs$missing, TRUE))
    al <- .ps_num(H, "al_kcl_cmol")
    st <- ifelse(is.na(al), NA, al > 2)
    yes <- .haht_run_thk(H, st %in% TRUE, cs$top, cs$bottom)
    may <- .haht_run_thk(H, !(st %in% FALSE), cs$top, cs$bottom)
    ev <- list(control_section = cs[c("top", "bottom")], al_kcl_run_cm = yes,
               al_kcl_run_possible_cm = may)
    if (yes >= 30) return(fa("allic", ev))
    if (may < 30) return(not_used("Oxisols take only the allic class, and no layer 30 cm or more thick has more than 2 cmol(+) KCl-extractable Al per kg", ev))
    return(fa(NULL, c(ev, list(reason = "allic class undecided")), "al_kcl_cmol", TRUE))
  }

  calc_taxa <- ctx$entisol || ctx$gelisol || ctx$aquand || ctx$aquept ||
    ctx$aquoll || ctx$gelic_taxon
  if (!calc_taxa) return(not_used("taxon outside the lists of p. 368"))
  if (ctx$histel) return(not_used("Histels"))
  # exclusions shared by the calcareous and the acid / nonacid classes
  excl <- list()
  excl$duraquands_placaquands <- gg %in% c("duraquands", "placaquands")
  excl$sulf_frag_petraquepts <- gg %in% c("sulfaquepts", "fragiaquepts", "petraquepts")
  miss <- character(0)
  psv <- get_ps()$value
  ps_und <- .family_term_undecided_usda(get_ps())
  if (ctx$psamm_taxon || ctx$psammentic) {
    excl$psamm_no_class <- if (!is.null(psv)) FALSE else if (ps_und) NA else TRUE
  }
  excl$sandy_family <- if (!is.null(psv))
    psv %in% c("sandy", "sandy-skeletal", "cindery", "pumiceous", "fragmental")
  else if (ps_und) NA else FALSE
  if (ps_und) miss <- c(miss, get_ps()$missing)
  mn <- mineralogy %||% family_mineralogy_usda(pedon, order_code, great_group, subgroup,
                                               particle_size = get_ps())
  excl$mineralogy <- .fc_min_excluded_usda(mn)
  if (is.na(excl$mineralogy)) miss <- c(miss, mn$missing)
  # exclusions of the calcareous class only (Aquolls)
  if (ctx$aquoll) {
    excl$calci_natr_argiaquolls <- gg %in% c("calciaquolls", "natraquolls", "argiaquolls")
    if (gg %in% c("cryaquolls", "duraquolls")) {
      an <- .ps_clay_horizon_usda(pedon, kinds = c("argillic", "natric"))
      excl$cry_duraquolls_argillic_natric <- an$passed
      if (is.na(an$passed)) miss <- c(miss, an$missing)
    }
  }
  ex <- unlist(excl)
  ev <- list(exclusions = as.list(ex))
  if (any(ex %in% TRUE))
    return(not_used(paste("excluded:", paste(names(ex)[ex %in% TRUE], collapse = ", ")), ev))
  if (any(is.na(ex)))
    return(fa(NULL, c(ev, list(reason = paste("whether the class is used is undecided:",
                                               paste(names(ex)[is.na(ex)], collapse = ", ")))),
              miss, TRUE))

  ms <- .usda_mineral_surface(pedon)
  rl <- .root_limiting_usda(pedon)
  rld <- rl$top - ms
  gel_cs <- ctx$gelic_taxon && !ctx$histel
  # B. calcareous: its own control section (p. 368)
  calc_cs <- if (gel_cs || ctx$wassent) c(ms, min(ms + 25, rl$top))
             else if (rld <= 25) c(rl$top - 2.5, rl$top)
             else if (rld <= 50) c(ms + 25, rl$top)
             else c(ms + 25, ms + 50)
  eff <- .fc_effervescent_usda(H)
  calc <- .fc_all_parts3(H$top_cm, H$bottom_cm, eff, calc_cs[1L], calc_cs[2L])
  ev$calcareous_control_section <- calc_cs
  ev$calcareous <- calc
  if (isTRUE(calc)) return(fa("calcareous", ev))
  calc_miss <- c("effervescence_class", "caco3_pct")
  if (ctx$aquoll) {
    if (isFALSE(calc)) return(not_used("Aquolls take only the calcareous class, and the control section does not effervesce in all parts", ev))
    return(fa(NULL, c(ev, list(reason = "calcareous class undecided")), calc_miss, TRUE))
  }
  # C, D. acid and nonacid
  acs <- if (gel_cs) list(top = ms, bottom = min(ms + 25, rl$top)) else ps_cs()
  if (is.na(acs$top) || is.na(acs$bottom))
    return(fa(NULL, c(ev, list(reason = "control section undecided")),
              get_ps()$missing %||% acs$missing, TRUE))
  ph <- .ps_num(H, "ph_cacl2")
  acid <- .fc_all_parts3(H$top_cm, H$bottom_cm, ifelse(is.na(ph), NA, ph < 5.0),
                         acs$top, acs$bottom)
  ev$acid_control_section <- c(acs$top, acs$bottom)
  ev$acid <- acid
  if (is.na(calc))
    return(fa(NULL, c(ev, list(reason = "calcareous class undecided")),
              c(calc_miss, if (is.na(acid)) "ph_cacl2"), TRUE))
  if (isTRUE(acid)) return(fa("acid", ev))
  if (isFALSE(acid)) return(fa("nonacid", ev))
  fa(NULL, c(ev, list(reason = "acid or nonacid undecided")), "ph_cacl2", TRUE)
}


# ---- soil temperature classes (pp. 368-369) ---------------------------------

.FC_TEMP_CLASSES_USDA <- c("frigid", "mesic", "thermic", "hyperthermic",
                           "isofrigid", "isomesic", "isothermic", "isohyperthermic")
.FC_GELIC_CLASSES_USDA <- c("hypergelic", "pergelic", "subgelic")

#' USDA family: soil temperature class (KST 13th ed., pp. 368-369)
#'
#' Gelisols and the Gelic suborders and great groups take hypergelic (mean
#' annual soil temperature -10 C or lower), pergelic (-10 to -4 C) or
#' subgelic (-4 to +1 C), read from \code{site$mean_annual_soil_temp_C}; not
#' recorded, the class is undecided (reading F14). Other soils take the class
#' of the recorded \code{site$soil_temperature_regime} (frigid, mesic,
#' thermic, hyperthermic and their iso forms; there is no cryic class).
#' A class implied by a higher taxon is omitted: frigid in the cryic
#' suborders, great groups and subgroups. Without a recorded regime the class
#' is undecided, unless \code{infer = TRUE} (opt-in) suggests one from
#' latitude and elevation (\code{evidence$suggestion = TRUE}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param infer Suggest a class from latitude and elevation when no regime is
#'   recorded. Default \code{FALSE}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_temperature_regime_usda <- function(pedon, infer = FALSE, order_code = NULL,
                                           great_group = NULL, subgroup = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("pp. 368-369; ch. 16")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("temperature_regime", value, ev, missing, ref, undecided)
  implied <- function(value, ev) {
    if (identical(value, "frigid") && ctx$cry_taxon)
      return(fa(NULL, c(ev, list(reason = "not used: frigid is implied by the cryic taxon",
                                  class_implied = value))))
    fa(value, ev)
  }
  str <- pedon$site$soil_temperature_regime %||% NA_character_
  reg <- if (.usda_regime_blank(str)) NA_character_
         else gsub("[^a-z]", "", tolower(.usda_str_value(as.character(str)[1L])))
  # A. Gelisols and Gelic taxa: by the mean annual soil temperature
  if (ctx$gelic_taxon) {
    mast <- suppressWarnings(as.numeric(pedon$site$mean_annual_soil_temp_C %||% NA)[1L])
    if (!is.na(mast) && mast <= 1) {
      v <- if (mast <= -10) "hypergelic" else if (mast <= -4) "pergelic" else "subgelic"
      return(fa(v, list(inferred = FALSE, source = "site$mean_annual_soil_temp_C",
                        mean_annual_soil_temp_C = mast)))
    }
    if (is.na(mast) && !is.na(reg) && reg %in% .FC_GELIC_CLASSES_USDA)
      return(fa(reg, list(inferred = FALSE, source = "site$soil_temperature_regime")))
    if (is.na(mast))
      return(fa(NULL, list(inferred = FALSE, reason = paste(
        "Gelisols and Gelic taxa take a gelic class by the mean annual soil",
        "temperature, which is not recorded")), "site$mean_annual_soil_temp_C", TRUE))
    # warmer than +1 C: the keys B and C below, from the recorded regime
  }
  if (!is.na(reg)) {
    ev <- list(inferred = FALSE, source = "site$soil_temperature_regime", regime = reg)
    if (reg %in% .FC_TEMP_CLASSES_USDA) return(implied(reg, ev))
    if (reg == "cryic" && ctx$cry_taxon)
      return(fa(NULL, c(ev, list(reason = "not used: frigid is implied by the cryic taxon (no cryic class)"))))
    why <- if (reg %in% c("cryic", "gelic", .FC_GELIC_CLASSES_USDA))
      sprintf("a %s regime outside the taxa that imply it: frigid or isofrigid by the difference between mean summer and mean winter soil temperature", reg)
    else sprintf("soil temperature regime '%s' not recognised", reg)
    return(fa(NULL, c(ev, list(reason = why)),
              c("site$soil_temperature_regime", "site$mean_annual_soil_temp_C"), TRUE))
  }
  lat <- suppressWarnings(as.numeric(pedon$site$lat %||% NA))
  if (!isTRUE(infer) || is.na(lat) || ctx$gelic_taxon) {
    return(fa(NULL, list(inferred = FALSE,
                         reason = if (!isTRUE(infer)) "no soil temperature regime recorded (not inferred)"
                                  else "no soil temperature regime recorded and no latitude"),
              "site$soil_temperature_regime", TRUE))
  }
  elev <- suppressWarnings(as.numeric(pedon$site$elevation_m %||% 0))
  if (is.na(elev)) elev <- 0
  # Crude MAAT model (deg C); MAST ~ MAAT. Tropical-to-temperate lapse.
  maat <- 27 - 0.3 * abs(lat) - 0.0065 * elev
  base <- if (maat < 8) "frigid"
          else if (maat < 15) "mesic"
          else if (maat < 22) "thermic"
          else "hyperthermic"
  iso <- abs(lat) < 23   # low seasonal range -> iso- prefix
  value <- if (iso) paste0("iso", base) else base
  ev <- list(inferred = TRUE, suggestion = TRUE, mast_est_c = maat, lat = lat,
             elevation_m = elev, iso = iso)
  if (identical(value, "frigid") && ctx$cry_taxon)
    return(fa(NULL, c(ev, list(reason = "not used: frigid is implied by the cryic taxon"))))
  FamilyAttribute$new(name = "temperature_regime", value = value, evidence = ev,
                      missing = "site$soil_temperature_regime", reference = ref)
}


# ---- soil depth classes (p. 369) ---------------------------------------------

#' USDA family: soil depth class (KST 13th ed., p. 369)
#'
#' Only "shallow": Oxisols less than 100 cm from the mineral soil surface to
#' a root-limiting layer; other mineral soils and Folistels less than 50 cm
#' from the mineral soil surface; other Histels less than 50 cm. Not used in
#' Lithic subgroups or in soils with a fragipan. The root-limiting layers
#' are those of the particle-size control section (\code{.root_limiting_usda()},
#' p. 357): field observations, absent when not recorded (reading F15).
#' The depth classes of Histosols (p. 372) are not keyed in this version.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_depth_class_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                    subgroup = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("p. 369")
  fa <- function(value, ev, missing = character(0))
    .fc_attr_usda("depth_class", value, ev, missing, ref)
  if (ctx$histosol)
    return(fa(NULL, list(reason = paste("the depth classes of Histosols (KST 13th ed.,",
                                        "p. 372) are not keyed in this version"))))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers")))
  if (ctx$lithic) return(fa(NULL, list(reason = "not used: Lithic subgroup")))
  rl <- .root_limiting_usda(pedon)
  if (is.finite(rl$fragipan_top))
    return(fa(NULL, list(reason = "not used: soil with a fragipan",
                         fragipan_top_cm = rl$fragipan_top)))
  if (!is.finite(rl$top))
    return(fa(NULL, list(reason = "no root-limiting layer recorded")))
  surf <- if (ctx$histel && !ctx$folistel) min(h$top_cm, na.rm = TRUE)
          else .usda_mineral_surface(pedon)
  depth <- rl$top - surf
  limit <- if (ctx$oxisol) 100 else 50
  ev <- list(root_limiting_top_cm = rl$top, root_limiting_kind = rl$kind,
             depth_cm = depth, limit_cm = limit)
  if (depth < limit) return(fa("shallow", ev))
  fa(NULL, c(ev, list(reason = sprintf("root-limiting layer at %g cm or deeper", limit))))
}


# ---- rupture-resistance classes (p. 369) -------------------------------------

#' USDA family: rupture-resistance class (KST 13th ed., p. 369)
#'
#' "ortstein" for Spodosols with an ortstein horizon (p. 27 of the KST:
#' spodic materials in a layer 50 percent or more pedogenically cemented,
#' 2.5 cm or more thick). A field observation: a cemented spodic layer
#' recorded (suffix m, \code{pan_type} "ortstein", or a
#' \code{cementation_class} in a spodic layer), unless its recorded share of
#' cementation (\code{duripan_pct}) is below 50; not recorded, absent.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_rupture_resistance_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                           subgroup = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("p. 369")
  fa <- function(value, ev) .fc_attr_usda("rupture_resistance", value, ev, character(0), ref)
  if (!ctx$spodosol) return(fa(NULL, list(reason = "not used: Spodosols only")))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers")))
  orte <- .usda_ortstein_layers(pedon)
  if (length(orte))
    return(fa("ortstein", list(layers = orte)))
  fa(NULL, list(reason = "no ortstein horizon recorded"))
}

# Layers of ortstein (KST 13th ed., p. 27): spodic materials (a layer of the
# spodic horizon, or a B horizon with the suffix h or s), 50 percent or more
# pedogenically cemented (the suffix m, pan_type "ortstein", or a recorded
# cementation class; a recorded share below 50 rules it out), 2.5 cm or more
# thick. A field observation: not recorded, absent.
.usda_ortstein_layers <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  if (!n) return(integer(0))
  d <- .usda_desig(h$designation %||% rep(NA_character_, n))
  sp <- tryCatch(spodic_horizon_usda(pedon), error = function(e) NULL)
  sp_layers <- if (!is.null(sp) && isTRUE(sp$passed)) sp$layers else integer(0)
  spodic <- seq_len(n) %in% sp_layers |
    (!is.na(d$master) & grepl("B", d$master) & !is.na(d$suffix) & grepl("[hs]", d$suffix))
  pan <- .ps_txt(h, "pan_type")
  cem <- .ps_txt(h, "cementation_class")
  cemented <- (!is.na(d$suffix) & grepl("m", d$suffix)) |
    (!is.na(pan) & pan == "ortstein") |
    (!is.na(cem) & cem %in% .USDA_CEMENTED)
  share <- .ps_num(h, "duripan_pct")
  cemented <- cemented & !(!is.na(share) & share < 50)
  thick <- !is.na(h$bottom_cm) & !is.na(h$top_cm) & (h$bottom_cm - h$top_cm) >= 2.5
  which((spodic | (!is.na(pan) & pan == "ortstein")) & cemented & thick)
}


# ---- classes of coatings on sands (pp. 369-370) -----------------------------

#' USDA family: classes of coatings on sands (KST 13th ed., pp. 369-370)
#'
#' Quartzipsamments only: "coated" when the weighted average silt plus twice
#' the weighted average clay over the particle-size control section is more
#' than 5, else "uncoated". The layers measured must cover half the control
#' section (reading F18).
#'
#' @inheritParams family_cec_activity_usda
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_coatings_usda <- function(pedon, order_code = NULL, great_group = NULL,
                                 subgroup = NULL, particle_size = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("pp. 369-370")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("coatings", value, ev, missing, ref, undecided)
  if (!identical(ctx$great_group, "quartzipsamments"))
    return(fa(NULL, list(reason = "not used: Quartzipsamments only")))
  h <- pedon$horizons
  if (is.null(h) || !nrow(h)) return(fa(NULL, list(reason = "no layers"), "horizons", TRUE))
  ps <- particle_size %||% family_particle_size_usda(pedon, order_code, great_group, subgroup)
  cs <- ps$evidence$control_section
  if (is.null(cs) || is.na(cs$top) || is.na(cs$bottom))
    return(fa(NULL, list(reason = "control section undecided"), ps$missing, TRUE))
  L <- .ps_layers_usda(pedon, ctx)
  s2c <- L$silt + 2 * L$clay
  r <- .wavg_clip_usda(L$top, L$bottom, s2c, cs$top, cs$bottom)
  ev <- list(control_section = cs[c("top", "bottom")], cover = r$cover)
  if (is.na(r$value))
    return(fa(NULL, c(ev, list(reason = "silt + 2 x clay undecided")),
              c("silt_pct", "clay_pct"), TRUE))
  ev$silt_plus_2clay <- r$value
  fa(if (r$value > 5) "coated" else "uncoated", ev)
}


# ---- classes of permanent cracks (p. 370) ------------------------------------

#' USDA family: classes of permanent cracks (KST 13th ed., p. 370)
#'
#' Fluvaquents and Humaquepts only: "cracked" when, throughout a layer 50 cm
#' or more thick within the control section (the base of a plow layer or 25
#' cm, whichever is deeper, to 100 cm from the soil surface), there are
#' continuous, permanent, lateral and vertical cracks 2 mm or more wide
#' (\code{cracks_width_cm} 0.2 or more) at average lateral intervals of less
#' than 50 cm (\code{polygonal_cracks_spacing_cm}). Permanence is a field
#' observation (\code{cracks_permanent}): not recorded, absent. With
#' permanent cracks recorded, a width or spacing not recorded leaves the
#' class undecided.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code,great_group,subgroup The taxa assigned above the family.
#' @return A \code{\link{FamilyAttribute}}.
#' @noRd
family_cracks_usda <- function(pedon, order_code = NULL, great_group = NULL,
                               subgroup = NULL) {
  ctx <- .fc_context_usda(order_code, great_group, subgroup)
  ref <- .fc_ref_usda("p. 370")
  fa <- function(value, ev, missing = character(0), undecided = FALSE)
    .fc_attr_usda("cracks", value, ev, missing, ref, undecided)
  if (!(ctx$great_group %in% c("fluvaquents", "humaquepts")))
    return(fa(NULL, list(reason = "not used: Fluvaquents and Humaquepts only")))
  H <- .fc_layers_usda(pedon)
  if (!nrow(H)) return(fa(NULL, list(reason = "no layers")))
  surf <- min(H$top_cm, na.rm = TRUE)
  ap <- .sg_ie_ap_bottom(H)
  lo <- max(c(surf + 25, ap), na.rm = TRUE); hi <- surf + 100
  perm <- as.logical(.usda_col(H, "cracks_permanent", NA))
  wd <- .ps_num(H, "cracks_width_cm"); sp <- .ps_num(H, "polygonal_cracks_spacing_cm")
  st <- ifelse(!(perm %in% TRUE), FALSE,
         ifelse(is.na(wd) | is.na(sp), NA, wd >= 0.2 & sp < 50))
  yes <- .haht_run_thk(H, st %in% TRUE, lo, hi)
  may <- .haht_run_thk(H, !(st %in% FALSE), lo, hi)
  ev <- list(control_section = c(lo, hi), run_cm = yes, run_possible_cm = may)
  if (yes >= 50) return(fa("cracked", ev))
  if (may < 50) return(fa(NULL, c(ev, list(reason = "no permanent cracks recorded through 50 cm"))))
  fa(NULL, c(ev, list(reason = "permanent cracks recorded; width or spacing not recorded")),
     c("cracks_width_cm", "polygonal_cracks_spacing_cm"), TRUE)
}
