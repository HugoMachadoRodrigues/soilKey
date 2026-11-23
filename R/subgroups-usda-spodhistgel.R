# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Subgroup tests of the Spodosols (Ch. 14), Histosols (Ch. 10) and Gelisols
# (Ch. 9), v0.9.427
# =============================================================
#
# The subgroup lists of inst/rules/usda/subgroups/{spodosols,histosols,
# gelisols}.yaml follow the keys of the KST 13th edition (pp. 189-198,
# 199-205, 311-320). Where a subgroup of these orders means something that a
# helper shared with other orders does not test (the depth is taken from
# another surface, the thickness is of one layer rather than a sum, the
# criterion is three-valued), the test is written here, with names
# sg_spodosols_*, sg_histosols_* and sg_gelisols_*. Nothing in this file
# changes a helper that other orders call.
#
# Missing data (Hugo's rules): a field observation not recorded is absent
# (contacts, buried horizons, water layers, limnic layers, ice, cracks,
# lamellae, designations); a measurement not recorded is undecided (NA) when
# it could change the answer.
# =============================================================


# ---- small shared readings ---------------------------------------------

.sg_shg_and3 <- function(...) .usda_all3(list(...))
.sg_shg_or3 <- function(...) .usda_any3(list(...))

.sg_shg_result <- function(name, passed, layers = integer(0), evidence = list(),
                           missing = character(0), reference = "") {
  DiagnosticResult$new(
    name = name, passed = passed,
    layers = if (isTRUE(passed)) as.integer(layers) else integer(0),
    evidence = evidence,
    missing = if (is.na(passed)) unique(missing) else character(0),
    reference = reference)
}

# The soil surface (the top of the profile) and the mineral soil surface
# (KST 13th ed., p. 7).
.sg_shg_soil_top <- function(h) {
  t <- suppressWarnings(min(h$top_cm, na.rm = TRUE))
  if (is.finite(t)) t else 0
}
.sg_shg_mineral_top <- function(pedon) {
  .usda_mineral_surface(pedon, .usda_organic_status(pedon))
}

# Masters read from the designations (numbers and carets removed).
.sg_shg_master <- function(h) .usda_desig(h$designation)$master

# Mineral soil material layer by layer (TRUE / FALSE / NA): not organic soil
# material (.usda_organic_status), and not rock (R), paralithic material (Cr),
# limnic material (L), water (W) or ice (designations with ff).
.sg_shg_mineral <- function(pedon) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  m <- .sg_shg_master(h)
  d <- ifelse(is.na(h$designation), "", h$designation)
  other <- (!is.na(m) & (m %in% c("R", "L", "W") | grepl("^Cr", .usda_desig(h$designation)$core))) |
           grepl("ff", d)
  ifelse(other, FALSE, !org)
}

# A lithic contact (an R layer, a field observation) whose top is within
# `max_cm` of `base` (cm).
.sg_shg_lithic <- function(pedon, base, max_cm, name, ref) {
  h <- pedon$horizons
  m <- .sg_shg_master(h)
  k <- which(!is.na(m) & m == "R" & !is.na(h$top_cm) & h$top_cm - base <= max_cm + 1e-9)
  .sg_shg_result(name, length(k) > 0L, k,
                 evidence = list(from_cm = base, max_cm = max_cm), reference = ref)
}


# =====================================================================
# SPODOSOLS (KST 13th ed., Ch. 14, pp. 311-320)
# =====================================================================

#' Lithic subgroups of the Spodosols (KST 13th ed., pp. 311-320)
#'
#' A lithic contact within \code{max_cm} (50) of the mineral soil surface. Up
#' to v0.9.426 \code{lithic_contact_usda()} measured from the top of the
#' profile, so an O horizon shortened the window.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_spodosols_lithic_usda <- function(pedon, max_cm = 50) {
  .sg_shg_lithic(pedon, .sg_shg_mineral_top(pedon), max_cm, "sg_spodosols_lithic_usda",
                 "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Lithic Alaquods, p. 311)")
}

#' Andic subgroups of the Spodosols (KST 13th ed., pp. 312-319)
#'
#' Andic soil properties (p. 32, \code{.andic_layer_status_usda()}) throughout
#' horizons with a total thickness of 25 cm or more within 75 cm of the
#' mineral soil surface or of the top of an organic layer with andic soil
#' properties, whichever is shallower. Up to v0.9.426 the Andic subgroups of
#' the mineral orders (18 cm of a low bulk density and Al + 1/2 Fe over 1.0).
#' Undecided layers leave the test NA when they could reach 25 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_andic_usda <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Andic Cryaquods, p. 312)"
  h <- pedon$horizons
  if (!nrow(h)) return(.sg_shg_result("sg_spodosols_andic_usda", FALSE, reference = ref))
  st <- .andic_layer_status_usda(h)
  org <- .usda_organic_status(pedon)
  ms <- .usda_mineral_surface(pedon, org)
  org_andic <- which(org %in% TRUE & st$status %in% TRUE & !is.na(h$top_cm))
  base <- min(c(ms, h$top_cm[org_andic]))
  ov <- pmax(pmin(h$bottom_cm, base + 75) - pmax(h$top_cm, base), 0)
  ov[is.na(ov)] <- 0
  yes <- sum(ov[st$status %in% TRUE]); may <- sum(ov[is.na(st$status)])
  passed <- if (yes >= 25) TRUE else if (yes + may < 25) FALSE else NA
  und <- which(ov > 0 & is.na(st$status))
  .sg_shg_result("sg_spodosols_andic_usda", passed, which(ov > 0 & st$status %in% TRUE),
                 evidence = list(from_cm = base, andic_cm = yes, undecided_cm = may),
                 missing = unlist(st$missing[und]), reference = ref)
}

#' Aquic subgroups of the Cryods, Gelods and Orthods (KST 13th ed., pp.
#' 314-319)
#'
#' Redoximorphic features in one or more horizons within \code{max_cm} (75)
#' of the mineral soil surface and aquic conditions
#' (\code{aquic_conditions_usda()}). Up to v0.9.426
#' \code{aquic_subgroup_usda()}, within 100 cm of the top of the profile.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_spodosols_aquic_usda <- function(pedon, max_cm = 75) {
  ms <- .sg_shg_mineral_top(pedon)
  r <- aquic_conditions_usda(pedon, max_top_cm = ms + max_cm)
  .sg_shg_result("sg_spodosols_aquic_usda", r$passed, r$layers,
                 evidence = list(aquic = r, from_cm = ms, max_cm = max_cm),
                 missing = r$missing,
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Aquic Haplocryods, p. 314)")
}

#' Aquandic subgroups of the Cryods (KST 13th ed., pp. 314-315)
#'
#' Both \code{sg_spodosols_aquic_usda()} (75 cm) and
#' \code{sg_spodosols_andic_usda()}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_aquandic_usda <- function(pedon) {
  aq <- sg_spodosols_aquic_usda(pedon)
  an <- sg_spodosols_andic_usda(pedon)
  p <- .sg_shg_and3(aq$passed, an$passed)
  .sg_shg_result("sg_spodosols_aquandic_usda", p, union(aq$layers, an$layers),
                 evidence = list(aquic = aq$passed, andic = an$passed),
                 missing = c(aq$missing, an$missing),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Aquandic Duricryods, p. 314)")
}

# The tops (cm) of the spodic horizon the subgroup tests may read: the top of
# the horizon when it is decided, or the tops of its undecided layers.
.sg_spod_spodic <- function(pedon) {
  sl <- .spodic_layers_for_tests(pedon)
  h <- pedon$horizons
  L <- sl$layers[order(h$top_cm[sl$layers])]
  list(sure = sl$sure, layers = L,
       tops = if (!length(L)) numeric(0) else if (sl$sure) min(h$top_cm[L]) else h$top_cm[L],
       bottom = if (length(L)) max(h$bottom_cm[L]) else NA_real_,
       missing = sl$sp$missing %||% character(0))
}

#' Arenic and Grossarenic subgroups of the Spodosols (KST 13th ed., pp.
#' 311-317)
#'
#' A texture class (fine earth) of coarse sand, sand, fine sand, loamy coarse
#' sand, loamy sand or loamy fine sand throughout a layer from the mineral
#' soil surface to the top of a spodic horizon at a depth of 75 to 125 cm
#' (Arenic) or of 125 cm or more (Grossarenic, \code{min_cm = 125, max_cm =
#' Inf}). The texture is read by \code{.usda_texture_grades()} (loamy fine
#' sand or coarser, the very fine grades excluded). Three-valued: a texture
#' not recorded in a layer above the spodic horizon, or a spodic horizon
#' still undecided, leaves it NA. Up to v0.9.426 \code{arenic_subgroup_usda()}
#' skipped the layers without clay or sand and measured from the top of the
#' profile.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Depth window of the top of the spodic horizon.
#' @noRd
sg_spodosols_arenic_usda <- function(pedon, min_cm = 75, max_cm = 125) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Arenic Alaquods, p. 312)"
  h <- pedon$horizons
  ms <- .sg_shg_mineral_top(pedon)
  sp <- .sg_spod_spodic(pedon)
  sandy <- .usda_texture_grades(h)$lfs_or_coarser
  one <- function(top) {
    d <- top - ms
    if (d < min_cm - 1e-9 || d >= max_cm) return(FALSE)
    above <- which(!is.na(h$top_cm) & h$bottom_cm > ms & h$top_cm < top)
    .usda_all3(sandy[above])
  }
  v <- vapply(sp$tops, one, NA)
  passed <- .usda_any3(v)
  if (!sp$sure && isTRUE(passed)) passed <- NA
  .sg_shg_result(if (is.infinite(max_cm)) "sg_spodosols_grossarenic_usda" else "sg_spodosols_arenic_usda",
                 passed, which(!is.na(h$top_cm) & h$top_cm < (if (length(sp$tops)) min(sp$tops) else 0) & h$bottom_cm > ms),
                 evidence = list(spodic_top_cm = sp$tops, mineral_surface_cm = ms,
                                 window = c(min_cm, max_cm)),
                 missing = c("clay_pct", "sand_pct", "texture_class_usda", if (!sp$sure) sp$missing),
                 reference = ref)
}

#' @rdname sg_spodosols_arenic_usda
#' @noRd
sg_spodosols_grossarenic_usda <- function(pedon) sg_spodosols_arenic_usda(pedon, 125, Inf)

# An argillic or kandic horizon whose layers satisfy `where` (a function of
# the layer indices returning TRUE / FALSE per layer), three-valued, with the
# horizon's layers that satisfy it.
.sg_spod_argkan <- function(pedon, where, bs_min = NULL) {
  out <- list()
  b <- if (!is.null(bs_min)) .bs_sum_cations_usda(pedon) else NULL
  for (hz in list(argillic_usda(pedon), kandic_horizon_usda(pedon))) {
    p <- hz$passed
    L <- hz$layers %||% integer(0)
    L <- L[where(L)]
    v <- if (isFALSE(p)) FALSE
         else if (!length(L)) { if (isTRUE(p)) FALSE else NA }
         else if (is.null(bs_min)) p
         else {
           bs <- b$bs[L]
           bsv <- if (any(!is.na(bs) & .kst_round(bs) >= bs_min)) TRUE
                  else if (anyNA(bs)) NA else FALSE
           .sg_shg_and3(p, bsv)
         }
    out[[length(out) + 1L]] <- list(passed = v, layers = L, missing = hz$missing)
  }
  list(passed = .usda_any3(lapply(out, `[[`, "passed")),
       layers = unique(unlist(lapply(out, function(x) if (isTRUE(x$passed)) x$layers))),
       missing = c(unlist(lapply(out, `[[`, "missing")), if (!is.null(bs_min)) "bs_pct"))
}

#' Ultic, Argic and Alfic subgroups of the Spodosols (KST 13th ed., pp.
#' 311-320)
#'
#' An argillic or kandic horizon within 200 cm of the mineral soil surface
#' (Ultic, Argic), and for the Alfic subgroups a base saturation (by sum of
#' cations; NH4OAc where it is not recorded, reading L18) of 35 percent or
#' more in some part of it (\code{alfic = TRUE}). Three-valued on the
#' horizon and the base saturation. Up to v0.9.426 the horizon was taken
#' only when decided, and from the top of the profile.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param alfic \code{TRUE} for the Alfic subgroups.
#' @noRd
sg_spodosols_argic_usda <- function(pedon, alfic = FALSE) {
  h <- pedon$horizons
  ms <- .sg_shg_mineral_top(pedon)
  r <- .sg_spod_argkan(pedon, function(L) !is.na(h$top_cm[L]) & h$top_cm[L] - ms < 200,
                       bs_min = if (isTRUE(alfic)) 35 else NULL)
  .sg_shg_result(if (isTRUE(alfic)) "sg_spodosols_alfic_usda" else "sg_spodosols_argic_usda",
                 r$passed, r$layers, evidence = list(mineral_surface_cm = ms),
                 missing = r$missing,
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 14 (e.g. Alfic and Ultic Alaquods, p. 312)")
}

#' @rdname sg_spodosols_argic_usda
#' @noRd
sg_spodosols_alfic_usda <- function(pedon) sg_spodosols_argic_usda(pedon, alfic = TRUE)

#' Argillic or kandic horizon below the spodic horizon (Arenic Ultic
#' Alorthods, KST 13th ed., p. 317)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_argic_below_spodic_usda <- function(pedon) {
  h <- pedon$horizons
  sp <- .sg_spod_spodic(pedon)
  if (!length(sp$layers))
    return(.sg_shg_result("sg_spodosols_argic_below_spodic_usda", FALSE,
                          reference = "Soil Survey Staff (2022), KST 13th ed., p. 317"))
  bot <- sp$bottom
  r <- .sg_spod_argkan(pedon, function(L) !is.na(h$top_cm[L]) & h$top_cm[L] >= bot - 1e-6)
  p <- r$passed
  if (!sp$sure && isTRUE(p)) p <- NA
  .sg_shg_result("sg_spodosols_argic_below_spodic_usda", p, r$layers,
                 evidence = list(spodic_bottom_cm = bot), missing = r$missing,
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 317 (Arenic Ultic Alorthods)")
}

#' Turbic subgroups of the Gelods (KST 13th ed., pp. 315-316)
#'
#' Gelic materials (\code{gelic_materials_usda()}) within 200 cm of the
#' mineral soil surface. Up to v0.9.426 \code{turbic_subgroup_usda()} also
#' passed on permafrost alone.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_turbic_usda <- function(pedon) {
  ms <- .sg_shg_mineral_top(pedon)
  r <- gelic_materials_usda(pedon, max_top_cm = ms + 200)
  .sg_shg_result("sg_spodosols_turbic_usda", r$passed, r$layers,
                 evidence = list(mineral_surface_cm = ms),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 315 (Turbic Haplogelods)")
}

#' Fragic soil properties in the Haplorthods (KST 13th ed., p. 318)
#'
#' Fragic soil properties in 30 percent or more of the volume of a layer 15
#' cm or more thick whose upper boundary is within 100 cm of the mineral soil
#' surface (or in 60 percent or more of a layer 15 cm thick). Read from the
#' field records: a run of layers 15 cm or more thick, starting within 100
#' cm, with the suffix x in the designation, or the firm rupture-resistance
#' classes or the NASIS fragipan of \code{fragipan_usda()}. Field
#' observations: not recorded, absent. The volume share is not recorded, and
#' the 60 percent branch below 100 cm is not tested.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_fragic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_shg_mineral_top(pedon)
  d <- .usda_desig(h$designation)
  x <- !is.na(d$suffix) & grepl("x", d$suffix)
  ord <- order(h$top_cm)
  runs <- Filter(function(r) .usda_run_thickness(h, r) >= 15 && min(h$top_cm[r]) - ms < 100,
                 .usda_runs(x, ord))
  fp <- fragipan_usda(pedon, max_top_cm = ms + 100)
  p <- length(runs) > 0L || isTRUE(fp$passed)
  .sg_shg_result("sg_spodosols_fragic_usda", p,
                 c(unlist(runs), fp$layers), evidence = list(x_runs = runs, fragipan = fp$passed),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 318 (Fragic and Fragiaquic Haplorthods)")
}

#' Fragiaquic Haplorthods (KST 13th ed., p. 318): fragic soil properties and
#' the aquic conditions of the Aquic subgroups (75 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_fragiaquic_usda <- function(pedon) {
  f <- sg_spodosols_fragic_usda(pedon); a <- sg_spodosols_aquic_usda(pedon)
  .sg_shg_result("sg_spodosols_fragiaquic_usda", .sg_shg_and3(f$passed, a$passed),
                 union(f$layers, a$layers), missing = c(f$missing, a$missing),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 318")
}

#' Lamellic subgroups of the Haplorthods (KST 13th ed., p. 319)
#'
#' Two or more lamellae within 200 cm of the mineral soil surface, below the
#' spodic horizon and not below an argillic horizon. Lamellae are read from
#' the field record: layers whose designation joins two horizons with an
#' ampersand and a t (E&Bt), or with \code{lamellae_cumulative_cm} above 0;
#' two such layers make two lamellae or more. Up to v0.9.426 any two layers
#' with an ampersand, anywhere.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_spodosols_lamellic_usda <- function(pedon) {
  h <- pedon$horizons
  ms <- .sg_shg_mineral_top(pedon)
  des <- ifelse(is.na(h$designation), "", h$designation)
  lam <- (grepl("&", des, fixed = TRUE) & grepl("t", des)) |
         (!is.na(.usda_col(h, "lamellae_cumulative_cm", NA_real_)) &
            .usda_col(h, "lamellae_cumulative_cm", NA_real_) > 0)
  sp <- .sg_spod_spodic(pedon)
  sp_bot <- if (is.na(sp$bottom)) -Inf else sp$bottom
  ar <- argillic_usda(pedon)
  ar_top <- if (isTRUE(ar$passed) && length(ar$layers)) min(h$top_cm[ar$layers]) else Inf
  k <- which(lam & !is.na(h$top_cm) & h$top_cm - ms < 200 & h$top_cm >= sp_bot - 1e-6 &
               h$top_cm < ar_top)
  .sg_shg_result("sg_spodosols_lamellic_usda", length(k) >= 2L, k,
                 evidence = list(lamella_layers = k),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 319 (Lamellic Haplorthods)")
}


# =====================================================================
# HISTOSOLS (KST 13th ed., Ch. 10, pp. 199-205)
# =====================================================================

.sg_hist_tiers <- function(pedon) .histosol_tiers_usda(pedon, .usda_organic_status(pedon))

#' Lithic subgroups of the Histosols (KST 13th ed., pp. 199-205)
#'
#' Folists (\code{max_cm = 50}): a lithic contact within 50 cm of the soil
#' surface. Fibrists, Hemists and Saprists (\code{max_cm = NULL}): a lithic
#' contact at the lower boundary of the control section, that is, its top is
#' within the control section of Histosols (130 cm, or 160 cm under a moss
#' or very light surface, p. 41). Up to v0.9.426 \code{lithic_contact_usda()}
#' within 130 cm in every case.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm 50, or \code{NULL} for the control section.
#' @noRd
sg_histosols_lithic_usda <- function(pedon, max_cm = NULL) {
  h <- pedon$horizons
  top0 <- .sg_shg_soil_top(h)
  if (is.null(max_cm)) {
    tiers <- .sg_hist_tiers(pedon)
    max_cm <- if (tiers$moss) 160 else 130
  }
  .sg_shg_lithic(pedon, top0, max_cm, "sg_histosols_lithic_usda",
                 "Soil Survey Staff (2022), KST 13th ed., Ch. 10 (e.g. Lithic Haplohemists, p. 202)")
}

#' Hydric subgroups of the Histosols (KST 13th ed., pp. 199-202)
#'
#' A layer of water within the control section, below the surface tier: a
#' layer with the master W (not Wf, frozen) or a layer origin of water, whose
#' top is at or below the bottom of the surface tier and above the bottom of
#' the control section. Up to v0.9.426 any such layer within 130 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_hydric_usda <- function(pedon) {
  h <- pedon$horizons
  t <- .sg_hist_tiers(pedon)
  d <- ifelse(is.na(h$designation), "", h$designation)
  lo <- tolower(as.character(.usda_col(h, "layer_origin", NA_character_)))
  w <- grepl("^[0-9]*W(?![efr])", d, perl = TRUE) | (!is.na(lo) & lo == "water")
  k <- which(w & !is.na(h$top_cm) & h$bottom_cm > t$surface[2] & h$top_cm < t$bottom)
  .sg_shg_result("sg_histosols_hydric_usda", length(k) > 0L, k,
                 evidence = list(zone = c(t$surface[2], t$bottom)),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 10 (e.g. Hydric Haplohemists, p. 201)")
}

# Limnic layers: the master L (coprogenous earth, diatomaceous earth, marl)
# or a layer origin naming limnic material. A field observation.
.sg_shg_limnic_layers <- function(h) {
  m <- .usda_desig(h$designation)$master
  lo <- tolower(as.character(.usda_col(h, "layer_origin", NA_character_)))
  (!is.na(m) & m == "L") |
    (!is.na(lo) & grepl("limnic|coprogenous|diatomaceous|marl", lo))
}

#' Limnic subgroups of the Histosols and Histels (KST 13th ed., pp. 190,
#' 200-203)
#'
#' One or more limnic layers with a total thickness of 5 cm or more within
#' the control section (of Histosols, p. 41; for the Histels the same, ending
#' 25 cm below the top of permafrost). Up to v0.9.426 \code{limnic_usda()}
#' within 130 cm and also any lacustrine layer (lacustrine mineral sediment
#' is not limnic material).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_limnic_usda <- function(pedon) {
  h <- pedon$horizons
  t <- .sg_hist_tiers(pedon)
  k <- which(.sg_shg_limnic_layers(h) & !is.na(h$top_cm) & h$top_cm < t$bottom)
  thk <- .thk_in(h, k, 0, t$bottom)
  .sg_shg_result("sg_histosols_limnic_usda", thk >= 5, k,
                 evidence = list(limnic_cm = thk, control_section_cm = t$bottom),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 10 (e.g. Limnic Haplohemists, p. 202)")
}

#' @rdname sg_histosols_limnic_usda
#' @noRd
sg_gelisols_limnic_usda <- function(pedon) {
  r <- sg_histosols_limnic_usda(pedon)
  r$name <- "sg_gelisols_limnic_usda"
  r$reference <- "Soil Survey Staff (2022), KST 13th ed., p. 190 (Limnic Hemistels)"
  r
}

# Runs of adjoining layers of mineral soil material (in depth order), TRUE and
# possible (TRUE or NA), as lists of index vectors.
.sg_shg_mineral_runs <- function(pedon) {
  h <- pedon$horizons
  mn <- .sg_shg_mineral(pedon)
  ord <- order(h$top_cm)
  list(yes = .usda_runs(mn %in% TRUE, ord), may = .usda_runs(!(mn %in% FALSE), ord))
}

#' Terric subgroups of the Histosols (KST 13th ed., pp. 199-205)
#'
#' A layer of mineral soil material 30 cm or more thick whose upper boundary
#' is within the control section, below the surface tier. One layer: a run of
#' adjoining mineral layers (rock, paralithic, limnic, water and ice layers
#' excluded), which may continue below the control section. Mineral or
#' organic is read by \code{.usda_organic_status()}: a layer of unknown kind
#' leaves the test undecided when it could complete the 30 cm. Up to
#' v0.9.426 \code{terric_usda()} summed every mineral layer starting within
#' 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_terric_usda <- function(pedon) {
  h <- pedon$horizons
  t <- .sg_hist_tiers(pedon)
  r <- .sg_shg_mineral_runs(pedon)
  ok <- function(run) {
    top <- min(h$top_cm[run])
    top >= t$surface[2] - 1e-6 && top < t$bottom && .usda_run_thickness(h, run) >= 30
  }
  yes <- Filter(ok, r$yes); may <- Filter(ok, r$may)
  p <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  .sg_shg_result("sg_histosols_terric_usda", p, unlist(yes),
                 evidence = list(zone = c(t$surface[2], t$bottom)),
                 missing = c("oc_pct", "designation"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 10 (e.g. Terric Haplohemists, p. 202)")
}

# Mineral layers within the organic soil materials (organic material above
# and below) in [a, b): TRUE when one is 5 cm or more thick or there are two
# or more; FALSE when not even the undecided layers could make it.
.sg_shg_interbedded <- function(pedon, a, b) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  mn <- .sg_shg_mineral(pedon)
  ord <- order(h$top_cm)
  first_org <- suppressWarnings(min(h$top_cm[org %in% TRUE]))
  last_org <- suppressWarnings(max(h$bottom_cm[org %in% TRUE]))
  if (!is.finite(first_org)) return(list(passed = FALSE, layers = integer(0)))
  inside <- !is.na(h$top_cm) & h$top_cm >= first_org & h$bottom_cm <= last_org &
            h$bottom_cm > a & h$top_cm < b
  count <- function(flag) {
    runs <- .usda_runs(flag & inside, ord)
    if (!length(runs)) return(FALSE)
    length(runs) >= 2L || any(vapply(runs, function(r) .usda_run_thickness(h, r) >= 5, NA))
  }
  yes <- count(mn %in% TRUE); may <- count(!(mn %in% FALSE) & !(org %in% TRUE))
  list(passed = if (yes) TRUE else if (may) NA else FALSE,
       layers = which(mn %in% TRUE & inside))
}

#' Fluvaquentic subgroups of the Histosols (KST 13th ed., pp. 199-205)
#'
#' Less than 50 cm of human-transported material in the surface horizons
#' (\code{.usda_htm_surface_cm()}), and, within the organic soil materials,
#' one layer of mineral soil material 5 cm or more thick or two or more
#' layers of any thickness in the control section, below the surface tier.
#' Up to v0.9.426 \code{fluvaquentic_usda()}, the organic-carbon test of the
#' mineral orders.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_fluvaquentic_usda <- function(pedon) {
  t <- .sg_hist_tiers(pedon)
  ib <- .sg_shg_interbedded(pedon, t$surface[2], t$bottom)
  htm <- .usda_htm_surface_cm(pedon) < 50
  .sg_shg_result("sg_histosols_fluvaquentic_usda", .sg_shg_and3(htm, ib$passed), ib$layers,
                 evidence = list(htm_under_50 = htm, mineral_layers = ib$passed),
                 missing = c("oc_pct", "designation"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 10 (e.g. Fluvaquentic Haplohemists, p. 202)")
}

#' Sphagnic Cryofibrists (KST 13th ed., p. 200)
#'
#' Three-fourths or more of the fibre volume in the surface tier derived from
#' Sphagnum: the organic layers of the surface tier, weighted by thickness,
#' with \code{sphagnum_fibres_pct} of 75 or more. The botanical origin of the
#' fibres is a field identification: not recorded, not Sphagnum (as
#' \code{.usda_moss_surface()} reads it). Up to v0.9.426
#' \code{sphagnic_usda()}, which read the rubbed fibre content (the share of
#' fibres, not of Sphagnum).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_sphagnic_usda <- function(pedon) {
  h <- pedon$horizons
  t <- .sg_hist_tiers(pedon)
  org <- .usda_organic_status(pedon)
  k <- which(org %in% TRUE & !is.na(h$top_cm) & h$top_cm < t$surface[2])
  .sg_shg_sphagnum_share(pedon, k, 0, t$surface[2], "sg_histosols_sphagnic_usda",
                         "Soil Survey Staff (2022), KST 13th ed., p. 200 (Sphagnic Cryofibrists)")
}

.sg_shg_sphagnum_share <- function(pedon, k, a, b, name, ref) {
  h <- pedon$horizons
  if (!length(k)) return(.sg_shg_result(name, FALSE, reference = ref))
  w <- pmax(pmin(h$bottom_cm[k], b) - pmax(h$top_cm[k], a), 0)
  sph <- suppressWarnings(as.numeric(.usda_col(h, "sphagnum_fibres_pct", NA_real_)))[k]
  sph[is.na(sph)] <- 0
  share <- if (sum(w) > 0) sum(w * sph) / sum(w) else 0
  .sg_shg_result(name, share >= 75, k, evidence = list(sphagnum_share_pct = share),
                 reference = ref)
}

#' Fibric, Hemic and Sapric subgroups of the Histosols (KST 13th ed., pp.
#' 200-203)
#'
#' One or more layers of the kinds named in \code{kinds} with a total
#' thickness of 25 cm or more in the control section, below the surface tier
#' (\code{.histosol_kind_subgroup()}). The Hemic Sphagnofibrists and Hemic
#' Haplofibrists count hemic and sapric materials, the Hemic Haplosaprists
#' fibric and hemic ones; up to v0.9.426 the Hemic subgroups counted hemic
#' materials only.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kinds Any of \code{"fibric"}, \code{"hemic"}, \code{"sapric"}.
#' @noRd
sg_histosols_kinds_usda <- function(pedon, kinds = "hemic") {
  r <- .histosol_kind_subgroup(pedon, unlist(kinds), "sg_histosols_kinds_usda")
  r$evidence$kinds <- unlist(kinds)
  r
}

#' Fibric and Sapric subgroups of the Wassists (KST 13th ed., pp. 203-205)
#'
#' The predominance of the suborders (items BC and BD, p. 199): a greater
#' thickness of fibric (or sapric) soil materials than of any other kind in
#' the organic parts of the subsurface tier, or of the surface and
#' subsurface tiers over a continuous mineral layer 40 cm or more thick
#' starting in the subsurface tier (\code{.histosol_predominant()}). Fibric
#' Frasiwassists add no sulfidic materials within 100 cm
#' (\code{no_sulfidic = TRUE}). Up to v0.9.426 the 25 cm rule of the Fibric
#' and Sapric subgroups of the Hemists.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind \code{"fibric"} or \code{"sapric"}.
#' @param no_sulfidic Also no sulfidic materials within 100 cm.
#' @noRd
sg_histosols_predominant_usda <- function(pedon, kind = "fibric", no_sulfidic = FALSE) {
  p <- .histosol_predominant(pedon, kind)
  v <- p$passed
  if (isTRUE(no_sulfidic)) {
    sd <- sulfidic_materials_usda(pedon, max_top_cm = 100)$passed
    v <- .sg_shg_and3(v, !sd)
  }
  .sg_shg_result("sg_histosols_predominant_usda", v,
                 evidence = list(kind = kind, thickness = p$thickness, unknown_cm = p$unknown_cm),
                 missing = c("fiber_content_rubbed_pct", "von_post_index"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., pp. 203-205 (Fibric and Sapric Wassists)")
}

#' Halic subgroups of the Haplosaprists (KST 13th ed., p. 203)
#'
#' Throughout a layer 30 cm or more thick whose upper boundary is within the
#' control section, an electrical conductivity of 30 dS/m or more (1:1,
#' \code{ec_dS_m}) for 6 months or more in normal years (the duration is not
#' recorded; a measured value is taken as lasting). One layer: a run of
#' adjoining layers. An EC not measured leaves it undecided where it could
#' complete the run. Up to v0.9.426 \code{halic_subgroup_usda()} summed any
#' layers.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_histosols_halic_usda <- function(pedon) {
  h <- pedon$horizons
  t <- .sg_hist_tiers(pedon)
  ec <- suppressWarnings(as.numeric(.usda_col(h, "ec_dS_m", NA_real_)))
  ord <- order(h$top_cm)
  ok <- function(r) min(h$top_cm[r]) < t$bottom && .usda_run_thickness(h, r) >= 30
  yes <- Filter(ok, .usda_runs(!is.na(ec) & ec >= 30, ord))
  may <- Filter(ok, .usda_runs(is.na(ec) | ec >= 30, ord))
  p <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  .sg_shg_result("sg_histosols_halic_usda", p, unlist(yes),
                 missing = "ec_dS_m",
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 203 (Halic Haplosaprists)")
}


# =====================================================================
# GELISOLS (KST 13th ed., Ch. 9, pp. 189-198)
# =====================================================================

#' Lithic subgroups of the Gelisols (KST 13th ed., pp. 189-198)
#'
#' A lithic contact within \code{max_cm} of the soil surface (\code{from =
#' "soil"}: Histels, Histoturbels, Historthels) or of the mineral soil
#' surface (\code{from = "mineral"}: the other great groups). Up to v0.9.426
#' \code{lithic_contact_usda()}, from the top of the profile in every case.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth (cm).
#' @param from \code{"mineral"} or \code{"soil"}.
#' @noRd
sg_gelisols_lithic_usda <- function(pedon, max_cm = 50, from = "mineral") {
  base <- if (identical(from, "soil")) .sg_shg_soil_top(pedon$horizons) else .sg_shg_mineral_top(pedon)
  .sg_shg_lithic(pedon, base, max_cm, "sg_gelisols_lithic_usda",
                 "Soil Survey Staff (2022), KST 13th ed., Ch. 9 (e.g. Lithic Haplorthels, p. 192)")
}

#' Glacic subgroups of the Gelisols (KST 13th ed., pp. 189-198)
#'
#' A glacic layer (\code{glacic_layer_usda()}) within 100 cm of the soil
#' surface (\code{from = "soil"}) or of the mineral soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param from \code{"mineral"} or \code{"soil"}.
#' @noRd
sg_gelisols_glacic_usda <- function(pedon, from = "mineral") {
  base <- if (identical(from, "soil")) .sg_shg_soil_top(pedon$horizons) else .sg_shg_mineral_top(pedon)
  r <- glacic_layer_usda(pedon, max_top_cm = base + 100)
  .sg_shg_result("sg_gelisols_glacic_usda", r$passed, r$layers,
                 evidence = list(from_cm = base),
                 reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 9 (e.g. Glacic Haplorthels, p. 192)")
}

#' Terric subgroups of the Histels (KST 13th ed., pp. 189-190)
#'
#' A layer of mineral soil material 30 cm or more thick within 100 cm of the
#' soil surface: a run of adjoining mineral layers with 30 cm or more within
#' that depth. Three-valued as \code{sg_histosols_terric_usda()}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_gelisols_terric_usda <- function(pedon) {
  h <- pedon$horizons
  top0 <- .sg_shg_soil_top(h)
  r <- .sg_shg_mineral_runs(pedon)
  ok <- function(run) .thk_in(h, run, top0, top0 + 100) >= 30
  yes <- Filter(ok, r$yes); may <- Filter(ok, r$may)
  p <- if (length(yes)) TRUE else if (length(may)) NA else FALSE
  .sg_shg_result("sg_gelisols_terric_usda", p, unlist(yes),
                 missing = c("oc_pct", "designation"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 189 (Terric Fibristels)")
}

#' Thapto-Humic subgroups of the Gelisols (KST 13th ed., pp. 189-194)
#'
#' Within 200 cm of the soil surface, buried O and dark-coloured A horizons
#' (moist value of 3 or less) with a combined thickness of 20 cm or more and
#' 1.0 percent or more organic carbon (a buried layer meeting the criteria of
#' a histic, mollic, umbric or melanic epipedon has them as well). Buried
#' layers are read from the suffix b (a field observation); a buried O layer
#' of organic soil material has the carbon; a moist value or organic carbon
#' not recorded in a buried A layer leaves it undecided. Up to v0.9.426
#' \code{thapto_humic_usda()} took only layers with both recorded.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_gelisols_thapto_humic_usda <- function(pedon) {
  h <- pedon$horizons
  top0 <- .sg_shg_soil_top(h)
  d <- .usda_desig(h$designation)
  org <- .usda_organic_status(pedon)
  buried <- !is.na(d$suffix) & grepl("b", d$suffix) & !is.na(h$top_cm) & h$top_cm < top0 + 200
  vm <- suppressWarnings(as.numeric(h$munsell_value_moist))
  oc <- suppressWarnings(as.numeric(h$oc_pct))
  o_lay <- !is.na(d$master) & grepl("^O", d$master)
  a_lay <- !is.na(d$master) & grepl("^A", d$master)
  okA <- .sg_shg_vec_and(vm <= 3, oc >= 1)
  okO <- ifelse(org %in% TRUE, TRUE, oc >= 1)
  ok <- ifelse(buried & o_lay, okO, ifelse(buried & a_lay, okA, FALSE))
  w <- pmax(pmin(h$bottom_cm, top0 + 200) - h$top_cm, 0); w[is.na(w)] <- 0
  yes <- sum(w[ok %in% TRUE]); may <- sum(w[!(ok %in% FALSE)])
  p <- if (yes >= 20) TRUE else if (may < 20) FALSE else NA
  .sg_shg_result("sg_gelisols_thapto_humic_usda", p, which(ok %in% TRUE),
                 evidence = list(buried_dark_cm = yes, possible_cm = may),
                 missing = c("munsell_value_moist", "oc_pct"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 189 (Thapto-Humic Fibristels) and p. 192")
}

.sg_shg_vec_and <- function(a, b) ifelse(a %in% FALSE | b %in% FALSE, FALSE,
                                         ifelse(a %in% TRUE & b %in% TRUE, TRUE, NA))

# Slope under 25 % (not recorded: read as met, as the Fluvents and the
# Cumulic subgroups of the Mollisols read it).
.sg_shg_slope25 <- function(pedon) {
  sl <- suppressWarnings(as.numeric(pedon$site$slope_pct %||% NA_real_))[1]
  is.na(sl) || sl < 25
}

#' Fluvaquentic and Fluventic subgroups of the Orthels (KST 13th ed., pp.
#' 191-194)
#'
#' A slope of less than 25 percent (not recorded, read as met), less than 50
#' cm of human-transported material in the surface horizons, the
#' organic-carbon criteria of the Fluvents (0.2 percent or more at 125 cm
#' with no contact above, or an irregular decrease from 25 cm;
#' \code{.usda_fluvent_oc()}), and for the Fluvaquentic subgroups
#' (\code{aquic = TRUE}) redox depletions with chroma of 2 or less and aquic
#' conditions within 75 cm of the mineral soil surface
#' (\code{aquic_conditions_usda()}). Up to v0.9.426 \code{fluvaquentic_usda()}
#' (no slope or HTM item, any rise of carbon) and \code{fluventic_usda()}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param aquic \code{TRUE} for the Fluvaquentic subgroups.
#' @noRd
sg_gelisols_fluventic_usda <- function(pedon, aquic = FALSE) {
  oc <- .usda_fluvent_oc(pedon)
  item_oc <- .usda_any3(c(oc$at_125, oc$irregular))
  htm <- .usda_htm_surface_cm(pedon) < 50
  aq <- if (isTRUE(aquic)) {
    ms <- .sg_shg_mineral_top(pedon)
    aquic_conditions_usda(pedon, max_top_cm = ms + 75)$passed
  } else TRUE
  p <- .sg_shg_and3(.sg_shg_slope25(pedon), htm, aq, item_oc)
  .sg_shg_result(if (isTRUE(aquic)) "sg_gelisols_fluvaquentic_usda" else "sg_gelisols_fluventic_usda",
                 p, oc$layers,
                 evidence = list(oc_125 = oc$at_125, oc_irregular = oc$irregular,
                                 htm_under_50 = htm, aquic = aq),
                 missing = "oc_pct",
                 reference = "Soil Survey Staff (2022), KST 13th ed., pp. 191-194 (e.g. Fluvaquentic Haplorthels, p. 193)")
}

#' @rdname sg_gelisols_fluventic_usda
#' @noRd
sg_gelisols_fluvaquentic_usda <- function(pedon) sg_gelisols_fluventic_usda(pedon, aquic = TRUE)

#' Fluvaquentic subgroups of the Histels (KST 13th ed., pp. 190-191)
#'
#' Less than 50 cm of human-transported material in the surface horizons,
#' and, within the organic soil materials, one layer of mineral soil material
#' 5 cm or more thick or two or more layers of any thickness within 100 cm of
#' the soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_gelisols_histel_fluvaquentic_usda <- function(pedon) {
  top0 <- .sg_shg_soil_top(pedon$horizons)
  ib <- .sg_shg_interbedded(pedon, top0, top0 + 100)
  htm <- .usda_htm_surface_cm(pedon) < 50
  .sg_shg_result("sg_gelisols_histel_fluvaquentic_usda", .sg_shg_and3(htm, ib$passed), ib$layers,
                 missing = c("oc_pct", "designation"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 190 (Fluvaquentic Fibristels)")
}

#' Kind of organic soil material of the Histels (KST 13th ed., pp. 189-190)
#'
#' A greater thickness of \code{kind} (fibric, hemic or sapric, read by
#' \code{.usda_organic_kind()}) than of any other kind of organic soil
#' material to a depth of 50 cm or to a densic, lithic or paralithic contact,
#' whichever is shallower: the great groups Fibristels and Hemistels and the
#' Hemic and Sapric Glacistels. NA when layers of unknown kind could change
#' it. Up to v0.9.426 the great groups read the SiBCS classes
#' (\code{fibrico}, \code{hemico}) and the Glacistels subgroups the SiBCS
#' \code{hemico} and \code{saprico}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param kind \code{"fibric"}, \code{"hemic"} or \code{"sapric"}.
#' @param depth_cm Default 50.
#' @noRd
sg_gelisols_histel_kind_usda <- function(pedon, kind = "fibric", depth_cm = 50) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  top0 <- .sg_shg_soil_top(h)
  b <- min(top0 + depth_cm, .usda_contact_top(pedon))
  knd <- .usda_organic_kind(pedon, org)
  th <- function(k) .thk_in(h, which(org %in% TRUE & knd %in% k), top0, b)
  unk <- .thk_in(h, which((org %in% TRUE & is.na(knd)) | is.na(org)), top0, b)
  kinds <- c("fibric", "hemic", "sapric")
  t <- vapply(kinds, th, 0); names(t) <- kinds
  others <- setdiff(kinds, kind)
  p <- if (t[kind] > max(t[others] + unk)) TRUE
       else if (t[kind] + unk <= max(t[others])) FALSE else NA
  .sg_shg_result("sg_gelisols_histel_kind_usda", unname(p),
                 evidence = list(kind = kind, thickness = t, unknown_cm = unk, zone = c(top0, b)),
                 missing = c("fiber_content_rubbed_pct", "von_post_index"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., pp. 189-190 (Fibristels, Hemistels, Glacistels)")
}

#' Sphagnic Fibristels (KST 13th ed., p. 190)
#'
#' Three-fourths or more (by volume) of the fibric soil materials derived
#' from Sphagnum to a depth of 50 cm or a densic, lithic or paralithic
#' contact: the fibric layers there, weighted by thickness, with
#' \code{sphagnum_fibres_pct} of 75 or more (not recorded, not Sphagnum).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_gelisols_sphagnic_usda <- function(pedon) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  top0 <- .sg_shg_soil_top(h)
  b <- min(top0 + 50, .usda_contact_top(pedon))
  knd <- .usda_organic_kind(pedon, org)
  k <- which(org %in% TRUE & knd %in% "fibric" & !is.na(h$top_cm) & h$top_cm < b)
  .sg_shg_sphagnum_share(pedon, k, top0, b, "sg_gelisols_sphagnic_usda",
                         "Soil Survey Staff (2022), KST 13th ed., p. 190 (Sphagnic Fibristels)")
}

#' Cumulic subgroups of the Molliturbels, Umbriturbels, Mollorthels and
#' Umbrorthels (KST 13th ed., pp. 194-198)
#'
#' A mollic (\code{epipedon = "mollic"}) or umbric epipedon 40 cm or more
#' thick with a texture class finer than loamy fine sand, and a slope of less
#' than 25 percent (not recorded, read as met). The thickness runs from the
#' top of the epipedon's layers to their bottom; the texture of every layer
#' is read by \code{.usda_texture_grades()}. Three-valued. Up to v0.9.426
#' \code{cumulic_subgroup_usda()} summed the A horizons, whether or not they
#' formed the epipedon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param epipedon \code{"mollic"} or \code{"umbric"}.
#' @noRd
sg_gelisols_cumulic_usda <- function(pedon, epipedon = "mollic") {
  h <- pedon$horizons
  e <- if (identical(epipedon, "umbric")) umbric_epipedon_usda(pedon) else mollic_epipedon_usda(pedon)
  L <- e$layers %||% integer(0)
  p <- if (isFALSE(e$passed)) FALSE else if (!length(L)) NA else {
    thk <- max(h$bottom_cm[L]) - min(h$top_cm[L])
    fine <- .usda_all3(!.usda_texture_grades(h)$lfs_or_coarser[L])
    v <- .sg_shg_and3(thk >= 40, fine, .sg_shg_slope25(pedon))
    if (isTRUE(v) && !isTRUE(e$passed)) NA else v
  }
  .sg_shg_result("sg_gelisols_cumulic_usda", p, L,
                 evidence = list(epipedon = epipedon),
                 missing = c(e$missing, "clay_pct", "sand_pct", "texture_class_usda"),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 194 (Cumulic Mollorthels)")
}

#' Aquic subgroups of the Gelisols (KST 13th ed., pp. 192-198)
#'
#' Redox features (redox depletions in the Haplorthels, concentrations in
#' the others) and aquic conditions within \code{max_cm} of the mineral soil
#' surface: 75 cm for the Aquic Haplorthels, 100 cm for the Turbels and the
#' Mollorthels and Umbrorthels (\code{aquic_conditions_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cm Depth below the mineral soil surface.
#' @noRd
sg_gelisols_aquic_usda <- function(pedon, max_cm = 100) {
  r <- sg_spodosols_aquic_usda(pedon, max_cm = max_cm)
  r$name <- "sg_gelisols_aquic_usda"
  r$reference <- "Soil Survey Staff (2022), KST 13th ed., Ch. 9 (e.g. Aquic Haplorthels, p. 193)"
  r
}

#' Sulfuric subgroups of the Aquorthels and Aquiturbels (KST 13th ed., pp.
#' 191, 196)
#'
#' A sulfuric horizon (\code{sulfuric_horizon_usda()}) or sulfidic materials
#' (\code{sulfidic_materials_usda()}) within 100 cm of the mineral soil
#' surface. Up to v0.9.426 the sulfuric horizon only, from the top of the
#' profile.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sg_gelisols_sulfuric_usda <- function(pedon) {
  ms <- .sg_shg_mineral_top(pedon)
  a <- sulfuric_horizon_usda(pedon, max_top_cm = ms + 100)
  b <- sulfidic_materials_usda(pedon, max_top_cm = ms + 100)
  .sg_shg_result("sg_gelisols_sulfuric_usda", .sg_shg_or3(a$passed, b$passed),
                 union(a$layers, b$layers),
                 reference = "Soil Survey Staff (2022), KST 13th ed., p. 191 (Sulfuric Aquorthels)")
}
