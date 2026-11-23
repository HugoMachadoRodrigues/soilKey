# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Andisols helpers -- Chapter 6, pp 117-136
# =============================================================
#
# Andisols are mineral soils with andic soil properties dominant
# in the upper part of the profile. They form predominantly from
# weathered volcanic ash and other tephra, but also from materials
# weathered from non-volcanic sources high in active Al + Fe.
#
# v0.9.417 (Soil Taxonomy audit, section 2.1, items A1 to A13, readings L1
# to L4): the andic tests are three-valued. A measurement that is not
# recorded leaves a layer undecided (NA) wherever it could change the
# answer, and the result lists what is missing.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 6.
# =============================================================


# ---- Andic soil properties per layer (KST 13th ed., p. 32) -----------

# Andic soil properties per layer, TRUE / FALSE / NA (v0.9.417). KST 13th
# ed., p. 32: less than 25 % organic carbon (item 1) and one or both of
#   item 2: bulk density at 33 kPa <= 0.90 g/cm3, phosphate retention >= 85 %
#     and Al + 1/2 Fe (oxalate) >= 2.0 %;
#   item 3: 30 % or more of the fine earth is 0.02 to 2.0 mm (3a), phosphate
#     retention >= 25 % (3b), Al + 1/2 Fe >= 0.4 % (3c), volcanic glass >= 5 %
#     (3d) and 15.625 (Al + 1/2 Fe) + glass >= 36.25 (3e).
# Readings (audit section 3, accepted by Hugo):
#   L1: phosphate retention not measured leaves the layer undecided;
#   L2: where bulk density is not measured, a pH in NaF of 9.5 or more in a
#       carbonate-free layer stands in for item 2a (the KST names pH NaF as a
#       field indicator, p. 32); it is a named proxy;
#   L3: an oven-dry bulk density (ISIS) is read as the 33 kPa one (the reader
#       does that; it can only make a layer fail);
#   item 3a: particles_002_2mm_pct where recorded; otherwise sand >= 30 %
#       passes, sand + silt < 30 % fails, and anything else is undecided.
# Organic carbon not measured in a layer whose designation is mineral (not O
# or H) meets item 1: a mineral horizon is mineral soil material.
.andic_layer_status_usda <- function(h) {
  n <- nrow(h)
  col <- function(x) h[[x]] %||% rep(NA_real_, n)
  oc <- col("oc_pct"); bd <- col("bulk_density_g_cm3")
  al <- col("al_ox_pct"); fe <- col("fe_ox_pct")
  pr <- col("phosphate_retention_pct"); vg <- col("volcanic_glass_pct")
  naf <- col("ph_naf"); caco3 <- col("caco3_pct")
  sand <- col("sand_pct"); silt <- col("silt_pct")
  p002 <- col("particles_002_2mm_pct")
  des <- h$designation %||% rep(NA_character_, n)
  alfe <- al + fe / 2
  alfe_ge <- function(t) ifelse(!is.na(alfe), alfe >= t,
                                ifelse(!is.na(al) & al >= t, TRUE, NA))
  mineral_des <- !is.na(des) & !grepl("^[0-9]*[OHL]", des)
  c1 <- ifelse(!is.na(oc), oc < 25, ifelse(mineral_des, TRUE, NA))
  naf_proxy <- is.na(bd) & !is.na(naf) & naf >= 9.5 &
               !(!is.na(caco3) & caco3 > 0)
  bd_ok <- ifelse(naf_proxy, TRUE, bd <= 0.90)
  item2 <- bd_ok & (pr >= 85) & alfe_ge(2.0)
  f3a <- ifelse(!is.na(p002), p002 >= 30,
           ifelse(!is.na(sand) & sand >= 30, TRUE,
             ifelse(!is.na(sand) & !is.na(silt) & sand + silt < 30, FALSE, NA)))
  e3 <- ifelse(!is.na(alfe) & 15.625 * alfe >= 36.25, TRUE,
               15.625 * alfe + vg >= 36.25)
  item3 <- f3a & (pr >= 25) & alfe_ge(0.4) & (vg >= 5) & e3
  status <- c1 & (item2 | item3)
  miss <- lapply(seq_len(n), function(i) {
    if (!is.na(status[i])) return(character(0))
    c(if (is.na(c1[i])) "oc_pct",
      if (is.na(bd[i]) && !naf_proxy[i]) "bulk_density_g_cm3",
      if (is.na(bd[i]) && is.na(naf[i])) "ph_naf",
      if (is.na(pr[i])) "phosphate_retention_pct",
      if (is.na(al[i])) "al_ox_pct",
      if (is.na(fe[i])) "fe_ox_pct",
      if (is.na(vg[i])) "volcanic_glass_pct",
      if (is.na(f3a[i])) "particles_002_2mm_pct")
  })
  list(status = status, item2 = item2, item3 = item3,
       naf = which(naf_proxy & item2 %in% TRUE),
       missing = miss)
}


#' Andic soil properties (USDA, KST 13ed Ch 3, p 32)
#'
#' v0.9.417: the required characteristics of KST 13th ed., p. 32, per layer
#' and three-valued (\code{.andic_layer_status_usda()}): less than 25\%
#' organic carbon and either item 2 (bulk density <= 0.90, phosphate retention
#' >= 85\%, Al + 1/2 Fe >= 2.0\%) or item 3 (30\% or more of the fine earth
#' 0.02 to 2.0 mm, phosphate retention >= 25\%, Al + 1/2 Fe >= 0.4\%,
#' volcanic glass >= 5\% and 15.625 (Al + 1/2 Fe) + glass >= 36.25). TRUE when
#' a layer has them, FALSE when every layer is ruled out by measured values,
#' NA otherwise, with the missing fields listed. Up to v0.9.416 a value not
#' recorded failed the layer, and items 1, 3a and 3e were not tested.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 32.
#' @noRd
andic_soil_properties_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "andic_soil_properties_usda", list(),
        .andic_soil_properties_usda_uncached(pedon = pedon))
}

# The computation of andic_soil_properties_usda().
.andic_soil_properties_usda_uncached <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 32"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "andic_soil_properties_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref
    ))
  }
  st <- .andic_layer_status_usda(h)
  passing <- which(st$status %in% TRUE)
  passed <- if (length(passing)) TRUE
            else if (all(st$status %in% FALSE)) FALSE
            else NA
  naf_used <- length(intersect(st$naf, passing)) > 0L
  DiagnosticResult$new(
    name = "andic_soil_properties_usda", passed = passed,
    layers = passing,
    evidence = list(passing_layers = passing, layer_status = st$status,
                    item2 = st$item2, item3 = st$item3,
                    naf_field_test = st$naf),
    missing = if (is.na(passed)) unique(unlist(st$missing)) else character(0),
    reference = if (naf_used)
                  paste(ref, "[pH NaF field indicator where bulk density is not measured]")
                else ref,
    notes = if (naf_used)
              sprintf(paste("Item 2 of layer(s) %s read with pH NaF >= 9.5 in place of",
                            "the bulk density, not measured (KST 13th ed., p. 32: a field",
                            "indicator of andic soil properties)."),
                      paste(intersect(st$naf, passing), collapse = ", "))
            else NA_character_
  )
}


# ---- The depth window of the Andisol tests (KST 13th ed., p. 68) -------

# Depth (cm) of the first densic, lithic or paralithic contact, duripan or
# petrocalcic horizon at or below `from` (Inf when none). Read from the
# designation (R, Cr, Cd, qm, km) and from duripan_usda().
.andic_contact_cm_usda <- function(pedon, from = 0) {
  h <- pedon$horizons
  des <- h$designation %||% rep(NA_character_, nrow(h))
  d <- ifelse(is.na(des), "", des)
  hit <- grepl("^[0-9]*R($|[0-9])", d) | grepl("^[0-9]*Cr", d) |
         grepl("^[0-9]*C[a-z]*d", d) | grepl("qm|km", d)
  dp <- tryCatch(duripan_usda(pedon, max_top_cm = Inf)$layers,
                 error = function(e) integer(0))
  hit[dp] <- TRUE
  tops <- h$top_cm[hit & !is.na(h$top_cm) & h$top_cm >= from]
  if (length(tops)) min(tops) else Inf
}

# Top of the Andisol depth window: the mineral soil surface or the top of an
# organic layer with andic soil properties, whichever is shallower (KST 13th
# ed., p. 68). An organic layer (O, H or L designation, or 20 % organic carbon
# or more) above the mineral surface whose andic status is undecided gives an
# alternative top, `alt`; the tests that use the window are decided only when
# both tops give the same answer.
.andic_top_usda <- function(pedon, status = NULL) {
  h <- pedon$horizons
  if (nrow(h) == 0L) return(list(top = 0, alt = 0))
  if (is.null(status)) status <- .andic_layer_status_usda(h)$status
  des <- h$designation %||% rep(NA_character_, nrow(h))
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (!is.na(des) & grepl("^[0-9]*[OHL]", des))
  mtops <- h$top_cm[!org & !is.na(h$top_cm)]
  ms <- if (length(mtops)) min(mtops) else max(h$bottom_cm, na.rm = TRUE)
  above <- org & !is.na(h$top_cm) & h$top_cm < ms
  def <- h$top_cm[above & status %in% TRUE]
  may <- h$top_cm[above & is.na(status)]
  top <- min(c(ms, def))
  alt <- min(c(top, may))
  list(top = top, alt = alt, mineral_surface = ms)
}

# Thickness (cm) of the layers with status TRUE, and of those with status NA,
# inside [from, to].
.andic_thk_usda <- function(h, ok, from, to) {
  ov <- pmax(pmin(h$bottom_cm, to) - pmax(h$top_cm, from), 0)
  ov[is.na(ov)] <- 0
  c(yes = sum(ov[ok %in% TRUE]), may = sum(ov[is.na(ok)]))
}

# The 60 % rule of the Andisols (order key item D, KST 13th ed., p. 68) and of
# the Vitrands (p. 117) for the per-layer answers `ok`: 60 % or more of the
# thickness within 60 cm of the top (.andic_top_usda()), or between the top
# and a shallower densic, lithic or paralithic contact, duripan or
# petrocalcic horizon. TRUE / FALSE / NA, with the window.
.andic_sixty_usda <- function(pedon, ok, tops) {
  h <- pedon$horizons
  one <- function(top) {
    ct <- .andic_contact_cm_usda(pedon, from = top)
    bot <- min(top + 60, ct)
    w <- max(bot - top, 0)
    th <- .andic_thk_usda(h, ok, top, bot)
    need <- 0.6 * w
    res <- if (w <= 0) FALSE
           else if (th[["yes"]] >= need) TRUE
           else if (th[["yes"]] + th[["may"]] < need) FALSE
           else NA
    list(passed = res, top = top, bottom = bot, contact_cm = ct,
         yes_cm = th[["yes"]], undecided_cm = th[["may"]], need_cm = need)
  }
  a <- one(tops$top)
  if (tops$alt < tops$top) {
    b <- one(tops$alt)
    if (!identical(a$passed, b$passed)) a$passed <- NA
    a$alt_window <- b
  }
  a
}


# ---- Andisol Order qualifier ----------------------------------------

#' Andisol Order qualifier (USDA, KST 13th ed., p. 68)
#'
#' Andic soil properties in 60\% or more of the thickness either within 60 cm
#' of the mineral soil surface or of the top of an organic layer with andic
#' soil properties, whichever is shallower, or between that top and a
#' shallower densic, lithic or paralithic contact, duripan or petrocalcic
#' horizon (order key item D, KST 13th ed., p. 68). v0.9.417: the window starts
#' at that top and stops at the contact (up to v0.9.416 it ran from the soil
#' surface to 60 cm, counting O layers and ignoring contacts), and the answer
#' is three-valued: undecided layers count toward "may" and the test is NA
#' when they could carry it over 60\%.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13th ed., p. 68.
#' @noRd
andisol_qualifying_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "andisol_qualifying_usda", list(),
        .andisol_qualifying_usda_uncached(pedon = pedon))
}

# The computation of andisol_qualifying_usda().
.andisol_qualifying_usda_uncached <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 68 (order key item D)"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "andisol_qualifying_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  asp <- andic_soil_properties_usda(pedon)
  st <- .andic_layer_status_usda(h)
  tops <- .andic_top_usda(pedon, st$status)
  w <- .andic_sixty_usda(pedon, st$status, tops)
  inwin <- which(pmin(h$bottom_cm, w$bottom) - pmax(h$top_cm, w$top) > 0)
  passing <- intersect(inwin, which(st$status %in% TRUE))
  und <- intersect(inwin, which(is.na(st$status)))
  DiagnosticResult$new(
    name = "andisol_qualifying_usda", passed = w$passed,
    layers = if (isTRUE(w$passed)) passing else integer(0),
    evidence = list(window = w, andic_layers = passing,
                    undecided_layers = und, andic = asp),
    missing = if (is.na(w$passed)) unique(unlist(st$missing[und]))
              else character(0),
    reference = ref,
    notes = asp$notes
  )
}


# ---- Aquand Suborder qualifier --------------------------------------

#' Aquands Suborder qualifier (Cap 6, p 117)
#' Pass when histic OR aquic conditions in 40-50 cm with redox
#' features. Simplified: histic OR aquic_conditions(max_top=50).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquand_qualifying_usda <- function(pedon) {
  hi <- histic_epipedon_usda(pedon)
  aq <- aquic_conditions_usda(pedon, max_top_cm = 50)
  passed <- isTRUE(hi$passed) || isTRUE(aq$passed)
  DiagnosticResult$new(
    name = "aquand_qualifying_usda", passed = passed,
    layers = c(hi$layers, aq$layers),
    evidence = list(histic = hi, aquic = aq),
    missing = aq$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117"
  )
}


# ---- Vitrand Suborder qualifier -------------------------------------

#' Vitrands qualifier (KST 13th ed., pp. 117-118)
#'
#' A 1500 kPa water retention of less than 15\% on air-dried samples and less
#' than 30\% on undried samples throughout 60\% or more of the thickness
#' within 60 cm of the mineral soil surface or of the top of an organic layer
#' with andic soil properties, or between that top and a shallower densic,
#' lithic or paralithic contact, duripan or petrocalcic horizon (suborder DF,
#' and the Vitr- great groups of the Aquands, Cryands, Torrands and Xerands).
#' v0.9.417: the window of the order test (\code{.andic_sixty_usda()}); up to
#' v0.9.416 it took every layer starting above 60 cm, whole, as the
#' denominator. \code{water_content_1500kpa} is read as the air-dried value
#' (reading L4: the ISIS value, of unknown sample state, is used here); the
#' undried criterion is tested where \code{water_content_1500kpa_undried} is
#' recorded and does not count against the layer where it is not. A layer
#' with no 1500 kPa value is undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Not used since v0.9.417 (the window is that of the
#'   order key); kept so that existing calls still work.
#' @noRd
vitrand_qualifying_usda <- function(pedon, max_top_cm = 60) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 117-118"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "vitrand_qualifying_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  wr <- h$water_content_1500kpa
  wru <- h[["water_content_1500kpa_undried"]] %||% rep(NA_real_, nrow(h))
  ok <- (wr < 15) & ifelse(is.na(wru), TRUE, wru < 30)
  ok[!is.na(wru) & wru >= 30] <- FALSE
  tops <- .andic_top_usda(pedon)
  w <- .andic_sixty_usda(pedon, ok, tops)
  inwin <- which(pmin(h$bottom_cm, w$bottom) - pmax(h$top_cm, w$top) > 0)
  DiagnosticResult$new(
    name = "vitrand_qualifying_usda", passed = w$passed,
    layers = intersect(inwin, which(ok %in% TRUE)),
    evidence = list(window = w, layer_ok = ok,
                    thk_pass_cm = w$yes_cm,
                    thk_total_cm = w$bottom - w$top),
    missing = if (is.na(w$passed)) "water_content_1500kpa" else character(0),
    reference = ref
  )
}


# ---- Hydric / Hydrudands / Hydrocryands ----------------------------

#' Hydrudands and Hydrocryands (KST 13th ed., pp. 121, 126)
#'
#' On undried samples, a 1500 kPa water retention of 100\% or more, by
#' weighted average, throughout one or more layers with a total thickness of
#' 35 cm between the mineral soil surface or the top of an organic layer with
#' andic soil properties and 100 cm below it, or throughout 60\% or more of
#' the thickness above a shallower densic, lithic or paralithic contact,
#' duripan or petrocalcic horizon (great groups DCB and DHD). v0.9.417: up to
#' v0.9.416 these great groups used the 70\% test of the Hydric subgroups.
#' Only \code{water_content_1500kpa_undried} is read: a value of unknown
#' sample state (\code{water_content_1500kpa}) leaves the test undecided
#' (reading L4).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
hydric_great_group_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 121, 126"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "hydric_great_group_andisol_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  wu <- h[["water_content_1500kpa_undried"]] %||% rep(NA_real_, nrow(h))
  t0 <- .andic_top_usda(pedon)$top
  ct <- .andic_contact_cm_usda(pedon, from = t0)
  bot <- min(t0 + 100, ct)
  need <- if (ct < t0 + 100) 0.6 * (ct - t0) else 35
  ov <- pmax(pmin(h$bottom_cm, bot) - pmax(h$top_cm, t0), 0)
  ov[is.na(ov)] <- 0
  air <- h$water_content_1500kpa
  # Reading N14 (Hugo, 2026-11-21): drying lowers the 1500 kPa retention of
  # these soils, so a value of 100 % or more on a dried sample, or on one of
  # unknown state, is a lower bound of the undried value: it counts toward a
  # pass, never toward a fail.
  lb <- wu
  use_air <- is.na(wu) & !is.na(air) & air >= 100
  lb[use_air] <- air[use_air]
  known <- which(ov > 0 & !is.na(lb))
  unknown <- which(ov > 0 & is.na(wu))
  # The best weighted average over `need` cm: the wettest layers first.
  k <- known[order(-lb[known])]
  cum <- 0; s <- 0
  for (i in k) {
    t <- min(ov[i], need - cum)
    if (t <= 0) break
    s <- s + t * lb[i]; cum <- cum + t
  }
  best <- if (cum > 0) s / cum else NA_real_
  passed <- if (need > 0 && cum >= need && best >= 100) TRUE
            else if (need <= 0 || sum(ov) < need) FALSE
            else if (length(unknown) == 0L) FALSE
            else NA
  DiagnosticResult$new(
    name = "hydric_great_group_andisol_usda", passed = passed,
    layers = if (isTRUE(passed)) k else integer(0),
    evidence = list(top_cm = t0, bottom_cm = bot, need_cm = need,
                    weighted_avg_pct = best,
                    air_dried_or_unknown = air[ov > 0]),
    missing = if (is.na(passed)) "water_content_1500kpa_undried" else character(0),
    reference = ref,
    notes = if (is.na(passed) && any(!is.na(air[ov > 0])))
              paste("water_content_1500kpa recorded, but the Hydr- great groups need",
                    "undried samples (KST 13th ed., p. 126); not read (reading L4).")
            else NA_character_
  )
}


#' Hydric (Andisols subgroups): on undried samples, a 1500 kPa water
#' retention of 70\% or more throughout a layer 35 cm or more thick within
#' 100 cm of the mineral soil surface or of the top of an organic layer with
#' andic soil properties (KST 13th ed., e.g. p. 129). v0.9.417: three-valued,
#' and the layers are clipped to that window. Reads
#' \code{water_content_1500kpa_undried} where recorded, otherwise
#' \code{water_content_1500kpa}, as up to v0.9.416.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
hydric_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 129"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "hydric_andisol_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  wu <- h[["water_content_1500kpa_undried"]] %||% rep(NA_real_, nrow(h))
  wr <- ifelse(!is.na(wu), wu, h$water_content_1500kpa)
  ok <- wr >= 70
  t0 <- .andic_top_usda(pedon)$top
  th <- .andic_thk_usda(h, ok, t0, t0 + 100)
  passed <- if (th[["yes"]] >= 35) TRUE
            else if (th[["yes"]] + th[["may"]] < 35) FALSE
            else NA
  inwin <- which(pmin(h$bottom_cm, t0 + 100) - pmax(h$top_cm, t0) > 0)
  DiagnosticResult$new(
    name = "hydric_andisol_usda", passed = passed,
    layers = intersect(inwin, which(ok %in% TRUE)),
    evidence = list(thickness_cm = th[["yes"]], undecided_cm = th[["may"]],
                    top_cm = t0),
    missing = if (is.na(passed)) "water_content_1500kpa" else character(0),
    reference = ref
  )
}


# ---- Melanic / Fulvic Andisols --------------------------------------

#' Melanic Andisols: melanic_epipedon present.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
melanic_andisol_usda <- function(pedon) {
  res <- melanic_epipedon_usda(pedon)
  res$name <- "melanic_andisol_usda"
  res
}


# The depth, thickness and organic carbon requirements of the melanic
# epipedon (KST 13th ed., p. 14), for the per-layer answers `ok`: an upper
# boundary at or within 30 cm of the top (.andic_top_usda()), and layers with
# a cumulative thickness of 30 cm or more within a total thickness of 40 cm
# that have `ok`, 4 % organic carbon or more each and 6 % or more as a
# weighted average. TRUE / FALSE / NA.
.melanic_span_usda <- function(h, ok, top) {
  oc <- h$oc_pct
  ok <- ok & (oc >= 4)
  starts <- sort(unique(c(top, h$top_cm[!is.na(h$top_cm) & h$top_cm > top &
                                           h$top_cm <= top + 30])))
  any_na <- FALSE; best <- NULL
  for (s0 in starts) {
    ov <- pmax(pmin(h$bottom_cm, s0 + 40) - pmax(h$top_cm, s0), 0)
    ov[is.na(ov)] <- 0
    yes <- which(ok %in% TRUE & ov > 0); may <- which(is.na(ok) & ov > 0)
    ty <- sum(ov[yes]); tm <- sum(ov[may])
    wavg <- if (ty > 0) sum(ov[yes] * oc[yes]) / ty else NA_real_
    if (ty >= 30 && wavg >= 6) {
      return(list(passed = TRUE, layers = yes, start_cm = s0,
                  thickness_cm = ty, oc_weighted_pct = wavg))
    }
    if (ty + tm >= 30) any_na <- TRUE
  }
  list(passed = if (any_na) NA else FALSE, layers = integer(0))
}


#' Fulvudands and Fulvicryands (KST 13th ed., pp. 121, 126)
#'
#' A layer that meets the depth, thickness and organic carbon requirements of
#' a melanic epipedon (p. 14): an upper boundary within 30 cm of the mineral
#' soil surface or of the top of an organic layer with andic soil properties,
#' and layers with a cumulative thickness of 30 cm or more within 40 cm that
#' have 6\% or more organic carbon as a weighted average and 4\% or more in
#' each. v0.9.417: up to v0.9.416 the test asked for 6\% in each layer
#' starting above 30 cm and excluded the melanic epipedon itself (the key
#' already does: these great groups come after the Melan- ones).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
fulvic_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 14, 121, 126"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "fulvic_andisol_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  t0 <- .andic_top_usda(pedon)$top
  r <- .melanic_span_usda(h, rep(TRUE, nrow(h)), t0)
  DiagnosticResult$new(
    name = "fulvic_andisol_usda", passed = r$passed, layers = r$layers,
    evidence = c(r, list(top_cm = t0)),
    missing = if (is.na(r$passed)) "oc_pct" else character(0),
    reference = ref
  )
}


# ---- Dur- great groups of the Andisols ------------------------------

#' Duraquands, Duricryands, Duritorrands, Durustands, Durudands
#' (KST 13th ed., pp. 117, 121, 124, 125, 132)
#'
#' In 75\% or more of each pedon, a coherent, pedogenically cemented horizon
#' within 100 cm of the mineral soil surface or of the top of an organic layer
#' with andic soil properties. v0.9.417: up to v0.9.416 these great groups
#' used the Duric subgroup test of the Spodosols (cemented layers in 90\% of
#' the thickness). A layer is cemented when \code{cementation_class} is weakly
#' cemented or more (reading L6) or, with no class recorded, when its
#' designation has an m; a class recorded as none rules it out, and no
#' cementation recorded is absent (a field observation). Its share of the
#' pedon is \code{duripan_pct} (the ISIS reader writes there the share of a
#' cemented pan from its continuity) or \code{lateral_cover_pct}; a cemented
#' layer with no share recorded is undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
duric_great_group_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 117, 121, 124, 125, 132"
  n <- nrow(h)
  if (n == 0L) {
    return(DiagnosticResult$new(
      name = "duric_great_group_andisol_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  t0 <- .andic_top_usda(pedon)$top
  cem <- tolower(h$cementation_class %||% rep(NA_character_, n))
  des <- h$designation %||% rep(NA_character_, n)
  cemented <- ifelse(!is.na(cem),
                     cem %in% c("weakly", "moderately", "strongly", "indurated"),
                     !is.na(des) & grepl("^[0-9]*[A-Z][A-Za-z]*m", des))
  share <- h$duripan_pct %||% rep(NA_real_, n)
  lat <- h[["lateral_cover_pct"]] %||% rep(NA_real_, n)
  share <- ifelse(is.na(share), lat, share)
  inwin <- !is.na(h$top_cm) & h$top_cm < t0 + 100 &
           (is.na(h$bottom_cm) | h$bottom_cm > t0)
  ok <- cemented & inwin & ifelse(is.na(share), NA, share >= 75)
  ok[!(cemented & inwin)] <- FALSE
  passed <- if (any(ok %in% TRUE)) TRUE else if (all(ok %in% FALSE)) FALSE else NA
  DiagnosticResult$new(
    name = "duric_great_group_andisol_usda", passed = passed,
    layers = which(ok %in% TRUE),
    evidence = list(top_cm = t0, cemented = which(cemented & inwin),
                    share_pct = share),
    missing = if (is.na(passed)) "duripan_pct" else character(0),
    reference = ref
  )
}


# ---- Acric / Acraquoxic / Acrudoxic Andisols ------------------------

#' Acric Subgroup helper (Andisols Acrudoxic / Acraquoxic /
#' Acrustoxic / etc.)
#'
#' Pass when the sum of extractable bases (NH4OAc) plus 1N KCl-Al
#' is < 2.0 cmol(+)/kg in fine earth, in a 30+ cm layer between
#' 25 and 100 cm. v0.8 proxy: ECEC <= 2.0 in B horizons.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
acric_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 100)
  ec  <- h$ecec_cmol[cand]
  miss <- if (all(is.na(ec))) "ecec_cmol" else character(0)
  passing <- cand[!is.na(ec) & ec < 2.0]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 30
  DiagnosticResult$new(
    name = "acric_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Alic Subgroup (high KCl-extractable Al) ------------------------

#' Alic Subgroup helper (Andisols)
#' Pass when al_kcl_cmol > 2.0 in a 10+ cm layer between 25 and 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alic_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm >= 25 & h$top_cm < 50)
  al <- h$al_kcl_cmol[cand]
  miss <- if (all(is.na(al))) "al_kcl_cmol" else character(0)
  passing <- cand[!is.na(al) & al > 2.0]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 10
  DiagnosticResult$new(
    name = "alic_andisol_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Pachic Subgroup helper (thick mollic/umbric, 50+ cm) -----------

#' Pachic Subgroup helper (Andisols, Mollisols)
#'
#' v0.9.421: a mollic epipedon 50 cm or more thick with a texture class
#' finer than loamy fine sand (the Pachic Mollisols, e.g. KST 13th ed., p.
#' 265; the 40 cm of the frigid Pachic Hapludolls is left to the subgroup
#' lists), read from the thickness of \code{mollic_epipedon_usda()} (KST pp.
#' 14-16), or an umbric epipedon 50 cm or more thick. Three-valued: NA where
#' the mollic epipedon may or may not reach 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
pachic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  e <- mo$evidence
  mo50 <- if (isFALSE(mo$passed)) FALSE else {
    thick <- if ((e$thickness_cm %||% 0) >= 50 && isTRUE(mo$passed)) TRUE
             else if ((e$thickness_possible_cm %||% 0) < 50) FALSE else NA
    fine <- if (isFALSE(thick)) FALSE else .mo_not(e$texture_lfs_or_coarser %||% NA)
    .mo_and(thick, fine)
  }
  ul <- um$layers
  uthk <- if (isTRUE(um$passed) && length(ul) > 0L)
            sum(pmax(h$bottom_cm[ul] - h$top_cm[ul], 0), na.rm = TRUE) else 0
  passed <- .mo_or(mo50, uthk >= 50)
  layers <- if (isTRUE(mo50)) mo$layers else if (uthk >= 50) ul else integer(0)
  DiagnosticResult$new(
    name = "pachic_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = list(mollic_thickness_cm = e$thickness_cm %||% 0,
                    mollic_thickness_possible_cm = e$thickness_possible_cm %||% 0,
                    mollic_50 = mo50, umbric_thickness_cm = uthk),
    missing = if (is.na(passed)) mo$missing else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., pp. 14-16, 265"
  )
}


# ---- Thaptic Subgroup helper (buried dark layer 25-100 cm) ----------

#' Thaptic Subgroup helper (Andisols, KST 13th ed., pp. 118-136)
#'
#' At a depth between 25 and 100 cm from the mineral soil surface or the top
#' of an organic layer with andic soil properties, a layer 10 cm or more thick
#' with more than 3.0\% organic carbon and the colours of a mollic epipedon
#' throughout (value 3 or less moist and 5 or less dry, chroma 3 or less
#' moist), underlying one or more horizons with a total thickness of 10 cm or
#' more whose moist colour value is 1 unit or more higher and whose organic
#' carbon is 1\% (absolute) or more lower. v0.9.417: up to v0.9.416 the
#' overlying lighter, poorer horizons, the chroma and the dry value were not
#' tested. Three-valued: a colour or carbon not recorded leaves the layer
#' undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
thaptic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 118 (e.g. Thaptic Duraquands)"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "thaptic_subgroup_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0), reference = ref))
  }
  t0 <- .andic_top_usda(pedon)$top
  oc <- h$oc_pct; vm <- h$munsell_value_moist; cm <- h$munsell_chroma_moist
  vd <- h$munsell_value_dry
  res <- logical(0); hits <- integer(0)
  for (i in seq_len(nrow(h))) {
    th <- min(h$bottom_cm[i], t0 + 100) - max(h$top_cm[i], t0 + 25)
    if (is.na(th) || th < 10) next
    dark <- (oc[i] > 3) & (vm[i] <= 3) & (cm[i] <= 3) & (vd[i] <= 5)
    if (isFALSE(dark)) next
    up <- which(!is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[i] &
                h$bottom_cm > t0)
    lt <- (vm[up] >= vm[i] + 1) & (oc[up] <= oc[i] - 1)
    tk <- pmax(h$bottom_cm[up] - pmax(h$top_cm[up], t0), 0)
    yes <- sum(tk[lt %in% TRUE]); may <- sum(tk[is.na(lt)])
    over <- if (yes >= 10) TRUE else if (yes + may < 10) FALSE else NA
    r <- dark & over
    res <- c(res, r)
    if (isTRUE(r)) hits <- c(hits, i)
  }
  passed <- if (length(hits)) TRUE
            else if (all(res %in% FALSE)) FALSE
            else NA
  DiagnosticResult$new(
    name = "thaptic_subgroup_usda", passed = passed, layers = hits,
    evidence = list(top_cm = t0),
    missing = if (is.na(passed))
                c("oc_pct", "munsell_value_moist", "munsell_chroma_moist",
                  "munsell_value_dry")
              else character(0),
    reference = ref
  )
}


# ---- Eutric Subgroup helper -----------------------------------------

#' Eutric Subgroup helper (Andisols)
#' Pass when base_saturation (sum-of-cations) >= 50\% in some part.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
eutric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  bs <- h$bs_pct
  passed <- any(!is.na(bs) & bs >= 50)
  DiagnosticResult$new(
    name = "eutric_subgroup_usda", passed = passed,
    layers = which(!is.na(bs) & bs >= 50),
    evidence = list(bs_pct = bs),
    missing = if (all(is.na(bs))) "bs_pct" else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Vitric Subgroup helper (proxy via volcanic_glass_pct) ----------

#' Vitric Subgroup helper (Andisols)
#' Pass when volcanic_glass_pct >= 30 in a 25+ cm layer within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vitric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  vg <- h$volcanic_glass_pct[cand]
  miss <- if (all(is.na(vg))) "volcanic_glass_pct" else character(0)
  passing <- cand[!is.na(vg) & vg >= 30]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= 25
  DiagnosticResult$new(
    name = "vitric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Spodic Andisols Subgroup helper --------------------------------

#' Spodic-Andisols Subgroup helper
#' Pass when albic horizon overlies a cambic OR spodic horizon,
#' OR when a spodic horizon is present in 50\%+ of the pedon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
spodic_andisol_usda <- function(pedon) {
  sp <- spodic_horizon_usda(pedon)
  al <- albic_horizon_usda(pedon)
  passed <- isTRUE(sp$passed) ||
              (isTRUE(al$passed) &&
                 isTRUE(.cambic_impl(pedon)$passed))   # v0.9.310: USDA reading
  DiagnosticResult$new(
    name = "spodic_andisol_usda", passed = passed,
    layers = c(sp$layers, al$layers),
    evidence = list(spodic = sp, albic = al),
    missing = unique(c(sp$missing %||% character(0),
                          al$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 6"
  )
}


# ---- Humic Andisols Subgroup helper ---------------------------------

#' Humic Andisols Subgroup helper (KST 13th ed., pp. 122-136)
#'
#' A mollic or umbric epipedon (e.g. Humic Vitricryands, p. 124) or, with
#' \code{melanic = TRUE}, a melanic, mollic or umbric epipedon (Humic
#' Durustands, Haplustands, Udivitrands, Ustivitrands and Vitrixerands, e.g.
#' p. 134). v0.9.417: the melanic epipedon of the second wording, and
#' three-valued (an undecided epipedon leaves the test undecided).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param melanic Whether a melanic epipedon also qualifies.
#' @noRd
humic_andisol_usda <- function(pedon, melanic = FALSE) {
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  me <- if (isTRUE(melanic)) melanic_epipedon_usda(pedon) else NULL
  ps <- c(mo$passed, um$passed, if (!is.null(me)) me$passed)
  passed <- if (any(ps %in% TRUE)) TRUE else if (all(ps %in% FALSE)) FALSE else NA
  DiagnosticResult$new(
    name = "humic_andisol_usda", passed = passed,
    layers = unique(c(mo$layers, um$layers, if (!is.null(me)) me$layers)),
    evidence = list(mollic = mo, umbric = um, melanic = me),
    missing = if (is.na(passed))
                unique(c(mo$missing, um$missing, if (!is.null(me)) me$missing))
              else character(0),
    reference = if (isTRUE(melanic))
                  "Soil Survey Staff (2022), KST 13th ed., p. 134"
                else "Soil Survey Staff (2022), KST 13th ed., p. 124"
  )
}


# ---- Ultic Andisols Subgroup helper ---------------------------------

#' Ultic subgroups of the Andisols (KST 13th ed., pp. 124, 129)
#'
#' An argillic or kandic horizon within 125 cm of the mineral soil surface or
#' of the top of an organic layer with andic soil properties, and a base
#' saturation (by sum of cations) of less than 35\% throughout the upper 50 cm
#' of it (or throughout it, if it is thinner). v0.9.417: up to v0.9.416 the
#' Ultic subgroups of the Andisols tested only the argillic or kandic horizon
#' (within 200 cm). soilKey's \code{bs_pct} is read as the base saturation; a
#' layer without it is undecided.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ultic_andisol_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 129"
  t0 <- if (nrow(h)) .andic_top_usda(pedon)$top else 0
  ak <- argillic_or_kandic_usda(pedon)
  lay <- ak$layers[!is.na(h$top_cm[ak$layers]) & h$top_cm[ak$layers] < t0 + 125]
  if (!isTRUE(ak$passed) || length(lay) == 0L) {
    return(DiagnosticResult$new(
      name = "ultic_andisol_usda",
      passed = if (is.na(ak$passed)) NA else FALSE,
      layers = integer(0), evidence = list(argillic_or_kandic = ak),
      missing = if (is.na(ak$passed)) ak$missing else character(0),
      reference = ref))
  }
  top <- min(h$top_cm[lay])
  k <- lay[h$top_cm[lay] < top + 50]
  bs <- h$bs_pct[k]
  passed <- if (any(!is.na(bs) & bs >= 35)) FALSE
            else if (all(!is.na(bs))) TRUE
            else NA
  DiagnosticResult$new(
    name = "ultic_andisol_usda", passed = passed,
    layers = if (isTRUE(passed)) k else integer(0),
    evidence = list(argillic_or_kandic = ak, upper_layers = k, bs_pct = bs),
    missing = if (is.na(passed)) "bs_pct" else character(0),
    reference = ref
  )
}
