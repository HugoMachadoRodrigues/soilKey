# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Helpers for Histosols Suborder/GG/SG keys -- Chapter 10, pp 199-205
# =============================================================
#
# Histosols are organic soils that are NOT Gelisols (i.e. no
# permafrost within 100 cm). The 5 Suborders are distinguished by:
#   Folists  -- saturated < 30 days/yr (well drained)
#   Wassists -- water table >= 2 cm above surface for 21+ hr/day
#   Fibrists -- fibric soil materials dominate (low decomposition)
#   Saprists -- sapric soil materials dominate (high decomposition)
#   Hemists  -- hemic (intermediate)
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 10.
# =============================================================


# ---- Organic soil materials: shared readings (v0.9.418) ---------------

# The kind of organic soil material of each layer (KST 13th ed., pp. 40-41):
# "fibric", "hemic", "sapric" or NA (not organic, or not recorded). The rubbed
# fibre content decides when measured (fibric 3/4 or more by volume, sapric
# less than 1/6, hemic in between); else the von Post index (H1-H4 fibric,
# H5-H6 hemic, H7-H10 sapric); else the suffix of an O or H designation (i
# fibric, e hemic, a sapric). Up to v0.9.417 the suborders read the SiBCS
# classes (40 and 17 % fibre).
.usda_organic_kind <- function(pedon, org = .usda_organic_status(pedon)) {
  h <- pedon$horizons
  n <- nrow(h)
  fib <- suppressWarnings(as.numeric(h$fiber_content_rubbed_pct))
  vp <- suppressWarnings(as.numeric(h$von_post_index))
  d <- .desg_master(h$designation, site = pedon$site)
  d0 <- ifelse(is.na(d), "", d)
  sfx <- ifelse(grepl("^[OH][a-z]*[iea]", d0), sub("^[OH]([a-z]*?)([iea]).*$", "\\2", d0), NA)
  out <- rep(NA_character_, n)
  out <- ifelse(!is.na(fib), ifelse(fib >= 75, "fibric", ifelse(fib < 100 / 6, "sapric", "hemic")),
         ifelse(!is.na(vp), ifelse(vp <= 4, "fibric", ifelse(vp <= 6, "hemic", "sapric")),
         ifelse(!is.na(sfx), c(i = "fibric", e = "hemic", a = "sapric")[sfx], NA_character_)))
  out[!(org %in% TRUE)] <- NA_character_
  unname(out)
}

# Saturated with water for 30 days or more (cumulative) in normal years, or
# artificially drained, layer by layer: TRUE / FALSE / NA. A site recorded as
# artificially drained meets it in every layer; otherwise the layer's
# water_saturation_days decides (30 or more, or under 30); then an H master
# designation (the FAO 2006 H horizon is saturated for prolonged periods or
# drained) and, in mineral layers, aquic conditions (redoximorphic features
# with low chroma) read as saturated; otherwise undecided. The drainage class
# is not read (reading L12).
.usda_saturated30 <- function(pedon, org = .usda_organic_status(pedon)) {
  h <- pedon$horizons
  n <- nrow(h)
  if (isTRUE(.wrb_site_drained(pedon))) return(rep(TRUE, n))
  days <- suppressWarnings(as.numeric(h$water_saturation_days %||% rep(NA, n)))
  d <- .desg_master(h$designation, site = pedon$site)
  hdes <- !is.na(d) & grepl("^H", d)
  aq <- seq_len(n) %in% aquic_conditions_usda(pedon, max_top_cm = Inf)$layers & !(org %in% TRUE)
  ifelse(!is.na(days), days >= 30, ifelse(hdes | aq, TRUE, NA))
}

# Not saturated for 30 days or more and not artificially drained (the
# folistic epipedon and the Folists): the complement of .usda_saturated30().
.usda_unsaturated30 <- function(pedon, org = .usda_organic_status(pedon)) {
  s <- .usda_saturated30(pedon, org)
  !s
}

# The upper boundary (cm) of a densic, lithic or paralithic contact (R or Cr,
# or a C with the suffix d), Inf when none is recorded.
.usda_contact_top <- function(pedon) {
  h <- pedon$horizons
  d <- .desg_master(h$designation, site = pedon$site)
  k <- which(!is.na(d) & grepl("^(R|Cr|Cd)", d) & !is.na(h$top_cm))
  if (length(k)) min(h$top_cm[k]) else Inf
}

# Moss or low bulk density in the surface layers of an organic soil (KST
# 13th ed., p. 41): the organic layers within 60 cm of the surface are
# three-fourths or more Sphagnum or other moss fibres (sphagnum_fibres_pct;
# not recorded, none) or have a bulk density under 0.1 g/cm3.
.usda_moss_surface <- function(pedon, org = .usda_organic_status(pedon)) {
  h <- pedon$horizons
  k <- which(org %in% TRUE & !is.na(h$top_cm) & h$top_cm < 60)
  if (!length(k)) return(FALSE)
  sph <- suppressWarnings(as.numeric(h$sphagnum_fibres_pct %||% rep(NA, nrow(h))))[k]
  bd <- suppressWarnings(as.numeric(h$bulk_density_g_cm3))[k]
  all((!is.na(sph) & sph >= 75) | (!is.na(bd) & bd < 0.1))
}

# The control section of Histosols and its tiers (KST 13th ed., p. 41):
# 160 cm with a moss or light surface, else 130 cm, or less to a densic,
# lithic or paralithic contact or 25 cm below the top of permafrost. The
# surface tier is 0-60 cm (moss or light) or 0-30 cm; the subsurface tier the
# next 60 cm; the bottom tier the rest.
.histosol_tiers_usda <- function(pedon, org = .usda_organic_status(pedon)) {
  moss <- .usda_moss_surface(pedon, org)
  bottom <- if (moss) 160 else 130
  bottom <- min(bottom, .usda_contact_top(pedon))
  pf <- permafrost_within_usda(pedon, max_top_cm = bottom)
  if (isTRUE(pf$passed) && length(pf$layers))
    bottom <- min(bottom, min(pedon$horizons$top_cm[pf$layers]) + 25)
  surf <- min(if (moss) 60 else 30, bottom)
  sub <- min(surf + 60, bottom)
  list(moss = moss, bottom = bottom, surface = c(0, surf),
       subsurface = c(surf, sub), bottom_tier = c(sub, bottom))
}

# Thickness of the layers `k` within [a, b).
.thk_in <- function(h, k, a, b) {
  if (!length(k)) return(0)
  sum(pmax(pmin(h$bottom_cm[k], b) - pmax(h$top_cm[k], a), 0), na.rm = TRUE)
}


# ---- Histosols Order qualifier --------------------------------------

#' Histosols Order qualifier (USDA, KST 13th ed., pp. 7, 67-68)
#'
#' v0.9.418: the key to soil orders, item B. Soils that (1) do not have
#' andic soil properties in 60 percent or more of the thickness from the soil
#' surface to 60 cm or a densic, lithic or paralithic contact or duripan
#' (\code{.histosol_andic_usda()}, layer by layer with the andic test of p. 32) and (2) have organic soil materials that:
#' \enumerate{
#'   \item[a.] overlie cindery, fragmental or pumiceous materials, or fill
#'     their interstices, with a densic, lithic or paralithic contact directly
#'     below (fragmental read as 90 percent rock fragments or more); or
#'   \item[b.] with the underlying cindery, fragmental or pumiceous
#'     materials, total 40 cm or more between the soil surface and 50 cm; or
#'   \item[c.] make two-thirds or more of the soil to a densic, lithic or
#'     paralithic contact, with 10 cm or less of mineral horizons; or
#'   \item[d.] are saturated for 30 days or more per year or artificially
#'     drained (see \code{.usda_saturated30}), have an upper boundary within
#'     40 cm of the surface and total 60 cm or more (three-fourths moss fibres
#'     or a bulk density under 0.1) or 40 cm or more (other materials) within
#'     the control section of Histosols (reading L11; up to v0.9.417 the
#'     upper 100 cm).
#' }
#' Organic soil material is 12 percent organic carbon or more (p. 7); without
#' a measurement, an O or H designation reads as organic (reading L10), and a
#' measured organic carbon under 12 percent overrides it. Up to v0.9.417 the
#' test summed the layers whose designation began with O or H (missing 2O1 or
#' 4O1) within 100 cm against 40 cm, with no organic carbon and no andic
#' exclusion. Permafrost within 100 cm (a Gelisol) fails the test, as before.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13th ed., pp. 7, 67-68.
#' @noRd
histosol_qualifying_usda <- function(pedon) {
  # v0.9.400: memoised within a classification (R/memo.R)
  .memo(pedon, "histosol_qualifying_usda", list(),
        .histosol_qualifying_usda_uncached(pedon = pedon))
}

# The computation of histosol_qualifying_usda().
.histosol_qualifying_usda_uncached <- function(pedon) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., pp. 7, 67-68 (key to soil orders, item B)"
  h <- pedon$horizons
  n <- nrow(h)
  if (n == 0L) {
    return(DiagnosticResult$new(name = "histosol_qualifying_usda", passed = FALSE,
                                layers = integer(0), evidence = list(),
                                missing = character(0), reference = ref))
  }
  org <- .usda_organic_status(pedon)
  ord <- order(h$top_cm)
  cf <- suppressWarnings(as.numeric(h$coarse_fragments_pct))
  contact <- .usda_contact_top(pedon)
  # fragmental (or cindery, pumiceous) materials: 90 % rock fragments or more
  # in a layer of soil (not the rock of a contact) under organic material
  first_org <- if (any(!(org %in% FALSE))) min(h$top_cm[!(org %in% FALSE)]) else Inf
  frag <- !is.na(cf) & cf >= 90 & !(org %in% TRUE) & h$top_cm < contact &
          h$top_cm >= first_org
  above_contact <- ord[h$top_cm[ord] < contact]
  yes <- which(org %in% TRUE); may <- which(!(org %in% FALSE))
  # a. organic over fragmental materials, a contact directly below
  item_a <- if (is.finite(contact) && length(above_contact) && length(yes)) {
    .all3u(ifelse(frag[above_contact], TRUE, org[above_contact]))
  } else FALSE
  # b. organic plus the fragmental materials below it: 40 cm in 0-50 cm
  fr <- which(frag)
  b_yes <- if (length(yes)) .thk_in(h, union(yes, fr), 0, 50) else 0
  b_may <- .thk_in(h, union(may, fr), 0, 50)
  item_b <- if (b_yes >= 40) TRUE else if (b_may < 40) FALSE else NA
  # c. two-thirds of the soil to a contact, mineral horizons 10 cm or less
  item_c <- if (is.finite(contact) && contact > 0) {
    mn_yes <- .thk_in(h, which(org %in% FALSE), 0, contact)
    mn_may <- .thk_in(h, which(!(org %in% TRUE)), 0, contact)
    o_yes <- .thk_in(h, yes, 0, contact); o_may <- .thk_in(h, may, 0, contact)
    if (o_yes >= 2 / 3 * contact && mn_may <= 10) TRUE
    else if (o_may < 2 / 3 * contact || mn_yes > 10) FALSE else NA
  } else FALSE
  # d. saturated or drained, upper boundary within 40 cm, 60 or 40 cm in the
  # control section
  tiers <- .histosol_tiers_usda(pedon, org)
  cs <- tiers$bottom
  top_yes <- if (length(yes)) min(h$top_cm[yes]) else Inf
  top_may <- if (length(may)) min(h$top_cm[may]) else Inf
  d_yes <- .thk_in(h, yes, 0, cs); d_may <- .thk_in(h, may, 0, cs)
  bd <- suppressWarnings(as.numeric(h$bulk_density_g_cm3))
  kind <- .usda_organic_kind(pedon, org)
  dense <- yes[(!is.na(bd[yes]) & bd[yes] >= 0.1) | kind[yes] %in% c("hemic", "sapric")]
  need <- if (tiers$moss) 60 else if (length(yes) && setequal(dense, yes)) 40 else NA
  thick <- if (is.na(need)) { if (d_yes >= 60) TRUE else if (d_may < 40) FALSE else NA }
           else if (d_yes >= need) TRUE else if (d_may < need) FALSE else NA
  sat <- .usda_saturated30(pedon, org)
  sat_any <- .any3u(sat[may[h$top_cm[may] < cs]])
  near <- if (top_yes < 40) TRUE else if (top_may >= 40) FALSE else NA
  item_d <- sat_any & near & thick
  organic <- item_a | item_b | item_c | item_d
  aq <- .histosol_andic_usda(pedon)
  pf <- permafrost_within_usda(pedon, max_top_cm = 100)
  passed <- !aq$passed & organic
  if (isTRUE(pf$passed)) passed <- FALSE
  miss <- character(0)
  if (is.na(passed)) {
    if (anyNA(org[may])) miss <- c(miss, "oc_pct")
    if (is.na(item_d) && is.na(sat_any)) miss <- c(miss, "water_saturation_days")
    if (is.na(thick)) miss <- c(miss, "bulk_density_g_cm3")
    if (is.na(aq$passed)) miss <- c(miss, aq$missing)
  }
  DiagnosticResult$new(
    name = "histosol_qualifying_usda", passed = passed,
    layers = if (!isFALSE(passed)) may else integer(0),
    evidence = list(organic = org, items = c(a = item_a, b = item_b, c = item_c, d = item_d),
                    organic_0_50_cm = b_yes, organic_in_control_section_cm = d_yes,
                    control_section_cm = cs, andic = aq, permafrost_present = isTRUE(pf$passed)),
    missing = unique(miss), reference = ref)
}


# ---- Wassists Suborder qualifier ------------------------------------

#' Wassists Suborder qualifier (KST 13ed, Ch 10, p 203)
#'
#' Histosols having a "field-observable water table 2 cm or more
#' above the soil surface for more than 21 hours of each day in
#' all years."  Diagnostic for the Wassists suborder.
#'
#' Implementation: pass when \code{site$water_table_cm_above_surface}
#' is provided and >= 2 (positive = above surface).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
wassist_qualifying_usda <- function(pedon) {
  wt <- pedon$site$water_table_cm_above_surface %||% NA_real_
  passed <- !is.na(wt) && wt >= 2
  DiagnosticResult$new(
    name = "wassist_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(water_table_cm_above_surface = wt),
    missing = if (is.na(wt)) "site$water_table_cm_above_surface"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
  )
}


# ---- Folists Suborder qualifier (well-drained organic) --------------

#' Folists Suborder qualifier (KST 13th ed., p. 199)
#'
#' v0.9.418: Histosols saturated with water for less than 30 cumulative
#' days in normal years and not artificially drained. Read on the organic
#' layers of the control section: a site recorded as artificially drained
#' fails, a layer's \code{water_saturation_days} decides (all under 30
#' passes, one of 30 or more fails), and an H designation fails (the FAO H
#' horizon is saturated for prolonged periods); otherwise the test is
#' undecided. Up to v0.9.417 it passed whenever no aquic conditions were
#' found, so a drained peat or one with no record became a Folist. The
#' drainage class is not read (reading L12).
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
folist_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  cs <- .histosol_tiers_usda(pedon, org)$bottom
  k <- which(org %in% TRUE & !is.na(h$top_cm) & h$top_cm < cs)
  dry <- .usda_unsaturated30(pedon, org)[k]
  passed <- if (!length(k)) FALSE else .all3u(dry)
  DiagnosticResult$new(
    name = "folist_qualifying_usda", passed = passed,
    layers = if (isTRUE(passed)) k else integer(0),
    evidence = list(organic_layers = k, under_30_days = dry,
                    drained = isTRUE(.wrb_site_drained(pedon))),
    missing = if (is.na(passed)) "water_saturation_days" else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 199"
  )
}


# ---- Soil Moisture / Temperature Regimes ----------------------------

# v0.9.370: the regimes the Chinese Soil Taxonomy 2001 adds to the site
# vocabulary, as the USDA key reads them. Stagnic (perched water),
# anthrostagnic (irrigated paddy) and peraquic (water table always at or near
# the surface) are saturation regimes, read as aquic; permagelic (soil
# perennially at or below 0 C) is read as gelic. Other values are returned in
# lower case, unchanged, so no profile recorded before reads differently.
.usda_smr_value <- function(x) {
  x <- tolower(as.character(x))
  x[!is.na(x) & trimws(x) %in% c("stagnic", "anthrostagnic", "peraquic")] <- "aquic"
  x
}
.usda_str_value <- function(x) {
  x <- tolower(as.character(x))
  # v0.9.415: and pergelic, the name of the gelic regime before the 8th
  # edition of the Keys
  x[!is.na(x) & trimws(x) %in% c("permagelic", "pergelic")] <- "gelic"
  x
}

# v0.9.415: a regime is a property of the climate of the soil (KST 13th ed.,
# pp. 47-49 and 53-55), measured or estimated outside the profile. A site that
# records none leaves every regime test undecided (NA), with the site field
# listed as missing; the key never infers one (audit decisions P2 and P3, Hugo,
# 2026-11-20). Up to v0.9.414 a regime not recorded made every regime test
# FALSE, so the key fell silently into the udic catch-all of 11 orders.
.usda_regime_blank <- function(x) {
  x <- as.character(x %||% NA_character_)[1L]
  is.na(x) || !nzchar(trimws(x))
}

.USDA_SMR_TARGETS <- c("aquic", "aridic", "torric", "udic", "perudic", "ustic", "xeric")
.USDA_STR_TARGETS <- c("gelic", "cryic", "frigid", "mesic", "thermic", "hyperthermic",
                       "isofrigid", "isomesic", "isothermic", "isohyperthermic")

#' Soil moisture regime helper (USDA, KST 13ed Ch 3, pp 47-49)
#'
#' TRUE when \code{pedon$site$soil_moisture_regime} is one of
#' \code{target}, FALSE when another regime is recorded, and NA (undecided,
#' \code{site$soil_moisture_regime} listed as missing) when none is recorded:
#' the regime is never inferred in the key. Aridic and torric are the same
#' regime (KST 13th ed., p. 48). \code{target} may name several regimes
#' ("ustic or xeric", KST 13th ed., pp. 199 and 295).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param target Character, one or more of "aquic", "aridic", "torric",
#'        "udic", "perudic", "ustic", "xeric".
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
soil_moisture_regime_usda <- function(pedon, target = "udic") {
  target <- match.arg(as.character(unlist(target)), .USDA_SMR_TARGETS, several.ok = TRUE)
  smr_recorded <- pedon$site$soil_moisture_regime %||% NA_character_
  blank <- .usda_regime_blank(smr_recorded)
  smr <- if (blank) NA_character_ else trimws(.usda_smr_value(as.character(smr_recorded)[1L]))
  # Aridic and torric are synonyms in KST 13ed (p. 48).
  accept <- target
  if (any(accept %in% c("aridic", "torric"))) accept <- union(accept, c("aridic", "torric"))
  passed <- if (blank) NA else smr %in% accept
  DiagnosticResult$new(
    name = paste0("smr_", paste(target, collapse = "_or_"), "_usda"), passed = passed,
    layers = integer(0),
    evidence = list(soil_moisture_regime = smr_recorded, target = target),
    missing = if (blank) "site$soil_moisture_regime" else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 47-49"
  )
}


#' Soil temperature regime helper (USDA, KST 13ed Ch 3, pp 53-55)
#'
#' TRUE when \code{pedon$site$soil_temperature_regime} matches
#' \code{target}, FALSE when another regime is recorded, and NA (undecided,
#' \code{site$soil_temperature_regime} listed as missing) when none is
#' recorded: the regime is never inferred from latitude or elevation in the
#' key. Temperature regimes:
#' \itemize{
#'   \item "gelic":     MAST <= 0 C (1 C in Gelisols)
#'   \item "cryic":     MAST 0-8 C, cold summers, no permafrost
#'   \item "frigid":    MAST < 8 C, warmer summers
#'   \item "mesic":     MAST 8-15 C
#'   \item "thermic":   MAST 15-22 C
#'   \item "hyperthermic": MAST >= 22 C
#'   \item Plus iso- variants (low summer-winter difference)
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param target Character, one or more of the recognized regimes.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
soil_temperature_regime_usda <- function(pedon, target = "mesic") {
  target <- match.arg(as.character(unlist(target)), .USDA_STR_TARGETS, several.ok = TRUE)
  str_recorded <- pedon$site$soil_temperature_regime %||% NA_character_
  blank <- .usda_regime_blank(str_recorded)
  str <- if (blank) NA_character_ else trimws(.usda_str_value(as.character(str_recorded)[1L]))
  passed <- if (blank) NA else str %in% target
  DiagnosticResult$new(
    name = paste0("str_", paste(target, collapse = "_or_"), "_usda"), passed = passed,
    layers = integer(0),
    evidence = list(soil_temperature_regime = str_recorded, target = target),
    missing = if (blank) "site$soil_temperature_regime" else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 53-55"
  )
}


# ---- Per-regime convenience helpers (called from YAML) --------------

#' Cryic soil temperature regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
str_cryic_usda  <- function(pedon) soil_temperature_regime_usda(pedon, "cryic")

#' Aridic soil moisture regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_aridic_usda <- function(pedon) soil_moisture_regime_usda(pedon, "aridic")

#' Torric soil moisture regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_torric_usda <- function(pedon) soil_moisture_regime_usda(pedon, "torric")

#' Ustic soil moisture regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_ustic_usda  <- function(pedon) soil_moisture_regime_usda(pedon, "ustic")

#' Xeric soil moisture regime (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_xeric_usda  <- function(pedon) soil_moisture_regime_usda(pedon, "xeric")

#' Udic soil moisture regime (USDA), as the formative element "ud"
#'
#' TRUE for a udic or a perudic regime: in the names of most taxa "ud"
#' stands for either (KST 13th ed., p. 48), e.g. Udults (p. 321). Perox,
#' which needs the perudic regime itself, calls
#' \code{soil_moisture_regime_usda(target = "perudic")}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_udic_usda   <- function(pedon) soil_moisture_regime_usda(pedon, c("udic", "perudic"))

#' Perudic soil moisture regime (USDA, KST 13th ed., p. 48)
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
smr_perudic_usda <- function(pedon) soil_moisture_regime_usda(pedon, "perudic")


# ---- Sulfidic / Sulfuric helpers ------------------------------------

#' Sulfidic materials helper (USDA, KST 13ed Ch 3, p 49)
#'
#' Pass when sulfidic materials (soft, dark, sulfide-rich) are
#' present within \code{max_top_cm}. Proxy: sulfidic_s_pct >= 0.75
#' AND in a layer >= 15 cm thick.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 100.
#' @param min_thickness_cm Default 15.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
sulfidic_materials_usda <- function(pedon, max_top_cm = 100,
                                         min_thickness_cm = 15) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  miss <- character(0)
  if (all(is.na(h$sulfidic_s_pct[cand]))) miss <- "sulfidic_s_pct"
  passing <- cand[!is.na(h$sulfidic_s_pct[cand]) &
                      h$sulfidic_s_pct[cand] >= 0.75]
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "sulfidic_materials_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk, threshold = min_thickness_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 49"
  )
}


#' Sulfic Subgroup helper (Haplowassists)
#' Pass when sulfidic materials within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
sulfic_subgroup_usda <- function(pedon) {
  res <- sulfidic_materials_usda(pedon, max_top_cm = 100)
  res$name <- "sulfic_subgroup_usda"
  res
}


# ---- Halic / Frasic Subgroup helpers --------------------------------

#' Halic Subgroup helper (Haplosaprists)
#'
#' Pass when EC >= 30 dS/m through a 30+ cm layer for 6+ months
#' (KST 13ed, Ch 10). v0.8 proxy: any layer with ec_dS_m >= 30.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ec Default 30.
#' @param min_thickness_cm Default 30.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
halic_subgroup_usda <- function(pedon, min_ec = 30,
                                     min_thickness_cm = 30) {
  h <- pedon$horizons
  miss <- if (all(is.na(h$ec_dS_m))) "ec_dS_m" else character(0)
  passing <- which(!is.na(h$ec_dS_m) & h$ec_dS_m >= min_ec)
  thk <- if (length(passing) > 0L)
           sum(pmax(h$bottom_cm[passing] - h$top_cm[passing], 0),
                 na.rm = TRUE)
         else 0
  passed <- thk >= min_thickness_cm
  DiagnosticResult$new(
    name = "halic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(thickness_cm = thk),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Frasiwassists Subgroup helper (Wassists)
#'
#' Pass when ec_dS_m < 0.6 (1:5 soil:water) in all horizons within
#' 100 cm. KST 13ed, Ch 10, p 203.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_ec Default 0.6.
#' @param max_top_cm Default 100.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
frasic_qualifying_usda <- function(pedon, max_ec = 0.6,
                                        max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "frasic_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no candidate layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
    ))
  }
  ecs <- h$ec_dS_m[cand]
  miss <- if (all(is.na(ecs))) "ec_dS_m" else character(0)
  # All layers with EC must satisfy < 0.6
  passed <- !all(is.na(ecs)) && all(is.na(ecs) | ecs < max_ec)
  DiagnosticResult$new(
    name = "frasic_qualifying_usda", passed = passed, layers = cand,
    evidence = list(ec_layers = ecs, threshold = max_ec),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 203"
  )
}


# ---- Hydric / Humilluvic Subgroup helpers ---------------------------

#' Hydric Subgroup helper (Histosols Cryofibrists / Sphagnofibrists /
#' etc.)
#'
#' Pass when there is a "layer of water" within the control section.
#' Detected via designation containing "W" (water layer) or
#' \code{layer_origin == "water"}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Default 130.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
hydric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  h <- pedon$horizons
  cand <- which((!is.na(h$designation) &
                    grepl("^W(?![efr])", h$designation, perl = TRUE)) |
                   (!is.na(h$layer_origin) &
                      tolower(h$layer_origin) == "water"))
  cand <- cand[!is.na(h$top_cm[cand]) & h$top_cm[cand] < max_top_cm]
  passed <- length(cand) > 0L
  DiagnosticResult$new(
    name = "hydric_subgroup_usda", passed = passed, layers = cand,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10"
  )
}


#' Humilluvic Subgroup helper (Luvihemists)
#'
#' Pass when a horizon >= 2 cm thick has humilluvic material (humus
#' translocated from above) >= 50\% volume. v0.8 deferred (no
#' specific column). Refinement to use a humilluvic_pct column or
#' a designation marker is planned.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
humilluvic_subgroup_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "humilluvic_subgroup_usda", passed = FALSE,
    layers = integer(0),
    evidence = list(reason = "humilluvic_pct column not in schema; deferred to v0.9"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 10, p 202"
  )
}


# ---- Predominance helpers (Saprists vs Hemists vs Fibrists) ---------

# Helper: cumulative thickness of layers passing a per-layer test
.cum_layer_thickness <- function(h, layers, max_top_cm = Inf) {
  if (length(layers) == 0L) return(0)
  ok <- !is.na(h$top_cm[layers]) & h$top_cm[layers] < max_top_cm
  layers <- layers[ok]
  if (length(layers) == 0L) return(0)
  sum(pmax(h$bottom_cm[layers] - h$top_cm[layers], 0), na.rm = TRUE)
}

# v0.9.418: the kind of organic soil material that is thickest in the tiers
# the key reads (KST 13th ed., p. 199, items BC and BD): the organic parts of
# the subsurface tier, or of the surface and subsurface tiers when a
# continuous mineral layer 40 cm or more thick has its upper boundary in the
# subsurface tier. Returns TRUE when `target` is surely thicker than each
# other kind, FALSE when it surely is not, NA when layers of unknown kind (or
# not known to be organic) could change it.
.histosol_predominant <- function(pedon, target) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  tiers <- .histosol_tiers_usda(pedon, org)
  a <- tiers$subsurface[1]; b <- tiers$subsurface[2]
  # a continuous mineral layer 40 cm or more thick starting in the subsurface tier
  ord <- order(h$top_cm)
  min_run <- FALSE; run_top <- NA; run_bot <- NA
  for (i in ord) {
    if (org[i] %in% FALSE) {
      if (is.na(run_top) || abs(h$top_cm[i] - run_bot) > 1e-6) run_top <- h$top_cm[i]
      run_bot <- h$bottom_cm[i]
      if (run_top >= a && run_top < b && run_bot - run_top >= 40) min_run <- TRUE
    } else { run_top <- NA; run_bot <- NA }
  }
  if (min_run) a <- 0
  kind <- .usda_organic_kind(pedon, org)
  th <- function(k) .thk_in(h, which(org %in% TRUE & kind %in% k), a, b)
  unk <- .thk_in(h, which((org %in% TRUE & is.na(kind)) | is.na(org)), a, b)
  kinds <- c("fibric", "hemic", "sapric")
  t_sure <- vapply(kinds, th, 0)
  names(t_sure) <- kinds
  others <- setdiff(kinds, target)
  passed <- if (t_sure[target] > max(t_sure[others] + unk)) TRUE
            else if (t_sure[target] + unk <= max(t_sure[others])) FALSE else NA
  list(passed = passed, thickness = t_sure, unknown_cm = unk,
       zone = c(a, b), tiers = tiers, mineral_layer_40cm = min_run)
}

#' Sapric_predominant_usda: Saprists Suborder qualifier (KST 13th ed., p. 199)
#'
#' v0.9.418: more sapric than any other kind of organic soil material in the
#' organic parts of the subsurface tier (or of the surface and subsurface
#' tiers over a mineral layer 40 cm thick), the kinds read by fibre content,
#' von Post index or designation suffix (\code{.usda_organic_kind}). Up to
#' v0.9.417 the SiBCS classes over 0-130 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
sapric_predominant_usda <- function(pedon) {
  p <- .histosol_predominant(pedon, "sapric")
  DiagnosticResult$new(
    name = "sapric_predominant_usda", passed = p$passed, layers = integer(0),
    evidence = p,
    missing = if (is.na(p$passed)) c("fiber_content_rubbed_pct", "von_post_index") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 199"
  )
}


#' Fibric_predominant_usda: Fibrists Suborder qualifier (KST 13th ed., p. 199)
#'
#' v0.9.418: more fibric than any other kind of organic soil material in the
#' tiers of \code{sapric_predominant_usda()}, no sulfuric horizon within 50
#' cm and no sulfidic materials within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
fibric_predominant_usda <- function(pedon) {
  p <- .histosol_predominant(pedon, "fibric")
  su <- sulfuric_horizon_usda(pedon, max_top_cm = 50)$passed
  sd <- sulfidic_materials_usda(pedon, max_top_cm = 100)$passed
  passed <- p$passed & !su & !sd
  DiagnosticResult$new(
    name = "fibric_predominant_usda", passed = passed, layers = integer(0),
    evidence = c(p, list(sulfuric_50 = su, sulfidic_100 = sd)),
    missing = if (is.na(passed)) c("fiber_content_rubbed_pct", "von_post_index") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., p. 199"
  )
}


# ---- Subgroup-level fibric/hemic/sapric helpers (tier rules) -------

# v0.9.418: layers of a kind of organic soil material with a total thickness
# of 25 cm or more in the control section, below the surface tier (the
# Fibric, Hemic and Sapric subgroups of Histosols, KST 13th ed., pp.
# 199-205), the kinds of .usda_organic_kind(). Up to v0.9.417 the SiBCS
# classes in 0-130 cm.
.histosol_kind_subgroup <- function(pedon, kinds, name, max_top_cm = NULL) {
  h <- pedon$horizons
  org <- .usda_organic_status(pedon)
  tiers <- .histosol_tiers_usda(pedon, org)
  kind <- .usda_organic_kind(pedon, org)
  a <- tiers$surface[2]; b <- if (is.null(max_top_cm)) tiers$bottom else min(tiers$bottom, max_top_cm)
  k <- which(org %in% TRUE & kind %in% kinds)
  yes <- .thk_in(h, k, a, b)
  may <- .thk_in(h, union(k, which((org %in% TRUE & is.na(kind)) | is.na(org))), a, b)
  passed <- if (yes >= 25) TRUE else if (may < 25) FALSE else NA
  DiagnosticResult$new(
    name = name, passed = passed, layers = if (isTRUE(passed)) k else integer(0),
    evidence = list(thickness_cm = yes, possible_cm = may, zone = c(a, b), threshold = 25),
    missing = if (is.na(passed)) c("fiber_content_rubbed_pct", "von_post_index") else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., pp. 199-205"
  )
}

#' Fibric Subgroup helper (Hemists, Saprists, Wassists): 25 cm or more of
#' fibric materials in the control section below the surface tier.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Unused bound kept for the subgroup lists (the control
#'   section decides).
#' @noRd
fibric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  .histosol_kind_subgroup(pedon, "fibric", "fibric_subgroup_usda")
}


#' Hemic Subgroup helper: 25 cm or more of hemic materials in the control
#' section below the surface tier.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Unused bound kept for the subgroup lists.
#' @noRd
hemic_subgroup_usda <- function(pedon, max_top_cm = 130) {
  .histosol_kind_subgroup(pedon, "hemic", "hemic_subgroup_usda")
}


#' Sapric Subgroup helper: 25 cm or more of sapric materials in the control
#' section below the surface tier.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Unused bound kept for the subgroup lists.
#' @noRd
sapric_subgroup_usda <- function(pedon, max_top_cm = 130) {
  .histosol_kind_subgroup(pedon, "sapric", "sapric_subgroup_usda")
}


# Histosols item B.1 (KST 13th ed., p. 67): andic soil properties in 60 % or
# more of the thickness between the SOIL surface (organic layers included,
# unlike the Andisol window) and 60 cm or a densic, lithic or paralithic
# contact or duripan. Three-valued on the shares of andic, not andic and
# undecided layers (the andic test of p. 32 excludes 25 % organic carbon or
# more). TRUE means the exclusion applies (andic).
.histosol_andic_usda <- function(pedon) {
  h <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., p. 67 (key to soil orders, item B.1)"
  st <- .andic_layer_status_usda(h)
  d <- .desg_master(h$designation, site = pedon$site)
  top0 <- suppressWarnings(min(h$top_cm, na.rm = TRUE))
  stop_k <- which(!is.na(d) & (grepl("^(R|Cr|Cd)", d) | grepl("qm", d)) & h$top_cm > top0)
  bot <- min(c(top0 + 60, h$top_cm[stop_k]))
  w <- pmax(pmin(h$bottom_cm, bot) - pmax(h$top_cm, top0), 0)
  w[is.na(w)] <- 0
  tot <- bot - top0
  yes <- sum(w[st$status %in% TRUE]); no <- sum(w[st$status %in% FALSE])
  passed <- if (tot <= 0) FALSE else if (yes >= 0.6 * tot - 1e-9) TRUE
            else if (yes + (tot - yes - no) < 0.6 * tot - 1e-9) FALSE else NA
  und <- which(w > 0 & is.na(st$status))
  DiagnosticResult$new(
    name = "histosol_andic_usda", passed = passed,
    layers = which(w > 0 & st$status %in% TRUE),
    evidence = list(top_cm = top0, bottom_cm = bot, andic_cm = yes, not_andic_cm = no),
    missing = if (is.na(passed)) unique(unlist(st$missing[und])) else character(0),
    reference = ref)
}
