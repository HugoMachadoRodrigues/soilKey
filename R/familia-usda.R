# =============================================================================
# v0.9.104 -- USDA Soil Taxonomy family level (5th category).
#
# The USDA family is a set of class modifiers PREPENDED to the subgroup name,
# e.g. "fine, kaolinitic, isohyperthermic Rhodic Hapludox". Unlike the Order ->
# Subgroup keys (deterministic first-pass), the family is MULTI-LABEL: each
# modifier dimension is computed independently from quantitative attributes --
# exactly the structure of the SiBCS familia (R/familia-sibcs.R), which this
# file mirrors. We reuse FamilyAttribute (R6) and .weighted_avg_in_depth() from
# there, and compute_ki/compute_kr() for mineralogy.
#
# Six dimensions, in canonical name order:
#   particle-size, mineralogy, CEC-activity, reaction, temperature, [depth].
#
# Thresholds follow Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed.,
# Ch. 16 (soil temperature regimes) and Ch. 17 (family differentiae). Where the
# schema lacks fine-sand granulometry, a documented approximation by sand_pct
# is used and recorded in $evidence.
# =============================================================================


#' USDA family: particle-size class (KST Ch. 17)
#'
#' Weighted average over the family control section (25--100 cm by default):
#' fragmental / *-skeletal (>= 35 percent rock fragments) / sandy / clayey
#' (fine, very-fine) / loamy / silty (coarse-/fine-).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Control-section depth window (cm).
#' @return A \code{\link{FamilyAttribute}} (\code{value} is the class or NULL).
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch. 17.
#' @noRd
family_particle_size_usda <- function(pedon, min_cm = 25, max_cm = 100) {
  h    <- pedon$horizons
  clay <- .weighted_avg_in_depth(h, "clay_pct", max_cm, min_cm)
  sand <- .weighted_avg_in_depth(h, "sand_pct", max_cm, min_cm)
  silt <- .weighted_avg_in_depth(h, "silt_pct", max_cm, min_cm)
  frag <- .weighted_avg_in_depth(h, "coarse_fragments_pct", max_cm, min_cm)
  ref  <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
  if (is.na(clay)) {
    return(FamilyAttribute$new(
      name = "particle_size", value = NULL,
      evidence = list(reason = "clay_pct unavailable in control section"),
      missing = "clay_pct", reference = ref))
  }
  frag_v <- if (is.na(frag)) 0 else frag
  fine_earth <- if (is.na(sand) || clay >= 35) {
    if (clay >= 60) "very-fine" else if (clay >= 35) "fine" else "loamy"
  } else if (clay < 15 && sand >= 70) {
    "sandy"
  } else if (clay >= 18) {
    if (sand >= 15) "fine-loamy" else "fine-silty"
  } else {
    if (sand >= 15) "coarse-loamy" else "coarse-silty"
  }
  value <- if (frag_v > 90) {
    "fragmental"
  } else if (frag_v >= 35) {
    base <- if (clay >= 35) "clayey" else if (fine_earth == "sandy") "sandy" else "loamy"
    paste0(base, "-skeletal")
  } else {
    fine_earth
  }
  FamilyAttribute$new(
    name = "particle_size", value = value,
    evidence = list(clay_pct = clay, sand_pct = sand, silt_pct = silt,
                    coarse_fragments_pct = frag_v,
                    note = if (is.na(sand))
                      "sand_pct missing; loamy/silty split unresolved" else NULL),
    missing = character(0), reference = ref)
}


#' USDA family: mineralogy class (KST Ch. 17)
#'
#' Priority key from the available chemistry: carbonatic (CaCO3 >= 40 percent)
#' -> oxidic (Kr < 0.75) -> micaceous (sand mica dominant) -> smectitic (high
#' CEC/clay activity) -> kaolinitic (Ki <= 2.2) -> siliceous (quartzose sand)
#' -> mixed (default).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Control-section depth window (cm).
#' @return A \code{\link{FamilyAttribute}}.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 17.
#' @noRd
family_mineralogy_usda <- function(pedon, min_cm = 25, max_cm = 100) {
  h    <- pedon$horizons
  ref  <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
  clay  <- .weighted_avg_in_depth(h, "clay_pct", max_cm, min_cm)
  cec   <- .weighted_avg_in_depth(h, "cec_cmol", max_cm, min_cm)
  caco3 <- .weighted_avg_in_depth(h, "caco3_pct", max_cm, min_cm)
  sio2  <- .weighted_avg_in_depth(h, "sio2_sulfuric_pct", max_cm, min_cm)
  al2o3 <- .weighted_avg_in_depth(h, "al2o3_sulfuric_pct", max_cm, min_cm)
  fe2o3 <- .weighted_avg_in_depth(h, "fe2o3_sulfuric_pct", max_cm, min_cm)
  mica  <- .weighted_avg_in_depth(h, "sand_mica_pct", max_cm, min_cm)
  kr    <- compute_kr(sio2, al2o3, fe2o3)
  ki    <- compute_ki(sio2, al2o3)
  activ <- if (!is.na(cec) && !is.na(clay) && clay > 0) cec / clay else NA_real_
  ev <- list(caco3_pct = caco3, ki = ki, kr = kr,
             cec_to_clay = activ, sand_mica_pct = mica)
  value <- if (!is.na(caco3) && caco3 >= 40) {
    "carbonatic"
  } else if (!is.na(kr) && kr < 0.75) {
    "oxidic"
  } else if (!is.na(mica) && mica >= 40) {
    "micaceous"
  } else if (!is.na(activ) && activ >= 0.60 && !is.na(clay) && clay >= 18) {
    "smectitic"
  } else if (!is.na(ki) && ki <= 2.2) {
    "kaolinitic"
  } else if (!all(is.na(c(kr, ki, activ, caco3, mica)))) {
    "mixed"
  } else {
    NULL
  }
  FamilyAttribute$new(
    name = "mineralogy", value = value, evidence = ev,
    missing = if (is.null(value))
      c("sio2_sulfuric_pct", "al2o3_sulfuric_pct", "cec_cmol") else character(0),
    reference = ref)
}


#' USDA family: cation-exchange activity class (KST Ch. 17)
#'
#' CEC(NH4OAc, pH 7)/clay ratio: superactive (>= 0.60), active (0.40--0.60),
#' semiactive (0.24--0.40), subactive (< 0.24). Skipped for clay < 10 percent.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Control-section depth window (cm).
#' @return A \code{\link{FamilyAttribute}}.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 17.
#' @noRd
family_cec_activity_usda <- function(pedon, min_cm = 25, max_cm = 100) {
  h    <- pedon$horizons
  ref  <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
  clay <- .weighted_avg_in_depth(h, "clay_pct", max_cm, min_cm)
  cec  <- .weighted_avg_in_depth(h, "cec_cmol", max_cm, min_cm)
  if (is.na(clay) || is.na(cec)) {
    return(FamilyAttribute$new(
      name = "cec_activity", value = NULL,
      evidence = list(reason = "CEC or clay unavailable"),
      missing = c(if (is.na(cec)) "cec_cmol", if (is.na(clay)) "clay_pct"),
      reference = ref))
  }
  if (clay < 10) {
    return(FamilyAttribute$new(
      name = "cec_activity", value = NULL,
      evidence = list(reason = "too sandy for an activity class", clay_pct = clay),
      missing = character(0), reference = ref))
  }
  ratio <- cec / clay
  value <- if (ratio >= 0.60) "superactive"
           else if (ratio >= 0.40) "active"
           else if (ratio >= 0.24) "semiactive"
           else "subactive"
  FamilyAttribute$new(
    name = "cec_activity", value = value,
    evidence = list(cec_cmol = cec, clay_pct = clay, cec_to_clay_ratio = ratio),
    missing = character(0), reference = ref)
}


#' USDA family: reaction class (KST Ch. 17)
#'
#' Conservative: returns "calcareous" when carbonates are present through the
#' control section; otherwise NULL (acid/nonacid terms apply only to specific
#' families and are not emitted by default).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cm,max_cm Control-section depth window (cm).
#' @return A \code{\link{FamilyAttribute}}.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 17.
#' @noRd
family_reaction_usda <- function(pedon, min_cm = 25, max_cm = 100) {
  h     <- pedon$horizons
  ref   <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
  caco3 <- .weighted_avg_in_depth(h, "caco3_pct", max_cm, min_cm)
  ph    <- .weighted_avg_in_depth(h, "ph_h2o", max_cm, min_cm)
  value <- if (!is.na(caco3) && caco3 > 0) "calcareous" else NULL
  FamilyAttribute$new(
    name = "reaction", value = value,
    evidence = list(caco3_pct = caco3, ph_h2o = ph),
    missing = character(0), reference = ref)
}


#' USDA family: soil temperature regime (KST Ch. 16)
#'
#' Uses \code{pedon$site$soil_temperature_regime} when supplied (high
#' confidence). Otherwise, when \code{infer = TRUE}, estimates the mean annual
#' soil temperature from latitude and elevation via a crude lapse-rate model
#' and assigns frigid/mesic/thermic/hyperthermic, with an \code{iso-} prefix in
#' the low-seasonality tropics (|lat| < 23). Inferred values set
#' \code{evidence$inferred = TRUE} and record the missing site field.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param infer Infer from lat/elevation when the site field is absent.
#' @return A \code{\link{FamilyAttribute}}.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 16.
#' @noRd
family_temperature_regime_usda <- function(pedon, infer = TRUE) {
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 16"
  str <- pedon$site$soil_temperature_regime %||% NA_character_
  if (!is.na(str) && nzchar(str)) {
    return(FamilyAttribute$new(
      name = "temperature_regime", value = tolower(str),
      evidence = list(inferred = FALSE,
                      source = "site$soil_temperature_regime"),
      missing = character(0), reference = ref))
  }
  lat <- suppressWarnings(as.numeric(pedon$site$lat %||% NA))
  if (!isTRUE(infer) || is.na(lat)) {
    return(FamilyAttribute$new(
      name = "temperature_regime", value = NULL,
      evidence = list(reason = "no STR field and inference disabled/no latitude"),
      missing = "site$soil_temperature_regime", reference = ref))
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
  FamilyAttribute$new(
    name = "temperature_regime", value = value,
    evidence = list(inferred = TRUE, mast_est_c = maat, lat = lat,
                    elevation_m = elev, iso = iso),
    missing = "site$soil_temperature_regime", reference = ref)
}


#' USDA family: soil depth class for shallow soils (KST Ch. 17)
#'
#' Detects the shallowest lithic/paralithic/densic/petro contact from horizon
#' designations (R / Cr / Cd / m suffix) and emits "very-shallow" (< 25 cm) or
#' "shallow" (< 50 cm); deeper soils get no depth term (NULL).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{FamilyAttribute}}.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 17.
#' @noRd
family_depth_class_usda <- function(pedon) {
  h   <- pedon$horizons
  ref <- "Soil Survey Staff (2022), KST 13th ed., Ch. 17"
  des <- as.character(h$designation)
  is_contact <- !is.na(des) &
    grepl("^([0-9]?)(R|Cr|Cd)|m$", des, perl = TRUE) &
    !is.na(h$top_cm)
  if (!any(is_contact)) {
    return(FamilyAttribute$new(
      name = "depth_class", value = NULL,
      evidence = list(reason = "no root-limiting contact detected"),
      missing = character(0), reference = ref))
  }
  depth <- min(h$top_cm[is_contact])
  value <- if (depth < 25) "very-shallow" else if (depth < 50) "shallow" else NULL
  FamilyAttribute$new(
    name = "depth_class", value = value,
    evidence = list(contact_top_cm = depth),
    missing = character(0), reference = ref)
}


# Per-order applicable dimensions. Particle-size / mineralogy / CEC-activity /
# temperature apply broadly; reaction and depth are conditional and self-gate
# (return NULL when not applicable), so the default config applies all six.
.family_dims_por_order_usda <- function(order_code = NULL) {
  c("particle_size", "mineralogy", "cec_activity", "reaction",
    "temperature_regime", "depth_class")
}


#' Classify the USDA family (5th level) of a pedon
#'
#' Runs the applicable family-modifier dimensions and returns them as a named
#' list of \code{\link{FamilyAttribute}} objects (multi-label; each dimension
#' is orthogonal). Mirrors \code{\link{classify_sibcs_familia}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code Optional USDA order code (selects applicable dimensions).
#' @param subgroup_code Optional subgroup code (reserved for refinements).
#' @param infer_temperature Passed to
#'        \code{family_temperature_regime_usda}.
#' @return Named list of \code{\link{FamilyAttribute}} objects.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 16--17.
#' @seealso \code{family_label_usda}, \code{\link{classify_usda}}.
#' @export
classify_usda_family <- function(pedon, order_code = NULL,
                                 subgroup_code = NULL,
                                 infer_temperature = TRUE) {
  dims <- .family_dims_por_order_usda(order_code)
  out  <- list()
  if ("particle_size" %in% dims)
    out$particle_size <- family_particle_size_usda(pedon)
  if ("mineralogy" %in% dims)
    out$mineralogy <- family_mineralogy_usda(pedon)
  if ("cec_activity" %in% dims)
    out$cec_activity <- family_cec_activity_usda(pedon)
  if ("reaction" %in% dims)
    out$reaction <- family_reaction_usda(pedon)
  if ("temperature_regime" %in% dims)
    out$temperature_regime <-
      family_temperature_regime_usda(pedon, infer = infer_temperature)
  if ("depth_class" %in% dims)
    out$depth_class <- family_depth_class_usda(pedon)
  out
}


#' Assemble the USDA family label from family attributes
#'
#' Joins the non-NULL \code{value}s with ", " in canonical USDA order
#' (particle-size, mineralogy, CEC-activity, reaction, temperature, depth).
#' The string is meant to be PREPENDED to the subgroup name.
#'
#' @param family Named list of \code{\link{FamilyAttribute}}, the return of
#'        \code{\link{classify_usda_family}}.
#' @return Single string (possibly empty).
#' @noRd
family_label_usda <- function(family) {
  order <- c("particle_size", "mineralogy", "cec_activity", "reaction",
             "temperature_regime", "depth_class")
  present <- intersect(order, names(family))
  vals <- vapply(present, function(k) family[[k]]$value %||% NA_character_,
                 character(1))
  vals <- vals[!is.na(vals)]
  paste(vals, collapse = ", ")
}
