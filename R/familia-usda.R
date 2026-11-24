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
# Ten dimensions, in canonical name order (p. 355):
#   particle-size, human-altered and human-transported material,
#   mineralogy, CEC-activity, reaction, temperature, depth,
#   rupture-resistance, coatings, permanent cracks.
#
# Thresholds follow Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed.,
# Ch. 16 (soil temperature regimes) and Ch. 17 (family differentiae). Since
# v0.9.431 the particle-size class follows the control section of pp.
# 357-358 (R/familia-usda-particle-size.R); since v0.9.432 the mineralogy
# class reads the same control section and the human-altered and
# human-transported material classes their own (R/familia-usda-mineralogy.R);
# since v0.9.433 the other classes follow pp. 366-370 with their taxon gates
# and control sections (R/familia-usda-classes.R).
# =============================================================================


# v0.9.431: family_particle_size_usda() and the control section for
# particle-size classes live in R/familia-usda-particle-size.R.


# v0.9.432: family_mineralogy_usda() and family_human_material_usda() (the
# key of KST 13th ed., pp. 362-366) live in R/familia-usda-mineralogy.R.


# v0.9.433: family_cec_activity_usda(), family_reaction_usda(),
# family_temperature_regime_usda(), family_depth_class_usda() and the
# rupture-resistance, coatings and permanent-cracks classes (KST 13th ed.,
# pp. 366-370) live in R/familia-usda-classes.R.


# The family dimensions in the order of the name (KST 13th ed., p. 355).
# Each one gates itself by taxon (value NULL, "not used", when the taxon does
# not use it), so every dimension is run.
.FAMILY_DIMS_USDA <- c("particle_size", "human_material", "mineralogy",
                       "cec_activity", "reaction", "temperature_regime",
                       "depth_class", "rupture_resistance", "coatings", "cracks")
.family_dims_por_order_usda <- function(order_code = NULL) {
  .FAMILY_DIMS_USDA
}


#' Classify the USDA family (5th level) of a pedon
#'
#' Runs the applicable family-modifier dimensions and returns them as a named
#' list of \code{\link{FamilyAttribute}} objects (multi-label; each dimension
#' is orthogonal). Mirrors \code{\link{classify_sibcs_familia}}.
#'
#' The dimensions, in the order of the name (KST 13th ed., p. 355):
#' \code{particle_size}, \code{human_material}, \code{mineralogy},
#' \code{cec_activity}, \code{reaction} (calcareous, acid, nonacid, allic),
#' \code{temperature_regime}, \code{depth_class} (shallow),
#' \code{rupture_resistance} (ortstein), \code{coatings} (coated, uncoated)
#' and \code{cracks} (cracked) (since v0.9.433, pp. 366-370). Each is a
#' class, or \code{NULL}: not used in the taxon (nothing missing), or
#' undecided (the missing columns in \code{$missing},
#' \code{evidence$undecided = TRUE} for the classes of pp. 366-370).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param order_code Optional USDA order code (e.g. \code{"AF"}).
#' @param subgroup_code Optional subgroup code (reserved for refinements).
#' @param great_group,subgroup Optional great group and subgroup names (e.g.
#'        \code{"Hapludalfs"}, \code{"Arenic Hapludalfs"}). With
#'        \code{order_code} they select the control section and the
#'        particle-size classes of KST 13th ed., ch. 17 (since v0.9.431);
#'        \code{classify_usda()} supplies the taxa it assigned.
#' @param infer_temperature Opt-in (default \code{FALSE} since v0.9.415):
#'        when \code{site$soil_temperature_regime} is not recorded, suggest
#'        a temperature class from latitude and elevation. Off, the family
#'        has no temperature class until the regime is recorded.
#' @param suggest_mineralogy Opt-in (default \code{FALSE}, v0.9.432): when
#'        the mineralogy class of KST 13th ed., pp. 363-366, is undecided,
#'        suggest one from stand-ins (the Kr and Ki of the sulfuric attack,
#'        CEC / clay, CaCO3, a quartz-dominated \code{sand_mineralogy}).
#'        The suggestion sets \code{evidence$suggestion = TRUE} and keeps
#'        the missing columns; \code{classify_usda()} then warns and lowers
#'        the evidence grade to "C".
#' @return Named list of \code{\link{FamilyAttribute}} objects.
#' @references Soil Survey Staff (2022), KST 13th ed., Ch. 16--17.
#' @seealso \code{family_label_usda}, \code{\link{classify_usda}}.
#' @export
classify_usda_family <- function(pedon, order_code = NULL,
                                 subgroup_code = NULL,
                                 infer_temperature = FALSE,
                                 great_group = NULL, subgroup = NULL,
                                 suggest_mineralogy = FALSE) {
  dims <- .family_dims_por_order_usda(order_code)
  # v0.9.400: one memo scope for the length of this call (R/memo.R)
  if (.memo_open()) on.exit(.memo_close(), add = TRUE)
  out  <- list()
  if ("particle_size" %in% dims)
    out$particle_size <- family_particle_size_usda(
      pedon, order_code = order_code, great_group = great_group,
      subgroup = subgroup %||% subgroup_code)
  if ("human_material" %in% dims)
    out$human_material <- family_human_material_usda(
      pedon, order_code = order_code, great_group = great_group,
      subgroup = subgroup %||% subgroup_code)
  if ("mineralogy" %in% dims)
    out$mineralogy <- family_mineralogy_usda(
      pedon, order_code = order_code, great_group = great_group,
      subgroup = subgroup %||% subgroup_code,
      particle_size = out$particle_size, suggest = suggest_mineralogy)
  sg <- subgroup %||% subgroup_code
  # v0.9.433: the classes of pp. 366-370, gated by the taxa assigned
  if ("cec_activity" %in% dims)
    out$cec_activity <- family_cec_activity_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg,
      particle_size = out$particle_size, mineralogy = out$mineralogy)
  if ("reaction" %in% dims)
    out$reaction <- family_reaction_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg,
      particle_size = out$particle_size, mineralogy = out$mineralogy)
  if ("temperature_regime" %in% dims)
    out$temperature_regime <- family_temperature_regime_usda(
      pedon, infer = infer_temperature, order_code = order_code,
      great_group = great_group, subgroup = sg)
  if ("depth_class" %in% dims)
    out$depth_class <- family_depth_class_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg)
  if ("rupture_resistance" %in% dims)
    out$rupture_resistance <- family_rupture_resistance_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg)
  if ("coatings" %in% dims)
    out$coatings <- family_coatings_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg,
      particle_size = out$particle_size)
  if ("cracks" %in% dims)
    out$cracks <- family_cracks_usda(
      pedon, order_code = order_code, great_group = great_group, subgroup = sg)
  out
}


#' Assemble the USDA family label from family attributes
#'
#' Joins the non-NULL \code{value}s with ", " in canonical USDA order (KST
#' 13th ed., p. 355: particle-size, human-altered and human-transported
#' material, mineralogy, CEC-activity, reaction, temperature, depth,
#' rupture-resistance, coatings, permanent cracks). Undecided terms are
#' left out (reading F20).
#' The string is meant to be PREPENDED to the subgroup name.
#'
#' @param family Named list of \code{\link{FamilyAttribute}}, the return of
#'        \code{\link{classify_usda_family}}.
#' @return Single string (possibly empty).
#' @noRd
family_label_usda <- function(family) {
  order <- .FAMILY_DIMS_USDA
  present <- intersect(order, names(family))
  vals <- vapply(present, function(k) family[[k]]$value %||% NA_character_,
                 character(1))
  vals <- vals[!is.na(vals)]
  paste(vals, collapse = ", ")
}
