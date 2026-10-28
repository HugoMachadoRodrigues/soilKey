# =============================================================================
# v0.9.293 -- the New Zealand Soil Classification, 4th edition (Manaaki
# Whenua 2026; Quarto source of 7 October 2026), keyed to the 15 orders of
# the Key to Soil Orders; v0.9.298 adds the 77 groups of the keys to groups
# (R/diagnostics-nzsc-groups.R, inst/rules/nzsc4/groups/). Subgroups follow.
# Experimental and opt-in: classify_all() runs it only when asked. The Cryic
# Soils of the appendix (Antarctic soils) are not keyed.
# =============================================================================

#' Classify a pedon under the New Zealand Soil Classification
#'
#' Keys a \code{\link{PedonRecord}} through the New Zealand Soil
#' Classification, 4th edition (Manaaki Whenua - Landcare Research 2026),
#' following its Key to Soil Orders and keys to groups. \strong{Experimental}:
#' this release keys the 15 orders (Organic, Gley, Ultic, Podzol, Allophanic,
#' Pumice, Melanic, Semiarid, Oxidic, Granular, Pallic, Brown, Anthropic,
#' Recent, Raw Soils) and their 77 groups (since 0.9.298, e.g. Orthic Brown
#' Soils, Perch-gley Pallic Soils); subgroups follow. The Cryic Soils of the
#' appendix are not keyed.
#'
#' The NZSC reads horizon notations and field tests. The diagnostics read
#' the New Zealand notation (Ah, E, Bw, Bt, Bg, Bh, Bs, Bo, Bx, Bw(g), Bhm,
#' Bsm, Blm, CR, R, Of, Om, Oh, Lm, Lh, an e prefix for anthropic soil
#' material; FAO designations are read by the same letters), moist Munsell
#' colours, texture (\code{field_texture} words or codes, or lab clay, silt
#' and sand), pH, P retention (\code{phosphate_retention_pct}), dry bulk
#' density, volcanic glass, coarse fragments and their origin, structure,
#' clay coatings, carbonate, ESP, and the field tests \code{naf_test}
#' (reactive-aluminium test: none, slight, moderate, strong),
#' \code{childs_test}, \code{sensitivity_class}, \code{fluidity_class},
#' \code{stickiness} and \code{strength_moist}. From the site it reads
#' \code{drainage_class}, \code{parent_material} (Melanic Soils),
#' \code{anthropic_truncation} and \code{overthickened_a} (the note to the
#' key on overthickened A horizons), and \code{designation_system}. The
#' groups also read the packing (\code{packing_degree}), Ksat
#' (\code{ksat_mm_h}), penetration resistance, where the redox segregations
#' are (\code{redox_segregations_location}), sulfate S, roots, a Maori
#' cultural layer (\code{maori_cultural_layer}) and the colour of coatings;
#' and, on the site, \code{soil_temp_30cm_C}, \code{mean_annual_air_temp_C},
#' \code{surface_water}, \code{methane_odour},
#' \code{mechanical_mixing_depth_cm} and \code{artificially_drained}. Features
#' a describer records when met (stratification, nodules, cracks, refuse or
#' cultural layers, permafrost, surface water) are absent when not recorded.
#' The choices where the text is silent are listed in
#' \code{system.file("rules", "nzsc4", "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("nzsc4")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"nzsc4"}, the deepest class in \code{name}, the order in
#'   \code{rsg_or_order}, the classes of each level in \code{levels}, the key
#'   trace, and \code{provisional_because} (classes before the assigned one
#'   that could not be decided).
#' @references Manaaki Whenua - Landcare Research (2026). New Zealand Soil
#'   Classification. 4th ed. \doi{10.7931/dl1-lrss-4-2026}.
#' @seealso \code{\link{make_nzsc4_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_nzsc4(make_nzsc4_example("brown"), on_missing = "silent")
#' res$name
#' @export
classify_nzsc4 <- function(pedon, rules = NULL,
                           on_missing = c("warn", "silent", "error"),
                           gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cssc_prepare_pedon(pedon)
  rules <- rules %||% load_rules("nzsc4")
  # the note on overthickened A horizons: such a soil enters the key at
  # Recent Soils
  entry <- inherits(pedon, "PedonRecord") && .nzsc_overthickened_entry(pedon)
  if (entry) rules$orders <- Filter(function(t) t$code %in% c("R", "W"), rules$orders)
  res <- classify_hierarchical("nzsc4", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (entry) {
    res$warnings <- c(res$warnings, paste(
      "An overthickened A horizon (site$overthickened_a) with its base at 45 cm or more:",
      "the key was entered at Recent Soils, as the note to the Key to Soil Orders says."))
  }
  if (inherits(pedon, "PedonRecord") &&
      all(is.na(pedon$horizons$designation %||% NA) | !nzchar(trimws(pedon$horizons$designation %||% "")))) {
    res$warnings <- c(res$warnings, paste(
      "No horizon designations: the NZSC reads many diagnostic horizons from the notation",
      "(Bw, Bt, Bg, Bh, Bs, Bo ...), which are then undecided."))
  }
  res
}


#' Example pedons for the New Zealand orders
#'
#' Small, plausible profiles written for the tests and examples, one per
#' order of the New Zealand Soil Classification, with designations in the
#' New Zealand notation and the properties the order key reads. Most key to
#' the last (Orthic or default) group of their order. They are not real
#' profiles.
#'
#' @param order One of \code{"organic"}, \code{"gley"}, \code{"ultic"},
#'   \code{"podzol"}, \code{"allophanic"}, \code{"pumice"},
#'   \code{"melanic"}, \code{"semiarid"}, \code{"oxidic"},
#'   \code{"granular"}, \code{"pallic"}, \code{"brown"},
#'   \code{"anthropic"}, \code{"recent"}, \code{"raw"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_nzsc4_example("pallic")
#' p$horizons[, c("designation", "top_cm", "bottom_cm")]
#' @export
make_nzsc4_example <- function(order = c("organic", "gley", "ultic", "podzol", "allophanic", "pumice",
                                         "melanic", "semiarid", "oxidic", "granular", "pallic", "brown",
                                         "anthropic", "recent", "raw")) {
  order <- match.arg(order)
  well <- list(id = paste0("nzsc4-", order), country = "NZ", designation_system = "nzsc",
               drainage_class = "well drained")
  mk <- function(h, site = well) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = site, horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  site <- function(...) utils::modifyList(well, list(...))
  switch(order,
    organic = mk(list(
      designation = c("Of", "Om", "Oh", "C"), top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 150),
      field_texture = c("peat", "peat", "peat", "silt loam"), oc_pct = c(45, 40, 35, 2),
      munsell_hue_moist = c("7.5YR", "7.5YR", "10YR", "5Y"), munsell_value_moist = c(3, 2, 2, 5),
      munsell_chroma_moist = c(3, 2, 1, 1), ph_h2o = c(4.5, 4.6, 4.8, 5.5)),
      site(drainage_class = "very poorly drained")),
    gley = mk(list(
      designation = c("Ap", "Bg1", "Bg2", "BCg", "Cg"), top_cm = c(0, 20, 45, 75, 100),
      bottom_cm = c(20, 45, 75, 100, 130), field_texture = c("silt loam", "silty clay loam", "silty clay loam", "silt loam", "silt loam"),
      clay_pct = c(22, 30, 32, 24, 20), silt_pct = c(60, 58, 56, 62, 64), sand_pct = c(18, 12, 12, 14, 16),
      munsell_hue_moist = c("10YR", "5Y", "5Y", "5Y", "5Y"), munsell_value_moist = c(3, 5, 5, 6, 6),
      munsell_chroma_moist = c(2, 1, 1, 1, 1), redoximorphic_features_pct = c(5, 20, 20, 10, 5),
      structure_grade = c("moderate", "weak", "weak", "structureless", "structureless"),
      structure_type = c("granular", "prismatic", "prismatic", "massive", "massive"),
      ph_h2o = c(5.6, 5.8, 6.0, 6.2, 6.3), oc_pct = c(4.5, 0.8, 0.4, 0.3, 0.2),
      phosphate_retention_pct = c(25, 15, 12, 10, 10), childs_test = c(NA, "positive", "positive", NA, NA),
      naf_test = c("slight", "none", "none", "none", "none"), volcanic_glass_pct = 0, coarse_fragments_pct = 0,
      bulk_density_g_cm3 = c(1.1, 1.3, 1.35, 1.4, 1.4)),
      site(drainage_class = "poorly drained")),
    ultic = mk(list(
      designation = c("Ah", "E", "Bt1", "Bt2", "BC"), top_cm = c(0, 10, 25, 60, 115),
      bottom_cm = c(10, 25, 60, 115, 140), field_texture = c("loam", "silt loam", "clay", "clay", "clay loam"),
      clay_pct = c(20, 18, 48, 50, 36), silt_pct = c(40, 50, 28, 26, 30), sand_pct = c(40, 32, 24, 24, 34),
      munsell_hue_moist = c("10YR", "10YR", "7.5YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 5, 5, 6),
      munsell_chroma_moist = c(2, 3, 6, 6, 4),
      structure_grade = c("moderate", "weak", "moderate", "moderate", "weak"),
      structure_type = c("granular", "platy", "blocky", "prismatic", "blocky"),
      clay_films_amount = c(NA, NA, "common", "many", "few"), strength_moist = c("weak", "weak", "firm", "very firm", "firm"),
      ph_h2o = c(4.8, 4.9, 5.0, 5.1, 5.2), oc_pct = c(5, 1, 0.5, 0.3, 0.2), phosphate_retention_pct = c(30, 20, 25, 25, 20))),
    podzol = mk(list(
      designation = c("Lm", "Ah", "E", "Bh", "Bs", "C"), top_cm = c(-3, 0, 8, 30, 35, 60),
      bottom_cm = c(0, 8, 30, 35, 60, 110), field_texture = c(NA, "sandy loam", "loamy sand", "sandy loam", "sandy loam", "loamy sand"),
      munsell_hue_moist = c("7.5YR", "10YR", "10YR", "5YR", "7.5YR", "2.5Y"), munsell_value_moist = c(2, 3, 6, 3, 5, 6),
      munsell_chroma_moist = c(2, 1, 2, 3, 6, 4), oc_pct = c(40, 6, 0.6, 3, 1.2, 0.2),
      ph_h2o = c(4.2, 4.3, 4.5, 4.4, 4.8, 5.0), naf_test = c(NA, "slight", "none", "strong", "strong", "moderate"))),
    allophanic = mk(list(
      designation = c("Ah", "Bw1", "Bw2", "C"), top_cm = c(0, 20, 50, 95), bottom_cm = c(20, 50, 95, 130),
      field_texture = c("silt loam", "silt loam", "silt loam", "sandy loam"),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(2, 4, 5, 6),
      munsell_chroma_moist = c(2, 4, 6, 4), structure_grade = c("moderate", "weak", "weak", "structureless"),
      structure_type = c("granular", "blocky", "blocky", "massive"), phosphate_retention_pct = c(88, 95, 96, 70),
      bulk_density_g_cm3 = c(0.6, 0.7, 0.75, 1.0), naf_test = "strong", ph_h2o = c(5.6, 5.9, 6.0, 6.1),
      oc_pct = c(8, 3, 1.5, 0.4))),
    pumice = mk(list(
      designation = c("Ap", "Bw", "BC", "C"), top_cm = c(0, 20, 35, 55), bottom_cm = c(20, 35, 55, 120),
      field_texture = c("sandy loam", "loamy sand", "loamy sand", "sand"), sand_pct = c(55, 70, 75, 85),
      silt_pct = c(35, 24, 20, 12), clay_pct = c(10, 6, 5, 3), volcanic_glass_pct = c(45, 60, 65, 70),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(3, 5, 6, 7),
      munsell_chroma_moist = c(2, 6, 4, 2), structure_grade = c("weak", "weak", "structureless", "structureless"),
      structure_type = c("granular", "blocky", "single grain", "single grain"),
      phosphate_retention_pct = c(55, 60, 40, 20), bulk_density_g_cm3 = c(0.9, 0.85, 0.95, 1.0),
      rock_origin = c(NA, NA, "rhyolitic tephra", "rhyolitic pumice"), ph_h2o = c(5.7, 6.0, 6.1, 6.2),
      oc_pct = c(4, 1, 0.4, 0.1))),
    melanic = mk(list(
      designation = c("Ah", "Bw", "Bk", "R"), top_cm = c(0, 22, 50, 75), bottom_cm = c(22, 50, 75, 90),
      field_texture = c("clay loam", "clay", "clay loam", NA), clay_pct = c(34, 45, 36, NA),
      silt_pct = c(36, 30, 34, NA), coarse_fragments_pct = c(2, 10, 25, NA),
      munsell_hue_moist = c("10YR", "10YR", "10YR", NA), munsell_value_moist = c(2, 3, 5, NA),
      munsell_chroma_moist = c(1, 2, 3, NA), structure_grade = c("strong", "moderate", "moderate", NA),
      structure_type = c("granular", "blocky", "blocky", NA), ped_size_max_mm = c(5, 20, 20, NA),
      stickiness = c("moderately sticky", "very sticky", "moderately sticky", NA),
      effervescence_class = c("none", "slight", "strong", NA), ph_h2o = c(6.8, 7.2, 8.0, NA),
      rock_origin = c(NA, NA, NA, "limestone"), oc_pct = c(6, 1.5, 0.6, NA), phosphate_retention_pct = c(35, 30, 20, NA),
      sand_pct = c(30, 25, 30, NA), volcanic_glass_pct = c(0, 0, 0, NA))),
    semiarid = mk(list(
      designation = c("Ap", "Bw", "Bk", "C"), top_cm = c(0, 18, 45, 80), bottom_cm = c(18, 45, 80, 120),
      field_texture = c("silt loam", "silt loam", "silt loam", "sandy loam"), clay_pct = c(18, 20, 18, 12),
      silt_pct = c(60, 58, 60, 40), munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"),
      munsell_value_moist = c(4, 5, 6, 6), munsell_chroma_moist = c(2, 4, 3, 3),
      structure_grade = c("weak", "weak", "weak", "structureless"), structure_type = c("granular", "blocky", "blocky", "massive"),
      secondary_carbonates_pct = c(0, 0, 8, 2), effervescence_class = c("none", "none", "strong", "moderate"),
      ph_h2o = c(6.8, 7.2, 8.2, 8.3), phosphate_retention_pct = c(12, 10, 8, 6), oc_pct = c(1.8, 0.6, 0.3, 0.1),
      sand_pct = c(22, 22, 22, 48), volcanic_glass_pct = 0, coarse_fragments_pct = 0, bulk_density_g_cm3 = c(1.2, 1.35, 1.4, 1.5)),
      site(drainage_class = "well drained", parent_material = "loess")),
    oxidic = mk(list(
      designation = c("Ah", "Bo1", "Bo2", "BC"), top_cm = c(0, 18, 60, 120), bottom_cm = c(18, 60, 120, 150),
      field_texture = c("clay", "clay", "clay", "clay"), clay_pct = c(55, 68, 70, 60), silt_pct = c(25, 18, 16, 20),
      munsell_hue_moist = c("5YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(3, 3, 3, 4),
      munsell_chroma_moist = c(3, 6, 6, 6), structure_grade = c("strong", "strong", "moderate", "weak"),
      structure_type = c("granular", "polyhedral", "polyhedral", "blocky"), naf_test = "none",
      phosphate_retention_pct = c(60, 70, 70, 60), bulk_density_g_cm3 = c(1.0, 1.1, 1.1, 1.2),
      ph_h2o = c(5.6, 5.7, 5.8, 5.8), rock_origin = c(NA, NA, NA, "weathered basalt"), oc_pct = c(5, 1, 0.6, 0.3),
      stickiness = "slightly sticky", sand_pct = c(20, 14, 14, 20), volcanic_glass_pct = 0, coarse_fragments_pct = 0)),
    granular = mk(list(
      designation = c("Ah", "Bw(h)1", "Bw(h)2", "BC"), top_cm = c(0, 15, 45, 95), bottom_cm = c(15, 45, 95, 130),
      field_texture = c("clay loam", "clay", "clay", "clay"), clay_pct = c(38, 55, 58, 52), silt_pct = c(32, 25, 24, 28),
      munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 5, 5, 5),
      munsell_chroma_moist = c(2, 6, 6, 6), structure_grade = c("moderate", "moderate", "moderate", "weak"),
      structure_type = c("granular", "polyhedral", "polyhedral", "blocky"), ped_size_max_mm = c(5, 10, 20, 50),
      strength_moist = c("weak", "slightly firm", "firm", "firm"), ph_h2o = c(5.6, 5.7, 5.8, 5.9),
      phosphate_retention_pct = c(55, 60, 60, 55), bulk_density_g_cm3 = c(1.0, 1.1, 1.1, 1.2),
      naf_test = c("moderate", "slight", "slight", "slight"), oc_pct = c(5, 1, 0.5, 0.3),
      rock_origin = c(NA, NA, NA, "weathered andesitic tephra"), sand_pct = c(30, 20, 18, 20),
      volcanic_glass_pct = c(5, 2, 2, 2), coarse_fragments_pct = 0)),
    pallic = mk(list(
      designation = c("Ap", "Bw", "Bx", "C"), top_cm = c(0, 20, 45, 90), bottom_cm = c(20, 45, 90, 120),
      field_texture = c("silt loam", "silt loam", "silt loam", "silt loam"), clay_pct = c(20, 22, 20, 16),
      silt_pct = c(62, 60, 64, 66), munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"),
      munsell_value_moist = c(4, 5, 6, 6), munsell_chroma_moist = c(2, 3, 3, 3),
      structure_grade = c("moderate", "moderate", "structureless", "structureless"),
      structure_type = c("granular", "blocky", "massive", "massive"),
      strength_moist = c("weak", "firm", "very firm", "firm"), phosphate_retention_pct = c(22, 18, 15, 12),
      ph_h2o = c(5.8, 6.0, 6.2, 6.3), oc_pct = c(3, 0.6, 0.3, 0.2), sand_pct = c(18, 18, 16, 18),
      volcanic_glass_pct = 0, coarse_fragments_pct = 0, bulk_density_g_cm3 = c(1.2, 1.4, 1.6, 1.5)),
      site(drainage_class = "moderately well drained", parent_material = "loess")),
    brown = mk(list(
      designation = c("Ah", "Bw1", "Bw2", "C"), top_cm = c(0, 20, 50, 85), bottom_cm = c(20, 50, 85, 120),
      field_texture = c("silt loam", "silt loam", "silt loam", "sandy loam"), clay_pct = c(24, 26, 24, 14),
      silt_pct = c(55, 54, 56, 40), munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"),
      munsell_value_moist = c(3, 5, 5, 6), munsell_chroma_moist = c(3, 6, 6, 4),
      structure_grade = c("moderate", "moderate", "weak", "structureless"),
      structure_type = c("granular", "blocky", "blocky", "massive"), strength_moist = c("weak", "slightly firm", "slightly firm", "weak"),
      phosphate_retention_pct = c(45, 40, 35, 25), bulk_density_g_cm3 = c(1.0, 1.1, 1.2, 1.3),
      ph_h2o = c(5.7, 5.9, 6.0, 6.0), oc_pct = c(5, 1.2, 0.5, 0.2), rock_origin = c(NA, NA, NA, "greywacke"),
      sand_pct = c(21, 20, 20, 46), volcanic_glass_pct = 0, coarse_fragments_pct = c(0, 2, 5, 20))),
    anthropic = mk(list(
      designation = c("eAp", "eC1", "eC2", "bAh", "bBw"), top_cm = c(0, 20, 45, 70, 90),
      bottom_cm = c(20, 45, 70, 90, 120), field_texture = c("loam", "clay loam", "sandy loam", "silt loam", "silt loam"),
      artefacts_pct = c(5, 15, 10, 0, 0), layer_origin = c("fill", "fill", "fill", NA, NA),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y", "10YR", "10YR"), munsell_value_moist = c(3, 4, 5, 3, 5),
      munsell_chroma_moist = c(3, 4, 3, 2, 6), structure_grade = c("weak", "structureless", "structureless", "moderate", "moderate"),
      structure_type = c("granular", "massive", "massive", "granular", "blocky"),
      ph_h2o = c(6.5, 7.0, 7.2, 5.8, 6.0), oc_pct = c(2.5, 0.8, 0.5, 4, 1), effervescence_class = "none",
      phosphate_retention_pct = c(25, 20, 15, 30, 30), volcanic_glass_pct = 0, sand_pct = c(40, 30, 60, 20, 20),
      coarse_fragments_pct = c(10, 15, 10, 0, 0))),
    recent = mk(list(
      designation = c("Ah", "C1", "C2"), top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 120),
      field_texture = c("silt loam", "fine sandy loam", "sand"), clay_pct = c(15, 8, 3), silt_pct = c(55, 30, 7),
      munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(3, 5, 6),
      munsell_chroma_moist = c(2, 3, 3), structure_grade = c("weak", "structureless", "structureless"),
      structure_type = c("granular", "massive", "single grain"), layer_origin = c(NA, "alluvium", "alluvium"),
      ph_h2o = c(6.0, 6.3, 6.5), oc_pct = c(3, 0.5, 0.1), phosphate_retention_pct = c(20, 10, 5),
      sand_pct = c(30, 62, 90), volcanic_glass_pct = 0, coarse_fragments_pct = 0)),
    raw = mk(list(
      designation = c("C1", "C2"), top_cm = c(0, 20), bottom_cm = c(20, 100),
      field_texture = c("sand", "sand"), clay_pct = c(2, 1), silt_pct = c(3, 2),
      munsell_hue_moist = c("2.5Y", "2.5Y"), munsell_value_moist = c(6, 6), munsell_chroma_moist = c(3, 3),
      structure_grade = "structureless", structure_type = "single grain", layer_origin = c("dune sand", "dune sand"),
      ph_h2o = c(7.0, 7.2), oc_pct = c(0.1, 0.05), effervescence_class = "none", sand_pct = c(95, 97),
      volcanic_glass_pct = 0, coarse_fragments_pct = 0, phosphate_retention_pct = c(3, 2)))
  )
}
