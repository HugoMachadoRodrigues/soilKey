# =============================================================================
# v0.9.277 -- the Australian Soil Classification, 3rd edition (Isbell & NCST
# 2021, online version with errata of 25 June 2026), keyed to the 15 orders;
# suborders since v0.9.280 (all orders since v0.9.282); great groups since
# v0.9.283 (ten orders since v0.9.284, all that have them since v0.9.285);
# subgroups since v0.9.286 (Kurosols, Sodosols, Chromosols; Dermosols, Kandosols, Ferrosols since
# v0.9.287; Vertosols, Calcarosols, Podosols, Organosols since v0.9.288; Hydrosols, Arenosols, Rudosols,
# Tenosols since v0.9.289; the Rudosolic and Tenosolic Redoxic and Oxyaquic Hydrosols since v0.9.291).
# Families follow.
# Experimental and opt-in: classify_all() runs it only when asked.
# =============================================================================

#' Classify a pedon under the Australian Soil Classification
#'
#' Keys a \code{\link{PedonRecord}} through the Australian Soil
#' Classification, 3rd edition (Isbell and the National Committee on Soil
#' and Terrain 2021), following the online version with its errata.
#' \strong{Experimental}: this release keys the 15 orders (the key to soil
#' orders) and the suborders of all of them: the Kurosols, Sodosols,
#' Chromosols, Ferrosols, Dermosols, Kandosols and Vertosols (colour classes,
#' and the Aquic Vertosols), the Organosols (the dominant kind of peat), the
#' Podosols (saturation of the B, sulfuric and hypersulfidic material), the
#' Hydrosols (the setting, then mottling), the Calcarosols (gypsum and the
#' carbonate classes), the Anthroposols (the kind of human-made material),
#' the Arenosols and Tenosols (chernic horizons, bleaching, hard materials
#' within 0.5 m, carbonate, nodules, then colour) and the Rudosols (salinity,
#' gypsum, shells, carbic materials, hard materials, sand, gravel, layers),
#' and the great groups of the Kurosols, Sodosols, Chromosols, Ferrosols,
#' Dermosols and Kandosols (pans in or under the B, structure, plasticity,
#' base status, exchangeable Ca/Mg, ESP and carbonate of the B2 or B2t), the
#' Vertosols (the surface), Calcarosols (what the soil directly overlies,
#' and the B), Podosols (the kinds of Podosol B), Organosols (drainage,
#' acid sulfate materials, carbonate and acidity), Hydrosols (acid sulfate
#' materials, the upper 0.5 m, and the order the soil would key to apart
#' from wetness), Arenosols, Rudosols and Tenosols (what the soil directly
#' overlies, nodular, gypsic and argic horizons, carbonate, texture, gravel);
#' the Anthroposols have none in the text; and the subgroups of the
#' Kurosols, Sodosols and Chromosols (chernic and vertic features, bleaching,
#' ferric and manganic horizons, sodicity of the lower B, mottling of the
#' B2t, base status, carbonate) and of the Dermosols, Kandosols and
#' Ferrosols (the same features read on the B2, a strongly acid B2, argic and
#' bauxitic horizons in the B, a snuffy A), of the Vertosols and Calcarosols
#' (acid sulfate materials, pans, sodicity, acidity, carbonate and mottling
#' of the upper 0.5 m of the solum and below it, subplasticity, surface
#' carbonate), the Podosols (coherence of the B, pans), the Organosols
#' (what the organic materials overlie), and the Hydrosols, Arenosols,
#' Rudosols and Tenosols (peat kinds, sulfidic materials near the surface,
#' acidity and carbonate of the upper 1.0 m, the B, the A2, the solum or
#' the soil, horizons with all but the thickness; the Rudosolic and
#' Tenosolic great groups of the Redoxic and Oxyaquic Hydrosols take the
#' list of the Rudosol or Tenosol suborder the soil keys to apart from
#' wetness). The families follow. The name is that of the deepest class
#' assigned, in the book's form "Subgroup, Great Group, Suborder Order"
#' ("Bleached, Eutrophic, Red Chromosol"); \code{rsg_or_order} keeps the
#' order.
#'
#' The ASC is largely morphological. The diagnostics read field texture
#' grades (\code{field_texture}, NCST Field Handbook codes; lab clay stands in
#' and is undecided near the limits), the Australian horizon notation (A1,
#' A2, B21t, Bhs, Bk ...; FAO/USDA designations are read by the role of each
#' horizon), the boundary between the A and the B, pH (strongly acid: pHw
#' below 5.5), ESP (sodic: 6 or more), subplasticity, free iron, grade of
#' pedality, carbonates and the moisture of the horizons when described
#' (\code{soil_water_status}: the Vertosols' cracks are not required in a
#' solum described moist), mottles, soft and hard carbonate, gypsum, the kind
#' of peat, acid sulfate tests, EC of a 1:5 extract, dry and moist strength,
#' ped size, nodules, pans and exchangeable bases. From the site it reads
#' \code{drainage_class} (Hydrosols, organic materials, Podosol suborders),
#' \code{tidal_inundation} (Hydrosols, organic materials),
#' \code{surface_condition} (a cracking
#' surface, Vertosols; a saline one, Salic Hydrosols), \code{landform},
#' \code{landform_element}, \code{tidal_zone}, \code{subaqueous} and
#' \code{salinised} (Hydrosol suborders) and \code{designation_system}
#' (\code{"asc"} for the Australian notation). The
#' choices where the text is silent are listed in
#' \code{system.file("rules", "asc3", "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("asc3")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"asc3"}, the deepest class in \code{name}, the order in
#'   \code{rsg_or_order}, the classes of each level in \code{levels}, the key
#'   trace, and \code{provisional_because} (classes before the assigned ones
#'   that could not be decided).
#' @references Isbell, R.F. and the National Committee on Soil and Terrain
#'   (2021). The Australian Soil Classification, 3rd ed. CSIRO Publishing,
#'   Melbourne. Online version: www.soilscienceaustralia.org.au/asc.
#' @seealso \code{\link{make_asc3_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")
#' res$name
#' @export
classify_asc3 <- function(pedon, rules = NULL,
                          on_missing = c("warn", "silent", "error"),
                          gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cssc_prepare_pedon(pedon)
  # .apart_from_wetness is set by the key for one walk (Rudosolic Hydrosols),
  # never read from the user's record
  if (inherits(pedon, "PedonRecord") && !is.null(pedon$site$.apart_from_wetness)) {
    pedon <- pedon$clone(deep = TRUE)
    pedon$site$.apart_from_wetness <- NULL
  }
  res <- classify_hierarchical("asc3", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (inherits(pedon, "PedonRecord") && identical(res$rsg_or_order, "Rudosol")) {
    # a cover under 0.3 m that is only a Rudosol is a phase of the buried soil
    x <- .asc_horizons(pedon)
    top_b <- suppressWarnings(min(x$h$top_cm[x$des$buried], na.rm = TRUE))
    if (is.finite(top_b) && top_b < 30) {
      res$warnings <- c(res$warnings, paste(
        "Less than 0.3 m of material that qualifies only as a Rudosol lies over a",
        "buried soil: the ASC then classifies the buried soil, with the cover as a",
        "phase (How to classify); soilKey keyed the cover."))
    }
  }
  res
}


#' Example pedons for the Australian orders
#'
#' Small, plausible profiles written for the tests and examples, one per
#' order of the Australian Soil Classification, with designations in the
#' Australian notation (Field Handbook), field textures and the properties
#' the order key reads. They are not real profiles.
#'
#' @param order One of the 15 orders, in lower case and plural:
#'   \code{"anthroposols"}, \code{"organosols"}, \code{"podosols"},
#'   \code{"vertosols"}, \code{"hydrosols"}, \code{"kurosols"},
#'   \code{"sodosols"}, \code{"chromosols"}, \code{"calcarosols"},
#'   \code{"ferrosols"}, \code{"dermosols"}, \code{"kandosols"},
#'   \code{"arenosols"}, \code{"rudosols"}, \code{"tenosols"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_asc3_example("kandosols")
#' p$horizons[, c("designation", "top_cm", "bottom_cm", "field_texture")]
#' @export
make_asc3_example <- function(order = c("anthroposols", "organosols", "podosols", "vertosols",
                                        "hydrosols", "kurosols", "sodosols", "chromosols",
                                        "calcarosols", "ferrosols", "dermosols", "kandosols",
                                        "arenosols", "rudosols", "tenosols")) {
  order <- match.arg(order)
  well <- list(id = paste0("asc3-", order), country = "AU", designation_system = "asc",
               drainage_class = "well drained")
  mk <- function(h, site = well) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = site, horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  # a texture-contrast profile: sandy loam A over a clay B2t at an abrupt
  # boundary; the B2t chemistry decides among Kurosols, Sodosols, Chromosols
  duplex <- function(ph, na, ca, mg, k) list(
    designation = c("A1", "A2e", "B21t", "B22t", "BC"), top_cm = c(0, 12, 30, 55, 80),
    bottom_cm = c(12, 30, 55, 80, 110), field_texture = c("SL", "SL", "LMC", "MC", "LC"),
    clay_pct = c(14, 12, 42, 48, 38), boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", NA),
    structure_grade = c("weak", "structureless", "strong", "moderate", "weak"),
    structure_type = c("granular", "massive", "prismatic", "blocky", "blocky"),
    ph_h2o = c(6.0, 6.0, ph, ph, ph + 0.3), cec_cmol = c(8, 5, 20, 22, 18),
    na_cmol = c(0.1, 0.1, na, na, na), ca_cmol = c(4, 2, ca, ca, ca), mg_cmol = c(1, 0.5, mg, mg, mg),
    k_cmol = c(0.4, 0.2, k, k, k), redoximorphic_features_pct = 0, oc_pct = c(1.8, 0.5, 0.4, 0.3, 0.2),
    munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR", "5YR"),
    munsell_value_moist = c(3, 6, 4, 4, 5), munsell_chroma_moist = c(2, 3, 6, 6, 6))
  switch(order,
    anthroposols = mk(list(
      designation = c("A1", "C1", "C2", "2A1b", "2B2b"), top_cm = c(0, 8, 25, 50, 70),
      bottom_cm = c(8, 25, 50, 70, 100), field_texture = c("L", "SCL", "SL", "CL", "LC"),
      clay_pct = c(22, 25, 15, 32, 38), artefacts_pct = c(5, 20, 15, 0, 0),
      layer_origin = c("spolic", "spolic", "spolic", NA, NA),
      structure_grade = c("weak", "structureless", "structureless", "moderate", "moderate"),
      ph_h2o = c(7.0, 7.5, 7.5, 6.5, 6.5), oc_pct = c(2, 0.5, 0.4, 1.5, 0.6))),
    organosols = mk(list(
      designation = c("P1", "P2", "C"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
      field_texture = c("HP", "AP", "LC"), clay_pct = c(NA, NA, 38), oc_pct = c(38, 30, 1.5),
      layer_origin = c(NA, NA, "alluvium"), munsell_value_moist = c(2, 2, 4), munsell_chroma_moist = c(1, 1, 2),
      sulfidic_s_pct = c(0.005, 0.005, 0.002), ph_h2o = c(4.8, 5.0, 5.5)), utils::modifyList(well, list(drainage_class = "very poorly drained"))),
    podosols = mk(list(
      designation = c("A1", "A2e", "B2hs", "B2s", "C"), top_cm = c(0, 10, 45, 60, 90),
      bottom_cm = c(10, 45, 60, 90, 130), field_texture = c("S", "S", "LS", "S", "S"),
      clay_pct = c(3, 2, 5, 3, 2), oc_pct = c(2.0, 0.3, 1.8, 0.6, 0.1),
      structure_grade = "structureless", structure_type = c("single-grain", "single-grain", "massive", "massive", "single-grain"),
      strength_moist = c("loose", "loose", "firm", "weak", "loose"),
      munsell_hue_moist = c("10YR", "10YR", "5YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 7, 3, 5, 7),
      munsell_chroma_moist = c(1, 1, 3, 6, 2), munsell_value_dry = c(5, 8, NA, NA, NA),
      ph_h2o = c(5.0, 5.3, 4.9, 5.2, 5.5)), utils::modifyList(well, list(drainage_class = "rapidly drained"))),
    vertosols = mk(list(
      designation = c("A1", "B21", "B22", "BC"), top_cm = c(0, 10, 50, 100),
      bottom_cm = c(10, 50, 100, 150), field_texture = c("MC", "HC", "HC", "MC"),
      clay_pct = c(52, 58, 60, 55), slickensides = c("absent", "common", "many", "few"),
      structure_type = c("granular", "lenticular", "lenticular", "blocky"),
      structure_grade = c("strong", "strong", "strong", "moderate"),
      cracks_width_cm = c(1.5, 1.0, NA, NA), ph_h2o = c(7.8, 8.2, 8.6, 8.8),
      effervescence_class = c("none", "none", "slight", "moderate"), cec_cmol = 40, na_cmol = c(0.5, 1, 2, 3),
      ca_cmol = 25, mg_cmol = 10, redoximorphic_features_pct = 0, sulfidic_s_pct = 0.002,
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 3, 4), munsell_chroma_moist = c(1, 1, 2, 2)),
      utils::modifyList(well, list(surface_condition = "cracking; self-mulching"))),
    hydrosols = mk(list(
      designation = c("A1", "B21g", "B22g", "C"), top_cm = c(0, 15, 45, 90),
      bottom_cm = c(15, 45, 90, 130), field_texture = c("CL", "LC", "LC", "SCL"),
      clay_pct = c(30, 38, 40, 28), boundary_distinctness = c("gradual", "gradual", "diffuse", NA),
      structure_grade = c("moderate", "weak", "weak", "structureless"),
      water_saturation_days = c(120, 150, 180, 200),
      redoximorphic_features_pct = c(0, 25, 30, 15), mottle_contrast = c(NA, "distinct", "prominent", "distinct"),
      ph_h2o = c(5.8, 6.0, 6.2, 6.5), oc_pct = c(3.0, 0.8, 0.5, 0.2)),
      utils::modifyList(well, list(drainage_class = "poorly drained", landform = "alluvial plain",
                                   surface_condition = "firm"))),
    kurosols = mk(duplex(ph = 5.0, na = 0.5, ca = 2, mg = 1.5, k = 0.3)),
    sodosols = mk(duplex(ph = 8.0, na = 4.0, ca = 6, mg = 5, k = 0.5)),
    chromosols = mk(duplex(ph = 6.5, na = 0.4, ca = 10, mg = 5, k = 0.5)),
    calcarosols = mk(list(
      designation = c("A1", "B21k", "B22k", "BCk", "C"), top_cm = c(0, 12, 40, 70, 110),
      bottom_cm = c(12, 40, 70, 110, 150), field_texture = c("SL", "SCL", "SCL", "SL", "SL"),
      clay_pct = c(14, 24, 26, 18, 15), effervescence_class = c("slight", "strong", "strong", "very strong", "strong"),
      secondary_carbonates_pct = c(0, 15, 30, 25, 5), caco3_pct = c(2, 12, 25, 20, 10),
      carbonate_soft_pct = c(0, 12, 22, 18, 5), carbonate_hard_pct = c(0, 3, 5, 5, 0), caso4_pct = 0,
      layer_origin = c(NA, NA, NA, NA, "aeolian"), cec_cmol = 15, na_cmol = 0.3,
      munsell_hue_moist = "7.5YR", munsell_value_moist = c(4, 5, 6, 6, 7), munsell_chroma_moist = c(3, 4, 4, 4, 4),
      boundary_distinctness = c("gradual", "gradual", "gradual", "clear", NA),
      structure_grade = c("weak", "weak", "weak", "structureless", "structureless"),
      ph_h2o = c(8.2, 8.6, 8.8, 9.0, 9.0))),
    ferrosols = mk(list(
      designation = c("A1", "B21", "B22", "B23"), top_cm = c(0, 15, 50, 100),
      bottom_cm = c(15, 50, 100, 150), field_texture = c("LC", "MC", "MC", "MC"),
      clay_pct = c(45, 55, 58, 60), fe_dcb_pct = c(7, 9, 10, 10),
      ca_cmol = c(8, 6, 6, 6), mg_cmol = c(3, 3, 3, 3), k_cmol = c(0.6, 0.4, 0.4, 0.4), na_cmol = c(0.2, 0.2, 0.2, 0.2),
      boundary_distinctness = c("gradual", "diffuse", "diffuse", NA),
      structure_grade = c("strong", "strong", "moderate", "moderate"),
      structure_type = c("granular", "polyhedral", "polyhedral", "polyhedral"),
      slickensides = "absent", ph_h2o = c(5.8, 5.6, 5.8, 6.0), ped_size_max_mm = c(5, 20, 20, 20),
      oc_pct = c(2.5, 1.0, 0.6, 0.4), redoximorphic_features_pct = 0, cec_cmol = c(16, 12, 11, 11),
      munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = c(4, 6, 6, 6))),
    dermosols = mk(list(
      designation = c("A1", "B21", "B22", "BC"), top_cm = c(0, 15, 50, 90),
      bottom_cm = c(15, 50, 90, 130), field_texture = c("CL", "LC", "LMC", "LC"),
      clay_pct = c(30, 38, 42, 36), fe_dcb_pct = c(1.5, 2.0, 2.2, 2.0),
      ca_cmol = c(9, 8, 8, 8), mg_cmol = c(4, 4, 4, 4), k_cmol = c(0.6, 0.5, 0.5, 0.5), na_cmol = c(0.3, 0.3, 0.3, 0.3),
      effervescence_class = "none",
      boundary_distinctness = c("gradual", "gradual", "gradual", NA),
      structure_grade = c("moderate", "moderate", "strong", "weak"),
      structure_type = c("granular", "blocky", "blocky", "blocky"), slickensides = "absent",
      munsell_hue_moist = "7.5YR", munsell_value_moist = c(3, 4, 4, 5), munsell_chroma_moist = c(2, 4, 4, 4),
      oc_pct = c(2.0, 0.8, 0.5, 0.3), redoximorphic_features_pct = c(0, 0, 0, 5), cec_cmol = c(18, 16, 16, 15),
      ph_h2o = c(6.2, 6.5, 6.8, 7.0))),
    kandosols = mk(list(
      designation = c("A1", "A3", "B21", "B22"), top_cm = c(0, 15, 30, 70),
      bottom_cm = c(15, 30, 70, 130), field_texture = c("SL", "SL", "SCL", "SCL"),
      clay_pct = c(12, 15, 24, 28), fe_dcb_pct = c(1.0, 1.2, 2.5, 2.8),
      ca_cmol = c(2.5, 2, 2, 2), mg_cmol = c(1, 1, 1, 1), k_cmol = c(0.3, 0.2, 0.2, 0.2), na_cmol = c(0.1, 0.1, 0.1, 0.1),
      strength_moist = c(NA, NA, "firm", "firm"),
      boundary_distinctness = c("gradual", "gradual", "diffuse", NA),
      structure_grade = c("weak", "structureless", "structureless", "weak"),
      structure_type = c("granular", "massive", "massive", "blocky"),
      munsell_hue_moist = c("7.5YR", "5YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 4, 4, 4),
      munsell_chroma_moist = c(2, 4, 6, 6), oc_pct = c(1.2, 0.5, 0.3, 0.2), redoximorphic_features_pct = 0,
      cec_cmol = c(5, 4, 4, 4),
      ph_h2o = c(6.0, 5.8, 5.9, 6.0))),
    arenosols = mk(list(
      designation = c("A1", "A3", "C1", "C2"), top_cm = c(0, 10, 40, 90),
      bottom_cm = c(10, 40, 90, 150), field_texture = c("S", "S", "S", "LS"),
      clay_pct = c(3, 3, 4, 6), structure_grade = "structureless", structure_type = "single-grain",
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 6, 7), munsell_chroma_moist = c(3, 6, 6, 6),
      ph_h2o = c(6.0, 6.2, 6.3, 6.5), oc_pct = c(0.8, 0.3, 0.1, 0.1))),
    rudosols = mk(list(
      designation = c("A1", "C", "R"), top_cm = c(0, 8, 35), bottom_cm = c(8, 35, 60),
      field_texture = c("LS", "LS", NA), clay_pct = c(6, 6, NA), coarse_fragments_pct = c(20, 40, NA),
      structure_grade = c("structureless", "structureless", NA), structure_type = c("single-grain", "single-grain", NA),
      ph_h2o = c(6.5, 6.8, NA), oc_pct = c(0.8, 0.2, NA))),
    tenosols = mk(list(
      designation = c("A1", "B2", "C"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 120),
      field_texture = c("LS", "SL", "LS"), clay_pct = c(7, 12, 8), fe_dcb_pct = c(0.4, 0.7, 0.3),
      boundary_distinctness = c("gradual", "gradual", NA),
      structure_grade = c("weak", "weak", "structureless"), structure_type = c("granular", "blocky", "single-grain"),
      layer_origin = c(NA, NA, "aeolian"), ferric_nodules_pct = 0, bauxitic_nodules_pct = 0,
      munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(3, 4, 4),
      ph_h2o = c(6.0, 6.3, 6.5), oc_pct = c(1.2, 0.4, 0.1)))
  )
}
