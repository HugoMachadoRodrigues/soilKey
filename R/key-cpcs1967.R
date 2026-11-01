# =============================================================================
# v0.9.330 -- the French soil classification (CPCS 1967, Commission de
# Pedologie et de Cartographie des Sols), keyed to its twelve classes. The
# CPCS gives no key: soilKey keys the classes in an order built from the
# text's own statements of precedence (Hugo, 2026-11-01). The sub-classes,
# groups and subgroups follow. Experimental and opt-in: classify_all() runs
# it only when asked.
# =============================================================================

#' Classify a pedon under the French soil classification (CPCS 1967)
#'
#' Keys a \code{\link{PedonRecord}} through the twelve classes of the
#' French soil classification of 1967 (Commission de Pedologie et de
#' Cartographie des Sols). \strong{Experimental}: this release keys the
#' classes (sols mineraux bruts, sols peu evolues, vertisols, andosols,
#' sols calcimagnesiques, sols isohumiques, sols brunifies, sols
#' podzolises, sols a sesquioxydes de fer et manganese, sols ferrallitiques, sols
#' hydromorphes, sols sodiques); the sub-classes, groups and subgroups
#' follow. The names are French, as the text writes them, in \code{name}
#' and in \code{levels$name_native}.
#'
#' The CPCS gives no key. soilKey keys the classes in an order built from
#' the text's own statements of precedence: raw mineral soils, vertisols,
#' sodic soils, andosols, podzolised, ferrallitic and sesquioxide soils,
#' hydromorphic soils, weakly developed soils, isohumic soils,
#' calcimagnesic soils, and the brunified soils last; a peat is a
#' hydromorphic soil, whatever lies under it. Every threshold of
#' the text is in % organic matter, read as 1.72 x organic carbon, with
#' depths from the top of the first mineral horizon.
#'
#' The key reads the old French horizon notation (Aoo, Ao, A1, A2, A3,
#' (B), B, Bh, Bfe, Bt, C, Cca, G, g, R, a Roman numeral for a change of
#' parent material), declared with \code{site$designation_system = "cpcs"}
#' or recognised by its symbols, beside the FAO letters; where a profile
#' has no designations, soilKey's diagnostic horizons stand in. From the
#' horizons it reads organic carbon and nitrogen, texture, pH, the CEC and
#' the exchangeable bases, carbonate and gypsum, the EC of the saturated
#' paste, Ki (\code{sio2_sulfuric_pct}, \code{al2o3_sulfuric_pct}), the
#' andic properties, moist and dry Munsell colours, mottles, slickensides,
#' cracks, structure and consistence, and the recorded origin of the
#' layers; from the site the soil temperature regime, the parent material
#' and recorded erosion. The choices where the text is silent are listed in
#' \code{system.file("rules", "cpcs1967", "readings.yaml", package =
#' "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("cpcs1967")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"cpcs1967"}, the class in \code{name} and \code{rsg_or_order},
#'   the classes of each level in \code{levels} (with \code{name_native}),
#'   the key trace, and \code{provisional_because} (classes before the
#'   assigned one that could not be decided).
#' @references Commission de Pedologie et de Cartographie des Sols (1967).
#'   Classification des sols. ENSA, Grignon. Digitised by the AFES (2011).
#' @seealso \code{\link{make_cpcs1967_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_cpcs1967(make_cpcs1967_example("ferrallitic"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_cpcs1967 <- function(pedon, rules = NULL,
                              on_missing = c("warn", "silent", "error"),
                              gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) {
    pedon <- .cssc_prepare_pedon(pedon)
    pedon <- .classify_apply_gapfill(pedon, gapfill)
  }
  rules <- rules %||% load_rules("cpcs1967")
  res <- classify_hierarchical("cpcs1967", pedon, rules = rules,
                               on_missing = on_missing, gapfill = FALSE)
  if (inherits(pedon, "PedonRecord") &&
      all(is.na(pedon$horizons$designation %||% NA) | !nzchar(trimws(pedon$horizons$designation %||% "")))) {
    res$warnings <- c(res$warnings, paste(
      "No horizon designations: the CPCS profile types (AC, (B), B, A2) are then read from soilKey's",
      "diagnostic horizons (argic, cambic, spodic, natric, ferralic, nitic, albic)."))
  }
  res
}


#' Example pedons for the CPCS 1967 classes
#'
#' Small, plausible profiles written for the tests and examples, one per
#' class of the French soil classification (CPCS 1967), with designations
#' in the old French notation and the properties the key reads. They are
#' not real profiles.
#'
#' @param class One of \code{"raw_mineral"}, \code{"weakly_developed"},
#'   \code{"vertisol"}, \code{"andosol"}, \code{"calcimagnesic"},
#'   \code{"isohumic"}, \code{"brunified"}, \code{"podzolised"},
#'   \code{"sesquioxide"}, \code{"ferrallitic"}, \code{"hydromorphic"},
#'   \code{"sodic"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' classify_cpcs1967(make_cpcs1967_example("podzolised"), on_missing = "silent")$name
#' @export
make_cpcs1967_example <- function(class = c("raw_mineral", "weakly_developed", "vertisol", "andosol",
                                            "calcimagnesic", "isohumic", "brunified", "podzolised",
                                            "sesquioxide", "ferrallitic", "hydromorphic", "sodic")) {
  class <- match.arg(class)
  mk <- function(h, site = list()) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    base <- list(id = paste0("cpcs1967-", class), country = "FR", designation_system = "cpcs",
                 drainage_class = "well drained")
    PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  switch(class,
    raw_mineral = mk(list(
      designation = c("(A)", "C1", "C2"), top_cm = c(0, 5, 40), bottom_cm = c(5, 40, 100),
      oc_pct = c(0.1, 0.05, 0.03), clay_pct = 8, silt_pct = 20, sand_pct = 72,
      layer_origin = "recent river alluvium", ph_h2o = 7.4,
      munsell_hue_moist = "2.5Y", munsell_value_moist = c(5, 5, 6), munsell_chroma_moist = 3,
      redoximorphic_features_pct = 0), list(landform = "floodplain")),
    weakly_developed = mk(list(
      designation = c("A1", "C1", "C2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
      oc_pct = c(1.5, 0.4, 0.2), n_total_pct = c(0.14, 0.04, 0.02), clay_pct = 20, silt_pct = 40,
      sand_pct = 40, layer_origin = "calcareous alluvium", ph_h2o = 7.9, caco3_pct = 5,
      cec_cmol = 15, bs_pct = 100, ca_cmol = 13, mg_cmol = 1.5, na_cmol = 0.1,
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(2, 3, 3),
      redoximorphic_features_pct = 0)),
    vertisol = mk(list(
      designation = c("A1", "(B)1", "(B)2", "C"), top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 150),
      oc_pct = c(1.5, 0.8, 0.5, 0.3), n_total_pct = c(0.13, 0.07, 0.05, 0.03),
      clay_pct = c(50, 55, 55, 45), silt_pct = 30, sand_pct = c(20, 15, 15, 25),
      slickensides = c("few", "common", "many", "few"), cracks_width_cm = c(2, 2, NA, NA),
      ph_h2o = 7.6, cec_cmol = 45, bs_pct = 95, ca_cmol = 35, mg_cmol = 8, na_cmol = 0.6,
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(1, 1, 2, 2),
      redoximorphic_features_pct = 0, structure_type = c("granular", "prismatic", "prismatic", "massive"))),
    andosol = mk(list(
      designation = c("A1", "(B)", "C"), top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 120),
      oc_pct = c(8, 3, 1), n_total_pct = c(0.6, 0.25, 0.08), clay_pct = 20, silt_pct = 45, sand_pct = 35,
      al_ox_pct = c(3.5, 4.0, 2.5), fe_ox_pct = c(1.5, 1.6, 1.0), bulk_density_g_cm3 = c(0.6, 0.7, 0.85),
      phosphate_retention_pct = c(92, 95, 88), cec_cmol = c(35, 30, 25), bs_pct = c(30, 20, 20),
      ph_h2o = 5.6, layer_origin = "volcanic ash",
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4), munsell_chroma_moist = c(1, 3, 4),
      redoximorphic_features_pct = 0), list(parent_material = "volcanic ash")),
    calcimagnesic = mk(list(
      designation = c("A1", "R"), top_cm = c(0, 25), bottom_cm = c(25, 60),
      oc_pct = c(5, NA), n_total_pct = c(0.45, NA), clay_pct = c(35, NA), silt_pct = c(40, NA),
      sand_pct = c(25, NA), caco3_pct = c(30, NA), ph_h2o = c(8.0, NA), cec_cmol = c(40, NA),
      ca_cmol = c(38, NA), mg_cmol = c(1, NA), na_cmol = c(0.2, NA), bs_pct = c(100, NA),
      rock_origin = c(NA, "hard limestone"), munsell_hue_moist = c("10YR", NA),
      munsell_value_moist = c(2, NA), munsell_chroma_moist = c(2, NA), redoximorphic_features_pct = c(0, NA),
      structure_type = c("granular", NA)), list(parent_material = "limestone")),
    isohumic = mk(list(
      designation = c("A1", "A3", "(B)", "Cca"), top_cm = c(0, 35, 70, 100), bottom_cm = c(35, 70, 100, 140),
      oc_pct = c(3, 2, 1.2, 0.3), n_total_pct = c(0.27, 0.18, 0.11, 0.03), clay_pct = 30, silt_pct = 50,
      sand_pct = 20, ph_h2o = c(7.2, 7.4, 7.6, 8.2), cec_cmol = 30, bs_pct = c(95, 96, 98, 100),
      ca_cmol = c(25, 26, 27, 28), mg_cmol = 4, na_cmol = 0.3, caco3_pct = c(0, 0, 0, 12),
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4, 5), munsell_chroma_moist = c(1, 2, 3, 3),
      redoximorphic_features_pct = 0), list(vegetation = "steppe grassland")),
    brunified = mk(list(
      designation = c("A1", "(B)", "C"), top_cm = c(0, 10, 60), bottom_cm = c(10, 60, 100),
      oc_pct = c(4, 0.8, 0.2), n_total_pct = c(0.25, 0.07, 0.02), clay_pct = c(20, 22, 15), silt_pct = 40,
      sand_pct = c(40, 38, 45), ph_h2o = c(4.8, 5.0, 5.2), cec_cmol = c(12, 10, 6), bs_pct = c(25, 20, 20),
      ca_cmol = c(2, 1.5, 1), mg_cmol = c(0.6, 0.4, 0.3), na_cmol = 0.05,
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 6, 4),
      redoximorphic_features_pct = 0), list(parent_material = "granite")),
    podzolised = mk(list(
      designation = c("Ao", "A1", "A2", "B2h", "B2fe", "C"), top_cm = c(0, 5, 12, 25, 35, 60),
      bottom_cm = c(5, 12, 25, 35, 60, 100), oc_pct = c(35, 5, 0.5, 3, 1.2, 0.2),
      n_total_pct = c(1.2, 0.2, 0.03, 0.15, 0.06, 0.02), clay_pct = c(NA, 5, 4, 6, 6, 4), silt_pct = c(NA, 10, 8, 9, 9, 6),
      sand_pct = c(NA, 85, 88, 85, 85, 90), ph_h2o = 4.2, cec_cmol = c(NA, 8, 2, 12, 6, 2), bs_pct = 10,
      munsell_hue_moist = c("7.5YR", "10YR", "10YR", "5YR", "7.5YR", "10YR"),
      munsell_value_moist = c(2, 3, 6, 2, 4, 6), munsell_chroma_moist = c(1, 1, 1, 2, 6, 4),
      munsell_value_dry = c(NA, 4, 7, 3, 5, 7), munsell_chroma_dry = c(NA, 1, 1, 2, 6, 3),
      redoximorphic_features_pct = c(NA, 0, 0, 0, 0, 0)), list(parent_material = "sandstone")),
    sesquioxide = mk(list(
      designation = c("A1", "B21t", "B22t", "C"), top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 140),
      oc_pct = c(1.5, 0.5, 0.3, 0.1), n_total_pct = c(0.13, 0.05, 0.03, 0.01), clay_pct = c(25, 40, 42, 35),
      silt_pct = 25, sand_pct = c(50, 35, 33, 40), cec_cmol = c(15, 20, 21, 18), bs_pct = c(70, 75, 80, 85),
      ca_cmol = c(8, 12, 13, 12), mg_cmol = 2, na_cmol = 0.1, ph_h2o = 6.5,
      sio2_sulfuric_pct = c(NA, 25, 25, 24), al2o3_sulfuric_pct = c(NA, 15, 15, 14),
      munsell_hue_moist = c("5YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(3, 4, 4, 5),
      munsell_chroma_moist = c(3, 6, 6, 6), clay_films_amount = c("none", "common", "common", "few"),
      redoximorphic_features_pct = 0), list(parent_material = "schist")),
    ferrallitic = mk(list(
      designation = c("A1", "B1", "B21", "B22"), top_cm = c(0, 15, 40, 100), bottom_cm = c(15, 40, 100, 180),
      oc_pct = c(2, 0.8, 0.5, 0.3), n_total_pct = c(0.18, 0.08, 0.05, 0.03), clay_pct = c(45, 55, 60, 60),
      silt_pct = 10, sand_pct = c(45, 35, 30, 30), cec_cmol = c(6, 4, 3, 3), bs_pct = 20,
      ca_cmol = 0.6, mg_cmol = 0.2, na_cmol = 0.02, ph_h2o = 4.8,
      sio2_sulfuric_pct = c(NA, 15, 15, 15), al2o3_sulfuric_pct = c(NA, 30, 31, 31),
      munsell_hue_moist = "2.5YR", munsell_value_moist = c(3, 3, 4, 4), munsell_chroma_moist = c(4, 6, 8, 8),
      redoximorphic_features_pct = 0), list(country = "CI", parent_material = "granite")),
    hydromorphic = mk(list(
      designation = c("A1g", "G1", "G2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
      oc_pct = c(3, 0.5, 0.3), n_total_pct = c(0.25, 0.05, 0.03), clay_pct = 35, silt_pct = 40, sand_pct = 25,
      ph_h2o = 6.5, cec_cmol = 20, bs_pct = 70, ca_cmol = 11, mg_cmol = 2.5, na_cmol = 0.2,
      munsell_hue_moist = c("10YR", "5GY", "5GY"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 1, 1),
      redoximorphic_features_pct = c(15, 5, 2)), list(drainage_class = "poorly drained", landform = "valley floor")),
    sodic = mk(list(
      designation = c("A1Sa", "C1Sa", "C2"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
      oc_pct = c(1, 0.4, 0.2), n_total_pct = c(0.1, 0.04, 0.02), clay_pct = 30, silt_pct = 40, sand_pct = 30,
      ec_dS_m = c(25, 18, 12), na_cmol = c(4, 3.5, 3), cec_cmol = 15, bs_pct = 100, ph_h2o = 8.4,
      munsell_hue_moist = "2.5Y", munsell_value_moist = c(5, 6, 6), munsell_chroma_moist = c(2, 2, 3),
      redoximorphic_features_pct = 0), list(drainage_class = "moderately well drained", landform = "playa margin"))
  )
}
