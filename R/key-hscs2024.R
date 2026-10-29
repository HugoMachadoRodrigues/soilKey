# =============================================================================
# v0.9.305 -- the Hungarian diagnostic soil classification (Micheli et al.
# 2024, "A diagnosztikus szemleletu hazai talajosztalyozasi rendszer",
# second approximation), keyed to its 15 soil types (section 7). The subtype
# and variety properties, added to the name, are not keyed yet.
# Experimental and opt-in: classify_all() runs it only when asked.
# =============================================================================

#' Classify a pedon under the Hungarian diagnostic soil classification
#'
#' Keys a \code{\link{PedonRecord}} through the Hungarian diagnostic soil
#' classification of Micheli et al. (2024, second approximation), following
#' its key to the soil types. \strong{Experimental}: this release keys the
#' 15 types (Laptalaj, Antropogen talaj, Koves-sziklas vaztalaj,
#' Kozethatasu talaj, Szolonyec talaj, Szoloncsak talaj, Duzzado agyagtalaj,
#' Reti talaj, Karbonat talaj, Mezosegi talaj, Agyagbemosodasos talaj,
#' Homoktalaj, Barnafold, Hordalektalaj, Foldeskopar). The subtype and
#' variety properties that the system adds to the name are not keyed yet.
#'
#' The names are the Hungarian singular, as the text's naming example writes
#' them; \code{levels$name_en} gives an English gloss. The key reads the FAO
#' (2006) horizon notation the text adopts (an l or r suffix shows reductive
#' conditions; i slickensides; k carbonates; n sodium; p ploughing; t clay
#' accumulation; u artefacts; w a cambic horizon), moist and dry Munsell
#' colours, texture (Hungarian names of the USDA triangle the text prints,
#' English names, or lab clay, silt and sand), organic carbon, bulk density,
#' CaCO3, effervescence, secondary carbonates, base saturation, exchangeable
#' Na, Mg and the CEC, structure, slickensides, cracks, clay films, coarse
#' fragments, artefacts, the layer origin, the dipyridyl test
#' (\code{childs_test}), redoximorphic features, and the total water-soluble
#' salts (\code{soluble_salts_pct}, Hungarian method). From the site it reads
#' \code{land_use}, \code{methane_odour}, \code{geomembrane},
#' \code{mechanical_mixing_depth_cm} and \code{anthropic_truncation}. Features
#' a describer records when met (artefacts, mottles, secondary carbonates,
#' cracks) are absent when not recorded. The choices where the text is
#' silent are listed in
#' \code{system.file("rules", "hscs2024", "readings.yaml", package = "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("hscs2024")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"hscs2024"}, the type in \code{name} and \code{rsg_or_order}, the
#'   type with its English gloss in \code{levels}, the key trace, and
#'   \code{provisional_because} (types before the assigned one that could
#'   not be decided).
#' @references Micheli E., Fuchs M., Lang V., Csorba A., Dobos E., Szegi T.,
#'   Kele G. (2024). A diagnosztikus szemleletu hazai talajosztalyozasi
#'   rendszer. Utmutato. Masodik kozelites. Magyar Agrar- es
#'   Elettudomanyi Egyetem, Godollo.
#' @seealso \code{\link{make_hscs2024_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_hscs2024(make_hscs2024_example("steppe"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_hscs2024 <- function(pedon, rules = NULL,
                              on_missing = c("warn", "silent", "error"),
                              gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cssc_prepare_pedon(pedon)
  rules <- rules %||% load_rules("hscs2024")
  res <- classify_hierarchical("hscs2024", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (inherits(pedon, "PedonRecord") &&
      all(is.na(pedon$horizons$designation %||% NA) | !nzchar(trimws(pedon$horizons$designation %||% "")))) {
    res$warnings <- c(res$warnings, paste(
      "No horizon designations: the Hungarian key reads some horizons from the FAO notation",
      "(an l or r suffix for reductive conditions, w for a cambic horizon, p for ploughing)."))
  }
  res
}


#' Example pedons for the Hungarian soil types
#'
#' Small, plausible profiles written for the tests and examples, one per
#' type of the Hungarian diagnostic soil classification (Micheli et al.
#' 2024), with designations in the FAO (2006) notation the text adopts and
#' the properties the key reads, including a total water-soluble salt
#' content (\code{soluble_salts_pct}) and exchangeable cations, so that the
#' types before each one are decided. They are not real profiles.
#'
#' @param type One of \code{"peat"}, \code{"anthropogenic"},
#'   \code{"rocky_skeletal"}, \code{"rock_influenced"}, \code{"solonetz"},
#'   \code{"solonchak"}, \code{"swelling_clay"}, \code{"meadow"},
#'   \code{"carbonate"}, \code{"steppe"}, \code{"clay_illuvial"},
#'   \code{"sandy"}, \code{"brown_earth"}, \code{"deposit"},
#'   \code{"weakly_developed"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_hscs2024_example("meadow")
#' p$horizons[, c("designation", "top_cm", "bottom_cm")]
#' @export
make_hscs2024_example <- function(type = c("peat", "anthropogenic", "rocky_skeletal", "rock_influenced",
                                           "solonetz", "solonchak", "swelling_clay", "meadow", "carbonate",
                                           "steppe", "clay_illuvial", "sandy", "brown_earth", "deposit",
                                           "weakly_developed")) {
  type <- match.arg(type)
  site <- list(id = paste0("hscs2024-", type), country = "HU", designation_system = "fao2006")
  # what the earlier types read, so that they are decided: low salts, low
  # sodium, no mottles, no carbonate
  base <- list(soluble_salts_pct = 0.02, na_cmol = 0.2, mg_cmol = 3, cec_cmol = 20,
               redoximorphic_features_pct = 0, coarse_fragments_pct = 0, caco3_pct = 0)
  mk <- function(h, s = site) {
    h <- utils::modifyList(base, h)
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = s, horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  switch(type,
    peat = mk(list(
      designation = c("H1", "H2", "Cr"), top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 120),
      oc_pct = c(35, 30, 1), bulk_density_g_cm3 = c(0.3, 0.35, 1.4),
      munsell_hue_moist = c("10YR", "10YR", "5Y"), munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 1))),
    anthropogenic = mk(list(
      designation = c("Au", "2Cu", "3C"), top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 130),
      layer_origin = c("urban fill", "fill", NA), artefacts_pct = c(40, 30, 0),
      oc_pct = c(2, 0.8, 0.2), clay_pct = 18, silt_pct = 40, sand_pct = 42,
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 3, 4))),
    rocky_skeletal = mk(list(
      designation = c("A", "R"), top_cm = c(0, 8), bottom_cm = c(8, 40), oc_pct = c(3, NA),
      clay_pct = c(15, NA), silt_pct = c(35, NA), sand_pct = c(50, NA),
      munsell_hue_moist = c("10YR", NA), munsell_value_moist = c(3, NA), munsell_chroma_moist = c(2, NA))),
    rock_influenced = mk(list(
      designation = c("A", "R"), top_cm = c(0, 20), bottom_cm = c(20, 50), oc_pct = c(4, NA),
      clay_pct = c(22, NA), silt_pct = c(40, NA), sand_pct = c(38, NA), caco3_pct = c(15, NA),
      munsell_hue_moist = c("10YR", NA), munsell_value_moist = c(2, NA), munsell_chroma_moist = c(2, NA))),
    solonetz = mk(list(
      designation = c("A", "E", "Btn", "Bkn", "Ck"), top_cm = c(0, 12, 20, 45, 80), bottom_cm = c(12, 20, 45, 80, 120),
      oc_pct = c(2, 1, 0.8, 0.4, 0.2), clay_pct = c(25, 22, 42, 38, 30), silt_pct = c(45, 48, 35, 40, 45),
      sand_pct = c(30, 30, 23, 22, 25), na_cmol = c(0.5, 1, 6, 7, 6), mg_cmol = c(3, 3, 8, 9, 8),
      cec_cmol = c(22, 18, 30, 28, 24), caco3_pct = c(0, 0, 0, 8, 12),
      structure_grade = c("moderate", "weak", "strong", "moderate", "structureless"),
      structure_type = c("granular", "platy", "columnar", "prismatic", "massive"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 4, 5, 6), munsell_chroma_moist = c(2, 2, 3, 3, 3))),
    solonchak = mk(list(
      designation = c("Az", "Cz", "Czl"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
      soluble_salts_pct = c(0.8, 0.4, 0.2), oc_pct = c(1.5, 0.5, 0.3), caco3_pct = c(10, 15, 15),
      clay_pct = c(20, 18, 15), silt_pct = c(50, 52, 50), sand_pct = c(30, 30, 35),
      structure_grade = c("weak", "structureless", "structureless"),
      structure_type = c("granular", "massive", "massive"),
      munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(2, 2, 2))),
    swelling_clay = mk(list(
      designation = c("A", "Bi", "C"), top_cm = c(0, 25, 80), bottom_cm = c(25, 80, 120),
      clay_pct = c(45, 50, 42), silt_pct = c(35, 30, 38), sand_pct = c(20, 20, 20), oc_pct = c(2.5, 1, 0.4),
      slickensides = c("few", "many", "common"), structure_grade = c("moderate", "strong", "moderate"),
      structure_type = c("granular", "wedge-shaped", "prismatic"), cracks_width_cm = c(2, 1, NA),
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4), munsell_chroma_moist = c(1, 2, 2))),
    meadow = mk(list(
      designation = c("A", "Bl", "Cr"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120),
      clay_pct = c(24, 26, 22), silt_pct = c(50, 50, 52), sand_pct = c(26, 24, 26), oc_pct = c(3, 1, 0.4),
      redoximorphic_features_pct = c(0, 15, 10),
      munsell_hue_moist = c("10YR", "2.5Y", "5Y"), munsell_value_moist = c(2, 5, 5), munsell_chroma_moist = c(1, 2, 1))),
    carbonate = mk(list(
      designation = c("Ak", "Ck"), top_cm = c(0, 20), bottom_cm = c(20, 110),
      caco3_pct = c(30, 35), oc_pct = c(1.5, 0.3), clay_pct = c(20, 18), silt_pct = c(55, 57), sand_pct = c(25, 25),
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6), munsell_chroma_moist = c(2, 3))),
    steppe = mk(list(
      designation = c("A", "AB", "Bk", "Ck"), top_cm = c(0, 40, 60, 90), bottom_cm = c(40, 60, 90, 130),
      oc_pct = c(2.5, 1.2, 0.5, 0.3), clay_pct = c(25, 26, 24, 22), silt_pct = c(55, 54, 56, 58), sand_pct = c(20, 20, 20, 20),
      bs_pct = c(90, 92, NA, NA), caco3_pct = c(0, 2, 12, 15), secondary_carbonates_pct = c(0, 0, 5, 2),
      structure_grade = c("strong", "moderate", "weak", "structureless"),
      structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5, 6), munsell_chroma_moist = c(2, 2, 3, 3),
      munsell_value_dry = c(4, 5, 6, 7), munsell_chroma_dry = c(2, 2, 3, 3))),
    clay_illuvial = mk(list(
      designation = c("A", "E", "Bt", "C"), top_cm = c(0, 20, 35, 80), bottom_cm = c(20, 35, 80, 120),
      clay_pct = c(15, 12, 30, 20), silt_pct = c(55, 58, 45, 50), sand_pct = c(30, 30, 25, 30),
      oc_pct = c(2, 0.6, 0.4, 0.2), bs_pct = c(60, 55, 70, 75), clay_films_amount = c(NA, NA, "common", NA),
      structure_grade = c("moderate", "weak", "moderate", "structureless"),
      structure_type = c("granular", "platy", "subangular blocky", "massive"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 5), munsell_chroma_moist = c(2, 3, 4, 4))),
    sandy = mk(list(
      designation = c("A", "C1", "C2"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 130),
      clay_pct = c(6, 3, 2), silt_pct = c(8, 4, 3), sand_pct = c(86, 93, 95), oc_pct = c(0.8, 0.2, 0.1),
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 6), munsell_chroma_moist = c(2, 4, 4))),
    brown_earth = mk(list(
      designation = c("A", "Bw", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
      clay_pct = c(20, 22, 20), silt_pct = c(42, 40, 42), sand_pct = c(38, 38, 38), oc_pct = c(1.5, 0.5, 0.2),
      structure_grade = c("moderate", "moderate", "structureless"),
      structure_type = c("granular", "subangular blocky", "massive"),
      munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(3, 4, 4))),
    deposit = mk(list(
      designation = c("A", "C1", "C2"), top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 120),
      clay_pct = c(18, 12, 15), silt_pct = c(62, 28, 70), sand_pct = c(20, 60, 15), oc_pct = c(1.2, 0.3, 0.5),
      structure_grade = c("weak", "structureless", "structureless"),
      structure_type = c("granular", "massive", "massive"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(2, 3, 3))),
    weakly_developed = mk(list(
      designation = c("A", "C"), top_cm = c(0, 15), bottom_cm = c(15, 100),
      clay_pct = c(18, 18), silt_pct = c(40, 40), sand_pct = c(42, 42), oc_pct = c(0.5, 0.1),
      structure_grade = c("weak", "structureless"), structure_type = c("granular", "massive"),
      munsell_hue_moist = "10YR", munsell_value_moist = c(5, 6), munsell_chroma_moist = c(3, 4)))
  )
}
