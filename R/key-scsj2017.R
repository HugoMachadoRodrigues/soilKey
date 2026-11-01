# =============================================================================
# v0.9.321 -- the Soil Classification System of Japan (SCSJ 2017, Japanese
# Society of Pedology), keyed to the ten great groups of chapter II and the
# 26 groups (0.9.322) and 120 subgroups (0.9.323) of chapter III. The soil
# phases follow.
# Experimental and opt-in:
# classify_all() runs it only when asked.
# =============================================================================

#' Classify a pedon under the Soil Classification System of Japan
#'
#' Keys a \code{\link{PedonRecord}} through the Soil Classification System
#' of Japan (Japanese Society of Pedology 2017), following its key to great
#' groups. \strong{Experimental}: this release keys the ten great groups
#' (Man-made soils, Organic Soils, Andosols, Podzols, Fluvic soils,
#' Red-Yellow soils, Stagnic soils, Eutrosols, Brown Forest soils,
#' Regosols) and their 26 groups (for instance Allophanic and
#' Non-allophanic Andosols, Fluvic Paddy soils, Magnesian Eutrosols, Sandy
#' Regosols), and their 120 subgroups (Haplic, Humic, Cumulic, Aquic ...);
#' the soil phases follow. Each level gives
#' the official English name and, in \code{levels$name_native}, the Japanese
#' one.
#'
#' The SCSJ reads the Japanese horizon notation (H, O, A, E, B, C, G with Go
#' and Gr, R; p ploughed, g gleyed, t, w, s, h, ir, mn; a 2 or II prefix for
#' a lithologic discontinuity, a trailing b for buried; the FAO/USDA notation
#' by the same letters), moist Munsell colours, texture, organic carbon,
#' the phosphate absorption coefficient (\code{phosphate_absorption_coef},
#' mg P2O5/100 g) or its substitutes (P retention, Alo + 1/2 Feo) and the
#' field active-aluminium test (\code{active_al_test}), rubbed fibre, pH,
#' base saturation, exchangeable Ca and Mg, Fe(d), Sio, Alp/Alo, the
#' exchange acidity (\code{exchange_acidity_y1}), the dipyridyl test
#' (\code{childs_test}), fluidity
#' and \code{n_value}, mottles and where they are, artefacts, and the
#' recorded origin of the layers (\code{layer_origin}, \code{rock_origin}:
#' fill, volcanic ejecta, alluvial deposits). From the site it reads
#' \code{land_use} or \code{flood_irrigated} (paddy land),
#' \code{parent_material}, \code{drainage_class},
#' \code{artificially_drained}, \code{acid_neutralising_amendment} and
#' \code{designation_system}. The choices where the text is silent are listed
#' in \code{system.file("rules", "scsj2017", "readings.yaml", package =
#' "soilKey")}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{load_rules("scsj2017")}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}} with \code{system_id}
#'   \code{"scsj2017"}, the deepest class in \code{name}, the great group in
#'   \code{rsg_or_order}, the classes of each level in \code{levels} (with
#'   \code{name_native}), the key trace, and \code{provisional_because}
#'   (classes before the assigned one that could not be decided).
#' @references The Fifth Committee for Soil Classification and Nomenclature
#'   (2017). Soil Classification System of Japan. The Japanese Society of
#'   Pedology.
#' @seealso \code{\link{make_scsj2017_example}}, \code{\link{soilkey_systems}}.
#' @examples
#' res <- classify_scsj2017(make_scsj2017_example("andosol"), on_missing = "silent")
#' res$name
#' res$levels
#' @export
classify_scsj2017 <- function(pedon, rules = NULL,
                              on_missing = c("warn", "silent", "error"),
                              gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  if (inherits(pedon, "PedonRecord")) pedon <- .cssc_prepare_pedon(pedon)
  rules <- rules %||% load_rules("scsj2017")
  res <- classify_hierarchical("scsj2017", pedon, rules = rules,
                               on_missing = on_missing, gapfill = gapfill)
  if (inherits(pedon, "PedonRecord") &&
      all(is.na(pedon$horizons$designation %||% NA) | !nzchar(trimws(pedon$horizons$designation %||% "")))) {
    res$warnings <- c(res$warnings, paste(
      "No horizon designations: the SCSJ reads several horizons from the notation",
      "(H peat, G gley, Bs/Bh podzolic, Bt clay, Ap plough layer), which are then read from properties alone."))
  }
  res
}


#' Example pedons for the Japanese great groups
#'
#' Small, plausible profiles written for the tests and examples, one per
#' great group of the Soil Classification System of Japan (2017), with
#' designations in the Japanese notation and the properties the key reads.
#' They are not real profiles.
#'
#' @param great_group One of \code{"man_made"}, \code{"organic"},
#'   \code{"andosol"}, \code{"podzol"}, \code{"fluvic"},
#'   \code{"red_yellow"}, \code{"stagnic"}, \code{"eutrosol"},
#'   \code{"brown_forest"}, \code{"regosol"}.
#' @return A \code{\link{PedonRecord}}.
#' @examples
#' p <- make_scsj2017_example("podzol")
#' p$horizons[, c("designation", "top_cm", "bottom_cm")]
#' @export
make_scsj2017_example <- function(great_group = c("man_made", "organic", "andosol", "podzol", "fluvic",
                                                  "red_yellow", "stagnic", "eutrosol", "brown_forest",
                                                  "regosol")) {
  great_group <- match.arg(great_group)
  base <- list(id = paste0("scsj2017-", great_group), country = "JP", designation_system = "scsj",
               drainage_class = "well drained", land_use = "forest")
  mk <- function(h, site = base) {
    n <- max(lengths(h))
    h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
    PedonRecord$new(site = site, horizons = ensure_horizon_schema(
      data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  }
  site <- function(...) utils::modifyList(base, list(...))
  switch(great_group,
    man_made = mk(list(
      designation = c("A", "C1", "C2", "2Ab", "2Bw"), top_cm = c(0, 10, 30, 55, 70),
      bottom_cm = c(10, 30, 55, 70, 110), layer_origin = c("fill", "fill", "fill", NA, NA),
      artefacts_pct = c(25, 40, 30, 0, 0), field_texture = c("loam", "sandy loam", "sandy loam", "clay loam", "clay loam"),
      clay_pct = c(18, 12, 12, 30, 32), oc_pct = c(2.0, 0.6, 0.5, 3.5, 1.0),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y", "10YR", "10YR"), munsell_value_moist = c(3, 4, 5, 2, 4),
      munsell_chroma_moist = c(3, 3, 3, 2, 4), structure_grade = c("weak", "structureless", "structureless", "moderate", "moderate"),
      ph_h2o = c(7.0, 7.4, 7.5, 5.6, 5.5), phosphate_absorption_coef = c(600, 400, 350, 900, 800),
      redoximorphic_features_pct = 0, artefacts_organic = c(FALSE, FALSE, FALSE, NA, NA)),
      site(land_use = "urban park")),
    organic = mk(list(
      designation = c("H1", "H2", "H3", "G"), top_cm = c(0, 25, 55, 85), bottom_cm = c(25, 55, 85, 110),
      oc_pct = c(35, 30, 28, 2), fiber_content_rubbed_pct = c(40, 30, 20, NA),
      munsell_hue_moist = c("7.5YR", "7.5YR", "10YR", "5GY"), munsell_value_moist = c(2, 2, 2, 5),
      munsell_chroma_moist = c(2, 1, 1, 1), ph_h2o = c(4.8, 4.9, 5.0, 6.0),
      phosphate_absorption_coef = c(500, 450, 450, 600), childs_test = c(NA, NA, NA, "positive"),
      layer_origin = c(NA, NA, NA, "lacustrine clay"), peat_moor_type = c("low-moor", "low-moor", "low-moor", NA)),
      site(drainage_class = "very poorly drained", land_use = "wetland")),
    andosol = mk(list(
      designation = c("A1", "A2", "Bw", "C"), top_cm = c(0, 20, 45, 80), bottom_cm = c(20, 45, 80, 110),
      oc_pct = c(9, 6, 2, 0.8), phosphate_absorption_coef = c(2200, 2100, 1900, 1400),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR"), munsell_value_moist = c(2, 2, 4, 5),
      munsell_chroma_moist = c(1, 2, 4, 6), structure_grade = c("moderate", "moderate", "weak", "structureless"),
      field_texture = c("loam", "loam", "silt loam", "sandy loam"), clay_pct = c(20, 22, 18, 10),
      ph_h2o = c(5.4, 5.6, 5.8, 6.0), bulk_density_g_cm3 = c(0.6, 0.65, 0.75, 0.9),
      layer_origin = c("volcanic ash", "volcanic ash", "volcanic ash", "pumice"), redoximorphic_features_pct = 0,
      si_ox_pct = c(1.2, 1.5, 2.0, 1.8), al_ox_pct = c(3.0, 3.4, 3.8, 2.6), al_pyro_pct = c(0.9, 0.8, 0.5, 0.3),
      exchange_acidity_y1 = c(1.5, 1.0, 0.5, 0.4))),
    podzol = mk(list(
      designation = c("O", "E", "Bhs", "Bs", "C"), top_cm = c(-6, 0, 10, 18, 45), bottom_cm = c(0, 10, 18, 45, 90),
      oc_pct = c(40, 1.0, 4.0, 1.8, 0.4), phosphate_absorption_coef = c(NA, 300, 1100, 1200, 700),
      munsell_hue_moist = c("7.5YR", "10YR", "5YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 6, 3, 4, 6),
      munsell_chroma_moist = c(2, 2, 3, 4, 4), field_texture = c(NA, "sandy loam", "sandy loam", "sandy loam", "loamy sand"),
      clay_pct = c(NA, 8, 10, 9, 6), ph_h2o = c(3.9, 4.2, 4.4, 4.7, 5.0),
      al_ox_pct = c(NA, 0.1, 0.9, 1.0, 0.4), fe_ox_pct = c(NA, 0.1, 0.6, 0.4, 0.2),
      layer_origin = c(NA, "weathered granite", "weathered granite", "weathered granite", "weathered granite"),
      redoximorphic_features_pct = 0),
      site(parent_material = "granite")),
    fluvic = mk(list(
      designation = c("A", "C1", "C2", "C3"), top_cm = c(0, 15, 45, 80), bottom_cm = c(15, 45, 80, 110),
      oc_pct = c(2.5, 0.8, 0.5, 0.3), phosphate_absorption_coef = c(700, 500, 450, 400),
      munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(3, 4, 5, 5),
      munsell_chroma_moist = c(2, 3, 3, 2), field_texture = c("silt loam", "sandy loam", "loamy sand", "sand"),
      clay_pct = c(16, 10, 6, 3), ph_h2o = c(6.0, 6.2, 6.4, 6.5),
      layer_origin = c("alluvium", "alluvium", "alluvium", "alluvium"), redoximorphic_features_pct = 0),
      site(land_use = "upland crops")),
    red_yellow = mk(list(
      designation = c("A", "Bt1", "Bt2", "BC"), top_cm = c(0, 10, 40, 85), bottom_cm = c(10, 40, 85, 110),
      oc_pct = c(3.0, 0.6, 0.4, 0.2), phosphate_absorption_coef = c(800, 700, 650, 600),
      munsell_hue_moist = c("10YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(3, 4, 5, 5),
      munsell_chroma_moist = c(3, 6, 8, 6), clay_pct = c(25, 45, 48, 38),
      field_texture = c("loam", "clay", "clay", "clay loam"), clay_films_amount = c(NA, "common", "many", "few"),
      structure_grade = c("moderate", "moderate", "moderate", "weak"), ph_h2o = c(4.9, 4.8, 4.9, 5.0),
      bs_pct = c(30, 20, 18, 20), layer_origin = "weathered sedimentary rock", redoximorphic_features_pct = 0)),
    stagnic = mk(list(
      designation = c("Ap", "Bg1", "Bg2", "Cg"), top_cm = c(0, 15, 40, 75), bottom_cm = c(15, 40, 75, 110),
      oc_pct = c(3.0, 0.8, 0.5, 0.3), phosphate_absorption_coef = c(900, 800, 700, 600),
      munsell_hue_moist = c("10YR", "2.5Y", "2.5Y", "5Y"), munsell_value_moist = c(3, 5, 6, 6),
      munsell_chroma_moist = c(2, 2, 2, 1), clay_pct = c(28, 32, 33, 30),
      field_texture = c("clay loam", "clay loam", "clay loam", "clay loam"),
      redoximorphic_features_pct = c(2, 20, 25, 10),
      redox_segregations_location = c(NA, "within peds", "within peds", "matrix"),
      structure_grade = c("moderate", "moderate", "weak", "structureless"), ph_h2o = c(5.8, 6.0, 6.2, 6.3),
      layer_origin = "Pleistocene terrace deposits"),
      site(drainage_class = "poorly drained", land_use = "upland crops")),
    eutrosol = mk(list(
      designation = c("A", "Bw1", "Bw2", "C"), top_cm = c(0, 15, 45, 80), bottom_cm = c(15, 45, 80, 110),
      oc_pct = c(3.5, 1.0, 0.6, 0.3), phosphate_absorption_coef = c(800, 700, 650, 500),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(3, 4, 4, 5),
      munsell_chroma_moist = c(2, 4, 4, 3), clay_pct = c(30, 35, 35, 28),
      field_texture = c("clay loam", "clay", "clay", "clay loam"),
      structure_grade = c("moderate", "moderate", "moderate", "weak"),
      ph_h2o = c(6.6, 6.9, 7.1, 7.3), bs_pct = c(80, 90, 95, 100), layer_origin = "weathered serpentinite",
      ca_cmol = c(4, 2.5, 2, 1.5), mg_cmol = c(10, 18, 20, 22), redoximorphic_features_pct = 0)),
    brown_forest = mk(list(
      designation = c("A", "B1", "B2", "C"), top_cm = c(0, 15, 45, 80), bottom_cm = c(15, 45, 80, 110),
      oc_pct = c(6, 1.8, 0.8, 0.3), phosphate_absorption_coef = c(1100, 900, 700, 500),
      munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(2, 4, 5, 5),
      munsell_chroma_moist = c(2, 4, 5, 4), clay_pct = c(22, 25, 24, 18),
      field_texture = c("loam", "clay loam", "clay loam", "sandy loam"),
      structure_grade = c("moderate", "moderate", "weak", "structureless"),
      ph_h2o = c(4.8, 5.0, 5.2, 5.4), bs_pct = c(20, 15, 18, 25), layer_origin = "weathered granite",
      redoximorphic_features_pct = 0)),
    regosol = mk(list(
      designation = c("A", "C1", "C2"), top_cm = c(0, 8, 40), bottom_cm = c(8, 40, 100),
      oc_pct = c(1.0, 0.2, 0.1), phosphate_absorption_coef = c(250, 150, 120),
      munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 6, 6),
      munsell_chroma_moist = c(2, 3, 3), clay_pct = c(4, 2, 2), field_texture = c("loamy sand", "sand", "sand"),
      structure_grade = c("weak", "structureless", "structureless"), ph_h2o = c(6.5, 6.8, 7.0),
      layer_origin = c("dune sand", "dune sand", "dune sand"), redoximorphic_features_pct = 0,
      caco3_pct = c(0, 0, 0)),
      site(land_use = "coastal pine forest"))
  )
}
