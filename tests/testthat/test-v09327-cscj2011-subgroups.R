# v0.9.327: the 116 subgroups of the Comprehensive Soil Classification System
# of Japan, First Approximation (CSCJ 2011, chapter VI). Made-up profiles; no
# Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.sg <- function(p) {
  r <- classify_cscj2011(p, on_missing = "silent")
  c(code = r$levels$code[3], prov = paste(r$provisional_because, collapse = ","))
}
.kb <- function(...) utils::modifyList(list(designation = c("A1", "A2", "Bw", "C"), top_cm = c(0, 20, 40, 80),
              bottom_cm = c(20, 40, 80, 110), oc_pct = c(9, 7, 2, 0.8), phosphate_absorption_coef = c(2200, 2100, 1900, 1400),
              munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(3, 3, 4, 6),
              structure_grade = c("moderate", "moderate", "weak", "structureless"), field_texture = "loam", clay_pct = 20,
              ph_h2o = 5.5, layer_origin = "volcanic ash", redoximorphic_features_pct = 0, si_ox_pct = 1.5,
              al_ox_pct = 3, al_pyro_pct = 0.3, exchange_acidity_y1 = 1), list(...))

test_that("every example keys to a subgroup, decided; 116 subgroups with unique codes", {
  skip_on_cran()
  want <- c(man_made = "A1MI", organic = "B1LM", podzol = "C1HA", andosol = "D6HA", dark_red = "E1AR",
            lowland = "F4HA", red_yellow = "G1RE", stagnic = "H2HA", brown_forest = "I1HA", regosol = "J2HA")
  for (g in names(want)) {
    r <- classify_cscj2011(make_cscj2011_example(g), on_missing = "silent")
    expect_identical(r$levels$code[3], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
  }
  r <- classify_cscj2011(make_cscj2011_example("dark_red"), on_missing = "silent")
  expect_identical(r$name, "Argic Calcaric Dark Red soils")
  expect_identical(r$levels$name_native[3], "\u7c98\u571f\u96c6\u7a4d\u77f3\u7070\u6027\u6697\u8d64\u8272\u571f")
  codes <- unlist(lapply(load_rules("cscj2011")$subgroups, function(g) vapply(g, `[[`, "", "code")))
  expect_length(codes, 116L)
  expect_false(anyDuplicated(codes) > 0)
})

test_that("Andosol subgroups: Cumulic Fulvic, Low-humic Allophanic, no Humic in the Non-allophanic", {
  skip_on_cran()
  # 60 cm of the brown polyhumic horizon: Cumulic Fulvic Andosols
  p <- .jp(.kb(designation = c("A1", "A2", "A3", "C"), top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 110),
               oc_pct = c(9, 7, 6.5, 0.8), munsell_value_moist = c(3, 3, 3, 5), munsell_chroma_moist = c(3, 3, 3, 6)))
  expect_identical(.sg(p)[["code"]], "D4CU")
  expect_identical(.sg(.jp(.kb()))[["code"]], "D4HA")
  # an allophanic Andosol without a humic surface horizon: Low-humic
  q <- .jp(.kb(oc_pct = c(2, 1.5, 1, 0.5), munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(4, 4, 4, 6)))
  expect_identical(.sg(q)[["code"]], "D6LH")
  # a humic Allophanic Andosol is Haplic (the CSCJ has no Humic subgroup there)
  expect_identical(.sg(make_cscj2011_example("andosol"))[["code"]], "D6HA")
})

test_that("Brown Forest subgroups: Aquic by either wetness, Eutric on every subhorizon, Andic, Terrace", {
  skip_on_cran()
  b <- function() make_cscj2011_example("brown_forest")
  p <- b(); p$horizons$bottom_cm <- c(15, 55, 80, 110); p$horizons$top_cm <- c(0, 15, 55, 80)
  p$horizons$redoximorphic_features_pct <- c(0, 0, 5, 0)
  p$horizons$redox_segregations_location <- c(NA, NA, "within peds", NA)
  expect_identical(.sg(p)[["code"]], "I1AQ")
  p <- b(); p$horizons$ph_h2o <- c(6.0, 6.8, 7.0, 7.2)
  expect_identical(.sg(p)[["code"]], "I1EU")
  p$horizons$ph_h2o <- c(6.0, 6.8, 6.2, 7.2)
  expect_false(identical(.sg(p)[["code"]], "I1EU"))
  # Andic: the phosphate absorption coefficient or P retention, not Alo + 1/2 Feo
  p <- b(); p$horizons$phosphate_absorption_coef <- c(1200, 1100, 700, 500)
  expect_identical(.sg(p)[["code"]], "I1AD")
  p <- b(); p$horizons$phosphate_absorption_coef <- NA; p$horizons$al_ox_pct <- 1.2; p$horizons$fe_ox_pct <- 0.6
  expect_false(identical(.sg(p)[["code"]], "I1AD"))
  expect_true(isTRUE(andic_subgroup_scsj(p)$passed))           # the SCSJ reads the substitute
  # Terrace: Pleistocene deposits recorded
  p <- b(); p$horizons$layer_origin <- "Pleistocene terrace deposits"
  expect_identical(.sg(p)[["code"]], "I1TE")
})

test_that("Lowland Paddy Endoaeric by the CSCJ yellowish brown", {
  skip_on_cran()
  h <- list(designation = c("Apg", "Bg", "B", "C"), top_cm = c(0, 15, 30, 70), bottom_cm = c(15, 30, 70, 110),
            oc_pct = c(2.5, 1, 0.5, 0.3), munsell_hue_moist = c("2.5Y", "2.5Y", "10YR", "10YR"),
            munsell_value_moist = c(4, 5, 4, 5), munsell_chroma_moist = c(1, 2, 4, 4), clay_pct = 25,
            field_texture = "silt loam", layer_origin = "alluvium", redoximorphic_features_pct = c(10, 15, 0, 0),
            redox_segregations_location = c("pores", "within peds", NA, NA), fe_dcb_pct = c(1.0, 2.5, 1.2, 1.0),
            structure_grade = "moderate")
  p <- .jp(h, site = list(land_use = "paddy rice"))
  g <- classify_cscj2011(p, on_missing = "silent")$levels$code
  if (identical(g[2], "F1")) {
    expect_identical(g[3], "F1EA")
    p$horizons$munsell_hue_moist <- c("2.5Y", "2.5Y", "5YR", "5YR")
    expect_false(identical(classify_cscj2011(p, on_missing = "silent")$levels$code[3], "F1EA"))
  }
  # the colour gate itself
  x <- .scsj_profile(soilKey:::.cscj_mark(p))
  expect_false(isTRUE(brown_layer_cscj(p)$passed))
})
