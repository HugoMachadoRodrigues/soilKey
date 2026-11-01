# v0.9.324: the soil phases of the Soil Classification System of Japan (SCSJ
# 2017, chapter V). Made-up profiles; no Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.ph <- function(p) classify_scsj2017(p, on_missing = "silent")$phases

test_that("the temperature phase, from the soil temperature or the USDA regime", {
  skip_on_cran()
  p <- make_scsj2017_example("brown_forest")
  for (t in list(c(5, "frigid"), c(8, "mesic"), c(14.9, "mesic"), c(15, "thermic"), c(22, "hyperthermic"))) {
    p$site$mean_annual_soil_temp_C <- as.numeric(t[1])
    ph <- .ph(p)
    expect_identical(ph$code[ph$kind == "temperature"], t[2], info = t[1])
  }
  p$site$mean_annual_soil_temp_C <- NULL
  p$site$soil_temperature_regime <- "isothermic"
  expect_identical(.ph(p)$code[1], "thermic")
  p$site$soil_temperature_regime <- "cryic"
  expect_identical(.ph(p)$code[1], "frigid")
  p$site$soil_temperature_regime <- NULL
  expect_null(.ph(p))
  # the class name is not changed
  expect_identical(classify_scsj2017(p, on_missing = "silent")$name, "Haplic Brown Forest soils")
})

test_that("man-made phases: Artifactual by gley, wet, gravel; Reformed; fill, mixed and cut on other soils", {
  skip_on_cran()
  p <- make_scsj2017_example("man_made")
  ph <- .ph(p)
  expect_identical(ph$code, "A1-plain")
  expect_identical(ph$name_native, "\u4eba\u5de5\u7269\u8cea\u9020\u6210\u76f8")
  # wetness or gley in the soil under the fill (a mottled fill would not be
  # a Man-made soil: criterion 1)
  p$horizons$redoximorphic_features_pct <- c(0, 0, 0, 10, 0)
  expect_identical(.ph(p)$code, "A1-wet")
  p$horizons$childs_test <- c(NA, NA, NA, NA, "positive")
  expect_identical(.ph(p)$code, "A1-gley")
  p$horizons$layer_origin <- c("fill (construction waste)", "fill", "fill", NA, NA)
  expect_identical(.ph(p)$name, "construction waste gley phase")
  # a fill of the same great group over an Andosol: an Andosol with a fill phase
  k <- list(designation = c("A", "C", "2Ab", "2Bw", "2C"), top_cm = c(0, 15, 40, 60, 90),
            bottom_cm = c(15, 40, 60, 90, 120), layer_origin = c("fill", "fill", "volcanic ash", "volcanic ash", "volcanic ash"),
            oc_pct = c(6, 4, 8, 2, 0.8), phosphate_absorption_coef = c(2000, 1900, 2200, 2000, 1700),
            clay_pct = c(18, 16, 20, 20, 15), field_texture = "loam", munsell_hue_moist = "10YR",
            munsell_value_moist = c(2, 3, 2, 4, 5), munsell_chroma_moist = c(2, 2, 1, 4, 6),
            structure_grade = c("weak", "weak", "moderate", "weak", "structureless"),
            ph_h2o = c(5.8, 5.9, 5.5, 5.7, 6.0), redoximorphic_features_pct = 0, artefacts_pct = 0)
  r <- classify_scsj2017(.jp(k), on_missing = "silent")
  expect_identical(r$levels$code[1], "C")
  expect_true("fill" %in% r$phases$code)
  expect_match(r$phases$name_native[r$phases$code == "fill"], "\u9ed2\u30dc\u30af\u571f\u76db\u571f\u9020\u6210\u76f8")
  # mixed and cut recorded at the site
  b <- make_scsj2017_example("brown_forest")
  b$site$mechanical_mixing_depth_cm <- 40; b$site$anthropic_truncation <- TRUE
  expect_setequal(.ph(b)$code, c("mixed", "cut"))
})

test_that("forest phases: the recorded type, the Podzols' development, the moisture phases", {
  skip_on_cran()
  b <- make_scsj2017_example("brown_forest")
  b$site$forest_soil_type <- "BD(d)"
  ph <- .ph(b)
  expect_identical(ph$code, "BDd")
  expect_identical(ph$name_native, "\u504f\u4e7e\u578b\u9069\u6f64\u6027\u76f8")
  # not forest: no forest phase
  b$site$land_use <- "upland crops"
  expect_null(.ph(b))
  # derived from the structure: a crumb A over a weakly blocky B
  d <- make_scsj2017_example("brown_forest")
  d$horizons$structure_type <- c("crumb", "subangular blocky", "subangular blocky", NA)
  expect_identical(.ph(d)$code, "BD")
  d$horizons$structure_type <- c("nutty", "nutty", "subangular blocky", NA)
  expect_identical(.ph(d)$code, "BC")
  # a massive, mottled B: wet phase
  w <- make_scsj2017_example("brown_forest")
  w$horizons$structure_type <- c("crumb", "massive", "massive", NA)
  w$horizons$redoximorphic_features_pct <- c(0, 5, 5, 0)
  r <- classify_scsj2017(w, on_missing = "silent")
  if (identical(r$levels$code[1], "I")) expect_identical(r$phases$code[r$phases$kind == "forest"], "BF")
  # structure not recorded: no moisture phase
  expect_null(.ph(make_scsj2017_example("brown_forest")))
  # Podzols: highly developed (morphology and bleached)
  expect_identical(.ph(make_scsj2017_example("podzol"))$code, "PDI")
})
