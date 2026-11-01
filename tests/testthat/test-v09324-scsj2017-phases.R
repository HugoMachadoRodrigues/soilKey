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
.code <- function(p, kind = NULL) { ph <- .ph(p); if (is.null(ph)) NULL else if (is.null(kind)) ph$code else ph$code[ph$kind == kind] }

test_that("the temperature phase, from the soil temperature or the USDA regime", {
  skip_on_cran()
  p <- make_scsj2017_example("brown_forest")
  for (t in list(c(5, "frigid"), c(8, "mesic"), c(14.9, "mesic"), c(15, "thermic"), c(22, "hyperthermic"))) {
    p$site$mean_annual_soil_temp_C <- as.numeric(t[1])
    expect_identical(.code(p, "temperature"), t[2], info = t[1])
  }
  p$site$mean_annual_soil_temp_C <- "12,5"
  expect_identical(.code(p), "mesic")
  p$site$mean_annual_soil_temp_C <- NULL
  p$site$soil_temperature_regime <- "isothermic"
  expect_identical(.code(p), "thermic")
  p$site$soil_temperature_regime <- "isohyperthermic"
  expect_identical(.code(p), "hyperthermic")
  p$site$soil_temperature_regime <- "cryic"
  expect_identical(.code(p), "frigid")
  p$site$soil_temperature_regime <- "mesic/thermic"
  expect_null(.ph(p))
  p$site$soil_temperature_regime <- NULL
  expect_null(.ph(p))
  # the class name is not changed; print shows the phases
  p$site$mean_annual_soil_temp_C <- 10
  r <- classify_scsj2017(p, on_missing = "silent")
  expect_identical(r$name, "Haplic Brown Forest soils")
  expect_true(any(grepl("Phases: Mesic", utils::capture.output(print(r), type = "message"))))
})

test_that("Artifactual soils: one phase, gley before wet before gravelly; undecided gives none", {
  skip_on_cran()
  p <- make_scsj2017_example("man_made")
  # gravel not recorded: gravelly or plain is undecided
  expect_null(.ph(p))
  p$horizons$gravel_pct <- 0
  expect_identical(.code(p), "A1-plain")
  expect_identical(.ph(p)$name_native, "\u4eba\u5de5\u7269\u8cea\u9020\u6210\u76f8")
  p$horizons$gravel_pct <- c(30, 0, 0, 0, 0)
  expect_identical(.ph(p)$name_native, "\u4eba\u5de5\u7269\u8cea\u792b\u8cea\u76f8")
  p$horizons$gravel_pct <- 0
  # wetness or gley in the soil under the fill (a mottled fill would not be
  # a Man-made soil: criterion 1)
  p$horizons$redoximorphic_features_pct <- c(0, 0, 0, 10, 0)
  expect_identical(.code(p), "A1-wet")
  p$horizons$childs_test <- c(NA, NA, NA, NA, "positive")
  expect_identical(.code(p), "A1-gley")
  # the recorded material, without the words that only say fill
  p$horizons$layer_origin <- c("landfill (construction waste)", "fill", "fill", NA, NA)
  expect_identical(.ph(p)$name, "construction waste gley phase")
  expect_identical(.ph(p)$name_native, "\u4eba\u5de5\u7269\u8cea\u30b0\u30e9\u30a4\u76f8")
  p$horizons$layer_origin <- c("\u76db\u571f\uff08\u5efa\u7bc9\u5ec3\u6750\uff09", "fill", "fill", NA, NA)
  expect_identical(.ph(p)$name_native, "\u5efa\u7bc9\u5ec3\u6750\u30b0\u30e9\u30a4\u76f8")
})

test_that("Reformed soils: the fill over the original soil, with the gley phase of the supplement", {
  skip_on_cran()
  k <- list(designation = c("^A", "^C", "2A", "2Bg", "2G"), top_cm = c(0, 20, 45, 60, 75), bottom_cm = c(20, 45, 60, 75, 110),
            layer_origin = c("fill (volcanic ash)", "fill (volcanic ash)", "alluvium", "alluvium", "alluvium"),
            oc_pct = c(6, 4, 2, 0.8, 0.5), phosphate_absorption_coef = c(2000, 1900, 600, 500, 400),
            clay_pct = c(18, 16, 20, 22, 25), field_texture = c("loam", "loam", "silt loam", "silty clay loam", "silty clay"),
            munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y", "5GY"), munsell_value_moist = c(2, 3, 3, 5, 5),
            munsell_chroma_moist = c(2, 2, 2, 2, 1), structure_grade = c("weak", "weak", "moderate", "weak", "structureless"),
            ph_h2o = 6, redoximorphic_features_pct = c(0, 0, 0, 10, 2), childs_test = c(NA, NA, NA, NA, "positive"),
            artefacts_pct = 0, gravel_pct = 0)
  r <- classify_scsj2017(.jp(k), on_missing = "silent")
  expect_identical(r$levels$code[2], "A2")
  expect_identical(r$phases$code, "A2-gley")
  expect_identical(r$phases$name_native, "\u9ed2\u30dc\u30af\u571f\uff0f\u6c96\u7a4d\u571f\u30b0\u30e9\u30a4\u76f8")
})

test_that("fill, mixed and cut phases on the other great groups", {
  skip_on_cran()
  # a thin fill is named by its materials: a kuroboku Ap of soil dressing
  a <- make_scsj2017_example("andosol")
  a$horizons$layer_origin[1] <- "soil dressing"
  expect_identical(.ph(a)$name_native[.ph(a)$code == "fill"], "\u9ed2\u30dc\u30af\u571f\u76db\u571f\u9020\u6210\u76f8")
  # a fill of the same great group over an Andosol (35 cm or more)
  k <- list(designation = c("A", "C", "2Ab", "2Bw", "2C"), top_cm = c(0, 15, 40, 60, 90),
            bottom_cm = c(15, 40, 60, 90, 120), layer_origin = c("fill", "fill", "volcanic ash", "volcanic ash", "volcanic ash"),
            oc_pct = c(6, 4, 8, 2, 0.8), phosphate_absorption_coef = c(2000, 1900, 2200, 2000, 1700),
            clay_pct = c(18, 16, 20, 20, 15), field_texture = "loam", munsell_hue_moist = "10YR",
            munsell_value_moist = c(2, 3, 2, 4, 5), munsell_chroma_moist = c(2, 2, 1, 4, 6),
            structure_grade = c("moderate", "weak", "moderate", "weak", "structureless"),
            ph_h2o = c(5.8, 5.9, 5.5, 5.7, 6.0), redoximorphic_features_pct = 0, artefacts_pct = 0)
  r <- classify_scsj2017(.jp(k), on_missing = "silent")
  expect_identical(r$levels$code[1], "C")
  expect_identical(r$phases$name_native[r$phases$code == "fill"], "\u9ed2\u30dc\u30af\u571f\u76db\u571f\u9020\u6210\u76f8")
  # mixing within the plough layer, or a purpose 'none': no phase
  b <- make_scsj2017_example("brown_forest")
  b$site$mechanical_mixing_depth_cm <- 10
  expect_null(.ph(b))
  b$site$mechanical_mixing_depth_cm <- 40
  expect_identical(.code(b), "mixed")
  b$site$mechanical_mixing_depth_cm <- NULL; b$site$mechanical_mixing_purpose <- "none"
  expect_null(.ph(b))
  # cut: a recorded truncation ('yes'); a clear-cut forest is not a cut
  b$site$mechanical_mixing_purpose <- NULL; b$site$land_use <- "clear-cut forest"
  expect_null(.ph(b))
  b$site$anthropic_truncation <- "yes"
  expect_identical(.code(b), "cut")
})

test_that("forest phases: forest land, the recorded type in any spelling, the 1975 groups", {
  skip_on_cran()
  b <- make_scsj2017_example("brown_forest")
  b$site$forest_soil_type <- "BD(d)"
  expect_identical(.code(b), "BDd")
  expect_identical(.ph(b)$name_native, "\u504f\u4e7e\u578b\u9069\u6f64\u6027\u76f8")
  for (ft in list(c("BDd", "BDd"), c("\uff22\uff24", "BD"), c("BlD(d)", "BDd"), c("lBlD", "BD"), c("RC", "BC"), c("dRB", "BB")))
    { b$site$forest_soil_type <- ft[1]; expect_identical(.code(b), ft[2], info = ft[1]) }
  b$site$forest_soil_type <- "BD"
  # not forest land; the words that only look like it
  for (lu in c("upland crops", "orange orchard", "tea plantation", "agroforestry", "\u8fb2\u6797\u5730"))
    { b$site$land_use <- lu; expect_null(.ph(b), info = lu) }
  for (lu in c("cedar plantation forest", "\u6749\u6797", "grazing"))
    { b$site$land_use <- lu; expect_identical(.code(b), "BD", info = lu) }
  # a recorded type says forest land when no land use is recorded
  b$site$land_use <- NULL
  expect_identical(.code(b), "BD")
  # a wet podzol type gives a Podzol no development phase
  p <- make_scsj2017_example("podzol"); p$site$forest_soil_type <- "PW(i)"
  expect_null(.ph(p))
  p$site$forest_soil_type <- "PD-II"
  expect_identical(.code(p), "PDII")
})

test_that("forest moisture phases from the structures, three-valued", {
  skip_on_cran()
  d <- make_scsj2017_example("brown_forest")
  expect_null(.ph(d))                    # no structure recorded
  st <- function(...) { d$horizons$structure_type <- c(...); .code(d) }
  expect_identical(st("crumb", "subangular blocky", "subangular blocky", NA), "BD")
  expect_identical(st("\u56e3\u7c92\u72b6", "\u584a\u72b6", "\u584a\u72b6", NA), "BD")   # crumb, in Japanese
  expect_identical(st("nutty", "nutty", "subangular blocky", NA), "BC")
  expect_identical(st("nutty", "angular blocky", "subangular blocky", NA), "BC")         # angular blocky = nutty
  expect_identical(st("nutty", "subangular blocky", "subangular blocky", NA), "BDd")     # nutty lower A only
  expect_identical(st("granular", "subangular blocky", "subangular blocky", NA), "BDd")
  expect_identical(st("crumb", NA, NA, NA), "BD")
  # a massive, mottled B: wet (the profile itself keys to the Stagnic soils,
  # which take no forest phase); mottles not recorded: undecided
  d$horizons$structure_type <- c("crumb", "massive", "massive", NA)
  d$horizons$redoximorphic_features_pct <- c(0, 5, 5, 0)
  expect_identical(.scsj_moisture_phase(.scsj_profile(d)), "BF")
  expect_identical(classify_scsj2017(d, on_missing = "silent")$levels$code[1], "G")
  expect_null(.ph(d))
  d$horizons$redoximorphic_features_pct <- c(0, NA, NA, 0)
  expect_true(is.na(.scsj_moisture_phase(.scsj_profile(d))))
  # slightly wet: no O, a crumb A of 25 cm or more, a B without blocky,
  # nutty or granular structure; a blocky B stays BD
  d$horizons$redoximorphic_features_pct <- 0
  d$horizons$top_cm <- c(0, 30, 45, 80); d$horizons$bottom_cm <- c(30, 45, 80, 110)
  expect_identical(st("crumb", "massive", "massive", NA), "BE")
  expect_identical(st("crumb", "subangular blocky", "subangular blocky", NA), "BD")
})

test_that("Podzols: the development phases on the key's pairings; leached spots not recorded", {
  skip_on_cran()
  p <- make_scsj2017_example("podzol")
  expect_identical(.code(p), "PDI")
  # no bleached horizon: analysis with morphology, moderately or weakly
  # developed by the leached spots; not recorded: no phase
  p$horizons$designation[2] <- "A"; p$horizons$munsell_value_moist[2] <- 4; p$horizons$munsell_chroma_moist[2] <- 3
  expect_identical(classify_scsj2017(p, on_missing = "silent")$levels$code[1], "D")
  expect_null(.ph(p))
  p$horizons$leached_spots <- c(NA, TRUE, NA, NA, NA)
  expect_identical(.code(p), "PDII")
  p$horizons$leached_spots <- FALSE
  expect_identical(.code(p), "PDIII")
})
