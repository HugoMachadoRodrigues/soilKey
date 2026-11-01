# v0.9.329: the soil phases of the Comprehensive Soil Classification System of
# Japan, First Approximation (CSCJ 2011, chapter VIII). Made-up profiles; no
# Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.ph <- function(p) classify_cscj2011(p, on_missing = "silent")$phases

test_that("temperature: soil, USDA regime, else air plus 2 C (estimated)", {
  skip_on_cran()
  p <- make_cscj2011_example("brown_forest")
  p$site$mean_annual_soil_temp_C <- 14
  expect_identical(.ph(p)$code, "mesic")
  p$site$mean_annual_soil_temp_C <- NULL; p$site$mean_annual_air_temp_C <- 13.5
  ph <- .ph(p)
  expect_identical(ph$code, "thermic")
  expect_match(ph$basis, "estimated")
  # the SCSJ does not estimate from the air
  expect_null(classify_scsj2017(p, on_missing = "silent")$phases)
  # no forest phases in the CSCJ
  p$site$forest_soil_type <- "BD"
  expect_false(any(.ph(p)$kind == "forest"))
})

test_that("man-made phases with the CSCJ names", {
  skip_on_cran()
  # the text's example: a high-humic kuroboku fill
  a <- make_cscj2011_example("andosol"); a$horizons$layer_origin[1] <- "soil dressing"
  ph <- .ph(a)
  expect_identical(ph$name_native[ph$code == "fill"], "\u591a\u8150\u690d\u8cea\u9ed2\u30dc\u30af\u571f\u76db\u571f\u9020\u6210\u76f8")
  # a coarse-textured dredged fill
  l <- make_cscj2011_example("lowland"); l$horizons$layer_origin[1] <- "dredged sand"
  l$horizons$texture_class_jp <- c("LS", "S", "LS", "S")
  ph <- .ph(l)
  expect_identical(ph$name_native[ph$code == "fill"], "\u7c97\u7c92\u8cea\u6d5a\u6e2b\u571f\u7802\u76db\u571f\u9020\u6210\u76f8")
  # 45 cm of it: a Reformed soil, the fill over the original soil's group
  l$horizons$layer_origin[2] <- "dredged sand"
  r <- classify_cscj2011(l, on_missing = "silent")
  expect_identical(r$levels$code[2], "A2")
  expect_identical(r$phases$name_native,
                   "\u7c97\u7c92\u8cea\u6d5a\u6e2b\u571f\u7802\uff0f\u672a\u719f\u4f4e\u5730\u571f\u9020\u6210\u76f8")
  # the Artifactual soils: gravel not recorded is no gravel layer in the CSCJ
  m <- make_cscj2011_example("man_made")
  expect_identical(.ph(m)$code, "A1-plain")
  expect_null(classify_scsj2017(m, on_missing = "silent")$phases)
  m$horizons$gravel_pct <- c(30, 0, 0, 0, 0)
  expect_identical(.ph(m)$name_native, "\u4eba\u5de5\u7269\u8cea\u792b\u8cea\u76f8")
})

test_that("Reformed soils: fill (texture and great group) over the original soil's group", {
  skip_on_cran()
  k <- list(designation = c("^A", "^C", "2A", "2Bw", "2C"), top_cm = c(0, 20, 45, 60, 90), bottom_cm = c(20, 45, 60, 90, 120),
            layer_origin = c("fill (alluvium)", "fill (alluvium)", "weathered sandstone", "weathered sandstone", "weathered sandstone"),
            oc_pct = c(2, 1, 2, 0.5, 0.3), phosphate_absorption_coef = c(600, 500, 600, 500, 400), clay_pct = c(30, 30, 25, 40, 35),
            texture_class_jp = c("LiC", "LiC", "CL", "LiC", "LiC"),
            munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5YR", "2.5YR"), munsell_value_moist = c(3, 4, 4, 4, 5),
            munsell_chroma_moist = c(2, 3, 3, 6, 8), structure_grade = c("weak", "weak", "moderate", "moderate", "weak"),
            ph_h2o = 5.0, bs_pct = 30, redoximorphic_features_pct = 0, artefacts_pct = 0, gravel_pct = 0)
  r <- classify_cscj2011(.jp(k, site = list(landform = "terrace")), on_missing = "silent")
  expect_identical(r$levels$code[2], "A2")
  expect_identical(r$phases$name_native,
                   "\u7d30\u7c92\u8cea\u4f4e\u5730\u571f\uff0f\u7c98\u571f\u96c6\u7a4d\u8d64\u9ec4\u8272\u571f\u9020\u6210\u76f8")
})
