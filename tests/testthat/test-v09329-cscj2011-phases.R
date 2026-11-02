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

test_that("the text's natural combination: a humic kuroboku fill on alluvium (IX, step D)", {
  skip_on_cran()
  h <- list(designation = c("A1", "A2", "2C1", "2C2"), top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 110),
            layer_origin = c("fill (volcanic ash)", "fill (volcanic ash)", "river alluvium", "river alluvium"),
            oc_pct = c(5, 4, 0.8, 0.5), phosphate_absorption_coef = c(2000, 1900, 700, 600),
            al_ox_pct = c(2.8, 2.6, 0.4, 0.3), fe_ox_pct = c(1.2, 1.1, 0.5, 0.4), si_ox_pct = c(1, 1, 0.1, 0.1),
            bulk_density_g_cm3 = c(0.7, 0.75, 1.2, 1.3),
            munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(2, 3, 4, 5),
            munsell_chroma_moist = c(2, 2, 3, 3), structure_grade = c("moderate", "moderate", "weak", "structureless"),
            field_texture = c("loam", "loam", "silt loam", "sandy loam"), clay_pct = c(20, 20, 18, 10),
            ph_h2o = c(5.6, 5.8, 6.2, 6.4), redoximorphic_features_pct = 0, artefacts_pct = 0)
  r <- classify_cscj2011(.jp(h, list(land_use = "upland field", drainage_class = "moderately well drained")),
                         on_missing = "silent")
  expect_identical(r$levels$code[1], "D")
  expect_identical(r$levels$name_native[4], "\u8150\u690d\u8cea\u4e0b\u5c64\u4f4e\u5730\u30a2\u30ed\u30d5\u30a7\u30f3\u8cea\u9ed2\u30dc\u30af\u571f")
  expect_identical(r$phases$name_native[r$phases$code == "fill"], "\u8150\u690d\u8cea\u9ed2\u30dc\u30af\u571f\u76db\u571f\u9020\u6210\u76f8")
})

test_that("the fill phase: the plough layer alone, none of the five kinds, a dark red topsoil, peat", {
  skip_on_cran()
  # a fill in the plough layer alone takes no phase in the CSCJ (IX, step A)
  b <- make_cscj2011_example("brown_forest"); b$horizons$layer_origin[1] <- "soil dressing"
  expect_identical(.ph(b)$name_native, "\u7d30\u7c92\u8cea\u76db\u571f\u9020\u6210\u76f8")
  b$horizons$designation[1] <- "Ap"
  expect_null(.ph(b))
  expect_identical(classify_scsj2017(b, on_missing = "silent")$phases$code, "fill")
  # none of the five kinds: a fill of 25 cm or more is keyed on its own and
  # named by its great group, a thinner one by its texture alone
  t <- list(designation = c("A", "B1", "2Ab", "2Bw", "2C"), top_cm = c(0, 10, 30, 50, 80), bottom_cm = c(10, 30, 50, 80, 110),
            layer_origin = c("fill (weathered granite)", "fill (weathered granite)", "weathered granite", "weathered granite", "weathered granite"),
            oc_pct = c(2.5, 1, 3, 1, 0.4), munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 3, 4, 5),
            munsell_chroma_moist = c(3, 4, 2, 4, 4), clay_pct = c(20, 22, 22, 25, 18), texture_class_jp = c("CL", "CL", "L", "CL", "SL"),
            phosphate_absorption_coef = 600, ph_h2o = 5.2, bs_pct = 40, redoximorphic_features_pct = 0, artefacts_pct = 0,
            gravel_pct = 0, structure_grade = "moderate")
  expect_identical(.ph(.jp(t))$name_native, "\u7d30\u7c92\u8cea\u8910\u8272\u68ee\u6797\u571f\u76db\u571f\u9020\u6210\u76f8")
  # a dark red colour in an A horizon counts under 2 % carbon only
  d <- t; d$layer_origin[2] <- "weathered granite"; d$top_cm <- c(0, 20, 30, 50, 80); d$bottom_cm <- c(20, 30, 50, 80, 110)
  d$munsell_hue_moist <- c("5YR", "10YR", "10YR", "10YR", "10YR"); d$oc_pct[1] <- 4
  expect_identical(.ph(.jp(d))$name_native, "\u7d30\u7c92\u8cea\u76db\u571f\u9020\u6210\u76f8")
  d$oc_pct[1] <- 1.5
  expect_identical(.ph(.jp(d))$name_native, "\u7d30\u7c92\u8cea\u6697\u8d64\u8272\u571f\u76db\u571f\u9020\u6210\u76f8")
  # a peat fill takes no texture word
  l <- make_cscj2011_example("lowland"); l$horizons$layer_origin[1] <- "soil dressing (peat)"; l$horizons$oc_pct[1] <- 30
  l$horizons$fiber_content_rubbed_pct <- c(40, NA, NA, NA)
  expect_identical(.ph(l)$name_native, "\u6709\u6a5f\u8cea\u571f\u76db\u571f\u9020\u6210\u76f8")
})
