# v0.9.321: the Soil Classification System of Japan (SCSJ 2017), the ten
# great groups of chapter II. Made-up profiles; no Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.gg <- function(p) classify_scsj2017(p, on_missing = "silent")

test_that("each example keys to its great group, decided", {
  skip_on_cran()
  want <- c(man_made = "A", organic = "B", andosol = "C", podzol = "D", fluvic = "E",
            red_yellow = "F", stagnic = "G", eutrosol = "H", brown_forest = "I", regosol = "J")
  for (g in names(want)) {
    r <- .gg(make_scsj2017_example(g))
    expect_identical(r$levels$code[1], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
    expect_identical(r$system_id, "scsj2017")
  }
  r <- .gg(make_scsj2017_example("andosol"))
  expect_identical(r$name, "Andosols")
  expect_identical(r$levels$name_native[1], "\u9ed2\u30dc\u30af\u571f\u5927\u7fa4")
  expect_true("scsj2017" %in% soilkey_systems()$id)
})

test_that("the Japanese notation is parsed", {
  skip_on_cran()
  d <- soilKey:::.scsj_designations(c("IIBw2", "2Ab", "Go", "Gr", "Apg", "Bir", "Cr", "H2", "Oa", "BC", "x?y"))
  expect_identical(d$lith[1:2], c(2L, 2L))
  expect_identical(d$role[c(1, 3, 8, 9, 10)], c("B", "G", "H", "O", "BC"))
  expect_true(d$buried[2])
  expect_true(all(d$gley_master[3:4]))
  expect_true(d$p[5] && d$g[5])
  expect_true(d$ir[6])
  expect_true(d$weathered_rock[7])
  expect_identical(d$decomp[9], "highly")
  expect_identical(d$role[11], "other")
  fao <- soilKey:::.scsj_designations("Cr", list(designation_system = "fao"))
  expect_true(fao$gley_master)
  expect_false(fao$weathered_rock)
})

test_that("the kuroboku feature: coefficient first, then substitutes, then the field test", {
  skip_on_cran()
  kb <- function(...) {
    p <- .jp(c(list(designation = "A", top_cm = 0, bottom_cm = 30, oc_pct = 5), list(...)))
    x <- soilKey:::.scsj_profile(p)
    x$kb
  }
  expect_true(kb(phosphate_absorption_coef = 1600, phosphate_retention_pct = 50))
  expect_false(kb(phosphate_absorption_coef = 1400, phosphate_retention_pct = 95))
  expect_true(kb(phosphate_retention_pct = 90))
  expect_true(kb(phosphate_retention_pct = 60, al_ox_pct = 2.0, fe_ox_pct = 0.4))
  expect_false(kb(phosphate_retention_pct = 60))
  expect_true(kb(active_al_test = "2+"))
  expect_false(kb(active_al_test = "+-"))
  expect_true(is.na(kb(active_al_test = "+")))
  expect_true(is.na(kb()))
  expect_false(kb(phosphate_absorption_coef = 2000, fiber_content_rubbed_pct = 30))
})

test_that("the soil surface skips a forest floor, but not peat or a very wet site", {
  skip_on_cran()
  h <- list(designation = c("O", "A", "Bw"), top_cm = c(0, 8, 30), bottom_cm = c(8, 30, 80),
            oc_pct = c(40, 6, 1))
  expect_identical(soilKey:::.scsj_profile(.jp(h))$surface, 8)
  expect_identical(soilKey:::.scsj_profile(.jp(h, list(drainage_class = "very poorly drained")))$surface, 0)
  h$designation[1] <- "H"
  expect_identical(soilKey:::.scsj_profile(.jp(h))$surface, 0)
})

test_that("peat under a thin fill keys Organic Soils", {
  skip_on_cran()
  p <- .jp(list(designation = c("A", "H1", "H2", "G"), top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 100),
                layer_origin = c("soil dressing fill", NA, NA, NA), oc_pct = c(4, 30, 30, 2),
                phosphate_absorption_coef = c(800, 600, 600, 500)),
           list(drainage_class = "poorly drained", land_use = "upland crops"))
  r <- soilKey:::organic_soils_scsj(p)
  expect_true(r$passed)
  expect_identical(r$evidence$fill_cm, 20)
  # 20 cm of peat only: not enough
  p2 <- .jp(list(designation = c("A", "H1", "C"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100),
                 layer_origin = c("fill", NA, "alluvium"), oc_pct = c(4, 30, 1),
                 phosphate_absorption_coef = c(800, 600, 500)))
  expect_false(soilKey:::organic_soils_scsj(p2)$passed)
})

test_that("red-yellow, Eutrosol and Brown Forest turn on colour and base status", {
  skip_on_cran()
  p <- make_scsj2017_example("red_yellow")
  # brown, not red or yellow: no longer Red-Yellow; acid: Brown Forest
  p$horizons$munsell_hue_moist <- c("10YR", "10YR", "10YR", "10YR")
  p$horizons$munsell_chroma_moist <- c(3, 4, 4, 4)
  expect_identical(.gg(p)$levels$code[1], "I")
  # base-rich subsurface: Eutrosols
  p$horizons$ph_h2o <- 6.5; p$horizons$bs_pct <- 80
  expect_identical(.gg(p)$levels$code[1], "H")
  # pH under 5.5 with base saturation not measured: Red-Yellow undecided
  q <- make_scsj2017_example("red_yellow")
  q$horizons$bs_pct <- NA_real_; q$horizons$ph_h2o <- 6.0
  r <- .gg(q)
  expect_true(is.na(r$trace$great_groups$F$passed))
  expect_identical(r$levels$code[1], "H")
  expect_true("great_groups:F" %in% r$provisional_because)
})

test_that("bedrock within 30 cm keeps a soil out of Brown Forest soils", {
  skip_on_cran()
  p <- .jp(list(designation = c("A", "Bw", "R"), top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 40),
                oc_pct = c(5, 1.5, NA), phosphate_absorption_coef = c(900, 800, NA), clay_pct = c(20, 22, NA),
                munsell_hue_moist = c("10YR", "10YR", NA), munsell_value_moist = c(3, 4, NA),
                munsell_chroma_moist = c(2, 4, NA), redoximorphic_features_pct = 0))
  r <- .gg(p)
  expect_false(r$trace$great_groups$I$passed)
  expect_identical(r$levels$code[1], "J")
})

test_that("Stagnic soils need the wet layers to reach 50 cm", {
  skip_on_cran()
  p <- make_scsj2017_example("stagnic")
  expect_true(soilKey:::stagnic_soils_scsj(p)$passed)
  p$horizons$redoximorphic_features_pct <- c(2, 20, 0, 0)
  p$horizons$designation <- c("Ap", "Bg", "B", "C")
  expect_false(soilKey:::stagnic_soils_scsj(p)$passed)
  # a g suffix alone is a wetness feature (kind not said)
  q <- make_scsj2017_example("stagnic")
  q$horizons$redoximorphic_features_pct <- NA_real_
  q$horizons$redox_segregations_location <- NA_character_
  expect_true(soilKey:::stagnic_soils_scsj(q)$passed)
})

test_that("paddy horizons: undecided without a land use, read on paddy land", {
  skip_on_cran()
  h <- list(designation = c("Ap", "E", "Bs", "C"), top_cm = c(0, 15, 25, 45), bottom_cm = c(15, 25, 45, 90),
            oc_pct = c(3, 0.8, 2, 0.4), phosphate_absorption_coef = c(500, 300, 1100, 600),
            munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 6),
            munsell_chroma_moist = c(2, 2, 4, 4), redoximorphic_features_pct = c(5, 0, 0, 0),
            fe_dcb_pct = c(0.8, 0.2, 1.5, 0.5), ph_h2o = c(5.5, 5.0, 5.2, 5.4))
  r <- soilKey:::podzols_scsj(.jp(h, list(land_use = NA)))
  expect_true(is.na(r$passed))
  expect_true(is.na(r$evidence$no_paddy_surface))
  expect_false(soilKey:::podzols_scsj(.jp(h, list(land_use = "paddy rice")))$passed)
  expect_true(soilKey:::podzols_scsj(.jp(h, list(land_use = "upland crops")))$passed)
  ps <- soilKey:::.scsj_paddy_surface_rows(soilKey:::.scsj_profile(.jp(h, list(land_use = "paddy"))))
  expect_true(ps$unbleached[1])
  expect_false(ps$bleached[1])
})

test_that("a fill of the same great group is not Man-made soils", {
  skip_on_cran()
  # 40 cm of kuroboku fill over a kuroboku soil: both Andosols
  p <- .jp(list(designation = c("A", "C", "2Ab", "2Bw", "2C"), top_cm = c(0, 15, 40, 60, 90),
                bottom_cm = c(15, 40, 60, 90, 120), layer_origin = c("fill", "fill", "volcanic ash", "volcanic ash", "volcanic ash"),
                oc_pct = c(6, 4, 8, 2, 0.8), phosphate_absorption_coef = c(2000, 1900, 2200, 2000, 1700),
                clay_pct = c(18, 16, 20, 20, 15), field_texture = c("loam", "loam", "loam", "silt loam", "loam"),
                munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR", "10YR"), munsell_value_moist = c(2, 3, 2, 4, 5),
                munsell_chroma_moist = c(2, 2, 1, 4, 6), structure_grade = c("weak", "weak", "moderate", "weak", "structureless"),
                ph_h2o = c(5.8, 5.9, 5.5, 5.7, 6.0), redoximorphic_features_pct = 0, artefacts_pct = 0))
  r <- soilKey:::man_made_soils_scsj(p)
  expect_identical(r$evidence$fill_cm, 40)
  expect_false(r$evidence$fill_other_great_group)
  expect_false(r$passed)
  expect_identical(.gg(p)$levels$code[1], "C")
  # the same kuroboku fill over a Brown Forest soil: heterogeneous
  p$horizons$phosphate_absorption_coef[3:5] <- c(900, 800, 600)
  p$horizons$layer_origin[3:5] <- "weathered granite"
  p$horizons$oc_pct[3:5] <- c(4, 1.2, 0.4)
  expect_true(soilKey:::man_made_soils_scsj(p)$passed)
  expect_identical(.gg(p)$levels$code[1], "A")
})

test_that("missing kuroboku data makes the key provisional, not wrong", {
  skip_on_cran()
  p <- make_scsj2017_example("brown_forest")
  p$horizons$phosphate_absorption_coef <- NA_real_
  r <- .gg(p)
  expect_true(is.na(r$trace$great_groups$C$passed))
  expect_identical(r$levels$code[1], "I")
  expect_true("great_groups:C" %in% r$provisional_because)
})

test_that("origins: volcanic rock is not ejecta, rock and terraces are not alluvium, fill is neither", {
  skip_on_cran()
  o <- function(lo, ro = NA, pm = NULL) {
    h <- data.frame(layer_origin = lo, rock_origin = ro, stringsAsFactors = FALSE)
    soilKey:::.scsj_origins(h, list(parent_material = pm), soilKey:::.scsj_designations(rep(NA, nrow(h))))
  }
  expect_true(o("volcanic ash")$ejecta)
  expect_false(o("weathered volcanic rock (andesite)")$ejecta)
  expect_false(o(NA, "\u706b\u5c71\u5ca9")$ejecta)
  expect_true(o(NA, "pumice")$ejecta)
  expect_true(o("alluvium")$alluvial)
  expect_false(o("Miocene marine mudstone")$alluvial)
  expect_false(o("\u6d77\u6210\u6bb5\u4e18\u5806\u7a4d\u7269")$alluvial)
  expect_false(o(NA, "marine")$alluvial)
  r <- o("dredged river sediment (fill)")
  expect_true(r$fill)
  expect_false(r$alluvial)
  expect_false(o("loess transported by wind")$fill)
  expect_true(o(NA, NA, "alluvium")$alluvial)
})

test_that("Podzols need the bleached horizon above the accumulation horizon (Hugo, 2026-10-31)", {
  skip_on_cran()
  h <- list(designation = c("A", "B1", "B2", "C"), top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 100),
            oc_pct = c(5, 1.5, 0.6, 0.3), phosphate_absorption_coef = c(900, 800, 600, 500), clay_pct = c(20, 22, 21, 18),
            munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(3, 4, 6, 6),
            munsell_chroma_moist = c(2, 3, 4, 4), ph_h2o = c(5.0, 5.1, 5.2, 5.4), redoximorphic_features_pct = 0)
  p <- .jp(h)
  expect_false(soilKey:::podzols_scsj(p)$passed)
  expect_identical(.gg(p)$levels$code[1], "I")
  # the same accumulation colour under a pale E: a Podzol
  h$designation <- c("A", "E", "Bs", "C"); h$munsell_hue_moist <- c("10YR", "10YR", "7.5YR", "10YR")
  h$munsell_value_moist <- c(3, 6, 4, 6); h$munsell_chroma_moist <- c(2, 2, 4, 4)
  expect_true(soilKey:::podzols_scsj(.jp(h))$passed)
  # an E alone is not both horizons
  h2 <- list(designation = c("A", "E", "C"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 90), oc_pct = c(5, 0.8, 0.3),
             phosphate_absorption_coef = c(900, 600, 500), munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
             munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 2, 4), clay_pct = c(15, 14, 12),
             redoximorphic_features_pct = 0)
  expect_false(soilKey:::podzols_scsj(.jp(h2))$passed)
  # a bleached paddy surface does not keep a Podzol out; the unbleached one does
  h3 <- list(designation = c("Ap", "E", "Bs", "C"), top_cm = c(0, 15, 25, 45), bottom_cm = c(15, 25, 45, 90),
             oc_pct = c(3, 0.8, 2, 0.4), phosphate_absorption_coef = c(500, 300, 1100, 600),
             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 6),
             munsell_chroma_moist = c(2, 2, 4, 4), redoximorphic_features_pct = c(0, 0, 0, 0),
             fe_dcb_pct = c(0.2, 0.2, 1.5, 0.5), ph_h2o = c(5.5, 5.0, 5.2, 5.4))
  expect_true(soilKey:::podzols_scsj(.jp(h3, list(land_use = "paddy")))$passed)
})

test_that("the forest-floor notation and an undesignated organic layer (Hugo, 2026-10-31)", {
  skip_on_cran()
  h <- list(designation = c("L", "F", "H", "A", "Bw"), top_cm = c(0, 2, 5, 10, 30), bottom_cm = c(2, 5, 10, 30, 80),
            oc_pct = c(NA, NA, NA, 6, 1.5))
  x <- soilKey:::.scsj_profile(.jp(h))
  expect_identical(x$role[1:3], c("O", "O", "O"))
  expect_identical(x$surface, 10)
  expect_false(x$peat[3])
  # without L or F, an H is peat
  expect_true(soilKey:::.scsj_profile(.jp(list(designation = c("H", "A"), top_cm = c(0, 30), bottom_cm = c(30, 60),
                                                 oc_pct = c(NA, 4))))$peat[1])
  # an undesignated layer of 30 % carbon: forest floor or peat by the drainage
  u <- list(designation = NA_character_, top_cm = c(0, 10), bottom_cm = c(10, 60), oc_pct = c(30, 2),
            phosphate_absorption_coef = c(500, 600))
  expect_identical(soilKey:::.scsj_profile(.jp(u))$surface, 10)
  xp <- soilKey:::.scsj_profile(.jp(u, list(drainage_class = "poorly drained")))
  expect_identical(xp$surface, 0)
  expect_true(xp$peat[1])
  xn <- soilKey:::.scsj_profile(.jp(u, list(drainage_class = NA)))
  expect_true(is.na(xn$surface))
  expect_true(is.na(soilKey:::brown_forest_soils_scsj(.jp(u, list(drainage_class = NA)))$passed))
  # measured carbon decides over the H of the notation
  expect_false(soilKey:::.scsj_profile(.jp(list(designation = c("H1", "H2"), top_cm = c(0, 20), bottom_cm = c(20, 60),
                                                  oc_pct = c(5, 4), phosphate_absorption_coef = 500)))$peat[1])
})

test_that("the subsurface section: 5 cm or more inside it, a gravelly topsoil, coverage (Hugo, 2026-10-31)", {
  skip_on_cran()
  p <- make_scsj2017_example("eutrosol")
  # an acid A reaching 22 cm does not count (2 cm inside the section)
  p$horizons$bottom_cm[1] <- 22; p$horizons$top_cm[2] <- 22; p$horizons$ph_h2o[1] <- 4.8; p$horizons$bs_pct[1] <- 20
  expect_true(soilKey:::eutrosols_scsj(p)$passed)
  # a gravelly topsoil does not empty the section
  q <- make_scsj2017_example("eutrosol"); q$horizons$gravel_pct <- c(30, 5, 5, 5)
  expect_true(soilKey:::eutrosols_scsj(q)$passed)
  # a gravel layer from 35 cm cuts it there
  q$horizons$gravel_pct <- c(0, 5, 30, 30); q$horizons$ph_h2o[3] <- 4.5; q$horizons$bs_pct[3] <- 20
  expect_identical(soilKey:::.scsj_control(soilKey:::.scsj_profile(q))$to, 45)
  expect_true(soilKey:::eutrosols_scsj(q)$passed)
  # a description that stops at 35 cm: undecided
  r <- .jp(list(designation = c("A", "Bw"), top_cm = c(0, 15), bottom_cm = c(15, 35), oc_pct = c(3, 1),
                phosphate_absorption_coef = c(800, 700), clay_pct = c(30, 35), munsell_hue_moist = "10YR",
                munsell_value_moist = c(3, 4), munsell_chroma_moist = c(2, 4), ph_h2o = c(6.6, 6.9), bs_pct = c(80, 90),
                redoximorphic_features_pct = 0))
  expect_true(is.na(soilKey:::eutrosols_scsj(r)$passed))
})

test_that("an open deepest horizon, a lithologic discontinuity, an unreadable row", {
  skip_on_cran()
  h <- list(designation = c("A", "Bw"), top_cm = c(0, 35), bottom_cm = c(35, NA), oc_pct = c(4, 1),
            phosphate_absorption_coef = c(800, 700), clay_pct = c(20, 24), munsell_hue_moist = "10YR",
            munsell_value_moist = c(3, 4), munsell_chroma_moist = c(2, 4), ph_h2o = 5, bs_pct = 20,
            redoximorphic_features_pct = 0)
  r <- .gg(.jp(h))
  expect_true(is.na(r$trace$great_groups$I$passed))
  # A over 2Bt: not an argic horizon (criterion 1)
  d <- .jp(list(designation = c("A", "2Bt", "2C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
                oc_pct = c(3, 0.5, 0.2), clay_pct = c(15, 40, 30), clay_films_amount = c(NA, "common", NA),
                phosphate_absorption_coef = c(700, 600, 500), munsell_hue_moist = c("10YR", "2.5YR", "5YR"),
                munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 6, 6), ph_h2o = 5, bs_pct = 20,
                redoximorphic_features_pct = 0))
  expect_false(soilKey:::.scsj_argic_rows(soilKey:::.scsj_profile(d))[2])
  expect_false(soilKey:::red_yellow_soils_scsj(d)$passed)
  # a row that cannot be read may be bedrock
  b <- .jp(list(designation = c("A", "Bw", "R?k"), top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 40),
                oc_pct = c(4, 1, NA), clay_pct = c(20, 22, NA)))
  expect_true(is.na(soilKey:::.scsj_no_rock_within(soilKey:::.scsj_profile(b), 30)))
})

test_that("a fill of sand over a Regosol waits for the groups", {
  skip_on_cran()
  p <- .jp(list(designation = c("C1", "C2", "2C"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100),
                layer_origin = c("fill", "fill", "weathered granite"), oc_pct = c(0.3, 0.2, 0.1),
                phosphate_absorption_coef = c(200, 150, 100), clay_pct = c(3, 3, 5),
                field_texture = c("sand", "sand", "loamy sand"), redoximorphic_features_pct = 0, artefacts_pct = 0))
  r <- soilKey:::man_made_soils_scsj(p)
  expect_true(is.na(r$evidence$fill_other_great_group))
  expect_true(is.na(r$passed))
})

test_that("review of PR 129: aliases, canonical origins, measured carbon, open bottoms", {
  skip_on_cran()
  expect_identical(soilKey:::.scsj_designations("Bw", list(designation_system = "cssc3"))$role, "other")
  expect_identical(soilKey:::.scsj_designations("Bw", list(designation_system = "New Zealand"))$role, "other")
  expect_true(soilKey:::.scsj_designations("Cr", list(designation_system = "wrb2022"))$gley_master)
  o <- soilKey:::.scsj_origins(data.frame(layer_origin = c("tephric", "fluvic")), list(),
                               soilKey:::.scsj_designations(c(NA, NA)))
  expect_identical(o$ejecta, c(TRUE, FALSE))
  expect_identical(o$alluvial, c(FALSE, TRUE))
  # an O horizon measured under 20 % carbon is not a forest floor
  x <- soilKey:::.scsj_profile(.jp(list(designation = c("O", "A", "Bw"), top_cm = c(0, 10, 30),
                                        bottom_cm = c(10, 30, 80), oc_pct = c(5, 3, 1))))
  expect_identical(x$surface, 0)
  # an open C from 25 cm may end over rock above 30 cm
  y <- soilKey:::.scsj_profile(.jp(list(designation = c("A", "Bw", "C"), top_cm = c(0, 10, 25),
                                        bottom_cm = c(10, 25, NA), oc_pct = c(4, 1, 0.3))))
  expect_true(is.na(soilKey:::.scsj_no_rock_within(y, 30)))
  # base-rich A, Bw and an open C: the section may not be covered
  e <- .jp(list(designation = c("A", "Bw", "C"), top_cm = c(0, 15, 35), bottom_cm = c(15, 35, NA),
                oc_pct = c(3, 1, 0.3), phosphate_absorption_coef = c(800, 700, 600), clay_pct = c(30, 35, 30),
                munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 4, 3),
                ph_h2o = 6.8, bs_pct = 90, redoximorphic_features_pct = 0))
  expect_true(is.na(soilKey:::.scsj_control(soilKey:::.scsj_profile(e))$covered))
  expect_true(is.na(soilKey:::eutrosols_scsj(e)$passed))
  # gravel from 23 cm: no horizon has 5 cm in the section
  q <- make_scsj2017_example("eutrosol"); q$horizons$gravel_pct <- c(0, 0, 30, 30)
  q$horizons$top_cm <- c(0, 15, 23, 80); q$horizons$bottom_cm <- c(15, 23, 80, 110)
  expect_length(soilKey:::.scsj_control(soilKey:::.scsj_profile(q))$rows, 0)
})

test_that("review of PR 129: paddy iron thickness, pairs inside one material, a geomembrane's depth", {
  skip_on_cran()
  pd <- list(designation = c("Ap", "Bir", "C"), top_cm = c(0, 15, 16), bottom_cm = c(15, 16, 80),
             oc_pct = c(2.5, 0.8, 0.5), phosphate_absorption_coef = c(700, 500, 450), redoximorphic_features_pct = c(5, 20, 5),
             fe_dcb_pct = c(0.8, NA, 0.6))
  x <- soilKey:::.scsj_profile(.jp(pd, list(land_use = "paddy")))
  expect_false(soilKey:::.scsj_paddy_fe_rows(x)[2])
  pz <- list(designation = c("A", "E", "2E", "2Bs", "2C"), top_cm = c(0, 10, 20, 30, 40), bottom_cm = c(10, 20, 30, 40, 90),
             oc_pct = c(5, 0.8, 0.8, 2, 0.4), phosphate_absorption_coef = c(600, 300, 300, 1100, 500),
             munsell_hue_moist = c("10YR", "10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 6, 4, 6),
             munsell_chroma_moist = c(2, 2, 2, 4, 4), clay_pct = 8, redoximorphic_features_pct = 0, ph_h2o = 4.5)
  expect_true(soilKey:::podzols_scsj(.jp(pz))$passed)
  mm <- .jp(list(designation = c("A", "C", "2Bw"), top_cm = c(0, 20, 100), bottom_cm = c(20, 100, 130),
                 oc_pct = c(2, 0.5, 1), geomembrane_present = c(FALSE, TRUE, FALSE), layer_origin = c("fill", "fill", NA),
                 artefacts_pct = 0, phosphate_absorption_coef = 500))
  xm <- soilKey:::.scsj_profile(mm)
  expect_true(is.na(soilKey:::.scsj_impermeable_within(xm, 30)))
  mm$site$geomembrane <- TRUE; mm$site$geomembrane_depth_cm <- 40
  expect_false(soilKey:::.scsj_impermeable_within(soilKey:::.scsj_profile(mm), 30))
})

test_that("Red-Yellow soils: the 25 cm are those of one horizon (Hugo, 2026-11-01)", {
  skip_on_cran()
  p <- make_scsj2017_example("red_yellow")
  expect_true(red_yellow_soils_scsj(p)$passed)
  # a 15 cm red argic horizon within 50 cm and another one far below: not 25 cm of one horizon
  h <- list(designation = c("A", "Bt1", "BC", "Bt2", "C"), top_cm = c(0, 30, 45, 150, 170),
            bottom_cm = c(30, 45, 150, 170, 200), oc_pct = c(3, 0.6, 0.3, 0.3, 0.2),
            munsell_hue_moist = c("10YR", "2.5YR", "10YR", "2.5YR", "10YR"), munsell_value_moist = c(3, 4, 5, 4, 5),
            munsell_chroma_moist = c(3, 6, 4, 6, 4), clay_pct = c(20, 40, 30, 40, 25),
            clay_films_amount = c(NA, "common", "few", "common", NA), ph_h2o = 4.8, bs_pct = 20,
            structure_grade = "moderate", layer_origin = "weathered sedimentary rock", redoximorphic_features_pct = 0)
  n <- max(lengths(h)); h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  q <- PedonRecord$new(site = list(id = "t", country = "JP", designation_system = "scsj", land_use = "forest",
                                   drainage_class = "well drained"),
                       horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  expect_false(isTRUE(red_yellow_soils_scsj(q)$passed))
})
