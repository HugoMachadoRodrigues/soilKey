# v0.9.323: the 120 subgroups of the Soil Classification System of Japan
# (SCSJ 2017, chapter III). Made-up profiles; no Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.sg <- function(p) {
  r <- classify_scsj2017(p, on_missing = "silent")
  list(code = r$levels$code[nrow(r$levels)], name = r$name, native = r$levels$name_native[nrow(r$levels)],
       prov = r$provisional_because, depth = nrow(r$levels))
}
.kb <- function(...) {
  h <- list(designation = c("A1", "A2", "Bw", "C"), top_cm = c(0, 20, 45, 80), bottom_cm = c(20, 45, 80, 120),
            oc_pct = c(9, 6, 2, 0.8), phosphate_absorption_coef = c(2200, 2100, 1900, 1600),
            munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 4, 5), munsell_chroma_moist = c(1, 2, 4, 6),
            structure_grade = c("moderate", "moderate", "weak", "structureless"), clay_pct = c(20, 22, 18, 10),
            field_texture = c("loam", "loam", "silt loam", "sandy loam"), ph_h2o = c(5.4, 5.6, 5.8, 6.0),
            layer_origin = "volcanic ash", redoximorphic_features_pct = 0, si_ox_pct = 1.5, al_ox_pct = 3,
            al_pyro_pct = 0.6, exchange_acidity_y1 = 1)
  utils::modifyList(h, list(...))
}

test_that("every example keys to a subgroup, decided, named in English and Japanese", {
  skip_on_cran()
  want <- c(man_made = "A1MI", organic = "B1LM", andosol = "C6HU", podzol = "D1HA", fluvic = "E4HA",
            red_yellow = "F1RE", stagnic = "G2HA", eutrosol = "H1HA", brown_forest = "I1HA", regosol = "J2HA")
  for (g in names(want)) {
    r <- .sg(make_scsj2017_example(g))
    expect_identical(r$code, want[[g]], info = g)
    expect_length(r$prov, 0)
    expect_identical(r$depth, 3L)
  }
  r <- .sg(make_scsj2017_example("andosol"))
  expect_identical(r$name, "Humic Allophanic Andosols")
  expect_identical(r$native, "\u8150\u690d\u8cea\u30a2\u30ed\u30d5\u30a7\u30f3\u8cea\u9ed2\u30dc\u30af\u571f")
})

test_that("the humic surface horizon: thickness, colour, the 10 cm allowance", {
  skip_on_cran()
  # 45 cm of humic A: Humic, not Cumulic
  expect_identical(.sg(.jp(.kb()))$code, "C6HU")
  # 60 cm: Cumulic
  cu <- .kb(top_cm = c(0, 30, 60, 90), bottom_cm = c(30, 60, 90, 120))
  expect_identical(.sg(.jp(cu))$code, "C6CU")
  # a lighter 8 cm layer inside it still counts (under 10 cm)
  gap <- .kb(designation = c("A1", "AB", "A2", "Bw", "C"), top_cm = c(0, 25, 33, 60, 90), bottom_cm = c(25, 33, 60, 90, 120),
             oc_pct = c(9, 2, 6, 2, 0.8), phosphate_absorption_coef = c(2200, 1800, 2100, 1900, 1600),
             munsell_value_moist = c(2, 4, 2, 4, 5), munsell_chroma_moist = c(1, 3, 2, 4, 6),
             structure_grade = "moderate", clay_pct = 20, field_texture = "loam", ph_h2o = 5.6)
  expect_identical(.sg(.jp(gap))$code, "C6CU")
  # carbon enough but too light: Brown-humic
  bh <- .kb(munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(3, 3, 4, 6))
  expect_identical(.sg(.jp(bh))$code, "C6BH")
  # carbon under 3 %: Haplic
  ha <- .kb(oc_pct = c(2.5, 2.0, 1.5, 0.8), munsell_value_moist = c(3, 3, 4, 5))
  expect_identical(.sg(.jp(ha))$code, "C6HA")
})

test_that("the buried humic layer and the buried upland soil", {
  skip_on_cran()
  th <- .kb(designation = c("A", "C1", "2Ab", "2Bw"), top_cm = c(0, 15, 30, 50), bottom_cm = c(15, 30, 50, 100),
            oc_pct = c(2.5, 1.0, 5, 1.5), munsell_value_moist = c(3, 5, 2, 4), munsell_chroma_moist = c(3, 4, 2, 4),
            phosphate_absorption_coef = c(2000, 1800, 2100, 1900))
  expect_identical(.sg(.jp(th))$code, "C6TH")
  # a buried non-kuroboku upland soil within 50 cm under kuroboku layers
  tu <- .kb(designation = c("A1", "A2", "2Bw", "2C"), top_cm = c(0, 15, 35, 70), bottom_cm = c(15, 35, 70, 120),
            phosphate_absorption_coef = c(2200, 2000, 700, 500), layer_origin = c("volcanic ash", "volcanic ash",
            "weathered granite", "weathered granite"), oc_pct = c(9, 6, 0.8, 0.3))
  expect_identical(.sg(.jp(tu))$code, "C6TU")
})

test_that("Gley Fluvic soils: Strong or Mottled by the gley layers' mottles; Thionic", {
  skip_on_cran()
  h <- list(designation = c("A", "Go", "Gr"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 110),
            oc_pct = c(2.5, 0.6, 0.4), phosphate_absorption_coef = c(600, 500, 400),
            munsell_hue_moist = c("10YR", "5GY", "5BG"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 1, 1),
            clay_pct = c(25, 30, 30), layer_origin = "marine clay", redoximorphic_features_pct = c(0, 10, 0))
  p <- .jp(h, list(drainage_class = "poorly drained", land_use = "upland crops"))
  r <- .sg(p)
  expect_identical(r$code, "E2MO")         # 10 % mottles in the Go from 20 to 50 cm (deeper than 25)
  h$redoximorphic_features_pct <- c(0, 2, 0)
  expect_identical(.sg(.jp(h, list(drainage_class = "poorly drained", land_use = "upland crops")))$code, "E2ST")
  h$redoximorphic_features_pct <- NA_real_
  expect_identical(.sg(.jp(h, list(drainage_class = "poorly drained", land_use = "upland crops")))$code, "E2MO")
  # jarosite with pH under 4 from 30 cm, 20 cm thick: Thionic
  h$jarosite_present <- c(FALSE, TRUE, FALSE); h$ph_h2o <- c(5, 3.6, 6.5)
  expect_identical(.sg(.jp(h, list(drainage_class = "poorly drained", land_use = "upland crops")))$code, "E2TN")
})

test_that("paddy subgroups: bleached surface, reverse gley, anthraquic", {
  skip_on_cran()
  pd <- list(designation = c("Ap", "Bir", "C1", "C2"), top_cm = c(0, 15, 25, 70), bottom_cm = c(15, 25, 70, 120),
             oc_pct = c(2.5, 0.8, 0.5, 0.3), phosphate_absorption_coef = c(700, 500, 450, 400),
             munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 4, 5, 5),
             munsell_chroma_moist = c(2, 4, 2, 2), clay_pct = 15, field_texture = "silt loam",
             layer_origin = "alluvium", redoximorphic_features_pct = c(5, 20, 5, 0), fe_dcb_pct = c(0.8, 2.4, 1.0, 0.6))
  expect_identical(.sg(.jp(pd, list(land_use = "paddy")))$code, "E1HA")
  # a bleached plough layer: Albic
  pd2 <- pd; pd2$redoximorphic_features_pct[1] <- 0; pd2$fe_dcb_pct <- c(0.3, 1.2, 0.8, 0.6)
  expect_identical(.sg(.jp(pd2, list(land_use = "paddy")))$code, "E1AL")
  # a gley layer from under the plough layer, stopping above 75 cm: Epi-gleyed
  pd3 <- pd; pd3$designation <- c("Ap", "Bir", "G", "C2"); pd3$top_cm <- c(0, 15, 25, 60); pd3$bottom_cm <- c(15, 25, 60, 120)
  pd3$designation <- c("Ap", "G", "Bir", "C2")
  pd3$fe_dcb_pct <- c(0.8, 0.5, 2.4, 0.6)
  r <- soilKey:::paddy_reverse_gley_scsj(.jp(pd3, list(land_use = "paddy")))
  expect_true(r$passed)
  expect_false(soilKey:::paddy_reverse_gley_scsj(.jp(pd3, list(land_use = "upland crops")))$passed)
})

test_that("Regosols subgroups: calcaric, granitic, para-lithic, marlitic", {
  skip_on_cran()
  tr <- list(designation = c("A", "C1", "C2"), top_cm = c(0, 10, 50), bottom_cm = c(10, 50, 100),
             oc_pct = c(2, 0.5, 0.3), phosphate_absorption_coef = c(600, 500, 400),
             field_texture = c("loam", "clay loam", "clay loam"), clay_pct = c(20, 30, 30),
             layer_origin = "weathered mudstone", redoximorphic_features_pct = 0, caco3_pct = 0)
  expect_identical(.sg(.jp(tr))$code, "J4HA")
  t2 <- tr; t2$caco3_pct <- c(0, 5, 8)
  expect_identical(.sg(.jp(t2))$code, "J4CA")
  t3 <- tr; t3$layer_origin <- "weathered granite"
  expect_identical(.sg(.jp(t3))$code, "J4GR")
  t4 <- tr; t4$layer_origin <- "marl"; t4$clay_pct <- c(32, 40, 45)
  expect_identical(.sg(.jp(t4))$code, "J4MA")
  t5 <- list(designation = c("A", "C", "Cr"), top_cm = c(0, 10, 60), bottom_cm = c(10, 60, 90),
             oc_pct = c(2, 0.5, NA), phosphate_absorption_coef = c(600, 500, NA), clay_pct = c(20, 25, NA),
             field_texture = c("loam", "clay loam", NA), layer_origin = c(NA, NA, "soft mudstone"),
             redoximorphic_features_pct = c(0, 0, NA), caco3_pct = c(0, 0, NA))
  expect_identical(.sg(.jp(t5))$code, "J4PL")
  # Lithosols: calcaric in the layer on the rock
  li <- list(designation = c("A", "C", "R"), top_cm = c(0, 8, 20), bottom_cm = c(8, 20, 40),
             oc_pct = c(3, 1, NA), phosphate_absorption_coef = c(600, 500, NA), clay_pct = c(20, 18, NA),
             field_texture = c("loam", "loam", NA), layer_origin = c(NA, NA, "limestone"),
             redoximorphic_features_pct = c(0, 0, NA), caco3_pct = c(1, 6, NA))
  expect_identical(.sg(.jp(li))$code, "J3CA")
})

test_that("Man-made and Peat soils subgroups", {
  skip_on_cran()
  p <- make_scsj2017_example("man_made")
  p$horizons$artefacts_organic <- c(TRUE, TRUE, FALSE, NA, NA)
  expect_identical(.sg(p)$code, "A1OR")
  o <- make_scsj2017_example("organic")
  o$horizons$fiber_content_rubbed_pct <- c(10, 8, 5, NA)
  expect_identical(.sg(o)$code, "B1SA")
  o2 <- make_scsj2017_example("organic"); o2$horizons$peat_moor_type <- c("high-moor", "high-moor", "low-moor", NA)
  expect_identical(.sg(o2)$code, "B1HM")
})

test_that("Brown Forest subgroups: andic, thapto-red-yellow, eutric", {
  skip_on_cran()
  b <- make_scsj2017_example("brown_forest")
  b$horizons$phosphate_absorption_coef <- c(1300, 1200, 1100, 500)
  expect_identical(.sg(b)$code, "I1AD")
  b <- make_scsj2017_example("brown_forest")
  b$horizons$munsell_hue_moist <- c("10YR", "10YR", "7.5YR", "5YR"); b$horizons$munsell_chroma_moist <- c(2, 4, 6, 6)
  b$horizons$munsell_value_moist <- c(2, 4, 5, 5)
  expect_identical(.sg(b)$code, "I1TR")
  b <- make_scsj2017_example("brown_forest")
  b$horizons$ph_h2o <- c(4.8, 5.0, 6.8, 7.0)
  expect_identical(.sg(b)$code, "I1EU")
})

test_that("review of the subgroups: windows, the humic walk, a split buried humic layer, Epi-gray", {
  skip_on_cran()
  # a layer starting at 52 under an undecided Btg from 45: may start above 50
  x <- soilKey:::.scsj_profile(.jp(list(designation = c("A", "Bt1", "Btg", "Bt2"), top_cm = c(0, 15, 45, 52),
                                        bottom_cm = c(15, 45, 52, 90), oc_pct = c(3, 0.5, 0.4, 0.3))))
  rows <- c(FALSE, FALSE, NA, TRUE)
  expect_true(is.na(soilKey:::.scsj_top_in(x, rows, 50, 75)))
  expect_true(soilKey:::.scsj_top_in(x, c(FALSE, FALSE, FALSE, TRUE), 50, 75))
  # an undecided layer from 30 continuing into a TRUE one: nothing starts in 50-75
  expect_false(soilKey:::.scsj_top_in(x, c(FALSE, NA, TRUE, TRUE), 50, 75))
  # a light cap at the top ends the humic surface horizon
  cap <- .kb(designation = c("A0", "A1", "Bw", "C"), top_cm = c(0, 8, 40, 80), bottom_cm = c(8, 40, 80, 120),
             munsell_value_moist = c(4, 2, 4, 5), munsell_chroma_moist = c(3, 1, 4, 6), oc_pct = c(2, 9, 2, 0.8))
  cap$designation <- c("A1", "A2", "Bw", "C")
  expect_false(soilKey:::humic_surface_scsj(.jp(cap))$passed)
  # 2Ab1 + 2Ab2 make one buried humic layer
  th <- .kb(designation = c("A", "C1", "2Ab1", "2Ab2", "2Bw"), top_cm = c(0, 15, 25, 31, 45), bottom_cm = c(15, 25, 31, 45, 100),
            oc_pct = c(2.5, 1.0, 5, 6, 1.5), munsell_value_moist = c(3, 5, 2, 2, 4), munsell_chroma_moist = c(3, 4, 2, 2, 4),
            phosphate_absorption_coef = c(2000, 1800, 2100, 2100, 1900), clay_pct = 20, field_texture = "loam", ph_h2o = 5.6,
            structure_grade = "moderate")
  expect_true(soilKey:::buried_humic_scsj(.jp(th))$passed)
  # Epi-gray: groundwater wetness anywhere between 25 and 50 cm
  h <- list(designation = c("Ap", "Bg", "G"), top_cm = c(0, 12, 40), bottom_cm = c(12, 40, 100),
            oc_pct = c(2.5, 0.6, 0.4), phosphate_absorption_coef = c(600, 500, 400),
            munsell_hue_moist = c("10YR", "2.5Y", "5BG"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 2, 1),
            clay_pct = 30, layer_origin = "marine clay", redoximorphic_features_pct = c(0, 10, 0),
            redox_segregations_location = c(NA, "ped faces", NA))
  expect_identical(.sg(.jp(h, list(drainage_class = "poorly drained", land_use = "upland crops")))$code, "E2GY")
})

test_that("review of the subgroups: paddy reverse gley under a gleyed Ap, either wetness, an H without P data", {
  skip_on_cran()
  pd <- .jp(list(designation = c("Ap1", "Ap2", "G", "C"), top_cm = c(0, 12, 18, 50), bottom_cm = c(12, 18, 50, 120),
                 oc_pct = c(2.5, 1.5, 0.6, 0.3), phosphate_absorption_coef = 500, childs_test = c(NA, "positive", NA, NA),
                 redoximorphic_features_pct = c(5, 5, 0, 0), fe_dcb_pct = c(0.8, 0.7, 0.4, 0.3), layer_origin = "alluvium"),
            list(land_use = "paddy"))
  expect_true(soilKey:::paddy_reverse_gley_scsj(pd)$passed)
  # a Bg of unknown kind: either wetness counts for the Allophanic Andosols' Aquic
  aq <- .kb(designation = c("A1", "A2", "Bg", "C"), redoximorphic_features_pct = c(0, 0, 10, 0),
            oc_pct = c(2.5, 2.0, 1.5, 0.8), munsell_value_moist = c(3, 3, 4, 5))
  expect_identical(.sg(.jp(aq))$code, "C6AQ")
  # an H with carbon and no P data is peat
  o <- make_scsj2017_example("organic"); o$horizons$phosphate_absorption_coef <- NA_real_
  expect_true(soilKey:::organic_soils_scsj(o)$passed)
})

test_that("Hugo's subgroup decisions: Brown-humic colour, Endoaeric, the Aeric paddy greyed horizon", {
  skip_on_cran()
  # 3 % carbon, 10YR 4/2: neither Humic (too light) nor Brown-humic (chroma 2)
  bh <- .kb(munsell_value_moist = c(4, 4, 4, 5), munsell_chroma_moist = c(2, 2, 4, 6))
  expect_identical(.sg(.jp(bh))$code, "C6HA")
  bh$munsell_chroma_moist <- c(3, 3, 4, 6)
  expect_identical(.sg(.jp(bh))$code, "C6BH")
  # Endoaeric: a brown Ap does not count
  ea <- list(designation = c("Ap", "Bir", "Bg", "C"), top_cm = c(0, 15, 20, 70), bottom_cm = c(15, 20, 70, 120),
             oc_pct = c(2.5, 0.8, 0.5, 0.3), phosphate_absorption_coef = 500,
             munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 4, 5, 5),
             munsell_chroma_moist = c(3, 6, 2, 2), clay_pct = 20, layer_origin = "alluvium",
             redoximorphic_features_pct = c(5, 20, 10, 0), fe_dcb_pct = c(0.8, 2.4, 0.8, 0.5))
  expect_false(soilKey:::brown_layer_scsj(.jp(ea, list(land_use = "paddy")))$passed)
})

test_that("review of PR 131: a B ends the humic walk, gaps over a buried humic layer, peat and artefact shares", {
  skip_on_cran()
  g <- function(fn, p, ...) fn(p, ...)$passed
  # A 0-20 humic, B 20-25, buried A 25-45 humic: the B ends the surface horizon
  h <- .kb(designation = c("A", "B", "2Ab", "2C"), top_cm = c(0, 20, 25, 45), bottom_cm = c(20, 25, 45, 100),
           munsell_value_moist = c(2, 4, 2, 5), munsell_chroma_moist = c(2, 4, 2, 6), oc_pct = c(6, 1.5, 6, 0.8))
  expect_false(g(humic_surface_scsj, .jp(h)))
  # only the layers that meet the criteria add up: 25 + 20 = 45 cm, not Cumulic
  h <- .kb(designation = c("A1", "A2", "A3", "C"), top_cm = c(0, 25, 33, 53), bottom_cm = c(25, 33, 53, 100),
           munsell_value_moist = c(2, 4, 2, 5), munsell_chroma_moist = c(2, 4, 2, 6), oc_pct = c(6, 6, 6, 0.8))
  expect_true(g(humic_surface_scsj, .jp(h), thick = 25))
  expect_false(g(humic_surface_scsj, .jp(h), thick = 50))
  # a lighter, leaner C at 10-20, a gap at 20-40, a dark 2Ab at 40-55: undecided
  b <- .kb(designation = c("A", "C", "2Ab", "2C"), top_cm = c(0, 10, 40, 55), bottom_cm = c(10, 20, 55, 100),
           munsell_value_moist = c(3, 5, 2, 5), munsell_chroma_moist = c(3, 4, 2, 6), oc_pct = c(2, 1, 6, 0.8))
  expect_true(is.na(g(buried_humic_scsj, .jp(b))))
  b$top_cm <- c(0, 10, 20, 35); b$bottom_cm <- c(10, 20, 35, 100)
  expect_true(g(buried_humic_scsj, .jp(b)))
  # Organic Artifactual: an organic horizon without a bottom may hold the majority
  a <- list(designation = c("^C1", "^C2"), top_cm = c(0, 30), bottom_cm = c(30, NA), artefacts_pct = 50,
            artefacts_organic = c(FALSE, TRUE), oc_pct = 1, layer_origin = "fill")
  expect_true(is.na(g(organic_artefacts_scsj, .jp(a))))
  a$artefacts_organic <- c(TRUE, FALSE)
  expect_true(is.na(g(organic_artefacts_scsj, .jp(a))))
  a$bottom_cm <- c(30, 40)
  expect_true(g(organic_artefacts_scsj, .jp(a)))
  # Sapric: 30 cm of sapric peat over an open horizon: 30 > 20 unknown
  pt <- list(designation = c("H1", "H2"), top_cm = c(0, 30), bottom_cm = c(30, NA), oc_pct = 35,
             fiber_content_rubbed_pct = c(10, NA))
  s <- .jp(pt, site = list(drainage_class = "very poorly drained"))
  expect_true(g(sapric_peat_scsj, s))
  # High-moor: the uppermost 25 cm of the peat, counted on the peat alone
  m <- list(designation = c("H1", "C", "H2"), top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 80),
            oc_pct = c(40, 1, 40), peat_moor_type = c("high-moor", NA, "low-moor"), field_texture = c(NA, "silt loam", NA))
  mp <- .jp(m, site = list(drainage_class = "very poorly drained"))
  expect_false(g(moor_type_scsj, mp))
  m$designation <- c("H1", "H2"); m$top_cm <- c(0, 30); m$bottom_cm <- c(10, 80); m$oc_pct <- c(40, 40)
  m$peat_moor_type <- c("high-moor", "low-moor"); m$field_texture <- NULL
  expect_true(is.na(g(moor_type_scsj, .jp(m, site = list(drainage_class = "very poorly drained")))))
  m$top_cm <- c(0, 10); m$peat_moor_type <- c("high-moor", "high-moor")
  expect_true(g(moor_type_scsj, .jp(m, site = list(drainage_class = "very poorly drained"))))
})

test_that("review of PR 131: an undecided segment across the top of the window", {
  skip_on_cran()
  x <- .scsj_profile(.jp(.kb(top_cm = c(0, 45, 90, 100), bottom_cm = c(45, 90, 100, 120))))
  expect_true(is.na(.scsj_top_in(x, c(FALSE, NA, FALSE, FALSE), 50, 75)))
  expect_false(.scsj_top_in(x, c(FALSE, TRUE, FALSE, FALSE), 50, 75))
  # a description that stops at 40 cm, not on rock
  x2 <- .scsj_profile(.jp(.kb(designation = c("A1", "A2"), top_cm = c(0, 20), bottom_cm = c(20, 40),
                               oc_pct = c(9, 6), phosphate_absorption_coef = c(2200, 2100), munsell_value_moist = c(2, 2),
                               munsell_chroma_moist = c(1, 2), structure_grade = "moderate", clay_pct = 20,
                               field_texture = "loam", ph_h2o = 5.5)))
  expect_true(is.na(.scsj_top_in(x2, c(FALSE, FALSE), 50, 75)))
})
