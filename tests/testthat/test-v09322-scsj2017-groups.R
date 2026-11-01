# v0.9.322: the 26 groups of the Soil Classification System of Japan (SCSJ
# 2017, chapter III). Made-up profiles; no Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.grp <- function(p) {
  r <- classify_scsj2017(p, on_missing = "silent")
  c(code = r$levels$code[2], prov = paste(r$provisional_because, collapse = ","))
}

# a kuroboku profile to modify
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
# an alluvial profile to modify
.al <- function(...) {
  h <- list(designation = c("A", "B", "C1", "C2"), top_cm = c(0, 15, 50, 80), bottom_cm = c(15, 50, 80, 120),
            oc_pct = c(2.5, 0.8, 0.5, 0.3), phosphate_absorption_coef = c(700, 500, 450, 400),
            munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(3, 4, 5, 5),
            munsell_chroma_moist = c(2, 4, 3, 2), clay_pct = c(18, 20, 12, 8),
            field_texture = c("silt loam", "silt loam", "sandy loam", "loamy sand"),
            structure_grade = c("moderate", "weak", "structureless", "structureless"),
            ph_h2o = 6.2, layer_origin = "alluvium", redoximorphic_features_pct = 0)
  utils::modifyList(h, list(...))
}

test_that("every example keys to a group, decided", {
  skip_on_cran()
  want <- c(man_made = "A1", organic = "B1", andosol = "C6", podzol = "D1", fluvic = "E4",
            red_yellow = "F1", stagnic = "G2", eutrosol = "H1", brown_forest = "I1", regosol = "J2")
  for (g in names(want)) {
    r <- classify_scsj2017(make_scsj2017_example(g), on_missing = "silent")
    expect_identical(r$levels$code[2], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
    expect_gte(nrow(r$levels), 2L)
    expect_false(anyNA(r$levels$name_native))
  }
})

test_that("Andosols: the six groups", {
  skip_on_cran()
  expect_identical(.grp(.jp(.kb()))[["code"]], "C6")
  # Podzolic: a bleached horizon over an accumulation one
  pz <- .kb(designation = c("A", "E", "Bs", "C"), munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"),
            munsell_value_moist = c(2, 6, 4, 5), munsell_chroma_moist = c(1, 2, 4, 4),
            phosphate_absorption_coef = c(2200, 1700, 2100, 1600), oc_pct = c(9, 3.5, 2, 0.8))
  expect_identical(.grp(.jp(pz))[["code"]], "C1")
  # a yellow C (the red-yellow feature) in the soil rules the morphology out
  # (criterion 2 read for the soil: Hugo, 2026-10-31)
  pzy <- pz; pzy$munsell_chroma_moist <- c(1, 2, 4, 6)
  expect_identical(.grp(.jp(pzy))[["code"]], "C6")
  # Regosolic: ejecta without the kuroboku feature, under 3 % carbon
  rg <- .kb(phosphate_absorption_coef = c(1200, 1000, 900, 600), oc_pct = c(2.5, 1.5, 0.8, 0.3))
  expect_identical(.grp(.jp(rg))[["code"]], "C2")
  # Gleyed: a gley layer from 45 cm reaching 120 cm
  gl <- .kb(designation = c("A1", "A2", "G1", "G2"))
  expect_identical(.grp(.jp(gl))[["code"]], "C3")
  # a gley layer that stops above 75 cm is not a groundwater gley layer
  gs <- .kb(designation = c("A1", "A2", "G", "C"), bottom_cm = c(20, 45, 70, 120), top_cm = c(0, 20, 45, 70))
  expect_false(soilKey:::.scsj_groundwater_gley_within(soilKey:::.scsj_profile(.jp(gs)), 50))
  # Wet: groundwater wetness (mottles on ped faces) from 20 cm
  wt <- .kb(redoximorphic_features_pct = c(0, 10, 15, 5),
            redox_segregations_location = c(NA, "ped faces", "ped faces", "voids"))
  expect_identical(.grp(.jp(wt))[["code"]], "C4")
  # Non-allophanic: y1 of 5 or more off farmland
  na <- .kb(exchange_acidity_y1 = c(8, 9, 6, 3))
  expect_identical(.grp(.jp(na))[["code"]], "C5")
  # on farmland y1 is not read; Sio and Alp/Alo say allophanic
  expect_identical(.grp(.jp(na, list(land_use = "upland crops")))[["code"]], "C6")
  # Sio under 0.6 % is non-allophanic anywhere
  sio <- .kb(si_ox_pct = 0.3, exchange_acidity_y1 = NA_real_)
  expect_identical(.grp(.jp(sio, list(land_use = "upland crops")))[["code"]], "C5")
})

test_that("Fluvic soils: paddy, gley, gray, brown, regosolic", {
  skip_on_cran()
  expect_identical(.grp(.jp(.al()))[["code"]], "E4")
  # paddy: plough layer with mottles and Fe(d), an iron accumulation under it
  pd <- .al(designation = c("Ap", "Bir", "C1", "C2"), top_cm = c(0, 15, 25, 70), bottom_cm = c(15, 25, 70, 120),
            redoximorphic_features_pct = c(5, 20, 5, 0), fe_dcb_pct = c(0.8, 2.4, 1.0, 0.6))
  expect_identical(.grp(.jp(pd, list(land_use = "paddy")))[["code"]], "E1")
  # gley from 30 cm to 120 cm
  gl <- .al(designation = c("A", "B", "G1", "G2"), top_cm = c(0, 15, 30, 80), bottom_cm = c(15, 30, 80, 120))
  expect_identical(.grp(.jp(gl))[["code"]], "E2")
  # groundwater wetness from 15 cm
  gy <- .al(redoximorphic_features_pct = c(0, 10, 10, 5), redox_segregations_location = c(NA, "voids", "voids", "voids"))
  expect_identical(.grp(.jp(gy))[["code"]], "E3")
  # a grey subsurface, no mottles: Regosolic
  rg <- .al(munsell_hue_moist = c("10YR", "2.5Y", "2.5Y", "2.5Y"), munsell_chroma_moist = c(2, 2, 2, 2),
            munsell_value_moist = c(3, 5, 5, 5))
  expect_identical(.grp(.jp(rg))[["code"]], "E5")
})

test_that("Red-Yellow, Stagnic and Eutrosols groups", {
  skip_on_cran()
  ry <- make_scsj2017_example("red_yellow")
  ry$horizons$designation <- c("A", "Bw1", "Bw2", "BC")
  ry$horizons$clay_pct <- c(30, 33, 34, 32); ry$horizons$clay_films_amount <- NA_character_
  ry$horizons$oc_pct <- c(3, 0.6, 0.4, 0.2)
  expect_identical(.grp(ry)[["code"]], "F2")
  st <- make_scsj2017_example("stagnic")
  st$horizons$designation <- c("Ap", "Bg", "G1", "G2")
  expect_identical(.grp(st)[["code"]], "G1")
  eu <- make_scsj2017_example("eutrosol")
  eu$horizons$ca_cmol <- c(15, 18, 20, 22); eu$horizons$mg_cmol <- c(4, 3, 3, 2)
  expect_identical(.grp(eu)[["code"]], "H2")
  eu$horizons$ca_cmol[3] <- 0.5   # one subhorizon of the subsurface under 0.1? 0.5/3 is not
  expect_identical(.grp(eu)[["code"]], "H2")
  eu$horizons$mg_cmol[3] <- 10    # 0.05: Magnesian
  expect_identical(.grp(eu)[["code"]], "H1")
})

test_that("Regosols: volcanogenous, sandy, lithosols, terrestrial", {
  skip_on_cran()
  vr <- .jp(list(designation = c("A", "C1", "C2"), top_cm = c(0, 5, 40), bottom_cm = c(5, 40, 100),
                 oc_pct = c(1, 0.2, 0.1), phosphate_absorption_coef = c(250, 150, 120),
                 layer_origin = c("pumice", "pumice", "scoria"), field_texture = c("loamy sand", "sand", "sand"),
                 clay_pct = c(4, 2, 2), redoximorphic_features_pct = 0))
  expect_identical(.grp(vr)[["code"]], "J1")
  expect_identical(.grp(make_scsj2017_example("regosol"))[["code"]], "J2")
  li <- .jp(list(designation = c("A", "C", "R"), top_cm = c(0, 8, 20), bottom_cm = c(8, 20, 40),
                 oc_pct = c(3, 1, NA), phosphate_absorption_coef = c(600, 500, NA), clay_pct = c(20, 18, NA),
                 field_texture = c("loam", "loam", NA), layer_origin = c(NA, NA, "granite"),
                 redoximorphic_features_pct = c(0, 0, NA)))
  expect_identical(.grp(li)[["code"]], "J3")
  tr <- .jp(list(designation = c("A", "C1", "C2"), top_cm = c(0, 10, 50), bottom_cm = c(10, 50, 100),
                 oc_pct = c(2, 0.5, 0.3), phosphate_absorption_coef = c(600, 500, 400),
                 field_texture = c("loam", "clay loam", "clay loam"), clay_pct = c(20, 30, 30),
                 layer_origin = "weathered mudstone", redoximorphic_features_pct = 0))
  expect_identical(.grp(tr)[["code"]], "J4")
})

test_that("Man-made: artifactual, and a sand fill over a Regosol of another group", {
  skip_on_cran()
  expect_identical(.grp(make_scsj2017_example("man_made"))[["code"]], "A1")
  p <- .jp(list(designation = c("C1", "C2", "2C1", "2C2"), top_cm = c(0, 20, 40, 70), bottom_cm = c(20, 40, 70, 110),
                layer_origin = c("fill", "fill", "weathered mudstone", "weathered mudstone"),
                oc_pct = c(0.3, 0.2, 0.5, 0.3), phosphate_absorption_coef = c(200, 150, 500, 400),
                field_texture = c("sand", "sand", "clay loam", "clay loam"), clay_pct = c(3, 3, 30, 30),
                redoximorphic_features_pct = 0, artefacts_pct = 0))
  r <- soilKey:::man_made_soils_scsj(p)
  expect_true(r$evidence$fill_other_great_group)
  expect_true(r$passed)
  expect_identical(.grp(p)[["code"]], "A2")
})
