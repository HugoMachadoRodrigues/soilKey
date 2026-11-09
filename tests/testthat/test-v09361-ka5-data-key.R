# v0.9.361: the horizons of the German KA5 on the recorded data (the plan's
# section 6): .ka5_identify() in its fixed order (Hugo's decision Q9), the
# readers (the GOF, organic substance Q10, the Ah minimum Q11, the clay
# differences, the Munsell steps Q12, the area classes and field codes), the
# flagged WRB stand-ins, the transitions, the notation against the data and
# the horizon panel.

.mk5 <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "ka5-test", country = "DE"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.idf <- function(p) soilKey:::.ka5_identify(p)
.codes <- function(p) .idf(p)$code
.gw <- list(groundwater_kind = "observed", groundwater_depth_cm = 60)
.pw <- list(groundwater_kind = "perched")

test_that("the thresholds carry their pages", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("rules", "ka5", "thresholds.yaml", package = "soilKey"))$thresholds
  expect_true(length(y) >= 60L)
  for (k in names(y)) {
    expect_false(is.null(y[[k]]$value), info = k)
    expect_true(grepl("p\\. ?[0-9]|Q[0-9]", y[[k]]$page), info = k)
  }
  expect_identical(soilKey:::.ka5_thr("bt_clay_diff"), c(3L, 5L, 8L))
  expect_identical(soilKey:::.ka5_thr("ec_z_dS_m"), 0.75)
})

test_that("the GOF: the top of the first horizon that is not L, O, Of, Oh or Ovh (p. 79)", {
  skip_on_cran()
  # O layers written above the mineral surface with negative depths ("+")
  p <- .mk5(list(designation = c("L", "Of", "Oh", "Ah", "Bv", "ilCv"), top_cm = c(-9, -4, -1, 0, 8, 40),
                 bottom_cm = c(-4, -1, 0, 8, 40, 90)), list(designation_system = "ka5"))
  f <- .idf(p)
  expect_identical(f$top_gof[4:6], c(0, 8, 40))
  expect_identical(f$top_gof[1:3], c(-9, -4, -1))
  # the same layers from 0: the GOF is 9 cm and the depths are read from it
  p <- .mk5(list(designation = c("L", "Of", "Oh", "Ah", "Bv", "ilCv"), top_cm = c(0, 5, 8, 9, 17, 49),
                 bottom_cm = c(5, 8, 9, 17, 49, 99)), list(designation_system = "ka5"))
  f <- .idf(p)
  expect_identical(f$top_gof[4:6], c(0, 8, 40))
  expect_match(f$evidence[4], "the GOF at 9 cm", fixed = TRUE)
  # on the data: two forest-floor layers with their fine substance
  p <- .mk5(list(top_cm = c(0, 3, 8, 20), bottom_cm = c(3, 8, 20, 60), oc_pct = c(40, 35, 3, 0.4),
                 fine_organic_matter_pct = c(30, 80, NA, NA), clay_pct = c(NA, NA, 12, 14), silt_pct = 30),
            list(humus_form = "moder"))
  f <- .idf(p)
  expect_identical(f$code[1:3], c("Of", "Oh", "Ah"))
  expect_identical(f$top_gof[3], 0)
  # a peat counts: its surface is the GOF
  p <- .mk5(list(top_cm = c(0, 40), bottom_cm = c(40, 150), oc_pct = c(50, 52), peat_moor_type = "high-moor"))
  expect_identical(.idf(p)$top_gof, c(0, 40))
})

test_that("organic substance (Q10): the loss on ignition, the factors and the 15 to < 17.4 % band", {
  skip_on_cran()
  org <- function(...) {
    x <- soilKey:::.ka5_profile(.mk5(list(top_cm = 0, bottom_cm = 20, ...)))
    soilKey:::.ka5_organic(x, 1L)$passed
  }
  expect_true(org(oc_pct = 20))
  expect_false(org(oc_pct = 14))
  expect_true(is.na(org(oc_pct = 16)))
  expect_true(org(oc_pct = 16, von_post_index = 5))
  expect_false(org(oc_pct = 16, loi_pct = 25))
  expect_true(org(oc_pct = 10, loi_pct = 40))
  expect_true(org(humus_class_ka5 = "h7"))
  expect_false(org(humus_class_ka5 = "h6"))
  # the band decided by the notation
  x <- soilKey:::.ka5_profile(.mk5(list(designation = c("Go-Aa", "Gr"), top_cm = c(0, 20), bottom_cm = c(20, 90),
                                         oc_pct = c(16, 0.3)), list(designation_system = "ka5")))
  r <- soilKey:::.ka5_organic(x, 1L)
  expect_false(r$passed)
  expect_true("designation" %in% r$tag)
  # on the data the band leaves the layer undecided between H and Aa
  f <- .idf(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(16, 0.5), clay_pct = c(NA, 20))))
  expect_true(is.na(f$code[1]))
  expect_true(all(c("H", "Aa") %in% f$candidates[[1]]))
  expect_match(f$evidence[1], "15 to < 17.4", fixed = TRUE)
})

test_that("the Ah minimum by texture and the silt of other fractions (Q11)", {
  skip_on_cran()
  am <- function(oc, clay, silt, site = list(), ...) {
    x <- soilKey:::.ka5_profile(.mk5(list(top_cm = 0, bottom_cm = 20, oc_pct = oc, clay_pct = clay, silt_pct = silt, ...), site))
    soilKey:::.ka5_ah_min(x, 1L)$passed
  }
  # 0.6 % organic substance: OC 0.35 x 1.72 = 0.60
  expect_true(am(0.36, 10, 30))
  expect_false(am(0.34, 10, 30))
  # 0.9 %: silt 50 % or more, or clay 17 to < 45
  expect_false(am(0.45, 10, 55))
  expect_true(am(0.53, 20, 30))
  # 1.2 %: clay 45 or more (45 in the upper class)
  expect_false(am(0.6, 45, 30))
  expect_true(am(0.71, 45, 30))
  # silt 45 to < 50 in other fractions: 0.6 or 0.9 undecided; KA5 fractions decide
  expect_true(is.na(am(0.45, 10, 47)))
  expect_true(am(0.45, 10, 47, list(particle_size_scheme = "ka5")))
  expect_true(am(0.6, 10, 47))
  # the humus class of field 29 (Tab. 15), read by its range
  expect_true(am(NA, 10, 30, humus_class_ka5 = "h2"))
  expect_false(am(NA, 45, 30, humus_class_ka5 = "h1"))
  expect_true(is.na(am(NA, 10, 30)))
})

test_that("Munsell steps (Q12): one page of hue or two chroma steps", {
  skip_on_cran()
  br <- function(h1, c1, h2, c2) {
    x <- soilKey:::.ka5_profile(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 50), munsell_hue_moist = c(h1, h2),
                                           munsell_value_moist = 4, munsell_chroma_moist = c(c1, c2))))
    soilKey:::.ka5_browner(x, 1L, 2L)$passed
  }
  expect_true(br("7.5YR", 4, "10YR", 4))
  expect_false(br("10YR", 4, "10YR", 3))
  expect_true(br("10YR", 6, "10YR", 4))
  expect_false(br("2.5Y", 4, "10YR", 4))
  expect_true(is.na(br(NA, NA, "10YR", 4)))
})

test_that("one undesignated layer per master and main suffix", {
  skip_on_cran()
  # F (p. 89)
  expect_identical(.codes(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 3, redoximorphic_features_pct = 10,
                                    munsell_hue_moist = "5Y", munsell_value_moist = 4, munsell_chroma_moist = 2), list(subaqueous = TRUE))), "Fo")
  expect_identical(.codes(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 5, munsell_hue_moist = "N", munsell_value_moist = 2,
                                    munsell_chroma_moist = 0), list(subaqueous = TRUE))), "Fr")
  expect_identical(.codes(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 8, munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                                    munsell_chroma_moist = 2), list(subaqueous = TRUE))), "Fh")
  expect_identical(.codes(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 0.2, munsell_hue_moist = "2.5Y", munsell_value_moist = 6,
                                    munsell_chroma_moist = 2), list(subaqueous = TRUE))), "Fi")
  expect_identical(.codes(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 0.4, munsell_hue_moist = "2.5Y", munsell_value_moist = 6,
                                    munsell_chroma_moist = 2, layer_origin = "tidal-marine"), list(tidal_zone = "intertidal"))), "tmFw")
  # H (pp. 89-91) and the O (pp. 90-91)
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150), oc_pct = c(45, 48, 50),
                                    peat_moor_type = "low-moor", structure_type = c("crumb", "massive", "massive"),
                                    redoximorphic_features_pct = c(0, 10, 0), water_saturation_days = c(100, 200, 365)),
                               list(artificially_drained = TRUE))), c("nHv", "nHw", "nHr"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 15, 30), bottom_cm = c(15, 30, 100), oc_pct = c(50, 48, 47),
                                    peat_moor_type = "low-moor", von_post_index = c(8, 6, 4), munsell_hue_moist = "N",
                                    munsell_value_moist = c(2, 3, 3), water_repellence = c("repellent", NA, NA),
                                    structure_type = c("single grain", "subangular blocky", "prismatic")))), c("nHm", "nHa", "nHt"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 1), bottom_cm = c(1, 30), oc_pct = c(45, 2), fine_organic_matter_pct = c(5, NA),
                                    clay_pct = c(NA, 10), silt_pct = 30), list(humus_form = "mull")))[1], "L")
  # anthropogenic and colluvial (pp. 106-108)
  expect_identical(.codes(.mk5(list(top_cm = c(0, 15, 30), bottom_cm = c(15, 30, 100), oc_pct = c(3, 1, 0.5),
                                    soil_gas_ch4_co2_pct = c(NA, 20, 40), redoximorphic_features_pct = c(0, 10, 0),
                                    reduction_colours_pct = c(0, 5, 90)), list(reductgas_source = "refuse_sludge_harbour_mud")))[2:3],
                   c("Yo", "Yr"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120), oc_pct = c(2, 1, 0.2), clay_pct = 15,
                                    rock_diggable = c(NA, NA, TRUE), caco3_pct = 0),
                               list(deep_cultivation_kind = "repeated_trenching", mechanical_mixing_depth_cm = 70, land_use = "vineyard"))),
                   c("R-Ap", "R", "ilC"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 100), oc_pct = c(2, 1.5, 0.3), clay_pct = 5, silt_pct = 10,
                                    p2o5_al_mg_kg = c(300, 250, 40)), list(deep_cultivation_kind = "plaggen", mechanical_mixing_depth_cm = 30)))[2], "E")
  expect_identical(.codes(.mk5(list(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 100), oc_pct = c(2, 1.2, 0.3), clay_pct = 15, silt_pct = 40,
                                    layer_origin = c(NA, "colluvic", NA), stratification_pattern = c(NA, "continuous", NA))))[2], "M")
  # G (pp. 104-106; Q13)
  expect_identical(.codes(.mk5(list(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120), oc_pct = c(3, 0.5, 0.3), clay_pct = 20, silt_pct = 40,
                                    redoximorphic_features_pct = c(0, 20, 1), reduction_colours_pct = c(0, 5, 98), rh_value = c(NA, NA, 15)), .gw)),
                   c("Ah", "Go", "Gr"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), oc_pct = c(3, 0.5, 0.3), clay_pct = 20,
                                    redoximorphic_features_pct = c(0, 20, 10), reduction_colours_pct = c(0, 30, 70)), .gw)),
                   c("Ah", "Gro", "Gor"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), oc_pct = c(3, 0.2, 0.2), clay_pct = c(10, 2, 15),
                                    redoximorphic_features_pct = c(0, 0, 1), bleached_pct = 0, reduction_colours_pct = c(0, 0, 99),
                                    childs_test = c(NA, "negative", "positive"),
                                    munsell_hue_moist = c("10YR", "10YR", "5GY"), munsell_value_moist = c(3, 6, 5),
                                    munsell_chroma_moist = c(2, 3, 1)), .gw))[2:3], c("Gw", "Gr"))
  # S (pp. 101-103)
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 100), oc_pct = c(3, 0.5, 0.3), clay_pct = c(15, 15, 30),
                                    redoximorphic_features_pct = c(0, 30, 40), bleached_pct = c(0, 55, 30), ksat_mm_h = c(NA, 20, 1),
                                    air_capacity_pct = c(NA, 8, 2), rh_value = c(NA, 17, NA)), .pw)), c("Ah", "Sw", "Sd"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 90), oc_pct = c(3, 0.3), clay_pct = c(12, 12), silt_pct = 60,
                                    sand_ffs_pct = 10, redoximorphic_features_pct = c(0, 40), bleached_pct = c(0, 45), air_capacity_pct = c(NA, 2)), .pw))[2],
                   "Sg")
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), oc_pct = c(3, 0.3, 0.2), clay_pct = 30,
                                    knick_degree = c(NA, "strong", NA))))[2], "Sq")
  # A (pp. 92-95)
  expect_identical(.codes(.mk5(list(top_cm = c(0, 10, 25, 40), bottom_cm = c(10, 25, 40, 80), oc_pct = c(4, 0.3, 2, 0.3), clay_pct = 3, silt_pct = 5,
                                    c_pyro_pct = c(NA, NA, 1.2, NA), fe_pyro_pct = c(NA, NA, 0.3, NA),
                                    munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(3, 6, 3, 5),
                                    munsell_chroma_moist = c(1, 1, 4, 4), rock_diggable = c(NA, NA, NA, TRUE))))[1:3], c("Ah", "Ae", "Bhs"))
  lb <- .codes(.mk5(list(top_cm = c(0, 25, 45, 90), bottom_cm = c(25, 45, 90, 130), oc_pct = c(2, 0.4, 0.3, 0.1),
                         clay_pct = c(15, 15, 24, 18), silt_pct = 60, clay_films_amount = c(NA, NA, "common", NA),
                         munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 4, 4, 4),
                         caco3_pct = c(0, 0, 0, 10))))
  expect_identical(lb[1:3], c("Ah", "Al", "Bt"))
  ael <- .codes(.mk5(list(top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 100), oc_pct = c(2, 0.3, 0.2), clay_pct = c(12, 12, 26),
                          silt_pct = 60, clay_films_amount = c(NA, NA, "many"), munsell_hue_moist = "10YR",
                          munsell_value_moist = c(3, 7, 4), munsell_chroma_moist = c(2, 2, 4), caco3_pct = 0)))
  expect_identical(ael[2:3], c("Ael", "Bt"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 60, 80), bottom_cm = c(60, 80, 150), oc_pct = c(2.5, 0.8, 0.3), clay_pct = 22, silt_pct = 65,
                                    bs_pct = c(95, 98, 100), worm_holes_pct = c(10, 5, 0), structure_type = c("granular", "granular", "massive"),
                                    secondary_carbonates_pct = c(0, 2, 5), caco3_pct = c(0, 5, 15), rock_diggable = c(NA, NA, TRUE))))[1:2],
                   c("Axh", "Acxh"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 1), bottom_cm = c(1, 100), oc_pct = c(0.2, 0.05), clay_pct = 5, silt_pct = 10,
                                    rock_diggable = c(NA, TRUE), caco3_pct = c(0, 0)))), c("Ai", "ilC"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 30), bottom_cm = c(30, 90), oc_pct = c(2, 0.2), clay_pct = 15, silt_pct = 30,
                                    rock_diggable = c(NA, TRUE), caco3_pct = 0), list(land_use = "arable"))), c("Ap", "ilC"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 90), oc_pct = c(12, 0.3), clay_pct = 20,
                                    munsell_hue_moist = c("10YR", "5G"), munsell_value_moist = c(2, 5), munsell_chroma_moist = c(1, 1),
                                    rh_value = c(NA, 12)), .gw)), c("Aa", "Gr"))
  # B (pp. 95-98), P (p. 100), T (p. 101)
  bv <- .codes(.mk5(list(top_cm = c(0, 15, 50, 90), bottom_cm = c(15, 50, 90, 120), oc_pct = c(2.5, 0.2, 0.1, 0.1), clay_pct = c(12, 15, 10, 10),
                         silt_pct = 30, munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(3, 4, 5, 5),
                         munsell_chroma_moist = c(2, 6, 4, 4), caco3_pct = 0, rock_diggable = c(NA, NA, NA, TRUE))))
  expect_identical(bv[1:2], c("Ah", "Bv"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90), oc_pct = c(2.5, 0.2, 0.1), clay_pct = 12,
                                    bulk_density_g_cm3 = c(1.0, 0.7, 1.4), caco3_pct = 0, rock_diggable = c(NA, NA, TRUE))))[2], "Bfv")
  expect_identical(.codes(.mk5(list(top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100), oc_pct = c(3, 0.4, 0.2), clay_pct = c(40, 55, 50),
                                    slickensides = c(NA, "common", NA), structure_type = c("granular", "prismatic", "massive"),
                                    parent_rock_ka5 = c(NA, "Tonstein", "Tonstein"), caco3_pct = c(0, 0, 8),
                                    rock_diggable = c(NA, NA, TRUE))))[2:3], c("P", "elC"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 10, 45), bottom_cm = c(10, 45, 80), oc_pct = c(5, 0.5, NA), clay_pct = c(40, 70, NA),
                                    structure_type = c("granular", "subangular blocky", NA), caco3_pct = c(0, 0, NA),
                                    carbonate_whole_soil_pct = c(NA, NA, 95), rock_diggable = c(NA, NA, FALSE),
                                    munsell_hue_moist = c("10YR", "7.5YR", NA), munsell_value_moist = c(3, 4, NA),
                                    munsell_chroma_moist = c(2, 6, NA))))[2:3], c("Tv", "cmC"))
  expect_identical(.codes(.mk5(list(top_cm = c(0, 10, 45), bottom_cm = c(10, 45, 80), oc_pct = c(5, 0.5, NA), clay_pct = c(40, 70, NA),
                                    structure_type = c("granular", "subangular blocky", NA), caco3_pct = c(0, 0, NA),
                                    carbonate_whole_soil_pct = c(NA, NA, 95), rock_diggable = c(NA, NA, FALSE),
                                    munsell_hue_moist = c("10YR", "2.5YR", NA), munsell_value_moist = c(3, 4, NA),
                                    munsell_chroma_moist = c(2, 6, NA))))[2], "Tu")
  # C (pp. 98-100; Q15)
  ahc <- function(...) .codes(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(4, NA), clay_pct = c(15, NA), ...)))[2]
  expect_identical(ahc(rock_diggable = c(NA, FALSE), caco3_pct = c(0, 0)), "imC")
  expect_identical(ahc(rock_diggable = c(NA, FALSE), carbonate_whole_soil_pct = c(NA, 95)), "cmC")
  expect_identical(ahc(rock_diggable = c(NA, TRUE), caco3_pct = c(1, 15)), "elC")
  expect_identical(ahc(coarse_fragments_pct = c(30, 98)), "xC")
  expect_identical(ahc(rock_diggable = c(NA, TRUE), caco3_pct = c(0, 0), weathering_stage = c(NA, "fresh")), "ilCn")
})

test_that("undecided tests give candidates with the attributes that would decide them", {
  skip_on_cran()
  # mottles with no record of the water: G or S
  f <- .idf(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 90), oc_pct = c(3, 0.3), clay_pct = 20,
                      redoximorphic_features_pct = c(0, 30), bleached_pct = c(0, 60))))
  expect_true(is.na(f$code[2]))
  expect_true(is.na(f$passed[2]))
  expect_true(all(c("Go", "Sw", "Sd") %in% f$candidates[[2]]))
  expect_true("groundwater_kind" %in% f$missing[[2]])
  # perched water, permeability and air capacity not recorded: Sw or Sd
  f <- .idf(.mk5(list(top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 100), oc_pct = c(3, 0.5, 0.3), clay_pct = c(15, 15, 30),
                      redoximorphic_features_pct = c(0, 30, 40), bleached_pct = c(0, 55, 30)), .pw))
  expect_true(all(c("Sw", "Sd") %in% f$candidates[[2]]))
  expect_true(all(c("ksat_mm_h", "air_capacity_pct") %in% f$missing[[2]]))
  # reduction colours with no rH or dipyridyl test: a Gr candidate
  f <- .idf(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 90), oc_pct = c(3, 0.3), clay_pct = 20,
                      munsell_hue_moist = c("10YR", "5G"), munsell_value_moist = c(3, 5), munsell_chroma_moist = c(2, 1)), .gw))
  expect_true("Gr" %in% f$candidates[[2]])
  expect_true(all(c("rh_value", "childs_test") %in% f$missing[[2]]))
  # a dark Ah is not read as reduction-coloured
  expect_identical(f$code[1], "Ah")
  # the bottom row with no row below in its layer: the Bv undecided (Q12)
  f <- .idf(.mk5(list(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90), oc_pct = c(2.5, 0.2, 0.1), clay_pct = c(12, 15, 10),
                      silt_pct = 30, munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 5),
                      munsell_chroma_moist = c(2, 6, 4), caco3_pct = 0)))
  expect_true(all(c("Bv", "ilC") %in% f$candidates[[3]]))
  # the carbonate of the rock not recorded: its prefix left open
  f <- .idf(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(4, NA), clay_pct = c(15, NA), rock_diggable = c(NA, TRUE))))
  expect_identical(f$code[2], "lC")
  expect_setequal(f$prefixes_na[[2]], c("i", "e", "c"))
})

test_that("the transitions with rules in the text (Q9)", {
  skip_on_cran()
  col <- list(munsell_hue_moist = c("10YR", "7.5YR", "10YR", "10YR"), munsell_value_moist = c(3, 4, 5, 5),
              munsell_chroma_moist = c(2, 6, 4, 4))
  base <- c(list(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 120), oc_pct = c(3, 0.3, 0.1, 0.1), clay_pct = 12,
                 silt_pct = 30, caco3_pct = 0, rock_diggable = c(NA, NA, NA, TRUE)), col)
  # S features 10 to < 80 % in a Bv: Bv-Sw; under 10: Sw-Bv (p. 102)
  expect_identical(.codes(.mk5(c(base, list(redoximorphic_features_pct = c(0, 15, 0, 0))), .pw))[2], "Bv-Sw")
  expect_identical(.codes(.mk5(c(base, list(redoximorphic_features_pct = c(0, 4, 0, 0))), .pw))[2], "Sw-Bv")
  # rust under 5 % in the groundwater zone: Go-Bv (p. 105)
  expect_identical(.codes(.mk5(c(base, list(redoximorphic_features_pct = c(0, 3, 0, 0))), .gw))[2], "Go-Bv")
  # rust 5 % or more in a Bv: Bv-Go
  expect_identical(.codes(.mk5(c(base, list(redoximorphic_features_pct = c(0, 8, 0, 0), reduction_colours_pct = 0)), .gw))[2], "Bv-Go")
  # the Ah humus minimum under the topsoil: Ah-Bv (p. 96)
  b2 <- base; b2$oc_pct <- c(3, 0.6, 0.1, 0.1)
  expect_identical(.codes(.mk5(b2))[2], "Ah-Bv")
  # a Go with the Ah minimum at the surface: Ah-Go
  expect_identical(.codes(.mk5(list(top_cm = c(0, 25), bottom_cm = c(25, 90), oc_pct = c(3, 0.3), clay_pct = 20,
                                    redoximorphic_features_pct = c(10, 1), reduction_colours_pct = c(0, 99),
                                    rh_value = c(NA, 12)), .gw)), c("Ah-Go", "Gr"))
})

test_that("the field codes and area classes read into the tests", {
  skip_on_cran()
  # fields 30 and 31 (Tab. 16, Tab. 4): rust f6 (30-50), wet bleaching f7 (50-70): 80 % or more
  f <- .idf(.mk5(list(top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 100), oc_pct = c(3, 0.5, 0.3), clay_pct = c(15, 15, 30),
                      hydromorphic_features_ka5 = c(NA, "ed,fl,g3,f6;rb,f7", "ed,f6;rb,mr,f6"),
                      ksat_mm_h = c(NA, 20, 1), air_capacity_pct = c(NA, 8, 2), rh_value = c(NA, 17, NA)), .pw))
  expect_identical(f$code[2:3], c("Sw", "Sd"))
  x <- soilKey:::.ka5_profile(.mk5(list(top_cm = 0, bottom_cm = 20, hydromorphic_features_ka5 = "ed,fl,g3,f6;rb,f7")))
  sh <- soilKey:::.ka5_shares(x, 1L)
  expect_identical(sh$rust, c(30, 50))
  expect_identical(sh$s, c(80, 100))
  # the carbonate class of field 46 (Tab. 40) gives the rock prefix
  f <- .idf(.mk5(list(top_cm = c(0, 20), bottom_cm = c(20, 60), oc_pct = c(4, NA), clay_pct = c(15, NA),
                      rock_diggable = c(NA, TRUE), carbonate_class_ka5 = c(NA, "c4"))))
  expect_identical(f$code[2], "elC")
  expect_true(grepl("carbonate class c4", f$evidence[2], fixed = TRUE))
})

test_that("the notation against the data: the written horizon is kept, the data go to the evidence", {
  skip_on_cran()
  h <- list(designation = c("Ah", "Bv", "Bt", "ilC"), top_cm = c(0, 25, 45, 90), bottom_cm = c(25, 45, 90, 130),
            oc_pct = c(2, 0.3, 0.3, 0.1), clay_pct = c(15, 15, 24, 18), silt_pct = 60,
            clay_films_amount = c(NA, NA, "common", NA), munsell_hue_moist = "10YR",
            munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 4, 4, 4), caco3_pct = c(0, 0, 0, 10),
            rock_diggable = c(NA, NA, NA, TRUE))
  f <- .idf(.mk5(h, list(designation_system = "ka5")))
  expect_identical(f$code, c("Ah", "Bv", "Bt", "ilC"))
  expect_identical(f$route, c("both", "notation", "both", "both"))
  expect_identical(f$data_code[2], "Al")
  expect_match(f$evidence[2], "the data read Al; the notation's Bv is kept", fixed = TRUE)
  expect_true("Al" %in% f$candidates[[2]])
  # a profile in another notation is read on the data
  h$designation <- c("Ap", "E", "Bt", "C")
  f <- .idf(.mk5(h, list(designation_system = "fao2006")))
  expect_identical(f$route, rep("data", 4L))
  # (v0.9.380: the FAO p states the tillage, so the FAO Ap is read Ap)
  expect_identical(f$code[1:3], c("Ap", "Al", "Bt"))
  # the frame is what the sequence engine reads: a KA5 formula matches it
  m <- match_horizon_sequence(f, soilKey:::.ka5_parse_formula("Ah/Al/Bt/C"))
  expect_true(isTRUE(m$passed))
})

test_that("the stand-ins are flagged and lower the grade", {
  skip_on_cran()
  # clay coatings not recorded: soilKey's argic horizon stands in beside the measured clay difference
  f <- .idf(.mk5(list(top_cm = c(0, 25, 45, 90), bottom_cm = c(25, 45, 90, 130), oc_pct = c(2, 0.4, 0.3, 0.1),
                      clay_pct = c(15, 14, 26, 20), silt_pct = 60, sand_pct = c(25, 26, 14, 20), munsell_hue_moist = "10YR",
                      munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 4, 4, 4), caco3_pct = c(0, 0, 0, 10),
                      rock_diggable = c(NA, NA, NA, TRUE))))
  expect_identical(f$code[3], "Bt")
  expect_true("argic" %in% f$stand_ins[[3]])
  expect_true(f$lower_grade[3])
  expect_match(f$evidence[3], "argic()", fixed = TRUE)
})

test_that("the horizon panel and the ASCII sources", {
  skip_on_cran()
  f <- .idf(.mk5(list(designation = c("Ah", "Bv", "ilCv"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90),
                      oc_pct = c(2.5, 0.4, 0.1)), list(designation_system = "ka5")))
  pn <- soilKey:::.ka5_horizon_panel(f)
  expect_identical(names(pn), c("depth", "designation", "horizon", "route", "candidates", "stand_ins", "evidence"))
  expect_identical(pn$depth, c("0-15", "15-50", "50-90"))
  expect_identical(pn$horizon, c("Ah", "Bv", "ilCv"))
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  for (fn in c("ka5-readers.R", "diagnostics-ka5-horizons.R")) {
    txt <- readLines(file.path(src, fn), warn = FALSE)
    expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)), info = fn)
  }
})

test_that("the review of 2026-11-06: prefixes from the data, the groundwater zone, Al or Ael, no topsoil under peat", {
  skip_on_cran()
  # D3: a notation C without i, e, c and an H without h, n, u take the data, else stay open
  f <- .idf(.mk5(list(designation = c("Ah", "mC"), top_cm = c(0, 20), bottom_cm = c(20, 60), carbonate_whole_soil_pct = c(NA, 95)),
                 list(designation_system = "ka5")))
  expect_true("c" %in% f$prefixes[[2]])
  f <- .idf(.mk5(list(designation = c("Ah", "lC"), top_cm = c(0, 20), bottom_cm = c(20, 60)), list(designation_system = "ka5")))
  expect_setequal(f$prefixes_na[[2]], c("i", "e", "c"))
  f <- .idf(.mk5(list(designation = c("Hw", "Hr"), top_cm = c(0, 40), bottom_cm = c(40, 150), peat_moor_type = "low-moor", oc_pct = 45),
                 list(designation_system = "ka5")))
  expect_true("n" %in% f$prefixes[[1]])
  # D12: a water table at 250 cm leaves the redox features above its zone to perched water
  ps <- list(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100), oc_pct = c(3, 0.5, 0.3), clay_pct = c(15, 15, 30), silt_pct = 60,
             sand_pct = c(25, 25, 10), redoximorphic_features_pct = c(0, 30, 40), bleached_pct = c(0, 55, 30),
             ksat_mm_h = c(NA, 20, 1), air_capacity_pct = c(NA, 8, 2), rh_value = c(NA, 17, NA))
  expect_identical(.codes(.mk5(ps, list(groundwater_depth_cm = 250))), c("Ah", "Sw", "Sd"))
  f <- .idf(.mk5(ps, list(groundwater_depth_cm = 60)))
  expect_true("Go" %in% f$candidates[[2]])
  expect_false("Sw" %in% f$candidates[[2]])
  # D11: the Ael colour not recorded: Al or Ael
  f <- .idf(.mk5(list(top_cm = c(0, 10, 45, 100), bottom_cm = c(10, 45, 100, 130), oc_pct = c(2, 0.3, 0.2, 0.1),
                      clay_pct = c(10, 6, 26, 18), silt_pct = 60, sand_pct = c(30, 34, 14, 22),
                      munsell_hue_moist = c("10YR", NA, "7.5YR", "10YR"), munsell_value_moist = c(3, NA, 4, 5),
                      munsell_chroma_moist = c(2, NA, 4, 4), clay_films_amount = c(NA, NA, "common", NA), caco3_pct = c(0, 0, 0, 12))))
  expect_true(is.na(f$code[2]))
  expect_true(all(c("Al", "Ael") %in% f$candidates[[2]]))
  # D19: no run of topsoils under 30 cm or more of peat
  f <- .idf(.mk5(list(designation = c("H1", "H2", "Cr"), top_cm = c(0, 40, 90), bottom_cm = c(40, 90, 150), oc_pct = c(40, 45, 1))))
  expect_false(isTRUE(substr(f$carrying[3], 1, 1) == "A"))
  # D15 and R3: the conventions carry their numbers; the F kind from colour cuts lowers the grade
  expect_identical(soilKey:::.ka5_thr("srd_days_min"), 300L)
  expect_identical(soilKey:::.ka5_thr("gw_zone_above_cm"), 100L)
  f <- .idf(.mk5(list(top_cm = 0, bottom_cm = 40, oc_pct = 5, munsell_hue_moist = "N", munsell_value_moist = 2,
                      munsell_chroma_moist = 0), list(subaqueous = TRUE)))
  expect_true(f$lower_grade[1])
})
