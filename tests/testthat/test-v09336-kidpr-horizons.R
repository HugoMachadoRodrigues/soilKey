# v0.9.336: the key to the diagnostic horizons of the Russian Field Guide of
# 2008 (section 2.1, 66 positions) run on the recorded data
# (R/diagnostics-kidpr-horizons.R, R/kidpr-readers.R), Hugo's decisions of
# 2026-11-02 (Q4-Q8, Q11, Q12, Q18, Q20-Q23 and the defaults). One
# hand-written undesignated profile per outcome of the key, each read by the
# data route; the undecided branches, the notation route, the fallbacks, the
# WRB stand-ins, the overlays and the small horizons.

# --- builders --------------------------------------------------------------------

# One layer: a plain parent material unless told otherwise.
.ru_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", secondary_carbonates_form = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
# A profile of layers; the site natural (forest) and frost-free unless told.
.ru_P <- function(..., site = list()) {
  s <- list(frost_regime = "none", land_use = "forest")
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
# A grey-humus surface (AY), a brown middle horizon, an organic layer, a
# sandy layer, a deposited layer, a buried humus horizon.
.ru_AY <- function(top = 0, bottom = 15, ...) .ru_L(top, bottom, munsell_value_dry = 5, munsell_chroma_dry = 2,
                                                   structure_type = "granular", structure_grade = "moderate",
                                                   ph_h2o = 5.0, oc_pct = 2, clay_pct = 15, silt_pct = 50, sand_pct = 35, ...)
.ru_B <- function(top, bottom, ...) .ru_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 5, munsell_chroma_dry = 6,
                                          structure_type = "blocky", structure_grade = "moderate", clay_pct = 25, ...)
.ru_org <- function(top, bottom, ...) .ru_L(top, bottom, munsell_hue_dry = "7.5YR", munsell_value_dry = 3, munsell_chroma_dry = 3,
                                            plant_residues_pct = 80, peat_decomposition_pct = 30, oc_pct = 40, ...)
.ru_sand <- function(top, bottom, ...) .ru_L(top, bottom, clay_pct = 3, silt_pct = 5, sand_pct = 92, ...)
.ru_dep <- function(top, bottom, ...) .ru_L(top, bottom, layer_origin = "fluvic", ...)
.ru_Ab <- function(top, bottom) .ru_AY(top, bottom, designation = "Ab")
# Podzolic soils (AY over EL over BT) with the EL between two depths.
.ru_elp <- function(top_el = 15, bot_el = 35) {
  .ru_P(.ru_AY(0, top_el),
        .ru_L(top_el, bot_el, munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy",
              structure_grade = "weak", clay_pct = 10, silt_pct = 55, sand_pct = 35),
        .ru_B(bot_el, 90, clay_pct = 30, silt_pct = 40, sand_pct = 30, structure_type = "prismatic",
              clay_films_amount = "common"),
        .ru_L(90, 150))
}
# A chernozem: a dark AU over a middle layer over calcareous loess.
.ru_chz <- function(mid) .ru_P(.ru_AY(0, 40, munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7.2, oc_pct = 4),
                               mid, .ru_L(90, 150, caco3_pct = 10))
# A podzol: O, E, a BHF fading downward, sand.
.ru_podzol <- function(e_top = 8, e_bot = 20) {
  .ru_P(.ru_org(0, e_top, peat_decomposition_pct = 20),
        .ru_sand(e_top, e_bot, munsell_value_dry = 7, munsell_chroma_dry = 1, structure_type = "single grain"),
        .ru_sand(e_bot, 50, munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6, uncoated_grains_pct = 30),
        .ru_sand(50, 80, munsell_value_dry = 6, munsell_chroma_dry = 6, uncoated_grains_pct = 50),
        .ru_sand(80, 120, uncoated_grains_pct = 95))
}
.ru_id <- function(p) .kidpr_identify(p)
# Expect layer i of a profile read by the data route as `code`.
.ru_expect <- function(p, i, code, passed = TRUE) {
  f <- .ru_id(p)
  expect_identical(f$code[i], code)
  expect_identical(f$passed[i], passed)
  expect_identical(f$route[i], "data")
  expect_length(f$candidates[[i]], 0L)
  invisible(f)
}


# --- the key ------------------------------------------------------------------------

test_that("the key is the 66 positions of section 2.1, with the copies of the overlays", {
  skip_on_cran()
  b <- .kidpr_key_base()
  expect_identical(names(b), as.character(1:66))
  t <- .kidpr_key_tree()
  expect_true(all(c("S.5", "X.5", "V.24", "u.14", "ca.65", "S.V.24", "X.u.14") %in% names(t)))
  # every outcome of the base key is a horizon of the catalogue, a
  # modification, TT, a feature of the catalogue or soilKey's own ends
  to <- unlist(lapply(b, function(n) vapply(n$branches, function(x) x$to, "")))
  out <- setdiff(to, names(b))
  v <- .kidpr_vocabulary()
  horiz <- c(v$horizons, names(v$modifications), "TT")
  expect_setequal(intersect(out, horiz), out[grepl("^[A-Z]", out)])
  expect_true(all(out[grepl("^[a-z]", out)] %in% v$feature_codes))
  expect_setequal(out[grepl("^_", out)], c("_gap7", "_deposit", "_gap60", "_none"))
  # the 50 horizons and the three modifications are outcomes
  expect_true(all(setdiff(v$horizons, c("S", "X", "V")) %in% out))
  expect_true(all(c("AV", "BH", "BF", "TT", "SS") %in% out))
  # S, X and V are overlays: the copies after 3b, 4a and 23a carry them
  tout <- unlist(lapply(t, function(n) vapply(n$branches, function(x) x$to, "")))
  expect_true(all(c("_none+S", "_none+X", "_none+V", "AY+S", "AY+X", "G+V") %in% tout))
  expect_setequal(out[grepl("^[a-z]", out)], c("tr", "rh", "r", "ao", "e", "el"))
  # the namespace's identification is found by identify_horizons()
  expect_identical(.horizon_identifier("kidpr"), .kidpr_identify)
})


# --- surface humus horizons ------------------------------------------------------------

test_that("surface humus horizons: AY, AJ, AU, the dark vertic AV, ASN, SEL, W, AK", {
  skip_on_cran()
  .ru_expect(.ru_P(.ru_AY(), .ru_B(15, 60), .ru_L(60, 120)), 1, "AY")
  .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 6, ph_h2o = 7.6, structure_type = "crumb", structure_grade = "weak"),
                   .ru_L(15, 100)), 1, "AJ")
  f <- .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7, oc_pct = 4), .ru_L(15, 100)), 1, "AU")
  expect_match(f$key_path[1], "15\u0430 > 16\u0430: AU", fixed = TRUE)
  f <- .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7.5, slickensides = "many",
                               structure_type = "wedge-shaped", clay_pct = 50), .ru_L(40, 100)), 1, "V")
  expect_identical(f$modification[1], "AV")
  .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 8, structure_type = "columnar",
                          clay_films_amount = "many"), .ru_L(20, 100)), 1, "ASN")
  # 18b: platy, directly over a solonetz horizon
  f <- .ru_expect(.ru_P(.ru_AY(0, 12, munsell_value_dry = 6, ph_h2o = 7.6, structure_type = "platy"),
                        .ru_L(12, 40, munsell_value_dry = 5, munsell_chroma_dry = 3, structure_type = "columnar",
                              clay_pct = 40, clay_films_amount = "common", ph_h2o = 8.5),
                        .ru_L(40, 100)), 1, "SEL")
  expect_match(f$key_path[1], "18\u0431: SEL", fixed = TRUE)
  expect_identical(f$code[2], "BSN")
  # 5: the only horizon, under 5 cm in loam, with roots, over the parent material
  .ru_expect(.ru_P(.ru_L(0, 3, munsell_value_dry = 5, munsell_chroma_dry = 3, roots_abundance = "common"),
                   .ru_L(3, 50, designation = "C")), 1, "W")
  # without roots recorded there is no W (a field observation not recorded)
  f <- .ru_id(.ru_P(.ru_L(0, 3, munsell_value_dry = 5, munsell_chroma_dry = 3), .ru_L(3, 50, designation = "C")))
  expect_false(identical(f$code[1], "W"))
  # 21a: chestnut, the darkest of the profile, with permafrost
  .ru_expect(.ru_P(.ru_L(0, 15, munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 3, structure_type = "granular"),
                   .ru_L(15, 60), site = list(frost_regime = "permafrost")), 1, "AK")
})

test_that("agro horizons: P, PU, PB by the colour, PT and PTR by the organic matter", {
  skip_on_cran()
  arable <- list(land_use = "arable")
  .ru_expect(.ru_P(.ru_AY(0, 25, boundary_distinctness = "abrupt", boundary_topography = "smooth"), .ru_L(25, 100),
                   site = arable), 1, "P")
  .ru_expect(.ru_P(.ru_AY(0, 25, munsell_value_dry = 3, munsell_chroma_dry = 1, boundary_distinctness = "abrupt"),
                   .ru_L(25, 100), site = list(land_use = "cropland")), 1, "PU")
  f <- .ru_expect(.ru_P(.ru_L(0, 25, structure_type = "cloddy", boundary_distinctness = "abrupt"), .ru_L(25, 100),
                        site = arable), 1, "PB")
  expect_match(f$key_path[1], "20\u0430 > 21\u0431: PB", fixed = TRUE)
  .ru_expect(.ru_P(.ru_org(0, 25, boundary_distinctness = "abrupt", boundary_topography = "smooth"), .ru_org(25, 40),
                   .ru_L(40, 100), site = arable), 1, "PT")
  .ru_expect(.ru_P(.ru_L(0, 25, plant_residues_pct = 20, oc_pct = 12, munsell_value_dry = 3, munsell_chroma_dry = 2,
                         boundary_distinctness = "abrupt"), .ru_L(25, 100), site = arable), 1, "PTR")
  # the plough suffix p of another notation in the layers from the surface
  f <- .ru_id(.ru_P(.ru_AY(0, 20, designation = "Ap1"), .ru_AY(20, 28, designation = "Ap2"), .ru_L(28, 90, designation = "C")))
  expect_identical(f$code[1:2], c("P", "P"))
  # a designated profile with no plough mark is natural
  f <- .ru_id(.ru_P(.ru_AY(0, 20, designation = "Ah"), .ru_L(20, 90, designation = "C"), site = list(land_use = NA)))
  expect_identical(f$code[1], "AY")
})

test_that("organic horizons: H, O, T, TJ, TO, TE, TT, AH, AO and the feature ao", {
  skip_on_cran()
  .ru_expect(.ru_P(.ru_org(0, 20, peat_decomposition_pct = 60, munsell_value_dry = 2, munsell_chroma_dry = 1),
                   .ru_L(20, 80)), 1, "H")
  # 41a: 10 cm of peat or less (the key's inequality)
  .ru_expect(.ru_P(.ru_org(0, 10, peat_decomposition_pct = 20), .ru_AY(10, 20), .ru_L(20, 80)), 1, "O")
  .ru_expect(.ru_P(.ru_org(0, 30, peat_moor_type = "low-moor"), .ru_L(30, 80)), 1, "T")
  .ru_expect(.ru_P(.ru_org(0, 30, peat_moor_type = "mesophilic"), .ru_L(30, 80)), 1, "TJ")
  f <- .ru_id(.ru_P(.ru_org(0, 30, peat_moor_type = "high-moor"), .ru_org(30, 60, peat_moor_type = "high-moor"),
                    .ru_org(60, 120, peat_moor_type = "high-moor"), .ru_L(120, 150)))
  # TO in the upper 50 cm of the peat (Q22), TT below
  expect_identical(f$code[1:3], c("TO", "TO", "TT"))
  f <- .ru_id(.ru_P(.ru_org(0, 40, peat_moor_type = "low-moor"), .ru_org(40, 90, peat_moor_type = "transitional"),
                    .ru_org(90, 120, peat_moor_type = "mesophilic"), .ru_L(120, 150)))
  expect_identical(f$code, c("TE", "TE", "TJ", NA))
  # botany not recorded: T or TJ undecided
  f <- .ru_id(.ru_P(.ru_org(0, 30), .ru_L(30, 80)))
  expect_true(is.na(f$passed[1]))
  expect_setequal(f$candidates[[1]], c("T", "TJ"))
  expect_true("peat_moor_type" %in% f$missing[[1]])
  .ru_expect(.ru_P(.ru_L(0, 20, plant_residues_pct = 20, oc_pct = 10, munsell_value_dry = 2, munsell_chroma_dry = 1,
                         structure_type = "crumb"), .ru_L(20, 60)), 1, "AH")
  .ru_expect(.ru_P(.ru_L(0, 15, plant_residues_pct = 20, oc_pct = 12, munsell_hue_dry = "7.5YR", munsell_value_dry = 3,
                         munsell_chroma_dry = 3), .ru_L(15, 60)), 1, "AO")
  f <- .ru_id(.ru_P(.ru_org(0, 5, peat_decomposition_pct = 20),
                    .ru_L(5, 11, plant_residues_pct = 20, oc_pct = 12, munsell_hue_dry = "7.5YR", munsell_value_dry = 3,
                          munsell_chroma_dry = 3), .ru_L(11, 60)))
  expect_true(is.na(f$code[2]))
  expect_false(f$passed[2])
  expect_true(f$small[2])
  expect_identical(f$features[[2]], "ao")
})

test_that("stratified horizons of a deposit: RU, RY, RJ, the features rh and r, a thick raw deposit", {
  skip_on_cran()
  C <- .ru_L(70, 100, designation = "C")
  .ru_expect(.ru_P(.ru_dep(0, 50, munsell_value_dry = 3, munsell_chroma_dry = 1, oc_pct = 3), .ru_Ab(50, 70), C), 1, "RU")
  .ru_expect(.ru_P(.ru_dep(0, 50, munsell_value_dry = 5, munsell_chroma_dry = 2, ph_h2o = 5.5), .ru_Ab(50, 70), C), 1, "RY")
  .ru_expect(.ru_P(.ru_dep(0, 50, munsell_value_dry = 6, munsell_chroma_dry = 2, ph_h2o = 7.8), .ru_Ab(50, 70), C), 1, "RJ")
  # exactly 40 cm: a horizon when humified (35b), a feature when not (38a)
  .ru_expect(.ru_P(.ru_dep(0, 40, munsell_value_dry = 3, munsell_chroma_dry = 1), .ru_Ab(40, 70), C), 1, "RU")
  f <- .ru_id(.ru_P(.ru_dep(0, 40), .ru_Ab(40, 70), C))
  expect_true(is.na(f$code[1]) && f$small[1])
  expect_identical(f$features[[1]], "r")
  f <- .ru_id(.ru_P(.ru_dep(0, 20, munsell_value_dry = 5, munsell_chroma_dry = 2), .ru_Ab(20, 40), C))
  expect_identical(f$features[[1]], "rh")
  expect_true(f$small[1])
  expect_identical(f$buried[2], 1L)
  # over 40 cm and not humified: a technogenic formation or a natural deposit
  f <- .ru_id(.ru_P(.ru_dep(0, 60), .ru_Ab(60, 80), C))
  expect_true(is.na(f$code[1]))
  expect_false(f$passed[1])
  expect_match(f$key_path[1], "38\u0431: _deposit", fixed = TRUE)
  # a humus horizon formed in alluvium with no buried soil is no deposit (Q21)
  f <- .ru_id(.ru_P(.ru_dep(0, 20, munsell_value_dry = 5, munsell_chroma_dry = 2, structure_type = "granular"),
                    .ru_dep(20, 100)))
  expect_identical(f$code[1], "AY")
})


# --- eluvial horizons --------------------------------------------------------------------

test_that("eluvial horizons: E and e, EL and el, AEL, BEL, SEL", {
  skip_on_cran()
  f <- .ru_id(.ru_podzol())
  expect_identical(f$code[2], "E")
  expect_identical(f$route[2], "data")
  # 50a: 2 cm or less is the small horizon e (the key's inequality)
  f <- .ru_id(.ru_podzol(8, 10))
  expect_true(is.na(f$code[2]) && f$small[2])
  expect_identical(f$features[[2]], "e")
  .ru_expect(.ru_elp(15, 35), 2, "EL")
  f <- .ru_id(.ru_elp(15, 20))
  expect_true(is.na(f$code[2]) && f$small[2])
  expect_identical(f$features[[2]], "el")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 30, munsell_value_dry = 6, munsell_chroma_dry = 2, structure_type = "crumb",
                                   leached_spots = TRUE, clay_pct = 15),
                   .ru_B(30, 90, clay_films_amount = "common", structure_type = "prismatic"), .ru_L(90, 150)), 2, "AEL")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 30, munsell_value_dry = 6, munsell_chroma_dry = 3, structure_type = "blocky",
                                   structure_grade = "weak", variegated_colors = "10YR 7/2; 7.5YR 5/6"),
                   .ru_B(30, 90, clay_films_amount = "common", structure_type = "prismatic", clay_pct = 32), .ru_L(90, 150)), 2, "BEL")
  # 51a: a light horizon lighter than the solonetz horizon under it
  f <- .ru_expect(.ru_P(.ru_AY(0, 10, munsell_value_dry = 4, munsell_chroma_dry = 2, ph_h2o = 7.2),
                        .ru_L(10, 20, munsell_value_dry = 7, munsell_chroma_dry = 2, structure_type = "platy", clay_pct = 10,
                              silt_pct = 60, sand_pct = 30, ph_h2o = 7.5),
                        .ru_L(20, 50, munsell_value_dry = 4, munsell_chroma_dry = 3, structure_type = "columnar", clay_pct = 40,
                              clay_films_amount = "many", ph_h2o = 8.6),
                        .ru_L(50, 100)), 2, "SEL")
  expect_match(f$key_path[2], "51\u0430: SEL", fixed = TRUE)
})


# --- middle horizons -------------------------------------------------------------------------

test_that("horizons with iron films: BHF (BH, BF, plain), BFM, F, BAN", {
  skip_on_cran()
  f <- .ru_id(.ru_podzol())
  expect_identical(f$code[3:4], c("BHF", "BHF"))
  expect_identical(f$modification[3:4], c("BH", "BF"))
  # a colour neither coffee nor ochre: 57c, BHF with no modification
  p <- .ru_podzol()
  p$horizons$munsell_value_dry[4] <- 6; p$horizons$munsell_chroma_dry[4] <- 4
  f <- .ru_id(p)
  expect_identical(f$code[4], "BHF")
  expect_true(is.na(f$modification[4]))
  f <- .ru_id(.ru_P(.ru_AY(), .ru_sand(15, 40, munsell_hue_dry = "5YR", munsell_value_dry = 5, munsell_chroma_dry = 8,
                                       uncoated_grains_pct = 40, structure_type = "granular", structure_grade = "weak"),
                    .ru_sand(40, 70, munsell_hue_dry = "5YR", munsell_value_dry = 5, munsell_chroma_dry = 8, uncoated_grains_pct = 40),
                    .ru_sand(70, 100, uncoated_grains_pct = 95)))
  expect_identical(f$code[2:3], c("BFM", "BFM"))
  .ru_expect(.ru_P(.ru_AY(), .ru_sand(15, 30, uncoated_grains_pct = 20, cementation_class = "strongly", pan_type = "ironpan",
                                      munsell_value_dry = 4, munsell_chroma_dry = 6), .ru_L(30, 80)), 2, "F")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 50, layer_origin = "tephric", uncoated_grains_pct = 40, structure_type = "granular",
                                   munsell_hue_dry = "7.5YR", munsell_value_dry = 6, munsell_chroma_dry = 8), .ru_L(50, 100)), 2, "BAN")
})

test_that("horizons with cutans: BT, BI, ASN, BSN, CAT; the gap of position 60", {
  skip_on_cran()
  f <- .ru_expect(.ru_elp(), 3, "BT")
  expect_match(f$evidence[3], "clay ratio 3", fixed = TRUE)
  .ru_expect(.ru_P(.ru_AY(clay_pct = 22), .ru_B(15, 70, clay_pct = 26, clay_films_amount = "common", ph_h2o = 6.8),
                   .ru_L(70, 120)), 2, "BI")
  f <- .ru_expect(.ru_P(.ru_AY(0, 10, munsell_value_dry = 6, ph_h2o = 7.4, structure_type = "crumb"),
                        .ru_L(10, 40, munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1,
                              munsell_hue_dry = NA, munsell_value_dry = NA, munsell_chroma_dry = NA, structure_type = "columnar",
                              clay_pct = 40, clay_films_amount = "many"),
                        .ru_L(40, 100)), 2, "ASN")
  expect_match(f$key_path[2], "58\u0430 > 59\u0430: ASN", fixed = TRUE)
  .ru_expect(.ru_P(.ru_AY(0, 10, munsell_value_dry = 6, ph_h2o = 7.4, structure_type = "crumb"),
                   .ru_L(10, 40, munsell_value_dry = 5, munsell_chroma_dry = 4, structure_type = "columnar", clay_pct = 40,
                         clay_films_amount = "many", caco3_pct = 2), .ru_L(40, 100)), 2, "BSN")
  # 60a: continuous effervescence with white-eye
  f <- .ru_expect(.ru_chz(.ru_B(40, 90, munsell_hue_dry = "10YR", munsell_chroma_dry = 4, structure_type = "prismatic",
                                caco3_pct = 12, secondary_carbonates_form = "soft concretions", clay_films_amount = "common")), 2, "CAT")
  expect_match(f$key_path[2], "60\u0430: CAT", fixed = TRUE)
  # 63a: thin cutans, white-eye, darker than the parent
  p <- .ru_chz(.ru_B(40, 90, munsell_hue_dry = "10YR", munsell_chroma_dry = 4, structure_type = "prismatic",
                     caco3_pct = 12, secondary_carbonates_form = "soft concretions", clay_films_amount = "few"))
  p$horizons$munsell_value_dry[3] <- 7
  f <- .ru_expect(p, 2, "CAT")
  expect_match(f$key_path[2], "63\u0430: CAT", fixed = TRUE)
  # continuous effervescence with cutans and no white-eye: the key has no exit
  f <- .ru_id(.ru_P(.ru_AY(0, 20), .ru_B(20, 60, clay_films_amount = "common", structure_type = "prismatic", caco3_pct = 5),
                    .ru_L(60, 100, caco3_pct = 8)))
  expect_true(is.na(f$code[2]) && is.na(f$passed[2]))
  expect_setequal(f$candidates[[2]], c("CAT", "BI", "BT"))
})

test_that("carbonate and structural horizons: BCA, ML, BMK, BM, the feature ca, no horizon", {
  skip_on_cran()
  .ru_expect(.ru_chz(.ru_L(40, 90, munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "prismatic",
                           structure_grade = "weak", caco3_pct = 15, secondary_carbonates_form = "pseudomycelia", ph_h2o = 8.2)),
             2, "BCA")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 50, munsell_value_dry = 8, munsell_chroma_dry = 1, caco3_pct = 75), .ru_L(50, 100)), 2, "ML")
  f <- .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 6, ph_h2o = 7.8, structure_type = "crumb", structure_grade = "weak"),
                        .ru_B(15, 45, munsell_hue_dry = "5YR", munsell_value_dry = 5, munsell_chroma_dry = 6, structure_type = "prismatic",
                              structure_size = "fine", structure_type_secondary = "blocky", caco3_pct = 3, ph_h2o = 8),
                        .ru_L(45, 100, caco3_pct = 8)), 2, "BMK")
  # 64b: the feature ca, then 65
  expect_identical(f$features[[2]], "ca")
  expect_match(f$key_path[2], "64\u0431 > 65\u0430: BMK/ca", fixed = TRUE)
  f <- .ru_expect(.ru_P(.ru_AY(), .ru_B(15, 50, munsell_hue_dry = "10YR", munsell_chroma_dry = 4), .ru_L(50, 100)), 2, "BM")
  expect_match(f$key_path[2], "33\u0430 > 65\u0431: BM", fixed = TRUE)
  # a calcareous parent material: the feature ca and no horizon
  f <- .ru_id(.ru_P(.ru_AY(), .ru_B(15, 40), .ru_L(40, 100, caco3_pct = 8)))
  expect_true(is.na(f$code[3]))
  expect_false(f$passed[3])
  expect_identical(f$features[[3]], "ca")
  expect_false(f$small[3])
  # 66: no group property
  f <- .ru_id(.ru_P(.ru_AY(), .ru_B(15, 40), .ru_L(40, 100)))
  expect_true(is.na(f$code[3]))
  expect_false(f$passed[3])
  expect_match(f$key_path[3], "66\u0430: _none", fixed = TRUE)
})

test_that("cryogenic horizons: CRM, CR, CRH, BPL", {
  skip_on_cran()
  fr <- list(frost_regime = "long-seasonal")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 4, munsell_chroma_dry = 2, structure_type = "lenticular",
                                   structure_grade = "moderate"), .ru_L(45, 100), site = fr), 2, "CRM")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 4, munsell_chroma_dry = 2), .ru_L(45, 100), site = fr), 2, "CR")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 2, munsell_chroma_dry = 1, structure_type = "granular"),
                   .ru_L(45, 100), site = fr), 2, "CRH")
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "blocky",
                                   structure_grade = "weak"),
                   .ru_L(45, 80, munsell_value_dry = 7, munsell_chroma_dry = 3, caco3_pct = 12,
                         secondary_carbonates_form = "soft concretions"),
                   .ru_L(80, 120, caco3_pct = 8), site = fr), 2, "BPL")
  # permafrost from the horizon data alone
  .ru_expect(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 4, munsell_chroma_dry = 2), .ru_L(45, 100, permafrost_temp_C = -1),
                   site = list(frost_regime = NA)), 2, "CR")
})

test_that("hydrogenic and turbated horizons: G, Q, TUR, the feature tr, the gap of position 7", {
  skip_on_cran()
  moist <- function(top, bottom, hue, v, c) .ru_L(top, bottom, munsell_hue_moist = hue, munsell_value_moist = v,
                                                  munsell_chroma_moist = c, munsell_hue_dry = NA, munsell_value_dry = NA,
                                                  munsell_chroma_dry = NA)
  .ru_expect(.ru_P(.ru_AY(), moist(15, 60, "5GY", 6, 1), .ru_L(60, 100)), 2, "G")
  .ru_expect(.ru_P(.ru_AY(), moist(15, 60, "5Y", 5, 3), .ru_L(60, 100)), 2, "Q")
  .ru_expect(.ru_P(.ru_AY(0, 20), .ru_L(20, 70, foreign_fragments_pct = 90, structure_type = "blocky"), .ru_L(70, 100)), 2, "TUR")
  f <- .ru_id(.ru_P(.ru_L(0, 30, foreign_fragments_pct = 90, munsell_value_dry = 5, munsell_chroma_dry = 2), .ru_B(30, 60),
                    .ru_L(60, 100)))
  expect_true(is.na(f$code[1]))
  expect_false(f$passed[1])
  expect_identical(f$features[[1]], "tr")
  # a mixed layer of 20 cm under a natural horizon: neither 7a nor 7b
  f <- .ru_id(.ru_P(.ru_AY(0, 20), .ru_B(20, 50), .ru_L(50, 70, foreign_fragments_pct = 90), .ru_L(70, 100)))
  expect_true(is.na(f$passed[3]))
  expect_setequal(f$candidates[[3]], c("TUR", "tr"))
  # the site's mixing depth stands in for the fragments, flagged
  f <- .ru_id(.ru_P(.ru_AY(0, 25), .ru_L(25, 50), .ru_L(50, 100), site = list(mechanical_mixing_depth_cm = 60)))
  expect_identical(f$code[1], "TUR")
  expect_true("mixing_from_site" %in% f$fallbacks[[1]])
})


# --- overlays, the feature u, the feature v ------------------------------------------------

test_that("overlays: S over a horizon or alone, SS, X, V over a horizon or alone", {
  skip_on_cran()
  f <- .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 6, ph_h2o = 8, soluble_salts_pct = 2), .ru_L(15, 100)), 1, "AJ")
  expect_identical(f$overlay[[1]], "S")
  expect_match(f$key_path[1], "3\u0431 > 5\u0431", fixed = TRUE)
  # exactly 1 % is not more than 1 %
  f <- .ru_id(.ru_P(.ru_AY(munsell_value_dry = 6, ph_h2o = 8, soluble_salts_pct = 1), .ru_L(15, 100)))
  expect_length(f$overlay[[1]], 0L)
  f <- .ru_expect(.ru_P(.ru_L(0, 10, soluble_salts_pct = 2), .ru_L(10, 100)), 1, "S")
  expect_length(f$overlay[[1]], 0L)
  .ru_expect(.ru_P(.ru_L(0, 10, soluble_salts_pct = 2, sulfide_odour = TRUE, munsell_value_dry = 3, munsell_chroma_dry = 1),
                   .ru_L(10, 100)), 1, "SS")
  f <- .ru_expect(.ru_P(.ru_AY(contamination_hazard = "extremely hazardous"), .ru_L(15, 100)), 1, "AY")
  expect_identical(f$overlay[[1]], "X")
  # contamination recorded without its category: the overlay is undecided,
  # the horizon is not
  f <- .ru_id(.ru_P(.ru_AY(contamination_type = "heavy metals"), .ru_L(15, 100)))
  expect_identical(f$code[1], "AY")
  expect_identical(f$overlay_na[[1]], "X")
  expect_true("contamination_hazard" %in% f$missing[[1]])
  # 23a: V over a structural horizon, and V alone
  f <- .ru_expect(.ru_P(.ru_AY(), .ru_B(15, 50, munsell_hue_dry = "10YR", munsell_chroma_dry = 4, slickensides = "many",
                                        structure_type = "wedge-shaped", clay_pct = 45), .ru_L(50, 100)), 2, "BM")
  expect_identical(f$overlay[[2]], "V")
  f <- .ru_expect(.ru_P(.ru_AY(), .ru_B(15, 50, munsell_hue_dry = "10YR", munsell_chroma_dry = 4, slickensides = "many",
                                        structure_type = "wedge-shaped", clay_pct = 45),
                        .ru_L(50, 100, structure_type = "blocky", structure_grade = "strong")), 2, "V")
  expect_length(f$overlay[[2]], 0L)
  # one or two vertic signs: the feature v
  f <- .ru_id(.ru_P(.ru_AY(), .ru_B(15, 50, slickensides = "many", structure_type = "wedge-shaped", clay_pct = 25), .ru_L(50, 100)))
  expect_true("v" %in% f$features[[2]])
})

test_that("the feature u of a profile dark from a dark rock", {
  skip_on_cran()
  dk <- function(top, bottom) .ru_L(top, bottom, munsell_value_dry = 3, munsell_chroma_dry = 1)
  f <- .ru_expect(.ru_P(.ru_AY(munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7), dk(15, 60), dk(60, 100),
                        site = list(parent_material = "carbonaceous shale")), 1, "AU")
  expect_identical(f$features[[1]], "u")
  expect_match(f$key_path[1], "13\u0430 > 14\u0430", fixed = TRUE)
  # the rock not named: u undecided, the horizon decided
  f <- .ru_id(.ru_P(.ru_AY(munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7), dk(15, 60), dk(60, 100)))
  expect_identical(f$code[1], "AU")
  expect_identical(f$features_na[[1]], "u")
})


# --- undecided questions -----------------------------------------------------------------

test_that("an undecided question follows every branch: one outcome decides, several give candidates", {
  skip_on_cran()
  # no designation and no land use: ploughing undecided (Q18)
  f <- .ru_id(.ru_P(.ru_AY(), .ru_B(15, 60), .ru_L(60, 120), site = list(land_use = NA)))
  expect_true(is.na(f$code[1]) && is.na(f$passed[1]))
  expect_setequal(f$candidates[[1]], c("P", "AY"))
  expect_true(all(c("designation", "land_use") %in% f$missing[[1]]))
  expect_match(f$key_path[1], "14\u0431 > 19\u0431: P", fixed = TRUE)
  expect_match(f$key_path[1], "14\u0430 > 15\u0431 > 17\u0430: AY", fixed = TRUE)
  # no frost record: a dull brown middle horizon may be cryometamorphic
  f <- .ru_id(.ru_P(.ru_AY(), .ru_L(15, 45, munsell_value_dry = 4, munsell_chroma_dry = 2, structure_type = "blocky",
                                   structure_grade = "moderate"),
                    .ru_L(45, 100), site = list(frost_regime = NA)))
  expect_true(is.na(f$passed[2]))
  expect_true(all(c("CRM", "BM") %in% f$candidates[[2]]))
  expect_true("frost_regime" %in% f$missing[[2]])
  # a coloured sandy layer with no film record and no spodic data
  f <- .ru_id(.ru_P(.ru_AY(), .ru_sand(15, 50, munsell_hue_dry = "7.5YR", munsell_value_dry = 5, munsell_chroma_dry = 6),
                    .ru_sand(50, 100)))
  expect_true(is.na(f$passed[2]))
  expect_true("BFM" %in% f$candidates[[2]])
  expect_true("uncoated_grains_pct" %in% f$missing[[2]])
  # a decided question with an undecided one off its path decides
  f <- .ru_id(.ru_P(.ru_AY(), .ru_B(15, 60), .ru_L(60, 120)))
  expect_identical(f$code[1], "AY")
  expect_length(f$missing[[1]], 0L)
})


# --- the notation and the hints ---------------------------------------------------------------

test_that("a Russian designation keeps its horizon; the data key goes to the evidence", {
  skip_on_cran()
  p <- .ru_P(.ru_AY(designation = "AY", munsell_value_dry = 3, munsell_chroma_dry = 1, ph_h2o = 7),
             .ru_L(15, 40, designation = "BM"), .ru_L(40, 100, designation = "C"))
  f <- .ru_id(p)
  expect_identical(f$code, c("AY", "BM", "C"))
  expect_identical(f$route, c("notation", "notation", "notation"))
  expect_identical(f$data_code[1], "AU")
  expect_match(f$evidence[1], "the key on the data reads AU", fixed = TRUE)
  expect_match(f$evidence[1], "the notation's AY is kept", fixed = TRUE)
  # the data agree: both routes
  p <- .ru_P(.ru_AY(designation = "AY"), .ru_B(15, 60, designation = "BM", munsell_hue_dry = "10YR", munsell_chroma_dry = 4),
             .ru_L(60, 100, designation = "C"))
  f <- .ru_id(p)
  expect_identical(f$route[1:2], c("both", "both"))
  # features, small horizons, overlays and lithological layers come from the notation
  p <- .ru_P(.ru_org(0, 5, designation = "O"), .ru_sand(5, 7, designation = "e"),
             .ru_sand(7, 40, designation = "BHFg", munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6),
             .ru_sand(40, 90, designation = "CG"))
  f <- .ru_id(p)
  expect_identical(f$code, c("O", NA, "BHF", "C"))
  expect_true(f$small[2])
  expect_identical(f$features[[2]], "e")
  expect_identical(f$features[[3]], "g")
  expect_identical(f$rockflag[4], "G")
})

test_that("other notations give hints only; litter is no horizon; hard rock is R", {
  skip_on_cran()
  f <- .ru_id(.ru_P(.ru_AY(designation = "Ah"), .ru_B(15, 60, designation = "Bt", clay_films_amount = "common"),
                    .ru_L(60, 100, designation = "C")))
  expect_identical(f$code[2], "BT")
  expect_identical(f$route[2], "data")
  expect_match(f$evidence[2], "the designation 'Bt' suggests BT, BI", fixed = TRUE)
  f <- .ru_id(.ru_P(.ru_L(0, 3, designation = "Oi"), .ru_AY(3, 20, designation = "Ah"), .ru_L(20, 40, designation = "R")))
  expect_true(is.na(f$code[1]))
  expect_false(f$passed[1])
  expect_match(f$evidence[1], "litter", fixed = TRUE)
  expect_identical(f$code[2], "AY")
  expect_identical(f$code[3], "R")
})


# --- fallbacks and stand-ins ------------------------------------------------------------------

test_that("the fallbacks are flagged: dry from moist, USDA texture, EC for salts, LOI from carbon, pH", {
  skip_on_cran()
  # only the moist colour: dry value = moist value + 1
  p <- .ru_P(.ru_L(0, 15, munsell_hue_dry = NA, munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_hue_moist = "10YR",
                   munsell_value_moist = 4, munsell_chroma_moist = 2, structure_type = "granular", ph_h2o = 5, oc_pct = 2),
             .ru_B(15, 60), .ru_L(60, 120))
  f <- .ru_id(p)
  expect_identical(f$code[1], "AY")
  expect_true("dry_from_moist" %in% f$fallbacks[[1]])
  expect_match(f$evidence[1], "dry colour inferred from the moist one", fixed = TRUE)
  # the USDA classes stand for the Kachinsky gradations; the Kachinsky fields win
  f <- .ru_id(.ru_elp())
  expect_identical(f$code[2], "EL")
  expect_true("usda_texture" %in% f$fallbacks[[2]])
  p <- .ru_elp()
  p$horizons$phys_clay_pct <- c(30, 20, 45, 35)
  f <- .ru_id(p)
  expect_identical(f$code[2], "EL")
  expect_false("usda_texture" %in% f$fallbacks[[2]])
  # same texture by Kachinsky: E, not EL
  p$horizons$phys_clay_pct[2] <- 45
  expect_identical(.ru_id(p)$code[2], "E")
  # the EC of chapter 9 for the salts
  f <- .ru_id(.ru_P(.ru_L(0, 10, ec_dS_m = 20, munsell_value_dry = 6, munsell_chroma_dry = 2, ph_h2o = 8, structure_type = "crumb"),
                    .ru_L(10, 100)))
  expect_identical(f$overlay[[1]], "S")
  expect_true("ec_salts" %in% f$fallbacks[[1]])
  # a salt crust with no measurement (the field signs of position 2)
  f <- .ru_id(.ru_P(.ru_L(0, 10, munsell_value_dry = 6, munsell_chroma_dry = 2, ph_h2o = 8, structure_type = "crumb"),
                    .ru_L(10, 100), site = list(surface_condition = "salt crust")))
  expect_identical(f$overlay[[1]], "S")
  expect_true("salt_signs" %in% f$fallbacks[[1]])
  # LOI from organic carbon
  f <- .ru_id(.ru_P(.ru_L(0, 20, plant_residues_pct = NA, oc_pct = 25, munsell_value_dry = 2, munsell_chroma_dry = 1,
                          peat_decomposition_pct = 60), .ru_L(20, 60)))
  expect_identical(f$code[1], "H")
  expect_true(all(c("loi_from_oc", "residues_from_om") %in% f$fallbacks[[1]]))
  # pH(KCl) only, and no effervescence in an acid layer
  f <- .ru_id(.ru_P(.ru_AY(ph_h2o = NA, ph_kcl = 4.2), .ru_B(15, 60), .ru_L(60, 120)))
  expect_identical(f$code[1], "AY")
  expect_true("ph_kcl" %in% f$fallbacks[[1]])
  f <- .ru_id(.ru_P(.ru_AY(effervescence_class = NA), .ru_B(15, 60, effervescence_class = NA, ph_h2o = 5.5), .ru_L(60, 120)))
  expect_true("effervescence_from_ph" %in% f$fallbacks[[2]])
})

test_that("soilKey's WRB diagnostics stand in for missing field descriptors, flagged", {
  skip_on_cran()
  # argic() for the cutans of BT
  p <- .ru_elp()
  p$horizons$clay_films_amount[3] <- NA
  f <- .ru_id(p)
  expect_identical(f$code[3], "BT")
  expect_identical(f$stand_ins[[3]], "argic")
  expect_match(f$evidence[3], "argic()", fixed = TRUE)
  # calcic() for the carbonate neoformations of BCA
  f <- .ru_id(.ru_chz(.ru_L(40, 90, munsell_value_dry = 7, munsell_chroma_dry = 3, structure_type = "prismatic",
                            structure_grade = "weak", caco3_pct = 25, ph_h2o = 8.2, secondary_carbonates_form = NA)))
  expect_identical(f$code[2], "BCA")
  expect_identical(f$stand_ins[[2]], "calcic")
  # spodic() for the iron films of BHF
  p <- .ru_P(.ru_org(0, 8, peat_decomposition_pct = 20),
             .ru_sand(8, 20, munsell_value_dry = 7, munsell_chroma_dry = 1, structure_type = "single grain", ph_h2o = 4.5,
                      oc_pct = 0.3, al_ox_pct = 0.05, fe_ox_pct = 0.05, munsell_hue_moist = "10YR", munsell_value_moist = 6,
                      munsell_chroma_moist = 1),
             .ru_sand(20, 50, munsell_hue_dry = "7.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6, ph_h2o = 4.8, oc_pct = 1.5,
                      al_ox_pct = 0.6, fe_ox_pct = 0.8),
             .ru_sand(50, 120, uncoated_grains_pct = 95, ph_h2o = 5.2, al_ox_pct = 0.1, fe_ox_pct = 0.1))
  f <- .ru_id(p)
  expect_identical(f$code[3], "BHF")
  expect_identical(f$modification[3], "BH")
  expect_identical(f$stand_ins[[3]], "spodic")
  # the structure criterion of natric_horizon() for the solonetz structure
  p <- .ru_P(.ru_AY(0, 10, munsell_value_dry = 6, ph_h2o = 7.4, structure_type = "crumb"),
             .ru_L(10, 25, munsell_value_dry = 5, munsell_chroma_dry = 4, structure_type = NA, structure_grade = NA, clay_pct = 40,
                   clay_films_amount = "many", na_cmol = 6, cec_cmol = 20, ph_h2o = 8.5),
             .ru_L(25, 50, munsell_value_dry = 5, munsell_chroma_dry = 4, structure_type = "columnar", clay_pct = 42,
                   clay_films_amount = "many", na_cmol = 6, cec_cmol = 20, ph_h2o = 8.6),
             .ru_L(50, 100, clay_pct = 30))
  f <- .ru_id(p)
  expect_identical(f$code[2], "BSN")
  expect_identical(f$stand_ins[[2]], "natric_horizon")
  # the sodium alone never decides: without the structure anywhere, undecided
  p$horizons$structure_type[3] <- NA
  f <- .ru_id(p)
  expect_true(is.na(f$passed[2]))
  expect_true("structure_type" %in% f$missing[[2]])
})


# --- the frame ---------------------------------------------------------------------------

test_that("the frame is the one identify_horizons() gives and the matcher reads", {
  skip_on_cran()
  f <- .ru_id(.ru_elp())
  expect_identical(nrow(f), 4L)
  expect_true(all(c("horizon_idx", "code", "overlay", "features", "features_na", "small", "transitional", "buried",
                    "route", "passed", "evidence", "missing", "candidates", "modification", "overlay_na", "data_code",
                    "key_path", "stand_ins", "fallbacks") %in% names(f)))
  expect_identical(f$code, c("AY", "EL", "BT", NA))
  tab <- .as_horizon_table(f, keep_extra = TRUE)
  expect_identical(tab$code, f$code)
  expect_true(isTRUE(match_horizon_sequence(f, "AY-EL-BT-C")$passed))
  # no horizons: an empty frame
  p <- PedonRecord$new(site = list(), horizons = data.frame(top_cm = numeric(0), bottom_cm = numeric(0)))
  expect_identical(nrow(.ru_id(p)), 0L)
  expect_error(.kidpr_identify(list()), "PedonRecord")
})
