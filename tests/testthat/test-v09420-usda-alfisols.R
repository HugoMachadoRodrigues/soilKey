# v0.9.420 (Hugo, 2026-11-20): the Alfisols, Ultisols and Oxisols after the
# Soil Taxonomy audit (section 2.4, items U1-U3, O1-O5, G1-G5; readings
# L18-L26).
#   * the argillic horizon is the KST one (pp. 20-21): no texture test, KST
#     thresholds, no clay increase under a plow layer or across a lithologic
#     discontinuity, illuviation evidence in the horizon itself, undecided
#     without any (L22);
#   * the kandic horizon mixes the upper 18 cm and asks for an abrupt textural
#     change for its 5 cm alternative (L23); the oxic horizon is the KST one
#     (p. 28) with criterion 5 and the weatherable minerals read as met (L21);
#   * the Ultisol order reads the base saturation at the KST depth (p. 69),
#     NH4OAc standing in for the sum of cations (L18), the deepest horizon of a
#     short profile as an inference with grade C (L19); no inference from Al
#     saturation or pH; the Alfisol order has no base-saturation test;
#   * Pale-, Kandi-, Kanhapl-, Plinth-, Rhod-, Ferr- great groups, Ultic
#     Alfisols, Rhodic, Humic and Acric Oxisols follow the KST.

.v420_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "v420", country = "XX"), site),
                  horizons = ensure_horizon_schema(hz))
}

# A loamy profile with an argillic-like clay increase at 30 cm.
.v420_alf <- function(films = c(NA, NA, "common", "common"),
                      desg = c("A", "E", "B1", "B2"),
                      clay = c(12, 11, 20, 22), ...) {
  .v420_pedon(top_cm = c(0, 15, 30, 70), bottom_cm = c(15, 30, 70, 160),
              designation = desg, clay_pct = clay, silt_pct = 30,
              sand_pct = 100 - 30 - clay, clay_films_amount = films,
              munsell_hue_moist = "10YR", munsell_value_moist = 4,
              munsell_chroma_moist = 4, bs_pct = 60, cec_cmol = 10, ...)
}

# ---- Argillic horizon ------------------------------------------------------

test_that("the argillic horizon uses the KST thresholds and no texture test", {
  skip_on_cran()
  # 11 -> 20 % clay (+9): the KST +3 rule; the WRB thresholds asked for +6
  # only with no films, and its texture test is gone
  r <- soilKey:::argillic_usda(.v420_alf())
  expect_true(r$passed)
  expect_equal(sort(r$layers), c(3L, 4L))
  # +2 % only: no clay increase
  r <- soilKey:::argillic_usda(.v420_alf(clay = c(12, 11, 13, 13)))
  expect_false(r$passed)
  # a sandy argillic (loamy sand) must be 15 cm thick, and it is here
  s <- .v420_pedon(top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 100),
                   designation = c("A", "Bt1", "Bt2"),
                   clay_pct = c(2, 8, 8), silt_pct = 5, sand_pct = c(93, 87, 87))
  expect_true(soilKey:::argillic_usda(s)$passed)
})

test_that("the increase must be reached within 30 cm of the eluvial horizon", {
  skip_on_cran()
  # the A (10 %) lies 40 cm above the Bt (13 %); the AB within 30 cm has 11 %
  p <- .v420_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                   designation = c("A", "AB", "Bt"),
                   clay_pct = c(10, 11, 13), silt_pct = 30, sand_pct = c(60, 59, 57),
                   clay_films_amount = c(NA, NA, "common"))
  expect_false(soilKey:::argillic_usda(p)$passed)
  # with the A within 30 cm the +3 % is met
  p$horizons$top_cm <- c(0, 20, 40); p$horizons$bottom_cm <- c(20, 40, 120)
  expect_true(soilKey:::argillic_usda(p)$passed)
})

test_that("no clay increase is needed under a plow layer or a discontinuity", {
  skip_on_cran()
  # Ap 20 % over Bt 21 %: no KST increase, but the plow layer waives item 2
  p <- .v420_pedon(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                   designation = c("Ap", "Bt1", "Bt2"),
                   clay_pct = c(20, 21, 21), silt_pct = 40, sand_pct = c(40, 39, 39),
                   clay_films_amount = c(NA, "common", "few"))
  r <- soilKey:::argillic_usda(p)
  expect_true(r$passed)
  expect_match(r$evidence$waiver, "plow layer")
  # A over 2Bt (a lithologic discontinuity)
  q <- .v420_pedon(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                   designation = c("A", "2Bt1", "2Bt2"),
                   clay_pct = c(20, 21, 21), silt_pct = 40, sand_pct = c(40, 39, 39),
                   clay_films_amount = c(NA, "few", "few"))
  expect_match(soilKey:::argillic_usda(q)$evidence$waiver, "discontinuity")
  # IIBt (Roman numerals) too
  q$horizons$designation <- c("A", "IIBt1", "IIBt2")
  expect_true(soilKey:::argillic_usda(q)$passed)
})

test_that("illuviation must be in the horizon itself; none recorded is undecided", {
  skip_on_cran()
  # clay films only in the A: the B horizons show none (L22)
  r <- soilKey:::argillic_usda(.v420_alf(films = c("few", NA, NA, NA)))
  expect_true(is.na(r$passed))
  expect_true("clay_films_amount" %in% r$missing)
  # a t designation is evidence
  r <- soilKey:::argillic_usda(.v420_alf(films = NA, desg = c("A", "E", "Bt1", "Bt2")))
  expect_true(r$passed)
  # films, thin sections and the fine-clay ratio recorded as absent decide FALSE
  p <- .v420_alf(films = c(NA, NA, "none", "none"))
  p$horizons$clay_coatings_thin_section_pct <- 0
  p$horizons$cole_value <- 0.01
  expect_false(soilKey:::argillic_usda(p)$passed)
  # reading N5: clay films recorded as none in a B not designated t decide
  # FALSE on their own; a t designation or thin-section argillans still count
  expect_false(soilKey:::argillic_usda(.v420_alf(films = c(NA, NA, "none", "none")))$passed)
  p <- .v420_alf(films = c(NA, NA, "none", "none"))
  p$horizons$clay_coatings_thin_section_pct <- c(NA, NA, 3, 3)
  expect_true(soilKey:::argillic_usda(p)$passed)
})

# ---- Kandic horizon ----------------------------------------------------------

.v420_kandic <- function(a_bottom = 10, bd = "abrupt", a_clay = 10) {
  .v420_pedon(top_cm = c(0, a_bottom, 60), bottom_cm = c(a_bottom, 60, 160),
              designation = c("A", "Bt1", "Bt2"),
              clay_pct = c(a_clay, 30, 32), silt_pct = 20,
              sand_pct = c(90 - a_clay, 50, 48),
              cec_cmol = c(5, 3, 3), oc_pct = c(2, 0.5, 0.3),
              boundary_distinctness = c(bd, "gradual", NA))
}

test_that("the kandic horizon mixes the upper 18 cm (L23)", {
  skip_on_cran()
  # a 10 cm A with 10 % clay over 30 %: mixed 0-18 cm is 18.9 %, and 30 is
  # more than 1.2 times that
  r <- soilKey:::kandic_horizon_usda(.v420_kandic(a_bottom = 10, bd = "gradual"))
  expect_true(r$passed)
  expect_true(isTRUE(r$evidence$after_mixing))
  expect_equal(r$evidence$upper_boundary_cm, 18)
})

test_that("the 5 cm surface layer needs an abrupt textural change", {
  skip_on_cran()
  # A 8 cm, 10 % clay -> 30 %: the clay triples across an abrupt boundary
  r <- soilKey:::kandic_horizon_usda(.v420_kandic(a_bottom = 8, bd = "abrupt"))
  expect_true(r$passed)
  expect_identical(r$evidence$surface_horizon, "abrupt textural change")
  # 22 % -> 30 %: an abrupt boundary but no abrupt textural change; the mixed
  # 0-18 cm (26.4 %) is too close to 30 % (1.1 times)
  r <- soilKey:::kandic_horizon_usda(.v420_kandic(a_bottom = 8, bd = "abrupt", a_clay = 22))
  expect_false(isTRUE(r$passed))
})

# ---- Oxic horizon ------------------------------------------------------------

.v420_oxic <- function(bd = "diffuse", b_clay = 45, ...) {
  .v420_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 160),
              designation = c("A", "Bo1", "Bo2"),
              clay_pct = c(35, b_clay, 46), silt_pct = 15,
              sand_pct = c(50, 85 - b_clay, 39),
              cec_cmol = c(9, 5, 5), oc_pct = c(2, 0.6, 0.3),
              boundary_distinctness = c(bd, "diffuse", NA), ...)
}

test_that("the oxic horizon reads KST criterion 5 and is a subsurface horizon", {
  skip_on_cran()
  # 35 -> 45 %: an increase of 1.29 times at the upper boundary; with a
  # diffuse boundary it is spread over 15 cm or more
  r <- soilKey:::oxic_usda(.v420_oxic())
  expect_true(r$passed)
  expect_match(r$evidence$weatherable_minerals, "L21")
  # a clear boundary: the increase is not oxic; the Bo2 alone is 100 cm thick
  # and rises 45 -> 46 from the Bo1, so it is oxic from 60 cm
  r <- soilKey:::oxic_usda(.v420_oxic(bd = "clear"))
  expect_true(r$passed)
  expect_equal(r$evidence$upper_boundary_cm, 60)
  # boundary not recorded: undecided at 20 cm, decided at 60 cm
  r <- soilKey:::oxic_usda(.v420_oxic(bd = NA))
  expect_true(r$passed)
  # weatherable minerals recorded at 10 % or more: not oxic
  p <- .v420_oxic()
  p$horizons$weatherable_minerals_pct <- 12
  expect_false(soilKey:::oxic_usda(p)$passed)
})

test_that("apparent clay from 1500 kPa water only where measured by mass (L25)", {
  skip_on_cran()
  p <- .v420_oxic()
  p$horizons$cec_cmol <- c(9, 9, 9)               # 9 / 45 = 20 per kg clay
  p$horizons$water_content_1500kpa <- c(20, 20, 20)
  # 3 x (20 - 0.6) = 58 % apparent clay: 9 / 58 = 15.5 -> 16 or less
  expect_true(soilKey:::oxic_usda(p)$passed)
  # the same water converted from a volume is not used
  p$add_measurement(horizon_idx = 2L, attribute = "water_content_1500kpa",
                    value = 20, source = "measured",
                    notes = "ISIS wv1500 (cm3/100cm3) / bulk density", overwrite = TRUE)
  expect_false(soilKey:::.w1500_by_mass(p)[2])
})

# ---- Orders -----------------------------------------------------------------

.v420_ult <- function(bs = c(30, 25, 20, 15), bottom = 200, ...) {
  .v420_pedon(top_cm = c(0, 15, 40, 100), bottom_cm = c(15, 40, 100, bottom),
              designation = c("A", "E", "Bt1", "Bt2"),
              clay_pct = c(12, 10, 30, 32), silt_pct = 30, sand_pct = c(58, 60, 40, 38),
              clay_films_amount = c(NA, NA, "common", "few"),
              cec_cmol = 20, bs_pct = bs, ...)
}

test_that("Ultisols read the base saturation at the KST depth (U2, L18)", {
  skip_on_cran()
  # argillic from 40 cm: 125 cm below it is 165 cm, shallower than 180
  u <- soilKey:::ultisol_usda(.v420_ult(bs = c(60, 60, 60, 30)))
  expect_true(u$passed)
  expect_equal(u$evidence$bs_depth_cm, 165)
  expect_identical(u$evidence$bs_source, "NH4OAc")
  expect_match(u$evidence$note, "L18")
  # low in the argillic, 50 % at 165 cm: not an Ultisol (the mean over the
  # argillic said yes until v0.9.419)
  u <- soilKey:::ultisol_usda(.v420_ult(bs = c(20, 20, 20, 50)))
  expect_false(u$passed)
  # the sum of cations, where the acidity at pH 8.2 is recorded
  p <- .v420_ult(bs = c(60, 60, 60, 40))
  p$horizons$ca_cmol <- 5; p$horizons$mg_cmol <- 2; p$horizons$k_cmol <- 0.5
  p$horizons$na_cmol <- 0.5; p$horizons$extractable_acidity_ph82_cmol <- 16
  u <- soilKey:::ultisol_usda(p)                   # 8 / 24 = 33 %
  expect_true(u$passed)
  expect_identical(u$evidence$bs_source, "sum of cations")
})

test_that("no base saturation is inferred from Al saturation or pH (G5)", {
  skip_on_cran()
  p <- .v420_ult(bs = NA_real_, al_sat_pct = 80, ph_h2o = 4.5)
  u <- soilKey:::ultisol_usda(p)
  expect_true(is.na(u$passed))
  expect_true("bs_pct" %in% u$missing)
})

test_that("a profile ending above the depth uses its deepest horizon, grade C (L19)", {
  skip_on_cran()
  p <- .v420_ult(bs = c(60, 50, 30, 20), bottom = 140)
  p$site$soil_moisture_regime <- "udic"
  p$site$soil_temperature_regime <- "thermic"
  u <- soilKey:::ultisol_usda(p)
  expect_true(u$passed)
  expect_match(u$evidence$inference, "L19")
  r <- suppressWarnings(classify_usda(p))
  expect_identical(r$rsg_or_order, "Ultisols")
  expect_identical(r$evidence_grade, "C")
  expect_true(any(grepl("L19", r$warnings)))
})

test_that("the Alfisol order has no base-saturation test (U3)", {
  skip_on_cran()
  p <- .v420_ult(bs = c(60, 60, 60, 60))
  a <- soilKey:::alfisol_usda(p)
  expect_true(a$passed)
  # a kandic horizon alone also makes an Alfisol
  k <- .v420_kandic(a_bottom = 10, bd = "gradual")
  k$horizons$bs_pct <- 80
  expect_true(soilKey:::kandic_horizon_usda(k)$passed)
  expect_true(soilKey:::alfisol_usda(k)$passed)
})

test_that("the clay of the Oxisol order is rounded to the limit (L24)", {
  skip_on_cran()
  expect_identical(soilKey:::.kst_round(39.9), 40)
  expect_identical(soilKey:::.kst_round(34.5), 34)
  expect_identical(soilKey:::.kst_round(17.5), 18)
})

# ---- Great groups and subgroups ---------------------------------------------

test_that("the Pale- core reads the clay decrease below the maximum (G2, L20)", {
  skip_on_cran()
  # A 25 % over E 15 %: a decrease above the maximum does not count
  p <- .v420_pedon(top_cm = c(0, 15, 40, 100), bottom_cm = c(15, 40, 100, 200),
                   designation = c("A", "E", "Bt1", "Bt2"),
                   clay_pct = c(25, 15, 40, 38), silt_pct = 30, sand_pct = c(45, 55, 30, 32),
                   clay_films_amount = c(NA, NA, "many", "common"))
  expect_true(soilKey:::.pale_core_usda(p)$passed)
  # 40 -> 30 % below the maximum: a decrease of 25 %
  q <- p$clone(deep = TRUE); q$horizons$clay_pct[4] <- 30
  expect_false(soilKey:::.pale_core_usda(q)$passed)
  # skeletans of 5 % with a clay increase below waive it
  s <- .v420_pedon(top_cm = c(0, 20, 60, 90), bottom_cm = c(20, 60, 90, 200),
                   designation = c("A", "Bt1", "Bt2", "Bt3"),
                   clay_pct = c(15, 40, 30, 38), silt_pct = 30, sand_pct = c(55, 30, 40, 32),
                   skeletans_pct = c(NA, NA, 8, NA))
  expect_true(soilKey:::.pale_core_usda(s)$passed)
  # a profile that ends at 120 cm without a contact: undecided (L20)
  r <- p$clone(deep = TRUE); r$horizons$bottom_cm[4] <- 120
  expect_true(is.na(soilKey:::.pale_core_usda(r)$passed))
})

test_that("Paleustalfs need the lower-half colour of the KST", {
  skip_on_cran()
  p <- .v420_pedon(top_cm = c(0, 18, 40, 120), bottom_cm = c(18, 40, 120, 200),
                   designation = c("Ap", "Bt1", "Bt2", "Bt3"),
                   clay_pct = c(60, 70, 75, 76), silt_pct = 15, sand_pct = c(25, 15, 10, 9),
                   clay_films_amount = c(NA, "common", "many", "many"),
                   munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                   munsell_chroma_moist = 4, munsell_value_dry = 3)
  expect_false(soilKey:::pale_qualifying_usda(p, kst = "ustalfs")$passed)
  p$horizons$munsell_chroma_moist <- 6
  expect_true(soilKey:::pale_qualifying_usda(p, kst = "ustalfs")$passed)
  # the Mollisol reading (kst = NULL) is the earlier one
  expect_identical(soilKey:::pale_qualifying_usda(p)$name, "pale_qualifying_usda")
})

test_that("Rhod- great groups need the dry value; undecided without it (L26)", {
  skip_on_cran()
  p <- .v420_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
                   designation = c("A", "Bt1", "Bt2"),
                   clay_pct = c(20, 35, 36), silt_pct = 30, sand_pct = c(50, 35, 34),
                   clay_films_amount = c(NA, "common", "common"),
                   munsell_hue_moist = "10R", munsell_value_moist = 3,
                   munsell_chroma_moist = 4, munsell_value_dry = c(4, 4, 4))
  expect_true(soilKey:::rhodic_subgroup_usda(p, kst = "argillic")$passed)
  p$horizons$munsell_value_dry <- c(4, 5, 4)     # 2 units higher than moist
  expect_false(soilKey:::rhodic_subgroup_usda(p, kst = "argillic")$passed)
  p$horizons$munsell_value_dry <- NA_real_
  expect_true(is.na(soilKey:::rhodic_subgroup_usda(p, kst = "argillic")$passed))
})

test_that("Rhodic Oxisols need every horizon between 25 and 125 cm (O3)", {
  skip_on_cran()
  p <- .v420_oxic(munsell_hue_moist = c("2.5YR", "2.5YR", "5YR"),
                  munsell_value_moist = 3, munsell_chroma_moist = 6)
  expect_false(soilKey:::rhodic_subgroup_usda(p, kst = "oxisol")$passed)
  p$horizons$munsell_hue_moist <- "2.5YR"
  expect_true(soilKey:::rhodic_subgroup_usda(p, kst = "oxisol")$passed)
})

test_that("organic carbon mass has no default bulk density (O4)", {
  skip_on_cran()
  p <- .v420_oxic()
  p$horizons$oc_pct <- c(3, 1.5, 0.8)
  # without bulk density: up to 2.0 g/cm3 could reach 16 kg/m2 -> undecided
  expect_true(is.na(soilKey:::humic_oxisol_usda(p)$passed))
  p$horizons$bulk_density_g_cm3 <- 1.0            # 6 + 6 + 3.2 = 15.2
  expect_false(soilKey:::humic_oxisol_usda(p)$passed)
  p$horizons$bulk_density_g_cm3 <- 1.2            # 18.2
  expect_true(soilKey:::humic_oxisol_usda(p)$passed)
})

test_that("Acric great groups read the oxic or kandic subhorizons (O5)", {
  skip_on_cran()
  p <- .v420_oxic()
  p$horizons$ecec_cmol <- c(3, 0.5, 0.5)          # 0.5 / 45 -> 1.1 per kg clay
  p$horizons$ph_kcl <- c(4.5, 5.5, 5.6)
  expect_true(soilKey:::acric_oxisol_usda(p)$passed)
  p$horizons$ph_kcl <- c(4.5, 4.8, 4.9)
  expect_false(soilKey:::acric_oxisol_usda(p)$passed)
  # the Acrudoxic subgroups: ECEC 1.5 or less per kg clay within 150 cm
  expect_true(soilKey:::acric_oxisol_usda(p, subgroup = TRUE)$passed)
})

test_that("Ultic Alfisols need base saturation below 75 % throughout (G4)", {
  skip_on_cran()
  p <- .v420_ult(bs = c(80, 70, 60, 70))
  expect_true(soilKey:::ultic_subgroup_usda(p, alfisol = TRUE)$passed)
  p$horizons$bs_pct[4] <- 80
  expect_false(soilKey:::ultic_subgroup_usda(p, alfisol = TRUE)$passed)
  # the other orders keep the earlier reading
  expect_true(soilKey:::ultic_subgroup_usda(p)$passed)
})

test_that("Plinth- great groups need half the volume; Ferrudalfs need nodules (G3)", {
  skip_on_cran()
  p <- .v420_ult(plinthite_pct = c(NA, NA, 10, 10))
  expect_false(soilKey:::plinth_subgroup_usda(p, gg = TRUE)$passed)
  expect_true(soilKey:::plinth_subgroup_usda(p)$passed %in% c(TRUE, FALSE))
  p$horizons$plinthite_pct <- c(NA, NA, 10, 60)
  expect_true(soilKey:::plinth_subgroup_usda(p, gg = TRUE)$passed)
  # Fe-DCB alone no longer makes a Ferrudalf
  f <- .v420_ult(fe_dcb_pct = 6)
  expect_false(isTRUE(soilKey:::ferric_subgroup_usda(f)$passed))
})

test_that("Kanhapl- is reachable after Kandi-", {
  skip_on_cran()
  k <- .v420_kandic(a_bottom = 10, bd = "gradual")
  k$horizons$bottom_cm[3] <- 120                  # ends above 150 cm: Kandi- undecided
  expect_true(is.na(soilKey:::pale_qualifying_usda(k, kst = "ultisols")$passed))
  expect_true(soilKey:::kanhapl_qualifying_usda(k)$passed)
})
