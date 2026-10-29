# v0.9.313: organic material, the Histosols, the Andosols and Cryosols item 2
# read as WRB 2022 words them (Ch 3.3.13, Ch 3.1.15, Ch 3.2.3, Ch 4 pp 95, 98,
# 104), and the pH in NaF as a horizon attribute. Small made-up pedons only.

.v09313_pedon <- function(..., site = list(id = "v09313")) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(hz))
}

# ---- organic material: >= 20% soil organic carbon (Ch 3.3.13, p 91) ---------

test_that("the histic horizon reads organic material at 20% SOC", {
  skip_on_cran()
  peat <- function(oc) .v09313_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80),
                                     designation = c("H", "Cg"), oc_pct = c(oc, 0.5))
  expect_true(isTRUE(histic_horizon(peat(25))$passed))
  expect_false(isTRUE(histic_horizon(peat(15))$passed))       # 12% passed until v0.9.312
  expect_identical(histic_horizon(peat(15))$passed, FALSE)
  expect_true(is.na(histic_horizon(peat(NA))$passed))         # not recorded: undecided
  # the adjoining organic subhorizons are added up (5 + 7 cm)
  thin <- .v09313_pedon(top_cm = c(0, 5, 12), bottom_cm = c(5, 12, 60),
                        designation = c("Oe", "Oa", "Bw"), oc_pct = c(40, 30, 1))
  expect_true(isTRUE(histic_horizon(thin)$passed))
  expect_equal(histic_horizon(thin)$evidence$thickness_cm, 12)
  # the folic horizon too
  folic_peat <- function(oc) .v09313_pedon(top_cm = c(0, 15), bottom_cm = c(15, 50),
                                           designation = c("O", "C"), oc_pct = c(oc, 0.5),
                                           water_saturation_days = c(5, 5))
  expect_true(isTRUE(folic(folic_peat(30))$passed))
  expect_false(isTRUE(folic(folic_peat(15))$passed))
})

test_that("Floatic reads organic material at 20% SOC", {
  skip_on_cran()
  p <- function(oc) .v09313_pedon(top_cm = c(0, 60), bottom_cm = c(60, 100),
                                  designation = c("H", "C"), oc_pct = c(oc, 1),
                                  bulk_density_g_cm3 = c(0.08, 1.4))
  expect_true(isTRUE(soilKey:::qual_floatic(p(30))$passed))
  expect_false(isTRUE(soilKey:::qual_floatic(p(15))$passed))
})

# ---- Histosols key (Ch 4, p 95) ---------------------------------------------

test_that("Histosols item 1: >= 40 cm of organic material within 100 cm, starting <= 40 cm", {
  skip_on_cran()
  hs <- function(...) soilKey:::histosol(.v09313_pedon(...))$passed
  expect_true(isTRUE(hs(top_cm = c(0, 45), bottom_cm = c(45, 100),
                        designation = c("Ha", "Cg"), oc_pct = c(30, 0.5))))
  # 15% OC is not organic material (the old 12% gate passed this)
  expect_identical(hs(top_cm = c(0, 45), bottom_cm = c(45, 100),
                      designation = c("Ha", "Cg"), oc_pct = c(15, 0.5)), FALSE)
  # organic material starting > 40 cm
  expect_identical(hs(top_cm = c(0, 45), bottom_cm = c(45, 120),
                      designation = c("A", "Hb"), oc_pct = c(2, 30)), FALSE)
  # every organic layer within 100 cm counts, clipped at 100 cm: 30 + 20 cm
  expect_true(isTRUE(hs(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 150),
                        designation = c("A", "H1", "Cg", "H2"), oc_pct = c(3, 30, 1, 30))))
  # 30 + 5 cm within 100 cm is not enough
  expect_identical(hs(top_cm = c(0, 20, 50, 95), bottom_cm = c(20, 50, 95, 150),
                      designation = c("A", "H1", "Cg", "H2"), oc_pct = c(3, 30, 1, 30)), FALSE)
  # organic carbon not recorded in a layer that could make the 40 cm
  expect_true(is.na(hs(top_cm = c(0, 45), bottom_cm = c(45, 100),
                       designation = c("A", "C"), oc_pct = c(NA, 0.5))))
  # ... but not where it could not: 30 cm over mineral soil
  expect_identical(hs(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "C"), oc_pct = c(NA, 0.5)), FALSE)
})

test_that("Histosols item 2: >= 10 cm of organic material directly over rock or ice", {
  skip_on_cran()
  hs <- function(...) soilKey:::histosol(.v09313_pedon(...))$passed
  expect_true(isTRUE(hs(top_cm = c(0, 15), bottom_cm = c(15, 40),
                        designation = c("O", "R"), oc_pct = c(35, NA))))
  expect_true(isTRUE(hs(top_cm = c(0, 15), bottom_cm = c(15, 40),
                        designation = c("O", "I"), oc_pct = c(35, NA))))
  # a surface layer >= 10 cm over mineral soil is not a Histosol (the old gate
  # passed any surface layer >= 10 cm)
  p <- .v09313_pedon(top_cm = c(0, 15), bottom_cm = c(15, 80),
                     designation = c("O", "Bw"), oc_pct = c(35, 1.2))
  expect_identical(soilKey:::histosol(p)$passed, FALSE)
  expect_false(identical(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Histosols"))
  # thinner than 10 cm over rock
  expect_identical(hs(top_cm = c(0, 8), bottom_cm = c(8, 40),
                      designation = c("O", "R"), oc_pct = c(35, NA)), FALSE)
  # the canonical Histosol keys as before
  expect_identical(classify_wrb2022(make_histosol_canonical(), on_missing = "silent")$rsg_or_order,
                   "Histosols")
})

# ---- andic properties (Ch 3.2.3, p 73) --------------------------------------

test_that("andic properties are undecided, not absent, when the data are missing", {
  skip_on_cran()
  p <- .v09313_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Ah", "Bw"),
                     oc_pct = c(6, 2))
  expect_true(is.na(andic_properties(p)$passed))              # FALSE until v0.9.312
  expect_true(all(c("al_ox_pct", "bulk_density_g_cm3") %in% andic_properties(p)$missing))
  # measured and failing: Alox + 1/2 Feox < 2 everywhere
  q <- .v09313_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Ah", "Bw"),
                     al_ox_pct = c(0.4, 0.3), fe_ox_pct = c(0.6, 0.5))
  expect_identical(andic_properties(q)$passed, FALSE)
})

test_that("Alox + 1/2 Feox >= 2 with pH NaF >= 9.5 is andic where bulk density is not measured", {
  skip_on_cran()
  p <- function(naf, bd = NA_real_, caco3 = NA_real_)
    .v09313_pedon(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("Ah", "Bw"),
                  al_ox_pct = c(2.5, 2.2), fe_ox_pct = c(1.0, 0.8), ph_naf = c(naf, naf),
                  bulk_density_g_cm3 = c(bd, bd), caco3_pct = c(caco3, caco3))
  a <- andic_properties(p(10.2))
  expect_true(isTRUE(a$passed))
  expect_identical(a$layers, 1:2)
  expect_identical(a$evidence$naf_field_test$layers, 1:2)
  expect_true(isTRUE(a$evidence$naf_field_test$details$proxy))
  expect_match(a$notes, "proxy")
  expect_match(a$reference, "NaF")
  expect_true(is.na(andic_properties(p(9.0))$passed))         # a low NaF pH does not rule it out
  expect_identical(andic_properties(p(10.2, bd = 1.1))$passed, FALSE)  # measured bulk density decides
  expect_true(isTRUE(andic_properties(p(10.2, bd = 0.8))$passed))
  expect_true(is.na(andic_properties(p(10.2, caco3 = 5))$passed))     # the test needs carbonate-free soil
  # the USDA andic soil properties do not read it
  expect_false(isTRUE(soilKey:::andic_soil_properties_usda(p(10.2))$passed))
})

test_that("ph_naf is a registered horizon attribute and the ISIS reader reads phnf", {
  skip_on_cran()
  expect_identical(horizon_column_spec()[["ph_naf"]], "numeric")
  code <- function(item) list(item = item, description = NULL)
  pr <- list(profileCode = "XX313", countryCode = "XX", profileId = 1L,
             layers = list(
               list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 30L, layerName = "Ah",
                    phnf = list("10.4"), alExtox = list("2.4"), feExtacox = list("1.2"),
                    colourMoistHue = code("10YR")),
               list(layerNumber = 2L, upperDepth = 30L, lowerDepth = 70L, layerName = "Bw",
                    phnf = list("9.1", "9.3"), colourMoistHue = code("7.5YR"))))
  h <- as.data.frame(isis_profile_to_pedon(pr)$horizons)
  expect_equal(h$ph_naf, c(10.4, 9.2))
})

# ---- Andosols key (Ch 4, p 104) ---------------------------------------------

.v09313_andic <- function(top, bottom, desig, andic, ...) {
  .v09313_pedon(top_cm = top, bottom_cm = bottom, designation = desig,
                al_ox_pct = ifelse(andic, 2.5, 0.3), fe_ox_pct = ifelse(andic, 1, 0.4),
                bulk_density_g_cm3 = ifelse(andic, 0.7, 1.3), clay_pct = 20, silt_pct = 40,
                sand_pct = 40, oc_pct = ifelse(andic, 5, 1), volcanic_glass_pct = 0, ...)
}

test_that("Andosols item 1a adds up every andic layer within 100 cm", {
  skip_on_cran()
  # andic 0-20 and 30-60: 50 cm, the uppermost starting <= 25 cm. Until v0.9.312
  # only the layers starting <= 25 cm counted (20 cm).
  p <- .v09313_andic(c(0, 20, 30, 60), c(20, 30, 60, 120), c("Ah", "Bw", "2Bw", "C"),
                     c(TRUE, FALSE, TRUE, FALSE))
  expect_true(isTRUE(andosol(p)$passed))
  expect_equal(andosol(p)$evidence$combined_thickness_cm, 50)
  # the uppermost andic layer must start <= 25 cm
  q <- .v09313_andic(c(0, 30, 90), c(30, 90, 150), c("A", "2Bw", "C"), c(FALSE, TRUE, FALSE))
  expect_identical(andosol(q)$passed, FALSE)
  # andic 0-20 and 90-150: 20 + 10 cm within 100 cm
  r <- .v09313_andic(c(0, 20, 90), c(20, 90, 150), c("Ah", "Bw", "2Bw"), c(TRUE, FALSE, TRUE))
  expect_true(isTRUE(andosol(r)$passed))
  s <- .v09313_andic(c(0, 20, 95), c(20, 95, 150), c("Ah", "Bw", "2Bw"), c(TRUE, FALSE, TRUE))
  expect_identical(andosol(s)$passed, FALSE)
  # an undecided layer that could make the 30 cm leaves the gate undecided
  u <- .v09313_andic(c(0, 20), c(20, 80), c("Ah", "Bw"), c(TRUE, FALSE))
  u$horizons$al_ox_pct[2] <- NA; u$horizons$bulk_density_g_cm3[2] <- NA
  expect_true(is.na(andosol(u)$passed))
})

test_that("Andosols item 1b: >= 60% of the soil above a limiting layer at 25-50 cm", {
  skip_on_cran()
  p <- .v09313_andic(c(0, 25, 40), c(25, 40, 60), c("Ah", "Bw", "R"), c(TRUE, FALSE, FALSE))
  expect_true(isTRUE(andosol(p)$passed))                      # 25 of 40 cm = 62.5%
  expect_identical(andosol(p)$evidence$item1a, FALSE)
  q <- .v09313_andic(c(0, 20, 40), c(20, 40, 60), c("Ah", "Bw", "R"), c(TRUE, FALSE, FALSE))
  expect_identical(andosol(q)$passed, FALSE)                  # 20 of 40 cm = 50%
})

test_that("a petroplinthic horizon within 100 cm excludes the Andosols", {
  skip_on_cran()
  p <- .v09313_andic(c(0, 35, 47), c(35, 47, 100), c("Ah", "Bsm", "C"), c(TRUE, FALSE, FALSE),
                     plinthite_pct = c(0, 50, 0),
                     cementation_class = c("none", "strongly", "none"))
  expect_true(isTRUE(petroplinthic(p)$passed))
  expect_identical(andosol(p)$passed, FALSE)
  p$horizons$plinthite_pct[2] <- 0
  expect_true(isTRUE(andosol(p)$passed))
})

test_that("an argic horizon of andic layers with no clay films does not exclude (N4)", {
  skip_on_cran()
  p <- .v09313_pedon(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                     designation = c("Ah", "Bw", "C"), clay_pct = c(15, 35, 15),
                     silt_pct = c(45, 35, 45), sand_pct = c(40, 30, 40),
                     al_ox_pct = c(2.5, 2.6, 0.5), fe_ox_pct = c(1, 1, 0.5),
                     bulk_density_g_cm3 = c(0.7, 0.8, 1.3), oc_pct = c(6, 2, 0.5),
                     clay_films_amount = c("none", "none", "none"))
  expect_true(isTRUE(argic(p)$passed))
  a <- andosol(p)
  expect_true(isTRUE(a$passed))
  expect_true(a$evidence$argic_of_andic_layers)
  # with clay films, the argic horizon excludes
  p$horizons$clay_films_amount[2] <- "common"
  expect_identical(andosol(p)$passed, FALSE)
  # and so it does when its layers are not andic
  p$horizons$clay_films_amount[2] <- "none"
  p$horizons$bulk_density_g_cm3[2] <- 1.2
  expect_identical(andosol(p)$passed, FALSE)
})

# ---- Cryosols item 2 (Ch 4, p 98) -------------------------------------------

.v09313_frozen <- function(desig = c("Ap", "Bg@1", "Bg@2", "Cg", "Cf"),
                           top = c(0, 20, 50, 90, 130), site = list(id = "v09313-cr")) {
  .v09313_pedon(top_cm = top, bottom_cm = c(top[-1], top[length(top)] + 30),
                designation = desig, boundary_topography = rep("wavy", length(top)),
                oc_pct = c(3, 1, 0.8, 0.5, 0.4), clay_pct = 25, silt_pct = 45, sand_pct = 30,
                site = site)
}

test_that("Cryosols item 2: cryic horizon <= 200 cm with cryogenic alteration within 100 cm", {
  skip_on_cran()
  p <- .v09313_frozen()
  expect_false(isTRUE(cryic_conditions(p)$passed))           # item 1: frozen only from 130 cm
  expect_true(isTRUE(soilKey:::cryosol(p)$passed))
  expect_identical(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Cryosols")
  # no evidence of cryogenic alteration
  q <- .v09313_frozen(desig = c("Ap", "Bg1", "Bg2", "Cg", "Cf"))
  q$horizons$boundary_topography <- "smooth"
  expect_identical(soilKey:::cryosol(q)$passed, FALSE)
  # cryic horizon starting > 200 cm
  r <- .v09313_frozen(top = c(0, 20, 50, 90, 210))
  expect_identical(soilKey:::cryosol(r)$passed, FALSE)
  # cryoturbated, no frozen layer described and no soil temperature: undecided;
  # a regime warmer than gelic rules permafrost out (v0.9.307)
  d <- c("Ap", "Bg@1", "Bg@2", "Cg", "Cg2")
  expect_true(is.na(soilKey:::cryosol(.v09313_frozen(desig = d))$passed))
  w <- .v09313_frozen(desig = d, site = list(id = "v09313-cr", soil_temperature_regime = "mesic"))
  expect_identical(soilKey:::cryosol(w)$passed, FALSE)
  # item 1 is kept
  expect_true(isTRUE(soilKey:::cryosol(make_cryosol_canonical())$passed))
})
