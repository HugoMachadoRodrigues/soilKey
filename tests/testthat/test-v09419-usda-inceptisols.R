# v0.9.419 (Hugo, 2026-11-20): Inceptisols, Entisols and Aridisols after the
# Soil Taxonomy audit (section 2.3, I1 to I12; R2; F12 and F14; readings L13
# to L16 as recommended).
#   * the USDA cambic horizon (KST 13th ed., pp. 22-23) is a function of its
#     own, read layer by layer and three-valued; item K of the Key to Soil
#     Orders (p. 70) is read in full;
#   * Psamments before Fluvents (p. 165); the Psamment texture in all layers
#     of the particle-size control section; the Fluvent organic carbon with a
#     rise of more than 0.05 points (L15);
#   * Oxyaquic reads saturation, not redox features (R2, L17);
#   * duripan, petrocalcic, gypsic and sombric horizons follow their text;
#   * the plaggen and anthropic epipedons exist.

.p419 <- function(h, site = list()) {
  PedonRecord$new(site = c(list(id = "v09419"), site), horizons = h)
}
.f419 <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)

# A, Bw, C: a better-drained cambic horizon by colour (KST 4b(1))
.cambic_profile <- function(...) {
  h <- data.frame(
    designation = c("A", "Bw", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
    clay_pct = c(18, 22, 18), sand_pct = c(40, 35, 42), silt_pct = c(42, 43, 40),
    oc_pct = c(1.2, 0.4, 0.2),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
    munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(2, 6, 3),
    structure_type = c("granular", "subangular blocky", "massive"),
    structure_grade = c("weak", "moderate", "massive"),
    redoximorphic_features_pct = c(0, 0, 0),
    stringsAsFactors = FALSE)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  h
}

test_that("the USDA cambic horizon reads the Bw by its own criteria", {
  skip_on_cran()
  p <- .p419(.cambic_profile())
  r <- .f419("cambic_horizon_usda", p)
  expect_true(r$passed)
  expect_identical(r$layers, 2L)
  # structure not recorded: undecided (criterion 3)
  h <- .cambic_profile(); h$structure_type[2] <- NA; h$structure_grade[2] <- NA
  r <- .f419("cambic_horizon_usda", .p419(h))
  expect_true(is.na(r$passed))
  expect_true("structure_type" %in% r$missing)
  # massive passes criterion 3 (absence of rock structure)
  h <- .cambic_profile(); h$structure_type[2] <- "massive"; h$structure_grade[2] <- "massive"
  expect_true(.f419("cambic_horizon_usda", .p419(h))$passed)
  # a layer designated C is not cambic (audit L14)
  # (the A above may still be one, reading N11, so look at the layers)
  h <- .cambic_profile(); h$designation[2] <- "C1"
  expect_false(2L %in% .f419("cambic_horizon_usda", .p419(h))$layers)
  # a laboratory loamy sand is read as coarser than very fine sand, unless
  # the field texture class names a loamy very fine sand (KST p. 22)
  # (the unplowed A, with more clay than the Bw, may itself be cambic by
  # reading N11, so look at the Bw)
  h <- .cambic_profile(); h$clay_pct[2] <- 6; h$sand_pct[2] <- 84; h$silt_pct[2] <- 10
  expect_false(2L %in% .f419("cambic_horizon_usda", .p419(h))$layers)
  h$texture_class_usda <- c(NA, "LVFS", NA)
  expect_true(2L %in% .f419("cambic_horizon_usda", .p419(h))$layers)
  # with no laboratory texture, the field class decides
  h <- .cambic_profile(); h$clay_pct[2] <- NA; h$sand_pct[2] <- NA; h$silt_pct[2] <- NA
  expect_true(is.na(.f419("cambic_horizon_usda", .p419(h))$passed))
  h$texture_class_usda <- c(NA, "SiL", NA)
  expect_true(.f419("cambic_horizon_usda", .p419(h))$passed)
})

test_that("the overlying horizon of criterion 4b(1) may be the A (audit L13)", {
  skip_on_cran()
  # the Bw is no brighter, redder or more clayey than the C below; it has a
  # higher value than the A above
  h <- .cambic_profile(munsell_hue_moist = c("10YR", "10YR", "10YR"),
                       munsell_value_moist = c(3, 5, 5),
                       munsell_chroma_moist = c(2, 3, 3),
                       clay_pct = c(18, 18, 18), sand_pct = c(40, 42, 42),
                       silt_pct = c(42, 40, 40))
  expect_true(.f419("cambic_horizon_usda", .p419(h))$passed)
  h$munsell_value_moist[1] <- 5; h$munsell_chroma_moist[1] <- 3
  expect_false(.f419("cambic_horizon_usda", .p419(h))$passed)
})

test_that("item K of the Key to Soil Orders: cambic, K.2 and undecided", {
  skip_on_cran()
  p <- .p419(.cambic_profile())
  k <- .f419("inceptisol_qualifying_usda", p)
  expect_true(k$passed)
  expect_true(k$evidence$k1a_cambic)
  r <- classify_usda(p, on_missing = "silent")
  expect_identical(r$rsg_or_order, "Inceptisols")
  # cambic undecided (no structure): the order is undecided and the key
  # goes on to the Entisols, graded C
  h <- .cambic_profile(); h$structure_type <- NA; h$structure_grade <- NA
  p <- .p419(h)
  expect_true(is.na(.f419("inceptisol_qualifying_usda", p)$passed))
  r <- classify_usda(p, on_missing = "silent")
  expect_identical(r$rsg_or_order, "Entisols")
  expect_identical(r$evidence_grade, "C")
  # K.2: an umbric epipedon over rock (the canonical Neossolo Litolico)
  r <- classify_usda(make_neossolo_canonical(), on_missing = "silent")
  # v0.9.429: the fixture records an ustic regime (NRCS map)
  expect_identical(r$name, "Lithic Humustepts")
  # a positive water potential at the surface (the Wassents) is not K
  p <- .p419(.cambic_profile(), site = list(water_table_cm_above_surface = 10))
  expect_false(.f419("inceptisol_qualifying_usda", p)$passed)
})

test_that("Psamments come before Fluvents and read every layer of the control section", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "C1", "C2", "C3"),
                  top_cm = c(0, 20, 60, 110), bottom_cm = c(20, 60, 110, 160),
                  clay_pct = c(4, 3, 3, 3), sand_pct = c(92, 94, 94, 94),
                  silt_pct = c(4, 3, 3, 3), oc_pct = c(0.8, 0.3, 0.4, 0.3),
                  stringsAsFactors = FALSE)
  p <- .p419(h, site = list(slope_pct = 1))
  expect_true(.f419("psamment_qualifying_usda", p)$passed)
  expect_true(.f419("fluvent_qualifying_usda", p)$passed)
  r <- classify_usda(p, on_missing = "silent")
  expect_identical(r$trace$suborders$LD$passed, TRUE)
  expect_match(r$name, "psamments$")
  # one loam layer in the control section: not a Psamment
  h$clay_pct[3] <- 20; h$sand_pct[3] <- 40; h$silt_pct[3] <- 40
  expect_false(.f419("psamment_qualifying_usda", .p419(h))$passed)
  # no texture at all: undecided (audit F12, psammentic subgroups too)
  h2 <- h; h2$clay_pct <- NA; h2$sand_pct <- NA; h2$silt_pct <- NA
  expect_true(is.na(.f419("psamment_qualifying_usda", .p419(h2))$passed))
  expect_true(is.na(.f419("psammentic_subgroup_usda", .p419(h2))$passed))
})

test_that("Fluvents: organic carbon, a rise of more than 0.05 points (L15), slope and contact", {
  skip_on_cran()
  r <- classify_usda(make_fluvisol_canonical(), on_missing = "silent")
  # v0.9.425: the dark upper 15 cm make it Mollic (KST p. 173)
  expect_identical(r$name, "Mollic Udifluvents")
  h <- data.frame(designation = c("A", "C1", "C2", "C3"),
                  top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 110),
                  clay_pct = 20, sand_pct = 40, silt_pct = 40,
                  oc_pct = c(1.5, 0.50, 0.54, 0.10), stringsAsFactors = FALSE)
  p <- .p419(h, site = list(slope_pct = 2))
  oc <- soilKey:::.usda_fluvent_oc(p)
  expect_false(oc$irregular)            # 0.04 points is no irregular decrease
  expect_true(is.na(oc$at_125))         # the profile ends above 125 cm
  expect_true(is.na(.f419("fluvent_qualifying_usda", p)$passed))
  h$oc_pct[3] <- 0.60
  expect_true(.f419("fluvent_qualifying_usda", .p419(h, site = list(slope_pct = 2)))$passed)
  # the layer that straddles 25 cm is read (audit F14)
  expect_true(.f419("fluventic_usda", .p419(h))$passed)
  # a slope of 25 % or more, or a lithic contact within 25 cm, is not a Fluvent
  expect_false(.f419("fluvent_qualifying_usda", .p419(h, site = list(slope_pct = 30)))$passed)
  h2 <- h; h2$designation[2] <- "R"
  expect_false(.f419("fluvent_qualifying_usda", .p419(h2, site = list(slope_pct = 2)))$passed)
  # a missing value between two layers leaves the decrease undecided
  h3 <- h; h3$oc_pct <- c(1.5, 0.5, NA, 0.1)
  expect_true(is.na(soilKey:::.usda_fluvent_oc(.p419(h3))$irregular))
})

test_that("Oxyaquic reads saturation, not redox features (R2, L17)", {
  skip_on_cran()
  h <- .cambic_profile(redoximorphic_features_pct = c(0, 10, 15))
  r <- .f419("oxyaquic_subgroup_usda", .p419(h))
  expect_true(is.na(r$passed))
  expect_identical(r$missing, "water_saturation_days")
  h$water_saturation_days <- c(0, 35, 40)
  expect_true(.f419("oxyaquic_subgroup_usda", .p419(h))$passed)
  h$water_saturation_days <- c(0, 5, 10)
  expect_false(.f419("oxyaquic_subgroup_usda", .p419(h))$passed)
  h$water_saturation_days <- c(0, 25, 10)    # 20 consecutive days possible
  expect_true(is.na(.f419("oxyaquic_subgroup_usda", .p419(h))$passed))
  h$water_saturation_days <- NA
  p <- .p419(h, site = list(flooding_days_growing_period = 40))
  expect_true(.f419("oxyaquic_subgroup_usda", p)$passed)
})

.pan_profile <- function(des, ...) {
  h <- data.frame(designation = c("A", des, "C"), top_cm = c(0, 20, 50),
                  bottom_cm = c(20, 50, 100), clay_pct = 20, sand_pct = 40,
                  silt_pct = 40, stringsAsFactors = FALSE)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  .p419(h)
}

test_that("duripan, petrocalcic and gypsic horizons read their cementing agent and visible gypsum", {
  skip_on_cran()
  # duripan: no longer only indurated (audit I9)
  expect_true(.f419("duripan_usda", .pan_profile("Bqm", cementation_class = c(NA, "strongly", NA)))$passed)
  expect_true(.f419("duripan_usda", .pan_profile("Bw", duripan_pct = c(0, 70, 0)))$passed)
  expect_false(.f419("duripan_usda", .pan_profile("Bw", duripan_pct = c(0, 35, 0)))$passed)
  expect_true(is.na(.f419("duripan_usda", .pan_profile("Bq", cementation_class = c(NA, "weakly", NA)))$passed))
  expect_false(.f419("duripan_usda", .pan_profile("Bw"))$passed)
  # petrocalcic: carbonates as the cementing agent (audit I10)
  expect_true(.f419("petrocalcic_subgroup_usda", .pan_profile("Bkm"))$passed)
  expect_false(.f419("petrocalcic_subgroup_usda",
                     .pan_profile("Bqm", caco3_pct = c(0, 20, 0)))$passed)
  expect_true(is.na(.f419("petrocalcic_subgroup_usda",
                          .pan_profile("Bw", cementation_class = c(NA, "moderately", NA)))$passed))
  expect_false(.f419("petrocalcic_subgroup_usda", .pan_profile("Bk", caco3_pct = c(0, 30, 10)))$passed)
  # gypsic: visible secondary gypsum is a field observation (audit I10)
  expect_false(.f419("gypsic_horizon_usda", .pan_profile("Bw", caso4_pct = c(0, 20, 0)))$passed)
  expect_true(.f419("gypsic_horizon_usda", .pan_profile("By", caso4_pct = c(0, 20, 0)))$passed)
  expect_true(is.na(.f419("gypsic_horizon_usda", .pan_profile("Bw", gypsum_visible_pct = c(0, 5, 0)))$passed))
  expect_false(.f419("gypsic_horizon_usda", .pan_profile("By", caso4_pct = c(0, 4, 0)))$passed)
})

test_that("the sombric horizon is not a buried or gleyed layer (audit I12)", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "Bw", "Bh", "C"), top_cm = c(0, 20, 60, 90),
                  bottom_cm = c(20, 60, 90, 150), clay_pct = 30, sand_pct = 40, silt_pct = 30,
                  oc_pct = c(2.0, 0.5, 1.2, 0.3),
                  munsell_value_moist = c(3, 5, 3, 5), munsell_chroma_moist = c(2, 6, 3, 6),
                  ph_h2o = c(4.8, 4.9, 4.7, 5.0), al_ox_pct = 0.1, fe_ox_pct = 0.2,
                  stringsAsFactors = FALSE)
  expect_true(.f419("sombric_horizon_usda", .p419(h))$passed)
  h$designation[3] <- "Ab"
  expect_false(.f419("sombric_horizon_usda", .p419(h))$passed)
  h$designation[3] <- "Bg"
  expect_false(.f419("sombric_horizon_usda", .p419(h))$passed)
  h$designation[3] <- "Bh"; h$al_ox_pct <- NA
  expect_true(is.na(.f419("sombric_horizon_usda", .p419(h))$passed))
  h$al_ox_pct <- 0.1; h$ph_h2o[3] <- 5.6
  expect_false(.f419("sombric_horizon_usda", .p419(h))$passed)
})

test_that("plaggen and anthropic epipedons rest on recorded human-made material (audit I11)", {
  skip_on_cran()
  p <- .p419(.cambic_profile())
  expect_false(.f419("plaggen_epipedon_usda", p)$passed)
  expect_false(.f419("anthropic_epipedon_usda", p)$passed)
  h <- data.frame(designation = c("^Au", "^C", "2C"), top_cm = c(0, 30, 60),
                  bottom_cm = c(30, 60, 120), clay_pct = 20, sand_pct = 40, silt_pct = 40,
                  oc_pct = c(2, 0.5, 0.2), artefacts_pct = c(20, 10, 0),
                  structure_type = c("granular", "massive", "massive"),
                  stringsAsFactors = FALSE)
  expect_true(.f419("anthropic_epipedon_usda", .p419(h))$passed)
  h$structure_type[1] <- NA
  expect_true(is.na(.f419("anthropic_epipedon_usda", .p419(h))$passed))
  pl <- data.frame(designation = c("Ap1", "Ap2", "C"), top_cm = c(0, 30, 70),
                   bottom_cm = c(30, 70, 120), clay_pct = 5, sand_pct = 85, silt_pct = 10,
                   oc_pct = c(2.5, 1.5, 0.2), munsell_value_moist = c(3, 3, 5),
                   munsell_value_dry = c(4, 4, 6), munsell_chroma_moist = c(1, 2, 4),
                   layer_origin = c("plaggen", "plaggen", NA), stringsAsFactors = FALSE)
  expect_true(.f419("plaggen_epipedon_usda", .p419(pl))$passed)
  pl$oc_pct[2] <- NA
  expect_true(is.na(.f419("plaggen_epipedon_usda", .p419(pl))$passed))
})

test_that("Aquepts, Halaquepts and Eutrudepts follow their key text", {
  skip_on_cran()
  # Halaquepts: an EC alone is not the KAC criterion
  h <- .cambic_profile(ec_dS_m = c(10, 10, 10), esp_pct = c(2, 2, 2))
  expect_false(isTRUE(.f419("halaquept_qualifying_usda", .p419(h))$passed))
  h$esp_pct <- c(20, 20, 10)
  expect_true(.f419("halaquept_qualifying_usda", .p419(h))$passed)
  # Eutrudepts: base saturation between 25 and 75 cm
  h <- .cambic_profile(bs_pct = c(80, 40, 40))
  expect_false(.f419("eutric_inceptisol_usda", .p419(h))$passed)
  h$bs_pct <- c(80, NA, NA)
  expect_true(is.na(.f419("eutric_inceptisol_usda", .p419(h))$passed))
  h$bs_pct <- c(30, 65, 40)
  expect_true(.f419("eutric_inceptisol_usda", .p419(h))$passed)
  # Aquepts: aquic conditions and chroma of 1 or less within 50 cm
  h <- .cambic_profile(munsell_chroma_moist = c(1, 1, 1), redoximorphic_features_pct = c(0, 10, 10))
  h$designation <- c("A", "Bg", "Cg")
  expect_true(.f419("aquept_qualifying_usda", .p419(h))$passed)
  h <- .cambic_profile()
  expect_false(.f419("aquept_qualifying_usda", .p419(h))$passed)
})

test_that("in an unplowed soil an A may be the cambic horizon (reading N11)", {
  skip_on_cran()
  # A 0-35 cm, redder and structured, directly over an unaltered C
  h <- data.frame(
    designation = c("A", "C"), top_cm = c(0, 35), bottom_cm = c(35, 120),
    clay_pct = c(20, 18), sand_pct = c(40, 42), silt_pct = c(40, 40),
    oc_pct = c(0.8, 0.2),
    munsell_hue_moist = c("7.5YR", "10YR"),
    munsell_value_moist = c(4, 5), munsell_chroma_moist = c(4, 3),
    structure_type = c("subangular blocky", "massive"),
    structure_grade = c("moderate", "massive"),
    redoximorphic_features_pct = c(0, 0),
    stringsAsFactors = FALSE)
  expect_true(.f419("cambic_horizon_usda", .p419(h))$passed)
  # the same A plowed (Ap) is not cambic
  h$designation[1] <- "Ap"
  expect_false(isTRUE(.f419("cambic_horizon_usda", .p419(h))$passed))
})
