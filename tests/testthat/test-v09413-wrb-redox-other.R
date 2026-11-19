# v0.9.413: WRB 2022 redox and other qualifiers read again after the ISIS
# audit (Hugo, 2026-11-19): the Stagnic layer (Ch 5, p. 147), the bottom
# horizon and the mottle colours of stagnic properties (Ch 3.2.14, p. 81),
# Reductigleyic (Ch 5, p. 145), Albic criterion 2 (Ch 3.1.1, p. 34), Cambic
# and claric material (Ch 5, p. 129), Calcaric (Ch 5, p. 129; Ch 3.3.3,
# p. 86), the ferric horizon (Ch 3.1.11, p. 45), Grumic (Ch 5, p. 136) and
# Haplic (Ch 5, p. 136). The profiles are made up.

.v413_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "v413", country = "XX"), site),
                  horizons = ensure_horizon_schema(hz))
}

# ---- Stagnic ---------------------------------------------------------------

test_that("Stagnic finds a layer of adjoining horizons, not the whole run", {
  skip_on_cran()
  # 26-60 cm averages 30.7% of redox features; the whole run 26-80 only 23.1%
  p <- .v413_pedon(top_cm = c(0, 26, 37, 60, 80), bottom_cm = c(26, 37, 60, 80, 120),
                   designation = c("Ap", "ABg", "Bw1", "Bw2", "Bw3"),
                   munsell_hue_moist = "7.5YR", munsell_value_moist = 4,
                   munsell_chroma_moist = 4,
                   redoximorphic_features_pct = c(0, 37.5, 27.5, 10, 0),
                   mottle_colors = c(NA, "2.5YR 4/8", "2.5YR 4/8", "2.5YR 4/8", NA),
                   clay_pct = 30,
                   site = list(groundwater_kind = "none observed"))
  st <- soilKey:::qual_stagnic(p)
  expect_true(st$passed)
  expect_equal(st$layers, 2:3)
})

test_that("a mottled bottom horizon with nothing described below can be stagnic", {
  skip_on_cran()
  p <- .v413_pedon(top_cm = c(0, 45), bottom_cm = c(45, 95),
                   designation = c("Ap", "Btg"),
                   munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5),
                   munsell_chroma_moist = 2,
                   redoximorphic_features_pct = c(0, 20),
                   mottle_colors = c(NA, "5YR 4/6"), clay_pct = c(20, 30),
                   site = list(groundwater_kind = "none observed"))
  z <- soilKey:::.wrb_redox_layers(p)
  expect_false(z$gleyic[2])
  expect_true(z$stagnic[2])
  expect_true(soilKey:::qual_stagnic(p)$passed)
})

test_that("the mottle colours must contrast as 3.2.14 criteria 1.b, 2.b and 3.c ask", {
  skip_on_cran()
  ct <- soilKey:::.stagnic_mottle_contrast
  # same hue, darker: neither oximorphic (2.b) nor reductimorphic (1.b)
  expect_false(ct("7.5YR", 6, 5, "7.5YR 4/4"))
  # 2.b: 2.5 hue units redder and a higher chroma; or black
  expect_true(ct("7.5YR", 4, 4, "2.5YR 4/8"))
  expect_true(ct("10YR", 5, 4, "10YR 2/1"))
  # 1.b: value 1 higher and chroma 1 lower
  expect_true(ct("10YR", 5, 4, "10YR 7/2"))
  # 3.c between two mottle colours, whatever the matrix
  expect_true(ct("N", 5, 0, "10YR 7/1; 5YR 5/8"))
  # unrecorded colours decide nothing
  expect_true(is.na(ct("10YR", 4, 3, NA)))
  expect_true(is.na(ct(NA, NA, NA, "10YR 6/6")))
  # mottles that do not contrast are not stagnic
  p <- .v413_pedon(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 105),
                   designation = c("Ap", "Bt1", "Bt2"),
                   munsell_hue_moist = "7.5YR", munsell_value_moist = c(3, 6, 6),
                   munsell_chroma_moist = c(4, 5, 5),
                   redoximorphic_features_pct = c(0, 27.5, 27.5),
                   mottle_colors = c(NA, "7.5YR 4/4", "7.5YR 4/4"), clay_pct = 30,
                   site = list(groundwater_kind = "none observed"))
  expect_false(soilKey:::qual_stagnic(p)$passed)
})

# ---- Reductigleyic ---------------------------------------------------------

test_that("Reductigleyic: a mottled layer that is not gleyic does not meet criterion 2", {
  skip_on_cran()
  p <- .v413_pedon(top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 120),
                   designation = c("Ah", "Bg", "Bw", "C"),
                   munsell_hue_moist = c("10YR", "5Y", "10YR", "10YR"),
                   munsell_value_moist = c(3, 5, 5, 5),
                   munsell_chroma_moist = c(2, 1, 4, 4),
                   redoximorphic_features_pct = c(0, 0, 10, 0),
                   mottle_colors = c(NA, NA, "5YR 5/8", NA), clay_pct = 25,
                   site = list(groundwater_kind = "none observed"))
  r <- soilKey:::qual_reductigleyic(p)
  expect_true(r$passed)
  expect_false(any(r$evidence$gleyic_criterion2 %in% TRUE))
})

# ---- Albic -----------------------------------------------------------------

.v413_argic <- function(e_value = 6, bt_value = 4, bt_chroma = 4, e_dry = 7) {
  .v413_pedon(top_cm = c(0, 15, 35, 80), bottom_cm = c(15, 35, 80, 120),
              designation = c("A", "E", "Bt", "BC"),
              munsell_hue_moist = "10YR",
              munsell_value_moist = c(3, e_value, bt_value, 5),
              munsell_chroma_moist = c(2, 2, bt_chroma, 4),
              munsell_value_dry = c(5, e_dry, bt_value + 1, 6),
              munsell_chroma_dry = c(2, 2, bt_chroma, 4),
              clay_pct = c(10, 8, 35, 30), silt_pct = 30, sand_pct = c(60, 62, 35, 40),
              clay_films_amount = c(NA, NA, "common", NA),
              structure_grade = "moderate", structure_type = "subangular blocky")
}

test_that("Albic asks criterion 2: a claric layer above an argic horizon, not inside it", {
  skip_on_cran()
  p <- .v413_argic()
  skip_if_not(isTRUE(argic(p)$passed))
  expect_true(soilKey:::qual_albic(p)$passed)
  # the claric colours in the argic horizon itself, under a dark E
  q <- .v413_argic(e_value = 3, bt_value = 6, bt_chroma = 2)
  skip_if_not(isTRUE(argic(q)$passed))
  c2 <- soilKey:::.wrb_albic_criterion2(q, 3L)
  expect_false(isTRUE(c2))
  expect_false(isTRUE(soilKey:::qual_albic(q)$passed))
  expect_length(soilKey:::.wrb_albic_layers(q, 3L), 0L)
})

# ---- Cambic ----------------------------------------------------------------

test_that("Cambic sets the claric layers aside, undecided when the claric colour is", {
  skip_on_cran()
  p <- function(vm, cm, vd, cd)
    .v413_pedon(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
                designation = c("A", "Bw", "C"),
                munsell_hue_moist = "10YR", munsell_value_moist = c(3, vm, 5),
                munsell_chroma_moist = c(2, cm, 4),
                munsell_value_dry = c(5, vd, 6), munsell_chroma_dry = c(2, cd, 4),
                clay_pct = 20, silt_pct = 40, sand_pct = 40,
                structure_grade = c("moderate", "moderate", NA),
                structure_type = c("granular", "subangular blocky", "massive"))
  brown <- p(4, 4, 5, 4)
  skip_if_not(isTRUE(cambic(brown)$passed))
  expect_true(soilKey:::qual_cambic(brown)$passed)
  expect_false(soilKey:::qual_cambic(p(6, 2, 7, 2))$passed)
  expect_true(is.na(soilKey:::qual_cambic(p(6, 2, NA, NA))$passed))
})

# ---- Calcaric and calcic -----------------------------------------------------

test_that("calcaric material: the laboratory and the field reaction must agree", {
  skip_on_cran()
  h <- data.table::data.table(caco3_pct = c(2.7, 2.7, 1, NA, 5),
                              effervescence_class = c("none", "strong", "strong", "strong", NA))
  expect_equal(soilKey:::.wrb_calcaric_status(h), c(FALSE, TRUE, FALSE, TRUE, TRUE))
})

test_that("calcic: a layer described as non-calcareous holds no 15% CaCO3", {
  skip_on_cran()
  p <- .v413_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80),
                   designation = c("A", "Bw"), effervescence_class = "none")
  expect_false(calcic(p)$passed)
  q <- .v413_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80),
                   designation = c("A", "Bw"), effervescence_class = "strong")
  expect_true(is.na(calcic(q)$passed))
})

test_that("Calcaric is not held back by an undecided horizon deeper than 100 cm", {
  skip_on_cran()
  p <- .v413_pedon(top_cm = c(0, 28, 105, 140), bottom_cm = c(28, 105, 140, 180),
                   designation = c("Ap", "Bt1", "2Bwk", "3Ck"),
                   caco3_pct = c(9.8, 8.7, 56.4, 75.8), clay_pct = 25)
  expect_true(soilKey:::qual_calcaric(p)$passed)
})

# ---- Ferric ----------------------------------------------------------------

test_that("ferric: fine and medium mottles are no coarse masses", {
  skip_on_cran()
  p <- function(size) .v413_pedon(top_cm = c(0, 20), bottom_cm = c(20, 60),
                                  designation = c("A", "Bt"),
                                  redoximorphic_features_pct = c(0, 30),
                                  mottle_colors = c(NA, "2.5YR 4/8"),
                                  mottle_size_class = c(NA, size),
                                  ferric_nodules_pct = c(0, 0))
  expect_false(ferric(p("medium"))$passed)
  expect_true(is.na(ferric(p(NA))$passed))
})

test_that("the ISIS reader reads the FAO (2006) concretion fields", {
  skip_on_cran()
  code <- function(item) list(item = item, description = NULL)
  pr <- list(profileCode = "XX413", countryCode = "XX", profileId = 1L,
             layers = list(
               list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah",
                    colourMoistHue = code("10YR")),
               list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 60L, layerName = "Bcs",
                    colourMoistHue = code("10YR"),
                    concretionsMineralHardness = code("H"), concretionsMineralKinds = code("N"),
                    concretionsMineralSize = code("M"), mineralConcAbundance = code("M"),
                    mineralConcNature = code("FM")),
               list(layerNumber = 3L, upperDepth = 60L, lowerDepth = 90L, layerName = "Bc",
                    colourMoistHue = code("10YR"),
                    concretionsMineralHardness = code("H"),
                    concretionsMineralSize = code("V"), mineralConcAbundance = code("M"),
                    mineralConcNature = code("F"))))
  h <- as.data.frame(isis_profile_to_pedon(pr)$horizons)
  expect_equal(h$ferric_nodules_pct[2], 27.5)
  expect_true(is.na(h$ferric_nodules_pct[3]))    # < 2 mm
})

# ---- Grumic ----------------------------------------------------------------

test_that("Grumic reads the second structure of the surface layer", {
  skip_on_cran()
  p <- function(g2) .v413_pedon(top_cm = c(0, 20), bottom_cm = c(20, 80),
                                designation = c("Ap", "Bi"),
                                structure_grade = "strong", structure_type = "subangular blocky",
                                structure_size = "coarse",
                                structure_grade_secondary = c(g2, NA),
                                structure_type_secondary = c("granular", NA),
                                structure_size_secondary = c("medium", NA), clay_pct = 50)
  expect_true(soilKey:::qual_grumic(p("strong"))$passed)
  expect_false(soilKey:::qual_grumic(p("moderate"))$passed)
})

test_that("the ISIS reader keeps the grade and size of the second structure", {
  skip_on_cran()
  code <- function(item) list(item = item, description = NULL)
  pr <- list(profileCode = "XX414", countryCode = "XX", profileId = 1L,
             layers = list(
               list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ap",
                    colourMoistHue = code("10YR"),
                    structureGrade1 = code("ST"), structureType = code("SB"), structureSize = code("CO"),
                    structureGrade2 = code("ST"), structureType2 = code("GR"), structureSize2 = code("ME"))))
  h <- as.data.frame(isis_profile_to_pedon(pr)$horizons)
  expect_equal(h$structure_grade_secondary, "strong")
  expect_equal(h$structure_size_secondary, "medium")
  expect_equal(h$structure_type_secondary, "granular")
})

# ---- Haplic ----------------------------------------------------------------

test_that("an undecided presence qualifier stays undecided", {
  skip_on_cran()
  na_diag <- DiagnosticResult$new(name = "x", passed = NA, layers = integer(0),
                                  evidence = list(), missing = "x", reference = "x")
  expect_true(is.na(soilKey:::.q_presence("X", na_diag, 100, NULL)$passed))
})

test_that("Haplic only when every principal qualifier is decided absent", {
  skip_on_cran()
  # no texture recorded below 20 cm: Skeletic and others stay undecided
  p <- .v413_pedon(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bw"))
  r <- resolve_wrb_qualifiers(p, "CM")
  und <- vapply(r$trace, function(t) is.na(t$passed[1]), NA)
  skip_if_not(any(und))
  expect_false("Haplic" %in% r$principal)
  # the name with no principal qualifier
  expect_identical(format_wrb_name("Cambisols", character(0), "Loamic"),
                   "Cambisol (Loamic)")
})
