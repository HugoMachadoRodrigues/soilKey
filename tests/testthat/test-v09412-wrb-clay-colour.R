# v0.9.412 -- WRB 2022 qualifiers about clay, the argic horizon and colour:
# cambic criterion 3.b against any overlying layer, the Rhodic and Chromic
# layer, Profondic, Differentic, Geric and Hypergeric, Cutanic with a natric
# horizon, the abrupt textural difference with a clay content not recorded,
# and the exchangeable acidity of the ISIS reader. Every profile is synthetic.

.mk412 <- function(...) PedonRecord$new(horizons = data.frame(..., stringsAsFactors = FALSE))

test_that("cambic criterion 3.b compares with any overlying mineral layer (Ch 3.1.5, p. 39)", {
  skip_on_cran()
  p <- .mk412(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
              designation = c("Ah", "Bw1", "Bw2"),
              munsell_hue_moist = c("10YR", "2.5YR", "2.5YR"),
              munsell_value_moist = c(3, 3, 3), munsell_chroma_moist = c(2, 6, 6))
  r <- test_cambic_soil_formation(p$horizons, candidate_layers = 3L)
  expect_true(3L %in% r$layers)               # Bw2 against the Ah, across Bw1
  expect_equal(r$details[["3"]]$evidence$b_layer, 1L)
  # a lithic discontinuity in between stops the comparison
  q <- .mk412(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
              designation = c("Ah", "Bw1", "2Bw2"),
              munsell_hue_moist = c("10YR", "2.5YR", "2.5YR"),
              munsell_value_moist = c(3, 3, 3), munsell_chroma_moist = c(2, 6, 6))
  expect_false(3L %in% test_cambic_soil_formation(q$horizons, candidate_layers = 3L)$layers)
})

test_that("Rhodic reads the part of a horizon inside 25-150 cm (Ch 5, p. 145)", {
  skip_on_cran()
  # a red Bt from 15 to 80 cm: 55 cm inside the range (until v0.9.411 dropped)
  p <- .mk412(top_cm = c(0, 15), bottom_cm = c(15, 80), designation = c("Ah", "Bt"),
              munsell_hue_moist = c("2.5YR", "10R"), munsell_value_moist = c(3, 3),
              munsell_chroma_moist = c(4, 4), munsell_value_dry = c(4, 3.5))
  r <- qual_rhodic(p)
  expect_true(r$passed)
  expect_equal(r$evidence$thickness_cm, 55)
  # a red layer starting at 150 cm has no part inside the range
  q <- .mk412(top_cm = c(0, 20, 150), bottom_cm = c(20, 150, 250),
              designation = c("Ah", "Bw", "Bg"),
              munsell_hue_moist = c("10YR", "10YR", "2.5YR"),
              munsell_value_moist = c(3, 4, 3), munsell_chroma_moist = c(2, 4, 6))
  expect_false(qual_rhodic(q)$passed)
})

test_that("the Rhodic layer is one run of adjoining horizons", {
  skip_on_cran()
  # two red layers of 20 cm with a yellow one between: no layer of 30 cm
  p <- .mk412(top_cm = c(0, 30, 50, 70), bottom_cm = c(30, 50, 70, 90),
              designation = c("Ah", "Bt1", "Bt2", "Bt3"),
              munsell_hue_moist = c("10YR", "2.5YR", "10YR", "2.5YR"),
              munsell_value_moist = c(3, 3, 5, 3), munsell_chroma_moist = c(2, 6, 6, 6))
  expect_false(qual_rhodic(p)$passed)
})

test_that("Chromic counts a horizon starting above 25 cm and excludes Rhodic (Ch 5, p. 130)", {
  skip_on_cran()
  p <- .mk412(top_cm = c(0, 5, 22, 46), bottom_cm = c(5, 22, 46, 66),
              designation = c("Ah", "AB", "B", "Bg"),
              munsell_hue_moist = c("7.5YR", "7.5YR", "5YR", "5YR"),
              munsell_value_moist = c(3, 5, 4, 4), munsell_chroma_moist = c(2, 6, 6, 8))
  r <- qual_chromic(p)
  expect_true(r$passed)
  expect_equal(r$evidence$thickness_cm, 41)
})

test_that("Profondic reads the decrease of clay from its maximum (Ch 5, p. 144)", {
  skip_on_cran()
  base <- function(top, bot, clay, desg)
    .mk412(top_cm = top, bottom_cm = bot, designation = desg, clay_pct = clay,
           silt_pct = 30, sand_pct = 70 - clay,
           munsell_hue_moist = "7.5YR", munsell_value_moist = 4, munsell_chroma_moist = 4)
  keep <- base(c(0, 20, 60, 110), c(20, 60, 110, 160), c(12, 30, 32, 30),
               c("Ah", "Bt1", "Bt2", "BC"))
  expect_true(isTRUE(argic(keep)$passed))
  expect_true(qual_profondic(keep)$passed)
  drop <- base(c(0, 20, 60, 110), c(20, 60, 110, 160), c(12, 30, 32, 24),
               c("Ah", "Bt1", "Bt2", "C"))
  expect_false(qual_profondic(drop)$passed)       # 24 / 32 = 0.75
  short <- base(c(0, 20, 60), c(20, 60, 100), c(12, 30, 32), c("Ah", "Bt1", "Bt2"))
  expect_true(is.na(qual_profondic(short)$passed)) # description stops at 100 cm (L4)
  rock <- .mk412(top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 120),
                 designation = c("Ah", "Bt1", "Bt2", "R"), clay_pct = c(12, 30, 32, NA),
                 silt_pct = c(30, 30, 30, NA), sand_pct = c(58, 40, 38, NA))
  expect_false(qual_profondic(rock)$passed)       # limiting layer above 150 cm
})

test_that("Differentic uses the argic switches: a plough layer directly above (Ch 3.1.3, p. 36)", {
  skip_on_cran()
  mk <- function(top_name)
    .mk412(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c(top_name, "Bt"),
           clay_pct = c(15, 30), silt_pct = c(30, 30), sand_pct = c(55, 40),
           clay_films_amount = c(NA, "many"))
  ap <- mk("Ap")
  expect_true(isTRUE(argic(ap)$passed))           # by its clay films (2.b)
  expect_false(qual_differentic(ap)$passed)       # 2.a.ii
  expect_true(qual_differentic(mk("Ah"))$passed)
})

test_that("Geric takes the ECEC or bases plus exchangeable acidity as an upper bound (Ch 5, p. 135)", {
  skip_on_cran()
  # Mg not recorded: undecided from the bases, decided by the ECEC
  p <- .mk412(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("Ah", "Bw"),
              clay_pct = c(20, 30), ca_cmol = c(1, 0.1), mg_cmol = NA, k_cmol = c(0.2, 0),
              na_cmol = c(0, 0), al_kcl_cmol = c(0.5, 0.6), ecec_cmol = c(2.6, 0.8))
  expect_true(qual_geric(p)$passed)
  expect_false(qual_hypergeric(p)$passed)         # 0.8 / 30 = 2.7 per kg clay
  p$horizons$ecec_cmol <- NA_real_
  expect_true(is.na(qual_geric(p)$passed))
  # no Al: the four bases plus the exchangeable acidity
  q <- .mk412(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("Ah", "Bw"),
              clay_pct = c(40, 60), ca_cmol = c(0.6, 0.2), mg_cmol = c(0.8, 0.1),
              k_cmol = c(0.1, 0), na_cmol = c(0, 0), exch_acidity_kcl_cmol = c(0.3, 0.2))
  expect_true(qual_geric(q)$passed)
  expect_true(qual_hypergeric(q)$passed)          # 0.5 / 60 = 0.83 per kg clay
})

test_that("Cutanic reads a natric horizon too (Ch 5, p. 131)", {
  skip_on_cran()
  p <- .mk412(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("Ah", "Btn"),
              clay_pct = c(12, 30), silt_pct = c(30, 30), sand_pct = c(58, 40),
              clay_films_amount = c(NA, "many"))
  testthat::local_mocked_bindings(
    argic = function(pedon, ...) DiagnosticResult$new(name = "argic", passed = FALSE,
      layers = integer(0), evidence = list(), missing = character(0), reference = ""),
    natric_horizon = function(pedon, ...) DiagnosticResult$new(name = "natric",
      passed = TRUE, layers = 2L, evidence = list(), missing = character(0), reference = ""),
    .package = "soilKey")
  r <- qual_cutanic(p)
  expect_true(r$passed)
  expect_equal(r$layers, 2L)
})

test_that("an abrupt textural difference with a clay content not recorded is undecided (Ch 3.2.1, p. 72)", {
  skip_on_cran()
  p <- .mk412(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
              designation = c("E", "Bt", "Btv"), clay_pct = c(6, 13, NA),
              boundary_distinctness = c("clear", "abrupt", NA))
  expect_true(is.na(abrupt_textural_difference(p)$passed))
  # the boundary already rules the pair out
  p$horizons$boundary_distinctness <- c("clear", "gradual", NA)
  expect_false(abrupt_textural_difference(p)$passed)
})

test_that("the Cambic qualifier asks cambic criterion 3 (Hugo, L10)", {
  skip_on_cran()
  mk <- function(bw)
    .mk412(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
           designation = c("Ah", "Bw", "C"), clay_pct = c(20, 22, 20),
           silt_pct = c(40, 40, 40), sand_pct = c(40, 38, 40),
           structure_grade = c("moderate", "moderate", "weak"),
           structure_type = c("granular", "subangular blocky", "subangular blocky"),
           munsell_hue_moist = c("10YR", bw, "10YR"), munsell_value_moist = c(3, 4, 4),
           munsell_chroma_moist = c(2, 4, 4))
  yes <- mk("7.5YR")
  skip_if_not(isTRUE(cambic(yes)$passed))
  expect_true(qual_cambic(yes)$passed)
  # the same colour as the Ah and the C, and no clay step: no evidence found
  no <- yes
  no$horizons$munsell_hue_moist[2] <- "10YR"; no$horizons$munsell_value_moist[2] <- 3
  no$horizons$munsell_chroma_moist[2] <- 2
  skip_if_not(isTRUE(cambic(no)$passed))
  expect_true(is.na(qual_cambic(no)$passed))
})

test_that("the ISIS reader reads the exchangeable acidity", {
  skip_on_cran()
  it <- function(x) list(item = x, description = NULL)
  prof <- list(profileCode = "XX412", countryCode = NULL,
    layers = list(
      list(layerNumber = 1L, upperDepth = 0, lowerDepth = 20, layerName = "A",
           clay = "30", acidityExc = list("0.4"), colourMoistHue = it("10YR"),
           colourMoistValue = it("3"), colourMoistChroma = it("2"))),
    wrbClassifications = list())
  p <- suppressMessages(suppressWarnings(isis_profile_to_pedon(prof)))
  expect_equal(p$horizons$exch_acidity_kcl_cmol, 0.4)
})
