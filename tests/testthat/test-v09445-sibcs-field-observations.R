# v0.9.445: the second change of the SiBCS 2018 audit (decision G2, Hugo,
# 2026-11-27). A field observation not recorded is absent (plinthite, redox
# features, cementation, durinodes, artefacts); a measurement, a colour or a
# structure not recorded is undecided (the B textural, the B planico, the
# histico, the A chernozemico, proeminente and humico, the B nitico, the
# mudanca textural abrupta, the decomposition of the Organossolos).

.g2 <- function(designation, top, bottom, ...) {
  hz <- data.table::data.table(designation = designation, top_cm = top,
                               bottom_cm = bottom, ...)
  PedonRecord$new(site = list(id = "g2", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("plinthite, redox features and cementation not recorded are absent", {
  skip_on_cran()
  p <- .g2(c("A", "Bw", "C"), c(0, 20, 80), c(20, 80, 150),
           clay_pct = c(20, 30, 30))
  expect_false(soilKey:::carater_plintico(p)$passed)
  expect_false(soilKey:::carater_redoxico(p)$passed)
  expect_false(soilKey:::carater_durico(p)$passed)
  expect_false(soilKey:::carater_placico(p)$passed)
  expect_false(horizonte_plintico(p)$passed)
  # recorded, they still count
  q <- .g2(c("A", "Bf", "C"), c(0, 20, 80), c(20, 80, 150),
           clay_pct = c(20, 30, 30), plinthite_pct = c(NA, 8, NA))
  expect_true(soilKey:::carater_plintico(q)$passed)
})

test_that("the A antropico needs artefacts recorded", {
  skip_on_cran()
  p <- .g2(c("Ap", "A2", "B"), c(0, 15, 40), c(15, 40, 100),
           oc_pct = c(2, 1.5, 0.5), p_mehlich3_mg_kg = c(80, 60, 5))
  expect_false(horizonte_A_antropico(p)$passed)
})

test_that("the mudanca textural abrupta is undecided where the distinctness is not recorded", {
  skip_on_cran()
  m <- function(bd) .g2(c("A", "Bt", "C"), c(0, 20, 80), c(20, 80, 150),
                        clay_pct = c(10, 35, 35),
                        boundary_distinctness = c(bd, NA, NA))
  expect_true(mudanca_textural_abrupta(m("abrupt"))$passed)
  expect_false(mudanca_textural_abrupta(m("gradual"))$passed)
  r <- mudanca_textural_abrupta(m(NA))
  expect_true(is.na(r$passed))
  expect_true("boundary_distinctness" %in% r$missing)
})

test_that("the B textural is undecided where the clay of the A or B is not measured", {
  skip_on_cran()
  p <- .g2(c("A", "Bt1", "Bt2"), c(0, 20, 60), c(20, 60, 120),
           clay_pct = c(NA, 40, 45), silt_pct = c(NA, 20, 20))
  r <- B_textural(p)
  expect_true(is.na(r$passed))
  expect_true("clay_pct" %in% r$missing)
  # measured and well below the ratio, with no other item: absent
  q <- .g2(c("A", "Bw1", "Bw2"), c(0, 20, 60), c(20, 60, 120),
           clay_pct = c(30, 33, 34), silt_pct = c(20, 20, 20),
           boundary_distinctness = c("gradual", "diffuse", NA))
  expect_false(B_textural(q)$passed)
})

test_that("item (i) of the B textural is undecided with cerosidade and no structure", {
  skip_on_cran()
  p <- .g2(c("A", "Bt1", "Bt2"), c(0, 20, 60), c(20, 60, 120),
           clay_pct = c(30, 38, 40), silt_pct = c(20, 20, 20),
           boundary_distinctness = c("gradual", "diffuse", NA),
           clay_films_amount = c(NA, "comum", "comum"))
  expect_true(is.na(B_textural(p)$passed))
})

test_that("the A chernozemico is undecided when a colour, the grade or V is not recorded", {
  skip_on_cran()
  a <- function(...) .g2(c("A1", "A2", "C"), c(0, 15, 30), c(15, 30, 100),
                         oc_pct = c(2, 1.5, 0.3), bs_pct = c(80, 75, 70),
                         munsell_value_moist = c(2, 3, 5),
                         munsell_chroma_moist = c(2, 2, 4), ...)
  full <- a(munsell_value_dry = c(4, 5, 6),
            structure_grade = c("moderada", "forte", NA))
  expect_true(horizonte_A_chernozemico(full)$passed)
  no_dry <- a(structure_grade = c("moderada", "forte", NA))
  expect_true(is.na(horizonte_A_chernozemico(no_dry)$passed))
  no_grade <- a(munsell_value_dry = c(4, 5, 6))
  expect_true(is.na(horizonte_A_chernozemico(no_grade)$passed))
  light <- a(munsell_value_dry = c(6, 6, 6),
             structure_grade = c("moderada", "forte", NA))
  expect_false(horizonte_A_chernozemico(light)$passed)
})

test_that("the histico is undecided where O layers without carbon could make it", {
  skip_on_cran()
  p <- .g2(c("H1", "H2", "C"), c(0, 10, 40), c(10, 40, 100),
           oc_pct = c(20, NA, 0.5))
  expect_true(is.na(horizonte_histico(p)$passed))
  q <- .g2(c("H1", "H2", "C"), c(0, 10, 40), c(10, 40, 100),
           oc_pct = c(20, 15, 0.5))
  expect_true(horizonte_histico(q)$passed)
})

test_that("the B planico is undecided where the colour of the first B is not recorded", {
  skip_on_cran()
  p <- .g2(c("A", "Btg", "C"), c(0, 20, 70), c(20, 70, 120),
           clay_pct = c(8, 35, 30), silt_pct = c(10, 20, 20),
           boundary_distinctness = c("abrupt", NA, NA))
  expect_true(is.na(B_planico(p)$passed))
  q <- .g2(c("A", "Btg", "C"), c(0, 20, 70), c(20, 70, 120),
           clay_pct = c(8, 35, 30), silt_pct = c(10, 20, 20),
           boundary_distinctness = c("abrupt", NA, NA),
           munsell_hue_moist = c("10YR", "10YR", "10YR"),
           munsell_value_moist = c(4, 5, 6), munsell_chroma_moist = c(2, 2, 3))
  expect_true(B_planico(q)$passed)
})

test_that("the decomposition classes are undecided without fibre or von Post", {
  skip_on_cran()
  p <- .g2(c("H1", "H2", "C"), c(0, 20, 60), c(20, 60, 100),
           oc_pct = c(25, 20, 1))
  expect_true(is.na(saprico(p)$passed))
  q <- .g2(c("H1", "H2", "C"), c(0, 20, 60), c(20, 60, 100),
           oc_pct = c(25, 20, 1), von_post_index = c(8, 8, NA))
  expect_true(saprico(q)$passed)
  expect_false(fibrico(q)$passed)
})

test_that("the Neossolos exclusions are three-valued", {
  skip_on_cran()
  # no B and a glei test left undecided within 50 cm: undecided, not Neossolo
  p <- make_neossolo_canonical()
  r <- soilKey:::neossolo(p)
  expect_true(is.logical(r$passed))
  expect_true(all(r$evidence$exclusions_undecided %in%
                    c("glei", "plintico", "vertico", "chernozemico_carbonatico")))
})

test_that("a CaCO3 not determined is no carbonate", {
  skip_on_cran()
  p <- .g2(c("A", "C"), c(0, 30), c(30, 100), clay_pct = c(20, 20))
  expect_false(soilKey:::carater_carbonatico(p)$passed)
  expect_false(soilKey:::carater_hipocarbonatico(p)$passed)
  expect_false(horizonte_calcico(p)$passed)
  q <- .g2(c("A", "C"), c(0, 30), c(30, 100), clay_pct = c(20, 20),
           caco3_pct = c(NA, 20))
  expect_true(soilKey:::carater_carbonatico(q)$passed)
})
