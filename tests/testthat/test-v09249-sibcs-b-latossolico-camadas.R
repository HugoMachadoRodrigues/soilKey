# v0.9.249: the horizons the B latossolico does not present take out their
# own layers, not the B latossolico of the whole profile.
#
# SiBCS 2018 (Cap 2, p. 59): "o horizonte B latossolico e um horizonte
# subsuperficial que nao apresenta caracteristicas diagnosticas de horizontes
# glei, B textural, B nitico e plintico". The sentence is about the horizon.
# The test voided the B latossolico of the profile when gleyic properties of
# WRB (mottles of 5% or more in a layer that begins within 50 cm), a plinthic
# horizon or a B nitico were found in any layer. Decided on 2026-10-13: the
# layers of those horizons, as SiBCS defines them, are no part of the
# B latossolico; what is left makes it if it makes its 50 cm.

.lc <- function(designation, top, bottom, clay = 50, cec = 4, drainage = NA_character_, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = rep_len(clay, n), silt_pct = 15, sand_pct = 85 - rep_len(clay, n),
    cec_cmol = rep_len(cec, n), ...)
  PedonRecord$new(site = list(id = "lc", country = "BR", lat = -20, lon = -44,
                              drainage = drainage),
                  horizons = ensure_horizon_schema(hz))
}

test_that("mottles in a layer near the surface do not void the B latossolico", {
  skip_on_cran()
  # 10% of mottles from the B1 down, in a red B: gleyic properties for WRB,
  # no horizonte glei
  p <- .lc(c("A", "Bw1", "Bw2", "Bw3"), c(0, 20, 60, 120), c(20, 60, 120, 200),
           cec = c(8, 4, 4, 4),
           munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6,
           redoximorphic_features_pct = c(0, 10, 10, 10))
  expect_true(isTRUE(gleyic_properties(p)$passed))
  expect_false(isTRUE(horizonte_glei(p)$passed))
  r <- B_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:4)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Latossolos")
})

test_that("a glei horizon below the B latossolico takes out its own layers", {
  p <- .lc(c("A", "Bw1", "Bw2", "Cg"), c(0, 20, 70, 130), c(20, 70, 130, 200),
           cec = c(8, 4, 4, 4), drainage = "Mal drenado",
           munsell_hue_moist = c("10YR", "2.5YR", "2.5YR", "2.5Y"),
           munsell_value_moist = c(3, 4, 4, 6), munsell_chroma_moist = c(3, 6, 6, 1))
  expect_equal(horizonte_glei(p)$layers, 4L)
  r <- B_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_equal(r$evidence$excluded_by$glei$layers, 4L)
})

test_that("a plinthic layer inside the B is no part of it; what is left has to make 50 cm", {
  skip_on_cran()
  p <- .lc(c("A", "Bw1", "Bwf", "Bw2"), c(0, 20, 80, 100), c(20, 80, 100, 180),
           cec = c(8, 4, 4, 4), plinthite_pct = c(0, 0, 30, 0))
  expect_equal(horizonte_plintico(p)$layers, 3L)
  r <- B_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, c(2L, 4L))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Latossolos")
  # 40 cm above the plinthic layer and 40 cm below it: neither makes 50 cm
  q <- .lc(c("A", "Bw1", "Bwf", "Bw2"), c(0, 20, 60, 100), c(20, 60, 100, 140),
           cec = c(8, 4, 4, 4), plinthite_pct = c(0, 0, 30, 0))
  expect_false(isTRUE(B_latossolico(q)$passed))
})

test_that("a B latossolico below a B nitico makes no Latossolo", {
  skip_on_cran()
  # a B nitico with its cerosidade in the lower B (so that it is no
  # B textural by item i) over a less clayey layer that meets the
  # B latossolico: the first diagnostic horizon prevails (Cap 4, p. 110)
  p <- make_nitossolo_canonical()
  h <- data.table::copy(p$horizons)
  h$designation <- c("A", "Bt1", "Bt2", "Bw")
  h$top_cm <- c(0, 20, 150, 200); h$bottom_cm <- c(20, 150, 200, 270)
  h$clay_films_amount <- c(NA, NA, "many", NA)
  h$clay_pct <- c(45, 55, 60, 30); h$silt_pct <- c(25, 23, 20, 20)
  h$sand_pct <- 100 - h$clay_pct - h$silt_pct
  h$cec_cmol <- c(8, 7, 6, 3)
  p$horizons <- h
  expect_false(isTRUE(B_textural(p)$passed))
  expect_equal(B_nitico(p)$layers, 2:3)
  bl <- B_latossolico(p)
  expect_true(isTRUE(bl$passed))
  expect_equal(bl$layers, 4L)
  l <- latossolo(p)
  expect_true(l$evidence$below_b_nitico)
  expect_false(isTRUE(l$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Nitossolos")
})

test_that("the canonical profiles keep their order", {
  skip_on_cran()
  expect_equal(classify_sibcs(make_latossolo_canonical(), on_missing = "silent")$rsg_or_order, "Latossolos")
  expect_equal(classify_sibcs(make_nitossolo_canonical(), on_missing = "silent")$rsg_or_order, "Nitossolos")
  expect_equal(classify_sibcs(make_gleissolo_canonical(), on_missing = "silent")$rsg_or_order, "Gleissolos")
  expect_equal(classify_sibcs(make_plintossolo_canonical(), on_missing = "silent")$rsg_or_order, "Plintossolos")
  expect_equal(classify_sibcs(make_argissolo_canonical(), on_missing = "silent")$rsg_or_order, "Argissolos")
})
