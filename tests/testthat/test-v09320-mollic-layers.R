# v0.9.314: the WRB 2022 mollic and umbric horizons read layer by layer
# (Ch 2.1, p. 23: every criterion except thickness in every subhorizon).
# Made-up profiles.

.v09320 <- function(top = c(0, 25), bottom = c(25, 80), designation = c("Ah", "Bw"),
                    vm = c(2, 3), cm = c(2, 2), vd = c(3, 4), oc = c(3, 1.5),
                    bs = 30, type = c("granular", "subangular blocky"),
                    size = c("fine", "medium"), clay = 25, sand = 45,
                    caco3 = NA_real_) {
  n <- length(top)
  h <- data.frame(top_cm = top, bottom_cm = bottom, designation = designation,
                  munsell_hue_moist = "10YR", munsell_value_moist = vm,
                  munsell_chroma_moist = cm, munsell_hue_dry = "10YR",
                  munsell_value_dry = vd, munsell_chroma_dry = 2,
                  oc_pct = oc, clay_pct = clay, sand_pct = sand,
                  silt_pct = 100 - clay - sand, bs_pct = bs,
                  ph_h2o = ifelse(bs < 50, 5, 6.8), caco3_pct = caco3,
                  structure_type = type, structure_grade = "moderate",
                  structure_size = size, stringsAsFactors = FALSE)
  PedonRecord$new(site = list(id = "made-up"), horizons = h)
}

test_that("every layer of the horizon meets criteria 1-3", {
  skip_on_cran()
  # both layers dark, carbon-rich and structured: one horizon of 80 cm
  u <- umbric_horizon(.v09320())
  expect_true(u$passed)
  expect_identical(sort(as.integer(u$layers)), 1:2)
  # a massive Bw is not part of it; the Ah alone is 25 cm
  u <- umbric_horizon(.v09320(type = c("granular", "massive"), size = c("fine", NA)))
  expect_true(u$passed)
  expect_identical(as.integer(u$layers), 1L)
  # a massive surface layer: no horizon, whatever is undecided below
  u <- umbric_horizon(.v09320(type = c("massive", "granular"), vm = c(NA, 3), vd = c(NA, 4)))
  expect_false(u$passed)
  # carbon in every layer: 0.4% in the Bw ends the horizon at 15 cm
  expect_false(isTRUE(umbric_horizon(.v09320(top = c(0, 15), bottom = c(15, 80), oc = c(3, 0.4)))$passed))
  # the mollic horizon the same way
  m <- mollic(.v09320(bs = 70, type = c("granular", "massive"), size = c("fine", NA)))
  expect_true(m$passed)
  expect_identical(as.integer(m$layers), 1L)
  expect_identical(as.integer(m$evidence$thickness$details$layers), 1L)
})

test_that("the colour follows criterion 3 and its items", {
  skip_on_cran()
  col <- function(p, umbric = TRUE) as.logical(soilKey:::.wrb_mollic_colour_layers(p$horizons, umbric = umbric))
  # a. value <= 3 moist and <= 5 dry, chroma <= 3 moist
  expect_identical(col(.v09320()), c(TRUE, TRUE))
  expect_identical(col(.v09320(vd = c(6, 4)))[1], FALSE)
  # no moist colour: a dry value of 5 leaves it undecided, 6 fails it
  expect_true(is.na(col(.v09320(vm = c(NA, 3), cm = c(NA, 2), vd = c(5, 4)))[1]))
  expect_false(col(.v09320(vm = c(NA, 3), cm = c(NA, 2), vd = c(6, 4)))[1])
  # a moist value and no dry value: the dry value is read as moist + 1
  expect_true(col(.v09320(vd = c(NA, 4)))[1])
  # umbric b / mollic c: loamy sand, value 4 moist, >= 2.5% SOC
  ls <- .v09320(vm = c(4, 3), vd = c(6, 4), clay = 5, sand = 85)
  expect_true(col(ls)[1])
  expect_true(col(ls, umbric = FALSE)[1])
  expect_false(col(.v09320(vm = c(4, 3), vd = c(6, 4)))[1])
  # mollic b: CaCO3 >= 15 and < 40%, value and chroma <= 3 moist, >= 1% SOC
  b <- .v09320(vd = c(6, 6), oc = c(1.2, 1.2), caco3 = 20)
  expect_identical(col(b, umbric = FALSE), c(TRUE, TRUE))
  expect_identical(col(b), c(FALSE, FALSE))
  # no colour at all: >= 1.5% SOC is read as dark (Hugo, 2026-10-29)
  nc <- .v09320(vm = c(NA, NA), cm = c(NA, NA), vd = c(NA, NA), oc = c(2, 1))
  expect_identical(col(nc), c(TRUE, NA))
  expect_identical(attr(soilKey:::.wrb_mollic_colour_layers(nc$horizons), "source")[1],
                   "no colour recorded, >= 1.5% SOC (proxy)")
  # each layer records where its colour reading came from
  src <- attr(soilKey:::.wrb_mollic_colour_layers(.v09320(vd = c(NA, 4))$horizons), "source")
  expect_identical(src, c("moist colour, dry value as moist + 1", "colour as recorded"))
})

test_that("a plough layer takes the colour of the subhorizon below it", {
  skip_on_cran()
  p <- .v09320(top = c(0, 20), bottom = c(20, 45), designation = c("Ap", "A2"),
               vm = c(4, 3), vd = c(5, 4))
  s <- soilKey:::.wrb_mollic_layer_status(p$horizons, umbric = TRUE)
  expect_identical(s$colour, c(TRUE, TRUE))
  expect_true(umbric_horizon(p)$passed)
  # not so for an A that is no plough layer
  q <- .v09320(top = c(0, 20), bottom = c(20, 45), designation = c("A1", "A2"),
               vm = c(4, 3), vd = c(5, 4))
  expect_false(umbric_horizon(q)$passed)
})

test_that("undecided layers leave the horizon undecided only if they could make it", {
  skip_on_cran()
  # the Ah is 10 cm; the Bw has no colour recorded and 1% SOC: thickness undecided
  p <- .v09320(top = c(0, 10), bottom = c(10, 60), vm = c(2, NA), cm = c(2, NA), vd = c(3, NA),
               oc = c(3, 1))
  expect_true(is.na(umbric_horizon(p)$passed))
  # with 2% SOC the Bw is read as dark (no colour recorded) and the horizon is 60 cm
  p2 <- .v09320(top = c(0, 10), bottom = c(10, 60), vm = c(2, NA), cm = c(2, NA), vd = c(3, NA),
                oc = c(3, 2))
  expect_true(umbric_horizon(p2)$passed)
  # the same with a massive Bw: the horizon is the 10 cm Ah, too thin
  q <- .v09320(top = c(0, 10), bottom = c(10, 60), vm = c(2, NA), cm = c(2, NA), vd = c(3, NA),
               oc = c(3, 1), type = c("granular", "massive"), size = c("fine", NA))
  expect_false(umbric_horizon(q)$passed)
})
