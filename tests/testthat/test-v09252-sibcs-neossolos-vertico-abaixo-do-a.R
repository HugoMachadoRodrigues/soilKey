# v0.9.252: item (c) of the Neossolos step by its letter.
#
# SiBCS 2018, Cap 4, p. 112: Neossolos are soils with no diagnostic B that
# satisfy, among others, "c) Ausencia de horizonte vertico imediatamente
# abaixo de horizonte A". The step tested a vertic horizon that begins
# within 50 cm, whatever lay between it and the A. Decided on 2026-10-13: by
# the letter, with the horizons of transition from the A (AB, AC) counted
# with the A.

.nv <- function(designation, top, bottom, clay, slickensides = NA_character_, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = clay, silt_pct = 25, sand_pct = 75 - clay,
    slickensides = rep_len(slickensides, n), ...)
  PedonRecord$new(site = list(id = "nv", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("a vertic horizon right below the A keeps the profile out of the Neossolos", {
  p <- .nv(c("A", "Cv1", "Cv2"), c(0, 20, 60), c(20, 60, 120), c(40, 45, 45),
           slickensides = c(NA, "Comum", "Comum"))
  n <- neossolo(p)
  expect_true(n$evidence$vertico_below_a)
  expect_false(isTRUE(n$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos")
})

test_that("with a C between the A and the vertic horizon, the item is satisfied", {
  # the vertic horizon begins at 35 cm, below a C1 with few slickensides: it
  # begins within 50 cm, which the step used to test, and is not right below
  # the A
  p <- .nv(c("A", "C1", "C2", "C3"), c(0, 15, 35, 80), c(15, 35, 80, 130), c(40, 42, 45, 45),
           slickensides = c(NA, "Pouca", "Comum", "Comum"))
  expect_equal(soilKey:::horizonte_vertico(p)$layers, 3:4)
  n <- neossolo(p)
  expect_false(n$evidence$vertico_below_a)
  expect_true(isTRUE(n$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Neossolos")
})

test_that("the horizons of transition from the A count with the A", {
  for (tr in c("AC", "AB", "A/C", "A3")) {
    p <- .nv(c("A", tr, "Cv"), c(0, 15, 35), c(15, 35, 120), c(40, 42, 45),
             slickensides = c(NA, NA, "Comum"))
    n <- neossolo(p)
    expect_true(n$evidence$vertico_below_a, info = tr)
    expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos", info = tr)
  }
  # an E, or a C, does not
  for (ab in c("E", "C")) {
    p <- .nv(c("A", ab, "Cv"), c(0, 15, 35), c(15, 35, 120), c(40, 42, 45),
             slickensides = c(NA, NA, "Comum"))
    expect_false(neossolo(p)$evidence$vertico_below_a, info = ab)
  }
})

test_that("a vertic horizon that begins at the surface is there too, and a profile with no designation is tested as before", {
  p <- .nv(c("Av", "Cv"), c(0, 30), c(30, 120), c(45, 45), slickensides = "Comum")
  expect_true(neossolo(p)$evidence$vertico_below_a)
  q <- .nv(rep(NA_character_, 3), c(0, 15, 35), c(15, 35, 120), c(40, 42, 45),
           slickensides = c(NA, NA, "Comum"))
  expect_true(neossolo(q)$evidence$vertico_below_a)
  r <- .nv(rep(NA_character_, 3), c(0, 15, 70), c(15, 70, 120), c(40, 42, 45),
           slickensides = c(NA, NA, "Comum"))
  expect_false(neossolo(r)$evidence$vertico_below_a)
})

test_that("a vertic horizon with the colours of the glei is still a glei horizon", {
  # the suborder of the Vertissolos Hidromorficos asks for it
  hz <- data.table::data.table(
    designation = c("A", "Cg1", "Cg2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
    clay_pct = 45, silt_pct = 25, sand_pct = 30,
    munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(3, 5, 5),
    munsell_chroma_moist = c(3, 1, 1), slickensides = c(NA, "Comum", "Comum"))
  p <- PedonRecord$new(site = list(id = "g", country = "BR", drainage = "Mal drenado"),
                       horizons = ensure_horizon_schema(hz))
  expect_equal(soilKey:::horizonte_vertico(p)$layers, 2:3)
  expect_equal(soilKey:::horizonte_glei(p)$layers, 2:3)
  r <- classify_sibcs(p, on_missing = "silent")
  expect_equal(r$rsg_or_order, "Vertissolos")
  expect_match(r$name, "Hidrom")
})

test_that("the canonical profiles keep their order", {
  expect_equal(classify_sibcs(make_vertissolo_canonical(), on_missing = "silent")$rsg_or_order, "Vertissolos")
  expect_equal(classify_sibcs(make_nitossolo_canonical(), on_missing = "silent")$rsg_or_order, "Nitossolos")
  expect_equal(classify_sibcs(make_gleissolo_canonical(), on_missing = "silent")$rsg_or_order, "Gleissolos")
  expect_equal(classify_sibcs(make_neossolo_canonical(), on_missing = "silent")$rsg_or_order, "Neossolos")
})
