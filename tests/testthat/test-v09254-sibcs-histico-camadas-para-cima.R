# v0.9.254: an O or H layer recorded upward from the mineral soil.
#
# The older descriptions give the organic layers that lie on a mineral soil
# upward from its top: "O 120-0", or "Hd 110-60" above "Hoj 60-0", over a
# "Cg 0-10". With an upper depth greater than the lower, such a layer had a
# thickness below zero and was never part of the horizonte histico: BDsolos
# RJ 14357, an Organossolo Haplico described as "O 120-0" with 17% of organic
# carbon, keyed Neossolos. Decided on 2026-10-13: the layer is read upward,
# with the thickness between its two figures.

.up_profile <- function(designation, top, bottom, oc) {
  hz <- data.table::data.table(designation = designation, top_cm = top,
                               bottom_cm = bottom, oc_pct = oc,
                               clay_pct = 20, silt_pct = 30, sand_pct = 50)
  PedonRecord$new(site = list(id = "up", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("an O recorded upward has the thickness between its two figures", {
  p <- .up_profile("O", 120, 0, 17.3)
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 1L)
  expect_equal(r$evidence$thickness_cm, 120)
  expect_equal(r$evidence$horizons$top_cm, -120)
  expect_equal(r$evidence$horizons$bottom_cm, 0)
  expect_equal(r$evidence$surface_cm, -120)
  expect_equal(r$evidence$recorded_upward, 1L)
  o <- organossolo(p)
  expect_true(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 80)
})

test_that("two H layers recorded upward adjoin and make one horizon over the mineral soil", {
  # as in BDsolos RJ 5333: Cg 0-10, Hoj 60-0, Hd 110-60
  p <- .up_profile(c("Cg", "Hoj", "Hd"), c(0, 60, 110), c(10, 0, 60), c(NA, 21.8, 24.3))
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_setequal(r$layers, 2:3)
  expect_equal(nrow(r$evidence$horizons), 1L)
  expect_equal(r$evidence$thickness_cm, 110)
  expect_equal(r$evidence$surface_cm, -110)
  expect_true(isTRUE(organossolo(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Organossolos")
})

test_that("an H of 30 cm recorded upward is a histic horizon, and under the 40 cm of the key", {
  p <- .up_profile(c("H", "Cg"), c(30, 0), c(0, 60), c(14.2, 1))
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$thickness_cm, 30)
  o <- organossolo(p)
  expect_false(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 30)
})

test_that("the 80 cm of the key are counted from the top of the layers recorded upward", {
  # H 30-0 upward, a mineral layer of 45 cm, a buried H from 45 cm down:
  # the first 80 cm of the soil end 50 cm below the mineral surface
  p <- .up_profile(c("H", "Cg", "2H"), c(30, 0, 45), c(0, 45, 100), c(20, 1, 25))
  r <- horizonte_histico(p)
  expect_equal(r$evidence$horizons$passed, c(TRUE, TRUE))
  expect_equal(r$evidence$surface_cm, -30)
  o <- organossolo(p)
  expect_equal(o$evidence$within_80_cm, 35)
  expect_false(isTRUE(o$passed))
  # with the buried H five centimetres higher, 40 cm
  q <- .up_profile(c("H", "Cg", "2H"), c(30, 0, 40), c(0, 40, 100), c(20, 1, 25))
  expect_equal(organossolo(q)$evidence$within_80_cm, 40)
  expect_true(isTRUE(organossolo(q)$passed))
})

test_that("a thin litter recorded upward is no histic horizon and moves nothing", {
  p <- .up_profile(c("O2", "O1", "A1", "Bw"), c(2, 5, 0, 20), c(0, 2, 20, 120), c(30, 35, 2, 0.5))
  r <- horizonte_histico(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$horizons$thickness_cm, 5)
  expect_false(isTRUE(organossolo(p)$passed))
  expect_equal(p$horizons$top_cm, c(2, 5, 0, 20))
  expect_equal(p$horizons$bottom_cm, c(0, 2, 20, 120))
})

test_that("only an O or H is read upward", {
  # a mineral layer with its depths the wrong way round is a slip of the
  # sheet, not a layer above the soil, whatever its organic carbon
  p <- .up_profile(c("A", "Cg"), c(30, 30), c(0, 90), c(15, 1))
  r <- horizonte_histico(p)
  expect_false(isTRUE(r$passed))
  expect_length(r$layers, 0L)
  # and an O without the organic carbon of organic material is no candidate
  q <- .up_profile(c("O", "A"), c(30, 0), c(0, 40), c(5, 2))
  expect_false(isTRUE(horizonte_histico(q)$passed))
})

test_that("a profile recorded downward is read as before", {
  p <- make_organossolo_canonical()
  r <- horizonte_histico(p)
  expect_equal(r$layers, 1:3)
  expect_equal(r$evidence$surface_cm, 0)
  expect_length(r$evidence$recorded_upward, 0L)
  expect_equal(organossolo(p)$evidence$within_80_cm, 80)
})
