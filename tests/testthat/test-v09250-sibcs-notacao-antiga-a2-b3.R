# v0.9.250: in a profile of the older notation the A2 is the E and the B3 is
# the BC.
#
# The notation in use in Brazil until 1988 named the eluvial horizon "A2"
# and the horizon of transition from the B to the C "B3"; the present one
# writes them "E" and "BC". SiBCS asks about both by their present names: an
# E above the B makes a B textural (Cap 2, p. 55, item f), and the BC is left
# out of the B in the textural ratio (p. 56, note 4). The key read the A2 as
# an A and the B3 as a B. Decided on 2026-10-13: both are read by what they
# name, in the profiles of the older notation.

.on <- function(designation, top, bottom, clay, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = clay, silt_pct = 20, sand_pct = 80 - clay, ...)
  PedonRecord$new(site = list(id = "on", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("a profile is of the older notation by a B followed at once by a number, or a '(B)'", {
  expect_true(.desg_old_notation(c("A1", "A3", "B1", "B21", "B22", "B3", "C")))
  expect_true(.desg_old_notation(c("A1", "IIB2t", "IIC")))
  expect_true(.desg_old_notation(c("A", "(B)", "C")))
  expect_true(.desg_old_notation(c("A", "II(B)2", "C")))
  expect_false(.desg_old_notation(c("A", "AB", "BA", "Bt1", "Bt2", "BC", "C")))
  expect_false(.desg_old_notation(c("A1", "A2", "Bw1", "Bw2")))
  expect_false(.desg_old_notation(c("A", "C1", "C2")))
  expect_false(.desg_old_notation(c(NA_character_, NA_character_)))
  expect_false(.desg_old_notation(character(0)))
})

test_that("in such a profile the A2 is read E and the B3 is read BC", {
  expect_equal(.desg_master(c("A1", "A2", "B1", "B21t", "IIB3", "C")),
               c("A1", "E", "B1", "B21t", "BC", "C"))
  expect_equal(.desg_master(c("A1", "A21", "A22", "(B)1", "B3t")),
               c("A1", "E1", "E2", "Bi1", "BCt"))
  # in the present notation "A1, A2" are two parts of an A
  expect_equal(.desg_master(c("A1", "A2", "Bt1", "Bt2")), c("A1", "A2", "Bt1", "Bt2"))
  # one designation alone says nothing of its profile, unless it is given
  expect_equal(.desg_master("A2"), "A2")
  expect_equal(.desg_master("A2", profile = c("A1", "A2", "B2t")), "E")
  expect_equal(.desg_master(c("A3", "B1")), c("A3", "B1"))
})

test_that("an A2 above the B makes a B textural by item (f)", {
  skip_on_cran()
  # A1 12%, A2 10%, B2t 16%: a ratio of 1.45, under the 1.80 of item (h), no
  # abrupt change and no cerosidade
  p <- .on(c("A1", "A2", "B21t", "B22t", "C"), c(0, 15, 40, 80, 130), c(15, 40, 80, 130, 180),
           c(12, 10, 16, 17, 14), boundary_distinctness = c("clear", "gradual", "diffuse", "gradual", NA))
  r <- B_textural(p)
  expect_true(isTRUE(r$passed))
  expect_true(r$evidence$item_f)
  expect_false(r$evidence$item_h)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Argissolos")
  # the same horizons in the present notation, where A2 is a part of the A
  q <- .on(c("A1", "A2", "Bt1", "Bt2", "C"), c(0, 15, 40, 80, 130), c(15, 40, 80, 130, 180),
           c(12, 10, 16, 17, 14), boundary_distinctness = c("clear", "gradual", "diffuse", "gradual", NA))
  expect_false(isTRUE(B_textural(q)$evidence$item_f))
})

test_that("a B3 is left out of the B in the textural ratio", {
  # A 20%; B21 30%, B22 32%, B3 60%: with the B3 the mean of the B is 40.7%
  # and the ratio 2.03; without it, 31% and 1.55, under the 1.70 of item (h)
  p <- .on(c("A1", "B21", "B22", "B3", "C"), c(0, 20, 30, 40, 50), c(20, 30, 40, 50, 120),
           c(20, 30, 32, 60, 40))
  rt <- test_ratio_textural_sibcs(p$horizons)
  expect_equal(round(rt$details$ratio, 2), 1.55)
  expect_false(isTRUE(rt$passed))
  # written in the present notation with a Bt3, the three count
  q <- .on(c("A", "Bt1", "Bt2", "Bt3", "C"), c(0, 20, 30, 40, 50), c(20, 30, 40, 50, 120),
           c(20, 30, 32, 60, 40))
  expect_true(isTRUE(test_ratio_textural_sibcs(q$horizons)$passed))
})

test_that("a B3 is no first B sub-horizon, as a BC is not", {
  p <- .on(c("A1", "B3", "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30))
  r <- mudanca_textural_abrupta(p)
  expect_false(isTRUE(r$passed))
  expect_true(is.na(r$evidence$first_b))
  # a B2 is
  expect_true(isTRUE(mudanca_textural_abrupta(
    .on(c("A1", "B2", "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30)))$passed))
})

test_that("the canonical profiles, written in the present notation, keep their class", {
  for (f in c("make_argissolo_canonical", "make_latossolo_canonical", "make_planossolo_canonical",
              "make_cambissolo_canonical", "make_espodossolo_canonical")) {
    p <- get(f)()
    expect_false(.desg_old_notation(p$horizons$designation), info = f)
  }
})
