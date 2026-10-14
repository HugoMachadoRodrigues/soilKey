# v0.9.256: more marks of the older notation of horizons.
#
# Since v0.9.250 the "A2" of a profile written in the notation in use until
# 1988 is read as the E of today, and its "B3" as the BC. The profile was
# known to be in that notation by a B followed at once by a number ("B1",
# "B21t") or by a "(B)". A podzol described "A1, A21, A22, Bhir" has neither,
# and its A2 was read as a part of the A: with no E above it, its Bhir was no
# B espodico by item (a) of the book (v0.9.255).
#
# Decided on 2026-10-13: an A or a C followed at once by two figures, the
# suffix "ir" in a B and the suffix "pl" in a B or a C are marks of that
# notation too. The suffix "cn" is not.

test_that("two figures after an A or a C mark the older notation", {
  expect_true(.desg_old_notation(c("A11", "A12", "A2", "Bt1", "Bt2")))
  expect_true(.desg_old_notation(c("A1", "A21", "A22", "Bh")))
  expect_true(.desg_old_notation(c("A", "C11", "C12")))
  expect_true(.desg_old_notation(c("A1", "IIC21")))
  # one figure, or figures after the letters, are of the present notation
  expect_false(.desg_old_notation(c("A1", "A2", "Bt1", "Bt2", "C1", "C2")))
  expect_false(.desg_old_notation(c("Ap1", "Ap2", "Bw1", "Bw2")))
  expect_false(.desg_old_notation(c("A", "Bi", "Cr1", "Cr2")))
})

test_that("the suffixes 'ir' in a B and 'pl' in a B or a C mark it", {
  expect_true(.desg_old_notation(c("A1", "A2", "Bh", "Bhir")))
  expect_true(.desg_old_notation(c("A", "Bir")))
  expect_true(.desg_old_notation(c("A1", "A2", "A3", "Bpl")))
  expect_true(.desg_old_notation(c("A", "Bt", "2Cpl")))
  # "pl" in an A is no mark, nor "ir" in a C
  expect_false(.desg_old_notation(c("Apl", "Bt")))
  expect_false(.desg_old_notation(c("A", "Bi", "Cir")))
})

test_that("the suffix 'cn' is no mark of it", {
  d <- c("A1", "A2", "AB", "BA", "Bw1", "Bw2", "BC", "C1", "Ccn")
  expect_false(.desg_old_notation(d))
  expect_equal(.desg_master(d), d)       # the A2 stays the second part of the A
})

test_that("in a profile so marked the A2 is the E and the B3 the BC", {
  expect_equal(.desg_master(c("A1", "A21", "A22", "Bhir")), c("A1", "E1", "E2", "Bhir"))
  expect_equal(.desg_master(c("A1", "A2", "Bh", "Bhir")), c("A1", "E", "Bh", "Bhir"))
  expect_equal(.desg_master(c("A11", "A12", "A2", "Bpl", "B3pl")),
               c("A11", "A12", "E", "Bpl", "BCpl"))
  # the parts of the A1 are not the E
  expect_equal(.desg_master(c("A11", "A12", "C11")), c("A11", "A12", "C11"))
})

test_that("a podzol described A1, A21, A22, Bhir has its E above the B espodico", {
  skip_on_cran()
  hz <- data.table::data.table(
    designation = c("A1", "A21", "A22", "Bhir"),
    top_cm = c(0, 35, 70, 100), bottom_cm = c(35, 70, 100, 160),
    oc_pct = c(1.2, 0.2, 0.2, 1.5), clay_pct = c(2, 2, 2, 6), silt_pct = 3,
    sand_pct = c(95, 95, 95, 91),
    munsell_hue_moist = c("10YR", "10YR", "10YR", "7.5YR"),
    munsell_value_moist = c(3, 7, 8, 3), munsell_chroma_moist = c(1, 1, 1, 2),
    ph_h2o = 4.6)
  p <- PedonRecord$new(site = list(id = "pz", country = "BR"),
                       horizons = ensure_horizon_schema(hz))
  r <- B_espodico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 4L)
  expect_equal(r$evidence$item_a$e_above, 3L)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos")
  # the same sheet with its horizons written "A1, A2, A3, B": no mark, no E
  q <- p$clone(deep = TRUE)
  hq <- data.table::copy(q$horizons); hq$designation <- c("A1", "A2", "A3", "B"); q$horizons <- hq
  expect_false(isTRUE(B_espodico(q)$passed))
})
