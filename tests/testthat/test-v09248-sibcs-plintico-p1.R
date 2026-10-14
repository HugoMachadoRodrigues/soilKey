# v0.9.248: the "p1" of B and C horizons is the "pl" of plinthite.
#
# The surveys before the present notation wrote plinthite "pl" (B21pl, Cpl),
# which the morphological mode reads since v0.9.234. The BDsolos export also
# has it as "p1", the letter "l" written as the figure 1: B2tp1, B22tp1, Cp1,
# B2tp1cn, in 219 horizons of 145 profiles, 135 of them with "plintico",
# "laterita" or "concrecionario" in their reference name. No B or C of the
# export is written with "p2": the "p1" is not the first of a series.

.p1_morph <- function(on) options(soilKey.morphological_inference = on)

.p1_profile <- function(designation) {
  # the canonical Plintossolo without the volume of plinthite, which no survey
  # of the export records
  p <- make_plintossolo_canonical()
  h <- data.table::copy(p$horizons)
  h$plinthite_pct <- NA_real_
  h$designation <- designation
  p$horizons <- h
  p
}

test_that("a 'p1' in a B or C horizon is plinthite in the morphological mode", {
  skip_on_cran()
  old <- .p1_morph(TRUE); on.exit(options(old), add = TRUE)
  p <- .p1_profile(c("A1", "B2tp1", "Cp1"))
  r <- horizonte_plintico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_equal(r$evidence$designation_inference$source, "designation_pl")
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos")
  for (d in c("B22tp1", "IIB2p1", "B2tp1cn", "(B)22p1", "Btp1"))
    expect_true(isTRUE(horizonte_plintico(.p1_profile(c("A1", d, "Cp1")))$passed), info = d)
})

test_that("the default mode does not read it", {
  old <- .p1_morph(FALSE); on.exit(options(old), add = TRUE)
  expect_false(isTRUE(horizonte_plintico(.p1_profile(c("A1", "B2tp1", "Cp1")))$passed))
})

test_that("a ploughed layer, and the first of a numbered series, are not plinthite", {
  old <- .p1_morph(TRUE); on.exit(options(old), add = TRUE)
  # "Ap1" is a ploughed layer
  expect_false(isTRUE(horizonte_plintico(.p1_profile(c("Ap1", "Bt", "C")))$passed))
  expect_false(isTRUE(horizonte_plintico(.p1_profile(c("Ap1", "Ap2", "C")))$passed))
  # "Bp1" beside "Bp2": a series, not "pl"
  expect_false(isTRUE(horizonte_plintico(.p1_profile(c("Ap", "Bp1", "Bp2")))$passed))
})

test_that("it has to make the 15 cm of the horizon", {
  old <- .p1_morph(TRUE); on.exit(options(old), add = TRUE)
  thin <- .p1_profile(c("A1", "B2tp1", "C"))
  h <- data.table::copy(thin$horizons); h$top_cm <- c(0, 20, 30); h$bottom_cm <- c(20, 30, 150)
  thin$horizons <- h
  expect_false(isTRUE(horizonte_plintico(thin)$passed))
})

test_that("'pl' and 'f' are read as before, and the canonical Plintossolo keeps its class", {
  skip_on_cran()
  old <- .p1_morph(TRUE); on.exit(options(old), add = TRUE)
  expect_true(isTRUE(horizonte_plintico(.p1_profile(c("A1", "B21pl", "B22pl")))$passed))
  expect_true(isTRUE(horizonte_plintico(.p1_profile(c("A1", "Btf", "Cf")))$passed))
  .p1_morph(FALSE)
  expect_equal(classify_sibcs(make_plintossolo_canonical(), on_missing = "silent")$rsg_or_order,
               "Plintossolos")
})
