# v0.9.208: a profile with no horizons is refused, not graded A.
#
# Until v0.9.207 classify_all() on a PedonRecord with zero horizons answered
# Haplic Regosol, Neossolos Regoliticos Distroficos tipicos and Typic
# Udorthents, all with evidence grade A: every class failed or passed on
# emptiness, the keys ended at their catch-all groups, and a pedon with no
# provenance recorded is read as measured. SiBCS also asserted low base
# saturation ("Distroficos") with no base saturation at all, because eutrofico()
# with no layer to read answered FALSE and distrofico() is its negation.

.empty_pedon <- function() {
  PedonRecord$new(site = list(id = "empty"),
                  horizons = data.frame(top_cm = numeric(0), bottom_cm = numeric(0)))
}

test_that("each key refuses a profile with no horizons, with a clear error", {
  p <- .empty_pedon()
  expect_error(classify_wrb2022(p, on_missing = "silent"), "no horizons",
               class = "soilKey_no_horizons")
  expect_error(classify_sibcs(p, on_missing = "silent"), "no horizons",
               class = "soilKey_no_horizons")
  expect_error(classify_usda(p, on_missing = "silent"), "no horizons",
               class = "soilKey_no_horizons")
  # the message names the function the user called
  expect_error(classify_usda(p), "classify_usda\\(\\)")
})

test_that("classify_all() reports the refusal per system instead of a class", {
  warns <- character(0)
  res <- withCallingHandlers(
    classify_all(.empty_pedon(), on_missing = "silent"),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_null(res$wrb)
  expect_null(res$sibcs)
  expect_null(res$usda)
  expect_true(all(is.na(unlist(res$summary))))
  expect_length(warns, 3L)
  expect_true(all(grepl("no horizons", warns, fixed = TRUE)))
})

test_that("eutrofico() with nothing to read says 'no data', so distrofico() does not pass", {
  p <- .empty_pedon()
  e <- eutrofico(p)
  expect_true(is.na(e$passed))
  expect_identical(e$missing, "bs_pct")
  expect_true(is.na(distrofico(p)$passed))
})

test_that("a profile with horizons still classifies, and base status still works", {
  # unchanged behaviour: one horizon is enough to run the keys
  p <- PedonRecord$new(site = list(id = "one"),
                       horizons = data.frame(top_cm = 0, bottom_cm = 30,
                                             designation = "A"))
  res <- suppressWarnings(classify_all(p, on_missing = "silent"))
  expect_false(is.null(res$wrb))
  expect_false(is.null(res$sibcs))
  expect_false(is.null(res$usda))
  # with base saturation recorded, eutrofico/distrofico answer as before
  f <- make_ferralsol_canonical()
  expect_false(eutrofico(f)$passed)
  expect_true(distrofico(f)$passed)
  # with base saturation missing in every layer, both are NA, as before
  h <- f$horizons
  h$bs_pct <- NA_real_
  g <- PedonRecord$new(site = f$site, horizons = h)
  expect_true(is.na(eutrofico(g)$passed))
  expect_identical(eutrofico(g)$missing, "bs_pct")
})
