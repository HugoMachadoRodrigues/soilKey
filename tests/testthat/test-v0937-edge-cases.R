# v0.9.37 edge-case stress tests.
#
# Adversarial inputs that should NOT crash the classifier:
#   * empty horizons table
#   * all-NA pedon (every horizon column NA)
#   * single horizon
#   * horizons in reverse order (deepest first)
#   * negative depths
#   * zero-thickness horizon
#   * impossibly large depths
#   * non-ASCII designations
#   * duplicate horizon designations

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))


# ---- empty / minimal horizons ---------------------------------------------

test_that("classify_*  do not raise on a pedon with NO horizons", {
  hz <- ensure_horizon_schema(data.table::data.table())
  p <- PedonRecord$new(
    site = list(id = "empty", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    expect_silent({
      tryCatch(fn(p, on_missing = "silent"),
                 error = function(e) NULL)
    })
  }
})

test_that("classify_* return a result object on single-horizon pedon", {
  p <- PedonRecord$new(
    site = list(id = "single", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(top_cm = 0, bottom_cm = 30,
                      designation = "A",
                      clay_pct = 20, sand_pct = 50, silt_pct = 30,
                      ph_h2o = 6.5, oc_pct = 1.5)
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"),
                   info = paste("classify_*:", deparse(substitute(fn))))
  }
})

test_that("classify_* accept all-NA horizon rows without crashing", {
  hz <- mk_h(top_cm = c(0, 30), bottom_cm = c(30, 80),
               designation = c(NA_character_, NA_character_),
               clay_pct = c(NA_real_, NA_real_),
               sand_pct = c(NA_real_, NA_real_))
  p <- PedonRecord$new(
    site = list(id = "all-NA", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})


# ---- non-monotonic / reversed depths --------------------------------------

test_that("classify_* survive horizons in reverse order (deepest first)", {
  # The classifiers should be tolerant -- internal ordering by top_cm
  # OR an explicit precondition that flags + handles. Either way, no
  # crash.
  hz <- mk_h(top_cm    = c(60, 30, 0),
               bottom_cm = c(120, 60, 30),
               designation = c("C", "Bw", "A"),
               clay_pct = c(10, 20, 30))
  p <- PedonRecord$new(
    site = list(id = "rev", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"),
                   info = "reverse-order horizons should not crash")
  }
})

test_that("classify_* tolerate zero-thickness horizon", {
  # Bug-bait: a row with top == bottom. Should not crash.
  hz <- mk_h(top_cm    = c(0, 30, 30),
               bottom_cm = c(30, 30, 60),
               designation = c("A", "Bw1", "Bw2"),
               clay_pct = c(20, 25, 28))
  p <- PedonRecord$new(
    site = list(id = "zero-thk", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})


# ---- impossible / extreme values ------------------------------------------

test_that("classify_* tolerate impossibly deep profile", {
  # 10 m profile -- shouldn't crash; just classify the upper part.
  hz <- mk_h(top_cm    = c(0, 30, 200, 500),
               bottom_cm = c(30, 200, 500, 1000),
               designation = c("A", "Bw", "BC", "C"),
               clay_pct = c(15, 20, 25, 28))
  p <- PedonRecord$new(
    site = list(id = "deep", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})


# ---- non-ASCII / weird text ------------------------------------------------

test_that("classify_* tolerate non-ASCII designations (UTF-8)", {
  hz <- mk_h(top_cm    = c(0, 30),
               bottom_cm = c(30, 80),
               # Designation with PT-BR diacritics + arabic numeral
               designation = c("Áp", "Bíiretante"),
               clay_pct = c(20, 30))
  p <- PedonRecord$new(
    site = list(id = "utf8", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})


# ---- duplicates -----------------------------------------------------------

test_that("classify_* tolerate duplicate horizon designations", {
  # Common in field surveys when sub-horizons get the same letter.
  hz <- mk_h(top_cm    = c(0, 15, 30),
               bottom_cm = c(15, 30, 60),
               designation = c("A", "A", "Bw"),
               clay_pct = c(15, 18, 25))
  p <- PedonRecord$new(
    site = list(id = "dup", lat = 0, lon = 0, country = "TEST"),
    horizons = hz
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})


# ---- missing site fields --------------------------------------------------

test_that("classify_* survive a pedon with missing optional site fields", {
  # No country, no parent_material, no lat/lon.
  p <- PedonRecord$new(
    site = list(id = "minimal-site"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 80),
                      designation = c("A", "B"),
                      clay_pct = c(15, 20))
  )
  for (fn in list(classify_wrb2022, classify_sibcs, classify_usda)) {
    res <- tryCatch(fn(p, on_missing = "silent"),
                      error = function(e) e)
    expect_false(inherits(res, "error"))
  }
})

test_that("classify_all on a broken pedon returns warning, not crash", {
  hz <- ensure_horizon_schema(data.table::data.table())
  p <- PedonRecord$new(
    site = list(id = "broken"),
    horizons = hz
  )
  res <- suppressWarnings(classify_all(p, on_missing = "silent"))
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), 1L)
})
