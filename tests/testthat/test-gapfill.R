# Tests for within-pedon depth gap-fill (v0.9.120, front F).

# A profile with an interior NA (H2) and a tail NA below the deepest measured
# layer (H5). Mid-depths: 10, 30, 50, 75, 105.
.gf_partial_pedon <- function() {
  h <- data.frame(
    top_cm    = c(0, 20, 40, 60, 90),
    bottom_cm = c(20, 40, 60, 90, 120),
    clay_pct  = c(15, NA, 35, 40, NA),
    ph_h2o    = c(5.5, 5.8, NA, 6.4, 6.6)
  )
  PedonRecord$new(horizons = h)
}

test_that("gapfill_within_pedon interpolates an interior gap exactly", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  gapfill_within_pedon(p, attrs = "clay_pct")
  # H2 mid=30 sits between measured mids 10 (15%) and 50 (35%) -> 25%.
  expect_equal(p$horizons$clay_pct[2], 25)
  # The other measured cells are untouched.
  expect_equal(p$horizons$clay_pct[c(1, 3, 4)], c(15, 35, 40))
})

test_that("gapfill never extrapolates beyond the measured range", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  gapfill_within_pedon(p, attrs = "clay_pct")
  # H5 mid=105 is below the deepest measured mid (75) -> stays NA.
  expect_true(is.na(p$horizons$clay_pct[5]))
})

test_that("gapfill fills multiple attributes independently", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  gapfill_within_pedon(p, attrs = c("clay_pct", "ph_h2o"))
  # ph_h2o H3 mid=50 between mids 30 (5.8) and 75 (6.4) -> linear.
  expect_equal(p$horizons$ph_h2o[3],
               stats::approx(c(10, 30, 75, 105), c(5.5, 5.8, 6.4, 6.6),
                             xout = 50)$y)
  info <- attr(p, "gapfill_within_pedon")
  expect_setequal(info$attrs, c("clay_pct", "ph_h2o"))
})

test_that("interpolated cells carry inferred_prior provenance => grade C", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  gapfill_within_pedon(p, attrs = "clay_pct")
  expect_true("inferred_prior" %in% p$provenance$source)
  expect_identical(compute_evidence_grade(p, list()), "C")
})

test_that("gapfill never overwrites a measured cell (authority order)", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                  clay_pct = c(10, 20, 30))
  p <- PedonRecord$new(horizons = h)
  # Even with overwrite=TRUE, measured cells are protected by authority.
  gapfill_within_pedon(p, attrs = "clay_pct", overwrite = TRUE)
  expect_identical(p$horizons$clay_pct, c(10, 20, 30))
})

test_that("a single measured point cannot anchor an interpolation", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                  clay_pct = c(NA, 20, NA))
  p <- PedonRecord$new(horizons = h)
  gapfill_within_pedon(p, attrs = "clay_pct")
  expect_equal(attr(p, "gapfill_within_pedon")$n_filled, 0L)
  expect_true(is.na(p$horizons$clay_pct[1]))
  expect_true(is.na(p$horizons$clay_pct[3]))
})

test_that("classify(gapfill=FALSE) is identical to the default (byte-identical)", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  expect_identical(classify_usda(p, gapfill = FALSE),    classify_usda(p))
  expect_identical(classify_sibcs(p, gapfill = FALSE),   classify_sibcs(p))
  expect_identical(classify_wrb2022(p, gapfill = FALSE), classify_wrb2022(p))
})

test_that("classify(gapfill=TRUE) never mutates the caller's pedon", {
  skip_on_cran()
  p <- .gf_partial_pedon()
  before_h    <- data.table::copy(p$horizons)
  before_prov <- data.table::copy(p$provenance)
  classify_usda(p, gapfill = TRUE)
  classify_sibcs(p, gapfill = TRUE)
  classify_wrb2022(p, gapfill = TRUE)
  classify_all(p, gapfill = TRUE)
  expect_identical(p$horizons,   before_h)
  expect_identical(p$provenance, before_prov)
})

test_that("gapfill accepts character and list specifications", {
  skip_on_cran()
  p1 <- .gf_partial_pedon()
  r_chr <- classify_usda(p1, gapfill = "clay_pct")
  p2 <- .gf_partial_pedon()
  r_lst <- classify_usda(p2, gapfill = list(attrs = "clay_pct"))
  expect_identical(r_chr$name, r_lst$name)
  # An invalid spec is rejected.
  expect_error(classify_usda(.gf_partial_pedon(), gapfill = 1L),
               "must be FALSE")
})

test_that("a pedon with no interior gaps is unaffected by gapfill", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                  clay_pct = c(10, 20, 30))
  p <- PedonRecord$new(horizons = h)
  gapfill_within_pedon(p)
  expect_equal(attr(p, "gapfill_within_pedon")$n_filled, 0L)
  expect_identical(p$horizons$clay_pct, c(10, 20, 30))
})
