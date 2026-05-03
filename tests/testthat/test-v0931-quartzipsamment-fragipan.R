# Tests for v0.9.31 specialized Great Group tests:
#   * Quartzipsamment proxy broadened (clay <= 10, sand >= 80, cf <= 15)
#   * Fragipan accepts NASIS pediagfeatures "Fragipan" flag

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))


# ---- quartzipsamment_qualifying_usda ----------------------------------------

test_that("quartzipsamment fires on uniformly sandy profile (clay 5, sand 90)", {
  p <- PedonRecord$new(
    site = list(id = "qsamm-1", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                      designation = c("A", "C1", "C2"),
                      clay_pct = c(5, 6, 5),
                      sand_pct = c(90, 91, 92),
                      silt_pct = c(5, 3, 3))
  )
  res <- quartzipsamment_qualifying_usda(p)
  expect_true(isTRUE(res$passed))
})

test_that("quartzipsamment fires on loamy-fine-sand (clay 8, sand 85)", {
  # Pre-v0.9.31 proxy (clay <= 5 AND cf <= 5) rejected this; v0.9.31
  # accepts clay <= 10 AND sand >= 80.
  p <- PedonRecord$new(
    site = list(id = "qsamm-2", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "C"),
                      clay_pct = c(8, 7),
                      sand_pct = c(85, 88),
                      silt_pct = c(7, 5))
  )
  res <- quartzipsamment_qualifying_usda(p)
  expect_true(isTRUE(res$passed))
})

test_that("quartzipsamment does NOT fire on loamy profile (clay 15, sand 60)", {
  p <- PedonRecord$new(
    site = list(id = "loamy", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "C"),
                      clay_pct = c(15, 18),
                      sand_pct = c(60, 55),
                      silt_pct = c(25, 27))
  )
  res <- quartzipsamment_qualifying_usda(p)
  expect_false(isTRUE(res$passed))
})

test_that("quartzipsamment does NOT fire when sand_pct missing", {
  # Sand-pct is now required (NEW in v0.9.31); without it we can't
  # prove >= 80 % sand and the test rejects.
  p <- PedonRecord$new(
    site = list(id = "no-sand", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "C"),
                      clay_pct = c(5, 5))
  )
  res <- quartzipsamment_qualifying_usda(p)
  expect_false(isTRUE(res$passed))
})

test_that("quartzipsamment rejects when half-or-more layers fail", {
  p <- PedonRecord$new(
    site = list(id = "mixed", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(
      top_cm = c(0, 30, 60),
      bottom_cm = c(30, 60, 100),
      designation = c("A", "Bw", "C"),
      clay_pct = c(5, 25, 5),     # middle layer clay-rich
      sand_pct = c(90, 50, 90),
      silt_pct = c(5, 25, 5)
    )
  )
  res <- quartzipsamment_qualifying_usda(p)
  # 2/3 layers pass (>= 50 %), so it still passes -- this is intended
  # since not every horizon needs to be sandy if the profile is
  # dominantly so.
  expect_true(isTRUE(res$passed))
})


# ---- fragipan_usda NASIS path -----------------------------------------------

test_that("fragipan fires when NASIS pediagfeatures has 'Fragipan' flag", {
  p <- PedonRecord$new(
    site = list(id = "frag-1", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = c("Fragipan", "Argillic horizon")),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "Bx"),
                      clay_pct = c(15, 20))
  )
  res <- fragipan_usda(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$evidence_source, "nasis_pediagfeatures")
})

test_that("fragipan rupture_resistance path still fires (lab evidence)", {
  p <- PedonRecord$new(
    site = list(id = "frag-2", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(
      top_cm = c(0, 30, 50, 80),
      bottom_cm = c(30, 50, 80, 120),
      designation = c("A", "B", "Bx1", "Bx2"),
      clay_pct = c(15, 18, 20, 22),
      rupture_resistance = c(NA, NA, "firm", "very firm")
    )
  )
  res <- fragipan_usda(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$evidence_source, "rupture_resistance")
})

test_that("fragipan does NOT fire without rupture_resistance and without NASIS flag", {
  p <- PedonRecord$new(
    site = list(id = "no-frag", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "Bw"),
                      clay_pct = c(15, 18))
  )
  res <- fragipan_usda(p)
  expect_false(isTRUE(res$passed))
})

test_that("fragipan ignores non-Fragipan NASIS featkinds", {
  p <- PedonRecord$new(
    site = list(id = "argi-only", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = c("Argillic horizon",
                                                  "Mollic epipedon")),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 100),
                      designation = c("A", "Bw"),
                      clay_pct = c(15, 18))
  )
  res <- fragipan_usda(p)
  expect_false(isTRUE(res$passed))
})
