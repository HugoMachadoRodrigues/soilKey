# Tests for v0.9.27 clay-illuviation evidence test (KST 13ed Ch 3 p 4)
#
# argillic_clay_films_test reads two complementary NASIS-derived slots:
#   pedon$site$nasis_diagnostic_features   (pediagfeatures.featkind vector)
#   pedon$horizons$clay_films_amount       (per-horizon, from phpvsf)

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))


# ---- Direct positive evidence: NASIS pediagfeatures argillic flag ---------

test_that("clay-films-test PASSES when NASIS argillic flag present", {
  p <- PedonRecord$new(
    site = list(id = "p1", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = c("Argillic horizon",
                                                  "Cambic horizon")),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_true(isTRUE(res$passed))
  expect_true(res$evidence$pediagfeatures_argillic_flag)
})

test_that("clay-films-test handles featkind case-insensitively", {
  p <- PedonRecord$new(
    site = list(id = "p2", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = "ARGILLIC HORIZON"),
    horizons = mk_h(top_cm = 0, bottom_cm = 30, designation = "A",
                      clay_pct = 20)
  )
  expect_true(isTRUE(argillic_clay_films_test(p)$passed))
})


# ---- Direct positive evidence: per-horizon clay_films_amount ---------------

test_that("clay-films-test PASSES when clay_films_amount filled", {
  p <- PedonRecord$new(
    site = list(id = "p3", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25),
                      clay_films_amount = c(NA, "common"))
  )
  res <- argillic_clay_films_test(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$horizons_with_clay_films, 1L)
  expect_equal(res$layers, 2L)
})

test_that("clay-films-test PASSES with clay_films but NO pediagfeatures", {
  p <- PedonRecord$new(
    site = list(id = "p4", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25),
                      clay_films_amount = c(NA, "many"))
  )
  expect_true(isTRUE(argillic_clay_films_test(p)$passed))
})


# ---- Indeterminate: no evidence at all -----------------------------------

test_that("clay-films-test returns NA when both slots empty", {
  p <- PedonRecord$new(
    site = list(id = "p5", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_true(is.na(res$passed))
  expect_true("nasis_diagnostic_features" %in% res$missing)
})


# ---- Negative: pediagfeatures present but no argillic flag -----------------

test_that("clay-films-test FALSE when only non-argillic NASIS featkinds", {
  p <- PedonRecord$new(
    site = list(id = "p6", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = c("Cambic horizon",
                                                  "Mollic epipedon")),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bw"),
                      clay_pct = c(15, 17))
  )
  res <- argillic_clay_films_test(p)
  expect_false(isTRUE(res$passed))
  expect_false(is.na(res$passed))
  expect_false(res$evidence$pediagfeatures_argillic_flag)
})


# ---- Integration: argillic_usda routing ------------------------------------

test_that("argillic_usda uses KST thresholds when clay-films evidence present", {
  # +3.7 pp clay-jump: WRB rejects, KST accepts.
  p <- PedonRecord$new(
    site = list(id = "p7", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = "Argillic horizon"),
    horizons = mk_h(
      top_cm      = c(0,  10, 30),
      bottom_cm   = c(10, 30, 60),
      designation = c("A", "E", "Bt"),
      clay_pct    = c(10, 8.6, 12.3),
      silt_pct    = c(40, 35, 30),
      sand_pct    = c(50, 56.4, 57.7),
      bs_pct      = c(70, 70, 70),
      oc_pct      = c(2, 0.5, 0.3)
    )
  )
  res <- argillic_usda(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$argillic_tier$threshold_system, "usda")
})

test_that("argillic_usda uses WRB thresholds when no clay-films evidence", {
  # Same +3.7 pp profile but WITHOUT clay-films evidence -> WRB rejects.
  p <- PedonRecord$new(
    site = list(id = "p8", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(
      top_cm      = c(0,  10, 30),
      bottom_cm   = c(10, 30, 60),
      designation = c("A", "E", "Bt"),
      clay_pct    = c(10, 8.6, 12.3),
      silt_pct    = c(40, 35, 30),
      sand_pct    = c(50, 56.4, 57.7),
      bs_pct      = c(70, 70, 70),
      oc_pct      = c(2, 0.5, 0.3)
    )
  )
  res <- argillic_usda(p)
  expect_false(isTRUE(res$passed))
  expect_equal(res$evidence$argillic_tier$threshold_system, "wrb2022")
})

test_that("argillic_usda canonical Luvisol still passes (regression-safe)", {
  pr <- make_luvisol_canonical()
  res <- argillic_usda(pr)
  expect_true(isTRUE(res$passed))
})
