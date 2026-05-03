# Tests for v0.9.28 designation-based clay-films proxy.
#
# KST 13ed Ch 18 defines master horizon symbol 't' as "an accumulation
# of silicate clay that has either formed in the horizon and is
# subsequently translocated within it, or has been moved into the
# horizon by illuviation". A pedologist who wrote 'Bt' / 'Btk' / 'Btx'
# in the field designation is making the clay-illuviation claim that
# the KST 13ed argillic test requires. v0.9.28 accepts that as
# positive evidence in argillic_clay_films_test() when NASIS records
# are absent.

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))


# ---- positive: designation 't' suffix without NASIS data -------------------

test_that("designation Bt alone (no NASIS) triggers clay-films-test PASS", {
  p <- PedonRecord$new(
    site = list(id = "p1", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$evidence_source, "designation_t_suffix")
  expect_equal(res$evidence$horizons_with_t_designation, 1L)
  expect_equal(res$layers, 2L)
})

test_that("designation Btk / Btx / Bt1 / 2Bt all match", {
  for (des in c("Btk", "Btx", "Bt1", "2Bt", "Btss", "BAt", "ABt")) {
    p <- PedonRecord$new(
      site = list(id = "p", lat = 0, lon = 0, country = "US"),
      horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                        designation = c("A", des),
                        clay_pct = c(15, 25))
    )
    res <- argillic_clay_films_test(p)
    expect_true(isTRUE(res$passed),
                  info = paste("designation:", des))
  }
})


# ---- negative: designations without 't' suffix don't trigger ---------------

test_that("designation Bw / Bk / BC / A / O do NOT trigger", {
  for (des in c("Bw", "Bk", "BC", "A", "O", "C", "R", "Bg", "Bs", "Bh")) {
    p <- PedonRecord$new(
      site = list(id = "p", lat = 0, lon = 0, country = "US"),
      horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                        designation = c("A", des),
                        clay_pct = c(15, 25))
    )
    res <- argillic_clay_films_test(p)
    expect_false(isTRUE(res$passed),
                   info = paste("designation:", des, " -- should not trigger"))
  }
})


# ---- false-positive guards (regex strictness) ------------------------------

test_that("designation regex anchors to master horizon (no false-positive on 't' anywhere)", {
  # A free-text 'test' field must NOT match: it lacks the master
  # horizon prefix [ABCEORW].
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "test"),  # not a real horizon code
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_false(isTRUE(res$passed))
})


# ---- evidence-source priority ---------------------------------------------

test_that("NASIS pediagfeatures wins over designation-proxy when both present", {
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US",
                  nasis_diagnostic_features = "Argillic horizon"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_true(isTRUE(res$passed))
  # Source priority: pediagfeatures > clay_films_amount > designation
  expect_equal(res$evidence$evidence_source, "nasis_pediagfeatures")
})

test_that("clay_films_amount wins over designation-proxy when both present (no pediagfeatures)", {
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
                      designation = c("A", "Bt"),
                      clay_films_amount = c(NA, "common"),
                      clay_pct = c(15, 25))
  )
  res <- argillic_clay_films_test(p)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$evidence_source, "nasis_phpvsf")
})


# ---- integration with argillic_usda -----------------------------------------

test_that("argillic_usda uses KST tier when designation has 't' (no NASIS)", {
  # +3.7 pp clay-jump: WRB rejects, KST accepts.
  # No NASIS data BUT designation 'Bt' triggers KST tier via v0.9.28 proxy.
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US"),
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

test_that("argillic_usda regression-safe when designation has NO 't' suffix", {
  # Same profile but designation lacks 't' -> falls back to WRB tier
  # -> WRB rejects the +3.7 pp profile.
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US"),
    horizons = mk_h(
      top_cm      = c(0,  10, 30),
      bottom_cm   = c(10, 30, 60),
      designation = c("A", "E", "Bw"),
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


# ---- evidence-absent scenarios ---------------------------------------------

test_that("clay-films-test returns NA when NO designation field at all", {
  hz <- data.table::data.table(top_cm = c(0, 30),
                                  bottom_cm = c(30, 60),
                                  clay_pct = c(15, 25))
  hz <- ensure_horizon_schema(hz)
  # Force-NA the designation column (the schema may have created it).
  hz$designation <- NA_character_
  p <- PedonRecord$new(
    site = list(id = "p", lat = 0, lon = 0, country = "US"),
    horizons = hz
  )
  res <- argillic_clay_films_test(p)
  expect_true(is.na(res$passed))
  expect_true("designation" %in% res$missing)
})
