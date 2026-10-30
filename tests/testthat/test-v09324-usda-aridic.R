# v0.9.324: the USDA Aridisols read the aridic moisture regime only from
# site$soil_moisture_regime. KST 13 p 69 (Key to Soil Orders, item G.1.a)
# needs "An aridic soil moisture regime"; p 48 defines it by the moisture
# control section in normal years, a property estimated from climatic data
# (p 47). Up to v0.9.314 a missing regime was read as aridic whenever the
# surface organic carbon was below 1 % or not recorded. Now a missing regime
# leaves the Aridisols undecided (NA) and the key goes on.
# All profiles are made up.

.aridic_calcic_profile <- function(smr = NULL, oc = c(0.4, 0.2, 0.1)) {
  site <- list(id = "made-up", lat = 0, lon = 0, country = "XX",
               parent_material = "alluvium")
  if (!is.null(smr)) site$soil_moisture_regime <- smr
  PedonRecord$new(
    site = site,
    horizons = data.frame(
      top_cm      = c(0, 30, 80),
      bottom_cm   = c(30, 80, 150),
      designation = c("A", "Bk", "Bk2"),
      caco3_pct   = c(2, 25, 35),
      oc_pct      = oc,
      ph_h2o      = c(7.5, 8.0, 8.2),
      bs_pct      = c(80, 90, 95),
      clay_pct    = c(15, 22, 22),
      silt_pct    = c(40, 30, 30),
      sand_pct    = c(45, 48, 48)
    )
  )
}

test_that("a recorded aridic (or torric) regime keys a calcic profile to the Aridisols", {
  skip_on_cran()
  for (r in c("aridic", "torric", "Aridic")) {
    p <- .aridic_calcic_profile(smr = r)
    expect_true(isTRUE(aridisol_usda(p)$passed), info = r)
    expect_identical(classify_usda(p, on_missing = "silent")$rsg_or_order,
                     "Aridisols", info = r)
  }
})

test_that("no recorded regime leaves the Aridisols undecided and the key goes on", {
  skip_on_cran()
  p <- .aridic_calcic_profile(smr = NULL)
  aq <- aridisol_qualifying_usda(p)
  expect_true(isTRUE(aq$evidence$diag_present))   # the calcic horizon is there
  expect_true(is.na(aq$passed))
  expect_true(is.na(aridisol_usda(p)$passed))
  expect_true("site$soil_moisture_regime" %in% aridisol_usda(p)$missing)
  res <- classify_usda(p, on_missing = "silent")
  expect_false(identical(res$rsg_or_order, "Aridisols"))
  expect_true(is.na(res$trace$orders$AS$passed))
  amb <- vapply(res$ambiguities, function(u) u$rsg_code, "")
  expect_true("AS" %in% amb)
})

test_that("low or unrecorded surface organic carbon no longer stands for an aridic regime", {
  skip_on_cran()
  # The old fallback passed when every horizon starting within 5 cm had
  # organic carbon below 1 % or not recorded.
  low_oc <- .aridic_calcic_profile(smr = NULL, oc = c(0.3, 0.2, 0.1))
  no_oc  <- .aridic_calcic_profile(smr = NULL, oc = c(NA, NA, NA))
  for (p in list(low_oc, no_oc)) {
    expect_true(is.na(aridisol_usda(p)$passed))
    expect_false(identical(classify_usda(p, on_missing = "silent")$rsg_or_order,
                           "Aridisols"))
  }
})

test_that("a recorded regime that is not aridic fails the Aridisols (FALSE, not NA)", {
  skip_on_cran()
  for (r in c("udic", "ustic", "xeric", "perudic", "aquic")) {
    p <- .aridic_calcic_profile(smr = r)
    expect_identical(aridisol_qualifying_usda(p)$passed, FALSE, info = r)
    expect_identical(aridisol_usda(p)$passed, FALSE, info = r)
    res <- classify_usda(p, on_missing = "silent")
    expect_false(identical(res$rsg_or_order, "Aridisols"), info = r)
    amb <- vapply(res$ambiguities, function(u) u$rsg_code, "")
    expect_false("AS" %in% amb, info = r)
  }
})

test_that("no qualifying horizon fails the Aridisols even without a recorded regime", {
  skip_on_cran()
  # A deep, uniform, low-carbon sandy profile (made up): no argillic, natric,
  # cambic, calcic, gypsic, salic horizon or duripan. FALSE wins over NA.
  p <- PedonRecord$new(
    site = list(id = "made-up", lat = 0, lon = 0, country = "XX"),
    horizons = data.frame(
      top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
      designation = c("A", "C1", "C2"),
      clay_pct = c(4, 4, 4), silt_pct = c(4, 4, 4), sand_pct = c(92, 92, 92),
      oc_pct = c(0.5, 0.1, 0.05), ph_h2o = c(6, 6, 6)
    )
  )
  expect_identical(aridisol_qualifying_usda(p)$passed, FALSE)
  expect_identical(aridisol_usda(p)$passed, FALSE)
})

test_that("a humid argillic profile without a recorded regime is not an Aridisol", {
  skip_on_cran()
  # Made-up well-drained red clayey profile of a humid region with a clay
  # increase (argillic) and low surface carbon: the case that the old
  # fallback sent to the Aridisols when no regime was recorded.
  p <- PedonRecord$new(
    site = list(id = "made-up", lat = -22, lon = -43, country = "XX"),
    horizons = data.frame(
      top_cm = c(0, 15, 40, 100), bottom_cm = c(15, 40, 100, 180),
      designation = c("A", "BA", "Bt1", "Bt2"),
      clay_pct = c(20, 30, 45, 46), silt_pct = c(15, 12, 10, 10),
      sand_pct = c(65, 58, 45, 44),
      oc_pct = c(0.9, 0.5, 0.3, 0.2), ph_h2o = c(5.0, 4.9, 4.8, 4.8),
      bs_pct = c(30, 20, 15, 15), clay_films_amount = c(NA, "few", "common", "common")
    )
  )
  expect_false(isTRUE(aridisol_usda(p)$passed))
  expect_false(identical(classify_usda(p, on_missing = "silent")$rsg_or_order,
                         "Aridisols"))
})
