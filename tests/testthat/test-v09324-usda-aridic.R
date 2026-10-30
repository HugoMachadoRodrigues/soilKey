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
      ec_dS_m     = c(0.5, 0.6, 0.8),   # no salic horizon (item G.2 fails)
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
  # cambic, calcic, gypsic, salic horizon or duripan, with the carbonate,
  # gypsum and salinity recorded so that item G.2 fails too. FALSE wins over
  # NA.
  p <- PedonRecord$new(
    site = list(id = "made-up", lat = 0, lon = 0, country = "XX"),
    horizons = data.frame(
      top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
      designation = c("A", "C1", "C2"),
      clay_pct = c(4, 4, 4), silt_pct = c(4, 4, 4), sand_pct = c(92, 92, 92),
      oc_pct = c(0.5, 0.1, 0.05), ph_h2o = c(6, 6, 6),
      caco3_pct = 0, caso4_pct = 0, ec_dS_m = 0.3
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


# ---- Item G of the Key to Soil Orders, point by point (KST 13 p 69) ------

.g_profile <- function(top, bottom, des, caco3, smr = "aridic", str = "thermic", ...) {
  n <- length(top)
  site <- list(id = "made-up")
  if (!is.null(smr)) site$soil_moisture_regime <- smr
  if (!is.null(str)) site$soil_temperature_regime <- str
  PedonRecord$new(site = site, horizons = data.frame(
    top_cm = top, bottom_cm = bottom, designation = des, caco3_pct = caco3,
    caso4_pct = 0, ec_dS_m = 0.5, oc_pct = c(0.4, rep(0.1, n - 1)),
    ph_h2o = 7.8, bs_pct = 90, clay_pct = 18, silt_pct = 40, sand_pct = 42,
    munsell_value_moist = 5, munsell_chroma_moist = 3, munsell_value_dry = 7, ...))
}

test_that("G.1.b: a mollic epipedon is not ochric, so an aridic calcic soil with one is not an Aridisol", {
  skip_on_cran()
  p <- PedonRecord$new(site = list(id = "made-up", soil_moisture_regime = "aridic"),
    horizons = data.frame(
      top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150), designation = c("A", "Bk", "Bk2"),
      caco3_pct = c(2, 25, 35), caso4_pct = 0, ec_dS_m = c(0.5, 0.6, 0.8),
      oc_pct = c(2.5, 0.5, 0.2), ph_h2o = c(7.5, 8, 8.2), bs_pct = c(80, 90, 95),
      clay_pct = 18, silt_pct = 40, sand_pct = 42,
      munsell_value_moist = c(2, 4, 5), munsell_chroma_moist = c(2, 3, 3),
      munsell_value_dry = c(4, 6, 6), structure_grade = c("moderate", "weak", "weak")))
  expect_true(isTRUE(mollic_epipedon_usda(p)$passed))   # the mollic is there
  aq <- aridisol_qualifying_usda(p)
  expect_identical(aq$evidence$epipedon$passed, FALSE)
  expect_identical(aq$passed, FALSE)
  expect_false(identical(classify_usda(p, on_missing = "silent")$rsg_or_order, "Aridisols"))
})

test_that("G.1.b: no epipedon at the surface (C horizon) is not an Aridisol; no designation is undecided", {
  skip_on_cran()
  p <- .g_profile(c(0, 40, 60), c(40, 60, 180), c("C1", "Bk", "C2"), c(1, 30, 5))
  expect_identical(aridisol_qualifying_usda(p)$evidence$epipedon$passed, FALSE)
  expect_identical(aridisol_usda(p)$passed, FALSE)
  q <- .g_profile(c(0, 40, 60), c(40, 60, 180), c(NA, NA, NA), c(1, 30, 5))
  expect_true(is.na(aridisol_qualifying_usda(q)$evidence$epipedon$passed))
})

test_that("G.1.c: the horizons count within 100 cm of the surface, not 200", {
  skip_on_cran()
  at60  <- .g_profile(c(0, 40, 60), c(40, 60, 180), c("A", "C", "Bk"), c(1, 1, 30))
  at120 <- .g_profile(c(0, 40, 120), c(40, 120, 180), c("A", "C", "Bk"), c(1, 1, 30))
  expect_true(isTRUE(aridisol_usda(at60)$passed))
  expect_identical(aridisol_qualifying_usda(at120)$evidence$item_c$passed, FALSE)
  expect_identical(aridisol_usda(at120)$passed, FALSE)
})

test_that("G.1.c: a cambic horizon needs a lower depth of 25 cm, or a cryic regime", {
  skip_on_cran()
  sg <- c("weak", "moderate", "massive")
  thin   <- .g_profile(c(0, 5, 22), c(5, 22, 150), c("A", "Bw", "C"), 1, structure_grade = sg)
  deep   <- .g_profile(c(0, 5, 40), c(5, 40, 150), c("A", "Bw", "C"), 1, structure_grade = sg)
  cryic  <- .g_profile(c(0, 5, 22), c(5, 22, 150), c("A", "Bw", "C"), 1, str = "cryic", structure_grade = sg)
  no_str <- .g_profile(c(0, 5, 22), c(5, 22, 150), c("A", "Bw", "C"), 1, str = NULL, structure_grade = sg)
  expect_true(isTRUE(.cambic_impl(thin)$passed))      # a cambic horizon, 5 to 22 cm
  expect_identical(aridisol_usda(thin)$passed, FALSE)
  expect_true(isTRUE(aridisol_usda(deep)$passed))
  expect_true(isTRUE(aridisol_usda(cryic)$passed))
  expect_true(is.na(aridisol_usda(no_str)$passed))    # cryic regime undecided
})

test_that("G.1.c: a sulfuric or WRB duric horizon is not in the list of item G", {
  skip_on_cran()
  mk <- function(...) PedonRecord$new(
    site = list(id = "made-up", soil_moisture_regime = "aridic", soil_temperature_regime = "thermic"),
    horizons = data.frame(top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 150),
      designation = c("A", "C1", "C2"), caco3_pct = 0, caso4_pct = 0, ec_dS_m = 0.5,
      oc_pct = c(0.4, 0.1, 0.1), ph_h2o = 7.5, bs_pct = 90, clay_pct = 12,
      silt_pct = 30, sand_pct = 58, munsell_value_moist = 5, munsell_chroma_moist = 3,
      munsell_value_dry = 7, structure_grade = "massive", ...))
  sulf  <- mk(sulfidic_s_pct = c(0, 0, 1))
  duric <- mk(duripan_pct = c(0, 35, 0))
  expect_true(isTRUE(sulfuric_horizon_usda(sulf, 200)$passed))
  expect_true(isTRUE(duric_horizon(duric)$passed))
  expect_identical(aridisol_usda(sulf)$passed, FALSE)
  expect_identical(aridisol_usda(duric)$passed, FALSE)
})

.g2_profile <- function(smr = "ustic", sat = c(40, 60, 90), ph = c(8, 8.2, 8.4)) {
  PedonRecord$new(site = list(id = "made-up", soil_moisture_regime = smr),
    horizons = data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
      designation = c("Az", "Bz", "Cz"), ec_dS_m = c(60, 45, 30), ph_h2o = ph,
      water_saturation_days = sat, clay_pct = 20, sand_pct = 40, silt_pct = 40,
      oc_pct = c(0.5, 0.3, 0.1), caco3_pct = 1, caso4_pct = 0))
}

test_that("G.2: a salic horizon, saturated a month, dry at some time, no sulfuric horizon: Aridisols without an aridic regime", {
  skip_on_cran()
  p <- .g2_profile()
  aq <- aridisol_qualifying_usda(p)
  expect_identical(aq$evidence$item_g1, FALSE)          # ustic, not aridic
  expect_identical(aq$evidence$item_g2$passed, TRUE)
  expect_identical(classify_usda(p, on_missing = "silent")$rsg_or_order, "Aridisols")
  # saturated for less than 30 days in every layer: G.2 fails
  expect_identical(aridisol_usda(.g2_profile(sat = c(5, 10, 10)))$passed, FALSE)
  # a udic regime does not say whether the control section is ever dry
  expect_true(is.na(aridisol_usda(.g2_profile(smr = "udic"))$passed))
  # pH below 4.0 within 150 cm: a sulfuric horizon cannot be ruled out
  expect_true(is.na(aridisol_usda(.g2_profile(ph = c(3.6, 3.8, 4.2)))$passed))
})
