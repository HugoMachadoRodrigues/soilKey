test_that("natric_horizon passes on canonical Solonetz fixture", {
  res <- natric_horizon(make_solonetz_canonical())
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("natric_horizon fails on profiles without argic", {
  expect_false(isTRUE(natric_horizon(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(natric_horizon(make_chernozem_canonical())$passed))
})

test_that("natric_horizon fails on profiles with argic but low Na", {
  expect_false(isTRUE(natric_horizon(make_luvisol_canonical())$passed))
  expect_false(isTRUE(natric_horizon(make_acrisol_canonical())$passed))
})

test_that("compute_esp matches the Na/CEC*100 formula", {
  expect_equal(soilKey:::compute_esp(2.0, 10), 20)
  expect_true(is.na(soilKey:::compute_esp(NA, 10)))
  expect_true(is.na(soilKey:::compute_esp(2, 0)))
})

test_that("nitic_horizon passes on canonical Nitisol fixture", {
  res <- nitic_horizon(make_nitisol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("nitic_horizon does not pass on the Ferralsol fixture", {
  # v0.9.220: no ferralic exclusion (WRB 2022 has none; Nitisols key out before
  # Ferralsols). The Ferralsol fixture has clay >= 30 and Fe-dith >= 4, but no
  # Fe-ox (criterion 3) and weak, unshiny structure in the Bw (criterion 2).
  res <- nitic_horizon(make_ferralsol_canonical())
  expect_false(isTRUE(res$passed))
  cr <- res$evidence$criteria
  expect_true(all(cr$structure_shiny[4:5] %in% FALSE))   # Bw1, Bw2: weak
})

test_that("nitic_horizon needs Fe-ox, structure and shiny faces (v0.9.220)", {
  pr <- make_nitisol_canonical()
  expect_true(isTRUE(nitic_horizon(pr)$passed))
  no_ox <- pr$clone(deep = TRUE); no_ox$horizons$fe_ox_pct <- NA_real_
  expect_true(is.na(nitic_horizon(no_ox)$passed))
  low_ratio <- pr$clone(deep = TRUE); low_ratio$horizons$fe_ox_pct <- c(0.3, 0.3, 0.3)
  expect_false(isTRUE(nitic_horizon(low_ratio)$passed))   # 0.3 / 9 < 0.05
  few <- pr$clone(deep = TRUE); few$horizons$clay_films_amount <- c(NA, "few", "few")
  expect_false(isTRUE(nitic_horizon(few)$passed))
  plin <- pr$clone(deep = TRUE); plin$horizons$plinthite_pct <- c(0, 30, 30)
  expect_false(isTRUE(nitic_horizon(plin)$passed))        # part of a plinthic horizon
  # the legacy reading, opt-in
  withr::local_options(soilKey.morphological_inference = TRUE)
  expect_true(isTRUE(nitic_horizon(no_ox)$passed))
})

test_that("the Nitisol key applies the Chapter 4 criteria (v0.9.220)", {
  pr <- make_nitisol_canonical()
  expect_true(isTRUE(nitisol(pr)$passed))
  sandy <- pr$clone(deep = TRUE); sandy$horizons$clay_pct[1] <- 20   # < 57/2
  expect_false(isTRUE(nitisol(sandy)$passed))
})

test_that("planic_features passes on canonical Planosol fixture", {
  res <- planic_features(make_planosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("planic_features fails when boundary is not abrupt", {
  pr <- make_planosol_canonical()
  pr$horizons$boundary_distinctness <- "gradual"
  expect_false(isTRUE(planic_features(pr)$passed))
})

test_that("planic_features can run without boundary requirement", {
  pr <- make_luvisol_canonical()
  res <- planic_features(pr, require_abrupt_boundary = FALSE)
  # Luvisol has clay 18 -> 35 (ratio 1.94 < 2), still fails
  expect_false(isTRUE(res$passed))
})

test_that("test_abrupt_textural_change tests clay doubling", {
  h <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 80),
    clay_pct = c(15, 30),
    boundary_distinctness = c("abrupt", NA_character_)
  )
  res <- test_abrupt_textural_change(h)
  expect_equal(res$layers, 2L)   # ratio 30/15 = 2.0 with abrupt boundary
})

test_that("stagnic_properties passes on canonical Stagnosol fixture", {
  res <- stagnic_properties(make_stagnosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("stagnic_properties fails on Gleysol (redox does not decay)", {
  pr <- make_gleysol_canonical()
  res <- stagnic_properties(pr)
  expect_false(isTRUE(res$passed))
})

test_that("retic_properties passes on canonical Retisol fixture", {
  res <- retic_properties(make_retisol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("retic_properties fails on profiles without glossic features", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical,
                    make_chernozem_canonical)) {
    expect_false(isTRUE(retic_properties(fn())$passed))
  }
})

test_that("cryic_conditions passes on canonical Cryosol fixture", {
  res <- cryic_conditions(make_cryosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("cryic_conditions fails on warm-climate profiles", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical)) {
    expect_false(isTRUE(cryic_conditions(fn())$passed))
  }
})

test_that("anthric_horizons passes on canonical Anthrosol fixture", {
  res <- anthric_horizons(make_anthrosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("anthric_horizons fails on natural profiles", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical,
                    make_chernozem_canonical, make_andosol_canonical)) {
    expect_false(isTRUE(anthric_horizons(fn())$passed))
  }
})

test_that("test_stagnic_pattern correctly distinguishes perched from gleyic", {
  # Perched (decays with depth)
  h_perched <- data.table::data.table(
    top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
    redoximorphic_features_pct = c(0, 25, 2)
  )
  expect_true(isTRUE(test_stagnic_pattern(h_perched)$passed))

  # Gleyic (continues with depth)
  h_gleyic <- data.table::data.table(
    top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
    redoximorphic_features_pct = c(0, 25, 30)
  )
  expect_false(isTRUE(test_stagnic_pattern(h_gleyic)$passed))
})
