# v0.9.429 (Hugo, 2026-11-23; USDA audit, wave 4): the canonical profiles
# record the soil moisture and temperature regimes read from the NRCS global
# regime maps (USDA-NRCS 1997) at their sites, and a Mollisol with an aridic
# regime that borders on ustic or xeric is an Ustoll or a Xeroll (KST 13,
# p. 66), read from the NASIS moisture subclass.

.v09429_ns <- asNamespace("soilKey")
.v09429_fx <- grep("^make_.*canonical$", ls(.v09429_ns), value = TRUE)

test_that("every canonical profile but the Cryosol records both regimes", {
  testthat::skip_on_cran()
  smr_ok <- c("udic", "perudic", "ustic", "xeric", "aridic")
  str_ok <- c("cryic", "frigid", "mesic", "thermic", "isothermic", "isohyperthermic")
  for (f in setdiff(.v09429_fx, "make_cryosol_canonical")) {
    s <- get(f, envir = .v09429_ns)()$site
    expect_true(s$soil_moisture_regime %in% smr_ok, info = f)
    expect_true(s$soil_temperature_regime %in% str_ok, info = f)
  }
  expect_null(make_cryosol_canonical()$site$soil_moisture_regime)
})

test_that("the regimes follow the map, also where an aridic one was recorded", {
  testthat::skip_on_cran()
  expect_identical(make_calcisol_canonical()$site$soil_moisture_regime, "xeric")
  expect_identical(make_solonchak_canonical()$site$soil_moisture_regime, "ustic")
  expect_identical(make_chernozem_canonical()$site$soil_moisture_regime, "xeric")
  expect_identical(make_podzol_canonical()$site$soil_temperature_regime, "cryic")
  expect_identical(make_kastanozem_canonical()$site$soil_moisture_subclass, "ustic aridic")
  # the SiBCS copies inherit the regime of the WRB profile they copy
  expect_identical(make_latossolo_canonical()$site$soil_temperature_regime, "isohyperthermic")
  expect_identical(make_luvissolo_canonical()$site$soil_moisture_regime, "aridic")
})

test_that("aridic bordering on ustic or xeric reads the NASIS subclass", {
  testthat::skip_on_cran()
  f <- function(smr, sub, side) {
    p <- make_kastanozem_canonical()
    p$site$soil_moisture_regime <- smr
    p$site$soil_moisture_subclass <- sub
    soilKey:::smr_aridic_bordering_usda(p, side)$passed
  }
  expect_true(f("aridic", "ustic aridic", "ustic"))
  expect_false(f("aridic", "ustic aridic", "xeric"))
  expect_true(f("torric", "xeric torric", "xeric"))
  expect_false(f("aridic", "typic aridic", "ustic"))
  expect_true(is.na(f("aridic", NULL, "ustic")))
  expect_true(is.na(f("aridic", "weak aridic", "ustic")))
  expect_false(f("udic", NULL, "ustic"))
  expect_true(is.na(f(NULL, NULL, "ustic")))
})

test_that("an aridic Mollisol without a subclass leaves Xerolls and Ustolls undecided", {
  testthat::skip_on_cran()
  p <- make_kastanozem_canonical()
  expect_identical(classify_usda(p)$name, "Aridic Haplustolls")
  p$site$soil_moisture_subclass <- NULL
  r <- classify_usda(p)
  sub <- r$trace$suborders
  pass <- vapply(sub, function(x) if (is.na(x$passed)) NA else isTRUE(x$passed), NA)
  names(pass) <- vapply(sub, function(x) x$name, "")
  expect_true(is.na(pass[["Xerolls"]]))
  expect_true(is.na(pass[["Ustolls"]]))
  amb <- vapply(r$ambiguities, function(a) a$rsg_name, "")
  expect_true(all(c("Xerolls", "Ustolls") %in% amb))
  expect_identical(r$evidence_grade, "C")
})

test_that("the canonical USDA names reflect the recorded regimes", {
  testthat::skip_on_cran()
  nm <- function(f) classify_usda(get(f, envir = .v09429_ns)())$name
  expect_identical(nm("make_podzol_canonical"), "Typic Haplocryods")
  expect_identical(nm("make_histosol_canonical"), "Terric Cryohemists")
  expect_identical(nm("make_chernozem_canonical"), "Cumulic Haploxerolls")
  expect_identical(nm("make_vertisol_canonical"), "Chromic Haplusterts")
  expect_identical(nm("make_acrisol_canonical"), "Typic Paleudults")
})
