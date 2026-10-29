# v0.9.307 (Jelle Janssen, ISRIC; Hugo, 2026-10-29): a soil temperature regime
# warmer than gelic rules out permafrost (KST 13, Ch. 3, p. 50: the cryic regime
# has no permafrost, the others are warmer), so a profile from Kenya no longer
# leaves the Cryosols and Gelisols open for want of a permafrost temperature.

.v09307_pedon <- function(str = NULL, pf = NA_real_, desig = NULL) {
  p <- make_ferralsol_canonical()
  h <- p$horizons
  h$permafrost_temp_C <- pf
  if (!is.null(desig)) h$designation <- desig
  site <- p$site
  site$soil_temperature_regime <- str
  PedonRecord$new(site = site, horizons = h)
}

test_that("a regime warmer than gelic decides the WRB cryic conditions", {
  skip_on_cran()
  open <- cryic_conditions(.v09307_pedon())
  expect_true(is.na(open$passed))
  expect_true("permafrost_temp_C" %in% open$missing)
  for (r in c("isohyperthermic", "Thermic", " mesic ", "cryic", "isofrigid")) {
    x <- cryic_conditions(.v09307_pedon(r))
    expect_identical(x$passed, FALSE, info = r)
    expect_length(x$missing, 0)
    expect_true(isTRUE(x$evidence$temperature_regime$rules_out_permafrost), info = r)
  }
  # gelic, the older pergelic, or a word that is not a regime: still open
  for (r in c("gelic", "pergelic", "warm", NA)) {
    expect_true(is.na(cryic_conditions(.v09307_pedon(r))$passed), info = r)
  }
})

test_that("a measured permafrost or a frozen designation still wins", {
  skip_on_cran()
  expect_true(isTRUE(cryic_conditions(.v09307_pedon("mesic", pf = -2))$passed))
  p <- .v09307_pedon("mesic")
  d <- p$horizons$designation
  d[2] <- "Bf"
  expect_true(isTRUE(cryic_conditions(.v09307_pedon("mesic", desig = d))$passed))
})

test_that("the WRB key no longer lists the Cryosols as a close call", {
  skip_on_cran()
  amb <- function(p) vapply(classify_wrb2022(p, on_missing = "silent")$ambiguities,
                            function(a) a$rsg_code %||% "", "")
  expect_true("CR" %in% amb(.v09307_pedon()))
  expect_false("CR" %in% amb(.v09307_pedon("isohyperthermic")))
  # the name does not change
  expect_identical(classify_wrb2022(.v09307_pedon("isohyperthermic"), on_missing = "silent")$name,
                   classify_wrb2022(.v09307_pedon(), on_missing = "silent")$name)
})

test_that("the USDA permafrost test reads the regime too", {
  skip_on_cran()
  pf <- soilKey:::permafrost_within_usda(.v09307_pedon())
  expect_true("permafrost_temp_C" %in% pf$missing)
  pf <- soilKey:::permafrost_within_usda(.v09307_pedon("isohyperthermic"))
  expect_false(pf$passed)
  expect_length(pf$missing, 0)
  ge <- soilKey:::gelisol_usda(.v09307_pedon("thermic"))
  expect_false(ge$passed)
  expect_length(ge$missing, 0)
  expect_true(soilKey:::gelisol_usda(.v09307_pedon("thermic", pf = -1))$passed)
})

test_that("the Pro app has a temperature regime input that reads and drops", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = env)
  expect_identical(env$.sk_str_value("IsoHyperthermic"), "isohyperthermic")
  expect_identical(env$.sk_str_value("pergelic"), "gelic")
  expect_identical(env$.sk_str_value("hot"), "")
  expect_identical(env$.sk_str_value(NULL), "")
  s <- env$.sk_site_with_str(list(id = "x"), "thermic")
  expect_identical(s$soil_temperature_regime, "thermic")
  expect_false("soil_temperature_regime" %in% names(env$.sk_site_with_str(s, "")))
  tr <- yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))
  for (lang in c("en", "pt"))
    expect_true(all(c("pedon.temperature_regime", "pedon.help_temperature_regime",
                      "pedon.str_unknown") %in% names(tr[[lang]])), info = lang)
})
