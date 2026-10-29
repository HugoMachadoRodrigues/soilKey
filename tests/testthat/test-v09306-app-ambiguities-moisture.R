# v0.9.306 (Jelle Janssen, ISRIC): the Pro app's "close calls" panel named
# every ambiguous class "?" (it read name/code, the results carry rsg_name
# and rsg_code), and the editor had no input for the soil moisture regime
# that the Soil Taxonomy key reads.

.v09306_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "[.]R$", full.names = TRUE)) sys.source(f, envir = env)
  env
}

test_that("an ambiguity is labelled with its class name and code", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09306_env()
  p <- make_ferralsol_canonical()
  h <- p$horizons
  h$coarse_fragments_pct <- NA; h$slickensides <- NA; h$plinthite_pct <- NA
  r <- classify_wrb2022(PedonRecord$new(site = p$site, horizons = h), on_missing = "silent")
  labels <- vapply(r$ambiguities, env$.sk_amb_label, "")
  expect_true("Cryosols (CR)" %in% labels)
  expect_false(any(labels == "?"))
  # older shapes still read
  expect_identical(env$.sk_amb_label(list(name = "Vertisols", code = "VR")), "Vertisols (VR)")
  expect_identical(env$.sk_amb_label(list()), "?")
})

test_that("the soil moisture regime input reads, normalises and drops", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  env <- .v09306_env()
  expect_identical(env$.sk_smr_value("Ustic"), "ustic")
  expect_identical(env$.sk_smr_value("torric"), "aridic")
  expect_identical(env$.sk_smr_value("bogus"), "")
  expect_identical(env$.sk_smr_value(NULL), "")
  expect_identical(env$.sk_smr_value(NA), "")
  s <- env$.sk_site_with_smr(list(id = "x", soil_moisture_regime = "udic"), "ustic")
  expect_identical(s$soil_moisture_regime, "ustic")
  s <- env$.sk_site_with_smr(list(id = "x", soil_moisture_regime = "udic"), "")
  expect_false("soil_moisture_regime" %in% names(s))
  # the regime reaches the Soil Taxonomy key
  p <- make_chernozem_canonical()
  p2 <- PedonRecord$new(site = env$.sk_site_with_smr(p$site, "ustic"), horizons = p$horizons)
  expect_identical(p2$site$soil_moisture_regime, "ustic")
  expect_true(all(c("pedon.moisture_regime", "pedon.help_moisture_regime", "pedon.smr_unknown") %in%
                    names(yaml::read_yaml(system.file("i18n", "translations.yaml", package = "soilKey"))$pt)))
})
