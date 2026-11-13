# v0.9.406 -- the ISIS parent material, volcanic materials as a non-alluvial
# origin, and gypsum not measured and not named (Hugo, 2026-11-13). Every
# profile below is synthetic.

.p406 <- function(..., site = list(id = "p406")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}

.isis406 <- function(...) {
  cd <- function(item, description) list(item = item, description = description)
  lith <- list(...)
  c(list(profileCode = "XX406", countryCode = "XX", geog = list(latitude = 1, longitude = 1)),
    lapply(lith, function(v) cd(substr(v, 1, 1), v)),
    list(layers = list(
      list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah", clay = list("20")),
      list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 60L, layerName = "C", clay = list("15")))))
}

test_that("the ISIS reader reads every lithology field as the parent material", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.isis406(
    lithologyParmat = "Unconsolidated and unspecified deposits",
    lithologyParmatDeposition = "Ejecta ash",
    lithologyParmatXxx = "Basic ash",
    lithologyParmatXx2 = "Ejecta ash"))
  expect_identical(p$site$parent_material,
                   "Unconsolidated and unspecified deposits; Ejecta ash; Basic ash")
  q <- isis_profile_to_pedon(.isis406(lithologyParmatDeposition = "Ejecta ash"))
  expect_identical(q$site$parent_material, "Ejecta ash")
  expect_null(isis_profile_to_pedon(.isis406())$site$parent_material)
  # the water depositions stay in rock_origin only
  m <- isis_profile_to_pedon(.isis406(lithologyParmat = "Unconsolidated and unspecified deposits",
                                      lithologyParmatDeposition = "Marine sediments"))
  expect_identical(m$site$parent_material, "Unconsolidated and unspecified deposits")
  expect_equal(m$horizons$rock_origin[1], "marine")
  f <- isis_profile_to_pedon(.isis406(lithologyParmatDeposition = "Fluvio-glacial sediments",
                                      lithologyParmatXxx = "Coarse-grained acid igneous"))
  expect_identical(f$site$parent_material, "Coarse-grained acid igneous")
  for (f in c("lithologyParmat2", "lithologyParmatXxx", "lithologyParmatXx2"))
    expect_match(soilKey:::.isis_profile_fields(), f)
})

test_that("volcanic parent materials are a non-alluvial origin", {
  skip_on_cran()
  origin <- function(pm) soilKey:::.f88_sediment_origin(soilKey:::.f88_ctx(
    .p406(top_cm = 0, bottom_cm = 30, designation = "Ah", site = list(id = "s", parent_material = pm))))
  for (pm in c("Ejecta ash", "Basic ash", "Tuff", "Pumice", "Unconsolidated pyroclastic rocks",
               "Basic lapilli", "Scoria", "Lava flow"))
    expect_false(origin(pm), info = pm)
  # alluvial sediments still win, and a slope wash is not volcanic ash
  expect_true(origin("Fluvio-glacial sediments; Basic ash"))
  expect_true(is.na(origin("Slope wash")))
  # tephra with a buried A: organic carbon that rises with depth is no longer
  # read as fluvic once the origin is recorded as volcanic
  oc <- c(12, 5, 8, 2)
  mk <- function(pm) .p406(top_cm = c(0, 30, 50, 70), bottom_cm = c(30, 50, 70, 130),
                           designation = c("Ah1", "Ah2", "2Ahb", "2C"), oc_pct = oc,
                           clay_pct = 20, site = list(id = "t", parent_material = pm))
  expect_false(fluvic_properties_fao88(mk("Ejecta ash"))$passed)
  expect_false(recent_alluvial_deposits_fao74(mk("Ejecta ash"))$passed)
  expect_true(fluvic_properties_fao88(mk("Recent alluvium"))$passed)
})

test_that("ash is a whole word for the volcanic material of the andic properties", {
  skip_on_cran()
  mk <- function(pm) .p406(top_cm = c(0, 20), bottom_cm = c(20, 40), designation = c("Ah", "Bw"),
                           clay_pct = 20, oc_pct = 2, site = list(id = "a", parent_material = pm))
  fl <- function(p) soilKey:::.f88_andic_layer(soilKey:::.f88_ctx(p))$flags
  expect_true(all(grepl("andic_absent_no_volcanic_material", fl(mk("Slope wash")))))
  expect_false(any(grepl("andic_absent_no_volcanic_material", fl(mk("Ejecta ash"))), na.rm = TRUE))
})

test_that("gypsum not measured and not named is none", {
  skip_on_cran()
  base <- list(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 110), clay_pct = 25, ph_h2o = 7.8)
  p <- do.call(.p406, c(base, list(designation = c("Ah", "Bw", "C"))))
  x <- soilKey:::.f88_ctx(p)
  g <- soilKey:::.f88_gypsum(x)
  expect_identical(g$value, c(0, 0, 0))
  expect_true(all(g$flags == "gypsum_not_measured_none_named"))
  expect_false(gypsic_fao88(p)$passed)
  # named in the record: undecided as before
  named <- list(
    desg = do.call(.p406, c(base, list(designation = c("Ah", "Bwy", "C")))),
    visible = do.call(.p406, c(base, list(designation = c("Ah", "Bw", "C"), gypsum_visible_pct = c(0, 2, 0)))),
    measured = do.call(.p406, c(base, list(designation = c("Ah", "Bw", "C"), caso4_pct = c(NA, 3, NA)))),
    parent = do.call(.p406, c(base, list(designation = c("Ah", "Bw", "C"),
                                         site = list(id = "g", parent_material = "Evaporites: anhydrite, gypsum")))))
  for (k in names(named)) {
    v <- soilKey:::.f88_gypsum(soilKey:::.f88_ctx(named[[k]]))
    expect_true(anyNA(v$value), info = k)
    expect_false(any(v$flags %in% "gypsum_not_measured_none_named"), info = k)
  }
  # an electrical conductivity still decides first
  q <- do.call(.p406, c(base, list(designation = c("Ah", "Bwy", "C"), ec_dS_m = 1)))
  expect_true(all(soilKey:::.f88_gypsum(soilKey:::.f88_ctx(q))$flags == "no_gypsum_at_ece_below_2"))
})

test_that("the readings of 0.9.406 are recorded with Hugo's date", {
  skip_on_cran()
  for (sys in c("fao1988", "fao1974")) {
    rd <- yaml::read_yaml(system.file("rules", sys, "readings.yaml", package = "soilKey"))$readings
    r406 <- Filter(function(r) identical(r$version, "0.9.406"), rd)
    expect_length(r406, 2L)
    for (r in r406) expect_match(r$reading, "\\(Hugo, 2026-11-13\\)\\.$")
  }
})
