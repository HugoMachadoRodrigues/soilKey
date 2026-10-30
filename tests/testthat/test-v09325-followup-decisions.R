# v0.9.315: readings decided by Hugo on 2026-10-30 after the follow-up round.
# Made-up profiles; no ISIS data is stored in the package.

test_that("an undecided earlier member keeps the texture group out of the name", {
  skip_on_cran()
  mk <- function(b_clay) {
    h <- data.frame(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "Bw"),
                    clay_pct = c(20, b_clay), silt_pct = c(40, if (is.na(b_clay)) NA else 40),
                    sand_pct = c(40, if (is.na(b_clay)) NA else 40),
                    oc_pct = c(1.2, 0.4), stringsAsFactors = FALSE)
    PedonRecord$new(site = list(id = "made-up"), horizons = h)
  }
  # the Bw has no particle size: Arenic and Clayic are undecided before Loamic
  p <- mk(NA_real_)
  expect_true(is.na(qual_arenic(p)$passed))
  expect_true(isTRUE(qual_loamic(p)$passed))
  r <- resolve_wrb_qualifiers(p, "CM")
  expect_false("Loamic" %in% r$supplementary)
  # with the Bw recorded as loam the group gives Loamic
  r2 <- resolve_wrb_qualifiers(mk(20), "CM")
  expect_true("Loamic" %in% r2$supplementary)
})

test_that("a limiting layer has no texture class", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "R"),
                  clay_pct = c(20, NA), silt_pct = c(40, NA), sand_pct = c(40, NA),
                  stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "made-up"), horizons = h)
  expect_false(qual_arenic(p)$passed)
  expect_false(qual_clayic(p)$passed)
  expect_true(qual_loamic(p)$passed)
})

.v09325_isis <- function(cracks = "N", dryfrom = 0) {
  cd <- function(item, description = item) list(item = item, description = description)
  list(profileCode = "ZZ001", countryCode = "ZZ",
       cracksSurface = if (is.na(cracks)) NULL else cd(cracks),
       dryfrom = dryfrom,
       layers = list(
         list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 30L, layerName = "A",
              clay = list("45"), structureType = cd("SB", "Subangular blocky")),
         list(layerNumber = 2L, upperDepth = 30L, lowerDepth = 100L, layerName = "Bw",
              clay = list("55"), structureType = cd("AB", "Angular blocky")),
         list(layerNumber = 3L, upperDepth = 100L, lowerDepth = 150L, layerName = "C",
              clay = list("50"))))
}

test_that("'no cracks observed' is no cracks only in a profile described dry", {
  skip_on_cran()
  w <- function(...) as.data.frame(isis_profile_to_pedon(.v09325_isis(...))$horizons)$cracks_width_cm
  expect_equal(w(cracks = "N", dryfrom = 0), c(0, 0, NA))         # horizons starting above 100 cm
  expect_true(all(is.na(w(cracks = "N", dryfrom = NA))))         # described moist: unknown
  expect_equal(w(cracks = "M", dryfrom = NA), c(1, 1, NA))       # medium cracks, 1-2 cm
  expect_true(all(is.na(w(cracks = "F", dryfrom = 0))))          # fine (< 1 cm) is not read
  expect_true(all(is.na(w(cracks = NA, dryfrom = 0))))
  pv <- isis_profile_to_pedon(.v09325_isis())$provenance
  expect_true(any(grepl("described dry at the surface", pv$notes)))
})

test_that("the arid canonical fixtures record their moisture regime", {
  skip_on_cran()
  for (f in c("make_calcisol_canonical", "make_gypsisol_canonical",
              "make_solonchak_canonical", "make_durisol_canonical")) {
    p <- get(f, asNamespace("soilKey"))()
    expect_identical(p$site$soil_moisture_regime, "aridic")
  }
  expect_match(classify_usda(make_calcisol_canonical(), on_missing = "silent")$name, "ids$")
  expect_match(classify_usda(make_solonchak_canonical(), on_missing = "silent")$name, "ids$")
})
