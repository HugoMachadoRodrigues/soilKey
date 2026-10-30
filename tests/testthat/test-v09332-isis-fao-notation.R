# v0.9.317: ISIS profiles carry FAO horizon designations, declared as such, so
# the Canadian key does not read the FAO z (salts) as its z (a frozen layer).
# A made-up profile in the ISIS shape; no ISIS data is stored in the package.

test_that("an ISIS profile declares the FAO notation and is no Cryosol from FAO z horizons", {
  skip_on_cran()
  cd <- function(item) list(item = item, description = item)
  prof <- list(profileCode = "CA999", countryCode = "CA", layers = list(
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 15L, layerName = "Ah", clay = list("25"),
         orgc = list("30")),
    list(layerNumber = 2L, upperDepth = 15L, lowerDepth = 40L, layerName = "Btn", clay = list("38")),
    list(layerNumber = 3L, upperDepth = 40L, lowerDepth = 90L, layerName = "Ckz1", clay = list("30")),
    list(layerNumber = 4L, upperDepth = 90L, lowerDepth = 120L, layerName = "Ckz2", clay = list("28"))))
  p <- isis_profile_to_pedon(prof)
  expect_identical(p$site$designation_system, "fao2006")
  expect_false(isTRUE(soilKey:::cryosolic_cssc(p)$passed))
  r <- classify_cssc3(p, on_missing = "silent")
  expect_false(identical(r$levels$name[r$levels$level == "orders"][1], "Cryosolic"))
})
