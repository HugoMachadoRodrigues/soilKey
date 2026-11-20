# v0.9.416 -- the ISIS reader after the Soil Taxonomy audit (Hugo,
# 2026-11-20, audit section 2.7, items B1 to B5): the field texture class and
# its reading where clay and sand are not measured, chroma 0, the base
# saturation of layers whose bases lack Ca, the optical density of the oxalate
# extract, the decomposition of peat, the duripan share and sand from clay and
# silt. Every profile below is synthetic.

.it416 <- function(x) list(item = x, description = NULL)
.p416 <- function(layers) {
  list(profileCode = "XX416", countryCode = NULL, layers = layers,
       wrbClassifications = list(), usdaClassifications = list())
}
.ped416 <- function(...) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = list(id = "v416", country = "XX"),
                  horizons = ensure_horizon_schema(hz))
}

# ---- the texture class -----------------------------------------------------

test_that("a texture class reads as sandy loam or finer, vfs or finer, lfs or coarser", {
  skip_on_cran()
  r <- soilKey:::.texture_class_reading(c("SiL", "LS", "LFS", "LVFS", "VFS", "S", "US",
                                          "CS", "CSL", "fine sandy loam", "C", "HC", "xx", NA))
  expect_identical(r$class, c("SIL", "LS", "LFS", "LVFS", "VFS", "S", "US", "COS", "COSL",
                              "FSL", "C", "C", NA, NA))
  expect_identical(r$sandy_loam_or_finer,
                   c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, NA, NA))
  # KST 13th ed., p. 22: very fine sand, loamy very fine sand or finer; S and US have no grade
  expect_identical(r$vfs_or_finer,
                   c(TRUE, FALSE, FALSE, TRUE, TRUE, NA, NA, FALSE, TRUE, TRUE, TRUE, TRUE, NA, NA))
  # KST 13th ed., p. 165: loamy fine sand or coarser
  expect_identical(r$lfs_or_coarser,
                   c(FALSE, TRUE, TRUE, FALSE, FALSE, NA, NA, TRUE, FALSE, FALSE, FALSE, FALSE, NA, NA))
})

test_that("the texture test reads the field class only where clay and sand are not measured", {
  skip_on_cran()
  h <- data.table::data.table(top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 80),
                              clay_pct = c(NA, NA, 5, 25), silt_pct = c(NA, NA, 5, 30),
                              sand_pct = c(NA, NA, 90, 45),
                              texture_class_usda = c("SiL", "LS", "C", NA))
  t <- soilKey:::test_texture_argic(h)
  expect_true(t$passed)
  expect_identical(t$layers, c(1L, 4L))   # layer 3: the lab sand wins over the field clay
  expect_identical(t$details[["1"]]$texture_class_usda, "SIL")
  # no lab texture and no class: undecided, as before
  h2 <- data.table::data.table(top_cm = 0, bottom_cm = 20, clay_pct = NA_real_,
                               silt_pct = NA_real_, sand_pct = NA_real_,
                               texture_class_usda = NA_character_)
  expect_true(is.na(soilKey:::test_texture_argic(h2)$passed))
  # a loamy sand decides "not sandy loam or finer"
  h2$texture_class_usda <- "LS"
  expect_false(soilKey:::test_texture_argic(h2)$passed)
})

test_that("Psamments read the field class where clay and sand are not measured", {
  skip_on_cran()
  p <- .ped416(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "C1", "C2"),
               texture_class_usda = c("LFS", "FS", "S"))
  # v0.9.419: every layer of the particle-size control section (25 to 100
  # cm, KST p. 165); a sand of no known grade (S) leaves it undecided
  r <- soilKey:::psamment_qualifying_usda(p)
  expect_true(is.na(r$passed))
  expect_identical(r$layers, 2:3)
  p$horizons$texture_class_usda[3] <- "MS"
  expect_true(soilKey:::psamment_qualifying_usda(p)$passed)
  p2 <- .ped416(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "C1", "C2"),
                texture_class_usda = c("LVFS", "SL", "LS"))
  expect_false(soilKey:::psamment_qualifying_usda(p2)$passed)
})

# ---- the reader --------------------------------------------------------------

test_that("ISIS textureClassesFao2006Small is the field texture class", {
  skip_on_cran()
  L <- list(list(upperDepth = 0, lowerDepth = 20, layerName = "A", textureClassesFao2006Small = .it416("SiL")),
            list(upperDepth = 20, lowerDepth = 50, layerName = "Bw", textureClassesFao2006Small = .it416("CSL"),
                 clay = "12", silt = "20", sand = "68"))
  p <- isis_profile_to_pedon(.p416(L))
  expect_identical(p$horizons$texture_class_usda, c("SiL", "CSL"))
  expect_true(any(p$provenance$attribute == "texture_class_usda"))
  expect_true(grepl("textureClassesFao2006Small", soilKey:::.isis_profile_fields()))
})

test_that("chroma 0 is read from the colour written whole, and N has chroma 0", {
  skip_on_cran()
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "Ah", colourMoistHue = .it416("7.5YR"),
         colourMoistValue = .it416("2"), colourMoistMatrix = .it416("7.5YR 2/0")),
    list(upperDepth = 20, lowerDepth = 40, layerName = "Bt", colourMoistHue = .it416("10YR"),
         colourMoistValue = .it416("3"), colourMoistMatrix = .it416("2.5Y 3/0")),
    list(upperDepth = 40, lowerDepth = 60, layerName = "C", colourMoistMatrix = .it416("N 5/")),
    list(upperDepth = 60, lowerDepth = 80, layerName = "Cg", colourMoistHue = .it416("10YR"),
         colourMoistValue = .it416("5"), colourMoistMatrix = .it416("10YR 5/1")))
  h <- isis_profile_to_pedon(.p416(L))$horizons
  expect_identical(h$munsell_chroma_moist[1], 0)
  expect_true(is.na(h$munsell_chroma_moist[2]))     # another hue: not read
  expect_identical(h$munsell_hue_moist[3], "N")
  expect_identical(h$munsell_chroma_moist[3], 0)    # the neutral hue
  expect_true(is.na(h$munsell_chroma_moist[4]))     # only chroma 0 is read so
})

test_that("bases recorded without Ca give no base saturation", {
  skip_on_cran()
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "Ak", mgExcph7 = "1.2", kExcph7 = "1.1",
         naExcph7 = "0", exbas = "2.3", cecph7 = "47", tceq = "190"),
    list(upperDepth = 20, lowerDepth = 50, layerName = "Bw", caExcph7 = "10", mgExcph7 = "2",
         kExcph7 = "0.5", naExcph7 = "0.1", exbas = "12.6", cecph7 = "20"),
    list(upperDepth = 50, lowerDepth = 80, layerName = "C", exbas = "8", cecph7 = "10"))
  p <- isis_profile_to_pedon(.p416(L))
  h <- p$horizons
  expect_true(is.na(h$bs_pct[1]))
  expect_true(is.na(h$ecec_cmol[1]))
  expect_equal(h$bs_pct[2], 63)
  expect_equal(h$bs_pct[3], 80)                     # a total recorded on its own is read
  expect_match(p$site$depth_note, "without Ca in 1 layer")
})

test_that("sand is 100 - clay - silt when only those two are given", {
  skip_on_cran()
  L <- list(list(upperDepth = 0, lowerDepth = 20, layerName = "Ap", clay = "11.1", silt = "26.1"))
  p <- isis_profile_to_pedon(.p416(L))
  expect_equal(p$horizons$sand_pct, 62.8)
  expect_match(p$provenance$notes[p$provenance$attribute == "sand_pct"], "100 - clay - silt")
})

test_that("ODOE, peat decomposition and the duripan share are read", {
  skip_on_cran()
  L <- list(
    list(upperDepth = 0, lowerDepth = 10, layerName = "Oi", organicMattterDecomprate = .it416("S")),
    list(upperDepth = 10, lowerDepth = 20, layerName = "Ah", organicMattterDecomprate = .it416("H"),
         colourMoistHue = .it416("10YR")),
    list(upperDepth = 20, lowerDepth = 40, layerName = "Bhs", opticalDensityOxalate = "0.31",
         colourMoistHue = .it416("5YR")),
    list(upperDepth = 40, lowerDepth = 60, layerName = "Bqm1", pansKind = .it416("D"),
         cementCompactDegree = .it416("I"), cementCompactContinuity = .it416("D")),
    list(upperDepth = 60, lowerDepth = 80, layerName = "Bqm2", pansKind = .it416("D"),
         cementCompactDegree = .it416("S")),
    list(upperDepth = 80, lowerDepth = 90, layerName = "Bm", pansKind = .it416("D"),
         cementCompactDegree = .it416("S"), cementCompactContinuity = .it416("B")),
    list(upperDepth = 90, lowerDepth = 120, layerName = "Cqm", colourMoistHue = .it416("10YR")))
  h <- isis_profile_to_pedon(.p416(L))$horizons
  expect_equal(h$odoe[3], 0.31)
  expect_identical(h$von_post_index[1:2], c(3L, NA))       # an A horizon is not peat
  expect_identical(h$duripan_pct[4:6], c(70, 95, 25))       # continuity; the m; broken
  expect_true(is.na(h$duripan_pct[7]))                      # the designation alone stays unknown
  expect_identical(h$cementation_class[4], "indurated")
})
