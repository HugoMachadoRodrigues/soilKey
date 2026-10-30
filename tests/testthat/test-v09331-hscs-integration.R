# v0.9.316: the HSCS 2024 subtype and variety properties end to end (Micheli
# et al. 2024): the guide's own naming example (p. 7), the hortic route of the
# anthropogenic horizon, the ISIS mottle size, and the degree rows. Made-up
# profiles.

.v09331_guide <- function(...) {
  h <- data.frame(
    designation = c("A", "E", "Bt1", "Bt2", "C"),
    top_cm = c(0, 15, 35, 70, 100), bottom_cm = c(15, 35, 70, 100, 130),
    clay_pct = c(14, 12, 28, 26, 20), oc_pct = c(0.45, 0.3, 0.25, 0.15, 0.1),
    ph_h2o = c(5.2, 5.0, 4.9, 5.1, 5.3), bs_pct = c(35, 30, 32, 36, 40),
    caco3_pct = 0, effervescence_class = "none",
    coarse_fragments_pct = c(15, 50, 55, 60, 70), coarse_fragments_shape = "angular",
    clay_films_amount = c(NA, NA, "common", "common", NA),
    structure_grade = c("weak", "weak", "moderate", "moderate", "structureless"),
    structure_type = c("granular", "platy", "subangular blocky", "subangular blocky", "massive"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 4, 4, 5), munsell_chroma_moist = c(3, 3, 4, 4, 4),
    munsell_value_dry = c(5, 5, 5, 5, 6), munsell_chroma_dry = c(4, 4, 4, 4, 4),
    redoximorphic_features_pct = 0, soluble_salts_pct = 0.02, na_cmol = 0.1, mg_cmol = 1, ca_cmol = 3,
    cec_cmol = 10, slickensides = "absent", artefacts_pct = 0, stringsAsFactors = FALSE)
  mod <- list(...)
  for (nm in names(mod)) h[[nm]] <- mod[[nm]]
  PedonRecord$new(site = list(id = "guide-example", country = "HU", designation_system = "fao2006",
                              land_use = "forest"),
                  horizons = ensure_horizon_schema(data.table::as.data.table(h)))
}

test_that("the guide's naming example comes out as the guide writes it (p. 7)", {
  skip_on_cran()
  r <- classify_hscs2024(.v09331_guide(), on_missing = "silent")
  expect_identical(r$name, "Savany\u00fa, humuszszeg\u00e9ny Agyagbemos\u00f3d\u00e1sos talaj (sek\u00e9lyen k\u0151zett\u00f6rmel\u00e9kes)")
  expect_identical(r$levels$level, c("types", "subtype", "subtype", "variety"))
  expect_identical(r$levels$code[-1], c("savanyu", "humuszszegeny", "kozettormelekes"))
  st <- r$qualifiers$status
  expect_equal(st$top[st$id == "kozettormelekes"], 15)
  # base saturation over 50 % turns the base-status row to telitetlen
  r2 <- classify_hscs2024(.v09331_guide(bs_pct = c(60, 60, 65, 70, 70), ph_h2o = 6.2), on_missing = "silent")
  expect_match(r2$name, "^Tel\u00edtetlen, humuszszeg\u00e9ny Agyagbemos\u00f3d\u00e1sos talaj")
  # rounded fragments make it kavicsos instead
  r3 <- classify_hscs2024(.v09331_guide(coarse_fragments_shape = "rounded"), on_missing = "silent")
  expect_match(r3$name, "\\(sek\u00e9lyen kavicsos\\)$")
})

test_that("a stronger degree that is undecided leaves its row unnamed", {
  skip_on_cran()
  # no base saturation and no pH: the base-status row names nothing
  r <- classify_hscs2024(.v09331_guide(bs_pct = NA, ph_h2o = NA), on_missing = "silent")
  expect_false(grepl("Savany\u00fa|Tel\u00edtett|Tel\u00edtetlen", r$name))
  st <- r$qualifiers$status
  expect_true(is.na(st$passed[st$id == "telitett"]))
})

test_that("the hortic route makes the anthropogenic horizon (8.1, criterion 1)", {
  skip_on_cran()
  mk <- function(p2o5) {
    h <- data.frame(designation = c("Ap", "A2", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                    clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = c(3, 2, 0.3),
                    p2o5_al_mg_kg = p2o5, caco3_pct = 0, soluble_salts_pct = 0.02,
                    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = 2,
                    stringsAsFactors = FALSE)
    PedonRecord$new(site = list(id = "hortic", country = "HU", designation_system = "fao2006"),
                    horizons = ensure_horizon_schema(data.table::as.data.table(h)))
  }
  r <- classify_hscs2024(mk(c(900, 700, 50)), on_missing = "silent")
  expect_identical(r$levels$code[1], "ANT")
  expect_match(r$name, "hortikus|Hortikus")
  expect_false(identical(classify_hscs2024(mk(c(200, 150, 50)), on_missing = "silent")$levels$code[1], "ANT"))
})

test_that("the ISIS reader reads the mottle size (FAO 2006 classes)", {
  skip_on_cran()
  cd <- function(item) list(item = item, description = item)
  prof <- list(profileCode = "ZZ002", countryCode = "ZZ", layers = list(
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 30L, layerName = "A", clay = list("30")),
    list(layerNumber = 2L, upperDepth = 30L, lowerDepth = 70L, layerName = "Bg", clay = list("35"),
         mottlesAbundance = cd("C"), mottlesSizeclass = cd("A")),
    list(layerNumber = 3L, upperDepth = 70L, lowerDepth = 120L, layerName = "Cg", clay = list("30"),
         mottlesAbundance = cd("F"), mottlesSizeclass = cd("C"))))
  h <- as.data.frame(isis_profile_to_pedon(prof)$horizons)
  expect_identical(h$mottle_size_class, c(NA, "coarse", NA))   # the old C class is not read
})
