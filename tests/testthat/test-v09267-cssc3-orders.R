# v0.9.267: the Canadian System of Soil Classification, 3rd ed., keyed to the
# order (Key to Soil Orders, Ch 3, pp. 33-34): the designation parser, the
# Ch 2 diagnostics read by properties first and designations second, and the
# order key through the generic classifier.

test_that("Canadian designations are parsed in their own notation", {
  p <- soilKey:::.parse_designation_cssc(
    c("Bfjtj", "IIBt", "LFH", "Bhf", "Bfh", "Ae and Bt", "Ckg", "Cca",
      "Bssk", "Oco", "Bm2", "IC", NA))
  comp <- function(i, k = 1L) p[[i]][[k]]
  expect_identical(comp(1)$suffixes, c("f", "t"))
  expect_identical(comp(1)$weak, c("f", "t"))
  expect_identical(comp(2)$roman, "II"); expect_identical(comp(2)$master, "B")
  expect_identical(comp(3)$master, "LFH")
  expect_identical(comp(4)$suffixes, "hf")
  expect_identical(comp(5)$suffixes, "hf")
  expect_length(p[[6]], 2L)
  expect_identical(comp(7)$suffixes, c("k", "g"))
  expect_identical(comp(8)$suffixes, "ca")
  expect_identical(comp(9)$suffixes, c("ss", "k"))
  expect_identical(comp(10)$suffixes, "co")
  expect_identical(comp(11)$suffixes, "m")
  expect_identical(comp(12)$roman, "I"); expect_identical(comp(12)$master, "C")
  expect_length(p[[13]], 0L)
  # f is not plinthite and Btj is not a Bt
  is_bt <- soilKey:::.cssc_is(p[1:2], "B", "t", weak = "no")
  expect_identical(is_bt, c(FALSE, TRUE))
})

test_that("each example pedon keys to its order", {
  skip_on_cran()
  for (o in c("cryosolic", "organic", "vertisolic", "podzolic", "gleysolic",
              "solonetzic", "chernozemic", "luvisolic", "brunisolic",
              "regosolic")) {
    r <- classify_cssc3(make_cssc3_example(o), on_missing = "silent")
    expect_identical(tolower(r$name), o, info = o)
    expect_identical(r$system_id, "cssc3")
    expect_identical(r$levels$level, "orders")
  }
})

test_that("the podzolic example keys to the Podzolic order on CRAN too", {
  r <- classify_cssc3(make_cssc3_example("podzolic"), on_missing = "silent")
  expect_identical(r$name, "Podzolic")
})

test_that("a Bt within 50 cm takes a podzolic B out of the Podzolic order", {
  skip_on_cran()
  p <- make_cssc3_example("podzolic")
  p$horizons$designation[4] <- "Bt"
  p$horizons$clay_pct[4] <- 14
  p$horizons$clay_films_amount[4] <- "common"
  r <- classify_cssc3(p, on_missing = "silent")
  expect_identical(r$name, "Luvisolic")
  expect_false(isTRUE(r$trace$orders$D$passed))
})

test_that("designations decide when the properties are missing, and say so", {
  skip_on_cran()
  p <- make_cssc3_example("podzolic")
  p$horizons$fe_pyro_pct <- NA_real_
  p$horizons$al_pyro_pct <- NA_real_
  r <- classify_cssc3(p, on_missing = "silent")
  expect_identical(r$name, "Podzolic")
  expect_true(any(grepl("surveyor's horizon designations", r$warnings)))
  d <- soilKey:::podzolic_b_cssc(p)
  expect_identical(d$evidence$route, "designations")
  # with the properties, the route is the properties
  d <- soilKey:::podzolic_b_cssc(make_cssc3_example("podzolic"))
  expect_identical(d$evidence$route, "properties")
})

test_that("an unknown soil climate leaves the Chernozemic order undecided", {
  skip_on_cran()
  p <- make_cssc3_example("chernozemic")
  p$site$mean_annual_soil_temp_C <- NULL
  p$site$soil_moisture_subclass <- NULL
  r <- classify_cssc3(p, on_missing = "silent")
  expect_identical(r$name, "Brunisolic")
  expect_identical(r$provisional_because, "orders:G")
  # the USDA regimes stand in when the Canadian fields are absent
  p$site$soil_temperature_regime <- "frigid"
  p$site$soil_moisture_regime <- "ustic"
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Chernozemic")
})

test_that("a dark Ah is not read as gleyed", {
  skip_on_cran()
  p <- make_cssc3_example("brunisolic")
  p$horizons$munsell_chroma_moist[1] <- 1
  expect_false(soilKey:::gleying_cssc(p)$passed)
  p$horizons$munsell_chroma_moist[2] <- 1
  expect_true(soilKey:::gleying_cssc(p)$passed)
})

test_that("classify_all runs the Canadian system only when asked", {
  skip_on_cran()
  p <- make_cssc3_example("luvisolic")
  res <- classify_all(p, on_missing = "silent")
  expect_named(res, c("wrb", "sibcs", "usda", "summary"))
  res <- classify_all(p, systems = c("wrb", "cssc"), on_missing = "silent")
  expect_named(res, c("wrb", "sibcs", "usda", "cssc3", "summary"))
  expect_identical(res$summary$cssc3, "Luvisolic")
  expect_identical(load_rules("cssc"), load_rules("cssc3"))
})

# ---- cases from the review (each with the book's answer) ---------------------

.mk_cssc <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("Organic option 2 needs a mineral layer within 40 cm; folic over rock counts", {
  skip_on_cran()
  # fibric surface needs 60 cm; with the mineral C at 55 cm, option 2 does not apply
  p <- .mk_cssc(list(designation = c("Of", "C"), top_cm = c(0, 55), bottom_cm = c(55, 100),
                     oc_pct = c(45, 0.5), bulk_density_g_cm3 = c(0.05, 1.4)))
  expect_false(soilKey:::organic_soil_cssc(p)$passed)
  # folic material more than twice a thin mineral layer over rock (p. 33, B.1.c)
  p <- .mk_cssc(list(designation = c("LFH", "Bm", "R"), top_cm = c(0, 20, 28),
                     bottom_cm = c(20, 28, 40), oc_pct = c(40, 1, NA)))
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Organic")
})

test_that("measured properties that fail beat a designation", {
  skip_on_cran()
  p <- make_cssc3_example("podzolic")
  p$horizons$fe_pyro_pct[3] <- 0.05
  p$horizons$al_pyro_pct[3] <- 0.05
  d <- soilKey:::podzolic_b_cssc(p)
  expect_false(d$passed)
  expect_identical(d$evidence$route, "properties")
})

test_that("FAO/USDA designations are not read as Canadian", {
  skip_on_cran()
  p <- .mk_cssc(list(designation = c("Ap", "Bw", "Bkm", "C"), top_cm = c(0, 20, 50, 80),
                     bottom_cm = c(20, 50, 80, 120)))
  expect_false(soilKey:::.cssc_notation(p$horizons$designation, p$site)$ok)
  expect_false(isTRUE(soilKey:::brunisolic_b_cssc(p)$passed))
  # Canadian two-letter suffixes are not signs of another notation
  expect_true(soilKey:::.cssc_notation(c("Ah", "Bm", "Cca", "Csa", "Oco", "Bgf"),
                                       list())$ok)
  # a declared notation wins
  expect_true(soilKey:::.cssc_notation(c("Ah", "Bm"),
                                       list(country = "US", designation_system = "cssc"))$ok)
  expect_false(soilKey:::.cssc_notation(c("Ah", "Bm"), list(country = "US"))$ok)
})

test_that("gley details: Bgf, neutral hue, unknown mottle contrast, dark AB", {
  skip_on_cran()
  p <- .mk_cssc(list(designation = c("Ah", "Bgf", "C"), top_cm = c(0, 15, 50),
                     bottom_cm = c(15, 50, 100)))
  expect_true(soilKey:::gleying_cssc(p)$passed)
  p <- .mk_cssc(list(top_cm = c(0, 20), bottom_cm = c(20, 100),
                     munsell_hue_moist = c("10YR", "N"), munsell_value_moist = c(2, 5),
                     munsell_chroma_moist = c(2, NA)))
  expect_true(soilKey:::gleying_cssc(p)$passed)
  p <- .mk_cssc(list(top_cm = c(0, 20), bottom_cm = c(20, 100),
                     munsell_hue_moist = c("10YR", "10YR"), munsell_value_moist = c(2, 5),
                     munsell_chroma_moist = c(2, 2), redoximorphic_features_pct = c(NA, 15)))
  expect_true(is.na(soilKey:::gleying_cssc(p)$passed))
  p <- .mk_cssc(list(designation = c("Ah", "AB", "Bm", "Cca"), top_cm = c(0, 15, 30, 60),
                     bottom_cm = c(15, 30, 60, 100),
                     munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR"),
                     munsell_value_moist = c(2, 3, 4, 5), munsell_chroma_moist = c(1, 1, 3, 3)))
  expect_false(soilKey:::gleying_cssc(p)$passed)
})

test_that("an Aej thinner than the Ah above meets the Chernozemic Ae condition", {
  skip_on_cran()
  p <- make_cssc3_example("chernozemic")
  h <- p$horizons
  h <- rbind(h[1], h[1], h[2:3])
  h$designation <- c("Ah", "Aej", "Bm", "Cca")
  h$top_cm <- c(0, 15, 19, 40); h$bottom_cm <- c(15, 19, 40, 90)
  h$munsell_value_dry[2] <- NA
  p$horizons <- h
  expect_true(soilKey:::chernozemic_ae_cssc(p)$passed)
})

test_that("slickensides recorded absent rule out the Vertisolic order", {
  skip_on_cran()
  p <- .mk_cssc(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
                     slickensides = c("absent", "few", "absent")))
  expect_false(soilKey:::vertisolic_cssc(p)$passed)
})
