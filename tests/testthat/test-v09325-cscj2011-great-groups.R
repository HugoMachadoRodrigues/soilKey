# v0.9.325: the ten great groups of the Comprehensive Soil Classification
# System of Japan, First Approximation (CSCJ 2011). Made-up profiles; no
# Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.gg <- function(p) {
  r <- classify_cscj2011(p, on_missing = "silent")
  c(code = r$levels$code[1], prov = paste(r$provisional_because, collapse = ","))
}

test_that("every example keys to its great group, decided, named in English and Japanese", {
  skip_on_cran()
  want <- c(man_made = "A", organic = "B", podzol = "C", andosol = "D", dark_red = "E", lowland = "F",
            red_yellow = "G", stagnic = "H", brown_forest = "I", regosol = "J")
  for (g in names(want)) {
    r <- classify_cscj2011(make_cscj2011_example(g), on_missing = "silent")
    expect_identical(r$levels$code[1], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
    expect_identical(r$system_id, "cscj2011")
  }
  r <- classify_cscj2011(make_cscj2011_example("dark_red"), on_missing = "silent")
  expect_identical(r$name, "Dark Red soils")
  expect_identical(r$levels$name_native[1], "\u6697\u8d64\u8272\u571f\u5927\u7fa4")
  expect_true("cscj2011" %in% soilkey_systems()$id)
  # the SCSJ reads the same examples as before
  expect_identical(classify_scsj2017(make_scsj2017_example("eutrosol"), on_missing = "silent")$levels$code[1], "H")
})

test_that("the edition: the CSCJ argic limits and immature kuroboku feature", {
  skip_on_cran()
  # clay 12 % over 15 %: argic by the CSCJ (+3 under 15 %), not by the SCSJ (x1.4)
  h <- list(designation = c("A", "B", "C"), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 110),
            clay_pct = c(12, 15, 12), clay_films_amount = c("none", "none", "none"), oc_pct = c(2, 0.5, 0.3),
            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 4, 4))
  p <- .jp(h)
  x_s <- .scsj_profile(p); x_c <- .scsj_profile(soilKey:::.cscj_mark(p))
  expect_identical(x_c$edition, "cscj2011")
  expect_false(isTRUE(.scsj_argic_rows(x_s)[2]))
  expect_true(.scsj_argic_rows(x_c)[2])
  # immature kuroboku by Alo + 1/2 Feo: the SCSJ only
  k <- list(designation = c("A", "C"), top_cm = c(0, 30), bottom_cm = c(30, 100), oc_pct = c(1.5, 0.5),
            al_ox_pct = c(0.8, 0.6), fe_ox_pct = c(0.4, 0.4), layer_origin = "volcanic ash",
            munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5), munsell_chroma_moist = c(3, 4))
  p <- .jp(k)
  expect_true(.scsj_profile(p)$ikb_raw[1])
  expect_true(is.na(.scsj_profile(soilKey:::.cscj_mark(p))$ikb_raw[1]))
})

test_that("Podzols before Andosols; the Man-made soils without the SCSJ Regosol rule", {
  skip_on_cran()
  # a kuroboku soil with a bleached E over a Bs: a Podzol in the CSCJ, a
  # Podzolic Andosol in the SCSJ
  pz <- list(designation = c("A", "E", "Bs", "C"), top_cm = c(0, 15, 25, 60), bottom_cm = c(15, 25, 60, 100),
             oc_pct = c(8, 1, 3, 1), phosphate_absorption_coef = c(2000, 1600, 2100, 1800),
             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 6, 4, 5),
             munsell_chroma_moist = c(1, 2, 6, 4), al_ox_pct = c(2.5, 0.3, 3.0, 2.0), fe_ox_pct = c(1, 0.1, 1.5, 0.8),
             layer_origin = "volcanic ash", redoximorphic_features_pct = 0, field_texture = "loam", clay_pct = 15)
  p <- .jp(pz)
  expect_identical(classify_scsj2017(p, on_missing = "silent")$levels$code[1:2], c("C", "C1"))
  expect_identical(.gg(p)[["code"]], "C")
})

test_that("Dark Red soils: dark red, or limestone with a high base status; no rock within 30 cm", {
  skip_on_cran()
  ex <- function() make_cscj2011_example("dark_red")
  base <- ex()
  g <- function(p) dark_red_soils_cscj(soilKey:::.cscj_mark(p))$passed
  expect_true(g(base))
  # dark red without limestone
  d <- ex(); d$horizons$layer_origin <- "weathered basalt"; d$horizons$rock_origin <- c(NA, NA, NA, NA, "basalt")
  d$site$parent_material <- "basalt"; d$horizons$caco3_pct <- 0
  expect_true(g(d))
  # limestone, not dark red: the base status decides
  l <- ex(); l$horizons$munsell_hue_moist <- c("10YR", "10YR", "10YR", "10YR", NA)
  l$horizons$munsell_value_moist <- c(3, 4, 4, 5, NA); l$horizons$munsell_chroma_moist <- c(2, 4, 4, 4, NA)
  expect_true(g(l))
  l$horizons$ph_h2o <- c(5.2, 5.0, 5.3, 6.0, NA); l$horizons$bs_pct <- c(30, 25, 35, 60, NA)
  expect_false(g(l))
  # liming makes the high status undecided
  l2 <- ex(); l2$horizons$munsell_hue_moist <- l$horizons$munsell_hue_moist
  l2$horizons$munsell_value_moist <- l$horizons$munsell_value_moist
  l2$horizons$munsell_chroma_moist <- l$horizons$munsell_chroma_moist
  l2$site$acid_neutralising_amendment <- TRUE
  expect_true(is.na(g(l2)))
  # neither dark red nor limestone: not Dark Red; the colour words
  n <- l$clone(deep = TRUE); n$horizons$layer_origin <- "weathered granite"; n$horizons$rock_origin <- c(NA, NA, NA, NA, "granite")
  n$site$parent_material <- "granite"; n$horizons$caco3_pct <- 0
  expect_false(g(n))
  cw <- .scsj_colour_words(data.frame(munsell_hue_moist = c("5YR", "5YR", "2.5YR", "7.5YR"),
                                      munsell_value_moist = c(3, 4, 2, 3), munsell_chroma_moist = c(4, 4, 6, 4)), "cscj2011")
  expect_identical(cw$dark_red, c(TRUE, TRUE, TRUE, FALSE))
  expect_identical(cw$black, c(FALSE, FALSE, FALSE, FALSE))
  # rock within 30 cm
  r <- ex(); r$horizons$top_cm <- c(0, 12, 25, 28, 30); r$horizons$bottom_cm <- c(12, 25, 28, 30, 60)
  expect_false(g(r))
})

test_that("Red-Yellow soils of the CSCJ: no base-status criterion", {
  skip_on_cran()
  ry <- make_cscj2011_example("red_yellow")
  ry$horizons$ph_h2o <- c(6.5, 6.8, 7.0, 7.0); ry$horizons$bs_pct <- c(80, 85, 90, 90)
  expect_identical(.gg(ry)[["code"]], "G")
  # the SCSJ asks for an acid or unsaturated subhorizon
  expect_false(isTRUE(red_yellow_soils_scsj(ry)$passed))
})
