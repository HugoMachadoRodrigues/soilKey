# v0.9.410 -- WRB 2022 base-status qualifiers: the rounding tie, the depth
# range from the mineral soil surface to a limiting layer, mineral layers only
# outside Histosols, Magnesic as one continuous layer, Oligoeutric, and the
# optional subqualifiers used by the name builder (WRB 2022 Ch 2.3, p. 26).

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("bases equal to 4 x Al count as bases >= 4 x Al despite the binary sum", {
  testthat::skip_on_cran()
  # 0.4 + 0.3 + 0.1 + 0 is stored just below 4 x 0.2 (ISIS ZW009, 33-44 cm)
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 100),
                     al_cmol = c(0, 0.2), ca_cmol = c(1, 0.4),
                     mg_cmol = c(0.3, 0.3), k_cmol = c(0.1, 0.1),
                     na_cmol = c(0, 0)))
  expect_true(qual_hypereutric(p)$passed)
})

test_that("the depth range stops at a limiting layer starting >= 50 cm", {
  testthat::skip_on_cran()
  # rock at 80 cm (ISIS CN045): the 80-100 cm were missing data before
  p <- mk(data.frame(top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 100),
                     designation = c("A", "Bw", "R"),
                     al_cmol = c(0, 0, NA), ca_cmol = c(10, 10, NA),
                     mg_cmol = c(2, 2, NA)))
  expect_true(qual_hypereutric(p)$passed)
  expect_true(qual_eutric(p)$passed)
})

test_that("the depth range is counted from the mineral soil surface", {
  testthat::skip_on_cran()
  # 30 cm of peat over mineral soil: 20-100 cm of the mineral soil is 50-130 cm
  p <- mk(data.frame(top_cm = c(0, 30, 50, 100), bottom_cm = c(30, 50, 100, 130),
                     oc_pct = c(35, 1, 1, 0.5), ph_h2o = c(4, 5, 5, 5),
                     al_cmol = c(NA, 3, 0, 0), ca_cmol = c(NA, 0.5, 5, 5),
                     mg_cmol = c(NA, 0.2, 1, 1)))
  w <- soilKey:::.wrb_base_status_window(p)
  expect_equal(w, c(50, 130))
  expect_true(qual_eutric(p)$passed)
})

test_that("outside Histosols organic layers are left out; in Histosols they decide", {
  testthat::skip_on_cran()
  # a buried acid organic layer at 20-60 cm over base-rich mineral soil
  p <- mk(data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
                     oc_pct = c(2, 30, 1), ph_h2o = c(6, 4, 6.5),
                     al_cmol = c(0, NA, 0), ca_cmol = c(8, NA, 9),
                     mg_cmol = c(2, NA, 2)))
  expect_true(qual_eutric(p)$passed)
  expect_false(qual_dystric(p)$passed)
  expect_true(qual_dystric(p, rsg_code = "HS")$passed)
})

test_that("Magnesic: one continuous layer >= 30 cm within 100 cm", {
  testthat::skip_on_cran()
  # two separate 20 cm layers do not make one of 40 cm (ISIS PE008, ZA008)
  gap <- mk(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                       ca_cmol = c(1, 3, 1), mg_cmol = c(2, 1, 2)))
  expect_false(qual_magnesic(gap)$passed)
  # only the 20 cm above 100 cm count
  deep <- mk(data.frame(top_cm = c(0, 80), bottom_cm = c(80, 140),
                        ca_cmol = c(3, 1), mg_cmol = c(1, 2)))
  expect_false(qual_magnesic(deep)$passed)
  # Ca or Mg not recorded: undecided
  na <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                      ca_cmol = c(1, NA), mg_cmol = c(2, NA)))
  expect_true(is.na(qual_magnesic(na)$passed))
  # Ca and Mg both 0 is not a ratio < 1
  zero <- mk(data.frame(top_cm = 0, bottom_cm = 60, ca_cmol = 0, mg_cmol = 0))
  expect_false(qual_magnesic(zero)$passed)
  # second branch: the major part above a limiting layer starting < 60 cm
  sh <- mk(data.frame(top_cm = c(0, 25, 40), bottom_cm = c(25, 40, 60),
                      designation = c("A", "B", "R"),
                      ca_cmol = c(1, 3, NA), mg_cmol = c(2, 1, NA)))
  expect_true(qual_magnesic(sh)$passed)
})

test_that("Oligoeutric: bases >= Al and bases < 5 cmolc/kg clay in the major part", {
  testthat::skip_on_cran()
  p <- mk(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 120),
                     clay_pct = c(50, 60), al_cmol = c(0.5, 0.4),
                     ca_cmol = c(1, 0.6), mg_cmol = c(0.3, 0.2),
                     k_cmol = c(0.1, 0.05), na_cmol = c(0, 0)))
  expect_true(qual_oligoeutric(p)$passed)
  expect_false(qual_oligoeutric(p, rsg_code = "HS")$passed)
  rich <- mk(data.frame(top_cm = 0, bottom_cm = 120, clay_pct = 30,
                        al_cmol = 0, ca_cmol = 8, mg_cmol = 2))
  expect_false(qual_oligoeutric(rich)$passed)
  noclay <- mk(data.frame(top_cm = 0, bottom_cm = 120, al_cmol = 0,
                          ca_cmol = 0.5, mg_cmol = 0.2))
  expect_true(is.na(qual_oligoeutric(noclay)$passed))
})

test_that("the name builder uses the optional subqualifiers in every RSG", {
  testthat::skip_on_cran()
  tab <- soilKey:::.wrb_optional_subqualifiers
  expect_identical(tab$Eutric, c("Oligoeutric", "Hypereutric"))
  expect_identical(tab$Dystric, "Hyperdystric")
  for (q in unique(unlist(tab)))
    expect_true(exists(paste0("qual_", tolower(q)), envir = asNamespace("soilKey")))
  # Cambisols list Dystric/Eutric only (Ch 4): Hypereutric replaces Eutric
  cm <- resolve_wrb_qualifiers(make_cambisol_canonical(), "CM")
  expect_true("Hypereutric" %in% cm$principal)
  expect_false("Eutric" %in% cm$principal)
  # Ferralsol fixture: bases 0.6-1.9 against 50-60% clay -> Oligoeutric,
  # which has preference over Hypereutric (Ch 5, p. 133)
  fr <- resolve_wrb_qualifiers(make_ferralsol_canonical(), "FR")
  expect_true("Oligoeutric" %in% fr$supplementary)
  # Hyperdystric replaces Dystric
  acid <- mk(data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                        designation = c("A", "Bw1", "Bw2"),
                        clay_pct = c(30, 32, 33), al_cmol = c(3, 3, 3),
                        ca_cmol = c(0.3, 0.2, 0.2), mg_cmol = c(0.1, 0.1, 0.1),
                        k_cmol = c(0.1, 0.1, 0.1), na_cmol = c(0, 0, 0)))
  r <- resolve_wrb_qualifiers(acid, "CM")
  expect_true("Hyperdystric" %in% r$principal)
  expect_false("Dystric" %in% r$principal)
})

test_that("an undecided preferred subqualifier leaves the qualifier", {
  testthat::skip_on_cran()
  # cations not recorded: Hypereutric holds by pH (Table 9.3), Oligoeutric is
  # undecided, so Eutric stays
  p <- mk(data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                     designation = c("A", "Bw1", "Bw2"), clay_pct = c(20, 22, 22),
                     oc_pct = c(1.5, 0.8, 0.5), ph_h2o = c(7, 7.2, 7.4)))
  expect_true(qual_hypereutric(p)$passed)
  expect_true(is.na(qual_oligoeutric(p)$passed))
  r <- resolve_wrb_qualifiers(p, "CM")
  expect_true("Eutric" %in% r$principal)
  expect_identical(r$trace$Hypereutric, NULL)
})
