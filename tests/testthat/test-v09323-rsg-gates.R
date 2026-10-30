# v0.9.323: WRB 2022 Ch 4 gates read as the key words them: path 2 of the
# Gleysols (p. 103), a mollic or umbric horizon over a gleyic layer.
# Made-up profiles.

# A dark, carbon-rich topsoil over a grey reduced subsoil. With bs 80 the
# topsoil is a mollic horizon, with bs 30 an umbric horizon.
.v09323_gl <- function(top = c(0, 25, 55), bottom = c(25, 55, 90),
                       designation = c("Ah1", "Ahg2", "Bg"),
                       hue = c("10YR", "10YR", "5GY"), vm = c(2, 2, 5),
                       cm = c(1, 1, 1), vd = c(3, 3, 6), oc = c(3, 2, 0.3),
                       redox = c(0, 3, 0), bs = 80) {
  h <- data.frame(top_cm = top, bottom_cm = bottom, designation = designation,
                  munsell_hue_moist = hue, munsell_value_moist = vm,
                  munsell_chroma_moist = cm, munsell_hue_dry = hue,
                  munsell_value_dry = vd, munsell_chroma_dry = cm,
                  oc_pct = oc, clay_pct = 25, sand_pct = 40, silt_pct = 35,
                  bs_pct = bs, ph_h2o = ifelse(bs < 50, 5, 6.8),
                  structure_type = "subangular blocky", structure_grade = "moderate",
                  structure_size = "medium", redoximorphic_features_pct = redox,
                  stringsAsFactors = FALSE)
  PedonRecord$new(site = list(id = "made-up"), horizons = h)
}


# ---- Gleysols, path 2 ----------------------------------------------------------

test_that("Gleysols path 2: a mollic horizon over a gleyic layer", {
  skip_on_cran()
  # mollic 0-55 cm, reducing (g) in the Ahg2 below 40 cm; Bg 55-90 gleyic and
  # reducing. Path 1 fails: the gleyic layer starts at 55 cm.
  p <- .v09323_gl()
  expect_true(isTRUE(mollic(p)$passed))
  g <- gleysol(p)
  expect_false(isTRUE(g$evidence$path1_ok))
  expect_true(isTRUE(g$evidence$path2_ok))
  expect_true(isTRUE(g$passed))
  expect_equal(g$evidence$path2$horizon, "mollic")
  expect_equal(g$layers, 1:3)
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Gleysols")
  # strict mode does not touch path 2
  expect_true(isTRUE(gleysol(p, strict = TRUE)$passed))
})

test_that("Gleysols path 2: an umbric horizon reads the same", {
  skip_on_cran()
  p <- .v09323_gl(bs = 30)
  expect_true(isTRUE(umbric_horizon(p)$passed))
  g <- gleysol(p)
  expect_true(isTRUE(g$passed))
  expect_equal(g$evidence$path2$horizon, "umbric")
})

test_that("Gleysols path 2: each item fails on its own", {
  skip_on_cran()
  # 2.a: the horizon must be more than 40 cm thick (0-40 here)
  thin <- .v09323_gl(top = c(0, 25, 40), bottom = c(25, 40, 90))
  expect_false(isTRUE(gleysol(thin)$evidence$path2_ok))
  # 2.a: no reducing conditions in the part of the horizon below 40 cm
  dry <- .v09323_gl(designation = c("Ah1", "Ah2", "Bg"))
  expect_false(gleysol(dry)$evidence$path2_ok)
  expect_false(isTRUE(gleysol(dry)$passed))
  # 2.b: the gleyic layer underneath is 5 cm and ends above 65 cm
  short <- .v09323_gl(top = c(0, 25, 55, 60), bottom = c(25, 55, 60, 90),
                      designation = c("Ah1", "Ahg2", "Bg", "C"),
                      hue = c("10YR", "10YR", "5GY", "10YR"), vm = c(2, 2, 5, 5),
                      cm = c(1, 1, 1, 4), vd = c(3, 3, 6, 6), oc = c(3, 2, 0.3, 0.2),
                      redox = c(0, 3, 0, 0))
  expect_false(gleysol(short)$evidence$path2_ok)
  # 2.b: 10 cm from 55 cm reaches 65 cm: the layer meets both limits
  ten <- short$clone(deep = TRUE)
  ten$horizons$bottom_cm[3] <- 65; ten$horizons$top_cm[4] <- 65
  expect_true(isTRUE(gleysol(ten)$evidence$path2_ok))
  # 2.b: the layer must be directly underneath the horizon (a gap of 10 cm)
  gap <- .v09323_gl(top = c(0, 25, 65), bottom = c(25, 55, 100))
  expect_false(gleysol(gap)$evidence$path2_ok)
  # no mollic or umbric horizon: a pale topsoil
  pale <- .v09323_gl(vm = c(5, 5, 5), vd = c(7, 7, 6))
  expect_false(gleysol(pale)$evidence$path2_ok)
})

test_that("Gleysols path 2: what is not recorded leaves it undecided", {
  skip_on_cran()
  # the reduction below 40 cm is not recorded (no designation, no mottles)
  u <- .v09323_gl(designation = c(NA, NA, "Bg"), redox = c(NA, NA, 0))
  g <- gleysol(u)
  expect_true(is.na(g$evidence$path2_ok))
  expect_true(is.na(g$passed))
  expect_true("redoximorphic_features_pct" %in% g$missing)
  # the colour of the layer underneath is not recorded
  v <- .v09323_gl(hue = c("10YR", "10YR", NA))
  v$horizons$munsell_chroma_moist[3] <- NA
  expect_true(is.na(gleysol(v)$evidence$path2_ok))
  # the colour of a layer that may belong to the horizon is not recorded (and
  # its 1% SOC does not read it as dark): the horizon's lower limit, 25 or
  # 55 cm, is undecided, and only 55 cm could meet 2.a
  w <- .v09323_gl(vm = c(2, NA, 5), vd = c(3, NA, 6), oc = c(3, 1, 0.3))
  w$horizons$munsell_chroma_moist[2] <- NA
  w$horizons$munsell_chroma_dry[2] <- NA
  w$horizons$munsell_hue_moist[2] <- NA
  w$horizons$munsell_hue_dry[2] <- NA
  expect_true(isTRUE(mollic(w)$passed))
  expect_equal(as.integer(mollic(w)$layers), 1L)
  expect_true(is.na(gleysol(w)$evidence$path2_ok))
})
