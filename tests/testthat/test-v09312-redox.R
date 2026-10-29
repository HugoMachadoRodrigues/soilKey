# v0.9.312: redox and plinthite as WRB 2022 states them: gleyic properties
# (Ch 3.2.6, p. 75), stagnic properties (3.2.14, p. 81), reducing conditions
# (3.2.10), the abrupt textural difference (3.2.1) and retic properties
# (3.2.11); the petroplinthic, pisoplinthic and plinthic horizons (3.1.27,
# 3.1.28, 3.1.30); and the Gleysols, Plinthosols, Planosols and Stagnosols
# keys (Ch 4, p. 103-108). The profiles are made up.
#
# Gleyic or stagnic, where a description does not say where the mottles sit
# (Hugo, 2026-10-29): gleyic where the matrix is reductimorphic, where the
# mottles lie on a layer with such a matrix, or where the site records
# groundwater within 150 cm that is not perched; stagnic otherwise; undecided
# with neither colour nor water evidence.

.rx_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "rx", country = "XX"), site),
                  horizons = ensure_horizon_schema(hz))
}

# A over a mottled Bg with a brown matrix over a C: the colour gives no answer
.rx_mottled <- function(site = list(), c_hue = "10YR", c_chroma = 4, c_redox = 0) {
  .rx_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
            designation = c("A", "Bg", "C"),
            munsell_hue_moist = c("10YR", "10YR", c_hue),
            munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 4, c_chroma),
            redoximorphic_features_pct = c(0, 20, c_redox),
            clay_pct = c(20, 22, 22), site = site)
}


# ---- colours and groundwater -----------------------------------------------

test_that("reductimorphic colours are those of 3.2.6 criterion 1", {
  skip_on_cran()
  rc <- soilKey:::.reductimorphic_colour
  # 1.a: N, 10Y, GY, G, BG, B or PB, any chroma
  expect_equal(rc(c("N", "10Y", "5GY", "7.5GY", "2.5G", "5BG", "10B", "5PB"), 4),
               rep(TRUE, 8))
  # 1.b: 2.5Y or 5Y with a chroma of 2 or less
  expect_equal(rc(c("2.5Y", "5Y", "2.5Y", "5Y"), c(2, 1, 3, 4)), c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(rc(c("10YR", "7.5Y", "5YR"), 1), c(FALSE, FALSE, FALSE))
  expect_true(is.na(rc(NA, 1)))
  expect_true(is.na(rc("2.5Y", NA)))
})

test_that("the site's groundwater reads within 150 cm and not perched", {
  skip_on_cran()
  gw <- soilKey:::.groundwater_within_150
  expect_true(gw(list(groundwater_kind = "observed", groundwater_depth_cm = 80)))
  expect_false(gw(list(groundwater_kind = "observed", groundwater_depth_cm = 200)))
  expect_false(gw(list(groundwater_kind = "perched", groundwater_depth_cm = 40)))
  expect_false(gw(list(groundwater_kind = "none observed")))
  expect_true(gw(list(groundwater_kind = "flooded")))
  expect_true(is.na(gw(list(groundwater_kind = "apparent"))))
  expect_true(is.na(gw(list())))
})


# ---- gleyic and stagnic properties -----------------------------------------

test_that("oximorphic over reductimorphic is gleyic (no decay test)", {
  skip_on_cran()
  # the textbook gleyic sequence: mottles over a reduced C. The test of
  # mottles that decrease with depth, which is not WRB, called it stagnic.
  p <- .rx_mottled(c_hue = "5G", c_chroma = 1)
  g <- gleyic_properties(p, max_top_cm = 100)
  expect_true(isTRUE(g$passed))
  expect_equal(g$layers, 2:3)
  expect_false(isTRUE(stagnic_properties(p)$passed))
  # continuous rock with a gleyic hue has no gleyic properties
  r <- .rx_pedon(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "R"),
                 munsell_hue_moist = c("10YR", "5G"), munsell_value_moist = c(3, 5),
                 munsell_chroma_moist = c(2, 1), redoximorphic_features_pct = c(0, 0))
  expect_false(isTRUE(gleyic_properties(r)$passed))
})

test_that("a reductimorphic matrix is gleyic with or without mottles", {
  skip_on_cran()
  p <- .rx_pedon(top_cm = c(0, 20), bottom_cm = c(20, 70), designation = c("A", "Bg"),
                 munsell_hue_moist = c("10YR", "2.5Y"), munsell_value_moist = c(3, 5),
                 munsell_chroma_moist = c(2, 1), redoximorphic_features_pct = c(0, 25))
  expect_true(isTRUE(gleyic_properties(p)$passed))
  p$horizons$munsell_chroma_moist[2] <- 3          # 2.5Y 5/3 is no 1.b colour
  expect_true(is.na(gleyic_properties(p)$passed))   # mottles, no evidence
})

test_that("mottles over a brown matrix: the groundwater decides, else undecided", {
  skip_on_cran()
  expect_true(is.na(gleyic_properties(.rx_mottled())$passed))
  expect_true(is.na(stagnic_properties(.rx_mottled())$passed))
  wet <- .rx_mottled(site = list(groundwater_kind = "observed", groundwater_depth_cm = 90))
  expect_true(isTRUE(gleyic_properties(wet)$passed))
  expect_false(isTRUE(stagnic_properties(wet)$passed))
  for (s in list(list(groundwater_kind = "perched"), list(groundwater_kind = "none observed"),
                 list(groundwater_kind = "observed", groundwater_depth_cm = 300))) {
    q <- .rx_mottled(site = s)
    expect_false(isTRUE(gleyic_properties(q)$passed))
    expect_true(isTRUE(stagnic_properties(q)$passed))
  }
})

test_that("a claric layer over the mottles is the stagnic sequence", {
  skip_on_cran()
  # 3.2.14 criterion 5: an Fe-poor claric layer over a mottled one
  p <- .rx_pedon(top_cm = c(0, 15, 40, 100), bottom_cm = c(15, 40, 100, 150),
                 designation = c("A", "Eg", "Btg", "C"),
                 munsell_hue_moist = "10YR",
                 munsell_value_moist = c(3, 6, 5, 5), munsell_chroma_moist = c(2, 2, 4, 4),
                 munsell_value_dry = c(4, 7, 6, 6), munsell_chroma_dry = c(2, 2, 4, 4),
                 redoximorphic_features_pct = c(0, 0, 20, 0), clay_pct = c(15, 15, 25, 25))
  s <- stagnic_properties(p)
  expect_true(isTRUE(s$passed))
  expect_equal(s$layers, 2:3)
  expect_false(isTRUE(gleyic_properties(p, max_top_cm = 100)$passed))
  # with nothing described under the mottles, criterion 3 cannot be ruled out
  q <- p$clone(deep = TRUE); q$horizons <- q$horizons[1:3, ]
  expect_true(is.na(gleyic_properties(q, max_top_cm = 100)$passed))
})

test_that("reducing conditions read every suffix of the designation", {
  skip_on_cran()
  h <- data.table::data.table(
    top_cm = seq(0, 80, 10), bottom_cm = seq(10, 90, 10),
    designation = c("Apg", "Btg", "2Bg", "BCg", "Cr", "Bl", "B21pl", "Bj", "Bt"),
    redoximorphic_features_pct = 0, sulfidic_s_pct = NA_real_)
  r <- soilKey:::test_reducing_conditions(h)
  expect_equal(r$layers, 1:6)
})


# ---- Gleysols ----------------------------------------------------------------

test_that("the Gleysols layer of 25 cm may be made of adjoining sublayers", {
  skip_on_cran()
  p <- .rx_pedon(top_cm = c(0, 20, 35, 50), bottom_cm = c(20, 35, 50, 90),
                 designation = c("A", "Bg1", "Bg2", "BC"),
                 munsell_hue_moist = c("10YR", "5GY", "5GY", "10YR"),
                 munsell_value_moist = c(3, 5, 5, 5), munsell_chroma_moist = c(2, 1, 1, 4),
                 redoximorphic_features_pct = c(0, 0, 0, 0))
  r <- gleysol(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  p$horizons$munsell_hue_moist[3] <- "10YR"      # Bg1 alone is 15 cm
  expect_false(isTRUE(gleysol(p)$passed))
  q <- .rx_mottled()                              # mottles, no evidence
  expect_true(is.na(gleysol(q)$passed))
})


test_that("the Gleyic qualifier asks 25 cm of gleyic layers with reducing conditions", {
  skip_on_cran()
  # Ch 5, p. 135: a dark 2.5Y 2/2 topsoil has the colour of criterion 1.b
  # but no sign of reduction, so no Gleyic; a 2.5Y 5/1 Bg of 30 cm has both
  p <- .rx_pedon(top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 80),
                 designation = c("A", "Bg", "Cg"),
                 munsell_hue_moist = "2.5Y", munsell_value_moist = c(2, 5, 5),
                 munsell_chroma_moist = c(2, 1, 1), redoximorphic_features_pct = 0)
  expect_true(isTRUE(gleyic_properties(p)$passed))
  g <- qual_gleyic(p)
  expect_true(isTRUE(g$passed))
  expect_equal(g$layers, 2:3)
  q <- p$clone(deep = TRUE); q$horizons$designation <- c("A", "B", "C")
  expect_false(isTRUE(qual_gleyic(q)$passed))
})


# ---- Stagnosols ----------------------------------------------------------------

test_that("Stagnosols: stagnic area of one third of 0-60 cm and reducing conditions", {
  skip_on_cran()
  # a grey stagnic Bg of 30 cm: the whole layer is redox features
  p <- .rx_pedon(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 100),
                 designation = c("Ah", "Bg", "BC"),
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5),
                 munsell_chroma_moist = c(2, 1, 4), redoximorphic_features_pct = c(0, 25, 0),
                 site = list(groundwater_kind = "perched"))
  r <- stagnosol(p)
  expect_true(isTRUE(r$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Stagnosols")
  # a brown matrix: 25% of 30 cm is an eighth of 60 cm
  q <- p$clone(deep = TRUE); q$horizons$munsell_chroma_moist[2] <- 4
  expect_false(isTRUE(stagnosol(q)$passed))
  # no water record, no claric layer: undecided
  u <- p$clone(deep = TRUE); u$site$groundwater_kind <- NULL
  expect_true(is.na(stagnosol(u)$passed))
})

test_that("Stagnosols: a claric layer over a dense layer, with and without reduction", {
  skip_on_cran()
  # 3.2.14 criterion 4: claric colours above a layer of 1.5 kg/dm3 or more
  p <- .rx_pedon(top_cm = c(0, 10, 35), bottom_cm = c(10, 35, 100),
                 designation = c("A", "Eg", "Bt"),
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 5),
                 munsell_chroma_moist = c(2, 2, 4), munsell_value_dry = c(4, 7, 6),
                 munsell_chroma_dry = c(2, 2, 4), redoximorphic_features_pct = 0,
                 clay_pct = c(15, 15, 25), bulk_density_g_cm3 = c(1.3, 1.4, 1.6))
  expect_true(isTRUE(stagnosol(p)$passed))           # 25 of 60 cm, reducing (g)
  q <- p$clone(deep = TRUE); q$horizons$designation[2] <- "E"
  expect_false(isTRUE(stagnosol(q)$passed))          # no reducing conditions
  expect_false(isTRUE(qual_stagnic(q)$passed))       # nor the Stagnic qualifier
  d <- p$clone(deep = TRUE); d$horizons$bulk_density_g_cm3[3] <- 1.4
  expect_false(isTRUE(stagnic_properties(d)$passed))
})


# ---- the abrupt textural difference and the Planosols --------------------------

test_that("the abrupt textural difference reads the transition (criterion 5)", {
  skip_on_cran()
  mk <- function(b) .rx_pedon(top_cm = c(0, 30), bottom_cm = c(30, 80),
                              designation = c("E", "Bt"), clay_pct = c(10, 30),
                              boundary_distinctness = c(b, NA))
  expect_true(isTRUE(abrupt_textural_difference(mk("abrupt"))$passed))
  expect_true(isTRUE(abrupt_textural_difference(mk("very abrupt"))$passed))
  expect_false(isTRUE(abrupt_textural_difference(mk("clear"))$passed))
  expect_true(is.na(abrupt_textural_difference(mk(NA))$passed))
  # criterion 2 counts from the mineral soil surface
  o <- .rx_pedon(top_cm = c(0, 10, 15), bottom_cm = c(10, 15, 60),
                 designation = c("Oa", "E", "Bt"), oc_pct = c(30, 1, 0.5),
                 clay_pct = c(NA, 10, 30), boundary_distinctness = c("abrupt", "abrupt", NA))
  expect_false(isTRUE(abrupt_textural_difference(o)$passed))
})

.rx_planosol <- function(atd_top = 30, e_redox = 8, site = list(groundwater_kind = "perched")) {
  .rx_pedon(top_cm = c(0, 15, atd_top), bottom_cm = c(15, atd_top, atd_top + 50),
            designation = c("A", "E", "Btg"),
            boundary_distinctness = c("clear", "abrupt", "gradual"),
            munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 4),
            munsell_chroma_moist = c(3, 2, 2), munsell_value_dry = c(5, 7, 5),
            munsell_chroma_dry = c(3, 2, 2), redoximorphic_features_pct = c(0, e_redox, 20),
            clay_pct = c(15, 12, 35), site = site)
}

test_that("Planosols: an abrupt textural difference within 75 cm with stagnic properties", {
  skip_on_cran()
  p <- .rx_planosol()
  r <- planosol(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_false(isTRUE(planosol(.rx_planosol(atd_top = 90))$passed))     # deeper than 75 cm
  # the clay jump alone, with no redox features: no Planosol (the v0.2
  # planic_features fallback is gone)
  dry <- .rx_planosol(e_redox = 0, site = list())
  dry$horizons$redoximorphic_features_pct <- 0
  dry$horizons$designation <- c("A", "E", "Bt")
  dry$horizons$munsell_value_moist <- c(4, 4, 4)
  dry$horizons$munsell_chroma_moist <- c(3, 4, 4)
  expect_false(isTRUE(planosol(dry)$passed))
  expect_true(isTRUE(planic_features(dry)$passed))
})


# ---- retic properties ----------------------------------------------------------

test_that("retic properties read E/B and B/E and an irregular E boundary", {
  skip_on_cran()
  p <- make_retisol_canonical()
  p$horizons$designation <- c("A", "E", "Bt", "BC")
  expect_false(isTRUE(retic_properties(p)$passed))
  a <- p$clone(deep = TRUE); a$horizons$designation[2] <- "2E/Bt"
  expect_true(isTRUE(retic_properties(a)$passed))
  b <- p$clone(deep = TRUE); b$horizons$designation[3] <- "Bt/E"
  expect_true(isTRUE(retic_properties(b)$passed))
  i <- p$clone(deep = TRUE); i$horizons$boundary_topography <- c("smooth", "irregular", "wavy", NA)
  expect_true(isTRUE(retic_properties(i)$passed))
  i$horizons$boundary_topography[2] <- "smooth"
  expect_false(isTRUE(retic_properties(i)$passed))
})


# ---- plinthite ---------------------------------------------------------------

.rx_plinth <- function(top = 30, plinthite = c(0, 20, 20, 0), ...) {
  .rx_pedon(top_cm = c(0, top, top + 10, top + 20), bottom_cm = c(top, top + 10, top + 20, top + 60),
            designation = c("A", "Btv1", "Btv2", "C"), plinthite_pct = plinthite,
            ferric_nodules_pct = 0, ...)
}

test_that("plinthic: 15% plinthite over 15 cm of adjoining sublayers", {
  skip_on_cran()
  r <- plinthic(.rx_plinth())
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_false(isTRUE(plinthic(.rx_plinth(plinthite = c(0, 20, 10, 0)))$passed))
  # criterion 3: Feox / Fedith below 0.1
  hi <- .rx_plinth(fe_dcb_pct = 5, fe_ox_pct = 0.8)
  expect_false(isTRUE(plinthic(hi)$passed))
  # not recorded in one layer, none in the others: undecided
  h <- data.table::data.table(top_cm = c(0, 30), bottom_cm = c(30, 60), plinthite_pct = c(0, NA))
  expect_true(is.na(soilKey:::test_plinthite_concentration(h)$passed))
})

test_that("Plinthosols: a plinthic, pisoplinthic or petroplinthic horizon within 100 cm", {
  skip_on_cran()
  expect_true(isTRUE(plinthosol(.rx_plinth(top = 80))$passed))
  expect_false(isTRUE(plinthosol(.rx_plinth(top = 110))$passed))
  # pisoplinthic: 40% Fe nodules over 15 cm
  piso <- .rx_plinth(plinthite = 0)
  piso$horizons$ferric_nodules_pct <- c(0, 60, 45, 0)
  expect_true(isTRUE(pisoplinthic(piso)$passed))
  expect_true(isTRUE(plinthosol(piso)$passed))
  piso$horizons$ferric_nodules_pct <- c(0, 30, 30, 0)
  expect_false(isTRUE(pisoplinthic(piso)$passed))
  piso$horizons$ferric_nodules_pct <- NA_real_
  expect_true(is.na(pisoplinthic(piso)$passed))
  # petroplinthic: an iron pan at least strongly cemented, 10 cm
  pet <- .rx_plinth(plinthite = 0)
  pet$horizons$pan_type <- c(NA, "ferricrete", NA, NA)
  pet$horizons$cementation_class <- c(NA, "indurated", NA, NA)
  expect_true(isTRUE(petroplinthic(pet)$passed))
  expect_true(isTRUE(plinthosol(pet)$passed))
  pet$horizons$cementation_class[2] <- "moderately"
  expect_false(isTRUE(petroplinthic(pet)$passed))
  pet$horizons$cementation_class[2] <- NA
  expect_true(is.na(petroplinthic(pet)$passed))
})


# ---- the ISIS groundwater ------------------------------------------------------

test_that("the ISIS reader reads the site's groundwater", {
  skip_on_cran()
  code <- function(item, description = NULL) list(item = item, description = description)
  prof <- function(kind, depth = NULL, top = NULL) list(
    profileCode = "XX002", countryCode = "XX", profileId = 2L,
    geog = list(latitude = 0, longitude = 0),
    groundwaterKind = if (!is.null(kind)) code(kind), depthGroundwater = depth,
    groundwaterTop = top,
    layers = list(list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 30L, layerName = "A",
                       clay = list("20"))))
  s <- isis_profile_to_pedon(prof("W", list("120")), accessed = as.Date("2026-10-29"))$site
  expect_equal(s$groundwater_kind, "observed"); expect_equal(s$groundwater_depth_cm, 120)
  s <- isis_profile_to_pedon(prof("W", list("0"), list("40")))$site    # the level in groundwaterTop
  expect_equal(s$groundwater_depth_cm, 40)
  s <- isis_profile_to_pedon(prof("N", list("0")))$site
  expect_equal(s$groundwater_kind, "none observed"); expect_null(s$groundwater_depth_cm)
  s <- isis_profile_to_pedon(prof("P", list("60")))$site
  expect_equal(s$groundwater_kind, "perched")
  s <- isis_profile_to_pedon(prof("F", list("0")))$site
  expect_equal(s$groundwater_kind, "flooded"); expect_equal(s$groundwater_depth_cm, 0)
  s <- isis_profile_to_pedon(prof(NULL))$site
  expect_null(s$groundwater_kind)
  expect_match(soilKey:::.isis_profile_fields(), "depthGroundwater groundwaterTop groundwaterKind")
})
