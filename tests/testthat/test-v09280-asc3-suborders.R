# v0.9.280: suborders of the Australian Soil Classification for the colour
# orders (Kurosols, Sodosols, Chromosols, Ferrosols, Dermosols, Kandosols,
# Vertosols): the colour classes, their windows, the dominant class and the
# Aquic Vertosols.

.mk_ascs <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("moist colours fall in the ASC colour classes", {
  cc <- soilKey:::.asc_colour_class1
  expect_identical(cc("2.5YR", 4, 6), "Red")
  expect_identical(cc("5YR", 5, 3), "Red")                 # 5YR is 'or redder'
  expect_identical(cc("7.5YR", 4, 4), "Brown")
  expect_identical(cc("10YR", 6, 6), "Yellow")
  expect_identical(cc("10YR", 7, 3), "Grey")                # yellower, value >= 6, chroma 3
  expect_identical(cc("2.5YR", 5, 2), "Grey")
  expect_identical(cc("10YR", 3, 1), "Black")
  expect_identical(cc("N", 2.5, NA), "Black")
  expect_identical(cc("5GY", 6, 1), "Grey")
  expect_identical(cc("10R", 3, 4), "Red")                  # Red has no value limit
  # between chips, sides in different classes: unknown
  expect_true(is.na(cc("10YR", 3.5, 1)))                    # Black or Grey
  expect_true(is.na(cc("10YR", 5, 2.5)))                    # Grey or Brown
  expect_true(is.na(cc("6.25YR", 4, 4)))                    # 5YR (Red) or 7.5YR (Brown)
  expect_identical(cc("6YR", 4, 4), "Red")                  # nearest chip 5YR
  expect_true(is.na(cc(NA, 4, 4)))
})

test_that("each colour order keys to its suborder, with nothing undecided", {
  skip_on_cran()
  want <- c(kurosols = "Red Kurosol", sodosols = "Red Sodosol", chromosols = "Red Chromosol",
            ferrosols = "Red Ferrosol", dermosols = "Brown Dermosol", kandosols = "Red Kandosol",
            vertosols = "Black Vertosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    # since 0.9.283 the name goes on to the great group: the suborder is the
    # second level
    expect_identical(r$levels$name[r$levels$level == "suborders"], want[[o]], info = o)
    expect_length(r$provisional_because, 0L)
  }
  # every order now has suborders (0.9.282)
  expect_identical(classify_asc3(make_asc3_example("tenosols"), on_missing = "silent")$name, "Brown Tenosol")
})

test_that("the dominant class: the major part, else the largest share, a tie undecided", {
  skip_on_cran()
  mk <- function(hues, tops, bots) .mk_ascs(list(
    designation = c("A1", rep("B2", length(hues))), top_cm = c(0, tops), bottom_cm = c(tops[1], bots),
    field_texture = "LC", structure_grade = c("weak", rep("moderate", length(hues))),
    munsell_hue_moist = c("10YR", hues), munsell_value_moist = c(3, rep(4, length(hues))),
    munsell_chroma_moist = c(2, rep(4, length(hues)))))
  cs <- function(p, col) soilKey:::colour_suborder_asc(p, col, "b2_50")$passed
  p <- mk(c("2.5YR", "7.5YR"), c(20, 50), c(50, 90))           # window 20-70: Red 30, Brown 20
  expect_true(cs(p, "Red")); expect_false(cs(p, "Brown"))
  p <- mk(c("2.5YR", "7.5YR", "10YR"), c(20, 40, 55), c(40, 55, 100))   # Red 20, Brown 15, Yellow? no: 10YR 4/4 Brown
  expect_true(cs(p, "Brown"))                                  # Brown 15 + 15 = 30 of 50
  p <- mk(c("2.5YR", "7.5YR"), c(20, 45), c(45, 90))           # Red 25, Brown 25: a tie
  expect_true(is.na(cs(p, "Red"))); expect_true(is.na(cs(p, "Brown")))
  expect_false(cs(p, "Yellow"))
  p$horizons$munsell_value_moist[3] <- 6; p$horizons$munsell_chroma_moist[3] <- 6   # 10YR... keep 7.5YR 6/6: Yellow
  p$horizons$munsell_value_moist[2] <- NA                      # Red row unknown: Yellow 25 of 50
  expect_true(is.na(cs(p, "Yellow")))
})

test_that("the windows: upper 0.2 m of the B2t, upper 0.5 m of the B2, of the solum", {
  skip_on_cran()
  # a Chromosol whose B21t is red over 20 cm and whose B22t is yellow: Red
  p <- make_asc3_example("chromosols")
  p$horizons$munsell_hue_moist <- c("10YR", "10YR", "2.5YR", "10YR", "10YR")
  p$horizons$munsell_value_moist <- c(3, 6, 4, 6, 6); p$horizons$munsell_chroma_moist <- c(2, 3, 6, 6, 6)
  r <- soilKey:::colour_suborder_asc(p, "Red", "upper_b2t")
  expect_true(r$passed); expect_identical(r$evidence$window_cm, c(30, 50))
  # the same colours read over the upper 0.5 m of the B2: B21t 25 cm red, B22t 25 cm yellow
  expect_true(is.na(soilKey:::colour_suborder_asc(p, "Red", "b2_50")$passed))
  # a Vertosol over the upper 0.5 m of the solum
  p <- make_asc3_example("vertosols")
  r <- soilKey:::colour_suborder_asc(p, "Black", "solum_50")
  expect_true(r$passed); expect_identical(r$evidence$window_cm, c(0, 50))
})

test_that("Aquic Vertosols read saturation days, then the drainage class", {
  skip_on_cran()
  aq <- function(p) soilKey:::aquic_vertosol_asc(p)$passed
  p <- make_asc3_example("vertosols")
  expect_false(aq(p))                                          # well drained
  p$site$drainage_class <- "imperfectly drained"; expect_false(aq(p))
  p$site$drainage_class <- "poorly drained"; expect_true(is.na(aq(p)))
  p$site$drainage_class <- "very poorly drained"; expect_true(aq(p))
  expect_identical(classify_asc3(p, on_missing = "silent")$name, "Aquic Vertosol")
  p$horizons$water_saturation_days <- c(20, 30, 40, 50)          # the days decide
  expect_false(aq(p))
  p$horizons$water_saturation_days <- c(20, 120, NA, NA)
  expect_true(aq(p))
})

test_that("the NatSoil benchmark reports the suborders", {
  skip_on_cran()
  a <- make_asc3_example("chromosols"); a$site$id <- "a"
  a$site$reference_asc_order <- "CH"; a$site$reference_asc_suborder <- "AA"
  b <- make_asc3_example("dermosols"); b$site$id <- "b"
  b$site$reference_asc_order <- "DE"; b$site$reference_asc_suborder <- "AA"      # surveyed Red, keys Brown
  bm <- benchmark_natsoil(list(a = a, b = b), verbose = FALSE)
  expect_identical(bm$suborders$n, 2L)
  expect_equal(bm$suborders$accuracy, 50)
  expect_identical(bm$results$predicted_suborder, c("AA", "AB"))
})

# ---- review round -----------------------------------------------------------------

test_that("a gap in the record is a part of unknown colour, not the end of the window", {
  skip_on_cran()
  p <- .mk_ascs(list(designation = c("A1", "B21", "B22"), top_cm = c(0, 15, 45), bottom_cm = c(15, 35, 100),
                     field_texture = "LC", structure_grade = c("weak", "moderate", "moderate"),
                     munsell_hue_moist = c("10YR", "2.5YR", "7.5YR"), munsell_value_moist = c(3, 4, 4),
                     munsell_chroma_moist = c(2, 6, 4)))
  r <- soilKey:::colour_suborder_asc(p, "Red", "b2_50")
  expect_identical(r$evidence$window_cm, c(15, 65))           # through the 35-45 gap
  expect_true(is.na(r$passed))                                 # Red 20, Brown 20, 10 unknown
  # Red 25, a 2 cm gap, Brown 23: the gap may make a tie
  p$horizons$bottom_cm[2] <- 40; p$horizons$top_cm[3] <- 42
  expect_true(is.na(soilKey:::colour_suborder_asc(p, "Red", "b2_50")$passed))
  # Red 22, Brown 10, 8 cm unknown, Yellow 10: Red wins whatever the gap is
  p <- .mk_ascs(list(designation = c("A1", "B21", "B22", "B23"), top_cm = c(0, 15, 37, 55),
                     bottom_cm = c(15, 37, 47, 100), field_texture = "LC",
                     structure_grade = c("weak", "moderate", "moderate", "moderate"),
                     munsell_hue_moist = c("10YR", "2.5YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 4, 4, 6),
                     munsell_chroma_moist = c(2, 6, 4, 6)))
  expect_true(soilKey:::colour_suborder_asc(p, "Red", "b2_50")$passed)
  expect_false(soilKey:::colour_suborder_asc(p, "Brown", "b2_50")$passed)
})

test_that("chips, hues and Vertosols without designations", {
  cc <- soilKey:::.asc_colour_class1
  expect_identical(cc("10YR", 3.2, 1), "Black")               # nearest chip 3
  expect_identical(cc(NA, 5, 1), "Grey")                      # Grey holds for all hues
  expect_identical(cc("N", 5, 3), "Grey")                     # a neutral hue has no chroma
  skip_on_cran()
  p <- make_asc3_example("vertosols"); p$horizons$designation <- NA_character_
  r <- soilKey:::colour_suborder_asc(p, "Black", "solum_50")
  expect_true(r$passed)
})

test_that("second review: overlaps, open bottoms and other horizons in the window", {
  skip_on_cran()
  ku <- function(p) soilKey:::kurosol_asc(p)$passed
  base <- function(des, tops, bots, ph, hue = "7.5YR") .mk_ascs(list(
    designation = c("A1", "A2e", des), top_cm = c(0, 12, tops), bottom_cm = c(12, 30, bots),
    field_texture = c("SL", "SL", rep("LMC", length(des))), clay_pct = c(14, 12, rep(42, length(des))),
    boundary_distinctness = c("clear", "abrupt", rep("gradual", length(des))), ph_h2o = c(6, 6, ph),
    munsell_hue_moist = c("10YR", "10YR", rep(hue, length(des))), munsell_value_moist = 4, munsell_chroma_moist = 4))
  # overlapping B2t rows that disagree: no depth counted twice, undecided
  p <- base(c("B21t", "B22t"), c(30, 35), c(50, 60), c(6.5, 5.0))
  expect_true(is.na(ku(p)))
  # a B2t row with no bottom leaves the window open to 0.2 m
  p <- base(c("B21t", "B22t"), c(30, 35), c(35, NA), c(5.0, 6.5))
  expect_true(is.na(ku(p)))
  # a BC inside the 0.2 m ends the B2t window
  p <- base(c("B21t", "BC", "B22t"), c(30, 42, 60), c(42, 60, 90), c(5.0, 6.5, 6.5))
  expect_true(ku(p))
  # a Vertosol with no designations and thin rows keeps its colour
  v <- make_asc3_example("vertosols"); v$horizons$designation <- NA_character_
  v <- .mk_ascs(list(top_cm = seq(0, 95, 5), bottom_cm = seq(5, 100, 5), field_texture = "HC",
                     slickensides = "common", cracks_width_cm = 1, munsell_hue_moist = "10YR",
                     munsell_value_moist = 2, munsell_chroma_moist = 1),
                site = list(designation_system = NULL))
  expect_true(soilKey:::colour_suborder_asc(v, "Black", "solum_50")$passed)
})

test_that("organic materials keep their own saturation limit (30 days, as Soil Taxonomy)", {
  skip_on_cran()
  sat <- soilKey:::.asc_organic_saturation; long <- soilKey:::.asc_long_saturation
  p <- make_asc3_example("organosols")
  for (d in c("very poorly drained", "poorly drained", "imperfectly drained", "moderately well drained")) {
    p$site$drainage_class <- d
    expect_identical(sat(p), switch(d, "very poorly drained" = TRUE, "moderately well drained" = FALSE, NA), info = d)
  }
  p$site$drainage_class <- "imperfectly drained"
  expect_false(long(p))                                    # weeks: not 2-3 months
  expect_true(is.na(sat(p)))                               # weeks: maybe 30 days
  # 16 and 15 % organic C, imperfectly drained: organic materials undecided
  p$horizons$oc_pct <- c(16, 15, 1.5); p$horizons$clay_pct <- c(10, 10, 38)
  expect_true(is.na(soilKey:::organosol_asc(p)$passed))
})
