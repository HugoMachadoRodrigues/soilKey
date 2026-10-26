# v0.9.289: subgroups of the Australian Soil Classification for the
# Hydrosols, Arenosols, Rudosols and Tenosols.

sg_s4 <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv$code[lv$level == "subgroups"] }
mk_s4 <- function(df, site = list()) {
  PedonRecord$new(site = utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                                drainage_class = "well drained"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}
ex_s4 <- function(order, h = list()) {
  p <- make_asc3_example(order); d <- as.data.frame(p$horizons)
  for (k in names(h)) d[[k]] <- h[[k]]
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(d)))
}

test_that("the examples, and the lists", {
  skip_on_cran()
  expect_identical(classify_asc3(make_asc3_example("arenosols"), on_missing = "silent")$name, "Basic, Tenic, Yellow Arenosol")
  expect_identical(classify_asc3(make_asc3_example("tenosols"), on_missing = "silent")$name, "Basic, Regolithic, Brown Tenosol")
  sg <- load_rules("asc3")$subgroups
  n <- function(path) length(soilKey:::.level_list(sg, path, path[length(path)]))
  expect_identical(n(c("HY", "CW", "IY")), 3L); expect_identical(n(c("HY", "CW", "EV")), 0L)
  expect_identical(n(c("HY", "EG", "XX")), 3L); expect_identical(n(c("HY", "ED", "BG")), 40L)
  expect_identical(n(c("HY", "DT", "CX")), 33L); expect_identical(n(c("HY", "ED", "EQ")), 34L)
  expect_identical(n(c("HY", "ED", "GT")), 0L); expect_identical(n(c("HY", "IU", "XX")), 0L)
  expect_identical(n(c("RE", "AC", "XX")), 13L); expect_identical(n(c("RE", "AC", "CQ")), 10L); expect_identical(n(c("RE", "JT", "XX")), 0L)
  expect_identical(n(c("RU", "CY", "XX")), 3L); expect_identical(n(c("RU", "EL", "XX")), 0L)
  expect_identical(length(sg[["RU-GV"]]), 3L)
  te <- vapply(c("BF", "BE", "IL", "IM", "AW", "CY", "AT", "AA"), function(s) n(c("TE", s, "XX")), 0L)
  expect_identical(unname(te), c(4L, 9L, 6L, 3L, 12L, 7L, 13L, 13L))
})

test_that("a suborder with no great groups takes its subgroups directly", {
  skip_on_cran()
  lutic <- function(ph) mk_s4(data.frame(designation = c("A1", "C"), top_cm = c(0, 10), bottom_cm = c(10, 80),
                                         field_texture = c("L", "LC"), clay_pct = c(20, 38), ph_h2o = ph,
                                         effervescence_class = "none", structure_grade = c("weak", "structureless"),
                                         structure_type = c("granular", "massive"), strength_moist = "firm",
                                         ec_1to5_dS_m = 0.1, stringsAsFactors = FALSE))
  r <- classify_asc3(lutic(c(6.5, 6.8)), on_missing = "silent")
  expect_identical(r$name, "Basic, Lutic Rudosol")
  expect_identical(r$levels$level, c("orders", "suborders", "subgroups"))
  expect_identical(sg_s4(lutic(c(5, 5.2))), "AI")
})

test_that("Hydrosols: the Redoxic list, the Intertidal peat, the Salic subgroups", {
  skip_on_cran()
  # the example with a sulfide test, bases and colours: Eutrophic
  p <- ex_s4("hydrosols", list(sulfidic_s_pct = 0.002, ca_cmol = c(8, 6, 6, 5), mg_cmol = 3, k_cmol = 0.4, na_cmol = 0.3,
                                cec_cmol = 15, munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5, 5),
                                munsell_chroma_moist = 2, slickensides = "absent"))
  expect_identical(sg_s4(p), "AH")
  # Natric on the upper 0.2 m of the B2
  p2 <- ex_s4("hydrosols", list(na_cmol = c(0.3, 3, 3, 3), cec_cmol = 15))
  expect_true(soilKey:::natric_asc(p2, "upper_b2")$passed)
  # sulfidic materials near the surface
  d <- data.frame(designation = c("A1", "Cg1", "Cg2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                  sulfidic_s_pct = c(0.002, 0.05, 0.05), stringsAsFactors = FALSE)
  expect_true(soilKey:::episulfidic_asc(mk_s4(d))$passed)
  d$sulfidic_s_pct <- c(0.002, 0.002, 0.05)
  expect_false(soilKey:::episulfidic_asc(mk_s4(d))$passed)
  d$sulfidic_s_pct <- c(NA, 0.002, 0.05)
  expect_true(is.na(soilKey:::episulfidic_asc(mk_s4(d))$passed))
  # fusic surface layers
  d <- data.frame(designation = c("A1", "B2"), top_cm = c(0, 15), bottom_cm = c(15, 60), layer_origin = c("burnt peat", NA),
                  stringsAsFactors = FALSE)
  expect_true(soilKey:::fusic_surface_asc(mk_s4(d))$passed)
  d$layer_origin <- NA_character_
  expect_false(soilKey:::fusic_surface_asc(mk_s4(d))$passed)
})

test_that("acidity and carbonate over a part of the profile", {
  skip_on_cran()
  pp <- function(p, ...) soilKey:::part_prop_asc(p, ...)$passed
  # the Arenosol example: not strongly acid, not calcareous in the upper 1.0 m
  a <- make_asc3_example("arenosols")
  expect_false(pp(a, "acid", "upper100", "major")); expect_true(pp(a, "calcareous", "upper100", "none"))
  a2 <- ex_s4("arenosols", list(ph_h2o = 5.0))
  expect_true(pp(a2, "acid", "upper100", "major")); expect_identical(sg_s4(a2), "AI")
  # an A2: none, the condition fails
  t <- make_asc3_example("tenosols")
  expect_false(pp(t, "acid", "a2", "major")); expect_false(pp(t, "not_acid", "a2", "major"))
  # Chernic Tenosols with no B: the B condition fails
  d <- data.frame(designation = c("A1", "C"), top_cm = c(0, 30), bottom_cm = c(30, 90), ph_h2o = 5, stringsAsFactors = FALSE)
  expect_false(pp(mk_s4(d), "acid", "b", "major"))
  # the solum and the soil
  d <- data.frame(designation = c("A1", "B2", "C"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 90), ph_h2o = c(5, 5, 7),
                  effervescence_class = c("none", "none", "strong"), stringsAsFactors = FALSE)
  expect_true(pp(mk_s4(d), "acid", "solum", "major")); expect_true(pp(mk_s4(d), "calcareous", "solum", "none"))
  expect_true(pp(mk_s4(d), "calcareous", "soil", "some"))
})

test_that("the sub horizons: every requirement but the thickness", {
  skip_on_cran()
  d <- data.frame(designation = c("A1", "R"), top_cm = c(0, 10), bottom_cm = c(10, 30), field_texture = c("SL", NA),
                  clay_pct = c(14, NA), oc_pct = c(3, NA), ph_h2o = c(5, NA), munsell_hue_moist = "10YR",
                  munsell_value_moist = c(2, NA), munsell_chroma_moist = c(1, NA), munsell_value_dry = c(4, NA),
                  structure_grade = c("moderate", NA), structure_type = c("granular", NA), stringsAsFactors = FALSE)
  p <- mk_s4(d)
  expect_false(soilKey:::chernic_sg_asc(p, "melacic")$passed)
  expect_true(soilKey:::chernic_sg_asc(p, "submelacic")$passed)
  expect_false(soilKey:::chernic_sg_asc(p, "submelanic")$passed)
  expect_false(soilKey:::chernic_sg_asc(p, "subhumose")$passed)
  p$horizons$oc_pct[1] <- 7
  expect_true(soilKey:::chernic_sg_asc(p, "subhumose")$passed)
})
