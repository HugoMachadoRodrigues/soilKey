# v0.9.326: the 27 groups of the Comprehensive Soil Classification System of
# Japan, First Approximation (CSCJ 2011, chapter VI). Made-up profiles; no
# Japanese data.

.jp <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "JP", designation_system = "scsj", drainage_class = "well drained",
               land_use = "forest")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.grp <- function(p) {
  r <- classify_cscj2011(p, on_missing = "silent")
  c(code = r$levels$code[2], prov = paste(r$provisional_because, collapse = ","))
}

test_that("every example keys to a group, decided, named in English and Japanese", {
  skip_on_cran()
  want <- c(man_made = "A1", organic = "B1", podzol = "C1", andosol = "D6", dark_red = "E1", lowland = "F4",
            red_yellow = "G1", stagnic = "H2", brown_forest = "I1", regosol = "J2")
  for (g in names(want)) {
    r <- classify_cscj2011(make_cscj2011_example(g), on_missing = "silent")
    expect_identical(r$levels$code[2], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
  }
  r <- classify_cscj2011(make_cscj2011_example("dark_red"), on_missing = "silent")
  expect_identical(r$levels$name[2], "Calcaric Dark Red soils")
  expect_identical(r$levels$name_native[2], "\u77f3\u7070\u6027\u6697\u8d64\u8272\u571f")
  expect_identical(length(unlist(lapply(load_rules("cscj2011")$groups, function(g) vapply(g, `[[`, "", "code")))), 27L)
})

test_that("Fulvic Andosols: the brown polyhumic kuroboku surface horizon, 30 cm in 40 cm, mean 6 %", {
  skip_on_cran()
  kb <- function(...) utils::modifyList(list(designation = c("A1", "A2", "Bw", "C"), top_cm = c(0, 20, 40, 80),
              bottom_cm = c(20, 40, 80, 110), oc_pct = c(9, 7, 2, 0.8), phosphate_absorption_coef = c(2200, 2100, 1900, 1400),
              munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 5), munsell_chroma_moist = c(3, 3, 4, 6),
              structure_grade = c("moderate", "moderate", "weak", "structureless"), field_texture = "loam", clay_pct = 20,
              ph_h2o = 5.5, layer_origin = "volcanic ash", redoximorphic_features_pct = 0, si_ox_pct = 1.5,
              al_ox_pct = 3, al_pyro_pct = 0.3, exchange_acidity_y1 = 1), list(...))
  expect_identical(.grp(.jp(kb()))[["code"]], "D4")
  # black (value under 3): not brown
  expect_identical(.grp(.jp(kb(munsell_value_moist = c(2, 2, 4, 5))))[["code"]], "D6")
  # every layer 4 % or more but the mean under 6 %
  expect_identical(.grp(.jp(kb(oc_pct = c(5, 5, 2, 0.8))))[["code"]], "D6")
  # a thin layer that fails between two that meet: the meeting layers still
  # add up to 30 cm within 40 cm, and the failing one is not in the mean
  p <- .jp(kb(designation = c("A1", "A2", "A3", "C"), top_cm = c(0, 18, 22, 40), bottom_cm = c(18, 22, 40, 110),
              oc_pct = c(9, 3, 8, 0.8)))
  expect_identical(.grp(p)[["code"]], "D4")
  # carbon not measured in one counted layer: undecided
  p <- .jp(kb(oc_pct = c(9, NA, 2, 0.8)))
  expect_true(is.na(fulvic_andosols_cscj(p)$passed))
})

test_that("Dark Red groups: Calcaric, Dystric by pH alone, Eutric; liming leaves Dystric undecided", {
  skip_on_cran()
  nonlime <- function() {
    p <- make_cscj2011_example("dark_red")
    p$horizons$layer_origin <- "weathered basalt"; p$horizons$rock_origin <- c(NA, NA, NA, NA, "basalt")
    p$site$parent_material <- "basalt"; p$horizons$caco3_pct <- 0
    p
  }
  expect_identical(.grp(make_cscj2011_example("dark_red"))[["code"]], "E1")
  p <- nonlime()
  expect_identical(.grp(p)[["code"]], "E3")
  p$horizons$ph_h2o <- c(5.2, 5.0, 5.3, 6.0, NA)
  expect_identical(.grp(p)[["code"]], "E2")
  q <- nonlime(); q$site$acid_neutralising_amendment <- TRUE
  g <- .grp(q)
  expect_identical(g[["code"]], "E3")
  expect_match(g[["prov"]], "E2")
})

test_that("Brown Lowland soils: the CSCJ yellowish brown, without 5YR", {
  skip_on_cran()
  p <- make_cscj2011_example("lowland")
  p$horizons$munsell_hue_moist <- c("10YR", "5YR", "5YR", "5YR"); p$horizons$munsell_value_moist <- c(3, 4, 4, 4)
  p$horizons$munsell_chroma_moist <- c(2, 4, 4, 4)
  expect_identical(.grp(p)[["code"]], "F5")
  # the SCSJ reads 5YR as brown
  expect_identical(classify_scsj2017(p, on_missing = "silent")$levels$code[2], "E4")
  p$horizons$munsell_hue_moist <- c("10YR", "7.5YR", "7.5YR", "7.5YR")
  expect_identical(.grp(p)[["code"]], "F4")
})

test_that("review: the brown polyhumic mean with open horizons, gaps and unreadable designations; Dystric", {
  skip_on_cran()
  kb <- function(...) utils::modifyList(list(designation = c("A1", "A2"), top_cm = c(0, 30), bottom_cm = c(30, NA),
              oc_pct = c(5, 20), phosphate_absorption_coef = 2200, munsell_hue_moist = "10YR",
              munsell_value_moist = 3, munsell_chroma_moist = 3, structure_grade = "moderate", field_texture = "loam",
              clay_pct = 20, ph_h2o = 5.5, layer_origin = "volcanic ash", redoximorphic_features_pct = 0,
              si_ox_pct = 1.5, al_ox_pct = 3, al_pyro_pct = 0.3, exchange_acidity_y1 = 1), list(...))
  g <- function(h) fulvic_andosols_cscj(.jp(h))$passed
  # an open A2 that may end at once: the mean is not decided
  expect_true(is.na(g(kb())))
  expect_true(is.na(g(kb(oc_pct = c(6.5, 4), top_cm = c(0, 25), bottom_cm = c(25, NA)))))
  # a gap inside the 40 cm
  expect_true(is.na(g(kb(top_cm = c(0, 30), bottom_cm = c(20, 40), oc_pct = c(5.5, 5.5)))))
  # closed and complete: decided
  expect_false(g(kb(designation = c("A1", "A2", "C"), top_cm = c(0, 30, 31), bottom_cm = c(30, 31, 100),
                     oc_pct = c(5, 9, 1), munsell_value_moist = c(3, 3, 5), munsell_chroma_moist = c(3, 3, 4))))
  # an unreadable designation may be a surface horizon
  expect_true(is.na(g(kb(designation = c("X1", "X2"), top_cm = c(0, 20), bottom_cm = c(20, 40), oc_pct = c(8, 8)))))
  # Dystric with no subsurface section: not met
  p <- make_cscj2011_example("dark_red")
  p$horizons$layer_origin <- "weathered basalt"; p$horizons$rock_origin <- c(NA, NA, NA, NA, "basalt")
  p$site$parent_material <- "basalt"; p$horizons$caco3_pct <- 0
  p$horizons$top_cm <- c(0, 20, 40, 70, 95); p$horizons$bottom_cm <- c(20, 40, 70, 95, 120)
  p$horizons$gravel_pct <- c(0, 30, 30, 30, NA)
  expect_false(dystric_dark_red_cscj(p)$passed)
})

test_that("verification: open horizons and the rock in the undescribed depth", {
  skip_on_cran()
  kb <- function(...) utils::modifyList(list(designation = c("A1", "A2"), top_cm = c(0, 25), bottom_cm = c(25, NA),
              oc_pct = c(5, 5), phosphate_absorption_coef = 2200, munsell_hue_moist = "10YR",
              munsell_value_moist = 3, munsell_chroma_moist = 3, structure_grade = "moderate", field_texture = "loam",
              clay_pct = 20, ph_h2o = 5.5, layer_origin = "volcanic ash", redoximorphic_features_pct = 0,
              si_ox_pct = 1.5, al_ox_pct = 3, al_pyro_pct = 0.3, exchange_acidity_y1 = 1), list(...))
  # below an open horizon a richer layer may follow: undecided
  expect_true(is.na(fulvic_andosols_cscj(.jp(kb()))$passed))
  # a gap above the rock counts as undescribed
  x <- .scsj_profile(.jp(kb(designation = c("A", "A2", "R"), top_cm = c(0, 25, 30), bottom_cm = c(20, 30, 60),
                            oc_pct = c(5, 5, NA))))
  expect_equal(soilKey:::.cscj_undescribed(x, 0, 40), 5)
  # Dystric: an open last horizon in the section leaves it undecided
  p <- make_cscj2011_example("dark_red")
  p$horizons$layer_origin <- "weathered basalt"; p$horizons$rock_origin <- c(NA, NA, NA, NA, "basalt")
  p$site$parent_material <- "basalt"; p$horizons$caco3_pct <- 0
  p$horizons <- p$horizons[1:3, ]
  p$horizons$top_cm <- c(0, 12, 50); p$horizons$bottom_cm <- c(12, 50, NA)
  expect_true(is.na(dystric_dark_red_cscj(p)$passed))
})
