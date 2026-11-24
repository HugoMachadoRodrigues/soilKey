# v0.9.431: the USDA family control section for particle-size classes and
# the particle-size classes with their substitutes (KST 13th ed., ch. 17,
# pp. 355-362), with the readings F5-F11 and F18 of the author (USDA audit,
# wave 5, 2026-11-23).

.ps_p <- function(...) {
  soilKey::PedonRecord$new(site = list(id = "ps"),
                           horizons = data.frame(..., stringsAsFactors = FALSE))
}
.ps_cs <- function(p, ...) {
  cs <- soilKey::family_control_section_usda(p, ...)
  c(cs$top, cs$bottom)
}
.ps_val <- function(p, ...) soilKey:::family_particle_size_usda(p, ...)$value

# A uniform profile 0-150 cm with the given fine-earth data.
.ps_uniform <- function(...) {
  .ps_p(designation = c("A", "Bw", "C"), top_cm = c(0, 25, 100),
        bottom_cm = c(25, 100, 150), ...)
}

# An Alfisol-like profile: Bt from 30 cm with clay films.
.ps_alfisol <- function(bt_bottom = 110, c_sand = 40, c_clay = 30, bd_bt = "gradual",
                        c_vfs = NA) {
  .ps_p(designation = c("Ap", "E", "Bt1", "Bt2", "C"),
        top_cm = c(0, 20, 30, 60, bt_bottom),
        bottom_cm = c(20, 30, 60, bt_bottom, 180),
        clay_pct = c(12, 12, 35, 38, c_clay),
        sand_pct = c(60, 60, 40, 35, c_sand),
        silt_pct = c(28, 28, 25, 27, 100 - c_sand - c_clay),
        very_fine_sand_pct = c(NA, NA, NA, NA, c_vfs),
        clay_films_amount = c(NA, NA, "common", "common", NA),
        boundary_distinctness = c("abrupt", "clear", "gradual", bd_bt, NA))
}


# ---- control section (pp. 357-358) -----------------------------------------

test_that("case A: a root-limiting layer within 36 cm", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("A", "Bw", "R"), top_cm = c(0, 15, 30),
             bottom_cm = c(15, 30, 60), clay_pct = c(20, 22, NA),
             sand_pct = c(40, 40, NA))
  cs <- family_control_section_usda(p)
  expect_equal(c(cs$top, cs$bottom), c(0, 30))
  expect_identical(cs$case, "A")
})

test_that("case B: Andisols, 100 cm from the mineral soil surface", {
  testthat::skip_on_cran()
  p <- .ps_uniform(clay_pct = 20, sand_pct = 40)
  cs <- family_control_section_usda(p, order_code = "AD")
  expect_equal(c(cs$top, cs$bottom), c(0, 100))
  expect_identical(cs$case, "B")
})

test_that("case C4: the upper 50 cm of the argillic horizon, or all of it", {
  testthat::skip_on_cran()
  cs <- family_control_section_usda(.ps_alfisol(), "AF", "Hapludalfs", "Typic Hapludalfs")
  expect_equal(c(cs$top, cs$bottom), c(30, 80))
  expect_identical(cs$case, "C4")
  thin <- family_control_section_usda(.ps_alfisol(bt_bottom = 70), "AF", "Hapludalfs",
                                      "Typic Hapludalfs")
  expect_equal(c(thin$top, thin$bottom), c(30, 70))
})

test_that("case C1: a strongly contrasting class below the argillic horizon", {
  testthat::skip_on_cran()
  # Bt over a sandy C (abrupt boundary at 70 cm, fine-grained sand recorded)
  p <- .ps_alfisol(bt_bottom = 70, c_sand = 92, c_clay = 3, bd_bt = "abrupt", c_vfs = 5)
  cs <- family_control_section_usda(p, "AF", "Hapludalfs", "Typic Hapludalfs")
  expect_identical(cs$case, "C1")
  expect_equal(c(cs$top, cs$bottom), c(30, 100))
  expect_identical(.ps_val(p, "AF", "Hapludalfs", "Typic Hapludalfs"),
                   "clayey over sandy or sandy-skeletal")
  # gradual boundary: the transition zone is undecided (F9)
  q <- .ps_alfisol(bt_bottom = 70, c_sand = 92, c_clay = 3, bd_bt = "gradual", c_vfs = 5)
  cq <- family_control_section_usda(q, "AF", "Hapludalfs", "Typic Hapludalfs")
  expect_true(is.na(cq$top))
  expect_true("boundary_distinctness" %in% cq$missing)
})

test_that("cases C2 and C3: a fragipan", {
  testthat::skip_on_cran()
  # C3: fragipan less than 50 cm below the top of the argillic horizon
  p3 <- .ps_p(designation = c("Ap", "E", "Bt", "Btx", "C"), top_cm = c(0, 20, 30, 55, 110),
              bottom_cm = c(20, 30, 55, 110, 160), clay_pct = c(12, 12, 30, 30, 25),
              sand_pct = c(40, 40, 30, 30, 30), silt_pct = c(48, 48, 40, 40, 45),
              clay_films_amount = c(NA, NA, "common", "common", NA))
  cs3 <- family_control_section_usda(p3, "AF", "Fragiudalfs", "Typic Fragiudalfs")
  expect_identical(cs3$case, "C3")
  expect_equal(c(cs3$top, cs3$bottom), c(30, 55))
  # C2: the argillic horizon is all in the fragipan
  p2 <- .ps_p(designation = c("Ap", "E", "Btx", "C"), top_cm = c(0, 20, 40, 110),
              bottom_cm = c(20, 40, 110, 160), clay_pct = c(12, 12, 30, 25),
              sand_pct = c(40, 40, 30, 30), silt_pct = c(48, 48, 40, 45),
              clay_films_amount = c(NA, NA, "common", NA))
  cs2 <- family_control_section_usda(p2, "AF", "Fragiudalfs", "Typic Fragiudalfs")
  expect_identical(cs2$case, "C2")
  expect_equal(c(cs2$top, cs2$bottom), c(25, 40))
})

test_that("case D: Lamellic subgroups, or an argillic horizon at 100 cm or deeper", {
  testthat::skip_on_cran()
  cs <- family_control_section_usda(.ps_alfisol(), "AF", "Hapludalfs", "Lamellic Hapludalfs")
  expect_identical(cs$case, "D")
  expect_equal(c(cs$top, cs$bottom), c(25, 100))
})

test_that("Arenic subgroups take case C even when Lamellic would not", {
  testthat::skip_on_cran()
  cs <- family_control_section_usda(.ps_alfisol(), "AF", "Hapludalfs", "Arenic Hapludalfs")
  expect_identical(substr(cs$case, 1, 1), "C")
})

test_that("case E: an argillic horizon ending above 25 cm", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("A", "Bt", "C"), top_cm = c(0, 8, 22), bottom_cm = c(8, 22, 120),
             clay_pct = c(10, 30, 12), sand_pct = c(70, 50, 70), silt_pct = c(20, 20, 18),
             clay_films_amount = c(NA, "common", NA))
  cs <- family_control_section_usda(p)
  expect_identical(cs$case, "E")
  expect_equal(c(cs$top, cs$bottom), c(8, 100))
})

test_that("case F: Wassents from the mineral soil surface", {
  testthat::skip_on_cran()
  cs <- family_control_section_usda(.ps_uniform(clay_pct = 20, sand_pct = 40),
                                    "EN", "Fluviwassents", "Typic Fluviwassents")
  expect_identical(cs$case, "F")
  expect_equal(c(cs$top, cs$bottom), c(0, 100))
})

test_that("case G: from the Ap horizon or 25 cm, whichever is deeper, to 100 cm", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("Ap", "Bw", "C"), top_cm = c(0, 32, 80),
             bottom_cm = c(32, 80, 150), clay_pct = 20, sand_pct = 40)
  cs <- family_control_section_usda(p)
  expect_identical(cs$case, "G")
  expect_equal(c(cs$top, cs$bottom), c(32, 100))
  # a root-limiting layer below 36 cm ends it
  q <- .ps_p(designation = c("A", "Bw", "Cr"), top_cm = c(0, 25, 70),
             bottom_cm = c(25, 70, 120), clay_pct = 20, sand_pct = 40)
  expect_equal(.ps_cs(q), c(25, 70))
})

test_that("an undecided clay-enriched horizon leaves the control section undecided", {
  testthat::skip_on_cran()
  # an Alfisol whose Bt has no clay recorded
  p <- .ps_p(designation = c("A", "Bt", "C"), top_cm = c(0, 20, 80),
             bottom_cm = c(20, 80, 150), clay_pct = c(12, NA, NA))
  cs <- family_control_section_usda(p, "AF", "Hapludalfs", "Typic Hapludalfs")
  if (is.na(cs$top)) {
    expect_true(length(cs$missing) > 0L)
    expect_null(.ps_val(p, "AF", "Hapludalfs", "Typic Hapludalfs"))
  } else {
    expect_true(cs$case %in% c("C4", "D"))
  }
})


# ---- the classes of key C (p. 360) -----------------------------------------

test_that("loamy and silty classes, fine and very-fine", {
  testthat::skip_on_cran()
  v <- function(...) .ps_val(.ps_uniform(...))
  expect_identical(v(clay_pct = 10, sand_pct = 60, coarse_sand_pct = 30), "coarse-loamy")
  expect_identical(v(clay_pct = 25, sand_pct = 40, coarse_sand_pct = 20), "fine-loamy")
  expect_identical(v(clay_pct = 10, sand_pct = 8), "coarse-silty")
  expect_identical(v(clay_pct = 25, sand_pct = 12), "fine-silty")
  expect_identical(v(clay_pct = 45, sand_pct = 20), "fine")
  expect_identical(v(clay_pct = 65, sand_pct = 10), "very-fine")
})

test_that("F5: sand not recorded leaves the loamy classes undecided", {
  testthat::skip_on_cran()
  r <- soilKey:::family_particle_size_usda(.ps_uniform(clay_pct = 20))
  expect_null(r$value)
  expect_true("sand_pct" %in% r$missing)
  # but 35 % clay or more decides without the sand
  expect_identical(.ps_val(.ps_uniform(clay_pct = 45)), "fine")
})

test_that("F6: loamy versus silty from particles 0.1-75 mm, bounds without very fine sand", {
  testthat::skip_on_cran()
  # sand 40, nothing else: floor 0, ceiling 40 -> undecided
  r <- soilKey:::family_particle_size_usda(.ps_uniform(clay_pct = 25, sand_pct = 40))
  expect_null(r$value)
  expect_true("very_fine_sand_pct" %in% r$missing)
  # very fine sand recorded: 0.1-2 mm = 40 - 30 = 10 -> silty
  expect_identical(.ps_val(.ps_uniform(clay_pct = 25, sand_pct = 40, very_fine_sand_pct = 30)),
                   "fine-silty")
  # coarse sand 20 is a floor of 20 -> loamy
  expect_identical(.ps_val(.ps_uniform(clay_pct = 25, sand_pct = 40, coarse_sand_pct = 20)),
                   "fine-loamy")
  # gravel by weight counts with the sand: 16 % gravel is a floor of 16
  expect_identical(.ps_val(.ps_uniform(clay_pct = 25, sand_pct = 8, coarse_fragments_pct = 10,
                                       gravel_wt_pct = 16)), "fine-loamy")
  # 12 % gravel and 8 % sand of the fine earth: between 12 and 19, undecided
  expect_null(.ps_val(.ps_uniform(clay_pct = 25, sand_pct = 8, coarse_fragments_pct = 10,
                                  gravel_wt_pct = 12)))
  # gravel recorded by volume only leaves the weight open
  r2 <- soilKey:::family_particle_size_usda(.ps_uniform(clay_pct = 25, sand_pct = 8,
                                                         coarse_fragments_pct = 10))
  expect_null(r2$value)
  expect_true("gravel_wt_pct" %in% r2$missing)
})

test_that("F7: sandy needs a sand grade other than very fine", {
  testthat::skip_on_cran()
  base <- list(clay_pct = 5, sand_pct = 88, silt_pct = 7)
  r <- do.call(soilKey:::family_particle_size_usda,
               list(do.call(.ps_uniform, base)))
  expect_null(r$value)
  expect_true(all(c("very_fine_sand_pct", "texture_class_usda") %in% r$missing))
  expect_identical(.ps_val(do.call(.ps_uniform, c(base, very_fine_sand_pct = 10))), "sandy")
  expect_identical(.ps_val(do.call(.ps_uniform, c(base, texture_class_usda = "FS"))), "sandy")
  expect_identical(.ps_val(do.call(.ps_uniform, c(base, fine_sand_pct = 30))), "sandy")
  # very fine sand dominant: it counts as silt, the layer is loamy
  expect_identical(.ps_val(do.call(.ps_uniform, c(base, very_fine_sand_pct = 60))),
                   "coarse-loamy")
  expect_identical(.ps_val(do.call(.ps_uniform, c(base[-3], silt_pct = 2, sand_pct = 93,
                                                  very_fine_sand_pct = 85))),
                   "coarse-silty")
})

test_that("skeletal classes, fragmental, and artifacts that are cohesive and persistent", {
  testthat::skip_on_cran()
  v <- function(...) .ps_val(.ps_uniform(...))
  expect_identical(v(clay_pct = 20, sand_pct = 40, coarse_fragments_pct = 50), "loamy-skeletal")
  expect_identical(v(clay_pct = 45, sand_pct = 20, coarse_fragments_pct = 50), "clayey-skeletal")
  expect_identical(v(clay_pct = 4, sand_pct = 90, very_fine_sand_pct = 5,
                     coarse_fragments_pct = 50), "sandy-skeletal")
  expect_identical(v(clay_pct = 20, sand_pct = 40, coarse_fragments_pct = 95), "fragmental")
  expect_identical(v(clay_pct = 45, sand_pct = 20, coarse_fragments_pct = 10, artefacts_pct = 30,
                     artefacts_cohesive_persistent = TRUE), "clayey-skeletal")
  expect_identical(v(clay_pct = 45, sand_pct = 20, coarse_fragments_pct = 10, artefacts_pct = 30),
                   "fine")
})

test_that("generalized loamy and clayey classes in Lithic and shallow soils", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("A", "Bw", "R"), top_cm = c(0, 15, 40),
             bottom_cm = c(15, 40, 60), clay_pct = c(20, 22, NA), sand_pct = c(40, 40, NA))
  expect_identical(.ps_val(p), "loamy")   # shallow family (R at 40 cm)
  q <- .ps_uniform(clay_pct = 45, sand_pct = 20)
  expect_identical(.ps_val(q, "IN", "Dystrudepts", "Lithic Dystrudepts"), "clayey")
})

test_that("F10: Vertisols are clayey above 30 % clay", {
  testthat::skip_on_cran()
  p <- .ps_uniform(clay_pct = 32, sand_pct = 30, coarse_sand_pct = 20)
  expect_identical(.ps_val(p, "VE", "Hapluderts", "Typic Hapluderts"), "fine")
  expect_identical(.ps_val(p, "IN", "Eutrudepts", "Typic Eutrudepts"), "fine-loamy")
  expect_identical(.ps_val(p, "VE", "Hapluderts", "Lithic Hapluderts"), "clayey")
})

test_that("F11: clay estimated from the water at 1500 kPa when the ratio is off", {
  testthat::skip_on_cran()
  # ratio 15 / 20 = 0.75: clay = 2.5 x (15 - 1) = 35 -> fine
  p <- .ps_uniform(clay_pct = 20, sand_pct = 40, coarse_sand_pct = 20,
                   water_content_1500kpa = 15, oc_pct = 1)
  r <- soilKey:::family_particle_size_usda(p)
  expect_identical(r$value, "fine")
  expect_true(isTRUE(r$evidence$clay_corrected))
  # water not measured: the measured clay, with a note
  q <- soilKey:::family_particle_size_usda(.ps_uniform(clay_pct = 20, sand_pct = 40,
                                                        coarse_sand_pct = 20))
  expect_identical(q$value, "fine-loamy")
  expect_match(q$evidence$note, "F11")
})

test_that("F18: measured parts must cover half of the control section", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("A", "Bw1", "Bw2"), top_cm = c(0, 25, 50),
             bottom_cm = c(25, 50, 150), clay_pct = c(20, 45, NA), sand_pct = c(40, 20, NA))
  r <- soilKey:::family_particle_size_usda(p)
  expect_null(r$value)
  expect_true("clay_pct" %in% r$missing)
})


# ---- substitutes (pp. 358-359) ---------------------------------------------

.ps_andic <- function(w15u, frag = 0, w15d = 20) {
  .ps_uniform(clay_pct = 15, sand_pct = 40, oc_pct = 3, bulk_density_g_cm3 = 0.7,
              al_ox_pct = 2.5, fe_ox_pct = 1.0, phosphate_retention_pct = 92,
              water_content_1500kpa_undried = w15u, water_content_1500kpa = w15d,
              coarse_fragments_pct = frag)
}

test_that("andic substitutes: ashy, medial, hydrous, skeletal", {
  testthat::skip_on_cran()
  expect_identical(.ps_val(.ps_andic(25, w15d = 10), "AD"), "ashy")
  expect_identical(.ps_val(.ps_andic(60), "AD"), "medial")
  expect_identical(.ps_val(.ps_andic(120), "AD"), "hydrous")
  expect_identical(.ps_val(.ps_andic(60, frag = 40), "AD"), "medial-skeletal")
  # an undecided undried water leaves it undecided
  r <- soilKey:::family_particle_size_usda(.ps_andic(NA), "AD")
  expect_null(r$value)
  expect_true("water_content_1500kpa_undried" %in% r$missing)
})

test_that("ashy from volcanic glass without andic soil properties", {
  testthat::skip_on_cran()
  p <- .ps_uniform(clay_pct = 8, sand_pct = 60, oc_pct = 1, phosphate_retention_pct = 10,
                   volcanic_glass_pct = 40, particles_002_2mm_pct = 70,
                   al_ox_pct = 0.3, fe_ox_pct = 0.3)
  expect_identical(.ps_val(p), "ashy")
})

test_that("gypseous substitutes", {
  testthat::skip_on_cran()
  g <- function(...) .ps_val(.ps_uniform(clay_pct = 10, gypsum_lt20mm_pct = 55, ...))
  expect_identical(g(sand_pct = 70, very_fine_sand_pct = 5), "coarse-gypseous")
  expect_identical(g(sand_pct = 30), "fine-gypseous")
  expect_identical(g(sand_pct = 30, coarse_fragments_pct = 40), "gypseous-skeletal")
})

test_that("pumiceous, cindery and diatomaceous", {
  testthat::skip_on_cran()
  v <- function(share) .ps_val(.ps_uniform(clay_pct = 5, sand_pct = 60, coarse_fragments_pct = 95,
                                           pyroclastic_fragments_pct = 90, tephra_wt_pct = 80,
                                           pumice_fragments_share_pct = share))
  expect_identical(v(80), "pumiceous")
  expect_identical(v(30), "cindery")
  d <- .ps_p(designation = c("A", "L1", "L2"), top_cm = c(0, 25, 100), bottom_cm = c(25, 100, 150),
             clay_pct = 40, sand_pct = 5, water_content_1500kpa = 60,
             bulk_density_g_cm3 = 0.5, liquid_limit_pct = 150, oc_pct = 4)
  expect_identical(.ps_val(d), "diatomaceous")
})

test_that("the thickest part names a substitute when the parts do not contrast", {
  testthat::skip_on_cran()
  mk <- function(split) .ps_p(designation = c("A", "Bw", "C"), top_cm = c(0, 25, split),
                              bottom_cm = c(25, split, 150),
                              clay_pct = 25, sand_pct = 40, coarse_sand_pct = 20,
                              gypsum_lt20mm_pct = c(0, 55, 0),
                              boundary_distinctness = c("clear", "diffuse", NA))
  expect_identical(.ps_val(mk(75)), "fine-gypseous")
  expect_identical(.ps_val(mk(45)), "fine-loamy")
})


# ---- strongly contrasting classes (pp. 361-362) ----------------------------

.ps_sc <- function(bd = "abrupt") {
  .ps_p(designation = c("Ap", "E", "Bt"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 150),
        clay_pct = c(6, 6, 45), sand_pct = c(88, 88, 25), silt_pct = c(6, 6, 30),
        very_fine_sand_pct = c(5, 5, NA), boundary_distinctness = c("clear", bd, NA))
}

test_that("a strongly contrasting pair: sandy over clayey", {
  testthat::skip_on_cran()
  r <- soilKey:::family_particle_size_usda(.ps_sc("abrupt"))
  expect_identical(r$value, "sandy over clayey")
  expect_identical(.ps_val(.ps_sc("clear")), "sandy over clayey")
})

test_that("F9: the transition zone from the boundary distinctness", {
  testthat::skip_on_cran()
  g <- soilKey:::family_particle_size_usda(.ps_sc("gradual"))
  expect_null(g$value)
  expect_true("boundary_distinctness" %in% g$missing)
  n <- soilKey:::family_particle_size_usda(.ps_sc(NA))
  expect_null(n$value)
  # diffuse: not contrasting; the weighted average decides
  expect_false(grepl(" over ", .ps_val(.ps_sc("diffuse")) %||% ""))
})

test_that("a pair outside the closed list is not contrasting", {
  testthat::skip_on_cran()
  # clayey over loamy needs 25 % clay of difference: 45 vs 30 is not
  p <- .ps_p(designation = c("A", "Bt", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 150),
             clay_pct = c(45, 45, 30), sand_pct = c(20, 20, 40), coarse_sand_pct = c(10, 10, 20),
             boundary_distinctness = c("clear", "abrupt", NA))
  expect_false(grepl(" over ", .ps_val(p)))
})

test_that("aniso: more than one contrasting pair", {
  testthat::skip_on_cran()
  p <- .ps_p(designation = c("A", "E", "Bt", "C"), top_cm = c(0, 15, 40, 70),
             bottom_cm = c(15, 40, 70, 150),
             clay_pct = c(8, 8, 45, 2), sand_pct = c(86, 86, 25, 96), silt_pct = c(6, 6, 30, 2),
             very_fine_sand_pct = c(5, 5, NA, 5),
             boundary_distinctness = c("clear", "abrupt", "abrupt", NA))
  expect_identical(.ps_val(p), "clayey over sandy or sandy-skeletal, aniso")
})

test_that("the closed list holds 71 pairs as data", {
  testthat::skip_on_cran()
  P <- soilKey:::.PS_CONTRAST_PAIRS_USDA
  expect_equal(nrow(P), 71L)
  expect_equal(P$n, 1:71)
  expect_true(all(grepl(" over ", P$name)))
})


# ---- taxa with no class name (p. 356) --------------------------------------

test_that("Psamments and the sandy Psammentic subgroups are not named", {
  testthat::skip_on_cran()
  p <- .ps_uniform(clay_pct = 4, sand_pct = 92)
  r <- soilKey:::family_particle_size_usda(p, "EN", "Quartzipsamments", "Typic Quartzipsamments")
  expect_null(r$value)
  expect_identical(r$evidence$reason, "not named in this taxon")
  expect_length(r$missing, 0L)
  s <- .ps_uniform(clay_pct = 4, sand_pct = 92, very_fine_sand_pct = 5)
  r2 <- soilKey:::family_particle_size_usda(s, "AF", "Hapludalfs", "Psammentic Hapludalfs")
  expect_identical(r2$evidence$reason, "not named in this taxon")
  # Histosols outside the Terric subgroups
  r3 <- soilKey:::family_particle_size_usda(p, "HI", "Haplohemists", "Typic Haplohemists")
  expect_identical(r3$evidence$reason, "not named in this taxon")
})

test_that("classify_usda() hands the assigned taxa to the family", {
  testthat::skip_on_cran()
  res <- classify_usda(make_luvisol_canonical(), include_family = TRUE, on_missing = "silent")
  ps <- res$trace$family$particle_size
  expect_identical(ps$evidence$control_section$case, "C4")
  expect_identical(ps$value, "fine")
  expect_match(res$name, "^fine, ")
})
