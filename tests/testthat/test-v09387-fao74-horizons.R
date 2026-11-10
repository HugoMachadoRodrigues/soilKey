# v0.9.387: the FAO-Unesco legend, part 7: the diagnostic horizons of the
# Legend of 1974 (Soil Map of the World, Vol. I, pp. 21-27) as three-valued
# predicates <name>_fao74(), R/diagnostics-fao74.R. A pass and a near miss at
# every printed number, the same profile through _fao88 and _fao74 where the
# two editions differ, and the stand-in flags.

.p74 <- function(..., site = list(id = "p")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}
.ok <- function(res) res$passed

# A dark A over a C: a mollic A of 30 cm when `oc` and `bs` allow it.
.dark74 <- function(oc = 2, bs = 70, ...) {
  base <- list(top_cm = c(0, 30), bottom_cm = c(30, 120), designation = c("A", "C"),
               clay_pct = 25, silt_pct = 35, sand_pct = 40, oc_pct = c(oc, 0.2),
               bs_pct = c(bs, bs), cec_cmol = 12,
               munsell_hue_moist = "10YR", munsell_value_moist = c(2, 5),
               munsell_chroma_moist = c(2, 4), munsell_value_dry = c(3, 6),
               munsell_chroma_dry = c(2, 4), structure_type = c("granular", "massive"),
               structure_grade = c("moderate", NA), ph_h2o = 6.5)
  a <- list(...)
  base[names(a)] <- a
  do.call(.p74, base)
}

# An A and E over a B and a C; the B with clay films unless changed.
.lu74 <- function(...) {
  base <- list(top_cm = c(0, 20, 40, 90), bottom_cm = c(20, 40, 90, 150),
               designation = c("A", "E", "Bt", "C"),
               clay_pct = c(12, 12, 25, 22), silt_pct = c(20, 20, 25, 25),
               sand_pct = c(68, 68, 50, 53), oc_pct = c(1.5, 0.4, 0.4, 0.2),
               cec_cmol = c(8, 4, 10, 8), bs_pct = c(60, 55, 70, 80),
               munsell_hue_moist = c("10YR", "10YR", "5YR", "7.5YR"),
               munsell_value_moist = c(4, 6, 4, 5), munsell_chroma_moist = c(3, 2, 6, 4),
               munsell_value_dry = c(6, 7, 5, 6), munsell_chroma_dry = c(3, 2, 6, 4),
               clay_films_amount = c(NA, NA, "common", NA))
  a <- list(...)
  base[names(a)] <- a
  do.call(.p74, base)
}

test_that("the fourteen predicates exist and return DiagnosticResults of the 1974 legend", {
  skip_on_cran()
  ids <- c("h_horizon", "histic_h", "mollic_a", "umbric_a", "ochric_a", "argillic_b",
           "natric_b", "cambic_b", "spodic_b", "oxic_b", "calcic", "gypsic", "sulfuric",
           "albic_e")
  expect_identical(.F74_HZ$id, ids)
  p <- .lu74()
  for (id in ids) {
    r <- get(paste0(id, "_fao74"))(p)
    expect_s3_class(r, "DiagnosticResult")
    expect_identical(r$name, paste0(id, "_fao74"))
    expect_true(is.logical(r$passed) && length(r$passed) == 1L)
    expect_match(r$reference, "1974")
    expect_identical(r$evidence$edition, "1974")
  }
  # the 1988 predicates still say 1988
  expect_identical(argic_b_fao88(p)$evidence$edition, "1988")
  src <- test_path("..", "..", "R", "diagnostics-fao74.R")
  skip_if_not(file.exists(src))
  src <- readLines(src, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", src, perl = TRUE)))
  # edition isolation: the 1974 file names no 1988 threshold table
  expect_false(any(grepl("fao_thresholds_1988", src, fixed = TRUE)))
})

test_that("H horizon: organic matter from 20 % (no clay) to 30 % (60 % clay), saturated", {
  skip_on_cran()
  h <- function(oc, clay, desg = "H", site = list(id = "h"))
    .p74(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c(desg, "Cg"),
         oc_pct = c(oc, 0.5), clay_pct = c(clay, 20), silt_pct = c(NA, 40), sand_pct = c(NA, 40),
         site = site)
  # OM = OC x 1.724 (Q11): 11.61 x 1.724 = 20.02; 11.59 x 1.724 = 19.98
  expect_true(.ok(h_horizon_fao74(h(11.61, 0))))
  expect_false(.ok(h_horizon_fao74(h(11.59, 0))))
  expect_true("om_from_oc_x1.724" %in% h_horizon_fao74(h(11.61, 0))$evidence$flags)
  # 30 % at more than 60 % clay: 17.41 -> 30.02; 17.39 -> 29.98
  expect_true(.ok(h_horizon_fao74(h(17.41, 70))))
  expect_false(.ok(h_horizon_fao74(h(17.39, 70))))
  # 25 % at 30 % clay: 14.51 -> 25.02; 14.49 -> 24.98
  expect_true(.ok(h_horizon_fao74(h(14.51, 30))))
  expect_false(.ok(h_horizon_fao74(h(14.49, 30))))
  # not saturated: an O horizon is not an H
  expect_false(.ok(h_horizon_fao74(h(25, 0, "O"))))
  # undesignated: the drainage decides, else undecided
  r <- h_horizon_fao74(h(25, 0, NA))
  expect_true(is.na(r$passed)); expect_true("drainage_class" %in% r$missing)
  expect_true(.ok(h_horizon_fao74(h(25, 0, NA, list(id = "w", drainage_class = "poorly drained")))))
  expect_false(.ok(h_horizon_fao74(h(25, 0, NA, list(id = "w", drainage_class = "well drained")))))
  # clay not recorded: 30 % OM passes, below 20 % fails, between undecided
  expect_true(.ok(h_horizon_fao74(h(17.41, NA))))
  expect_false(.ok(h_horizon_fao74(h(11.59, NA))))
  expect_true(is.na(.ok(h_horizon_fao74(h(14.5, NA)))))
})

test_that("histic H: more than 20 cm, no printed depth, and the 14-28 % mixing line", {
  skip_on_cran()
  hh <- function(top, bot) .p74(top_cm = c(0, top, bot), bottom_cm = c(top, bot, bot + 50),
                                designation = c("A", "H", "Cg"), oc_pct = c(2, 30, 0.5),
                                clay_pct = c(20, NA, 20), silt_pct = 40, sand_pct = 40)
  expect_false(.ok(histic_h_fao74(hh(10, 30))))     # 20 cm: not more than 20
  expect_true(.ok(histic_h_fao74(hh(10, 31))))
  # an H at 45-70 cm: the 1988 histic H has its upper boundary within 40 cm;
  # 1974 prints no depth (read within 125 cm, Q10)
  p <- hh(45, 70)
  expect_true(.ok(histic_h_fao74(p)))
  expect_false(.ok(histic_h_fao88(p)))
  # the thin surface layer, mixed to 25 cm, at 30 % clay: 1974 OM >= 14 +
  # 14 x 30 / 60 = 21; 1988 OC >= 8 + 8 x 30 / 60 = 12
  thin <- function(oc) .p74(top_cm = c(0, 10), bottom_cm = c(10, 60), designation = c("H", "Cg"),
                            oc_pct = c(oc, 1), clay_pct = 30, silt_pct = 30, sand_pct = 40)
  expect_true(.ok(histic_h_fao74(thin(29))))       # (290 + 15) / 25 = 12.2 OC = 21.03 OM
  expect_false(.ok(histic_h_fao74(thin(28.9))))    # 12.16 OC = 20.96 OM
  expect_true(.ok(histic_h_fao88(thin(28.7))))     # 12.08 OC passes 1988
  expect_false(.ok(histic_h_fao74(thin(28.7))))    # 20.83 OM fails 1974
  # 28 % at more than 60 % clay, 14 % without clay
  thin2 <- function(oc, clay) .p74(top_cm = c(0, 10), bottom_cm = c(10, 60),
                                   designation = c("H", "Cg"), oc_pct = c(oc, 1),
                                   clay_pct = clay, silt_pct = 30, sand_pct = 70 - pmin(clay, 70))
  expect_true(.ok(histic_h_fao74(thin2(39.2, 70))))   # (392 + 15) / 25 x 1.724 = 28.07
  expect_false(.ok(histic_h_fao74(thin2(39.0, 70))))  # 27.93
  expect_true(.ok(histic_h_fao74(thin2(18.9, 0))))    # 14.07
  expect_false(.ok(histic_h_fao74(thin2(18.7, 0))))   # 13.93
  # a plough layer of 25 cm or more, mixed to 25 cm
  ap <- function(oc) .p74(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Ap", "C"),
                          oc_pct = c(oc, 1), clay_pct = 30, silt_pct = 30, sand_pct = 40)
  expect_true(.ok(histic_h_fao74(ap(12.2))))
  expect_false(.ok(histic_h_fao74(ap(12.1))))
  # eutric and dystric by pH (H2O 1:5) 5.5
  e <- .p74(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("H", "Cg"),
            oc_pct = c(30, 0.5), clay_pct = c(NA, 20), silt_pct = 40, sand_pct = 40,
            ph_h2o_1to5 = c(5.5, 6))
  expect_true(.ok(histic_h_fao74(e, base = "eutric")))
  expect_false(.ok(histic_h_fao74(e, base = "dystric")))
  d <- .p74(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("H", "Cg"),
            oc_pct = c(30, 0.5), clay_pct = c(NA, 20), silt_pct = 40, sand_pct = 40,
            ph_h2o_1to5 = c(5.4, 6))
  expect_true(.ok(histic_h_fao74(d, base = "dystric")))
  expect_false(.ok(histic_h_fao74(d, base = "eutric")))
  # buried under 50 cm of new material (74 p. 32)
  b <- function(top) .p74(top_cm = c(0, top, top + 30), bottom_cm = c(top, top + 30, top + 80),
                          designation = c("C", "Hb", "Cg"), oc_pct = c(0.5, 30, 0.5),
                          clay_pct = c(20, NA, 20), silt_pct = 40, sand_pct = 40)
  expect_true(.ok(histic_h_fao74(b(49))))
  expect_false(.ok(histic_h_fao74(b(50))))
})

test_that("mollic A: 1 % organic matter, 4 % with lime, and no strongly humic waiver", {
  skip_on_cran()
  # 1 % OM: OC 0.59 x 1.724 = 1.017 passes, 0.58 -> 0.99992 fails; 1988
  # asks 0.6 % OC
  expect_true(.ok(mollic_a_fao74(.dark74(oc = 0.59))))
  expect_false(.ok(mollic_a_fao74(.dark74(oc = 0.58))))
  expect_false(.ok(mollic_a_fao88(.dark74(oc = 0.59))))
  expect_true(.ok(mollic_a_fao88(.dark74(oc = 0.6))))
  expect_true("om_from_oc_x1.724" %in% mollic_a_fao74(.dark74())$evidence$flags)
  # base saturation 50 %
  expect_true(.ok(mollic_a_fao74(.dark74(bs = 50))))
  expect_false(.ok(mollic_a_fao74(.dark74(bs = 49))))
  # colours: moist value and chroma below 3.5, dry value below 5.5
  expect_false(.ok(mollic_a_fao74(.dark74(munsell_value_moist = c(4, 6)))))
  expect_false(.ok(mollic_a_fao74(.dark74(munsell_chroma_moist = c(4, 4)))))
  expect_false(.ok(mollic_a_fao74(.dark74(munsell_value_dry = c(6, 7)))))
  # the lime waiver: more than 40 % lime, moist value 5 or less, 4 % OM
  lime <- function(oc, cc = 45) .dark74(oc = oc, caco3_pct = c(cc, 50),
                                        munsell_value_moist = c(4, 6), munsell_value_dry = c(6, 8),
                                        munsell_chroma_moist = c(2, 3))
  expect_true(.ok(mollic_a_fao74(lime(2.33))))     # 4.017 OM
  expect_false(.ok(mollic_a_fao74(lime(2.32))))    # 3.99968 OM
  expect_false(.ok(mollic_a_fao74(lime(2.33, cc = 40))))
  # the colour contrast with the C: 1988 waives it when strongly humic
  # (weighted OC over 1.4 % to 100 cm), 1974 does not
  hum <- .dark74(oc = 4, oc_pct = c(4, 1), munsell_value_moist = c(2, 2),
                 munsell_value_dry = c(3, 3), munsell_chroma_moist = c(2, 2))
  expect_true(.ok(mollic_a_fao88(hum)))
  expect_false(.ok(mollic_a_fao74(hum)))
  # thickness: 18 cm and more than a third of a solum under 75 cm; more
  # than 25 cm otherwise; 10 cm on rock
  th <- function(a, b) .dark74(top_cm = c(0, a, b), bottom_cm = c(a, b, 150),
                               designation = c("A", "Bw", "C"), oc_pct = c(2, 0.3, 0.2),
                               bs_pct = 70, munsell_value_moist = c(2, 4, 5),
                               munsell_chroma_moist = c(2, 4, 4), munsell_value_dry = c(3, 5, 6),
                               munsell_chroma_dry = c(2, 4, 4),
                               structure_type = c("granular", "subangular blocky", "massive"),
                               structure_grade = c("moderate", "moderate", NA))
  expect_true(.ok(mollic_a_fao74(th(20, 50))))     # 20 > 50 / 3
  expect_false(.ok(mollic_a_fao74(th(17, 50))))    # below 18
  expect_false(.ok(mollic_a_fao74(th(20, 75))))    # solum 75: more than 25 cm asked
  expect_true(.ok(mollic_a_fao74(th(26, 80))))
  rk <- .dark74(top_cm = c(0, 10), bottom_cm = c(10, 50), designation = c("A", "R"),
                oc_pct = c(2, NA), munsell_value_moist = c(2, NA), munsell_chroma_moist = c(2, NA),
                munsell_value_dry = c(3, NA), munsell_chroma_dry = c(2, NA), bs_pct = c(70, NA))
  expect_true(.ok(mollic_a_fao74(rk)))
})

test_that("umbric A: base saturation below 50 %, the consistence rule and plaggen", {
  skip_on_cran()
  expect_true(.ok(umbric_a_fao74(.dark74(bs = 49))))
  expect_false(.ok(umbric_a_fao74(.dark74(bs = 50))))
  # massive and hard when dry: excluded unless the A never dries
  hard <- function(site = list(id = "u")) .dark74(bs = 30, structure_type = c("massive", "massive"),
                                                 rupture_resistance = c("hard", NA), site = site)
  expect_false(.ok(umbric_a_fao74(hard())))
  r <- umbric_a_fao74(hard(list(id = "u", soil_moisture_regime = "perudic")))
  expect_true(.ok(r))
  expect_true("umbric_always_moist_no_consistence_restriction" %in% r$evidence$flags)
  expect_false(.ok(umbric_a_fao88(hard(list(id = "u", soil_moisture_regime = "perudic")))))
  # plaggen A horizons are excluded in 1974; the 1988 umbric A reads the
  # measured citric P2O5 first
  pl <- .dark74(bs = 30, layer_origin = c("plaggen", NA), p2o5_citric_mg_kg = c(100, 50))
  expect_true(.ok(umbric_a_fao88(pl)))
  r <- umbric_a_fao74(pl)
  expect_false(.ok(r))
  expect_true("plaggen_a_not_umbric" %in% r$evidence$flags)
})

test_that("ochric A: very weak below a line from 1 % (sand/clay 1) to 0.5 % OM (13)", {
  skip_on_cran()
  # a light A of 40 cm over a C
  oa <- function(oc, clay, sand, rock = FALSE) {
    if (rock) {
      return(.p74(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "R"),
                  oc_pct = c(oc, NA), clay_pct = c(clay, NA), sand_pct = c(sand, NA),
                  silt_pct = c(100 - clay - sand, NA), bs_pct = c(90, NA),
                  munsell_hue_moist = "10YR", munsell_value_moist = c(5, NA),
                  munsell_chroma_moist = c(4, NA), munsell_value_dry = c(7, NA),
                  munsell_chroma_dry = c(4, NA)))
    }
    .p74(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "C"),
         oc_pct = c(oc, 0.1), clay_pct = clay, sand_pct = sand, silt_pct = 100 - clay - sand,
         bs_pct = 90, munsell_hue_moist = "10YR", munsell_value_moist = c(5, 6),
         munsell_chroma_moist = c(4, 4), munsell_value_dry = c(7, 7), munsell_chroma_dry = c(4, 4))
  }
  g <- function(...) ochric_grade_fao74(oa(...))
  # ratio 1 (40 / 40): below 1 % OM
  expect_identical(g(0.574, 40, 40), "very weak")   # 0.9896
  expect_identical(g(0.59, 40, 40), "weak")         # 1.017
  # ratio 13 (65 / 5): below 0.5 % OM
  expect_identical(g(0.29, 5, 65), "very weak")     # 0.49996
  expect_identical(g(0.30, 5, 65), "weak")          # 0.5172
  # ratio 7 (70 / 10): linear, 0.75 % (Q26)
  expect_identical(g(0.43, 10, 70), "very weak")    # 0.7413
  expect_identical(g(0.44, 10, 70), "weak")         # 0.7586
  # hard rock within 40 cm: 1.2 % (ratio 1) in the upper 18 cm
  expect_identical(g(0.69, 40, 40, rock = TRUE), "very weak")   # 1.1896
  expect_identical(g(0.70, 40, 40, rock = TRUE), "weak")        # 1.2068
  # the predicate with its grade
  expect_true(.ok(ochric_a_fao74(oa(0.29, 5, 65), grade = "very_weak")))
  expect_false(.ok(ochric_a_fao74(oa(0.29, 5, 65), grade = "weak")))
  expect_true(.ok(ochric_a_fao74(oa(0.30, 5, 65), grade = "weak")))
  # undecided without the sand
  p <- .p74(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "C"),
            oc_pct = c(0.3, 0.1), clay_pct = 5, bs_pct = 90, munsell_hue_moist = "10YR",
            munsell_value_moist = 5, munsell_chroma_moist = 4, munsell_value_dry = 7,
            munsell_chroma_dry = 4)
  expect_true(is.na(ochric_grade_fao74(p)))
  # a mollic A is not ochric
  expect_identical(ochric_grade_fao74(.dark74()), "none")
  # fresh stratified alluvium: not ochric in 1988, ochric in 1974
  fr <- .p74(top_cm = c(0, 15), bottom_cm = c(15, 100), designation = c("A", "C"),
             oc_pct = c(0.5, 0.3), clay_pct = 20, sand_pct = 40, silt_pct = 40, bs_pct = 90,
             munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 4,
             munsell_value_dry = 7, munsell_chroma_dry = 4,
             stratification_pattern = c("fine", NA), layer_origin = c("alluvial", NA))
  expect_false(.ok(ochric_a_fao88(fr)))
  expect_true(.ok(ochric_a_fao74(fr)))
})

test_that("argillic B: the 3 / 1.2 / 8 increase within 30 cm and illuviation required", {
  skip_on_cran()
  expect_true(.ok(argillic_b_fao74(.lu74())))
  inc <- function(e, b) .lu74(clay_pct = c(e, e, b, b), silt_pct = 20,
                              sand_pct = c(80 - e, 80 - e, 80 - b, 80 - b))
  expect_true(.ok(argillic_b_fao74(inc(10, 13))))      # 3 points below 15 %
  expect_false(.ok(argillic_b_fao74(inc(10, 12.9))))
  expect_true(.ok(argillic_b_fao74(inc(15, 18))))      # exactly 15: ratio 1.2 (default 12)
  expect_false(.ok(argillic_b_fao74(inc(15, 17.9))))
  expect_true(.ok(argillic_b_fao74(inc(20, 24))))
  expect_false(.ok(argillic_b_fao74(inc(20, 23.9))))
  expect_true(.ok(argillic_b_fao74(inc(40, 48))))      # exactly 40: 8 points
  expect_false(.ok(argillic_b_fao74(inc(40, 47.9))))
  expect_true(.ok(argillic_b_fao74(inc(42, 50))))
  expect_false(.ok(argillic_b_fao74(inc(42, 49.9))))
  # within 30 cm: a transition layer of 30 cm keeps it, 31 cm does not
  tr <- function(t) .p74(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 100 + t),
                         designation = c("E", "BE", "Bt"), clay_pct = c(10, 12, 14),
                         silt_pct = 20, sand_pct = c(70, 68, 66),
                         clay_films_amount = c(NA, NA, "common"))
  expect_true(.ok(argillic_b_fao74(tr(30))))
  expect_false(.ok(argillic_b_fao74(tr(31))))
  # no clay skins, no Bt: the 1988 argic B passes (an increase within 15 cm
  # under 18 cm of coarser material), the 1974 argillic B does not (Q13)
  nb <- .lu74(designation = c("A", "E", "B", "C"), clay_films_amount = NA)
  expect_true(.ok(argic_b_fao88(nb)))
  r <- argillic_b_fao74(nb)
  expect_false(.ok(r))
  # a Bt designation counts as the observation, flagged
  r <- argillic_b_fao74(.lu74(clay_films_amount = NA))
  expect_true(.ok(r))
  expect_true("bt_designation_for_clay_skins" %in% r$evidence$flags)
  # more fine clay than the eluvial horizon, when recorded in both
  expect_false(.ok(argillic_b_fao74(.lu74(fine_clay_pct = c(6, 6, 6, 6)))))
  expect_true(.ok(argillic_b_fao74(.lu74(fine_clay_pct = c(6, 6, 14, 10)))))
  expect_true("fine_clay_not_compared" %in% argillic_b_fao74(.lu74())$evidence$flags)
  # thickness: one tenth of the overlying horizons (100 cm: 10 cm), 7.5 cm
  # in loamy material, 15 cm in sand and loamy sand
  thk <- function(t, clay = c(12, 25, 22), sand = c(68, 50, 53))
    .p74(top_cm = c(0, 100, 100 + t), bottom_cm = c(100, 100 + t, 160),
         designation = c("E", "Bt", "C"), clay_pct = clay, sand_pct = sand,
         silt_pct = 100 - clay - sand, clay_films_amount = c(NA, "common", NA))
  expect_true(.ok(argillic_b_fao74(thk(10))))
  expect_false(.ok(argillic_b_fao74(thk(9))))
  sandy <- function(t) .p74(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 120),
                            designation = c("E", "Bt", "C"), clay_pct = c(2, 6, 3),
                            sand_pct = c(95, 90, 94), silt_pct = c(3, 4, 3),
                            clay_films_amount = c(NA, "common", NA))
  expect_true(.ok(argillic_b_fao74(sandy(15))))
  expect_false(.ok(argillic_b_fao74(sandy(14))))
  lo <- .p74(top_cm = c(0, 20, 27.5), bottom_cm = c(20, 27.5, 120), designation = c("E", "Bt", "C"),
             clay_pct = c(12, 25, 22), sand_pct = c(68, 50, 53), silt_pct = c(20, 25, 25),
             clay_films_amount = c(NA, "common", NA))
  expect_true(.ok(argillic_b_fao74(lo)))
  lo2 <- .p74(top_cm = c(0, 20, 27), bottom_cm = c(20, 27, 120), designation = c("E", "Bt", "C"),
              clay_pct = c(12, 25, 22), sand_pct = c(68, 50, 53), silt_pct = c(20, 25, 25),
              clay_films_amount = c(NA, "common", NA))
  expect_false(.ok(argillic_b_fao74(lo2)))
  # more than 15 cm where the eluvial and illuvial horizons reach below 150
  # cm (a B starting at 170 cm, read with max_top_cm = 200)
  deep <- function(t) .p74(top_cm = c(0, 170, 170 + t), bottom_cm = c(170, 170 + t, 250),
                           designation = c("E", "Bt", "C"), clay_pct = c(12, 25, 22),
                           sand_pct = c(68, 50, 53), silt_pct = c(20, 25, 25),
                           clay_films_amount = c(NA, "common", NA))
  expect_true(.ok(argillic_b_fao74(deep(16), max_top_cm = 200)))
  expect_false(.ok(argillic_b_fao74(deep(15), max_top_cm = 200)))
  expect_false(.ok(argillic_b_fao74(deep(16))))      # upper limit 125 cm (Q10)
  # lamellae: 15 cm combined
  la <- function(s) .lu74(designation = c("A", "E", "E&Bt", "C"), lamellae_cumulative_cm = c(NA, NA, s, NA))
  expect_true(.ok(argillic_b_fao74(la(15))))
  expect_false(.ok(argillic_b_fao74(la(14))))
  # swelling 2:1 clays: pressure faces with a third more fine clay
  sw <- function(fc) .p74(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                          designation = c("A", "B", "C"), clay_pct = c(35, 45, 40),
                          silt_pct = 30, sand_pct = c(35, 25, 30), smectite_pct = c(NA, 70, NA),
                          slickensides = c(NA, "few", NA), fine_clay_pct = c(10, fc, 12))
  r <- argillic_b_fao74(sw(25))   # 25 / 45 = 0.56 vs 10 / 35 = 0.29
  expect_true(.ok(r))
  expect_true("pressure_faces_of_2to1_clays" %in% r$evidence$flags)
  expect_false(.ok(argillic_b_fao74(sw(13))))
})

test_that("natric B: argillic items 1-5, columnar structure and ESP above 15 %", {
  skip_on_cran()
  na <- function(esp, st = "columnar") .lu74(structure_type = c(NA, NA, st, NA),
                                            esp_pct = c(2, 3, esp, esp))
  expect_true(.ok(natric_b_fao74(na(15.1))))
  expect_false(.ok(natric_b_fao74(na(15))))
  expect_false(.ok(argillic_b_fao74(na(15.1))))
  expect_true(.ok(argillic_b_fao74(na(15))))
  expect_false(.ok(natric_b_fao74(na(30, "subangular blocky"))))
})

test_that("cambic B: weatherable minerals, alteration, reduction and a base at 25 cm", {
  skip_on_cran()
  cb <- function(bot = 25, cec = 5, ...) {
    base <- list(top_cm = c(0, 15, bot), bottom_cm = c(15, bot, 120), designation = c("A", "Bw", "C"),
                 clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = c(0.8, 0.3, 0.1),
                 cec_cmol = c(8, cec, 5), bs_pct = 80,
                 munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_value_moist = c(5, 5, 5),
                 munsell_chroma_moist = c(3, 6, 4), munsell_value_dry = c(6, 6, 6),
                 munsell_chroma_dry = c(3, 6, 4), structure_type = c("granular", "subangular blocky", "massive"),
                 structure_grade = c("weak", "moderate", NA), ph_h2o = 7)
    a <- list(...); base[names(a)] <- a
    do.call(.p74, base)
  }
  expect_true(.ok(cambic_b_fao74(cb())))
  # base at 25 cm or more; no minimum thickness in 1974, 15 cm in 1988
  expect_false(.ok(cambic_b_fao74(cb(bot = 24))))
  expect_false(.ok(cambic_b_fao88(cb())))            # 10 cm thick
  expect_true(.ok(cambic_b_fao88(cb(bot = 30))))
  # CEC (NH4OAc) above 16 me per 100 g clay (20 % clay: 3.2 is 16)
  expect_false(.ok(cambic_b_fao74(cb(cec = 3.2))))
  expect_true(.ok(cambic_b_fao74(cb(cec = 3.3))))
  # or more than 3 % weatherable minerals other than muscovite
  expect_true(.ok(cambic_b_fao74(cb(cec = 3, weatherable_minerals_pct = 3.1, muscovite_pct = 0))))
  expect_false(.ok(cambic_b_fao74(cb(cec = 3, weatherable_minerals_pct = 3, muscovite_pct = 0))))
  expect_false(.ok(cambic_b_fao74(cb(cec = 3, weatherable_minerals_pct = 8, muscovite_pct = 5))))
  # or more than 6 % muscovite
  expect_true(.ok(cambic_b_fao74(cb(cec = 3, weatherable_minerals_pct = 6.1, muscovite_pct = 6.1))))
  expect_false(.ok(cambic_b_fao74(cb(cec = 3, weatherable_minerals_pct = 6, muscovite_pct = 6))))
  # texture: sand and loamy sand are not very fine sand or finer
  r <- cambic_b_fao74(cb(clay_pct = 5, silt_pct = 5, sand_pct = 90, cec_cmol = c(8, 2, 1)))
  expect_false(.ok(r))
  # no alteration: the same chroma, hue and clay as the C
  na <- cb(munsell_hue_moist = "10YR", munsell_chroma_moist = c(3, 4, 4))
  expect_false(.ok(cambic_b_fao74(na)))
  # reduction colours: unmottled chroma 1 or less at a value of 4 or more,
  # below 1 at a lower value; 2 or less when mottled
  rd <- function(v, c, mott = NA) cb(munsell_hue_moist = "10YR", munsell_value_moist = c(5, v, 5),
                                     munsell_chroma_moist = c(3, c, c), mottle_colors = c(NA, mott, NA))
  expect_true(.ok(cambic_b_fao74(rd(5, 1))))
  expect_false(.ok(cambic_b_fao74(rd(5, 2))))
  expect_true(.ok(cambic_b_fao74(rd(5, 2, "7.5YR 5/8"))))
  expect_false(.ok(cambic_b_fao74(rd(5, 3, "7.5YR 5/8"))))
  expect_false(.ok(cambic_b_fao74(rd(3, 1))))
  expect_true(.ok(cambic_b_fao74(rd(3, 0))))
})

test_that("spodic B: below 12.5 cm and below an Ap (1974) versus below an A (1988)", {
  skip_on_cran()
  sp <- function(desg, a_bot) .p74(top_cm = c(0, a_bot, 40), bottom_cm = c(a_bot, 40, 100),
                                   designation = c(desg, "Bhs", "C"), clay_pct = c(4, 5, 3),
                                   silt_pct = 5, sand_pct = c(91, 90, 92), oc_pct = c(2, 1.5, 0.2),
                                   fe_pyro_pct = c(NA, 0.5, NA), al_pyro_pct = c(NA, 0.6, NA),
                                   fe_dcb_pct = c(NA, 1, NA), al_dcb_pct = c(NA, 0.8, NA),
                                   cec_ph82_cmol = c(NA, 10, NA), ph_h2o = 4.5)
  expect_true(.ok(spodic_b_fao88(sp("A", 8))))
  expect_false(.ok(spodic_b_fao74(sp("A", 8))))       # top at 8 cm
  expect_true(.ok(spodic_b_fao74(sp("A", 13))))
  expect_true(.ok(spodic_b_fao74(sp("Ap", 20))))
  # the index of accumulation 65: (CEC 8.2 x 100 / clay - clay / 2) x thickness
  ix <- function(c82) .p74(top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 100),
                           designation = c("E", "Bhs", "C"), clay_pct = c(4, 10, 3),
                           silt_pct = 5, sand_pct = c(91, 85, 92), oc_pct = c(1, 1.5, 0.2),
                           fe_pyro_pct = c(NA, 1, NA), al_pyro_pct = c(NA, 1.5, NA),
                           fe_dcb_pct = c(NA, 2, NA), al_dcb_pct = c(NA, 2, NA),
                           cec_ph82_cmol = c(NA, c82, NA), ph_h2o = 4.5)
  expect_true(.ok(spodic_b_fao74(ix(1.16))))          # (11.6 - 5) x 10 = 66
  expect_false(.ok(spodic_b_fao74(ix(1.14))))         # 64
})

test_that("oxic B: 30 cm, retention and CEC per 100 g clay, traces, texture, boundaries", {
  skip_on_cran()
  ox <- function(t = 60, clay = 40, cec = 6, bases = 3.9, wm = 1, bnd = NA, two = FALSE, ...) {
    if (two) {
      base <- list(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "Bo1", "Bo2"),
                   boundary_distinctness = c("clear", bnd, NA))
    } else {
      base <- list(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 200),
                   designation = c("A", "Bo", "C"), boundary_distinctness = NA)
    }
    base <- c(base, list(clay_pct = clay, silt_pct = 10, sand_pct = 90 - clay, oc_pct = c(1.5, 0.5, 0.2),
                         cec_cmol = c(8, cec, cec), ca_cmol = bases / 2, mg_cmol = bases / 4,
                         k_cmol = bases / 8, na_cmol = bases / 8, al_kcl_cmol = 0,
                         weatherable_minerals_pct = wm, bs_pct = 50))
    a <- list(...); base[names(a)] <- a
    do.call(.p74, base)
  }
  expect_true(.ok(oxic_b_fao74(ox())))
  expect_true(.ok(oxic_b_fao74(ox(t = 30))))
  expect_false(.ok(oxic_b_fao74(ox(t = 29))))
  # bases + KCl Al below 10 me per 100 g clay (40 % clay: 4.0 is 10), or
  # NH4Cl retention 10 or less (ecec_cmol for it, flagged, Q12)
  expect_false(.ok(oxic_b_fao74(ox(bases = 4, ecec_cmol = 4.1))))
  r <- oxic_b_fao74(ox(bases = 4, ecec_cmol = 4))
  expect_true(.ok(r))
  expect_true("ecec_for_nh4cl_cec" %in% r$evidence$flags)
  # neither measured: undecided in 1974, so the USDA oxic horizon stands in
  r <- oxic_b_fao74(ox(bases = 4))
  expect_true(length(r$evidence$stand_in) == 1L)
  expect_true(r$evidence$lower_grade)
  # CEC (NH4OAc) 16 or less (6.4 is 16)
  expect_true(.ok(oxic_b_fao74(ox(cec = 6.4))))
  expect_false(.ok(oxic_b_fao74(ox(cec = 6.5))))
  # more than 15 % clay (bases and CEC scaled)
  expect_true(.ok(oxic_b_fao74(ox(clay = 16, cec = 2, bases = 1))))
  expect_false(.ok(oxic_b_fao74(ox(clay = 15, cec = 2, bases = 1))))
  # traces of primary aluminosilicates: 3 % or less (soilKey's number)
  r <- oxic_b_fao74(ox(wm = 3))
  expect_true(.ok(r))
  expect_true("traces_of_primary_minerals_read_as_3pct" %in% r$evidence$flags)
  expect_false(.ok(oxic_b_fao74(ox(wm = 3.1))))
  # internal boundaries mostly gradual or diffuse
  expect_true(.ok(oxic_b_fao74(ox(two = TRUE, bnd = "gradual"))))
  expect_false(.ok(oxic_b_fao74(ox(two = TRUE, bnd = "abrupt"))))
  r <- oxic_b_fao74(ox(two = TRUE, bnd = NA))
  expect_true(.ok(r))
  expect_true("oxic_internal_boundaries_not_recorded" %in% r$evidence$flags)
  # rock structure below 5 %
  expect_false(.ok(oxic_b_fao74(ox(rock_structure_pct = 5))))
  # NH4Cl retention 10 or less: criterion read with cec_method nh4cl
  p <- ox(cec = 4, bases = 5, cec_method = "nh4cl")
  crit <- oxic_b_fao74(p)$evidence
  x <- .f74_ctx(p)
  expect_true(.f74_hz_oxic(x)$evidence$criteria$retention[2])
  p <- ox(cec = 4.1, bases = 5, cec_method = "nh4cl")
  expect_false(.f74_hz_oxic(.f74_ctx(p))$evidence$criteria$retention[2])
  # the 1988 ferralic B asks water-dispersible clay and the silt/clay ratio;
  # the 1974 oxic B does not
  expect_true(is.na(.ok(ferralic_b_fao88(ox(weatherable_minerals_pct = 1)))) ||
                !isTRUE(.ok(ferralic_b_fao88(ox()))))
})

test_that("calcic and gypsic horizons: shared with 1988, the gypsic thickness strict", {
  skip_on_cran()
  ca <- function(t, cc) .p74(top_cm = c(0, 30, 30 + t), bottom_cm = c(30, 30 + t, 150),
                             designation = c("A", "Bk", "C"), caco3_pct = c(2, cc, 5),
                             clay_pct = 20, silt_pct = 40, sand_pct = 40)
  expect_true(.ok(calcic_fao74(ca(15, 15))))
  expect_false(.ok(calcic_fao74(ca(14, 15))))
  expect_false(.ok(calcic_fao74(ca(15, 14.9))))
  for (p in list(ca(15, 15), ca(14, 15), ca(15, 14.9)))
    expect_identical(.ok(calcic_fao74(p)), .ok(calcic_fao88(p)))
  gy <- function(t, g) .p74(top_cm = c(0, 30, 30 + t), bottom_cm = c(30, 30 + t, 150),
                            designation = c("A", "By", "C"), caso4_pct = c(0, g, 0),
                            clay_pct = 20, silt_pct = 40, sand_pct = 40)
  expect_true(.ok(gypsic_fao88(gy(15, 12))))       # 15 cm or more (1988)
  expect_false(.ok(gypsic_fao74(gy(15, 12))))      # more than 15 cm (1974)
  expect_true(.ok(gypsic_fao74(gy(16, 9.375))))    # 16 x 9.375 = 150
  expect_false(.ok(gypsic_fao74(gy(16, 9.3))))
})

test_that("sulfuric horizon: pH (1:1) below 3.5 and jarosite mottles (1974)", {
  skip_on_cran()
  su <- function(ph, jar = TRUE, t = 20, mott = NA)
    .p74(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 150), designation = c("A", "Bj", "Cg"),
         ph_h2o_1to1 = c(5, ph, 6), jarosite_present = c(NA, jar, NA), mottle_colors = c(NA, mott, NA),
         clay_pct = 30, silt_pct = 40, sand_pct = 30)
  expect_true(.ok(sulfuric_fao74(su(3.4))))
  expect_false(.ok(sulfuric_fao74(su(3.5))))
  # without jarosite: the 1988 horizon passes (Q18), 1974 does not
  r <- sulfuric_fao74(su(3.4, NA))
  expect_false(.ok(r))
  expect_true("jarosite_not_recorded_absent" %in% r$evidence$flags)
  expect_true(.ok(sulfuric_fao88(su(3.4, NA))))
  # jarosite read from a mottle colour of hue 2.5Y or yellower, chroma 6+
  expect_true(.ok(sulfuric_fao74(su(3.4, NA, mott = "2.5Y 7/6"))))
  expect_false(.ok(sulfuric_fao74(su(3.4, NA, mott = "2.5Y 7/5"))))
  expect_false(.ok(sulfuric_fao74(su(3.4, NA, mott = "10YR 6/8"))))
  # no thickness printed in 1974; 15 cm in 1988
  expect_true(.ok(sulfuric_fao74(su(3.4, t = 10))))
  expect_false(.ok(sulfuric_fao88(su(3.4, t = 10))))
})

test_that("albic E: no chroma limit at the middle values in 1974", {
  skip_on_cran()
  al <- function(vm, cm, vd) .lu74(munsell_value_moist = c(4, vm, 4, 5),
                                   munsell_chroma_moist = c(3, cm, 6, 4),
                                   munsell_value_dry = c(6, vd, 5, 6),
                                   munsell_chroma_dry = c(3, cm, 6, 4))
  p <- al(5, 3, 6)
  expect_false(.ok(albic_e_fao88(p)))
  expect_true(.ok(albic_e_fao74(p)))
  expect_true(.ok(albic_e_fao74(al(6, 3, 7))))
  expect_false(.ok(albic_e_fao74(al(6, 4, 7))))
  expect_false(.ok(albic_e_fao74(al(3, 2, 4))))       # too dark
})

test_that("stand-ins: tried only when the 1974 answer is undecided, USDA first, flagged", {
  skip_on_cran()
  # clay not recorded in the B: the 1974 argillic B is undecided
  p <- .lu74(clay_pct = c(12, 12, NA, 22))
  fake <- function(passed, name) function(pedon)
    DiagnosticResult$new(name = name, passed = passed, layers = if (isTRUE(passed)) 3L else integer(0),
                         evidence = list(), missing = character(0), reference = "test")
  testthat::local_mocked_bindings(argillic_usda = fake(TRUE, "argillic_usda"),
                                  argic = fake(FALSE, "argic"))
  r <- argillic_b_fao74(p)
  expect_true(.ok(r))
  expect_identical(r$evidence$stand_in, "argillic_usda")
  expect_true(r$evidence$lower_grade)
  expect_true("stand_in_argillic_usda" %in% r$evidence$flags)
  expect_true(length(r$evidence$undecided_1974) > 0L)
  # a decided 1974 answer is never replaced
  r <- argillic_b_fao74(.lu74(designation = c("A", "E", "B", "C"), clay_films_amount = NA))
  expect_false(.ok(r))
  expect_length(r$evidence$stand_in, 0L)
})

test_that("the USDA stand-in undecided: the WRB one is tried next", {
  skip_on_cran()
  p <- .lu74(clay_pct = c(12, 12, NA, 22))
  fake <- function(passed, name) function(pedon)
    DiagnosticResult$new(name = name, passed = passed, layers = integer(0),
                         evidence = list(), missing = character(0), reference = "test")
  testthat::local_mocked_bindings(argillic_usda = fake(NA, "argillic_usda"),
                                  argic = fake(FALSE, "argic"))
  r <- argillic_b_fao74(p)
  expect_false(.ok(r))
  expect_identical(r$evidence$stand_in, "argic")
})

test_that("the numbers of the 1974 table and the readings of this part are recorded", {
  skip_on_cran()
  f <- test_path("..", "..", "data-raw", "fao_thresholds_1974.yaml")
  fr <- test_path("..", "..", "data-raw", "fao_readings.yaml")
  skip_if_not(file.exists(f) && file.exists(fr))
  t74 <- yaml::read_yaml(f)
  mh <- t74$master_horizons
  expect_equal(.f74_h_line(0), mh$h_om_min_pct_at_clay_0)
  expect_equal(.f74_h_line(mh$h_clay_line_pct), mh$h_om_min_pct_at_clay_gt_60)
  expect_equal(.f74_h_line(100), mh$h_om_min_pct_at_clay_gt_60)
  hz <- t74$horizons
  expect_equal(hz$histic_h$om_min_pct_at_clay_0, 14)
  expect_equal(hz$histic_h$om_min_pct_at_clay_gt_60, 28)
  expect_equal(hz$mollic_a$om_min_pct, 1)
  expect_equal(hz$mollic_a$om_min_pct_lime_waiver, 4)
  expect_equal(.f74_ochric_limit(1, hz$ochric_a$very_weak_om_lt_pct_at_sand_clay_1,
                                 hz$ochric_a$very_weak_om_lt_pct_at_sand_clay_13), 1)
  expect_equal(.f74_ochric_limit(13, 1, 0.5), hz$ochric_a$very_weak_om_lt_pct_at_sand_clay_13)
  expect_equal(.f74_ochric_limit(7, 1, 0.5), 0.75)
  expect_equal(hz$oxic_b$primary_minerals_traces_max_pct$value, 3)
  expect_true(hz$gypsic$thickness_gt_cm == 15)
  y <- yaml::read_yaml(fr)
  r387 <- Filter(function(r) identical(r$version, "0.9.387"), y$readings)
  expect_gte(length(r387), 10L)
  expect_true(all(vapply(r387, function(r) identical(r$edition, 1974L), NA)))
  expect_true(all(grepl("for Hugo to confirm|\\(Hugo, 2026-11-0[89]\\)\\.$",
                        vapply(r387, `[[`, "", "reading"))))
})
