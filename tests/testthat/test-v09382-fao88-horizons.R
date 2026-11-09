# v0.9.382: the FAO-Unesco legend, part 2: the 16 diagnostic horizons of the
# Revised Legend of 1988 (ch. IV, pp. 25-33) as three-valued predicates
# <name>_fao88(), R/diagnostics-fao88-horizons.R. A pass and a near miss per
# horizon and the printed edges of plan section 5.1.

.p88 <- function(..., site = list(id = "p")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}
.ok <- function(res) res$passed

# A sandy loam A and E over a Bt with clay films, over a C.
.luvic88 <- function(...) {
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
  do.call(.p88, base)
}

test_that("the sixteen predicates exist and return DiagnosticResults of the 1988 legend", {
  skip_on_cran()
  ids <- c("histic_h", "mollic_a", "fimic_a", "umbric_a", "ochric_a", "argic_b", "natric_b",
           "cambic_b", "spodic_b", "ferralic_b", "calcic", "petrocalcic", "gypsic",
           "petrogypsic", "sulfuric", "albic_e")
  expect_identical(.F88_HZ$id, ids)
  p <- .luvic88()
  for (id in ids) {
    r <- get(paste0(id, "_fao88"))(p)
    expect_s3_class(r, "DiagnosticResult")
    expect_identical(r$name, paste0(id, "_fao88"))
    expect_true(is.logical(r$passed) && length(r$passed) == 1L)
    expect_match(r$reference, "Revised Legend")
    expect_identical(r$evidence$edition, "1988")
  }
  src <- test_path("..", "..", "R", "diagnostics-fao88-horizons.R")
  skip_if_not(file.exists(src))
  src <- readLines(src, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", src, perl = TRUE)))
})

test_that("histic H: H horizons, the thin surface layer and the 8-16 % carbon line", {
  skip_on_cran()
  wet <- list(id = "w", drainage_class = "poorly drained")
  p <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("H", "Cg"),
            oc_pct = c(25, 1), clay_pct = c(NA, 20), silt_pct = c(NA, 40), sand_pct = c(NA, 40))
  expect_true(.ok(histic_h_fao88(p)))
  # more than 20 cm: exactly 20 at a depth fails (the thin path is for the surface)
  p <- .p88(top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 100), designation = c("A", "H", "C"),
            oc_pct = c(2, 30, 1), clay_pct = c(20, NA, 20), silt_pct = 40, sand_pct = 40)
  expect_false(.ok(histic_h_fao88(p)))
  p <- .p88(top_cm = c(0, 30, 51), bottom_cm = c(30, 51, 100), designation = c("A", "H", "C"),
            oc_pct = c(2, 30, 1), clay_pct = c(20, NA, 20), silt_pct = 40, sand_pct = 40)
  expect_true(.ok(histic_h_fao88(p)))
  # upper boundary within 40 cm
  p <- .p88(top_cm = c(0, 41, 80), bottom_cm = c(41, 80, 120), designation = c("A", "H", "C"),
            oc_pct = c(2, 30, 1), clay_pct = c(20, NA, 20), silt_pct = 40, sand_pct = 40)
  expect_false(.ok(histic_h_fao88(p)))
  # the thin surface layer mixed to 25 cm: OC >= 8 + 8 x clay / 60
  thin <- function(oc, clay) .p88(top_cm = c(0, 10), bottom_cm = c(10, 60), designation = c("H", "C"),
                                  oc_pct = c(oc, 1), clay_pct = c(clay, clay),
                                  silt_pct = 30, sand_pct = 100 - 30 - clay)
  expect_true(.ok(histic_h_fao88(thin(30, 30))))      # (300 + 15) / 25 = 12.6 >= 12
  expect_false(.ok(histic_h_fao88(thin(28, 30))))     # 11.8 < 12
  expect_true(.ok(histic_h_fao88(thin(38.5, 65))))    # 16 at more than 60 % clay
  expect_false(.ok(histic_h_fao88(thin(38, 65))))     # 15.8
  expect_true(.ok(histic_h_fao88(thin(20, 0))))       # (200 + 15) / 25 = 8.6 >= 8
  r <- histic_h_fao88(thin(25, NA))                   # 10.6, no clay: between 8 and 16
  expect_true(is.na(r$passed)); expect_true("clay_pct" %in% r$missing)
  # a plough layer 25 cm or more thick, mixed to 25 cm
  ap <- function(oc) .p88(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Ap", "C"),
                          oc_pct = c(oc, 1), clay_pct = 30, silt_pct = 30, sand_pct = 40)
  expect_true(.ok(histic_h_fao88(ap(12))))
  expect_false(.ok(histic_h_fao88(ap(11.9))))
  # organic soil materials (Q15): 20 % passes; between the saturated limit and
  # 20 % the drainage decides
  om <- function(oc, site = list(id = "o")) .p88(top_cm = c(0, 30), bottom_cm = c(30, 80),
                                                 designation = c("H", "C"), oc_pct = c(oc, 1),
                                                 clay_pct = 20, silt_pct = 40, sand_pct = 40, site = site)
  expect_true(is.na(.ok(histic_h_fao88(om(15)))))
  expect_true(.ok(histic_h_fao88(om(15, wet))))
  expect_false(.ok(histic_h_fao88(om(15, list(id = "d", drainage_class = "well drained")))))
  expect_false(.ok(histic_h_fao88(om(13.9, wet))))   # below 12 + 0.1 x 20
  # eutric / dystric by pH (H2O 1:5) 5.5 (Q15)
  p <- .p88(top_cm = c(0, 15, 30), bottom_cm = c(15, 30, 100), designation = c("H1", "H2", "C"),
            oc_pct = c(30, 30, 1), clay_pct = c(NA, NA, 20), silt_pct = 40, sand_pct = 40,
            ph_h2o_1to5 = c(5.6, 5.4, 6))
  expect_false(.ok(histic_h_fao88(p, "eutric")))
  expect_true(.ok(histic_h_fao88(p, "dystric")))
})

# A dark A over a B and a C; the solum is the A and the B.
.mollic88 <- function(a_bot = 30, b_bot = 80, ...) {
  base <- list(top_cm = c(0, a_bot, b_bot), bottom_cm = c(a_bot, b_bot, 150),
               designation = c("A", "Bw", "C"), clay_pct = 30, silt_pct = 40, sand_pct = 30,
               oc_pct = c(2.5, 0.8, 0.3), bs_pct = c(80, 85, 90), cec_cmol = c(25, 20, 18),
               munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 5),
               munsell_chroma_moist = c(2, 3, 4), munsell_value_dry = c(4, 6, 7),
               munsell_chroma_dry = c(2, 3, 4), structure_type = c("granular", "subangular blocky", "massive"),
               structure_grade = c("moderate", "moderate", NA))
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("mollic and umbric A: colour, carbon, base saturation and the solum rule", {
  skip_on_cran()
  expect_true(.ok(mollic_a_fao88(.mollic88())))
  expect_false(.ok(umbric_a_fao88(.mollic88())))
  expect_false(.ok(ochric_a_fao88(.mollic88())))
  # base saturation below 50 %: umbric
  p <- .mollic88(bs_pct = c(45, 40, 40))
  expect_false(.ok(mollic_a_fao88(p))); expect_true(.ok(umbric_a_fao88(p)))
  # value moist below 3.5: a 3.5 fails (Q24), a 4 fails
  expect_false(.ok(mollic_a_fao88(.mollic88(munsell_value_moist = c(4, 4, 5), caco3_pct = 0))))
  # ... without CaCO3 the lime waiver (value 5 or less with 2.5 % OC) is open
  expect_true(is.na(.ok(mollic_a_fao88(.mollic88(munsell_value_moist = c(4, 4, 5))))))
  expect_true(.ok(mollic_a_fao88(.mollic88(munsell_value_moist = c(3, 4, 5)))))
  # the colour contrast with the C: one unit
  expect_false(.ok(mollic_a_fao88(.mollic88(munsell_value_moist = c(3, 4, 3.5), oc_pct = c(1, 0.3, 0.2)))))
  # ... waived when strongly humic (weighted OC over 0-100 cm above 1.4 %)
  expect_true(.ok(mollic_a_fao88(.mollic88(munsell_value_moist = c(3, 4, 3.5), oc_pct = c(3, 1, 0.3)))))
  # organic carbon 0.6 %
  expect_false(.ok(mollic_a_fao88(.mollic88(oc_pct = c(0.59, 0.4, 0.2)))))
  # the solum rule: solum < 75 cm, 18 cm and more than a third; 75 cm or more,
  # more than 25 cm (exactly 75 on the stricter side, default 2)
  expect_false(.ok(mollic_a_fao88(.mollic88(a_bot = 25, b_bot = 75))))
  expect_true(.ok(mollic_a_fao88(.mollic88(a_bot = 25, b_bot = 74))))
  expect_true(.ok(mollic_a_fao88(.mollic88(a_bot = 26, b_bot = 75))))
  expect_false(.ok(mollic_a_fao88(.mollic88(a_bot = 24, b_bot = 74))))   # 24 < 74 / 3
  expect_false(.ok(mollic_a_fao88(.mollic88(a_bot = 17, b_bot = 40))))   # under 18 cm
  # 10 cm on hard rock
  rk <- function(t) .p88(top_cm = c(0, t), bottom_cm = c(t, 60), designation = c("A", "R"),
                         clay_pct = c(30, NA), silt_pct = c(40, NA), sand_pct = c(30, NA),
                         oc_pct = c(3, NA), bs_pct = c(80, NA), munsell_hue_moist = c("10YR", NA),
                         munsell_value_moist = c(2, NA), munsell_chroma_moist = c(2, NA),
                         munsell_value_dry = c(3, NA))
  r <- mollic_a_fao88(rk(10))
  expect_true(.ok(r)); expect_true("rock_below_a_contrast_not_read" %in% r$evidence$flags)
  expect_false(.ok(mollic_a_fao88(rk(9))))
  # more than 40 % finely divided lime: value moist 5 or less and 2.5 % OC
  lime <- function(oc) .mollic88(caco3_pct = c(45, 50, 60), munsell_value_moist = c(5, 6, 7),
                                 munsell_value_dry = c(7, 8, 8), oc_pct = c(oc, 2, 0.2))
  expect_true(.ok(mollic_a_fao88(lime(3))))
  expect_false(.ok(mollic_a_fao88(lime(2.4))))
  # massive and hard when dry
  expect_false(.ok(mollic_a_fao88(.mollic88(structure_type = c("massive", "subangular blocky", "massive"),
                                            rupture_resistance = c("hard", NA, NA)))))
  # the dry value from the moist one + 1, flagged
  r <- mollic_a_fao88(.mollic88(munsell_value_dry = NA))
  expect_true(.ok(r)); expect_true("dry_value_from_moist_plus_1" %in% r$evidence$flags)
})

test_that("mollic A: citric P2O5 and the anthropic layers it targets (Q14)", {
  skip_on_cran()
  r <- mollic_a_fao88(.mollic88())
  expect_true("p2o5_not_recorded_no_anthropic_layer" %in% r$evidence$flags)
  expect_true(.ok(mollic_a_fao88(.mollic88(p2o5_citric_mg_kg = c(249, 100, 50)))))
  expect_false(.ok(mollic_a_fao88(.mollic88(p2o5_citric_mg_kg = c(250, 100, 50)))))
  # not when it increases below the A
  expect_true(.ok(mollic_a_fao88(.mollic88(p2o5_citric_mg_kg = c(300, 400, 50)))))
  r <- mollic_a_fao88(.mollic88(artefacts_pct = c(3, 0, 0)))
  expect_false(.ok(r)); expect_true("p2o5_exclusion_from_anthropic_record" %in% r$evidence$flags)
})

test_that("fimic A: 50 cm of manured surface layers; the WRB stand-ins only make it pass", {
  skip_on_cran()
  f <- function(b, ...) .p88(top_cm = c(0, b), bottom_cm = c(b, 120), designation = c("Ap", "C"),
                              clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = c(2, 0.3), ...)
  expect_true(.ok(fimic_a_fao88(f(50, artefacts_pct = c(2, 0)))))
  expect_false(.ok(fimic_a_fao88(f(49, artefacts_pct = c(2, 0)))))
  expect_true(.ok(fimic_a_fao88(f(55, p2o5_citric_mg_kg = c(1200, 50)))))
  expect_false(.ok(fimic_a_fao88(f(55, p2o5_citric_mg_kg = c(250, 50)))))
  r <- fimic_a_fao88(f(60))
  expect_false(.ok(r)); expect_true("anthropic_layer_not_recorded_absent" %in% r$evidence$flags)
  local_mocked_bindings(plaggic = function(pedon, ...) DiagnosticResult$new(
    name = "plaggic", passed = TRUE, layers = 1L))
  r <- fimic_a_fao88(f(60))
  expect_true(.ok(r))
  expect_identical(r$evidence$stand_in, "plaggic")
  expect_true(r$evidence$lower_grade)
})

test_that("ochric A: an A that is not mollic or umbric; fresh stratified alluvium is not", {
  skip_on_cran()
  p <- .luvic88()
  expect_true(.ok(ochric_a_fao88(p)))
  expect_false(.ok(mollic_a_fao88(p)))
  p <- .luvic88(stratification_pattern = c("continuous", NA, NA, NA),
                recent_deposit = c(TRUE, NA, NA, NA))
  expect_false(.ok(ochric_a_fao88(p)))
  p <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Bw", "C"),
            clay_pct = 20, silt_pct = 40, sand_pct = 40)
  expect_false(.ok(ochric_a_fao88(p)))
})

# A coarse A and BA over a B; `skins` sets the clay films of the B.
.argic88 <- function(ba_bot = 45, b_clay = 14.5, skins = "common", desg = "Bt", ...) {
  base <- list(top_cm = c(0, 20, ba_bot, 120), bottom_cm = c(20, ba_bot, 120, 160),
               designation = c("A", "BA", desg, "C"), clay_pct = c(10, 12, b_clay, 12),
               silt_pct = c(30, 30, 30, 30), sand_pct = c(60, 58, 100 - 30 - b_clay, 58),
               cec_cmol = c(6, 6, 6, 4), clay_films_amount = c(NA, NA, skins, NA))
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("argic B: the increase within 30 cm with clay skins, 15 cm without (Q13)", {
  skip_on_cran()
  # the B (14.5 %) reaches +3 over the A (10 %) 25 cm below it; +2.5 over the BA
  expect_true(.ok(argic_b_fao88(.argic88())))
  expect_false(.ok(argic_b_fao88(.argic88(skins = "none", desg = "Bw"))))
  expect_false(.ok(argic_b_fao88(.argic88(ba_bot = 51))))          # 31 cm
  expect_true(.ok(argic_b_fao88(.argic88(ba_bot = 50))))           # 30 cm
  # without skins: 15 cm, and the overlying coarser horizon 18 cm or more
  expect_true(.ok(argic_b_fao88(.argic88(ba_bot = 35, skins = NA, desg = "Bw"))))
  expect_false(.ok(argic_b_fao88(.argic88(ba_bot = 36, skins = NA, desg = "Bw"))))
  # a recorded Bt counts as the clay-skin observation, flagged
  r <- argic_b_fao88(.argic88(skins = NA, desg = "Bt"))
  expect_true(.ok(r)); expect_true("bt_designation_for_clay_skins" %in% r$evidence$flags)
  # "few" clay films = 1 %
  expect_true(.ok(argic_b_fao88(.argic88(skins = "few", desg = "Bw"))))
  expect_true(.ok(argic_b_fao88(.argic88(skins = NA, desg = "Bw", micromorph_argillans_pct = c(NA, NA, 1, NA)))))
  # the clay bands 3 / 1.2 / 8 (breaks 15 and 40)
  inc <- function(a, b) .p88(top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "Bt"),
                             clay_pct = c(a, b), silt_pct = 30, sand_pct = 100 - 30 - c(a, b),
                             cec_cmol = 10, clay_films_amount = c(NA, "common"))
  expect_true(.ok(argic_b_fao88(inc(14, 17)))); expect_false(.ok(argic_b_fao88(inc(14, 16.9))))
  expect_true(.ok(argic_b_fao88(inc(30, 36)))); expect_false(.ok(argic_b_fao88(inc(30, 35.9))))
  expect_true(.ok(argic_b_fao88(inc(42, 50)))); expect_false(.ok(argic_b_fao88(inc(42, 49.9))))
})

test_that("argic B: thickness 7.5 cm and one tenth, the 18 / 5 cm rule, plough layers, discontinuities", {
  skip_on_cran()
  th <- function(top, bot) .p88(top_cm = c(0, top, bot), bottom_cm = c(top, bot, bot + 50),
                                designation = c("A", "Bt", "C"), clay_pct = c(10, 20, 10),
                                silt_pct = 30, sand_pct = c(60, 50, 60), cec_cmol = 8,
                                clay_films_amount = c(NA, "common", NA))
  expect_true(.ok(argic_b_fao88(th(30, 37.5))))
  expect_false(.ok(argic_b_fao88(th(30, 37))))
  expect_true(.ok(argic_b_fao88(th(100, 110))))      # one tenth of 100 cm
  expect_false(.ok(argic_b_fao88(th(100, 109))))
  # lamellae: 15 cm combined
  lam <- function(l) .p88(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 130), designation = c("A", "E&Bt", "C"),
                          clay_pct = c(5, 9, 5), silt_pct = 30, sand_pct = c(65, 61, 65), cec_cmol = 4,
                          clay_films_amount = c(NA, "common", NA), lamellae_cumulative_cm = c(NA, l, NA))
  expect_true(.ok(argic_b_fao88(lam(15)))); expect_false(.ok(argic_b_fao88(lam(14))))
  # no skins: the coarser horizon above 18 cm, or 5 cm over an abrupt transition
  ns <- function(a, bd = NA) .p88(top_cm = c(0, a), bottom_cm = c(a, 80), designation = c("A", "Bw"),
                                  clay_pct = c(10, 20), silt_pct = 30, sand_pct = c(60, 50), cec_cmol = 8,
                                  boundary_distinctness = c(bd, NA))
  expect_true(.ok(argic_b_fao88(ns(18))))
  expect_false(.ok(argic_b_fao88(ns(17))))
  expect_true(.ok(argic_b_fao88(ns(5, "abrupt"))))
  expect_false(.ok(argic_b_fao88(ns(4.9, "abrupt"))))
  expect_true(.ok(argic_b_fao88(ns(5, "clear"))))             # less than 5 cm (default 5)
  expect_false(.ok(argic_b_fao88(ns(5, "gradual"))))
  # under a plough layer only, clay skins are required
  pl <- function(sk) .p88(top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("Ap", "Bw"),
                          clay_pct = c(10, 20), silt_pct = 30, sand_pct = c(60, 50), cec_cmol = 8,
                          clay_films_amount = c(NA, sk))
  expect_false(.ok(argic_b_fao88(pl(NA))))
  expect_true(.ok(argic_b_fao88(pl("common"))))
  # across a lithological discontinuity the increase is undecided
  r <- argic_b_fao88(.p88(top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "2Bt"),
                          clay_pct = c(10, 25), silt_pct = 30, sand_pct = c(60, 45), cec_cmol = 8,
                          clay_films_amount = c(NA, "common")))
  expect_true(is.na(r$passed))
  # sandy loam or finer and 8 % clay
  expect_false(.ok(argic_b_fao88(.p88(top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "Bt"),
                                      clay_pct = c(4, 7.9), silt_pct = 30, sand_pct = c(66, 62.1),
                                      cec_cmol = 4, clay_films_amount = c(NA, "common")))))
  # upper limit within 125 cm (p. 42); buried under 50 cm of new material
  deep <- .p88(top_cm = c(0, 100, 130), bottom_cm = c(100, 130, 180), designation = c("C1", "C2", "Bt"),
               clay_pct = c(10, 10, 25), silt_pct = 30, sand_pct = c(60, 60, 45), cec_cmol = 10,
               clay_films_amount = c(NA, NA, "common"))
  expect_false(.ok(argic_b_fao88(deep)))
  expect_true(.ok(argic_b_fao88(deep, max_top_cm = Inf)))
  bur <- .p88(top_cm = c(0, 55, 75), bottom_cm = c(55, 75, 120), designation = c("C", "Ab", "Btb"),
              clay_pct = c(10, 10, 25), silt_pct = 30, sand_pct = c(60, 60, 45), cec_cmol = 10,
              clay_films_amount = c(NA, NA, "common"))
  expect_false(.ok(argic_b_fao88(bur)))
})

# An A over a columnar B with exchangeable sodium.
.natric88 <- function(esp = c(5, 25, 30, 20), ...) {
  base <- list(top_cm = c(0, 15, 45, 90), bottom_cm = c(15, 45, 90, 150),
               designation = c("A", "Btn1", "Btn2", "C"), clay_pct = c(15, 35, 32, 28),
               silt_pct = 30, sand_pct = c(55, 35, 38, 42), cec_cmol = c(10, 20, 18, 15),
               esp_pct = esp, clay_films_amount = c(NA, "common", "few", NA),
               structure_type = c("platy", "columnar", "prismatic", "massive"))
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("natric B: columnar or prismatic structure and ESP above 15 % in its upper 40 cm", {
  skip_on_cran()
  p <- .natric88()
  expect_true(.ok(natric_b_fao88(p)))
  expect_false(.ok(argic_b_fao88(p)))
  # "more than 15 %": 15 fails (the Mg + Na path is then undecided without
  # the acidity at pH 8.2 when ESP > 15 is found deeper)
  p <- .natric88(esp = c(5, 15, 15, 15))
  expect_false(.ok(natric_b_fao88(p)))
  expect_true(.ok(argic_b_fao88(p)))
  # Mg + Na above Ca + acidity (pH 8.2) in the upper 40 cm, ESP > 15 within 200 cm
  mg <- function(acid) .natric88(esp = c(5, 10, 12, 20), mg_cmol = c(2, 8, 8, 8),
                                 na_cmol = c(0.5, 2, 2, 3), ca_cmol = c(5, 6, 6, 6),
                                 extractable_acidity_ph82_cmol = c(1, acid, acid, acid))
  expect_true(.ok(natric_b_fao88(mg(3))))
  expect_false(.ok(natric_b_fao88(mg(4))))
  r <- natric_b_fao88(mg(NA))
  expect_true(is.na(r$passed)); expect_true("extractable_acidity_ph82_cmol" %in% r$missing)
  # without columnar or prismatic structure (or tongues), not natric
  expect_false(.ok(natric_b_fao88(.natric88(structure_type = c("platy", "subangular blocky",
                                                                "subangular blocky", "massive")))))
  expect_true(.ok(natric_b_fao88(.natric88(structure_type = c("platy", "subangular blocky", "subangular blocky", "massive"),
                                           tongue_depth_cm = c(3, NA, NA, NA)))))
})

# An A over a Bw over a C: the Bw stronger in chroma than the C.
.cambic88 <- function(b_top = 20, b_bot = 50, ...) {
  base <- list(top_cm = c(0, b_top, b_bot), bottom_cm = c(b_top, b_bot, 120),
               designation = c("A", "Bw", "C"), clay_pct = c(18, 20, 18), silt_pct = 40,
               sand_pct = c(42, 40, 42), oc_pct = c(1, 0.5, 0.2), cec_cmol = c(8, 6, 5),
               bs_pct = c(40, 40, 50), ph_h2o = c(5.5, 5.5, 5.8),
               munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5),
               munsell_chroma_moist = c(3, 6, 4), munsell_value_dry = c(6, 6, 7),
               structure_type = c("granular", "subangular blocky", "massive"),
               structure_grade = c("weak", "moderate", NA),
               fe_pyro_pct = 0.05, al_pyro_pct = 0.05)   # no spodic B
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("cambic B: thickness 15 cm with its base at 25 cm, non-ferralic, alteration", {
  skip_on_cran()
  expect_true(.ok(cambic_b_fao88(.cambic88())))
  expect_false(.ok(cambic_b_fao88(.cambic88(b_top = 20, b_bot = 34))))   # 14 cm
  expect_true(.ok(cambic_b_fao88(.cambic88(b_top = 10, b_bot = 25))))
  expect_false(.ok(cambic_b_fao88(.cambic88(b_top = 9, b_bot = 24))))    # base above 25 cm
  # no alteration against the C: same chroma, hue and clay, and carbonates
  # not read
  r <- cambic_b_fao88(.cambic88(munsell_chroma_moist = c(3, 4, 4), clay_pct = c(18, 18, 18),
                                sand_pct = c(42, 42, 42), ph_h2o = NA))
  expect_true(is.na(r$passed))
  # ... a water pH below 6.5 reads as no carbonate, flagged
  expect_true(.ok(cambic_b_fao88(.cambic88(munsell_chroma_moist = c(3, 4, 4), clay_pct = c(18, 18, 18),
                                           sand_pct = c(42, 42, 42)))))
  # ... with the C free of carbonate, structure suffices (item 5c)
  expect_true(.ok(cambic_b_fao88(.cambic88(munsell_chroma_moist = c(3, 4, 4), clay_pct = c(18, 18, 18),
                                           sand_pct = c(42, 42, 42), caco3_pct = c(0, 0, 0)))))
  # ferralic in every way (CEC <= 16 per kg clay, silt/clay <= 0.2, WM, WDC): not cambic
  r <- cambic_b_fao88(.cambic88(silt_pct = c(5, 3, 5), sand_pct = c(77, 77, 77), cec_cmol = c(3, 3, 3),
                                weatherable_minerals_pct = 2, water_dispersible_clay_pct = 1))
  expect_false(.ok(r))
  # cemented: not cambic
  expect_false(.ok(cambic_b_fao88(.cambic88(cementation_class = c(NA, "strongly", NA)))))
})

# A podzol: Ah, E, a Bhs with the pyrophosphate chemistry, C.
.spodic88 <- function(c82 = c(10, 2, 15, 1), ...) {
  base <- list(top_cm = c(0, 10, 30, 80), bottom_cm = c(10, 30, 43, 150),
               designation = c("Ah", "E", "Bhs", "C"), clay_pct = c(5, 3, 5, 4),
               silt_pct = c(10, 8, 8, 6), sand_pct = c(85, 89, 87, 90), oc_pct = c(3, 0.5, 3, 0.2),
               fe_pyro_pct = c(0.1, 0.05, 0.8, 0.05), al_pyro_pct = c(0.1, 0.05, 0.6, 0.05),
               fe_dcb_pct = c(0.2, 0.1, 1, 0.2), al_dcb_pct = c(0.1, 0.1, 0.8, 0.1),
               cec_ph82_cmol = c82, ph_h2o = c(4, 4.2, 4.5, 5))
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("spodic B: the pyrophosphate chemistry of 1975 and the index of 65 (Q17)", {
  skip_on_cran()
  expect_true(.ok(spodic_b_fao88(.spodic88())))
  # index = (CEC 8.2 per kg clay - clay / 2) x thickness: (0.375 x 20 - 2.5) x 13 = 65
  expect_true(.ok(spodic_b_fao88(.spodic88(c82 = c(10, 2, 0.375, 1)))))
  expect_false(.ok(spodic_b_fao88(.spodic88(c82 = c(10, 2, 0.37, 1)))))
  # (Fe + Al)pyro at least half of (Fe + Al)dithionite
  expect_true(.ok(spodic_b_fao88(.spodic88(fe_dcb_pct = c(0.2, 0.1, 2, 0.2)))))     # 1.4 = (2 + 0.8) / 2
  expect_false(.ok(spodic_b_fao88(.spodic88(fe_dcb_pct = c(0.2, 0.1, 2.1, 0.2)))))
  # with pyrophosphate Fe below 0.1 %, (Al pyro + OC) / clay
  expect_true(.ok(spodic_b_fao88(.spodic88(fe_pyro_pct = c(0.1, 0.05, 0.05, 0.05),
                                           fe_dcb_pct = c(0.2, 0.1, 0.1, 0.2), al_dcb_pct = c(0.1, 0.1, 0.6, 0.1)))))
  # cemented by organic matter with Fe / Al, more than 2.5 cm
  p <- .p88(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100), designation = c("E", "Bhsm", "C"),
            clay_pct = 3, silt_pct = 7, sand_pct = 90, cementation_class = c(NA, "strongly", NA))
  expect_true(.ok(spodic_b_fao88(p)))
  # below 12.5 cm when there is no A or E above
  p <- .p88(top_cm = c(0, 10, 60), bottom_cm = c(10, 60, 120), designation = c("Oa", "Bhsm", "C"),
            clay_pct = 3, silt_pct = 7, sand_pct = 90, cementation_class = c(NA, "strongly", NA))
  expect_false(.ok(spodic_b_fao88(p)))
  # without the 1975 data the soilKey spodic readers stand in, flagged
  sw <- .p88(top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 150),
             designation = c("Ah", "E", "Bhs", "C"), clay_pct = c(5, 3, 5, 4),
             silt_pct = c(10, 8, 8, 6), sand_pct = c(85, 89, 87, 90), oc_pct = c(3, 0.5, 2, 0.2),
             al_ox_pct = c(0.1, 0.05, 0.9, 0.1), fe_ox_pct = c(0.1, 0.05, 0.6, 0.1),
             ph_h2o = c(4, 4.2, 4.5, 5), munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"),
             munsell_value_moist = c(3, 6, 3, 6), munsell_chroma_moist = c(1, 1, 4, 3),
             munsell_value_dry = c(4, 7, 4, 7), munsell_chroma_dry = c(1, 1, 4, 3))
  r <- spodic_b_fao88(sw)
  expect_identical(r$passed, spodic_horizon_usda(sw)$passed)
  expect_true("spodic_horizon_usda" %in% r$evidence$stand_in)
  expect_true("stand_in_for_spodic_chemistry_1975" %in% r$evidence$flags)
  expect_true(r$evidence$lower_grade)
  # a calcareous layer or one of pH 7 or more holds no spodic B (soilKey's reading)
  r <- spodic_b_fao88(.mollic88(ph_h2o = c(7.5, 7.8, 8)))
  expect_false(.ok(r)); expect_true("not_spodic_calcareous_or_ph_7" %in% r$evidence$flags)
})

# A Ferralsol: low-activity clay in a deep Bw.
.ferralic88 <- function(cec = c(8, 5, 4, 4), b2_bot = 120, ...) {
  base <- list(top_cm = c(0, 20, 60, b2_bot), bottom_cm = c(20, 60, b2_bot, 200),
               designation = c("A", "Bw1", "Bw2", "Bw3"), clay_pct = c(50, 60, 62, 60),
               silt_pct = c(8, 7, 6, 6), sand_pct = c(42, 33, 32, 34), oc_pct = c(2, 0.8, 0.5, 0.3),
               cec_cmol = cec, bs_pct = c(20, 10, 8, 8), ph_h2o = c(5, 4.8, 4.9, 5),
               weatherable_minerals_pct = c(2, 1, 1, 1), water_dispersible_clay_pct = c(15, 2, 0, 0),
               al_ox_pct = 0.2, fe_ox_pct = 0.4)
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, base)
}

test_that("ferralic B: CEC 16 or ECEC 12 per kg clay, 30 cm, 8 % clay, silt/clay, WDC (Q16)", {
  skip_on_cran()
  r <- ferralic_b_fao88(.ferralic88())
  expect_true(.ok(r))
  expect_true("weatherable_minerals_20_200um_for_50_200um" %in% r$evidence$flags)
  expect_true("volcanic_glass_not_recorded_none" %in% r$evidence$flags)
  # CEC 16 or less per kg clay: 9.6 cmol at 60 % clay is exactly 16
  one <- function(cec, ecec = NA, ...) .p88(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
                                            designation = c("A", "Bw", "C"), clay_pct = c(50, 60, 60),
                                            silt_pct = c(8, 6, 6), sand_pct = c(42, 34, 34),
                                            cec_cmol = c(10, cec, 10), ecec_cmol = c(NA, ecec, NA),
                                            weatherable_minerals_pct = 1, water_dispersible_clay_pct = 0,
                                            al_ox_pct = 0.2, fe_ox_pct = 0.4, ...)
  expect_true(.ok(ferralic_b_fao88(one(9.6))))
  r <- ferralic_b_fao88(one(9.7))                   # 16.2: the ECEC would decide
  expect_true(is.na(r$passed)); expect_true("exch_acidity_kcl_cmol" %in% r$missing)
  # ... or ECEC 12 or less (as recorded, flagged)
  r <- ferralic_b_fao88(one(9.7, 7.2))
  expect_true(.ok(r)); expect_true("ecec_as_recorded" %in% r$evidence$flags)
  expect_false(.ok(ferralic_b_fao88(one(9.7, 7.3))))
  # bases + 1 M KCl acidity (p. 31 item 3)
  expect_true(.ok(ferralic_b_fao88(one(9.7, ca_cmol = c(1, 4, 1), mg_cmol = c(1, 1.5, 1), k_cmol = c(0.1, 0.1, 0.1),
                                       na_cmol = c(0.1, 0.1, 0.1), exch_acidity_kcl_cmol = c(1, 1.5, 1)))))
  # 30 cm
  expect_true(.ok(ferralic_b_fao88(.p88(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
                                        designation = c("A", "Bw", "C"), clay_pct = 60, silt_pct = 6,
                                        sand_pct = 34, cec_cmol = c(10, 5, 10), weatherable_minerals_pct = 1,
                                        water_dispersible_clay_pct = 0, al_ox_pct = 0.2, fe_ox_pct = 0.4))))
  expect_false(.ok(ferralic_b_fao88(.p88(top_cm = c(0, 20, 49), bottom_cm = c(20, 49, 100),
                                         designation = c("A", "Bw", "C"), clay_pct = 60, silt_pct = 6,
                                         sand_pct = 34, cec_cmol = c(10, 5, 10), weatherable_minerals_pct = 1,
                                         water_dispersible_clay_pct = 0, al_ox_pct = 0.2, fe_ox_pct = 0.4))))
  # silt/clay 0.2 or less
  expect_false(.ok(ferralic_b_fao88(.ferralic88(silt_pct = c(8, 13, 13, 13), sand_pct = c(42, 27, 25, 27)))))
  # 8 % clay (Q16)
  expect_false(.ok(ferralic_b_fao88(.ferralic88(clay_pct = c(7, 7.9, 7.9, 7.9), silt_pct = 1,
                                                sand_pct = c(92, 91.1, 91.1, 91.1), cec_cmol = 1))))
  # water-dispersible clay not recorded: undecided
  r <- ferralic_b_fao88(.ferralic88(water_dispersible_clay_pct = NA))
  expect_true(is.na(r$passed)); expect_true("water_dispersible_clay_pct" %in% r$missing)
  # an argic increase in a ferralic B is not an argic B
  expect_false(.ok(argic_b_fao88(.ferralic88())))
})

test_that("calcic and petrocalcic horizons", {
  skip_on_cran()
  ca <- function(ck = 25, c = 10, thk = 30, desg = "Bk", ...) .p88(top_cm = c(0, 20, 20 + thk),
                                                                    bottom_cm = c(20, 20 + thk, 150),
                                                                    designation = c("A", desg, "C"),
                                                                    caco3_pct = c(2, ck, c), ...)
  expect_true(.ok(calcic_fao88(ca())))
  expect_false(.ok(calcic_fao88(ca(ck = 14.9))))
  expect_false(.ok(calcic_fao88(ca(ck = 19.4, c = 14.5))))      # less than 5 more than below
  expect_true(.ok(calcic_fao88(ca(ck = 19.5, c = 14.5))))
  # the deepest layer of 15 % or more has no deeper layer to compare with
  expect_true(is.na(calcic_fao88(ca(ck = 20, c = 15.1))$passed))
  expect_false(.ok(calcic_fao88(ca(thk = 14))))
  expect_true(.ok(calcic_fao88(ca(thk = 15))))
  # on very calcareous material, with secondary carbonate recorded
  r <- calcic_fao88(ca(ck = 30, c = 45, desg = "B"))
  expect_false(isTRUE(r$passed)); expect_false(2L %in% r$layers[r$evidence$units[[1]]$status %in% TRUE])
  expect_true(.ok(calcic_fao88(ca(ck = 30, c = 45, desg = "B",
                                  secondary_carbonates_form = c(NA, "soft concretions", NA)))))
  expect_true(.ok(calcic_fao88(ca(ck = 30, c = 45))))           # the suffix k records it
  # secondary soft carbonates of 5 % by volume
  expect_true(.ok(calcic_fao88(ca(ck = 18, c = 14, carbonate_soft_pct = c(NA, 5, NA)))))
  expect_false(.ok(calcic_fao88(ca(ck = 18, c = 14, carbonate_soft_pct = c(NA, 4, NA)))))
  # carbonate not measured: undecided, unless no effervescence or a pH below 6.5
  r <- calcic_fao88(.p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "B")))
  expect_true(is.na(r$passed))
  r <- calcic_fao88(.p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "B"), ph_h2o = 5))
  expect_false(.ok(r))
  # petrocalcic: a cemented calcic layer, no minimum thickness (default 4)
  pc <- function(cem, desg = "Bkm") .p88(top_cm = c(0, 30, 35), bottom_cm = c(30, 35, 100),
                                         designation = c("A", desg, "R"), caco3_pct = c(5, 60, NA),
                                         cementation_class = c(NA, cem, NA))
  expect_true(.ok(petrocalcic_fao88(pc("indurated"))))
  expect_false(.ok(petrocalcic_fao88(pc("weakly"))))
  r <- petrocalcic_fao88(pc(NA))
  expect_true(.ok(r)); expect_true("cementation_from_designation" %in% r$evidence$flags)
  expect_false(.ok(petrocalcic_fao88(pc(NA, "Bk"))))
})

test_that("gypsic and petrogypsic horizons: 5 % more than the C and the product of 150", {
  skip_on_cran()
  gy <- function(g, thk = 20, c = 3, cem = NA) .p88(top_cm = c(0, 20, 20 + thk), bottom_cm = c(20, 20 + thk, 150),
                                                    designation = c("A", "By", "C"), caso4_pct = c(1, g, c),
                                                    cementation_class = c(NA, cem, NA))
  expect_true(.ok(gypsic_fao88(gy(8))))              # +5 and 20 x 8 = 160
  expect_false(.ok(gypsic_fao88(gy(7.9))))           # +4.9
  expect_true(.ok(gypsic_fao88(gy(10, thk = 15))))   # 15 x 10 = 150
  expect_false(.ok(gypsic_fao88(gy(9.9, thk = 15)))) # 148.5
  expect_false(.ok(gypsic_fao88(gy(30, thk = 14))))
  expect_true(.ok(petrogypsic_fao88(gy(60, thk = 5, cem = "strongly"))))
  expect_false(.ok(petrogypsic_fao88(gy(60, thk = 5))))
  # gypsum not measured with ECe below 2 dS/m: none, flagged
  r <- gypsic_fao88(.p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "C"), ec_dS_m = 0.5))
  expect_false(.ok(r))
})

test_that("sulfuric horizon: pH (1:1) below 3.5 over 15 cm; jarosite not required (Q18)", {
  skip_on_cran()
  su <- function(ph, thk = 15, col = "ph_h2o_1to1") {
    a <- list(top_cm = c(0, 20, 20 + thk), bottom_cm = c(20, 20 + thk, 100), designation = c("A", "Bj", "Cg"))
    a[[col]] <- c(4.5, ph, 6)
    do.call(.p88, a)
  }
  expect_true(.ok(sulfuric_fao88(su(3.4))))
  expect_false(.ok(sulfuric_fao88(su(3.5))))
  expect_false(.ok(sulfuric_fao88(su(3.4, thk = 14))))
  r <- sulfuric_fao88(su(3.4, col = "ph_h2o"))
  expect_true(.ok(r)); expect_true("ph_h2o_for_1to1" %in% r$evidence$flags)
  expect_false(r$evidence$jarosite)
})

test_that("albic E: the value and chroma classes, the red parent material", {
  skip_on_cran()
  al <- function(vm, cm, vd = NA, cd = NA, desg = "E", c_hue = "10YR") .p88(
    top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 100), designation = c("A", desg, "C"),
    munsell_hue_moist = c("10YR", "10YR", c_hue), munsell_value_moist = c(3, vm, 5),
    munsell_chroma_moist = c(2, cm, 4), munsell_value_dry = c(5, vd, 6), munsell_chroma_dry = c(2, cd, 4))
  expect_true(.ok(albic_e_fao88(al(6, 3))))
  expect_false(.ok(albic_e_fao88(al(6, 4))))
  expect_true(.ok(albic_e_fao88(al(5, 2))))
  expect_false(.ok(albic_e_fao88(al(5, 3))))          # "closer to 2 than to 3"
  expect_false(.ok(albic_e_fao88(al(3, 1))))          # too dark
  expect_true(.ok(albic_e_fao88(al(3, 3, vd = 7, cd = 3))))
  r <- albic_e_fao88(al(5, 3, c_hue = "5YR"))
  expect_true(.ok(r)); expect_true("chroma_3_on_red_parent_material" %in% r$evidence$flags)
  expect_false(.ok(albic_e_fao88(al(6, 2, desg = "Bw"))))
  r <- albic_e_fao88(al(6, 2, desg = NA))
  expect_true(.ok(r)); expect_true("albic_by_colour_only" %in% r$evidence$flags)
})

test_that("profiles without depths are undecided; the canonical fixtures read without error", {
  skip_on_cran()
  p <- suppressWarnings(PedonRecord$new(site = list(id = "n"), horizons = make_empty_horizons(2)))
  r <- argic_b_fao88(p)
  expect_true(is.na(r$passed)); expect_true("top_cm" %in% r$missing)
  expect_false(.ok(histic_h_fao88(.p88(top_cm = c(0, 10), bottom_cm = c(10, 100), designation = c("A", "R"),
                                       oc_pct = c(2, NA), clay_pct = c(20, NA)))))
  expected <- list(chernozem = "mollic_a", podzol = "spodic_b", solonetz = "natric_b",
                   calcisol = "calcic", gypsisol = "gypsic", histosol = "histic_h",
                   luvisol = "argic_b", umbrisol = "umbric_a", planosol = "albic_e")
  for (k in names(expected)) {
    p <- get(paste0("make_", k, "_canonical"))()
    expect_true(.ok(get(paste0(expected[[k]], "_fao88"))(p)), info = k)
  }
  # without the spodic chemistry the cambic B of the Cambisol stays undecided
  # (Hugo, 2026-11-09: the missing measurement is not read as absent)
  p <- make_cambisol_canonical()
  expect_true(is.na(spodic_b_fao88(p)$passed)); expect_true(is.na(cambic_b_fao88(p)$passed))
  for (f in c("make_ferralsol_canonical", "make_vertisol_canonical", "make_leptosol_canonical")) {
    p <- get(f)()
    for (id in .F88_HZ$id) expect_no_error(get(paste0(id, "_fao88"))(p))
  }
})

test_that("the review of 2026-11-09: mollic A (defects 1-3, 9; decisions B and mixing)", {
  skip_on_cran()
  base <- list(clay_pct = 20, silt_pct = 40, sand_pct = 40, bs_pct = 80, p2o5_citric_mg_kg = 10)
  mk <- function(...) { a <- list(...); b <- base; b[names(a)] <- a; do.call(.p88, b) }
  # 1: a layer straddling 18 cm is read on its own values
  p <- mk(top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 120), designation = c("A1", "A2", "C"),
          oc_pct = c(4, 0.3, 0.2), munsell_value_moist = c(2, 5, 6), munsell_chroma_moist = c(1, 4, 4),
          munsell_value_dry = c(3, 6, 7))
  r <- mollic_a_fao88(p)
  expect_false(.ok(r)); expect_equal(r$evidence$thickness_cm, 15)
  # 2: the strict colour with 0.6 % OC passes even with more than 40 % lime
  p <- mk(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "C"), oc_pct = c(1, 0.2),
          bs_pct = 100, caco3_pct = c(45, 60), munsell_value_moist = c(3, 6),
          munsell_chroma_moist = c(2, 3), munsell_value_dry = c(5, 8))
  expect_true(.ok(mollic_a_fao88(p)))
  # 3: CaCO3 not recorded leaves the waiver open; a pH below 6.5 reads it absent
  lw <- function(...) mk(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "C"),
                         oc_pct = c(3, 0.2), munsell_value_moist = c(4, 7), munsell_chroma_moist = c(2, 3),
                         munsell_value_dry = c(6, 8), ...)
  expect_true(is.na(.ok(mollic_a_fao88(lw()))))
  r <- mollic_a_fao88(lw(ph_h2o = c(6, 6)))
  expect_false(.ok(r))
  expect_true(.ok(mollic_a_fao88(lw(caco3_pct = c(45, 50)))))
  # 9: an open solum (the description ends in the B): FALSE when the A is no
  # more than a third of its lower bound
  op <- function(b) mk(top_cm = c(0, 20), bottom_cm = c(20, b), designation = c("A", "Bw"),
                       oc_pct = c(3, 0.5), munsell_value_moist = c(2, 5), munsell_chroma_moist = c(1, 4),
                       munsell_value_dry = c(3, 6))
  expect_false(.ok(mollic_a_fao88(op(70))))         # 20 <= 70 / 3
  expect_true(is.na(.ok(mollic_a_fao88(op(55)))))   # 20 > 55 / 3, the solum may be deeper
  # B: the A and the solum from the mineral surface, an O on top left out
  p <- mk(top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 120), designation = c("Oa", "A", "Bw", "C"),
          clay_pct = c(NA, 20, 20, 20), oc_pct = c(30, 3, 0.5, 0.2), munsell_value_moist = c(2, 2, 5, 6),
          munsell_chroma_moist = c(1, 1, 4, 4), munsell_value_dry = c(3, 3, 6, 7))
  r <- mollic_a_fao88(p)
  expect_true(.ok(r)); expect_equal(r$evidence$solum_cm, 50)
})

test_that("the review of 2026-11-09: histic, argic, cementation, albic and the stand-ins", {
  skip_on_cran()
  # 4: a plough layer may be an Hp or Op
  p <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("Hp", "Cg"), oc_pct = c(14, 1),
            clay_pct = 30, silt_pct = 40, sand_pct = 30, site = list(id = "w", drainage_class = "poorly drained"))
  expect_true(.ok(histic_h_fao88(p)))
  # F: moderately well drained leaves the organic material undecided
  om <- function(d) .p88(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("H", "C"),
                         oc_pct = c(15, 1), clay_pct = 20, silt_pct = 40, sand_pct = 40,
                         site = list(id = "o", drainage_class = d))
  expect_true(is.na(.ok(histic_h_fao88(om("moderately well drained")))))
  expect_true(.ok(histic_h_fao88(om("imperfectly drained"))))
  expect_false(.ok(histic_h_fao88(om("well drained"))))
  # 5: lamellae_cumulative_cm of 0 is no lamellae: the 7.5 cm rule
  lam0 <- .luvic88(lamellae_cumulative_cm = c(0, 0, 0, 0))
  expect_true(.ok(argic_b_fao88(lam0)))
  # 8: no clay skins and a missing clay on the way up: undecided
  r <- argic_b_fao88(.luvic88(designation = c("A", "E", "Bw", "C"), clay_films_amount = NA,
                              clay_pct = c(12, NA, 25, 22), silt_pct = c(20, NA, 25, 25),
                              sand_pct = c(68, NA, 50, 53)))
  expect_true(is.na(r$passed))
  # 13: the Bt of an E&Bt counts as the clay-skin observation
  r <- argic_b_fao88(.luvic88(designation = c("A", "E", "E&Bt", "C"), clay_films_amount = NA))
  expect_true(.ok(r)); expect_true("bt_designation_for_clay_skins" %in% r$evidence$flags)
  # 7: petrocalcic needs k with m in the designation; a silica pan is not it
  pc <- function(d) .p88(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120), designation = c("A", d, "C"),
                         caco3_pct = c(2, 20, 10))
  expect_false(.ok(petrocalcic_fao88(pc("Bqm"))))
  expect_true(.ok(petrocalcic_fao88(pc("Bkm"))))
  expect_false(.ok(petrogypsic_fao88(.p88(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120),
                                          designation = c("A", "Bkm", "C"), caso4_pct = c(0, 20, 2)))))
  expect_true(.ok(petrogypsic_fao88(.p88(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120),
                                         designation = c("A", "Bym", "C"), caso4_pct = c(0, 20, 2)))))
  # 10: "closer to 2 than to 3" is below 2.5; a dry value not recorded is the
  # moist value + 1, flagged
  al <- function(ch) .p88(top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 100), designation = c("A", "E", "Bt"),
                          munsell_value_moist = c(2, 5, 4), munsell_chroma_moist = c(1, ch, 4))
  r <- albic_e_fao88(al(2.4))
  expect_true(.ok(r)); expect_true("dry_value_from_moist_plus_1" %in% r$evidence$flags)
  expect_false(.ok(albic_e_fao88(al(2.5))))
  # C: an undesignated pale layer is read only below a surface layer and above a B or C
  un <- function(d) .p88(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150), designation = d,
                         munsell_value_moist = c(3, 6, 5), munsell_chroma_moist = c(2, 2, 4),
                         munsell_value_dry = c(4, 7, 6), munsell_chroma_dry = c(2, 2, 4))
  expect_true(is.na(.ok(albic_e_fao88(un(c(NA, NA, NA))))))
  r <- albic_e_fao88(un(c("A", NA, "Bt")))
  expect_true(.ok(r)); expect_true("albic_by_colour_only" %in% r$evidence$flags)
  # A: andic properties absent, flagged, without volcanic material recorded
  r <- ferralic_b_fao88(.ferralic88(al_ox_pct = NA, fe_ox_pct = NA))
  expect_true(.ok(r)); expect_true("andic_absent_no_volcanic_material" %in% r$evidence$flags)
  r <- ferralic_b_fao88(.ferralic88(al_ox_pct = NA, fe_ox_pct = NA, site = list(id = "v", parent_material = "volcanic ash")))
  expect_true(is.na(r$passed))
})

test_that("the review of 2026-11-09: the spodic B (defects 6 and 11)", {
  skip_on_cran()
  # the older suffix ir records the illuvial sesquioxides of a cemented layer
  p <- .p88(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100), designation = c("E", "Birm", "C"),
            clay_pct = 3, silt_pct = 7, sand_pct = 90, cementation_class = c(NA, "strongly", NA))
  expect_true(.ok(spodic_b_fao88(p)))
  # without the chemistry a Bw is undecided, not ruled out by its designation
  p <- .p88(top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 120), designation = c("A", "Bw1", "Bw2", "C"),
            clay_pct = c(5, 6, 6, 4), silt_pct = 5, sand_pct = c(90, 89, 89, 91), oc_pct = c(2, 0.5, 0.3, 0.1),
            ph_h2o = c(4.5, 4.6, 4.8, 5))
  r <- spodic_b_fao88(p)
  expect_true(is.na(r$passed))
  expect_length(r$evidence$stand_in, 0L); expect_false(r$evidence$lower_grade)
  # a stand-in that rules the chemistry out leaves the undecided item-2 unit
  # (pellets in a layer of unknown clay) undecided, and is recorded
  p <- .p88(top_cm = c(0, 10, 30, 60), bottom_cm = c(10, 30, 60, 120), designation = c("A", "E", "Bhs", "C"),
            clay_pct = c(5, 3, NA, 4), silt_pct = c(5, 5, NA, 5), sand_pct = c(90, 92, NA, 91),
            oc_pct = c(2, 0.3, 1, 0.1), spodic_pellets = c(NA, NA, TRUE, NA), ph_h2o = c(4.5, 4.6, 4.8, 5),
            al_ox_pct = 0.05, fe_ox_pct = 0.05)
  expect_false(spodic_horizon_usda(p)$passed)
  r <- spodic_b_fao88(p)
  expect_true(is.na(r$passed))
  expect_identical(r$evidence$stand_in, "spodic_horizon_usda"); expect_true(r$evidence$lower_grade)
})

test_that("the readings soilKey adds are recorded for Hugo in data-raw/fao_readings.yaml", {
  skip_on_cran()
  f <- test_path("..", "..", "data-raw", "fao_readings.yaml")
  skip_if_not(file.exists(f))
  y <- yaml::read_yaml(f)
  topics <- vapply(y$readings, `[[`, "", "topic")
  expect_true(any(grepl("0.9.382", vapply(y$readings, function(r) paste(r$reading, r$version %||% ""), ""))))
  expect_true(all(c("Mixing the 18 cm of the mollic A", "Volcanic glass not recorded",
                    "Code readings confirmed on the review") %in% topics))
  r382 <- Filter(function(r) identical(r$version, "0.9.382"), y$readings)
  expect_true(all(grepl("\\(Hugo, 2026-11-0[89]\\)\\.$", vapply(r382, `[[`, "", "reading")) |
                  grepl("for Hugo to confirm", vapply(r382, `[[`, "", "reading"))))
})
