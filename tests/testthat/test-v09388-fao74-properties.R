# v0.9.388: the FAO-Unesco legend, part 8: the diagnostic properties of the
# Legend of 1974 (pp. 27-32) and the terms of its key without a definition
# section, as three-valued predicates <name>_fao74(), R/diagnostics-fao74.R.
# A pass and a near miss at every printed number, the same profile through
# _fao88 and _fao74 where the two editions differ, and the stand-in flags.

.q74 <- function(..., site = list(id = "q")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}
.ok <- function(res) res$passed

.lq74 <- function(...) {
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
  do.call(.q74, base)
}

test_that("the 28 predicates exist and return DiagnosticResults of the 1974 legend", {
  skip_on_cran()
  ids <- c("abrupt_textural_change", "albic_material", "aridic_regime", "amorphous_complex",
           "ferralic_properties", "ferric_properties", "gilgai", "high_om_b", "high_salinity",
           "hydromorphic_properties", "interfingering", "permafrost", "plinthite",
           "slickensides", "smeary", "soft_powdery_lime", "sulfidic_materials",
           "takyric_features", "thin_iron_pan", "tonguing", "vertic_properties",
           "weatherable_minerals", "recent_alluvial_deposits", "calcareous",
           "bleached_coatings", "heavy_clay", "exchangeable_sodium", "acric_cec")
  expect_identical(.F74_PR$id, ids)
  p <- .lq74()
  for (id in ids) {
    r <- get(paste0(id, "_fao74"))(p)
    expect_s3_class(r, "DiagnosticResult")
    expect_identical(r$name, paste0(id, "_fao74"))
    expect_true(is.logical(r$passed) && length(r$passed) == 1L)
    expect_match(r$reference, "1974")
    expect_identical(r$evidence$edition, "1974")
  }
  # the shared readers still answer for 1988 in a 1988 context
  for (f in c("slickensides", "smeary", "plinthite", "permafrost", "interfingering",
              "weatherable_minerals", "tonguing")) {
    r <- get(paste0(f, "_fao88"))(p)
    expect_identical(r$evidence$edition, "1988")
    expect_identical(r$name, paste0(f, "_fao88"))
  }
})

test_that("abrupt textural change: A or E, doubling or 20 points with doubling below, 8 cm", {
  skip_on_cran()
  ab <- function(c1, c2, c3 = c2, bd = "abrupt", desg = c("A", "Bt1", "Bt2"))
    .q74(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100), designation = desg,
         clay_pct = c(c1, c2, c3), silt_pct = 20, sand_pct = 80 - c(c1, c2, c3),
         boundary_distinctness = c(bd, NA, NA))
  expect_true(.ok(abrupt_textural_change_fao74(ab(10, 20))))
  expect_false(.ok(abrupt_textural_change_fao74(ab(10, 19.9))))
  # 20 % or more: 20 points and, somewhere in the lower horizon, double
  expect_true(.ok(abrupt_textural_change_fao74(ab(20, 40))))
  expect_false(.ok(abrupt_textural_change_fao74(ab(20, 39.9, 39.9))))
  p <- ab(25, 45, 45)
  expect_false(.ok(abrupt_textural_change_fao74(p)))
  expect_true(.ok(abrupt_textural_change_fao88(p)))     # 1988: 20 points suffice
  expect_true(.ok(abrupt_textural_change_fao74(ab(25, 45, 50))))
  # the upper horizon an A or E
  expect_false(.ok(abrupt_textural_change_fao74(ab(10, 20, desg = c("Bw", "Bt1", "Bt2")))))
  # within 8 cm: abrupt or clear; gradual undecided; diffuse fails;
  # not recorded, absent
  expect_true(.ok(abrupt_textural_change_fao74(ab(10, 20, bd = "clear"))))
  expect_true(is.na(.ok(abrupt_textural_change_fao74(ab(10, 20, bd = "gradual")))))
  expect_false(.ok(abrupt_textural_change_fao74(ab(10, 20, bd = "diffuse"))))
  r <- abrupt_textural_change_fao74(ab(10, 20, bd = NA))
  expect_false(.ok(r))
  expect_true("boundary_distinctness_not_recorded_absent" %in% r$evidence$flags)
  # across a thin layer: 8 cm passes in 1974 (1988: under 5 cm)
  th <- function(t) .q74(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 100),
                         designation = c("E", "EB", "Bt"), clay_pct = c(10, 14, 20),
                         silt_pct = 20, sand_pct = c(70, 66, 60))
  expect_true(.ok(abrupt_textural_change_fao74(th(8))))
  expect_false(.ok(abrupt_textural_change_fao74(th(9))))
  expect_false(.ok(abrupt_textural_change_fao88(th(8))))
})

test_that("albic material: the albic colours outside an E", {
  skip_on_cran()
  am <- function(desg, vm, cm, vd) .q74(top_cm = c(0, 60), bottom_cm = c(60, 120),
                                         designation = c(desg, "C"), munsell_value_moist = c(vm, 3),
                                         munsell_chroma_moist = c(cm, 4), munsell_value_dry = c(vd, 4),
                                         munsell_chroma_dry = c(cm, 4), clay_pct = 3, sand_pct = 94,
                                         silt_pct = 3)
  expect_true(.ok(albic_material_fao74(am("C1", 6, 3, 7))))
  expect_false(.ok(albic_material_fao74(am("C1", 6, 4, 7))))
  expect_true(.ok(albic_material_fao74(am("C1", 5, 6, 6))))      # no limit at the middle values
  expect_false(.ok(albic_material_fao74(am("C1", 3, 2, 4))))
  r <- albic_material_fao74(am("E", 6, 3, 7))
  expect_false(isTRUE(1L %in% r$layers))                         # an E is not albic material
  expect_true(.ok(albic_material_fao74(am("C1", 6, 3, 7), window_cm = c(0, 50))))
})

test_that("aridic moisture regime: the regime recorded at the site (Q26)", {
  skip_on_cran()
  p <- function(r) .lq74(site = list(id = "a", soil_moisture_regime = r))
  expect_true(.ok(aridic_regime_fao74(p("aridic"))))
  expect_true(.ok(aridic_regime_fao74(p("torric"))))
  expect_false(.ok(aridic_regime_fao74(p("ustic"))))
  r <- aridic_regime_fao74(.lq74())
  expect_true(is.na(.ok(r)))
  expect_true("soil_moisture_regime" %in% r$missing)
})

test_that("amorphous exchange complex: bulk density below 0.85 and one other mark (Q20)", {
  skip_on_cran()
  ac <- function(bd = 0.84, oc = 0.5, c82 = NA, naf = NA, w15 = NA, dta = FALSE, clay = 30)
    .q74(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("Ah", "Bw"),
         bulk_density_g_cm3 = c(bd, 1.2), oc_pct = c(oc, 0.2), cec_ph82_cmol = c(c82, NA),
         ph_naf = c(naf, NA), water_content_1500kpa = c(w15, 10), dta_low_endotherm = c(dta, FALSE),
         clay_pct = c(clay, 30), silt_pct = 40, sand_pct = 30)
  expect_true(.ok(amorphous_complex_fao74(ac(oc = 0.7), max_top_cm = 35)))
  expect_false(.ok(amorphous_complex_fao74(ac(bd = 0.85, oc = 0.7), max_top_cm = 35)))
  r <- amorphous_complex_fao74(ac(oc = 0.6, c82 = 15, naf = 9.4, w15 = 30, clay = 30))
  expect_false(.ok(r))                                           # every mark at its limit
  expect_true(.ok(amorphous_complex_fao74(ac(c82 = 15.1, clay = 10, oc = 0.5))))   # 151 per 100 g clay
  expect_false(.ok(amorphous_complex_fao74(ac(c82 = 15, clay = 10, oc = 0.5, w15 = 5))))
  expect_true(.ok(amorphous_complex_fao74(ac(naf = 9.5, w15 = 25))))
  expect_false(.ok(amorphous_complex_fao74(ac(naf = 9.5, w15 = 19))))  # 15-bar water below 20 %
  expect_true(.ok(amorphous_complex_fao74(ac(w15 = 31, clay = 30))))
  expect_true(.ok(amorphous_complex_fao74(ac(dta = TRUE))))
  # nothing but the density: undecided, the andic stand-ins then decide
  fake <- function(passed, name) function(pedon)
    DiagnosticResult$new(name = name, passed = passed, layers = if (isTRUE(passed)) 1L else integer(0),
                         evidence = list(), missing = character(0), reference = "test")
  testthat::local_mocked_bindings(andic_soil_properties_usda = fake(TRUE, "andic_soil_properties_usda"))
  r <- amorphous_complex_fao74(ac(oc = NA, dta = NA))
  expect_true(.ok(r))
  expect_identical(r$evidence$stand_in, "andic_soil_properties_usda")
  expect_true("stand_in_andic_soil_properties_usda" %in% r$evidence$flags)
})

test_that("ferralic and ferric properties: CEC (NH4Cl) below 24 me per 100 g clay", {
  skip_on_cran()
  cb <- function(cec, method = "nh4cl") .q74(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 120),
                                               designation = c("A", "Bw", "C"), clay_pct = 20,
                                               silt_pct = 40, sand_pct = 40, cec_cmol = c(8, cec, 5),
                                               cec_method = method, bs_pct = 80, ph_h2o = 7,
                                               munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
                                               munsell_value_moist = 5, munsell_chroma_moist = c(3, 6, 4),
                                               munsell_value_dry = 6, munsell_chroma_dry = c(3, 6, 4),
                                               structure_type = c("granular", "subangular blocky", "massive"),
                                               structure_grade = c("weak", "moderate", NA))
  expect_true(.ok(ferralic_properties_fao74(cb(4.7))))    # 23.5
  expect_false(.ok(ferralic_properties_fao74(cb(4.8))))   # 24
  # the NH4Cl CEC from ecec_cmol, flagged (Q12)
  p <- .q74(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 120), designation = c("A", "Bw", "C"),
            clay_pct = 20, silt_pct = 40, sand_pct = 40, ecec_cmol = c(8, 4, 5))
  r <- ferralic_properties_fao74(p)
  expect_true(.ok(r))
  expect_true("ecec_for_nh4cl_cec" %in% r$evidence$flags)
  # ferric: in a subhorizon of the argillic B (25 % clay: 6.0 is 24)
  expect_true(.ok(ferric_properties_fao74(.lq74(ecec_cmol = c(NA, NA, 5.9, NA)))))
  r <- ferric_properties_fao74(.lq74(ecec_cmol = c(NA, NA, 6, NA)))
  expect_false(isTRUE(.ok(r)))
  # the 1988 ferric properties have no CEC route
  expect_false(isTRUE(.ok(ferric_properties_fao88(.lq74(ecec_cmol = c(NA, NA, 5.9, NA))))))
  # the mottles of 1988: many (15 %) coarse red mottles
  mt <- function(pct) .lq74(redoximorphic_features_pct = c(NA, NA, pct, NA),
                            mottle_size_class = c(NA, NA, "coarse", NA),
                            mottle_colors = c(NA, NA, "2.5YR 4/8", NA))
  expect_true(.ok(ferric_properties_fao74(mt(15))))
  expect_false(isTRUE(.ok(ferric_properties_fao74(mt(14)))))
})

test_that("gilgai, high organic matter in the B and high salinity", {
  skip_on_cran()
  expect_true(.ok(gilgai_fao74(.lq74(site = list(id = "g", gilgai_presence = TRUE)))))
  expect_true(.ok(gilgai_fao74(.lq74(site = list(id = "g", microrelief_form = "gilgai")))))
  expect_false(.ok(gilgai_fao74(.lq74())))
  # 1.35 % OM weighted to 100 cm: OC 0.79 x 1.724 = 1.362; 0.78 -> 1.345
  om <- function(oc, o = FALSE) {
    if (o) return(.q74(top_cm = c(0, 10), bottom_cm = c(10, 120), designation = c("O", "Bo"),
                       oc_pct = c(30, oc), clay_pct = c(NA, 50), silt_pct = 10, sand_pct = 40))
    .q74(top_cm = 0, bottom_cm = 120, designation = "Bo", oc_pct = oc, clay_pct = 50,
         silt_pct = 10, sand_pct = 40)
  }
  expect_true(.ok(high_om_b_fao74(om(0.79))))
  expect_false(.ok(high_om_b_fao74(om(0.78))))
  expect_false(.ok(high_om_b_fao74(om(0.78, o = TRUE))))       # the O is excluded
  # Acrisols: 1.5 % OM in the upper part of the B (0.88 -> 1.517; 0.87 -> 1.4999)
  expect_true(.ok(high_om_b_fao74(.lq74(oc_pct = c(0.5, 0.4, 0.88, 0.2)), kind = "acrisol")))
  expect_false(.ok(high_om_b_fao74(.lq74(oc_pct = c(0.5, 0.4, 0.87, 0.2)), kind = "acrisol")))
  expect_false(.ok(high_om_b_fao74(.lq74(oc_pct = c(0.5, 0.4, 0.88, 0.2)))))
  # high salinity: ECe above 15 within 125 / 90 / 75 cm by the texture of the
  # upper 30 cm; above 4 within 25 cm at pH (1:1) above 8.5
  hs <- function(top, ec, clay, sand, ph = 7) .q74(top_cm = c(0, top), bottom_cm = c(top, top + 40),
                                                  designation = c("A", "Bz"), clay_pct = clay,
                                                  sand_pct = sand, silt_pct = 100 - clay - sand,
                                                  ec_dS_m = c(1, ec), ph_h2o_1to1 = ph)
  expect_true(.ok(high_salinity_fao74(hs(125, 16, 5, 90))))     # coarse
  expect_false(.ok(high_salinity_fao74(hs(125, 15, 5, 90))))
  expect_false(.ok(high_salinity_fao74(hs(126, 16, 5, 90))))
  expect_true(.ok(high_salinity_fao74(hs(90, 16, 20, 40))))      # medium
  expect_false(.ok(high_salinity_fao74(hs(91, 16, 20, 40))))
  expect_true(.ok(high_salinity_fao74(hs(75, 16, 40, 20))))      # fine
  expect_false(.ok(high_salinity_fao74(hs(76, 16, 40, 20))))
  al <- function(ec, ph) .q74(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "B"),
                              clay_pct = 40, sand_pct = 20, silt_pct = 40, ec_dS_m = c(ec, 1),
                              ph_h2o_1to1 = c(ph, 8))
  expect_true(.ok(high_salinity_fao74(al(4.1, 8.6))))
  expect_false(.ok(high_salinity_fao74(al(4.1, 8.5))))
  expect_false(.ok(high_salinity_fao74(al(4, 8.6))))
})

test_that("hydromorphic properties: groundwater, histic H, bluish hues and colours 4.1-4.5", {
  skip_on_cran()
  # 4.5 other soils, finer than loamy fine sand
  ot <- function(vm, cm, mott = NA, hue = "10YR", contrast = NA, clay = 25, sand = 40, top = 30,
                 site = list(id = "h"))
    .q74(top_cm = c(0, top), bottom_cm = c(top, top + 50), designation = c("A", "Bg"),
         clay_pct = clay, sand_pct = sand, silt_pct = 100 - clay - sand,
         munsell_hue_moist = c("10YR", hue), munsell_value_moist = c(4, vm),
         munsell_chroma_moist = c(3, cm), munsell_value_dry = c(5, vm + 1),
         munsell_chroma_dry = c(3, cm), mottle_colors = c(NA, mott),
         mottle_contrast = c(NA, contrast), oc_pct = c(0.5, 0.2), bs_pct = 60, site = site)
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 1))))
  expect_false(.ok(hydromorphic_properties_fao74(ot(5, 2))))
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 2, "7.5YR 5/8"))))
  expect_false(.ok(hydromorphic_properties_fao74(ot(5, 3, "7.5YR 5/8"))))
  expect_false(.ok(hydromorphic_properties_fao74(ot(3, 1))))
  expect_true(.ok(hydromorphic_properties_fao74(ot(3, 0))))
  # coarse: hue between 10YR and 10Y with distinct mottles, chroma 3
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 3, "7.5YR 5/8", "2.5Y", "distinct", 5, 90))))
  expect_false(.ok(hydromorphic_properties_fao74(ot(5, 3, "7.5YR 5/8", "2.5Y", "faint", 5, 90))))
  # bluish hues, the histic H, groundwater
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 4, hue = "5GY"))))
  expect_false(.ok(hydromorphic_properties_fao74(ot(5, 4, hue = "10Y"))))
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 4, site = list(id = "g", groundwater_depth_cm = 60)))))
  hh <- .q74(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("H", "C"), oc_pct = c(30, 0.3),
             clay_pct = c(NA, 20), sand_pct = 40, silt_pct = 40, munsell_hue_moist = "10YR",
             munsell_value_moist = c(2, 5), munsell_chroma_moist = c(1, 4))
  expect_true(.ok(hydromorphic_properties_fao74(hh)))
  # within 50 cm (the Gleysols) and 100 cm (the Gleyic Cambisols)
  expect_false(.ok(hydromorphic_properties_fao74(ot(5, 1, top = 60))))
  expect_true(.ok(hydromorphic_properties_fao74(ot(5, 1, top = 60), max_top_cm = 100)))
  # 4.2 mollic soils, the lower mollic A of chroma 1 or less
  mo <- function(cm_a, vm_c, cm_c, mott = NA) .q74(top_cm = c(0, 30), bottom_cm = c(30, 100),
                                                   designation = c("A", "Cg"), clay_pct = 25,
                                                   silt_pct = 35, sand_pct = 40, oc_pct = c(2, 0.2),
                                                   bs_pct = 70, munsell_hue_moist = "10YR",
                                                   munsell_value_moist = c(2, vm_c),
                                                   munsell_chroma_moist = c(cm_a, cm_c),
                                                   munsell_value_dry = c(3, vm_c + 1),
                                                   munsell_chroma_dry = c(cm_a, cm_c),
                                                   mottle_colors = c(NA, mott),
                                                   structure_type = c("granular", "massive"),
                                                   structure_grade = c("moderate", NA))
  expect_true(isTRUE(mollic_a_fao74(mo(1, 5, 0))$passed))
  expect_true(.ok(hydromorphic_properties_fao74(mo(1, 5, 0))))
  expect_false(.ok(hydromorphic_properties_fao74(mo(1, 5, 1))))         # below 1 without mottles
  expect_true(.ok(hydromorphic_properties_fao74(mo(1, 5, 1, "7.5YR 5/6"))))   # below 1.5 mottled
  # the lower mollic A of chroma 2
  expect_true(.ok(hydromorphic_properties_fao74(mo(2, 4, 1))))
  expect_false(.ok(hydromorphic_properties_fao74(mo(2, 4, 2))))
  expect_true(.ok(hydromorphic_properties_fao74(mo(2, 5, 2, "7.5YR 5/6"))))
  expect_identical(hydromorphic_properties_fao74(mo(2, 5, 2))$evidence$kind_of_soil, "mollic")
  # 4.3 an argillic B right below an Ap
  ar <- function(cm, mott) .lq74(designation = c("Ap", "AB", "Bt", "C"), top_cm = c(0, 20, 30, 90),
                                 bottom_cm = c(20, 30, 90, 150), munsell_value_moist = c(3, 4, 5, 5),
                                 munsell_chroma_moist = c(2, 3, cm, 4), munsell_hue_moist = "10YR",
                                 mottle_colors = c(NA, NA, mott, NA),
                                 clay_pct = c(12, 13, 25, 22))
  r <- hydromorphic_properties_fao74(ar(2, "7.5YR 5/8"))
  expect_identical(r$evidence$kind_of_soil, "other")    # the AB stands between the Ap and the B
  ap <- function(cm, mott) .q74(top_cm = c(0, 25, 80), bottom_cm = c(25, 80, 150),
                                designation = c("Ap", "Bt", "C"), clay_pct = c(12, 25, 22),
                                silt_pct = c(20, 25, 25), sand_pct = c(68, 50, 53),
                                clay_films_amount = c(NA, "common", NA), munsell_hue_moist = "10YR",
                                munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(3, cm, 4),
                                munsell_value_dry = c(6, 6, 6), mottle_colors = c(NA, mott, NA))
  r <- hydromorphic_properties_fao74(ap(2, "7.5YR 5/8"))
  expect_identical(r$evidence$kind_of_soil, "argillic")
  expect_true(.ok(r))
  expect_false(.ok(hydromorphic_properties_fao74(ap(3, "7.5YR 5/8"))))
  expect_false(.ok(hydromorphic_properties_fao74(ap(2, NA))))
  # the stand-ins when undecided
  und <- ot(NA, NA)
  fake <- function(passed, name) function(pedon)
    DiagnosticResult$new(name = name, passed = passed, layers = if (isTRUE(passed)) 2L else integer(0),
                         evidence = list(), missing = character(0), reference = "test")
  testthat::local_mocked_bindings(aquic_conditions_usda = fake(TRUE, "aquic_conditions_usda"))
  r <- hydromorphic_properties_fao74(und)
  expect_true(.ok(r))
  expect_identical(r$evidence$stand_in, "aquic_conditions_usda")
})

test_that("properties shared with 1988: interfingering, permafrost, plinthite and others", {
  skip_on_cran()
  inf <- function(pct) .lq74(skeletans_pct = c(NA, NA, pct, NA), skeletan_thickness_mm = c(NA, NA, 1.5, NA))
  expect_true(.ok(interfingering_fao74(inf(16))))
  expect_false(.ok(interfingering_fao74(inf(15))))
  expect_true(.ok(permafrost_fao74(.lq74(permafrost_temp_C = c(NA, NA, NA, 0)), max_top_cm = 200)))
  expect_false(.ok(permafrost_fao74(.lq74(permafrost_temp_C = c(5, 4, 2, 1)), max_top_cm = 200)))
  expect_true(.ok(plinthite_fao74(.lq74(plinthite_pct = c(0, 0, 1, 0)))))
  expect_false(.ok(plinthite_fao74(.lq74(plinthite_pct = 0))))
  expect_true(.ok(slickensides_fao74(.lq74(slickensides = c(NA, NA, "few", NA)))))
  expect_true(.ok(smeary_fao74(.lq74(smeary_consistence = c(TRUE, NA, NA, NA)))))
  r <- soft_powdery_lime_fao74(.lq74(carbonate_soft_pct = c(NA, NA, 5, NA)))
  expect_true(.ok(r))
  expect_true("numbers_of_1988_q23" %in% r$evidence$flags)
  expect_false(.ok(soft_powdery_lime_fao74(.lq74(carbonate_soft_pct = c(0, 0, 4.9, 0)))))
  expect_true(.ok(soft_powdery_lime_fao74(.lq74(carbonate_coatings_faces_pct = c(NA, NA, 50, NA)))))
  # tonguing into the argillic B of 1974 (moderately fine: 10 mm)
  tg <- function(w) .lq74(tongue_vol_pct = c(NA, NA, 16, NA), tongue_width_mm = c(NA, NA, w, NA),
                          tongue_depth_cm = c(NA, NA, 10, NA))
  expect_true(.ok(tonguing_fao74(tg(10))))
  expect_false(.ok(tonguing_fao74(tg(9))))
  # without clay skins (and no Bt) there is no argillic B of 1974 to receive them
  nb <- tg(10)
  nb <- .lq74(designation = c("A", "E", "B", "C"), clay_films_amount = NA,
              tongue_vol_pct = c(NA, NA, 16, NA), tongue_width_mm = c(NA, NA, 10, NA),
              tongue_depth_cm = c(NA, NA, 10, NA))
  expect_true(.ok(tonguing_fao88(nb)))
  expect_false(.ok(tonguing_fao74(nb)))
  expect_true(.ok(weatherable_minerals_fao74(.lq74(weatherable_minerals_pct = 3))))
  expect_false(.ok(weatherable_minerals_fao74(.lq74(weatherable_minerals_pct = 2.9))))
  # calcareous: the numbers of 1988, flagged (Q23)
  r <- calcareous_fao74(.lq74(caco3_pct = c(1, 3, 3, 3)), window_cm = c(20, 50))
  expect_true(.ok(r))
  expect_true("numbers_of_1988_q23" %in% r$evidence$flags)
  expect_false(.ok(calcareous_fao74(.lq74(caco3_pct = c(1, 2, 3, 3)), window_cm = c(20, 50))))
})

test_that("sulfidic materials, takyric features, thin iron pan and vertic properties", {
  skip_on_cran()
  su <- function(s, cc, jar = NA) .q74(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Cg"),
                                       s_total_pct = c(0.1, s), caco3_pct = c(0, cc), ph_h2o = 7,
                                       jarosite_present = c(NA, jar), clay_pct = 30, silt_pct = 40,
                                       sand_pct = 30)
  expect_true(.ok(sulfidic_materials_fao74(su(0.75, 2.2))))
  expect_false(.ok(sulfidic_materials_fao74(su(0.74, 0))))
  expect_false(.ok(sulfidic_materials_fao74(su(0.75, 2.25))))
  # jarosite mottles make it sulfuric, not sulfidic (1974); 1988 does not ask
  expect_false(.ok(sulfidic_materials_fao74(su(1, 0, TRUE))))
  expect_true(.ok(sulfidic_materials_fao88(su(1, 0, TRUE))))
  tk <- function(clay) .q74(top_cm = c(0, 5), bottom_cm = c(5, 60), designation = c("A", "C"),
                            clay_pct = clay, sand_pct = 20, silt_pct = 80 - clay,
                            polygonal_cracks_spacing_cm = c(20, NA), structure_type = c("platy", "massive"))
  expect_true(.ok(takyric_features_fao74(tk(36))))
  expect_false(.ok(takyric_features_fao74(tk(35))))
  pan <- function(t, pt = NA, cem = "strongly") .q74(top_cm = c(0, 20, 20 + t), bottom_cm = c(20, 20 + t, 100),
                                                    designation = c("E", "Bsm", "Bs"), pan_type = c(NA, pt, NA),
                                                    cementation_class = c(NA, cem, NA))
  expect_true(.ok(thin_iron_pan_fao74(pan(1, "thin ironpan"))))
  r <- thin_iron_pan_fao74(pan(0.5))
  expect_true(.ok(r))
  expect_true("thin_iron_pan_from_designation" %in% r$evidence$flags)
  expect_false(.ok(thin_iron_pan_fao74(pan(5))))
  # vertic: cracks 1 cm wide within 50 cm of the top of the B, reaching the
  # upper B
  vt <- function(w, layer = 2, ct = NA) {
    cw <- c(NA, NA, NA, NA); cw[layer] <- w
    ctop <- c(NA, NA, NA, NA); ctop[layer] <- ct
    .q74(top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 150), designation = c("A", "Bw1", "Bw2", "C"),
         clay_pct = 40, silt_pct = 30, sand_pct = 30, cracks_width_cm = cw, cracks_top_cm = ctop)
  }
  expect_true(.ok(vertic_properties_fao74(vt(1))))
  expect_false(.ok(vertic_properties_fao74(vt(0.9))))
  expect_false(.ok(vertic_properties_fao74(vt(1, layer = 3))))          # not shown to reach the upper B
  expect_true(.ok(vertic_properties_fao74(vt(1, layer = 3, ct = 30))))
  expect_false(.ok(vertic_properties_fao74(vt(2, layer = 4, ct = 0))))  # below 70 cm
})

test_that("recent alluvial deposits: origin, organic matter 0.35 %, stratification, sulfidic", {
  skip_on_cran()
  ra <- function(oc, pm = "alluvium", ...) {
    base <- list(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 130), designation = c("A", "C1", "C2"),
                 oc_pct = oc, clay_pct = 20, silt_pct = 40, sand_pct = 40)
    a <- list(...); base[names(a)] <- a
    do.call(.q74, c(base, list(site = list(id = "r", parent_material = pm))))
  }
  expect_true(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.21)))))   # 0.362 % OM
  expect_false(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.2)))))   # 0.345 % OM
  # 1988 reads 0.20 % OC: 0.202 passes 1988 and fails 1974
  p <- ra(c(0.5, 0.3, 0.202))
  expect_true(.ok(fluvic_properties_fao88(p)))
  expect_false(.ok(recent_alluvial_deposits_fao74(p)))
  expect_true(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.1, 0.35)))))   # irregular decrease
  expect_false(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.21), pm = "granite"))))
  expect_true(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.21), pm = "colluvium"))))
  expect_true(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.1),
                                                    stratification_pattern = c(NA, "fine", NA)))))
  expect_true(.ok(recent_alluvial_deposits_fao74(ra(c(0.5, 0.3, 0.1), s_total_pct = c(NA, NA, 1),
                                                    caco3_pct = c(0, 0, 0)))))
})

test_that("key terms: bleached coatings, heavy clay, sodium above 6 % and the Acric CEC", {
  skip_on_cran()
  expect_true(.ok(bleached_coatings_fao74(.lq74(bleached_ped_coatings = c(TRUE, NA, NA, NA)))))
  r <- bleached_coatings_fao74(.lq74(coatings_value_moist = c(5, NA, NA, NA),
                                     uncoated_grains_pct = c(10, NA, NA, NA)))
  expect_true(.ok(r))
  expect_true("bleached_coatings_from_coating_colour_and_uncoated_grains" %in% r$evidence$flags)
  expect_false(.ok(bleached_coatings_fao74(.lq74(coatings_value_moist = c(4, NA, NA, NA),
                                                 uncoated_grains_pct = c(10, NA, NA, NA)))))
  expect_false(.ok(bleached_coatings_fao74(.lq74())))
  expect_true(.ok(heavy_clay_fao74(.lq74(clay_pct = c(12, 12, 60, 22)))))
  expect_false(.ok(heavy_clay_fao74(.lq74(clay_pct = c(12, 12, 59.9, 22)))))
  expect_true(.ok(exchangeable_sodium_fao74(.lq74(esp_pct = c(1, 1, 6.1, 1)))))
  expect_false(.ok(exchangeable_sodium_fao74(.lq74(esp_pct = c(1, 1, 6, 1)))))
  # 1.5 me per 100 g clay (40 % clay: 0.6)
  ac <- function(e) .q74(top_cm = c(0, 20), bottom_cm = c(20, 120), designation = c("A", "Bo"),
                         clay_pct = 40, silt_pct = 10, sand_pct = 50, ecec_cmol = c(3, e))
  expect_true(.ok(acric_cec_fao74(ac(0.6))))
  expect_false(.ok(acric_cec_fao74(.q74(top_cm = c(0, 20), bottom_cm = c(20, 120),
                                        designation = c("A", "Bo"), clay_pct = 40, silt_pct = 10,
                                        sand_pct = 50, ecec_cmol = c(3, 0.61)))))
})

test_that("the readings of this part are recorded for Hugo and the 1974 table holds its numbers", {
  skip_on_cran()
  f <- test_path("..", "..", "data-raw", "fao_thresholds_1974.yaml")
  fr <- test_path("..", "..", "data-raw", "fao_readings.yaml")
  skip_if_not(file.exists(f) && file.exists(fr))
  t74 <- yaml::read_yaml(f)
  pr <- t74$properties
  expect_equal(pr$abrupt_textural_change$distance_max_cm, 8)
  expect_equal(pr$amorphous_complex$bulk_density_lt_g_cm3, 0.85)
  expect_equal(pr$amorphous_complex$cec_ph82_clay_gt, 150)
  expect_equal(pr$high_om_in_b$om_weighted_min_pct, 1.35)
  expect_equal(pr$high_om_in_b$acrisols_upper_b_om_min_pct, 1.5)
  expect_equal(unlist(pr$high_salinity$depth_by_class_cm), c(coarse = 125, medium = 90, fine = 75))
  expect_equal(pr$recent_alluvial_deposits$om_gt_pct, 0.35)
  expect_equal(pr$vertic_properties$within_cm_of_b_top, 50)
  y <- yaml::read_yaml(fr)
  r388 <- Filter(function(r) identical(r$version, "0.9.388"), y$readings)
  expect_gte(length(r388), 8L)
  expect_true(all(grepl("for Hugo to confirm|\\(Hugo, 2026-11-0[89]\\)\\.$",
                        vapply(r388, `[[`, "", "reading"))))
})
