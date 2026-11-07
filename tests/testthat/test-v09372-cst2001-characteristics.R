# v0.9.372: the 25 diagnostic characteristics of the Chinese Soil Taxonomy
# (2001, English edition): three-valued readers, grades, kinds, stand-ins and
# the characteristics of the horizon panel.

.cp <- function(h, site = list()) {
  PedonRecord$new(site = c(list(id = "cst"), site), horizons = h)
}
.cf <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)
.cl <- function(p, id) {
  soilKey:::.cst_ch(soilKey:::.cst_ctx(p), id)
}


test_that("the table holds the 25 characteristics and every reader runs on an empty profile", {
  skip_on_cran()
  t <- soilKey:::.cst_characteristic_table()
  expect_equal(nrow(t), 25L)
  expect_true(all(vapply(t$fun, exists, NA, envir = asNamespace("soilKey"))))
  # the evidences of characteristics in the p. 19 list
  expect_setequal(t$id[t$evidence], c("vertic_features", "gleyic_features", "alic_property",
                                       "phosphic_property", "sodic_property"))
  p0 <- .cp(data.frame(top_cm = numeric(0), bottom_cm = numeric(0)))
  for (f in setdiff(t$fun, c("moisture_regime_cst", "temperature_regime_cst"))) {
    expect_false(isTRUE(.cf(f, p0)$passed), info = f)
  }
  expect_equal(soilKey:::.cst_cthr("isohumic", "rh_max"), 0.4)
  expect_equal(soilKey:::.cst_cthr("sodic", "esp_min"), 30)
  r <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  rd <- vapply(r, `[[`, "", "reading")
  tp <- vapply(r, `[[`, "", "topic")
  p3 <- c("Isohumic property: Rh 0.4", "Organic soil materials and the tiers", "Vertic features",
          "Gleyic hue bands and redoxic features", "Sulfidic materials and the sulfuric horizon",
          "The characteristics in the panel")
  expect_true(all(p3 %in% tp))
  expect_true(all(grepl("\\(Hugo, 2026-11-0[46]\\)\\.$", rd[tp %in% p3])))
  # the readings of later parts (0.9.373 on) may wait for his word
  expect_false(any(grepl("for Hugo to confirm, 2026-11-0[45]", rd)))
})


# --- organic soil materials ---------------------------------------------------------

test_that("organic soil materials: carbon, saturation, the five kinds and the tiers", {
  skip_on_cran()
  mk <- function(fib, oc = 30, sat = 200, py = NA, vp = NA, res = NA) {
    .cp(data.frame(top_cm = c(0, 40, 100), bottom_cm = c(40, 100, 140), designation = c("Oi", "Oa", "C"),
                   oc_pct = c(oc, 25, 0.4), clay_pct = c(NA, NA, 20), water_saturation_days = sat,
                   fiber_content_rubbed_pct = c(fib, 10, NA), pyrophosphate_color = c(py, NA, NA),
                   von_post_index = c(vp, NA, NA), plant_residues_pct = c(res, NA, NA)))
  }
  r <- .cl(mk(80), "organic_soil_materials")
  expect_equal(r$full, c(TRUE, TRUE, FALSE))
  expect_equal(r$kind[1:2], c("fibric", "sapric"))
  expect_equal(.cl(mk(50), "organic_soil_materials")$kind[1], "hemic")
  expect_equal(.cl(mk(50, py = "10YR 7/2"), "organic_soil_materials")$kind[1], "fibric")
  expect_equal(.cl(mk(16), "organic_soil_materials")$kind[1], "sapric")
  expect_equal(.cl(mk(17), "organic_soil_materials")$kind[1], "hemic")
  # the von Post index stands in, flagged
  r <- .cl(mk(NA, vp = "H5"), "organic_soil_materials")
  expect_equal(r$kind[1], "hemic"); expect_true("kind_from_von_post" %in% r$lflags[[1]])
  expect_equal(.cl(mk(NA, res = 80), "organic_soil_materials")$kind[1], "folic")
  # peat carbon (12-20 %) needs a month of saturation; unknown, undecided
  p <- mk(80, oc = 15, sat = 10)
  expect_false(.cl(p, "organic_soil_materials")$full[1])
  expect_true(is.na(.cl(mk(80, oc = 15, sat = NA), "organic_soil_materials")$full[1]))
  # kinds asked by the key
  expect_true(.cf("organic_soil_materials_cst", mk(80), kind = "fibric")$passed)
  expect_true(.cf("organic_soil_materials_cst", mk(80), kind = "sapric")$passed)
  expect_false(.cf("organic_soil_materials_cst", mk(80), kind = "folic")$passed)
  expect_true(is.na(.cf("organic_soil_materials_cst", mk(NA), kind = "fibric", top_max = 10)$passed))
  # the control section and tiers
  tt <- soilKey:::.cst_histosol_tiers(mk(80))
  expect_true(is.na(tt$value))
  h <- data.frame(top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 140), designation = c("Oi", "Oe", "Oa"),
                  oc_pct = 30, water_saturation_days = 300, fiber_content_rubbed_pct = c(80, 50, 10),
                  sphagnum_fibres_pct = c(10, 10, 10), bulk_density_g_cm3 = c(0.15, 0.2, 0.3))
  tt <- soilKey:::.cst_histosol_tiers(.cp(h))
  expect_equal(tt$value, 130)
  expect_equal(unname(tt$dominant), c("fibric", "hemic", "sapric"))
  h$sphagnum_fibres_pct <- 90
  tt <- soilKey:::.cst_histosol_tiers(.cp(h))
  # moss to 60 cm: control 160; a hemic layer within 0-60 gives the 30 cm tier
  expect_equal(tt$value, 160); expect_equal(unname(tt$tiers$surface), c(0, 30))
  h$fiber_content_rubbed_pct <- c(80, 80, 10)
  expect_equal(unname(soilKey:::.cst_histosol_tiers(.cp(h))$tiers$surface), c(0, 60))
  # a contact ends the control section
  h2 <- rbind(h[1:2, ], data.frame(top_cm = 90, bottom_cm = 100, designation = "R", oc_pct = NA,
                                   water_saturation_days = NA, fiber_content_rubbed_pct = NA,
                                   sphagnum_fibres_pct = NA, bulk_density_g_cm3 = NA))
  expect_equal(soilKey:::.cst_histosol_tiers(.cp(h2))$value, 90)
})


# --- lithologic characters ---------------------------------------------------------------

test_that("lithologic characters: the seven kinds", {
  skip_on_cran()
  # alluvial: stratification within 50 cm and an irregular carbon; floods
  al <- data.frame(top_cm = c(0, 20, 50, 90), bottom_cm = c(20, 50, 90, 130), designation = c("Ap", "C1", "2C2", "3C3"),
                   stratification_pattern = c(NA, "continuous", NA, NA), oc_pct = c(1.2, 0.4, 0.6, 0.3))
  r <- .cf("lc_alluvial_cst", .cp(al, list(flood_frequency = "annual")))
  expect_true(r$passed); expect_equal(r$evidence$kind, "alluvial"); expect_false(r$evidence$provisional)
  r <- .cf("lc_alluvial_cst", .cp(al))
  expect_true(r$passed); expect_true(r$evidence$provisional)
  expect_false(.cf("lc_alluvial_cst", .cp(al, list(flood_frequency = "none")))$passed)
  al2 <- al; al2$oc_pct <- c(1.2, 0.6, 0.4, 0.1)
  expect_false(.cf("lc_alluvial_cst", .cp(al2, list(flood_frequency = "annual")))$passed)
  al2$oc_pct[4] <- 0.2
  expect_true(.cf("lc_alluvial_cst", .cp(al2, list(flood_frequency = "annual")))$passed)
  # sandy: loamy sand or coarser to 100 cm, single grain, OC 1.5 g/kg or less
  sd <- data.frame(top_cm = 0, bottom_cm = 120, designation = "C", sand_pct = 92, clay_pct = 3, silt_pct = 5,
                   structure_type = "single grain", oc_pct = 0.08)
  expect_true(.cf("lc_sandy_cst", .cp(sd))$passed)
  sd$oc_pct <- 0.2
  expect_false(.cf("lc_sandy_cst", .cp(sd))$passed)
  sd$oc_pct <- 0.08; sd$structure_type <- NA
  expect_true(is.na(.cf("lc_sandy_cst", .cp(sd))$passed))
  # loess: 10YR 7/4 dry, silty, calcareous
  ls <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 150), designation = c("A", "C"),
                   munsell_hue_moist = c("10YR", "10YR"), munsell_value_dry = c(5, 7), munsell_chroma_dry = c(3, 4),
                   silt_pct = c(60, 66), clay_pct = c(18, 16), sand_pct = c(22, 18), caco3_pct = c(9, 11))
  r <- .cf("lc_loess_cst", .cp(ls))
  expect_true(r$passed); expect_equal(r$layers, 2L)
  ls$caco3_pct[2] <- 7.9
  expect_false(.cf("lc_loess_cst", .cp(ls))$passed)
  # purplish: 2.5RP-10RP, fragments under 4 cm
  pu <- data.frame(top_cm = 0, bottom_cm = 60, designation = "C", munsell_hue_moist = "5RP", rock_fragment_max_cm = 3)
  expect_true(.cf("lc_purplish_cst", .cp(pu))$passed)
  pu$rock_fragment_max_cm <- 6
  expect_false(.cf("lc_purplish_cst", .cp(pu))$passed)
  pu$rock_fragment_max_cm <- NA
  expect_true(is.na(.cf("lc_purplish_cst", .cp(pu))$passed))
  # red beds and northern red earths
  rb <- data.frame(top_cm = 0, bottom_cm = 80, designation = "Bw", munsell_hue_moist = "10R",
                   munsell_value_moist = 4, munsell_chroma_moist = 6, caco3_pct = 0)
  r <- .cf("lc_red_beds_cst", .cp(rb))
  expect_true(r$passed); expect_equal(r$evidence$red_beds_kind, "red beds")
  rb$caco3_pct <- 3
  expect_equal(.cf("lc_red_beds_cst", .cp(rb))$evidence$red_beds_kind, "red beds")
  expect_equal(.cf("lc_red_beds_cst", .cp(rb, list(parent_material = "red earth")))$evidence$red_beds_kind,
               "northern red earths")
  rb$munsell_chroma_moist <- 5
  expect_false(.cf("lc_red_beds_cst", .cp(rb))$passed)
  # carbonate rocks: a contact on limestone, BS 50 and pH 5.5 above it
  cr <- data.frame(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 90), designation = c("A", "Bw", "R"),
                   bs_pct = c(80, 90, NA), ph_h2o = c(6.8, 7.2, NA))
  expect_true(.cf("lc_carbonate_rocks_cst", .cp(cr, list(parent_material = "limestone")))$passed)
  cr$ph_h2o[2] <- 5.2
  expect_false(.cf("lc_carbonate_rocks_cst", .cp(cr, list(parent_material = "limestone")))$passed)
  expect_false(.cf("lc_carbonate_rocks_cst", .cp(cr))$passed)
  # coral sands
  co <- data.frame(top_cm = 0, bottom_cm = 60, designation = "C", sand_pct = 85, caco3_pct = 70)
  expect_true(.cf("lc_coral_sands_cst", .cp(co, list(parent_material = "coral sand")))$passed)
  co$caco3_pct <- 45
  expect_false(.cf("lc_coral_sands_cst", .cp(co, list(parent_material = "coral sand")))$passed)
  # all kinds at once
  ls$caco3_pct[2] <- 11
  r <- .cf("lithologic_characters_cst", .cp(ls))
  expect_true(r$passed); expect_equal(r$evidence$kind, "loess")
  expect_false(isTRUE(r$evidence$kinds[["coral_sands"]]))
})


# --- contacts, anthro-silting, anthroturbic -------------------------------------------------

test_that("lithic and paralithic contacts", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 40), designation = c("A", "R"))
  expect_true(.cf("lithic_contact_cst", .cp(h))$passed)
  expect_true(.cf("lithic_contact_cst", .cp(h), top_max = 30)$passed)
  expect_false(.cf("lithic_contact_cst", .cp(h), top_max = 25)$passed)
  expect_false(.cf("paralithic_contact_cst", .cp(h))$passed)
  h$designation[2] <- "Cr"
  expect_false(.cf("lithic_contact_cst", .cp(h))$passed)
  expect_false(.cf("paralithic_contact_cst", .cp(h))$passed)
  expect_true(.cf("paralithic_contact_cst", .cp(h, list(designation_system = "fao")))$passed)
  # no designation: undecided above the depth asked
  h$designation <- NA
  expect_true(is.na(.cf("lithic_contact_cst", .cp(h), top_max = 50)$passed))
  expect_false(.cf("lithic_contact_cst", .cp(h), top_max = 30)$passed)
})

test_that("anthro-silting materials and the anthroturbic layer", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("Ap", "C"),
                  silting_sieve_test = c("positive", NA))
  expect_true(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 0.6, silting_kind = "irrigation")))$passed)
  expect_false(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 0.6, silting_kind = "interception")))$passed)
  expect_true(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 12)))$passed)
  expect_true(is.na(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 0.6)))$passed))
  expect_false(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 0.4)))$passed)
  h$silting_sieve_test <- NA
  expect_false(.cf("anthro_silting_materials_cst", .cp(h, list(silting_rate_cm_yr = 12)))$passed)
  a <- data.frame(top_cm = c(0, 25, 80), bottom_cm = c(25, 80, 120), designation = c("Ap", "C", "C"),
                  foreign_fragments_pct = c(NA, 3, 10))
  r <- .cf("anthroturbic_layer_cst", .cp(a))
  expect_true(r$passed); expect_equal(r$layers, 2:3)
  a$foreign_fragments_pct <- c(10, 2.9, NA)
  expect_false(.cf("anthroturbic_layer_cst", .cp(a))$passed)
})


# --- vertic features ------------------------------------------------------------------------------

test_that("vertic features and vertic evidence", {
  skip_on_cran()
  mk <- function(clay = c(45, 50, 50), crack = c(1.5, 1.5, NA), slick = c(NA, "many", "many"), cole = c(0.08, 0.09, 0.09),
                 des = c("A", "Bss", "BCss")) {
    .cp(data.frame(top_cm = c(0, 18, 60), bottom_cm = c(18, 60, 110), designation = des, clay_pct = clay,
                   cracks_width_cm = crack, slickensides = slick, cole_value = cole))
  }
  r <- .cf("vertic_features_cst", mk())
  expect_true(r$passed); expect_equal(r$evidence$grade, "full")
  # clay under 30 % in the upper 18 cm: no features, but the evidence
  r <- .cf("vertic_features_cst", mk(clay = c(25, 50, 50)))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  expect_true(.cf("vertic_features_cst", mk(clay = c(25, 50, 50)), grade = "evidence")$passed)
  # no cracks recorded on an unploughed soil: absent
  r <- .cf("vertic_features_cst", mk(crack = c(NA, NA, NA)))
  expect_false(r$passed); expect_equal(r$evidence$grade, "none")
  # ploughed: undecided
  expect_true(is.na(.cf("vertic_features_cst", mk(crack = c(NA, NA, NA), des = c("Ap", "Bss", "BCss")))$passed))
  # slickensides few: no features, the evidence still needs only some
  r <- .cf("vertic_features_cst", mk(slick = c(NA, "few", "few")))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  # self-swallowing stands for the slickensides
  h <- data.frame(top_cm = c(0, 18, 60), bottom_cm = c(18, 60, 110), designation = c("A", "Bw", "BC"),
                  clay_pct = c(45, 50, 50), cracks_width_cm = c(1, 1, NA), humic_infillings = c(NA, TRUE, NA))
  expect_true(.cf("vertic_features_cst", .cp(h))$passed)
  # the clay undecided
  expect_true(is.na(.cf("vertic_features_cst", mk(clay = c(45, NA, 50)))$passed))
})


# --- gleyic and redoxic features -------------------------------------------------------------------

test_that("gleyic features: colours, the dipyridyl test (Q11), rH and the evidence", {
  skip_on_cran()
  mk <- function(hue = "5Y", v = 5, c = 1, childs = NA, fcn = NA, eh = NA, ph = NA, rm = NA) {
    .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bg"),
                   munsell_hue_moist = c("10YR", hue), munsell_value_moist = c(3, v), munsell_chroma_moist = c(3, c),
                   childs_test = c(NA, childs), ferricyanide_test = c(NA, fcn), eh_mv = c(NA, eh), ph_h2o = c(NA, ph),
                   reduced_matrix_pct = c(NA, rm)))
  }
  r <- .cf("gleyic_features_cst", mk(childs = "positive"))
  expect_true(r$passed); expect_false(r$evidence$provisional); expect_equal(r$layers, 2L)
  # without a test (Q11): provisional
  r <- .cf("gleyic_features_cst", mk())
  expect_true(r$passed); expect_true(r$evidence$provisional)
  expect_true("provisional" %in% r$evidence$flags)
  # a negative test: no
  expect_false(.cf("gleyic_features_cst", mk(childs = "negative"))$passed)
  expect_true(.cf("gleyic_features_cst", mk(childs = "negative", fcn = "positive"))$passed)
  # rH = Eh/29 + 2 pH: 19 or less full, over 19 to 25 evidence
  r <- .cf("gleyic_features_cst", mk(eh = 58, ph = 8.5))      # rH 19
  expect_true(r$passed); expect_false(r$evidence$provisional)
  r <- .cf("gleyic_features_cst", mk(eh = 87, ph = 8.5))      # rH 20
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  expect_true(.cf("gleyic_features_cst", mk(eh = 232, ph = 8.5), grade = "at_least_evidence")$passed)   # rH 25
  expect_false(.cf("gleyic_features_cst", mk(eh = 233, ph = 8.5), grade = "at_least_evidence")$passed)  # rH 25.03
  # the share of the reduced matrix
  r <- .cf("gleyic_features_cst", mk(childs = "positive", rm = 40))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  expect_true(.cf("gleyic_features_cst", mk(childs = "positive", rm = 51))$passed)
  expect_false(.cf("gleyic_features_cst", mk(childs = "positive", rm = 20), grade = "at_least_evidence")$passed)
  # the colours: 2.5Y chroma 3 yes, 4 no; 10YR chroma 2 yes, 3 no; 5YR chroma 1 only
  expect_true(.cf("gleyic_features_cst", mk(hue = "2.5Y", c = 3, childs = "positive"))$passed)
  expect_false(.cf("gleyic_features_cst", mk(hue = "2.5Y", c = 4, childs = "positive"))$passed)
  expect_true(.cf("gleyic_features_cst", mk(hue = "10YR", c = 2, childs = "positive"))$passed)
  expect_false(.cf("gleyic_features_cst", mk(hue = "10YR", c = 3, childs = "positive"))$passed)
  expect_false(.cf("gleyic_features_cst", mk(hue = "5YR", c = 2, childs = "positive"))$passed)
  expect_true(.cf("gleyic_features_cst", mk(hue = "N", c = 0, childs = "positive"))$passed)
  expect_true(.cf("gleyic_features_cst", mk(hue = "5GY", c = 6, childs = "positive"))$passed)
  # colour not recorded: undecided
  expect_true(is.na(.cf("gleyic_features_cst", mk(v = NA, c = NA, childs = "positive"))$passed))
  # within 10 cm: the Bg is deeper
  expect_false(.cf("gleyic_features_cst", mk(childs = "positive"), top_max = 10)$passed)
})

test_that("redoxic features: mottles, nodules, low chroma and a reduced matrix", {
  skip_on_cran()
  mk <- function(mot = NA, fn = NA, c = 4, rm = NA, oc = 0.3, site = list()) {
    .cp(data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100), designation = c("A", "Bw", "C"),
                   mottle_colors = c(NA, mot, NA), ferric_nodules_pct = c(NA, fn, NA),
                   munsell_chroma_moist = c(3, c, 4), reduced_matrix_pct = c(NA, rm, NA), oc_pct = c(1.5, oc, 0.2)), site)
  }
  expect_true(.cf("redoxic_features_cst", mk(mot = "7.5YR 5/6"))$passed)
  expect_false(.cf("redoxic_features_cst", mk(mot = "10YR 6/2"))$passed)
  expect_true(.cf("redoxic_features_cst", mk(fn = 2))$passed)
  expect_true(.cf("redoxic_features_cst", mk(rm = 20))$passed)
  expect_false(.cf("redoxic_features_cst", mk(rm = 30))$passed)
  expect_false(.cf("redoxic_features_cst", mk())$passed)
  # low chroma alone: under a saturation regime yes, without one undecided
  expect_true(.cf("redoxic_features_cst", mk(c = 2, site = list(soil_moisture_regime = "aquic")))$passed)
  expect_true(is.na(.cf("redoxic_features_cst", mk(c = 2))$passed))
  # no carbon exclusion (Hugo, 2026-11-06); a well-drained record says no
  expect_true(.cf("redoxic_features_cst", mk(c = 2, oc = 1, site = list(soil_moisture_regime = "aquic")))$passed)
  expect_false(.cf("redoxic_features_cst", mk(c = 2, site = list(drainage_class = "well drained")))$passed)
  # between dark layers (chroma 2) the chroma must be 1 or less
  h <- data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100), designation = c("A", "Bw", "C"),
                  munsell_chroma_moist = c(2, 1, 2), oc_pct = c(1.5, 0.3, 0.2))
  r <- .cl(.cp(h, list(soil_moisture_regime = "aquic")), "redoxic_features")
  expect_equal(r$full, c(FALSE, TRUE, FALSE))
  h$munsell_chroma_moist <- c(4, 2, 4)
  r <- .cl(.cp(h, list(soil_moisture_regime = "aquic")), "redoxic_features")
  expect_equal(r$full, c(FALSE, TRUE, FALSE))
})


# --- permafrost, frost-thawic features, n value --------------------------------------------------------

test_that("permafrost layer, frost-thawic features and the n value", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 120), designation = c("A", "Bw", "Cf"),
                  permafrost_temp_C = c(NA, NA, -2))
  expect_true(.cf("permafrost_layer_cst", .cp(h))$passed)
  expect_true(.cf("permafrost_layer_cst", .cp(h), top_max = 100)$passed)
  expect_false(.cf("permafrost_layer_cst", .cp(h), top_max = 50)$passed)
  h2 <- h; h2$designation[3] <- "C"; h2$permafrost_temp_C <- NA
  # frozen ground is a field observation: none in a description that reaches
  # the depth asked, undecided when it stops above (R3)
  expect_false(.cf("permafrost_layer_cst", .cp(h2))$passed)
  expect_false(.cf("permafrost_layer_cst", .cp(h2), top_max = 100)$passed)
  expect_true(is.na(.cf("permafrost_layer_cst", .cp(h2), top_max = 200)$passed))
  expect_false(.cf("permafrost_layer_cst", .cp(h2, list(soil_temperature_regime = "frigid")), top_max = 200)$passed)
  expect_true(is.na(.cf("permafrost_layer_cst", .cp(h2, list(soil_temperature_regime = "cryic")), top_max = 200)$passed))
  expect_false(.cf("permafrost_layer_cst", .cp(h2, list(soil_temperature_regime = "mesic")))$passed)
  expect_false(.cf("permafrost_layer_cst", .cp(h2, list(frost_regime = "seasonal")))$passed)
  # frost-thawic
  f <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 40), designation = c("A", "Bw"),
                  structure_type = c("granular", "lenticular"))
  expect_true(.cf("frost_thawic_features_cst", .cp(f))$passed)
  f$structure_type[2] <- "blocky"
  expect_true(is.na(.cf("frost_thawic_features_cst", .cp(f))$passed))
  expect_false(.cf("frost_thawic_features_cst", .cp(f, list(soil_temperature_regime = "thermic")))$passed)
  expect_true(.cf("frost_thawic_features_cst", .cp(f, list(freeze_thaw_days_pct = 70)))$passed)
  expect_false(.cf("frost_thawic_features_cst", .cp(f, list(freeze_thaw_days_pct = 69)))$passed)
  f$cryoturbation <- c(NA, "present")
  expect_true(.cf("frost_thawic_features_cst", .cp(f, list(freeze_thaw_days_pct = 10)))$passed)
  # n value 0.7, the fluidity class flagged
  n <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50), designation = c("A", "Cg"), n_value = c(0.5, 0.7))
  r <- .cf("n_value_cst", .cp(n))
  expect_true(r$passed); expect_equal(r$layers, 2L)
  n$n_value <- c(0.5, 0.69)
  expect_false(.cf("n_value_cst", .cp(n))$passed)
  n$n_value <- NA; n$fluidity_class <- c("non-fluid", "slightly fluid")
  r <- .cf("n_value_cst", .cp(n))
  expect_true(r$passed); expect_true(r$evidence$lower_grade)
  expect_false(.cf("n_value_cst", .cp(n), min = 1)$passed)
  n$fluidity_class <- NA
  expect_true(is.na(.cf("n_value_cst", .cp(n))$passed))
})


# --- isohumic and humic properties ----------------------------------------------------------------------

test_that("isohumic property: Rh 0.39 against 0.41 (Q9), default and measured bulk densities (Q10)", {
  skip_on_cran()
  # two layers of the same texture and density: Rh = 20 x c1 / (20 x c1 + 80 x c2)
  mk <- function(rh, bd = c(1.3, 1.3)) {
    c1 <- 2; c2 <- 20 * c1 * (1 - rh) / (80 * rh)
    .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "B"),
                   clay_pct = c(20, 20), sand_pct = c(40, 40), silt_pct = c(40, 40),
                   oc_pct = c(c1, c2), bulk_density_g_cm3 = bd))
  }
  r <- .cf("isohumic_property_cst", mk(0.39))
  expect_true(r$passed); expect_equal(unname(r$evidence$rh[["low"]]), 0.39, tolerance = 1e-6)
  expect_false(.cf("isohumic_property_cst", mk(0.41))$passed)
  # the printed 4 would pass any profile; 0.41 fails under the reading 0.4
  expect_false(.cf("isohumic_property_cst", mk(0.9))$passed)
  # default bulk densities: loam A 1.2, B 1.4 (Q10), flagged
  r <- .cf("isohumic_property_cst", mk(0.39, bd = c(NA, NA)))
  expect_true("bd_default_cst" %in% r$evidence$flags)
  expect_false(isTRUE(all.equal(unname(r$evidence$rh[["low"]]), 0.39)))
  # measured densities change the ratio: the A denser than the B
  expect_false(.cf("isohumic_property_cst", mk(0.39, bd = c(1.5, 1.2)))$passed)
  # carbon missing: undecided
  p <- mk(0.39); p$horizons$oc_pct[2] <- NA
  expect_true(is.na(.cf("isohumic_property_cst", p)$passed))
  # no designation and no density: undecided
  q <- mk(0.39, bd = c(NA, NA)); q$horizons$designation <- NA
  expect_true(is.na(.cf("isohumic_property_cst", q)$passed))
})

test_that("humic property: coatings in the B and 12 kg/m2", {
  skip_on_cran()
  mk <- function(oc = c(3, 1.2, 0.8), coat = c(NA, "common", NA), bd = c(1.1, 1.3, 1.4)) {
    .cp(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100), designation = c("A", "Bt1", "Bt2"),
                   oc_pct = oc, humus_coatings = coat, bulk_density_g_cm3 = bd, clay_pct = 40))
  }
  # 0.25 x 1100 x 0.03 + 0.35 x 1300 x 0.012 + 0.4 x 1400 x 0.008 = 8.25 + 5.46 + 4.48 = 18.19
  r <- .cf("humic_property_cst", mk())
  expect_true(r$passed)
  expect_equal(unname(r$evidence$oc_storage[["low"]]), 18.19, tolerance = 1e-6)
  expect_false(.cf("humic_property_cst", mk(oc = c(1.5, 0.6, 0.4)))$passed)
  expect_false(.cf("humic_property_cst", mk(coat = c(NA, NA, NA)))$passed)
  r <- .cf("humic_property_cst", mk(coat = c(NA, NA, NA), bd = c(NA, NA, NA)))
  expect_false(r$passed)
  # default densities (clayey A 1.3, B 1.5) flagged
  r <- .cf("humic_property_cst", mk(bd = c(NA, NA, NA)))
  expect_true(r$passed); expect_true("bd_default_cst" %in% r$evidence$flags)
  expect_true(is.na(.cf("humic_property_cst", mk(oc = c(3, NA, 0.8)))$passed))
})


# --- andic, ferric, allitic, alic -----------------------------------------------------------------------

test_that("andic property: the oxalate path and the glass path", {
  skip_on_cran()
  mk <- function(alfe = 2.4, bd = 0.8, pr = 90, glass = NA, part = NA) {
    .cp(data.frame(top_cm = 0, bottom_cm = 70, designation = "Bw",
                   al_ox_pct = alfe, fe_ox_pct = 0, bulk_density_g_cm3 = bd,
                   phosphate_retention_pct = pr, volcanic_glass_pct = glass,
                   particles_002_2mm_pct = part, oc_pct = 2))
  }
  expect_true(.cf("andic_property_cst", mk())$passed)
  expect_false(.cf("andic_property_cst", mk(bd = 0.95, glass = 0, part = 40))$passed)
  expect_false(.cf("andic_property_cst", mk(alfe = 1.9, glass = 2, part = 40))$passed)
  # glass path: Alo + 1/2 Feo 0.4 with 30 % glass; 1.2 needs 17.5 %
  expect_true(.cf("andic_property_cst", mk(alfe = 0.4, bd = 1.2, pr = 30, glass = 30, part = 40))$passed)
  expect_true(.cf("andic_property_cst", mk(alfe = 1.2, bd = 1.2, pr = 30, glass = 17.5, part = 40))$passed)
  expect_false(.cf("andic_property_cst", mk(alfe = 1.2, bd = 1.2, pr = 30, glass = 17, part = 40))$passed)
  expect_false(.cf("andic_property_cst", mk(alfe = 1.2, bd = 1.2, pr = 24, glass = 40, part = 40))$passed)
  r <- .cf("andic_property_cst", mk(alfe = NA, bd = NA, pr = NA))
  expect_true(is.na(r$passed))
})

test_that("ferric property in the whole B (Q23)", {
  skip_on_cran()
  mk <- function(hue = c("10YR", "5YR", "2.5YR"), fe = c(NA, NA, NA), des = c("A", "Bw1", "Bw2"), ft = NA) {
    .cp(data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 110), designation = des,
                   munsell_hue_moist = hue, fe_dcb_pct = fe, fe_total_pct = ft))
  }
  r <- .cf("ferric_property_cst", mk())
  expect_true(r$passed); expect_equal(r$layers, 2:3)
  # DCB 1.0 % of a total 5 % Fe2O3 (3.5 % Fe): 29 %
  expect_false(.cf("ferric_property_cst", mk(hue = c("10YR", "5YR", "7.5YR"), fe = c(NA, NA, 1.0), ft = 5))$passed)
  expect_true(.cf("ferric_property_cst", mk(hue = c("10YR", "5YR", "7.5YR"), fe = c(NA, NA, 1.0), ft = 3.5))$passed)
  expect_true(.cf("ferric_property_cst", mk(hue = c("10YR", "5YR", "7.5YR"), fe = c(NA, NA, 1.4)))$passed)
  expect_true(is.na(.cf("ferric_property_cst", mk(hue = c("10YR", "5YR", "7.5YR")))$passed))
  expect_false(.cf("ferric_property_cst", mk(des = c("A", "C1", "C2")))$passed)
})

test_that("allitic property and the Ki stand-in (Q16)", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("A", "Bw"),
                  sio2_al2o3_clay = c(2.6, 2.0), sio2_al2o3_naoh = c(1.5, NA))
  r <- .cf("allitic_property_cst", .cp(h))
  expect_true(r$passed); expect_equal(r$layers, 2L)
  h$sio2_al2o3_clay[2] <- 2.1
  expect_true(is.na(.cf("allitic_property_cst", .cp(h))$passed))
  h$sio2_al2o3_naoh <- c(1.5, 1.2)
  # the fine-earth branch not measured: undecided; measured above 2.0: no
  expect_true(is.na(.cf("allitic_property_cst", .cp(h))$passed))
  h$sio2_sulfuric_pct <- 30; h$al2o3_sulfuric_pct <- 20
  expect_false(.cf("allitic_property_cst", .cp(h))$passed)
  h$sio2_sulfuric_pct <- NULL; h$al2o3_sulfuric_pct <- NULL
  h$sio2_al2o3_naoh <- c(1.5, 1.0)
  expect_true(.cf("allitic_property_cst", .cp(h))$passed)
  k <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 80), designation = c("A", "Bw"),
                  sio2_sulfuric_pct = c(NA, 20), al2o3_sulfuric_pct = c(NA, 20))
  r <- .cf("allitic_property_cst", .cp(k))
  expect_true(r$passed); expect_true("ki_sulfuric_substitute" %in% r$evidence$flags)
  expect_true(is.na(.cf("allitic_property_cst", .cp(k[1, ]))$passed))
})

test_that("alic property and alic evidence", {
  skip_on_cran()
  mk <- function(cec = 12, phk = 3.8, al = 6, alsat = 70, silt = 20, clay = 40) {
    .cp(data.frame(top_cm = 0, bottom_cm = 80, designation = "Bt",
                   cec_cmol = cec, clay_pct = clay, silt_pct = silt, sand_pct = 100 - silt - clay,
                   ph_kcl = phk, al_kcl_cmol = al, al_sat_pct = alsat))
  }
  # CEC 30 cmol/kg clay, Al 15 /kg clay and 50 % of the CEC, Al sat 70
  r <- .cf("alic_property_cst", mk())
  expect_true(r$passed); expect_equal(r$evidence$grade, "full")
  # pH(KCl) 4.1: not alic, but the evidence (Al sat and Al by clay)
  r <- .cf("alic_property_cst", mk(phk = 4.1))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  # CEC under 24 /kg clay: neither
  r <- .cf("alic_property_cst", mk(cec = 9))
  expect_equal(r$evidence$grade, "none")
  # silt/clay over 0.60 and no TRB: not alic, evidence
  r <- .cf("alic_property_cst", mk(silt = 30, clay = 40))
  expect_equal(r$evidence$grade, "evidence")
  # one evidence item met and the rest unknown: undecided
  r <- .cf("alic_property_cst", mk(phk = 3.9, al = NA, alsat = NA), grade = "at_least_evidence")
  expect_true(is.na(r$passed))
})


# --- phosphic, sodic, calcaric, base saturation, sulfidic -------------------------------------------------

test_that("phosphic property and phosphic evidence", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "B", "C"),
                  p_total_pct = c(5, 2, 0.5), caco3_pct = c(30, 45, 70))
  r <- .cl(.cp(h), "phosphic_property")
  expect_equal(r$full, c(TRUE, TRUE, FALSE)); expect_equal(r$evid, c(TRUE, TRUE, TRUE))
  expect_true(.cf("phosphic_property_cst", .cp(h))$passed)
  h$caco3_pct <- c(70, 45, 30)
  r <- .cf("phosphic_property_cst", .cp(h))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  h$p_total_pct <- c(0.14, 0.1, 0.1)
  expect_false(.cf("phosphic_property_cst", .cp(h), grade = "at_least_evidence")$passed)
  h$p_total_pct <- NA
  expect_true(is.na(.cf("phosphic_property_cst", .cp(h))$passed))
})

test_that("sodic property and sodic evidence", {
  skip_on_cran()
  mk <- function(esp, na, mg = 5, cec = 20) {
    .cp(data.frame(top_cm = 0, bottom_cm = 60, designation = "Btn",
                   esp_pct = esp, na_cmol = na, mg_cmol = mg, cec_cmol = cec))
  }
  expect_true(.cf("sodic_property_cst", mk(30, 6))$passed)
  r <- .cf("sodic_property_cst", mk(30, 1.9, mg = 1))
  expect_false(r$passed); expect_equal(r$evidence$grade, "none")
  r <- .cf("sodic_property_cst", mk(29, 5.8, mg = 2))
  expect_false(r$passed); expect_equal(r$evidence$grade, "evidence")
  expect_true(.cf("sodic_property_cst", mk(10, 2, mg = 8))$passed)       # Na + Mg 50 %
  expect_equal(.cf("sodic_property_cst", mk(4.9, 1, mg = 2))$evidence$grade, "none")
  r <- .cf("sodic_property_cst", mk(NA, NA, mg = NA, cec = NA))
  expect_true(is.na(r$passed))
})

test_that("calcaric property within 50 cm and the 20 g/kg excess", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "B", "C"),
                  caco3_pct = c(1, 2, 3))
  expect_true(.cf("calcaric_property_cst", .cp(h))$passed)
  h$caco3_pct <- c(0.5, 0.8, 3)
  expect_false(.cf("calcaric_property_cst", .cp(h))$passed)
  h$effervescence_class <- c("slight", NA, NA)
  expect_true(.cf("calcaric_property_cst", .cp(h))$passed)
  h$effervescence_class <- NA
  h$caco3_pct <- c(1, 4, 1)
  r <- .cf("calcaric_property_cst", .cp(h))
  expect_false(r$passed)
  # the increment on measured CaCO3 only (R5)
  h$caco3_pct <- c(1, NA, 1)
  expect_true(.cf("calcaric_property_cst", .cp(h))$passed)
  h$caco3_pct <- NA
  expect_true(is.na(.cf("calcaric_property_cst", .cp(h))$passed))
  h$effervescence_class <- c("strong", NA, NA)
  r <- .cf("calcaric_property_cst", .cp(h))
  expect_true(r$passed); expect_true("calcaric_by_effervescence" %in% r$evidence$flags)
  h$effervescence_class <- NA; h$ph_h2o <- c(5.8, 6.0, 6.2)
  expect_false(.cf("calcaric_property_cst", .cp(h))$passed)
})

test_that("base saturation classes in a window", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 130), designation = c("A", "B", "C"),
                  bs_pct = c(60, 50, 34))
  p <- .cp(h)
  expect_false(.cf("base_saturation_cst", p, "eutric")$passed)
  expect_true(.cf("base_saturation_cst", p, "eutric", top = 0, bottom = 60)$passed)
  expect_true(.cf("base_saturation_cst", p, "dystrophic", top = 60, bottom = 125)$passed)
  expect_true(.cf("base_saturation_cst", p, "eutrophic", top = 0, bottom = 50)$passed)
  expect_true(.cf("base_saturation_cst", p, "dystric", every = FALSE)$passed)
  h$bs_pct[2] <- NA
  expect_true(is.na(.cf("base_saturation_cst", .cp(h), "eutric", top = 0, bottom = 60)$passed))
  expect_false(.cf("base_saturation_cst", .cp(h), "eutric", top = 0, bottom = 125)$passed)
})

test_that("sulfidic materials: no number in the book, a lower grade always", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 90), designation = c("A", "Cg"),
                  sulfidic_s_pct = c(0.001, 0.5))
  r <- .cf("sulfidic_materials_cst", .cp(h))
  expect_true(r$passed); expect_true(r$evidence$lower_grade); expect_equal(r$layers, 2L)
  h$sulfidic_s_pct <- c(0.001, 0.005)
  expect_false(.cf("sulfidic_materials_cst", .cp(h))$passed)
  h$incubation_ph <- c(5, 3.5)
  expect_true(.cf("sulfidic_materials_cst", .cp(h))$passed)
  h$sulfidic_s_pct <- NA; h$incubation_ph <- NA
  expect_true(is.na(.cf("sulfidic_materials_cst", .cp(h))$passed))
})


# --- the salic horizon's EC paths (Q13) ------------------------------------------------------------------

test_that("EC paths of the salic horizon (Q13): salts, the 1:1 EC, never the paste", {
  skip_on_cran()
  mk <- function(salts = NA, ec11 = NA, ece = NA, site = list(climatic_moisture_regime = "udic")) {
    .cp(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Az", "Bz"),
                   soluble_salts_pct = salts, ec_1to1_dS_m = ec11, ec_dS_m = ece), site)
  }
  expect_true(.cf("salic_horizon_cst", mk(salts = 1.5))$passed)
  expect_true(.cf("salic_horizon_cst", mk(ec11 = 20))$passed)
  expect_false(.cf("salic_horizon_cst", mk(ec11 = 14))$passed)
  # the saturated-paste EC is never read
  expect_true(is.na(.cf("salic_horizon_cst", mk(ece = 60))$passed))
  # an arid region raises the limits
  expect_false(.cf("salic_horizon_cst", mk(ec11 = 20, site = list(climatic_moisture_regime = "aridic")))$passed)
  # unknown region: between the limits undecided
  expect_true(is.na(.cf("salic_horizon_cst", mk(salts = 1.5, site = list()))$passed))
  # the 1:1 EC of the evidence (3 dS/m elsewhere)
  expect_equal(soilKey:::.cst_grade(.cf("salic_horizon_cst", mk(ec11 = 4))), "evidence")
})


# --- WRB 2022 differences as fixtures ----------------------------------------------------------------------

test_that("fixtures of the differences from the WRB 2022", {
  skip_on_cran()
  # sodic: ESP 20 is sodic in the WRB (15), only sodic evidence in the CST
  s <- .cp(data.frame(top_cm = 0, bottom_cm = 60, designation = "Bn",
                      esp_pct = 20, na_cmol = 4, mg_cmol = 2, cec_cmol = 20))
  expect_equal(.cf("sodic_property_cst", s)$evidence$grade, "evidence")
  # isohumic: the WRB has no storage ratio; a deep humus with Rh 0.35 is isohumic
  ih <- .cp(data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "AB", "B"),
                       oc_pct = c(2, 1.6, 0.9), clay_pct = 22, sand_pct = 30, silt_pct = 48,
                       bulk_density_g_cm3 = c(1.2, 1.3, 1.4)))
  expect_true(.cf("isohumic_property_cst", ih)$passed)
  # gleyic: the WRB gleyic properties need reducing conditions; the CST
  # takes the Chinese gley colours (2.5Y 5/3) with a test
  g <- .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bg"),
                      munsell_hue_moist = c("10YR", "2.5Y"), munsell_value_moist = c(3, 5),
                      munsell_chroma_moist = c(2, 3), childs_test = c(NA, "positive")))
  expect_true(.cf("gleyic_features_cst", g)$passed)
  # alic: pH(KCl) 4.0 and Al saturation 60 % (the WRB alic has neither)
  expect_equal(soilKey:::.cst_cthr("alic", "ph_kcl_max"), 4.0)
  # andic: the KST 1996 text, BD 0.90 and P retention 85 %
  expect_equal(soilKey:::.cst_cthr("andic", "p_retention_min_pct"), 85)
  # ferric: no WRB counterpart; 5YR or DCB iron 14 g/kg in the whole B
  expect_equal(soilKey:::.cst_cthr("ferric", "fe_dcb_min_pct"), 1.4)
})


# --- the panel -----------------------------------------------------------------------------------------------

test_that("the horizon panel lists the layer characteristics", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100), designation = c("Ah", "Bg", "Btn"),
                  munsell_hue_moist = c("10YR", "5Y", "10YR"), munsell_value_moist = c(3, 5, 5),
                  munsell_chroma_moist = c(2, 1, 4), childs_test = c(NA, "positive", NA),
                  esp_pct = c(NA, NA, 12), na_cmol = c(NA, NA, 2.4), mg_cmol = c(NA, NA, 3), cec_cmol = c(NA, NA, 20),
                  oc_pct = c(2, 0.4, 0.2))
  fr <- soilKey:::.cst_identify(.cp(h))
  expect_true("characteristics" %in% names(fr))
  expect_equal(fr$characteristics[[2]], "Gleyic features")
  expect_equal(fr$characteristic_evidences[[3]], "Sodic evidence")
  expect_match(fr$evidence[2], "characteristics: Gleyic features")
  expect_false(grepl("characteristics", fr$evidence[1]))
})


# --- the review of 2026-11-06: the seven defects and Hugo's decisions -----------------------

test_that("review defects D1-D7", {
  skip_on_cran()
  # D1: a rock layer without colour no longer leaves the colour characters
  # undecided
  L <- readRDS(system.file("extdata", "leptosol_canonical.rds", package = "soilKey"))
  r <- .cf("lithologic_characters_cst", L)
  expect_false(r$passed)
  expect_false(any(is.na(r$evidence$kinds[c("loess", "purplish", "red_beds")])))
  # D2: a calcareous sediment or karst is no carbonate rock
  d2 <- function(pm) .cp(data.frame(top_cm = c(0, 60), bottom_cm = c(60, 80), designation = c("Bw", "R"),
                                    bs_pct = c(90, NA), ph_h2o = c(8, NA)), list(parent_material = pm))
  expect_false(.cf("lc_carbonate_rocks_cst", d2("calcareous loess"))$passed)
  expect_false(.cf("lc_carbonate_rocks_cst", d2("karst plateau basalt"))$passed)
  expect_true(.cf("lc_carbonate_rocks_cst", d2("dolomite"))$passed)
  # D3: the 60 cm tier needs fibric moss materials
  d3 <- .cp(data.frame(top_cm = c(0, 60, 120), bottom_cm = c(60, 120, 200), designation = c("Oe", "Oa", "Oa"),
                       oc_pct = 40, water_saturation_days = 300, fiber_content_rubbed_pct = c(50, 10, 10),
                       sphagnum_fibres_pct = c(80, NA, NA), bulk_density_g_cm3 = c(.15, .2, .2)))
  expect_equal(unname(soilKey:::.cst_histosol_tiers(d3)$tiers$surface), c(0, 30))
  # D4: frozen designations after a discontinuity numeral
  d4 <- .cp(data.frame(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100), designation = c("A", "Bw", "2Cf")))
  r <- .cf("permafrost_layer_cst", d4)
  expect_true(r$passed); expect_equal(r$layers, 3L); expect_length(r$missing, 0L)
  expect_true(.cf("permafrost_layer_cst", .cp(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80),
                                                       designation = c("A", "IICff"))))$passed)
  # D5: a scaly E is no frost-thawic feature; no designation, undecided
  d5 <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 40), designation = c("A", "E"),
                   structure_type = c("granular", "lenticular"))
  expect_false(.cf("frost_thawic_features_cst", .cp(d5, list(soil_temperature_regime = "thermic")))$passed)
  d5$designation <- NA
  expect_true(is.na(.cf("frost_thawic_features_cst", .cp(d5, list(soil_temperature_regime = "thermic")))$passed))
  # D6: the provisional mark in the panel; stand-ins are function names
  d6 <- .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bg"),
                       munsell_hue_moist = c("10YR", "5Y"), munsell_value_moist = c(3, 5),
                       munsell_chroma_moist = c(3, 1), sulfidic_s_pct = c(0.001, 0.5)))
  fr <- soilKey:::.cst_identify(d6)
  expect_true("Gleyic features (provisional)" %in% fr$characteristics[[2]])
  expect_length(fr$characteristic_stand_ins[[2]], 0L)
  expect_true(.cf("sulfidic_materials_cst", d6)$evidence$lower_grade)
  # D7: the fine-earth Ki ORed with the clay ratio
  d7 <- .cp(data.frame(top_cm = 0, bottom_cm = 60, designation = "Bw", sio2_al2o3_clay = 2.3,
                       sio2_al2o3_naoh = 1.5, sio2_sulfuric_pct = 20, al2o3_sulfuric_pct = 22))
  r <- .cf("allitic_property_cst", d7)
  expect_true(r$passed); expect_true("ki_sulfuric_substitute" %in% r$evidence$flags)
})

test_that("Hugo's decisions of the review R1-R11", {
  skip_on_cran()
  # R1: a paddy Ap 2.5Y 4/1 with 1.8 % OC under a saturation regime is redoxic
  r1 <- function(site) .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("Ap", "Bw"),
                                      munsell_hue_moist = c("2.5Y", "10YR"), munsell_value_moist = c(4, 5),
                                      munsell_chroma_moist = c(1, 4), oc_pct = c(1.8, 0.5)), site)
  expect_true(.cf("redoxic_features_cst", r1(list(soil_moisture_regime = "anthrostagnic")))$passed)
  expect_true(is.na(.cf("redoxic_features_cst", r1(list()))$passed))
  expect_false(.cf("redoxic_features_cst", r1(list(drainage_class = "well drained")))$passed)
  # R2: northern red earths by the parent material, no hue limit
  ne <- data.frame(top_cm = 0, bottom_cm = 80, designation = "Bw", munsell_hue_moist = "5YR",
                   munsell_value_moist = 5, munsell_chroma_moist = 6, caco3_pct = 3)
  r <- .cf("lc_red_beds_cst", .cp(ne, list(parent_material = "red clay")))
  expect_true(r$passed); expect_equal(r$evidence$red_beds_kind, "northern red earths")
  expect_false(.cf("lc_red_beds_cst", .cp(ne))$passed)
  ne$caco3_pct <- 0
  expect_false(.cf("lc_red_beds_cst", .cp(ne, list(parent_material = "red clay")))$passed)
  # R3: covered in the permafrost test above
  # R4: the rock not named
  r4 <- data.frame(top_cm = c(0, 60), bottom_cm = c(60, 80), designation = c("Bw", "R"), bs_pct = c(90, NA),
                   ph_h2o = c(8, NA))
  expect_true(is.na(.cf("lc_carbonate_rocks_cst", .cp(r4))$passed))
  r4b <- data.frame(top_cm = 0, bottom_cm = 130, designation = "Bw", bs_pct = 90, ph_h2o = 8, coarse_fragments_pct = 0)
  expect_false(.cf("lc_carbonate_rocks_cst", .cp(r4b))$passed)
  # R5: covered in the calcaric test above
  # R6: gley colours without a test in an E: not gleyic unless redox or
  # saturation is recorded
  r6 <- function(site = list(), mot = NA) .cp(data.frame(top_cm = c(0, 15), bottom_cm = c(15, 40), designation = c("A", "E"),
                       munsell_hue_moist = c("10YR", "10YR"), munsell_value_moist = c(3, 6),
                       munsell_chroma_moist = c(2, 1), mottle_colors = c(NA, mot)), site)
  expect_false(.cf("gleyic_features_cst", r6())$passed)
  expect_true(.cf("gleyic_features_cst", r6(list(soil_moisture_regime = "stagnic")))$passed)
  expect_true(.cf("gleyic_features_cst", r6(mot = "7.5YR 5/6"))$passed)
  # R7: Rh scaled to the contact depth (80 cm: 16 / 80), not the solum
  r7 <- .cp(data.frame(top_cm = c(0, 16, 40, 80), bottom_cm = c(16, 40, 80, 90), designation = c("A", "AB", "C", "R"),
                       oc_pct = c(2, 1.5, 1, NA), bulk_density_g_cm3 = c(1.2, 1.2, 1.2, NA)))
  r <- .cf("isohumic_property_cst", r7)
  expect_equal(unname(r$evidence$depths), c(16, 80))
  expect_equal(unname(r$evidence$rh[["low"]]), 32 / (32 + 36 + 40), tolerance = 1e-6)
  # R8: the sulfuric horizon takes a lower grade only on the WRB stand-ins
  r8 <- .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("Bj", "Cg"),
                       munsell_hue_moist = c("10YR", "5Y"), jarosite_present = c(TRUE, NA),
                       sulfate_s_pct = c(0.05, NA), ph_h2o = c(3.5, 6.5), sulfidic_s_pct = c(NA, 0.5)))
  r <- .cf("sulfuric_horizon_cst", r8)
  expect_true(r$passed); expect_false(r$evidence$lower_grade)
  # R9: no reversal of over 10 % between layers
  r9 <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "B", "C"),
                   p_total_pct = c(5, 5.4, 1), caco3_pct = c(30, 29, 70))
  expect_true(.cf("phosphic_property_cst", .cp(r9))$passed)
  r9$p_total_pct[2] <- 5.6
  expect_false(.cf("phosphic_property_cst", .cp(r9))$passed)
  # R10: the sandy carbon as the weighted mean of 0-100 cm
  r10 <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 120), designation = c("A", "C"), sand_pct = 92,
                    clay_pct = 3, silt_pct = 5, structure_type = "single grain", oc_pct = c(0.5, 0.1))
  expect_true(.cf("lc_sandy_cst", .cp(r10))$passed)          # (10 x 0.5 + 90 x 0.1) / 100 = 0.14
  r10$oc_pct[1] <- 0.7
  expect_false(.cf("lc_sandy_cst", .cp(r10))$passed)
  # R11: fragments under 4 cm in every purplish layer
  r11 <- data.frame(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("Bw", "C"),
                    munsell_hue_moist = c("5RP", "7.5RP"), rock_fragment_max_cm = c(2, 6))
  expect_false(.cf("lc_purplish_cst", .cp(r11))$passed)
  r11$rock_fragment_max_cm[2] <- 3
  expect_true(.cf("lc_purplish_cst", .cp(r11))$passed)
})
