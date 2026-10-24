# v0.9.281: suborders of the Australian Soil Classification for the
# Organosols (the dominant peat), Podosols (saturation of the B, sulfuric and
# hypersulfidic material), Hydrosols (settings, mottling) and Calcarosols
# (gypsum, carbonate classes, the calcareous horizon).

.mk_asc2 <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("the examples of the four orders key to a decided suborder", {
  skip_on_cran()
  want <- c(organosols = "Sapric Organosol", podosols = "Aeric Podosol",
            hydrosols = "Redoxic Hydrosol", calcarosols = "Hypercalcic Calcarosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    # since 0.9.284 the name may go on to the great group
    expect_identical(r$levels$name[r$levels$level == "suborders"], want[[o]], info = o)
    expect_length(r$provisional_because, 0L)
  }
})

# ---- Organosols ---------------------------------------------------------------

.peat <- function(tex, tops, bots, extra = list()) .mk_asc2(utils::modifyList(list(
  designation = c(rep("P", length(tex)), "C"), top_cm = c(tops, max(bots)), bottom_cm = c(bots, max(bots) + 30),
  field_texture = c(tex, "LC"), oc_pct = c(rep(35, length(tex)), 1), clay_pct = c(rep(NA, length(tex)), 40)), extra),
  list(drainage_class = "very poorly drained"))

test_that("the kind of peat: squeeze test, rubbed fibre, field texture, designation", {
  skip_on_cran()
  x <- soilKey:::.asc_horizons(.mk_asc2(list(
    designation = c("P1", "P2", "P3", "P4", "P5", "Oe", "P6"), top_cm = seq(0, 60, 10), bottom_cm = seq(10, 70, 10),
    von_post_index = c(2, 5, 8, NA, NA, NA, NA), fiber_content_rubbed_pct = c(10, NA, NA, 80, 50, NA, 20),
    field_texture = c("IP", "IP", "IP", "AP", "AP", NA, NA), oc_pct = 30)))
  expect_identical(soilKey:::.asc_peat_types(x),
                   c("fibric", "hemic", "sapric", "fibric", NA, "hemic", "hemic"))
  # rubbed fibre of two fifths to three quarters is not sapric, whatever the
  # texture says; a fibric or hemic texture there is taken
  x <- soilKey:::.asc_horizons(.mk_asc2(list(designation = c("P1", "P2"), top_cm = c(0, 20), bottom_cm = c(20, 40),
                                             fiber_content_rubbed_pct = 50, field_texture = c("AP", "HP"), oc_pct = 30)))
  expect_identical(soilKey:::.asc_peat_types(x), c(NA, "hemic"))
  # rubbed fibre between two fifths and three quarters, with nothing else: unknown
  x <- soilKey:::.asc_horizons(.mk_asc2(list(designation = "P1", top_cm = 0, bottom_cm = 30,
                                             fiber_content_rubbed_pct = 50, oc_pct = 30)))
  expect_true(is.na(soilKey:::.asc_peat_types(x)))
})

test_that("the dominant peat by thickness; a tie goes to the more decomposed kind", {
  skip_on_cran()
  ps <- function(p, k) soilKey:::peat_suborder_asc(p, k)$passed
  p <- .peat(c("IP", "HP", "AP"), c(0, 30, 50), c(30, 50, 70))           # fibric 30, hemic 20, sapric 20
  expect_true(ps(p, "fibric")); expect_false(ps(p, "hemic")); expect_false(ps(p, "sapric"))
  expect_identical(classify_asc3(p, on_missing = "silent")$name, "Fibric Organosol")
  p <- .peat(c("IP", "AP"), c(0, 25), c(25, 50))                          # 25 and 25: no kind dominant
  expect_false(ps(p, "fibric")); expect_true(ps(p, "sapric"))
  p <- .peat(c("IP", "HP"), c(0, 25), c(25, 50))
  expect_true(ps(p, "hemic"))
  # an organic horizon of unknown kind that could change the answer
  p <- .peat(c("IP", "HP", "P?"), c(0, 30, 50), c(30, 50, 65))           # fibric 30, hemic 20, unknown 15
  expect_true(is.na(ps(p, "fibric")))
  p <- .peat(c("IP", "HP", "P?"), c(0, 40, 60), c(40, 60, 65))           # fibric 40, hemic 20, unknown 5
  expect_true(ps(p, "fibric"))
})

# ---- Podosols -----------------------------------------------------------------

.podo <- function(site = list(), extra = list()) .mk_asc2(utils::modifyList(list(
  designation = c("A1", "A2e", "B2h", "B2s", "C"), top_cm = c(0, 10, 45, 60, 90), bottom_cm = c(10, 45, 60, 90, 130),
  field_texture = c("S", "S", "LS", "S", "S"), clay_pct = c(3, 2, 5, 3, 2), oc_pct = c(2, 0.3, 1.8, 0.6, 0.1),
  ph_h2o = c(5.0, 5.3, 4.9, 5.2, 5.5)), extra), site)

test_that("the saturation of the B of a Podosol from the drainage class or the days", {
  skip_on_cran()
  # the name down to the suborder (the great groups follow since 0.9.284)
  sub <- function(p) {
    r <- classify_asc3(p, on_missing = "silent"); lv <- r$levels[r$levels$level != "great_groups", ]
    c(lv$name[nrow(lv)] %||% NA_character_,
      paste(grep("^great_groups:", r$provisional_because, value = TRUE, invert = TRUE), collapse = ","))
  }
  expect_identical(sub(.podo(list(drainage_class = "rapidly drained"))), c("Aeric Podosol", ""))
  expect_identical(sub(.podo(list(drainage_class = "moderately well drained"))), c("Semiaquic Podosol", ""))
  expect_identical(sub(.podo(list(drainage_class = "imperfectly drained"))), c("Semiaquic Podosol", ""))
  # well drained: free drainage or short saturation, undecided between them
  r <- sub(.podo(list(drainage_class = "well drained")))
  expect_identical(r, c("Podosol", "suborders:AL,suborders:EJ"))
  # poorly drained: short or long saturation
  r <- sub(.podo(list(drainage_class = "poorly drained")))
  expect_identical(r[1], "Podosol"); expect_match(r[2], "^suborders:EJ,")
  # saturation days of the B horizons decide
  p <- .podo(list(drainage_class = "well drained"), list(water_saturation_days = c(NA, NA, 120, 100, NA),
                                                       incubation_ph = c(4.9, 5.2, 5.0, 5.1, 5.3)))
  expect_identical(sub(p), c("Aquic Podosol", ""))
  # days for some B horizons only: an unrecorded one may be wetter
  p <- .podo(list(drainage_class = "very poorly drained"), list(water_saturation_days = c(NA, NA, 0, NA, NA)))
  expect_identical(soilKey:::.asc_b_saturation(p), "long")
  p <- .podo(list(drainage_class = NA), list(water_saturation_days = c(NA, NA, 30, NA, NA)))
  expect_identical(soilKey:::.asc_b_saturation(p), c("short", "long"))
  pos <- function(d) soilKey:::.asc_b_saturation(.podo(list(drainage_class = d)))
  expect_identical(pos("very poorly drained"), "long")
  expect_identical(pos(NA), c("none", "short", "long"))
})

test_that("sulfuric material at least 15 cm thick, and hypersulfidic material", {
  skip_on_cran()
  wet <- list(drainage_class = "very poorly drained")
  # pH below 4 with jarosite over 20 cm of the A2
  p <- .podo(wet, list(ph_h2o = c(5.0, 3.6, 4.9, 5.2, 5.5), jarosite_present = c(NA, TRUE, NA, NA, NA)))
  expect_true(soilKey:::acid_sulfate_ab_asc(p, "sulfuric")$passed)
  expect_identical(classify_asc3(p, on_missing = "silent")$name, "Aquic-Sulfuric Podosol")
  # only 10 cm of it
  p <- .podo(wet, list(designation = c("A1", "A2e", "B2h", "B2s", "C"), top_cm = c(0, 10, 20, 60, 90),
                       bottom_cm = c(10, 20, 60, 90, 130), ph_h2o = c(5.0, 3.6, 4.9, 5.2, 5.5),
                       jarosite_present = c(NA, TRUE, NA, NA, NA)))
  expect_false(soilKey:::acid_sulfate_ab_asc(p, "sulfuric")$passed)
  # a pH of 4 or more everywhere is not sulfuric
  expect_false(soilKey:::acid_sulfate_ab_asc(.podo(wet), "sulfuric")$passed)
  # a pH below 4 with no evidence of sulfide oxidation is untested
  p <- .podo(wet, list(ph_h2o = c(3.8, 3.7, 4.9, 5.2, 5.5)))
  expect_true(is.na(soilKey:::acid_sulfate_ab_asc(p, "sulfuric")$passed))
  # hypersulfidic: a field pH of 4 or more that falls below 4 by 0.5 or more
  p <- .podo(wet, list(incubation_ph = c(NA, NA, 4.6, 3.2, 5.0)))
  expect_true(soilKey:::acid_sulfate_ab_asc(p, "hypersulfidic")$passed)
  expect_identical(classify_asc3(p, on_missing = "silent")$name, "Aquic-Hypersulfidic Podosol")
  p <- .podo(wet, list(incubation_ph = c(4.8, 5.0, 4.6, 4.9, 5.0)))
  expect_false(soilKey:::acid_sulfate_ab_asc(p, "hypersulfidic")$passed)
  expect_identical(classify_asc3(p, on_missing = "silent")$name, "Aquic Podosol")
  # a drop of exactly 0.5 to below 4 counts; no field pH leaves it undecided
  p <- .podo(wet, list(ph_h2o = c(5.0, 5.3, 4.1, 5.2, 5.5), incubation_ph = c(4.8, 5.0, 3.6, 4.9, 5.0)))
  expect_true(soilKey:::acid_sulfate_ab_asc(p, "hypersulfidic")$passed)
  p <- .podo(wet, list(ph_h2o = c(5.0, 5.3, NA, 5.2, 5.5), incubation_ph = c(4.8, 5.0, 3.5, 4.9, 5.0)))
  expect_true(is.na(soilKey:::acid_sulfate_ab_asc(p, "hypersulfidic")$passed))
  # a row of unknown role may not be an A or B horizon
  p <- .podo(wet, list(designation = c("A1", "A2e", "B2h", "B2s", "C", NA), top_cm = c(0, 10, 45, 60, 90, 130),
                       bottom_cm = c(10, 45, 60, 90, 130, 160), field_texture = "S", clay_pct = 2, oc_pct = 0.5,
                       ph_h2o = 5, incubation_ph = c(4.8, 5.0, 4.6, 4.9, 5.0, 3.2)))
  expect_true(is.na(soilKey:::acid_sulfate_ab_asc(p, "hypersulfidic")$passed))
  # not incubated: untested
  r <- classify_asc3(.podo(wet), on_missing = "silent")
  expect_identical(r$name, "Aquic Podosol"); expect_true("suborders:JE" %in% r$provisional_because)
})

# ---- Hydrosols ----------------------------------------------------------------

.hydro <- function(site = list(), extra = list()) .mk_asc2(utils::modifyList(list(
  designation = c("A1", "B21g", "B22g", "C"), top_cm = c(0, 15, 45, 90), bottom_cm = c(15, 45, 90, 130),
  field_texture = c("CL", "LC", "LC", "SCL"), clay_pct = c(30, 38, 40, 28), water_saturation_days = 150,
  redoximorphic_features_pct = c(0, 25, 30, 15), mottle_contrast = c(NA, "distinct", "prominent", "distinct")),
  extra), utils::modifyList(list(drainage_class = "very poorly drained", landform = "flood plain", salinised = FALSE), site))

test_that("the settings of the Hydrosols from the site record", {
  skip_on_cran()
  hs <- function(p, s) soilKey:::hydrosol_setting_asc(p, s)$passed
  nm <- function(p) classify_asc3(p, on_missing = "silent")$name
  expect_identical(nm(.hydro(list(tidal_zone = "intertidal"))), "Intertidal Hydrosol")
  expect_identical(nm(.hydro(list(landform = "supratidal flat"))), "Supratidal Hydrosol")
  expect_identical(nm(.hydro(list(landform = "playa"))), "Hypersalic Hydrosol")
  expect_identical(nm(.hydro(list(subaqueous = TRUE, landform = "lake"))), "Subaqueous Hydrosol")
  expect_identical(nm(.hydro(list(salinised = NA, surface_condition = "saline; soft"))), "Salic Hydrosol")
  # a tidal flat without the zone, or tidal flooding: the tidal zone is undecided
  p <- .hydro(list(landform = "tidal flat"))
  expect_true(is.na(hs(p, "subtidal"))); expect_true(is.na(hs(p, "intertidal")))
  expect_false(hs(p, "hypersalic"))
  p <- .hydro(list(landform = NA, tidal_inundation = TRUE))
  expect_true(is.na(hs(p, "intertidal")))
  # a playa plain: the profile may lie on the playa or beside it
  expect_true(is.na(hs(.hydro(list(landform = "playa plain")), "hypersalic")))
  # each landform field on its own: a playa element on a plain pattern is a
  # playa; a lake element on a lacustrine plain may be under water
  expect_true(hs(.hydro(list(landform = "plain", landform_element = "playa")), "hypersalic"))
  expect_true(is.na(hs(.hydro(list(landform = "lacustrine plain", landform_element = "lake")), "subaqueous")))
  # 'basalt' is not salt
  expect_false(hs(.hydro(list(salinised = NA, surface_condition = "hard setting; basalt boulders")), "salic"))
  # another landform rules the settings out
  p <- .hydro()
  for (s in c("subtidal", "subaqueous", "intertidal", "supratidal", "extratidal", "hypersalic"))
    expect_false(hs(p, s), info = s)
  expect_false(hs(p, "salic"))
  expect_identical(nm(p), "Redoxic Hydrosol")
  # a setting the site record does not mention is absent
  p <- .hydro(list(landform = NA, salinised = NA))
  for (s in c("subtidal", "subaqueous", "intertidal", "supratidal", "extratidal", "hypersalic", "salic"))
    expect_false(hs(p, s), info = s)
  r <- classify_asc3(p, on_missing = "silent")
  expect_identical(r$name, "Redoxic Hydrosol"); expect_length(r$provisional_because, 0L)
})

test_that("Redoxic and Oxyaquic Hydrosols: the major part mottled or whole coloured", {
  skip_on_cran()
  mp <- function(p, m = TRUE) soilKey:::mottled_profile_asc(p, m)$passed
  nm <- function(p) classify_asc3(p, on_missing = "silent")$name
  expect_true(mp(.hydro()))
  # faint mottles, or 10 % or less, are not a mottled horizon
  p <- .hydro(extra = list(mottle_contrast = c(NA, "faint", "faint", "faint")))
  expect_false(mp(p)); expect_true(mp(p, FALSE)); expect_identical(nm(p), "Oxyaquic Hydrosol")
  p <- .hydro(extra = list(redoximorphic_features_pct = c(0, 6, 10, 15)))
  expect_false(mp(p)); expect_true(mp(p, FALSE))
  # mottled 65 of 130 cm: half, neither more than half mottled nor whole coloured
  p <- .hydro(extra = list(designation = c("A1", "B21g", "B22g", "C"), top_cm = c(0, 15, 65, 115),
                           bottom_cm = c(15, 65, 115, 130), redoximorphic_features_pct = c(0, 30, 0, 30),
                           mottle_contrast = c(NA, "distinct", NA, "distinct")))
  expect_false(mp(p)); expect_false(mp(p, FALSE)); expect_identical(nm(p), "Hydrosol")
  # mottles not recorded: undecided
  p <- .hydro(extra = list(redoximorphic_features_pct = NA, mottle_contrast = NA))
  expect_true(is.na(mp(p))); expect_true(is.na(mp(p, FALSE)))
})

# ---- Calcarosols --------------------------------------------------------------

.calc <- function(soft, hard, extra = list(), des = c("A1", "B21k", "B22k", "BCk")) .mk_asc2(utils::modifyList(list(
  designation = des, top_cm = c(0, 12, 40, 70), bottom_cm = c(12, 40, 70, 110),
  field_texture = c("SL", "SCL", "SCL", "SL"), clay_pct = c(14, 24, 26, 18),
  effervescence_class = c("slight", "strong", "strong", "very strong"), secondary_carbonates_pct = c(0, 15, 30, 25),
  caco3_pct = c(2, 12, 25, 20), carbonate_soft_pct = soft, carbonate_hard_pct = hard, caso4_pct = 0,
  ph_h2o = c(8.2, 8.6, 8.8, 9.0)), extra))

test_that("the carbonate classes of a calcareous horizon", {
  skip_on_cran()
  nm <- function(p) classify_asc3(p, on_missing = "silent")$name
  expect_identical(nm(.calc(c(0, 5, 10, 5), c(0, 10, 60, 20))), "Lithocalcic Calcarosol")
  expect_identical(nm(.calc(c(0, 5, 10, 5), c(0, 10, 50, 20))), "Supracalcic Calcarosol")    # 50 is 20-50
  expect_identical(nm(.calc(c(0, 5, 10, 5), c(0, 10, 20, 5))), "Supracalcic Calcarosol")     # 20 too
  expect_identical(nm(.calc(c(0, 12, 22, 18), c(0, 3, 5, 5))), "Hypercalcic Calcarosol")
  p <- .calc(c(0, 8, 10, 8), c(0, 3, 5, 5), list(caco3_pct = c(2, 8, 12, 10)))
  expect_identical(nm(p), "Calcic Calcarosol")
  # hard carbonate not recorded: the classes that read it are undecided
  r <- classify_asc3(.calc(c(0, 8, 10, 8), NA, list(caco3_pct = c(2, 8, 12, 10))), on_missing = "silent")
  expect_true("suborders:DA" %in% r$provisional_because)
})

test_that("the calcareous horizon: k, or 10 % or more segregations in a B or BC", {
  skip_on_cran()
  ch <- function(p) soilKey:::.asc_calcareous_horizons(soilKey:::.asc_horizons(p))
  expect_identical(ch(.calc(c(0, 5, 10, 5), c(0, 5, 5, 5))), c(FALSE, TRUE, TRUE, TRUE))
  p <- .calc(c(0, 5, 2, 5), c(0, 7, 1, 0), list(secondary_carbonates_pct = c(0, 12, 3, 5)),
             des = c("A1", "B21", "B22", "BC"))                                  # 12, 3 and 5 %
  expect_identical(ch(p), c(FALSE, TRUE, FALSE, FALSE))
  p <- .calc(c(0, 5, 10, 5), c(0, 5, 5, 5), des = c("A1", "B21k", "B22km", "BCk"))
  expect_true(is.na(ch(p)[3]))
  # 15 % segregations recorded, their soft part unknown: still 10 % or more
  p <- .calc(c(0, NA, NA, NA), c(0, 8.5, 8.5, 8.5), list(secondary_carbonates_pct = c(0, 15, 15, 15)),
             des = c("A1", "B21", "B22", "BC"))
  expect_identical(ch(p), c(FALSE, TRUE, TRUE, TRUE))
  # undesignated rows below the description may be calcareous horizons
  p <- .calc(c(0, 0, 30, 30), c(0, 0, 5, 5), list(secondary_carbonates_pct = c(0, 0, 35, 35)),
             des = c("A1", "B21", NA, NA))
  expect_true(all(is.na(ch(p)[3:4])))
  r <- classify_asc3(p, on_missing = "silent")
  expect_true(length(r$provisional_because) > 0L)
  # ... and with only lab data and no soft carbonate recorded
  p <- .calc(c(0, 0, NA, NA), c(0, 0, 0, 0), list(secondary_carbonates_pct = c(0, 5, NA, NA)),
             des = c("A1", "B21", NA, NA))
  expect_true(all(is.na(ch(p)[3:4])))
  # less than 2 % visible carbonate recorded: not one
  p <- .calc(c(0, 0, 1, 1), c(0, 0, 0, 0), list(secondary_carbonates_pct = c(0, 0, 1, 1)),
             des = c("A1", "B21", NA, NA))
  expect_false(any(ch(p)))
})

test_that("Hypocalcic, Carbonatic and Hypergypsic Calcarosols", {
  skip_on_cran()
  cs <- function(p, k) soilKey:::carbonate_suborder_asc(p, k)$passed
  nm <- function(p) classify_asc3(p, on_missing = "silent")$name
  # slight to moderate effervescence only, no visible carbonate, little hard carbonate
  p <- .calc(0, 0, list(effervescence_class = c("slight", "moderate", "moderate", "slight"),
                        secondary_carbonates_pct = 0, caco3_pct = c(1, 3, 4, 3)),
             des = c("A1", "B21", "B22", "BC"))
  expect_true(cs(p, "hypocalcic")); expect_identical(nm(p), "Hypocalcic Calcarosol")
  # less than 2 % soft carbonate, though strongly effervescent
  p <- .calc(c(0, 1, 1, 1), c(0, 5, 5, 5), list(caco3_pct = c(1, 1.5, 1.5, 1)))
  expect_true(cs(p, "hypocalcic"))
  # calcareous throughout, more than 40 % fine-earth carbonate in the upper 0.1 m
  p <- .calc(c(30, 12, 22, 18), c(0, 3, 5, 5), list(caco3_pct = c(45, 30, 25, 20),
                                                     effervescence_class = "very strong"))
  expect_true(cs(p, "carbonatic")); expect_identical(nm(p), "Carbonatic Calcarosol")
  # a slight or moderate effervescence in the upper 0.1 m rules it out
  p <- .calc(c(0, 12, 22, 18), c(0, 3, 5, 5), list(caco3_pct = c(NA, 12, 25, 20)))
  expect_false(cs(p, "carbonatic"))
  # dominantly gypsum
  p <- .calc(c(0, 12, 22, 18), c(0, 3, 5, 5), list(caso4_pct = c(20, 60, 70, 65)))
  expect_true(cs(p, "hypergypsic")); expect_identical(nm(p), "Hypergypsic Calcarosol")
  # no gypsum recorded: a soil made of gypsum crystals is not
  p <- .calc(c(0, 12, 22, 18), c(0, 3, 5, 5), list(caso4_pct = NA))
  expect_false(cs(p, "hypergypsic"))
})

# ---- NatSoil -------------------------------------------------------------------

test_that("NatSoil carbonate segregations, calcrete, mottles and the landform", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  hz <- function(master, subdiv, suffix, top, bottom, extra = list()) c(list(
    h_desig_master = master, h_desig_subdiv = subdiv, h_desig_suffix = suffix,
    h_depth_upper = top, h_depth_lower = bottom, h_texture = "SCL",
    z_colours = list(list(col_moisture_stat = "M", col_hue = "10YR", col_value = 5, col_chroma = 4))), extra)
  site <- list(agency_code = "999", proj_code = "TEST", s_id = "9", s_patt_type = "PLY",
               usage_license = "https://creativecommons.org/licenses/by/4.0/", citation = "Synthetic",
               z_observations = list(list(o_asc_ord = "CA", o_asc_subord = "CQ", o_drainage = "4",
    z_horizons = list(
      hz("A", "1", NULL, 0, 0.1, list(h_carbonate_eff = "S")),
      hz("B", "21", "k", 0.1, 0.4, list(h_carbonate_eff = "H", z_segregations = list(
        list(seg_nature = "K", seg_form = "S", seg_abun = 3), list(seg_nature = "K", seg_form = "N", seg_abun = 2)))),
      hz("B", "22", "k", 0.4, 0.7, list(z_coarse_frags = list(list(cf_lith = "KC", cf_abun = 4)),
        z_segregations = list(list(seg_nature = "F", seg_abun = 2)),
        z_mottles = list(list(mott_type = "M", mott_abun = 3, mott_contrast = "D"),
                         list(mott_type = "X", mott_abun = 4, mott_contrast = "P")))),
      # a segregation of unknown form, a fragment of unknown lithology;
      # distinct mottles of 2-10 % beside faint ones of 10-20 %
      hz("B", "23", "k", 0.7, 0.9, list(z_segregations = list(list(seg_nature = "K", seg_abun = 2)),
        z_coarse_frags = list(list(cf_abun = 1)),
        z_mottles = list(list(mott_abun = 2, mott_contrast = "D"), list(mott_abun = 3, mott_contrast = "F")))),
      # distinct mottles beside mottles of unrecorded contrast; calcrete
      # fragments of 50-90 % and no segregation record
      hz("C", NULL, NULL, 0.9, 1.1, list(z_coarse_frags = list(list(cf_lith = "KC", cf_abun = 5)),
        z_mottles = list(list(mott_abun = 2, mott_contrast = "D"), list(mott_abun = 2))))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "s.json.gz")
  con <- gzfile(f, "w"); writeLines(jsonlite::toJSON(list(site), auto_unbox = TRUE, null = "null"), con); close(con)
  p <- load_natsoil_pedons(f, verbose = FALSE)[[1]]
  h <- as.data.frame(p$horizons)
  expect_identical(p$site$landform, "playa plain")
  expect_equal(h$carbonate_soft_pct[1:3], c(NA, 15, 0))
  expect_equal(h$carbonate_hard_pct[1:3], c(NA, 6, 35))      # nodules; calcrete fragments
  expect_equal(h$redoximorphic_features_pct[3], 15)          # biological mixing is not a mottle
  expect_identical(h$mottle_contrast[3], "distinct")
  # soft 0-6 % spans two classes: unknown; hard 0-7 % stays below 20 %
  expect_true(is.na(h$carbonate_soft_pct[4])); expect_lt(h$carbonate_hard_pct[4], 20)
  expect_identical(h$mottle_contrast[4], "faint")            # beyond 10 % the mottles are faint
  expect_equal(h$redoximorphic_features_pct[4], 21)
  expect_true(is.na(h$mottle_contrast[5]))                   # 6 % distinct, 6 % of unknown contrast
  expect_true(is.na(h$carbonate_soft_pct[5]))                # segregations not recorded
  expect_gt(h$carbonate_hard_pct[5], 50)                     # 70 % calcrete at least
})
