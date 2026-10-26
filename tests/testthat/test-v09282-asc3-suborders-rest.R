# v0.9.282: suborders of the Australian Soil Classification for the
# Anthroposols, Arenosols, Rudosols and Tenosols: peaty, humose, melacic and
# melanic horizons, the conspicuously bleached A2, hard materials within
# 0.5 m, salinity, stratification, gravel, nodules and carbonate.

.mk_asc3 <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("the examples of the four orders key to a decided suborder", {
  skip_on_cran()
  want <- c(anthroposols = "Spolic Anthroposol", arenosols = "Yellow Arenosol",
            rudosols = "Leptic Rudosol", tenosols = "Brown Tenosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    expect_identical(r$levels$name[r$levels$level == "suborders"], want[[o]], info = o)
    expect_length(grep("^great_groups:", r$provisional_because, value = TRUE, invert = TRUE), 0L)   # great groups may wait (0.9.285)
  }
})

# ---- peaty, humose, melacic, melanic --------------------------------------------

.tenosol <- function(h = list(), site = list()) .mk_asc3(utils::modifyList(list(
  designation = c("A1", "A3", "C"), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
  field_texture = c("SL", "SL", "LS"), clay_pct = c(10, 12, 8), oc_pct = c(1.0, 0.5, 0.2),
  structure_grade = c("weak", "weak", "structureless"), structure_type = c("granular", "massive", "single-grain"),
  layer_origin = c(NA, NA, "aeolian"), ferric_nodules_pct = 0, bauxitic_nodules_pct = 0,
  munsell_hue_moist = "7.5YR", munsell_value_moist = c(4, 4, 5), munsell_chroma_moist = c(4, 4, 4),
  ph_h2o = c(6.0, 6.2, 6.5)), h), site)

test_that("a humose horizon: average organic C over the upper 0.2 m against the clay", {
  skip_on_cran()
  ch <- function(p) soilKey:::.asc_chernic_parts(p)$humose
  expect_true(ch(.tenosol(list(oc_pct = c(5, 0.5, 0.2)))))            # 5 % at 10 % clay (limit 4.33)
  expect_false(ch(.tenosol(list(oc_pct = c(3, 0.5, 0.2)))))
  # 60 % clay needs 6 %
  expect_false(ch(.tenosol(list(oc_pct = c(5, 0.5, 0.2), clay_pct = c(60, 60, 8), field_texture = "HC"))))
  # a black A1 of 15 cm with nothing described below may go on
  p <- .mk_asc3(list(designation = "A1", top_cm = 0, bottom_cm = 15, field_texture = "SL", clay_pct = 10, oc_pct = NA,
                     munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1,
                     munsell_value_dry = 4, munsell_chroma_dry = 1, ph_h2o = 5))
  expect_true(is.na(soilKey:::.asc_chernic_parts(p)$dark))
  # no organic C in the upper 0.2 m: undecided
  expect_true(is.na(ch(.tenosol(list(oc_pct = c(NA, 0.5, 0.2))))))
  # a shallow profile on rock cannot hold 0.2 m
  p <- .mk_asc3(list(designation = c("A1", "R"), top_cm = c(0, 15), bottom_cm = c(15, 30), oc_pct = c(5, NA),
                     field_texture = c("SL", NA), clay_pct = c(10, NA), ph_h2o = c(6, NA)))
  expect_false(ch(p))
})

test_that("melacic and melanic horizons: black, dry value 5 or less, 0.2 m", {
  skip_on_cran()
  dark <- function(p) soilKey:::.asc_chernic_parts(p)$dark
  blk <- list(munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = c(1, 2, 4),
              munsell_value_dry = c(4, 5, 7), munsell_chroma_dry = c(1, 2, 3))
  # pH below 5.5: melacic, no structure needed
  expect_true(dark(.tenosol(c(blk, list(ph_h2o = c(5.0, 5.2, 6.0))))))
  # pH 5.5 or more: melanic needs moderate or strong peds of 20 mm or less
  expect_false(dark(.tenosol(c(blk, list(ph_h2o = 6.0)))))
  p <- .tenosol(c(blk, list(ph_h2o = 6.0, structure_grade = c("moderate", "moderate", "structureless"),
                            ped_size_max_mm = c(10, 20, NA))))
  expect_true(dark(p))
  expect_false(dark(.tenosol(c(blk, list(ph_h2o = 6.0, structure_grade = c("moderate", "moderate", "structureless"),
                                         ped_size_max_mm = c(50, 50, NA))))))
  # waived for a weak dry strength
  expect_true(dark(.tenosol(c(blk, list(ph_h2o = 6.0, strength_dry = c("weak", "weak", "firm"))))))
  # no dry colour: undecided
  expect_true(is.na(dark(.tenosol(c(blk[1:3], list(ph_h2o = 5.0))))))
  # a peaty horizon
  p <- .tenosol(list(designation = c("P", "A1", "C"), oc_pct = c(30, 1, 0.2), clay_pct = c(NA, 12, 8),
                     field_texture = c("HP", "SL", "LS")), site = list(drainage_class = "very poorly drained"))
  expect_true(soilKey:::.asc_chernic_parts(p)$peaty)
})

# ---- the conspicuously bleached A2 -------------------------------------------------

test_that("the conspicuously bleached A2: e, j or the dry colour", {
  skip_on_cran()
  bl <- function(des, dv = NA, dc = NA, extra = list()) soilKey:::bleached_a2_asc(.tenosol(utils::modifyList(list(
    designation = c("A1", des, "C"), munsell_value_dry = c(5, dv, 6), munsell_chroma_dry = c(2, dc, 4),
    redoximorphic_features_pct = 0), extra)))$passed
  expect_true(bl("A2e"))
  expect_false(bl("A2j", 8, 1))                         # sporadically bleached
  expect_true(bl("A2", 7, 2))
  expect_false(bl("A2", 5, 3))
  expect_true(is.na(bl("A2")))                          # no e, no dry colour
  expect_true(is.na(bl("A2", 7, 2, list(redoximorphic_features_pct = NA))))   # mottles not recorded
  expect_false(bl("A2sb", 7, 2))                        # the older sb: sporadically bleached
  expect_false(bl("A2", 7, 2, list(redoximorphic_features_pct = c(0, 30, 0))))   # 30 % mottles
  expect_false(bl("B2", 7, 2))                          # not an A2
  # value 6 counts beside a horizon of 5YR or redder
  expect_false(bl("A2", 6, 3))
  expect_true(bl("A2", 6, 3, list(munsell_hue_moist = c("7.5YR", "7.5YR", "2.5YR"))))
  expect_true(is.na(bl("A2", 6, 3, list(munsell_hue_moist = c(NA, "7.5YR", NA)))))   # neighbours' hue unknown
  # an E of the FAO notation, by its dry colour
  p <- .tenosol(list(designation = c("A", "E", "C"), munsell_value_dry = c(5, 7, 6), munsell_chroma_dry = c(2, 2, 4),
                     redoximorphic_features_pct = 0), site = list(designation_system = "fao"))
  expect_true(soilKey:::bleached_a2_asc(p)$passed)
})

# ---- hard materials within 0.5 m ----------------------------------------------------

test_that("leptic: rock, a hard layer or saprolite within 0.5 m", {
  skip_on_cran()
  lp <- function(des, tops, bots, extra = list()) soilKey:::leptic_asc(.mk_asc3(c(list(
    designation = des, top_cm = tops, bottom_cm = bots, field_texture = "SL", clay_pct = 10), extra)))$passed
  expect_true(lp(c("A1", "A2e", "R"), c(0, 15, 40), c(15, 40, 60)))
  expect_false(lp(c("A1", "A2e", "R"), c(0, 15, 60), c(15, 60, 80)))
  expect_true(is.na(lp(c("A1", "A2e"), c(0, 15), c(15, 40))))           # stops above 0.5 m
  expect_true(is.na(lp(c("A1", "A2e"), c(0, 15), c(15, 50))))           # stops at 0.5 m
  expect_true(lp(c("A1", "C"), c(0, 30), c(30, 70), list(layer_origin = c(NA, "saprolite"))))
  expect_false(lp(c("A1", "C"), c(0, 30), c(30, 70), list(layer_origin = c(NA, "alluvium"))))
  expect_true(is.na(lp(c("A1", "C"), c(0, 30), c(30, 70))))              # saprolite or sediment
  expect_true(lp(c("A1", "B2k", "B3km"), c(0, 15, 45), c(15, 45, 60)))   # a calcrete pan
})

# ---- the Tenosol key ------------------------------------------------------------------

test_that("the Tenosol suborders in key order", {
  skip_on_cran()
  nm <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv <- lv[lv$level != "great_groups", ]; lv$name[nrow(lv)] }   # down to the suborder (0.9.285)
  blk <- list(munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 5), munsell_chroma_moist = c(1, 2, 4),
              munsell_value_dry = c(4, 5, 7), munsell_chroma_dry = c(1, 2, 3), ph_h2o = c(5.0, 5.2, 6.0))
  expect_identical(nm(.tenosol(blk)), "Chernic Tenosol")
  # over rock within 0.5 m
  p <- .tenosol(c(blk, list(designation = c("A1", "A3", "R"), top_cm = c(0, 25, 45), bottom_cm = c(25, 45, 60),
                            layer_origin = NA)))
  expect_identical(nm(p), "Chernic-Leptic Tenosol")
  # a conspicuously bleached A2: not Chernic
  p <- .tenosol(c(blk[c("munsell_hue_moist", "munsell_value_moist", "munsell_chroma_moist")],
                  list(designation = c("A1", "A2e", "C"), munsell_value_dry = c(4, 7, 7), munsell_chroma_dry = c(1, 2, 3),
                       ph_h2o = c(5.0, 5.2, 6.0))))
  expect_identical(nm(p), "Bleached Tenosol")
  # a ferric horizon over more than half of the solum
  expect_identical(nm(.tenosol(list(ferric_nodules_pct = c(25, 40, 0)))), "Sesqui-Nodular Tenosol")
  # 25 of the 60 cm of the solum: not more than half
  expect_identical(nm(.tenosol(list(ferric_nodules_pct = c(25, 0, 0)))), "Brown Tenosol")
  # an undesignated row between two solum horizons is solum
  p <- .tenosol(list(designation = c("A1", NA, "B2"), ferric_nodules_pct = 30, layer_origin = NA))
  expect_true(soilKey:::sesqui_nodular_asc(p)$passed)
  # nodules in the C are not in the solum
  expect_false(soilKey:::sesqui_nodular_asc(.tenosol(list(ferric_nodules_pct = c(0, 0, 40))))$passed)
  # more than 20 % fine-earth carbonate over 0.2 m
  p <- .tenosol(list(caco3_pct = c(2, 25, 30), effervescence_class = c("slight", "very strong", "very strong"),
                     ph_h2o = c(7.8, 8.4, 8.6)))
  expect_true(soilKey:::calcenic_asc(p)$passed)
  expect_false(soilKey:::calcenic_asc(.tenosol())$passed)         # not calcareous
  # strongly calcareous, no laboratory CaCO3, segregations of other kinds only
  p <- .tenosol(list(effervescence_class = "very strong", carbonate_soft_pct = 0, ph_h2o = 8.5))
  expect_true(is.na(soilKey:::calcenic_asc(p)$passed))
})

# ---- Arenosols and Rudosols --------------------------------------------------------

.sand <- function(h = list(), site = list()) .mk_asc3(utils::modifyList(list(
  designation = c("A1", "C1", "C2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
  field_texture = "S", clay_pct = 3, oc_pct = c(0.6, 0.2, 0.1), structure_grade = "structureless",
  structure_type = "single-grain", munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 6),
  munsell_chroma_moist = c(3, 6, 6), ph_h2o = c(6.0, 6.2, 6.4)), h), site)

test_that("salinity, gypsum and the Hypersalic water table", {
  skip_on_cran()
  ss <- function(p, k) soilKey:::saline_suborder_asc(p, k)$passed
  expect_true(ss(.sand(list(caso4_pct = c(20, 70, 80), ec_1to5_dS_m = 1)), "hypergypsic"))
  expect_false(ss(.sand(list(caso4_pct = c(20, 70, 80), ec_1to5_dS_m = 3)), "hypergypsic"))
  expect_false(ss(.sand(), "hypergypsic"))                          # no gypsum recorded
  p <- .sand(list(ec_1to5_dS_m = c(4, 3, 2.5), redoximorphic_features_pct = 0))
  expect_true(ss(p, "hypersalic"))
  expect_false(ss(.sand(list(ec_1to5_dS_m = c(4, 3, 2.5), redoximorphic_features_pct = c(0, 15, 15))), "hypersalic"))
  expect_false(ss(.sand(list(ec_1to5_dS_m = c(4, 3, 2.5), redoximorphic_features_pct = 0),
                        site = list(drainage_class = "poorly drained")), "hypersalic"))
  # the thickness counted is that within the upper 1.0 m
  p <- .sand(list(designation = c("A1", "C1", "C2"), top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 300),
                  ec_1to5_dS_m = c(5, 5, 0.5)))
  expect_identical(soilKey:::.asc_salinity(p), "high")
  # no EC: a salt crust leaves it open, none reads as not highly saline
  expect_identical(soilKey:::.asc_salinity(.sand()), "low")
  expect_true(is.na(soilKey:::.asc_salinity(.sand(site = list(surface_condition = "saline; salt crust")))))
})

test_that("Carbonatic, Calsilic and Stratic Arenosols", {
  skip_on_cran()
  ac <- function(p, k) soilKey:::arenosol_carbonate_asc(p, k)$passed
  calc <- list(effervescence_class = "very strong", ph_h2o = 8.5)
  expect_true(ac(.sand(c(calc, list(caco3_pct = c(45, 30, 20)))), "carbonatic"))
  expect_false(ac(.sand(c(calc, list(caco3_pct = c(30, 30, 20)))), "carbonatic"))
  # a non-calcareous A1 over calcareous sand: Calsilic, not Carbonatic
  p <- .sand(list(effervescence_class = c("none", "very strong", "very strong"), ph_h2o = c(6.5, 8.5, 8.6),
                  caco3_pct = c(0.2, 10, 12)))
  expect_false(ac(p, "carbonatic")); expect_true(ac(p, "calsilic"))
  lv <- classify_asc3(p, on_missing = "silent")$levels
  expect_identical(lv$name[lv$level == "suborders"], "Calsilic Arenosol")
  # described to 0.6 m only: undecided
  p <- .sand(c(calc, list(caco3_pct = c(45, 30, 20), designation = c("A1", "C1", "C2"), top_cm = c(0, 20, 40),
                          bottom_cm = c(20, 40, 60))))
  expect_true(is.na(ac(p, "carbonatic")))
  # a thick A1 does not stand in for the undescribed bottom of the window
  p <- .mk_asc3(list(designation = c("A1", "C1"), top_cm = c(0, 30), bottom_cm = c(30, 90), field_texture = "S",
                     clay_pct = 3, effervescence_class = "very strong", ph_h2o = 8.5, caco3_pct = c(5, 10)))
  expect_true(is.na(ac(p, "calsilic")))
  # a lithologic discontinuity: Stratic
  p <- .sand(list(designation = c("A1", "C1", "2C2")))
  expect_true(soilKey:::material_suborder_asc(p, "stratic")$passed)
})

test_that("Rudosol suborders: shells, carbic debris, sands, loams, layers, gravel", {
  skip_on_cran()
  shallow <- function(h = list()) .mk_asc3(utils::modifyList(list(
    designation = c("A1", "C1", "C2"), top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 70),
    field_texture = "LS", clay_pct = 6, structure_grade = "structureless", structure_type = "single-grain",
    layer_origin = "aeolian", ph_h2o = 6.5), h))
  ms <- function(p, k) soilKey:::material_suborder_asc(p, k)$passed
  ro <- function(p, k) soilKey:::rudosol_origin_asc(p, k)$passed
  expect_true(ro(shallow(list(layer_origin = c("shell sand", "shell sand", "shell sand"))), "shelly"))
  expect_false(ro(shallow(), "shelly"))
  expect_true(ro(shallow(list(layer_origin = c("charcoal debris", "carbic", "aeolian"))), "carbic"))
  expect_true(ms(shallow(), "arenic"))
  # a loam: Lutic
  p <- shallow(list(field_texture = "L", clay_pct = 20, structure_type = "massive", strength_dry = "firm"))
  expect_false(ms(p, "arenic")); expect_true(ms(p, "lutic"))
  # coherent sand: Lutic
  p <- shallow(list(structure_type = "massive", strength_dry = "firm", strength_moist = "firm"))
  expect_false(ms(p, "arenic")); expect_true(ms(p, "lutic"))
  # massive sand of weak strength in one state only: undecided
  expect_true(is.na(ms(shallow(list(structure_type = "massive", strength_dry = "weak")), "arenic")))
  # gravel
  p <- shallow(list(coarse_fragments_pct = c(5, 30, 40)))
  expect_false(ms(p, "arenic")); expect_true(ms(p, "clastic"))
  # a buried soil: Stratic
  p <- shallow(list(designation = c("A1", "C1", "2A1b")))
  expect_false(ms(p, "arenic")); expect_true(ms(p, "stratic"))
})

test_that("the Anthroposol suborders from the layer origin", {
  skip_on_cran()
  an <- function(origin, k, thick = 40, extra = list()) soilKey:::anthroposol_kind_asc(.mk_asc3(c(list(
    designation = c("C1", "2A1b"), top_cm = c(0, thick), bottom_cm = c(thick, 100), field_texture = "SL",
    clay_pct = 12, layer_origin = c(origin, NA), artefacts_pct = c(10, 0)), extra)), k)$passed
  expect_true(an("landfill refuse", "garbic")); expect_false(an("landfill refuse", "urbic"))
  expect_true(an("dredged estuarine sediment", "dredgic"))
  expect_true(an("mine spoil", "spolic"))
  expect_true(an("mill-mud", "cumulic")); expect_false(an("mill-mud", "cumulic", thick = 20))
  p <- .mk_asc3(list(designation = c("C1", "C2"), top_cm = c(0, 20), bottom_cm = c(20, NA), field_texture = "SL",
                     clay_pct = 12, layer_origin = "mill mud", artefacts_pct = 5))
  expect_true(is.na(soilKey:::anthroposol_kind_asc(p, "cumulic")$passed))      # thickness unknown
  # an origin that names no kind
  expect_true(is.na(an("fill material", "urbic")))
  # urbic artefacts alone do not tell Urbic from Spolic
  expect_true(is.na(an("fill material", "urbic", extra = list(artefacts_urbic_pct = c(10, 0)))))
})

# ---- NatSoil ---------------------------------------------------------------------------

test_that("NatSoil EC 1:5, strength, ped size and nodules", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  hz <- function(master, subdiv, top, bottom, extra = list()) c(list(
    h_desig_master = master, h_desig_subdiv = subdiv, h_depth_upper = top, h_depth_lower = bottom, h_texture = "SL",
    z_colours = list(list(col_moisture_stat = "M", col_hue = "10YR", col_value = 4, col_chroma = 3))), extra)
  site <- list(agency_code = "999", proj_code = "TEST", s_id = "8", usage_license = "https://creativecommons.org/licenses/by/4.0/",
               citation = "Synthetic", z_observations = list(list(o_asc_ord = "TE", o_asc_subord = "IL", o_drainage = "4",
    z_horizons = list(
      hz("A", "1", 0, 0.1, list(z_strengths = list(list(strg_class = "2", strg_moisture_stat = "D")),
                                z_structures = list(list(str_ped_grade = "M", str_ped_type = "SB", str_ped_size = "4")),
                                z_samples = list(list(z_lab_results = list(list(labm_code = "3A1", labr_value = 0.05)))))),
      hz("B", "2", 0.1, 0.4, list(z_strengths = list(list(strg_class = "4", strg_moisture_stat = "M")),
                                  z_segregations = list(list(seg_nature = "F", seg_form = "N", seg_abun = 3),
                                                        list(seg_nature = "N", seg_form = "C", seg_abun = 3)))),
      hz("B", "3", 0.4, 0.6, list(z_segregations = list(list(seg_nature = "F", seg_abun = 2),
                                                        list(seg_nature = "F", seg_form = "N", seg_abun = 3)))),
      # nodules of unidentified nature; two classes of 2-10 and 10-20 %
      hz("B", "31", 0.6, 0.7, list(z_segregations = list(list(seg_nature = "U", seg_form = "N", seg_abun = 4)))),
      hz("C", NULL, 0.7, 0.9, list(z_segregations = list(list(seg_nature = "F", seg_form = "N", seg_abun = 2),
                                                        list(seg_nature = "F", seg_form = "C", seg_abun = 3))))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "s.json.gz")
  con <- gzfile(f, "w"); writeLines(jsonlite::toJSON(list(site), auto_unbox = TRUE, null = "null"), con); close(con)
  h <- as.data.frame(load_natsoil_pedons(f, verbose = FALSE)[[1]]$horizons)
  expect_equal(h$ec_1to5_dS_m[1], 0.05)
  expect_identical(h$strength_dry[1], "weak"); expect_identical(h$strength_moist[2], "very firm")
  expect_equal(h$ped_size_max_mm[1], 20)
  # since 0.9.285 only what is recorded counts
  expect_true(is.na(h$ferric_nodules_pct[1]) && !is.nan(h$ferric_nodules_pct[1]))   # no segregation record
  expect_equal(h$ferric_nodules_pct[2], 30)             # 15 + 15
  expect_equal(h$ferric_nodules_pct[3], 15)             # a segregation of unknown form is not counted
  expect_equal(h$ferric_nodules_pct[4], 0)              # nor one of unidentified nature
  expect_true(is.nan(h$ferric_nodules_pct[5]))          # 12 to 30 %: either side of 20
})
