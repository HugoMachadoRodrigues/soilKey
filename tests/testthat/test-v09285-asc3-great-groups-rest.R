# v0.9.285: great groups of the Australian Soil Classification for the
# Hydrosols, Arenosols, Rudosols and Tenosols, and what the great groups share
# (acid sulfate materials, what the soil directly overlies).

.mk_g3 <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
# the name down to the great group (the subgroups follow since 0.9.289)
nm3 <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv <- lv[lv$level != "subgroups", ]; lv$name[nrow(lv)] }
lv3 <- function(p, l) { r <- classify_asc3(p, on_missing = "silent")$levels; r$name[r$level == l] }

test_that("the examples key to a great group", {
  skip_on_cran()
  want <- c(hydrosols = "Kandosolic, Redoxic Hydrosol", arenosols = "Tenic, Yellow Arenosol",
            rudosols = "Leptic Rudosol", tenosols = "Regolithic, Brown Tenosol")
  for (o in names(want)) expect_identical(nm3(make_asc3_example(o)), want[[o]], info = o)
  # the Tenosol example is decided; the Hydrosol one waits on the sulfide test
  expect_length(classify_asc3(make_asc3_example("tenosols"), on_missing = "silent")$provisional_because, 0L)
  expect_true("great_groups:EU" %in% classify_asc3(make_asc3_example("hydrosols"), on_missing = "silent")$provisional_because)
})

# a Tenosol (weak B over a C) whose substrate and horizons the tests change
.teno3 <- function(h = list(), site = list()) .mk_g3(utils::modifyList(list(
  designation = c("A1", "B2", "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 120),
  field_texture = c("LS", "SL", "LS"), clay_pct = c(7, 12, 8), fe_dcb_pct = c(0.4, 0.7, 0.3),
  boundary_distinctness = c("gradual", "gradual", NA), structure_grade = c("weak", "weak", "structureless"),
  structure_type = c("granular", "blocky", "single-grain"), layer_origin = c(NA, NA, "aeolian"),
  ferric_nodules_pct = 0, bauxitic_nodules_pct = 0, manganiferous_nodules_pct = 0,
  munsell_hue_moist = c("10YR", "7.5YR", "7.5YR"), munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(3, 4, 4),
  ph_h2o = c(6.0, 6.3, 6.5), oc_pct = c(1.2, 0.4, 0.1)), h), site)

test_that("what a Tenosol directly overlies", {
  skip_on_cran()
  sg <- function(p, k, ...) soilKey:::substrate_gg_asc(p, k, ...)$passed
  expect_identical(nm3(.teno3()), "Regolithic, Brown Tenosol")
  # rock directly under the B (at 0.6 m: not a Leptic Tenosol)
  p <- .teno3(list(designation = c("A1", "B2", "R"), layer_origin = NA))
  expect_identical(nm3(p), "Lithic, Brown Tenosol")
  # the same within 0.5 m is a Leptic Tenosol, which has its own list
  p2 <- p; p2$horizons$bottom_cm[2] <- 45; p2$horizons$top_cm[3] <- 45
  expect_identical(nm3(p2), "Lithic, Leptic Tenosol")
  # saprolite, marl
  expect_identical(nm3(.teno3(list(layer_origin = c(NA, NA, "saprolite")))), "Paralithic, Brown Tenosol")
  expect_identical(nm3(.teno3(list(layer_origin = c(NA, NA, "lacustrine marl"), field_texture = c("LS", "SL", "LC")))),
                   "Marly, Brown Tenosol")
  # a C over rock: the soil directly overlies the C, not the rock
  p <- .mk_g3(list(designation = c("A1", "B2", "C", "R"), top_cm = c(0, 15, 50, 90), bottom_cm = c(15, 50, 90, 110),
                   field_texture = c("LS", "SL", "LS", NA), clay_pct = c(7, 12, 8, NA), layer_origin = c(NA, NA, "aeolian", NA),
                   structure_grade = c("weak", "weak", "structureless", NA)))
  expect_false(sg(p, "lithic")); expect_true(sg(p, "regolithic", from = "b"))
  # a C of unrecorded origin is not hard rock, but may be saprolite or marl
  p <- .teno3(list(layer_origin = NA, field_texture = c("LS", "SL", "LC")))
  expect_false(sg(p, "lithic")); expect_true(is.na(sg(p, "paralithic"))); expect_true(is.na(sg(p, "marly")))
  # sandy material is not marl
  expect_false(sg(.teno3(list(layer_origin = NA)), "marly"))
  # pans directly under the solum: red-brown hardpan, ferricrete, silcrete, calcrete
  pan <- function(kind, des = "Cm") .teno3(list(designation = c("A1", "B2", des), pan_type = c(NA, NA, kind),
                                                 cementation_class = c(NA, NA, "strongly"), layer_origin = NA))
  expect_identical(nm3(pan("red-brown hardpan")), "Duric, Brown Tenosol")
  expect_identical(nm3(pan("ferricrete")), "Petroferric, Brown Tenosol")
  expect_identical(nm3(pan("silcrete")), "Silpanic, Brown Tenosol")
  expect_identical(nm3(pan("calcrete")), "Petrocalcic, Brown Tenosol")
  # a ferric horizon over ferricrete
  p <- pan("ferricrete"); p$horizons$ferric_nodules_pct <- c(30, 0, NA)
  expect_identical(nm3(p), "Ferric-Petroferric, Brown Tenosol")
  # a reticulate horizon (named in the record), with and without a ferric horizon
  p <- .teno3(list(layer_origin = c(NA, NA, "reticulate horizon")))
  expect_identical(nm3(p), "Reticulate, Brown Tenosol")
  # the Regolithic Tenosols need the B directly over the unconsolidated material
  p <- .teno3(list(designation = c("A1", "A3", "C"), structure_type = c("granular", "single-grain", "single-grain")))
  expect_false(sg(p, "regolithic", from = "b"))
})

test_that("Tenosol horizons: ferric, bauxitic, argic, inceptic, tephric, gritty, Arenic", {
  skip_on_cran()
  ng <- function(p, k, w = "profile") soilKey:::nodular_horizon_asc(p, k, w)$passed
  # a four-horizon Tenosol whose B21 holds the nodules (less than half of the solum: not Sesqui-Nodular)
  t4 <- function(top2, h = list()) .mk_g3(utils::modifyList(list(
    designation = c("A1", "B21", "B22", "C"), top_cm = c(0, 15, top2, 50), bottom_cm = c(15, top2, 50, 120),
    field_texture = c("LS", "SL", "SL", "LS"), clay_pct = c(7, 12, 12, 8), fe_dcb_pct = 0.5,
    structure_type = c("granular", "blocky", "blocky", "single-grain"), structure_grade = c("weak", "weak", "weak", "structureless"),
    layer_origin = c(NA, NA, NA, "aeolian"), boundary_distinctness = c("gradual", "gradual", "gradual", NA),
    ferric_nodules_pct = c(0, 30, 0, 0), bauxitic_nodules_pct = 0, manganiferous_nodules_pct = 0,
    munsell_hue_moist = c("10YR", "7.5YR", "7.5YR", "7.5YR"), munsell_value_moist = c(3, 4, 4, 5),
    munsell_chroma_moist = c(3, 4, 4, 4), ph_h2o = 6.2, oc_pct = c(1.2, 0.4, 0.3, 0.1)), h))
  p <- t4(28)
  expect_true(ng(p, "ferric")); expect_identical(nm3(p), "Ferric, Brown Tenosol")
  # a ferric horizon less than 0.1 m thick is none
  expect_false(ng(t4(22), "ferric"))
  expect_identical(nm3(t4(28, list(ferric_nodules_pct = 0, bauxitic_nodules_pct = c(0, 40, 0, 0)))), "Bauxitic, Brown Tenosol")
  # nodules not recorded are absent; an amount on either side of 20 % (NaN) is undecided
  p <- .teno3(list(ferric_nodules_pct = NA))
  expect_false(ng(p, "ferric"))
  p <- .teno3(list(ferric_nodules_pct = c(0, NaN, 0)))
  expect_true(is.na(ng(p, "ferric")))
  # an argic horizon (lamellae), an inceptic C/B, tephric materials, a gritty feel
  p <- .teno3(list(designation = c("A1", "B2", "C lamellae")))
  expect_true(soilKey:::argic_horizon_asc(p, "profile")$passed)
  expect_false(soilKey:::argic_horizon_asc(p, "b")$passed)
  expect_true(soilKey:::inceptic_asc(.teno3(list(designation = c("A1", "B2", "C/B"))))$passed)
  expect_false(soilKey:::inceptic_asc(.teno3())$passed)
  p <- .teno3(list(layer_origin = c(NA, NA, "volcanic ash")))
  expect_true(soilKey:::tephric_asc(p)$passed)
  expect_true(is.na(soilKey:::andic_great_group_asc(p)$passed))
  expect_false(soilKey:::gritty_asc(.teno3())$passed)
  p <- .teno3(list(gritty = c(TRUE, TRUE, TRUE), designation = c("A1", "B2", "R"), layer_origin = NA))
  expect_identical(nm3(p), "Gritty-Lithic, Brown Tenosol")
  # Arenic: sandy in more than 80 % of the profile, loose, no layer over 15 % clay
  ar <- soilKey:::tenosol_arenic_asc
  p <- .teno3(list(field_texture = c("S", "LS", "S"), clay_pct = c(3, 6, 3), structure_type = "single-grain",
                   structure_grade = "structureless"))
  expect_true(ar(p)$passed)
  expect_false(ar(.teno3())$passed)                  # the SL B (12 % clay) is 35 of 120 cm
  p$horizons$coarse_fragments_pct <- c(0, 25, 0); expect_false(ar(p)$passed)
})

test_that("Bleached-Leptic Tenosols: a ferric horizon in or under the A2, gritty groups", {
  skip_on_cran()
  ng <- function(p, k, w) soilKey:::nodular_horizon_asc(p, k, w)$passed
  p <- .mk_g3(list(designation = c("A1", "A2e", "B2", "R"), top_cm = c(0, 10, 25, 40), bottom_cm = c(10, 25, 40, 60),
                   field_texture = c("SL", "SL", "SL", NA), ferric_nodules_pct = c(0, 0, 30, NA)))
  expect_true(ng(p, "ferric", "a2"))
  p$horizons$ferric_nodules_pct <- c(30, 0, 0, NA)
  expect_false(ng(p, "ferric", "a2"))
  # no A2
  p <- .teno3(list(ferric_nodules_pct = c(0, 30, 0)))
  expect_false(ng(p, "ferric", "a2"))
  # an undesignated row below the B cannot be the A2; one above it may be
  p <- .mk_g3(list(designation = c("A1", "B2", NA, "R"), top_cm = c(0, 10, 30, 50), bottom_cm = c(10, 30, 50, 70),
                   field_texture = "SL", ferric_nodules_pct = c(0, 30, 0, NA)))
  expect_false(ng(p, "ferric", "a2"))
  p <- .mk_g3(list(designation = c("A1", NA, "B2", "R"), top_cm = c(0, 10, 30, 50), bottom_cm = c(10, 30, 50, 70),
                   field_texture = "SL", ferric_nodules_pct = c(0, 0, 30, NA)))
  expect_true(is.na(ng(p, "ferric", "a2")))
})

# an Arenosol (deep sand) whose lower part the tests change
.aren3 <- function(h = list(), site = list()) .mk_g3(utils::modifyList(list(
  designation = c("A1", "A3", "C1", "C2"), top_cm = c(0, 10, 40, 90), bottom_cm = c(10, 40, 90, 150),
  field_texture = c("S", "S", "S", "LS"), clay_pct = c(3, 3, 4, 6), structure_grade = "structureless",
  structure_type = "single-grain", ferric_nodules_pct = 0, bauxitic_nodules_pct = 0, manganiferous_nodules_pct = 0,
  munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 6, 7), munsell_chroma_moist = c(3, 6, 6, 6),
  ph_h2o = c(6.0, 6.2, 6.3, 6.5), oc_pct = c(0.8, 0.3, 0.1, 0.1)), h), site)

test_that("Arenosols: texture classes, hard layers below 1.0 m, carbonate", {
  skip_on_cran()
  expect_identical(nm3(.aren3()), "Tenic, Yellow Arenosol")
  expect_identical(nm3(.aren3(list(field_texture = c("S", "S", "S", "S"), clay_pct = 3))), "Palic, Yellow Arenosol")
  p <- .aren3(list(designation = c("A1", "A3", "C1", "C2"), top_cm = c(0, 10, 40, 55), bottom_cm = c(10, 40, 55, 150),
                   field_texture = c("S", "S", "SL-", "S"), clay_pct = c(3, 3, 12, 3)))
  expect_identical(nm3(p), "Kandic, Yellow Arenosol")
  # a layer beginning below 1.0 m does not make it Tenic
  p <- .aren3(list(designation = c("A1", "A3", "C1", "C2"), top_cm = c(0, 10, 40, 110), bottom_cm = c(10, 40, 110, 150),
                   field_texture = c("S", "S", "S", "LS")))
  expect_identical(nm3(p), "Palic, Yellow Arenosol")
  # rock below 1.0 m directly under the soil: Lithic
  p <- .aren3(list(designation = c("A1", "A2", "B2", "R"), top_cm = c(0, 10, 40, 110), bottom_cm = c(10, 40, 110, 130),
                   field_texture = c("S", "S", "S", NA), clay_pct = c(3, 3, 4, NA)))
  expect_true(soilKey:::substrate_gg_asc(p, "lithic", below_cm = 100)$passed)
  # the same rock at 0.6 m does not count
  p$horizons$top_cm <- c(0, 10, 40, 60); p$horizons$bottom_cm <- c(10, 40, 60, 80)
  expect_false(soilKey:::substrate_gg_asc(p, "lithic", below_cm = 100)$passed)
  # a gypsic horizon within 1.0 m
  p <- .aren3(list(gypsum_visible_pct = c(0, 30, 30, 0)))
  expect_true(soilKey:::gypsic_horizon_asc(p, 100)$passed)
  expect_false(soilKey:::gypsic_horizon_asc(.aren3(), 100)$passed)       # no gypsum recorded: none
  p <- .aren3(list(caso4_pct = c(0, 5, 5, 0)))
  expect_false(soilKey:::gypsic_horizon_asc(p, 100)$passed)               # lab gypsum 20 % or less
  p <- .aren3(list(caso4_pct = c(0, 30, 5, 0)))
  expect_true(is.na(soilKey:::gypsic_horizon_asc(p, 100)$passed))         # more: visible part unknown
  # Gravic only from a record of heavy minerals
  expect_false(soilKey:::gravic_asc(.aren3())$passed)
  expect_true(soilKey:::gravic_asc(.aren3(list(heavy_minerals_pct = 5)))$passed)
})

test_that("Calsilic Arenosols: Epibasic and Hypervescent", {
  skip_on_cran()
  cg <- function(p, k) soilKey:::calsilic_gg_asc(p, k)$passed
  p <- .aren3(list(effervescence_class = c("none", "strong", "strong", "strong")))
  expect_true(cg(p, "epibasic")); expect_false(cg(p, "hypervescent"))
  p <- .aren3(list(effervescence_class = c("strong", "strong", "strong", "strong")))
  expect_false(cg(p, "epibasic")); expect_true(cg(p, "hypervescent"))
  p <- .aren3(list(effervescence_class = c("slight", "strong", "strong", "strong"), caco3_pct = c(10, 20, 20, 20)))
  expect_true(cg(p, "hypervescent"))
  # a slight effervescence with no analysis or visual estimate leaves it open
  p <- .aren3(list(effervescence_class = c("slight", "strong", "strong", "strong")))
  expect_true(is.na(cg(p, "hypervescent")))
})

test_that("Hypersalic soils: Halic, Subhalic, the acid sulfate groups", {
  skip_on_cran()
  hg <- function(p, k) soilKey:::halite_asc(p, k)$passed
  p <- .aren3(list(halite_visible_pct = c(60, 60, 0, 0)))
  expect_true(hg(p, "halic")); expect_false(hg(p, "subhalic"))
  expect_false(hg(.aren3(), "halic")); expect_true(hg(.aren3(), "subhalic"))   # no halite recorded
  ag <- function(p, k) soilKey:::acid_sulfate_gg_asc(p, k)$passed
  p <- .aren3(list(sulfidic_s_pct = c(0.005, 0.005, 0.3, 0.3), incubation_ph = c(NA, NA, 3.2, 3.4), ph_h2o = c(6, 6, 6.5, 6.5)))
  expect_true(ag(p, "hypersulfidic")); expect_true(ag(p, "sulfidic"))
  expect_false(ag(p, "histic_hypersulfidic"))                     # no organic materials
  expect_false(ag(p, "monosulfidic_sulfuric"))
})

# a Hydrosol (very poorly drained, mottled B) for the great groups
.hydro3 <- function(h = list(), site = list()) .mk_g3(utils::modifyList(list(
  designation = c("A1", "B21g", "B22g", "C"), top_cm = c(0, 15, 45, 90), bottom_cm = c(15, 45, 90, 160),
  field_texture = c("CL", "LC", "LC", "SCL"), clay_pct = c(30, 38, 40, 28),
  boundary_distinctness = c("gradual", "gradual", "diffuse", NA), structure_grade = c("moderate", "weak", "weak", "structureless"),
  water_saturation_days = c(120, 150, 180, 200), redoximorphic_features_pct = c(0, 25, 30, 15),
  mottle_contrast = c(NA, "distinct", "prominent", "distinct"), ph_h2o = c(5.8, 6.0, 6.2, 6.5),
  oc_pct = c(3.0, 0.8, 0.5, 0.2), sulfidic_s_pct = 0.002), h),
  utils::modifyList(list(drainage_class = "poorly drained", landform = "alluvial plain", surface_condition = "firm"), site))

test_that("Hydrosols of the inland suborders: the -solic great groups", {
  skip_on_cran()
  p <- .hydro3()
  expect_identical(lv3(p, "suborders"), "Redoxic Hydrosol")
  expect_identical(nm3(p), "Kandosolic, Redoxic Hydrosol")
  # structured B2: Dermosolic
  p <- .hydro3(list(structure_grade = c("moderate", "moderate", "strong", "structureless")))
  expect_identical(nm3(p), "Dermosolic, Redoxic Hydrosol")
  # calcareous throughout with no textural B: Calcarosolic
  p <- .hydro3(list(effervescence_class = "strong", ph_h2o = 8.5, caco3_pct = 10))
  expect_identical(nm3(p), "Calcarosolic, Redoxic Hydrosol")
  # a clear textural B, strongly acid; sodic and not acid; neither
  tb <- function(ph, na) .hydro3(list(field_texture = c("SL", "MC", "MC", "LC"), clay_pct = c(12, 50, 50, 42),
                                      boundary_distinctness = c("abrupt", "gradual", "diffuse", NA), ph_h2o = ph,
                                      na_cmol = na, cec_cmol = 20))
  expect_identical(nm3(tb(5.0, 0.2)), "Kurosolic, Redoxic Hydrosol")
  expect_identical(nm3(tb(7.5, 4)), "Sodosolic, Redoxic Hydrosol")
  expect_identical(nm3(tb(7.5, 0.2)), "Chromosolic, Redoxic Hydrosol")
  # a petroferric horizon anywhere in the profile
  p <- .hydro3(list(designation = c("A1", "B21g", "B22g", "Bcm"), pan_type = c(NA, NA, NA, "ferricrete"),
                    cementation_class = c(NA, NA, NA, "indurated")))
  expect_identical(nm3(p), "Petroferric, Redoxic Hydrosol")
})

test_that("Hydrosols of the tidal and saline suborders", {
  skip_on_cran()
  hg <- function(p, k) soilKey:::hydrosol_gg_asc(p, k)$passed
  # mottled and whole coloured upper 0.5 m
  expect_true(hg(.hydro3(), "mottled") %in% c(TRUE, NA))
  p <- .hydro3(list(redoximorphic_features_pct = 0))
  expect_false(hg(p, "mottled")); expect_true(hg(p, "haplic"))
  # organic materials dominating the upper 0.5 m: Histic
  p <- .hydro3(list(designation = c("P1", "P2", "Cg1", "Cg2"), field_texture = c("HP", "AP", "LC", "LC"),
                    oc_pct = c(30, 25, 2, 1), clay_pct = c(NA, NA, 40, 40)), list(drainage_class = "very poorly drained"))
  expect_true(hg(p, "histic"))
  # faunal debris only from a record of it
  expect_false(hg(.hydro3(), "faunic"))
  expect_true(hg(.hydro3(list(layer_origin = c("shell debris", "shell debris", NA, NA))), "faunic"))
  # particle size: more than half of the fine earth
  p <- .hydro3(list(field_texture = "HC", clay_pct = c(60, 60, 60, 60)))
  expect_true(hg(p, "argillaceous")); expect_false(hg(p, "arenaceous"))
  p <- .hydro3(list(field_texture = "S", clay_pct = c(3, 3, 3, 3)))
  expect_true(hg(p, "arenaceous")); expect_false(hg(p, "lutaceous"))
  p <- .hydro3(list(field_texture = "ZL", clay_pct = c(15, 15, 15, 15), silt_pct = c(65, 65, 65, 65)))
  expect_true(hg(p, "lutaceous")); expect_false(hg(p, "arenaceous"))
  # lab clay alone does not tell sand from silt; clay and sand give the silt
  p <- .hydro3(list(field_texture = NA, clay_pct = c(6, 6, 7, 7)))
  expect_true(is.na(hg(p, "arenaceous"))); expect_true(is.na(hg(p, "lutaceous")))
  p <- .hydro3(list(field_texture = NA, clay_pct = 20, sand_pct = 10))
  expect_true(hg(p, "lutaceous"))
  # a heavy clay field texture is clay-sized
  expect_true(hg(.hydro3(list(field_texture = "HC", clay_pct = NA)), "argillaceous"))
  # Histic-Hypersulfidic
  p <- .hydro3(list(designation = c("P1", "P2", "Cg1", "Cg2"), field_texture = c("HP", "AP", "LC", "LC"),
                    oc_pct = c(30, 25, 2, 1), clay_pct = c(NA, NA, 40, 40), sulfidic_s_pct = c(0.01, 0.01, 0.5, 0.5),
                    incubation_ph = c(NA, NA, 3.2, 3.3), ph_h2o = c(5, 5, 6.5, 6.5)), list(drainage_class = "very poorly drained"))
  expect_true(soilKey:::acid_sulfate_gg_asc(p, "histic_hypersulfidic")$passed)
})

test_that("Rudosols: Shelly, Leptic and Clastic", {
  skip_on_cran()
  sg <- function(p, k) soilKey:::substrate_gg_asc(p, k)$passed
  # coquina: named, or shelly and cemented
  p <- .mk_g3(list(designation = c("A1", "C"), top_cm = c(0, 20), bottom_cm = c(20, 60), field_texture = c("S", NA),
                   layer_origin = c("shell grit", "coquina")))
  expect_true(sg(p, "coquinic"))
  p$horizons$layer_origin <- c("shell grit", "shell bed"); p$horizons$cementation_class <- c(NA, "weakly")
  expect_true(sg(p, "coquinic"))
  p$horizons$cementation_class <- NA
  expect_false(sg(p, "coquinic"))
  # Leptic Rudosol on rock directly under the A
  p <- .mk_g3(list(designation = c("A1", "R"), top_cm = c(0, 20), bottom_cm = c(20, 40), field_texture = c("LS", NA),
                   clay_pct = c(6, NA), structure_grade = c("structureless", NA), structure_type = c("single-grain", NA)))
  expect_identical(nm3(p), "Lithic, Leptic Rudosol")
  # the example: a gravelly C of unrecorded origin between the A and the rock
  r <- classify_asc3(make_asc3_example("rudosols"), on_missing = "silent")
  expect_identical(r$provisional_because, "great_groups:DU")
  # Clastic: the origin of the gravel
  cl <- function(origin) .mk_g3(list(designation = c("A1", "C"), top_cm = c(0, 10), bottom_cm = c(10, 80),
                                     field_texture = c("LS", "LS"), coarse_fragments_pct = c(30, 60), layer_origin = origin))
  expect_true(soilKey:::clastic_gg_asc(cl("scree"), "colluvic")$passed)
  expect_true(soilKey:::clastic_gg_asc(cl("alluvial gravel"), "fluvic")$passed)
  expect_false(soilKey:::clastic_gg_asc(cl(NA), "fluvic")$passed)
  expect_true(soilKey:::tephric_asc(cl("scoria"), gravel = TRUE)$passed)
})

test_that("a horizon recorded as a cemented pan is a pan whatever its designation", {
  skip_on_cran()
  sg <- function(p, k) soilKey:::substrate_gg_asc(p, k)$passed
  p <- .mk_g3(list(designation = c("A1", "A2e", "BC"), top_cm = c(0, 3, 45), bottom_cm = c(3, 45, 46),
                   field_texture = c("LS", "LS", NA), pan_type = c("none", "none", "ferricrete"),
                   cementation_class = c(NA, NA, "strongly")))
  expect_true(sg(p, "petroferric"))
  # a densipan is not cemented, and an uncemented layer of no named kind is no pan
  p$horizons$pan_type[3] <- "densipan"; p$horizons$cementation_class[3] <- NA
  expect_false(sg(p, "petroferric"))
})

test_that("cemented layers, AC horizons, petroreticulate and cemented saprolite", {
  skip_on_cran()
  sg <- function(p, k) soilKey:::substrate_gg_asc(p, k)$passed
  # a cemented C that is no pan may be hard rock
  p <- .mk_g3(list(designation = c("A1", "C"), top_cm = c(0, 20), bottom_cm = c(20, 40), field_texture = c("LS", NA),
                   cementation_class = c(NA, "indurated"), pan_type = c(NA, "none")))
  expect_true(is.na(sg(p, "lithic"))); expect_false(sg(p, "regolithic"))
  # ... and one whose origin names saprolite (saprock) is saprolite
  p$horizons$layer_origin <- c(NA, "saprock"); p$horizons$cementation_class <- c(NA, "moderately")
  expect_true(sg(p, "paralithic"))
  # an AC is part of the solum: the soil directly overlies the rock below it
  p <- .mk_g3(list(designation = c("A1", "AC", "R"), top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 50),
                   field_texture = c("LS", "LS", NA)))
  expect_true(sg(p, "lithic"))
  # a petroreticulate horizon named in the layer origin is Petroferric when the layer is cemented
  expect_true(sg(.teno3(list(layer_origin = c(NA, NA, "petroreticulate horizon"),
                             cementation_class = c(NA, NA, "indurated"))), "petroferric"))
  expect_true(is.na(sg(.teno3(list(layer_origin = c(NA, NA, "petroreticulate horizon"))), "petroferric")))
  # a recorded ferricrete in the solum with no cementation class is a pan
  expect_identical(nm3(.teno3(list(pan_type = c(NA, "ferricrete", NA)))), "Petroferric, Brown Tenosol")
})

test_that("Arenosols: reticulate and Inceptic below 1.0 m; Kandic and Tenic with a gap; solum and A2 readings", {
  skip_on_cran()
  # a reticulate horizon at 0.85 m does not count in an Arenosol
  p <- .aren3(list(designation = c("A1", "A2", "A3", "C"), top_cm = c(0, 10, 40, 85), bottom_cm = c(10, 40, 85, 150),
                   field_texture = c("S", "S", "S", "CS"), clay_pct = c(3, 3, 4, 8),
                   layer_origin = c(NA, NA, NA, "reticulate horizon")))
  expect_false(soilKey:::substrate_gg_asc(p, "reticulate", below_cm = 100)$passed)
  expect_false(grepl("Reticulate", nm3(p)))
  # a C/B at 0.5 m is not Inceptic in an Arenosol
  p <- .aren3(list(designation = c("A1", "A3", "C/B", "C"), layer_origin = c(NA, NA, NA, "saprolite")))
  expect_false(soilKey:::inceptic_asc(p, below_cm = 100)$passed)
  expect_true(soilKey:::inceptic_asc(p)$passed)
  # a gap in the upper 1.0 m may hold a loamy sand layer: Tenic undecided
  p <- .aren3(list(designation = c("A1", "A3", "C1", "C2"), top_cm = c(0, 10, 40, 80), bottom_cm = c(10, 40, 60, 150),
                   field_texture = "S", clay_pct = 3))
  expect_true(is.na(soilKey:::sand_texture_gg_asc(p, "tenic")$passed))
  # a row of unknown role between two solum horizons is solum
  p <- .mk_g3(list(designation = c("A1", NA, "B2", "C"), top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 100),
                   field_texture = "SL", ferric_nodules_pct = c(0, 30, 0, 0)))
  expect_true(soilKey:::nodular_horizon_asc(p, "ferric", "solum")$passed)
  # the A2, not an A3, contains or overlies the ferric horizon
  p <- .mk_g3(list(designation = c("A1", "A2e", "A3", "B2"), top_cm = c(0, 10, 25, 32), bottom_cm = c(10, 25, 32, 50),
                   field_texture = "SL", ferric_nodules_pct = c(0, 0, 0, 30)))
  expect_false(soilKey:::nodular_horizon_asc(p, "ferric", "a2")$passed)
})

test_that("thin pans, ferricrete named by the origin, residual cemented layers", {
  skip_on_cran()
  sg <- function(p, k) soilKey:::substrate_gg_asc(p, k)$passed
  # a thin ironpan within the B does not make the B a pan: the soil overlies the rock
  p <- .teno3(list(designation = c("A1", "B2", "R"), pan_type = c(NA, "thin ironpan", NA), layer_origin = NA))
  expect_true(sg(p, "lithic"))
  # ferricrete-derived colluvium is unconsolidated, not ferricrete; pisolitic ferricrete is ferricrete
  p <- .teno3(list(layer_origin = c(NA, NA, "colluvium derived from ferricrete")))
  expect_false(sg(p, "petroferric")); expect_true(sg(p, "regolithic"))
  expect_true(sg(.teno3(list(layer_origin = c(NA, NA, "pisolitic ferricrete"), cementation_class = c(NA, NA, "strongly"))),
                 "petroferric"))
  expect_false(sg(.teno3(list(layer_origin = c(NA, NA, "ferricrete pisoliths"))), "petroferric"))
  # a cemented ferricrete named in the solum is a pan there; a parent rock named on every row is not
  expect_true(sg(.teno3(list(layer_origin = c(NA, "ferricrete", "aeolian"), cementation_class = c(NA, "strongly", NA))),
                 "petroferric"))
  p <- .teno3(list(rock_origin = "ferricrete"))
  expect_true(soilKey:::substrate_gg_asc(p, "regolithic", from = "b")$passed)
  expect_false(sg(p, "petroferric"))
  # an uncemented B2 named "ferricrete": it may be the pan (undecided)
  expect_true(is.na(sg(.teno3(list(layer_origin = c(NA, "ferricrete", "aeolian"))), "petroferric")))
  # a thin ironpan in an uncemented aeolian C: the C is still unconsolidated material
  expect_true(sg(.teno3(list(pan_type = c(NA, NA, "thin ironpan"))), "regolithic"))
  # a cemented "other pan" is a pan, not rock
  p <- .teno3(list(pan_type = c(NA, NA, "other pan"), cementation_class = c(NA, NA, "indurated"), layer_origin = NA))
  expect_false(sg(p, "lithic"))
  # an indurated layer of "in situ" origin may be rock or saprolite
  p <- .teno3(list(layer_origin = c(NA, NA, "in situ granite"), cementation_class = c(NA, NA, "indurated")))
  expect_true(is.na(sg(p, "lithic"))); expect_true(is.na(sg(p, "paralithic")))
})
