# v0.9.383: the FAO-Unesco legend, part 3: the 27 diagnostic properties of
# the Revised Legend of 1988 (ch. V, pp. 34-41) as three-valued predicates
# <name>_fao88(), R/diagnostics-fao88-properties.R. A pass and a near miss
# per property and the printed edges of plan section 5.2.

.p88 <- function(..., site = list(id = "p")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}
.ok <- function(res) res$passed

# Three layers to 150 cm with one property column set on the middle one.
.mid <- function(..., top = c(0, 30, 80), bottom = c(30, 80, 150),
                 designation = c("A", "Bw", "C"), site = list(id = "m")) {
  base <- list(top_cm = top, bottom_cm = bottom, designation = designation,
               clay_pct = 25, silt_pct = 35, sand_pct = 40)
  a <- list(...)
  base[names(a)] <- a
  do.call(.p88, c(base, list(site = site)))
}

test_that("the 27 predicates exist and return DiagnosticResults of chapter V", {
  skip_on_cran()
  ids <- c("abrupt_textural_change", "andic_properties", "calcareous", "calcaric",
           "continuous_hard_rock", "ferralic_properties", "ferric_properties",
           "fluvic_properties", "geric_properties", "gleyic_properties", "stagnic_properties",
           "gypsiferous", "interfingering", "nitic_properties", "organic_soil_materials",
           "permafrost", "plinthite", "salic_properties", "slickensides", "smeary",
           "sodic_properties", "soft_powdery_lime", "strongly_humic", "sulfidic_materials",
           "tonguing", "vertic_properties", "weatherable_minerals")
  expect_identical(.F88_PR$id, ids)
  expect_length(ids, 27L)
  p <- .mid()
  for (id in ids) {
    r <- get(paste0(id, "_fao88"))(p)
    expect_s3_class(r, "DiagnosticResult")
    expect_identical(r$name, paste0(id, "_fao88"))
    expect_true(is.logical(r$passed) && length(r$passed) == 1L)
    expect_match(r$reference, "ch. V")
    expect_identical(r$evidence$edition, "1988")
  }
  e <- .p88(top_cm = numeric(0), bottom_cm = numeric(0))
  expect_false(.ok(calcareous_fao88(e)))
  src <- test_path("..", "..", "R", "diagnostics-fao88-properties.R")
  skip_if_not(file.exists(src))
  src <- readLines(src, warn = FALSE)
  expect_false(any(grepl("[^\\x01-\\x7F]", src, perl = TRUE)))
})

test_that("abrupt textural change: doubling below 20 %, 20 points from 20 %, less than 5 cm", {
  skip_on_cran()
  atc <- function(c1, c2, bd = "abrupt") .p88(top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("E", "Bt"), clay_pct = c(c1, c2), silt_pct = 30, sand_pct = 100 - 30 - c(c1, c2),
    boundary_distinctness = c(bd, NA))
  expect_true(.ok(abrupt_textural_change_fao88(atc(17, 34))))     # the printed example
  expect_false(.ok(abrupt_textural_change_fao88(atc(17, 33))))
  expect_true(.ok(abrupt_textural_change_fao88(atc(30, 50))))     # the printed example
  expect_false(.ok(abrupt_textural_change_fao88(atc(30, 49))))
  expect_false(.ok(abrupt_textural_change_fao88(atc(20, 39))))    # 20 % is the second item
  expect_true(.ok(abrupt_textural_change_fao88(atc(19.9, 39.8))))
  # over less than 5 cm: abrupt or clear (clara); gradual fails; not recorded
  # absent, flagged (Hugo, 2026-11-09)
  expect_true(.ok(abrupt_textural_change_fao88(atc(10, 30, "clear"))))
  expect_true(.ok(abrupt_textural_change_fao88(atc(10, 30, "Clara"))))
  expect_false(.ok(abrupt_textural_change_fao88(atc(10, 30, "gradual"))))
  r <- abrupt_textural_change_fao88(atc(10, 30, NA))
  expect_false(r$passed)
  expect_true("boundary_distinctness_not_recorded_absent" %in% r$evidence$flags)
  # across a thin transitional layer (D5): E 10 %, EB 15 % (3 cm), Bt 25 %
  thin <- function(eb_cm) .p88(top_cm = c(0, 30, 30 + eb_cm), bottom_cm = c(30, 30 + eb_cm, 80),
    designation = c("E", "EB", "Bt"), clay_pct = c(10, 15, 25), sand_pct = c(60, 55, 45), silt_pct = 30,
    boundary_distinctness = c("gradual", "gradual", NA))
  r <- abrupt_textural_change_fao88(thin(3))
  expect_true(r$passed); expect_true("change_across_thin_layer" %in% r$evidence$flags)
  expect_false(.ok(abrupt_textural_change_fao88(thin(5))))
  # a depth limit
  expect_false(.ok(abrupt_textural_change_fao88(atc(10, 30), max_top_cm = 20)))
})

test_that("andic properties: the three items and the sliding glass content", {
  skip_on_cran()
  an <- function(...) .mid(..., designation = c("A", "Bw", "C"))
  # (1) Alox + 1/2 Feox >= 2.0, BD <= 0.9, P retention > 85
  p <- an(al_ox_pct = c(1.5, 1.5, 0.1), fe_ox_pct = 1, bulk_density_g_cm3 = 0.8,
          phosphate_retention_pct = 90)
  expect_true(.ok(andic_properties_fao88(p)))
  p <- an(al_ox_pct = c(1.5, 1.5, 0.1), fe_ox_pct = 1, bulk_density_g_cm3 = 0.8,
          phosphate_retention_pct = 85, volcanic_glass_pct = 0)
  expect_false(.ok(andic_properties_fao88(p)))                    # more than 85
  p <- an(al_ox_pct = c(1.4, 1.4, 0.1), fe_ox_pct = 1, bulk_density_g_cm3 = 0.9,
          phosphate_retention_pct = 90, volcanic_glass_pct = 0)
  expect_false(.ok(andic_properties_fao88(p)))                    # 1.9 < 2.0
  # (2) more than 60 % volcaniclastic material and 0.4 %
  p <- an(pyroclastic_fragments_pct = c(61, 61, 0), al_ox_pct = 0.3, fe_ox_pct = 0.2)
  expect_true(.ok(andic_properties_fao88(p)))
  p <- an(pyroclastic_fragments_pct = c(60, 60, 0), al_ox_pct = 0.3, fe_ox_pct = 0.2,
          volcanic_glass_pct = 0)
  expect_false(.ok(andic_properties_fao88(p)))
  # (3) glass 30 % at 0.4 or less, 5 % at 2.0 or more, proportional between
  g <- function(alfe, glass) an(sand_pct = 50, silt_pct = 30, clay_pct = 20, al_ox_pct = alfe,
                                fe_ox_pct = 0, volcanic_glass_pct = glass)
  expect_true(.ok(andic_properties_fao88(g(0.3, 30))))
  expect_false(.ok(andic_properties_fao88(g(0.3, 29))))
  expect_true(.ok(andic_properties_fao88(g(2.0, 5))))
  expect_false(.ok(andic_properties_fao88(g(2.0, 4))))
  expect_true(.ok(andic_properties_fao88(g(1.2, 17.5))))          # midway: 17.5 %
  expect_false(.ok(andic_properties_fao88(g(1.2, 17))))
  # the fraction 0.02-2 mm below 30 %
  p <- an(particles_002_2mm_pct = 29, sand_pct = 20, silt_pct = 40, clay_pct = 40,
          al_ox_pct = 0.3, fe_ox_pct = 0, volcanic_glass_pct = 50)
  expect_false(.ok(andic_properties_fao88(p)))
  # no volcanic material recorded: absent, flagged; named, undecided
  r <- andic_properties_fao88(an())
  expect_false(r$passed)
  expect_true("andic_absent_no_volcanic_material" %in% r$evidence$flags)
  r <- andic_properties_fao88(.mid(site = list(id = "v", parent_material = "volcanic ash")))
  expect_true(is.na(r$passed)); expect_true("al_ox_pct" %in% r$missing)
  # throughout 0-35 cm (the Andosols)
  p <- an(al_ox_pct = c(1.5, 1.5, 0.1), fe_ox_pct = 1, bulk_density_g_cm3 = 0.8,
          phosphate_retention_pct = c(90, 90, 10))
  expect_true(.ok(andic_properties_fao88(p, window_cm = c(0, 35))))
  p <- an(al_ox_pct = c(1.5, 0.1, 0.1), fe_ox_pct = 1, bulk_density_g_cm3 = 0.8,
          phosphate_retention_pct = c(90, 10, 10), volcanic_glass_pct = 0)
  expect_false(.ok(andic_properties_fao88(p, window_cm = c(0, 35))))
})

test_that("the ferralic B (item 7) reads the andic properties, Hugo's reading kept", {
  skip_on_cran()
  fb <- function(...) .p88(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
    designation = c("A", "Bw1", "Bw2"), clay_pct = c(40, 50, 50), silt_pct = c(10, 8, 8),
    sand_pct = c(50, 42, 42), cec_cmol = c(8, 4, 4), weatherable_minerals_pct = 2,
    water_dispersible_clay_pct = 2, ...)
  expect_true(.ok(ferralic_b_fao88(fb())))
  r <- ferralic_b_fao88(fb())
  expect_true("andic_absent_no_volcanic_material" %in% r$evidence$flags)
  p <- fb(al_ox_pct = 1.5, fe_ox_pct = 1, bulk_density_g_cm3 = 0.8, phosphate_retention_pct = 90)
  expect_false(.ok(ferralic_b_fao88(p)))
})

test_that("an andic mantle of 35 to 50 cm buries the horizons below it (p. 42)", {
  skip_on_cran()
  m <- function(...) .p88(top_cm = c(0, 40, 70), bottom_cm = c(40, 70, 120),
                          designation = c("A", "Ab", "Btb"), clay_pct = c(10, 30, 40),
                          silt_pct = 30, sand_pct = c(60, 40, 30), oc_pct = c(2, 1, 0.5),
                          ec_dS_m = c(1, 20, 20), ferric_nodules_pct = c(0, 5, 5), ...)
  # not andic (no volcanic material): the buried layers stay diagnostic
  expect_true(.ok(ferric_properties_fao88(m())))
  # an andic mantle: they are not diagnostic for a horizon-bound property ...
  p <- m(al_ox_pct = c(1.5, 0, 0), fe_ox_pct = c(1, 0, 0), bulk_density_g_cm3 = c(0.8, 1.3, 1.3),
         phosphate_retention_pct = c(90, 10, 10))
  expect_false(.ok(ferric_properties_fao88(p)))
  # ... but a property of the present state is read in them (Hugo, 2026-11-09)
  expect_true(.ok(salic_properties_fao88(p, max_top_cm = 50)))
  # undecided andic mantle: undecided
  p <- m(site = list(id = "v", parent_material = "tephra"))
  expect_true(is.na(ferric_properties_fao88(p)$passed))
})

test_that("calcareous and calcaric: more than 2 % CaCO3 or strong effervescence, 20-50 cm", {
  skip_on_cran()
  expect_true(.ok(calcareous_fao88(.mid(caco3_pct = c(0, 2.1, 0)))))
  expect_false(.ok(calcareous_fao88(.mid(caco3_pct = c(0, 2, 0)))))
  # strong effervescence in most of the fine earth, flagged as 1 M HCl
  r <- calcareous_fao88(.mid(caco3_pct = c(0, 1, 0), effervescence_class = c(NA, "strong", NA)))
  expect_true(r$passed)
  expect_true("effervescence_1m_hcl_for_10pct_hcl" %in% r$evidence$flags)
  expect_false(.ok(calcareous_fao88(.mid(caco3_pct = c(0, 1, 0), effervescence_class = c(NA, "strong", NA),
                                          effervescence_extent = c(NA, "local", NA)))))
  # without CaCO3 only strong effervescence decides (Hugo, 2026-11-09)
  r <- calcareous_fao88(.mid(effervescence_class = c("none", "strong", "none")))
  expect_true(r$passed); expect_true("effervescence_1m_hcl_for_10pct_hcl" %in% r$evidence$flags)
  expect_true(.ok(calcareous_fao88(.mid(effervescence_class = c(NA, "violent", NA)))))
  for (e in c("moderate", "slight", "none"))
    expect_true(is.na(calcareous_fao88(.mid(effervescence_class = e))$passed), info = e)
  # without CaCO3 a water pH below 7 (6.5 until 0.9.404; Hugo, 2026-11-12) is
  # not calcareous, flagged; with strong effervescence too, a conflict:
  # undecided, flagged (Hugo, 2026-11-09)
  r <- calcareous_fao88(.mid(effervescence_class = "none", ph_h2o = 5))
  expect_false(r$passed); expect_true("no_carbonate_at_ph_below_7" %in% r$evidence$flags)
  expect_false(.ok(calcareous_fao88(.mid(ph_h2o = 6.4))))
  expect_false(.ok(calcareous_fao88(.mid(ph_h2o = 6.9))))
  expect_true(is.na(calcareous_fao88(.mid(ph_h2o = 7))$passed))
  r <- calcareous_fao88(.mid(effervescence_class = c(NA, "strong", NA), ph_h2o = 5))
  expect_true(is.na(r$passed))
  expect_true("strong_effervescence_at_ph_below_7_conflict" %in% r$evidence$flags)
  # a measured CaCO3 still decides whatever the pH
  expect_true(.ok(calcareous_fao88(.mid(caco3_pct = c(0, 3, 0), ph_h2o = 6))))
  r <- calcareous_fao88(.mid())
  expect_true(is.na(r$passed)); expect_true("caco3_pct" %in% r$missing)
  # calcaric: throughout 20-50 cm
  expect_true(.ok(calcaric_fao88(.mid(caco3_pct = c(5, 5, 5)))))
  expect_false(.ok(calcaric_fao88(.mid(caco3_pct = c(5, 1, 5)))))
  expect_false(.ok(calcaric_fao88(.mid(caco3_pct = c(1, 5, 5)))))  # 20-30 cm in the A
  r <- calcaric_fao88(.mid(caco3_pct = c(5, NA, 5)))
  expect_true(is.na(r$passed))
})

test_that("continuous hard rock: R layers, not pans; within a depth", {
  skip_on_cran()
  p <- .p88(top_cm = c(0, 25), bottom_cm = c(25, 60), designation = c("A", "R"),
            clay_pct = c(20, NA), silt_pct = c(30, NA), sand_pct = c(50, NA))
  expect_true(.ok(continuous_hard_rock_fao88(p, max_top_cm = 30)))
  expect_false(.ok(continuous_hard_rock_fao88(p, max_top_cm = 20)))
  pan <- .p88(top_cm = c(0, 25), bottom_cm = c(25, 60), designation = c("A", "Bkm"),
              cementation_class = c(NA, "indurated"))
  expect_false(.ok(continuous_hard_rock_fao88(pan, max_top_cm = 30)))
  nd <- .p88(top_cm = c(0, 25), bottom_cm = c(25, 60), designation = c("A", "C"),
             bedrock_pct = c(NA, 95), rock_diggable = c(NA, FALSE))
  r <- continuous_hard_rock_fao88(nd, max_top_cm = 30)
  expect_true(r$passed); expect_true("rock_from_bedrock_not_diggable" %in% r$evidence$flags)
  # a description that stops above the depth leaves it undecided
  short <- .p88(top_cm = 0, bottom_cm = 20, designation = "A")
  expect_true(is.na(continuous_hard_rock_fao88(short, max_top_cm = 30)$passed))
})

test_that("ferralic properties: CEC below 24 per kg clay or 4 per kg soil, cambic B or below the A", {
  skip_on_cran()
  fp <- function(cec, clay = 20) .p88(top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 150),
    designation = c("A", "C1", "C2"), clay_pct = c(10, clay, clay), silt_pct = 10,
    sand_pct = c(80, 90 - clay, 90 - clay), cec_cmol = c(10, cec, 10))
  expect_true(.ok(ferralic_properties_fao88(fp(3.5))))            # 17.5 per kg clay
  expect_true(.ok(ferralic_properties_fao88(fp(4.7))))            # 23.5 per kg clay
  expect_false(.ok(ferralic_properties_fao88(fp(4.8))))           # 24 per kg clay
  expect_true(.ok(ferralic_properties_fao88(fp(3.9, clay = 10)))) # 39 per clay, 3.9 per soil
  expect_false(.ok(ferralic_properties_fao88(fp(4, clay = 10))))  # 4 per soil, 40 per clay
  r <- ferralic_properties_fao88(fp(NA))
  expect_true(is.na(r$passed)); expect_true("cec_cmol" %in% r$missing)
})

test_that("ferric properties: many coarse red mottles or small nodules", {
  skip_on_cran()
  fe <- function(...) .mid(designation = c("A", "Bt", "C"), ...)
  m <- function(pct = 20, size = "coarse", col = "2.5YR 4/6") fe(redoximorphic_features_pct = c(0, pct, 0),
    mottle_size_class = c(NA, size, NA), mottle_colors = c(NA, col, NA))
  expect_true(.ok(ferric_properties_fao88(m())))
  expect_true(.ok(ferric_properties_fao88(m(15))))                 # many = 15 % or more
  expect_false(.ok(ferric_properties_fao88(m(14))))
  expect_false(.ok(ferric_properties_fao88(m(size = "medium"))))   # coarse > 20 mm
  expect_false(.ok(ferric_properties_fao88(m(col = "7.5YR 5/4")))) # not redder, chroma 4
  expect_true(.ok(ferric_properties_fao88(m(col = "10YR 5/6"))))   # chroma above 5
  r <- ferric_properties_fao88(m(col = NA))
  expect_true(is.na(r$passed))
  # nodules up to 2 cm
  expect_true(.ok(ferric_properties_fao88(fe(ferric_nodules_pct = c(0, 5, 0), nodule_size_max_cm = c(NA, 2, NA)))))
  expect_false(.ok(ferric_properties_fao88(fe(ferric_nodules_pct = c(0, 5, 0), nodule_size_max_cm = c(NA, 2.5, NA)))))
  r <- ferric_properties_fao88(fe(ferric_nodules_pct = c(0, 5, 0)))
  expect_true(r$passed); expect_true("nodule_size_not_recorded_accepted" %in% r$evidence$flags)
  # nothing recorded: absent
  expect_false(.ok(ferric_properties_fao88(fe())))
})

test_that("fluvic properties: organic carbon above 0.20 % to 125 cm, irregular, or 25 % stratified", {
  skip_on_cran()
  fl <- function(oc, site = list(id = "f", parent_material = "recent alluvium"), ...)
    .p88(top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 150), designation = c("A", "C1", "C2", "C3"),
         clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = oc, site = site, ...)
  expect_true(.ok(fluvic_properties_fao88(fl(c(1, 0.6, 0.3, 0.21)))))
  expect_false(.ok(fluvic_properties_fao88(fl(c(1, 0.6, 0.3, 0.2)))))    # not above 0.20
  expect_true(.ok(fluvic_properties_fao88(fl(c(1, 0.3, 0.6, 0.1)))))     # irregular
  # stratification in 25 % or more of 150 cm... of the 125 cm read
  st <- function(n) fl(c(1, 0.5, 0.2, 0.1), stratification_pattern = c(NA, rep("continuous", n), rep(NA, 3 - n)))
  expect_false(.ok(fluvic_properties_fao88(fl(c(1, 0.5, 0.2, 0.1)))))
  expect_true(.ok(fluvic_properties_fao88(st(1))))                        # 40 of 125 cm
  p <- fl(c(1, 0.5, 0.2, 0.1), stratification_pattern = c("continuous", NA, NA, NA))
  expect_false(.ok(fluvic_properties_fao88(p)))                           # 20 of 125 cm
  # a sand stratum with less carbon over finer sediments that meet it
  p <- .p88(top_cm = c(0, 20, 40, 100), bottom_cm = c(20, 40, 100, 150), designation = c("A", "C1", "C2", "C3"),
            clay_pct = c(20, 3, 20, 20), silt_pct = c(40, 2, 40, 40), sand_pct = c(40, 95, 40, 40),
            oc_pct = c(1, 0.1, 0.5, 0.25), site = list(id = "s", parent_material = "alluvium"))
  r <- fluvic_properties_fao88(p)
  expect_true(r$passed)
  # a recorded non-sedimentary origin: no; origin not recorded: the criteria, flagged
  expect_false(.ok(fluvic_properties_fao88(fl(c(1, 0.6, 0.3, 0.21), site = list(id = "g", parent_material = "granite")))))
  r <- fluvic_properties_fao88(fl(c(1, 0.6, 0.3, 0.21), site = list(id = "u")))
  expect_true(r$passed)
  expect_true("sediment_origin_not_recorded_criteria_decide" %in% r$evidence$flags)
})

test_that("geric properties: 1.5 cmol per kg clay, or delta pH of +0.1", {
  skip_on_cran()
  ge <- function(acid, kcl = NA, h2o = NA) .mid(designation = c("A", "Bw", "C"), clay_pct = 40,
    silt_pct = 20, sand_pct = 40, ca_cmol = 0.2, mg_cmol = 0.05, k_cmol = 0.03, na_cmol = 0.02,
    exch_acidity_kcl_cmol = acid, ph_kcl = kcl, ph_h2o = h2o)
  expect_true(.ok(geric_properties_fao88(ge(0.3))))      # 0.6 / 40 x 100 = 1.5
  expect_false(.ok(geric_properties_fao88(ge(0.31, 4.0, 4.5))))
  expect_true(.ok(geric_properties_fao88(ge(2, 5.6, 5.5))))   # delta pH +0.1
  expect_false(.ok(geric_properties_fao88(ge(2, 5.55, 5.5)))) # +0.05
  r <- geric_properties_fao88(ge(NA))
  expect_true(is.na(r$passed))
  # KCl aluminium for the acidity, flagged
  p <- .mid(clay_pct = 40, silt_pct = 20, sand_pct = 40, ca_cmol = 0.2, mg_cmol = 0.05, k_cmol = 0.03,
            na_cmol = 0.02, al_kcl_cmol = 0.2)
  r <- geric_properties_fao88(p)
  expect_true(r$passed); expect_true("al_kcl_for_kcl_acidity" %in% r$evidence$flags)
})

test_that("gleyic properties: two of three signs, each pair", {
  skip_on_cran()
  gw <- list(id = "g", groundwater_kind = "observed", groundwater_depth_cm = 60)
  dry <- list(id = "d")
  gl <- function(site = dry, hue = "10YR", value = 5, chroma = 4, test = NA)
    .p88(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150), designation = c("A", "Bg", "C"),
         clay_pct = 25, silt_pct = 35, sand_pct = 40,
         munsell_hue_moist = c("10YR", hue, "10YR"), munsell_value_moist = c(3, value, 5),
         munsell_chroma_moist = c(3, chroma, 4), ferricyanide_test = c(NA, test, NA), site = site)
  # a + b
  expect_true(.ok(gleyic_properties_fao88(gl(site = gw, test = "positive"))))
  # a + c
  expect_true(.ok(gleyic_properties_fao88(gl(hue = "5GY", chroma = 1, test = "positive"))))
  # b + c
  expect_true(.ok(gleyic_properties_fao88(gl(site = gw, hue = "5GY", chroma = 1, test = "negative"))))
  # one sign only
  expect_false(.ok(gleyic_properties_fao88(gl(test = "positive"))))
  # groundwater with a negative test and a brown matrix: the negative test
  # does not refute and the colour does not, so undecided (Hugo, 2026-11-09)
  expect_true(is.na(gleyic_properties_fao88(gl(site = gw, test = "negative"))$passed))
  # a negative test does not switch off the colour route: a + c
  expect_true(.ok(gleyic_properties_fao88(gl(hue = "5GY", chroma = 1, test = "negative"))))
  # all three tests negative and a brown matrix: no reduction, one sign
  p <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Bw"), site = gw,
            munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 4,
            rh_value = 25, ferricyanide_test = "negative", childs_test = "negative")
  expect_false(.ok(gleyic_properties_fao88(p)))
  # no test: the colour pattern counts as reduction, flagged (Q21): a + c
  r <- gleyic_properties_fao88(gl(hue = "5GY", chroma = 1))
  expect_true(r$passed); expect_true("reduction_from_colour_pattern" %in% r$evidence$flags)
  # chroma 2, value 5 (reduction by colour) with groundwater: a + b
  expect_true(.ok(gleyic_properties_fao88(gl(site = gw, chroma = 2))))
  # a dark humus-stained layer does not confirm reduction: undecided
  expect_true(is.na(gleyic_properties_fao88(gl(site = gw, value = 3, chroma = 1))$passed))
  # an E layer never takes the colour route
  pe <- .p88(top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 100), designation = c("A", "E", "Bt"), site = gw,
             munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4), munsell_chroma_moist = c(2, 1, 4))
  r <- gleyic_properties_fao88(pe)
  expect_true(is.na(r$passed)); expect_true(is.na(r$evidence$signs$reduction[2]))
  # rH = Eh / 29 + 2 pH <= 19
  rh <- function(eh) .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Bg"),
                          eh_mv = c(NA, eh), ph_h2o = 6, site = gw,
                          munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 4)
  expect_true(.ok(gleyic_properties_fao88(rh(203))))    # 7 + 12 = 19
  expect_true(is.na(gleyic_properties_fao88(rh(204))$passed))   # a negative rH alone: undecided
  # groundwater beyond the capillary fringe, or perched, is not the sign
  far <- list(id = "f", groundwater_kind = "observed", groundwater_depth_cm = 101)
  expect_false(.ok(gleyic_properties_fao88(gl(site = far, test = "positive"))))
  per <- list(id = "p", groundwater_kind = "perched", groundwater_depth_cm = 40)
  expect_false(.ok(gleyic_properties_fao88(gl(site = per, test = "positive"))))
  # gley colours with mottles inside the peds fail sign c
  p <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Bg"),
            munsell_hue_moist = c("10YR", "5G"), munsell_value_moist = c(3, 5), munsell_chroma_moist = c(2, 1),
            redoximorphic_features_pct = c(0, 10), redox_segregations_location = c(NA, "within peds"),
            ferricyanide_test = c(NA, "positive"))
  expect_false(.ok(gleyic_properties_fao88(p)))
  # within a depth (the Gleysols, 50 cm)
  expect_false(.ok(gleyic_properties_fao88(.p88(top_cm = c(0, 60), bottom_cm = c(60, 120),
    designation = c("A", "Bg"), munsell_hue_moist = c("10YR", "5GY"), munsell_value_moist = c(4, 5),
    munsell_chroma_moist = c(3, 1)), max_top_cm = 50)))
})

test_that("gleyic properties of Podzols: grains free of iron below the spodic B (item 4)", {
  skip_on_cran()
  pz <- function(ign) .p88(top_cm = c(0, 10, 20, 40), bottom_cm = c(10, 20, 40, 100),
    designation = c("A", "E", "Bhs", "C"), clay_pct = 3, silt_pct = 5, sand_pct = 92,
    cementation_class = c(NA, NA, "strongly", NA), ignition_redder = c(NA, NA, NA, ign),
    munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(3, 6, 3, 6),
    munsell_chroma_moist = c(2, 1, 4, 3), ferricyanide_test = c(NA, NA, NA, "positive"))
  r <- gleyic_properties_fao88(pz(FALSE))
  expect_true(r$passed); expect_true("podzol_grains_free_of_iron_ignition" %in% r$evidence$flags)
  expect_false(.ok(gleyic_properties_fao88(pz(TRUE))))
})

test_that("stagnic properties within 50 cm: reduction with the mottling pattern", {
  skip_on_cran()
  sg <- function(chroma = 2, mott = 10, mcol = "7.5YR 5/6", test = NA, top = 20, value = 5, ...)
    .p88(top_cm = c(0, top, 80), bottom_cm = c(top, 80, 150), designation = c("A", "Bg", "C"),
         munsell_hue_moist = "10YR", munsell_value_moist = c(3, value, 5), munsell_chroma_moist = c(3, chroma, 4),
         redoximorphic_features_pct = c(0, mott, 0), mottle_colors = c(NA, mcol, NA),
         ferricyanide_test = c(NA, test, NA), ...)
  expect_true(.ok(stagnic_properties_fao88(sg(test = "positive"))))
  # a negative test does not refute; the gley colour confirms
  expect_true(.ok(stagnic_properties_fao88(sg(test = "negative"))))
  expect_false(.ok(stagnic_properties_fao88(sg(chroma = 3, test = "positive"))))  # chroma 2 or less
  # mottles not of higher chroma
  expect_false(.ok(stagnic_properties_fao88(sg(mcol = "10YR 6/1", test = "positive"))))
  # Fe-Mn concretions with a low-chroma matrix
  expect_true(.ok(stagnic_properties_fao88(sg(mott = 0, mcol = NA, test = "positive",
                                              fe_mn_nodules_pct = c(0, 5, 0)))))
  # not mottled: chroma 1 or less
  expect_true(.ok(stagnic_properties_fao88(sg(chroma = 1, mott = 0, mcol = NA, test = "positive"))))
  expect_false(.ok(stagnic_properties_fao88(sg(chroma = 2, mott = 0, mcol = NA, test = "positive"))))
  # no test: the colour pattern, flagged (Q21)
  r <- stagnic_properties_fao88(sg())
  expect_true(r$passed); expect_true("reduction_from_colour_pattern" %in% r$evidence$flags)
  # a dark A of chroma 1 does not confirm reduction: undecided
  expect_true(is.na(stagnic_properties_fao88(sg(chroma = 1, mott = 0, mcol = NA, value = 3))$passed))
  # within 50 cm
  expect_false(.ok(stagnic_properties_fao88(sg(top = 55, test = "positive"))))
  # item 3d is reported, not required
  r <- stagnic_properties_fao88(sg(test = "positive"))
  expect_true("chroma_increasing_with_depth" %in% names(r$evidence$items))
})

test_that("gypsiferous, sodic and salic properties: the printed numbers", {
  skip_on_cran()
  expect_true(.ok(gypsiferous_fao88(.mid(caso4_pct = c(0, 5, 0)))))
  expect_false(.ok(gypsiferous_fao88(.mid(caso4_pct = c(0, 4.9, 0)))))
  expect_true(.ok(gypsiferous_fao88(.mid(caso4_pct = c(5, 5, 0)), window_cm = c(20, 50))))
  # sodic: ESP 15 or more, or Na + Mg 50 % or more of the CEC
  so <- function(na, mg, cec = 20) .mid(na_cmol = c(0, na, 0), mg_cmol = c(1, mg, 1), cec_cmol = cec)
  expect_true(.ok(sodic_properties_fao88(so(3, 1))))         # 15 %
  expect_false(.ok(sodic_properties_fao88(so(2.9, 1))))
  expect_true(.ok(sodic_properties_fao88(so(2, 8))))         # (2 + 8) / 20 = 50 %
  expect_false(.ok(sodic_properties_fao88(so(2, 7.9))))
  expect_true(.ok(sodic_properties_fao88(.mid(esp_pct = c(0, 15, 0), mg_cmol = 1, cec_cmol = 20))))
  # salic: ECe above 15 within 30 cm, or above 4 at pH (1:1) above 8.5
  sa <- function(ec, ph = 7, top2 = 20) .p88(top_cm = c(0, top2), bottom_cm = c(top2, 100),
    designation = c("A", "Bz"), ec_dS_m = c(1, ec), ph_h2o_1to1 = c(7, ph))
  expect_true(.ok(salic_properties_fao88(sa(15.1))))
  expect_false(.ok(salic_properties_fao88(sa(15))))
  expect_true(.ok(salic_properties_fao88(sa(4.1, 8.6))))
  expect_false(.ok(salic_properties_fao88(sa(4.1, 8.5))))
  expect_false(.ok(salic_properties_fao88(sa(4, 9))))
  expect_false(.ok(salic_properties_fao88(sa(20, top2 = 31))))   # within 30 cm
  # pH not at 1:1: ph_h2o stands in, flagged
  p <- .p88(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "Bz"),
            ec_dS_m = c(1, 6), ph_h2o = c(7, 9))
  r <- salic_properties_fao88(p)
  expect_true(r$passed); expect_true("ph_h2o_for_1to1" %in% r$evidence$flags)
  r <- salic_properties_fao88(.p88(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "Bz"),
                                   ec_dS_m = c(1, 6)))
  expect_true(is.na(r$passed))
  # an upper bound of the EC rules a layer out
  p <- .p88(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "Bz"), ec_max_dS_m = c(1, 3))
  expect_false(.ok(salic_properties_fao88(p)))
})

test_that("sulfidic materials: 0.75 % S, CCE below 3 x S, pH above 3.5", {
  skip_on_cran()
  su <- function(s, cce = 0, ph = 6.5) .p88(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "Cr"),
    s_total_pct = c(0.1, s), caco3_pct = c(0, cce), ph_h2o = c(6, ph))
  expect_true(.ok(sulfidic_materials_fao88(su(0.75))))
  expect_false(.ok(sulfidic_materials_fao88(su(0.74))))
  expect_true(.ok(sulfidic_materials_fao88(su(1, cce = 2.9))))
  expect_false(.ok(sulfidic_materials_fao88(su(1, cce = 3))))
  expect_false(.ok(sulfidic_materials_fao88(su(1, ph = 3.5))))
  # the sulfidic S as a lower bound
  p <- .p88(top_cm = c(0, 20), bottom_cm = c(20, 100), designation = c("A", "Cr"),
            sulfidic_s_pct = c(0, 0.5), caco3_pct = 0, ph_h2o = 6.5)
  expect_true(is.na(sulfidic_materials_fao88(p)$passed))
})

test_that("strongly humic: more than 1.4 % carbon over 0-100 cm, zero below rock", {
  skip_on_cran()
  sh <- function(oc) .p88(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 120), designation = c("A", "Bw", "C"),
                          oc_pct = oc)
  expect_true(.ok(strongly_humic_fao88(sh(c(3, 1, 0.8)))))     # (90 + 30 + 32) / 100 = 1.52
  expect_false(.ok(strongly_humic_fao88(sh(c(2.4, 1, 0.5)))))  # (72 + 30 + 20) / 100 = 1.22
  expect_false(.ok(strongly_humic_fao88(sh(c(3, 1, 0.5)))))    # 1.40, not more
  rock <- .p88(top_cm = c(0, 60), bottom_cm = c(60, 100), designation = c("A", "R"), oc_pct = c(2.5, NA))
  expect_true(.ok(strongly_humic_fao88(rock)))                  # 150 / 100
  rock2 <- .p88(top_cm = c(0, 60), bottom_cm = c(60, 100), designation = c("A", "R"), oc_pct = c(2.3, NA))
  expect_false(.ok(strongly_humic_fao88(rock2)))                # 138 / 100
  expect_true(is.na(strongly_humic_fao88(sh(c(3, NA, 1)))$passed))
})

test_that("organic soil materials: 12-18 % saturated, 20 % never saturated", {
  skip_on_cran()
  o <- function(oc, clay, site) .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("H", "C"),
                                     oc_pct = c(oc, 1), clay_pct = c(clay, 20), site = site)
  wet <- list(id = "w", drainage_class = "poorly drained")
  well <- list(id = "d", drainage_class = "well drained")
  expect_true(.ok(organic_soil_materials_fao88(o(15, 30, wet))))     # 12 + 3 = 15
  expect_false(.ok(organic_soil_materials_fao88(o(14.9, 30, wet))))
  expect_true(.ok(organic_soil_materials_fao88(o(18, 70, wet))))     # 18 at 60 % clay or more
  expect_false(.ok(organic_soil_materials_fao88(o(19, 30, well))))
  expect_true(.ok(organic_soil_materials_fao88(o(20, 30, well))))
  expect_true(is.na(organic_soil_materials_fao88(o(16, 30, list(id = "u")))$passed))
})

test_that("permafrost, plinthite, slickensides, smeary consistence", {
  skip_on_cran()
  pf <- .p88(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "Cf"), permafrost_temp_C = c(2, -1))
  expect_true(.ok(permafrost_fao88(pf, max_top_cm = 200)))
  expect_false(.ok(permafrost_fao88(pf, max_top_cm = 30)))
  pf2 <- .p88(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "Cf"))
  r <- permafrost_fao88(pf2, max_top_cm = 200)
  expect_true(r$passed); expect_true("permafrost_from_designation" %in% r$evidence$flags)
  # D1: the frozen suffix by the declared notation
  fz <- function(ds, d = "Bf") .p88(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", d),
                                    site = c(list(id = "f"), if (!is.null(ds)) list(designation_system = ds)))
  expect_true(.ok(permafrost_fao88(fz(NULL), max_top_cm = 200)))
  expect_true(.ok(permafrost_fao88(fz("fao2006", "Bhf"), max_top_cm = 200)))
  expect_false(.ok(permafrost_fao88(fz("cssc"), max_top_cm = 200)))          # f is Fe and Al there
  expect_false(.ok(permafrost_fao88(fz("cssc", "Bhf"), max_top_cm = 200)))
  expect_true(.ok(permafrost_fao88(fz("cssc", "Bmz"), max_top_cm = 200)))    # z is frozen there
  r <- permafrost_fao88(fz("sibcs"), max_top_cm = 200)
  expect_true(is.na(r$passed)); expect_true("frozen_suffix_other_notation_not_read" %in% r$evidence$flags)
  expect_true(is.na(permafrost_fao88(fz("sibcs", "Bhf"), max_top_cm = 200)$passed))
  r <- permafrost_fao88(.mid(ice_pct = c(NA, 5, NA)), max_top_cm = 200)
  expect_true(r$passed); expect_true("permafrost_from_ice" %in% r$evidence$flags)
  r <- permafrost_fao88(.mid(), max_top_cm = 200)
  expect_false(r$passed); expect_true("permafrost_not_recorded_absent" %in% r$evidence$flags)
  expect_false(.ok(permafrost_fao88(.mid(permafrost_temp_C = 0.5), max_top_cm = 200)))
  # plinthite: the amount set by the key
  pl <- function(pct, ...) .mid(designation = c("A", "Bv", "C"), plinthite_pct = c(0, pct, 0), ...)
  expect_true(.ok(plinthite_fao88(pl(5))))
  expect_false(.ok(plinthite_fao88(pl(0))))
  expect_true(.ok(plinthite_fao88(pl(25), min_pct = 25)))
  expect_false(.ok(plinthite_fao88(pl(24), min_pct = 25)))
  expect_true(.ok(plinthite_fao88(pl(30), min_pct = 25, min_thickness_cm = 15, max_top_cm = 50)))
  expect_false(.ok(plinthite_fao88(pl(30, plinthite_hardens = c(NA, FALSE, NA)))))
  r <- plinthite_fao88(.mid(designation = c("A", "Bv", "C")), min_pct = 25)
  expect_true(is.na(r$passed))
  r <- plinthite_fao88(.mid(designation = c("A", "Bv", "C")))
  expect_true(r$passed); expect_true("plinthite_from_designation_v" %in% r$evidence$flags)
  # D1: v is plinthite in the FAO notation only
  r <- plinthite_fao88(.mid(designation = c("A", "Bv", "C"), site = list(id = "s", designation_system = "sibcs")))
  expect_true(is.na(r$passed)); expect_true("suffix_v_other_notation_not_read" %in% r$evidence$flags)
  expect_true(.ok(plinthite_fao88(.mid(designation = c("A", "Bv", "C"), site = list(id = "s", designation_system = "fao2006")))))
  # slickensides
  for (sl in c("very few", "few", "distinct", "yes", "present"))
    expect_true(.ok(slickensides_fao88(.mid(slickensides = c(NA, sl, NA)))), info = sl)
  expect_false(.ok(slickensides_fao88(.mid(slickensides = c(NA, "none", NA)))))
  expect_false(.ok(slickensides_fao88(.mid())))
  # smeary
  expect_true(.ok(smeary_fao88(.mid(smeary_consistence = c(NA, TRUE, NA)))))
  r <- smeary_fao88(.mid(thixotropic_index = c(NA, 60, NA)))
  expect_true(r$passed); expect_true("thixotropic_index_for_smeary" %in% r$evidence$flags)
  expect_false(.ok(smeary_fao88(.mid(thixotropic_index = c(NA, 49, NA)))))
})

test_that("soft powdery lime: 50 % of the faces or 5 % soft nodules, no pseudomycelia", {
  skip_on_cran()
  expect_true(.ok(soft_powdery_lime_fao88(.mid(carbonate_coatings_faces_pct = c(0, 50, 0)))))
  expect_false(.ok(soft_powdery_lime_fao88(.mid(carbonate_coatings_faces_pct = c(0, 49, 0)))))
  expect_true(.ok(soft_powdery_lime_fao88(.mid(carbonate_soft_pct = c(0, 5, 0)))))
  expect_false(.ok(soft_powdery_lime_fao88(.mid(carbonate_soft_pct = c(0, 4.9, 0)))))
  expect_false(.ok(soft_powdery_lime_fao88(.mid(secondary_carbonates_form = c(NA, "pseudomycelia", NA)))))
  # D4: coatings named without an amount: undecided, flagged
  r <- soft_powdery_lime_fao88(.mid(secondary_carbonates_form = c(NA, "coatings", NA)))
  expect_true(is.na(r$passed)); expect_true("coatings_named_without_amount" %in% r$evidence$flags)
  expect_true(is.na(soft_powdery_lime_fao88(.mid(secondary_carbonates_form = c(NA, "soft concretions", NA)))$passed))
  expect_false(.ok(soft_powdery_lime_fao88(.mid())))
})

test_that("nitic and vertic properties", {
  skip_on_cran()
  ni <- function(clay = 45, grade = "strong", st2 = "polyhedral", films = "common", fo = NA, fd = NA)
    .mid(designation = c("A", "Bt", "C"), clay_pct = c(30, clay, 40), silt_pct = 20,
         sand_pct = c(50, 80 - clay, 40), structure_type = c("granular", "angular blocky", NA),
         structure_grade = c("moderate", grade, NA), structure_type_secondary = c(NA, st2, NA),
         clay_films_amount = c(NA, films, NA), fe_ox_pct = fo, fe_dcb_pct = fd)
  expect_true(.ok(nitic_properties_fao88(ni())))
  expect_false(.ok(nitic_properties_fao88(ni(clay = 29))))
  expect_false(.ok(nitic_properties_fao88(ni(grade = "weak"))))
  expect_true(is.na(nitic_properties_fao88(ni(st2 = NA))$passed))
  expect_true(.ok(nitic_properties_fao88(ni(fo = 0.5, fd = 10))))     # 5 % of DCB
  expect_false(.ok(nitic_properties_fao88(ni(fo = 0.45, fd = 10))))
  # 0.2 % Fe2O3 is 0.14 % Fe (x 0.6994)
  expect_true(.ok(nitic_properties_fao88(ni(fo = 0.15, fd = 2))))
  expect_false(.ok(nitic_properties_fao88(ni(fo = 0.13, fd = 2))))
  # vertic: clayey (30 %) with cracks, slickensides or wedge-shaped aggregates
  ve <- function(clay, ...) .mid(designation = c("A", "Bw", "C"), clay_pct = c(30, clay, 30),
                                 silt_pct = 20, sand_pct = c(50, 80 - clay, 50), ...)
  expect_true(.ok(vertic_properties_fao88(ve(30, cracks_width_cm = c(NA, 1, NA)))))
  expect_false(.ok(vertic_properties_fao88(ve(29, cracks_width_cm = c(NA, 1, NA)))))
  expect_true(.ok(vertic_properties_fao88(ve(40, slickensides = c(NA, "few", NA)))))
  expect_true(.ok(vertic_properties_fao88(ve(40, structure_type = c(NA, "wedge-shaped", NA)))))
  expect_false(.ok(vertic_properties_fao88(ve(60))))
})

test_that("tonguing: 5 / 10 / 15 mm by texture, more than 15 %, deeper than wide", {
  skip_on_cran()
  tg <- function(clay, width, vol = 20, depth = 10)
    .p88(top_cm = c(0, 20, 40, 100), bottom_cm = c(20, 40, 100, 150), designation = c("A", "E", "Bt", "C"),
         clay_pct = c(10, 8, clay, clay - 2), silt_pct = c(30, 30, 30, 30),
         sand_pct = c(60, 62, 70 - clay, 72 - clay),
         munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 5), munsell_chroma_moist = c(2, 2, 4, 4),
         munsell_value_dry = c(5, 7, 5, 6), munsell_chroma_dry = c(2, 2, 4, 4),
         clay_films_amount = c(NA, NA, "common", NA),
         tongue_vol_pct = c(NA, NA, vol, NA), tongue_width_mm = c(NA, NA, width, NA),
         tongue_depth_cm = c(NA, NA, depth, NA))
  expect_true(.ok(tonguing_fao88(tg(45, 5))))      # fine (clay): 5 mm
  expect_false(.ok(tonguing_fao88(tg(45, 4))))
  expect_true(.ok(tonguing_fao88(tg(30, 10))))     # moderately fine (clay loam): 10 mm
  expect_false(.ok(tonguing_fao88(tg(30, 9))))
  expect_true(.ok(tonguing_fao88(tg(18, 15))))     # medium (loam): 15 mm
  expect_false(.ok(tonguing_fao88(tg(18, 14))))
  expect_false(.ok(tonguing_fao88(tg(45, 5, vol = 15))))
  expect_false(.ok(tonguing_fao88(tg(45, 50, depth = 4))))   # 40 mm deep, 50 mm wide
  r <- tonguing_fao88(tg(45, NA, vol = NA, depth = NA))
  expect_false(r$passed)
  # Chernozems: the A into a C
  ch <- .p88(top_cm = c(0, 50), bottom_cm = c(50, 120), designation = c("A", "Ck"),
             tongue_vol_pct = c(NA, 20), tongue_depth_cm = c(NA, 30), tongue_width_mm = c(NA, 50))
  expect_true(.ok(tonguing_fao88(ch, kind = "chernozem")))
  expect_false(.ok(tonguing_fao88(ch, kind = "albic")))
})

test_that("interfingering and weatherable minerals", {
  skip_on_cran()
  inf <- function(sk, th) .p88(top_cm = c(0, 20, 40, 100), bottom_cm = c(20, 40, 100, 150),
    designation = c("A", "E", "Bt", "C"), clay_pct = c(10, 8, 30, 28), silt_pct = 30,
    sand_pct = c(60, 62, 40, 42), munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 5),
    munsell_chroma_moist = c(2, 2, 4, 4), munsell_value_dry = c(5, 7, 5, 6),
    munsell_chroma_dry = c(2, 2, 4, 4), clay_films_amount = c(NA, NA, "common", NA),
    skeletans_pct = c(NA, NA, sk, NA), skeletan_thickness_mm = c(NA, NA, th, NA))
  expect_true(.ok(interfingering_fao88(inf(16, 1.5))))
  expect_false(.ok(interfingering_fao88(inf(15, 1.5))))
  expect_false(.ok(interfingering_fao88(inf(20, 1))))
  # D7: not recorded, absent, flagged
  r <- interfingering_fao88(inf(NA, NA))
  expect_false(r$passed); expect_true("skeletans_not_recorded_absent" %in% r$evidence$flags)
  expect_true(.ok(weatherable_minerals_fao88(.mid(weatherable_minerals_pct = c(5, 10, 5)))))
  expect_false(.ok(weatherable_minerals_fao88(.mid(weatherable_minerals_pct = c(5, 9, 5)))))
  expect_true(is.na(weatherable_minerals_fao88(.mid())$passed))
})

test_that("buried layers are not read and undescribed depths are undecided", {
  skip_on_cran()
  bur <- .p88(top_cm = c(0, 55), bottom_cm = c(55, 100), designation = c("C", "Btb"),
              caco3_pct = c(0, 10), ferric_nodules_pct = c(0, 5))
  # a property of the present state is read in a buried layer, one bound to a
  # horizon is not (Hugo, 2026-11-09)
  expect_true(.ok(calcareous_fao88(bur)))
  expect_false(.ok(ferric_properties_fao88(bur)))
  short <- .p88(top_cm = 0, bottom_cm = 20, designation = "A", ec_dS_m = 1)
  r <- salic_properties_fao88(short)
  expect_true(is.na(r$passed)); expect_true("bottom_cm" %in% r$missing)
})

test_that("the readings of this part are recorded for Hugo in data-raw/fao_readings.yaml", {
  skip_on_cran()
  f <- test_path("..", "..", "data-raw", "fao_readings.yaml")
  skip_if_not(file.exists(f))
  y <- yaml::read_yaml(f)
  r383 <- Filter(function(r) identical(r$version, "0.9.383"), y$readings)
  topics <- vapply(r383, `[[`, "", "topic")
  expect_true(all(c("Gleyic and stagnic properties", "Reduction from tests and colour",
                    "The andic mantle of p. 42", "Depths of the properties") %in% topics))
  expect_true(all(grepl("\\((Hugo|soilKey's reading, for Hugo to confirm), 2026-11-0[89]\\)\\.$",
                        vapply(r383, `[[`, "", "reading"))))
})

test_that("the review of 2026-11-09: decisions and defects", {
  skip_on_cran()
  # fluvic: an increase of more than 0.2 points is irregular (decision 3)
  fl <- function(oc) .p88(top_cm = c(0, 20, 60, 100), bottom_cm = c(20, 60, 100, 150),
    designation = c("A", "C1", "C2", "C3"), clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = oc,
    site = list(id = "f", parent_material = "alluvium"))
  expect_true(.ok(fluvic_properties_fao88(fl(c(1, 0.3, 0.51, 0.1)))))
  expect_false(.ok(fluvic_properties_fao88(fl(c(1, 0.3, 0.5, 0.1)))))
  # ferric: Fe-Mn nodules alone are undecided, flagged (decision 10)
  r <- ferric_properties_fao88(.mid(fe_mn_nodules_pct = c(NA, 3, NA)))
  expect_true(is.na(r$passed)); expect_true("fe_mn_nodules_not_read_as_ferric" %in% r$evidence$flags)
  # D6: with the lower bound of S, CaCO3 of three times it does not refute
  su <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Cr"),
             sulfidic_s_pct = c(0.1, 0.8), s_total_pct = c(0.1, NA), caco3_pct = c(0, 3), ph_h2o = 7)
  expect_true(is.na(sulfidic_materials_fao88(su)$passed))
  su2 <- .p88(top_cm = c(0, 30), bottom_cm = c(30, 100), designation = c("A", "Cr"),
              sulfidic_s_pct = c(0.1, 0.8), s_total_pct = c(0.1, NA), caco3_pct = c(0, 2), ph_h2o = 7)
  expect_true(.ok(sulfidic_materials_fao88(su2)))
  # D8: Chernozem tongues without a width are undecided
  ch <- .p88(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "C"),
             tongue_vol_pct = c(NA, 20), tongue_depth_cm = c(NA, 10))
  expect_true(is.na(tonguing_fao88(ch, kind = "chernozem")$passed))
  # D9: a petrocalcic horizon is not rock even when the rock reader counts it
  pk <- .p88(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("A", "Bkm"),
             bedrock_pct = c(NA, 95), rupture_resistance = c(NA, "very hard"))
  expect_false(.ok(continuous_hard_rock_fao88(pk, max_top_cm = 50)))
  # D9: strongly humic passes when the carbon described already exceeds 140
  expect_true(.ok(strongly_humic_fao88(.p88(top_cm = c(0, 40), bottom_cm = c(40, 80), oc_pct = 2.5))))
  expect_true(is.na(strongly_humic_fao88(.p88(top_cm = c(0, 40), bottom_cm = c(40, 80), oc_pct = 1.5))$passed))
  # D9: the Podzol route of item 4 applies to stagnic properties too
  pz <- .p88(top_cm = c(0, 10, 20, 40), bottom_cm = c(10, 20, 40, 100),
    designation = c("A", "E", "Bhs", "C"), clay_pct = 3, silt_pct = 5, sand_pct = 92,
    cementation_class = c(NA, NA, "strongly", NA), ignition_redder = c(NA, NA, NA, FALSE),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 3, 6), munsell_chroma_moist = c(2, 1, 4, 3))
  r <- stagnic_properties_fao88(pz)
  expect_true(r$passed); expect_true("podzol_grains_free_of_iron_ignition" %in% r$evidence$flags)
  # decision 1: argic item 6 reads the same boundary reader (clara; not recorded absent, flagged)
  ag <- function(bd) .p88(top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 80), designation = c("A", "E", "B"),
    clay_pct = c(30, 10, 30), sand_pct = c(40, 60, 40), silt_pct = 30, boundary_distinctness = c(bd, bd, NA))
  # the coarser horizon above the B is 10 cm: enough over an abrupt or clear
  # transition (5 cm), not otherwise (18 cm)
  expect_true(.ok(argic_b_fao88(ag("clara"))))
  r <- argic_b_fao88(ag(NA))
  expect_false(r$passed)
  expect_true("boundary_distinctness_not_recorded_absent" %in% r$evidence$flags)
  expect_false("boundary_distinctness_not_recorded_absent" %in% argic_b_fao88(ag("gradual"))$evidence$flags)
  expect_identical(soilKey:::.fao_boundary_sharp(data.frame(boundary_distinctness = c("Abrupta", "gradual", NA)))$value,
                   c(TRUE, FALSE, FALSE))
})
