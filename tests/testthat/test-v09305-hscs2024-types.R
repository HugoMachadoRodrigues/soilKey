# v0.9.305: the Hungarian diagnostic soil classification (Micheli et al.
# 2024, second approximation), keyed to its 15 soil types. One example per
# type keys to it with every earlier type decided; Hugo's decisions of
# 2026-10-29 and the readings where the text is silent
# (inst/rules/hscs2024/readings.yaml).

.hu_types <- c(peat = "LAP", anthropogenic = "ANT", rocky_skeletal = "KOV", rock_influenced = "KOZ",
               solonetz = "SZN", solonchak = "SZK", swelling_clay = "DUZ", meadow = "RET",
               carbonate = "KAR", steppe = "MEZ", clay_illuvial = "AGY", sandy = "HOM",
               brown_earth = "BAR", deposit = "HOR", weakly_developed = "FOL")
.hu_code <- function(p) classify_hscs2024(p, on_missing = "silent")$levels$code[1]

# an example with some columns replaced (whole columns, or row edits as a
# list of row = value)
.hu_mod <- function(type, h = list(), site = list()) {
  p <- make_hscs2024_example(type)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
.hu_pass <- function(fun, p) fun(p)$passed

test_that("each example keys to its type, with every earlier type decided", {
  skip_on_cran()
  for (t in names(.hu_types)) {
    r <- classify_hscs2024(make_hscs2024_example(t), on_missing = "silent")
    expect_identical(r$system_id, "hscs2024", info = t)
    expect_identical(r$levels$code[1], .hu_types[[t]], info = t)
    expect_length(r$provisional_because, 0L)
    expect_false(is.na(r$levels$name_en[1]), info = t)
  }
  r <- classify_hscs2024(make_hscs2024_example("clay_illuvial"), on_missing = "silent")
  # since 0.9.316 the name carries the subtype and variety properties too
  expect_identical(r$levels$name[1], "Agyagbemosódásos talaj")
  expect_true(grepl("Agyagbemosódásos talaj", r$name, fixed = TRUE))
  expect_identical(r$levels$name_en[1], "Clay-illuvial soil")
})

test_that("the registry lists the HSCS as an opt-in system with valid rules", {
  skip_on_cran()
  s <- soilkey_systems()
  expect_true("hscs2024" %in% s$id)
  expect_false(s$default[s$id == "hscs2024"])
  expect_identical(soilKey:::.system_resolve("hscs"), "hscs2024")
  expect_identical(soilKey:::.system_resolve("hungary"), "hscs2024")
  v <- soilKey:::.validate_rules("hscs2024")
  expect_length(v$missing, 0L); expect_length(v$unsuffixed, 0L); expect_length(v$codes, 0L)
  k <- load_rules("hscs2024")$types
  expect_length(k, 15L)
  expect_true(isTRUE(k[[15]]$tests$default))
  res <- classify_all(make_hscs2024_example("steppe"), systems = c("wrb", "hscs"), on_missing = "silent")
  expect_true(grepl("Mezőségi talaj", res$summary$hscs2024, fixed = TRUE))
  expect_length(yaml::read_yaml(system.file("rules", "hscs2024", "readings.yaml", package = "soilKey"))$readings, 22L)   # v0.9.316: five readings of the subtype and variety properties
  # the other hierarchical systems keep their three columns
  expect_identical(names(classify_nzsc4(make_nzsc4_example("brown"), on_missing = "silent")$levels),
                   c("level", "code", "name"))
})

test_that("the FAO (2006) notation is read", {
  skip_on_cran()
  d <- soilKey:::.hscs_designations(c("Btnh", "2Czl", "C(k)", "ABi", "Cr", "Bss", "H2", "A2", "Ap", "C/R", "Bw", "Ahb", "x?y"))
  expect_identical(d$role, c("B", "C", "C", "AB", "C", "B", "H", "A", "A", "C/R", "B", "A", "other"))
  expect_identical(d$t[1], TRUE); expect_identical(d$n[1], TRUE)
  expect_identical(d$prefix[2], "2"); expect_true(d$z[2] && d$l[2])
  expect_true(d$k[3])                        # weak k in parentheses
  expect_true(d$i[4]); expect_true(d$r[5]); expect_true(d$i[6])   # USDA ss read as i
  expect_true(d$p[9]); expect_true(d$w[11]); expect_true(d$buried[12])
})

test_that("Hungarian texture names are read on the USDA triangle", {
  skip_on_cran()
  h <- data.frame(field_texture = c("vályogos homok", "homokos agyagos vályog", "iszapos agyag", "durva homok",
                                    "valyog", "agyag", NA),
                  clay_pct = c(NA, NA, NA, NA, NA, NA, 45), silt_pct = c(NA, NA, NA, NA, NA, NA, 30),
                  sand_pct = c(NA, NA, NA, NA, NA, NA, 25), stringsAsFactors = FALSE)
  expect_identical(soilKey:::.hscs_texture_class(h),
                   c("loamy sand", "sandy clay loam", "silty clay", "sand", "loam", "clay", "clay"))
})

test_that("Laptalajok: bulk density not measured counts as 1 or less (Hugo's decision)", {
  skip_on_cran()
  expect_true(.hu_pass(peat_type_hscs, .hu_mod("peat", list(bulk_density_g_cm3 = NA_real_))))
  expect_false(.hu_pass(peat_type_hscs, .hu_mod("peat", list(bulk_density_g_cm3 = c(1.2, 1.2, 1.4)))))
  # an organic horizon buried below 40 cm is not within it
  expect_false(.hu_pass(peat_type_hscs, .hu_mod("peat", list(designation = c("A", "H", "Cr"), top_cm = c(0, 45, 90),
                                                             bottom_cm = c(45, 90, 120), oc_pct = c(3, 35, 1)))))
})

test_that("Reti talajok: reductive conditions need a test or an l or r suffix (Hugo's decision)", {
  skip_on_cran()
  # gley colours at 50-100 but no l/r suffix and no test: undecided
  p <- .hu_mod("meadow", list(designation = c("A", "Bw", "C")))
  expect_true(is.na(.hu_pass(meadow_type_hscs, p)))
  # a positive dipyridyl test within 50 cm decides
  expect_true(.hu_pass(meadow_type_hscs, .hu_mod("meadow", list(designation = c("A", "Bw", "C"),
                                                                childs_test = list(`2` = "positive")))))
  # negative tests in every horizon within 50 cm fail
  expect_false(.hu_pass(meadow_type_hscs, .hu_mod("meadow", list(designation = c("A", "Bw", "C"),
                                                                 childs_test = c("negative", "negative", NA)))))
  # methane on the site decides the first part
  expect_true(.hu_pass(meadow_type_hscs, .hu_mod("meadow", list(designation = c("A", "Bw", "C")),
                                                 site = list(methane_odour = TRUE))))
  # brown, unmottled colours at 50-100 fail the second part
  expect_false(.hu_pass(meadow_type_hscs, .hu_mod("meadow", list(munsell_hue_moist = "10YR",
                                                                 munsell_chroma_moist = 4,
                                                                 redoximorphic_features_pct = 0,
                                                                 designation = c("A", "Bw", "C")))))
})

test_that("Szoloncsak talajok: soluble salts are not converted from the EC (Hugo's decision)", {
  skip_on_cran()
  p <- .hu_mod("solonchak", list(soluble_salts_pct = NA_real_, ec_dS_m = 25))
  expect_true(is.na(.hu_pass(solonchak_type_hscs, p)))
  expect_false(.hu_pass(solonchak_type_hscs, .hu_mod("solonchak", list(soluble_salts_pct = 0.1))))
  # 0.15 % in a run 15 cm thick within 50 cm
  expect_true(.hu_pass(solonchak_type_hscs, .hu_mod("solonchak", list(soluble_salts_pct = c(0.1, 0.15, 0.1)))))
})

test_that("Szolonyec talajok: Na and Na + Mg as shares of the CEC (Hugo's decision)", {
  skip_on_cran()
  # Btn: Na 3 of CEC 20 = 15 %
  expect_true(.hu_pass(solonetz_type_hscs, .hu_mod("solonetz", list(na_cmol = list(`3` = 3), mg_cmol = list(`3` = 0),
                                                                    cec_cmol = list(`3` = 20)))))
  # Na 2 + Mg 3 of 20 = 25 %
  expect_true(.hu_pass(solonetz_type_hscs, .hu_mod("solonetz", list(na_cmol = c(0.2, 0.2, 2, 0.2, 0.2),
                                                                    mg_cmol = c(1, 1, 3, 1, 1), cec_cmol = 20))))
  expect_false(.hu_pass(solonetz_type_hscs, .hu_mod("solonetz", list(na_cmol = 0.2, mg_cmol = 1, cec_cmol = 20))))
  # esp_pct read when Na or the CEC is not given
  expect_true(.hu_pass(solonetz_type_hscs, .hu_mod("solonetz", list(na_cmol = NA_real_, esp_pct = c(2, 3, 18, 20, 20)))))
  # a columnar structure is required where the soil is not ploughed
  expect_false(.hu_pass(solonetz_type_hscs, .hu_mod("solonetz", list(structure_type = "prismatic"))))
})

test_that("Duzzado agyagtalajok: slickensides common or more; recorded cracks optional (Hugo's decision)", {
  skip_on_cran()
  sw <- function(...) .hu_mod("swelling_clay", list(structure_type = "prismatic", ...))
  expect_false(.hu_pass(swelling_clay_type_hscs, sw(slickensides = c("few", "few", "few"), designation = c("A", "B", "C"))))
  expect_true(.hu_pass(swelling_clay_type_hscs, sw(slickensides = c("few", "common", "common"), designation = c("A", "B", "C"),
                                                   cracks_width_cm = NA_real_)))
  # less than 30 % clay above the swelling horizon fails
  expect_false(.hu_pass(swelling_clay_type_hscs, sw(clay_pct = c(25, 50, 42))))
})

test_that("Hordalektalajok: 10 points of USDA sand make stratified material (Hugo's decision)", {
  skip_on_cran()
  st <- function(sand) .hu_mod("weakly_developed", list(sand_pct = sand, silt_pct = 100 - 18 - sand,
                                                        designation = c("A", "C"), oc_pct = c(0.5, 0.1)))
  expect_true(.hu_pass(deposit_type_hscs, st(c(42, 52))))
  expect_false(.hu_pass(deposit_type_hscs, st(c(42, 51))))
  # a rise of organic carbon of 0.2 points with depth
  expect_true(.hu_pass(deposit_type_hscs, .hu_mod("weakly_developed", list(oc_pct = c(0.3, 0.5)))))
})

test_that("the remaining types read their criteria", {
  skip_on_cran()
  # rock at 10 cm or less, or within 25 cm
  expect_false(.hu_pass(rocky_skeletal_type_hscs, make_hscs2024_example("rock_influenced")))
  expect_true(.hu_pass(rock_influenced_type_hscs, .hu_mod("brown_earth", list(coarse_fragments_pct = c(85, 90, 95)))))
  # artefacts: 20 % as a weighted average over 100 cm
  expect_false(.hu_pass(anthropogenic_type_hscs, .hu_mod("anthropogenic", list(artefacts_pct = c(10, 10, 0),
                                                                               layer_origin = NA_character_,
                                                                               designation = c("A", "2C", "3C")))))
  # a u suffix (FAO: artefacts) marks an anthropogenic horizon: 90 cm here
  expect_true(.hu_pass(anthropogenic_type_hscs, .hu_mod("anthropogenic", list(artefacts_pct = c(10, 10, 0),
                                                                              layer_origin = NA_character_))))
  expect_true(.hu_pass(anthropogenic_type_hscs, .hu_mod("brown_earth", list(), site = list(mechanical_mixing_depth_cm = 60))))
  # 25 % CaCO3 in 15 cm within 50 cm
  expect_false(.hu_pass(carbonate_type_hscs, .hu_mod("carbonate", list(caco3_pct = c(20, 22)))))
  # a clay accumulation horizon excludes the Mezosegi talajok
  expect_false(.hu_pass(steppe_type_hscs, .hu_mod("steppe", list(clay_pct = c(20, 30, 24, 22)))))
  # the clay ratio: 1.4 times the horizon above
  expect_false(.hu_pass(clay_illuvial_type_hscs, .hu_mod("clay_illuvial", list(clay_pct = c(15, 12, 16, 14)))))
  # ploughed: clay films decide
  expect_true(.hu_pass(clay_illuvial_type_hscs, .hu_mod("clay_illuvial", list(designation = c("Ap", "E", "Bt", "C"),
                                                                              clay_pct = c(15, 14, 16, 14)))))
  # finer layers more than 15 cm fail the Homoktalajok
  expect_false(.hu_pass(sandy_type_hscs, .hu_mod("sandy", list(clay_pct = c(6, 20, 2), silt_pct = c(8, 40, 3),
                                                               sand_pct = c(86, 40, 95)))))
  # a cambic horizon read against the C below it
  expect_true(.hu_pass(brown_earth_type_hscs, .hu_mod("brown_earth", list(designation = c("A", "B", "C")))))
  expect_false(.hu_pass(brown_earth_type_hscs, .hu_mod("brown_earth", list(designation = c("A", "B", "C"),
                                                                           munsell_hue_moist = "10YR",
                                                                           munsell_value_moist = c(3, 5, 5),
                                                                           munsell_chroma_moist = c(3, 4, 4),
                                                                           structure_grade = "structureless",
                                                                           structure_type = "massive"))))
})

# The cases two independent reviews (text and logic) found before the first
# release; each was wrong in the first draft.
.hu_P <- function(..., site = list(id = "x")) {
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::data.table(...)))
}

test_that("review cases: rock, gaps, sub-horizons and buried horizons", {
  skip_on_cran()
  # an R horizon is rock, not soil with missing data
  expect_false(solonetz_type_hscs(.hu_P(designation = c("A", "Bw", "R"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 100),
                                        na_cmol = c(.2, .2, NA), mg_cmol = c(3, 3, NA), cec_cmol = c(20, 20, NA),
                                        structure_type = c("granular", "subangular blocky", NA)))$passed)
  # Bt1 and Bt2 are read against the E above the clay accumulation horizon
  expect_true(clay_illuvial_type_hscs(.hu_P(designation = c("A", "E", "Bt1", "Bt2", "C"), top_cm = c(0, 20, 35, 42, 80),
                                            bottom_cm = c(20, 35, 42, 80, 120), clay_pct = c(15, 12, 24, 30, 20),
                                            silt_pct = c(55, 58, 50, 45, 50), sand_pct = c(30, 30, 26, 25, 30)))$passed)
  # a gap in the description may hold salts
  expect_true(is.na(solonchak_type_hscs(.hu_P(designation = c("Az", "Bz", "C"), top_cm = c(0, 20, 30),
                                              bottom_cm = c(10, 30, 120), soluble_salts_pct = c(.8, .6, .05)))$passed))
  # an undesignated uppermost row is not a cambic horizon
  expect_false(brown_earth_type_hscs(.hu_P(designation = c(NA, "C"), top_cm = c(0, 15), bottom_cm = c(15, 100),
                                           munsell_hue_moist = "10YR", munsell_value_moist = c(5, 6),
                                           munsell_chroma_moist = c(3, 4)))$passed)
  # a buried Ap is not deep ploughing
  expect_false(anthropogenic_type_hscs(.hu_P(designation = c("Ap", "C", "Apb", "C"), top_cm = c(0, 25, 70, 95),
                                             bottom_cm = c(25, 70, 95, 130)))$passed)
  # the swelling horizon is a subsurface one
  expect_false(swelling_clay_type_hscs(.hu_P(designation = c("A", "C"), top_cm = c(0, 30), bottom_cm = c(30, 120),
                                             clay_pct = c(45, 20), slickensides = c("many", "none"),
                                             structure_type = c("granular", "massive")))$passed)
})

test_that("review cases: the swelling horizon, sand, carbonate and rock windows", {
  skip_on_cran()
  # the clay above is read down to the swelling horizon that qualifies
  expect_false(swelling_clay_type_hscs(.hu_P(designation = c("A", "B", "Bi", "C"), top_cm = c(0, 20, 40, 80),
                                             bottom_cm = c(20, 40, 80, 120), clay_pct = c(35, 25, 45, 40),
                                             slickensides = c("common", "none", "many", "few")))$passed)
  # a swelling horizon may begin below a description that ends at 60 cm
  expect_true(is.na(swelling_clay_type_hscs(.hu_P(designation = c("A", "B"), top_cm = c(0, 30), bottom_cm = c(30, 60),
                                                  clay_pct = 40, slickensides = "none",
                                                  structure_type = "prismatic"))$passed))
  # no sandy material over a cemented layer
  expect_false(sandy_type_hscs(.hu_P(designation = c("A", "Bqm", "C"), top_cm = c(0, 10, 60), bottom_cm = c(10, 60, 120),
                                     clay_pct = c(25, 20, 25), silt_pct = 40, sand_pct = c(35, 40, 35)))$passed)
  # a different carbonate content (2 points or more) against the horizon below
  bc <- function(v) brown_earth_type_hscs(.hu_P(designation = c("A", "BC", "C"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 110),
                                                caco3_pct = c(9, v, 10), structure_grade = c("weak", "structureless", "structureless"),
                                                structure_type = c("granular", "massive", "massive"), munsell_hue_moist = "10YR",
                                                munsell_value_moist = c(5, 6, 6), munsell_chroma_moist = c(3, 4, 4)))$passed
  expect_false(bc(9.8)); expect_true(bc(5))
  # slight effervescence may be the trace class, below 2 %
  x <- soilKey:::.hscs_horizons(.hu_P(designation = c("A", "C"), top_cm = c(0, 20), bottom_cm = c(20, 80),
                                      effervescence_class = c("slight", "moderate")))
  expect_identical(soilKey:::.hscs_calcareous_rows(x), c(NA, TRUE))
  # over rock above 100 cm there is no horizon at 100 cm
  expect_false(deposit_type_hscs(.hu_P(designation = c("A", "C", "R"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 80),
                                       sand_pct = c(40, 40, NA), coarse_fragments_pct = c(5, 5, NA),
                                       oc_pct = c(1.5, 0.3, NA)))$passed)
})

test_that("review cases: shallow descriptions and clay-only data", {
  skip_on_cran()
  # a negative test to 40 cm does not cover 0-50 cm
  expect_true(is.na(meadow_type_hscs(.hu_P(designation = "A", top_cm = 0, bottom_cm = 40,
                                           childs_test = "negative"))$passed))
  # an 18 cm humic A may go on below the description
  expect_true(is.na(steppe_type_hscs(.hu_P(designation = "A", top_cm = 0, bottom_cm = 18, oc_pct = 2.5, clay_pct = 25,
                                           silt_pct = 55, sand_pct = 20, bs_pct = 90, structure_grade = "strong",
                                           structure_type = "granular", munsell_hue_moist = "10YR",
                                           munsell_value_moist = 2, munsell_chroma_moist = 2))$passed))
  # clay alone decides loamy sand or finer, and not sandy
  p <- .hu_P(designation = c("A", "E", "Bt", "C"), top_cm = c(0, 20, 35, 80), bottom_cm = c(20, 35, 80, 120),
             clay_pct = c(15, 12, 30, 20))
  expect_true(clay_illuvial_type_hscs(p)$passed)
  expect_false(sandy_type_hscs(p)$passed)
  # a row of unknown material is not taken as mineral for a horizon
  expect_true(is.na(solonchak_type_hscs(.hu_P(designation = NA_character_, top_cm = 0, bottom_cm = 20,
                                              soluble_salts_pct = .8))$passed))
  # artefacts: the part below a 40 cm description reads as none
  expect_false(anthropogenic_type_hscs(.hu_P(designation = c("A", "C"), top_cm = c(0, 20), bottom_cm = c(20, 40),
                                             artefacts_pct = 25))$passed)
})
