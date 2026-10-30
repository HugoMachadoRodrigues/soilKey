# v0.9.334 -- read_nsd_pedons() and benchmark_nsd(): the New Zealand National
# Soils Database survey exports. The tables below are made up in the shape of
# an NSDR export (no NSD data in the repository).

.nsd_fake_survey <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  w <- function(name, d, bom = FALSE, header = NULL) {
    f <- file.path(dir, name)
    utils::write.csv(d, f, row.names = FALSE, na = "")
    txt <- readLines(f)
    if (!is.null(header)) txt[1] <- header
    if (bom) txt[1] <- paste0("\ufeff", txt[1])
    writeLines(txt, f, useBytes = TRUE)
  }
  w("dm_surveydataset.csv", data.frame(id = "1042-x", name = "Made-up Survey", code = "MUS"))
  w("cl_concept.csv", data.frame(
    tablename = c("sd_horizon", "sd_horizon", "sd_horizon", "sd_soil"),
    columnname = c("texture_value", "consistence_strength", "texture_value", "classifier_nzsc"),
    conceptid = c("1", "2", "3", "4"), code = c("ZL", "2", "SL", "MOT"),
    name = c("silt loam", "weak", "sandy loam", "Typic Orthic Pumice Soil")),
    bom = TRUE, header = "tablename, columnname, conceptid, code, name")
  w("sa_site.csv", data.frame(
    id = "1042-x", sa_site_id = c("55-a", "55-b", "55-c"),
    nsdridentifier = c("nsdr:1", "nsdr:2", "nsdr:3"), identifier = c("SB1", "SB2", "SB3"),
    location_geometry_srid = "2193",
    location_geometry_x = c("1846148.367", NA, NA), location_geometry_y = c("5716690.437", NA, NA),
    region = "Taupo"), bom = TRUE)
  w("sd_soil.csv", data.frame(
    sa_site_id = c("55-a", "55-b", "55-c"), sd_soil_id = c("47-a", "47-b", "47-c"),
    date = "1968-11-29",
    classifier_nzsc = c("MOT [Typic Orthic Pumice Soil]", "MIW [Welded Impeded Pumice Soil]",
                        "BLAM [DO NOT USE - Acidic-mafic Allophanic Brown Soil]"),
    classifier_nz_genetic = "Yellow-brown pumice soil", classifier_nz_series = "MADEUP",
    profiledrainage = c("well", "poor.", "imperfect"),
    classifier_usda = c("TYPIC UDIVITRAND", "AQUIC VITRANDEPT", NA)))
  w("sd_soil_note.csv", data.frame(sd_soil_id = "47-a", sd_soil_note_id = "n1",
                                   note = "PARENT ROCK: water-sorted Taupo Pumice"))
  w("sd_horizon.csv", data.frame(
    sd_soil_id = c("47-a", "47-a", "47-a", "47-b", "47-b", "47-b", "47-b", "47-c"),
    sd_horizon_id = paste0("49-", 1:8), horizonnumber = c(1, 2, 3, 1, 2, 3, 4, 1),
    designation = c("A1", "(B)", "C", "O", "B2g", "Bfe", "G", "Ap"),
    horizondepth_maxop = c("<=", "<=", ">", "<=", "<=", "<=", "<=", "<="),
    horizondepth_maxval = c(13, 28, 60, 0, 30, 31, 60, 20),
    horizondepth_minop = ">=", horizondepth_minval = c(0, 13, 28, -10, 0, 30, 31, 0),
    horizondepth_uom = "cm",
    lowerboundarydistinctness = c("4", "2", NA, "1", "3", "1", NA, NA),
    texture_value = c("ZL", "SL", "S", NA, "ZL", NA, "silty clay loam", "ZL"),
    texture_qualifier = c(NA, "3", NA, NA, NA, NA, NA, NA),
    texture_legacy_texturemodifier = c(NA, NA, NA, NA, "peaty", NA, NA, NA),
    consistence_strength = c("2", "moderately firm", "loose", NA, "4", NA, NA, NA),
    consistence_stickiness = c("1", "2", NA, NA, "3", NA, NA, NA),
    consistence_failure = c(NA, "B", NA, NA, NA, NA, NA, NA),
    soilwaterstate = NA,
    designation_fao = c("Ah", NA, NA, "H", NA, "Bms", NA, NA)))
  w("sd_horizon_matrixcolour.csv", data.frame(
    sd_horizon_id = c("49-1", "49-2", "49-2", "49-3", "49-5", "49-6", "49-7", "49-8"),
    sd_horizon_matrixcolour_id = paste0("c", 1:8), moisturestatus = "unknown",
    hue = c("10YR", "7.5YR", "10YR", "2.5Y", "5Y", "5YR", "5GY", "10YR/2.5Y"),
    value = c("2", "4", "5", "6", "5", "3", "5", "3"),
    chroma = c("1", "4", "6", "3", "2", "6", "1", "2")))
  w("em_mottle.csv", data.frame(
    sd_horizon_id = c("49-5", "49-5"), em_mottle_id = c("m1", "m2"),
    abundance = c("3", "2"), geometry_size = c("4", "5"), contrast = c("2", "3"),
    matrixcolour_moisturestatus = "unknown", matrixcolour_munsell_hue = c("7.5YR", "5YR"),
    matrixcolour_munsell_value = c("5", "4"), matrixcolour_munsell_chroma = c("6", "8")))
  w("em_ped.csv", data.frame(
    sd_horizon_id = c("49-1", "49-2", "49-2", "49-3"),
    em_ped_id = c("131-0-1", "131-0-2", "131-1-2", "131-0-3"),
    abundance = NA, geometry_size = c("4", "5", "3", NA), geometry_shape = NA,
    legacy_grade = c("moderately developed", "weakly developed", "moderately developed", "single grain"),
    type = c("crumb", "nut", "granular", NA)))
  w("em_surfacefeature.csv", data.frame(
    sd_horizon_id = c("49-2", "49-5"), em_surfacefeature_id = c("s1", "s2"),
    type = c("CC", "OG"), abundance = c("3", "2"), distinctness = c("2", "1"), continuity = NA,
    matrixcolour_moisturestatus = "unknown", matrixcolour_munsell_hue = c("7.5YR", "10YR"),
    matrixcolour_munsell_value = c("5", "3"), matrixcolour_munsell_chroma = c("4", "2")))
  w("em_pan.csv", data.frame(sd_horizon_id = "49-6", em_pan_id = "p1", type = "ironpan",
                             continuity = "continuous"))
  w("em_root.csv", data.frame(sd_horizon_id = c("49-1", "49-3"), em_root_id = c("r1", "r2"),
                              abundance = c("4", "no"), type = NA, geometry_size = NA))
  w("lc_landscape.csv", data.frame(sa_site_id = "55-a", lc_landscape_id = "l1", landform = "terrace"))
  w("lc_landuse.csv", data.frame(sa_site_id = c("55-a", "55-b"), lc_landuse_id = c("u1", "u2"),
                                 drained = c("n", "y")))
  w("lc_climate.csv", data.frame(sa_site_id = "55-a", lc_climate_id = "k1",
                                 meantemperatureannual_val = "11.5"))
  # samples: two in the A1 (0-5, 5-13), one in the (B) by mid-depth only,
  # one across the C; one for profile b
  w("sa_sample.csv", data.frame(
    id = "1042-x", sa_site_id = c("55-a", "55-a", "55-a", "55-a", "55-b"),
    sa_laboratorysample_id = paste0("82-s", 1:5, "-lab"), sa_sample_id = paste0("82-s", 1:5),
    field_samplingdepth_maxval = c(5, 13, 25, 50, 20), field_samplingdepth_minval = c(0, 5, 15, 30, 5),
    lab_samplingdepth_maxval = NA, lab_samplingdepth_minval = NA,
    sd_horizon_id_est = c("49-1", "49-1", NA, "49-3", "49-5")))
  ob <- data.frame(
    sa_sample_lab_id = c("82-s1-lab", "82-s2-lab", "82-s1-lab", "82-s3-lab", "82-s3-lab",
                         "82-s3-lab", "82-s4-lab", "82-s4-lab", "82-s5-lab", "82-s5-lab",
                         "82-s1-lab"),
    ob_observation_id = paste0("o", 1:11), resulttime = NA,
    method = c("3A", "3A", "5G", NA, NA, NA, NA, NA, NA, NA, "mir-carbon [MIR - carbon]"),
    propertytypeid = NA,
    propertytypename = c("totalc", "totalc", "pretention", "fe2", "drybulkdensityfineearth",
                         "volcanicglasssand", "calciumcarbonate", "totalc", "totalc", "phh2o", "totalc"),
    result_op = c("=", "=", "=", "=", "=", "=", "<", "=", "=", "=", NA),
    result_uom = c("%", "%", "%", "%", "t/m3", "%", "%", "%", "%", "U", "w/w %"),
    result_val = c("12", "6", "40", "8", "0.8", "55", "0.4", "0.5", "20", "4.9", "99"))
  w("ob_observation.csv", ob,
    header = paste0("sa_sample_lab_id,ob_observation_id,resulttime,method,propertytypeid,propertytypename,",
                    "result_op,result_uom,result_val,quality,propertytypeid,propertytypename,val,op,uom"))
  invisible(dir)
}

test_that("read_nsd_pedons() reads the site, the classes and the provenance", {
  skip_on_cran()
  d <- .nsd_fake_survey(file.path(tempdir(), "nsd_fake_1"))
  p <- read_nsd_pedons(d)
  expect_named(p, c("SB1", "SB2", "SB3"))
  s <- p$SB1$site
  expect_identical(s$designation_system, "nzsc")
  expect_identical(s$country, "NZ")
  expect_equal(s$lat, -38.66418, tolerance = 1e-5)
  expect_equal(s$lon, 175.82910, tolerance = 1e-5)
  expect_identical(s$reference_nzsc, "MOT")
  expect_identical(s$reference_nzsc_name, "Typic Orthic Pumice Soil")
  expect_identical(c(s$reference_nzsc_order, s$reference_nzsc_group, s$reference_nzsc_subgroup),
                   c("M", "MO", "MOT"))
  expect_identical(s$reference_usda, "Andisols")
  expect_identical(s$reference_usda_grtgroup, "udivitrands")
  expect_identical(s$reference_usda_subgroup, "typic udivitrands")
  expect_identical(s$drainage_class, "well drained")
  expect_identical(s$parent_material, "water-sorted Taupo Pumice")
  expect_identical(s$landform, "terrace")
  expect_false(s$artificially_drained)
  expect_equal(s$mean_annual_air_temp_C, 11.5)
  expect_identical(s$survey, "Made-up Survey")
  expect_match(s$licence, "CC BY-ND 4.0")
  expect_match(s$citation, "10.26060/95m4-cz25")
  # 3rd-edition code MIW is the 4th edition's MIC; Andepts noted
  s2 <- p$SB2$site
  expect_identical(s2$reference_nzsc, "MIC")
  expect_match(s2$reference_nzsc_note, "MIW")
  expect_identical(s2$reference_usda, "Inceptisols")
  expect_match(s2$reference_usda_edition, "Andepts")
  expect_identical(s2$drainage_class, "poorly drained")
  expect_true(s2$artificially_drained)
  # BLAM, withdrawn in the 4th edition, is read at no level
  s3 <- p$SB3$site
  expect_true(is.na(s3$reference_nzsc_order))
  expect_match(s3$reference_nzsc_note, "withdrawn")
  # provenance names the NSD table
  pv <- as.data.frame(p$SB1$provenance)
  expect_true(any(grepl("NSD totalc", pv$notes)))
  expect_true(all(pv$source == "measured"))
})

test_that("read_nsd_pedons() decodes the field description", {
  skip_on_cran()
  d <- .nsd_fake_survey(file.path(tempdir(), "nsd_fake_2"))
  p <- read_nsd_pedons(d)
  h <- as.data.frame(p$SB1$horizons)
  expect_identical(h$designation, c("Ah", "Bw", "C"))            # designation_fao, then (B)
  expect_identical(h$field_texture, c("silt loam", "fine sandy loam", "sand"))
  expect_identical(h$strength_moist, c("weak", NA, "loose"))      # moderately firm: not read
  expect_identical(h$consistence_moist, c(NA, "brittle", NA))
  expect_identical(h$stickiness, c("non-sticky", "slightly sticky", NA))
  expect_identical(h$structure_type, c("crumb", "polyhedral", "single grain"))
  expect_identical(h$structure_grade, c("moderate", "weak", "structureless"))
  expect_identical(h$structure_type_secondary, c(NA, "granular", NA))
  expect_equal(h$ped_size_max_mm, c(10, 20, NA))
  expect_identical(h$munsell_hue_moist, c("10YR", "7.5YR", "2.5Y"))  # the first of two
  expect_equal(h$munsell_value_moist, c(2, 4, 6))
  expect_identical(h$boundary_distinctness, c("gradual", "abrupt", NA))
  expect_identical(h$clay_films_amount, c(NA, "common", NA))
  expect_identical(h$roots_abundance, c("many", NA, "none"))
  # described horizons with no mottle record: none
  expect_equal(h$redoximorphic_features_pct, c(0, 0, 0))
  h2 <- as.data.frame(p$SB2$horizons)
  # depths shifted to start at 0; H read as O; old notation translated
  expect_equal(h2$top_cm, c(0, 10, 40, 41))
  expect_identical(h2$designation, c("O", "Bg2", "Bfm", "G"))      # Bms 1 cm thick: placic
  expect_equal(h2$redoximorphic_features_pct[2], 17.5 + 6)
  expect_identical(h2$mottle_contrast[2], "prominent")
  expect_identical(h2$field_texture[2], "peaty silt loam")
  expect_identical(h2$strength_moist[2], "firm")
  expect_equal(h2$coatings_value_moist[2], 3)
  expect_identical(h2$pan_type[3], "thin ironpan (placic)")
  expect_true(is.na(h2$redoximorphic_features_pct[4]))           # a G horizon stays unknown
  expect_match(p$SB2$site$depth_note, "shifted by 10")
  # as recorded
  pa <- read_nsd_pedons(d, old_notation = "asis")
  expect_identical(as.data.frame(pa$SB2$horizons)$designation, c("O", "B2g", "Bfm", "G"))
  # a hue between two is read as the first
  expect_identical(as.data.frame(p$SB3$horizons)$munsell_hue_moist, "10YR")
})

test_that("read_nsd_pedons() gives each horizon its laboratory samples", {
  skip_on_cran()
  d <- .nsd_fake_survey(file.path(tempdir(), "nsd_fake_3"))
  h <- as.data.frame(read_nsd_pedons(d)$SB1$horizons)
  # A1: samples 0-5 (12 %) and 5-13 (6 %) by thickness; the MIR value is not read
  expect_equal(h$oc_pct[1], (12 * 5 + 6 * 8) / 13)
  expect_equal(h$phosphate_retention_pct[1], 40)
  # (B): the sample with no estimate, by its mid-depth (20 cm)
  expect_equal(h$clay_pct[2], 8)
  expect_equal(h$bulk_density_g_cm3[2], 0.8)
  expect_equal(h$volcanic_glass_pct[2], 55)
  # C: CaCO3 below the detection limit is 0
  expect_equal(h$caco3_pct[3], 0)
  expect_equal(h$oc_pct[3], 0.5)
  h2 <- as.data.frame(read_nsd_pedons(d)$SB2$horizons)
  expect_equal(h2$ph_h2o[2], 4.9)
  # a horizon with no sample of its own takes the one that overlaps it
  expect_true(is.na(h2$oc_pct[3]))                               # 30-31 cm before the shift: no overlap
  expect_equal(h2$oc_pct[2], 20)
})

test_that("read_nsd_pedons() reads zips and keeps only classified profiles on request", {
  skip_on_cran()
  d <- .nsd_fake_survey(file.path(tempdir(), "nsd_fake_4"))
  expect_length(read_nsd_pedons(d, classified_only = TRUE), 3L)
  zip_ok <- nzchar(Sys.which("zip"))
  skip_if_not(zip_ok, "no zip program")
  z <- file.path(tempdir(), "nsd_fake_4.zip")
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  utils::zip(z, list.files("."), flags = "-q")
  setwd(old)
  pz <- read_nsd_pedons(z)
  pd <- read_nsd_pedons(d)
  expect_identical(names(pz), names(pd))
  expect_identical(as.data.frame(pz$SB1$horizons), as.data.frame(pd$SB1$horizons))
  expect_error(read_nsd_pedons(file.path(tempdir(), "no_such_nsd_survey")), "no NSD survey")
})

test_that("benchmark_nsd() compares the key with the surveyor's codes", {
  skip_on_cran()
  d <- .nsd_fake_survey(file.path(tempdir(), "nsd_fake_5"))
  p <- read_nsd_pedons(d)
  b <- benchmark_nsd(p, level = "order", verbose = FALSE)
  expect_identical(b$level, "order")
  expect_identical(b$n_compared, 2L)                             # BLAM is read at no level
  expect_true(all(c("reference", "predicted", "agree", "provisional", "provisional_because",
                    "predicted_group", "predicted_subgroup") %in% names(b$predictions)))
  expect_true(b$accuracy >= 0 && b$accuracy <= 1)
  bs <- benchmark_nsd(p, level = "subgroup", verbose = FALSE)
  expect_identical(sort(bs$predictions$reference), c("MIC", "MOT"))
  expect_error(benchmark_nsd(list()), "non-empty")
})

test_that("the older New Zealand notation is read as the key reads designations", {
  skip_on_cran()
  d <- soilKey:::.nsd_designation(
    orig = c("B2fe", "(B)g", "(B1g)", "B2G", "A12g", "C1ca", "Bms", "Bms", "A2", "G1", "Bw", "O-2"),
    fao = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "2H", NA),
    thick = c(NA, NA, NA, NA, NA, NA, 0.5, 3, NA, NA, NA, NA))
  expect_identical(d$designation, c("Bs2", "Bwg", "Bwg1", "Bg2", "Ag12", "Ck1", "Bfm", "Bsm", "A2", "G1", "2O", "O2"))
  expect_match(d$note[1], "older New Zealand notation")
  expect_match(d$note[8], "ortstein-pan")
  expect_match(d$note[11], "FAO H")
  a <- soilKey:::.nsd_designation(orig = "B2fe", fao = NA, old_notation = "asis")
  expect_identical(a$designation, "B2fe")
})

test_that("the surveyor's NZSC and Soil Taxonomy names are parsed by level", {
  skip_on_cran()
  r <- soilKey:::.nsd_nzsc_reference("M [Pumice Soil]")
  expect_identical(c(r$order, r$group, r$subgroup), c("M", NA, NA))
  r <- soilKey:::.nsd_nzsc_reference("ZPHQ [Humose-ortstein Perch-gley Podzol]")
  expect_identical(c(r$order, r$group, r$subgroup), c("Z", "ZP", "ZPHQ"))
  expect_true(is.na(soilKey:::.nsd_nzsc_reference(NA)$code))
  u <- soilKey:::.nsd_usda_reference("THAPTO - HISTIC FLUVAQUENT")
  expect_identical(c(u$order, u$grtgroup, u$subgroup), c("Entisols", "fluvaquents", "thapto-histic fluvaquents"))
  expect_identical(soilKey:::.nsd_usda_reference("ASHY,MESIC VITRUDAND")$subgroup, NA_character_)
  expect_identical(soilKey:::.nsd_usda_reference("TYPIC GIBBSIAQUOX")$order, "Oxisols")
  ll <- soilKey:::.nsd_nztm_to_lonlat(1600000, 10000000 - 4428236)
  expect_equal(ll$lon, 173, tolerance = 1e-9)
})

test_that("nsd_citation() names the NSD and its licence", {
  skip_on_cran()
  expect_match(nsd_citation(), "National Soils Database")
  expect_match(nsd_citation(as.Date("2026-10-30")), "2026-10-30")
})
