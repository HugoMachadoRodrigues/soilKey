# v0.9.308: the ISIS reader (ISRIC World Soil Reference Collection). The
# profiles below are made up in the shape the ISIS GraphQL service returns
# (values as lists of strings, codes as {item, description}); no ISRIC data is
# stored in the package.

.v09308_code <- function(item, description = NULL) list(item = item, description = description)

.v09308_profile <- function(code = "XX001") {
  list(
    profileCode = code, countryCode = "XX", profileId = 1L,
    geog = list(latitude = -1.5, longitude = 36.2),
    moistureRegimeSoil = NULL, temperatureRegimeSoil = NULL,
    drainageClass = .v09308_code("W", "Well drained"),
    siteElevation = list("1500"), slopeGradientNumeric = list("2"),
    wrbClassifications = list(
      list(publicationYear = 2022L, referenceSoilGroupCode = "FR", referenceSoilGroup = "Ferralsol",
           qualifier = list(list(qualifier = "Humic", position = "supplementary", place = 2L),
                            list(qualifier = "Rhodic", position = "principal", place = 1L),
                            list(qualifier = "Clayic", position = "supplementary", place = 1L))),
      list(publicationYear = 1998L, referenceSoilGroupCode = "FR", referenceSoilGroup = "Ferralsol",
           qualifier = list())),
    usdaClassifications = list(
      list(publicationYear = 1975L, orderName = "Oxisol", suborder = "Ustox", greatGroup = "Eutrustox",
           subgroup = "Rhodic", moistureRegime = "ustic", temperatureRegime = "isohyperthermic"),
      list(publicationYear = 1999L, orderName = "Oxisol", suborder = "Ustox", greatGroup = "Eutrustox",
           subgroup = "Rhodic", moistureRegime = NULL, temperatureRegime = NULL)),
    layers = list(
      list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah",
           clay = list("40.0", "42.0"), silt = list("20"), sand = list("39"), phaq = list("6.0"),
           orgc = list("20.0"), nitkjd = list("1.5"), cecph7 = list("20"), caExcph7 = list("8"),
           mgExcph7 = list("2"), kExcph7 = list("0.5"), naExcph7 = list("0.1"), exbas = list("10"),
           alExch = list("0.4"), tceq = list("50"), gypsumContent = list("10"), feExtdith = list("6.2"),
           colourMoistHue = .v09308_code("2.5YR"), colourMoistValue = .v09308_code("3"),
           colourMoistChroma = .v09308_code("2"), structureGrade1 = .v09308_code("WE", "weak"),
           structureType = .v09308_code("SB", "Subangular blocky"),
           consistenceSoilmassMoist = .v09308_code("FR", "Friable"),
           consistenceSoilmassDry = .v09308_code("SHA", "Slightly hard"),
           rocksArtefactAbundVol = .v09308_code("F", "Few (2-5%)"),
           cutansAbundance = .v09308_code("0", "none")),
      # a bulk density / water sample at its own depth, with the layer number of the horizon above
      list(layerNumber = 1L, upperDepth = 25L, lowerDepth = 30L, bdfiod = list("1.20"), wv1500 = list("24")),
      list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 60L, layerName = "Bt",
           clay = list("55"), cecph7 = list("8"), exbas = list("5"), cfvo = list("12"),
           cutansKind = .v09308_code("C", "clay"), cutansAbundance = .v09308_code("P", "patchy"),
           cutansThickness = .v09308_code("f", "thin (faint)"),
           mottlesAbundance = .v09308_code("C", "Common (5-15%)"),
           mottlesColour = .v09308_code("5YR 5/8", "yellowish red"),
           rocksArtefactAbundVol = .v09308_code("C", "Common (5-15%)"),
           structureType = .v09308_code("PM", "Porous massive")),
      list(layerNumber = 3L, upperDepth = 60L, lowerDepth = 100L, layerName = "Btv",
           clay = list("50"), cutansKind = .v09308_code("C", "clay"),
           structureType = .v09308_code("AW", "Angular blocky (wedge-shaped)")),
      list(layerNumber = 4L, upperDepth = 100L, lowerDepth = 140L, layerName = "BCss",
           clay = list("48"), cutansKind = .v09308_code("P", "slickensides"),
           cutansAbundance = .v09308_code("B", "broken/common"),
           pansKind = .v09308_code("D", "duripan")),
      # laboratory data only
      list(layerNumber = 5L, upperDepth = 140L, lowerDepth = 180L, clay = list("45"), phaq = list("5.5")))
  )
}

test_that("an ISIS profile becomes a pedon with soilKey units and words", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.v09308_profile(), accessed = as.Date("2026-10-29"))
  h <- as.data.frame(p$horizons)
  expect_equal(nrow(h), 5L)                       # the sample row is not a horizon
  expect_equal(h$clay_pct[1], 41)                 # replicates averaged
  expect_equal(h$oc_pct[1], 2)                    # g/kg -> %
  expect_equal(h$n_total_pct[1], 0.15)
  expect_equal(h$caco3_pct[1], 5)
  expect_equal(h$caso4_pct[1], 1)
  expect_equal(h$fe_dcb_pct[1], 6.2)
  expect_equal(h$bs_pct[1], 50)                   # 100 x 10 / 20
  expect_equal(h$ecec_cmol[1], 10.4)              # bases + Al
  expect_equal(h$al_sat_pct[1], 100 * 0.4 / 10.4)
  expect_equal(h$al_kcl_cmol[1], 0.4)
  # the bulk density sample (25-30 cm) belongs to the Bt (20-60 cm)
  expect_true(is.na(h$bulk_density_g_cm3[1]))
  expect_equal(h$bulk_density_g_cm3[2], 1.2)
  expect_equal(h$water_content_1500kpa[2], 24 / 1.2)
  expect_match(p$site$depth_note, "1 laboratory sample row")
  # field codes
  expect_identical(h$structure_grade[1], "weak")
  expect_identical(h$structure_type[1:3], c("subangular blocky", "massive", "wedge-shaped angular blocky"))
  expect_identical(h$consistence_moist[1], "friable")
  expect_identical(h$rupture_resistance[1], "slightly hard")
  expect_identical(h$munsell_hue_moist[1], "2.5YR")
  expect_equal(h$munsell_value_moist[1], 3)
  # coarse fragments: the class middle, unless measured
  expect_equal(h$coarse_fragments_pct[1], 3.5)
  expect_equal(h$coarse_fragments_pct[2], 12)
})

test_that("cutans, mottles and the absences are read as Hugo decided", {
  skip_on_cran()
  h <- as.data.frame(isis_profile_to_pedon(.v09308_profile())$horizons)
  expect_identical(h$clay_films_amount, c("none", "few", "few", NA, NA))      # 0 = none; P = few; no abundance = few
  expect_identical(h$clay_films_strength[2], "faint")
  expect_identical(h$slickensides[4], "common")                              # cutans of kind slickensides
  expect_equal(h$redoximorphic_features_pct[2], 10)
  expect_identical(h$mottle_colors[2], "5YR 5/8")
  # absent unless recorded, in the described horizons only
  expect_equal(h$redoximorphic_features_pct[c(1, 3, 4)], c(0, 0, 0))
  expect_true(is.na(h$redoximorphic_features_pct[5]))                       # laboratory data only
  expect_identical(h$slickensides[1:3], rep("absent", 3))
  expect_equal(h$plinthite_pct[c(1, 2, 4)], c(0, 0, 0))
  expect_true(is.na(h$plinthite_pct[3]))                                    # Btv: plinthite suggested
  expect_true(is.na(h$duripan_pct[4]))                                      # a duripan is recorded
  expect_equal(h$artefacts_pct[1], 0)
  expect_identical(h$geomembrane_present[1], FALSE)
  expect_true(is.na(h$artefacts_pct[5]))
  # and nothing is inferred when asked not to
  h2 <- as.data.frame(isis_profile_to_pedon(.v09308_profile(), absent_unless_recorded = FALSE)$horizons)
  expect_true(all(is.na(h2$plinthite_pct)))
  expect_true(is.na(h2$slickensides[1]))
  expect_true(is.na(h2$redoximorphic_features_pct[1]))
})

test_that("the site carries ISRIC's names, the regimes and the attribution", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.v09308_profile(), accessed = as.Date("2026-10-29"))
  s <- p$site
  expect_identical(s$reference_wrb, "Ferralsols")
  expect_identical(s$reference_wrb_full, "Rhodic Ferralsol (Clayic, Humic)")
  expect_identical(s$reference_wrb_edition, "2022")
  expect_identical(s$reference_usda, "Oxisols")
  expect_identical(s$reference_usda_subgroup, "Rhodic Eutrustox")
  expect_identical(s$reference_usda_edition, "1999")
  # the regimes of the latest USDA record that gives them
  expect_identical(s$soil_moisture_regime, "ustic")
  expect_identical(s$soil_temperature_regime, "isohyperthermic")
  expect_identical(s$drainage_class, "well drained")
  expect_equal(c(s$lat, s$lon), c(-1.5, 36.2))
  expect_match(s$citation, "2026-10-29")
  expect_match(s$licence, "non-commercial")
  expect_match(s$attribution, "XX001")
  # every value names its ISIS field
  expect_true(all(c("clay_pct", "bs_pct", "clay_films_amount") %in% p$provenance$attribute))
  expect_true(any(grepl("ISIS clay", p$provenance$notes)))
  # the profile's own regime wins; torric is aridic; Solonetz is its own plural
  q <- .v09308_profile()
  q$moistureRegimeSoil <- .v09308_code("DT", "Torric")
  q$wrbClassifications[[1]]$referenceSoilGroup <- "Solonetz"
  s2 <- isis_profile_to_pedon(q)$site
  expect_identical(s2$soil_moisture_regime, "aridic")
  expect_identical(s2$reference_wrb, "Solonetz")
})

test_that("a profile with sample rows only has no horizons", {
  skip_on_cran()
  q <- .v09308_profile()
  q$layers <- q$layers[2]
  p <- isis_profile_to_pedon(q)
  expect_true(is.null(p$horizons) || nrow(p$horizons) == 0L)
  expect_match(p$site$depth_note, "sample rows only")
})

test_that("read_isis pages through ISIS with the codes as variables", {
  skip_on_cran()
  calls <- list()
  fake <- function(query, variables = NULL, timeout = 60) {
    calls[[length(calls) + 1L]] <<- list(query = query, variables = variables)
    off <- as.integer(sub(".*offset: ([0-9]+).*", "\\1", query))
    first <- as.integer(sub(".*first: ([0-9]+), offset.*", "\\1", query))
    codes <- unlist(variables$codes)
    take <- codes[seq_along(codes) > off][seq_len(min(first, max(0L, length(codes) - off)))]
    take <- setdiff(take, "NOPE")
    list(data = list(profiles = lapply(take, .v09308_profile)), error = NULL)
  }
  local_mocked_bindings(.isis_post = fake)
  codes <- sprintf("xx%03d", 1:7)
  out <- read_isis(codes)
  expect_identical(names(out), toupper(codes))
  expect_length(calls, 2L)                                   # 5 + 2
  expect_identical(unlist(calls[[1]]$variables$codes), toupper(codes))
  expect_match(calls[[1]]$query, "profileCode: \\{ in: \\$codes \\}")
  expect_s3_class(out[[1]], "PedonRecord")
  calls <- list()
  expect_warning(read_isis(c("XX001", "NOPE")), "no profile NOPE")
  expect_error(read_isis(), "profile_codes")
  # a failed request returns what was read, with a warning
  n <- 0L
  local_mocked_bindings(.isis_post = function(query, variables = NULL, timeout = 60) {
    n <<- n + 1L
    if (n == 1L) fake(query, variables) else list(data = NULL, error = "timeout")
  })
  expect_warning(part <- read_isis(codes), "timeout")
  expect_length(part, 5L)
})

test_that("benchmark_isis sets soilKey beside ISRIC", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.v09308_profile())
  q <- .v09308_profile("XX002"); q$wrbClassifications <- q$wrbClassifications[2]
  b <- benchmark_isis(list(p, isis_profile_to_pedon(q)))
  expect_identical(b$profile, c("XX001", "XX002"))
  expect_true(all(c("isric_wrb", "soilkey_wrb", "isric_rsg", "soilkey_rsg", "wrb_agree",
                    "isric_usda_order", "soilkey_usda_order", "usda_order_agree") %in% names(b)))
  expect_type(b$wrb_agree, "logical")
  expect_true(is.na(b$wrb_agree[2]))                       # a 1998 name is not scored
  expect_match(isis_citation(as.Date("2026-10-29")), "graphql.isric.org/isis")
})

# A second made-up profile for the readings of 2026-10-29: laboratory rows
# merged into the described horizons, EC 1:2.5 bounds, cracks and gilgai,
# inclusions, plinthite and the mottles of a g horizon.
.v09308_profile2 <- function() {
  cd <- .v09308_code
  list(
    profileCode = "XX003", countryCode = "XX", cracking = cd("C", "(small) cracks"),
    microreliefKind = cd("G", "gilgai"),
    layers = list(
      list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah",
           colourMoistHue = cd("10YR"), horizBoundaryDistinctness = cd("C", "Clear (2-5 cm)"),
           horizBoundaryTopography = cd("S", "Smooth")),
      list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 10L, clay = list("20"), sand = list("50"), elco25 = list("0.5")),
      list(layerNumber = 1L, upperDepth = 10L, lowerDepth = 20L, clay = list("30"), sand = list("50")),
      list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 50L, layerName = "Bg",
           colourMoistHue = cd("5Y"), cutansKind = cd("P", "slickensides"), cutansAbundance = cd("P", "patchy")),
      list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 50L, clay = list("40"), elco25 = list("20"),
           clayWaterdispTotal = list("4")),
      list(layerNumber = 3L, upperDepth = 50L, lowerDepth = 80L, layerName = "Btk", clay = list("45"),
           inclusionsComposition = cd("K", "calcareous"), mineralConcAbundance = cd("C", "Common"),
           cutansAbundance = cd("P", "patchy")),
      list(layerNumber = 4L, upperDepth = 80L, lowerDepth = 110L, layerName = "Bv", clay = list("45"),
           mottlesAbundance = cd("M", "Many (15-40%)"), inclusionsComposition = cd("F", "ferruginous"),
           concretionsMineralHardness = cd("S", "Soft"), mineralConcAbundance = cd("F", "Few")),
      list(layerNumber = 5L, upperDepth = 110L, lowerDepth = 140L, layerName = "Bc", clay = list("40"),
           inclusionsComposition = cd("F", "ferruginous"), concretionsMineralHardness = cd("H", "Hard"),
           inclusionsSize = cd("M", "medium: 2-10 mm"), mineralConcAbundance = cd("A", "Abundant"),
           pansKind = cd("L", "iron stone ( indurated plinthite)")),
      list(layerNumber = 6L, upperDepth = 140L, lowerDepth = 160L, clay = list("35")))
  )
}

test_that("laboratory rows go to the described horizon that holds them", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.v09308_profile2())
  h <- as.data.frame(p$horizons)
  expect_identical(h$designation, c("Ah", "Bg", "Btk", "Bv", "Bc", NA))
  expect_equal(h$clay_pct[1:2], c(25, 40))              # thickness-weighted mean; one sample
  expect_equal(h$silt_pct[1], 25)                       # 100 - sand - clay
  expect_equal(h$water_dispersible_clay_pct[2], 4)
  expect_equal(h$clay_pct[6], 35)                       # outside every described horizon: a horizon of its own
  expect_match(p$site$depth_note, "3 laboratory sample row")
  expect_identical(h$boundary_distinctness[1], "clear")
  expect_identical(h$boundary_topography[1], "smooth")
  # a described horizon with no laboratory value takes the samples that overlap it
  q <- .v09308_profile2()
  q$layers[[3]]$upperDepth <- 5L; q$layers[[3]]$lowerDepth <- 30L   # mid-depth 17.5: still in the Ah
  q$layers[[5]]$upperDepth <- 45L; q$layers[[5]]$lowerDepth <- 75L  # mid-depth 60: in the Btk, overlaps the Bg by 5 cm
  h2 <- as.data.frame(isis_profile_to_pedon(q)$horizons)
  expect_equal(h2$clay_pct[2], (30 * 10 + 40 * 5) / 15)   # overlap-weighted: 10 cm of one sample, 5 of the other
})

test_that("EC 1:2.5 is used only where it decides the salic horizon", {
  skip_on_cran()
  h <- as.data.frame(isis_profile_to_pedon(.v09308_profile2())$horizons)
  expect_true(is.na(h$ec_dS_m[1]))
  expect_equal(h$ec_max_dS_m[1], 0.5 * 12.5)            # an upper bound
  expect_equal(h$ec_dS_m[2], 20)                        # a lower bound that is enough
  t1 <- soilKey:::test_ec_concentration(data.frame(ec_dS_m = NA_real_, ec_max_dS_m = 10, ph_h2o = 7))
  expect_false(t1$passed)                               # 10 < 15 at pH 7
  t2 <- soilKey:::test_ec_concentration(data.frame(ec_dS_m = NA_real_, ec_max_dS_m = 10, ph_h2o = NA_real_),
                                        alkaline_min_dS_m = 8)
  expect_true(is.na(t2$passed))                         # pH unknown: the alkaline limit 8 applies
  t3 <- soilKey:::test_ec_concentration(data.frame(ec_dS_m = NA_real_, ec_max_dS_m = 20, ph_h2o = 7))
  expect_true(is.na(t3$passed))
})

test_that("cracks, slickensides, inclusions, plinthite and g-horizon mottles", {
  skip_on_cran()
  p <- isis_profile_to_pedon(.v09308_profile2())
  h <- as.data.frame(p$horizons)
  expect_identical(h$cracks_width_cm[1:5], c(0.5, 0.5, 0.5, 0.5, NA))   # horizons starting above 100 cm
  expect_true(isTRUE(p$site$gilgai_presence))
  expect_identical(h$slickensides[2], "common")                         # patchy slickensides
  expect_identical(h$clay_films_amount[3], "few")                       # cutans of no kind in a t horizon
  expect_equal(h$secondary_carbonates_pct[3], 10)
  expect_equal(h$plinthite_pct[4], 27.5 + 3.5)                          # mottles + soft Fe concretions in a v horizon
  expect_equal(h$ferric_nodules_pct[5], 60)                             # hard concretions over 2 mm
  expect_identical(h$pan_type[5], "ferricrete")
  expect_true(is.na(h$redoximorphic_features_pct[2]))                   # a g horizon with no mottle field
  expect_equal(h$redoximorphic_features_pct[1], 0)
  # gilgai alone, with no cracking recorded
  q <- .v09308_profile2(); q$cracking <- NULL
  h2 <- as.data.frame(isis_profile_to_pedon(q)$horizons)
  expect_equal(h2$cracks_width_cm[1], 0.5)
  expect_true(any(grepl("gilgai", isis_profile_to_pedon(q)$provenance$notes)))
})
