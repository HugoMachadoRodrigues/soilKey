# v0.9.317: WRB 2022 Chapter 3 criteria as the text words them, and guards of
# the ISIS reader. The argic horizon criteria 2.a.i (a lithic discontinuity
# read from the data, Ch 3.2.7) and 2.a.ii (no plough layer directly above);
# the nitic horizon's pressure faces and second-level structure (3.1.22); the
# mollic horizon's criterion 1 (structure) and criterion 5 (weighted base
# saturation, 3.1.20); the irragric horizon (3.1.18); the ISIS reader's
# pressure faces, R rows inside a described horizon and particle-size records
# of zeros. The profiles are made up; no ISIS data is stored in the package.

.v09317_pedon <- function(...) {
  h <- data.frame(..., stringsAsFactors = FALSE)
  PedonRecord$new(site = list(id = "v09317"), horizons = h)
}

# ---- lithic discontinuity (Ch 3.2.7) ------------------------------------------

test_that("the ratio sand to silt alone shows no lithic discontinuity (Hugo, 2026-10-29)", {
  skip_on_cran()
  ld <- function(sand, silt, clay, cf = c(NA, NA)) soilKey:::.wrb_lithic_discontinuity_above(
    .v09317_pedon(designation = c("A", "Bw"), top_cm = c(0, 20), bottom_cm = c(20, 60),
                  clay_pct = clay, silt_pct = silt, sand_pct = sand,
                  coarse_fragments_pct = cf)$horizons)$at
  # criterion 2 also needs 2.b (not the original variation of the parent
  # material), which the data do not show: 60/30 over 30/50 is not read
  expect_identical(ld(c(60, 30), c(30, 50), c(10, 20)), c(FALSE, FALSE))
  # criterion 6: 10% (absolute) more coarse fragments in the overlying layer
  expect_identical(ld(c(60, 30), c(30, 50), c(10, 20), cf = c(30, 5)), c(FALSE, TRUE))
})

test_that("a lithic discontinuity is read from the coarse fragments (3.2.7, criterion 6)", {
  skip_on_cran()
  ld <- function(cf, worm = c(NA, NA)) soilKey:::.wrb_lithic_discontinuity_above(
    .v09317_pedon(designation = c("A", "Bw"), top_cm = c(0, 20), bottom_cm = c(20, 60),
                  clay_pct = c(10, 20), silt_pct = c(30, 27), sand_pct = c(60, 53),
                  coarse_fragments_pct = cf, worm_holes_pct = worm)$horizons)$at
  expect_identical(ld(c(30, 5)), c(FALSE, TRUE))     # 25% more above
  expect_identical(ld(c(12, 5)), c(FALSE, FALSE))    # 7% more
  expect_identical(ld(c(5, 30)), c(FALSE, FALSE))    # fewer above: criterion 7, not read
  # "unless the difference is created by animal activity": undecided, not read
  expect_identical(ld(c(30, 5), worm = c(5, 0)), c(FALSE, FALSE))
})

# ---- argic horizon (Ch 3.1.3) ---------------------------------------------------

test_that("argic criterion 2.a.i: no clay increase across a lithic discontinuity read from the data", {
  skip_on_cran()
  p <- function(cf, films = NA_character_) .v09317_pedon(
    designation = c("A", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, 60),
    clay_pct = c(10, 20), sand_pct = c(60, 30), silt_pct = c(30, 50),
    coarse_fragments_pct = cf, clay_films_amount = c(NA, films))
  # the ratio sand to silt alone is no discontinuity (Hugo, 2026-10-29)
  expect_true(isTRUE(argic(p(c(5, 5)))$passed))
  r <- argic(p(c(30, 5)))                                # criterion 6: coarse fragments
  expect_false(r$passed)
  expect_equal(r$evidence$clay_increase$lithic_discontinuities$above_layer, 2L)
  # illuvial clay is evidenced (2.b.ii): argic across the discontinuity
  expect_true(isTRUE(argic(p(c(30, 5), "common"))$passed))
  # the USDA argillic and the SiBCS B textural keep their reading
  expect_true(isTRUE(soilKey:::.argic_impl(p(c(30, 5)))$passed))
})

test_that("argic criterion 2.a.ii: no clay increase below a plough layer directly above", {
  skip_on_cran()
  p <- function(desg, films = NA_character_) .v09317_pedon(
    designation = desg, top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 70),
    clay_pct = c(10, 10, 20), sand_pct = c(60, 60, 53), silt_pct = c(30, 30, 27),
    clay_films_amount = c(NA, NA, films))
  # Ap1 and Ap2 over the Bt: the coarser layer's lowermost sublayer is ploughed
  expect_false(argic(p(c("Ap1", "Ap2", "Bt")))$passed)
  expect_equal(test_clay_increase_argic(p(c("Ap1", "Ap2", "Bt"))$horizons,
                                        plough_layer = TRUE)$layers, integer(0))
  # an E between the Ap and the Bt is the coarser layer directly above
  expect_true(isTRUE(argic(p(c("Ap", "E", "Bt")))$passed))
  # coatings on >= 15% of the aggregate surfaces (2.b.ii) still make it argic
  expect_true(isTRUE(argic(p(c("Ap1", "Ap2", "Bt"), "common"))$passed))
  # no designations: nothing is read as ploughed
  expect_true(isTRUE(argic(p(rep(NA_character_, 3)))$passed))
  # the earlier reading, for the USDA and SiBCS keys
  expect_true(isTRUE(soilKey:::.argic_impl(p(c("Ap1", "Ap2", "Bt")))$passed))
  # a Bt whose clay is not known stays undecided, as before
  q <- .v09317_pedon(designation = c("A", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, 60),
                     clay_pct = c(10, NA), sand_pct = c(60, NA), silt_pct = c(30, NA))
  expect_true(is.na(argic(q)$passed))
})

# ---- nitic horizon (Ch 3.1.22) --------------------------------------------------

.v09317_nitic <- function(type = "polyhedral", grade = "strong", secondary = NA_character_,
                          pressure = "common", films = NA_character_) {
  .v09317_pedon(
    designation = c("A", "Bt"), top_cm = c(0, 15), bottom_cm = c(15, 65),
    clay_pct = c(45, 55), silt_pct = c(25, 20), sand_pct = c(30, 25),
    structure_type = c("granular", type), structure_grade = c("strong", grade),
    structure_type_secondary = c(NA, secondary), pressure_faces = c("none", pressure),
    clay_films_amount = c(NA, films), fe_dcb_pct = c(8, 9), fe_ox_pct = c(0.6, 0.6))
}

test_that("nitic criterion 2.b: polyhedral structure with pressure faces on >= 25% of the surfaces", {
  skip_on_cran()
  r <- nitic_horizon(.v09317_nitic())
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2L)
  expect_true(isTRUE(nitic_horizon(.v09317_nitic(pressure = "many"))$passed))
  expect_false(nitic_horizon(.v09317_nitic(pressure = "few"))$passed)
  r <- nitic_horizon(.v09317_nitic(pressure = NA_character_))
  expect_true(is.na(r$passed))
  expect_true("pressure_faces" %in% r$missing)
  # clay coatings are not pressure faces (3.1.22: "In addition, clay coatings
  # may be found")
  expect_false(nitic_horizon(.v09317_nitic(pressure = "none", films = "many"))$passed)
  expect_true(is.na(nitic_horizon(.v09317_nitic(pressure = NA_character_, films = "many"))$passed))
})

test_that("nitic criterion 2.a: moderate to strong blocky structure breaking into polyhedral elements", {
  skip_on_cran()
  b <- function(...) nitic_horizon(.v09317_nitic(type = "angular blocky", ...))$passed
  expect_true(isTRUE(b(secondary = "polyhedral")))
  expect_true(isTRUE(b(secondary = "flat-edged")))
  expect_false(b(secondary = "subangular blocky"))
  expect_true(is.na(b(secondary = NA_character_)))             # no second level recorded
  expect_false(b(secondary = "polyhedral", grade = "weak"))
  expect_false(b(secondary = "polyhedral", pressure = "few"))
  # the SiBCS B nitico keeps its reading of cerosidade
  p1 <- .v09317_nitic(pressure = "common", films = "common")
  p2 <- .v09317_nitic(pressure = NA_character_, films = "common")
  expect_identical(B_nitico(p1)$passed, B_nitico(p2)$passed)
})

# ---- mollic horizon (Ch 3.1.20) -------------------------------------------------

.v09317_mollic <- function(type = "granular", size = "fine", grade = "moderate",
                           top = c(0, 15), bottom = c(15, 25), bs = c(80, 80),
                           ph = c(NA, NA)) {
  .v09317_pedon(
    designation = c("Ah1", "Ah2", "Bw"), top_cm = c(top, 25), bottom_cm = c(bottom, 60),
    munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 5),
    munsell_chroma_moist = c(2, 2, 4), munsell_value_dry = c(3, 3, 6),
    clay_pct = c(25, 25, 28), silt_pct = c(35, 35, 33), sand_pct = c(40, 40, 39),
    oc_pct = c(2.5, 1.5, 0.4), bs_pct = c(bs, 85), ph_h2o = c(ph, 6.5),
    structure_type = c(rep(type, length.out = 2), "subangular blocky"),
    structure_size = c(rep(size, length.out = 2), "medium"),
    structure_grade = c(rep(grade, length.out = 2), "moderate"))
}

test_that("mollic criterion 1: aggregate structure <= 10 cm or cloddy in >= 50% of the horizon", {
  skip_on_cran()
  m <- function(...) mollic(.v09317_mollic(...))$passed
  expect_true(isTRUE(m()))
  # Hugo (2026-10-29): structure or size not recorded does not block
  # criterion 1; it fails where the record shows otherwise
  r <- mollic(.v09317_mollic(type = NA_character_, size = NA_character_, grade = NA_character_))
  expect_true(isTRUE(r$passed))
  expect_false(m(type = "massive", size = NA_character_, grade = "structureless"))
  expect_false(m(type = "single grain", size = NA_character_, grade = "structureless"))
  expect_true(isTRUE(m(size = NA_character_)))                # aggregate size not recorded
  expect_true(isTRUE(m(type = "prismatic", size = "coarse")))  # 50-100 mm
  expect_false(m(type = "prismatic", size = "very coarse"))    # > 100 mm (Table 8.45)
  expect_false(m(type = "subangular blocky", size = "extremely coarse"))
  expect_true(isTRUE(m(type = "cloddy", size = NA_character_)))
  # by volume: 15 cm of granular and 10 cm of massive structure pass, 10 and 15 do not
  p <- .v09317_mollic(type = c("granular", "massive"), size = c("fine", NA),
                      grade = c("moderate", "structureless"))
  expect_true(isTRUE(mollic(p)$passed))
  q <- .v09317_mollic(type = c("granular", "massive"), size = c("fine", NA),
                      grade = c("moderate", "structureless"), top = c(0, 10), bottom = c(10, 25))
  expect_false(mollic(q)$passed)
  # the USDA mollic epipedon keeps its reading
  h <- .v09317_mollic(type = NA_character_, size = NA_character_, grade = NA_character_)$horizons
  expect_true(isTRUE(test_mollic_structure(h, candidate_layers = 1:2)$passed))
  expect_true(isTRUE(test_mollic_structure(h, candidate_layers = 1:2, system = "wrb2022")$passed))
})

test_that("umbric criterion 1 reads the structure as the mollic one does", {
  skip_on_cran()
  u <- function(...) umbric_horizon(.v09317_mollic(bs = c(20, 20), ...))$passed
  expect_true(isTRUE(u()))
  expect_true(isTRUE(u(type = NA_character_, size = NA_character_, grade = NA_character_)))
  expect_false(u(type = "massive", size = NA_character_, grade = "structureless"))
})

test_that("mollic criterion 5: base saturation >= 50% on a weighted average", {
  skip_on_cran()
  # 5 cm at 33% and 20 cm at 89%: 77.8% on a weighted average
  r <- mollic(.v09317_mollic(top = c(0, 5), bottom = c(5, 25), bs = c(33, 89)))
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$base_saturation$details$weighted_average, (5 * 33 + 20 * 89) / 25)
  expect_equal(r$evidence$base_saturation$layers, 1:2)
  # 15 cm at 30% and 10 cm at 60%: 42%
  expect_false(mollic(.v09317_mollic(bs = c(30, 60)))$passed)
  # one layer not recorded: between 24% and 76%, undecided
  expect_true(is.na(mollic(.v09317_mollic(bs = c(NA, 60)))$passed))
  # pHwater 6.5 where BS is not recorded (Annex 2, Table 9.2): >= 50%
  expect_true(isTRUE(mollic(.v09317_mollic(bs = c(NA, 60), ph = c(6.5, NA)))$passed))
})

# ---- irragric horizon (Ch 3.1.18) -----------------------------------------------

test_that("the irragric horizon needs the criteria of 3.1.18, not an Ap of 20 cm", {
  skip_on_cran()
  p <- function(type = "granular", ap_bottom = 30, oc_c = 0.2, c_type = "massive",
                clay_c = 20) .v09317_pedon(
    designation = c("Ap", "C"), top_cm = c(0, ap_bottom), bottom_cm = c(ap_bottom, 80),
    clay_pct = c(25, clay_c), silt_pct = c(40, 40), sand_pct = c(35, 60 - clay_c),
    oc_pct = c(1.5, oc_c), structure_type = c(type, c_type),
    structure_size = c("fine", NA), structure_grade = c("moderate", NA))
  # every criterion soilKey reads holds, but the raising of the land surface
  # (criterion 6) is not recorded: undecided
  r <- irragric(p())
  expect_true(is.na(r$passed))
  expect_true(is.na(r$evidence$criteria$raised_surface[1]))
  expect_true(isTRUE(r$evidence$criteria$structure[1]))
  expect_true(isTRUE(r$evidence$criteria$clay_increase[1]))   # 25 against 20
  expect_false(qual_irragric(p())$passed)
  # a massive Ap fails criterion 1 in every stack
  expect_false(irragric(p(type = "massive"))$passed)
  # an Ap of 15 cm over a C with 0.2% SOC: too thin alone, too poor with the C
  expect_false(irragric(p(ap_bottom = 15))$passed)
  # with 0.4% SOC and blocky structure in the C, the 80 cm stack fails
  # criterion 3: its subhorizons differ by 5% clay, 20% (relative)
  expect_false(irragric(p(ap_bottom = 15, oc_c = 0.4, c_type = "subangular blocky"))$passed)
  # 25 against 22% clay: undecided (criteria 2, 3, 5 and 6)
  expect_true(is.na(irragric(p(ap_bottom = 15, oc_c = 0.4, c_type = "subangular blocky",
                               clay_c = 22))$passed))
})

# ---- ISIS reader ----------------------------------------------------------------

.v09317_code <- function(item, description = NULL) list(item = item, description = description)

.v09317_isis <- function(layers) {
  list(profileCode = "XX317", countryCode = "XX", profileId = 1L,
       geog = list(latitude = 0, longitude = 0), layers = layers)
}

test_that("the ISIS reader reads pressure faces and the second structure", {
  skip_on_cran()
  prof <- .v09317_isis(list(
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 15L, layerName = "A",
         clay = list("45"), structureType = .v09317_code("SB", "Subangular blocky"),
         cutansKind = .v09317_code("C", "clay"), cutansAbundance = .v09317_code("B", "broken")),
    list(layerNumber = 2L, upperDepth = 15L, lowerDepth = 70L, layerName = "Bt",
         clay = list("55"), structureType = .v09317_code("AB", "Angular blocky"),
         structureType2 = .v09317_code("SN", "Nutty subangular blocky"),
         structureGrade1 = .v09317_code("ST", "strong"),
         cutansKind = .v09317_code("R", "pressure"), cutansAbundance = .v09317_code("B", "broken")),
    list(layerNumber = 3L, upperDepth = 70L, lowerDepth = 100L, clay = list("50"))))
  h <- as.data.frame(isis_profile_to_pedon(prof)$horizons)
  # v0.9.313: the A records clay cutans in the one cutans field, so its pressure
  # faces could not have been recorded there and stay unknown
  expect_identical(h$pressure_faces, c(NA, "common", NA))
  expect_identical(h$clay_films_amount[1:2], c("common", NA))
  expect_identical(h$structure_type_secondary[2], "nutty subangular blocky")
  expect_true("structureType2" %in% soilKey:::.ISIS_MORPH)   # asked of the service
  expect_true(is.na(isis_profile_to_pedon(prof, absent_unless_recorded = FALSE)$horizons$pressure_faces[1]))
  # a free cutans field (abundance none) still reads them as absent
  prof$layers[[1]]$cutansKind <- NULL
  prof$layers[[1]]$cutansAbundance <- .v09317_code("0", "none")
  h <- as.data.frame(isis_profile_to_pedon(prof)$horizons)
  expect_identical(h$pressure_faces[1], "none")
  expect_identical(h$slickensides[1], "absent")
})

test_that("an R row inside a described horizon is left out, with a note", {
  skip_on_cran()
  prof <- .v09317_isis(list(
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 7L, clay = list("32.5"), phaq = list("8.0")),
    list(layerNumber = 6L, upperDepth = 0L, lowerDepth = 5L, layerName = "R",
         structureType = .v09317_code("MA", "Massive"), rocksArtefactAbundVol = .v09317_code("D", "Dominant")),
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 35L, layerName = "Apkc",
         structureType = .v09317_code("SB", "Subangular blocky")),
    list(layerNumber = 2L, upperDepth = 35L, lowerDepth = 60L, layerName = "Bk", clay = list("30")),
    list(layerNumber = 3L, upperDepth = 60L, lowerDepth = 80L, layerName = "R",
         rocksArtefactAbundVol = .v09317_code("D", "Dominant"))))
  p <- isis_profile_to_pedon(prof)
  h <- as.data.frame(p$horizons)
  expect_identical(h$designation, c("Apkc", "Bk", "R"))   # the R below the profile stays
  expect_equal(h$clay_pct[1], 32.5)                        # the 0-7 sample goes to the Apkc
  expect_match(p$site$depth_note, "R (0-5 cm) lies inside the described horizon Apkc (0-35 cm)",
               fixed = TRUE)
})

test_that("a particle-size record of zeros only is missing, not 0% clay", {
  skip_on_cran()
  prof <- .v09317_isis(list(
    list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah",
         clay = list("11.4"), silt = list("25.0"), sand = list("63.6")),
    list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 45L, layerName = "Ck2",
         clay = list("0.0"), phaq = list("8.2")),
    list(layerNumber = 3L, upperDepth = 45L, lowerDepth = 80L, layerName = "C",
         clay = list("0.0"), silt = list("1.6"), sand = list("98.4"))))
  p <- isis_profile_to_pedon(prof)
  h <- as.data.frame(p$horizons)
  expect_true(is.na(h$clay_pct[2]))
  expect_equal(h$ph_h2o[2], 8.2)                 # the rest of the record stays
  expect_equal(h$clay_pct[3], 0)                 # a sand with no clay is a measurement
  expect_match(p$site$depth_note, "Ck2 (20-45 cm) is all zeros", fixed = TRUE)
})
