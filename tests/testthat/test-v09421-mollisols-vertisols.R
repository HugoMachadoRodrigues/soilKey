# v0.9.421 (Hugo, 2026-11-20): Mollisols and Vertisols after the Soil
# Taxonomy audit (section 2.5, M1 to M9; readings L19, L27 to L33).
#   * the mollic epipedon of KST 13th ed. pp. 14-16: the upper 18 cm mixed
#     whatever the designations, carbonate colours, the thickness rules, the
#     structure test (FAO "hard" = moderately hard), no colour, pH or Al
#     shortcuts, three-valued;
#   * the Mollisols of item I (pp. 69-70): base saturation to the depth of
#     I.2, the deepest horizon sampled standing in (grade C);
#   * the Vertisols of item F (p. 69), Xererts, Dystr- Vertisols.

.v09421_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "v09421"), site),
                  horizons = ensure_horizon_schema(hz))
}

# A dark, base-rich profile; arguments replace columns.
.v09421_mollic <- function(top = c(0, 10, 40, 90), bottom = c(10, 40, 90, 150),
                           designation = c("Ap", "Bw1", "Bw2", "C"),
                           vm = c(2, 3, 5, 6), vd = c(3, 5, 7, 7), cm = c(2, 2, 4, 4),
                           oc = c(3, 1.2, 0.4, 0.2), bs = c(80, 80, 85, 90),
                           clay = c(25, 26, 24, 20), sand = c(35, 35, 40, 45),
                           st = c("granular", "subangular blocky", "subangular blocky", "massive"),
                           rr = c("soft", "slightly hard", "slightly hard", "slightly hard"),
                           ca = c(0, 0, 0, 0), site = list()) {
  .v09421_pedon(top_cm = top, bottom_cm = bottom, designation = designation,
                munsell_hue_moist = "10YR", munsell_value_moist = vm,
                munsell_value_dry = vd, munsell_chroma_moist = cm,
                oc_pct = oc, bs_pct = bs, clay_pct = clay, sand_pct = sand,
                silt_pct = 100 - clay - sand, structure_type = st,
                rupture_resistance = rr, caco3_pct = ca, ph_h2o = 7.2,
                site = site)
}

test_that("the mollic epipedon mixes the upper 18 cm whatever the designations", {
  testthat::skip_on_cran()
  p <- .v09421_mollic()
  m <- soilKey:::mollic_epipedon_usda(p)
  expect_true(m$passed)
  # Ap 0-10 and Bw1 10-40 together: 40 cm
  expect_equal(m$evidence$thickness_cm, 40)
  expect_equal(m$evidence$mix_depth_cm, c(0, 18))
  # a light Ap over a dark Bw: the mixture decides, not the Ap alone
  q <- .v09421_mollic(vm = c(4, 3, 5, 6), vd = c(6, 5, 7, 7))
  expect_false(soilKey:::mollic_epipedon_usda(q)$passed)
})

test_that("a dry colour not recorded is undecided unless the moist value is 2 or less (L27)", {
  testthat::skip_on_cran()
  p <- .v09421_mollic(vd = c(NA, NA, 7, 7))
  m <- soilKey:::mollic_epipedon_usda(p)
  expect_true(is.na(m$passed))
  expect_true("munsell_value_dry" %in% m$missing)
  q <- .v09421_mollic(vm = c(2, 2, 5, 6), vd = c(NA, NA, 7, 7))
  r <- soilKey:::mollic_epipedon_usda(q)
  expect_true(r$passed)
  expect_match(r$evidence$dry_value_inferred, "moist value \\+ 1")
  # no Munsell at all: organic carbon does not stand in for colour
  s <- .v09421_mollic(vm = c(NA, NA, 5, 6), vd = c(NA, NA, 7, 7), cm = c(NA, NA, 4, 4))
  expect_true(is.na(soilKey:::mollic_epipedon_usda(s)$passed))
})

test_that("calcium carbonate lightens the colour allowed (item 4) and raises the carbon (6a)", {
  testthat::skip_on_cran()
  p <- .v09421_mollic(vm = c(4, 4, 5, 6), vd = c(6, 6, 7, 7), ca = c(45, 45, 50, 50),
                      oc = c(3, 2.6, 0.4, 0.2))
  expect_true(soilKey:::mollic_epipedon_usda(p)$passed)
  q <- .v09421_mollic(vm = c(4, 4, 5, 6), vd = c(6, 6, 7, 7), ca = c(45, 45, 50, 50),
                      oc = c(1.5, 1.5, 0.4, 0.2))
  expect_false(soilKey:::mollic_epipedon_usda(q)$passed)
})

test_that("massive and hard (FAO) passes the structure test; very hard fails (L33)", {
  testthat::skip_on_cran()
  st <- c("massive", "massive", "subangular blocky", "massive")
  p <- .v09421_mollic(st = st, rr = c("hard", "hard", "hard", "hard"))
  expect_true(soilKey:::mollic_epipedon_usda(p)$passed)
  q <- .v09421_mollic(st = st, rr = c("very hard", "very hard", "hard", "hard"))
  expect_false(soilKey:::mollic_epipedon_usda(q)$passed)
  r <- .v09421_mollic(st = st, rr = c(NA, NA, "hard", "hard"))
  m <- soilKey:::mollic_epipedon_usda(r)
  expect_true(is.na(m$passed))
  expect_true("rupture_resistance" %in% m$missing)
})

test_that("base saturation not measured is undecided, with no pH proxy (L28)", {
  testthat::skip_on_cran()
  p <- .v09421_mollic(bs = c(NA, NA, NA, NA))
  m <- soilKey:::mollic_epipedon_usda(p)
  expect_true(is.na(m$passed))
  expect_true("bs_pct" %in% m$missing)
})

test_that("item 7: 10 cm directly above a lithic contact, 18 cm otherwise", {
  testthat::skip_on_cran()
  p <- .v09421_pedon(top_cm = c(0, 12), bottom_cm = c(12, 40), designation = c("A", "R"),
                     munsell_value_moist = c(2, NA), munsell_value_dry = c(3, NA),
                     munsell_chroma_moist = c(1, NA), oc_pct = c(3, NA), bs_pct = c(80, NA),
                     clay_pct = c(30, NA), sand_pct = c(30, NA), silt_pct = c(40, NA),
                     structure_type = c("granular", NA), rupture_resistance = c("soft", NA))
  m <- soilKey:::mollic_epipedon_usda(p)
  expect_true(m$passed)
  expect_equal(m$evidence$required_cm, 10)
  q <- .v09421_mollic(vm = c(2, 5, 5, 6), vd = c(3, 7, 7, 7), top = c(0, 12, 40, 90),
                      bottom = c(12, 40, 90, 150))
  expect_false(soilKey:::mollic_epipedon_usda(q)$passed)
})

test_that("a Mollisol sampled shallower than item I.2 is read at its deepest horizon, grade C (L19)", {
  testthat::skip_on_cran()
  p <- .v09421_mollic(site = list(soil_moisture_regime = "udic",
                                  soil_temperature_regime = "mesic"))
  mq <- soilKey:::mollisol_qualifying_usda(p)
  expect_true(mq$passed)
  expect_identical(mq$evidence$inferences,
    "base saturation read at 150 cm, the deepest horizon sampled, as a stand-in for 180 cm")
  r <- suppressWarnings(classify_usda(p, on_missing = "silent"))
  expect_match(r$rsg_or_order, "Mollisols")
  expect_identical(r$evidence_grade, "C")
  expect_true(any(grepl("stand-in for 180 cm", r$warnings)))
  # base saturation below 50 % in the zone decides FALSE
  q <- .v09421_mollic(bs = c(80, 80, 40, 40))
  expect_false(soilKey:::mollisol_qualifying_usda(q)$passed)
})

test_that("the Vertisols of item F: slickensides, clay, cracks (L30, L31)", {
  testthat::skip_on_cran()
  v <- function(clay = c(45, 50, 50), slick = c(NA, "common", "many"), cracks = NA,
                st = c("granular", "angular blocky", "angular blocky"))
    .v09421_pedon(top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 120),
                  designation = c("Ap", "Bss1", "Bss2"), clay_pct = clay,
                  slickensides = slick, cracks_width_cm = cracks, structure_type = st)
  r <- soilKey:::vertisol_qualifying_usda(v())
  expect_true(r$passed)
  expect_match(r$notes, "L30")
  # a horizon reaching into 18-50 cm with less than 30 % clay (L31)
  expect_false(soilKey:::vertisol_qualifying_usda(v(clay = c(45, 28, 50)))$passed)
  # no slickensides recorded, structure not recorded, no cracks: absent
  expect_false(soilKey:::vertisol_qualifying_usda(v(slick = NA, st = NA))$passed)
  # cracks recorded but no slickensides and no structure: item 1 undecided
  r <- soilKey:::vertisol_qualifying_usda(v(slick = NA, st = NA, cracks = 1))
  expect_true(is.na(r$passed))
  expect_true("structure_type" %in% r$missing)
})

test_that("Xererts need the temperature condition; regimes not recorded are undecided (L32)", {
  testthat::skip_on_cran()
  p <- make_vertisol_canonical()
  p$site$soil_moisture_regime <- "xeric"
  p$site$soil_temperature_regime <- "thermic"
  expect_true(soilKey:::xerert_qualifying_usda(p)$passed)
  p$site$soil_temperature_regime <- "isothermic"
  expect_false(soilKey:::xerert_qualifying_usda(p)$passed)
  p$site$soil_temperature_regime <- NULL
  expect_true(is.na(soilKey:::xerert_qualifying_usda(p)$passed))
})

test_that("Dystr- Vertisols read the pH and EC, not the base saturation (M8)", {
  testthat::skip_on_cran()
  d <- function(ph, ec, bs = 40)
    .v09421_pedon(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bss"),
                  ph_cacl2 = ph, ec_dS_m = ec, bs_pct = bs)
  expect_true(soilKey:::dystric_vertisol_usda(d(c(4.3, 4.4), c(1, 1)))$passed)
  expect_false(soilKey:::dystric_vertisol_usda(d(c(6, 6), c(1, 1), bs = 30))$passed)
  r <- soilKey:::dystric_vertisol_usda(d(c(4.3, 4.4), c(NA, NA)))
  expect_true(is.na(r$passed))
  expect_true("ec_dS_m" %in% r$missing)
})

test_that("Rendolls: thin mollic over 40 % CaCO3, udic or cryic (M4)", {
  testthat::skip_on_cran()
  # A over a thin calcareous C (too thin for a calcic horizon) over limestone
  r <- function(site) .v09421_pedon(top_cm = c(0, 25, 35), bottom_cm = c(25, 35, 60),
    designation = c("A", "C", "R"), munsell_value_moist = c(2, 6, NA),
    munsell_value_dry = c(3, 8, NA), munsell_chroma_moist = c(1, 2, NA),
    oc_pct = c(4, 0.3, NA), bs_pct = c(100, 100, NA), caco3_pct = c(10, 60, NA),
    clay_pct = c(30, 20, NA), sand_pct = c(30, 30, NA), silt_pct = c(40, 50, NA),
    structure_type = c("granular", "massive", NA),
    rupture_resistance = c("soft", "slightly hard", NA), site = site)
  expect_true(soilKey:::rendoll_qualifying_usda(r(list(soil_moisture_regime = "udic")))$passed)
  expect_false(soilKey:::rendoll_qualifying_usda(r(list(soil_moisture_regime = "ustic",
                                                        soil_temperature_regime = "mesic")))$passed)
  expect_true(is.na(soilKey:::rendoll_qualifying_usda(r(list()))$passed))
})

test_that("Pachic follows the mollic thickness; Entic Durustolls read the argillic over the duripan", {
  testthat::skip_on_cran()
  pa <- soilKey:::pachic_subgroup_usda(make_phaeozem_canonical())
  expect_true(pa$passed)
  expect_gte(pa$evidence$mollic_thickness_cm, 50)
  expect_false(soilKey:::pachic_subgroup_usda(.v09421_mollic())$passed)
  du <- .v09421_pedon(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 90),
                      designation = c("A", "Bw", "Bqm"), cementation_class = c(NA, NA, "indurated"),
                      duripan_pct = c(NA, NA, 90), clay_pct = c(20, 21, 21),
                      clay_films_amount = c(NA, "none", "none"))  # reading N5
  expect_true(soilKey:::entic_durustoll_usda(du)$passed)
})
