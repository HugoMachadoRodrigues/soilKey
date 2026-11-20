# v0.9.411: WRB 2022 andic and humus qualifiers (Aluandic, Silandic,
# Eutrosilic, Protospodic, Mulmic, Hyperhumic, Humic, Ochric) and the mollic,
# umbric and chernic horizons. The profiles are made up.

.v09411_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = utils::modifyList(list(id = "v09411", lat = 0, lon = 0,
                                                country = "TEST"), site),
                  horizons = ensure_horizon_schema(hz))
}

# An Andosol-like profile: andic layers by Al_ox + 1/2 Fe_ox >= 2 and bulk
# density <= 0.9 (WRB 2022 Ch 3.2.3, criteria 1 and 2).
.v09411_andic <- function(si, ca = c(3, 3, 3), top = c(0, 20, 50),
                          bottom = c(20, 50, 100)) {
  .v09411_pedon(top_cm = top, bottom_cm = bottom,
                designation = c("Ah", "Bw1", "Bw2"),
                al_ox_pct = c(3, 4, 4), fe_ox_pct = c(2, 2, 2), si_ox_pct = si,
                bulk_density_g_cm3 = c(0.6, 0.6, 0.7), oc_pct = c(6, 3, 2),
                ca_cmol = ca, mg_cmol = c(1, 1, 1), k_cmol = c(0.5, 0.5, 0.5),
                na_cmol = c(0.1, 0.1, 0.1),
                clay_pct = c(20, 22, 22), silt_pct = c(40, 40, 40),
                sand_pct = c(40, 38, 38))
}

test_that("Aluandic and Silandic split the andic layers by Si_ox 0.6% (p. 128, 146)", {
  skip_on_cran()
  # Si_ox well below the old Al/Si ratio boundary but >= 0.6%: Silandic
  p <- .v09411_andic(si = c(1.5, 2, 2))
  expect_true(qual_silandic(p)$passed)
  expect_false(qual_aluandic(p)$passed)
  p <- .v09411_andic(si = c(0.3, 0.4, 0.5))
  expect_true(qual_aluandic(p)$passed)
  expect_false(qual_silandic(p)$passed)
  # >= 15 cm each: Ah 0-20 Si 0.3, Bw 20-100 Si 1.2 -> both apply
  p <- .v09411_andic(si = c(0.3, 1.2, 1.2))
  expect_true(qual_aluandic(p)$passed)
  expect_true(qual_silandic(p)$passed)
  # 10 cm of Si < 0.6% is not enough
  q <- .v09411_andic(si = c(0.3, 1.2, 1.2), top = c(0, 10, 50), bottom = c(10, 50, 100))
  expect_false(qual_aluandic(q)$passed)
  # Si_ox not recorded: undecided
  expect_true(is.na(qual_silandic(.v09411_andic(si = c(NA, NA, NA)))$passed))
  expect_true(is.na(qual_aluandic(.v09411_andic(si = c(NA, NA, NA)))$passed))
})

test_that("Aluandic and Silandic at different depths: both with specifiers (Ch 2.1, p. 24-25)", {
  skip_on_cran()
  p <- .v09411_andic(si = c(0.3, 1.2, 1.2))
  r <- resolve_wrb_qualifiers(p, "AN")
  expect_true("Aluandic" %in% r$principal)       # without specifiers: the first
  expect_false("Silandic" %in% r$principal)
  r <- resolve_wrb_qualifiers(p, "AN", specifiers = TRUE)
  expect_true("Epialuandic" %in% r$principal)
  expect_true("Katosilandic" %in% r$principal)
})

test_that("Eutrosilic: >= 30 cm of andic layers with bases >= 15 cmol/kg (p. 133)", {
  skip_on_cran()
  # no Silandic needed: Si_ox < 0.6% throughout
  rich <- .v09411_andic(si = c(0.3, 0.3, 0.3), ca = c(20, 20, 20))
  expect_false(qual_silandic(rich)$passed)
  expect_true(qual_eutrosilic(rich)$passed)
  # only the 20 cm Ah is rich: < 30 cm
  thin <- .v09411_andic(si = c(1, 1, 1), ca = c(20, 3, 3))
  expect_false(qual_eutrosilic(thin)$passed)
})

test_that("Protospodic: a lowest Al_ox of 0 above gives no threshold (p. 147)", {
  skip_on_cran()
  mk <- function(al) .v09411_pedon(top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 60),
                                   designation = c("A", "B", "C"),
                                   al_ox_pct = al, oc_pct = c(1, 0.5, 0.2),
                                   clay_pct = 5, silt_pct = 5, sand_pct = 90)
  expect_false(isTRUE(qual_protospodic(mk(c(0, 0.01, 0.01)))$passed))
  expect_false(isTRUE(qual_protospodic(mk(c(0.4, 0.5, 0.5)))$passed))
})

test_that("Mulmic needs structure, drained organic origin and 10 cm at the surface (p. 90, 141)", {
  skip_on_cran()
  mk <- function(type = "subangular blocky", size = "fine", deep_oc = 35,
                 top = c(0, 15), bottom = c(15, 60)) {
    .v09411_pedon(top_cm = top, bottom_cm = bottom,
                  designation = c("Ap", if (deep_oc >= 20) "Oa" else "Bw"),
                  oc_pct = c(12, deep_oc), munsell_value_moist = c(2, 2),
                  munsell_chroma_moist = c(1, 1), structure_type = c(type, NA),
                  structure_size = c(size, NA))
  }
  expect_true(qual_mulmic(mk())$passed)
  # granular or massive: no
  expect_false(qual_mulmic(mk(type = "granular"))$passed)
  expect_false(qual_mulmic(mk(type = NA, size = NA))$passed)     # not recorded = absent
  # no organic material below and no drainage record: not mulmic
  expect_false(qual_mulmic(mk(deep_oc = 3))$passed)
  # a drainage record stands for the origin
  p <- mk(deep_oc = 3); p$site$drainage_class <- "artificially drained"
  expect_true(qual_mulmic(p)$passed)
  # < 10 cm thick
  expect_false(qual_mulmic(mk(top = c(0, 8), bottom = c(8, 60)))$passed)
})

test_that("Hyperhumic and the Humic bound (p. 137)", {
  skip_on_cran()
  mk <- function(oc) .v09411_pedon(top_cm = c(0, 25, 50), bottom_cm = c(25, 50, 100),
                                   designation = c("Ah", "Bw", "C"), oc_pct = oc)
  expect_true(qual_hyperhumic(mk(c(8, 4, 1)))$passed)        # (8 + 4) / 2 = 6
  expect_false(qual_hyperhumic(mk(c(5, 3, 1)))$passed)       # 4
  expect_true(qual_humic(mk(c(5, 3, 1)))$passed)
  # PK004-like: 1 cm unrecorded cannot lift 0.09% to 1% (mineral SOC < 20%)
  p <- .v09411_pedon(top_cm = c(0, 1, 9, 30), bottom_cm = c(1, 9, 30, 60),
                     designation = c("Ak1", "Ak2", "Bt1", "Bt2"),
                     oc_pct = c(NA, 0.26, 0.05, 0.07))
  expect_false(qual_humic(p)$passed)
  expect_true(qual_ochric(p)$passed)          # (0.26 x 8 + 0.05 x 1) / 10 >= 0.2
  # 14 cm unrecorded could: undecided
  expect_true(is.na(qual_humic(mk(c(0.8, NA, 0.2)))$passed))
})

test_that("Humic gives way to Hyperhumic and Profundihumic", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  expect_identical(ns$.suppress_qualifier_siblings(c("Hyperhumic", "Humic")), "Hyperhumic")
  expect_identical(ns$.drop_redundant_qualifiers(c("Dystric", "Humic"),
                                                 c("Profundihumic", "Dystric", "Humic")),
                   "Dystric")
  # exactly Humic: Profundihumic and Hyperhumic stay
  expect_identical(ns$.drop_redundant_qualifiers(c("Hyperhumic", "Profundihumic", "Humic"),
                                                 c("Hyperhumic", "Profundihumic", "Humic")),
                   c("Hyperhumic", "Profundihumic"))
})

test_that("Ochric: weighted average over the upper 10 cm of the mineral soil (p. 142)", {
  skip_on_cran()
  mk <- function(oc) .v09411_pedon(top_cm = c(0, 2, 30), bottom_cm = c(2, 30, 60),
                                   designation = c("A1", "A2", "B"), oc_pct = oc,
                                   munsell_value_moist = 5, munsell_chroma_moist = 4)
  # 2 cm at 0.3% over 0.15%: (0.6 + 1.2) / 10 = 0.18
  expect_false(qual_ochric(mk(c(0.3, 0.15, 0.1)))$passed)
  expect_true(qual_ochric(mk(c(0.5, 0.2, 0.1)))$passed)
  # no carbon recorded at all: undecided
  expect_true(is.na(qual_ochric(mk(c(NA, NA, NA)))$passed))
})

test_that("Mollic criterion 4: >= 0.6% more SOC than a dark parent material (p. 52)", {
  skip_on_cran()
  mk <- function(c_oc) .v09411_pedon(top_cm = c(0, 15, 55), bottom_cm = c(15, 55, 90),
                                     designation = c("Ah", "Bw", "C"),
                                     oc_pct = c(1.11, 0.86, c_oc),
                                     munsell_value_moist = 3, munsell_chroma_moist = 1,
                                     munsell_value_dry = 4, bs_pct = 100, ph_h2o = 6.5,
                                     structure_type = "subangular blocky",
                                     structure_size = "fine",
                                     clay_pct = 40, silt_pct = 30, sand_pct = 30)
  expect_false(isTRUE(mollic(mk(0.9))$passed))       # NI009: 1.11 < 0.90 + 0.6
  expect_true(mollic(mk(0.2))$passed)
})

test_that("Mollic colour proxy: part of the colour recorded within 3a, >= 1.5% SOC (L11)", {
  skip_on_cran()
  mk <- function(chroma) .v09411_pedon(top_cm = c(0, 20), bottom_cm = c(20, 65),
                                       designation = c("Ap", "Bw"),
                                       oc_pct = c(2.25, 0.5),
                                       munsell_value_moist = c(2, 5),
                                       munsell_chroma_moist = c(chroma, 1),
                                       bs_pct = 90, ph_h2o = 7.5,
                                       structure_type = "subangular blocky",
                                       structure_size = "medium",
                                       clay_pct = 50, silt_pct = 30, sand_pct = 20)
  expect_true(mollic(mk(NA))$passed)          # CU020: value 2, no chroma, 2.25%
  expect_false(isTRUE(mollic(mk(4))$passed))  # a recorded chroma 4 decides
})

test_that("Chernic: coarse fragments not recorded count as none (p. 40)", {
  skip_on_cran()
  p <- .v09411_pedon(top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 95),
                     designation = c("Ah", "AB", "Bt"),
                     oc_pct = c(2.4, 1.2, 0.7), munsell_value_moist = c(3, 3, 3),
                     munsell_chroma_moist = c(1, 2, 4), bs_pct = 90, ph_h2o = 7.5,
                     structure_type = "subangular blocky",
                     structure_size = c("very fine", "fine", "fine"),
                     coarse_fragments_pct = c(NA, NA, 3.5),
                     clay_pct = c(45, 60, 77), silt_pct = 30, sand_pct = c(25, 10, 3))
  expect_true(chernic(p)$passed)                # MZ005: 0-50 cm
})
