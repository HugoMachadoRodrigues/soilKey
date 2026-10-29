# v0.9.309 (ISRIC's KE009, from Jelle Janssen): four WRB 2022 qualifiers and
# the USDA key to the Oxisols read by the texts. The profile below is made up
# in the pattern of a clayey red Ferralsol (Rhodic Eutrustox) from Kenya; no
# ISRIC data is stored in the package. Its Ah is 18 cm thick: 20 cm would make
# it a mollic horizon.

.v09309_ferralsol <- function(oc = c(2.4, 0.8, 0.25, 0.3, 0.2, 0.15),
                              al = NA_real_, ph = c(6.6, 6.9, 6.5, 6.4, 6.3, 6.0)) {
  h <- data.frame(
    top_cm = c(0, 18, 32, 50, 80, 120), bottom_cm = c(18, 32, 50, 80, 120, 160),
    designation = c("Ah", "AB", "Bt1", "Bt2", "Bt3", "BC"),
    clay_pct = c(52, 63, 64, 65, 60, 58), silt_pct = c(15, 9, 7, 7, 11, 12),
    sand_pct = c(33, 28, 29, 28, 29, 30), oc_pct = oc, ph_h2o = ph,
    ph_kcl = ph - 0.7, cec_cmol = c(19, 12.5, 9.5, 9.7, 9.2, 8),
    ca_cmol = c(10, 6.5, 5, 4.6, 3.8, 3.2), mg_cmol = c(2.4, 1.4, 1.1, 1.2, 1.3, 1.3),
    k_cmol = c(1.5, 0.8, 0.4, 0.3, 0.1, 0.1), na_cmol = c(0, 0.1, 0.2, 0.3, 0.4, 0.2),
    al_cmol = al, fe_dcb_pct = c(6.1, 6.2, 6.3, 6.4, 6.5, 6.2),
    munsell_hue_moist = "2.5YR", munsell_value_moist = c(2.5, 3, 3, 3, 3, 3),
    munsell_chroma_moist = c(2, 4, 4, 4, 4, 4),
    munsell_hue_dry = "2.5YR", munsell_value_dry = c(3, 3, 4, 4, 4, 4),
    munsell_chroma_dry = c(2, 4, 5, 6, 7, 7),
    structure_type = c("subangular blocky", "subangular blocky", "massive", "massive", "massive", "massive"),
    structure_grade = c("weak", "weak", NA, NA, NA, NA),
    redoximorphic_features_pct = 0, slickensides = "absent", plinthite_pct = 0,
    stringsAsFactors = FALSE)
  h$bs_pct <- 100 * (h$ca_cmol + h$mg_cmol + h$k_cmol + h$na_cmol) / h$cec_cmol
  PedonRecord$new(site = list(id = "ke-like", country = "KE", soil_moisture_regime = "ustic",
                              soil_temperature_regime = "isohyperthermic"),
                  horizons = h)
}

test_that("Humic is the weighted average to 50 cm from the mineral surface", {
  skip_on_cran()
  p <- .v09309_ferralsol()
  r <- qual_humic(p)
  # (2.4 x 18 + 0.8 x 14 + 0.25 x 18) / 50 = 1.178
  expect_equal(r$evidence$oc_weighted, (2.4 * 18 + 0.8 * 14 + 0.25 * 18) / 50)
  expect_true(r$passed)
  expect_false(qual_humic(.v09309_ferralsol(oc = c(1.5, 0.8, 0.25, 0.3, 0.2, 0.15)))$passed)
  # carbon unknown in part of the 50 cm: undecided unless the rest already reaches 1%
  expect_true(is.na(qual_humic(.v09309_ferralsol(oc = c(1.2, NA, 0.25, 0.3, 0.2, 0.15)))$passed))
  expect_true(qual_humic(.v09309_ferralsol(oc = c(3, NA, 0.25, 0.3, 0.2, 0.15)))$passed)
  # below a limiting layer inside the 50 cm the range counts 0
  q <- .v09309_ferralsol(oc = c(2.2, 2.2, 0.25, 0.3, 0.2, 0.15))
  h <- q$horizons; h$designation[3:6] <- "R"; h$clay_pct[3:6] <- NA; h$oc_pct[3:6] <- NA
  q <- PedonRecord$new(site = q$site, horizons = h)
  expect_equal(qual_humic(q)$evidence$oc_weighted, 2.2 * 32 / 50)
})

test_that("Ochric needs the Humic criteria not met", {
  skip_on_cran()
  expect_false(qual_ochric(.v09309_ferralsol())$passed)
  expect_true(qual_ochric(.v09309_ferralsol(oc = c(1.5, 0.8, 0.25, 0.3, 0.2, 0.15)))$passed)
})

test_that("Eutric and Dystric read the pH when the cations are missing", {
  skip_on_cran()
  # no exchangeable Al: pH water 6.3-6.9 with < 2% carbon is above 4.6 (Table 9.3)
  expect_true(qual_eutric(.v09309_ferralsol())$passed)
  expect_false(qual_dystric(.v09309_ferralsol())$passed)
  acid <- .v09309_ferralsol(ph = c(4.4, 4.3, 4.2, 4.2, 4.3, 4.4))
  expect_true(qual_dystric(acid)$passed)
  expect_false(qual_eutric(acid)$passed)
  # the measured Al still wins over the pH
  al <- .v09309_ferralsol(al = c(20, 20, 20, 20, 20, 20))
  expect_true(qual_dystric(al)$passed)
  # carbon unknown: a pH between the class limits (4.6 and 5.4) is undecided
  st <- soilKey:::.wrb_ph_cation_status
  expect_true(is.na(st(5.0, NA, NA, 1)))
  expect_false(st(5.5, NA, NA, 1))
  expect_true(st(4.5, NA, NA, 1))
  expect_true(st(4.7, NA, 1, 1) == FALSE)
  expect_true(st(4.7, NA, 3, 1))                 # 2-7.5% carbon: the limit is 4.9
  expect_false(st(6.4, NA, 1, 4))                # bases >= 4 x Al
  expect_true(st(3.8, NA, 1, 4))                 # Al > 4 x bases
  expect_identical(st(5.0, NA, 1, 4), "mid")
  expect_identical(st(NA, 4.0, 1, 1), FALSE)     # KCl when water is missing
})

test_that("the ferric horizon is read from nodules, not from Fe-dithionite", {
  skip_on_cran()
  p <- .v09309_ferralsol()                       # 6% Fe-dithionite, no nodules recorded
  expect_false(isTRUE(ferric(p)$passed))
  expect_false(isTRUE(qual_ferric(p)$passed))
  h <- p$horizons
  h$ferric_nodules_pct <- c(0, 0, 10, 12, 0, 0)  # 48 cm with >= 5% nodules
  expect_true(ferric(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$ferric_nodules_pct <- 0                      # recorded none, and no mottles
  expect_false(ferric(PedonRecord$new(site = p$site, horizons = h))$passed)
  h$ferric_nodules_pct <- NA; h$redoximorphic_features_pct <- 0
  expect_true(is.na(ferric(PedonRecord$new(site = p$site, horizons = h))$passed))
})

test_that("the kandic horizon follows KST 13", {
  skip_on_cran()
  k <- soilKey:::kandic_horizon_usda(.v09309_ferralsol())
  expect_true(k$passed)
  expect_equal(k$evidence$upper_boundary_cm, 18)  # 52 -> 63% clay: > 40% overlying, +11 (8 needed)
  # too small an increase: 52 -> 58% (+6, 8 needed over > 40%)
  p <- .v09309_ferralsol(); h <- p$horizons; h$clay_pct <- c(52, 58, 58, 58, 57, 56)
  expect_false(isTRUE(soilKey:::kandic_horizon_usda(PedonRecord$new(site = p$site, horizons = h))$passed))
  # high-activity clay: CEC 30 per kg clay throughout the window
  h <- p$horizons; h$cec_cmol <- 0.30 * h$clay_pct
  expect_false(isTRUE(soilKey:::kandic_horizon_usda(PedonRecord$new(site = p$site, horizons = h))$passed))
})

test_that("the key to the Oxisols reads both items of KST 13", {
  skip_on_cran()
  p <- .v09309_ferralsol()
  o <- soilKey:::oxisol_usda(p)
  expect_true(o$passed)
  expect_false(o$evidence$item1_oxic_no_kandic)      # a kandic horizon within 150 cm
  expect_true(o$evidence$item2_clayey_surface_kandic) # >= 40% clay in 0-18 cm and a kandic within 100 cm
  expect_identical(classify_usda(p, on_missing = "silent")$name, "Rhodic Eutrustox")
  # the same profile with a loamy surface (30% clay) is no Oxisol by item 2
  h <- p$horizons; h$clay_pct[1] <- 30; h$sand_pct[1] <- 55
  expect_false(isTRUE(soilKey:::oxisol_usda(PedonRecord$new(site = p$site, horizons = h))$passed))
})

test_that("the made-up Kenyan Ferralsol gets ISRIC's kind of WRB name", {
  skip_on_cran()
  expect_identical(classify_wrb2022(.v09309_ferralsol(), on_missing = "silent")$name,
                   "Rhodic Ferralsol (Clayic, Epic, Eutric, Humic)")
})
