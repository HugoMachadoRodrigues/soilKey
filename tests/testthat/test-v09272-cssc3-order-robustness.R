# v0.9.272: the orders and great groups of the Canadian System of Soil
# Classification read open bottoms, other notations and climate spellings
# as the subgroups do.

.ob <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.ob_class <- function(h, site = list()) classify_cssc3(.ob(h, site), on_missing = "silent")
.ob_chern <- list(mean_annual_soil_temp_C = 4, soil_moisture_subclass = "subhumid")

test_that("a deepest horizon with no bottom leaves the order undecided", {
  skip_on_cran()
  lv <- list(designation = c("LFH", "Ae", "Bt"), top_cm = c(0, 4, 20), bottom_cm = c(4, 20, NA),
             oc_pct = c(35, 0.5, 0.4), clay_pct = c(NA, 12, 28),
             clay_films_amount = c(NA, "none", "common"))
  r <- .ob_class(lv)
  expect_true("orders:H" %in% r$provisional_because)
  expect_true(is.na(soilKey:::bt_horizon_cssc(.ob(lv))$passed))
  lv$bottom_cm[3] <- 60
  expect_identical(.ob_class(lv)$name, "Orthic Gray Luvisol")
  r <- .ob_class(list(designation = c("Ah", "Bm"), top_cm = c(0, 8), bottom_cm = c(8, NA),
                      oc_pct = c(3, 0.8), ph_cacl2 = c(5.6, 5.9)))
  expect_true("orders:I" %in% r$provisional_because)
  r <- .ob_class(list(designation = c("LFH", "Ae", "Bfc"), top_cm = c(0, 5, 12),
                      bottom_cm = c(5, 12, NA), oc_pct = c(40, 0.6, 1.5)))
  expect_true("orders:D" %in% r$provisional_because)
  # a chernozemic A of unknown thickness
  ch <- soilKey:::chernozemic_a_cssc(.ob(list(designation = "Ah", top_cm = 0, bottom_cm = NA,
    munsell_value_dry = 4, munsell_value_moist = 2, munsell_chroma_moist = 2, oc_pct = 3,
    n_total_pct = 0.25, bs_pct = 95), .ob_chern))
  expect_true(is.na(ch$passed))
})

test_that("adjacent Bt horizons are added up towards the 5 cm", {
  skip_on_cran()
  bt <- function(d, top, bottom) soilKey:::bt_horizon_cssc(.ob(list(
    designation = d, top_cm = top, bottom_cm = bottom)))$passed
  expect_true(bt(c("Ae", "Btg1", "Btg2", "C"), c(0, 10, 13, 16), c(10, 13, 16, 50)))
  expect_false(bt(c("Ae", "Btg", "C"), c(0, 10, 13), c(10, 13, 50)))
  # a thin Btg on top of a Bt is the gleyed part of the Bt
  lg <- soilKey:::luvic_gleysol_cssc(.ob(list(designation = c("LFH", "Btg", "Bt"),
    top_cm = c(0, 10, 14), bottom_cm = c(10, 14, 34))))
  expect_true(lg$passed)
  lg <- soilKey:::luvic_gleysol_cssc(.ob(list(designation = c("Ap", "Bg", "Btg"),
    top_cm = c(0, 40, 44), bottom_cm = c(40, 44, NA))))
  expect_true(is.na(lg$passed))
})

test_that("the word 'and' of a split horizon does not switch the notation off", {
  p <- .ob(list(designation = c("Ah", "Ae and Bt", "Bt", "Ck"), top_cm = c(0, 10, 20, 50),
                bottom_cm = c(10, 20, 50, 90)))
  expect_true(soilKey:::.cssc_horizons(p)$notation$ok)
  p <- .ob(list(designation = c("Ah", "E and Bt", "Bt"), top_cm = c(0, 10, 20),
                bottom_cm = c(10, 20, 50)))
  expect_false(soilKey:::.cssc_horizons(p)$notation$ok)
})

test_that("the climate subclass is read whatever its spelling", {
  cl <- function(w) soilKey:::.cssc_chernozemic_climate(list(mean_annual_soil_temp_C = 4,
                                                               soil_moisture_subclass = w))$passed
  expect_true(cl("Sub-humid"))
  expect_true(cl("Subhumid "))
  expect_true(cl("SEMI-ARID"))
  expect_false(cl("Per-humid"))
  expect_true(is.na(cl("subaquic")))
  expect_true(is.na(cl("")))
})

usda_chern <- function(d, ...) {
  h <- list(designation = d, top_cm = c(0, 18, 40, 70), bottom_cm = c(18, 40, 70, 100),
            munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 4, 5),
            munsell_chroma_moist = c(2, 3, 3, 3), munsell_value_dry = c(4, 5, 6, 7),
            munsell_chroma_dry = c(2, 3, 3, 3), oc_pct = c(3, 1, 0.6, 0.4),
            n_total_pct = c(0.25, 0.09, NA, NA), bs_pct = c(95, 98, 100, 100), ca_cmol = 18,
            mg_cmol = 4, k_cmol = 1, na_cmol = 0.1, clay_pct = 25)
  utils::modifyList(h, list(...))
}

test_that("a Chernozem described in another notation reaches its order", {
  skip_on_cran()
  r <- .ob_class(usda_chern(c("A", "Bw", "Bk", "C")), .ob_chern)
  expect_identical(r$rsg_or_order, "Chernozemic")
  expect_identical(r$levels$name[2], "Dark Brown Chernozem")
  # an E: thinner than the A above passes, a light thick one fails, else undecided
  ae <- function(h) soilKey:::chernozemic_ae_cssc(.ob(h, .ob_chern))$passed
  expect_true(ae(usda_chern(c("A", "E", "Bw", "C"), top_cm = c(0, 18, 22, 70),
                            bottom_cm = c(18, 22, 70, 100))))
  expect_false(ae(usda_chern(c("A", "E", "Bw", "C"))))
  expect_true(is.na(ae(usda_chern(c("A", "E", "Bw", "C"), munsell_value_dry = c(4, 4, 6, 7)))))
  # a buried Eb does not count
  expect_true(ae(usda_chern(c("A", "Bw", "Eb", "C"))))
})

test_that("other notations: t, s and h on a B, and the E of the Solonetzic soils", {
  skip_on_cran()
  # a B with t is a Bt by designation, a B without one is not
  lu <- list(designation = c("Oi", "E", "Bt", "C"), top_cm = c(0, 4, 20, 60),
             bottom_cm = c(4, 20, 60, 100), oc_pct = c(35, 0.5, 0.4, 0.3))
  expect_true(soilKey:::bt_horizon_cssc(.ob(lu))$passed)
  expect_identical(.ob_class(lu)$rsg_or_order, "Luvisolic")
  lu$designation[3] <- "Bw"
  expect_false(soilKey:::bt_horizon_cssc(.ob(lu))$passed)
  # a Bw is not podzolic B material by designation, a Bs is; an E is never
  pz <- list(designation = c("Oe", "E", "Bw", "C"), top_cm = c(0, 5, 12, 40),
             bottom_cm = c(5, 12, 40, 90), oc_pct = c(NA, 0.6, 1.5, 0.2))
  expect_false(soilKey:::podzolic_b_cssc(.ob(pz))$passed)
  pz$designation[3] <- "Bs"
  expect_true(soilKey:::podzolic_b_cssc(.ob(pz))$passed)
  pz <- list(designation = c("A", "E", "C"), top_cm = c(0, 5, 25), bottom_cm = c(5, 25, 60),
             oc_pct = c(3, 1.5, 0.2), clay_pct = 6, sand_pct = 80, fe_pyro_pct = c(0.1, 0.6, 0.05),
             al_pyro_pct = c(0.1, 0.4, 0.05), munsell_hue_moist = "5YR",
             munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 6, 3))
  expect_false(soilKey:::podzolic_b_cssc(.ob(pz))$passed)
  # the E and the AB of a Solod in another notation
  so <- .ob(list(designation = c("A", "E", "AB", "Btn", "C"), top_cm = c(0, 10, 14, 22, 40),
                 bottom_cm = c(10, 14, 22, 40, 90)))
  expect_true(soilKey:::solod_cssc(so)$passed)
})

test_that("review fixes: designated Bt of unknown thickness, Bt runs across 50 cm", {
  skip_on_cran()
  bt <- function(h, ...) soilKey:::bt_horizon_cssc(.ob(h), ...)$passed
  # only an Ap above: the designation decides, and its thickness is unknown
  expect_true(is.na(bt(list(designation = c("Ap", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, NA),
                            clay_pct = c(30, 32), clay_films_amount = c(NA, "common")))))
  # a Bt of 48-70 cm begins within 50 cm, whatever part lies below it
  p <- list(designation = c("Ae", "Bf", "Bt1", "Bt2", "C"), top_cm = c(0, 10, 48, 51, 70),
            bottom_cm = c(10, 48, 51, 70, 90))
  expect_true(bt(p, max_top_cm = 50))
  # overlapping rows are not counted twice; a gap ends the run
  expect_false(bt(list(designation = c("Ae", "Bt", "Btg", "C"), top_cm = c(0, 20, 20, 23),
                       bottom_cm = c(20, 23, 23, 60))))
  expect_false(bt(list(designation = c("Ae", "Bt", "Bt", "C"), top_cm = c(0, 20, 30, 33),
                       bottom_cm = c(20, 23, 33, 60))))
  # a Bt with no clay recorded is not overruled by the clay of other rows
  expect_true(bt(list(designation = c("Ae", "Bt", "Bm", "C"), top_cm = c(0, 10, 30, 50),
                      bottom_cm = c(10, 30, 50, 90), clay_pct = c(10, NA, NA, 20))))
  # an undesignated horizon in a designated profile may be the Bt
  expect_true(is.na(bt(list(designation = c("Ae", NA, "C"), top_cm = c(0, 10, 40),
                            bottom_cm = c(10, 40, 80)))))
  # in another notation a C is not a Bt, whatever its clay
  us <- function(h) soilKey:::bt_horizon_cssc(.ob(h, list(country = "US")))$passed
  expect_false(us(list(designation = c("A", "E", "C"), top_cm = c(0, 10, 25),
                       bottom_cm = c(10, 25, 80), clay_pct = c(12, 10, 30),
                       clay_films_amount = c(NA, NA, "common"))))
})

test_that("review fixes: open horizons in the Bf, Bt and chernozemic A tests", {
  skip_on_cran()
  # a Bf already more than 10 cm is no thin Bf, open or not
  expect_false(soilKey:::brunisolic_b_cssc(.ob(list(designation = c("Ah", "Bf1", "Bf2"),
    top_cm = c(0, 8, 20), bottom_cm = c(8, 20, NA))))$passed)
  # a Luvisolic Cryosol whose Bt is already 10 cm
  expect_true(soilKey:::luvisolic_cryosol_cssc(.ob(list(designation = c("Ah", "Aey", "Bty1", "Bty2"),
    top_cm = c(0, 5, 20, 30), bottom_cm = c(5, 20, 30, NA))))$passed)
  # a pale open Ah fails the chernozemic A whatever its thickness
  ch <- soilKey:::chernozemic_a_cssc(.ob(list(designation = "Ah", top_cm = 0, bottom_cm = NA,
    munsell_value_dry = 6, munsell_value_moist = 4, munsell_chroma_moist = 3, oc_pct = 0.5,
    n_total_pct = 0.05, bs_pct = 40), .ob_chern))
  expect_false(ch$passed)
  # an open Ah under a closed one: decided only where both weights agree
  ch <- soilKey:::chernozemic_a_cssc(.ob(list(designation = c("Ah1", "Ah2"), top_cm = c(0, 6),
    bottom_cm = c(6, NA), munsell_value_dry = c(4, 6), munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3), oc_pct = 3, n_total_pct = 0.25, bs_pct = 95), .ob_chern))
  expect_true(is.na(ch$passed))
})

test_that("review fixes: capitals, sulfides and E-B transitions in other notations", {
  skip_on_cran()
  us <- list(country = "US")
  pz <- list(designation = c("A", "E", "BHS", "C"), top_cm = c(0, 5, 12, 40),
             bottom_cm = c(5, 12, 40, 90), oc_pct = c(3, 0.6, 1.5, 0.2), clay_pct = 6,
             sand_pct = 80, fe_pyro_pct = c(0.1, 0.05, 0.6, 0.05), al_pyro_pct = c(0.1, 0.05, 0.4, 0.05),
             munsell_hue_moist = "5YR", munsell_value_moist = c(3, 6, 4, 5),
             munsell_chroma_moist = c(2, 2, 6, 3))
  expect_true(soilKey:::podzolic_b_cssc(.ob(pz, us))$passed)
  expect_identical(soilKey:::.cssc_other_case(c("BHS", "2BT1", "BC", "AB", "Bt", "E/B")),
                   c("Bhs", "2Bt1", "BC", "AB", "Bt", "E/B"))
  # Bse (sulfides) is not podzolic B by designation
  expect_false(soilKey:::podzolic_b_cssc(.ob(list(designation = c("A", "Bse", "C"),
    top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80)), us))$passed)
  # the E-B transition of a Solod
  expect_true(soilKey:::solod_cssc(.ob(list(designation = c("A", "E", "EB", "Btn", "C"),
    top_cm = c(0, 10, 14, 22, 40), bottom_cm = c(10, 14, 22, 40, 90)), us))$passed)
})

test_that("second review fixes: the Bt decided horizon by horizon", {
  skip_on_cran()
  bt <- function(h, ...) soilKey:::bt_horizon_cssc(.ob(h), ...)$passed
  # a thin deep IIBt does not void the failed clay test of the Bt
  expect_false(bt(list(designation = c("Ae", "Bt", "BC", "IIBt"), top_cm = c(0, 20, 40, 100),
                       bottom_cm = c(20, 40, 100, 102), clay_pct = c(20, 21, 20, 30),
                       clay_films_amount = c(NA, "common", NA, NA))))
  # an Ae with no clay: the Bt cannot be compared with it, the designation decides
  expect_true(bt(list(designation = c("Ah", "Ae", "Bt1", "Bt2"), top_cm = c(0, 5, 15, 19),
                      bottom_cm = c(5, 15, 19, 40), clay_pct = c(25, NA, 30, 28),
                      clay_films_amount = c(NA, NA, "common", "common"))))
  # clay on an organic layer is no comparison: the first mineral horizon is no Bt by properties
  expect_false(bt(list(designation = c("LFH", "Bm", "C"), top_cm = c(0, 5, 30),
                       bottom_cm = c(5, 30, 80), oc_pct = c(40, 1, 0.3), clay_pct = c(5, 30, 28),
                       clay_films_amount = c(NA, "common", NA))))
  # clay films recorded as absent rule a horizon out, even untested or under an Ap
  expect_false(bt(list(designation = c("A", "Eg", "Btg", "Cg"), top_cm = c(0, 10, 25, 60),
                       bottom_cm = c(10, 25, 60, 90), clay_films_amount = c(NA, NA, "none", NA))))
  expect_false(bt(list(designation = c("Ap", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, 60),
                       clay_films_amount = c(NA, "none"))))
  # rows given in any order, with a tie in their tops, give the same answer
  a <- list(designation = c("Ae", "Bt1", "Btg", "Bt2", "C"), top_cm = c(0, 20, 20, 23, 26),
            bottom_cm = c(20, 23, 21, 26, 60))
  b <- lapply(a, function(v) v[c(1, 3, 2, 4, 5)])
  expect_identical(classify_cssc3(.ob(a), on_missing = "silent")$name,
                   classify_cssc3(.ob(b), on_missing = "silent")$name)
})

test_that("second review fixes: chernozemic A thresholds, capitals, overlaps", {
  skip_on_cran()
  # an Ah of 15 cm over an open Ah2: some thickness of Ah2 passes the colour
  ch <- soilKey:::chernozemic_a_cssc(.ob(list(designation = c("Ah1", "Ah2"), top_cm = c(0, 15),
    bottom_cm = c(15, NA), munsell_value_dry = c(5, 5.8), munsell_value_moist = c(3.8, 3.0),
    munsell_chroma_moist = c(2, 2), oc_pct = 3, n_total_pct = 0.25, bs_pct = 95), .ob_chern))
  expect_true(is.na(ch$crit_colour %||% ch$evidence$colour))
  # a transition in capitals stays a transition
  expect_identical(soilKey:::.cssc_other_case(c("BCT", "AP", "EBT", "OA")), c("BCt", "Ap", "EBt", "Oa"))
  # a Bf recorded twice over the same depth is counted once
  cover <- soilKey:::.cssc_cover(soilKey:::.cssc_horizons(.ob(list(designation = c("Ae", "Bf", "Bf"),
    top_cm = c(0, 10, 10), bottom_cm = c(10, 16, 16)))), 2:3)
  expect_identical(cover$cm, 6)
  expect_true(soilKey:::brunisolic_b_cssc(.ob(list(designation = c("Ae", "Bf", "Bf", "C"),
    top_cm = c(0, 10, 10, 16), bottom_cm = c(10, 16, 16, 60))))$passed)
  # an undesignated open horizon under a thin Btg may be part of the Bt
  lg <- soilKey:::luvic_gleysol_cssc(.ob(list(designation = c("Aeg", "Btg", NA),
    top_cm = c(0, 10, 13), bottom_cm = c(10, 13, NA))))
  expect_true(is.na(lg$passed))
  # an open Ah sharing its top with another A horizon does not stop the test
  expect_no_error(soilKey:::chernozemic_a_cssc(.ob(list(designation = c("Ah", "Ahe", "Ap"),
    top_cm = c(0, 20, 20), bottom_cm = c(20, NA, 30)), .ob_chern)))
})

test_that("third review fixes: the eluvial comparison, discontinuities, open Btg and Bf", {
  skip_on_cran()
  bt <- function(h, ...) soilKey:::bt_horizon_cssc(.ob(h), ...)$passed
  # a missing clay on the Ah does not reopen a Bt that the measured Ae rules out
  expect_false(bt(list(designation = c("Ah", "Ae", "Bt", "C"), top_cm = c(0, 8, 20, 50),
                       bottom_cm = c(8, 20, 50, 90), clay_pct = c(NA, 20, 21, 20),
                       clay_films_amount = c(NA, "none", "common", "none"))))
  # a IIBt under a IIAe is compared with it (no discontinuity between them)
  expect_false(bt(list(designation = c("Ah", "IIAe", "IIBt", "IIC"), top_cm = c(0, 10, 25, 50),
                       bottom_cm = c(10, 25, 50, 90), clay_pct = c(15, 20, 21, 20),
                       clay_films_amount = c(NA, NA, "common", NA))))
  # an open Btg run that may reach 5 cm leaves the Luvic Gleysol undecided
  lg <- soilKey:::luvic_gleysol_cssc(.ob(list(designation = c("Aeg", "Bt", "BC", "Btg"),
    top_cm = c(0, 20, 30, 40), bottom_cm = c(20, 30, 40, NA))))
  expect_true(is.na(lg$passed))
  # a Bf ruled out by its chemistry does not count by its designation
  pz <- list(designation = c("Ae", "Bf", "Bm"), top_cm = c(0, 10, 30), bottom_cm = c(10, 30, NA),
             oc_pct = c(0.5, 0.3, 0.3), clay_pct = 6, sand_pct = 80, fe_pyro_pct = c(0.05, 0.05, NA),
             al_pyro_pct = c(0.05, 0.05, NA), munsell_hue_moist = "10YR",
             munsell_value_moist = c(6, 5, 5), munsell_chroma_moist = c(2, 3, 3))
  expect_false(soilKey:::podzolic_b_cssc(.ob(pz))$passed)
  # a Luvisolic Cryosol whose Bt may go on in an open IIBty
  cr <- soilKey:::luvisolic_cryosol_cssc(.ob(list(designation = c("Ah", "Aey", "Bty", "BCy", "IIBty"),
    top_cm = c(0, 5, 20, 26, 30), bottom_cm = c(5, 20, 26, 30, NA))))
  expect_true(is.na(cr$passed))
})
