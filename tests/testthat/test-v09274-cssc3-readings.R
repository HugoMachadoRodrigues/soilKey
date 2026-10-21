# v0.9.274: three readings of the Canadian System of Soil Classification that
# the NPDB benchmark raised: chroma 1 or less counts as gley colour only in
# materials that brighten when oxidised; a soil with a Bt within 50 cm is
# Gleysolic only when the gley colours are in the Bt; base saturation of the
# chernozemic A is read as by a neutral (unbuffered) salt.

.rd <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.gley <- function(h) soilKey:::gleying_cssc(.rd(h))$passed

test_that("chroma 1 or less counts where the material brightens when oxidised", {
  skip_on_cran()
  grey <- list(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 90),
               munsell_hue_moist = c("10YR", "2.5Y", "10YR"),
               munsell_value_moist = c(3, 6, 5), munsell_chroma_moist = c(2, 1, 4))
  # the describer decides: a Ck is grey of its own, a Ckg is gleyed, a Ckgj
  # is only weakly gleyed
  expect_false(.gley(c(list(designation = c("Ah", "Ck", "C")), grey)))
  expect_true(.gley(c(list(designation = c("Ah", "Ckg", "C")), grey)))
  expect_false(.gley(c(list(designation = c("Ah", "Ckgj", "C")), grey)))
  # another notation is read the same way
  expect_false(.gley(c(list(designation = c("A", "Ck", "C")), grey)))
  expect_true(.gley(c(list(designation = c("A", "Cg", "C")), grey)))
  # without designations: gleyed, unless the horizon holds carbonates (the
  # clay rules out a Bt)
  und <- c(list(designation = NA_character_, clay_pct = 20), grey)
  expect_true(.gley(und))
  expect_true(is.na(.gley(c(und, list(caco3_pct = c(0, 15, 0))))))
  expect_true(.gley(c(und, list(caco3_pct = c(0, 1, 0)))))
  # the other clauses keep reading the colours alone
  mott <- grey; mott$munsell_hue_moist[2] <- "5Y"; mott$munsell_chroma_moist[2] <- 2
  expect_true(.gley(c(list(designation = c("Ah", "Ck", "C"), mottle_contrast = c(NA, "prominent", NA)), mott)))
  blue <- grey; blue$munsell_hue_moist[2] <- "5GY"
  expect_true(.gley(c(list(designation = c("Ah", "Ck", "C")), blue)))
  # a Bt of chroma 1 designated without g is not a Btg by its colours when
  # mottles are recorded absent; with mottles unknown its colours leave it
  # open and its designation (a Bt, not a Btg) decides
  btc <- list(designation = c("Ae", "Bt", "Cg"),
    top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 90), clay_pct = c(12, 30, 25),
    clay_films_amount = c(NA, "common", NA), munsell_hue_moist = c("10YR", "2.5Y", "5Y"),
    munsell_value_moist = c(6, 5, 5), munsell_chroma_moist = c(2, 1, 1))
  lg <- soilKey:::luvic_gleysol_cssc(.rd(btc))
  expect_true(is.na(lg$evidence$by_properties))
  expect_false(lg$passed)
  btc$redoximorphic_features_pct <- 0
  expect_false(soilKey:::luvic_gleysol_cssc(.rd(btc))$evidence$by_properties)
})

test_that("with a Bt within 50 cm the gley colours must be in the Bt", {
  skip_on_cran()
  luv <- list(designation = c("LFH", "Ae", "Bt", "Ckg"), top_cm = c(0, 5, 20, 40),
              bottom_cm = c(5, 20, 40, 90), oc_pct = c(30, 0.6, 0.4, 0.2),
              clay_pct = c(NA, 12, 30, 25), clay_films_amount = c(NA, NA, "common", NA),
              munsell_hue_moist = c(NA, "10YR", "10YR", "5Y"),
              munsell_value_moist = c(NA, 6, 4, 5), munsell_chroma_moist = c(NA, 2, 4, 1))
  # gleyed only in the C below the Bt: a Luvisol (a Gleyed one)
  g <- soilKey:::gleying_cssc(.rd(luv))
  expect_false(g$passed)
  expect_true(g$evidence$bt_within_50)
  expect_identical(classify_cssc3(.rd(luv), on_missing = "silent")$rsg_or_order, "Luvisolic")
  # gleyed in the Bt as well: a Luvic Gleysol
  btg <- luv; btg$designation[3] <- "Btg"; btg$munsell_hue_moist[3] <- "2.5Y"
  btg$munsell_value_moist[3] <- 5; btg$munsell_chroma_moist[3] <- 1
  r <- classify_cssc3(.rd(btg), on_missing = "silent")
  expect_identical(r$rsg_or_order, "Gleysolic")
  expect_identical(r$levels$name[2], "Luvic Gleysol")
  # a run of gleyed horizons that reaches into the Bt is enough
  btlow <- luv; btlow$designation <- c("LFH", "Ae", "Bt", "Btg", "Ckg")
  btlow$top_cm <- c(0, 5, 20, 30, 40); btlow$bottom_cm <- c(5, 20, 30, 40, 90)
  btlow$oc_pct <- c(30, 0.6, 0.4, 0.3, 0.2); btlow$clay_pct <- c(NA, 12, 30, 30, 25)
  btlow$clay_films_amount <- c(NA, NA, "common", "common", NA)
  btlow$munsell_hue_moist <- c(NA, "10YR", "10YR", "2.5Y", "5Y")
  btlow$munsell_value_moist <- c(NA, 6, 4, 5, 5); btlow$munsell_chroma_moist <- c(NA, 2, 4, 1, 1)
  expect_true(.gley(btlow))
  # a Bt that begins below 50 cm does not ask for it
  deep <- list(designation = c("Ah", "Bg", "Bt"), top_cm = c(0, 10, 55), bottom_cm = c(10, 55, 90),
               clay_pct = c(15, 16, 30), clay_films_amount = c(NA, NA, "common"),
               munsell_hue_moist = c("10YR", "2.5Y", "10YR"), munsell_value_moist = c(3, 5, 4),
               munsell_chroma_moist = c(2, 1, 4))
  expect_true(.gley(deep))
  # a solonetzic B gleyed or not: Gleysolic by its gleying (Ch 7, p. 83)
  sz <- list(designation = c("Ah", "Bnt", "Ckg"), top_cm = c(0, 12, 35), bottom_cm = c(12, 35, 80),
             clay_pct = c(18, 32, 28), ca_cmol = c(12, 8, 10), na_cmol = c(0.5, 4, 6),
             munsell_hue_moist = c("10YR", "10YR", "5Y"), munsell_value_moist = c(3, 3, 5),
             munsell_chroma_moist = c(2, 3, 1))
  g <- soilKey:::gleying_cssc(.rd(sz))
  expect_true(g$passed)
  expect_true(g$evidence$solonetzic_b)
})

test_that("an undecided Bt decides only where it cannot change the answer", {
  skip_on_cran()
  # no designations and no clay: every horizon that may be a Bt is gleyed
  all_g <- list(designation = NA_character_, top_cm = c(0, 10, 30), bottom_cm = c(10, 30, 60),
                oc_pct = c(5, 0.5, 0.3), munsell_hue_moist = c("10YR", "5Y", "5Y"),
                munsell_value_moist = c(2, 5, 5), munsell_chroma_moist = c(1, 1, 1))
  g <- soilKey:::gleying_cssc(.rd(all_g))
  expect_true(is.na(g$evidence$bt_within_50))
  expect_true(g$passed)
  # a horizon that may be a Bt and is not gleyed leaves it undecided
  brown <- list(designation = NA_character_, top_cm = c(0, 10, 25, 45),
                bottom_cm = c(10, 25, 45, 80), oc_pct = c(5, 0.6, 0.5, 0.3),
                munsell_hue_moist = c("10YR", "10YR", "5Y", "5Y"),
                munsell_value_moist = c(2, 4, 5, 5), munsell_chroma_moist = c(1, 4, 1, 1))
  expect_true(is.na(.gley(brown)))
  # the clay rules the Bt out, and the gleying passes
  brown$clay_pct <- 20
  expect_true(.gley(brown))
})

test_that("the base saturation of the chernozemic A is read as by a neutral salt", {
  skip_on_cran()
  chern <- list(mean_annual_soil_temp_C = 4, soil_moisture_subclass = "subhumid")
  a <- function(...) {
    h <- utils::modifyList(list(designation = c("Ah", "Bm", "Ck"), top_cm = c(0, 15, 40),
      bottom_cm = c(15, 40, 90), munsell_value_dry = c(4, 5, 6), munsell_value_moist = c(2, 3, 5),
      munsell_chroma_moist = c(2, 3, 3), oc_pct = c(3, 0.8, 0.3), n_total_pct = c(0.25, 0.08, 0.03),
      ca_cmol = c(15, 14, 20), mg_cmol = c(3, 3, 2), k_cmol = c(0.8, 0.5, 0.4),
      na_cmol = c(0.1, 0.1, 0.2)), list(...))
    soilKey:::chernozemic_a_cssc(.rd(h, chern))$evidence$base_saturation
  }
  # base saturation at pH 7 above 80 %: the unbuffered value is higher
  expect_true(a(bs_pct = 90))
  # at or below 80 % at pH 7: decided by the pH, otherwise undecided
  expect_true(a(bs_pct = 70, ph_cacl2 = 6.0))
  expect_true(a(bs_pct = 70, ph_h2o = 6.4))
  expect_true(is.na(a(bs_pct = 70, ph_cacl2 = 5.0)))
  expect_true(is.na(a(bs_pct = 70)))
  # measured on the effective CEC: it decides, either way
  expect_true(a(bs_pct = 70, ecec_cmol = c(20, 19, 23)))
  expect_false(a(bs_pct = 95, ecec_cmol = c(25, 19, 23), ph_cacl2 = 6.5))
  # the bases over the bases and the exchangeable Al are an upper bound (the
  # H is left out): they can fail the A, not pass it
  expect_false(a(al_cmol = c(6, 1, 0)))
  expect_true(is.na(a(al_cmol = c(0.2, 0, 0))))
  expect_true(a(al_cmol = c(0.2, 0, 0), ph_cacl2 = 6))
  # Ca must still be the dominant cation
  expect_false(a(bs_pct = 95, mg_cmol = c(16, 3, 2)))
  # the whole A is weighed: a measured 75 % over most of it is not offset
  two <- list(designation = c("Ah1", "Ah2", "Ck"), top_cm = c(0, 3, 40), bottom_cm = c(3, 40, 90),
              munsell_value_dry = 4, munsell_value_moist = 2, munsell_chroma_moist = 2,
              oc_pct = 3, n_total_pct = 0.25, ca_cmol = 15, mg_cmol = 3, k_cmol = 0.8, na_cmol = 0.1,
              ecec_cmol = c(NA, 25.2, NA), ph_cacl2 = c(6.5, NA, NA))
  expect_false(soilKey:::chernozemic_a_cssc(.rd(two, chern))$evidence$base_saturation)
})

test_that("review fixes: open mottles, base saturation bounds, the Cryosol A, an open Bt", {
  skip_on_cran()
  # a C of chroma 1 in another notation with unknown mottles may still be
  # gleyed by clause b or c: undecided, not FALSE
  us <- list(designation = c("Ap", "C"), top_cm = c(0, 20), bottom_cm = c(20, 80), clay_pct = 20,
             munsell_hue_moist = c("10YR", "2.5Y"), munsell_value_moist = c(3, 5),
             munsell_chroma_moist = c(2, 1))
  expect_true(is.na(soilKey:::gleying_cssc(.rd(us, list(country = "US")))$passed))
  us$mottle_contrast <- c(NA, "prominent")
  expect_true(soilKey:::gleying_cssc(.rd(us, list(country = "US")))$passed)
  us$mottle_contrast <- NULL; us$redoximorphic_features_pct <- c(0, 0)
  expect_false(soilKey:::gleying_cssc(.rd(us, list(country = "US")))$passed)
  # base saturation bounds
  bsn <- function(...) {
    h <- data.frame(ca_cmol = NA_real_, mg_cmol = NA_real_, k_cmol = NA_real_, na_cmol = NA_real_,
                    ecec_cmol = NA_real_, al_cmol = NA_real_, cec_cmol = NA_real_, bs_pct = NA_real_,
                    ph_cacl2 = NA_real_, ph_h2o = NA_real_)
    v <- list(...); for (k in names(v)) h[[k]] <- v[[k]]
    unlist(soilKey:::.cssc_bs_neutral_salt(h, 1L))
  }
  expect_equal(bsn(bs_pct = 150), c(lo = 100, hi = 100))
  # pH in water stands in only without a pH in CaCl2
  expect_equal(bsn(bs_pct = 70, ph_cacl2 = 5.0, ph_h2o = 6.1), c(lo = 70, hi = 100))
  expect_gt(bsn(bs_pct = 70, ph_h2o = 6.1)[["lo"]], 80)
  # a base missing: a lower bound on the effective CEC
  expect_equal(bsn(ca_cmol = 15, mg_cmol = 3, k_cmol = 0.8, ecec_cmol = 19)[["lo"]], 100 * 18.8 / 19)
  # the bases over a pH 7 CEC stand for bs_pct
  expect_equal(bsn(ca_cmol = 15, mg_cmol = 3, k_cmol = 0.8, na_cmol = 0.2, cec_cmol = 20)[["lo"]], 95)
  # the exchangeable Al leaves the H out: an upper bound only
  expect_equal(bsn(ca_cmol = 7, mg_cmol = 1, k_cmol = 0.5, na_cmol = 0.5, al_cmol = 1), c(lo = 0, hi = 90))
  expect_equal(bsn(ca_cmol = 7, mg_cmol = 1, k_cmol = 0.5, na_cmol = 0.5, al_cmol = 3), c(lo = 0, hi = 75))
  # a pH below the limit alone bounds nothing
  expect_true(all(is.na(bsn(ph_cacl2 = 5.0))))
  # the Gleysolic Cryosol A: an undesignated horizon of undecided gley colour
  cry <- list(designation = NA_character_, top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80),
              oc_pct = c(3, 0.5, 0.4), caco3_pct = c(5, 0, 0),
              munsell_hue_moist = c("2.5Y", "10YR", "10YR"), munsell_value_moist = c(4, 5, 5),
              munsell_chroma_moist = c(1, 4, 4))
  expect_true(is.na(soilKey:::gleysolic_cryosol_cssc(.rd(cry))$passed))
  cry$caco3_pct <- 0
  expect_true(soilKey:::gleysolic_cryosol_cssc(.rd(cry))$passed)
  # an undecided Bt: every way it may go agrees, so the gleying passes
  open_bt <- list(designation = NA_character_, top_cm = c(0, 15, 30, 33),
                  bottom_cm = c(15, 30, 33, 48), oc_pct = c(4, 0.6, 0.5, 0.4),
                  clay_pct = c(15, NA, 30, NA), clay_films_amount = c(NA, NA, "common", NA),
                  munsell_hue_moist = c("10YR", NA, "5Y", NA), munsell_value_moist = c(2, NA, 5, NA),
                  munsell_chroma_moist = c(1, NA, 1, NA))
  g <- soilKey:::gleying_cssc(.rd(open_bt))
  expect_true(is.na(g$evidence$bt_within_50))
  expect_true(g$passed)
  # a surface A over 50 cm thick: the evidence reads the horizon below it
  thick <- soilKey:::gleying_cssc(.rd(list(designation = c("Ah", "Bg"), top_cm = c(0, 60),
    bottom_cm = c(60, 80), munsell_hue_moist = c("10YR", "5Y"), munsell_value_moist = c(2, 5),
    munsell_chroma_moist = c(1, 1))))
  expect_true(thick$passed)
  expect_true(thick$evidence$by_properties)
})

test_that("second review fixes: a Bt of unknown thickness, the Cryosol A, bad data", {
  skip_on_cran()
  # a gleyed Btg of unknown bottom: whether it is a Bt or too thin to be one,
  # the gleying passes
  q1 <- list(designation = c("Ah", "Ae", "Btg"), top_cm = c(0, 10, 20), bottom_cm = c(10, 20, NA),
             clay_pct = c(12, 10, 30), clay_films_amount = c(NA, NA, "common"),
             munsell_hue_moist = c("10YR", "10YR", "5Y"), munsell_value_moist = c(3, 6, 5),
             munsell_chroma_moist = c(2, 2, 1))
  expect_true(.gley(q1))
  # a gleyed Bg over an ungleyed Bt of unknown bottom stays undecided
  q2 <- q1; q2$designation <- c("Ah", "Bg", "Bt"); q2$munsell_hue_moist <- c("10YR", "5Y", "10YR")
  q2$munsell_value_moist <- c(3, 5, 5); q2$munsell_chroma_moist <- c(2, 1, 4)
  expect_true(is.na(.gley(q2)))
  # the Cryosol A: a calcareous undesignated horizon of undecided colour is
  # either part of the A (then the gleyed horizon below is tested) or the
  # gleyed horizon itself; both pass
  cry <- list(designation = NA_character_, top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80),
              oc_pct = c(3, 0.5, 0.4), caco3_pct = c(5, 0, 0),
              munsell_hue_moist = c("2.5Y", "5Y", "10YR"), munsell_value_moist = c(4, 5, 5),
              munsell_chroma_moist = c(1, 1, 4))
  expect_true(soilKey:::gleysolic_cryosol_cssc(.rd(cry))$passed)
  # the evidence follows the Bt rule
  ev <- soilKey:::gleying_cssc(.rd(list(designation = c("Ah", "Ae", "Bt", "Ckg"),
    top_cm = c(0, 10, 20, 45), bottom_cm = c(10, 20, 45, 90), clay_pct = c(15, 12, 30, 25),
    clay_films_amount = c(NA, NA, "common", NA), munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"),
    munsell_value_moist = c(3, 6, 4, 5), munsell_chroma_moist = c(2, 2, 4, 1))))
  expect_false(ev$passed)
  expect_false(ev$evidence$by_properties)
  # bad data: negative values and contradictory bounds are read as no data
  bsn <- function(...) {
    h <- data.frame(ca_cmol = NA_real_, mg_cmol = NA_real_, k_cmol = NA_real_, na_cmol = NA_real_,
                    ecec_cmol = NA_real_, al_cmol = NA_real_, cec_cmol = NA_real_, bs_pct = NA_real_,
                    ph_cacl2 = NA_real_, ph_h2o = NA_real_)
    v <- list(...); for (k in names(v)) h[[k]] <- v[[k]]
    unlist(soilKey:::.cssc_bs_neutral_salt(h, 1L))
  }
  expect_equal(bsn(ca_cmol = -9, mg_cmol = 3, k_cmol = 0.8, na_cmol = 0.1, ecec_cmol = 20)[["lo"]],
               100 * 3.9 / 20)
  expect_true(all(is.na(bsn(bs_pct = 95, ca_cmol = 7, mg_cmol = 1, k_cmol = 0.5, na_cmol = 0.5,
                            al_cmol = 3))))
  # the KCl Al stands in for the exchangeable Al; the higher pH 7 bound wins
  expect_equal(bsn(ca_cmol = 7, mg_cmol = 1, k_cmol = 0.5, na_cmol = 0.5, al_kcl_cmol = 3)[["hi"]], 75)
  expect_equal(bsn(bs_pct = 60, ca_cmol = 15, mg_cmol = 3, k_cmol = 0.8, na_cmol = 0.2,
                   cec_cmol = 20)[["lo"]], 95)
})

test_that("third review fixes: bad values in the Ca dominance and the pH", {
  skip_on_cran()
  chern <- list(mean_annual_soil_temp_C = 4, soil_moisture_subclass = "subhumid")
  base <- list(designation = c("Ah", "Bm", "Ck"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 90),
               munsell_value_dry = 4, munsell_value_moist = c(2, 4, 5), munsell_chroma_moist = c(2, 3, 3),
               oc_pct = c(3, 0.5, 0.3), n_total_pct = c(0.25, 0.05, 0.03))
  bs <- function(...) soilKey:::chernozemic_a_cssc(.rd(utils::modifyList(base, list(...)), chern))$evidence$base_saturation
  # a negative Ca is no data for the dominance as well
  expect_true(bs(ca_cmol = c(-9, NA, NA), mg_cmol = c(3, NA, NA), bs_pct = c(95, NA, NA)))
  # the KCl Al counts in the dominance as the exchangeable Al does
  expect_identical(bs(ca_cmol = c(2, NA, NA), bs_pct = c(90, NA, NA), al_cmol = c(5, NA, NA)),
                   bs(ca_cmol = c(2, NA, NA), bs_pct = c(90, NA, NA), al_kcl_cmol = c(5, NA, NA)))
  expect_false(bs(ca_cmol = c(2, NA, NA), bs_pct = c(90, NA, NA), al_kcl_cmol = c(5, NA, NA)))
  # a pH outside 0-14 is no data: the pH in water stands in
  ex <- list(ca_cmol = c(6, NA, NA), mg_cmol = c(1, NA, NA), k_cmol = c(0.3, NA, NA),
             na_cmol = c(0.2, NA, NA), al_cmol = c(2.5, NA, NA), ph_h2o = c(7, NA, NA))
  expect_true(is.na(do.call(bs, ex)))
  expect_true(is.na(do.call(bs, c(ex, list(ph_cacl2 = c(-9, NA, NA))))))
})
