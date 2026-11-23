# v0.9.415 (Hugo, 2026-11-20): the USDA key records its undecided tests at
# every level and reads the soil regimes only from the record (audit decisions
# P1 (c), P2 (a), P3 (a), reading L34, and R1).
#   * a regime test is undecided (NA) when no regime is recorded, listing the
#     site field; "ud" covers udic and perudic (KST 13th ed., p. 48);
#   * every NA taxon tested before the one assigned, at any level, is in
#     $ambiguities with its level, and the grade falls from A or B to C;
#   * Udults need udic or perudic, Xerults xeric (p. 321); Ustox and
#     Ustifolists take ustic or xeric (pp. 295, 199); Perox perudic;
#   * the family temperature class is not inferred unless asked for.

.v09415_with_site <- function(pedon, ...) {
  p <- pedon$clone(deep = TRUE)
  extra <- list(...)
  for (k in names(extra)) p$site[[k]] <- extra[[k]]
  p
}

test_that("regime tests are undecided when no regime is recorded", {
  testthat::skip_on_cran()
  p <- make_cambisol_canonical()
  p$site$soil_moisture_regime <- NULL
  p$site$soil_temperature_regime <- NULL
  for (fn in c("smr_aridic_usda", "smr_torric_usda", "smr_ustic_usda",
               "smr_xeric_usda", "smr_udic_usda", "smr_perudic_usda")) {
    r <- get(fn, asNamespace("soilKey"))(p)
    expect_true(is.na(r$passed), info = fn)
    expect_identical(r$missing, "site$soil_moisture_regime", info = fn)
  }
  r <- soilKey:::str_cryic_usda(p)
  expect_true(is.na(r$passed))
  expect_identical(r$missing, "site$soil_temperature_regime")
  # gelic: no permafrost data and no regime -> undecided
  expect_true(is.na(soilKey:::str_gelic_usda(p)$passed))
  # an empty string is no record either
  p$site$soil_moisture_regime <- ""
  expect_true(is.na(soilKey:::smr_ustic_usda(p)$passed))
})

test_that("a recorded regime decides the regime tests", {
  testthat::skip_on_cran()
  p <- .v09415_with_site(make_cambisol_canonical(),
                         soil_moisture_regime = "perudic",
                         soil_temperature_regime = "isothermic")
  expect_true(soilKey:::smr_udic_usda(p)$passed)      # "ud" covers perudic
  expect_true(soilKey:::smr_perudic_usda(p)$passed)
  expect_false(soilKey:::smr_ustic_usda(p)$passed)
  expect_false(soilKey:::str_cryic_usda(p)$passed)
  expect_false(soilKey:::str_gelic_usda(p)$passed)
  expect_length(soilKey:::smr_ustic_usda(p)$missing, 0L)
  u <- .v09415_with_site(p, soil_moisture_regime = "udic")
  expect_true(soilKey:::smr_udic_usda(u)$passed)
  expect_false(soilKey:::smr_perudic_usda(u)$passed)
  t <- .v09415_with_site(p, soil_moisture_regime = "Torric")
  expect_true(soilKey:::smr_aridic_usda(t)$passed)
  g <- .v09415_with_site(p, soil_temperature_regime = "pergelic")
  expect_true(soilKey:::str_gelic_usda(g)$passed)
})

test_that("undecided tests are recorded at every level and lower the grade", {
  testthat::skip_on_cran()
  # the canonical Cambisol has no regime: the Udepts are reached past the
  # undecided Gelepts, Cryepts, Xerepts and Ustepts
  res <- classify_usda(make_cambisol_canonical(), on_missing = "silent")
  # v0.9.425: no free carbonates and 0.2 % organic carbon at 125 cm (KST p. 228)
  expect_identical(res$name, "Dystric Fluventic Eutrudepts")
  expect_identical(res$evidence_grade, "C")
  lv <- vapply(res$ambiguities, function(a) a$level, "")
  cd <- vapply(res$ambiguities, function(a) a$rsg_code, "")
  expect_true(all(c("KB", "KC", "KD", "KE") %in% cd[lv == "suborders"]))
  expect_true(all(lv %in% c("orders", "suborders", "great_groups", "subgroups")))
  a1 <- res$ambiguities[[which(cd == "KE")[1L]]]
  expect_named(a1, c("rsg_code", "rsg_name", "reason", "level"))
  expect_match(a1$reason, "site\\$soil_moisture_regime")
  expect_true(all(c("site$soil_moisture_regime", "site$soil_temperature_regime")
                  %in% res$missing_data))
  expect_true(any(grepl("undecided test", res$warnings)))
  # the trace table shows them as undecided, not failed
  tt <- key_trace_table(res)
  expect_identical(tt$status[tt$phase == "suborders" & tt$code == "KE"], "undecided")
  expect_false(any(tt$status == "indeterminate"))
})

test_that("a recorded regime decides the suborder and keeps the grade", {
  testthat::skip_on_cran()
  p <- .v09415_with_site(make_cambisol_canonical(),
                         soil_moisture_regime = "ustic",
                         soil_temperature_regime = "mesic")
  res <- classify_usda(p, on_missing = "silent")
  expect_identical(res$trace$suborder_assigned$name, "Ustepts")
  lv <- vapply(res$ambiguities, function(a) a$level, "")
  expect_false("suborders" %in% lv)
})

test_that("Ultisol suborders follow the recorded regime (L34, p. 321)", {
  testthat::skip_on_cran()
  p <- make_acrisol_canonical()
  none <- classify_usda(p, on_missing = "silent")
  expect_identical(none$name, "Ultisols")
  expect_null(none$trace$suborder_assigned)
  cd <- vapply(none$ambiguities, function(a) a$rsg_code, "")
  lv <- vapply(none$ambiguities, function(a) a$level, "")
  expect_true(all(c("HC", "HD", "HE") %in% cd[lv == "suborders"]))
  expect_true(any(grepl("^Suborder undecided", none$warnings)))
  sub <- function(smr) {
    classify_usda(.v09415_with_site(p, soil_moisture_regime = smr),
                  on_missing = "silent")$trace$suborder_assigned$name
  }
  expect_identical(sub("udic"), "Udults")
  expect_identical(sub("perudic"), "Udults")
  expect_identical(sub("ustic"), "Ustults")
  expect_identical(sub("xeric"), "Xerults")
  # the key order of p. 321: Aquults, Humults, Udults, Ustults, Xerults
  codes <- vapply(load_rules("usda")$suborders$UT, function(x) x$code, "")
  expect_identical(codes, c("HA", "HB", "HC", "HD", "HE"))
})

test_that("Ustox and Ustifolists take ustic or xeric; Perox perudic", {
  testthat::skip_on_cran()
  rules <- load_rules("usda")
  ox <- make_ferralsol_canonical()
  sub <- function(smr) {
    soilKey::run_usda_suborder(.v09415_with_site(ox, soil_moisture_regime = smr),
                               "OX", rules)$assigned$name
  }
  expect_identical(sub("xeric"), "Ustox")
  expect_identical(sub("ustic"), "Ustox")
  expect_identical(sub("perudic"), "Perox")
  expect_identical(sub("udic"), "Udox")
  fol <- rules$great_groups$BA
  ust <- fol[[which(vapply(fol, function(x) x$name, "") == "Ustifolists")]]
  h <- make_histosol_canonical()
  expect_true(isTRUE(soilKey::evaluate_rsg_tests(
    .v09415_with_site(h, soil_moisture_regime = "xeric"), ust$tests)$passed))
  expect_true(is.na(soilKey::evaluate_rsg_tests(
    .v09415_with_site(h, soil_moisture_regime = NA), ust$tests)$passed))
})

test_that("the family temperature class is not inferred unless asked for", {
  testthat::skip_on_cran()
  p <- .v09415_with_site(make_ferralsol_canonical(), lat = -10, elevation_m = 200)
  p$site$soil_temperature_regime <- NULL
  off <- soilKey:::family_temperature_regime_usda(p)
  expect_null(off$value)
  expect_identical(off$missing, "site$soil_temperature_regime")
  on <- soilKey:::family_temperature_regime_usda(p, infer = TRUE)
  expect_identical(on$value, "isohyperthermic")
  expect_true(isTRUE(on$evidence$suggestion))
  res <- classify_usda(p, include_family = TRUE, on_missing = "silent")
  expect_false(grepl("iso", res$name))
  res2 <- classify_usda(p, include_family = TRUE, infer_temperature = TRUE,
                        on_missing = "silent")
  expect_match(res2$name, "isohyperthermic")
  expect_identical(res2$evidence_grade, "C")
  expect_true(any(grepl("suggestion inferred from latitude", res2$warnings)))
  # a recorded regime is used as recorded, with no iso added
  rec <- .v09415_with_site(p, soil_temperature_regime = "hyperthermic")
  expect_identical(soilKey:::family_temperature_regime_usda(rec, infer = TRUE)$value,
                   "hyperthermic")
})

test_that("the WRB trace table still reads NA as undecided", {
  testthat::skip_on_cran()
  tt <- key_trace_table(classify_wrb2022(make_cambisol_canonical(),
                                         on_missing = "silent"))
  expect_true(all(tt$status %in% c("passed", "failed", "undecided",
                                   "selected", "info")))
})
