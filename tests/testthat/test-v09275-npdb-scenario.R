# v0.9.275: a scenario for the NPDB with the dry colour of the A horizons
# and the soil climate estimated, each as a range (no network, no NPDB data).

.np <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "p", country = "CA", designation_system = "cssc",
                                 reference_cssc_order = "Chernozemic"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

# monthly normals for one point: mean, minimum and maximum temperatures and
# precipitation
.normals <- function(id, lat, ta, range = 12, prec) {
  d <- data.frame(id = id, lat = lat, lon = -100)
  for (m in 1:12) d[[sprintf("ta_m%02d", m)]] <- ta[m]
  for (m in 4:10) {
    d[[sprintf("tn_m%02d", m)]] <- ta[m] - range / 2
    d[[sprintf("tx_m%02d", m)]] <- ta[m] + range / 2
    d[[sprintf("ac_m%02d", m)]] <- prec[m - 3]
  }
  d
}
.prairie_ta <- c(-17, -14, -7, 4, 11, 16, 19, 17, 11, 4, -6, -14)

test_that("the radiation and day length follow FAO-56", {
  skip_on_cran()
  # FAO-56 example 8: 20 degrees S on 3 September, Ra = 32.2 MJ m-2 day-1
  expect_equal(soilKey:::.extraterrestrial_radiation(-20, 246), 32.2, tolerance = 0.01)
  expect_equal(soilKey:::.day_length(0, 80), 12, tolerance = 1e-6)
  expect_gt(soilKey:::.day_length(55, 172), 16)
})

test_that("the soil climate reads the moisture index and a soil temperature range", {
  skip_on_cran()
  dry <- .normals("dry", 50, .prairie_ta, prec = c(20, 45, 65, 55, 40, 30, 15))
  wet <- .normals("wet", 50, .prairie_ta, prec = c(60, 90, 110, 100, 90, 80, 60))
  cl <- npdb_soil_climate(rbind(dry, wet))
  expect_equal(cl$maat[1], mean(.prairie_ta))
  expect_equal(cl$mast_hi - cl$mast_lo, c(4, 4))
  # the growing season: April (4 C) and October (4 C) are left out
  expect_identical(cl$gs_months, c(5, 5))
  expect_equal(cl$p_gs[1], 45 + 65 + 55 + 40 + 30)
  expect_true(cl$cmi[1] < cl$cmi[2])
  expect_true(all(cl$moisture_subclass %in% c("arid", "subarid", "semiarid", "subhumid", "humid", "perhumid")))
  expect_true(cl$cmi_hargreaves[1] < cl$cmi[1])           # Hargreaves reads drier here
  expect_identical(cl$moisture_subclass[2], "perhumid")
  # Table 4 limits
  sub <- function(cmi) as.character(cut(cmi, c(-Inf, 24.5, 45.5, 58.5, 73.5, 84.5, Inf),
    labels = c("arid", "subarid", "semiarid", "subhumid", "humid", "perhumid")))
  expect_identical(sub(c(24, 25, 45, 46, 58, 59, 73, 74, 84, 85)),
                   c("arid", "subarid", "subarid", "semiarid", "semiarid", "subhumid", "subhumid",
                     "humid", "humid", "perhumid"))
  # no month above 5 degrees: no index
  cold <- npdb_soil_climate(.normals("cold", 70, rep(-10, 12), prec = rep(20, 7)))
  expect_true(is.na(cold$cmi))
  expect_error(npdb_soil_climate(dry[, 1:5]), "missing columns")
})

test_that("the dry colour model is fitted on the A horizons that record both colours", {
  skip_on_cran()
  set.seed(42)
  peds <- lapply(1:30, function(i) {
    vm <- sample(c(2, 2.5, 3, 3.5, 4), 1); cm <- sample(1:3, 1)
    .np(list(designation = c("Ah", "Bm"), top_cm = c(0, 15), bottom_cm = c(15, 40),
             munsell_value_moist = c(vm, 4), munsell_value_dry = c(vm + 1.5 + (i %% 3 - 1) * 0.5, 6),
             munsell_chroma_moist = c(cm, 3), munsell_chroma_dry = c(cm, 3)),
        list(id = paste0("p", i)))
  })
  m <- npdb_dry_colour_model(peds)
  expect_s3_class(m, "npdb_dry_colour_model")
  expect_equal(unname(m$value$coef[2]), 1, tolerance = 0.1)
  expect_identical(m$value$n, 30L)                      # the Bm is not read
  expect_true(m$value$band[1] < 0 && m$value$band[2] > 0)
  expect_output(print(m), "dry value")
  expect_error(npdb_dry_colour_model(peds[1:5]), "fewer than 20")
})

test_that("the scenario fills what is missing and keeps the ranges", {
  skip_on_cran()
  m <- structure(list(value = list(coef = c(1.7, 0.9), band = c(-1, 1), rmse = 0.8, n = 100L),
                      chroma = list(coef = c(0.7, 0.6), band = c(-0.9, 0.7), rmse = 0.7, n = 100L),
                      level = 0.8), class = "npdb_dry_colour_model")
  p <- .np(list(designation = c("Ah", "Ahe", "Bm", "Ck"), top_cm = c(0, 10, 20, 40),
                bottom_cm = c(10, 20, 40, 90), munsell_value_moist = c(2, 3, 4, 6),
                munsell_value_dry = c(NA, 5, NA, NA), munsell_chroma_moist = c(1, 2, 3, 3)))
  cl <- data.frame(id = "p", maat = -1, mast_lo = -1, mast_hi = 3, cmi = 76,
                   moisture_subclass = "humid", moisture_subclass_hargreaves = "subhumid")
  sc <- npdb_scenario(list(p = p), climate = cl, dry_colour = m)[[1]]
  h <- sc$horizons
  expect_equal(h$munsell_value_dry, c(1.7 + 0.9 * 2, 5, NA, NA))     # A horizons only; 5 recorded
  expect_equal(h$munsell_chroma_dry[1], 0.7 + 0.6 * 1)
  expect_equal(sc$site$mean_annual_soil_temp_C, 1)
  expect_identical(sc$site$soil_moisture_subclass, "humid")
  expect_identical(sc$site$npdb_scenario$dry_value_rows, 1L)
  expect_equal(sc$site$npdb_scenario$mast_range, c(-1, 3))
  expect_identical(sc$site$npdb_scenario$moisture_alt, "subhumid")
  expect_null(p$site$npdb_scenario)                                  # the input is not changed
  # central version + 2 x 2 x 2 x 2 ends
  v <- soilKey:::.npdb_scenario_versions(sc)
  expect_length(v, 17L)
  expect_true(all(vapply(v[-1], function(q) q$horizons$munsell_value_dry[1], 0) %in% c(2.5, 4.5)))
  # a soil temperature range on one side of 0 and 8 is not varied
  cl$mast_lo <- 1; cl$mast_hi <- 5; cl$moisture_subclass_hargreaves <- "humid"
  sc2 <- npdb_scenario(list(p = p), climate = cl, dry_colour = m)[[1]]
  expect_null(sc2$site$npdb_scenario$mast_range)
  expect_null(sc2$site$npdb_scenario$moisture_alt)
  # recorded climate is never replaced
  p$site$mean_annual_soil_temp_C <- 6; p$site$soil_moisture_subclass <- "semiarid"
  sc3 <- npdb_scenario(list(p = p), climate = cl)[[1]]
  expect_equal(sc3$site$mean_annual_soil_temp_C, 6)
  expect_identical(sc3$site$soil_moisture_subclass, "semiarid")
  expect_error(npdb_scenario(list(p), dry_colour = list()), "npdb_dry_colour_model")
})

test_that("benchmark_npdb decides a scenario pedon only where every end agrees", {
  skip_on_cran()
  chern <- function(id, vm, mast = c(1, 5), sub = "subhumid") {
    p <- .np(list(designation = c("Ah", "Bm", "Ck"), top_cm = c(0, 18, 40), bottom_cm = c(18, 40, 90),
                  munsell_value_moist = c(vm, 4, 5), munsell_chroma_moist = c(2, 3, 3),
                  oc_pct = c(3, 0.8, 0.3), n_total_pct = c(0.25, 0.08, 0.03), bs_pct = c(95, 98, 100),
                  ca_cmol = c(18, 16, 20), mg_cmol = c(4, 4, 3), clay_pct = c(25, 26, 24)),
             list(id = id))
    list(p = p, cl = data.frame(id = id, maat = mast[1], mast_lo = mast[1], mast_hi = mast[2], cmi = 65,
                                moisture_subclass = sub, moisture_subclass_hargreaves = sub))
  }
  m <- structure(list(value = list(coef = c(1.7, 0.9), band = c(-1, 1), rmse = 0.8, n = 100L),
                      chroma = list(coef = c(0.7, 0.6), band = c(-0.9, 0.7), rmse = 0.7, n = 100L),
                      level = 0.8), class = "npdb_dry_colour_model")
  a <- chern("a", 2)                    # dry 3.5, 2.5 to 4.5: below 5.5 at both ends
  b <- chern("b", 3.4)                  # dry 4.8, 3.8 to 5.8: crosses 5.5
  c <- chern("c", 2, mast = c(-2, 2))   # soil temperature crosses 0
  peds <- list(a = a$p, b = b$p, c = c$p)
  sc <- npdb_scenario(peds, climate = rbind(a$cl, b$cl, c$cl), dry_colour = m)
  r <- benchmark_npdb(sc, level = "order", set_aside = character(0), verbose = FALSE)$predictions
  expect_identical(r$predicted[r$id == "a"], "Chernozemic")
  expect_false(r$provisional[r$id == "a"])
  expect_true(r$provisional[r$id == "b"] && r$scenario_split[r$id == "b"])
  expect_true(r$provisional[r$id == "c"] && r$scenario_split[r$id == "c"])
  expect_true(all(r$scenario))
  # a pedon outside the scenario is classified once, as before
  r0 <- benchmark_npdb(list(a = a$p), level = "order", set_aside = character(0), verbose = FALSE)$predictions
  expect_false(r0$scenario)
})

test_that("fetch_aafc_normals checks its arguments before any request", {
  skip_on_cran()
  skip_if_not_installed("httr"); skip_if_not_installed("jsonlite")
  expect_error(fetch_aafc_normals(lat = c(50, 51), lon = -100), "differ in length")
  expect_error(fetch_aafc_normals(50, -100, period = "1951-1980"))
})

test_that("review fixes: subclasses in between, two calls, missing ids", {
  skip_on_cran()
  m <- structure(list(value = list(coef = c(1.7, 0.9), band = c(-1, 1), rmse = 0.8, n = 100L),
                      chroma = list(coef = c(0.7, 0.6), band = c(-0.9, 0.7), rmse = 0.7, n = 100L),
                      level = 0.8), class = "npdb_dry_colour_model")
  p <- .np(list(designation = c("Ah", "Bm", "Ck"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 90),
                munsell_value_moist = c(2, 4, 6), munsell_chroma_moist = c(1, 3, 3)))
  cl <- data.frame(id = "p", maat = -1, mast_lo = -1, mast_hi = 3, cmi = 76,
                   moisture_subclass = "humid", moisture_subclass_hargreaves = "semiarid")
  # humid and semiarid: subhumid, which the key reads exactly, is tried too
  sc <- npdb_scenario(list(p = p), climate = cl)[[1]]
  expect_identical(sc$site$npdb_scenario$moisture_alt, c("subhumid", "semiarid"))
  v <- soilKey:::.npdb_scenario_versions(sc)
  expect_setequal(unique(vapply(v, function(q) q$site$soil_moisture_subclass, "")),
                  c("humid", "subhumid", "semiarid"))
  # a second call adds to the ranges of the first
  two <- npdb_scenario(npdb_scenario(list(p = p), climate = cl), dry_colour = m)[[1]]
  expect_equal(two$site$npdb_scenario$mast_range, c(-1, 3))
  expect_identical(two$site$npdb_scenario$dry_value_rows, 1L)
  # a pedon with no id gets no climate, and the others still do
  q <- p$clone(deep = TRUE); q$site$id <- NULL
  out <- npdb_scenario(list(a = q, p = p), climate = cl)
  expect_null(out$a$site$soil_moisture_subclass)
  expect_identical(out$p$site$soil_moisture_subclass, "humid")
  expect_error(npdb_soil_climate(data.frame(), mast_offset = 2), "two numbers")
  # numeric or missing ids do not upset the folds of the colour model
  peds <- lapply(1:25, function(i) .np(list(designation = "Ah", top_cm = 0, bottom_cm = 15,
    munsell_value_moist = 2 + (i %% 4) / 2, munsell_value_dry = 3.5 + (i %% 4) / 2 + (i %% 3 - 1) / 4,
    munsell_chroma_moist = 1 + i %% 3, munsell_chroma_dry = 1 + i %% 3), list(id = 1000 + i)))
  expect_s3_class(npdb_dry_colour_model(peds), "npdb_dry_colour_model")
  for (k in seq_along(peds)) peds[[k]]$site$id <- NULL
  expect_s3_class(npdb_dry_colour_model(peds), "npdb_dry_colour_model")
})

test_that("second review fixes: ranges set again, offsets, ids and tables", {
  skip_on_cran()
  p <- .np(list(designation = c("Ah", "Bm"), top_cm = c(0, 15), bottom_cm = c(15, 40),
                munsell_value_moist = c(2, 4), munsell_chroma_moist = c(1, 3)), list(id = 100000))
  cl1 <- data.frame(id = "100000", maat = -1, mast_lo = -1, mast_hi = 3, cmi = 76,
                    moisture_subclass = "Humid", moisture_subclass_hargreaves = "semiarid")
  one <- npdb_scenario(list(p), climate = cl1)[[1]]
  expect_identical(one$site$soil_moisture_subclass, "humid")         # a number id still matches
  expect_equal(one$site$npdb_scenario$mast_range, c(-1, 3))
  # cleared and set again: the ranges of the first call do not stay
  one$site$mean_annual_soil_temp_C <- NULL; one$site$soil_moisture_subclass <- NULL
  cl2 <- data.frame(id = "100000", maat = 3, mast_lo = 3, mast_hi = 7, cmi = 76,
                    moisture_subclass = "humid", moisture_subclass_hargreaves = "humid")
  two <- npdb_scenario(list(one), climate = cl2)[[1]]
  expect_equal(two$site$mean_annual_soil_temp_C, 5)
  expect_null(two$site$npdb_scenario)
  expect_length(soilKey:::.npdb_scenario_versions(two), 1L)
  # the moisture index goes with the subclass it was read from
  p$site$soil_moisture_subclass <- "semiarid"
  expect_null(npdb_scenario(list(p), climate = cl1)[[1]]$site$npdb_cmi)
  # a table without the second subclass or the soil temperature
  p$site$soil_moisture_subclass <- NULL
  bare <- data.frame(id = "100000", maat = NA, cmi = 50, moisture_subclass = "semiarid")
  b <- npdb_scenario(list(p), climate = bare)[[1]]
  expect_identical(b$site$soil_moisture_subclass, "semiarid")
  expect_null(b$site$mean_annual_soil_temp_C)
  # offsets in either order; text is refused
  nm <- .normals("x", 50, .prairie_ta, prec = rep(50, 7))
  expect_identical(npdb_soil_climate(nm, c(4, 0))$mast_lo, npdb_soil_climate(nm, c(0, 4))$mast_lo)
  expect_error(npdb_soil_climate(nm, c("0", "4")), "two numbers")
  # with the moisture alone there is no copy of the central version
  q <- npdb_scenario(list(p), climate = data.frame(id = "100000", maat = 5, mast_lo = 5, mast_hi = 6,
    cmi = 76, moisture_subclass = "humid", moisture_subclass_hargreaves = "subhumid"))[[1]]
  v <- soilKey:::.npdb_scenario_versions(q)
  expect_identical(vapply(v, function(x) x$site$soil_moisture_subclass, ""), c("humid", "subhumid"))
})
