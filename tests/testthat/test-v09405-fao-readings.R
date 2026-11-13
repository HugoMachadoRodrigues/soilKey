# v0.9.405 -- readings of the FAO-Unesco legends for profiles that record
# little (Hugo, 2026-11-12). Every profile below is synthetic.

.p405 <- function(..., site = list(id = "p405")) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  suppressWarnings(PedonRecord$new(site = site, horizons = h))
}

test_that("a layer designated as mineral is not organic material", {
  skip_on_cran()
  p <- .p405(top_cm = c(0, 17), bottom_cm = c(17, 40), designation = c("A1", "B2ca"),
             clay_pct = c(38, 36), ph_h2o = 8.1)
  for (ed in c("1988", "1974")) {
    r <- suppressWarnings(classify_fao(p, edition = ed, on_missing = "silent"))
    g <- if (ed == "1988") "HS" else "O"
    expect_false(r$trace$major_groups[[g]]$passed, info = ed)
  }
  x <- soilKey:::.f88_ctx(p)
  expect_identical(soilKey:::.fao_mineral_designated(x), c(TRUE, TRUE))
  o <- soilKey:::.f88_organic(x)
  expect_identical(o$value, c(FALSE, FALSE))
  expect_true(all(o$flags == "mineral_layer_from_designation"))
  # an H layer, or a layer with no designation, is not read so
  q <- .p405(top_cm = c(0, 20), bottom_cm = c(20, 40), designation = c("H", NA), clay_pct = 10)
  expect_identical(soilKey:::.fao_mineral_designated(soilKey:::.f88_ctx(q)), c(FALSE, FALSE))
})

test_that("no organic layer below the deepest layer described", {
  skip_on_cran()
  # 0-30 cm of mineral soil: 50 cm of the upper 80 are not described
  p <- .p405(top_cm = 0, bottom_cm = 30, designation = "Ah", oc_pct = 3, clay_pct = 20)
  expect_false(histosols_gate_fao88(p)$passed)
  expect_false(histosols_gate_fao74(p)$passed)
})

test_that("the spodic B is absent from a described profile that names none", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), clay_pct = 5, silt_pct = 5,
             sand_pct = 90, oc_pct = c(2, 0.6, 0.2), ph_h2o = 4.5)
  p <- do.call(.p405, c(hz, list(designation = c("A", "Bw", "C"))))
  r <- spodic_b_fao88(p)
  expect_false(r$passed); expect_true("spodic_b_not_in_the_designations" %in% r$evidence$flags)
  expect_false(spodic_b_fao74(p)$passed)
  # a Bs leaves the chemistry to decide
  p <- do.call(.p405, c(hz, list(designation = c("E", "Bs", "C"))))
  expect_false("spodic_b_not_in_the_designations" %in% (spodic_b_fao88(p)$evidence$flags %||% character(0)))
})

test_that("fluvic properties: undecided criteria with no sediment origin are absent", {
  skip_on_cran()
  p <- .p405(top_cm = c(0, 17), bottom_cm = c(17, 46), designation = c("A", "C"), clay_pct = c(32, 50))
  r <- fluvic_properties_fao88(p)
  expect_false(r$passed)
  expect_true("fluvic_undecided_no_sediment_origin_recorded_absent" %in% r$evidence$flags)
  r <- recent_alluvial_deposits_fao74(p)
  expect_false(r$passed)
  # with an alluvial parent material recorded the criteria stay undecided
  q <- .p405(top_cm = c(0, 17), bottom_cm = c(17, 46), designation = c("A", "C"), clay_pct = c(32, 50),
             site = list(id = "q", parent_material = "alluvium"))
  expect_true(is.na(fluvic_properties_fao88(q)$passed))
})

test_that("plinthite, a field feature, is absent below the description; rock is not read so", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bt"))
  r <- soilKey:::.fao_within(h, c(FALSE, FALSE), 125, field = TRUE)
  expect_false(r$value)
  expect_true("absent_below_the_description" %in% r$flags)
  expect_true(is.na(soilKey:::.fao_within(h, c(FALSE, FALSE), 125)$value))
})

test_that("no free carbonate below pH 7", {
  skip_on_cran()
  p <- .p405(top_cm = c(0, 30), bottom_cm = c(30, 60), designation = c("A", "B"), ph_h2o = c(6.8, 6.9))
  x <- soilKey:::.f88_ctx(p)
  cc <- soilKey:::.f88_caco3(x)
  expect_identical(cc$value, c(0, 0))
  expect_true(all(cc$flags == "no_carbonate_at_ph_below_7"))
  q <- .p405(top_cm = 0, bottom_cm = 30, designation = "A", ph_h2o = 7)
  expect_true(is.na(soilKey:::.f88_caco3(soilKey:::.f88_ctx(q))$value))
})
