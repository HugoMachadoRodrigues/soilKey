# Tests for report_to_qgis() -- the GeoPackage exporter for QGIS.
# Uses a real PedonRecord fixture + handcrafted ClassificationResults
# so no network is needed. Skips entirely when sf is not installed.


test_that("report_to_qgis() validates inputs", {
  skip_if_not_installed("sf")
  pedon <- make_ferralsol_canonical()
  expect_error(
    report_to_qgis("not a pedon", list(),
                     file = tempfile(fileext = ".gpkg")),
    "PedonRecord"
  )
  expect_error(
    report_to_qgis(pedon, "not a list",
                     file = tempfile(fileext = ".gpkg")),
    "list"
  )
  expect_error(
    report_to_qgis(pedon, list(),
                     file = tempfile(fileext = ".kml")),
    "\\.gpkg"
  )
  expect_error(
    report_to_qgis(pedon, list(), file = ""),
    "non-empty path"
  )
})


test_that("report_to_qgis() writes a multi-layer GeoPackage with the canonical pedon_point layer", {
  skip_if_not_installed("sf")
  pedon <- make_ferralsol_canonical()
  results <- list(
    wrb = ClassificationResult$new(
      system = "WRB 2022",
      name   = "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)",
      rsg_or_order   = "Ferralsols",
      qualifiers     = list(principal = c("Geric", "Ferric", "Rhodic"),
                              supplementary = c("Clayic", "Humic")),
      evidence_grade = "A"
    )
  )
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)
  res <- report_to_qgis(pedon, results, file = out)
  expect_equal(normalizePath(res), normalizePath(out))
  expect_true(file.exists(out))

  # Inspect the layers.
  layers <- sf::st_layers(out)$name
  expect_true("pedon_point" %in% layers)
  expect_true("horizons_table" %in% layers)

  pt <- sf::read_sf(out, layer = "pedon_point")
  expect_s3_class(pt, "sf")
  expect_equal(nrow(pt), 1L)
  expect_equal(pt$wrb_rsg,    "Ferralsols")
  expect_equal(pt$wrb_grade,  "A")
  expect_equal(pt$wrb_principal,
                 "Geric, Ferric, Rhodic")
  expect_equal(pt$wrb_supplementary,
                 "Clayic, Humic")
  expect_equal(pt$site_id, pedon$site$id)
})


test_that("report_to_qgis() round-trips horizons_table with site_id + horizon_idx columns", {
  skip_if_not_installed("sf")
  pedon <- make_ferralsol_canonical()
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)
  report_to_qgis(pedon, list(), file = out)
  hz <- sf::read_sf(out, layer = "horizons_table")
  expect_equal(nrow(hz), nrow(pedon$horizons))
  expect_true(all(c("site_id", "horizon_idx") %in% names(hz)))
  expect_equal(hz$site_id[1], pedon$site$id)
  expect_equal(hz$horizon_idx[1], 1L)
})


test_that("report_to_qgis() refuses to overwrite when overwrite = FALSE", {
  skip_if_not_installed("sf")
  pedon <- make_ferralsol_canonical()
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)
  report_to_qgis(pedon, list(), file = out)
  expect_error(
    report_to_qgis(pedon, list(), file = out, overwrite = FALSE),
    "already exists"
  )
})


test_that("report_to_qgis() warns and continues when the pedon has no coordinates", {
  skip_if_not_installed("sf")
  pedon <- PedonRecord$new(
    site = list(id = "no-coords"),
    horizons = data.frame(top_cm = 0, bottom_cm = 30,
                            designation = "A", clay_pct = 25)
  )
  out <- tempfile(fileext = ".gpkg")
  on.exit(unlink(out), add = TRUE)
  expect_warning(
    report_to_qgis(pedon, list(), file = out),
    "no \\(lat, lon\\)"
  )
  layers <- sf::st_layers(out)$name
  expect_true("pedon_point_attributes" %in% layers)
  expect_false("pedon_point" %in% layers)
})
