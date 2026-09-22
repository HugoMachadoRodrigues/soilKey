# Tests for the v0.9.99 field-photo-only classification pipeline:
# compute_per_attribute_evidence_grade(), apply_soilgrids_depth_prior(),
# and the classify_from_photos() orchestrator.

# ---- compute_per_attribute_evidence_grade ---------------------------------

test_that("an all-measured pedon yields a zero-row grade table", {
  skip_on_cran()
  g <- compute_per_attribute_evidence_grade(make_ferralsol_canonical())
  expect_s3_class(g, "data.table")
  expect_identical(names(g), c("horizon_idx", "attribute", "grade"))
  expect_equal(nrow(g), 0L)
})

test_that("per-attribute grade reflects each cell's provenance source", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  p$add_measurement(1, "clay_pct", 51, "extracted_vlm",
                    confidence = 0.7, overwrite = TRUE)
  p$add_measurement(2, "cec_cmol", 6,  "inferred_prior",
                    confidence = 0.5, overwrite = TRUE)
  p$add_measurement(3, "ph_h2o",  4.9, "predicted_spectra",
                    confidence = 0.8, overwrite = TRUE)
  g <- compute_per_attribute_evidence_grade(p)
  # Use base-R subsetting so the assertion works without data.table NSE
  # being available in the test scope.
  expect_equal(g$grade[g$horizon_idx == 1 & g$attribute == "clay_pct"], "D")
  expect_equal(g$grade[g$horizon_idx == 2 & g$attribute == "cec_cmol"], "C")
  expect_equal(g$grade[g$horizon_idx == 3 & g$attribute == "ph_h2o"],   "B")
})

test_that("the most authoritative source wins a multiply-sourced cell", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  p$add_measurement(1, "clay_pct", 49, "inferred_prior",
                    confidence = 0.5, overwrite = TRUE)
  # measured outranks inferred_prior -> cell grade must be A.
  p$add_measurement(1, "clay_pct", 50, "measured",
                    confidence = 1, overwrite = TRUE)
  g <- compute_per_attribute_evidence_grade(p)
  expect_equal(g$grade[g$horizon_idx == 1 & g$attribute == "clay_pct"], "A")
})

test_that("compute_evidence_grade returns E for a user-assumed value", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  p$add_measurement(1, "clay_pct", 50, "user_assumed",
                    confidence = 0.2, overwrite = TRUE)
  expect_equal(classify_wrb2022(p, on_missing = "silent")$evidence_grade, "E")
})

# ---- apply_soilgrids_depth_prior ------------------------------------------

test_that("depth prior fills NA horizon attributes as inferred_prior", {
  skip_on_cran()
  p <- make_cambisol_canonical()
  p$horizons$clay_pct <- NA_real_
  apply_soilgrids_depth_prior(
    p, attrs = "clay_pct",
    depth_profiles = list(clay_pct = c(18, 20, 24, 28, 30, 30)))
  expect_false(anyNA(p$horizons$clay_pct))
  expect_true(all(p$horizons$clay_pct >= 0))
  expect_gt(sum(p$provenance$source == "inferred_prior"), 0L)
})

test_that("depth prior interpolates at the horizon mid-depth", {
  skip_on_cran()
  p <- make_cambisol_canonical()
  # One horizon spanning 0-20 cm -> mid-depth 10 cm == the 5-15cm slice mid.
  p$horizons <- p$horizons[1, ]
  p$horizons$top_cm <- 0; p$horizons$bottom_cm <- 20
  p$horizons$clay_pct <- NA_real_
  apply_soilgrids_depth_prior(
    p, attrs = "clay_pct",
    depth_profiles = list(clay_pct = c(10, 20, 30, 40, 50, 60)))
  # mid 10 cm sits exactly on the second slice mid (10 cm) -> value 20.
  expect_equal(p$horizons$clay_pct[1], 20)
})

test_that("depth prior never overwrites a measured value by default", {
  skip_on_cran()
  p <- make_cambisol_canonical()
  measured <- p$horizons$clay_pct
  apply_soilgrids_depth_prior(
    p, attrs = "clay_pct",
    depth_profiles = list(clay_pct = c(1, 1, 1, 1, 1, 1)))
  expect_equal(p$horizons$clay_pct, measured)
})

test_that("depth prior skips gracefully when there are no coordinates", {
  skip_on_cran()
  p <- make_cambisol_canonical()
  p$site$lat <- NULL; p$site$lon <- NULL
  expect_warning(apply_soilgrids_depth_prior(p, attrs = "clay_pct"),
                 "coordinates")
})

# ---- classify_from_photos -------------------------------------------------

test_that("classify_from_photos requires a provider", {
  skip_on_cran()
  expect_error(classify_from_photos(images = "x.jpg"), "provider")
})

test_that("classify_from_photos reports an error for a missing image", {
  skip_on_cran()
  res <- classify_from_photos(images = "does-not-exist.jpg",
                              provider = photo_mock(), soilgrids = FALSE)
  expect_true(!is.null(res$error))
  expect_null(res$pedon)
})

test_that("classify_from_photos builds and classifies a pedon from a photo", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("jsonvalidate")
  img <- photo_test_image()
  res <- classify_from_photos(images = img, lat = -22.7, lon = -43.6,
                              country = "BR", provider = photo_mock(),
                              soilgrids = FALSE)
  expect_null(res$error)
  expect_s3_class(res$pedon, "PedonRecord")
  expect_equal(nrow(res$pedon$horizons), 3L)
  expect_s3_class(res$wrb, "ClassificationResult")
  expect_true(res$wrb$evidence_grade %in% c("C", "D", "E"))
  expect_gt(sum(res$pedon$provenance$source == "extracted_vlm"), 0L)
})

test_that("classify_from_photos back-fills attributes from a depth prior", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("jsonvalidate")
  img <- photo_test_image()
  res <- classify_from_photos(images = img, lat = -22.7, lon = -43.6,
                              provider = photo_mock(), soilgrids = TRUE,
                              depth_profiles = photo_depth_profiles())
  expect_null(res$error)
  expect_gt(sum(res$pedon$provenance$source == "inferred_prior"), 0L)
  # With texture + chemistry filled, the clayey low-CEC profile keys to
  # Ferralsols rather than the data-poor Regosol catch-all.
  expect_equal(res$wrb$rsg_or_order, "Ferralsols")
})

test_that("classify_from_photos returns a summary row per system", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("jsonvalidate")
  img <- photo_test_image()
  res <- classify_from_photos(images = img, lat = -22.7, lon = -43.6,
                              provider = photo_mock(), soilgrids = FALSE)
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), 3L)
  expect_setequal(res$summary$system, c("wrb", "sibcs", "usda"))
})
