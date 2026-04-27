# ================================================================
# Tests for R/spectra-ossl.R
#
# fill_from_spectra() ties together preprocess_spectra(),
# predict_ossl_*() and PedonRecord$add_measurement(). The tests below
# validate the contract end-to-end on a synthetic pedon.
# ================================================================


test_that("pi_to_confidence() saturates at the expected boundaries", {
  # zero-width interval -> confidence = 1
  expect_equal(pi_to_confidence(10, 10, value = 10), 1)
  # width = 4 * |value| -> confidence = 0
  expect_equal(pi_to_confidence(0, 40, value = 10), 0)
  # half-width relative => 0.5
  expect_equal(pi_to_confidence(8, 12, value = 10), 0.9)

  # NA inputs -> neutral
  expect_equal(pi_to_confidence(NA, 10, value = 10), 0.5)
  expect_equal(pi_to_confidence(0, 10, value = NA),  0.5)
  # zero value path
  expect_lt(pi_to_confidence(-1, 1, value = 0), 1)
  expect_gte(pi_to_confidence(-1, 1, value = 0), 0)
})


test_that("pi_to_confidence() vectorises", {
  out <- pi_to_confidence(c(0, 5, 10), c(20, 15, 10), value = c(10, 10, 10))
  expect_length(out, 3L)
  expect_true(all(out >= 0 & out <= 1))
  # Width 0 row -> confidence 1.
  expect_equal(out[3], 1)
})


test_that("make_synthetic_pedon_with_spectra() returns a usable PedonRecord", {
  pedon <- make_synthetic_pedon_with_spectra()
  expect_s3_class(pedon, "PedonRecord")
  expect_equal(nrow(pedon$horizons), 5L)
  expect_true(is.matrix(pedon$spectra$vnir))
  expect_equal(ncol(pedon$spectra$vnir), 2151L)
  expect_equal(nrow(pedon$spectra$vnir), 5L)
})


test_that("fill_from_spectra() rejects non-PedonRecord input", {
  expect_error(fill_from_spectra(list()),
                "PedonRecord")
})


test_that("fill_from_spectra() requires a spectra$vnir matrix", {
  pedon <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  expect_error(fill_from_spectra(pedon), "vnir is missing|missing")
})


test_that("fill_from_spectra() rejects shape mismatches", {
  pedon <- make_synthetic_pedon_with_spectra(n_horizons = 5L)
  pedon$spectra$vnir <- pedon$spectra$vnir[1:3, , drop = FALSE]
  expect_error(fill_from_spectra(pedon, verbose = FALSE),
                "nrow")
})


test_that("fill_from_spectra() validates library name", {
  pedon <- make_synthetic_pedon_with_spectra()
  expect_error(fill_from_spectra(pedon, library = "elsewhere", verbose = FALSE),
                "ossl")
})


test_that("fill_from_spectra() writes provenance entries with predicted_spectra source", {
  pedon <- make_synthetic_pedon_with_spectra()
  before <- nrow(pedon$provenance)

  pedon <- fill_from_spectra(
    pedon,
    method     = "mbl",
    region     = "south_america",
    properties = c("clay_pct", "sand_pct", "silt_pct"),
    k_neighbors = 50L,
    verbose    = FALSE
  )

  after <- nrow(pedon$provenance)
  expect_gt(after, before)
  expect_true(all(pedon$provenance$source[seq.int(before + 1L, after)] ==
                       "predicted_spectra"))
  # Confidence values are in [0, 1].
  conf <- pedon$provenance$confidence[seq.int(before + 1L, after)]
  expect_true(all(conf >= 0 & conf <= 1))
})


test_that("fill_from_spectra() populates horizon columns it predicted", {
  pedon <- make_synthetic_pedon_with_spectra()
  expect_true(all(is.na(pedon$horizons$clay_pct)))

  pedon <- fill_from_spectra(
    pedon,
    method     = "mbl",
    region     = "global",
    properties = "clay_pct",
    verbose    = FALSE
  )
  expect_true(all(!is.na(pedon$horizons$clay_pct)))
  ranges <- soilKey:::.ossl_property_ranges()
  expect_true(all(pedon$horizons$clay_pct >= ranges$clay_pct[1]))
  expect_true(all(pedon$horizons$clay_pct <= ranges$clay_pct[2]))
})


test_that("fill_from_spectra() respects existing measured values by default", {
  pedon <- make_synthetic_pedon_with_spectra()
  # Pre-seed a measured clay value -- the predicted_spectra path must not
  # overwrite a measured cell when overwrite = FALSE.
  pedon$add_measurement(
    horizon_idx = 1L,
    attribute   = "clay_pct",
    value       = 40,
    source      = "measured",
    confidence  = 1
  )

  pedon <- fill_from_spectra(
    pedon,
    method     = "mbl",
    region     = "global",
    properties = "clay_pct",
    overwrite  = FALSE,
    verbose    = FALSE
  )
  expect_equal(pedon$horizons$clay_pct[1L], 40)
})


test_that("fill_from_spectra() with overwrite = TRUE overrides measured values", {
  pedon <- make_synthetic_pedon_with_spectra()
  pedon$add_measurement(
    horizon_idx = 1L,
    attribute   = "clay_pct",
    value       = 40,
    source      = "measured",
    confidence  = 1
  )

  pedon <- fill_from_spectra(
    pedon,
    method     = "mbl",
    region     = "global",
    properties = "clay_pct",
    overwrite  = TRUE,
    verbose    = FALSE
  )
  expect_false(identical(pedon$horizons$clay_pct[1L], 40))
})


test_that("fill_from_spectra() works for all three methods", {
  for (m in c("mbl", "plsr_local", "pretrained")) {
    pedon <- make_synthetic_pedon_with_spectra()
    pedon <- fill_from_spectra(
      pedon, method = m, region = "global",
      properties = c("ph_h2o", "oc_pct"),
      verbose = FALSE
    )
    expect_true(all(!is.na(pedon$horizons$ph_h2o)),
                  info = sprintf("method = %s", m))
    expect_true(all(!is.na(pedon$horizons$oc_pct)),
                  info = sprintf("method = %s", m))
  }
})


test_that("fill_from_spectra() is deterministic for fixed inputs", {
  p1 <- make_synthetic_pedon_with_spectra()
  p2 <- make_synthetic_pedon_with_spectra()

  p1 <- fill_from_spectra(p1, method = "mbl", region = "global",
                            properties = "cec_cmol", verbose = FALSE)
  p2 <- fill_from_spectra(p2, method = "mbl", region = "global",
                            properties = "cec_cmol", verbose = FALSE)
  expect_equal(p1$horizons$cec_cmol, p2$horizons$cec_cmol)
})


test_that("fill_from_spectra() notes carry method/region/PI metadata", {
  pedon <- make_synthetic_pedon_with_spectra()
  pedon <- fill_from_spectra(
    pedon, method = "mbl", region = "south_america",
    properties = "clay_pct", k_neighbors = 75L, verbose = FALSE
  )
  notes <- pedon$provenance$notes
  expect_true(any(grepl("OSSL/mbl/south_america", notes)))
  expect_true(any(grepl("k=75", notes)))
})
