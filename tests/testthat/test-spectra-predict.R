# ================================================================
# Tests for R/spectra-predict.R
#
# We test the synthetic backend (the one always available) for every
# entry point. The real `resemble`-backed branch is exercised through
# a duck-typed mock library so we can validate the wrapper without
# requiring the resemble package.
# ================================================================


make_synth_vnir <- function(n_horizons = 5L,
                              wavelengths = 350:2500,
                              seed = 11L) {
  set.seed(seed)
  base <- 0.25 + 0.0001 * (wavelengths - 350)
  noise <- matrix(rnorm(n_horizons * length(wavelengths), 0, 0.005),
                    nrow = n_horizons)
  X <- sweep(noise, 2, base, `+`)
  colnames(X) <- as.character(wavelengths)
  X
}


test_that("predict_ossl_mbl() returns the canonical schema", {
  X <- make_synth_vnir()
  out <- predict_ossl_mbl(
    X,
    properties = c("clay_pct", "ph_h2o", "oc_pct"),
    region     = "south_america",
    k          = 100L
  )
  expect_s3_class(out, "data.table")
  expect_named(out, c("horizon_idx", "property", "value",
                       "pi95_low", "pi95_high", "n_neighbors"),
                ignore.order = FALSE)
  expect_equal(nrow(out), 5L * 3L)
  expect_equal(sort(unique(out$property)),
                sort(c("clay_pct", "ph_h2o", "oc_pct")))
  expect_equal(sort(unique(out$horizon_idx)), 1:5)
  # PI95 must bracket the point prediction.
  expect_true(all(out$pi95_low <= out$value + 1e-9))
  expect_true(all(out$pi95_high >= out$value - 1e-9))
  # Backend tag must record the path taken.
  expect_equal(attr(out, "backend"), "synthetic")
  # n_neighbors is recorded for MBL.
  expect_true(all(out$n_neighbors == 100L))
})


test_that("predict_ossl_plsr_local() shares the schema with MBL", {
  X <- make_synth_vnir()
  out <- predict_ossl_plsr_local(X,
                                   properties = c("cec_cmol", "bs_pct"),
                                   region = "global", k = 50L)
  expect_named(out, c("horizon_idx", "property", "value",
                       "pi95_low", "pi95_high", "n_neighbors"))
  expect_equal(nrow(out), 5L * 2L)
  expect_true(all(out$n_neighbors == 50L))
  expect_equal(attr(out, "backend"), "synthetic")
})


test_that("predict_ossl_pretrained() reports NA n_neighbors", {
  X <- make_synth_vnir()
  out <- predict_ossl_pretrained(X,
                                   properties = c("clay_pct", "fe_dcb_pct"),
                                   region = "global")
  expect_true(all(is.na(out$n_neighbors)))
  expect_equal(attr(out, "backend"), "synthetic")
})


test_that("synthetic predictions are deterministic and within plausible ranges", {
  X <- make_synth_vnir()
  o1 <- predict_ossl_mbl(X, properties = c("clay_pct", "ph_h2o"), region = "global")
  o2 <- predict_ossl_mbl(X, properties = c("clay_pct", "ph_h2o"), region = "global")
  expect_equal(o1, o2)

  ranges <- soilKey:::.ossl_property_ranges()
  for (p in unique(o1$property)) {
    rng <- ranges[[p]]
    sub <- o1[o1$property == p, ]
    expect_true(all(sub$value     >= rng[1] - 1e-9))
    expect_true(all(sub$value     <= rng[2] + 1e-9))
    expect_true(all(sub$pi95_low  >= rng[1] - 1e-9))
    expect_true(all(sub$pi95_high <= rng[2] + 1e-9))
  }
})


test_that("different spectra yield different synthetic predictions", {
  X1 <- make_synth_vnir(seed = 1L)
  X2 <- make_synth_vnir(seed = 2L)
  o1 <- predict_ossl_mbl(X1, properties = "clay_pct", region = "global")
  o2 <- predict_ossl_mbl(X2, properties = "clay_pct", region = "global")
  expect_false(isTRUE(all.equal(o1$value, o2$value)))
})


test_that("predict_ossl_*() rejects unknown property names", {
  X <- make_synth_vnir()
  expect_error(predict_ossl_mbl(X, properties = "not_a_property"),
                "unknown OSSL property")
  expect_error(predict_ossl_plsr_local(X, properties = "still_bogus"),
                "unknown OSSL property")
  expect_error(predict_ossl_pretrained(X, properties = "?"),
                "unknown OSSL property")
})


test_that("predict_ossl_*() rejects malformed X / properties", {
  expect_error(predict_ossl_mbl(NULL, properties = "clay_pct"),
                "matrix")
  expect_error(predict_ossl_mbl(make_synth_vnir(), properties = character(0)),
                "non-empty")
})


test_that("region argument is validated", {
  X <- make_synth_vnir()
  expect_error(predict_ossl_mbl(X, properties = "clay_pct",
                                  region = "atlantis"))
})


test_that("predict_ossl_pretrained() uses ossl_models when supplied", {
  X <- make_synth_vnir(n_horizons = 3L)

  # Build a fake "model" with a predict() method that returns deterministic
  # values; this exercises the "real" branch of predict_ossl_pretrained().
  fake_model <- structure(
    list(centre = 25, half_width = 5),
    class = "fake_ossl_model"
  )
  predict.fake_ossl_model <- function(object, newdata, ...) {
    n <- nrow(newdata)
    data.frame(
      value     = rep(object$centre, n),
      pi95_low  = rep(object$centre - object$half_width, n),
      pi95_high = rep(object$centre + object$half_width, n)
    )
  }
  # Register the S3 method for the duration of this test.
  registerS3method("predict", "fake_ossl_model", predict.fake_ossl_model,
                    envir = asNamespace("base"))

  out <- predict_ossl_pretrained(
    X,
    properties = "clay_pct",
    ossl_models = list(clay_pct = fake_model)
  )
  expect_equal(unique(out$value), 25)
  expect_equal(unique(out$pi95_low), 20)
  expect_equal(unique(out$pi95_high), 30)
  expect_equal(attr(out, "backend"), "pretrained")
})


test_that("PI95 widths respond to region tweak (synthetic only)", {
  # Synthetic implementation widens spread for non-global regions.
  X <- make_synth_vnir()
  o_global <- predict_ossl_mbl(X, properties = "clay_pct", region = "global")
  o_sa     <- predict_ossl_mbl(X, properties = "clay_pct", region = "south_america")

  w_global <- mean(o_global$pi95_high - o_global$pi95_low)
  w_sa     <- mean(o_sa$pi95_high - o_sa$pi95_low)
  expect_gt(w_sa, w_global)
})
