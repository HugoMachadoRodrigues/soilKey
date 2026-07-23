# =============================================================================
# v0.9.191 -- the provenance ledger must name the backend that actually ran.
#
# Up to v0.9.190 fill_from_spectra() wrote its note as "OSSL/<method>/<region>"
# unconditionally. Without an `ossl_library` (the default, and what the Shiny
# app passes) the numbers come from .predict_synthetic() -- runif() draws
# inside plausibility ranges -- so the ledger recorded a placeholder as an OSSL
# prediction. For a package whose central claim is provenance, that is the one
# label it must never get wrong.
# =============================================================================

# A pedon with two horizons and a matching Vis-NIR matrix. The spectra are
# arbitrary: with no library supplied the synthetic backend runs regardless,
# which is exactly the path under test.
.spec_pedon <- function(n_h = 2L, n_bands = 60L) {
  hz <- data.frame(
    top_cm    = c(0, 30)[seq_len(n_h)],
    bottom_cm = c(30, 80)[seq_len(n_h)],
    clay_pct  = rep(NA_real_, n_h),
    oc_pct    = rep(NA_real_, n_h)
  )
  set.seed(11)
  X <- matrix(runif(n_h * n_bands, 0.1, 0.6), nrow = n_h,
              dimnames = list(NULL, as.character(seq(400, by = 10,
                                                     length.out = n_bands))))
  soilKey::PedonRecord$new(site = list(id = "spec-01"), horizons = hz,
                           spectra = list(vnir = X))
}

test_that("v0.9.191: synthetic gap-fill is labelled SYNTHETIC, never OSSL", {
  skip_on_cran()
  p <- suppressWarnings(
    soilKey::fill_from_spectra(.spec_pedon(), method = "mbl",
                               region = "global", verbose = FALSE))
  notes <- p$provenance$notes[p$provenance$source == "predicted_spectra"]
  expect_gt(length(notes), 0L)
  expect_true(all(grepl("^SYNTHETIC/", notes)))
  # The old, wrong label must not reappear anywhere in the ledger.
  expect_false(any(grepl("^OSSL/", notes)))
})

test_that("v0.9.191: every method reports the backend it really used", {
  skip_on_cran()
  # No ossl_library / ossl_models is supplied, so all three fall through to the
  # synthetic predictor -- and all three must say so.
  for (m in c("mbl", "plsr_local", "pretrained")) {
    p <- suppressWarnings(
      soilKey::fill_from_spectra(.spec_pedon(), method = m,
                                 region = "global", verbose = FALSE))
    expect_identical(p$spectra$gapfill_backend, "synthetic",
                     info = paste("method:", m))
    notes <- p$provenance$notes[p$provenance$source == "predicted_spectra"]
    expect_true(all(grepl(paste0("^SYNTHETIC/", m, "/"), notes)),
                info = paste("method:", m))
  }
})

test_that("v0.9.191: the app can tell placeholders from predictions", {
  skip_on_cran()
  # mod_spectra.R switches its notification on exactly this field; if it stops
  # being set, the app silently goes back to reporting a plain success.
  p <- suppressWarnings(
    soilKey::fill_from_spectra(.spec_pedon(), verbose = FALSE))
  expect_true("gapfill_backend" %in% names(p$spectra))
  expect_true(p$spectra$gapfill_backend %in%
                c("synthetic", "resemble", "pretrained"))
})

test_that("v0.9.191: the UI no longer promises an OSSL download", {
  skip_on_cran()
  # The old string told users the first run would download an OSSL cache. No
  # code path in the app ever did, so it described a capability that does not
  # exist. Both locales must be free of that promise and carry the warning key.
  y <- yaml::read_yaml(system.file("i18n", "translations.yaml",
                                   package = "soilKey"))
  for (loc in c("en", "pt")) {
    expect_true("spectra.gapfill_synthetic" %in% names(y[[loc]]),
                info = loc)
    expect_false(grepl("downloads an OSSL cache|baixa um cache OSSL",
                       y[[loc]][["spectra.help_first_use"]]),
                 info = loc)
  }
})
