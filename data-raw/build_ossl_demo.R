# =============================================================================
# Build a small OSSL "demo" dataset for the package.
#
# Goal: ship a reproducible, ~ few-hundred-KB artefact under
# `data/ossl_demo_sa.rda` so vignettes / examples / tests can exercise
# the spectra pipeline without needing a network. The shape is the
# canonical `list(Xr, Yr, metadata)` consumed by
# `predict_ossl_mbl()` / `fill_from_spectra()`.
#
# This file is *not* a substitute for real OSSL data. It is a
# physically plausible synthetic that mirrors the OSSL property
# ranges (per `.ossl_property_ranges()` in `R/spectra-predict.R`) and
# the typical Vis-NIR/SWIR shape (350-2500 nm at 1-nm). Run
# `download_ossl_subset(region = "south_america")` to obtain the real
# dataset for paper-grade work.
#
# Re-build with:
#   source("data-raw/build_ossl_demo.R")
# =============================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(soilKey)
  }
  library(data.table)
})

build_ossl_demo_sa <- function(n_profiles = 80L,
                                 wavelengths = 350:2500,
                                 seed       = 20260430L) {
  set.seed(seed)
  n_wl <- length(wavelengths)

  # Generate a baseline reflectance with a tropical Latossolo / Argissolo
  # signature (1400 nm + 1900 nm absorption + ~2200 nm clay-OH).
  baseline <- 0.30 + 0.0001 * (wavelengths - 350)

  # Draw n_profiles spectra by perturbing the baseline with property-
  # correlated absorption depths.
  Xr <- matrix(NA_real_, nrow = n_profiles, ncol = n_wl)
  Yr <- data.frame(
    clay_pct   = pmin(pmax(rnorm(n_profiles, 45, 14), 5), 85),
    sand_pct   = pmin(pmax(rnorm(n_profiles, 30, 12), 5), 80),
    silt_pct   = pmin(pmax(rnorm(n_profiles, 20,  8), 5), 50),
    cec_cmol   = pmin(pmax(rnorm(n_profiles,  8,  4), 1), 30),
    bs_pct     = pmin(pmax(rnorm(n_profiles, 35, 20), 3), 95),
    ph_h2o     = pmin(pmax(rnorm(n_profiles,  5.4, 0.8), 4.0), 8.0),
    oc_pct     = pmin(pmax(rlnorm(n_profiles, log(1.2), 0.6), 0.1), 6),
    fe_dcb_pct = pmin(pmax(rnorm(n_profiles,  6.5, 2.5), 0.5), 18),
    caco3_pct  = pmin(pmax(rlnorm(n_profiles, log(0.3), 1.0), 0), 20)
  )

  for (i in seq_len(n_profiles)) {
    clay <- Yr$clay_pct[i]
    oc   <- Yr$oc_pct[i]
    fe   <- Yr$fe_dcb_pct[i]
    spec <- baseline +
      0.06 * exp(-((wavelengths - 1400)^2) / 5000) * (oc / 5) +    # OH water
      0.10 * exp(-((wavelengths - 1900)^2) / 5000) * (clay / 60) + # clay-OH
      0.08 * exp(-((wavelengths - 2200)^2) / 6000) * (clay / 60) + # Al-OH
      0.04 * exp(-((wavelengths -  900)^2) / 9000) * (fe  /  8) + # Fe oxide
      rnorm(n_wl, sd = 0.005)
    Xr[i, ] <- pmin(pmax(spec, 0.05), 0.85)
  }
  colnames(Xr) <- as.character(wavelengths)

  list(
    Xr = Xr,
    Yr = Yr,
    metadata = list(
      region       = "south_america",
      n_profiles   = n_profiles,
      properties   = names(Yr),
      snapshot     = "demo (synthetic, deterministic seed)",
      source_url   = "(none -- run download_ossl_subset() for real data)",
      cache_file   = NA_character_,
      seed         = seed,
      wavelengths_nm = range(wavelengths),
      note         = paste0(
        "Synthetic OSSL-shaped artefact for vignettes / examples / ",
        "tests. Property ranges follow OSSL global summary statistics; ",
        "spectra are physically plausible but not real measurements. ",
        "For paper-grade work, replace with the real OSSL South-America ",
        "subset via download_ossl_subset(region='south_america')."
      )
    )
  )
}

ossl_demo_sa <- build_ossl_demo_sa()

# Sanity checks.
stopifnot(
  is.matrix(ossl_demo_sa$Xr),
  nrow(ossl_demo_sa$Xr) == nrow(ossl_demo_sa$Yr),
  ncol(ossl_demo_sa$Xr) == 2151L,
  all(c("clay_pct", "sand_pct", "ph_h2o") %in% names(ossl_demo_sa$Yr)),
  identical(ossl_demo_sa$metadata$region, "south_america")
)

usethis::use_data(ossl_demo_sa, overwrite = TRUE, compress = "xz")
message(sprintf(
  "Wrote data/ossl_demo_sa.rda (%d profiles x %d wavelengths)",
  nrow(ossl_demo_sa$Xr), ncol(ossl_demo_sa$Xr)
))
