# Download an OSSL subset and return an \`ossl_library\` artefact

Fetches a region-filtered subset of the Open Soil Spectral Library
(Sanderman et al. 2024) and assembles it into the \`list(Xr, Yr,
metadata)\` shape consumed by
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
and
[`predict_ossl_plsr_local`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md).
The result is cached under \`tools::R_user_dir("soilKey", "cache")\` so
subsequent calls in the same session (or future R sessions) skip the
network.

## Usage

``` r
download_ossl_subset(
  region = c("global", "south_america", "north_america", "europe", "africa", "asia",
    "oceania"),
  properties = c("clay_pct", "sand_pct", "silt_pct", "cec_cmol", "bs_pct", "ph_h2o",
    "oc_pct", "fe_dcb_pct", "caco3_pct"),
  wavelengths = 350:2500,
  endpoint = NULL,
  cache_dir = NULL,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- region:

  One of `"global"`, `"south_america"`, `"north_america"`, `"europe"`,
  `"africa"`, `"asia"`, `"oceania"`. Filters the OSSL training rows by
  their site coordinates' continent.

- properties:

  Character vector of OSSL property names to keep in \`Yr\` (drops other
  reference columns to keep the artefact small). Defaults to the
  WRB-relevant set used by
  [`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md).

- wavelengths:

  Integer vector of wavelengths (nm) the returned `Xr` matrix will be
  interpolated to. Defaults to Vis-NIR/SWIR (350-2500 nm at 1-nm
  resolution, 2151 columns).

- endpoint:

  OSSL HTTP endpoint serving the JSON manifest; overrideable via
  `options(soilKey.ossl_endpoint = ...)` for testing or for using a
  private mirror. The default is the public Soil Spectroscopy GG bucket.

- cache_dir:

  Cache directory; defaults to `tools::R_user_dir("soilKey", "cache")`.

- force:

  If `TRUE`, re-fetches even when a cached subset exists.

- verbose:

  If `TRUE`, emits a \`cli\` summary of the fetch.

## Value

A list with elements `Xr` (numeric matrix, rows = training profiles,
columns = wavelengths in nm), `Yr` (data.frame with the requested
property columns, rows aligned to `Xr`), and `metadata` (snapshot date,
region, n profiles, source URL, and the SHA-256 of the cache file). Pass
it as the `ossl_library` argument to
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
or
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md).

## Details

This function intentionally does **not** fall back to the synthetic
predictor on network failure – a missing OSSL artefact is a real
condition that the caller must handle, and silent fallback would make
benchmarks meaningless.

## References

Sanderman, J., Savage, K., Dangal, S.R.S., Duran, G., Rivard, C.,
Cardona, M.T., Sandzhieva, A., Aramian, A. & Safanelli, J.L. (2024).
Soil Spectroscopy for Global Good – the Open Soil Spectral Library
(OSSL). <https://soilspectroscopy.org/>.
