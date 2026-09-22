# Canonical schema for an \`ossl_library\` object

[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
and
[`predict_ossl_plsr_local`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md)
take an `ossl_library` argument that must be a list with two named
elements:

## Usage

``` r
ossl_library_template(
  wavelengths = 350:2500,
  properties = c("clay_pct", "sand_pct", "silt_pct", "cec_cmol", "bs_pct", "ph_h2o",
    "oc_pct", "fe_dcb_pct", "caco3_pct")
)
```

## Arguments

- wavelengths:

  Integer vector of wavelengths (default `350:2500` nm for
  Vis-NIR/SWIR).

- properties:

  Character vector of property column names to seed the empty `Yr`
  data.frame with.

## Value

A list with `Xr` (a 0-row matrix of the right column dimension) and `Yr`
(an empty data.frame with the requested columns).

## Details

- `Xr`: numeric matrix, rows = OSSL training spectra, columns =
  wavelengths. Must align (after preprocessing) with the column space
  used by the spectra you predict on.

- `Yr`: data.frame keyed by property name (e.g. `clay_pct`, `cec_cmol`),
  one row per training spectrum.

This function returns an empty template you can populate from a real
OSSL extract (e.g. via the `ossl-import` Python package or the public S3
mirror at `https://storage.googleapis.com/soilspec4gg-public/`).

soilKey does **not** bundle OSSL data; until you populate this template
with real values, all \`predict_ossl\_\*\` calls fall back to the
deterministic synthetic predictor (which prints a warning).
