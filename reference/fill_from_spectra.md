# Fill missing soil attributes from spectra via OSSL

Given a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
carrying a `spectra$vnir` matrix (rows = horizons, columns = wavelengths
in nm), pre-processes the spectra, predicts the requested soil
properties using the chosen OSSL-backed method, and writes the
predictions into the pedon's horizons table via
`pedon$add_measurement(..., source = "predicted_spectra")`. Each call
updates the pedon's provenance log so that downstream classification can
derive an evidence grade.

## Usage

``` r
fill_from_spectra(
  pedon,
  library = "ossl",
  region = c("global", "south_america", "north_america", "europe", "africa"),
  properties = c("clay_pct", "sand_pct", "silt_pct", "cec_cmol", "bs_pct", "ph_h2o",
    "oc_pct", "fe_dcb_pct", "caco3_pct"),
  method = c("mbl", "plsr_local", "pretrained"),
  preprocess = "snv+sg1",
  k_neighbors = 100L,
  overwrite = FALSE,
  ossl_library = NULL,
  ossl_models = NULL,
  verbose = TRUE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  with a `spectra$vnir` matrix.

- library:

  Currently only `"ossl"` is supported.

- region:

  One of `"global"`, `"south_america"`, `"north_america"`, `"europe"`,
  `"africa"`. Used to subset the OSSL training data when supported by
  the underlying backend.

- properties:

  Character vector of OSSL-supported property names to predict. Default
  covers the most-requested WRB/SiBCS-relevant attributes.

- method:

  One of `"mbl"`, `"plsr_local"`, `"pretrained"`.

- preprocess:

  Pre-processing pipeline; passed to
  [`preprocess_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md).

- k_neighbors:

  Number of neighbours for memory-based methods.

- overwrite:

  If `FALSE` (default), only fill cells whose existing provenance is
  weaker than `predicted_spectra`.

- ossl_library:

  Optional OSSL library object (see
  [`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)).

- ossl_models:

  Optional named list of pretrained models (see
  [`predict_ossl_pretrained`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md)).

- verbose:

  If `TRUE`, prints a cli summary.

## Value

The mutated pedon, invisibly. Provenance entries with
`source = "predicted_spectra"` are added per (horizon, property).

## Details

By default, predicted values do **not** overwrite measured values (the
`add_measurement()` authority logic protects them). Setting
`overwrite = TRUE` forces overwrite of any non-measured value.

## See also

[`preprocess_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md),
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md),
[`predict_ossl_plsr_local`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md),
[`predict_ossl_pretrained`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md),
[`pi_to_confidence`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md).
