# Train pre-trained PLSR models from an OSSL library

Iterates over `properties` and fits one PLSR model per target against
the OSSL spectra in `ossl_library$Xr`, with internal cross-validation to
pick the optimal number of components per property. The returned list is
a drop-in replacement for the `ossl_models` argument of
[`predict_ossl_pretrained`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md)
and
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md).

## Usage

``` r
train_pls_from_ossl(
  ossl_library,
  properties = c("clay_pct", "sand_pct", "silt_pct", "cec_cmol", "ph_h2o", "oc_pct"),
  ncomp_max = 20L,
  validation = c("CV", "LOO", "none"),
  segments = 10L,
  preprocess = "snv+sg1",
  min_n = 50L,
  verbose = TRUE
)
```

## Arguments

- ossl_library:

  A list with two named elements: `Xr` (numeric matrix of training
  spectra) and `Yr` (data.frame keyed by property name, one row per
  training spectrum). See
  [`ossl_library_template`](https://hugomachadorodrigues.github.io/soilKey/reference/ossl_library_template.md).

- properties:

  Character vector of column names in `ossl_library$Yr` to train models
  for. Defaults to the six core soil properties exposed by OSSL.

- ncomp_max:

  Integer. Upper bound on the number of PLS components to consider
  during cross-validation. Defaults to 20.

- validation:

  One of `"CV"` (default, k-fold), `"LOO"` (leave-one-out, slow),
  `"none"` (uses `ncomp_max` components without selection).

- segments:

  Number of CV segments when `validation = "CV"`. Default 10.

- preprocess:

  Pre-processing label passed to
  [`preprocess_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/preprocess_spectra.md).
  Stored on the trained models so
  [`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
  can reapply it.

- min_n:

  Minimum number of valid training samples (after dropping rows with
  non-finite y or X). Properties below this threshold are skipped with a
  warning. Default 50.

- verbose:

  If `TRUE` (default), prints a per-property summary on completion.

## Value

A named list of `soilKey_pls_model` objects, one per successfully
trained property. Carries `trained_at`, `soilKey_version` and
`preprocess` attributes for provenance.

## Details

Spectra are pre-processed inside the function (default `"snv+sg1"`); the
same preprocessing is used downstream by
[`predict_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_from_spectra.md)
so the user does not have to remember which transform was applied at
training time.

## Examples

``` r
if (FALSE) { # \dontrun{
lib <- download_ossl_subset(region = "south_america")
models <- train_pls_from_ossl(lib,
                               properties = c("clay_pct", "ph_h2o"))
result <- predict_from_spectra(my_pedon, models = models)
} # }
```
