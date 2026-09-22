# Predict soil properties from spectra

Ergonomic, named entry point for the OSSL-backed predictive pipeline.
Accepts either a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
or a numeric spectra matrix, applies the same preprocessing used at
training time (recorded on each model), and returns predictions in the
canonical long-form schema.

## Usage

``` r
predict_from_spectra(
  pedon_or_spectra,
  models = NULL,
  properties = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- pedon_or_spectra:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  (predictions merged into the pedon) OR a numeric matrix / vector of
  raw Vis-NIR spectra (rows = horizons, columns = wavelengths).

- models:

  A named list of `soilKey_pls_model` objects (output of
  [`train_pls_from_ossl`](https://hugomachadorodrigues.github.io/soilKey/reference/train_pls_from_ossl.md)).
  Required.

- properties:

  Character vector of property names to predict. Defaults to all
  properties in `models`.

- overwrite:

  Passed to
  [`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
  when `pedon_or_spectra` is a PedonRecord.

- verbose:

  Verbosity passed downstream.

- ...:

  Ignored (reserved for future backends).

## Value

Either the mutated `PedonRecord` (invisibly) or a data.table with
columns `horizon_idx`, `property`, `value`, `pi95_low`, `pi95_high`,
`n_neighbors`.

## Details

When `pedon_or_spectra` is a `PedonRecord`, this function delegates to
[`fill_from_spectra`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
with `method = "pretrained"` and the predictions are written back to the
pedon (with `source = "predicted_spectra"` provenance). When
`pedon_or_spectra` is a numeric matrix or vector, this function returns
the prediction data.table directly without touching any pedon.

## Examples

``` r
if (FALSE) { # \dontrun{
lib <- download_ossl_subset(region = "south_america")
models <- train_pls_from_ossl(lib,
                                properties = c("clay_pct", "ph_h2o"))
predict_from_spectra(my_pedon, models = models)
} # }
```
