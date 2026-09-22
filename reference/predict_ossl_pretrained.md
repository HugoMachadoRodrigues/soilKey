# Pre-trained OSSL prediction

Applies the OSSL-distributed pre-trained PLSR / Cubist models for a set
of soil properties to pre-processed spectra. Pre-trained models are
loaded from `ossl_models`, a named list of property models that each
must implement a `predict(model, X)` interface returning a data.frame
with columns `value`, `pi95_low`, `pi95_high`. When `ossl_models` is
`NULL`, the synthetic predictor is used.

## Usage

``` r
predict_ossl_pretrained(
  X,
  properties,
  region = "global",
  ossl_models = NULL,
  ...
)
```

## Arguments

- X:

  A pre-processed numeric matrix (rows = horizons, columns =
  wavelengths).

- properties:

  Character vector of OSSL-supported property names.

- region:

  One of `"global"`, `"south_america"`, `"north_america"`, `"europe"`,
  `"africa"`.

- ossl_models:

  Optional named list of pre-trained models, keyed by property name.

- ...:

  Reserved.

## Value

A data.table with columns
`horizon_idx, property, value, pi95_low, pi95_high, n_neighbors`.
`n_neighbors` is `NA_integer_` for pre-trained models. The `"backend"`
attribute records which path was taken.
