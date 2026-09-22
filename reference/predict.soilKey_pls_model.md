# Predict from a soilKey_pls_model

S3 method that applies a trained PLSR model from
[`train_pls_from_ossl`](https://hugomachadorodrigues.github.io/soilKey/reference/train_pls_from_ossl.md)
to a (pre-processed) numeric matrix and returns predictions plus a 95
built from the cross-validated training RMSE.

## Usage

``` r
# S3 method for class 'soilKey_pls_model'
predict(object, X, ...)
```

## Arguments

- object:

  A `soilKey_pls_model` object.

- X:

  A pre-processed numeric matrix (rows = samples, columns =
  wavelengths). Must have the same column count used at training time.

- ...:

  Reserved.

## Value

A data.frame with columns `value`, `pi95_low`, `pi95_high`, one row per
sample.
