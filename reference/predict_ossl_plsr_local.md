# Local PLSR prediction against the OSSL library

Selects the `k` nearest neighbours to each test spectrum in the OSSL
training set and fits a local PLS regression. Like
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md),
this function dispatches to
[`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html)
(with a `local_algorithm = "pls"` setting) when the dependency is
available; otherwise it falls back to the synthetic predictor.

## Usage

``` r
predict_ossl_plsr_local(
  X,
  properties,
  region = "global",
  k = 100L,
  ossl_library = NULL,
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

- k:

  Integer number of neighbours.

- ossl_library:

  Optional list with the OSSL training spectra (`Xr`) and reference
  values (`Yr`, a data.frame keyed by `properties`). When `NULL`, the
  synthetic path is used.

- ...:

  Additional arguments forwarded to
  [`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html).

## Value

A data.table with the same schema as
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md).
