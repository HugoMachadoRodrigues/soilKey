# Memory-based learning prediction against the OSSL library

Predicts a set of soil properties from pre-processed Vis-NIR or MIR
spectra using *memory-based learning* (MBL) – the recommended OSSL
workflow for heterogeneous libraries. Defaults follow the literature
(Ramirez-Lopez et al., 2013): `k = 100` neighbours, PLS-score
dissimilarity, local PLS regression with 5 components, internal
leave-one-out validation.

## Usage

``` r
predict_ossl_mbl(
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

A data.table with columns
`horizon_idx, property, value, pi95_low, pi95_high, n_neighbors`. The
`"backend"` attribute records which path was taken (`"resemble"` or
`"synthetic"`).

## Details

If
[`resemble::mbl`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html)
is installed and an `ossl_library` artefact is supplied (a list with
elements `Xr`, `Yr`) the function delegates to
[`resemble::mbl()`](https://l-ramirez-lopez.github.io/resemble/reference/mbl.html);
otherwise it returns a deterministic synthetic prediction conditioned on
the input spectra so that downstream code, tests and vignettes run
without external dependencies. The fallback is annotated via the `notes`
attribute on the returned data.table.

## References

Ramirez-Lopez, L., Behrens, T., Schmidt, K., Stevens, A., Demattê, J. A.
M., & Scholten, T. (2013). The spectrum-based learner: A new local
approach for modeling soil Vis-NIR spectra of complex datasets.
*Geoderma*, 195–196, 268–279.
