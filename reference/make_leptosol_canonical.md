# Build the canonical Leptosol fixture

Synthetic mountain-slope Leptosol on metamorphic rock: a thin A (10 cm)
directly over continuous rock. By construction:

- [`leptic_features`](https://hugomachadorodrigues.github.io/soilKey/reference/leptic_features.md):
  PASSES (R at 10 cm \<= 25).

- Other diagnostics fail on thickness, missing data, or absent
  diagnostic features.

## Usage

``` r
make_leptosol_canonical()
```

## Value

A
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
