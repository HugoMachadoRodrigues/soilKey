# Frasiwassists Subgroup helper (Wassists)

Pass when ec_dS_m \< 0.6 (1:5 soil:water) in all horizons within 100 cm.
KST 13ed, Ch 10, p 203.

## Usage

``` r
frasic_qualifying_usda(pedon, max_ec = 0.6, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_ec:

  Default 0.6.

- max_top_cm:

  Default 100.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
