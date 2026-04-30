# Duric Subgroup helper (USDA Spodosols)

Pass when a pedogenically cemented horizon (extremely weakly coherent or
stronger) is present in 90%+ of the pedon within 100 cm. v0.8 proxy: any
horizon with cementation_class \>= "weakly".

## Usage

``` r
duric_subgroup_usda(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 100.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
