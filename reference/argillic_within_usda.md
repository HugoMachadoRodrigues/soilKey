# Argillic horizon helper (USDA, KST 13ed Ch 3)

Wrapper around argillic_usda that simply re-exports the DiagnosticResult
with a max-depth check (default 100 cm for Argiorthels Subgroup keys).

## Usage

``` r
argillic_within_usda(pedon, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 100 cm.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
