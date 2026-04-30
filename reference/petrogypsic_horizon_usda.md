# Petrogypsic horizon helper (USDA)

Pass when a horizon has `cementation_class` in {strongly, indurated} AND
`caso4_pct >= 5` within `max_top_cm`.

## Usage

``` r
petrogypsic_horizon_usda(pedon, max_top_cm = 100)
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
