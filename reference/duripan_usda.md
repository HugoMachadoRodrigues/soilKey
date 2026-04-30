# Duripan (USDA, KST 13ed Ch 3, pp 36-37)

Silica-cemented horizon, very strongly resistant. Detected via
`cementation_class == "indurated"` AND `duripan_pct >= 50`.

## Usage

``` r
duripan_usda(pedon, max_top_cm = 100)
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
