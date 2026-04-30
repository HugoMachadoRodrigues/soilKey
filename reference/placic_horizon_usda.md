# Placic horizon (USDA, KST 13ed Ch 3, pp 47-48)

Pass when a thin (1-25 mm) Fe/Mn-cemented horizon is present. Detected
via designation containing 'm' (cemented) AND `cementation_class` in
{strongly, indurated} AND thickness between 1 mm and 25 mm.

## Usage

``` r
placic_horizon_usda(pedon, max_top_cm = 100)
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
