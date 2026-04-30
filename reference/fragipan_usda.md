# Fragipan (USDA, KST 13ed Ch 3, p 38)

Pass when a horizon has fragic soil properties:

- rupture_resistance class \>= "firm" (firm, very firm, extremely firm);
  AND

- brittle manner of failure (proxy: not in schema; v0.8 uses
  rupture-resistance as primary indicator).

Plus thickness \>= 15 cm.

## Usage

``` r
fragipan_usda(pedon, max_top_cm = 100)
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
