# Sulfuric horizon helper (USDA, KST 13ed Ch 3)

Pass when sulfidic_s_pct present in any horizon within `max_top_cm`
(proxy: KST sulfuric horizon requires pH \< 4.0 OR sulfidic materials
AND certain mottle colors; this v0.8 uses sulfidic_s_pct \>= 0.75 as
proxy).

## Usage

``` r
sulfuric_horizon_usda(pedon, max_top_cm = 100, min_s_pct = 0.75)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 100.

- min_s_pct:

  Default 0.75.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
