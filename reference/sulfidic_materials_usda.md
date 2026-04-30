# Sulfidic materials helper (USDA, KST 13ed Ch 3, p 49)

Pass when sulfidic materials (soft, dark, sulfide-rich) are present
within `max_top_cm`. Proxy: sulfidic_s_pct \>= 0.75 AND in a layer \>=
15 cm thick.

## Usage

``` r
sulfidic_materials_usda(pedon, max_top_cm = 100, min_thickness_cm = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 100.

- min_thickness_cm:

  Default 15.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
