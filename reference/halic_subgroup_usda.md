# Halic Subgroup helper (Haplosaprists)

Pass when EC \>= 30 dS/m through a 30+ cm layer for 6+ months (KST 13ed,
Ch 10). v0.8 proxy: any layer with ec_dS_m \>= 30.

## Usage

``` r
halic_subgroup_usda(pedon, min_ec = 30, min_thickness_cm = 30)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ec:

  Default 30.

- min_thickness_cm:

  Default 30.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
