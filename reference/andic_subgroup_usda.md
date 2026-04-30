# Andic Subgroup helper (USDA, KST 13ed)

Pass when, throughout one or more horizons with total thickness \>= 18
cm within 75 cm of the surface:

- bulk_density_g_cm3 \<= 1.0 (at 33 kPa); AND

- Al + 0.5 \* Fe (oxalate-extractable) \> 1.0 percent.

KST 13ed, p 117 (Andisols core, applies to subgroup criteria too).

## Usage

``` r
andic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
