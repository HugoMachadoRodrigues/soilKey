# Floatic qualifier (fc): Histosol that floats on water.

WRB 2022 Ch 5 / Ch 4 Histosols (p 96): organic material with very low
bulk density (\< 0.1 g/cm3 dry, OR \< 0.4 g/cm3 in fully saturated
state) that floats on water. Practical proxy: oc_pct \>= 12 (Histic
threshold) AND bulk_density_g_cm3 \<= 0.4 in any layer of the upper 100
cm.

## Usage

``` r
qual_floatic(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
