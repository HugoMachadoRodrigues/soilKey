# Wassists Suborder qualifier (KST 13ed, Ch 10, p 203)

Histosols having a "field-observable water table 2 cm or more above the
soil surface for more than 21 hours of each day in all years."
Diagnostic for the Wassists suborder.

## Usage

``` r
wassist_qualifying_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation: pass when `site$water_table_cm_above_surface` is
provided and \>= 2 (positive = above surface).
