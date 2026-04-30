# Folists Suborder qualifier (KST 13ed, Ch 10, p 200)

Histosols saturated for less than 30 days per year (and not artificially
drained). Implementation: pass when there is no aquic conditions and no
glei designation in the upper 50 cm.

## Usage

``` r
folist_qualifying_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
