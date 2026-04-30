# Histosols Order qualifier (USDA, KST 13ed, Ch 2, p 7)

Organic soils not meeting the Gelisols requirements (no permafrost
within 100 cm). The KST defines Histosols as soils with organic soil
materials that meet specific thickness/depth criteria (Ch 2, pp 7-9; see
also Ch 3 organic soil materials).

## Usage

``` r
histosol_qualifying_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation: pass when cumulative organic-layer thickness
(designation H or O) within 0-100 cm \>= 40 cm AND no permafrost within
100 cm.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 2, pp 7-9.
