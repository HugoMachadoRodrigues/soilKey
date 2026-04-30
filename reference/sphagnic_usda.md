# Sphagnic Subgroup helper (Histels Fibristels)

Pass when 75 percent or more of the fibric soil materials are derived
from Sphagnum to a depth of 50 cm or to a contact, whichever is
shallower (KST 13ed, p 190).

## Usage

``` r
sphagnic_usda(pedon, max_top_cm = 50, min_sphagnum_pct = 75)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 50.

- min_sphagnum_pct:

  Default 75.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation uses `fiber_content_rubbed_pct >= 75` as a proxy. A more
specific Sphagnum-fraction column is deferred.

## References

Soil Survey Staff (2022), KST 13ed, Ch. 9, p 190.
