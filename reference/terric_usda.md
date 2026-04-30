# Terric Subgroup helper (Histels)

Pass when a layer of mineral soil material 30 cm or more thick occurs
within 100 cm of the soil surface (KST 13ed, p 190).

## Usage

``` r
terric_usda(pedon, min_thickness_cm = 30, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness_cm:

  Default 30.

- max_top_cm:

  Default 100.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 9.
