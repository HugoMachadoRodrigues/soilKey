# Histels Suborder qualifier (USDA, KST 13ed)

Pass when a Gelisol has organic soil materials that:

- Total \>= 40 cm cumulative thickness within 0-50 cm; OR

- Comprise \>= 80% (by volume) of 0-50 cm.

KST 13ed, Ch 9, p 189 (item AA in Key to Suborders).

## Usage

``` r
histel_qualifying_usda(pedon, min_thickness_cm = 40, max_top_cm = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness_cm:

  Default 40.

- max_top_cm:

  Default 50.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 9, p 189.
