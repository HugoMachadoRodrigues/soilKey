# Lithic contact within X cm of the surface (USDA Subgroup helper)

Pass when a horizon designation matches an R contact within
`max_top_cm`. Default 50 cm (Subgroup-level depth bound). For Gelisols
organic soil materials (Folistels), the depth is 50 cm; for
Fibristels/Hemistels/Sapristels and other Gelisols, it is 100 cm (KST
13ed, p 46).

## Usage

``` r
lithic_contact_usda(pedon, max_top_cm = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 50 cm.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45; Ch. 9 various
Subgroups.
