# USDA family: reaction class (KST Ch. 17)

Conservative: returns "calcareous" when carbonates are present through
the control section; otherwise NULL (acid/nonacid terms apply only to
specific families and are not emitted by default).

## Usage

``` r
family_reaction_usda(pedon, min_cm = 25, max_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cm, max_cm:

  Control-section depth window (cm).

## Value

A
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 17.
