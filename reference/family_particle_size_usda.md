# USDA family: particle-size class (KST Ch. 17)

Weighted average over the family control section (25–100 cm by default):
fragmental / \*-skeletal (\>= 35 percent rock fragments) / sandy /
clayey (fine, very-fine) / loamy / silty (coarse-/fine-).

## Usage

``` r
family_particle_size_usda(pedon, min_cm = 25, max_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cm, max_cm:

  Control-section depth window (cm).

## Value

A
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
(`value` is the class or NULL).

## References

Soil Survey Staff (2022), Keys to Soil Taxonomy 13th ed., Ch. 17.
