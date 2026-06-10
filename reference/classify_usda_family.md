# Classify the USDA family (5th level) of a pedon

Runs the applicable family-modifier dimensions and returns them as a
named list of
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
objects (multi-label; each dimension is orthogonal). Mirrors
[`classify_sibcs_familia`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs_familia.md).

## Usage

``` r
classify_usda_family(
  pedon,
  order_code = NULL,
  subgroup_code = NULL,
  infer_temperature = TRUE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- order_code:

  Optional USDA order code (selects applicable dimensions).

- subgroup_code:

  Optional subgroup code (reserved for refinements).

- infer_temperature:

  Passed to
  [`family_temperature_regime_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/family_temperature_regime_usda.md).

## Value

Named list of
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
objects.

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 16–17.

## See also

[`family_label_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/family_label_usda.md),
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).
