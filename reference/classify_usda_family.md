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
  infer_temperature = FALSE,
  great_group = NULL,
  subgroup = NULL,
  suggest_mineralogy = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- order_code:

  Optional USDA order code (e.g. `"AF"`).

- subgroup_code:

  Optional subgroup code (reserved for refinements).

- infer_temperature:

  Opt-in (default `FALSE` since v0.9.415): when
  `site$soil_temperature_regime` is not recorded, suggest a temperature
  class from latitude and elevation. Off, the family has no temperature
  class until the regime is recorded.

- great_group, subgroup:

  Optional great group and subgroup names (e.g. `"Hapludalfs"`,
  `"Arenic Hapludalfs"`). With `order_code` they select the control
  section and the particle-size classes of KST 13th ed., ch. 17 (since
  v0.9.431);
  [`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
  supplies the taxa it assigned.

- suggest_mineralogy:

  Opt-in (default `FALSE`, v0.9.432): when the mineralogy class of KST
  13th ed., pp. 363-366, is undecided, suggest one from stand-ins (the
  Kr and Ki of the sulfuric attack, CEC / clay, CaCO3, a
  quartz-dominated `sand_mineralogy`). The suggestion sets
  `evidence$suggestion = TRUE` and keeps the missing columns;
  [`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
  then warns and lowers the evidence grade to "C".

## Value

Named list of
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md)
objects.

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 16–17.

## See also

`family_label_usda`,
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).
