# USDA family: soil depth class for shallow soils (KST Ch. 17)

Detects the shallowest lithic/paralithic/densic/petro contact from
horizon designations (R / Cr / Cd / m suffix) and emits "very-shallow"
(\< 25 cm) or "shallow" (\< 50 cm); deeper soils get no depth term
(NULL).

## Usage

``` r
family_depth_class_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 17.
