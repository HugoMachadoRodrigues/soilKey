# USDA family: soil temperature regime (KST Ch. 16)

Uses `pedon$site$soil_temperature_regime` when supplied (high
confidence). Otherwise, when `infer = TRUE`, estimates the mean annual
soil temperature from latitude and elevation via a crude lapse-rate
model and assigns frigid/mesic/thermic/hyperthermic, with an `iso-`
prefix in the low-seasonality tropics (\|lat\| \< 23). Inferred values
set `evidence$inferred = TRUE` and record the missing site field.

## Usage

``` r
family_temperature_regime_usda(pedon, infer = TRUE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- infer:

  Infer from lat/elevation when the site field is absent.

## Value

A
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md).

## References

Soil Survey Staff (2022), KST 13th ed., Ch. 16.
