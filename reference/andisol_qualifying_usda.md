# Andisol Order qualifier (USDA, KST 13ed Ch 3, p 7)

Andisols have andic soil properties in 60%+ of the thickness between the
surface and either:

- a depth of 60 cm; or

- a densic, lithic, or paralithic contact, a duripan, or a petrocalcic
  horizon (whichever is shallower).

v0.8.6 implementation: pass when total thickness of layers with
andic_soil_properties is \>= 0.6 \* (depth from surface to 60 cm).

## Usage

``` r
andisol_qualifying_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

Soil Survey Staff (2022), KST 13ed, Ch. 6, p 117.
