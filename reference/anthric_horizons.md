# Anthric horizons (WRB 2022)

Tests for any of five anthropogenic surface horizons recognised by WRB
2022 (hortic, irragric, plaggic, pretic, terric). Diagnostic of
Anthrosols. Two alternative paths qualify:

1.  **Designation**: any layer's designation contains one of
    `hortic|irragric|plaggic|pretic|terric`.

2.  **Property-based**: a surface layer (top_cm \<= 5) at least
    `min_thickness_cm` cm thick (default 20) with elevated dark colour
    (Munsell value moist \<= `max_munsell_value`, default 4) AND
    elevated plant-available P (`p_mehlich3_mg_kg` \>= `min_p_mg_kg`,
    default 50).

Either path qualifies.

## Usage

``` r
anthric_horizons(
  pedon,
  min_thickness_cm = 20,
  min_p_mg_kg = 50,
  max_munsell_value = 4
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness_cm:

  Minimum thickness for the property-based path (default 20).

- min_p_mg_kg:

  Minimum plant-available P (Mehlich 3, mg/kg) for the property-based
  path (default 50).

- max_munsell_value:

  Maximum Munsell value moist for the property-based path (default 4).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Anthrosols.
