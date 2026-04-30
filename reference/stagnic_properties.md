# Stagnic properties (WRB 2022)

Tests for redoximorphic features driven by perched water. Distinct from
gleyic (groundwater): stagnic features appear in upper layers AND redox
decreases substantially with depth (the perched layer sits above a
slowly permeable subsoil that itself is not saturated).

## Usage

``` r
stagnic_properties(
  pedon,
  max_top_cm = 100,
  min_redox_pct = 5,
  decay_factor = 3
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) of candidate shallow layers (default 100).

- min_redox_pct:

  Minimum redox feature percent in the shallow layer (default 5).

- decay_factor:

  Required factor of redox decrease with depth (default 3, i.e., deeper
  redox \< shallow / 3).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 3, Stagnic properties.
