# Histic horizon (WRB 2022)

A surface (or near-surface, after drainage) horizon of organic material;
diagnostic of Histosols. Two alternative qualifying paths per WRB 2022:

- **Contiguous**: a single layer of organic material (OC % \>= `min_oc`)
  reaching the surface and at least `min_thickness` cm thick (default 10
  cm).

- **Cumulative**: organic material totalling `cumulative_min_cm` cm
  (default 40) within the upper `cumulative_max_depth_cm` (default 80).
  Relevant for folic / mossy Histosols on slopes.

Either path qualifies. The "after drainage" qualifier (recently drained
organic soils) is treated as implicit since the same OC and thickness
criteria apply.

## Usage

``` r
histic_horizon(
  pedon,
  min_thickness = 10,
  min_oc = 12,
  surface_top_cm = 0,
  cumulative_min_cm = 40,
  cumulative_max_depth_cm = 80
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm) for the contiguous path (default 10).

- min_oc:

  Minimum organic carbon % (default 12, WRB 2022; equivalent to `>= 20%`
  organic matter).

- surface_top_cm:

  Maximum top depth (cm) for a layer to be considered "surface-related"
  in the contiguous path (default 0).

- cumulative_min_cm:

  Minimum cumulative thickness (cm) for the cumulative path (default
  40).

- cumulative_max_depth_cm:

  Depth window (cm) for the cumulative path (default 80).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 3, Histic horizon and organic
material.
