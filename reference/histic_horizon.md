# Histic horizon (WRB 2022)

A surface horizon (or near-surface, after drainage) of organic material
\>= 10 cm thick; diagnostic of Histosols.

## Usage

``` r
histic_horizon(pedon, min_thickness = 10, min_oc = 12, surface_top_cm = 0)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm) of contiguous organic material from the surface
  (default 10).

- min_oc:

  Minimum organic carbon % (default 12, WRB 2022; equivalent to `>= 20%`
  organic matter).

- surface_top_cm:

  Maximum top depth (cm) for a layer to be considered "surface-related"
  (default 0; the histic horizon must reach the surface, possibly after
  drainage).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- [`test_oc_above`](https://hugomachadorodrigues.github.io/soilKey/reference/test_oc_above.md)
  – OC % \>= 12

- [`test_top_at_or_above`](https://hugomachadorodrigues.github.io/soilKey/reference/test_top_at_or_above.md)
  – top_cm \<= 0

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 10 cm

v0.3 limitations: WRB 2022 also accepts a 40 cm cumulative
organic-material thickness within the upper 80 cm (relevant for folic /
mossy Histosols on slopes); v0.4 will add the cumulative variant. The
"after drainage" qualifier (recently-drained organic soils) is also
deferred.

## References

IUSS Working Group WRB (2022), Chapter 3, Histic horizon and organic
material.
