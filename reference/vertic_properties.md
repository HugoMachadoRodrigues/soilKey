# Vertic properties (WRB 2022)

Tests whether any horizon shows vertic properties – shrink-swell clay
behaviour evidenced by slickensides, wedge-shaped peds, and deep cracks.
Diagnostic for Vertisols.

## Usage

``` r
vertic_properties(
  pedon,
  min_clay = 30,
  min_thickness = 25,
  slickenside_levels = c("common", "many", "continuous")
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_clay:

  Minimum clay percent (default 30, per WRB 2022).

- min_thickness:

  Minimum thickness (cm) of the vertic layer (default 25 per WRB 2022 Ch
  3.2.x).

- slickenside_levels:

  Vector of `slickensides` values accepted as evidence (default
  `c("common", "many", "continuous")`).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- [`test_clay_above`](https://hugomachadorodrigues.github.io/soilKey/reference/test_clay_above.md)
  – clay \>= 30%

- [`test_slickensides_present`](https://hugomachadorodrigues.github.io/soilKey/reference/test_slickensides_present.md)
  – slickensides at or above the "common" level

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – combined vertic layer thickness \>= 25 cm (v0.3.1 added per WRB
  2022)

v0.3.1: thickness gate added. Limitations remaining: WRB also accepts
deep cracks (\>= 1 cm wide extending from the surface to \>= 50 cm
depth, when soil is dry) and wedge-shaped peds as alternative evidence;
this implementation requires clay + slickensides. The "after mixing of
upper 18 cm" clause from WRB is still deferred.

## References

IUSS Working Group WRB (2022), Chapter 3.2 – Vertic properties.
