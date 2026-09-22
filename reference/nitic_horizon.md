# Nitic horizon (WRB 2022)

Tests for the nitic horizon: a clay-rich (\>= 30%), Fe-rich (DCB Fe \>=
4%) subsurface horizon at least 30 cm thick. Diagnostic of Nitisols. WRB
2022 additionally requires polyhedral / nutty structure with shiny ped
surfaces and a gradual (non-abrupt) clay decrease with depth.

## Usage

``` r
nitic_horizon(
  pedon,
  min_clay = 30,
  min_fe_dcb = 4,
  min_thickness = 30,
  max_clay_drop_pct = 8,
  max_decrease_depth = 50
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_clay:

  Minimum clay % (default 30).

- min_fe_dcb:

  Minimum DCB-extractable Fe % (default 4).

- min_thickness:

  Minimum thickness in cm (default 30).

- max_clay_drop_pct:

  Maximum clay drop (percentage points) between adjacent layers within
  `max_decrease_depth` before failing the gradual-decrease test (default
  8).

- max_decrease_depth:

  Depth window (cm) for the gradual-decrease check (default 50).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Required (AND-combined) sub-tests:

- Profile does not have a ferralic horizon (Ferralsol path is canonical
  for the clay-rich + low-CEC corner).

- clay % \>= `min_clay`.

- fe_dcb_pct \>= `min_fe_dcb`.

- thickness \>= `min_thickness`.

Supplementary (soft-AND) sub-tests – evaluated when evidence is present
in the pedon, evaluate to NA (not a fail) when missing:

- structure_type matches polyhedral / nutty / (sub)angular blocky.

- slickensides / shiny ped surfaces present (proxy for WRB's "shiny ped
  surfaces").

- clay does not decrease abruptly between adjacent layers within 50 cm
  of the surface (gradual-decrease pattern; drop \> 8 percentage points
  fails).

Supplementary tests fail (return passed = FALSE) only when evidence
actively contradicts the criterion; missing evidence is permissive.

## References

IUSS Working Group WRB (2022), Chapter 3, Nitic horizon.
