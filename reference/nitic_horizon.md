# Nitic horizon (WRB 2022)

Tests for the nitic horizon: a clay-rich (\>= 30%), Fe-rich subsurface
horizon at least 30 cm thick. Diagnostic of Nitisols.

## Usage

``` r
nitic_horizon(pedon, min_clay = 30, min_fe_dcb = 4, min_thickness = 30)
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

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.3 simplification: WRB 2022 also requires polyhedral / nutty structure
with shiny ped surfaces and a gradual clay decrease with depth (no clay
maximum at the top of the B). v0.4 will add the structural /
depth-pattern tests.

## References

IUSS Working Group WRB (2022), Chapter 3, Nitic horizon.
