# Plinthic horizon (WRB 2022)

Tests whether any horizon meets the plinthic horizon criteria. Plinthite
is Fe-rich material that hardens irreversibly on repeated wetting and
drying; the plinthic horizon is the diagnostic of Plinthosols.

## Usage

``` r
plinthic(pedon, min_thickness = 15, min_plinthite_pct = 15)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness in cm (default 15).

- min_plinthite_pct:

  Minimum volume % plinthite (default 15).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-tests:

- [`test_plinthite_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_plinthite_concentration.md)
  – plinthite volume % \>= 15

- [`test_minimum_thickness`](https://hugomachadorodrigues.github.io/soilKey/reference/test_minimum_thickness.md)
  – thickness \>= 15 cm

v0.2 limitations: WRB 2022 also accepts profiles with \>= 40% red
Fe-rich mottles as alternative criterion – not yet wired. The
"irreversibly hardens" criterion is conceptual and requires field
observation; v0.2 takes `plinthite_pct` as already representing true
plinthite (as opposed to soft mottles).

## References

IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon.
