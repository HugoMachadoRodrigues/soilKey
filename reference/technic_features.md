# Technic features (WRB 2022)

Tests whether the profile contains \>= 20% by volume of artefacts (any
human-made or human-altered material) within the upper 100 cm.
Diagnostic of Technosols.

## Usage

``` r
technic_features(pedon, min_pct = 20, max_top_cm = 100)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_pct:

  Minimum artefact percent (default 20).

- max_top_cm:

  Maximum top depth (cm) (default 100).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-test:
[`test_artefacts_concentration`](https://hugomachadorodrigues.github.io/soilKey/reference/test_artefacts_concentration.md).

v0.3 limitations: WRB 2022 also accepts a continuous geomembrane within
100 cm OR technic hard material (concrete, asphalt, mine spoil) covering
\>= 95% within 5 cm of the surface as alternative qualifying conditions.
v0.4 will add these alternate paths.

## References

IUSS Working Group WRB (2022), Chapter 5, Technosols.
