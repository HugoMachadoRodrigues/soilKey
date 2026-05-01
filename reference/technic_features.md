# Technic features (WRB 2022)

Tests for any of three WRB 2022 alternative qualifying conditions for
Technosols:

1.  Artefacts \>= `artefacts_min_pct` (default 20%) by volume within the
    upper `max_top_cm` (default 100 cm).

2.  A continuous geomembrane (`geomembrane_present == TRUE`) within the
    upper 100 cm.

3.  Technic hard material (concrete, asphalt, mine spoil) with
    `technic_hardmaterial_pct >= hardmaterial_min_pct` (default 95%) at
    the surface (top_cm \<= `hardmaterial_max_top_cm`, default 5).

Either path qualifies.

## Usage

``` r
technic_features(
  pedon,
  artefacts_min_pct = 20,
  max_top_cm = 100,
  hardmaterial_min_pct = 95,
  hardmaterial_max_top_cm = 5
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- artefacts_min_pct:

  Minimum artefact percent (default 20).

- max_top_cm:

  Maximum top depth (cm) for the artefact and geomembrane paths (default
  100).

- hardmaterial_min_pct:

  Minimum hard-material coverage (%) for the technic-hard-material path
  (default 95).

- hardmaterial_max_top_cm:

  Surface depth window (cm) for the technic-hard-material path (default
  5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Technosols.
