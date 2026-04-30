# Fluvic material (WRB 2022)

Tests whether the profile shows fluvic material features: alternating
textures across consecutive horizons within the upper 100 cm AND an
irregular (non-monotone) organic carbon pattern with depth. Diagnostic
of Fluvisols.

## Usage

``` r
fluvic_material(pedon, max_top_cm = 100, min_clay_swing = 8)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Maximum top depth (cm) considered (default 100).

- min_clay_swing:

  Minimum absolute clay-percent change between consecutive layers
  required to count as alternation (default 8 percentage points).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Sub-test:
[`test_fluvic_stratification`](https://hugomachadorodrigues.github.io/soilKey/reference/test_fluvic_stratification.md).

v0.3 limitations: WRB 2022 fluvic material also requires age (typically
\<100 years for sediment freshness), which v0.3 does not check (no
temporal fields in the schema). The stratification proxy is conservative
– truly heterogeneous floodplain profiles with dramatic texture swings
will pass; subtle alluvial sequences may miss. v0.4 will refine.

## References

IUSS Working Group WRB (2022), Chapter 3, Fluvic material.
