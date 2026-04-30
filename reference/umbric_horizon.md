# Umbric horizon (WRB 2022)

Tests for the umbric horizon – a thick, dark, organic-rich surface
horizon like mollic, but with low base saturation (\< 50%). Diagnostic
of Umbrisols.

## Usage

``` r
umbric_horizon(
  pedon,
  min_thickness = 20,
  min_oc = 0.6,
  max_bs = 50,
  surface_top_cm = 5
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm; default 20).

- min_oc:

  Minimum SOC % (default 0.6).

- max_bs:

  Maximum base saturation % (default 50; profile must be BELOW this).

- surface_top_cm:

  Maximum top_cm for surface-related layers (default 5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

Implementation reuses every mollic sub-test except the BS test, which is
inverted via
[`test_bs_below`](https://hugomachadorodrigues.github.io/soilKey/reference/test_bs_below.md).

## References

IUSS Working Group WRB (2022), Chapter 3, Umbric horizon.
