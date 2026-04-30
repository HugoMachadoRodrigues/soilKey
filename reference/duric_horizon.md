# Duric horizon (WRB 2022)

Tests for \>= 10% volume of duripan nodules (Si-cemented) within a
horizon at least 10 cm thick. Diagnostic of Durisols.

## Usage

``` r
duric_horizon(pedon, min_thickness = 10, min_duripan_pct = 10)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Minimum thickness (cm; default 10 per WRB 2022).

- min_duripan_pct:

  Minimum duripan volume % (default 10 per WRB 2022).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

v0.3.1: thresholds aligned with WRB 2022 Ch 3.1.7 (10%, 10 cm) –
previous v0.3 used 15%/15 cm. Petroduric (cemented continuous duripan)
detection still deferred and will be added in v0.4.

## References

IUSS Working Group WRB (2022), Chapter 3.1.7 – Duric horizon (p. 41).
