# Planic features (WRB 2022)

Tests whether the profile shows an abrupt textural change between
adjacent horizons (clay-doubling within 7.5 cm vertical distance,
typically at the E/Bt boundary). Diagnostic of Planosols.

## Usage

``` r
planic_features(pedon, min_ratio = 2, require_abrupt_boundary = TRUE)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_ratio:

  Minimum clay ratio (default 2.0).

- require_abrupt_boundary:

  If TRUE (default), the upper horizon must have `boundary_distinctness`
  matching "abrupt".

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Planosols.
