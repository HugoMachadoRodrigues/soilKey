# Leptic features (WRB 2022)

Tests whether continuous rock or rock-like material occurs within
`max_depth` cm of the surface. Diagnostic of Leptosols.

## Usage

``` r
leptic_features(pedon, max_depth = 25)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth:

  Maximum depth (cm) at which continuous rock must appear (default 25).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## Details

v0.3 implementation infers continuous rock from horizon designation (any
layer with designation matching `"^R"` or `"^Cr"` – standard pedological
codes for hard / continuous rock and weathered rock-like substrate
respectively).

## References

IUSS Working Group WRB (2022), Chapter 5, Leptosols.
