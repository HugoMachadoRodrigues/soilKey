# Hydragric horizon (WRB 2022): subsoil hydric horizon under anthraquic. v0.3.3 detects via designation pattern `Bg|Brg` immediately below an anthraquic-like topsoil.

Hydragric horizon (WRB 2022): subsoil hydric horizon under anthraquic.
v0.3.3 detects via designation pattern `Bg|Brg` immediately below an
anthraquic-like topsoil.

## Usage

``` r
hydragric(pedon, min_thickness = 20)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_thickness:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
