# Dolomitic material (WRB 2022 Ch 3.3.5): \\= 2% Mg-rich carbonate, CaCO3/MgCO3 \< 1.5. v0.3.3: detects via designation pattern `kdo|do|magn` as proxy when ratio data missing.

Dolomitic material (WRB 2022 Ch 3.3.5): \\= 2% Mg-rich carbonate,
CaCO3/MgCO3 \< 1.5. v0.3.3: detects via designation pattern
`kdo|do|magn` as proxy when ratio data missing.

## Usage

``` r
dolomitic_material(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
