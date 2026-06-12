# Solimovic material (WRB 2022 Ch 3.3.17): hetero genous mass-movement material on slopes / footslopes (formerly "colluvic"). v0.3.3: detects via `rock_origin == "colluvial"` OR `layer_origin == "solimovic"`.

Solimovic material (WRB 2022 Ch 3.3.17): hetero genous mass-movement
material on slopes / footslopes (formerly "colluvic"). v0.3.3: detects
via `rock_origin == "colluvial"` OR `layer_origin == "solimovic"`.

## Usage

``` r
solimovic_material(pedon)
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
