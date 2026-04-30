# Hydric Subgroup helper (Histosols Cryofibrists / Sphagnofibrists / etc.)

Pass when there is a "layer of water" within the control section.
Detected via designation containing "W" (water layer) or
`layer_origin == "water"`.

## Usage

``` r
hydric_subgroup_usda(pedon, max_top_cm = 130)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 130.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
