# Limnic material (WRB 2022 Ch 3.3.10): subaquatic deposits (coprogenous earth, diatomaceous earth, marl, gyttja). v0.3.3: detects via `rock_origin %in% c("lacustrine", "marine")` or designation pattern.

Limnic material (WRB 2022 Ch 3.3.10): subaquatic deposits (coprogenous
earth, diatomaceous earth, marl, gyttja). v0.3.3: detects via
`rock_origin %in% c("lacustrine", "marine")` or designation pattern.

## Usage

``` r
limnic_material(pedon)
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
