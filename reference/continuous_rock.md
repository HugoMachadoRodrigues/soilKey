# Continuous rock (WRB 2022 Ch 3.2.5)

Consolidated material below the soil. v0.3.3: detects via designation
`R` or `Cr` on the lowermost (or any) layer.

## Usage

``` r
continuous_rock(pedon)
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
