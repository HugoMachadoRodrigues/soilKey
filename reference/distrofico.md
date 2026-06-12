# Solo distrofico (SiBCS Cap 1, p 30)

Negacao operacional de
[`eutrofico`](https://hugomachadorodrigues.github.io/soilKey/reference/eutrofico.md):
V \< 50% no horizonte diagnostico subsuperficial.

## Usage

``` r
distrofico(pedon, max_v = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_v:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
