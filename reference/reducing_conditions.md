# Reducing conditions (WRB 2022 Ch 3.2.10) – per-pedon test wrapping `test_reducing_conditions`.

Reducing conditions (WRB 2022 Ch 3.2.10) – per-pedon test wrapping
`test_reducing_conditions`.

## Usage

``` r
reducing_conditions(pedon, min_redox_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_redox_pct:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
