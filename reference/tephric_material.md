# Tephric material (WRB 2022 Ch 3.3.19): \\= 30% volcanic glass in 0.02-2 mm fraction AND no andic / vitric properties.

Tephric material (WRB 2022 Ch 3.3.19): \\= 30% volcanic glass in 0.02-2
mm fraction AND no andic / vitric properties.

## Usage

``` r
tephric_material(pedon, min_glass = 30)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_glass:

  Numeric threshold or option (see Details).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence.
