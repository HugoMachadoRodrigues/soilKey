# Acrisol RSG diagnostic (WRB 2022)

Tests whether a profile satisfies the Acrisol RSG criteria: an argic
horizon with low-activity clay (CEC \< 24 cmol_c/kg clay) AND low base
saturation (BS \< 50%) within at least one argic layer.

## Usage

``` r
acrisol(pedon, max_cec = 24, max_bs = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_cec:

  Maximum CEC per kg clay (default 24).

- max_bs:

  Maximum base saturation % (default 50).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Acrisols.
