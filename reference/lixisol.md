# Lixisol RSG diagnostic (WRB 2022)

argic + CEC \< 24 cmol_c/kg clay + BS \>= 50%.

## Usage

``` r
lixisol(pedon, max_cec = 24, min_bs = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_cec:

  Maximum CEC per kg clay (default 24).

- min_bs:

  Minimum base saturation % (default 50).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Lixisols.
