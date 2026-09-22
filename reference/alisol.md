# Alisol RSG diagnostic (WRB 2022)

argic + CEC \>= 24 cmol_c/kg clay + Al saturation \>= 50%.

## Usage

``` r
alisol(pedon, min_cec = 24, min_al_sat = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cec:

  Minimum CEC per kg clay (default 24).

- min_al_sat:

  Minimum Al saturation % (default 50).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Alisols.
