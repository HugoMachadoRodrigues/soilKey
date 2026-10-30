# Luvisol RSG diagnostic (WRB 2022)

The Luvisols of the WRB 2022 key (Ch 4, p. 122): "Other soils having an
argic horizon starting \\\le\\ 100 cm from the mineral soil surface".

## Usage

``` r
luvisol(pedon, min_cec = 24, max_al_sat = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cec, max_al_sat:

  Unused since v0.9.310, kept so that existing calls still run.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 4, Luvisols (p. 122).
