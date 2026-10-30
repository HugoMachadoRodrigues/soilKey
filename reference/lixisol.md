# Lixisol RSG diagnostic (WRB 2022)

The Lixisols of the WRB 2022 key (Ch 4, p. 120): an argic horizon
starting \\\le\\ 100 cm from the mineral soil surface and a CEC of \< 24
cmolc/kg clay in some subhorizon of the argic horizon within 150 cm. The
key reaches them after the Acrisols.

## Usage

``` r
lixisol(pedon, max_cec = 24, min_bs = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_cec:

  CEC per kg clay below which the clay is of low activity (default 24).

- min_bs:

  Unused since v0.9.310, kept so that existing calls still run.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 4, Lixisols (p. 120).
