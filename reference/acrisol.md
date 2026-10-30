# Acrisol RSG diagnostic (WRB 2022)

The Acrisols of the WRB 2022 key (Ch 4, p. 119): an argic horizon
starting \\\le\\ 100 cm from the mineral soil surface; a CEC (1 M
NH4OAc, pH 7) of \< 24 cmolc/kg clay in some subhorizon of the argic
horizon within 150 cm of the mineral soil surface; and exchangeable Al
\> exchangeable (Ca+Mg+K+Na) in half or more of 50-100 cm from the
mineral soil surface, or of the lower half of the mineral soil above a
limiting layer starting \\\le\\ 100 cm, whichever is shallower. Where
the cations are not available, the pH is read by WRB 2022 Annex 2, Table
9.3.

## Usage

``` r
acrisol(pedon, max_cec = 24, max_bs = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_cec:

  CEC per kg clay below which the clay is of low activity (default 24).

- max_bs:

  Unused since v0.9.310 (WRB 2022 does not use the base saturation
  here), kept so that existing calls still run.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 4, Acrisols (p. 119).
