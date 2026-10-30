# Alisol RSG diagnostic (WRB 2022)

The Alisols of the WRB 2022 key (Ch 4, p. 121): an argic horizon
starting \\\le\\ 100 cm from the mineral soil surface and exchangeable
Al \> exchangeable (Ca+Mg+K+Na) in half or more of 50-100 cm (or of the
lower half of the mineral soil above a limiting layer starting \\\le\\
100 cm). The key reaches them after the Acrisols and Lixisols.

## Usage

``` r
alisol(pedon, min_cec = 24, min_al_sat = 50)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_cec, min_al_sat:

  Unused since v0.9.310, kept so that existing calls still run.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 4, Alisols (p. 121).
