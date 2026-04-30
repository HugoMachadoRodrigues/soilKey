# Chernozem RSG diagnostic (WRB 2022)

Tests whether a profile satisfies the Chernozem RSG criteria: a mollic
horizon plus secondary carbonates somewhere in the profile, plus chroma
(moist) \<= 2 in at least one layer of the upper 20 cm.

## Usage

``` r
chernozem(pedon, max_chroma_upper = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_chroma_upper:

  Maximum moist chroma in the upper part (default 2, per WRB 2022).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Chernozems.
