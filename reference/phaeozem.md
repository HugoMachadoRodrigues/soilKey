# Phaeozem RSG diagnostic (WRB 2022)

Tests whether a profile satisfies the Phaeozem RSG criteria (WRB 2022 Ch
4, p. 113): a mollic horizon AND a base saturation (1 M NH4OAc, pH 7)
\\= 50% throughout to 100 cm from the mineral soil surface or to a
limiting layer, whichever is shallower. Carbonates are allowed: the
Chernozems and Kastanozems are keyed out before. Where BS is not
recorded, pH is read as in WRB 2022 Annex 2, Table 9.2. A profile not
described down to that depth is undecided.

## Usage

``` r
phaeozem(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 4, Phaeozems (p. 113).
