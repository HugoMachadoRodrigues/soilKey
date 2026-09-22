# Kastanozem RSG diagnostic (WRB 2022)

Tests whether a profile satisfies the Kastanozem RSG criteria: a mollic
horizon plus secondary carbonates plus NOT-Chernozem colour (chroma
(moist) \> 2 in the upper 20 cm).

## Usage

``` r
kastanozem(pedon, max_chroma_upper = 2)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_chroma_upper:

  Maximum moist chroma to qualify as Chernozem (default 2). Kastanozem
  requires the upper-20-cm chroma to EXCEED this value.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Kastanozems.
