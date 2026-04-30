# Retic properties (WRB 2022)

Tests whether any horizon designation indicates retic features (glossic
tongues of bleached material penetrating into a clay- enriched horizon).
v0.3 detects these via designation pattern matching
`"glossic|retic|albeluvic"` (case-insensitive). Diagnostic of Retisols.

## Usage

``` r
retic_properties(pedon, pattern = "glossic|retic|albeluvic")
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- pattern:

  Regex (default `"glossic|retic|albeluvic"`).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Retisols.
