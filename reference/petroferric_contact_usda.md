# Petroferric contact helper (USDA, KST 13ed Ch 3, p 48)

Ironstone-like layer with \>50% Fe oxides, indurated. v0.8 proxy:
`cementation_class` in {strongly, indurated} AND `plinthite_pct >= 50`
(Fe-rich) AND `coarse_fragments_pct >= 50`.

## Usage

``` r
petroferric_contact_usda(pedon, max_top_cm = 125)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 125.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
