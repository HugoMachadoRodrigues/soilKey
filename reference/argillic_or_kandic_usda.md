# Argillic-or-Kandic helper (USDA, used in Spodosols Subgroups)

Pass when EITHER an argillic OR a kandic horizon is present within
`max_top_cm`.

## Usage

``` r
argillic_or_kandic_usda(pedon, max_top_cm = 200, min_bs = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_top_cm:

  Default 200.

- min_bs:

  Optional minimum BS for "Alfic" subgroups.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
