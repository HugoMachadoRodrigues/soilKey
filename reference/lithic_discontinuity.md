# Lithic discontinuity (WRB 2022 Ch 3.2.7)

Significant abrupt change in parent material between two layers. v0.3.3
simplified: detects via large discontinuity in coarse_fragments_pct (\>=
10pp absolute jump) OR rock_origin difference between consecutive
layers.

## Usage

``` r
lithic_discontinuity(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
