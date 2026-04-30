# Ruptic-Histic Subgroup helper

Pass when surface organic soil materials are discontinuous OR change in
thickness fourfold or more within a pedon. v0.8 approximation: returns
FALSE – requires multi-pedon transect data not in the single-pedon
schema. Refinement deferred.

## Usage

``` r
ruptic_histic_subgroup_usda(pedon)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).
