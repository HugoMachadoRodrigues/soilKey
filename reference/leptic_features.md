# Leptic features (WRB 2022)

Tests whether continuous rock or rock-like material occurs within
`max_depth` cm of the surface. Two alternative paths qualify per WRB
2022:

1.  **Designation**: a layer at depth \<= `max_depth` with designation
    matching `"^R"` or `"^Cr"` (continuous rock or weathered rock-like
    substrate).

2.  **Coarse fragments**: a layer at depth \<= `max_depth` with
    coarse_fragments_pct \>= `min_coarse_pct` (default 90% by volume),
    interpreted as rock-dominated even when not R / Cr-designated.

Either path qualifies.

## Usage

``` r
leptic_features(pedon, max_depth = 25, min_coarse_pct = 90)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- max_depth:

  Maximum depth (cm) at which continuous rock or rock-dominated material
  must appear (default 25).

- min_coarse_pct:

  Minimum coarse-fragment percent for the coarse-fragments path (default
  90).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Leptosols.
