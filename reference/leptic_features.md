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
leptic_features(pedon, max_depth = 25, min_coarse_pct = NULL, engine = NULL)
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
  90 in soilkey engine, 50 in aqp engine; `NULL` picks a default per
  engine).

- engine:

  One of `"soilkey"` (default; strict 90\\ cfvo threshold) or `"aqp"`
  (LUCAS-friendly relaxed 50\\ requiring positive evidence of rock
  contact – v0.9.66 tightening). The thin-topsoil path fires only when a
  horizon ending within `max_depth` also satisfies *at least one*
  of: (a) designation contains "R" (e.g.\\ AR, BR, Cr, R, Rk), (b)
  `coarse_fragments_pct >= 30` (gravelly), or (c) a deeper horizon is
  R/Cr-designated. Users with a strong external prior (e.g.\\ a
  parent-material survey that documents rock \< 25 cm but did not record
  it in the horizon table) can opt back into the original v0.9.65 loose
  behaviour with `options(soilKey.leptic_assume_rock_below = TRUE)`.
  `NULL` (the default) reads `getOption("soilKey.diagnostic_engine")`.

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md).

## References

IUSS Working Group WRB (2022), Chapter 5, Leptosols.
