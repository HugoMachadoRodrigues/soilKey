# Protocalcic properties (WRB 2022 Ch 3.2.8)

Accumulations of secondary carbonates, visible when moist, that occupy
\\= 5% of the exposed area with masses, nodules, concretions or
filaments, or cover \\= 10% of the surfaces of aggregates, biopore walls
or the undersides of coarse fragments (WRB 2022 3.2.8, p. 78). The test
reads the recorded abundance of visible secondary carbonates,
`secondary_carbonates_pct` (or the visual `carbonate_soft_pct` plus
`carbonate_hard_pct`). Laboratory CaCO3 is not evidence of secondary
carbonates: with no abundance recorded, a layer with no CaCO3
(`caco3_pct` 0, or no effervescence) has no protocalcic properties and
any other layer is undecided. A `k` suffix with no abundance is
undecided too, unless `options(soilKey.morphological_inference = TRUE)`,
where it is accepted as morphological evidence.

## Usage

``` r
protocalcic_properties(pedon, min_secondary_pct = 5)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_secondary_pct:

  Minimum visible secondary carbonates, % of the exposed area (default
  5).

## Value

A
[`DiagnosticResult`](https://hugomachadorodrigues.github.io/soilKey/reference/DiagnosticResult.md)
recording whether the diagnostic is present, the qualifying layers, and
the supporting evidence; the per-layer status (TRUE / FALSE / NA) is in
`evidence$layer_status`.
