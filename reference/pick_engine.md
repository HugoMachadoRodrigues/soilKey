# Choose the best diagnostic engine for a single pedon

Per-pedon heuristic: returns `"aqp"` if the pedon's horizon table has
the morphological richness that makes aqp's canonical NRCS dispatch
reliable, otherwise returns `"soilkey"` (the more permissive hand-coded
path).

## Usage

``` r
pick_engine(pedon, min_score = 3L)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- min_score:

  Integer (1-5). Minimum completeness score for `"aqp"` engine to fire
  (default 3).

## Value

Character: `"aqp"` or `"soilkey"`.

## Heuristic

We score each pedon on a 0-5 morphology-completeness scale; aqp fires
when score \\= `min_score` (default 3). The five axes:

1.  **Designation present** (any layer has a non-blank `designation`,
    e.g. "A1", "Bt2", "Bw").

2.  **Texture quantitative** (any layer has both `clay_pct` and
    `sand_pct` populated).

3.  **Munsell complete** (any layer has all three of
    `munsell_hue_moist`, `munsell_value_moist`, `munsell_chroma_moist`
    populated).

4.  **Structure recorded** (any layer has a non-blank
    `structure_grade`).

5.  **Clay films / argic evidence** (any layer has a non-blank
    `clay_films_amount` or designation pattern matching `Bt`).

## Why this matters

On BDsolos RJ (data-rich), the heuristic recommends aqp for ~99
canonical thresholds). On LUCAS topsoil-only (data-sparse), it
recommends aqp for ~0 clay-films / designation axes are unfilled.
Calling `classify_*(pedon)` routed through the heuristic gives the
correct engine per pedon, recovering both the BDsolos RJ lift AND the
LUCAS robustness.

## See also

[`argic`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md),
[`cambic`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md).
