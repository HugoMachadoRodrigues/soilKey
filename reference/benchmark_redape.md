# Benchmark soilKey SiBCS predictions against the Redape gold standard

Runs
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
on each pedon and compares against the curator-validated reference label
(Order / Suborder / Great Group / Subgroup). Returns per-level accuracy
and the confusion matrix at the requested granularity.

## Usage

``` r
benchmark_redape(
  pedons,
  level = c("order", "subordem", "gde_grupo", "subgrupo"),
  verbose = TRUE
)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects (typically from
  [`load_redape_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_redape_pedons.md)).

- level:

  One of `"order"` (default), `"subordem"`, `"gde_grupo"`, or
  `"subgrupo"`.

- verbose:

  Print progress (default `TRUE`).

## Value

A list with `accuracy`, `n_compared`, `confusion`, `per_class_recall`,
and the per-pedon `predictions` table. `predictions` now also includes
columns `ref_norm` and `pred_norm` – the canonical comparison keys – for
downstream auditing.

## v0.9.81 level-aware comparison

Earlier versions accepted the `level` argument but always used
`rsg_or_order` for the prediction and the order field for the reference,
so all four levels reported identical accuracy. v0.9.81 reads the
level-specific slots from `res$trace` (subordem, grande_grupo, subgrupo)
and concatenates the matching reference fields, applying SiBCS-aware
Portuguese pluralisation so the comparison key matches the predictor's
plural Title Case form.
