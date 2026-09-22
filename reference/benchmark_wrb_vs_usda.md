# Benchmark soilKey WRB predictions against a USDA-derived ground truth

Convenience wrapper: applies
[`annotate_wrb_from_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/annotate_wrb_from_usda.md)
to attach derived WRB labels, runs
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
on each pedon, and returns top-1 accuracy + per-RSG recall.

## Usage

``` r
benchmark_wrb_vs_usda(pedons, verbose = TRUE)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects with `site$reference_usda` populated (typically from
  [`load_kssl_pedons_gpkg`](https://hugomachadorodrigues.github.io/soilKey/reference/load_kssl_pedons_gpkg.md)).

- verbose:

  Print progress.

## Value

A list with `accuracy`, `n_compared`, `confusion`, `per_class_recall`.
