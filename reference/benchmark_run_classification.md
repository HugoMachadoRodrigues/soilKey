# Run a benchmark across one of the loaded pedon lists

Classifies each pedon in `pedons` against the named system, compares
against the published reference (e.g. `site$reference_wrb`), and returns
a confusion matrix + top-1 / top-3 accuracy + bootstrap CI on top-1.

## Usage

``` r
benchmark_run_classification(
  pedons,
  system = c("wrb2022", "sibcs", "usda"),
  level = c("order", "subgroup"),
  boot_n = 1000L
)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects (output of one of the `load_*` functions).

- system:

  One of `"wrb2022"`, `"sibcs"`, `"usda"`.

- level:

  Granularity of the comparison: `"order"` for the top-level RSG / Ordem
  / Order; `"subgroup"` for the full assigned name. Default `"order"`.

- boot_n:

  Bootstrap replicates for CI (default 1000).

## Value

A list with elements `accuracy_top1`, `accuracy_ci`, `confusion`, and
`per_pedon` (one row per pedon with predicted vs reference).
