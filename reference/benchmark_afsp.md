# Benchmark soilKey WRB predictions against AfSP ground truth

Benchmark soilKey WRB predictions against AfSP ground truth

## Usage

``` r
benchmark_afsp(pedons, verbose = TRUE)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  from
  [`load_afsp_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_pedons.md)
  or
  [`load_afsp_sample`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_sample.md).

- verbose:

  Print progress.

## Value

List with `accuracy`, `n_compared`, `confusion`, `per_class_recall`.
