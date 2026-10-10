# Benchmark soilKey WRB predictions against AfSP ground truth

Benchmark soilKey WRB predictions against AfSP ground truth

## Usage

``` r
benchmark_afsp(pedons, verbose = TRUE)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects, as
  [`load_afsp_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_pedons.md)
  returns, or the list that
  [`load_afsp_sample`](https://hugomachadorodrigues.github.io/soilKey/reference/load_afsp_sample.md)
  returns (the profiles are then taken from its `$pedons`; until
  v0.9.222 that list stopped with "\$ operator is invalid for atomic
  vectors").

- verbose:

  Print progress.

## Value

List with `accuracy`, `n_compared`, `confusion`, `per_class_recall`.
