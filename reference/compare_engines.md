# Side-by-side comparison of soilKey vs aqp diagnostic engines

Runs the soilKey hand-coded diagnostic and the aqp wrapper on the same
pedon, returns both results plus an agreement flag. Useful for A/B
benchmarks and for choosing which engine to use per dataset.

## Usage

``` r
compare_engines(pedon, diagnostic = c("argic", "cambic"))
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- diagnostic:

  One of `"argic"` or `"cambic"`.

## Value

A list with `soilkey`, `aqp`, `agree`.
