# Benchmark the Australian order key on NatSoil

Classifies NatSoil pedons with
[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md)
and compares the order with the one the surveyors gave
(`site$reference_asc_order`). A pedon is decided when no order before
the assigned one was left undecided for missing data; the agreement is
given for all pedons and for the decided ones. The surveys predate the
3rd edition's Arenosols (2021), whose soils earlier editions placed in
the Tenosols and Rudosols: the *edition* figures count an Arenosol given
to a surveyed Tenosol or Rudosol as agreement.

## Usage

``` r
benchmark_natsoil(pedons, cores = 1L, verbose = TRUE)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  from
  [`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md).

- cores:

  Number of processes
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html);
  always 1 on Windows).

- verbose:

  Print a summary.

## Value

A list: `n`; `accuracy` and `accuracy_edition`; `decided` (count),
`decided_accuracy` and `decided_accuracy_edition`; `by_confidence` (a
data frame by the surveyors' confidence, 1 = all necessary analytical
data); `recall` by surveyed order; `confusion` (rows the survey, columns
soilKey); `first_undecided` (the first order left undecided, among
provisional pedons); and `results`, one row per pedon.

## See also

[`load_natsoil_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_natsoil_pedons.md),
[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md).
