# Benchmark the Australian key on Queensland SALI

Classifies SALI pedons
([`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md))
with
[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md)
and compares them with the surveyors' classification, with the metrics
of
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md)
(which it calls): the order agreement over all pedons and over the
decided ones (no order before the assigned one left undecided for
missing data), the Arenosols of the 3rd edition counted as Tenosols or
Rudosols in the *edition* figures, the suborders given the order, the
great groups given the suborder and the subgroups given the great group.
It adds a table by surveyed order and one by the ASC edition the report
cites.

## Usage

``` r
benchmark_sali(pedons, cores = 1L, verbose = TRUE, family = FALSE)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  from
  [`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md).

- cores:

  Number of processes
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html);
  always 1 on Windows).

- verbose:

  Print a summary.

- family:

  Also compare the families (see
  [`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md);
  SALI labels carry no family codes, so this adds nothing yet).

## Value

The list of
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md),
plus `by_order` (by surveyed order: `n`, `agree` and `decided_agree` in
per cent, `decided`, and soilKey's most frequent order,
`top_prediction`) and `by_edition` (the same by the edition of the ASC
reference, `site$sali_asc_edition`).

## See also

[`read_sali_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_sali_pedons.md),
[`benchmark_natsoil`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_natsoil.md).
