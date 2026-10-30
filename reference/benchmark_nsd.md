# Benchmark the New Zealand key against the National Soils Database

Classifies NSD pedons
([`read_nsd_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/read_nsd_pedons.md))
with
[`classify_nzsc4`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_nzsc4.md)
and compares the result with the surveyor's NZSC class at the level
asked: the order (the first letter of the code), the group (the first
two letters) or the subgroup (the whole code). A pedon is compared at a
level only when its recorded class reaches that level (a code of one
letter names only the order). A result is provisional when a class
before the assigned one at the level asked, or a level above it, could
not be decided (`$provisional_because` of
[`classify_nzsc4`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_nzsc4.md));
`accuracy_decided` counts only the results that are not provisional.

## Usage

``` r
benchmark_nsd(
  pedons,
  level = c("order", "group", "subgroup"),
  cores = 1L,
  verbose = TRUE
)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)s
  from `read_nsd_pedons`.

- level:

  `"order"`, `"group"` or `"subgroup"`.

- cores:

  Number of processes
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html);
  always 1 on Windows).

- verbose:

  Print a summary.

## Value

A list: `level`; `accuracy`; `accuracy_decided`; `n_compared`;
`n_decided`; `confusion` (reference by prediction, codes);
`per_class_recall`; `predictions` (one row per pedon with a reference:
the recorded and predicted codes at every level, `agree`, `provisional`
and `provisional_because`).
