# Benchmark the Canadian key against the National Pedon Database

Classifies NPDB pedons
([`load_npdb_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_npdb_pedons.md))
with
[`classify_cssc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cssc3.md)
and compares the result with the survey's classification at the level
asked.

## Usage

``` r
benchmark_npdb(
  pedons,
  level = c("order", "great_group", "subgroup"),
  set_aside = "Chernozemic",
  cores = 1L,
  verbose = TRUE
)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  from
  [`load_npdb_pedons`](https://hugomachadorodrigues.github.io/soilKey/reference/load_npdb_pedons.md).

- level:

  `"order"`, `"great_group"` or `"subgroup"`.

- set_aside:

  Reference orders reported apart (`character(0)` for none).

- cores:

  Number of processes
  ([`parallel::mclapply`](https://rdrr.io/r/parallel/mclapply.html);
  always 1 on Windows).

- verbose:

  Print a summary.

## Value

A list: `level`; `accuracy` (share of agreement outside `set_aside`);
`accuracy_decided`; `n_compared`; `n_decided`; `accuracy_all` (all
orders); `set_aside` (a table of where those pedons land); `confusion`;
`per_class_recall`; `predictions` (one row per pedon; `scenario` marks
pedons classified at the ends of their ranges and `scenario_split` those
whose versions disagree).

## Details

The records hold no soil climate and no dry colour, which the key reads
for the Chernozemic order and its great groups (Ch 3, p. 34; Ch 5, p.
63) and for the Gray Brown Luvisols; the orders named in `set_aside` (by
default the Chernozemic) are therefore reported apart, with where they
land, and left out of `accuracy`. A result is provisional when an
undecided class comes before the one assigned (`$provisional_because` of
[`classify_cssc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_cssc3.md));
`accuracy_decided` counts only the results that are not provisional at
the level asked or above.

Pedons from
[`npdb_scenario`](https://hugomachadorodrigues.github.io/soilKey/reference/npdb_scenario.md)
carry estimates as ranges; each is classified at its central estimate
and at every combination of the ends of its ranges (and of the moisture
subclasses between its two estimates), and a class counts as decided
only where every version gives it and none is provisional (otherwise the
central class is reported as provisional). Run such pedons with
`set_aside = character(0)` to include the Chernozems.
