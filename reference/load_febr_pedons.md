# Load the Embrapa FEBR superconjunto into a list of PedonRecords

Reads the FEBR `febr-superconjunto.txt` export (one row per camada /
horizon, with the profile-level classification denormalised onto every
row), groups rows by `(dataset_id, observacao_id)`, and returns a list
of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects with all three reference taxa attached on `$site`:
`reference_sibcs` (raw FEBR string, e.g. "LATOSSOLO VERMELHO"),
`reference_usda`, `reference_wrb`.

## Usage

``` r
load_febr_pedons(
  path,
  head = NULL,
  require_classification = c("sibcs", "any", "wrb", "usda"),
  verbose = TRUE
)
```

## Arguments

- path:

  Path to `febr-superconjunto.txt`.

- head:

  Optional integer; if not `NULL`, return only the first `head` unique
  profiles after grouping.

- require_classification:

  One of `c("any", "sibcs", "wrb", "usda")`. Default `"sibcs"`: drop
  profiles whose chosen classification is NA. `"any"` keeps profiles
  with at least one of the three.

- verbose:

  If `TRUE` (default), summarises the load.

## Value

A list of
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
objects.

## Details

Drops profiles whose `taxon_sibcs` is NA (no usable reference). Drops
camadas with no horizon-depth information (no `profund_sup` /
`profund_inf`).
