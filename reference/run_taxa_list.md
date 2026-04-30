# Iterate a flat taxa list and evaluate tests in canonical order

Internal iterator extracted from
[`run_taxonomic_key`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxonomic_key.md)
so nested categorical levels (subordens, grandes grupos, subgrupos,
familias) can be iterated directly, without going through the
`rules[[level_key]]` indirection that only makes sense at the top level.

## Usage

``` r
run_taxa_list(pedon, taxa)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- taxa:

  A list of taxon entries; each entry must have `code`, `name`, and
  `tests` fields, where `tests` is a block parseable by
  [`evaluate_rsg_tests`](https://hugomachadorodrigues.github.io/soilKey/reference/evaluate_rsg_tests.md).

## Value

A list with `assigned` (the entry of the assigned taxon, or NULL when
`taxa` was empty) and `trace`.

## Details

Behavioural note: when `taxa` is empty or `NULL`, returns
`list(assigned = NULL, trace = list())` – a sub-level lookup with no
canonical entries is non-fatal. The top-level
[`run_taxonomic_key`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxonomic_key.md)
keeps the stricter "missing list is an error" semantics by guarding
before calling this helper.
