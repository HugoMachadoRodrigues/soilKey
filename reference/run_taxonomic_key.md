# Run a taxonomic key (system-agnostic engine)

Iterates over the taxa list at `rules[[level_key]]` in canonical order;
the first taxon whose tests pass is assigned. `evaluate_rsg_tests` is
reused as the per-taxon evaluator regardless of system – the test
combinator semantics (`all_of` / `any_of` / `default` /
`not_implemented_v01`) are the same in all three systems.

## Usage

``` r
run_taxonomic_key(pedon, rules, level_key)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  A parsed rule set (output of
  [`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md)).

- level_key:

  Name of the taxa list inside `rules`: typically `"rsgs"` (WRB),
  `"orders"` (USDA), or `"ordens"` (SiBCS).

## Value

A list with `assigned` (the YAML entry of the assigned taxon) and
`trace` (one entry per taxon tested).

## Details

Used at the TOP level (RSG / Order / Ordem). For nested categorical
levels (subordens, grandes grupos, subgrupos, familias) iterate the flat
taxa list directly via
[`run_taxa_list`](https://hugomachadorodrigues.github.io/soilKey/reference/run_taxa_list.md).
