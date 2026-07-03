# Flatten a classification key trace into a tabular form

Normalises the system-dependent decision trace carried by a
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
into a single, ordered data frame of display rows. WRB 2022 stores a
flat list of reference-soil-group test steps; the hierarchical SiBCS and
USDA keys store a nested list of phases (orders, suborders, great
groups, subgroups, family, ...), each holding candidate steps, an
assigned-taxon record, family attributes, or a bare label. This function
walks all of those shapes and returns one row per step, in the order the
key was evaluated, so every consumer
([`print()`](https://rdrr.io/r/base/print.html), the HTML and PDF
reports, the Shiny app) can render the trace uniformly.

## Usage

``` r
key_trace_table(x)
```

## Arguments

- x:

  A
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md),
  or the `trace` list taken from one.

## Value

A `data.frame` with one row per trace step and columns:

- `phase`:

  Key phase / level the step belongs to (e.g. `"orders"`,
  `"subgrupos"`); empty for the flat WRB trace.

- `code`:

  Taxon or attribute code.

- `name`:

  Taxon or attribute name (or attribute value).

- `status`:

  One of `"passed"`, `"failed"`, `"indeterminate"` (a test that could
  not be evaluated for want of data), `"selected"` (the taxon assigned
  at a level), or `"info"` (a family attribute or label, not a pass/fail
  test).

- `missing`:

  Comma-separated attributes that were missing for the step (empty when
  none).

- `n_missing`:

  Integer count of missing attributes.

A zero-row data frame with those columns when the trace is empty.

## Examples

``` r
res <- classify_sibcs(make_ferralsol_canonical())
head(key_trace_table(res))
#>    phase code         name        status                    missing n_missing
#> 1 ordens    O Organossolos        failed                                    0
#> 2 ordens    R    Neossolos        failed                                    0
#> 3 ordens    V  Vertissolos indeterminate               slickensides         1
#> 4 ordens    E Espodossolos indeterminate       al_ox_pct, fe_ox_pct         2
#> 5 ordens    S  Planossolos        failed                                    0
#> 6 ordens    G   Gleissolos indeterminate redoximorphic_features_pct         1
```
