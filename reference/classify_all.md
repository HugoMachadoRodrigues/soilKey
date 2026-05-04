# Classify a pedon across all three taxonomic systems

Convenience wrapper that runs
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
and
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
on the same
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
and returns a single named list with one entry per system (plus a
`summary` table that's handy for reports).

## Usage

``` r
classify_all(
  pedon,
  systems = "all",
  on_missing = c("warn", "silent", "error"),
  include_familia = TRUE,
  ...
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- systems:

  Character vector. Any subset of `c("wrb2022", "sibcs", "usda")`, or
  the literal `"all"` (default) to run every system.

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`. Forwarded verbatim
  to each classifier.

- include_familia:

  Forwarded to
  [`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md)
  (default `TRUE`). Has no effect on the other systems.

- ...:

  Additional named arguments are silently ignored.

## Value

A named list with elements:

- `wrb` –
  [`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
  from
  [`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
  (or `NULL` if the system was skipped or errored).

- `sibcs` – as above, from
  [`classify_sibcs()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md).

- `usda` – as above, from
  [`classify_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

- `summary` – a 1-row `data.frame` with one column per system, holding
  the resulting `$name` (or `NA` when the system was skipped / errored).
  Useful for tabulating many pedons in one shot.

## Details

Each classifier still produces its own
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with the full key trace and evidence grade – nothing is collapsed or
homogenised. The wrapper exists for ergonomics, not abstraction.

## Selecting a subset of systems

Pass `systems = c("wrb2022", "sibcs")` (or any other subset) to skip
systems you don't need. Default `systems = "all"` runs all three.

## Errors and partial results

If a single classifier raises an error, the corresponding slot of the
returned list is set to `NULL` and a one-line warning is emitted (so you
can rerun the offender on its own to see the full traceback). The other
classifiers still run and their results are returned. This matches the
spirit of `on_missing = "warn"` on the individual classifiers.

## Side effects

None. The classifiers do not mutate `pedon`; the wrapper does not attach
any side-channel state.

## See also

[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
pr <- make_ferralsol_canonical()
all_three <- classify_all(pr)
all_three$summary
#>                              wrb                                       sibcs
#> 1 Geric Ferric Rhodic Chromic ... Latossolos Vermelhos Distroficos tipicos...
#>             usda
#> 1 Rhodic Hapludox

# WRB + USDA only (skip SiBCS):
classify_all(pr, systems = c("wrb2022", "usda"))$summary
} # }
```
