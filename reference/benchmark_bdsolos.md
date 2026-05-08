# Benchmark soilKey classifiers against BDsolos national reference labels

Runs
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md),
[`classify_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_sibcs.md),
and
[`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md)
on each
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
loaded from a BDsolos CSV via
[`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md),
then compares each predicted classification against the corresponding
BDsolos reference label (`reference_sibcs`, `reference_wrb`,
`reference_st`) and reports per-system accuracy, per-class recall, and a
confusion matrix.

## Usage

``` r
benchmark_bdsolos(
  pedons,
  systems = c("wrb2022", "sibcs", "usda"),
  sibcs_level = c("order", "subordem"),
  max_n = NULL,
  verbose = TRUE
)
```

## Arguments

- pedons:

  A list of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects, typically produced by
  [`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md).

- systems:

  Character vector. Any subset of `c("wrb2022", "sibcs", "usda")`.
  Default runs all three.

- sibcs_level:

  One of `"order"` (default) or `"subordem"`. Forwarded to
  [`normalise_febr_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md).

- max_n:

  Optional integer; cap classification at the first `max_n` pedons.
  `NULL` (default) classifies every pedon.

- verbose:

  If `TRUE` (default), emits cli progress messages.

## Value

A list with elements:

- `per_system` – named list (one entry per requested system) of
  `list(accuracy, n_compared, n_correct, n_errors, confusion, per_class)`
  (or `list(accuracy = NA_real_, message)` when no reference labels were
  present).

- `coverage` – named list of `list(n_with_ref, n_total, pct)` per
  system.

- `config` – named list capturing
  `n_pedons, systems, sibcs_level, soilKey_version, timestamp`.

## Reference label coverage

BDsolos densely populates `reference_sibcs` (~82 of the v0.9.59 audit)
but sparsely populates `reference_wrb` and `reference_st` (UF-dependent;
~5 states). The function always reports the per-system label coverage
(`$coverage`) so the caller can judge how representative each accuracy
figure is.

## Comparison level

SiBCS comparison is at `level = "order"` by default, which converts the
BDsolos all-caps Portuguese label (e.g.
`"ARGISSOLO VERMELHO Tb EUTROFICO ..."`) to the soilKey plural Title
Case form (`"Argissolos"`) via
[`normalise_febr_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md).
Set `sibcs_level = "subordem"` to compare the first two SiBCS tokens
(Ordem + Subordem).

WRB and USDA comparisons are at the Reference Soil Group / Order level:
[`normalise_febr_wrb()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_wrb.md)
strips qualifier parens and pluralises the bare RSG
(`"Xanthic Ferralsol"` -\> `"Ferralsols"`);
[`normalise_febr_usda()`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md)
maps the suffix of the last subgroup token to the USDA Order
(`"Typic Haplorthox"` -\> `"Oxisols"`).

## Errors and missing-label handling

Pedons without a reference label for a given system are silently
excluded from THAT system's comparison (but still classified for the
other two systems). If a system has zero pedons with a reference label,
the corresponding `$per_system` entry has `accuracy = NA_real_` and
`message = "no_reference_labels"`. Classifier errors are caught
per-pedon and recorded in `n_errors`; they do not abort the run.

## See also

[`load_bdsolos_csv`](https://hugomachadorodrigues.github.io/soilKey/reference/load_bdsolos_csv.md),
[`benchmark_lucas_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md),
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md),
[`normalise_febr_sibcs`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_sibcs.md),
[`normalise_febr_wrb`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_wrb.md),
[`normalise_febr_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/normalise_febr_usda.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Single UF -- typical SiBCS-dense slice
peds <- load_bdsolos_csv("RJ.csv")
bench <- benchmark_bdsolos(peds, systems = c("sibcs", "wrb2022", "usda"))
bench$coverage      # how many pedons had each reference label
bench$per_system$sibcs$accuracy
bench$per_system$sibcs$confusion

# Subordem level
bench2 <- benchmark_bdsolos(peds, systems = "sibcs",
                               sibcs_level = "subordem")
} # }
```
