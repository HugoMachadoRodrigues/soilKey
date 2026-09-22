# Run a benchmark across one of the loaded pedon lists

Classifies each pedon in `pedons` against the named system, compares
against the published reference (e.g. `site$reference_wrb`), and returns
a confusion matrix + top-1 / top-3 accuracy + bootstrap CI on top-1.

## Usage

``` r
benchmark_run_classification(
  pedons,
  system = c("wrb2022", "sibcs", "usda"),
  level = c("order", "subgroup", "subordem", "great_group", "suborder"),
  boot_n = 1000L
)
```

## Arguments

- pedons:

  List of
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
  objects (output of one of the `load_*` functions).

- system:

  One of `"wrb2022"`, `"sibcs"`, `"usda"`.

- level:

  Granularity of the comparison:

  - `"order"` (default) – the top-level RSG / Ordem / Order, compared
    against `cls$rsg_or_order`;

  - `"subgroup"` – the full classified name (Subgroup in USDA, Subgrupo
    in SiBCS, RSG + qualifiers in WRB), compared against `cls$name`
    after case-insensitive token normalisation;

  - `"subordem"` – SiBCS-only, the 2nd-level "Ordem + Subordem" (e.g.
    "Latossolos Vermelhos"). Comparison via the first two normalised
    tokens of the predicted name vs the reference;

  - `"great_group"` (USDA, v0.9.24) – the LAST token of the subgroup
    name (e.g. `"typic hapludalfs"` -\> `"hapludalfs"`). Isolates
    whether the Great Group machinery is correct independent of subgroup
    modifiers (Typic / Aquic / Vertic / Cumulic / Pachic / etc.). Reads
    `site$reference_usda_grtgroup`;

  - `"suborder"` (USDA, v0.9.24) – maps the Great Group prediction to
    its canonical Suborder suffix (`"hapludalfs"` -\> `"udalfs"`) using
    the KST 13ed Ch 4 ~70-Suborder list. Reads
    `site$reference_usda_suborder`.

- boot_n:

  Bootstrap replicates for CI (default 1000).

## Value

A list with elements `accuracy_top1`, `accuracy_ci`, `confusion`, and
`per_pedon` (one row per pedon with predicted vs reference).
