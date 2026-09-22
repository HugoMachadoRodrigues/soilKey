# Unified cross-dataset benchmark across SiBCS / WRB / USDA

Runs a system's soilKey classifier on every dataset that has reference
labels for that system, then pools the results into a single
nation-/world-wide accuracy estimate.

## Usage

``` r
benchmark_unified(
  systems = c("all", "wrb2022", "sibcs", "usda"),
  datasets = c("all", "bdsolos", "febr", "kssl", "lucas_esdb"),
  paths = NULL,
  max_n_per_dataset = NULL,
  engine = c("soilkey", "aqp", "both"),
  harmonize = FALSE,
  gapfill = FALSE,
  verbose = TRUE
)
```

## Arguments

- systems:

  Character vector. Any subset of `c("wrb2022", "sibcs", "usda")`.
  Default `"all"` runs all three.

- datasets:

  Character vector. Any subset of
  `c("bdsolos", "febr", "kssl", "lucas_esdb")`. Default `"all"` pools
  every dataset that has reference labels for the requested systems.
  Datasets without reference labels for a system are silently excluded
  from that system's pooled result.

- paths:

  Named list of dataset paths. Element names should match those in
  `datasets`. If `NULL` (default), soilKey looks for canonical paths
  under `"~/soil_data/"`.

- max_n_per_dataset:

  Optional integer to cap per-dataset sample size (useful for
  development / debugging). `NULL` (default) classifies every available
  pedon.

- engine:

  Currently forwarded to Phase-1 aqp wiring. One of `"soilkey"`
  (default), `"aqp"`, `"both"`. When `"aqp"`, sets
  `options(soilKey.diagnostic_engine = "aqp")` for the duration of the
  benchmark, which routes
  [`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
  /
  [`cambic()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md)
  through the canonical
  [`aqp::getArgillicBounds`](https://ncss-tech.github.io/aqp/reference/getArgillicBounds.html)
  / `getCambicBounds`.

- harmonize:

  If `TRUE` (default `FALSE`), applies
  [`harmonize_to_gsm`](https://hugomachadorodrigues.github.io/soilKey/reference/harmonize_to_gsm.md)
  to each dataset's pedons before classification, putting all
  chemistry/texture on the GSM depth grid (0-5 / 5-15 / 15-30 / 30-60 /
  60-100 / 100-200 cm). Required for cross-dataset pooling integrity
  (Phase 2.3) but slow (~1-2 min for 1k pedons) and may degrade
  per-dataset accuracy slightly because the splined depths are
  approximations.

- gapfill:

  If not `FALSE` (the default), applies
  [`gapfill_within_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_within_pedon.md)
  to each dataset's pedons before classification, filling interior `NA`
  cells of the continuous depth-trending attributes by within-pedon
  linear interpolation. Accepts the same values as the `gapfill`
  argument of
  [`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
  (`TRUE`, a character vector of attributes, or a named list). Lets you
  measure the ON/OFF accuracy lift of gap-fill reproducibly through the
  harness.

- verbose:

  If `TRUE` (default), emits cli progress.

## Value

A list with elements:

- `per_system` – per-system pooled
  `list(accuracy, n_compared, n_correct, confusion, per_class)`.

- `per_system_per_dataset` – per-(system, dataset) same shape, for
  breakdown.

- `coverage` – per-(system, dataset) sample sizes and label coverage.

- `config` – captures
  `systems, datasets, engine, soilKey_version, timestamp`.

## Datasets and their reference labels

|                     |                                            |
|---------------------|--------------------------------------------|
| Dataset             | Systems with reference labels              |
| BDsolos             | SiBCS (dense), WRB (sparse), USDA (sparse) |
| FEBR superconjunto  | SiBCS, WRB, USDA (most rows have all 3)    |
| KSSL+NASIS          | USDA only (samp_taxsubgrp universal)       |
| LUCAS + ESDB raster | WRB (via lookup_esdb on coords)            |

For each (system, dataset) pair, this function:

1.  Loads pedons via the appropriate `load_*` helper.

2.  Filters to pedons with a populated reference label for the requested
    system.

3.  Normalises both reference and predicted labels via
    `normalise_febr_*()` / KSSL canonicalisation helpers.

4.  Calls the system's classifier and records pred-vs-ref.

Then pools per-system results across datasets.

## Engine selection (Phase 1 wiring)

For datasets with morphological data (BDsolos / FEBR), the diagnostics
that pivot Argissolos / Latossolos / Cambissolos classification can be
run with two engines:

- `engine = "soilkey"` (default) – the hand-coded WRB 6/1.4/20
  thresholds.

- `engine = "aqp"` – aqp::getArgillicBounds / getCambicBounds (KST 13ed
  3/1.2/8 thresholds).

On the v0.9.62 RJ benchmark (722 perfis), aqp was 14.8 pp stricter on
argic and 40.6 pp more permissive on cambic; the SiBCS Argissolos /
Latossolos / Cambissolos boundary is sensitive to both. `engine` is
currently forwarded to a future v0.9.63 wired
[`argic()`](https://hugomachadorodrigues.github.io/soilKey/reference/argic.md)
/
[`cambic()`](https://hugomachadorodrigues.github.io/soilKey/reference/cambic.md);
for now, `benchmark_unified()` reports separately per engine when
`engine = "both"`.

## See also

[`benchmark_bdsolos`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_bdsolos.md),
[`benchmark_lucas_2018`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_lucas_2018.md),
[`benchmark_run_classification`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_run_classification.md),
[`harmonize_to_gsm`](https://hugomachadorodrigues.github.io/soilKey/reference/harmonize_to_gsm.md).
