# Run the soilKey performance benchmark

Generates `n` synthetic pedons (5 horizons each, with the chemistry /
morphology populated for typical Argissolo / Latossolo / Cambissolo
cases), calls each classifier on each pedon, and reports per-call
latency + total throughput.

## Usage

``` r
benchmark_performance(
  n = 100L,
  systems = c("wrb2022", "sibcs", "usda"),
  include_familia = FALSE,
  seed = 42L,
  verbose = TRUE
)
```

## Arguments

- n:

  Integer. Number of synthetic pedons to generate. Default 100; pass
  1000 or higher for batch-level measurements.

- systems:

  Character vector. Which classifiers to time. Default
  `c("wrb2022", "sibcs", "usda")` (all three).

- include_familia:

  Pass-through to `classify_sibcs` when `"sibcs"` is in `systems`.
  Default `FALSE`.

- seed:

  RNG seed for reproducibility. Default 42.

- verbose:

  If `TRUE` (default), prints a per-system summary line.

## Value

A list with elements:

- `summary`:

  data.frame:
  `system, n_pedons, total_seconds, mean_seconds, median_seconds, pedons_per_minute`.

- `per_pedon`:

  data.frame with one row per (pedon, system) call:
  `i, system, seconds, status`.

- `config`:

  list with `n`, `seed`, `soilKey_version`, `R_version`, `platform`.

## Details

Designed to be a one-shot reproducible benchmark: the synthetic pedons
use a fixed RNG seed so timings on the same machine are comparable
across releases.

## Examples

``` r
if (FALSE) { # \dontrun{
bench <- benchmark_performance(n = 100)
bench$summary
#>     system n_pedons total_seconds mean_seconds median_seconds pedons_per_minute
#> 1 wrb2022      100        ~ 5-12      0.05-0.12      ~          ~
#> 2   sibcs      100        ~ 5-15      0.05-0.15      ~          ~
#> 3    usda      100        ~ 4-10      0.04-0.10      ~          ~
} # }
```
