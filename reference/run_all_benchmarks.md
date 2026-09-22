# Run the full soilKey benchmark suite and (optionally) write a report

Auto-detects which reference datasets are available locally, runs each
via
[`benchmark_unified`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_unified.md),
adds the offline canonical sanity row and the AfSP sample when present,
and returns a tidy accuracy summary. When `report_path` is given, a
consolidated Markdown report is written.

## Usage

``` r
run_all_benchmarks(
  datasets = "auto",
  paths = NULL,
  max_n = 300L,
  level = "order",
  report_path = NULL,
  verbose = TRUE
)
```

## Arguments

- datasets:

  `"auto"` (default) detects available datasets; otherwise any subset of
  `c("bdsolos", "febr", "kssl", "lucas_esdb", "redape")`, the literal
  `"canonical"` (only the fixture sanity row), or `"all"` (every dataset
  regardless of availability – absent ones are skipped).

- paths:

  Named list of dataset paths (see
  [`benchmark_unified`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_unified.md)).
  `NULL` uses the package defaults (override the root via
  `options(soilKey.benchmark_root = "...")`).

- max_n:

  Cap on pedons per dataset (keeps the run fast). Default 300.

- level:

  Comparison level forwarded where supported (currently the suite
  reports at `"order"` / top level).

- report_path:

  File to write the Markdown report to, `TRUE` to auto-name one under
  `inst/benchmarks/reports/`, or `NULL` (default) for no file.

- verbose:

  Print progress.

## Value

Invisibly, a list with `summary` (data.frame: dataset, system,
n_compared, accuracy), `per_system` (pooled), `raw` (full
`benchmark_unified` output), `weak` (zero-recall classes) and `config`.

## See also

[`benchmark_unified`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_unified.md),
[`benchmark_redape`](https://hugomachadorodrigues.github.io/soilKey/reference/benchmark_redape.md).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- run_all_benchmarks(max_n = 250,
                          report_path = TRUE)
res$summary
} # }
```
