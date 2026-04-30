# Render a soilKey classification report as self-contained HTML

See
[`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
for the generic. This function writes a single-file HTML report with
inline CSS (no external network requests, no \`htmltools\` dependency)
so it can be emailed or archived as-is.

## Usage

``` r
report_html(x, file, pedon = NULL, title = NULL, ...)
```

## Arguments

- x:

  A `ClassificationResult`, list of results, or `PedonRecord`.

- file:

  Output `.html` path.

- pedon:

  Optional `PedonRecord`.

- title:

  Report title.

- ...:

  Currently unused.

## Value

The output path, invisibly.
