# Render a soilKey classification report as PDF

See
[`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md)
for the generic dispatcher. This function assembles a temporary \`.Rmd\`
file with the same content as
[`report_html`](https://hugomachadorodrigues.github.io/soilKey/reference/report_html.md)
(site, cross-system summary, classification cards, horizons, provenance)
and renders it via
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Usage

``` r
report_pdf(x, file, pedon = NULL, title = NULL, ...)
```

## Arguments

- x:

  A `ClassificationResult`, list of results, or `PedonRecord`.

- file:

  Output `.pdf` path.

- pedon:

  Optional `PedonRecord`.

- title:

  Report title.

- ...:

  Passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

The output path, invisibly.
