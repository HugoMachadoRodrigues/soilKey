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
report_pdf(
  x,
  file,
  pedon = NULL,
  title = NULL,
  include_family = FALSE,
  specifiers = FALSE,
  lang = c("en", "pt"),
  ...
)
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

- include_family, specifiers:

  Passed through to the keys when `x` is a `PedonRecord`; see
  [`report`](https://hugomachadorodrigues.github.io/soilKey/reference/report.md).

- lang:

  Report language, `"en"` (default) or `"pt"` (Brazilian Portuguese).

- ...:

  Passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

## Value

The output path, invisibly.
