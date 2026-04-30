# Render a soilKey classification report

Produces a pedologist-facing report from one or more
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
objects, optionally including the source
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).
The HTML output is fully self-contained (single file, inline CSS); the
PDF output goes through
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
and therefore requires a working LaTeX install (or one of the
alternative engines accepted by `rmarkdown`).

## Usage

``` r
report(
  x,
  file,
  format = c("auto", "html", "pdf"),
  pedon = NULL,
  title = NULL,
  ...
)
```

## Arguments

- x:

  A `ClassificationResult`, a list of `ClassificationResult`s, or a
  `PedonRecord` (in which case all three keys are run automatically).

- file:

  Output path. The format is inferred from the extension (`.html` or
  `.pdf`) unless `format` is given explicitly.

- format:

  One of `"auto"`, `"html"`, `"pdf"`.

- pedon:

  Optional `PedonRecord`; when provided, its horizons table and
  provenance log are included.

- title:

  Optional report title.

- ...:

  Passed to method-specific renderers.

## Value

The output path, invisibly.

## Details

This is an S3 generic with methods for `ClassificationResult`, `list`,
and `PedonRecord`. Most users call `report()` directly with a list of
three results
(`list(classify_wrb2022(p), classify_sibcs(p), classify_usda(p))`) to
get a cross-system one-pager.
