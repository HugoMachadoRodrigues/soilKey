# Classify a pedon in the FAO-Unesco Revised Legend (1988)

The registry's classifier of `fao1988` (aliases `fao`, `fao88`,
`revised_legend`): `classify_fao(pedon, edition = "1988")`. See
[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md).

## Usage

``` r
classify_fao1988(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set (`load_rules("fao1988")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md),
as
[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md).

## See also

[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md),
[`make_fao_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_fao_example.md).

## Examples

``` r
classify_fao1988(make_fao_example("CM"), on_missing = "silent")$name
#> [1] "Chromic Cambisols"
```
