# Classify a pedon in the FAO-Unesco Legend of 1974

The registry's classifier of `fao1974` (aliases `fao74`,
`fao_unesco_1974`): `classify_fao(pedon, edition = "1974")`.
Experimental and opt-in. See
[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md).

## Usage

``` r
classify_fao1974(
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

  Optional pre-loaded rule set (`load_rules("fao1988")`, or
  `load_rules("fao1974")` with `edition = "1974"`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md),
as
[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md),
with `system_id` `"fao1974"`.

## References

FAO-Unesco (1974). Soil Map of the World 1:5 000 000. Volume I, Legend.
Unesco, Paris.

## See also

[`classify_fao`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao.md),
[`make_fao_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_fao_example.md).

## Examples

``` r
classify_fao1974(make_fao_example("L", edition = "1974"), on_missing = "silent")$name
#> [1] "Orthic Luvisols"
```
