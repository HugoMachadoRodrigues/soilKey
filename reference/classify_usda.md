# Classify a pedon under USDA Soil Taxonomy (13th edition)

Walks the canonical 4-level USDA key (Order -\> Suborder -\> Great Group
-\> Subgroup) using YAML rule files at:

- `inst/rules/usda/key.yaml`: Order key (12 entries)

- `inst/rules/usda/suborders/<order>.yaml`

- `inst/rules/usda/great-groups/<order>.yaml`

- `inst/rules/usda/subgroups/<order>.yaml`

## Usage

``` r
classify_usda(pedon, rules = NULL, on_missing = c("warn", "silent", "error"))
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set.

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with deepest-level taxon name. Each level's trace is in `$trace`.

## Details

Stops at the deepest level for which a YAML rule file is available (e.g.
v0.8.x: Gelisols full Path C; other 11 Orders at Order level only).

## References

Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th edition. USDA
Natural Resources Conservation Service.
