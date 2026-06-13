# Classify a pedon under USDA Soil Taxonomy (13th edition)

Walks the canonical USDA key (Order -\> Suborder -\> Great Group -\>
Subgroup) using YAML rule files at:

- `inst/rules/usda/key.yaml`: Order key (12 entries)

- `inst/rules/usda/suborders/<order>.yaml`

- `inst/rules/usda/great-groups/<order>.yaml`

- `inst/rules/usda/subgroups/<order>.yaml`

## Usage

``` r
classify_usda(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  include_family = FALSE,
  infer_temperature = TRUE,
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set.

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- include_family:

  If `TRUE`, derive and prepend the 5th-level family modifiers. Default
  `FALSE` (output byte-identical to earlier versions).

- infer_temperature:

  When deriving the family, infer the soil temperature regime from
  latitude/elevation if `site$soil_temperature_regime` is absent
  (default `TRUE`). See
  [`family_temperature_regime_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/family_temperature_regime_usda.md).

- gapfill:

  Opt-in within-pedon depth gap-fill, default `FALSE` (no-op,
  classification stays byte-identical). `TRUE` fills interior `NA` cells
  of the continuous depth-trending attributes by linear interpolation
  from the profile's own measured horizons; a character vector restricts
  it to those attributes; a named list is passed to
  [`gapfill_within_pedon`](https://hugomachadorodrigues.github.io/soilKey/reference/gapfill_within_pedon.md).
  Filled cells carry `inferred_prior` provenance, so the evidence grade
  drops to `"C"`. Runs on a deep copy – the caller's pedon is never
  mutated.

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with deepest-level taxon name. Each level's trace is in `$trace`; the
family attributes are in `$trace$family`.

## Details

With `include_family = TRUE` it additionally derives the 5th category,
the **family** – a set of class modifiers (particle-size, mineralogy,
CEC-activity, reaction, temperature regime, depth) PREPENDED to the
subgroup name, e.g. *"fine, kaolinitic, isohyperthermic Rhodic
Hapludox"*. See
[`classify_usda_family`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda_family.md).

## References

Soil Survey Staff (2022). Keys to Soil Taxonomy, 13th edition. USDA
Natural Resources Conservation Service.

## Examples

``` r
pedon <- make_ferralsol_canonical()
res <- classify_usda(pedon)
res$name
#> [1] "Rhodic Hapludox"
# include the 5th (family) level:
classify_usda(pedon, include_family = TRUE)$name
#> [1] "fine, mixed, subactive, isothermic Rhodic Hapludox"
```
