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
  infer_temperature = FALSE,
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

  When deriving the family and `site$soil_temperature_regime` is not
  recorded, suggest a temperature class from latitude and elevation (a
  crude lapse-rate model, "iso" within 23 degrees of the equator).
  Default `FALSE` since v0.9.415: the regime is not inferred unless
  asked for, and the family then carries no temperature class. A
  suggested class lowers the evidence grade to "C" and adds a warning.
  The key itself (orders to subgroups) never infers a regime.

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

## Undecided tests and regimes

A test whose data are missing is undecided (NA), not failed. The key
goes on past it, as the WRB key does, and records it: every taxon tested
before the one assigned, at the order, suborder, great group and
subgroup levels, whose result is NA is listed in `$ambiguities` (with
`level`, one of `"orders"`, `"suborders"`, `"great_groups"`,
`"subgroups"`), and `$missing_data` collects what was missing at every
level. When the name depends on an undecided test, it holds only if that
test fails: it is inferred by elimination, so the evidence grade falls
from `"A"` or `"B"` to `"C"` (the grade of an inferred result) and a
warning says so. When no taxon of a level passes and some were
undecided, the level is not named and the name stops at the level above.

The soil moisture and temperature regimes are read from
`site$soil_moisture_regime` and `site$soil_temperature_regime` and never
inferred in the key: with no regime recorded every regime test is
undecided and lists the site field as missing (KST 13th ed., pp. 47-55).
Taxa keyed as "Other" in the KST (Udalfs, Udepts, Udolls, Udox, ...) are
still reached, past the undecided regime taxa, with grade C; Udults need
a udic or perudic regime (p. 321; "ud" covers both, p. 48), so an
Ultisol with no regime recorded has no suborder.

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
#> [1] "fine, mixed, subactive Rhodic Hapludox"
```
