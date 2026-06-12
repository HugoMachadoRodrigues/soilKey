# Subgroup full-names registered in the USDA subgroup rule base.

Reads every `inst/rules/usda/subgroups/<order>.yaml` and returns the
union of subgroup `name` fields (e.g. `"Typic Hapludands"`).

## Usage

``` r
.coverage_registered_usda_subgroups()
```

## Value

Character vector of registered subgroup names (lower-cased, trimmed).
