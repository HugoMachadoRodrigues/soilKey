# Run the USDA Subgroup key for a given Great Group

Run the USDA Subgroup key for a given Great Group

## Usage

``` r
run_usda_subgroup(pedon, great_group_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- great_group_code:

  The Great Group code (e.g. "AAA" for Folistels).

- rules:

  Optional pre-loaded rule set.

## Value

A list with `assigned` and `trace`; assigned is NULL if the Great Group
has no subgroups YAML.
