# Run the USDA Great Group key for a given Suborder

Run the USDA Great Group key for a given Suborder

## Usage

``` r
run_usda_great_group(pedon, suborder_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- suborder_code:

  The Suborder code (e.g. "AA" for Histels).

- rules:

  Optional pre-loaded rule set.

## Value

A list with `assigned` and `trace`; assigned is NULL if the Suborder has
no great-groups YAML.
