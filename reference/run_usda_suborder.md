# Run the USDA Suborder key for a given Order

Run the USDA Suborder key for a given Order

## Usage

``` r
run_usda_suborder(pedon, order_code, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- order_code:

  The Order code (e.g. "GE" for Gelisols).

- rules:

  Optional pre-loaded rule set.

## Value

A list with `assigned` and `trace`; assigned is NULL if the Order has no
suborders YAML.
