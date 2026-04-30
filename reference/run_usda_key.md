# Run the USDA Soil Taxonomy Order key over a pedon

Run the USDA Soil Taxonomy Order key over a pedon

## Usage

``` r
run_usda_key(pedon, rules = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set; if NULL, reads
  `inst/rules/usda/key.yaml`.

## Value

A list with `assigned` (the YAML entry of the assigned Order) and
`trace`.
