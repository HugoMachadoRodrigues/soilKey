# Load a soilKey rule set (YAML)

Load a soilKey rule set (YAML)

## Usage

``` r
load_rules(system = c("wrb2022", "usda", "sibcs5"), package = "soilKey")
```

## Arguments

- system:

  One of `"wrb2022"` (full WRB 2022 key, v0.2 wires 16/32 RSGs),
  `"usda"` (USDA Soil Taxonomy, v0.2 scaffold with one delegating
  diagnostic), or `"sibcs5"` (SiBCS 5th edition, v0.2 scaffold with one
  delegating diagnostic).

- package:

  Package owning the rule files (default `"soilKey"`).

## Value

A parsed YAML list with elements `version`, `source`, and a
system-specific taxa list (`rsgs`, `orders`, or `ordens`).
