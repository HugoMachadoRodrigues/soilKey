# Load a soilKey rule set (YAML)

Load a soilKey rule set (YAML)

## Usage

``` r
load_rules(system = "wrb2022", package = "soilKey")
```

## Arguments

- system:

  Id of a registered classification system (see
  [`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md)):
  `"wrb2022"` (WRB 2022, the default), `"sibcs5"` (SiBCS 5th edition) or
  `"usda"` (Keys to Soil Taxonomy, 13th edition). Aliases such as
  `"wrb"` and `"sibcs"` are accepted.

- package:

  Package owning the rule files (default `"soilKey"`).

## Value

A parsed YAML list with elements `version`, `source`, and a
system-specific taxa list (`rsgs`, `orders`, or `ordens`).
