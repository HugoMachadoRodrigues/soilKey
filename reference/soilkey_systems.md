# Classification systems known to soilKey

Lists the classification systems registered in the package, one row per
system, in the order
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
runs them. Each system is a folder `inst/rules/<id>/` holding its key
and a `system.yaml` entry. The `id` is what
[`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md)
and
[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md)
accept; the aliases are accepted too.

## Usage

``` r
soilkey_systems()
```

## Value

A `data.frame` with one row per system and the columns `id`, `name`
(English display name), `edition`, `status`, `classifier` (the function
that classifies a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
in that system), `levels` and `aliases` (both comma-separated), and
`default` (whether `classify_all(systems = "all")` runs the system).

## See also

[`classify_all`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_all.md),
[`load_rules`](https://hugomachadorodrigues.github.io/soilKey/reference/load_rules.md).

## Examples

``` r
soilkey_systems()
#>        id                                                name
#> 1 wrb2022                                            WRB 2022
#> 2  sibcs5                                  SiBCS, 5th edition
#> 3    usda                                  USDA Soil Taxonomy
#> 4   cssc3 Canadian System of Soil Classification, 3rd edition
#> 5    asc3         Australian Soil Classification, 3rd edition
#>                                                         edition       status
#> 1                                            4th edition (2022)       stable
#> 2                                            5th edition (2018)       stable
#> 3                                           13th edition (2022)       stable
#> 4                                            3rd edition (1998) experimental
#> 5 3rd edition (2021), online version with errata (25 June 2026) experimental
#>         classifier                                       levels aliases default
#> 1 classify_wrb2022                                         rsgs     wrb    TRUE
#> 2   classify_sibcs ordens, subordens, grandes_grupos, subgrupos   sibcs    TRUE
#> 3    classify_usda   orders, suborders, great_groups, subgroups            TRUE
#> 4   classify_cssc3              orders, great_groups, subgroups    cssc   FALSE
#> 5    classify_asc3                                       orders     asc   FALSE
```
