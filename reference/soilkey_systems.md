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
#>         id
#> 1  wrb2022
#> 2   sibcs5
#> 3     usda
#> 4    cssc3
#> 5     asc3
#> 6    nzsc4
#> 7 hscs2024
#>                                                                    name
#> 1                                                              WRB 2022
#> 2                                                    SiBCS, 5th edition
#> 3                                                    USDA Soil Taxonomy
#> 4                   Canadian System of Soil Classification, 3rd edition
#> 5                           Australian Soil Classification, 3rd edition
#> 6                          New Zealand Soil Classification, 4th edition
#> 7 Hungarian diagnostic soil classification (2024, second approximation)
#>                                                         edition       status
#> 1                                            4th edition (2022)       stable
#> 2                                            5th edition (2018)       stable
#> 3                                           13th edition (2022)       stable
#> 4                                            3rd edition (1998) experimental
#> 5 3rd edition (2021), online version with errata (25 June 2026) experimental
#> 6           4th edition (2026), Quarto source of 7 October 2026 experimental
#> 7                         Second approximation (Útmutató, 2024) experimental
#>          classifier                                       levels       aliases
#> 1  classify_wrb2022                                         rsgs           wrb
#> 2    classify_sibcs ordens, subordens, grandes_grupos, subgrupos         sibcs
#> 3     classify_usda   orders, suborders, great_groups, subgroups              
#> 4    classify_cssc3              orders, great_groups, subgroups          cssc
#> 5     classify_asc3   orders, suborders, great_groups, subgroups           asc
#> 6    classify_nzsc4                    orders, groups, subgroups          nzsc
#> 7 classify_hscs2024                                        types hscs, hungary
#>   default
#> 1    TRUE
#> 2    TRUE
#> 3    TRUE
#> 4   FALSE
#> 5   FALSE
#> 6   FALSE
#> 7   FALSE
```
