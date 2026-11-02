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
#>          id
#> 1   wrb2022
#> 2    sibcs5
#> 3      usda
#> 4     cssc3
#> 5      asc3
#> 6     nzsc4
#> 7  hscs2024
#> 8  scsj2017
#> 9  cscj2011
#> 10 cpcs1967
#>                                                                     name
#> 1                                                               WRB 2022
#> 2                                                     SiBCS, 5th edition
#> 3                                                     USDA Soil Taxonomy
#> 4                    Canadian System of Soil Classification, 3rd edition
#> 5                            Australian Soil Classification, 3rd edition
#> 6                           New Zealand Soil Classification, 4th edition
#> 7  Hungarian diagnostic soil classification (2024, second approximation)
#> 8                             Soil Classification System of Japan (2017)
#> 9               Comprehensive Soil Classification System of Japan (2011)
#> 10                                French soil classification CPCS (1967)
#>                                                                                       edition
#> 1                                                                          4th edition (2022)
#> 2                                                                          5th edition (2018)
#> 3                                                                         13th edition (2022)
#> 4                                                                          3rd edition (1998)
#> 5                               3rd edition (2021), online version with errata (25 June 2026)
#> 6                                         4th edition (2026), Quarto source of 7 October 2026
#> 7                                                       Second approximation (Útmutató, 2024)
#> 8                    Soil Classification System of Japan (2017), Japanese Society of Pedology
#> 9        Comprehensive Soil Classification System of Japan, First Approximation (2011), NIAES
#> 10 Classification des sols, édition 1967, Commission de Pédologie et de Cartographie des Sols
#>          status        classifier
#> 1        stable  classify_wrb2022
#> 2        stable    classify_sibcs
#> 3        stable     classify_usda
#> 4  experimental    classify_cssc3
#> 5  experimental     classify_asc3
#> 6  experimental    classify_nzsc4
#> 7  experimental classify_hscs2024
#> 8  experimental classify_scsj2017
#> 9  experimental classify_cscj2011
#> 10 experimental classify_cpcs1967
#>                                            levels       aliases default
#> 1                                            rsgs           wrb    TRUE
#> 2    ordens, subordens, grandes_grupos, subgrupos         sibcs    TRUE
#> 3      orders, suborders, great_groups, subgroups                  TRUE
#> 4                 orders, great_groups, subgroups          cssc   FALSE
#> 5      orders, suborders, great_groups, subgroups           asc   FALSE
#> 6                       orders, groups, subgroups          nzsc   FALSE
#> 7                                           types hscs, hungary   FALSE
#> 8                 great_groups, groups, subgroups   scsj, japan   FALSE
#> 9  great_groups, groups, subgroups, series_groups          cscj   FALSE
#> 10         classes, subclasses, groups, subgroups  cpcs, france   FALSE
```
