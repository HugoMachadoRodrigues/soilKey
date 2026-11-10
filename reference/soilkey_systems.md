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
#>           id
#> 1    wrb2022
#> 2     sibcs5
#> 3       usda
#> 4      cssc3
#> 5       asc3
#> 6      nzsc4
#> 7   hscs2024
#> 8   scsj2017
#> 9   cscj2011
#> 10  cpcs1967
#> 11 kidpr2008
#> 12    rp2008
#> 13       ka5
#> 14    za1991
#> 15   cst2001
#> 16   fao1988
#>                                                                       name
#> 1                                                                 WRB 2022
#> 2                                                       SiBCS, 5th edition
#> 3                                                       USDA Soil Taxonomy
#> 4                      Canadian System of Soil Classification, 3rd edition
#> 5                              Australian Soil Classification, 3rd edition
#> 6                             New Zealand Soil Classification, 4th edition
#> 7    Hungarian diagnostic soil classification (2024, second approximation)
#> 8                               Soil Classification System of Japan (2017)
#> 9                 Comprehensive Soil Classification System of Japan (2011)
#> 10                                  French soil classification CPCS (1967)
#> 11                         Russian soil classification (Field Guide, 2008)
#> 12                                   French Référentiel pédologique (2008)
#> 13 German soil classification (Bodenkundliche Kartieranleitung, KA5, 2005)
#> 14                                South African soil classification (1991)
#> 15                           Chinese Soil Taxonomy (2001, English edition)
#> 16                                        FAO-Unesco Revised Legend (1988)
#>                                                                                              edition
#> 1                                                                                 4th edition (2022)
#> 2                                                                                 5th edition (2018)
#> 3                                                                                13th edition (2022)
#> 4                                                                                 3rd edition (1998)
#> 5                                      3rd edition (2021), online version with errata (25 June 2026)
#> 6                                                4th edition (2026), Quarto source of 7 October 2026
#> 7                                                              Second approximation (Útmutató, 2024)
#> 8                           Soil Classification System of Japan (2017), Japanese Society of Pedology
#> 9               Comprehensive Soil Classification System of Japan, First Approximation (2011), NIAES
#> 10        Classification des sols, édition 1967, Commission de Pédologie et de Cartographie des Sols
#> 11                     Полевой определитель почв России, Почвенный институт им. В.В. Докучаева, 2008
#> 12                                           Référentiel pédologique 2008, AFES, Éditions Quæ (2009)
#> 13                       Bodenkundliche Kartieranleitung, 5. Auflage, Ad-hoc-AG Boden, Hannover 2005
#> 14                                   Soil classification: a taxonomic system for South Africa (1991)
#> 15                                        Chinese Soil Taxonomy, English edition, Science Press 2001
#> 16 World Soil Resources Report 60, FAO 1988; corrected reprint 1990; ISRIC Technical Paper 20 (1997)
#>          status         classifier
#> 1        stable   classify_wrb2022
#> 2        stable     classify_sibcs
#> 3        stable      classify_usda
#> 4  experimental     classify_cssc3
#> 5  experimental      classify_asc3
#> 6  experimental     classify_nzsc4
#> 7  experimental  classify_hscs2024
#> 8  experimental  classify_scsj2017
#> 9  experimental  classify_cscj2011
#> 10 experimental  classify_cpcs1967
#> 11 experimental classify_kidpr2008
#> 12 experimental    classify_rp2008
#> 13 experimental       classify_ka5
#> 14 experimental    classify_za1991
#> 15 experimental   classify_cst2001
#> 16 experimental   classify_fao1988
#>                                            levels
#> 1                                            rsgs
#> 2    ordens, subordens, grandes_grupos, subgrupos
#> 3      orders, suborders, great_groups, subgroups
#> 4                 orders, great_groups, subgroups
#> 5      orders, suborders, great_groups, subgroups
#> 6                       orders, groups, subgroups
#> 7                                           types
#> 8                 great_groups, groups, subgroups
#> 9  great_groups, groups, subgroups, series_groups
#> 10         classes, subclasses, groups, subgroups
#> 11                       trunks, divisions, types
#> 12                                     references
#> 13            divisions, classes, types, subtypes
#> 14                                forms, families
#> 15           orders, suborders, groups, subgroups
#> 16                            major_groups, units
#>                                       aliases default
#> 1                                         wrb    TRUE
#> 2                                       sibcs    TRUE
#> 3                                                TRUE
#> 4                                        cssc   FALSE
#> 5                                         asc   FALSE
#> 6                                        nzsc   FALSE
#> 7                               hscs, hungary   FALSE
#> 8                                 scsj, japan   FALSE
#> 9                                        cscj   FALSE
#> 10                               cpcs, france   FALSE
#> 11                      kidpr, russia, ru2008   FALSE
#> 12                            rp, referentiel   FALSE
#> 13 germany, kartieranleitung, bodensystematik   FALSE
#> 14             za, sat, sat1991, south_africa   FALSE
#> 15                        cst, china, chinese   FALSE
#> 16                 fao, fao88, revised_legend   FALSE
```
