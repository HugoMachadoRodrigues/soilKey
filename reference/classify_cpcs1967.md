# Classify a pedon under the French soil classification (CPCS 1967)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the twelve classes of the French soil classification of 1967
(Commission de Pedologie et de Cartographie des Sols). **Experimental**:
this release keys the classes (sols mineraux bruts, sols peu evolues,
vertisols, andosols, sols calcimagnesiques, sols isohumiques, sols
brunifies, sols podzolises, sols a sesquioxydes de fer et manganese,
sols ferrallitiques, sols hydromorphes, sols sodiques), their 35
sub-classes, the climatic ones read from `site$koppen_climate`, else the
USDA soil temperature and moisture regimes, else the latitude (with no
climate, the non-climatic or temperate sub-class), their 101 groups and
their 276 subgroups (a group or subgroup the data cannot decide leaves
the name provisional; when no group of a sub-class, or no subgroup of a
group, fits, the name stops at the level above, and none is invented).
The names are French, as the text writes them, in `name` and in
`levels$name_native`; a subgroup named by an adjective is shown after
its group ("Rankers, a mor"), one the text names in full stands alone,
and the other subgroups whose tests pass are noted in the trace as
intergrades.

## Usage

``` r
classify_cpcs1967(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- rules:

  Optional pre-loaded rule set (`load_rules("cpcs1967")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"cpcs1967"`, the deepest class assigned in `name`, the
class in `rsg_or_order`, the classes of each level in `levels` (with
`name_native`), the key trace, and `provisional_because` (classes before
the assigned one that could not be decided).

## Details

The CPCS gives no key. soilKey keys the classes in an order built from
the text's own statements of precedence: raw mineral soils, vertisols,
sodic soils, andosols, podzolised, ferrallitic and sesquioxide soils,
hydromorphic soils, weakly developed soils, isohumic soils,
calcimagnesic soils, and the brunified soils last; a peat is a
hydromorphic soil, whatever lies under it. Every threshold of the text
is in depths from the top of the first mineral horizon.

The key reads the old French horizon notation (Aoo, Ao, A1, A2, A3, (B),
B, Bh, Bfe, Bt, C, Cca, G, g, R, a Roman numeral for a change of parent
material), declared with `site$designation_system = "cpcs"` or
recognised by its symbols, beside the FAO letters; where a profile has
no designations, soilKey's diagnostic horizons stand in. From the
horizons it reads organic carbon and nitrogen, texture, pH, the CEC and
the exchangeable bases, carbonate and gypsum, the EC of the saturated
paste, Ki (`sio2_sulfuric_pct`, `al2o3_sulfuric_pct`), the andic
properties, moist and dry Munsell colours, mottles, slickensides,
cracks, structure and consistence, clay films, coarse fragments, the
sand minerals, the fibre content of peat, ice (`ice_pct`, `ice_form`),
and the recorded origin of the layers; from the site the climate, the
soil temperature regime, the parent material, recorded erosion, the
water table, the elevation, patterned ground (`patterned_ground`) and
the humus form (`humus_form`); for some subgroups also active carbonate
(`caco3_active_pct`), the coarse and fine sand (`coarse_sand_pct`,
`fine_sand_pct`) and gibbsite (`gibbsite_pct`). The choices where the
text is silent are listed in
`system.file("rules", "cpcs1967", "readings.yaml", package = "soilKey")`.

## References

Commission de Pedologie et de Cartographie des Sols (1967).
Classification des sols. ENSA, Grignon. Digitised by the AFES (2011).

## See also

[`make_cpcs1967_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cpcs1967_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_cpcs1967(make_cpcs1967_example("ferrallitic"), on_missing = "silent")
res$name
#> [1] "Sols ferrallitiques moyennement désaturés en (B) typiques, Faiblement appauvri"
res$levels
#>        level  code
#> 1    classes     X
#> 2 subclasses   X-2
#> 3     groups  X/21
#> 4  subgroups X/216
#>                                                                             name
#> 1                                                            Sols ferrallitiques
#> 2                               Sols ferrallitiques moyennement désaturés en (B)
#> 3                      Sols ferrallitiques moyennement désaturés en (B) typiques
#> 4 Sols ferrallitiques moyennement désaturés en (B) typiques, Faiblement appauvri
#>                                                                      name_native
#> 1                                                            Sols ferrallitiques
#> 2                               Sols ferrallitiques moyennement désaturés en (B)
#> 3                      Sols ferrallitiques moyennement désaturés en (B) typiques
#> 4 Sols ferrallitiques moyennement désaturés en (B) typiques, Faiblement appauvri
```
