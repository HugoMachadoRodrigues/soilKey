# Classify a pedon under the Comprehensive Soil Classification System of Japan

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Comprehensive Soil Classification System of Japan, First
Approximation (Obara et al. 2011), following its key to great groups.
**Experimental**: this release keys the ten great groups (Man-made
soils, Organic soils, Podzols, Andosols, Dark Red soils, Lowland soils,
Red-Yellow soils, Stagnic soils, Brown Forest soils, Regosols), their 27
groups (for instance Fulvic and Allophanic Andosols, Calcaric Dark Red
soils, Brown Lowland soils) and their 116 subgroups (Haplic, Argic,
Low-humic, Terrace ...) and their 381 soil series groups (lithic,
skeletal, fine-textured, high-humic ..., read with the new attributes
`texture_class_jp` and `hardness_yamanaka_mm`), and gives the soil
phases of chapter VIII (temperature, estimated from the air temperature
plus 2 C when the soil temperature is not recorded, and the man-made
phases). Each level gives the English name of the text's appendix 2 and,
in `levels$name_native`, the Japanese one.

## Usage

``` r
classify_cscj2011(
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

  Optional pre-loaded rule set (`load_rules("cscj2011")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"cscj2011"`, the deepest class in `name`, the great
group in `rsg_or_order`, the classes of each level in `levels` (with
`name_native`), the key trace, `provisional_because` (classes before the
assigned one that could not be decided), and in `phases` the soil phases
of chapter VIII (temperature and man-made phases; the class name is left
as it is).

## Details

The CSCJ shares most of its diagnostics with the Soil Classification
System of Japan (2017), and soilKey reads them with the same code (see
[`classify_scsj2017`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_scsj2017.md)
for the attributes read), by the CSCJ's own limits where it has them:
the argic horizon (the WRB 2006 clay increase), the immature kuroboku
feature (P retention as its only substitute), and the dark red colour of
the Dark Red soils, which also read the origin from limestone or
calcareous deposits (`rock_origin`, `layer_origin`, the site
`parent_material`, `caco3_pct`). The choices where the text is silent
are listed in
`system.file("rules", "cscj2011", "readings.yaml", package = "soilKey")`.

## References

Obara, H., Ohkura, T., Takata, Y., Kohyama, K., Maejima, Y. and
Hamazaki, T. (2011). Comprehensive Soil Classification System of Japan,
First Approximation. Bulletin of the National Institute for
Agro-Environmental Sciences 29: 1-73.

## See also

[`make_cscj2011_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cscj2011_example.md),
[`classify_scsj2017`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_scsj2017.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_cscj2011(make_cscj2011_example("dark_red"), on_missing = "silent")
res$name
#> [1] "typic Argic Calcaric Dark Red soils"
res$levels
#>           level   code                                name
#> 1  great_groups      E                      Dark Red soils
#> 2        groups     E1             Calcaric Dark Red soils
#> 3     subgroups   E1AR       Argic Calcaric Dark Red soils
#> 4 series_groups E1ARTY typic Argic Calcaric Dark Red soils
#>                  name_native
#> 1               暗赤色土大群
#> 2             石灰性暗赤色土
#> 3     粘土集積石灰性暗赤色土
#> 4 典型粘土集積石灰性暗赤色土
```
