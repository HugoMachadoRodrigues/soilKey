# Classify a pedon under the Soil Classification System of Japan

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Soil Classification System of Japan (Japanese Society of
Pedology 2017), following its key to great groups. **Experimental**:
this release keys the ten great groups (Man-made soils, Organic Soils,
Andosols, Podzols, Fluvic soils, Red-Yellow soils, Stagnic soils,
Eutrosols, Brown Forest soils, Regosols); groups, subgroups and the soil
phases follow. Each level gives the official English name and, in
`levels$name_native`, the Japanese one.

## Usage

``` r
classify_scsj2017(
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

  Optional pre-loaded rule set (`load_rules("scsj2017")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"scsj2017"`, the deepest class in `name`, the great
group in `rsg_or_order`, the classes of each level in `levels` (with
`name_native`), the key trace, and `provisional_because` (classes before
the assigned one that could not be decided).

## Details

The SCSJ reads the Japanese horizon notation (H, O, A, E, B, C, G with
Go and Gr, R; p ploughed, g gleyed, t, w, s, h, ir, mn; a 2 or II prefix
for a lithologic discontinuity, a trailing b for buried; the FAO/USDA
notation by the same letters), moist Munsell colours, texture, organic
carbon, the phosphate absorption coefficient
(`phosphate_absorption_coef`, mg P2O5/100 g) or its substitutes (P
retention, Alo + 1/2 Feo) and the field active-aluminium test
(`active_al_test`), rubbed fibre, pH, base saturation, Fe(d), the
dipyridyl test (`childs_test`), fluidity and `n_value`, mottles and
where they are, artefacts, and the recorded origin of the layers
(`layer_origin`, `rock_origin`: fill, volcanic ejecta, alluvial
deposits). From the site it reads `land_use` or `flood_irrigated` (paddy
land), `parent_material`, `drainage_class`, `artificially_drained`,
`acid_neutralising_amendment` and `designation_system`. The choices
where the text is silent are listed in
`system.file("rules", "scsj2017", "readings.yaml", package = "soilKey")`.

## References

The Fifth Committee for Soil Classification and Nomenclature (2017).
Soil Classification System of Japan. The Japanese Society of Pedology.

## See also

[`make_scsj2017_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_scsj2017_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_scsj2017(make_scsj2017_example("andosol"), on_missing = "silent")
res$name
#> [1] "Andosols"
res$levels
#>          level code     name  name_native
#> 1 great_groups    C Andosols 黒ボク土大群
```
