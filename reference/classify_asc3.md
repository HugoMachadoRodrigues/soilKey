# Classify a pedon under the Australian Soil Classification

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Australian Soil Classification, 3rd edition (Isbell and the
National Committee on Soil and Terrain 2021), following the online
version with its errata. **Experimental**: this release keys the 15
orders (the key to soil orders) and the suborders of all of them: the
Kurosols, Sodosols, Chromosols, Ferrosols, Dermosols, Kandosols and
Vertosols (colour classes, and the Aquic Vertosols), the Organosols (the
dominant kind of peat), the Podosols (saturation of the B, sulfuric and
hypersulfidic material), the Hydrosols (the setting, then mottling), the
Calcarosols (gypsum and the carbonate classes), the Anthroposols (the
kind of human-made material), the Arenosols and Tenosols (chernic
horizons, bleaching, hard materials within 0.5 m, carbonate, nodules,
then colour) and the Rudosols (salinity, gypsum, shells, carbic
materials, hard materials, sand, gravel, layers), and the great groups
of the Kurosols, Sodosols, Chromosols, Ferrosols, Dermosols and
Kandosols (pans in or under the B, structure, plasticity, base status,
exchangeable Ca/Mg, ESP and carbonate of the B2 or B2t), the Vertosols
(the surface), Calcarosols (what the soil directly overlies, and the B),
Podosols (the kinds of Podosol B), Organosols (drainage, acid sulfate
materials, carbonate and acidity), Hydrosols (acid sulfate materials,
the upper 0.5 m, and the order the soil would key to apart from
wetness), Arenosols, Rudosols and Tenosols (what the soil directly
overlies, nodular, gypsic and argic horizons, carbonate, texture,
gravel); the Anthroposols have none in the text; and the subgroups of
the Kurosols, Sodosols and Chromosols (chernic and vertic features,
bleaching, ferric and manganic horizons, sodicity of the lower B,
mottling of the B2t, base status, carbonate) and of the Dermosols,
Kandosols and Ferrosols (the same features read on the B2, a strongly
acid B2, argic and bauxitic horizons in the B, a snuffy A), of the
Vertosols and Calcarosols (acid sulfate materials, pans, sodicity,
acidity, carbonate and mottling of the upper 0.5 m of the solum and
below it, subplasticity, surface carbonate), the Podosols (coherence of
the B, pans) and the Organosols (what the organic materials overlie).
The other subgroups and the families follow. The name is that of the
deepest class assigned, in the book's form "Subgroup, Great Group,
Suborder Order" ("Bleached, Eutrophic, Red Chromosol"); `rsg_or_order`
keeps the order.

## Usage

``` r
classify_asc3(
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

  Optional pre-loaded rule set (`load_rules("asc3")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"asc3"`, the deepest class in `name`, the order in
`rsg_or_order`, the classes of each level in `levels`, the key trace,
and `provisional_because` (classes before the assigned ones that could
not be decided).

## Details

The ASC is largely morphological. The diagnostics read field texture
grades (`field_texture`, NCST Field Handbook codes; lab clay stands in
and is undecided near the limits), the Australian horizon notation (A1,
A2, B21t, Bhs, Bk ...; FAO/USDA designations are read by the role of
each horizon), the boundary between the A and the B, pH (strongly acid:
pHw below 5.5), ESP (sodic: 6 or more), subplasticity, free iron, grade
of pedality, carbonates and the moisture of the horizons when described
(`soil_water_status`: the Vertosols' cracks are not required in a solum
described moist), mottles, soft and hard carbonate, gypsum, the kind of
peat, acid sulfate tests, EC of a 1:5 extract, dry and moist strength,
ped size, nodules, pans and exchangeable bases. From the site it reads
`drainage_class` (Hydrosols, organic materials, Podosol suborders),
`tidal_inundation` (Hydrosols, organic materials), `surface_condition`
(a cracking surface, Vertosols; a saline one, Salic Hydrosols),
`landform`, `landform_element`, `tidal_zone`, `subaqueous` and
`salinised` (Hydrosol suborders) and `designation_system` (`"asc"` for
the Australian notation). The choices where the text is silent are
listed in
`system.file("rules", "asc3", "readings.yaml", package = "soilKey")`.

## References

Isbell, R.F. and the National Committee on Soil and Terrain (2021). The
Australian Soil Classification, 3rd ed. CSIRO Publishing, Melbourne.
Online version: www.soilscienceaustralia.org.au/asc.

## See also

[`make_asc3_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_asc3_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")
res$name
#> [1] "Bleached, Eutrophic, Red Chromosol"
```
