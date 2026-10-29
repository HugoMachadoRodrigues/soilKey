# Classify a pedon under the New Zealand Soil Classification

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the New Zealand Soil Classification, 4th edition (Manaaki
Whenua - Landcare Research 2026), following its Key to Soil Orders and
keys to groups and subgroups. **Experimental**: this release keys the 15
orders (Organic, Gley, Ultic, Podzol, Allophanic, Pumice, Melanic,
Semiarid, Oxidic, Granular, Pallic, Brown, Anthropic, Recent, Raw
Soils), their 77 groups (since 0.9.298, e.g. Orthic Brown Soils,
Perch-gley Pallic Soils) and their 336 subgroups (keyed from 0.9.299 to
0.9.302, e.g. Peaty Orthic Gley Soils, Humose Orthic Podzol Soils,
Mottled Fragic Pallic Soils, Typic Orthic Brown Soils); seven groups
have no subgroups and keep the group name. The Cryic Soils of the
appendix are not keyed.

## Usage

``` r
classify_nzsc4(
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

  Optional pre-loaded rule set (`load_rules("nzsc4")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"nzsc4"`, the deepest class in `name` (the subgroup,
or the group where no subgroup is keyed yet), the order in
`rsg_or_order`, the classes of each level in `levels`, the key trace,
and `provisional_because` (classes before the assigned one that could
not be decided).

## Details

The NZSC reads horizon notations and field tests. The diagnostics read
the New Zealand notation (Ah, E, Bw, Bt, Bg, Bh, Bs, Bo, Bx, Bw(g), Bhm,
Bsm, Blm, CR, R, Of, Om, Oh, Lm, Lh, an e prefix for anthropic soil
material; FAO designations are read by the same letters), moist Munsell
colours, texture (`field_texture` words or codes, or lab clay, silt and
sand), pH, P retention (`phosphate_retention_pct`), dry bulk density,
volcanic glass, coarse fragments and their origin, structure, clay
coatings, carbonate, ESP, and the field tests `naf_test`
(reactive-aluminium test: none, slight, moderate, strong),
`childs_test`, `sensitivity_class`, `fluidity_class`, `stickiness` and
`strength_moist`. From the site it reads `drainage_class`,
`parent_material` (Melanic Soils), `anthropic_truncation` and
`overthickened_a` (the note to the key on overthickened A horizons), and
`designation_system`. The groups also read the packing
(`packing_degree`), Ksat (`ksat_mm_h`), penetration resistance, where
the redox segregations are (`redox_segregations_location`), sulfate S,
roots, a Maori cultural layer (`maori_cultural_layer`) and the colour of
coatings; and, on the site, `soil_temp_30cm_C`,
`mean_annual_air_temp_C`, `surface_water`, `methane_odour`,
`mechanical_mixing_depth_cm` and `artificially_drained`. The subgroups
also read sulfidic S (`sulfidic_s_pct`), the EC of a 1:5 extract
(`ec_1to5_dS_m`), the share of Sphagnum in the fibres
(`sphagnum_fibres_pct`), a shelly layer (`shelly_layer`), exchangeable
Ca and Mg, clay films, the colour of coatings on pores
(`pore_coatings_hue_moist`, `pore_coatings_value_moist`,
`pore_coatings_chroma_moist`), the weathering of rock fragments
(`weathering_stage`, `weathered_fragments_pct`) and the cementation
class; on the site, `mechanical_mixing_purpose` and
`acid_neutralising_amendment` (Mixed and Fill Anthropic Soils). Features
a describer records when met (stratification, nodules, cracks, refuse or
cultural layers, permafrost, surface water) are absent when not
recorded. The choices where the text is silent are listed in
`system.file("rules", "nzsc4", "readings.yaml", package = "soilKey")`.

## References

Manaaki Whenua - Landcare Research (2026). New Zealand Soil
Classification. 4th ed.
[doi:10.7931/dl1-lrss-4-2026](https://doi.org/10.7931/dl1-lrss-4-2026) .

## See also

[`make_nzsc4_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_nzsc4_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_nzsc4(make_nzsc4_example("brown"), on_missing = "silent")
res$name
#> [1] "Typic Orthic Brown Soils"
```
