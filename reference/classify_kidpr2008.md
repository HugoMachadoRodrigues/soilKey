# Classify a pedon under the Russian soil classification (Field Guide, 2008)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the trunks and divisions of the Russian soil classification, in the
version of its Field Guide (Polevoi opredelitel' pochv Rossii, Dokuchaev
Soil Science Institute, 2008). **Experimental**: this release keys the
five trunks (key 2.2: the organogenic, synlithogenic, postlithogenic and
weakly developed soils, and trunk E, not a soil: rock outcrops,
technogenic surface formations, or a deposit that is not humified over a
buried soil) and the 27 divisions (key 2.3). The types (profile
formulas) and subtypes follow in later releases.

## Usage

``` r
classify_kidpr2008(
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

  Optional pre-loaded rule set (`load_rules("kidpr2008")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"kidpr2008"`, the deepest class assigned in `name`
(the division, or the trunk when no division is assigned), the trunk in
`rsg_or_order`, the trunk and division in `levels` (with `name_native`,
the Russian name), the key trace, the notes on a profile that is not a
soil or whose combination of horizons the Field Guide does not provide
for in `warnings`, and `provisional_because` (classes before the
assigned one that could not be decided).

## Details

The keys read the diagnostic horizons of the profile: the Russian
notation where the profile is written in it (declared with
`site$designation_system = "kidpr"`, or recognised by an index only it
writes: AY, EL, BEL, BHF, BCA, PU, TUR ...), otherwise the key to
horizons 2.1 run on the recorded data (see
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md)).
From them the keys read whether a horizon is present, whether it starts
the profile (at the surface, or under a deposit of 40 cm or less),
whether it lies directly over another, whether the profile consists of
one horizon only, the depth of hard rock, a deposit at the surface (TPO
and the deposits the key to horizons finds on the data included), the
peat body of trunk A (more than 50 cm, the agro-peat horizons included)
and the thickness of a turbated horizon; of the site, a river floodplain
on modern alluvium (`landform`, `landform_element`, `layer_origin`,
`rock_origin`, `parent_material`, the alluvial mark of the notation; a
terrace is no floodplain) and the contaminated horizon X
(`contamination_hazard`). The ochre horizon BAN is formed in volcanic
ash, so it makes the volcanic soils alone. With several organic and
humus horizons at the surface the thickest is the diagnostic one (p.
37), for step 5 of the key to trunks as for the divisions; FAO and WRB
transitions (AB, BC, E/B) in which the key on the data finds no
diagnostic horizon are transitions, as those of the Russian notation; a
missing depth leaves the step that needs it undecided.

The keys are branching trees; soilKey flattens them into ordered lists
in which each gate is the conjunction of the conditions of the book's
path to the class, and repairs only the gaps the book's own types prove;
the other combinations the key leaves without an exit are "not provided
by the Field Guide of 2008": the trunk is assigned and the division left
unassigned, with a note. Names: an English gloss in `name`, the Russian
of the book in `levels$name_native`. The readings where the text is
silent are listed in
`system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey")`.

## References

Polevoi opredelitel' pochv Rossii \[Field Guide of Russian Soils\]
(2008). Dokuchaev Soil Science Institute, Moscow.

## See also

[`make_kidpr2008_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_kidpr2008_example.md),
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_kidpr2008(make_kidpr2008_example("CA"), on_missing = "silent")
res$name
#> [1] "Texture-differentiated soils"
res$levels
#>       level code                         name
#> 1    trunks    C         Postlithogenic soils
#> 2 divisions   CA Texture-differentiated soils
#>                          name_native
#> 1               Постлитогенные почвы
#> 2 Текстурно-дифференцированные почвы
```
