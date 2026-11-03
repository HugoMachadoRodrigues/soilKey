# Classify a pedon under the Russian soil classification (Field Guide, 2008)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the trunks and divisions of the Russian soil classification, in the
version of its Field Guide (Polevoi opredelitel' pochv Rossii, Dokuchaev
Soil Science Institute, 2008). **Experimental**: this release keys the
five trunks (key 2.2: the organogenic, synlithogenic, postlithogenic and
weakly developed soils, and trunk E, not a soil: rock outcrops,
technogenic surface formations, or a deposit that is not humified over a
buried soil) and the 27 divisions (key 2.3), and the types of every
division (the profile formulas of chapter 4: CA to CJ in v0.9.338, CK to
CT in v0.9.339, trunks A, B and D in v0.9.342). The chemozems CU have no
types: their name is built from the pollutants and the soil they were
(v0.9.342). The subtypes follow in later releases.

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
(the type, else the division, else the trunk), the trunk in
`rsg_or_order`, the trunk, division and type in `levels` (with
`name_native`, the Russian name), the key trace, the notes on a profile
that is not a soil or whose combination of horizons the Field Guide does
not provide for in `warnings`, and `provisional_because` (classes before
the assigned one that could not be decided).

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

The type is named by the formula of its profile: the profile is matched
against every formula of its division (see
[`kidpr2008_types`](https://hugomachadorodrigues.github.io/soilKey/reference/kidpr2008_types.md)),
closed on the horizons the division's formulas write, most specific
first and agro before natural. A profile no formula matches keeps its
division, the type unassigned, with the nearest formulas in the warnings
and in the trace (phase `types_nearest`).

A chemozem (CU, an X horizon: pollutants of the extremely hazardous
category within 30 cm) is named, as p. 134 asks, by the kind of
pollution and the original soil: the `pollutants` of the layers of the X
horizon, and the soil the profile keys to with the X masked (the X
overlays and the contamination records removed), down to its type. The
type level is soilKey's `"[CU:<code of the original soil>]"`, named
"Chemozems polluted with Cu, Ni, Co; original soil: Sod-podzolic"; the
original soil keyed is in the trace (phase `chemozem`), and an original
soil left undecided makes the name provisional.

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
#> [1] "Sod-podzolic"
res$levels
#>       level   code                         name
#> 1    trunks      C         Postlithogenic soils
#> 2 divisions     CA Texture-differentiated soils
#> 3     types [CA04]                 Sod-podzolic
#>                          name_native
#> 1               Постлитогенные почвы
#> 2 Текстурно-дифференцированные почвы
#> 3                Дерново-подзолистые
```
