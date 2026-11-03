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
(v0.9.342). The subtypes of every division's list follow the type
(v0.9.345).

## Usage

``` r
classify_kidpr2008(
  pedon,
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE,
  subtypes = TRUE,
  species = subtypes
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

- subtypes:

  Name the subtypes after the type (default `TRUE`); `FALSE` stops at
  the type.

- species:

  Name the species of chapter 5.1 after the subtypes (default: as
  `subtypes`).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"kidpr2008"`, the deepest class assigned in `name`
(the type with its subtypes, else the division, else the trunk), the
trunk in `rsg_or_order`, the trunk, division, type, subtypes and species
in `levels` (with `name_native`, the Russian name), the key trace, the
notes on a profile that is not a soil or whose combination of horizons
the Field Guide does not provide for in `warnings`, and
`provisional_because` (classes before the assigned one that could not be
decided).

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

The subtypes (v0.9.345): each division has one list of subtypes in
chapter 4 (26 lists, 558 entries; the chemozems have none), each the
index of a genetic feature of section 3.2, read by the feature tests
(the recorded data first, the Russian notation where they say nothing).
The soil is named by its type and every subtype of the list that passes,
in the order of the list, comma-separated and in the plural as printed
("Podzolistye gleevatye, yazykovatye"): the subtypes are the rows of
level `"subtype"` of `levels` (code: soilKey's key of the entry; `name`
the English gloss, `name_native` the Russian), the full name in English
in `name` and in Russian in `qualifiers$name_native`. When every entry
is FALSE the soil is of the typical subtype, soilKey's "\[tipichnye\]"
("\[typical\]", code NA); when some are undecided the name is
provisional, the undecided entries in `provisional_because`
("subtype:\<key\>") and in a warning. A feature the formula of the type
already writes (the g of the gley types, the BF of CB05, the permafrost
sign of the cryozems ...) is not named again, and a sentence of 3.2 that
names a single type restricts its subtype to it. Each entry, with its
evidence, is in the trace (phase `subtype`). A soil whose type is
unassigned gets no subtypes. The lists are in
`system.file("rules", "kidpr2008", "subtypes.yaml", package = "soilKey")`.

The species (v0.9.347): the twelve tables of chapter 5.1, the classes of
the thickness of the humus, peat and agro horizons, of the depth and
place of gleying, of the lower boundary of the bleached horizon, of the
layer over the solonetzic horizon, of the depth of the carbonates and of
the soluble salts and, in the organogenic trunk, of the decomposition,
the botanical composition (`peat_botanical_composition`) and the
thickness of the peat. Each table that applies to the type is read, and
its class follows the subtypes in the name, in the order of the tables
("Dernovo-podzolistye \[tipichnye\], srednemoshchnye,
glubokoelyuvial'nye"; the bleached species in the prefix form of p.
136): rows of level `"species"` of `levels` (code `"<table>:<class>"`),
`qualifiers$species` and `species_native`. A depth not recorded, an
undecided layer at the edge of a horizon, or a horizon that reaches the
bottom of the description when a deeper class is possible, leaves the
table undecided: it is not named, it is listed in
`qualifiers$species_undecided` and in a note, and the name stays as it
is. The tables are in
`system.file("rules", "kidpr2008", "species.yaml", package = "soilKey")`;
the varieties and ranks (5.2, 5.3) are not keyed.

The 1977 analogue (v0.9.347): when the type is assigned, the rows of the
correlation table of chapter 6 that stand for it are added as a note (a
line in `warnings`) and as `qualifiers$analogue_1977` (the 2004 name and
formula, the page, whether the soils were distinguished in 1977 and the
units of 1977 with their level and extent), never changing the
classification; where the table says the soils were not distinguished or
has no row for the type, the note says so. The table is in
`system.file("rules", "kidpr2008", "correlation_1977.yaml", package = "soilKey")`.

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
#> [1] "Sod-podzolic [typical], medium-thick, deep-eluvial"
res$levels
#>       level               code                         name
#> 1    trunks                  C         Postlithogenic soils
#> 2 divisions                 CA Texture-differentiated soils
#> 3     types             [CA04]                 Sod-podzolic
#> 4   subtype               <NA>                    [typical]
#> 5   species humus_light:medium                 medium-thick
#> 6   species      bleached:deep                 deep-eluvial
#>                          name_native
#> 1               Постлитогенные почвы
#> 2 Текстурно-дифференцированные почвы
#> 3                Дерново-подзолистые
#> 4                         [типичные]
#> 5                       среднемощные
#> 6                 глубокоэлювиальные
```
