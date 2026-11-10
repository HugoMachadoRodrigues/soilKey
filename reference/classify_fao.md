# Classify a pedon in the FAO-Unesco soil legend

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the FAO-Unesco legend of the Soil Map of the World.
**Experimental**: this release keys the 28 major soil groupings of the
Revised Legend (FAO 1988, World Soil Resources Report 60; the text of
the ISRIC reprint of 1997, Technical Paper 20), chapter X, pp. 75-89,
and their 153 soil units, in the key's order (Histosols, Anthrosols,
Leptosols, Vertisols, Fluvisols, Solonchaks, Gleysols, Andosols,
Arenosols, Regosols, Podzols, Plinthosols, Ferralsols, Planosols,
Solonetz, Greyzems, Chernozems, Kastanozems, Phaeozems, Podzoluvisols,
Gypsisols, Calcisols, Nitisols, Alisols, Acrisols, Luvisols, Lixisols,
Cambisols), on the 16 diagnostic horizons and 27 diagnostic properties
of chapters IV and V read on the data. Under its grouping a soil keys to
the first soil unit whose gate passes, in the key's order; the last unit
of each grouping is the key's "Other ..." residual (Haplic, Eutric, and
the Terric Histosols and Urbic Anthrosols, whose descriptions are shown,
not tested). Since v0.9.386 the result also gives the 16 phases of
chapter VIII (pp. 67-71) and the texture and slope classes of the map
symbol (Appendix 2, p. 107) in `$phases`, beside the unit, which they
never change. Since v0.9.389, `edition = "1974"` keys the Legend of 1974
(FAO-Unesco, Soil Map of the World, Vol. I, Unesco Paris), pp. 43-53:
its 26 major soil groups in the key's order (Histosols, Lithosols,
Vertisols, Fluvisols, Solonchaks, Gleysols, Andosols, Arenosols,
Regosols, Rankers, Rendzinas, Podzols, Ferralsols, Planosols, Solonetz,
Greyzems, Chernozems, Kastanozems, Phaeozems, Podzoluvisols, Xerosols,
Yermosols, Nitosols, Acrisols, Luvisols, Cambisols) and their 106 soil
units (the Lithosols, Rankers and Rendzinas are single units), on the
diagnostic horizons and properties of 1974 read on the data; its
registry id is `fao1974`
([`classify_fao1974`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao1974.md)).
Since v0.9.390 its result gives in `$phases` the 12 phases of 1974 (pp.
5-7: stony, lithic, petric, petrocalcic, petrogypsic, petroferric,
phreatic, fragipan, duripan, saline, sodic, cerrado) and the texture and
slope classes of its map symbol (pp. 4-5), in the same form.

## Usage

``` r
classify_fao(
  pedon,
  edition = c("1988", "1974"),
  rules = NULL,
  on_missing = c("warn", "silent", "error"),
  gapfill = FALSE
)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- edition:

  `"1988"` (the Revised Legend, default) or `"1974"` (the Legend of
  1974).

- rules:

  Optional pre-loaded rule set (`load_rules("fao1988")`, or
  `load_rules("fao1974")` with `edition = "1974"`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"fao1988"` (`"fao1974"` for the Legend of 1974);
`name` holds the soil unit as printed ("Haplic Luvisols") and
`rsg_or_order` the major soil grouping ("Luvisols"); `levels` has one
row per level (`major_groups`, `units`) with the code ("LV", "LVh"),
name, page of the key and soilKey's Portuguese gloss (`name_pt`); the
key trace of both levels and `provisional_because`; and `phases`, a
`data.frame` with one row per phase and one per class of the map symbol
(`kind` `"phase"`, `"texture_class"`, `"slope_class"`), its `status`
(`"present"`, `"absent"`, `"undecided"`, or `"not shown"` for a unit the
text excludes), `present` and `criterion` (`TRUE`, `FALSE`, `NA`), page,
basis, flags and the attributes that would decide an undecided phase
(`missing`).

## Details

The tests are three-valued: a measurement not recorded leaves its
criterion undecided, a field observation not recorded is absent. A
grouping or unit tested before the assigned one that the data leave
undecided makes the result provisional (`provisional_because`,
`"major_groups:<code>"` or `"units:<code>"`: the candidates). The key
has no residual grouping after the Cambisols: a soil that no gate takes
is `"[Not classified in the FAO-Unesco Revised Legend (1988)]"`, in
brackets because the name is soilKey's, with the gates that failed in
the warnings; when no gate passes but some are undecided, the name lists
them in brackets as the candidates. Depths are read from the soil
surface, H and O horizons included. The readings where the text is
silent are in
`system.file("rules", "fao1988", "readings.yaml", package = "soilKey")`.

## References

FAO (1988). FAO/Unesco Soil Map of the World, Revised Legend, with
corrections and updates. World Soil Resources Report 60, FAO, Rome.
Reprinted with updates as Technical Paper 20, ISRIC, Wageningen, 1997.

## See also

[`classify_fao1988`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao1988.md),
[`classify_fao1974`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_fao1974.md),
[`make_fao_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_fao_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_fao(make_fao_example("LV"), on_missing = "silent")
res$name
#> [1] "Albic Luvisols"
res$rsg_or_order
#> [1] "Luvisols"
res$levels
#>          level code           name                             name_pt page
#> 1 major_groups   LV       Luvisols               Luvissolos (Luvisols)   88
#> 2        units  LVa Albic Luvisols Luvissolos Álbicos (Albic Luvisols)   88
res$phases[res$phases$status != "absent", c("kind", "code", "name", "status")]
#>             kind     code            name    status
#> 6          phase  inundic   Inundic phase undecided
#> 9          phase phreatic  Phreatic phase undecided
#> 17 texture_class        2 Medium textured   present
#> 18   slope_class     <NA>            <NA> undecided
classify_fao(make_fao_example("Lc", edition = "1974"), edition = "1974",
             on_missing = "silent")$name
#> [1] "Chromic Luvisols"
```
