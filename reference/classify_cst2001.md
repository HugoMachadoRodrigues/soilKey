# Classify a pedon under the Chinese Soil Taxonomy (2001)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Chinese Soil Taxonomy (Cooperative Research Group on Chinese
Soil Taxonomy, 2001, the English edition published by Science Press).
**Experimental**: this release keys the 14 orders (key 5.1: Histosols,
Anthrosols, Spodosols, Andosols, Ferralosols, Vertosols, Aridosols,
Halosols, Gleyosols, Isohumosols, Ferrosols, Argosols, Cambosols and
Primosols), their 39 suborders and their 138 groups (key 5.2), with
soilKey's residuals `[E2] [Other Ferralosols]`,
`[A1.4] [Other Permagelic Histosols]` and
`[A2.5] [Other Orthic Histosols]` where the book prints none (the name
stops there, with a note). Since v0.9.375 it keys the subgroups of
orders A to G (176, with `[A1.3.x]` Lithic Hemi-Permagelic Histosols,
listed in Appendix 1 only, at its Appendix position); their criteria are
written as presets (`inst/rules/cst2001/presets.yaml`), expanded when
the rules are loaded. The groups of orders H to N stop at the group,
with a note, until their subgroups follow in a later release.

## Usage

``` r
classify_cst2001(
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

  Optional pre-loaded rule set (`load_rules("cst2001")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"cst2001"`, the subgroup (or the deepest class
assigned) in `name`, the order in `rsg_or_order`, the classes of each
level in `levels` (with `name_pt` and `book_provisional`), the key trace
with the horizon panel, `provisional_because`, and, when a class was
assigned on a provisional reading, in `candidates` the class the key
would reach without it (`kind` "alternative").

## Details

The key is read in the book's order, the first class whose criteria pass
assigned, as the book prescribes (each class lacks the properties of the
classes before it). The criteria read the 33 diagnostic horizons and the
25 diagnostic characteristics of chapter 3 on the recorded data (see
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md)
with `"cst2001"`), with their weaker evidence grades where the book
gives them, the soil moisture and temperature regimes (recorded on the
site, or from the aridity index, the site's records of paddy,
groundwater and permafrost, and the air temperature), and the few
criteria the key defines where it uses them. Every test is three-valued:
a measurement not recorded leaves it undecided, a field observation not
recorded is absent. A class tested before the assigned one that could
not be decided makes the name provisional (`provisional_because`); a
class assigned on a reading the book leaves open (gley colours without a
ferrous-iron test, cracks observed for their duration) carries a
warning; a class the book itself marks provisional is noted as such
(`levels$book_provisional`).

Names are the English of the book, in the plural of the key ("Permagelic
Histosols"), with soilKey's Portuguese gloss in `levels$name_pt`. The
approximate equivalents of the order in US Soil Taxonomy (1999) and the
WRB (1998), Tables 4.2 and 4.3, are added as a note
(`qualifiers$correlation_cst` and a line in `warnings`); they never
change the classification. A residual suborder or group named for the
udic regime (the Udic Andosols, Vertosols, Isohumosols, Ferrosols,
Argosols and Cambosols, the Udi- groups) or for the aquic one (the
Aqui-Orthic Halosols) reached by a soil whose regime read is another
keeps its class, with a note. The diagnostic horizons and
characteristics of each layer are in the trace (phase `horizons`). The
readings where the text is silent are listed in
`system.file("rules", "cst2001", "readings.yaml", package = "soilKey")`.

## References

Cooperative Research Group on Chinese Soil Taxonomy (2001). Chinese Soil
Taxonomy. Science Press, Beijing and New York.

## See also

[`make_cst2001_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cst2001_example.md),
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_cst2001(make_cst2001_example("K3"), on_missing = "silent")
res$name
#> [1] "Argi-Udic Ferrosols"
res$levels
#>       level code                name                      name_pt
#> 1    orders    K           Ferrosols      Ferrossolos (Ferrosols)
#> 2 suborders   K3      Udic Ferrosols           Ferrossolos údicos
#> 3    groups K3.4 Argi-Udic Ferrosols Ferrossolos údicos argílicos
#>   book_provisional
#> 1            FALSE
#> 2            FALSE
#> 3            FALSE
```
