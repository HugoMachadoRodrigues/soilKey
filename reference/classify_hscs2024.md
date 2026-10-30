# Classify a pedon under the Hungarian diagnostic soil classification

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Hungarian diagnostic soil classification of Micheli et al.
(2024, second approximation), following its key to the soil types.
**Experimental**: this release keys the 15 types (Laptalaj, Antropogen
talaj, Koves-sziklas vaztalaj, Kozethatasu talaj, Szolonyec talaj,
Szoloncsak talaj, Duzzado agyagtalaj, Reti talaj, Karbonat talaj,
Mezosegi talaj, Agyagbemosodasos talaj, Homoktalaj, Barnafold,
Hordalektalaj, Foldeskopar). Since v0.9.316 the name also carries the
subtype properties of the type's table (before the type, the first with
a capital letter) and its variety properties (after it, in parentheses),
in the table's order, with the depth modifiers of 8.7 (felszintol,
sekelyen, kozepmelyen, melyben) on properties defined by a layer: e.g.
"Savanyu, humuszszegeny Agyagbemosodasos talaj (sekelyen
kozettormelekes)", with the accents.

## Usage

``` r
classify_hscs2024(
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

  Optional pre-loaded rule set (`load_rules("hscs2024")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"hscs2024"`, the full name in `name` and the type in
`rsg_or_order`; `levels` has the type with its English gloss and one row
per subtype and variety property named; `qualifiers` holds the subtypes,
the varieties and `status`, every property of the type's table with its
verdict (TRUE, FALSE, NA), depth and label; and the key trace and
`provisional_because` (types before the assigned one that could not be
decided).

## Details

The names are the Hungarian singular, as the text's naming example
writes them; `levels$name_en` gives an English gloss. The key reads the
FAO (2006) horizon notation the text adopts (an l or r suffix shows
reductive conditions; i slickensides; k carbonates; n sodium; p
ploughing; t clay accumulation; u artefacts; w a cambic horizon), moist
and dry Munsell colours, texture (Hungarian names of the USDA triangle
the text prints, English names, or lab clay, silt and sand), organic
carbon, bulk density, CaCO3, effervescence, secondary carbonates, base
saturation, exchangeable Na, Mg and the CEC, structure, slickensides,
cracks, clay films, coarse fragments, artefacts, the layer origin, the
dipyridyl test (`childs_test`), redoximorphic features, and the total
water-soluble salts (`soluble_salts_pct`, Hungarian method). From the
site it reads `land_use`, `methane_odour`, `geomembrane`,
`mechanical_mixing_depth_cm` and `anthropic_truncation`. Features a
describer records when met (artefacts, mottles, secondary carbonates,
cracks) are absent when not recorded. The choices where the text is
silent are listed in
`system.file("rules", "hscs2024", "readings.yaml", package = "soilKey")`.

## References

Micheli E., Fuchs M., Lang V., Csorba A., Dobos E., Szegi T., Kele G.
(2024). A diagnosztikus szemleletu hazai talajosztalyozasi rendszer.
Utmutato. Masodik kozelites. Magyar Agrar- es Elettudomanyi Egyetem,
Godollo.

## See also

[`make_hscs2024_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_hscs2024_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_hscs2024(make_hscs2024_example("steppe"), on_missing = "silent")
res$name
#> [1] "Középmélyen erősen karbonátos, mély humuszos rétegű, igen erősen humuszos, fekete Mezőségi talaj (iszapos vályog)"
res$levels
#>                        level                 code                          name
#> 1                      types                  MEZ                Mezőségi talaj
#> eros_karbonatos      subtype      eros_karbonatos középmélyen erősen karbonátos
#> mely_humuszos_retegu subtype mely_humuszos_retegu          mély humuszos rétegű
#> igen_erosen_humuszos subtype igen_erosen_humuszos          igen erősen humuszos
#> fekete               subtype               fekete                        fekete
#> textura              variety              textura                iszapos vályog
#>                                           name_en
#> 1                    Steppe soil (chernozem-like)
#> eros_karbonatos               strongly calcareous
#> mely_humuszos_retegu      with a deep humic layer
#> igen_erosen_humuszos          very strongly humic
#> fekete                                      black
#> textura                             texture class
```
