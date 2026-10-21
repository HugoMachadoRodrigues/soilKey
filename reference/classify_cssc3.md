# Classify a pedon under the Canadian System of Soil Classification

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
through the Canadian System of Soil Classification, 3rd edition (Soil
Classification Working Group 1998). **Experimental**: this release keys
the ten orders (Key to Soil Orders, Ch 3, pp. 33-34), their 31 great
groups (Key to Soil Great Groups, pp. 35-36) and their 231 subgroups
(Key to Soil Subgroups, pp. 37-51). The name is the deepest class
assigned: the subgroup (e.g. "Terric Mesisol"), or the great group (e.g.
"Solonetz") when no subgroup can be given: the Solonetzic subgroups are
named by the colour of the A, and the key has no other subgroup for a
Solonetzic soil without an A (one eroded down to its solonetzic B); the
order is in `rsg_or_order` and every level in `levels`.

## Usage

``` r
classify_cssc3(
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

  Optional pre-loaded rule set (`load_rules("cssc3")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"cssc3"`, the deepest class assigned in `name`, the
order in `rsg_or_order`, every level in `levels`, the key trace, and
`provisional_because`.

## Details

The Canadian keys are written largely in terms of horizon designations,
which the book defines partly by measurable properties. Each diagnostic
reads the measured properties first (podzolic B chemistry with
pyrophosphate-extractable Fe and Al, the Bt clay increase, the
solonetzic B Ca/Na ratio, the chernozemic A, the gley colours) and the
surveyor's designations in the Canadian notation second (Bt, Bf, Bm,
Btj, Bn, Bv, Bss, Bg, Cz...). When the assigned order rests on
designations alone, a warning says so. The choices where the book is
silent are listed in
`system.file("rules", "cssc3", "readings.yaml", package = "soilKey")`.

The chernozemic A horizon includes a soil-climate criterion (mean annual
soil temperature at least 0 C, moisture subclass drier than humid), read
from `site$mean_annual_soil_temp_C` and `site$soil_moisture_subclass`,
or approximated from the USDA regimes. Without it the Chernozemic order
is undecided and any order keyed after it is reported as provisional
(`$provisional_because`). The Gray and Dark Gray subgroups of the Solods
and Solodized Solonetz also ask for a subhumid climate
(`site$soil_moisture_subclass`).

## References

Soil Classification Working Group (1998). The Canadian System of Soil
Classification, 3rd ed. Agriculture and Agri-Food Canada Publication
1646. NRC Research Press, Ottawa.

## See also

[`make_cssc3_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_cssc3_example.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_cssc3(make_cssc3_example("podzolic"), on_missing = "silent")
res$name
#> [1] "Orthic Humo-Ferric Podzol"
```
