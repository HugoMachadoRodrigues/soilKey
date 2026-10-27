# Family of a soil under the Australian Soil Classification

Places a soil in the family classes of its order (Australian Soil
Classification, 3rd edition, the 'Family Criteria' of each order page).
The family is not a level of the key: each criterion is a class of its
own, given by a one- or two-letter code. Most orders use the thickness
of the A1 horizon (of the whole A horizon in the Kurosols, Sodosols and
Chromosols and in the Kurosolic, Sodosolic and Chromosolic Hydrosols, as
the book's general rule has it), with any organic horizon over it;
gravel (2-60 mm) of the surface and A1 horizon (the larger of the
estimate of the ground surface, `site$surface_gravel_pct` or
`site$surface_coarse_fragments_pct`, and that of the A1); the field
texture of the A1 (its major part, peaty under organic surface horizons)
and the most clayey field texture of the B; soil depth; and water
repellence of the surface soil (`water_repellence`). The Vertosols use
laboratory clay (upper 0.1 m and the B), the Calcarosols the depth of
the first Bk horizon, the Arenosols and Rudosols the surface soil, and
the Organosols the kind and thickness of their organic materials and the
texture under them. Soil depth is the depth to rock, a cemented pan (not
a thin ironpan), another cemented layer or saprolite; otherwise, in a
soil with a B horizon, the depth to the C below it, and in a soil
without one, the depth described. The Anthroposols have no family, nor
do the Subtidal, Subaqueous and Intertidal Hydrosols. The readings where
the text is silent are in
`system.file("rules", "asc3", "readings.yaml", package = "soilKey")`
(topic "Families").

## Usage

``` r
classify_asc3_family(pedon, order = NULL, suborder = NULL, great_group = NULL)
```

## Arguments

- pedon:

  A
  [`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md).

- order, suborder, great_group:

  Codes of the soil's classes (as in `levels` of a
  [`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md)
  result). When `order` is `NULL` the soil is classified first.

## Value

A named list of
[`FamilyAttribute`](https://hugomachadorodrigues.github.io/soilKey/reference/FamilyAttribute.md),
one per criterion of the order that applies, in the order of the book's
family name: `value` is the class name (`NULL` when undecided or when no
class fits), `evidence$code` the class code (`"YY"` undecided, `"ZZ"` no
class of the order fits). An empty list when the soil has no family.

## References

Isbell, R.F. and the National Committee on Soil and Terrain (2021). The
Australian Soil Classification, 3rd ed. CSIRO Publishing, Melbourne.
Online version: www.soilscienceaustralia.org.au/asc.

## See also

[`classify_asc3`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_asc3.md).

## Examples

``` r
fam <- classify_asc3_family(make_asc3_example("chromosols"))
vapply(fam, function(a) a$evidence$code, "")
#>        thickness           gravel       a1_texture        b_texture 
#>              "C"              "E"              "L"              "O" 
#>            depth water_repellence 
#>              "W"             "YY" 
```
