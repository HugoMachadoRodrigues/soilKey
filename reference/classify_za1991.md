# Classify a pedon under the South African soil classification (1991)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the soil forms of the South African soil classification of 1991 (Soil
Classification Working Group, 1991; the "Blue Book"). **Experimental**:
the book itself is not available to soilKey; the key is read from its
open pages (the AGIS key and form pages, the general rules, the topsoil
pages), SASRI (1999) and van Huyssteen (2020). This release keys the 73
forms; the families follow.

## Usage

``` r
classify_za1991(
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

  Optional pre-loaded rule set (`load_rules("za1991")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"za1991"`: the form in `name` and `rsg_or_order` (its
code in `levels`, with `name_en`, soilKey's gloss: the sequence of its
horizons), the sequence read in `qualifiers$za_sequence` and the
identified layers in `qualifiers$za_horizons` (with `za_truncated`,
`za_land_class` and `za_nearest` when they apply), the general rules
applied, the stand-ins used and the nearest forms in `warnings`, and
`provisional_because`.

## Details

Each layer is read as one of the 30 diagnostic horizons and materials
(see
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md)):
from the name written in `diagnostic_za` (or in the designations of a
profile declared with `site$designation_system = "za"`), else by
soilKey's tests on the recorded data, three-valued, with WRB numbers
standing in, flagged, where no open source gives the book's (the podzol
B, the placic pan, the dorbank, the carbonate horizons and the colours
of the subsoil). The profile is first read by the general rules: only
horizons whose top lies above 1500 mm count; a recent deposit thinner
than 500 mm over a buried soil is set aside and the buried soil
classified, a thicker one is classified alone (`recent_deposit`, else
its origin, flagged); a profile with no topsoil whose first horizon is a
red or yellow-brown apedal B, a neocarbonate B, a dorbank or a hard
plinthic B is read as truncated, an orthic A supplied.

A form holds when its horizons are the first of the profile's, in order
(layers of no diagnostic horizon skipped), and the horizon that follows
is not one that would make another form (Hutton over hard rock is
Hutton; Hutton over a soft plinthic B is Bainsvlei). A description that
stops above 1500 mm leaves the longer forms undecided, so the shorter
one is provisional. The 500 mm rules of the form pages are tested too
(Valsrivier, Sepane, Swartland, the podzol forms; Witbank, Dundee, Namib
over a buried soil). A sequence no form has gives no form, with the
nearest forms in the warnings; an orthic A under 100 mm thick over
nothing diagnostic is the land class "rock and lithosols" (general
rules). Lichtenburg (orthic A, red apedal B, hard plinthic B) is named
by van Huyssteen (2020) but not by the AGIS key of 1991: it is keyed,
always provisional. The readings soilKey makes where the open sources
are silent are in
`system.file("rules", "za1991", "readings.yaml", package = "soilKey")`.

## References

Soil Classification Working Group (1991). Soil classification: a
taxonomic system for South Africa. Memoirs on the Agricultural Natural
Resources of South Africa No. 15. Department of Agricultural
Development, Pretoria.

## See also

[`make_za1991_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_za1991_example.md),
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_za1991(make_za1991_example("hutton"), on_missing = "silent")
res$name
#> [1] "Hutton"
res$qualifiers$za_sequence
#> [1] "OA - RAB"
```
