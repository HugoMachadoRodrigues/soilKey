# Classify a pedon under the German KA5 soil systematics (2005)

Keys a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md)
to the divisions (Abteilungen), classes (Klassen) and types (Typen) of
the German soil systematics of the Bodenkundliche Kartieranleitung, 5th
edition (Ad-hoc-AG Boden 2005, KA5), by its key Tab. 51.
**Experimental**: this release keys the 4 divisions, 21 classes and 56
types, and the 138 subtypes of the 28 terrestrial types from the type
texts (pp. 202-237); the subtypes of the semiterrestrial, subhydric and
Moore types, the overlays ("X ueber Y") and the varieties are not keyed
yet.

## Usage

``` r
classify_ka5(
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

  Optional pre-loaded rule set (`load_rules("ka5")`).

- on_missing:

  One of `"warn"` (default), `"silent"`, `"error"`.

- gapfill:

  Opt-in within-pedon gap-fill, as in
  [`classify_usda`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_usda.md).

## Value

A
[`ClassificationResult`](https://hugomachadorodrigues.github.io/soilKey/reference/ClassificationResult.md)
with `system_id` `"ka5"`: the deepest class assigned in `name`, the
division in `rsg_or_order`, the division, class and type in `levels`,
the key trace, and `provisional_because`.

## Details

The subtypes (Hugo's decision Q21): inside a type the transitional
subtypes (X-Y, a subtype of the type Y with the features of X) are
tested first in printed order, then the deviation subtypes in printed
order, the Norm subtype last as the fallback; the first that passes
names the subtype, and every subtype of the type that passes is listed
in `trace$subtypes_passing`. Each subtype is one of its printed formulas
matched on the horizons
([`match_horizon_sequence`](https://hugomachadorodrigues.github.io/soilKey/reference/match_horizon_sequence.md)),
its depth windows from the GOF and its numbers (base saturation, clay
differences, Cp/Fep, slope ...). The Rigosol and the Treposol take an
open subtype in brackets for the type still recognisable under the R
("\[BB-YY\]", Q22); the Treposol has no Norm subtype.

The key reads the horizons of the profile
(`identify_horizons(pedon, "ka5")`): the KA5 notation where the profile
is written in it (declared with `site$designation_system = "ka5"`, or
recognised by a sign only it writes: the degree sign, a lower-case
prefix before a master such as lC, fAh, aM, tmGo, Sw, Sd, Axh, Bfv ...),
otherwise soilKey's order of tests on the recorded data. The last symbol
of a designation carries the horizon (Bv-Go is a Go, Go-Bv a Bv). Depths
are read from the GOF, the top of the first horizon that is not L, O,
Of, Oh or Ovh (p. 79). The order and gates are Tab. 51's (the Moore, the
subhydric soils, the semiterrestrial soils, the terrestrial soils), with
the numbers of the type texts where the compressed key differs and two
rules of the figures Abb. 22 and 23 (Hugo's decisions Q5 and Q6). A gate
the data leave open makes the result provisional
(`provisional_because`); a type that holds only on the designation, a
measurement missing (the Fahlerde without its clay, C13), or by the
key's order with the record missing (the Rigosol without the kind of
deep cultivation, Q23), is listed there too.

Names as printed (German) in `name` and `levels$name`, with soilKey's
English gloss in `levels$name_en`. The horizon panel (one row per layer:
depths from the GOF, the horizon read, the route notation, data or both,
the candidates, the evidence) is in `trace$horizon_panel`, the full
frame in `trace$horizons`, the consistency flags of the figures in
`trace$figures`. The readings where the text is silent are listed in
`system.file("rules", "ka5", "readings.yaml", package = "soilKey")`.

## References

Ad-hoc-AG Boden (2005). Bodenkundliche Kartieranleitung. 5. Auflage.
Bundesanstalt fuer Geowissenschaften und Rohstoffe, Hannover.

## See also

[`make_ka5_example`](https://hugomachadorodrigues.github.io/soilKey/reference/make_ka5_example.md),
[`identify_horizons`](https://hugomachadorodrigues.github.io/soilKey/reference/identify_horizons.md),
[`soilkey_systems`](https://hugomachadorodrigues.github.io/soilKey/reference/soilkey_systems.md).

## Examples

``` r
res <- classify_ka5(make_ka5_example("BB"), on_missing = "silent")
res$name
#> [1] "Normbraunerde"
res$levels
#>       level           code                name             name_en
#> 1 divisions [terrestrisch] Terrestrische Böden   Terrestrial soils
#> 2   classes              B          Braunerden        Brown earths
#> 3     types             BB           Braunerde         Brown earth
#> 4  subtypes            BBn       Normbraunerde Typical brown earth
```
