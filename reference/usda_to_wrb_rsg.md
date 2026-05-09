# USDA Soil Taxonomy \<-\> WRB Reference Soil Group correlation table

Returns the single most-common WRB RSG for a given USDA Order + optional
Suborder. Based on IUSS WRB (2022) Annex 6.

## Usage

``` r
usda_to_wrb_rsg(usda_order, usda_suborder = NULL)
```

## Arguments

- usda_order:

  Character vector of USDA Order names. Case- insensitive; trailing 's'
  stripped (e.g.\\ both "Mollisols" and "Mollisol" accepted).

- usda_suborder:

  Optional character vector of USDA Suborder names (case-insensitive)
  used to refine the mapping. Same length as `usda_order` or recycled.

## Value

Character vector of WRB Reference Soil Group names (singular, no plural
's'). `NA` for unrecognised inputs.

## Caveat

This is a "best-guess" cross-walk for benchmark validation only.
Real-world correlation requires per-pedon evaluation of WRB diagnostic
horizons. Use this function to derive a reasonable *expected* WRB
classification from a USDA-classified pedon (e.g.\\ from KSSL/NASIS) so
that
[`classify_wrb2022()`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
can be validated against an external taxonomy on the same profiles.

## References

IUSS Working Group WRB (2022). *World Reference Base for Soil
Resources*, 4th edition, Annex 6. International Union of Soil Sciences,
Vienna.

## Examples

``` r
usda_to_wrb_rsg("Mollisols")
#> [1] "Phaeozem"
#> "Phaeozem"
usda_to_wrb_rsg("Aridisols", "Salids")
#> [1] "Solonchak"
#> "Solonchak"
usda_to_wrb_rsg(c("Spodosols", "Oxisols", "Vertisols"))
#> [1] "Podzol"    "Ferralsol" "Vertisol" 
#> c("Podzol", "Ferralsol", "Vertisol")
```
