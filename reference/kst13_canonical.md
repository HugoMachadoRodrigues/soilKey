# Keys to Soil Taxonomy 13th edition canonical reference

Convenience wrapper for `canonical_reference("ST_criteria_13th")`.
Returns a nested list of 3,153 parsed Keys-to-Soil-Taxonomy clauses per
chapter / page / key / taxon / code / clause / logic.

## Usage

``` r
kst13_canonical(prefer_pkg = TRUE)
```

## Arguments

- prefer_pkg:

  If `TRUE` (default), prefer the installed SoilTaxonomy package over
  the vendored copy. Set to `FALSE` to force the vendored copy (e.g. for
  reproducibility of a specific soilKey release).

## Details

Source: NCSS-tech `SoilTaxonomy` R package. Original: [USDA-NRCS (2022).
*Keys to Soil Taxonomy*, 13th
edition.](https://www.nrcs.usda.gov/sites/default/files/2022-09/Keys-to-Soil-Taxonomy.pdf)
