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

  Retained for compatibility and no longer used. Until v0.9.203 `FALSE`
  selected a copy bundled with soilKey; there is no bundled copy any
  more, so the data always come from SoilTaxonomy.

## Value

The canonical *Keys to Soil Taxonomy* (13th ed.) criteria reference (a
list / data.frame).

## Details

Source: NCSS-tech `SoilTaxonomy` R package. Original: [USDA-NRCS (2022).
*Keys to Soil Taxonomy*, 13th
edition.](https://www.nrcs.usda.gov/sites/default/files/2022-09/Keys-to-Soil-Taxonomy.pdf)
