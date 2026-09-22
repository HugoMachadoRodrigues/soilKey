# USDA Soil Taxonomy diagnostic features canonical table

Convenience wrapper for `canonical_reference("ST_features")`. Returns an
84-row data.frame with one row per diagnostic feature (epipedon /
subsurface horizon / property / material) and columns:
`group, name, chapter, page, description, criteria`. The `criteria`
column is a list-column; each element holds the parsed criteria text per
feature.

## Usage

``` r
st_features_canonical(prefer_pkg = TRUE)
```

## Arguments

- prefer_pkg:

  Retained for compatibility and no longer used. Until v0.9.203 `FALSE`
  selected a copy bundled with soilKey; there is no bundled copy any
  more, so the data always come from SoilTaxonomy.

## Value

The canonical Soil Taxonomy diagnostic-features reference (a list /
data.frame).
