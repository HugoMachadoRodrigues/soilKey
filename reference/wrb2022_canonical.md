# WRB 2022 canonical reference (parsed IUSS Working Group WRB 2022)

Convenience wrapper for `canonical_reference("WRB_4th_2022")`. Returns a
3-element list:

- `$rsg` (118 obs): Reference Soil Group + criteria text

- `$pq` (661 obs): principal qualifiers per RSG

- `$sq` (1167 obs): supplementary qualifiers per RSG

## Usage

``` r
wrb2022_canonical(prefer_pkg = TRUE)
```

## Arguments

- prefer_pkg:

  Retained for compatibility and no longer used. Until v0.9.203 `FALSE`
  selected a copy bundled with soilKey; there is no bundled copy any
  more, so the data always come from SoilTaxonomy.

## Value

The canonical WRB 2022 reference data (a list / data.frame of RSG and
qualifier criteria), read from the SoilTaxonomy package.

## Details

Source: NCSS-tech `SoilTaxonomy` R package. Original: IUSS Working Group
WRB (2022). *World Reference Base for Soil Resources*, 4th edition.
