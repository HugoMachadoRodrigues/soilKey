# WRB Reference Soil Group code-to-name table

The ESDB `WRBLV1.tif` raster encodes RSGs as 2-letter codes (e.g. `"FL"`
for Fluvisols).
[`classify_wrb2022`](https://hugomachadorodrigues.github.io/soilKey/reference/classify_wrb2022.md)
returns the English plural name (e.g. `"Fluvisols"`). This table maps
between the two. Codes follow IUSS Working Group WRB (2022); the legacy
`"AB"` (Albeluvisols, WRB 2006) is mapped to `NA` as it does not exist
in WRB 2022.

## Usage

``` r
.WRB_LV1_NAME_BY_CODE
```

## Format

An object of class `character` of length 31.
