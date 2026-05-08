# Mapping of SoilGrids 250m property names to soilKey horizon columns

SoilGrids stores nine soil properties at six standard depths;
[`lookup_soilgrids`](https://hugomachadorodrigues.github.io/soilKey/reference/lookup_soilgrids.md)
returns them in conventional units after the published per-property
scale factor. This table records the corresponding soilKey horizon
column plus an optional secondary multiplier needed to align with
soilKey unit conventions.

## Usage

``` r
.SOILGRIDS_TO_HORIZON_MAP
```

## Format

An object of class `list` of length 9.
