# Map FEBR layer-table columns to soilKey horizon attributes

The FEBR `camada` (layer) table uses standardised variable codes
documented at <https://www.pedometria.org/febr/dictionary/>. This
internal table records the regex patterns that map the most useful FEBR
codes onto the soilKey horizon schema. Multi-method codes (e.g. clay
determined by hydrometer vs sieve) are collapsed onto the single soilKey
column.

## Usage

``` r
.FEBR_TO_HORIZON_MAP
```

## Format

An object of class `list` of length 25.
