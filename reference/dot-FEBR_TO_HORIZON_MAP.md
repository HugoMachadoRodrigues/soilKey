# Map FEBR layer-table columns to soilKey horizon attributes

The FEBR `camada` (layer) table uses standardised variable codes
documented in the FEBR data dictionary (see
<https://www.pedometria.org/febr/> for the project home; the dictionary
path moved during 2024 – the codes themselves are stable). This internal
table records the regex patterns that map the most useful FEBR codes
onto the soilKey horizon schema. Multi-method codes (e.g.\\ clay
determined by hydrometer vs sieve) are collapsed onto the single soilKey
column.

## Usage

``` r
.FEBR_TO_HORIZON_MAP
```

## Format

An object of class `list` of length 25.
