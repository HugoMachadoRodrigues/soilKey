# Mapping from VLM-schema field names to PedonRecord horizon columns

Some schema field names match horizon column names directly (`clay_pct`,
`ph_h2o`, etc.); a few do not (`munsell_moist` expands to three
columns). This helper lists the simple 1-to-1 mappings; complex ones are
handled inline in the extraction body.

## Usage

``` r
horizon_simple_attr_map()
```
