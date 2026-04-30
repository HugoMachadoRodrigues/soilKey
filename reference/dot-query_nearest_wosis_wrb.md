# Query WoSIS GraphQL for the nearest WRB-labeled profile.

Returns `NULL` on transport failure; `NA` fields when the bbox has no
labeled WoSIS profile.

## Usage

``` r
.query_nearest_wosis_wrb(
  lat,
  lon,
  max_distance_km,
  endpoint = "https://graphql.isric.org/wosis/graphql",
  verbose = FALSE
)
```
