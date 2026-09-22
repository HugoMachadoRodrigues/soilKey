# Canonical horizon column specification

Returns the schema for the `horizons` `data.table` carried by a
[`PedonRecord`](https://hugomachadorodrigues.github.io/soilKey/reference/PedonRecord.md):
an ordered named list mapping column names to their R type (`"numeric"`
or `"character"`). Adding a new attribute means editing this single
function.

## Usage

``` r
horizon_column_spec()
```

## Value

Named list of column types in canonical order.

## Examples

``` r
spec <- horizon_column_spec()
head(names(spec))
#> [1] "top_cm"                "bottom_cm"             "designation"          
#> [4] "boundary_distinctness" "boundary_topography"   "munsell_hue_moist"    
```
